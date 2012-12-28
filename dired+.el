;;; dired+.el --- Extensions to Dired.
;;
;; Filename: dired+.el
;; Description: Extensions to Dired.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2013, Drew Adams, all rights reserved.
;; Created: Fri Mar 19 15:58:58 1999
;; Version: 21.2
;; Last-Updated: Fri Dec 28 09:05:49 2012 (-0800)
;;           By: dradams
;;     Update #: 6284
;; URL: http://www.emacswiki.org/dired+.el
;; Doc URL: http://www.emacswiki.org/DiredPlus
;; Keywords: unix, mouse, directories, diredp, dired
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `bookmark', `bookmark+', `bookmark+-1', `bookmark+-bmu',
;;   `bookmark+-key', `bookmark+-lit', `cl', `dired', `dired+',
;;   `dired-aux', `dired-x', `ffap', `misc-fns', `pp', `pp+',
;;   `subr-21', `thingatpt', `thingatpt+', `w32-browser'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to Dired.
;;
;;  This file extends functionalities provided by standard GNU Emacs
;;  files `dired.el', `dired-aux.el', and `dired-x.el'.
;;
;;  Key bindings changed.  Menus redefined.  `diredp-mouse-3-menu'
;;  popup menu added.  New commands.  Some commands enhanced.
;;
;;  All of the new functions, variables, and faces defined here have
;;  the prefix `diredp-' (for Dired Plus) in their names.
;;
;;  Additional suggested key bindings:
;;
;;    (define-key ctl-x-map   "d" 'diredp-dired-files)
;;    (define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)
;;
;;
;;  Fontification Level
;;  -------------------
;;
;;  If you want a maximum or minimum fontification for Dired mode,
;;  then customize option `font-lock-maximum-decoration'.  If you want
;;  a different fontification level for Dired than for other modes,
;;  you can do this too by customizing
;;  `font-lock-maximize-decoration'.
;;
;;
;;  Act on All Files
;;  ----------------
;;
;;  Most of the commands (such as `C' and `M-g') that operate on the
;;  marked files have the added feature here that multiple `C-u' use
;;  not the files that are marked or the next or previous N files, but
;;  *all* of the files in the Dired buffer.  Just what "all" files
;;  means changes with the number of `C-u', as follows:
;;
;;    `C-u C-u'         - Use all files present, but no directories.
;;    `C-u C-u C-u'     - Use all files and dirs except `.' and `..'.
;;    `C-u C-u C-u C-u' - use all files and dirs, `.' and `..'.
;;
;;    (More than four `C-u' act the same as two.)
;;
;;  This feature can be particularly useful when you have a Dired
;;  buffer with files chosen from multiple directories.
;;
;;  Note that this behavior is described only in the doc string of
;;  function `dired-get-marked-files'.  It is *not* described in the
;;  doc strings of the various commands, because that would require
;;  redefining each command separately here.  Instead, we redefine
;;  only macro `dired-map-over-marks' and function
;;  `dired-get-filename' in order to achieve this effect.
;;
;;
;;  Act on Marked (or All) Files Here and Below
;;  -------------------------------------------
;;
;;  The prefix argument behavior just described does not apply to the
;;  `diredp-*-recursive' commands.  These commands act on the marked
;;  files in the current Dired buffer or on all files in the directory
;;  if none are marked.
;;
;;  But these commands also handle marked subdirectories recursively,
;;  in the same way.  That is, they act also on the marked files in
;;  any marked subdirectories, found recursively.  If there is no
;;  Dired buffer for a given marked subdirectory then all of its files
;;  and subdirs are acted on.
;;
;;  With a prefix argument, all marks are ignored.  The commands act
;;  on all files in the current Dired buffer and all of its
;;  subdirectories, recursively.
;;
;;  All of the `diredp-*-recursive' commands are on prefix key `M-+',
;;  and they are available on submenu `Marked Here and Below' of the
;;  `Multiple' menu-bar menu.
;;
;;  If you use library `Icicles' then you have these additional
;;  commands/keys that act recursively on marked files.  They are in
;;  the `Icicles' submenu of menu `Multiple' > `Marked Here and
;;  Below'.
;;
;;  * `M-+ M-s M-s' or `M-s M-s m' - Use Icicles search (and its
;;                  on-demand replace) on the marked files.
;;
;;  * Save the names of the marked files:
;;
;;    `M-+ C-M->' - Save as a completion set, for use during
;;                  completion (e.g. with `C-x C-f').
;;
;;    `M-+ C->'   - Add marked names to the names in the current saved
;;                  completion set.
;;
;;    `M-+ C-}'   - Save persistently to an Icicles cache file, for
;;                  use during completion in another session.
;;
;;    `icicle-dired-save-marked-to-fileset-recursive' - Like `M-+
;;                  C-}', but save persistently to an Emacs fileset.
;;
;;    `M-+ C-M-}' - Save to a Lisp variable.
;;
;;
;;  In the other direction, if you have a saved set of file names then
;;  you can use `C-M-<' (`icicle-dired-chosen-files-other-window') in
;;  Dired to open a Dired buffer for just those files.  So you can
;;  mark some files and subdirs in a hierarchy of Dired buffers, use
;;  `M-+ C-}' to save their names persistently, then later use `C-{'
;;  to retrieve them, and `C-M-<' (in Dired) to open Dired on them.
;;
;;
;;  Inserted Subdirs, Multiple Dired Buffers, Files from Anywhere,...
;;  -----------------------------------------------------------------
;;
;;  These two standard Dired features are worth pointing out:
;;
;;  * You can insert multiple subdirectory listings into a single
;;    Dired buffer using `i' on each subdir line.  Use `C-u i' to
;;    specify `ls' switches.  Specifying switch `R' inserts the
;;    inserted subdirectory's subdirs also, recursively.  You can also
;;    use `i' to bounce between a subdirectory line and its
;;    inserted-listing header line.  You can delete a subdir listing
;;    using `C-u k' on its header line.  You can hide/show an inserted
;;    subdir using `$' and `M-$'.  You can use `C-_' to undo any of
;;    these operations.
;;
;;  * You can open a Dired buffer for an arbitrary set of files, from
;;    different directories.
;;
;;    First, you can pass a glob pattern with wildcards to `dired'
;;    interactively, as the file name.
;;
;;    Beyond that, you can invoke `dired' non-interactively, passing
;;    it a cons of buffer name and file names.  Relative file names
;;    are interpreted relative to the value of `default-directory'.
;;    Use absolute file names if appropriate.
;;
;;  Some other libraries, such as `Bookmark+' and `Icicles', make it
;;  easy to create or re-create Dired buffers that list specific files
;;  and have a particular set of markings.  This can be handy for
;;  using Dired buffers to manage projects.  In such use cases you
;;  might have multiple Dired buffers that have quite specific
;;  contents and that you want to keep around during a session.
;;
;;  This is one motivation for the Dired+ `diredp-*-recursive'
;;  commands, which act on the marked files in marked subdirectories,
;;  recursively.  In one sense these commands are an alternative to
;;  using a single Dired buffer with inserted subdirectories.  They
;;  let you use the same operations on the files in a set of Dired
;;  directories, without inserting those directories into an ancestor
;;  Dired buffer.
;;
;;  So you might have some subdirectories inserted in the same Dired
;;  buffer, and you might have separate Dired buffers for some
;;  subdirectories.  Sometimes it is useful to have both for the same
;;  subdirectory.  And sometimes it is useful to move from one
;;  presentation to the other.
;;
;;  You can use command `diredp-dired-inserted-subdirs' to open a
;;  separate Dired buffer for each of the subdirs that is inserted in
;;  the current Dired buffer.  Markings and Dired switches are
;;  preserved.
;;
;;  In the opposite direction, if you use `Icicles' then you can use
;;  multi-command `icicle-dired-insert-as-subdir', which lets you
;;  insert any number of directories you choose interactively into a
;;  Dired ancestor directory listing.  If a directory you choose to
;;  insert already has its own Dired buffer, then its markings and
;;  switches are preserved for the new, subdirectory listing in the
;;  ancestor Dired buffer.
;;
;;
;;  Faces defined here:
;;
;;    `diredp-compressed-file-suffix', `diredp-date-time',
;;    `diredp-deletion', `diredp-deletion-file-name',
;;    `diredp-dir-heading', `diredp-dir-priv', `diredp-display-msg',
;;    `diredp-exec-priv', `diredp-executable-tag', `diredp-file-name',
;;    `diredp-file-suffix', `diredp-flag-mark',
;;    `diredp-flag-mark-line', `diredp-get-file-or-dir-name',
;;    `diredp-ignored-file-name', `diredp-link-priv',
;;    `diredp-mode-line-flagged', `diredp-mode-line-marked'
;;    `diredp-no-priv', `diredp-number', `diredp-other-priv',
;;    `diredp-rare-priv', `diredp-read-priv', `diredp-symlink',
;;    `diredp-write-priv'.
;;
;;  Commands defined here:
;;
;;    `diredp-bookmark-this-file', `diredp-byte-compile-this-file',
;;    `diredp-capitalize', `diredp-capitalize-recursive',
;;    `diredp-capitalize-this-file', `diredp-chgrp-this-file',
;;    `diredp-chmod-this-file', `diredp-chown-this-file',
;;    `diredp-compress-this-file',
;;    `diredp-copy-filename-as-kill-recursive',
;;    `diredp-copy-tags-this-file', `diredp-copy-this-file',
;;    `diredp-delete-this-file', `diredp-describe-file',
;;    `diredp-describe-mode', `diredp-dired-files',
;;    `diredp-dired-files-other-window', `diredp-dired-for-files',
;;    `diredp-dired-for-files-other-window',
;;    `diredp-dired-inserted-subdirs', `diredp-dired-plus-help',
;;    `diredp-dired-this-subdir', `diredp-dired-union',
;;    `diredp-dired-union-other-window', `diredp-do-bookmark',
;;    `diredp-do-bookmark-in-bookmark-file',
;;    `diredp-do-bookmark-in-bookmark-file-recursive',
;;    `diredp-do-bookmark-recursive', `diredp-do-chmod-recursive',
;;    `diredp-do-chgrp-recursive', `diredp-do-chown-recursive',
;;    `diredp-do-copy-recursive', `diredp-do-decrypt-recursive',
;;    `diredp-do-encrypt-recursive',
;;    `diredp-do-find-marked-files-recursive', `diredp-do-grep',
;;    `diredp-do-grep-recursive', `diredp-do-hardlink-recursive',
;;    `diredp-do-isearch-recursive',
;;    `diredp-do-isearch-regexp-recursive',
;;    `diredp-do-move-recursive', `diredp-do-paste-add-tags',
;;    `diredp-do-paste-replace-tags', `diredp-do-print-recursive',
;;    `diredp-do-query-replace-regexp-recursive',
;;    `diredp-do-redisplay-recursive',
;;    `diredp-do-relsymlink-recursive', `diredp-do-remove-all-tags',
;;    `diredp-do-search-recursive', `diredp-do-set-tag-value',
;;    `diredp-do-shell-command-recursive', `diredp-do-sign-recursive',
;;    `diredp-do-symlink-recursive', `diredp-do-tag',
;;    `diredp-do-touch-recursive', `diredp-do-untag',
;;    `diredp-do-verify-recursive', `diredp-downcase-recursive',
;;    `diredp-downcase-this-file', `diredp-ediff', `diredp-fileset',
;;    `diredp-find-a-file', `diredp-find-a-file-other-frame',
;;    `diredp-find-a-file-other-window',
;;    `diredp-find-file-other-frame',
;;    `diredp-find-file-reuse-dir-buffer',
;;    `diredp-flag-region-files-for-deletion',
;;    `diredp-grep-this-file', `diredp-hardlink-this-file',
;;    `diredp-image-dired-comment-files-recursive',
;;    `diredp-image-dired-delete-tag-recursive',
;;    `diredp-image-dired-display-thumbs-recursive',
;;    `diredp-image-dired-tag-files-recursive',
;;    `diredp-insert-as-subdir', `diredp-insert-subdirs',
;;    `diredp-insert-subdirs-recursive',
;;    `diredp-list-marked-recursive', `diredp-load-this-file',
;;    `diredp-marked', `diredp-marked-other-window',
;;    `diredp-marked-recursive',
;;    `diredp-marked-recursive-other-window',
;;    `diredp-mark-files-tagged-all', `diredp-mark-files-tagged-none',
;;    `diredp-mark-files-tagged-not-all',
;;    `diredp-mark-files-tagged-some',
;;    `diredp-mark-files-tagged-regexp', `diredp-mark-region-files',
;;    `diredp-mark/unmark-extension', `diredp-mouse-3-menu',
;;    `diredp-mouse-backup-diff', `diredp-mouse-copy-tags',
;;    `diredp-mouse-describe-file', `diredp-mouse-diff',
;;    `diredp-mouse-do-bookmark', `diredp-mouse-do-byte-compile',
;;    `diredp-mouse-do-chgrp', `diredp-mouse-do-chmod',
;;    `diredp-mouse-do-chown', `diredp-mouse-do-compress',
;;    `diredp-mouse-do-copy', `diredp-mouse-do-delete',
;;    `diredp-mouse-do-grep', `diredp-mouse-do-hardlink',
;;    `diredp-mouse-do-load', `diredp-mouse-do-print',
;;    `diredp-mouse-do-remove-all-tags', `diredp-mouse-do-rename',
;;    `diredp-mouse-do-set-tag-value',
;;    `diredp-mouse-do-shell-command', `diredp-mouse-do-symlink',
;;    `diredp-mouse-do-tag', `diredp-mouse-do-untag',
;;    `diredp-mouse-downcase', `diredp-mouse-ediff',
;;    `diredp-mouse-find-file', `diredp-mouse-find-file-other-frame',
;;    `diredp-mouse-find-file-reuse-dir-buffer',
;;    `diredp-mouse-flag-file-deletion', `diredp-mouse-mark',
;;    `diredp-mouse-mark-region-files', `diredp-mouse-mark/unmark',
;;    `diredp-mouse-unmark', `diredp-mouse-upcase',
;;    `diredp-mouse-view-file',
;;    `diredp-multiple-w32-browser-recursive',
;;    `diredp-nb-marked-in-mode-name', `diredp-omit-marked',
;;    `diredp-omit-unmarked', `diredp-paste-add-tags-this-file',
;;    `diredp-paste-replace-tags-this-file', `diredp-print-this-file',
;;    `diredp-relsymlink-this-file',
;;    `diredp-remove-all-tags-this-file', `diredp-rename-this-file',
;;    `diredp-send-bug-report',
;;    `diredp-set-bookmark-file-bookmark-for-marked',
;;    `diredp-set-bookmark-file-bookmark-for-marked-recursive',
;;    `diredp-set-tag-value-this-file',
;;    `diredp-shell-command-this-file', `diredp-symlink-this-file',
;;    `diredp-tag-this-file', `diredp-toggle-find-file-reuse-dir',
;;    `diredp-touch-this-file', `diredp-toggle-marks-in-region',
;;    `diredp-unmark-files-tagged-all',
;;    `diredp-unmark-files-tagged-none',
;;    `diredp-unmark-files-tagged-not-all',
;;    `diredp-unmark-files-tagged-some', `diredp-unmark-region-files',
;;    `diredp-untag-this-file', `diredp-upcase-recursive',
;;    `diredp-upcase-this-file', `diredp-w32-drives',
;;    `diredp-w32-drives-mode', `toggle-diredp-find-file-reuse-dir'.
;;
;;  User options defined here:
;;
;;    `diff-switches', `diredp-prompt-for-bookmark-prefix-flag',
;;    `diredp-w32-local-drives'.
;;
;;  Non-interactive functions defined here:
;;
;;    `derived-mode-p' (Emacs < 22), `diredp-all-files',
;;    `diredp-ancestor-dirs', `diredp-bookmark',
;;    `diredp-create-files-non-directory-recursive',
;;    `diredp-directories-within',
;;    `diredp-dired-files-interactive-spec',
;;    `diredp-dired-plus-description',
;;    `diredp-dired-plus-description+links',
;;    `diredp-dired-plus-help-link', `diredp-dired-union-1',
;;    `diredp-dired-union-interactive-spec',
;;    `diredp-do-chxxx-recursive', `diredp-do-create-files-recursive',
;;    `diredp-do-grep-1', `diredp-ensure-mode',
;;    `diredp-fewer-than-2-files-p', `diredp-find-a-file-read-args',
;;    `diredp-files-within', `diredp-files-within-1',
;;    `diredp-get-confirmation-recursive', `diredp-get-files',
;;    `diredp-get-files-for-dir', `diredp-internal-do-deletions',
;;    `diredp-list-files', `diredp-make-find-file-keys-reuse-dirs',
;;    `diredp-make-find-file-keys-not-reuse-dirs', `diredp-maplist',
;;    `diredp-marked-here', `diredp-mark-files-tagged-all/none',
;;    `diredp-mark-files-tagged-some/not-all',
;;    `diredp-paste-add-tags', `diredp-paste-replace-tags',
;;    `diredp-read-bookmark-file-args', `diredp-remove-if-not',
;;    `diredp-set-tag-value', `diredp-tag',
;;    `diredp-this-file-marked-p', `diredp-this-file-unmarked-p',
;;    `diredp-this-subdir', `diredp-untag', `diredp-y-or-n-files-p'.
;;
;;  Variables defined here:
;;
;;    `diredp-file-line-overlay', `diredp-files-within-dirs-done',
;;    `diredp-font-lock-keywords-1', `diredp-loaded-p',
;;    `diredp-menu-bar-immediate-menu',
;;    `diredp-menu-bar-immediate-bookmarks-menu',
;;    `diredp-menu-bar-mark-menu', `diredp-menu-bar-operate-menu',
;;    `diredp-menu-bar-operate-bookmarks-menu',
;;    `diredp-menu-bar-recursive-marked-menu',
;;    `diredp-menu-bar-regexp-menu', `diredp-menu-bar-subdir-menu',
;;    `diredp-re-no-dot', `diredp-w32-drives-mode-map'.
;;
;;
;;  ***** NOTE: The following macros defined in `dired.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-map-over-marks'    - Treat multiple `C-u' specially.
;;  `dired-mark-if'           - Better initial msg - Emacs bug #8523.
;;
;;  ***** NOTE: The following functions defined in `dired.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-delete'         - Display message to warn that marked,
;;                              not flagged, files will be deleted.
;;  `dired-do-flagged-delete' - Display message to warn that flagged,
;;                              not marked, files will be deleted.
;;  `dired-find-file'         - Allow `.' and `..' (Emacs 20 only).
;;  `dired-get-filename'      - Test `./' and `../' (like `.', `..').
;;  `dired-goto-file'         - Fix Emacs bug #7126.
;;                              Remove `/' from dir before compare.
;;  `dired-insert-directory'  - Compute WILDCARD arg for
;;                              `insert-directory' for individual file
;;                              (don't just use nil). (Emacs 23+, and
;;                              only for MS Windows)
;;  `dired-insert-set-properties' - `mouse-face' on whole line.
;;  `dired-mark-files-regexp' - Add regexp to `regexp-search-ring'.
;;  `dired-mark-pop-up'       - Delete the window or frame popped up,
;;                              afterward, and bury its buffer. Do not
;;                              show a menu bar for pop-up frame.
;;  `dired-pop-to-buffer'     - Put window point at bob (bug #12281).
;;
;;; NOT YET:
;;; ;;  `dired-readin-insert'     - Use t as WILDCARD arg to
;;; ;;                              `dired-insert-directory'.  (Emacs 23+,
;;; ;;                              and only for MS Windows)
;;
;;  `dired-revert'            - Reset `mode-line-process' to nil.
;;  `dired-switches-escape-p' - Made compatible with Emacs 20, 21.
;;  `dired-up-directory'      - On Windows, go up to list of drives.
;;
;;  The following functions are included here with NO CHANGES to their
;;  definitions.  They are here only to take advantage of the new
;;  definition of macro `dired-map-over-marks':
;;
;;  `dired-do-redisplay', `dired-get-marked-files',
;;  `dired-map-over-marks-check',
;;  `image-dired-dired-insert-marked-thumbs' (from `image-dired.el').
;;
;;
;;  ***** NOTE: The following functions defined in `dired-aux.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-byte-compile', `dired-do-compress', `dired-do-load' -
;;     Redisplay only if at most one file is being treated.
;;  `dired-maybe-insert-subdir' - Go back to subdir line if in listing.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-x.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-find-marked-files' -
;;     Doc string reflects new `dired-simultaneous-find-file'.
;;  `dired-mark-sexp' - 1. Variable `s' -> `blks'.
;;                      2. Fixes to `uid' and `gid'.
;;  `dired-mark-unmarked-files' (Emacs < 24 only) - Emacs 24+ version.
;;  `dired-simultaneous-find-file' -
;;     Use separate frames instead of windows if `pop-up-frames' is
;;     non-nil, or if prefix arg < 0.
;;
;;  ***** NOTE: (Emacs 20 only) The following variable defined in
;;        `dired.el' has been REDEFINED HERE:
;;
;;  `dired-move-to-filename-regexp' - Recognize file size in k etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2012/12/18 dadams
;;     diredp-ediff: Better default for FILE2.  Thx to Michael Heerdegen.
;;     Require subr-21.el for Emacs 20.
;; 2012/11/17 dadams
;;     Added: derived-mode-p (for Emacs < 22), diredp-ensure-mode.
;;     Use diredp-ensure-mode everywhere for mode, so compatible with Sunrise Commander etc.
;; 2012/11/01 dadams
;;     Do not require ediff.el.  It is required in diredp-ediff itself.
;; 2012/10/06 dadams
;;     Added: minibuffer-with-setup-hook for code byte-compiled using Emacs < 22.
;; 2012/09/28 dadams
;;     Moved dired-*w32* bindings after normal mouse bindings, so they override them.
;; 2012/09/05 dadams
;;     diredp-(rename|copy|(rel)symlink|hardlink)-this-file: Bind use-file-dialog to nil.
;; 2012/08/26 dadams
;;     Set font-lock-defaults to a 3-element list, so it works with font-menus(-da).el.
;; 2012/08/25 dadams
;;     Added: redefinition of dired-pop-to-buffer (fix for bug #12281).
;;     dired-mark-pop-up: If buffer is shown in a separate frame, do not show menu bar.
;; 2012/07/10 dadams
;;     Removed unneeded substitute-key-definition for (next|previous)-line.
;; 2012/07/09 dadams
;;     Added redefinition of dired-mark-files-regexp: Push REGEXP onto regexp-search-ring.
;; 2012/06/21 dadams
;;     diredp-nb-marked-in-mode-name:
;;       Add marker numbers regardless of name match.
;;       Use text property dired+-mode-name to tell whether mode-name was already changed.
;; 2012/06/20 dadams
;;     Added: diredp-nb-marked-in-mode-name, diredp-mode-line-(flagged|marked).  Added to hooks.
;;     Thx to Michael Heerdegen.
;; 2012/06/14 dadams
;;     dired-mark-pop-up: Wrap save-excursion around window/frame deletion.
;;     dired-do-redisplay: Updated wrt Emacs 23: bind, (then run) dired-after-readin-hook.
;;     diredp-y-or-n-files-p: Corrected construction of prompt wrt final SPC.
;; 2012/06/13 dadams
;;     dired-buffers-for-dir: Updated wrt Emacs 23:
;;       If dired-directory is a list then expand FILE in DIR & check whether in cdr of list.
;;     diredp-get-files-for-dir, diredp-files-within-1, diredp-insert-as-subdir:
;;       Expand dir name before passing it to dired-buffers-for-dir.
;; 2012/06/05 dadams
;;     MS Windows: Just do not define commands that are inappropriate for Windows (instead of
;;       defining them to raise an error or making them invisible in menus).
;; 2012/06/02 dadams
;;     Added: diredp-do-(print|encrypt|decrypt|sign|verify)-recursive.  Menus.  Keys.
;;     diredp-do-move-recursive: Corrected to use dired-rename-file, not dired-copy-file.
;; 2012/05/30 dadams
;;     diredp-insert-as-subdir: Added optional arg IN-DIRED-NOW-P.  Pick up markings & switches
;;                              from sole Dired buffer for CHILD if not in Dired now.
;; 2012/05/29 dadams
;;     Added: diredp-do-(chxxx|chgrp|chown|touch)-recursive, diredp-touch-this-file,
;;       diredp-menu-bar-(immediate|operate)-bookmarks-menu.  Added to menus.  Bound to keys.
;;     Factored bookmark stuff into Bookmark(s) submenus.
;;     diredp-menu-bar-immediate-menu: Added dired-kill-subdir, [goto-subdir].
;;     diredp-dired-this-subdir, dired-maybe-insert-subdir: Corrected :visible/enable.
;;     diredp-dired-inserted-subdirs: Do dired-(remember-marks|mark-rememberd) in this-buff.
;;     diredp-mouse-3-menu:
;;       Do not use save-excursion, because some commands move point on purpose.  Just return to
;;         original point unless command intends to MOVEP.
;;       Added to menu dired-maybe-insert-subdir (two entries), dired-kill-subdir.
;;       Use *-this-file*, not *-do-*: copy|symlink|shell-command|grep|load (don't use :keys).
;; 2012/05/26 dadams
;;     diredp-dired-inserted-subdirs, diredp-insert-as-subdir:
;;       Preserve markings and switches in target buffer.
;;     dired-mark-pop-up: Use unwind-protect.  Bury buffer too.
;;     diredp-do-chmod-recursive: Use only 5 args if < Emacs 23.
;; 2012/05/25 dadams
;;     Added: diredp-insert-as-subdir, diredp-ancestor-dirs, diredp-maplist,
;;            diredp-do-redisplay-recursive, diredp-do-chmod-recursive.
;;            Bound diredp-do-chmod-recursive. to M-+ M and added to menu.
;;     diredp-get-files: Added optional arg DONT-ASKP.
;;     diredp-y-or-n-files-p: Kill list buffer if it was never shown.
;;     dired-mark-pop-up: ignore error when delete frame/window.
;; 2012/05/22 dadams
;;     diredp-get-files(-for-dir): Added optional arg INCLUDE-DIRS-P.
;;     Added: diredp-insert-subdirs(-recursive), diredp(-this)-dired-inserted-subdir(s).
;;            Added to menus.  Bound diredp-insert-subdirs* to (M-+) M-i.
;;     Bound diredp-capitalize(-recursive) to (M-+) %c.
;;     Added diredp-dired-union-other-window to Dirs menu.
;;     Updated diredp-dired-plus-description.
;; 2012/05/19 dadams
;;     Added: diredp-image-dired-*-recursive, diredp-*link-recursive,
;;            diredp-do-isearch(-regexp)-recursive, diredp-do-query-replace-regexp-recursive,
;;            diredp-do-search-recursive, diredp-(capitalize|(up|down)case)-recursive,
;;            diredp-create-files-non-directory-recursive.
;;              Bound on M-+ prefix key.  Added to menus.
;;     diredp-get-files, diredp-y-or-n-files-p, diredp-list-files, diredp-list-marked-recursive:
;;       Added optional arg PREDICATE.
;;     diredp-do-create-files-recursive: Removed MARKER-CHAR arg.  Hard-code to keep markings.
;;     diredp-do-(copy|move)-recursive: Use arg IGNORE-MARKS-P (forgot to use it).
;;                                      Removed MARKER-CHAR arg in call to d-d-c-f-r.
;;     Added missing autoload cookies.
;; 2012/05/06 dadsms
;;     diredp-y-or-n-files-p: Do not kill buffer *Files* - just bury it.
;; 2012/05/05 dadams
;;     Added: diredp-do-bookmark-recursive, diredp-do-bookmark-in-bookmark-file-recursive,
;;            diredp-set-bookmark-file-bookmark-for-marked-recursive.
;;              Bound to M-+ M-b, M-+ C-M-B, M-+ C-M-b, respectively.  Added to menus.
;;     diredp-bookmark: Added optional arg FILE.
;;     diredp-do-bookmark-in-bookmark-file: Added optional arg FILES.
;;     diredp-dired-plus-description: Updated.
;;     diredp-get-confirmation-recursive: Raise error if not in Dired.
;;     diredp-do-bookmark-recursive, diredp-marked-recursive(-other-window),
;;       diredp-multiple-w32-browser-recursive:
;;         Use diredp-get-confirmation-recursive.
;; 2012/05/04 dadams
;;     Added: dired-mark-unmarked-files for Emacs < 24.
;;     diredp-do-create-files-recursive: Corrected for Emacs < 24.
;;     diredp-do-create-files-recursive, diredp-(un)mark-files-tagged-regexp,
;;       diredp(-mouse)-do-(un)tag, diredp(-mouse)-do-remove-all-tags,
;;       diredp(-mouse)-do-paste-(add|replace)-tags, diredp(-mouse)-do-set-tag-value,
;;       diredp(-mouse)-do-bookmark(-in-bookmark-file), diredp-find-a-file-read-args,
;;       diredp-mouse-do-shell-command:
;;         Use lexical-let(*), to get closures for free vars in lambdas.
;; 2012/04/28 dadams
;;     Added:
;;       diredp-copy-filename-as-kill-recursive, diredp-do-copy-recursive,
;;       diredp-do-find-marked-files-recursive, diredp-do-grep-recursive,
;;       diredp-do-move-recursive, diredp-do-shell-command-recursive,
;;       diredp-list-marked-recursive, diredp-marked-recursive(-other-window),
;;       diredp-multiple-w32-browser-recursive, diredp-do-create-files-recursive,
;;       diredp-get-confirmation-recursive, diredp-list-files, diredp-y-or-n-files-p,
;;       diredp-menu-bar-recursive-marked-menu.
;;     diredp-get-files: Use diredp-y-or-n-files-p, not y-or-n-p.
;;     Commented out dired-readin-insert - see comment.
;;     Moved bookmark menu items to submenu Bookmarks.
;;     Added keys (with M-+ prefix) and menu items for new (*-recursive) commands.
;;     Reordered w32-browser stuff in menus.
;;     diredp-do-grep: Combined defs for diff Emacs versions - do version test at runtime.
;; 2012/04/25 dadams
;;     dired-insert-directory: Updated per Emacs 24.
;; 2012/04/23 dadams
;;     Added (moved here from Icicles, and renamed prefix):
;;       diredp-re-no-dot, diredp-get-files, diredp-get-files-for-dir, diredp-files-within,
;;       diredp-files-within-dirs-done.
;; 2012/04/05 dadams
;;     Added redefinition of dired-mark-pop-up, to fix Emacs bug #7533.  If they ever fix it
;;     then remove this hack.
;; 2012/03/13 dadams
;;     diredp-dired(-for)-files(-other-window):
;;       Bind: icicle-sort-comparer, icicle-all-candidates-list-alt-action-fn.
;;       Use icicle-(un)bind-file-candidate-keys.
;;     diredp-dired-files-interactive-spec: Updated doc strings accordingly.
;; 2012/03/07 dadams
;;     Added: dired-switches-escape-p.
;;     dired-get-filename: Updated wrt Emacs 24:
;;       whitespace quoting for bug #10469, filename quoting per Emacs 23.3+,
;;         MS Windows conversion of \ to / per Emacs 23.3+.
;;     dired-goto-file: Escape whitespace, per Emacs 24 (for bug #10469).
;; 2012/03/02 dadams
;;     Require cl.el at compile time even for Emacs 22+, for case macro.
;; 2012/02/28 dadams
;;     Do not bother to soft-require mkhtml.el anymore.
;; 2012/02/18 dadams
;;     Swapped keys for dired-w32(-browser|explore), so the former is M-RET, as in Bookmark+.
;; 2012/01/10 dadams
;;     diredp-font-lock-keywords-1: Corrected for date/time when locale is used, not iso.
;; 2011/12/19 dadams
;;     dired-insert-set-properties, dired-mark-sexp, diredp-(un)mark-region-files,
;;       diredp-flag-region-files-for-deletion, diredp-mouse-3-menu:
;;         Use line-(beginning|end)-position.
;; 2011/12/16 dadams
;;     diredp-menu-bar-mark-menu: Removed Revert item.
;;     diredp-menu-bar-subdir-menu: Added: image-dired-dired-toggle-marked-thumbs.
;;     diredp-mouse-3-menu:
;;       Use commands bound to keys, so the keys show up in the menu.  Prefer *-this-file.
;;       Correct the mark/unmark/flag menu-item visibility.  Added Capitalize.
;; 2011/12/09 dadams
;;     diredp-w32-drives: Use dolist, not mapcar.
;;     diredp-mouse-3-menu: Use easymenu to build the menu.  Conditionalize some items.
;;     Bind down-mouse-3, not mouse-3, to diredp-mouse-3-menu.  (bind mouse-3 to ignore).
;;     Added eval-when-compile for easymenu.el.
;; 2011/12//02 dadams
;;     Added diredp-internal-do-deletions.
;;     dired(-mouse)-do(-flagged)-delete, : Use diredp-internal-do-deletions, for trash.
;; 2011/11/29 dadams
;;     diredp-read-bookmark-file-args: Corrected use of list of default file names: > Emacs 23.1.
;; 2011/10/31 dadams
;;     dired-mode-hook: Call font-lock-refresh-defaults - see Emacs 24 bugs #6662 and #9919.
;; 2011/10/24 dadams
;;     Protect dired-show-file-type with fboundp.
;; 2011/09/03 dadams
;;     diredp-do-grep-1: Map shell-quote-argument over file names.  Thx to Joe Bloggs.
;; 2011/08/07 dadams
;;     diredp-bookmark (need to keep in sync with bmkp-make-record-for-target-file):
;;       Instead of image-bookmark-make-record, use explicit function that includes file, type.
;; 2011/07/25 dadams
;;     Changed featurep to eval-after-load, for bookmark+-1.el and w32-browser.el.
;; 2011/07/01 dadams
;;     Fixed typo: dired-toggle-find-file-reuse-dir -> ...diredp....  Thx to pasja on Emacs Wiki.
;; 2011/06/18 dadams
;;     Added: diredp-describe-mode, diredp-dired-plus-help(-link), diredp-help-button,
;;            diredp-dired-plus-description(+links), diredp-send-bug-report.
;;     Bound diredp-describe-mode to whatever describe-mode is bound to.
;;     All menus, :enable with mark-active: Added transient-mark-mode and mark != point.
;;     toggle-diredp-find-file-reuse-dir: Swapped which one is the alias.
;;     diredp-w32-list-mapped-drives: Display *Shell Command Output* at end.
;;     diredp-mouse-(describe-file|3-menu|mark/unmark|(find|view)-file(-other-window)):
;;       save-excursion set-buffer -> with-current-buffer.
;; 2011/06/08 dadams
;;     Added: diredp-dired-for-files(-other-window).
;; 2011/06/07 dadams
;;     Bound dired-show-file-type to _, since y is diredp-relsymlink-this-file.
;; 2011/04/25 dadams
;;     Added (from files+.el): dired(-mouse)-describe-file. Bound to C-h (C-)RET, added to menus.
;; 2011/04/23 dadams
;;     Added, bound (T c, T M-w, T 0, T v, T p, T C-y, T q), and added to menus:
;;       diredp-copy-tags-this-file, diredp-mouse-copy-tags,
;;       diredp(-mouse)(-do)-remove-all-tags(-this-file),
;;       diredp(-mouse)(-do)-set-tag-value(-this-file),
;;       diredp(-mouse)(-do)-paste-(add|replace)-tags(-this-file).
;;     diredp-mark-files-tagged-(all/none|some/not-all): Bound free var presentp.
;;     dired-map-over-marks: Corrected: Bind NEWARG and use that, not ARG.
;;     dired-get-marked-files: let* -> let.
;;     dired-do-redisplay, diredp-mouse-diff: when/if -> and.
;;     dired-readin-insert, dired-get-filename: if -> unless/when.
;;     diredp-mouse-find-file-reuse-dir-buffer: with-current-buffer, not save...
;;     diredp-mouse-mark/unmark: Removed unused bol/eol vars.
;; 2011/04/19 dadams
;;     Added: diredp-(un)mark-files-tagged-((not-)all|none|some|regexp|all/none|some/not-all),
;;            dired-mark-if.  Added Tagged submenu for Mark menu.
;;     Put tags commands on prefix key T, as in Bookmark+.  Removed C-(M-)+/- tags-cmd bindings.
;;     diredp-untag-this-file: Added prefix-arg behavior.
;; 2011/04/18 dadams
;;     Added: diredp-prompt-for-bookmark-prefix-flag.
;;            Use it in diredp(-mouse)-do-(un)tag, diredp-read-bookmark-file-args,
;;                      diredp(-mouse)-do-bookmark, diredp-(bookmark|(un)tag)-this-file.
;;     diredp-(bookmark|(un)tag)-this-file, diredp(-do)-bookmark, diredp-(un)tag,
;;       diredp-do-bookmark-in-bookmark-file, diredp-set-bookmark-file-bookmark-for-marked:
;;         Made PREFIX arg optional.
;; 2011/04/17 dadams
;;     Added: diredp-(bookmark|(un)tag)-this-file, diredp(-mouse)(-do)-(un)tag.
;;     diredp-mouse-3-menu: Added: diredp-mouse-do-(un)tag.
;;     diredp-menu-bar-immediate-menu: Added diredp-(un)tag-this-file, diredp-bookmark-this-file.
;;     diredp-menu-bar-operate-menu: Added diredp-do-(un)tag.
;;     Bound diredp-do-tag to C-+, diredp-tag-this-file to C-M-+, diredp-do-untag to C--,
;;           diredp-untag-this-file to C-M--, diredp-bookmark-this-file to C-B.
;;     diredp-bookmark: Use bmkp-autofile-set, not bmkp-file-target-set, so get autofile.
;;     diredp-read-bookmark-file-args, diredp(-mouse)-do-bookmark:
;;       Default for prefix is now an empty string, not the directory.
;;     diredp-mouse-do-bookmark: Removed optional second arg.
;;     Corrected typo: direp-read-bookmark-file-args -> diredp-read-bookmark-file-args.
;; 2011/03/25 dadams
;;     diredp-bookmark: Fixed typo: bmkp-file-indirect-set -> bmkp-file-target-set.
;; 2011/02/11 dadams
;;     diredp-deletion, diredp-deletion-file-name, diredp-executable-tag:
;;       Made default the same for dark background as for light.
;;     diredp-ignored-file-name: Made default a bit darker for dark background.
;; 2011/02/03 dadams
;;     All deffaces: Provided default values for dark-background screens too.
;; 2011/01/12 dadams
;;     dired-do-flagged-delete: Removed sit-for added on 1/02.
;; 2011/01/04 dadams
;;     defsubst -> defun everywhere.
;;     Removed autoload cookies from non def* sexps, defvar, and non-interactive functions.
;;     Added some missing autoload cookies for defcustom and commands.
;; 2011/01/02 dadams
;;     Added: diredp-this-file-(un)marked-p, diredp-toggle-marks-in-region.
;;     diredp-(un)mark-region-files, diredp-flag-region-files-for-deletion:
;;       Act only on marked/unmarked files (opposite).  Fix 2nd arg to dired-mark-if.
;;     diredp-mouse-3-menu:
;;       If region is active and mouse3.el was loaded, then use its popup.
;;       Fix Toggle Marked/Unmarked:
;;         Use diredp-toggle-marks-in-region, so widen, show details and use bol/eol.
;;     dired-do-flagged-delete: Added sit-for.
;; 2010/11/28 dadams
;;     diredp-mouse-3-menu: Added Toggle Marked/Unmarked for region menu.
;; 2010/10/20 dadams
;;     Moved Emacs 20 tweak to recognize k in file sizes to var dired-move-to-filename-regexp.
;;     Added diredp-loaded-p.
;; 2010/10/19 dadams
;;     diredp-font-lock-keywords-1:
;;       Handle decimal pt in file size.  Thx to Michael Heerdegen.
;;       Enable Emacs 20/21 to handle -h option (decimal point size).
;;     Renamed: face diredp-inode+size to diredp-number.
;; 2010/10/01 dadams
;;     dired-goto-file: Avoid infloop from looking for dir line.  Thx to not-use.dilines.net.
;; 2010/09/29 dadams
;;     Added: diredp-dired-union(-1|-other-window|-interactive-spec).
;;     dired-goto-file: fix for Emacs bug #7126.
;; 2010/09/27 dadams
;;     Renamed diredp-dired-interactive-spec to diredp-dired-files-interactive-spec.
;;     diredp-dired-files-interactive-spec: Respect file-list arg: kill existing Dired buffer.
;;                                          Fix use of prefix arg for switches.
;; 2010/09/26 dadams
;;     Added: dired-insert-directory: Compute WILDCARD arg for individual files.
;;     Added: dired-readin-insert: Use t as WILDCARD arg to dired-insert-directory.
;;     Added: diredp-dired-files(-other-window), diredp-dired-interactive-spec.
;; 2010/08/27 dadams
;;     Use diredp-font-lock-keywords-1 properly as a second level of fontification.
;;     Added: diredp-w32-drives(-mode(-map)), dired-up-directory.
;; 2010/08/07 dadams
;;     dired-map-over-marks: Removed loop that used dired-between-files.
;;     diredp-get-file-or-dir-name: test against subdir/..? also.
;;     dired-do-find-marked-files: Pass original ARG to dired-get-marked-files.
;; 2010/08/05 dadams
;;     diredp-bookmark:
;;       Handle image files (and sound files, if Bookmark+ is used).
;;       Use bmkp-file-indirect-set if available.
;;       Use error-message-string to get failure msg.
;; 2010/07/11 dadams
;;     Added: diredp-set-bookmark-file-bookmark-for-marked (C-M-b), diredp-mouse-do-bookmark,
;;            diredp-do-bookmark-in-bookmark-file (C-M-S-b), diredp-read-bookmark-file-args.
;;     Added them to the operate menu.  Added diredp-do-bookmark to mouse-3 menu.
;; 2010/07/07 dadams
;;     dired-do-*: Updated doc strings for prefix arg treatment from dired-map-over-marks-check.
;;     Added missing autoload cookies.
;; 2010/05/29 dadams
;;     diredp-bookmark: Use relative file name in bookmark name.
;;     Removed defvar of directory-listing-before-filename-regexp.
;; 2010/05/28 dadams
;;     Changed menu item for dired-create-directory to New Directory.  Moved it before Up Dir.
;; 2010/03/19 dadams
;;     diredp-font-lock-keywords-1: Handle date+time wrt regexp changes for Emacs 23.2.
;; 2010/01/31 dadams
;;     diredp-bookmark:
;;       Don't use bookmark-set or find-file-noselect - inline the needed bookmark-store code.
;;       Call bookmark-maybe-load-default-file.  Use rudimentary bookmark-make-record-function.
;; 2010/01/21 dadams
;;     Renamed:
;;       diredp-subst-find-alternate-for-find to diredp-make-find-file-keys-reuse-dirs
;;       diredp-subst-find-for-find-alternate to diredp-make-find-file-keys-not-reuse-dirs.
;;     diredp-make-find-file-keys(-not)-reuse-dirs: Handle also dired(-mouse)-w32-browser.
;; 2010/01/10 dadams
;;     Added: face diredp-inode+size.  Use in diredp-font-lock-keywords-1.
;;     diredp-font-lock-keywords-1: Allow decimal point in file size.  Thx to Regis.
;; 2010/01/05 dadams
;;     dired-insert-set-properties:
;;       Add text property dired-filename to the file name (for Emacs 23).
;; 2009/10/23 dadams
;;     diredp-font-lock-keywords-1: Override `l' and `t' matches in headings with default face.
;; 2009/10/13 dadams
;;     Added: diredp(-do)-bookmark.  Added to Multiple menu, and bound to M-b.
;; 2009/10/11 dadams
;;     diredp-menu-bar-immediate-menu:
;;       Added items: image display items, dired-maybe-insert-subdir.
;;       Test dired-do-relsymlink, not diredp-relsymlink-this-file.
;;     diredp-menu-bar-operate-menu:
;;       Added items: epa encryption items, image items, isearch items.
;;     diredp-menu-bar-subdir-menu:
;;       Added items: revert, isearch file names, dired-compare-directories.
;;     Removed macro menu-item-any-version - use menu-item everywhere (works for Emacs 20+).
;;     Added wdired-change-to-wdired-mode to subdir menu even for Emacs 20, if defined.
;; 2009/07/09 dadams
;;     dired-goto-file: Make sure we have a string before calling directory-file-name.
;; 2009/05/08 dadams
;;     dired-find-file (Emacs 20): Raise error if dired-get-filename returns nil.
;; 2009/04/26 dadams
;;     dired-insert-set-properties, diredp-(un)mark-region-files,
;;       diredp-flag-region-files-for-deletion, diredp-mouse-3-menu, diredp-mouse-mark/unmark:
;;         Bind inhibit-field-text-motion to t, to ensure real eol.
;; 2008/12/17 dadams
;;     diredp-font-lock-keywords-1: Don't do diredp-deletion, diredp-flag-mark for empty lines.
;; 2008/09/22 dadams
;;     Added: diredp-fileset, diredp-get-file-or-dir-name, and redefinitions of
;;            dired-map-over-marks, dired-find-file, and dired-mouse-find-file-other-window.
;;     Added vanilla code to pick up macro dired-map-over-marks:
;;       dired-get-marked-files, dired-do-delete, dired-map-over-marks-check,
;;       dired-do-redisplay, image-dired-dired-insert-marked-thumbs.
;;     diredp-find-file-other-frame, diredp-mouse-(find|view)-file:
;;       Added nil t args to dired-get-filename calls.
;;     diredp-do-grep(-1): Use new dired-get-marked-files instead of ad-hoc treatment of C-u.
;; 2008/09/21 dadams
;;     diredp-marked(-other-window): Don't treat zero prefix arg as numerical (no empty Dired).
;;     Added dired-find-file redefinition for Emacs 20.
;; 2008/09/11 dadams
;;     diredp-do-grep: Plain C-u means grep all files in Dired buffer.
;;     diredp-do-grep-1: Treat 'all value of FILES arg.
;;     Added: diredp-all-files.
;; 2008/09/09 dadams
;;     Added: diredp-marked(-other-window).  Added to menus.  Bound *-other-window to C-M-*.
;; 2008/09/07 dadams
;;     Added: diredp(-mouse)-do-grep(-1), diredp-grep-this-file.
;;     Bound diredp-do-grep to M-g.  Added grep commands to menus.
;; 2008/07/18 dadams
;;     Soft-require w32-browser.el.  Bind its commands in Dired map and menus.
;; 2008/03/08 dadams
;;     dired-maybe-insert-subdir: Fit one-window frame after inserting subdir.
;; 2008/03/07 dadams
;;     Added: redefinitions of dired-maybe-insert-subdir, dired-goto-file, dired-get-filename.
;;     Added: diredp-this-subdir.
;; 2007/11/27 dadams
;;     diredp-mouse(-backup)-diff: If available, use icicle-read-string-completing.
;; 2007/09/23 dadams
;;     Removed second arg to undefine-killer-commands.
;; 2007/07/27 dadams
;;     diredp-font-lock-keywords-1: Allow also for bz2 compressed files - Thx to Andreas Eder.
;; 2006/09/03 dadams
;;     diredp-font-lock-keywords-1: Corrected file size and inode number.  Thx to Peter Barabas.
;; 2006/08/20 dadams
;;     Added: diredp-find-a-file*.
;; 2006/06/18 dadams
;;     diredp-font-lock-keywords-1: Highlight file name (also) of flagged files.
;;                                  Use dired-del-marker instead of literal D.
;;     Added: diredp-deletion-file-name.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2006/01/07 dadams
;;     Added: link for sending bug report.
;; 2006/01/06 dadams
;;     Added defgroup Dired-Plus and used it. Added :link.
;; 2006/01/04 dadams
;;     Added defvar of directory-listing-before-filename-regexp, for Emacs 22 compatibility.
;; 2005/12/29 dadams
;;     Added: diredp-mouse-mark/unmark-mark-region-files.
;; 2005/12/26 dadams
;;     Updated groups.
;; 2005/12/05 dadams
;;     diredp-ignored-file-name: Made it slightly darker.
;; 2005/11/05 dadams
;;     Renamed all stuff defined here to have diredp- prefix.
;;     diredp-relsymlink-this-file: Protected with fboundp.
;;     Changed to soft require: dired-x.el.
;;     Removed comment to require this inside eval-after-load.
;; 2005/11/03 dadams
;;     Added: dired-display-msg.  Replace blue-foreground-face with it.
;;     Alias dired-do-toggle to dired-toggle-marks, if defined.
;; 2005/11/02 dadams
;;     Added: dired-get-file-for-visit, dired(-mouse)-find-alternate-file*,
;;            togglep-dired-find-file-reuse-dir, dired+-subst-find-*.
;;     Use defface for all faces.  Renamed without "-face".  No longer require def-face-const.
;;     dired-simultaneous-find-file: Minor bug fix (typo).
;; 2005/07/10 dadams
;;     dired-unmark-all-files-no-query -> dired-unmark-all-marks
;;       (thanks to Sivaram Neelakantan for bug report).
;; 2005/05/25 dadams
;;     string-to-int -> string-to-number everywhere.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/02/16 dadams
;;     Added dired-mark/unmark-extension. Replaced dired-mark-extension with it everywhere.
;; 2005/01/08 dadams
;;     Bind [S-mouse-1], instead of [S-down-mouse-1], to dired-mouse-mark-region-files.
;; 2004/11/20 dadams
;;     dired-mark-sexp: Search for literal month names only for versions before Emacs 20.
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/11/14 dadams
;;     Bound dired-no-confirm to non-nil for dired-mouse-*.
;;     Updated for Emacs 21 and improved highlighting:
;;       Spaces OK in file and directory names. Highlight date/time and size.
;; 2004/10/17 dadams
;;     Require cl only for Emacs 20, and only when compile.
;; 2004/10/01 dadams
;;     Updated to work with Emacs 21 also.
;; 2004/04/02 dadams
;;     dired-font-lock-keywords-1: Prefer using dired-omit-extensions
;;     to completion-ignored-extensions, if available.
;; 2004/03/22 dadams
;;     Added dired-mouse-mark-region-files and dired-mouse-mark/unmark.
;; 2000/09/27 dadams
;;     1. dired-font-lock-keywords-1: fixed for spaces in dir names.
;;     2. Added: dired-buffers-for-dir.
;; 1999/09/06 dadams
;;     Added S-*-mouse-2 bindings (same as C-*-mouse-2).
;; 1999/08/26 dadams
;;     1. Added *-face vars and dired-font-lock-keywords-1.
;;     2. Added possibility to use dired-font-lock-keywords-1 via hook.
;; 1999/08/26 dadams
;;     Changed key binding of dired-mouse-find-file from down-mouse-2 to mouse-2.
;; 1999/08/25 dadams
;;     Changed (C-)(M-)mouse-2 bindings.
;; 1999/08/25 dadams
;;     1. Added cmds & menu bar and key bindings: (dired-)find-file-other-frame.
;;     2. Changed binding for dired-display-file.
;; 1999/03/26 dadams
;;     1. Get rid of Edit menu-bar menu.
;;     2. dired-mouse-3-menu: Changed popup titles and item names.
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

(eval-when-compile (require 'cl)) ;; case (plus, for Emacs 20: dolist, pop, push)
(eval-when-compile (require 'easymenu)) ;; easy-menu-create-menu

(require 'dired) ;; dired-revert
(require 'dired-aux) ;; dired-bunch-files, dired-do-chxxx, dired-do-create-files,
                     ;; dired-mark-read-string, dired-read-shell-command,
                     ;; dired-run-shell-command, dired-shell-stuff-it
;; (require 'ediff-util) ;; ediff-read-file-name

(require 'dired-x nil t) ;; (no error if not found) dired-do-relsymlink
(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(when (memq system-type '(windows-nt ms-dos))
  ;; (no error if not found):
  (require 'w32-browser nil t));; dired-w32explore, dired-w32-browser, dired-mouse-w32-browser,
                               ;; dired-multiple-w32-browser
;;; (when (< emacs-major-version 21)
;;;   (require 'mkhtml nil t)) ;; (no error if not found): mkhtml-dired-files

;; Don't require Icicles, else get recursive requires.
;; (require 'icicles nil t) ;; (no error if not found): icicle-read-string-completing

(when (< emacs-major-version 21) (require 'subr-21)) ;; replace-regexp-in-string

;; Provide macro for code byte-compiled using Emacs < 22.
(eval-when-compile
 (when (< emacs-major-version 22)
   (defmacro minibuffer-with-setup-hook (fun &rest body)
     "Temporarily add FUN to `minibuffer-setup-hook' while executing BODY.
BODY should use the minibuffer at most once.
Recursive uses of the minibuffer are unaffected (FUN is not
called additional times).

This macro actually adds an auxiliary function that calls FUN,
rather than FUN itself, to `minibuffer-setup-hook'."
     ;; (declare (indent 1) (debug t))
     (let ((hook (make-symbol "setup-hook")))
       `(let (,hook)
         (setq ,hook  (lambda ()
                        ;; Clear out this hook so it does not interfere
                        ;; with any recursive minibuffer usage.
                        (remove-hook 'minibuffer-setup-hook ,hook)
                        (funcall ,fun)))
         (unwind-protect
              (progn (add-hook 'minibuffer-setup-hook ,hook) ,@body)
           (remove-hook 'minibuffer-setup-hook ,hook)))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired+)
(require 'dired+)                       ; Ensure loaded before compile this.

;; Quiet the byte-compiler.
(defvar bmkp-copied-tags)               ; In `bookmark+-1.el'
(defvar bmkp-current-bookmark-file)     ; In `bookmark+-1.el'
(defvar dired-keep-marker-hardlink)     ; In `dired-x.el'
(defvar dired-switches-alist)
(defvar dired-subdir-switches)
(defvar dired-touch-program) ; Emacs 22+
(defvar dired-use-ls-dired) ; Emacs 22+
(defvar filesets-data)
(defvar grep-use-null-device)
(defvar image-dired-line-up-method)     ; In `image-dired.el'
(defvar image-dired-thumbnail-buffer)   ; In `image-dired.el'

;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Variables

;; `dired-do-toggle' was renamed to `dired-toggle-marks' after Emacs 20.
(when (fboundp 'dired-toggle-marks) (defalias 'dired-do-toggle 'dired-toggle-marks))

;;; This is duplicated in `diff.el' and `vc.el'.
;;;###autoload
(defcustom diff-switches "-c"
  "*A string or list of strings specifying switches to be passed to diff."
  :type '(choice string (repeat string))
  :group 'dired :group 'diff)

;;;###autoload
(defcustom diredp-prompt-for-bookmark-prefix-flag nil
  "*Non-nil means prompt for a prefix string for bookmark names."
  :type 'boolean :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-w32-local-drives '(("C:" "Local disk"))
  "*Local MS Windows drives that you want to use for `diredp-w32-drives'.
Each entry is a list (DRIVE DESCRIPTION), where DRIVE is the drive
name and DESCRIPTION describes DRIVE."
  :type '(alist
          :key-type   (string        :tag "Drive name")
          :value-type (group (string :tag "Drive description")))
  :group 'Dired-Plus)

;; Same value as the default value of `icicle-re-no-dot'.
(defvar diredp-re-no-dot "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regexp that matches anything except `.' and `..'.")

(defvar diredp-w32-drives-mode-map (let ((map  (make-sparse-keymap)))
                                     (define-key map "q"       'bury-buffer)
                                     (define-key map "\r"      'widget-button-press)
                                     (define-key map [mouse-2] 'widget-button-click)
                                     map)
  "Keymap for `diredp-w32-drives-mode'.")

;;; $$$$$$ Starting with Emacs 22, *-move-to* is defvaraliased to *-listing-before*.
;;; But `files+.el' defines *-listing-before*, so we define it here too.
;;; (unless (> emacs-major-version 21)
;;;   (defvar directory-listing-before-filename-regexp dired-move-to-filename-regexp
;;;     "Regular expression to match up to the file name in a directory listing.
;;; The default value is designed to recognize dates and times
;;; regardless of the language."))
 
;;; Macros


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Better initial message - depends on `dired-marker-char'.
;;
(defmacro dired-mark-if (predicate msg)
  "Mark files for PREDICATE, according to `dired-marker-char'.
PREDICATE is evaluated on each line, with point at beginning of line.
MSG is a noun phrase for the type of files being marked.
It should end with a noun that can be pluralized by adding `s'.
Return value is the number of files marked, or nil if none were marked."
  `(let ((inhibit-read-only  t)
         count)
    (save-excursion
      (setq count  0)
      (when ,msg (message "%s %ss%s..."
                          ,(cond ((eq ?\040 dired-marker-char)            "Unmarking")
                                 ((eq dired-del-marker dired-marker-char) "Flagging")
                                 (t                                       "Marking"))
                          ,msg
                          ,(if (eq dired-del-marker dired-marker-char) " for deletion" "")))
      (goto-char (point-min))
      (while (not (eobp))
        (when ,predicate (delete-char 1) (insert dired-marker-char) (setq count  (1+ count)))
        (forward-line 1))
      (when ,msg (message "%s %s%s %s%s" count ,msg
                          (dired-plural-s count)
                          (if (eq dired-marker-char ?\040) "un" "")
                          (if (eq dired-marker-char dired-del-marker) "flagged" "marked"))))
    (and (> count 0)  count)))


;; Just a helper function for `dired-map-over-marks'.
(defun diredp-get-file-or-dir-name (arg)
  "Return name of next file or directory or nil if none.
Argument ARG:
 `all-files-no-dirs' or nil means skip directories.
 `all-files-no-dots' means skip `.' and `..'."
  (let ((fname  nil))
    (while (and (not fname)  (not (eobp)))
      (setq fname  (dired-get-filename t t))
      (when (and fname  (or (not arg) (eq arg 'all-files-no-dirs))  (file-directory-p fname))
        (setq fname  nil))
      (when (and fname  (eq arg 'all-files-no-dots)
                 (or (member fname '("." "..")) (string-match "/\.\.?$" fname)))
        (setq fname  nil))
      (forward-line 1))
    (forward-line -1)
    fname))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Treat multiple `C-u' specially.
;;
(defmacro dired-map-over-marks (body arg &optional show-progress
                                distinguish-one-marked)
  "Eval BODY with point on each marked line.  Return a list of BODY's results.
If no marked file could be found, execute BODY on the current line.
ARG, if non-nil, specifies the files to use instead of the marked files.
 If ARG is an integer, use the next ARG files (previous -ARG, if < 0).
   In that case point is dragged along.  This is so that commands on
   the next ARG (instead of the marked) files can be easily chained.
 If ARG is a cons with element 16, 64, or 256, corresponding to
   `C-u C-u', `C-u C-u C-u', or `C-u C-u C-u C-u', then use all files
   in the Dired buffer, where:
     16 includes NO directories (including `.' and `..')
     64 includes directories EXCEPT `.' and `..'
    256 includes ALL directories (including `.' and `..')
 If ARG is otherwise non-nil, use the current file.
If optional third arg SHOW-PROGRESS evaluates to non-nil,
 redisplay the dired buffer after each file is processed.
 No guarantee is made about the position on the marked line.
 BODY must ensure this itself, if it depends on this.
Search starts at the beginning of the buffer, thus the car of the list
 corresponds to the line nearest to the buffer's bottom.  This
 is also true for (positive and negative) integer values of ARG.
BODY should not be too long, since it is expanded four times.
If DISTINGUISH-ONE-MARKED is non-nil, then return (t FILENAME) instead
 of (FILENAME), if only one file is marked."
  ;;
  ;;Warning: BODY must not add new lines before point - this may cause an
  ;;endless loop.
  ;;This warning should not apply any longer, sk  2-Sep-1991 14:10.
  `(prog1
    (let ((inhibit-read-only  t)
          (newarg             ,arg)
          multi-C-u case-fold-search found results)
      (when (and (consp newarg)  (> (prefix-numeric-value newarg) 4))
        (setq newarg     (case (prefix-numeric-value newarg)
                           (16   'all-files-no-dirs) ; `C-u C-u'
                           (64   'all-files-no-dots) ; `C-u C-u C-u'
                           (256  'all-files) ; `C-u C-u C-u C-u'
                           (t    'all-files-no-dirs))
              multi-C-u  t))
      (if (and newarg  (not multi-C-u))
          (if (integerp newarg)
              (progn                    ; No `save-excursion', want to move point.
                (dired-repeat-over-lines newarg #'(lambda ()
                                                    (when ,show-progress (sit-for 0))
                                                    (setq results  (cons ,body results))))
                (if (< newarg 0) (nreverse results) results))
            ;; Non-nil, non-integer ARG means use current file:
            (list ,body))
        (let ((regexp  (dired-marker-regexp))
              next-position)
          (save-excursion
            (goto-char (point-min))
            ;; Remember position of next marked file before BODY can insert lines before the
            ;; just found file, confusing us by finding the same marked file again and again...
            (setq next-position  (and (if multi-C-u
                                          (diredp-get-file-or-dir-name newarg)
                                        (re-search-forward regexp nil t))
                                      (point-marker))
                  found          (not (null next-position)))
            (while next-position
              (goto-char next-position)
              (when ,show-progress (sit-for 0))
              (setq results  (cons ,body results))
              ;; move after last match
              (goto-char next-position)
              (forward-line 1)
              (set-marker next-position nil)
              (setq next-position  (and (if multi-C-u
                                            (diredp-get-file-or-dir-name newarg)
                                          (re-search-forward regexp nil t))
                                        (point-marker)))))
          (when (and ,distinguish-one-marked  (= (length results) 1))
            (setq results  (cons t results)))
          (if found results (list ,body)))))
    ;; `save-excursion' loses, again
    (dired-move-to-filename)))
 
;;; Utility functions

(unless (fboundp 'derived-mode-p)       ; Emacs 20, 21.
  (defun derived-mode-p (&rest modes)
    "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
    (let ((parent  major-mode))
      (while (and (not (memq parent modes))  (setq parent  (get parent 'derived-mode-parent))))
      parent)))

(defun diredp-ensure-mode ()
  (unless (derived-mode-p 'dired-mode)
    (error "You must be in Dired or a mode derived from it to use this command")))
 
;;; UNALTERED vanilla Emacs code to be reloaded, to use the new definition
;;; of `dired-map-over-marks'.  Unless otherwise noted, these are from the Emacs 23+ libraries.
;;; These definitions should be IDENTICAL to what's in vanilla Emacs.


;;; Stuff from `dired.el'.

;;; The only thing modified here is the doc string, which is updated
;;; to reflect the new ARG behavior.
(defun dired-get-marked-files (&optional localp arg filter distinguish-one-marked)
  "Return names of the marked files as a list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Values returned are normally absolute file names.
Optional arg LOCALP as in `dired-get-filename'.
Optional second argument ARG specifies files to use instead of marked.
 Usually ARG comes from the command's prefix arg.
 If ARG is an integer, use the next ARG files (previous -ARG, if < 0).
 If ARG is a cons with element 16, 64, or 256, corresponding to
  `C-u C-u', `C-u C-u C-u', or `C-u C-u C-u C-u', then use all files
  in the Dired buffer, where:
    16 includes NO directories (including `.' and `..')
    64 includes directories EXCEPT `.' and `..'
   256 includes ALL directories (including `.' and `..')
 If ARG is otherwise non-nil, use the current file.
Optional third argument FILTER, if non-nil, is a function to select
 some of the files: those for which (funcall FILTER FILENAME) is
 non-nil.
If DISTINGUISH-ONE-MARKED is non-nil, then return (t FILENAME) instead
 of (FILENAME), if only one file is marked.  Do not use non-nil
 DISTINGUISH-ONE-MARKED together with FILTER."
  (let ((all-of-them  (save-excursion
                        (dired-map-over-marks
                         (dired-get-filename localp) arg nil distinguish-one-marked)))
        result)
    (if (not filter)
        (if (and distinguish-one-marked  (eq (car all-of-them) t))
            all-of-them
          (nreverse all-of-them))
      (dolist (file  all-of-them) (when (funcall filter file) (push file result)))
      result)))



;;; Stuff from `dired-aux.el'.

(defun dired-map-over-marks-check (fun arg op-symbol &optional show-progress)
  "Map FUN over marked lines and display failures.
FUN takes zero args.  It returns non-nil (the offending object, e.g.
the short form of the filename) for a failure and probably logs a
detailed error explanation using function `dired-log'.

ARG is as in `dired-map-over-marks'.

OP-SYMBOL is a symbol describing the operation performed (e.g.
`compress').  It is used with `dired-mark-pop-up' to prompt the user
\(e.g. with `Compress * [2 files]? ') and to display errors (e.g.
`Failed to compress 1 of 2 files - type W to see why (\"foo\")')

SHOW-PROGRESS if non-nil means redisplay dired after each file."
  (and (dired-mark-confirm op-symbol arg)
       (let* ((total-list               ; All of FUN's return values
               (dired-map-over-marks (funcall fun) arg show-progress))
              (total     (length total-list))
              (failures  (delq nil total-list))
              (count     (length failures))
              (string    (if (eq op-symbol 'compress)
                             "Compress or uncompress"
                           (capitalize (symbol-name op-symbol)))))
         (if (not failures)
             (message "%s: %d file%s." string total (dired-plural-s total))
           ;; end this bunch of errors:
           (dired-log-summary (format "Failed to %s %d of %d file%s"
                                      (downcase string) count total (dired-plural-s total))
                              failures)))))

(when (boundp 'dired-subdir-switches)   ; Emacs 22+
  (defun dired-do-redisplay (&optional arg test-for-subdir) ; Bound to `l'
    "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
\\<dired-mode-map>\\[dired-reset-subdir-switches].
See Info node `(emacs)Subdir switches' for more details."
    ;; Moves point if the next ARG files are redisplayed.
    (interactive "P\np")
    (if (and test-for-subdir  (dired-get-subdir))
        (let* ((dir       (dired-get-subdir))
               (switches  (cdr (assoc-string dir dired-switches-alist))))
          (dired-insert-subdir dir (and arg  (read-string "Switches for listing: "
                                                          (or switches dired-subdir-switches
                                                              dired-actual-switches)))))
      (message "Redisplaying...")
      ;; `message' much faster than making `dired-map-over-marks' show progress
      (dired-uncache (if (consp dired-directory) (car dired-directory) dired-directory))
      (dired-map-over-marks (let ((fname  (dired-get-filename))
                                  ;; Postpone readin hook map over all marked files (Bug#6810).
                                  (dired-after-readin-hook nil))
                              (message "Redisplaying... `%s'" fname)
                              (dired-update-file-line fname))
                            arg)
      (run-hooks 'dired-after-readin-hook)
      (dired-move-to-filename)
      (message "Redisplaying...done"))))

(unless (boundp 'dired-subdir-switches) ; Emacs 20, 21
  (defun dired-do-redisplay (&optional arg test-for-subdir) ; Bound to `l'
    "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing."
    ;; Moves point if the next ARG files are redisplayed.
    (interactive "P\np")
    (if (and test-for-subdir  (dired-get-subdir))
        (dired-insert-subdir (dired-get-subdir)
                             (and arg  (read-string "Switches for listing: "
                                                    dired-actual-switches)))
      (message "Redisplaying...")
      ;; message much faster than making dired-map-over-marks show progress
      (dired-uncache (if (consp dired-directory) (car dired-directory) dired-directory))
      (dired-map-over-marks (let ((fname  (dired-get-filename)))
                              (message "Redisplaying... `%s'" fname)
                              (dired-update-file-line fname))
                            arg)
      (dired-move-to-filename)
      (message "Redisplaying...done"))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Made compatible with Emacs 20, 21, which do not have [:alnum].
;; Also, this is defined here because it is used elsewhere in the file.
;;
(defun dired-switches-escape-p (switches)
  "Return non-nil if the string SWITCHES contains `-b' or `--escape'."
  ;; Do not match things like "--block-size" that happen to contain "b".
  (if (> emacs-major-version 21)
      (string-match "\\(\\`\\| \\)-[[:alnum:]]*b\\|--escape\\>" switches)
    (string-match "\\(\\`\\| \\)-\\(\w\\|[0-9]\\)*b\\|--escape\\>" switches)))


;; From `dired.el'

(when (and (> emacs-major-version 22)  (featurep 'ls-lisp+))

;;; 2012/04/26: Commented this out.
;;;             Might need it again when update `ls-lisp+.el' to fix other things.
;;;
;;;   ;; Use t as WILDCARD arg to `dired-insert-directory'.
;;;   ;;
;;;   (defun dired-readin-insert ()
;;;     ;; Insert listing for the specified dir (and maybe file  list)
;;;     ;; already in dired-directory, assuming a clean buffer.
;;;     (let (dir file-list)
;;;       (if (consp dired-directory)
;;;           (setq dir        (car dired-directory)
;;;                 file-list  (cdr dired-directory))
;;;         (setq dir        dired-directory
;;;               file-list  ()))
;;;       (setq dir  (expand-file-name dir))
;;;       (if (and (equal "" (file-name-nondirectory dir))  (not file-list))
;;;           ;; If we are reading a whole single directory...
;;;           (dired-insert-directory dir dired-actual-switches nil nil t)
;;;         (unless (file-readable-p (directory-file-name (file-name-directory dir)))
;;;           (error "Directory `%s' inaccessible or nonexistent" dir))
;;;         ;; Else treat it as a wildcard spec.
;;;         (dired-insert-directory dir dired-actual-switches file-list t t))))

  ;; Compute WILDCARD arg for `insert-directory' for individual file (don't just use nil).
  ;;
  (defun dired-insert-directory (dir switches &optional file-list wildcard hdr)
    "Insert a directory listing of DIR, Dired style.
Use SWITCHES to make the listings.
If FILE-LIST is non-nil, list only those files.
Otherwise, if WILDCARD is non-nil, expand wildcards;
 in that case, DIR should be a file name that uses wildcards.
In other cases, DIR should be a directory name or a directory filename.
If HDR is non-nil, insert a header line with the directory name."
    (let ((opoint               (point))
          (process-environment  (copy-sequence process-environment))
          end)
      (when (and
             ;; Do not try to invoke `ls' if on DOS/Windows, where `ls-lisp' is used, unless
             ;; the user really wants to use `ls', as indicated by
             ;; `ls-lisp-use-insert-directory-program'.
             (or (not (featurep 'ls-lisp))  ls-lisp-use-insert-directory-program)
             (or (if (eq dired-use-ls-dired 'unspecified)
                     ;; Check if "ls --dired" gives exit code 0.  Put it in `dired-use-ls-dired'.
                     (or (setq dired-use-ls-dired  (eq 0 (call-process insert-directory-program
                                                                       nil nil nil "--dired")))
                         (progn (message "Command `ls' does not support switch `--dired' - see \
`dired-use-ls-dired'.")
                                nil))
                   dired-use-ls-dired)
                 (file-remote-p dir)))
        (setq switches  (concat "--dired " switches)))
      ;; We used to specify the C locale here, to force English month names.  This should not be
      ;; necessary any more with the new value of `directory-listing-before-filename-regexp'.
      (if file-list
          (dolist (f  file-list)
            (let ((beg  (point)))
              ;; Compute wildcard arg for this file.
              (insert-directory f switches (string-match "[[?*]" f) nil)
              ;; Re-align fields, if necessary.
              (dired-align-file beg (point))))
        (insert-directory dir switches wildcard (not wildcard)))
      ;; Quote certain characters, unless ls quoted them for us.
      (unless (if (fboundp 'dired-switches-escape-p)
                  (dired-switches-escape-p dired-actual-switches)
                (string-match "b" dired-actual-switches))
        (save-excursion
          (setq end  (point-marker))
          (goto-char opoint)
          (while (search-forward "\\" end t)
            (replace-match (apply #'propertize "\\\\" (text-properties-at (match-beginning 0)))
                           nil t))
          (goto-char opoint)
          (while (search-forward "\^m" end t)
            (replace-match (apply #'propertize "\\015" (text-properties-at (match-beginning 0)))
                           nil t))
          (set-marker end nil))
        ;; Comment in original, from some Emacs Dev developer:
        ;;
        ;; Replace any newlines in DIR with literal "\n" for the sake of the header line.  To
        ;; disambiguate a literal "\n" in the actual dirname, we also replace "\" with "\\".
        ;; I think this should always be done, irrespective of the value of
        ;; dired-actual-switches, because:
        ;;   i) Dired does not work with an unescaped newline in the directory name used in the
        ;;      header (bug=10469#28), and
        ;;  ii) "\" is always replaced with "\\" in the listing, so doing it in the header as
        ;;      well makes things consistent.
        ;; But at present it is done only if "-b" is in ls-switches, because newlines in dirnames
        ;; are uncommon, and people may have gotten used to seeing unescaped "\" in the headers.
        ;; Note: adjust `dired-build-subdir-alist' if you change this.
        (setq dir  (replace-regexp-in-string "\\\\" "\\\\" dir nil t)
              dir  (replace-regexp-in-string "\n" "\\n" dir nil t)))
      (dired-insert-set-properties opoint (point))
      ;; If we used `--dired' and it worked, the lines are already indented.  Else indent them.
      (unless (save-excursion (goto-char opoint) (looking-at "  "))
        (let ((indent-tabs-mode  nil)) (indent-rigidly opoint (point) 2)))
      ;; Insert text at the beginning to standardize things.
      (save-excursion
        (goto-char opoint)
        (when (and (or hdr wildcard)  (not (and (looking-at "^  \\(.*\\):$")
                                                (file-name-absolute-p (match-string 1)))))
          ;; `dired-build-subdir-alist' will replace the name by its expansion, so it does not
          ;; matter whether what we insert here is fully expanded, but it should be absolute.
          (insert "  " (directory-file-name (file-name-directory dir)) ":\n"))
        (when wildcard
          ;; Insert "wildcard" line where "total" line would be for a full dir.
          (insert "  wildcard " (file-name-nondirectory dir) "\n"))))))


;;; Stuff from `image-dired.el'.

(when (fboundp 'image-dired-get-thumbnail-image) ; Emacs 22+
  (defun image-dired-dired-insert-marked-thumbs () ; Not bound
    "Insert thumbnails before file names of marked files in the dired buffer."
    (interactive)
    (dired-map-over-marks
     (let* ((image-pos   (dired-move-to-filename))
            (image-file  (dired-get-filename))
            (thumb-file  (image-dired-get-thumbnail-image image-file))
            overlay)
       ;; If image is not already added, then add it.
       (unless (delq nil (mapcar (lambda (o) (overlay-get o 'put-image))
                                 ;; Can't use (overlays-at (point)), BUG?
                                 (overlays-in (point) (1+ (point)))))
         (put-image thumb-file image-pos)
         (setq overlay  (car (delq nil (mapcar (lambda (o) (and (overlay-get o 'put-image)  o))
                                               (overlays-in (point) (1+ (point)))))))
         (overlay-put overlay 'image-file image-file)
         (overlay-put overlay 'thumb-file thumb-file)))
     nil)
    (add-hook 'dired-after-readin-hook 'image-dired-dired-after-readin-hook nil t)))
 
;;; Key Bindings.


;; Menu Bar.
;; New order is (left -> right):
;;
;;     Dir  Regexp  Mark  Multiple  Single

;; Get rid of menu bar predefined in `dired.el'.
(define-key dired-mode-map [menu-bar] nil)
;; Get rid of Edit menu bar menu to save space.
(define-key dired-mode-map [menu-bar edit] 'undefined)


;; `Single' menu.
;;
;; REPLACE ORIGINAL `Immediate' menu in `dired.el'.
;;
(defvar diredp-menu-bar-immediate-menu (make-sparse-keymap "Single"))
(define-key dired-mode-map [menu-bar immediate]
  (cons "Single" diredp-menu-bar-immediate-menu))

(define-key diredp-menu-bar-immediate-menu [diredp-describe-file]
  '(menu-item "Describe" diredp-describe-file
    :help "Describe the file or directory at cursor"))
(define-key diredp-menu-bar-immediate-menu [separator-describe] '("--")) ; ---------------------

(when (fboundp 'image-dired-dired-display-image) ; Emacs 22+
    (define-key diredp-menu-bar-immediate-menu [image-dired-dired-display-external]
      '(menu-item "Display Image Externally" image-dired-dired-display-external
                  :help "Display image in external viewer"))
    (define-key diredp-menu-bar-immediate-menu [image-dired-dired-display-image]
      '(menu-item "Display Image" image-dired-dired-display-image
                  :help "Display sized image in a separate window"))
    (define-key diredp-menu-bar-immediate-menu [separator-image] '("--"))) ; -------------------

(when (fboundp 'diredp-chown-this-file)
  (define-key diredp-menu-bar-immediate-menu [chown]
    '(menu-item "Change Owner..." diredp-chown-this-file
      :help "Change the owner of file at cursor")))
(when (fboundp 'diredp-chgrp-this-file)
  (define-key diredp-menu-bar-immediate-menu [chgrp]
    '(menu-item "Change Group..." diredp-chgrp-this-file
      :help "Change the group of file at cursor")))
(define-key diredp-menu-bar-immediate-menu [chmod]
  '(menu-item "Change Mode..." diredp-chmod-this-file
    :help "Change mode (attributes) of file at cursor"))
(when (fboundp 'dired-do-touch)         ; Emacs 22+
  (define-key diredp-menu-bar-immediate-menu [touch]
    '(menu-item "Change Timestamp (`touch')..." diredp-touch-this-file
      :help "Change the timestamp of file at cursor, using `touch'")))
(define-key diredp-menu-bar-immediate-menu [separator-change] '("--")) ; -----------------------

(define-key diredp-menu-bar-immediate-menu [load]
  '(menu-item "Load" diredp-load-this-file
    :help "Load this Emacs Lisp file"))
(define-key diredp-menu-bar-immediate-menu [compile]
  '(menu-item "Byte Compile" diredp-byte-compile-this-file
    :help "Byte-compile this Emacs Lisp file"))
(define-key diredp-menu-bar-immediate-menu [command]
  '(menu-item "Shell Command..." diredp-shell-command-this-file
    :help "Run a shell command on file at cursor"))
(define-key diredp-menu-bar-immediate-menu [compress]
  '(menu-item "Compress/Uncompress" diredp-compress-this-file
    :help "Compress/uncompress file at cursor"))
(define-key diredp-menu-bar-immediate-menu [grep]
  '(menu-item "Grep..." diredp-grep-this-file :help "Grep file at cursor"))
(define-key diredp-menu-bar-immediate-menu [print]
  '(menu-item "Print..." diredp-print-this-file
    :help "Print file at cursor, supplying print command"))

(when (fboundp 'mkhtml-dired-files)
  (define-key diredp-menu-bar-immediate-menu [mkhtml-dired-files]
    '(menu-item "Create HTML" mkhtml-dired-files
      :help "Create an HTML file corresponding to file at cursor")))
(define-key diredp-menu-bar-immediate-menu [separator-misc] '("--")) ; -------------------------

(define-key diredp-menu-bar-immediate-menu [hardlink]
  '(menu-item "Hardlink to..." diredp-hardlink-this-file
    :help "Make hard links for current or marked files"))
(if (not (fboundp 'dired-do-relsymlink))
    (define-key diredp-menu-bar-immediate-menu [symlink]
      '(menu-item "Symlink to..." diredp-symlink-this-file
        :visible (fboundp 'make-symbolic-link) :help "Make symbolic link for file at cursor"))
  (define-key diredp-menu-bar-immediate-menu [symlink]
    '(menu-item "Symlink to (Absolute)..." diredp-symlink-this-file
      :help "Make absolute symbolic link for file at cursor"))
  (define-key diredp-menu-bar-immediate-menu [relsymlink]
    '(menu-item "Symlink to (Relative)..." diredp-relsymlink-this-file ; In `dired-x.el'.
      :help "Make relative symbolic link for file at cursor")))
(define-key diredp-menu-bar-immediate-menu [separator-link] '("--")) ; -------------------------

(define-key diredp-menu-bar-immediate-menu [delete]
  '(menu-item "Delete" diredp-delete-this-file :help "Delete file at cursor"))
(define-key diredp-menu-bar-immediate-menu [separator-delete] '("--")) ; -----------------------

(define-key diredp-menu-bar-immediate-menu [downcase]
  '(menu-item "Downcase" diredp-downcase-this-file
    ;; When running on plain MS-DOS, there is only one letter-case for file names.
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename file at cursor to a lower-case name"))
(define-key diredp-menu-bar-immediate-menu [upcase]
  '(menu-item "Upcase" diredp-upcase-this-file
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename file at cursor to an upper-case name"))
(define-key diredp-menu-bar-immediate-menu [capitalize]
  '(menu-item "Capitalize" diredp-capitalize-this-file
    :help "Capitalize (initial caps) name of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [rename]
  '(menu-item "Rename to..." diredp-rename-this-file :help "Rename file at cursor"))
(define-key diredp-menu-bar-immediate-menu [copy]
  '(menu-item "Copy to..." diredp-copy-this-file :help "Copy file at cursor"))
(define-key diredp-menu-bar-immediate-menu [separator-rename] '("--")) ; -----------------------

(define-key diredp-menu-bar-immediate-menu [backup-diff]
  '(menu-item "Diff with Backup" dired-backup-diff
    :help "Diff file at cursor with its latest backup"))
(define-key diredp-menu-bar-immediate-menu [diff]
  '(menu-item "Diff..." dired-diff
    :help "Compare file at cursor with another file using `diff'"))
(define-key diredp-menu-bar-immediate-menu [ediff]
  '(menu-item "Compare..." diredp-ediff :help "Compare file at cursor with another file"))
(define-key diredp-menu-bar-immediate-menu [separator-diff] '("--")) ; -------------------------

(define-key diredp-menu-bar-immediate-menu [dired-kill-subdir]
  '(menu-item "Remove This Inserted Subdir" dired-kill-subdir
    :visible (not (equal (dired-current-directory) default-directory)))) ; In subdir, not top.
(define-key diredp-menu-bar-immediate-menu [diredp-dired-this-subdir]
  '(menu-item "Dired This Inserted Subdir (Tear Off)"
    (lambda () (interactive) (diredp-dired-this-subdir t))
    :visible (and (cdr dired-subdir-alist) ; First is current dir.  Must have at least one more.
              (not (equal (dired-current-directory) default-directory))) ; Must be sub, not top.
    :help "Open Dired for subdir at or above point, tearing it off if inserted"))
(define-key diredp-menu-bar-immediate-menu [insert-subdir]
  '(menu-item "Insert This Subdir" dired-maybe-insert-subdir
    :visible (and (atom (diredp-this-subdir))
              (not (assoc (file-name-as-directory (diredp-this-subdir)) dired-subdir-alist)))
    :enable (atom (diredp-this-subdir))
    :help "Insert a listing of this subdirectory"))
(define-key diredp-menu-bar-immediate-menu [goto-subdir]
  '(menu-item "Go To Inserted Subdir" dired-maybe-insert-subdir
    :visible (and (atom (diredp-this-subdir))
              (assoc (file-name-as-directory (diredp-this-subdir)) dired-subdir-alist))
    :enable (atom (diredp-this-subdir))
    :help "Go to the inserted listing of this subdirectory"))
(define-key diredp-menu-bar-immediate-menu [separator-subdir] '("--" ; ------------------------
            :visible (or (atom (diredp-this-subdir)) ; Subdir line.
                         (not (equal (dired-current-directory) default-directory))))) ; Not top.

(define-key diredp-menu-bar-immediate-menu [view]
  '(menu-item "View (Read Only)" dired-view-file
    :help "Examine file at cursor in read-only mode"))
(define-key diredp-menu-bar-immediate-menu [display]
  '(menu-item "Display in Other Window" dired-display-file
    :help "Display file at cursor in a different window"))
;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(progn
    (define-key diredp-menu-bar-immediate-menu [dired-w32-browser]
      '(menu-item "Open Associated Windows App" dired-w32-browser
        :help "Open file using the Windows app associated with its file type"))
    (define-key diredp-menu-bar-immediate-menu [dired-w32explore]
      '(menu-item "Open in Windows Explorer" dired-w32explore
        :help "Open file in Windows Explorer"))))
(define-key diredp-menu-bar-immediate-menu [find-file-other-frame]
  '(menu-item "Open in Other Frame" diredp-find-file-other-frame
    :help "Edit file at cursor in a different frame"))
(define-key diredp-menu-bar-immediate-menu [find-file-other-window]
  '(menu-item "Open in Other Window" dired-find-file-other-window
    :help "Edit file at cursor in a different window"))
(define-key diredp-menu-bar-immediate-menu [find-file]
  '(menu-item "Open" dired-find-file :help "Edit file at cursor"))

;; `Single' > `Bookmarks' menu.
;;
(when (require 'bookmark+ nil t)
  (defvar diredp-menu-bar-immediate-bookmarks-menu (make-sparse-keymap "Bookmarks"))
  (define-key diredp-menu-bar-immediate-menu [bookmark]
    (cons "Bookmarks" diredp-menu-bar-immediate-bookmarks-menu))

  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-set-tag-value-this-file]
    '(menu-item "Set Tag Value..." diredp-set-tag-value-this-file
      :help "Set the value (not the name) of a given tag for this file"))
  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-paste-replace-tags-this-file]
    '(menu-item "Paste Tags (Replace)" diredp-paste-replace-tags-this-file
      :help "Replace tags for this file with previously copied tags"))
  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-paste-add-tags-this-file]
    '(menu-item "Paste Tags (Add)" diredp-paste-add-tags-this-file
      :help "Add previously copied tags to this file"))
  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-copy-tags-this-file]
    '(menu-item "Copy Tags" diredp-copy-tags-this-file
      :help "Copy the tags from this file, so you can paste them to another"))
  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-remove-all-tags-this-file]
    '(menu-item "Remove All Tags" diredp-remove-all-tags-this-file
      :help "Remove all tags from the file at cursor"))
  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-untag-this-file]
    '(menu-item "Remove Tags..." diredp-untag-this-file
      :help "Remove some tags from the file at cursor (`C-u': remove all tags)"))
  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-tag-this-file]
    '(menu-item "Add Tags..." diredp-tag-this-file :help "Add some tags to the file at cursor")))
(when (require 'bookmark+ nil t)
  (define-key diredp-menu-bar-immediate-bookmarks-menu [diredp-bookmark-this-file]
    '(menu-item "Bookmark..." diredp-bookmark-this-file :help "Bookmark the file at cursor")))


;; `Multiple' menu.
;;
;; REPLACE ORIGINAL "Operate" menu in `dired.el'.
;;
(defvar diredp-menu-bar-operate-menu (make-sparse-keymap "Multiple"))
(define-key dired-mode-map [menu-bar operate]
  (cons "Multiple" diredp-menu-bar-operate-menu))

(when (fboundp 'epa-dired-do-decrypt)   ; Emacs 23+
  (define-key diredp-menu-bar-operate-menu [epa-dired-do-decrypt]
    '(menu-item "Decrypt" epa-dired-do-decrypt :help "Decrypt file at cursor"))
  (define-key diredp-menu-bar-operate-menu [epa-dired-do-verify]
    '(menu-item "Verify" epa-dired-do-verify
      :help "Verify digital signature of file at cursor"))
  (define-key diredp-menu-bar-operate-menu [epa-dired-do-sign]
    '(menu-item "Sign" epa-dired-do-sign :help "Create digital signature of file at cursor"))
  (define-key diredp-menu-bar-operate-menu [epa-dired-do-encrypt]
    '(menu-item "Encrypt" epa-dired-do-encrypt :help "Encrypt file at cursor"))
  (define-key diredp-menu-bar-operate-menu [separator-encryption] '("--"))) ; ------------------

(when (fboundp 'image-dired-delete-tag) ; Emacs 22+
  (define-key diredp-menu-bar-operate-menu [image-dired-delete-tag]
    '(menu-item "Delete Image Tag..." image-dired-delete-tag
      :help "Delete image tag from marked files"))
  (define-key diredp-menu-bar-operate-menu [image-dired-tag-files]
    '(menu-item "Add Image Tags..." image-dired-tag-files
      :help "Add image tags to marked files"))
  (define-key diredp-menu-bar-operate-menu [image-dired-dired-comment-files]
    '(menu-item "Add Image Comment..." image-dired-dired-comment-files
      :help "Add image comment to marked files"))
  (define-key diredp-menu-bar-operate-menu [image-dired-display-thumbs]
    '(menu-item "Display Image Thumbnails" image-dired-display-thumbs
      :help "Display image thumbnails for marked image files"))
  (define-key diredp-menu-bar-operate-menu [separator-image] '("--"))) ; -----------------------

(unless (memq system-type '(windows-nt ms-dos))
  (define-key diredp-menu-bar-operate-menu [chown]
    '(menu-item "Change Owner..." dired-do-chown
      :help "Change the owner of marked files")))
(unless (memq system-type '(windows-nt ms-dos))
  (define-key diredp-menu-bar-operate-menu [chgrp]
    '(menu-item "Change Group..." dired-do-chgrp
      :help "Change the owner of marked files")))
(define-key diredp-menu-bar-operate-menu [chmod]
  '(menu-item "Change Mode..." dired-do-chmod
    :help "Change mode (attributes) of marked files"))
(when (fboundp 'dired-do-touch)         ; Emacs 22+
  (define-key diredp-menu-bar-operate-menu [touch]
    '(menu-item "Change Timestamp (`touch')..." dired-do-touch
      :help "Change the timestamp of the marked files, using `touch'")))
(define-key diredp-menu-bar-operate-menu [separator-change] '("--")) ; -------------------------

(when (fboundp 'dired-do-isearch-regexp) ; Emacs 23+
  (define-key diredp-menu-bar-operate-menu [isearch-regexp]
    '(menu-item "Isearch Regexp Files..." dired-do-isearch-regexp
      :help "Incrementally search marked files for regexp"))
  (define-key diredp-menu-bar-operate-menu [isearch]
    '(menu-item "Isearch Files..." dired-do-isearch
      :help "Incrementally search marked files for string")))
(define-key diredp-menu-bar-operate-menu [query-replace]
  (if (< emacs-major-version 21)
      '(menu-item "Query Replace..." dired-do-query-replace)
    '(menu-item "Query Replace..." dired-do-query-replace-regexp
      :help "Replace regexp in marked files")))
(define-key diredp-menu-bar-operate-menu [search]
  '(menu-item "Search Files..." dired-do-search :help "Search marked files for regexp"))
(define-key diredp-menu-bar-operate-menu [grep]
  '(menu-item "Grep..." diredp-do-grep :help "Grep marked, next N, or all files shown"))
(define-key diredp-menu-bar-operate-menu [separator-search] '("--")) ; -------------------------

(define-key diredp-menu-bar-operate-menu [load]
  '(menu-item "Load" dired-do-load :help "Load marked Emacs Lisp files"))
(define-key diredp-menu-bar-operate-menu [compile]
  '(menu-item "Byte Compile" dired-do-byte-compile :help "Byte-compile marked Emacs Lisp files"))
(define-key diredp-menu-bar-operate-menu [command]
  '(menu-item "Shell Command..." dired-do-shell-command
    :help "Run a shell command on each of marked files"))
(define-key diredp-menu-bar-operate-menu [compress]
  '(menu-item "Compress/Uncompress" dired-do-compress :help "Compress/uncompress marked files"))
(define-key diredp-menu-bar-operate-menu [print]
  '(menu-item "Print..." dired-do-print :help "Print marked files, supplying print command"))

(unless (require 'bookmark+ nil t)
  (define-key diredp-menu-bar-immediate-menu [diredp-bookmark-this-file]
    '(menu-item "Bookmark..." diredp-bookmark-this-file :help "Bookmark the file at cursor")))
(when (fboundp 'mkhtml-dired-files)
  (define-key diredp-menu-bar-operate-menu [mkhtml-dired-files]
    '(menu-item "Create HTML" mkhtml-dired-files
      :help "Create HTML files corresponding to marked files")))
(define-key diredp-menu-bar-operate-menu [separator-misc] '("--")) ; ---------------------------

(define-key diredp-menu-bar-operate-menu [hardlink]
  '(menu-item "Hardlink to..." dired-do-hardlink
    :help "Make hard links for current or marked files"))
(if (not (fboundp 'dired-do-relsymlink))
    (define-key diredp-menu-bar-operate-menu [symlink]
      '(menu-item "Symlink to..." dired-do-symlink
        :visible (fboundp 'make-symbolic-link)
        :help "Make symbolic links for current or marked files"))
  (define-key diredp-menu-bar-operate-menu [symlink]
    '(menu-item "Symlink to (Absolute)..." dired-do-symlink
      :help "Make absolute symbolic links for current or marked files"))
  (define-key diredp-menu-bar-operate-menu [relsymlink] ; In `dired-x.el'.
    '(menu-item "Symlink to (Relative)..." dired-do-relsymlink
      :help "Make relative symbolic links for current or marked files")))
(define-key diredp-menu-bar-operate-menu [separator-link] '("--")) ; ---------------------------

(define-key diredp-menu-bar-operate-menu [delete-flagged]
  '(menu-item "Delete Flagged" dired-do-flagged-delete
    :help "Delete all files flagged for deletion (D)"))
(define-key diredp-menu-bar-operate-menu [delete]
  '(menu-item "Delete Marked (not Flagged)" dired-do-delete
    :help "Delete current file or all marked files (not flagged files)"))
(define-key diredp-menu-bar-operate-menu [separator-delete] '("--")) ; -------------------------

(define-key diredp-menu-bar-operate-menu [downcase]
  '(menu-item "Downcase" dired-downcase
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename marked files to lowercase names"))
(define-key diredp-menu-bar-operate-menu [upcase]
  '(menu-item "Upcase" dired-upcase
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename marked files to uppercase names"))
(define-key diredp-menu-bar-operate-menu [capitalize]
  '(menu-item "Capitalize" diredp-capitalize
    :help "Capitalize (initial caps) the names of all marked files"))
(define-key diredp-menu-bar-operate-menu [rename]
  '(menu-item "Rename to..." dired-do-rename :help "Rename current file or move marked files"))
(define-key diredp-menu-bar-operate-menu [copy]
  '(menu-item "Copy to..." dired-do-copy :help "Copy current file or all marked files"))
(define-key diredp-menu-bar-operate-menu [separator-rename] '("--")) ; -------------------------

(when (fboundp 'dired-copy-filename-as-kill)
  (define-key diredp-menu-bar-operate-menu [kill-ring]
    '(menu-item "Copy File Names (to Paste)" dired-copy-filename-as-kill
      :help "Copy names of marked files onto kill ring, for pasting")))
(define-key diredp-menu-bar-operate-menu [diredp-insert-subdirs]
  '(menu-item "Insert Subdirs" diredp-insert-subdirs
    :help "Insert the marked subdirectories - like using `i' at each marked dir"))
(define-key diredp-menu-bar-operate-menu [diredp-marked-other-window]
  '(menu-item "Dired (Marked) in Other Window" diredp-marked-other-window
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files only, in other window"))
(define-key diredp-menu-bar-operate-menu [diredp-marked]
  '(menu-item "Dired (Marked)" diredp-marked
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files only"))
;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(define-key diredp-menu-bar-operate-menu [dired-multiple-w32-browser]
      '(menu-item "Open Associated Windows Apps" dired-multiple-w32-browser
        :help "Open files using the Windows apps associated with their file types")))
(when (fboundp 'dired-do-find-marked-files)
  (define-key diredp-menu-bar-operate-menu [find-files]
    '(menu-item "Open" dired-do-find-marked-files ; In `dired-x.el'.
      :help "Open each marked file for editing")))

(defvar diredp-menu-bar-recursive-marked-menu (make-sparse-keymap "Marked Here and Below"))
(define-key diredp-menu-bar-operate-menu [recursive-marked]
  (cons "Marked Here and Below" diredp-menu-bar-recursive-marked-menu))

(when (> emacs-major-version 22) ; Emacs 23+
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-decrypt-recursive]
    '(menu-item "Decrypt..." diredp-do-decrypt-recursive
      :help "Decrypt marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-verify-recursive]
    '(menu-item "Verify" diredp-do-verify-recursive
      :help "Verify marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-sign-recursive]
    '(menu-item "Sign..." diredp-do-sign-recursive
      :help "Sign marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-encrypt-recursive]
    '(menu-item "Encrypt..." diredp-do-encrypt-recursive
      :help "Encrypt marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [separator-crypt] '("--"))) ; --------------

(when (fboundp 'image-dired-delete-tag) ; Emacs 22+
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-image-dired-delete-tag-recursive]
    '(menu-item "Delete Image Tag..." diredp-image-dired-delete-tag-recursive
      :help "Delete image tag from marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-image-dired-tag-files-recursive]
    '(menu-item "Add Image Tags..." diredp-image-dired-tag-files-recursive
      :help "Add image tags to marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-image-dired-comment-files-recursive]
    '(menu-item "Add Image Comment..." diredp-image-dired-comment-files-recursive
      :help "Add image comment to marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-image-dired-display-thumbs-recursive]
    '(menu-item "Display Image Thumbnails" diredp-image-dired-display-thumbs-recursive
      :help "Show thumbnails for marked image files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [separator-image] '("--"))) ; --------------

(when (fboundp 'diredp-do-chown-recursive)
  (define-key diredp-menu-bar-recursive-marked-menu [chown]
    '(menu-item "Change Owner..." diredp-do-chown-recursive
      :help "Change the owner of marked files, including those in marked subdirs")))
(when (fboundp 'diredp-do-chgrp-recursive)
  (define-key diredp-menu-bar-recursive-marked-menu [chgrp]
    '(menu-item "Change Group..." diredp-do-chgrp-recursive
      :help "Change the owner of marked files, including those in marked subdirs")))
(define-key diredp-menu-bar-recursive-marked-menu [chmod]
  '(menu-item "Change Mode..." diredp-do-chmod-recursive
    :help "Change mode (attributes) of marked files, including those in marked subdirs"))
(when (fboundp 'dired-do-touch)         ; Emacs 22+
  (define-key diredp-menu-bar-recursive-marked-menu [touch]
    '(menu-item "Change Timestamp (`touch')..." diredp-do-touch-recursive
      :help "Change timestamp of marked files, including those in marked subdirs")))
(define-key diredp-menu-bar-recursive-marked-menu [separator-change] '("--")) ; ----------------

(when (fboundp 'dired-do-isearch-regexp) ; Emacs 23+
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-isearch-regexp-recursive]
    '(menu-item "Isearch Regexp Files..." diredp-do-isearch-regexp-recursive
      :help "Incrementally regexp search marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-isearch-recursive]
    '(menu-item "Isearch Files..." diredp-do-isearch-recursive
      :help "Incrementally search marked files, including those in marked subdirs")))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-query-replace-regexp-recursive]
  '(menu-item "Query Replace..." diredp-do-query-replace-regexp-recursive
    :help "Replace regexp in marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-search-recursive]
  '(menu-item "Search Files..." diredp-do-search-recursive
    :help "Regexp search marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-grep-recursive]
    '(menu-item "Grep..." diredp-do-grep-recursive
      :help "Run `grep' on the marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [separator-search] '("--")) ; ----------------

(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-shell-command-recursive]
    '(menu-item "Shell Command..." diredp-do-shell-command-recursive
      :help "Run shell command on the marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-print-recursive]
    '(menu-item "Print..." diredp-do-print-recursive
      :help "Print the marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [separator-misc] '("--")) ; ----------------

(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-hardlink-recursive]
  '(menu-item "Hardlink to..." diredp-do-hardlink-recursive
    :help "Make hard links for marked files, including those in marked subdirs"))
(if (not (fboundp 'dired-do-relsymlink))
    (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-symlink-recursive]
      '(menu-item "Symlink to..." diredp-do-symlink-recursive
        :visible (fboundp 'make-symbolic-link)
        :help "Make symbolic links for marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-symlink-recursive]
    '(menu-item "Symlink to (Absolute)..." diredp-do-symlink-recursive
      :help "Make absolute symbolic links for marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-recursive-marked-menu [diredp-do-relsymlink-recursive]
    '(menu-item "Symlink to (Relative)..." diredp-do-relsymlink-recursive
      :help "Make relative symbolic links for marked files, including those in marked subdirs")))
(define-key diredp-menu-bar-recursive-marked-menu [separator-link] '("--")) ; ------------------

(define-key diredp-menu-bar-recursive-marked-menu [diredp-capitalize-recursive]
  '(menu-item "Capitalize" diredp-capitalize-recursive
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Capitalize (initial caps) the names of all marked files"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-downcase-recursive]
  '(menu-item "Downcase" diredp-downcase-recursive
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename marked files to lowercase names"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-upcase-recursive]
  '(menu-item "Upcase" diredp-upcase-recursive
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename marked files to uppercase names"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-move-recursive]
    '(menu-item "Move to..." diredp-do-move-recursive
      :help "Move marked files, including in marked subdirs, to a given directory"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-copy-recursive]
    '(menu-item "Copy to..." diredp-do-copy-recursive
      :help "Copy marked files, including in marked subdirs, to a given directory"))
(define-key diredp-menu-bar-recursive-marked-menu [separator-rename] '("--")) ; ----------------

(define-key diredp-menu-bar-recursive-marked-menu [diredp-list-marked-recursive]
    '(menu-item "List Marked Files" diredp-list-marked-recursive
      :help "List the files marked here and in marked subdirs, recursively"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-copy-filename-as-kill-recursive]
    '(menu-item "Copy File Names (to Paste)" diredp-copy-filename-as-kill-recursive
      :help "Copy names of marked files here and in marked subdirs, to `kill-ring'"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-insert-subdirs-recursive]
  '(menu-item "Insert Subdirs" diredp-insert-subdirs-recursive
    :help "Insert the marked subdirectories, gathered recursively"))
(define-key diredp-menu-bar-recursive-marked-menu [separator-dirs] '("--")) ; ------------------

(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-bookmark-in-bookmark-file-recursive]
    '(menu-item "Bookmark in Bookmark File" diredp-do-bookmark-in-bookmark-file-recursive
      :help "Bookmark marked files, including those in marked subdirs, in a bookmark file"))
(define-key diredp-menu-bar-recursive-marked-menu
    [diredp-set-bookmark-file-bookmark-for-marked-recursive]
  '(menu-item "Create Bookmark-File Bookmark"
    diredp-set-bookmark-file-bookmark-for-marked-recursive
    :help "Create a bookmark-file bookmark for marked files, including in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-bookmark-recursive]
    '(menu-item "Bookmark" diredp-do-bookmark-recursive
      :help "Bookmark the marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [separator-bookmark] '("--")) ; --------------

(define-key diredp-menu-bar-recursive-marked-menu [diredp-marked-recursive-other-window]
    '(menu-item "Dired (Marked) in Other Window" diredp-marked-recursive-other-window
      :help "Open Dired (in other window) on marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-marked-recursive]
    '(menu-item "Dired (Marked)" diredp-marked-recursive
      :help "Open Dired on marked files, including those in marked subdirs"))
;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(define-key diredp-menu-bar-recursive-marked-menu [diredp-multiple-w32-browser-recursive]
    '(menu-item "Open Associated Windows Apps" diredp-multiple-w32-browser-recursive
      :help "Run Windows apps for with marked files, including those in marked subdirs")))
(define-key diredp-menu-bar-recursive-marked-menu [diredp-do-find-marked-files-recursive]
    '(menu-item "Open" diredp-do-find-marked-files-recursive
      :help "Find marked files simultaneously, including those in marked subdirs"))


;; `Multiple' > `Bookmarks' menu.
;;
(defvar diredp-menu-bar-operate-bookmarks-menu (make-sparse-keymap "Bookmarks"))
(define-key diredp-menu-bar-operate-menu [bookmark]
  (cons "Bookmarks" diredp-menu-bar-operate-bookmarks-menu))

(when (require 'bookmark+ nil t)
  (define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-set-tag-value]
    '(menu-item "Set Tag Value..." diredp-do-set-tag-value
      :help "Set the value of a given tag for the marked or next N files"))
  (define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-paste-replace-tags]
    '(menu-item "Paste Tags (Replace)" diredp-do-paste-replace-tags
      :help "Replace tags for the marked or next N files with copied tags"))
  (define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-paste-add-tags]
    '(menu-item "Paste Tags (Add)" diredp-do-paste-add-tags
      :help "Add previously copied tags to the marked or next N files"))
  (define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-remove-all-tags]
    '(menu-item "Remove All Tags" diredp-do-remove-all-tags
      :help "Remove all tags from the marked or next N files"))
  (define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-untag]
    '(menu-item "Remove Tags..." diredp-do-untag
      :help "Remove some tags from the marked or next N files"))
  (define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-tag]
    '(menu-item "Add Tags..." diredp-do-tag
      :help "Add some tags to the marked or next N files"))
  (define-key diredp-menu-bar-operate-bookmarks-menu [separator-book-2] '("--"))) ; ------------

(define-key diredp-menu-bar-operate-bookmarks-menu
    [diredp-do-bookmark-in-bookmark-file-recursive]
  '(menu-item "Bookmark in Bookmark File (Here and Below)..."
    diredp-do-bookmark-in-bookmark-file-recursive
    :help "Bookmark marked files (including in marked subdirs) in bookmark file and save it"))
(define-key diredp-menu-bar-operate-bookmarks-menu
    [diredp-set-bookmark-file-bookmark-for-marked-recursive]
  '(menu-item "Create Bookmark-File Bookmark (Here and Below)..."
    diredp-set-bookmark-file-bookmark-for-marked-recursive
    :help "Create a bookmark-file bookmark for marked files, including in marked subdirs"))
(define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-bookmark-recursive]
  '(menu-item "Bookmark (Here and Below)..." diredp-do-bookmark-recursive
    :help "Bookmark the marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-operate-bookmarks-menu [separator-book-1] '("--")) ; ---------------

(define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-bookmark-in-bookmark-file]
  '(menu-item "Bookmark in Bookmark File..." diredp-do-bookmark-in-bookmark-file
    :help "Bookmark the marked files in BOOKMARK-FILE and save BOOKMARK-FILE"))
(define-key diredp-menu-bar-operate-bookmarks-menu [diredp-set-bookmark-file-bookmark-for-marked]
  '(menu-item "Create Bookmark-File Bookmark..." diredp-set-bookmark-file-bookmark-for-marked
    :help "Create a bookmark-file bookmark, and bookmark the marked files in it"))
(define-key diredp-menu-bar-operate-bookmarks-menu [diredp-do-bookmark]
  '(menu-item "Bookmark..." diredp-do-bookmark :help "Bookmark the marked or next N files"))


;; `Regexp' menu.
;;
;; REPLACE ORIGINAL `Regexp' menu in `dired.el'.
;;
(defvar diredp-menu-bar-regexp-menu (make-sparse-keymap "Regexp"))
(define-key dired-mode-map [menu-bar regexp]
  (cons "Regexp" diredp-menu-bar-regexp-menu))
(define-key diredp-menu-bar-regexp-menu [hardlink]
  '(menu-item "Hardlink to..." dired-do-hardlink-regexp
    :help "Make hard links for files matching regexp"))
(if (not (fboundp 'dired-do-relsymlink-regexp))
    (define-key diredp-menu-bar-regexp-menu [symlink]
      '(menu-item "Symlink to..." dired-do-symlink-regexp
        :visible (fboundp 'make-symbolic-link)
        :help "Make symbolic links for files matching regexp"))
  (define-key diredp-menu-bar-regexp-menu [symlink]
    '(menu-item "Symlink to (Absolute)..." dired-do-symlink-regexp
      :visible (fboundp 'make-symbolic-link)
      :help "Make absolute symbolic links for files matching regexp"))
  (define-key diredp-menu-bar-regexp-menu [relsymlink] ; In `dired-x.el'.
    '(menu-item "Symlink to (Relative)..." dired-do-relsymlink-regexp
      :visible (fboundp 'make-symbolic-link)
      :help "Make relative symbolic links for files matching regexp")))
(define-key diredp-menu-bar-regexp-menu [rename]
  '(menu-item "Rename to..." dired-do-rename-regexp :help "Rename marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [copy]
  '(menu-item "Copy to..." dired-do-copy-regexp :help "Copy marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [flag]
  '(menu-item "Flag..." dired-flag-files-regexp :help "Flag files matching regexp for deletion"))
(define-key diredp-menu-bar-regexp-menu [mark]
  '(menu-item "Mark..." dired-mark-files-regexp
    :help "Mark files matching regexp for future operations"))
(define-key diredp-menu-bar-regexp-menu [mark-cont]
  '(menu-item "Mark Containing..." dired-mark-files-containing-regexp
    :help "Mark files whose contents matches regexp"))


;; "Mark" menu.
;;
;; REPLACE ORIGINAL "Mark" menu in `dired.el'.
;;
(defvar diredp-menu-bar-mark-menu (make-sparse-keymap "Mark"))
(define-key dired-mode-map [menu-bar mark] (cons "Mark" diredp-menu-bar-mark-menu))

(when (fboundp 'dired-flag-extension)
  (define-key diredp-menu-bar-mark-menu [flag-extension] ; In `dired-x.el'
    '(menu-item "Flag Extension..." dired-flag-extension
      :help "Flag all files that have a certain extension, for deletion")))
(define-key diredp-menu-bar-mark-menu [garbage-files]
  '(menu-item "Flag Garbage Files" dired-flag-garbage-files
    :help "Flag unneeded files for deletion"))
(define-key diredp-menu-bar-mark-menu [backup-files]
  '(menu-item "Flag Backup Files" dired-flag-backup-files
    :help "Flag all backup files for deletion"))
(define-key diredp-menu-bar-mark-menu [auto-save-files]
  '(menu-item "Flag Auto-save Files" dired-flag-auto-save-files
    :help "Flag auto-save files for deletion"))
(define-key diredp-menu-bar-mark-menu [flag-region]
  '(menu-item "Flag Region" diredp-flag-region-files-for-deletion
    :enable (and transient-mark-mode  mark-active  (not (eq (mark) (point))))
    :help "Flag all files in the region (selection) for deletion"))
(when (< emacs-major-version 21)
  (put 'diredp-flag-region-files-for-deletion
       'menu-enable '(and transient-mark-mode  mark-active  (not (eq (mark) (point))))))
(define-key diredp-menu-bar-mark-menu [deletion]
  '(menu-item "Flag" dired-flag-file-deletion :help "Flag current line's file for deletion"))
(define-key diredp-menu-bar-mark-menu [separator-flag] '("--")) ; ------------------------------

(define-key diredp-menu-bar-mark-menu [prev]
  '(menu-item "Previous Marked" dired-prev-marked-file :help "Move to previous marked file"))
(define-key diredp-menu-bar-mark-menu [next]
  '(menu-item "Next Marked" dired-next-marked-file :help "Move to next marked file"))
(define-key diredp-menu-bar-mark-menu [marks]
  '(menu-item "Change Marks..." dired-change-marks
    :help "Replace marker with another character"))
(define-key diredp-menu-bar-mark-menu [omit-unmarked]
  '(menu-item "Omit Unmarked" diredp-omit-unmarked :help "Hide lines of unmarked files"))
(define-key diredp-menu-bar-mark-menu [omit-marked]
  '(menu-item "Omit Marked" diredp-omit-marked :help "Hide lines of marked files"))
(define-key diredp-menu-bar-mark-menu [toggle-marks]
  (if (> emacs-major-version 21)
      '(menu-item "Toggle Marked/Unmarked" dired-toggle-marks
        :help "Mark unmarked files, unmark marked ones")
    '(menu-item "Toggle Marked/Unmarked" dired-do-toggle
      :help "Mark unmarked files, unmark marked ones")))
(define-key diredp-menu-bar-mark-menu [separator-mark] '("--")) ; ------------------------------

(when (fboundp 'dired-mark-sexp)
  (define-key diredp-menu-bar-mark-menu [mark-sexp] ; In `dired-x.el'.
    '(menu-item "Mark If..." dired-mark-sexp
      :help "Mark files for which specified condition is true")))
(define-key diredp-menu-bar-mark-menu [mark-extension]
  '(menu-item "Mark Extension..." diredp-mark/unmark-extension
    :help "Mark all files with specified extension"))
(define-key diredp-menu-bar-mark-menu [symlinks]
  '(menu-item "Mark Symlinks" dired-mark-symlinks
    :visible (fboundp 'make-symbolic-link) :help "Mark all symbolic links"))
(define-key diredp-menu-bar-mark-menu [directories]
  '(menu-item "Mark Directories" dired-mark-directories
    :help "Mark all directories except `.' and `..'"))
(define-key diredp-menu-bar-mark-menu [directory]
  '(menu-item "Mark Old Backups" dired-clean-directory
    :help "Flag old numbered backups for deletion"))
(define-key diredp-menu-bar-mark-menu [executables]
  '(menu-item "Mark Executables" dired-mark-executables :help "Mark all executable files"))
(define-key diredp-menu-bar-mark-menu [mark-region]
  '(menu-item "Mark Region" diredp-mark-region-files
    :enable (and transient-mark-mode  mark-active  (not (eq (mark) (point))))
    :help "Mark all of the files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-mark-region-files
       'menu-enable '(and transient-mark-mode  mark-active  (not (eq (mark) (point))))))
(define-key diredp-menu-bar-mark-menu [mark]
  '(menu-item "Mark" dired-mark :help "Mark current line's file for future operations"))
(define-key diredp-menu-bar-mark-menu [separator-unmark] '("--")) ; ----------------------------

(define-key diredp-menu-bar-mark-menu [unmark-all]
  '(menu-item "Unmark All" dired-unmark-all-marks :help "Remove all marks from all files"))
(define-key diredp-menu-bar-mark-menu [unmark-with]
  '(menu-item "Unmark Marked-With..." dired-unmark-all-files
    :help "Remove a specific mark (or all marks) from every file"))
(define-key diredp-menu-bar-mark-menu [unmark-region]
  '(menu-item "Unmark Region" diredp-unmark-region-files
    :enable (and transient-mark-mode  mark-active  (not (eq (mark) (point))))
    :help "Unmark all files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-unmark-region-files
       'menu-enable '(and transient-mark-mode  mark-active  (not (eq (mark) (point))))))
(define-key diredp-menu-bar-mark-menu [unmark]
  '(menu-item "Unmark" dired-unmark :help "Unmark or unflag current line's file"))

(when (require 'bookmark+ nil t)
  (defvar diredp-mark-tags-menu (make-sparse-keymap "Tagged")
    "`Tags' submenu for Dired menu-bar `Mark' menu.")
  (define-key diredp-menu-bar-mark-menu [mark-tags] (cons "Tagged" diredp-mark-tags-menu))

  (define-key diredp-mark-tags-menu [diredp-unmark-files-tagged-none]
    '(menu-item "Unmark Not Tagged with Any..." diredp-unmark-files-tagged-none
      :help "Unmark files that are not tagged with *any* of the tags you enter"))
  (define-key diredp-mark-tags-menu [diredp-unmark-files-tagged-not-all]
    '(menu-item "Unmark Not Tagged with All..." diredp-unmark-files-tagged-not-all
      :help "Unmark files that are not tagged with *all* tags"))
  (define-key diredp-mark-tags-menu [diredp-unmark-files-tagged-some]
    '(menu-item "Unmark Tagged with Some..." diredp-unmark-files-tagged-some
      :help "Unmark files that are tagged with at least one of the tags you enter"))
  (define-key diredp-mark-tags-menu [diredp-unmark-files-tagged-all]
    '(menu-item "Unmark Tagged with All..." diredp-unmark-files-tagged-all
      :help "Unmark files that are tagged with *each* tag you enter"))
  (define-key diredp-mark-tags-menu [diredp-unmark-files-tagged-regexp]
    '(menu-item "Unmark Tagged Matching Regexp..." diredp-unmark-files-tagged-regexp
      :help "Unmark files that have at least one tag that matches a regexp"))

  (define-key diredp-mark-tags-menu [diredp-mark-files-tagged-none]
    '(menu-item "Mark Not Tagged with Any..." diredp-mark-files-tagged-none
      :help "Mark files that are not tagged with *any* of the tags you enter"))
  (define-key diredp-mark-tags-menu [diredp-mark-files-tagged-not-all]
    '(menu-item "Mark Not Tagged with All..." diredp-mark-files-tagged-not-all
      :help "Mark files that are not tagged with *all* tags"))
  (define-key diredp-mark-tags-menu [diredp-mark-files-tagged-some]
    '(menu-item "Mark Tagged with Some..." diredp-mark-files-tagged-some
      :help "Mark files that are tagged with at least one of the tags you enter"))
  (define-key diredp-mark-tags-menu [diredp-mark-files-tagged-all]
    '(menu-item "Mark Tagged with All..." diredp-mark-files-tagged-all
      :help "Mark files that are tagged with *each* tag you enter"))
  (define-key diredp-mark-tags-menu [diredp-mark-files-tagged-regexp]
    '(menu-item "Mark Tagged Matching Regexp..." diredp-mark-files-tagged-regexp
      :help "Mark files that have at least one tag that matches a regexp")))


;; "Dir" menu.
;;
;; REPLACE ORIGINAL "Subdir" menu in `dired.el'.
;;
(defvar diredp-menu-bar-subdir-menu (make-sparse-keymap "Dir"))
(define-key dired-mode-map [menu-bar subdir]
  (cons "Dir" diredp-menu-bar-subdir-menu))
(define-key diredp-menu-bar-subdir-menu [hide-all]
  '(menu-item "Hide/Show All" dired-hide-all
    :help "Hide all subdirectories, leave only header lines"))
(define-key diredp-menu-bar-subdir-menu [hide-subdir]
  '(menu-item "Hide/Show Subdir" dired-hide-subdir
    :help "Hide or unhide current directory listing"))
(define-key diredp-menu-bar-subdir-menu [tree-down]
  '(menu-item "Tree Down" dired-tree-down :help "Go to first subdirectory header down the tree"))
(define-key diredp-menu-bar-subdir-menu [tree-up]
  '(menu-item "Tree Up" dired-tree-up :help "Go to first subdirectory header up the tree"))
(define-key diredp-menu-bar-subdir-menu [up]
  '(menu-item "Up Directory" dired-up-directory :help "Dired the parent directory"))
(define-key diredp-menu-bar-subdir-menu [prev-subdir]
  '(menu-item "Prev Subdir" dired-prev-subdir :help "Go to previous subdirectory header line"))
(define-key diredp-menu-bar-subdir-menu [next-subdir]
  '(menu-item "Next Subdir" dired-next-subdir :help "Go to next subdirectory header line"))
(define-key diredp-menu-bar-subdir-menu [prev-dirline]
  '(menu-item "Prev Dirline" dired-prev-dirline :help "Move to previous directory-file line"))
(define-key diredp-menu-bar-subdir-menu [next-dirline]
  '(menu-item "Next Dirline" dired-next-dirline :help "Move to next directory-file line"))
(define-key diredp-menu-bar-subdir-menu [insert]
  '(menu-item "This Subdir" dired-maybe-insert-subdir
    :help "Move to subdirectory line or listing"))
(define-key diredp-menu-bar-subdir-menu [separator-subdir] '("--")) ; --------------------------

(when (fboundp 'image-dired-dired-toggle-marked-thumbs) ; Emacs 24+
  (define-key diredp-menu-bar-subdir-menu [image-dired-dired-toggle-marked-thumbs]
    '(menu-item "Toggle Image Thumbnails" image-dired-dired-toggle-marked-thumbs
      :help "Add or remove image thumbnails in front of marked file names")))
(when (fboundp 'dired-isearch-filenames) ; Emacs 23+
  (define-key diredp-menu-bar-subdir-menu [isearch-filenames-regexp]
    '(menu-item "Isearch Regexp in File Names..." dired-isearch-filenames-regexp
      :help "Incrementally search for regexp in file names only"))
  (define-key diredp-menu-bar-subdir-menu [isearch-filenames]
    '(menu-item "Isearch in File Names..." dired-isearch-filenames
      :help "Incrementally search for literal text in file names only.")))
(when (or (> emacs-major-version 21) (fboundp 'wdired-change-to-wdired-mode))
  (define-key diredp-menu-bar-subdir-menu [wdired-mode]
    '(menu-item "Edit File Names (WDired)" wdired-change-to-wdired-mode
      :help "Put a dired buffer in a mode in which filenames are editable"
      :keys "C-x C-q" :filter (lambda (x) (and (derived-mode-p 'dired-mode)  x)))))
(when (fboundp 'dired-compare-directories) ; Emacs 22+
  (define-key diredp-menu-bar-subdir-menu [compare-directories]
    '(menu-item "Compare Directories..." dired-compare-directories
      :help "Mark files with different attributes in two Dired buffers")))
(define-key diredp-menu-bar-subdir-menu [create-directory] ; Moved from "Immediate".
  '(menu-item "New Directory..." dired-create-directory :help "Create a directory"))
(define-key diredp-menu-bar-subdir-menu [separator-dired-on-set] '("--")) ; --------------------

(define-key diredp-menu-bar-subdir-menu [diredp-dired-inserted-subdirs]
  '(menu-item "Dired Each Inserted Subdir..." diredp-dired-inserted-subdirs
    :enable (cdr dired-subdir-alist)    ; First elt is current dir.  Must have at least one more.
    :help "Open Dired for each of the inserted subdirectories"))
(define-key diredp-menu-bar-subdir-menu [diredp-dired-union-other-window]
  '(menu-item "Dired Union..." diredp-dired-union-other-window
    :help "Open Dired for the union of some existing Dired buffers"))
(define-key diredp-menu-bar-subdir-menu [diredp-fileset]
  '(menu-item "Dired Fileset..." diredp-fileset
    :enable (> emacs-major-version 21) :help "Open Dired on an Emacs fileset"))
(define-key diredp-menu-bar-subdir-menu [diredp-marked-other-window]
  '(menu-item "Dired Marked Files in Other Window" diredp-marked-other-window
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files only, in other window"))
(define-key diredp-menu-bar-subdir-menu [diredp-marked]
  '(menu-item "Dired Marked Files" diredp-marked
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files only"))
(define-key diredp-menu-bar-subdir-menu [dired]
  '(menu-item "Dired (Filter via Wildcards)..." dired
    :help "Explore a directory (you can provide wildcards)"))
(define-key diredp-menu-bar-subdir-menu [revert]
  '(menu-item "Refresh (Sync \& Show All)" revert-buffer :help "Update directory contents"))


;;; Mouse-3 menu binding.
(define-key dired-mode-map [down-mouse-3] 'diredp-mouse-3-menu)
(define-key dired-mode-map [mouse-3]      'ignore)


;;; Suggested bindings.
;;; (define-key ctl-x-map   "d" 'diredp-dired-files) ; `C-x d'
;;; (define-key ctl-x-4-map "d" 'diredp-dired-files-other-window) ; `C-x 4 d'


;;; Non-menu Dired bindings.

(define-key dired-mode-map [S-down-mouse-1] 'ignore) ; (normally `mouse-set-font')
;; `diredp-mouse-mark-region-files' provides Windows-Explorer behavior
;; for selecting (marking) files.
(define-key dired-mode-map [S-mouse-1] 'diredp-mouse-mark-region-files)     ; `S-mouse-1'
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file-other-window)   ; `mouse-2'
(define-key dired-mode-map [S-down-mouse-2] 'diredp-mouse-find-file)        ; `S-mouse-2'
(define-key dired-mode-map [S-mouse-2] 'ignore)
(define-key dired-mode-map [M-mouse-2] 'diredp-mouse-find-file-other-frame) ; `M-mouse-2'

;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(progn
    (define-key dired-mode-map [(control return)] 'dired-w32explore)        ; `C-RET'
    (define-key dired-mode-map [(meta return)] 'dired-w32-browser)          ; `M-RET'
    (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)))        ; `mouse-2'

(define-key dired-mode-map "="       'diredp-ediff)
;; This replaces the `dired-x.el' binding of `dired-mark-extension'.
(define-key dired-mode-map "*."      'diredp-mark/unmark-extension)                 ; `* .'
(define-key dired-mode-map [(control meta ?*)] 'diredp-marked-other-window)         ; `C-M-*'
(define-key dired-mode-map "\M-b"    'diredp-do-bookmark)                           ; `M-b'
(define-key dired-mode-map "\C-\M-b" 'diredp-set-bookmark-file-bookmark-for-marked) ; `C-M-b'
(define-key dired-mode-map [(control meta shift ?b)]                    ; `C-M-B' (aka `C-M-S-b')
  'diredp-do-bookmark-in-bookmark-file)
(define-key dired-mode-map "\M-g"    'diredp-do-grep)                               ; `M-g'
(when (fboundp 'mkhtml-dired-files)
  (define-key dired-mode-map "\M-h"  'mkhtml-dired-files))                          ; `M-h'
(define-key dired-mode-map "U"       'dired-unmark-all-marks)                       ; `U'
(substitute-key-definition 'describe-mode 'diredp-describe-mode                    ; `h', `C-h m'
                           dired-mode-map (current-global-map))

;; Tags - same keys as in `*Bookmark List*'.
;;
;; NOTE: If this changes then need to update `dired-sort-menu+.el' to reflect the changes.
;;
(define-key dired-mode-map "T"       nil) ; For Emacs20
(define-key dired-mode-map "T+"      'diredp-tag-this-file)                ; `T +'
(define-key dired-mode-map "T-"      'diredp-untag-this-file)              ; `T -'
(define-key dired-mode-map "T0"      'diredp-remove-all-tags-this-file)    ; `T 0'
(define-key dired-mode-map "Tc"      'diredp-copy-tags-this-file)          ; `T c'
(define-key dired-mode-map "Tp"      'diredp-paste-add-tags-this-file)     ; `T p'
(define-key dired-mode-map "Tq"      'diredp-paste-replace-tags-this-file) ; `T q'
(define-key dired-mode-map "Tv"      'diredp-set-tag-value-this-file)      ; `T v'
(define-key dired-mode-map "T\M-w"   'diredp-copy-tags-this-file)          ; `T M-w'
(define-key dired-mode-map "T\C-y"   'diredp-paste-add-tags-this-file)     ; `T C-y'
(define-key dired-mode-map "T>+"     'diredp-do-tag)                       ; `T > +'
(define-key dired-mode-map "T>-"     'diredp-do-untag)                     ; `T > -'
(define-key dired-mode-map "T>0"     'diredp-do-remove-all-tags)           ; `T > 0'
(define-key dired-mode-map "T>p"     'diredp-do-paste-add-tags)            ; `T > p'
(define-key dired-mode-map "T>q"     'diredp-do-paste-replace-tags)        ; `T > q'
(define-key dired-mode-map "T>v"     'diredp-do-set-tag-value)             ; `T > v'
(define-key dired-mode-map "T>\C-y"  'diredp-do-paste-add-tags)            ; `T > C-y'
(define-key dired-mode-map "Tm%"     'diredp-mark-files-tagged-regexp)     ; `T m %'
(define-key dired-mode-map "Tm*"     'diredp-mark-files-tagged-all)        ; `T m *'
(define-key dired-mode-map "Tm+"     'diredp-mark-files-tagged-some)       ; `T m +'
(define-key dired-mode-map "Tm~*"    'diredp-mark-files-tagged-not-all)    ; `T m ~ *'
(define-key dired-mode-map "Tm~+"    'diredp-mark-files-tagged-none)       ; `T m ~ +'
(define-key dired-mode-map "Tu%"     'diredp-unmark-files-tagged-regexp)   ; `T u %'
(define-key dired-mode-map "Tu*"     'diredp-unmark-files-tagged-all)      ; `T u *'
(define-key dired-mode-map "Tu+"     'diredp-unmark-files-tagged-some)     ; `T u +'
(define-key dired-mode-map "Tu~*"    'diredp-unmark-files-tagged-not-all)  ; `T u ~ *'
(define-key dired-mode-map "Tu~+"    'diredp-unmark-files-tagged-none)     ; `T u ~ +'
;; $$$$$$ (define-key dired-mode-map [(control ?+)] 'diredp-do-tag)
;; $$$$$$ (define-key dired-mode-map [(control ?-)] 'diredp-do-untag)


;; Commands for operating on the current line's file.  When possible,
;; these are lower-case versions of the upper-case commands for operating on
;; the marked files.  (Most of the other corresponding lower-case letters are already
;; defined and cannot be used here.)

;; $$$$$$ (define-key dired-mode-map [(control meta ?+)] 'diredp-tag-this-file)
;; $$$$$$ (define-key dired-mode-map [(control meta ?-)] 'diredp-untag-this-file)
(define-key dired-mode-map "\r"      'dired-find-file)                      ; `RET'
(define-key dired-mode-map (kbd "C-h RET")        'diredp-describe-file)    ; `C-h RET'
(define-key dired-mode-map (kbd "C-h C-<return>") 'diredp-describe-file)    ; `C-h C-RET'
(define-key dired-mode-map "%c"      'diredp-capitalize)                    ; `% c'
(define-key dired-mode-map "b"       'diredp-byte-compile-this-file)        ; `b'
(define-key dired-mode-map [(control shift ?b)] 'diredp-bookmark-this-file) ; `C-B'
(define-key dired-mode-map "\M-c"    'diredp-capitalize-this-file)          ; `M-c'
(when (fboundp 'diredp-chgrp-this-file)
  (define-key dired-mode-map [(control meta shift ?g)] 'diredp-chgrp-this-file)) ; `C-M-G'
(define-key dired-mode-map "\M-i"    'diredp-insert-subdirs)                ; `M-i'
(define-key dired-mode-map "\M-l"    'diredp-downcase-this-file)            ; `M-l'
(define-key dired-mode-map [(meta shift ?m)] 'diredp-chmod-this-file)       ; `M-M'
(define-key dired-mode-map "\C-o"    'diredp-find-file-other-frame)         ; `C-o'
(when (fboundp 'diredp-chown-this-file)
  (define-key dired-mode-map [(meta shift ?o)] 'diredp-chown-this-file))    ; `M-O'
(define-key dired-mode-map "\C-\M-o" 'dired-display-file)                   ; `C-M-o' (not `C-o')
(define-key dired-mode-map "\M-p"    'diredp-print-this-file)               ; `M-p'
(define-key dired-mode-map "r"       'diredp-rename-this-file)              ; `r'
(define-key dired-mode-map [(meta shift ?t)] 'diredp-touch-this-file)       ; `M-T'
(define-key dired-mode-map [(control meta shift ?t)] 'dired-do-touch)       ; `C-M-T'
(define-key dired-mode-map "\M-u"    'diredp-upcase-this-file)              ; `M-u'
(define-key dired-mode-map "y"       'diredp-relsymlink-this-file)          ; `y'
(define-key dired-mode-map "z"       'diredp-compress-this-file)            ; `z'
(when (fboundp 'dired-show-file-type)
  (define-key dired-mode-map "_"      'dired-show-file-type))               ; `_' (underscore)
(substitute-key-definition 'kill-line 'diredp-delete-this-file              ; `C-k', `delete'
                           dired-mode-map (current-global-map))


;; Commands that handle marked below, recursively.
;; Use `M-+' as a prefix key for all such commands.

(define-prefix-command 'diredp-recursive-map)
(define-key dired-mode-map "\M-+"  diredp-recursive-map) ; `M-+'

(when (> emacs-major-version 22)
  (define-key diredp-recursive-map ":d"        'diredp-do-decrypt-recursive)            ; `: d'
  (define-key diredp-recursive-map ":e"        'diredp-do-encrypt-recursive)            ; `: e'
  (define-key diredp-recursive-map ":s"        'diredp-do-sign-recursive)               ; `: s'
  (define-key diredp-recursive-map ":v"        'diredp-do-verify-recursive)             ; `: v'
  )
(define-key diredp-recursive-map "%c"          'diredp-capitalize-recursive)            ; `% c'
(define-key diredp-recursive-map "%l"          'diredp-downcase-recursive)              ; `% l'
(define-key diredp-recursive-map "%u"          'diredp-upcase-recursive)                ; `% u'
(define-key diredp-recursive-map "!"           'diredp-do-shell-command-recursive)      ; `!'
(define-key diredp-recursive-map (kbd "C-M-*") 'diredp-marked-recursive-other-window)   ; `C-M-*'
(define-key diredp-recursive-map "A"           'diredp-do-search-recursive)             ; `A'
(define-key diredp-recursive-map "\M-b"        'diredp-do-bookmark-recursive)           ; `M-b'
(define-key diredp-recursive-map (kbd "C-M-b")                                          ; `C-M-b'
  'diredp-set-bookmark-file-bookmark-for-marked-recursive)
(define-key diredp-recursive-map [(control meta shift ?b)]                              ; `C-M-B'
  'diredp-do-bookmark-in-bookmark-file-recursive)
(define-key diredp-recursive-map "C"           'diredp-do-copy-recursive)               ; `C'
(define-key diredp-recursive-map "F"           'diredp-do-find-marked-files-recursive)  ; `F'
(when (fboundp 'diredp-do-chgrp-recursive)
  (define-key diredp-recursive-map "G"         'diredp-do-chgrp-recursive))             ; `G'
(define-key diredp-recursive-map "\M-g"        'diredp-do-grep-recursive)               ; `M-g'
(define-key diredp-recursive-map "H"           'diredp-do-hardlink-recursive)           ; `H'
(define-key diredp-recursive-map "\M-i"        'diredp-insert-subdirs-recursive)        ; `M-i'
(define-key diredp-recursive-map "l"           'diredp-list-marked-recursive)           ; `l'
(define-key diredp-recursive-map "M"           'diredp-do-chmod-recursive)              ; `M'
(when (fboundp 'diredp-do-chown-recursive)
  (define-key diredp-recursive-map "O"         'diredp-do-chown-recursive))             ; `O'
(define-key diredp-recursive-map "P"           'diredp-do-print-recursive)              ; `P'
(define-key diredp-recursive-map "Q"         'diredp-do-query-replace-regexp-recursive) ; `Q'
(define-key diredp-recursive-map "R"           'diredp-do-move-recursive)               ; `R'
(define-key diredp-recursive-map "S"           'diredp-do-symlink-recursive)            ; `S'
(define-key diredp-recursive-map (kbd "M-s a C-s")                                ; `M-s a C-s'
  'diredp-do-isearch-recursive)
(define-key diredp-recursive-map (kbd "M-s a C-M-s")                              ; `M-s a C-M-s'
  'diredp-do-isearch-regexp-recursive)
(define-key diredp-recursive-map [(control meta shift ?t)] 'diredp-do-touch-recursive)  ; `C-M-T'
(define-key diredp-recursive-map "\C-tc"   'diredp-image-dired-comment-files-recursive) ; `C-t c'
(define-key diredp-recursive-map "\C-td"  'diredp-image-dired-display-thumbs-recursive) ; `C-t d'
(define-key diredp-recursive-map "\C-tr"      'diredp-image-dired-delete-tag-recursive) ; `C-t r'
(define-key diredp-recursive-map "\C-tt"       'diredp-image-dired-tag-files-recursive) ; `C-t t'
(define-key diredp-recursive-map "\M-w"        'diredp-copy-filename-as-kill-recursive) ; `M-w'
(define-key diredp-recursive-map "Y"           'diredp-do-relsymlink-recursive)         ; `Y'


;; Undefine some bindings that would try to modify a Dired buffer.  Their key sequences will
;; then appear to the user as available for local (Dired) definition.
(when (fboundp 'undefine-killer-commands) (undefine-killer-commands dired-mode-map))


(defgroup Dired-Plus nil
  "Various enhancements to Dired."
  :prefix "diredp-" :group 'dired
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
dired+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/dired+.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/DiredPlus")
  :link '(emacs-commentary-link :tag "Commentary" "dired+"))
 
;;; Face Definitions

;;; Miscellaneous faces.
(defface diredp-display-msg
    '((((background dark)) (:foreground "Yellow"))
      (t                   (:foreground "Blue")))
  "*Face used for message display."
  :group 'Dired-Plus)
(defvar diredp-display-msg 'diredp-display-msg)

;;; Faces used to fontify buffer when using second level of fontifying.
(defface diredp-dir-heading
    '((((background dark)) (:foreground "Yellow" :background "#00003F3F3434")) ; ~ dark green
      (t                   (:foreground "Blue" :background "Pink")))
  "*Face used for directory headings in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-dir-heading 'diredp-dir-heading)

(defface diredp-deletion
    '((t (:foreground "Yellow" :background "Red")))
  "*Face used for deletion flags (D) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-deletion 'diredp-deletion)

(defface diredp-deletion-file-name
    '((t (:foreground "Red")))
  "*Face used for names of deleted files in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-deletion-file-name 'diredp-deletion-file-name)

(defface diredp-flag-mark
    '((((background dark)) (:foreground "Blue" :background "#7575D4D41D1D")) ; ~ olive green
      (t                   (:foreground "Yellow" :background "Blueviolet")))
  "*Face used for flags and marks (except D) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark 'diredp-flag-mark)

(defface diredp-flag-mark-line
    '((((background dark)) (:background "#787831311414")) ; ~ dark red brown
      (t                   (:background "Skyblue")))
  "*Face used for flagged and marked lines in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark-line 'diredp-flag-mark-line)

(defface diredp-file-suffix
    '((((background dark)) (:foreground "#7474FFFF7474")) ; ~ light green
      (t                   (:foreground "DarkMagenta")))
  "*Face used for file suffixes in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-suffix 'diredp-file-suffix)

(defface diredp-number
    '((((background dark)) (:foreground "#FFFFFFFF7474")) ; ~ light yellow
      (t                   (:foreground "DarkBlue")))
  "*Face used for numerical fields in dired buffers.
In particular, inode number, number of hard links, and file size."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-number 'diredp-number)

(defface diredp-symlink
    '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
      (t                   (:foreground "DarkOrange")))
  "*Face used for symbolic links in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-symlink 'diredp-symlink)

(defface diredp-date-time
    '((((background dark)) (:foreground "#74749A9AF7F7")) ; ~ med blue
      (t                   (:foreground "DarkGoldenrod4")))
  "*Face used for date and time in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-date-time 'diredp-date-time)

(defface diredp-file-name
    '((((background dark)) (:foreground "Yellow"))
      (t                   (:foreground "Blue")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-name 'diredp-file-name)

(defface diredp-ignored-file-name
    '(;; (((background dark)) (:foreground "#FFFF921F921F")) ; ~ salmon
      ;; (((background dark)) (:foreground "#A71F5F645F64")) ; ~ dark salmon
      (((background dark)) (:foreground "#C29D6F156F15")) ; ~ salmon
      (t                   (:foreground "#00006DE06DE0")))                  ; ~ dark cyan
  "*Face used for ignored file names  in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-ignored-file-name 'diredp-ignored-file-name)

(defface diredp-compressed-file-suffix
    '((((background dark)) (:foreground "Blue"))
      (t                   (:foreground "Yellow")))
  "*Face used for compressed file suffixes in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-compressed-file-suffix 'diredp-compressed-file-suffix)

;; For this to show up, you need `F' among the options in `dired-listing-switches'.
;; For example, I use "-alF" for `dired-listing-switches'.
(defface diredp-executable-tag
    '((t (:foreground "Red")))
  "*Face used for executable tag (*) on file names in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-executable-tag 'diredp-executable-tag)

(defface diredp-dir-priv
    '((((background dark))
       (:foreground "#7474FFFFFFFF" :background "#2C2C2C2C2C2C")) ; ~ cyan, dark gray
      (t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory privilege indicator (d) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-dir-priv 'diredp-dir-priv)

(defface diredp-exec-priv
    '((((background dark)) (:background "#4F4F3B3B2121")) ; ~ dark brown
      (t                   (:background "LightSteelBlue")))
  "*Face used for execute privilege indicator (x) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-exec-priv 'diredp-exec-priv)

(defface diredp-other-priv
    '((((background dark)) (:background "#111117175555")) ; ~ dark blue
      (t                   (:background "PaleGoldenrod")))
  "*Face used for l,s,S,t,T privilege indicators in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-other-priv 'diredp-other-priv)

(defface diredp-write-priv
    '((((background dark)) (:background "#25258F8F2929")) ; ~ dark green
      (t                   (:background "Orchid")))
  "*Face used for write privilege indicator (w) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-write-priv 'diredp-write-priv)

(defface diredp-read-priv
    '((((background dark)) (:background "#999932325555")) ; ~ burgundy / dark magenta
      (t                   (:background "MediumAquamarine")))
  "*Face used for read privilege indicator (w) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-read-priv 'diredp-read-priv)

(defface diredp-no-priv
    '((((background dark)) (:background "#2C2C2C2C2C2C")) ; ~ dark gray
      (t                   (:background "LightGray")))
  "*Face used for no privilege indicator (-) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-no-priv 'diredp-no-priv)

(defface diredp-rare-priv
    '((((background dark)) (:foreground "Green" :background "#FFFF00008080")) ; ~ hot pink
      (t                   (:foreground "Magenta" :background "SpringGreen")))
  "*Face used for rare privilege indicators (b,c,s,m,p,S) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-rare-priv 'diredp-rare-priv)

(defface diredp-link-priv
    '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
      (t                   (:foreground "DarkOrange")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-link-priv 'diredp-link-priv)

;; Fix Emacs 20 recognition of fields up through file name when size is expressed using `k' etc.
(when (and (< emacs-major-version 21)  (not (boundp 'diredp-loaded-p))
           dired-move-to-filename-regexp ; These last two checks are just in case.
           (eq (aref dired-move-to-filename-regexp 7) ?\  ))
  (setq dired-move-to-filename-regexp  (concat "[0-9][BkKMGTPEZY]?"
                                               (substring dired-move-to-filename-regexp 7))))

;;; Define second level of fontifying.
(defvar diredp-font-lock-keywords-1
  (list
   '("^  \\(.+:\\)$" 1 diredp-dir-heading) ; Directory headers
   '("^  wildcard.*$" 0 'default)       ; Override others, e.g. `l' for `diredp-other-priv'.
   '("^  (No match).*$" 0 'default)     ; Override others, e.g. `t' for `diredp-other-priv'.
   '("[^ .]\\.\\([^. /]+\\)$" 1 diredp-file-suffix) ; Suffix
   '("\\([^ ]+\\) -> [^ ]+$" 1 diredp-symlink) ; Symbolic links

   ;; 1) Date/time and 2) filename w/o suffix.
   ;;    This is a bear, and it is fragile - Emacs can change `dired-move-to-filename-regexp'.
   (if (or (not (fboundp 'version<)) (version< emacs-version "23.2"))
       (list dired-move-to-filename-regexp
             (list 1 'diredp-date-time t t) ; Date/time
             (list "\\(.+\\)$" nil nil (list 0 diredp-file-name 'keep t))) ; Filename
     (list dired-move-to-filename-regexp
           (list 7 'diredp-date-time t t) ; Date/time, locale (western or eastern)
           (list 2 'diredp-date-time t t) ; Date/time, ISO
           (list "\\(.+\\)$" nil nil (list 0 diredp-file-name 'keep t)))) ; Filename

   ;; Files to ignore
   (list (concat "^  \\(.*\\(" (concat (mapconcat 'regexp-quote
                                                  (or (and (boundp 'dired-omit-extensions)
                                                           dired-omit-extensions)
                                                      completion-ignored-extensions)
                                                  "[*]?\\|")
                                       "[*]?")        ; Allow for executable flag (*).
                 "\\|\\.\\(g?z\\|Z\\)[*]?\\)\\)$") ; Compressed.
         1 diredp-ignored-file-name t)
   '("[^ .]\\.\\([bg]?[zZ]2?\\)[*]?$" 1 diredp-compressed-file-suffix t) ; Compressed (*.z)
   '("\\([*]\\)$" 1 diredp-executable-tag t) ; Executable (*)
   ;; Inode, hard-links, & file size (. and , are for the decimal point, depending on locale)
   ;; See comment for `directory-listing-before-filename-regexp' in `files.el' or `files+.el'.
   '("\\(\\([0-9]+\\([.,][0-9]+\\)?\\)[BkKMGTPEZY]? \\)" 1 diredp-number)

   ;; Directory names
   (list "^..\\([0-9]* \\)*d"
         (list dired-move-to-filename-regexp nil nil)
         (list "\\(.+\\)" nil nil '(0 diredp-dir-priv t t)))
   '("^..\\([0-9]* \\)*.........\\(x\\)" 2 diredp-exec-priv) ;o x
   '("^..\\([0-9]* \\)*.........\\([lsStT]\\)" 2 diredp-other-priv) ; o misc
   '("^..\\([0-9]* \\)*........\\(w\\)" 2 diredp-write-priv) ; o w
   '("^..\\([0-9]* \\)*.......\\(r\\)" 2 diredp-read-priv) ; o r
   '("^..\\([0-9]* \\)*......\\(x\\)" 2 diredp-exec-priv) ; g x
   '("^..\\([0-9]* \\)*....[^0-9].\\([lsStT]\\)" 2 diredp-other-priv) ; g misc
   '("^..\\([0-9]* \\)*.....\\(w\\)" 2 diredp-write-priv) ; g w
   '("^..\\([0-9]* \\)*....\\(r\\)" 2 diredp-read-priv) ; g r
   '("^..\\([0-9]* \\)*...\\(x\\)" 2 diredp-exec-priv) ; u x
   '("^..\\([0-9]* \\)*...\\([lsStT]\\)" 2 diredp-other-priv) ; u misc
   '("^..\\([0-9]* \\)*..\\(w\\)" 2 diredp-write-priv) ; u w
   '("^..\\([0-9]* \\)*.\\(r\\)" 2 diredp-read-priv) ; u r
   '("^..\\([0-9]* \\)*.\\([-rwxlsStT]+\\)" 2 diredp-no-priv keep) ;-
   '("^..\\([0-9]* \\)*\\([bcsmpS]\\)[-rwxlsStT]" 2 diredp-rare-priv) ; (rare)
   '("^..\\([0-9]* \\)*\\(l\\)[-rwxlsStT]" 2 diredp-link-priv) ; l
   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "].*$\\)")
         1 diredp-flag-mark-line t)     ; Flag/mark lines
   (list (concat "^\\([" (char-to-string dired-del-marker) "]\\)") ; Deletion flags (D)
         '(1 diredp-deletion t)
         '(".+" (dired-move-to-filename) nil (0 diredp-deletion-file-name t)))
   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "]\\)") ; Flags, marks (except D)
         1 diredp-flag-mark t)
   ) "2nd level of Dired highlighting.  See `font-lock-maximum-decoration'.")


;;; Provide for the second level of fontifying.
(add-hook 'dired-mode-hook
          (lambda ()
            (set (make-local-variable 'font-lock-defaults)
                 ;; Two levels.  Use 3-element list, since it is standard to have one more
                 ;; than the number of levels.  This is necessary for it to work with
                 ;; `font(-lock)-menus.el'.
                 '((dired-font-lock-keywords
                    dired-font-lock-keywords
                    diredp-font-lock-keywords-1)
                   t nil nil beginning-of-line))
            ;; Refresh `font-lock-keywords' from `font-lock-defaults'
            (when (fboundp 'font-lock-refresh-defaults) (font-lock-refresh-defaults))))
 
;;; Function Definitions

;;;###autoload
(defun diredp-dired-files (arg &optional switches) ; Not bound
  "Like `dired', but non-positive prefix arg prompts for files to list.
This is like `dired' unless you use a non-positive prefix arg.
In that case, you are prompted for names of files and directories to
list, and then you are prompted for the name of the Dired buffer that
lists them.  Use `C-g' when you are done entering file names to list.

In all cases, when inputting a file or directory name you can use
shell wildcards.

If you use Icicles, then in Icicle mode the following keys are bound
in the minibuffer during completion (`*' means the key requires
library `Bookmark+'):

  M-|       - Open Dired on the file names matching your input
  C-c +     - Create a new directory
 *C-x a +   - Add tags to the current-candidate file
 *C-x a -   - Remove tags from the current-candidate file
 *C-x m     - Access file bookmarks (not just autofiles)"
  (interactive (diredp-dired-files-interactive-spec ""))
  (when (consp arg)
    (let ((buf  (dired-find-buffer-nocreate (car arg)))) ; Respect file list.
      (when buf (kill-buffer buf))))
  (switch-to-buffer (dired-noselect arg switches)))

;;;###autoload
(defun diredp-dired-files-other-window (arg &optional switches) ; Not bound
  "Same as `diredp-dired-files' except uses another window."
  (interactive (diredp-dired-files-interactive-spec "in other window "))
  (when (consp arg)
    (let ((buf  (dired-find-buffer-nocreate (car arg)))) ; Respect file list.
      (when buf (kill-buffer buf))))
  (dired-other-window arg switches))

;;;###autoload
(defun diredp-dired-for-files (arg &optional switches) ; Not bound
  "Like `dired', but prompts for the specific files to list.
You are prompted for names of files and directories to list, and then
you are prompted for the name of the Dired buffer that lists them.
Use `C-g' when you are done entering file names to list.

In all cases, when inputting a file or directory name you can use
shell wildcards.

If you use Icicles, then in Icicle mode the following keys are bound
in the minibuffer during completion (`*' means the key requires
library `Bookmark+'):

  M-|       - Open Dired on the file names matching your input
  C-c +     - Create a new directory
 *C-x a +   - Add tags to the current-candidate file
 *C-x a -   - Remove tags from the current-candidate file
 *C-x m     - Access file bookmarks (not just autofiles)"
  (interactive
   (let ((current-prefix-arg  -1))
     (diredp-dired-files-interactive-spec "in other window ")))
  (diredp-dired-files-other-window arg switches))

;;;###autoload
(defun diredp-dired-for-files-other-window (arg &optional switches) ; Not bound
  "Same as `diredp-dired-for-files' except uses another window."
  (interactive
   (let ((current-prefix-arg  -1))
     (diredp-dired-files-interactive-spec "in other window ")))
  (diredp-dired-files-other-window arg switches))

(defun diredp-dired-files-interactive-spec (str)
  "`interactive' spec for `diredp-dired-files' commands.
STR is a string appended to the prompt.
With non-negative prefix arg, read switches.
With non-positive prefix arg, read files and dirs to list and then the
 Dired buffer name.  User uses `C-g' when done reading files and dirs.

If you use Icicles, then in Icicle mode the following keys are bound
in the minibuffer during completion (`*' means the key requires
library `Bookmark+'):

  M-|       - Open Dired on the file names matching your input
  C-c +     - Create a new directory
 *C-x a +   - Add tags to the current-candidate file
 *C-x a -   - Remove tags from the current-candidate file
 *C-x m     - Access file bookmarks (not just autofiles)"
  (list
   (unwind-protect
        (let ((icicle-sort-comparer  (or icicle-file-sort ; If not reading files then dirs first.
                                         (and (> (prefix-numeric-value current-prefix-arg) 0)
                                              'icicle-dirs-first-p)
                                         icicle-sort-comparer))
              (icicle-all-candidates-list-alt-action-fn ; M-|'
               (lambda (files)
                 (let ((enable-recursive-minibuffers  t))
                   (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
          (when (fboundp 'icicle-bind-file-candidate-keys) (icicle-bind-file-candidate-keys))
          (if (> (prefix-numeric-value current-prefix-arg) 0)
              ;; If a dialog box is about to be used, call `read-directory-name' so the dialog
              ;; code knows we want directories.  Some dialog boxes can only select directories
              ;; or files when popped up, not both.
              (if (and (fboundp 'read-directory-name)  (next-read-file-uses-dialog-p))
                  (read-directory-name (format "Dired %s(directory): " str) nil
                                       default-directory nil)
                (read-file-name (format "Dired %s(directory): " str) nil default-directory nil))
            (let ((insert-default-directory  nil)
                  (files                     ())
                  file)
              (while (condition-case nil ; Use lax completion, to allow wildcards.
                         (setq file  (read-file-name "File or dir (C-g when done): "))
                       (quit nil))
                (push file files))
              (cons (read-string "Dired buffer name: " nil nil default-directory) files))))
     (when (fboundp 'icicle-unbind-file-candidate-keys)
       (icicle-unbind-file-candidate-keys)))
   (and current-prefix-arg  (natnump (prefix-numeric-value current-prefix-arg))
        (read-string "Dired listing switches: " dired-listing-switches))))

;;;###autoload
(defun diredp-dired-union (dirbufs &optional switches) ; Not bound
  "Create a Dired buffer that is the union of some existing Dired buffers.
With a prefix arg, read `ls' switches.
You are prompted for the Dired buffers.  Use `C-g' when done choosing
them.  Then you are prompted for the name of the new Dired buffer.
Its `default-directory' is the same as the `default-directory' before
invoking the command.

The selected Dired listings are included in the order that you choose
them, and each entry is listed only once in the new Dired buffer.  The
new Dired listing respects the markings, subdirectory insertions, and
hidden subdirectories of the selected Dired listings.

However, in case of conflict between marked or unmarked status for the
same entry, the entry is marked.  Similarly, in case of conflict over
an included subdirectory between it being hidden or shown, it is
hidden, but its contained files are also listed."
  (interactive (diredp-dired-union-interactive-spec ""))
  (diredp-dired-union-1 dirbufs switches))

;;;###autoload
(defun diredp-dired-union-other-window (dirbufs &optional switches) ; Not bound
  "Same as `diredp-dired-union' but uses another window."
  (interactive (diredp-dired-union-interactive-spec ""))
  (diredp-dired-union-1 dirbufs switches 'OTHER-WINDOW))

;; $$$$$ Maybe I should set `dired-sort-inhibit' to t for now (?),
;; since there is an Emacs bug (at least on Windows) that prevents
;; sorting from working for a Dired buffer with an explicit file list.
(defun diredp-dired-union-1 (dirbufs switches &optional other-window-p)
  "Helper for `diredp-dired-union(-other-window)'."
  (let ((files        ())
        (marked       ())
        (subdirs      ())
        (hidden-dirs  ())
        hid-here files-here)
    (dolist (buf  (reverse (cdr dirbufs))) ; The car is the new Dired buffer name.
      (with-current-buffer buf
        (unwind-protect
             (progn
               (setq hid-here    (save-excursion (dired-remember-hidden))
                     files-here  (if (consp dired-directory)
                                     (reverse (cdr dired-directory)) ; Reverse bc will push.
                                   ()))
               (unless files-here
                 (save-excursion        ; This bit is more or less from `dired-toggle-marks'.
                   (goto-char (point-min))
                   (while (not (eobp))
                     (or (looking-at dired-re-dot)
                         (push (dired-get-filename nil 'NO-ERROR-P) files-here))
                     (forward-line 1)))
                 (setq files-here  (delq nil files-here)))
               (dolist (hid-here  hid-here) (push hid-here hidden-dirs))
               (dolist (sub  (cdr (reverse dired-subdir-alist)))
                 (push (list (car sub)) subdirs))
               (dolist (mkd  (dired-remember-marks (point-min) (point-max))) ; This unhides.
                 (push (car mkd) marked))
               (dolist (file  files-here)
                 (when (or (not (file-name-absolute-p file)) (not (member file files)))
                   (push file files))))
          (save-excursion               ; Hide subdirs that were hidden.
            (dolist (dir  hid-here)  (when (dired-goto-subdir dir) (dired-hide-subdir 1)))))))
    ;; (pp-eval-expression '(list (cons 'FILES files) (cons 'MARKED marked)
    ;;                            (cons 'SUBDIRS subdirs) (cons 'HIDDEN-SUBDIRS hidden-dirs)))
    (if other-window-p
        (dired-other-window (cons (car dirbufs) files) switches)
      (dired (cons (car dirbufs) files) switches))
    (with-current-buffer (car dirbufs)
      (let ((inhibit-read-only  t))
        (dired-insert-old-subdirs subdirs)
        (dired-mark-remembered          ; Don't really need `expand-file-name' - already abs.
         (mapcar (lambda (mf) (cons (expand-file-name mf dired-directory) 42)) marked))
        (save-excursion
          (dolist (dir  hidden-dirs)  (when (dired-goto-subdir dir) (dired-hide-subdir 1))))))))

(defun diredp-dired-union-interactive-spec (str)
  "`interactive' spec for `diredp-dired-dired' commands.
STR is a string appended to the prompt.
With a prefix arg, read switches.
Read names of Dired buffers to include, and then the new, Dired-union
 buffer name.  User uses `C-g' when done reading Dired buffer names."
  (list
   (let ((bufs     ())
         dirbufs buf)
     (dolist (dbuf  dired-buffers)
       (when (buffer-live-p (cdr dbuf))
         (push (cons (buffer-name (cdr dbuf)) (car dbuf)) dirbufs)))
     (while (and dirbufs  (condition-case nil
                              (setq buf  (completing-read
                                          "Dired buffer to include (C-g when done): "
                                          dirbufs nil t nil buffer-name-history
                                          (and dirbufs  (car (assoc (buffer-name) dirbufs)))))
                            (quit nil)))
       (push buf bufs)
       (setq dirbufs  (delete (cons buf (with-current-buffer buf default-directory)) dirbufs)))
     (setq bufs  (nreverse bufs))
     (cons (read-string "Dired-union buffer name: ") bufs))
   (and current-prefix-arg  (read-string "Dired listing switches: " dired-listing-switches))))

;;;###autoload
(defun diredp-fileset (flset-name)      ; Not bound
  "Open Dired on the files in fileset FLSET-NAME."
  (interactive
   (list (let ((fd  (or (and (require 'filesets nil t)  filesets-data)
                        (error "Feature `filesets' not provided"))))
           (completing-read "Open Dired on fileset: " filesets-data))))
  (let ((flset  (filesets-get-fileset-from-name flset-name))
        (files  ())
        (mode   nil))
    (unless (or (setq mode  (filesets-entry-mode flset)) ; ("my-fs" (:files "a" "b"))
                (setq flset  (cons "dummy" flset) ; (:files "a" "b")
                      mode   (filesets-entry-mode flset)))
      (error "Bad fileset: %S" flset-name))
    (message "Gathering file names...")
    (dolist (file  (filesets-get-filelist flset mode)) (push file files))
    (dired (cons (generate-new-buffer-name flset-name)
                 (nreverse (mapcar (lambda (file)
                                     (if (file-name-absolute-p file)
                                         (expand-file-name file)
                                       file))
                                   files))))))

;;;###autoload
(defun diredp-dired-this-subdir (&optional tear-off-p msgp)
  "Open Dired for the subdir at or above point.
If point is not on a subdir line, but is in an inserted subdir
listing, then use that subdir.

With a prefix arg:
 If the subdir is inserted and point is in the inserted listing then
 remove that listing and move to the ordinary subdir line.  In other
 words, when in an inserted listing, a prefix arg tears off the
 inserted subdir to its own Dired buffer."
  (interactive "P\np")
  (diredp-ensure-mode)
  (let* ((this-dir       default-directory)
         (this-subdir    (diredp-this-subdir))
         (on-dir-line-p  (atom this-subdir)))
    (unless on-dir-line-p               ; Subdir header line or non-directory file.
      (setq this-subdir  (car this-subdir)))
    (unless (string= this-subdir this-dir)
      (when tear-off-p
        (unless on-dir-line-p
          (dired-kill-subdir)           ; Tear it off.
          (dired-goto-file this-subdir))) ; Move to normal subdir line.
      (dired-other-window this-subdir))))

;;;###autoload
(defun diredp-dired-inserted-subdirs (&optional no-show-p msgp)
  "Open Dired for each of the subdirs inserted in this Dired buffer.
With a prefix arg, create the Dired buffers but do not display them.
Markings and current Dired switches are preserved."
  (interactive "P\np")
  (diredp-ensure-mode)
  (let ((this-dir    default-directory)
        (this-buff   (current-buffer))
        (this-frame  (selected-frame))
        marked)
    (unwind-protect
         (save-selected-window
           (dolist (entry  dired-subdir-alist)
             (unless (string= (car entry) this-dir)
               (setq marked
                     (with-current-buffer this-buff
                       (dired-remember-marks (dired-get-subdir-min entry)
                                             (dired-get-subdir-max entry))))
               (if (not no-show-p)
                   (dired-other-window (car entry) dired-actual-switches)
                 (dired-noselect (car entry) dired-actual-switches)
                 (when msgp (message "Dired buffers created but not shown")))
               (set-buffer this-buff)
               (let ((inhibit-read-only  t))
                 (dired-mark-remembered marked))
               (set-buffer-modified-p nil))))
      (select-frame-set-input-focus this-frame))))


;;; Actions on marked files and subdirs, recursively.

(defun diredp-get-files (&optional ignore-marks-p predicate include-dirs-p dont-askp)
  "Return file names from this Dired buffer and subdirectories, recursively.
The name are those that are marked in the current Dired buffer, or all
files in the directory if none are marked.  Marked subdirectories are
handled recursively in the same way.

If there is some included subdirectory that has a Dired buffer with
marked files, then (unless DONT-ASKP is non-nil) ask the user whether
to use the marked files in Dired buffers, as opposed to using all of
the files in included directories.  To this y-or-n question the user
can hit `l' to see the list of files that will be included (using
`diredp-list-files').

\(Directories in `icicle-ignored-directories' are skipped, if you use
Icicles.  Otherwise, directories in `vc-directory-exclusion-list' are
skipped.)

Non-nil IGNORE-MARKS-P means ignore all Dired markings:
just get all of the files in the current directory.

Non-nil PREDICATE means include only file names for which the
PREDICATE returns non-nil.

Non-nil INCLUDE-DIRS-P means include marked subdirectory names (but
also handle those subdirs recursively, picking up their marked files
and subdirs).

Non-nil DONT-ASKP means do not ask the user whether to use marked
instead of all.  Act as if the user was asked and replied `y'."
  (let ((askp  (list nil)))             ; The cons's car will be set to `t' if need to ask user.
    (if ignore-marks-p
        (diredp-files-within (directory-files default-directory 'FULL diredp-re-no-dot)
                             () nil nil predicate)
      ;; Pass FILES and ASKP to `diredp-get-files-for-dir', so we don't have to use them as
      ;; free vars there.  But that means that they each need to be a cons cell that we can
      ;; modify, so we can get back the updated info.
      (let ((files  (list 'DUMMY)))     ; The files picked up will be added to this list.
        (diredp-get-files-for-dir default-directory files askp include-dirs-p)
        (setq files  (cdr files))       ; Remove `DUMMY' from the modifed list.
        (if (or dont-askp  (not (car askp))
                (diredp-y-or-n-files-p "Use marked (instead of all) in subdir Dired buffers? "
                                       files
                                       predicate))
            (if predicate (diredp-remove-if-not predicate files) files)
          (setq files  ())
          (dolist (file  (diredp-marked-here))
            (if (not (file-directory-p file))
                (when (or (not predicate)  (funcall predicate file))
                  (add-to-list 'files file))
              (when include-dirs-p (setq files  (nconc files (list file))))
              (setq files  (nconc files (diredp-files-within
                                         (directory-files file 'FULL diredp-re-no-dot)
                                         () nil include-dirs-p predicate)))))
          (nreverse files))))))

(defun diredp-get-files-for-dir (dir accum askp &optional include-dirs-p)
  "Return file names for directory DIR and subdirectories, recursively.
Pick up names of all marked files in DIR if it has a Dired buffer, or
all files in DIR if not.  Handle subdirs recursively (only marked
subdirs, if Dired).

ACCUM is an accumulator list: the files picked up in this call are
nconc'd to it.

ASKP is a one-element list, the element indicating whether to ask the
user about respecting Dired markings.  It is set here to `t' if there
is a Dired buffer for DIR.

Non-nil optional arg INCLUDE-DIRS-P means include marked subdirectory
names (but also handle those subdirs recursively).

If there is more than one Dired buffer for DIR then raise an error."
  (dolist (file  (if (not (dired-buffers-for-dir (expand-file-name dir)))
                     (directory-files dir 'FULL diredp-re-no-dot)
                   (when (cadr (dired-buffers-for-dir (expand-file-name dir)))
                     (error "More than one Dired buffer for `%s'" dir))
                   (unless (equal dir default-directory) (setcar askp  t))
                   (with-current-buffer (car (dired-buffers-for-dir (expand-file-name dir)))
                     (diredp-marked-here))))
    (if (not (file-directory-p file))
        (setcdr (last accum) (list file))
      (when include-dirs-p (setcdr (last accum) (list file)))
      (diredp-get-files-for-dir file accum askp include-dirs-p))))

(defun diredp-marked-here ()
  "Marked files and subdirs in this Dired buffer, or all if none are marked.
Directories `.' and `..' are excluded."
  ;; If no file is marked, exclude `(FILENAME)': the unmarked file at cursor.
  ;; If there are no marked files as a result, return all files and subdirs in the dir.
  (let ((ff  (condition-case nil        ; Ignore error if on `.' or `..' and no file is marked.
                 (dired-get-marked-files nil nil nil 'DISTINGUISH-ONE-MARKED)
               (error nil))))
    (cond ((eq t (car ff))  (cdr ff))   ; Single marked
          ((cadr ff)        ff)         ; Multiple marked
          (t                (directory-files ; None marked
                             default-directory 'FULL diredp-re-no-dot 'NOSORT)))))

(defun diredp-y-or-n-files-p (prompt files &optional predicate)
  "PROMPT user with a \"y or n\" question about a list of FILES.
Return t if answer is \"y\".  Otherwise, return nil.

Like `y-or-n-p', but the user can also hit `l' to display the list of
files that the confirmation is for, in buffer `*Files'.  When
finished, buffer `*Files*' is killed if it was never shown, or is
hidden and buried otherwise.  Thus, if it was shown then it is still
available to revisit afterward (even if the user quit using `C-g').

PREDICATE is passed to `diredp-list-files', to list only file names
for which it returns non-nil."
  (let ((answer  'recenter))
    (cond (noninteractive
           (setq prompt  (concat prompt
                                 (and (not (eq ?\   (aref prompt (1- (length prompt)))))  " ")
                                 "(y or n; l to show file list) "))
           (let ((temp-prompt  prompt))
             (while (not (memq answer '(act skip)))
               (let ((str  (read-string temp-prompt)))
                 (cond ((member str '("y" "Y")) (setq answer  'act))
                       ((member str '("n" "N")) (setq answer  'skip))
                       (t (setq temp-prompt  (concat "Please answer y or n.  " prompt))))))))
          ((if (not (fboundp 'display-popup-menus-p))
               (and window-system  (listp last-nonmenu-event)  use-dialog-box)
             (and (display-popup-menus-p)  (listp last-nonmenu-event)  use-dialog-box))
           (setq answer  (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
          (t
           (let ((list-buf        (generate-new-buffer-name "*Files*"))
                 (list-was-shown  nil))
             (unwind-protect
                  (progn
                    (define-key query-replace-map "l" 'show)
                    (setq prompt  (concat prompt
                                          (and (eq ?\   (aref prompt (1- (length prompt))))
                                               "" " ")
                                          "(y or n; l to show file list) "))
                    (while (let* ((key  (let ((cursor-in-echo-area  t))
                                          (when minibuffer-auto-raise
                                            (raise-frame (window-frame (minibuffer-window))))
                                          (if (fboundp 'read-key)
                                              (read-key (propertize
                                                         (if (memq answer '(recenter show))
                                                             prompt
                                                           (concat "Please answer y or n.  "
                                                                   prompt))
                                                         'face 'minibuffer-prompt))
                                            (read-char-exclusive
                                             (if (memq answer '(recenter show))
                                                 prompt
                                               (concat "Please answer y or n.  " prompt)))))))
                             (setq answer  (lookup-key query-replace-map (vector key) t))
                             (case answer
                               ((skip act)  nil)
                               (recenter    (recenter) t)
                               (show        (diredp-list-files files nil list-buf predicate)
                                            (setq list-was-shown  t)) ; Record showing it.
                               (help        (message "Use `l' to show file list") (sit-for 1))
                               ((exit-prefix quit) (signal 'quit nil) t)
                               (t t)))
                      (ding)
                      (discard-input)))
               (save-window-excursion (pop-to-buffer list-buf)
                                      (condition-case nil ; Ignore error if user already deleted.
                                          (if (one-window-p) (delete-frame) (delete-window))
                                        (error nil))
                                      (if list-was-shown
                                          (bury-buffer list-buf)
                                        (kill-buffer list-buf)))
               (define-key query-replace-map "l" nil)))))
    (let ((ret  (eq answer 'act)))
      (unless noninteractive (message "%s %s" prompt (if ret "y" "n")))
      ret)))

(defun diredp-list-files (files &optional dir bufname predicate)
  "Display FILES, a list of file names.  Wildcard patterns are expanded.
The files are shown in a new buffer, `*Files*' by default.
Optional arg DIR serves as the default directory for expanding file
 names that are not absolute.  It defaults to `default-directory'.
Optional arg BUFNAME is the name of the buffer for the display.
 It defaults to `*Files*' (or `*Files*<N>' if `*Files*' exists)."
  (with-output-to-temp-buffer (or bufname  (generate-new-buffer-name "*Files*"))
    (princ "Files\n-----\n\n")
    (let ((all-files-no-wildcards  ())
          file-alist  file-dir)
      (dolist (file  files)
        (unless (or (string= file "")   ; Ignore empty file names.
                    (and predicate  (not (funcall predicate file))))
          (if (not (string-match "[[?*]" file))
              (add-to-list 'all-files-no-wildcards (expand-file-name file))
            (setq file-dir    (or (file-name-directory file)  default-directory)
                  file-alist  (directory-files-and-attributes file-dir 'FULL "[[?*]" 'NOSORT))
            (dolist (ff  file-alist)
              (add-to-list 'all-files-no-wildcards (expand-file-name (car ff) file-dir))))))
      (dolist (fff  (nreverse all-files-no-wildcards))
        (princ fff)
        (terpri)))))

(defvar diredp-files-within-dirs-done ()
  "Directories already processed by `diredp-files-within'.")


;; Not used in the Dired+ code yet.
(defun diredp-directories-within (&optional directory no-symlinks-p predicate)
  "List of accessible directories within DIRECTORY.
Directories in `icicle-ignored-directories' are skipped, if you use
Icicles.  Otherwise, directories in `vc-directory-exclusion-list' are
skipped.

Optional arg DIRECTORY defaults to the value of `default-directory'.
Non-nil optional arg NO-SYMLINKS-P means do not follow symbolic links.
Non-nil optional arg PREDICATE must be a function that accepts a
 file-name argument.  Only directories that satisfy PREDICATE are
 included in the result."
  (unless directory (setq directory  default-directory))
  (let ((dirs  (diredp-files-within (directory-files directory 'FULL diredp-re-no-dot)
                                    () no-symlinks-p 'INCLUDE-DIRS-P
                                    #'file-directory-p)))
    (if predicate (diredp-remove-if-not predicate dirs) dirs)))

;; Args INCLUDE-DIRS-P and PREDICATE are not used in the Dired+ code yet
;; (except in `diredp-directories-within', which also is not used yet).
;;
(defun diredp-files-within (file-list accum &optional no-symlinks-p include-dirs-p predicate)
  "List of readable files in FILE-LIST, handling directories recursively.
FILE-LIST is a list of file names or a function that returns such.
If a function then invoke it with no args to get the list of files.

Accessible directories in the list of files are processed recursively
to include their files and the files in their subdirectories.  The
directories themselves are not included, unless optional arg
INCLUDE-DIRS-P is non-nil.  (Directories in
`icicle-ignored-directories' are skipped, if you use Icicles.
Otherwise, directories in `vc-directory-exclusion-list' are skipped.)

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
  ;; Bind `diredp-files-within-dirs-done' for use as a free var in `diredp-files-within-1'."
  (let ((diredp-files-within-dirs-done  ()))
    (nreverse (diredp-files-within-1 file-list accum no-symlinks-p include-dirs-p predicate))))

;; `diredp-files-within-dirs-done' is free here, bound in `diredp-files-within'.
(defun diredp-files-within-1 (file-list accum no-symlinks-p include-dirs-p predicate)
  "Helper for `diredp-files-within'."
  (let ((files  (if (functionp file-list) (funcall file-list) file-list))
        (res    accum)
        file)
    (when (and files  predicate) (setq files  (diredp-remove-if-not predicate files)))
    (while files
      (setq file  (car files))
      (unless (and no-symlinks-p  (file-symlink-p file))
        (if (file-directory-p file)
            ;; Skip directory if ignored, already treated, or inaccessible.
            (when (and (not (member (file-name-nondirectory file)
                                    (if (boundp 'icicle-ignored-directories)
                                        icicle-ignored-directories
                                      (and (boundp 'vc-directory-exclusion-list)
                                           vc-directory-exclusion-list))))
                       (not (member (file-truename file) diredp-files-within-dirs-done))
                       (file-accessible-directory-p file))
              (setq res  (diredp-files-within-1
                          (or (and (functionp file-list)
                                   (dired-buffers-for-dir ; Removes killed buffers.
                                    (expand-file-name file))
                                   (with-current-buffer
                                       (cdr (assoc (file-name-as-directory file) dired-buffers))
                                     (funcall file-list)))
                              (directory-files file 'FULL diredp-re-no-dot))
                          res
                          no-symlinks-p
                          include-dirs-p
                          predicate))
              (when include-dirs-p (push file res))
              (push (file-truename file) diredp-files-within-dirs-done))
          (when (file-readable-p file) (push file res))))
      (pop files))
    res))

(defun diredp-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (when (funcall pred x) (push x result)))
    (nreverse result)))

(when (> emacs-major-version 21)        ; Emacs 20 has no PREDICATE arg to `read-file-name'.
  (defun diredp-insert-as-subdir (child ancestor &optional in-dired-now-p)
    "Insert the current Dired dir into a Dired listing of an ancestor dir.
Ancestor means parent, grandparent, etc. at any level.
You are prompted for the ancestor directory.  
The ancestor Dired buffer is selected.

Markings and switches in the current Dired buffer are preserved for
the subdir listing in the ancestor Dired buffer.

Note: If you use Icicles, then you can use
`icicle-dired-insert-as-subdir' instead: it is a multi-command.  It
does the same thing, but it lets you insert any number of descendent
directories into a given ancestor-directory Dired buffer.

Non-interactively:
 Insert CHILD dir into Dired listing for ANCESTOR dir.

 Non-nil optional arg IN-DIRED-NOW-P means to use the current buffer
 as the Dired buffer from which to pick up markings and switches.
 Otherwise, pick them up from a Dired buffer for CHILD, if there is
 exactly one such buffer."
    (interactive (progn (diredp-ensure-mode)
                        (list default-directory
                              (completing-read
                               "Insert this dir into ancestor dir: "
                               (mapcar #'list (diredp-ancestor-dirs default-directory)))
                              t)))
    (let ((child-dired-buf  (if in-dired-now-p
                                (current-buffer)
                              (dired-buffers-for-dir (expand-file-name child))))
          (switches         nil)
          (marked           nil))
      (when (consp child-dired-buf)
        (setq child-dired-buf  (and (= 1 (length child-dired-buf))  (car child-dired-buf))))
      (when child-dired-buf
        (with-current-buffer child-dired-buf
          (setq switches  dired-actual-switches
                marked    (dired-remember-marks (point-min) (point-max)))))
      (dired-other-window ancestor)
      (dired-insert-subdir child switches)
      (when marked (let ((inhibit-read-only  t)) (dired-mark-remembered marked)))
      (set-buffer-modified-p nil))))

(defun diredp-ancestor-dirs (dir)
  "Return a list of the ancestor directories of directory DIR."
  (mapcar #'file-name-as-directory
          (diredp-maplist (lambda (dd) (mapconcat #'identity (reverse dd) "/"))
                          (cdr (nreverse (split-string dir "/" t))))))

(defun diredp-maplist (function list)
  "Map FUNCTION over LIST and its cdrs.
A simple, recursive version of the classic `maplist'."
  (and list  (cons (funcall function list) (maplist function (cdr list)))))



;;; Commands operating on marked at all levels below (recursively)

(defun diredp-get-confirmation-recursive ()
  "Get confirmation from user to act on all files here and below.
Raise an error if not confirmed.
Raise an error first if not in Dired mode."
  (diredp-ensure-mode)
  (unless (y-or-n-p "Act on ALL files (or all marked if any) in and UNDER this dir? ")
    (error "OK, canceled")))

;;;###autoload
(defun diredp-insert-subdirs (&optional switches) ; Bound to `M-i'
  "Insert the marked subdirectories.
Like using \\<dired-mode-map>`\\[dired-maybe-insert-subdir]' at each marked directory line."
  (interactive (list (and current-prefix-arg
                          (read-string "Switches for listing: "
                                       (or (and (boundp 'dired-subdir-switches)
                                                dired-subdir-switches)
                                           dired-actual-switches)))))
  (dolist (subdir  (dired-get-marked-files nil nil #'file-directory-p))
    (dired-maybe-insert-subdir subdir switches)))

;;;###autoload
(defun diredp-insert-subdirs-recursive (&optional ignore-marks-p) ; Bound to `M-+ M-i'
  "Insert the marked subdirs, including those in marked subdirs.
Like `diredp-insert-subdirs', but act recursively on subdirs.
The subdirs inserted are those that are marked in the current Dired
buffer, or all subdirs in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way (their marked
subdirs are inserted...).

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (dolist (subdir  (diredp-get-files ignore-marks-p #'file-directory-p 'INCLUDE-SUBDIRS-P))
    (dired-maybe-insert-subdir subdir)))

;;;###autoload
(defun diredp-do-shell-command-recursive (command &optional ignore-marks-p) ; Bound to `M-+ !'
  "Run shell COMMAND on the marked files, including those in marked subdirs.
Like `dired-do-shell-command', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive
   (progn (diredp-get-confirmation-recursive)
          (let* ((prompt  "! on *: ")
                 (cmd     (minibuffer-with-setup-hook
                              (lambda ()
                                (set (make-local-variable 'minibuffer-default-add-function)
                                     'minibuffer-default-add-dired-shell-commands))
                            (let ((dired-no-confirm  t))
                              (if (functionp 'dired-guess-shell-command)
                                  ;; Guess cmd based only on files marked in current (top) dir.
                                  (dired-guess-shell-command prompt (dired-get-marked-files t))
                                (read-shell-command prompt nil nil))))))
            (list cmd current-prefix-arg))))
  (dired-do-shell-command command nil (diredp-get-files ignore-marks-p)))

;;;###autoload
(defun diredp-do-symlink-recursive (&optional ignore-marks-p) ; Bound to `M-+ S'
  "Make symbolic links to marked files, including those in marked subdirs.
Like `dired-do-symlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (diredp-do-create-files-recursive #'make-symbolic-link "Symlink" ignore-marks-p))

(when (fboundp 'dired-do-relsymlink)
  (defun diredp-do-relsymlink-recursive (&optional ignore-marks-p) ; Bound to `M-+ Y'
    "Relative symlink all marked files, including those in marked subdirs into a dir.
Like `dired-do-relsymlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

For absolute symlinks, use \\[diredp-do-symlink-recursive]."
    (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
    (diredp-do-create-files-recursive #'dired-make-relative-symlink "RelSymLink"
                                      ignore-marks-p)))

;;;###autoload
(defun diredp-do-hardlink-recursive (&optional ignore-marks-p) ; Bound to `M-+ H'
  "Add hard links for marked files, including those in marked subdirs.
Like `dired-do-hardlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (diredp-do-create-files-recursive #'dired-hardlink "Hardlink" ignore-marks-p))

;;;###autoload
(defun diredp-do-print-recursive (&optional ignore-marks-p) ; Bound to `M-+ P'
  "Print the marked files, including those in marked subdirs.
Like `dired-do-print', but act recursively on subdirs to pick up the
files to print.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (let* ((file-list  (diredp-get-files ignore-marks-p))
	 (command    (dired-mark-read-string
                      "Print %s with: "
                      (mapconcat #'identity (cons lpr-command (if (stringp lpr-switches)
                                                                  (list lpr-switches)
                                                                lpr-switches))
                                 " ")
                      'print nil file-list)))
    (dired-run-shell-command (dired-shell-stuff-it command file-list nil))))

;;;###autoload
(defun diredp-image-dired-display-thumbs-recursive (&optional ignore-marks-p append do-not-pop)
                                        ; Bound to `M-+ C-t d'
  "Display thumbnails of marked files, including those in marked subdirs.
Like `image-dired-display-thumbs', but act recursively on subdirs.
Optional arguments APPEND and DO-NOT-POP are as for
`image-dired-display-thumbs'.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (unless (require 'image-dired nil t)
                        (error "This command requires library `image-dired.el' (Emacs 22+)"))
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg)))
  (let ((buf  (image-dired-create-thumbnail-buffer))
        thumb-name  files  dired-buf)
    (setq files      (diredp-get-files ignore-marks-p)
          dired-buf  (current-buffer))
    (with-current-buffer buf
      (let ((inhibit-read-only  t))
        (if append (goto-char (point-max)) (erase-buffer))
        (mapc (lambda (curr-file)
                (setq thumb-name  (image-dired-thumb-name curr-file))
                (if (and (not (file-exists-p thumb-name))
                         (not (= 0 (image-dired-create-thumb curr-file thumb-name))))
                    (message "Thumb could not be created for file %s" curr-file)
                  (image-dired-insert-thumbnail thumb-name curr-file dired-buf)))
              files))
      (case image-dired-line-up-method
        (dynamic      (image-dired-line-up-dynamic))
        (fixed        (image-dired-line-up))
        (interactive  (image-dired-line-up-interactive))
        (none         nil)
        (t            (image-dired-line-up-dynamic))))
    (if do-not-pop
        (display-buffer image-dired-thumbnail-buffer)
      (pop-to-buffer image-dired-thumbnail-buffer))))

;;;###autoload
(defun diredp-image-dired-tag-files-recursive (&optional ignore-marks-p) ; Bound to `M-+ C-t t'
  "Tag marked file(s) in dired, including those in marked subdirs
Like `image-dired-tag-files', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (unless (require 'image-dired nil t)
                        (error "This command requires library `image-dired.el' (Emacs 22+)"))
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg)))
  (let ((tag  (read-string "Tags to add (separate tags with a semicolon): ")))
    (image-dired-write-tags (mapcar (lambda (x) (cons x tag))
                                    (diredp-get-files ignore-marks-p)))))

;;;###autoload
(defun diredp-image-dired-delete-tag-recursive (&optional ignore-marks-p) ; Bound to `M-+ C-t r'
  "Remove tag for selected file(s), including those in marked subdirs.
Like `image-dired-delete-tag', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (unless (require 'image-dired nil t)
                        (error "This command requires library `image-dired.el' (Emacs 22+)"))
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg)))
  (image-dired-remove-tag (diredp-get-files ignore-marks-p) (read-string "Tag to remove: ")))

;;;###autoload
(defun diredp-image-dired-comment-files-recursive (&optional ignore-marks-p)
                                        ; Bound to `M-+ C-t c'
  "Add comment to marked files in dired, including those in marked subdirs.
Like `image-dired-dired-comment-files' but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (unless (require 'image-dired nil t)
                        (error "This command requires library `image-dired.el' (Emacs 22+)"))
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg)))
  (let ((comment  (image-dired-read-comment)))
    (image-dired-write-comments (mapcar (lambda (curr-file) (cons curr-file comment))
                                        (diredp-get-files ignore-marks-p)))))

(when (> emacs-major-version 22)
  (defun diredp-do-decrypt-recursive (&optional ignore-marks-p) ; Bound to `M-+ : d'
    "Decrypt marked files, including those in marked subdirs.
Like `epa-dired-do-decrypt', but act recursively on subdirs to pick up
the files to decrypt.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
    (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
    (dolist (file  (diredp-get-files ignore-marks-p))
      (epa-decrypt-file (expand-file-name file)))
    (revert-buffer))


  (defun diredp-do-verify-recursive (&optional ignore-marks-p) ; Bound to `M-+ : v'
    "Verify marked files, including those in marked subdirs.
Like `epa-dired-do-verify', but act recursively on subdirs to pick up
the files to verify.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
    (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
    (dolist (file  (diredp-get-files ignore-marks-p))
      (epa-verify-file (expand-file-name file)))
    (revert-buffer))

  (defun diredp-do-sign-recursive (&optional ignore-marks-p) ; Bound to `M-+ : s'
    "Sign marked files, including those in marked subdirs.
Like `epa-dired-do-sign', but act recursively on subdirs to pick up
the files to sign.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
    (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
    (dolist (file  (diredp-get-files ignore-marks-p))
      (epa-sign-file (expand-file-name file)
                     (epa-select-keys (epg-make-context) "Select keys for signing.
If none are selected, the default secret key is used.  ")
                     (y-or-n-p "Make a detached signature? ")))
    (revert-buffer))

  (defun diredp-do-encrypt-recursive (&optional ignore-marks-p) ; Bound to `M-+ : e'
    "Encrypt marked files, including those in marked subdirs.
Like `epa-dired-do-encrypt', but act recursively on subdirs to pick up
the files to encrypt.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
    (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
    (dolist (file  (diredp-get-files ignore-marks-p))
      (epa-encrypt-file (expand-file-name file)
                        (epa-select-keys (epg-make-context) "Select recipients for encryption.
If none are selected, symmetric encryption is performed.  ")))
    (revert-buffer)))

;;;###autoload
(defun diredp-do-bookmark-recursive (&optional ignore-marks-p prefix) ; Bound to `M-+ M-b'
  "Bookmark the marked files, including those in marked subdirs.
Like `diredp-do-bookmark', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list current-prefix-arg
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: ")))))
  (dolist (file  (diredp-get-files ignore-marks-p))
    (diredp-bookmark prefix file 'NO-MSG-P)))

;;;###autoload
(defun diredp-do-bookmark-in-bookmark-file-recursive (bookmark-file ; Bound to `M-+ C-M-B')
                                                      &optional prefix ignore-marks-p
                                                      bfile-bookmarkp)
  "Bookmark files here and below in BOOKMARK-FILE and save BOOKMARK-FILE.
Like `diredp-do-bookmark-in-bookmark-file', but act recursively on
subdirs.  The files included are those that are marked in the current
Dired buffer, or all files in the directory if none are marked.
Marked subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive
   (progn (diredp-get-confirmation-recursive)
          (let ((d-r-b-f-args  (diredp-read-bookmark-file-args)))
            (list (car d-r-b-f-args) (cadr d-r-b-f-args) (car (cddr d-r-b-f-args))))))
  (diredp-do-bookmark-in-bookmark-file bookmark-file prefix nil bfile-bookmarkp
                                       (diredp-get-files ignore-marks-p)))

;;;###autoload
(defun diredp-set-bookmark-file-bookmark-for-marked-recursive (bookmark-file
                                                               &optional prefix arg)
                                        ; Bound to `M-+ C-M-b'
  "Bookmark the marked files and create a bookmark-file bookmark for them.
Like `diredp-set-bookmark-file-bookmark-for-marked', but act
recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (diredp-read-bookmark-file-args)))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-do-bookmark-in-bookmark-file-recursive bookmark-file prefix arg
                                                 'CREATE-BOOKMARK-FILE-BOOKMARK))

;;;###autoload
(defun diredp-do-find-marked-files-recursive (&optional ignore-marks-p) ; Bound to `M-+ F'
  "Find marked files simultaneously, including those in marked subdirs.
Like `dired-do-find-marked-files', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (dired-simultaneous-find-file (diredp-get-files ignore-marks-p) nil))

(when (fboundp 'dired-do-isearch-regexp) ; Emacs 23+
  (defun diredp-do-isearch-recursive (&optional ignore-marks-p) ; Bound to `M-+ M-s a C-s'
    "Isearch the marked files, including those in marked subdirs."
    (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
    (multi-isearch-files (diredp-get-files ignore-marks-p)))

  (defun diredp-do-isearch-regexp-recursive (&optional ignore-marks-p) ; `M-+ M-s a C-M-s'
    "Regexp-Isearch the marked files, including those in marked subdirs."
    (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
    (multi-isearch-files-regexp (diredp-get-files ignore-marks-p))))

(defun diredp-do-search-recursive (regexp &optional ignore-marks-p) ; Bound to `M-+ A'
  "Regexp-search the marked files, including those in marked subdirs.
Stops when a match is found.
To continue searching for the next match, use `\\[tags-loop-continue]'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list (read-string "Search marked files (regexp): ")
                            current-prefix-arg)))
  (tags-search regexp '(diredp-get-files ignore-marks-p)))

;;;###autoload
(defun diredp-do-query-replace-regexp-recursive (from to &optional ignore-marks-p)
                                        ; Bound to `M-+ Q'
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   (let ((common
          (query-replace-read-args
           "Query replace regexp in marked files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer
                        buffer-read-only))
          (error "File `%s' is visited read-only" file))))
  (tags-query-replace from to nil '(diredp-get-files ignore-marks-p)))

;;;###autoload
(defun diredp-do-grep-recursive (command-args &optional ignore-marks-p) ; Bound to `M+ M-g'
  "Run `grep' on marked files, including those in marked subdirs.
Like `diredp-do-grep', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (unless (if (< emacs-major-version 22)
                                  grep-command
                                (and grep-command  (or (not grep-use-null-device)
                                                       (eq grep-use-null-device t))))
                        (grep-compute-defaults))
                      (list (diredp-do-grep-1 (diredp-get-files current-prefix-arg))
                            current-prefix-arg)))
  (grep command-args))

;;;###autoload
(defun diredp-marked-recursive (dirname &optional ignore-marked-p) ; Not bound to a key
  "Open Dired on marked files, including those in marked subdirs.
Like `diredp-marked', but act recursively on subdirs.

See `diredp-do-find-marked-files-recursive' for a description of the
files included.  In particular, if no files are marked here or in a
marked subdir, then all files in the directory are included."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list (cons (generate-new-buffer-name (buffer-name))
                                  (diredp-get-files current-prefix-arg)))))
  (dired dirname))

;;;###autoload
(defun diredp-marked-recursive-other-window (dirname &optional ignore-marked-p)
                                        ; Bound to `M-+ C-M-*'
  "Same as `diredp-marked-recursive', but uses a different window."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list (cons (generate-new-buffer-name (buffer-name))
                                  (diredp-get-files current-prefix-arg)))))
  (dired-other-window dirname))

;;;###autoload
(defun diredp-list-marked-recursive (&optional ignore-marks-p predicate)
  "List the files marked here and in marked subdirs, recursively.
See `diredp-do-find-marked-files-recursive' for a description of the
files included.  In particular, if no files are marked here or in a
marked subdir, then all files in the directory are included.

Non-interactively, non-nil PREDICATE is a file-name predicate.  List
only the files for which it returns non-nil."
  (interactive                          ; No need for `diredp-get-confirmation-recursive' here.
   (progn (diredp-ensure-mode) (list current-prefix-arg)))
  (let ((files  (diredp-get-files ignore-marks-p predicate)))
    (diredp-list-files files)))


(when (and (memq system-type '(windows-nt ms-dos))  (fboundp 'w32-browser))
  (defun diredp-multiple-w32-browser-recursive (&optional ignore-marked-p)
    "Run Windows apps for with marked files, including those in marked subdirs.
Like `dired-multiple-w32-browser', but act recursively on subdirs.

See `diredp-do-find-marked-files-recursive' for a description of the
files included.  In particular, if no files are marked here or in a
marked subdir, then all files in the directory are included."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg)))
    (let ((files  (diredp-get-files ignore-marked-p)))
      (while files
        (w32-browser (car files))
        (sleep-for w32-browser-wait-time)
        (setq files (cdr files))))))

;;;###autoload
(defun diredp-copy-filename-as-kill-recursive (&optional arg) ; Bound to `M-+ M-w'
  "Copy names of marked files here and in marked subdirs, to `kill-ring'.
The names are separated by a space.

Like `dired-copy-filename-as-kill', but act recursively on subdirs.
\(Do not copy subdir names themselves.)

With no prefix arg, use relative file names.
With a zero prefix arg, use absolute file names.
With a plain prefix arg (`C-u'), use names relative to the current
Dired directory.  (This might contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdir name instead - prefix
arg and marked files are ignored in this case.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way."
  (interactive                          ; No need for `diredp-get-confirmation-recursive' here.
   (progn (diredp-ensure-mode) (list current-prefix-arg)))
  (let* ((files   (mapcar (cond ((zerop (prefix-numeric-value arg)) #'identity)
                                ((consp arg) (lambda (fn) (concat (dired-current-directory t)
                                                                  (file-name-nondirectory fn))))
                                (t (lambda (fn) (file-name-nondirectory fn))))
                          (diredp-get-files)))
         (string  (mapconcat #'identity files " ")))
    (if (eq last-command 'kill-region)
        (kill-append string nil)
      (kill-new string))
    (message "%s" string)))

;;;###autoload
(defun diredp-capitalize-recursive (&optional ignore-marks-p) ; Bound to `M-+ % c'
  "Rename marked files, including in marked subdirs, by capitalizing them.
Like `diredp-capitalize', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (diredp-create-files-non-directory-recursive
   #'dired-rename-file #'capitalize "Rename by capitalizing:" ignore-marks-p))

;;;###autoload
(defun diredp-upcase-recursive (&optional ignore-marks-p) ; Bound to `M-+ % u'
  "Rename marked files, including in marked subdirs, making them uppercase.
Like `dired-upcase', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (diredp-create-files-non-directory-recursive
   #'dired-rename-file #'upcase "Rename to uppercase:" ignore-marks-p))

;;;###autoload
(defun diredp-downcase-recursive (&optional ignore-marks-p) ; Bound to `M-+ % l'
  "Rename marked files, including in marked subdirs, making them lowercase.
Like `dired-downcase', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (diredp-create-files-non-directory-recursive
   #'dired-rename-file #'downcase "Rename to lowercase:" ignore-marks-p))

;;;###autoload
(defun diredp-do-move-recursive (&optional ignore-marks-p) ; Bound to `M-+ R'
  "Move marked files, including in marked subdirs, to a given directory.
Like `dired-do-rename', but act recursively on subdirs to pick up the
files to move.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

This means move the marked files of marked subdirs and their marked
subdirs, etc.  It does not mean move or rename the subdirs themselves
recursively.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

Renames any buffers that are visiting the files.

The default suggested for the target directory depends on the value of
`dired-dwim-target', which see."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (diredp-do-create-files-recursive #'dired-rename-file "Move" ignore-marks-p))

;;;###autoload
(defun diredp-do-copy-recursive (&optional ignore-marks-p) ; Bound to `M-+ C'
  "Copy marked files, including in marked subdirs, to a given directory.
Like `dired-do-copy', but act recursively on subdirs to pick up the
files to copy.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

This means copy the marked files of marked subdirs and their marked
subdirs, etc.  It does not mean copy the subdirs themselves
recursively.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

Preserves the last-modified date when copying, unless
`dired-copy-preserve-time' is nil.

The default suggested for the target directory depends on the value of
`dired-dwim-target', which see.

This command copies symbolic links by creating new ones, like UNIX
command `cp -d'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (let ((dired-recursive-copies  nil))  ; Doesn't have to be nil, but let's not go overboard now.
    (diredp-do-create-files-recursive #'dired-copy-file "Copy" ignore-marks-p)))

(defun diredp-do-create-files-recursive (file-creator operation ignore-marks-p)
  "Create a new file for each marked file, including those in marked subdirs.
Like `dired-do-create-files', but act recursively on subdirs, and
always keep markings.
Prompts for the target directory, in which to create the files.
FILE-CREATOR OPERATION is as in `dired-create-files'.
Non-nil IGNORE-MARKS-P means ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (lexical-let* ((fn-list     (diredp-get-files ignore-marks-p))
                 (target-dir  (dired-dwim-target-directory))
                 (defaults    (and (fboundp 'dired-dwim-target-defaults) ; Emacs 23+
                                   (dired-dwim-target-defaults fn-list target-dir)))
                 (target      (expand-file-name
                               (if (fboundp 'minibuffer-with-setup-hook) ; Emacs 22+
                                   (minibuffer-with-setup-hook
                                    (lambda ()
                                      (set (make-local-variable 'minibuffer-default-add-function)
                                           nil)
                                      (setq minibuffer-default  defaults))
                                    (funcall (if (fboundp 'read-directory-name)
                                                 #'read-directory-name
                                               #'read-file-name)
                                             (concat operation " files to: ")
                                             default-directory default-directory))
                                 (funcall (if (fboundp 'read-directory-name)
                                              #'read-directory-name
                                            #'read-file-name)
                                          (concat operation "files to: ")
                                          default-directory default-directory)))))
    (unless (file-directory-p target) (error "Target is not a directory: `%s'" target))
    (dired-create-files
     file-creator operation fn-list
     #'(lambda (from) (expand-file-name (file-name-nondirectory from) target))
     ;; Hard-code `*' marker, or else it will be removed in lower dirs because the code uses
     ;; `dired-file-marker', which only works in the current Dired directory.
     ?*)))

(defun diredp-create-files-non-directory-recursive (file-creator basename-constructor
                                                    operation &optional ignore-marks-p)
  "Apply FILE-CREATOR + BASENAME-CONSTRUCTOR to non-dir part of marked names.
Like `dired-create-files-non-directory', but act recursively on subdirs.

The files acted on are those marked in the current Dired buffer, or
all files in the directory if none are marked.  Marked subdirectories
are handled recursively in the same way.

With non-nil IGNORE-MARKS-P, ignore all marks - include all files in
this Dired buffer and all subdirs, recursively."
  (let (rename-non-directory-query)
    (dired-create-files
     file-creator
     operation
     (diredp-get-files ignore-marks-p)
     #'(lambda (from)
         (let ((to  (concat (file-name-directory from)
                            (funcall basename-constructor (file-name-nondirectory from)))))
           (and (let ((help-form  (format "\
Type SPC or `y' to %s one file, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
                                          (downcase operation)
                                          (downcase operation))))
                  (dired-query 'rename-non-directory-query (concat operation " `%s' to `%s'")
                               (dired-make-relative from) (dired-make-relative to)))
                to)))
     ;; Hard-code `*' marker, or else it will be removed in lower dirs because the code uses
     ;; `dired-file-marker', which only works in the current Dired directory.
     ?*)))

(defun diredp-do-chxxx-recursive (attribute-name program op-symbol
                                  &optional ignore-marks-p default)
  "Change attributes of the marked files, including those in marked subdirs.
Refresh their file lines.

Like `dired-do-chxxx', but act recursively on subdirs.  The subdirs
acted on are those that are marked in the current Dired buffer, or all
subdirs in the directory if none are marked.  Marked subdirectories
are handled recursively in the same way.

ATTRIBUTE-NAME is a string describing the attribute to the user.
PROGRAM is the program used to change the attribute.
OP-SYMBOL is the type of operation (for use in `dired-mark-pop-up').
Non-nil IGNORE-MARKS-P means ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (let* ((this-buff      (current-buffer))
         (files          (diredp-get-files ignore-marks-p))
         (prompt         (concat "Change " attribute-name " of %s to: "))
         (new-attribute  (if (> emacs-major-version 22)
                             (dired-mark-read-string prompt nil op-symbol
                                                     ignore-marks-p files default)
                           (dired-mark-read-string prompt nil op-symbol ignore-marks-p files)))
         (operation      (concat program " " new-attribute))
         failures)
    (setq failures  (dired-bunch-files 10000 (function dired-check-process)
                                       (append (list operation program)
                                               (unless (string-equal new-attribute "")
                                                 (if (equal attribute-name "Timestamp")
                                                     (list "-t" new-attribute)
                                                   (list new-attribute)))
                                               (and (string-match "gnu" system-configuration)
                                                    '("--"))) ; --------------------------------
                                       files))
    (with-current-buffer this-buff
      (diredp-do-redisplay-recursive 'MSGP))
    (when failures (dired-log-summary (format "%s: error" operation) nil))))

;;;###autoload
(defun diredp-do-chmod-recursive (&optional ignore-marks-p) ; Bound to `M-+ M'
  "Change the mode of the marked files, including those in marked subdirs.
Symbolic modes like `g+w' are allowed.

Note that marked subdirs are not changed.  Their markings are used only
to indicate that some of their files are to be changed."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (let* ((files    (diredp-get-files ignore-marks-p))
         (modestr  (and (stringp (car files))
                        (nth 8 (file-attributes (car files)))))
         (default  (and (stringp modestr)
                        (string-match "^.\\(...\\)\\(...\\)\\(...\\)$" modestr)
                        (replace-regexp-in-string "-" "" (format "u=%s,g=%s,o=%s"
                                                                 (match-string 1 modestr)
                                                                 (match-string 2 modestr)
                                                                 (match-string 3 modestr)))))
         (modes    (if (> emacs-major-version 22)
                       (dired-mark-read-string
                        "Change mode of marked files here and below to: " nil 'chmod
                        nil files default)
                     (dired-mark-read-string
                      "Change mode of marked files here and below to: " nil 'chmod
                      nil files))))
    (when (equal modes "") (error "No file mode specified"))
    (dolist (file  files)
      (set-file-modes file (or (and (string-match "^[0-7]+" modes)
                                    (string-to-number modes 8))
                               (file-modes-symbolic-to-number modes (file-modes file)))))
    (diredp-do-redisplay-recursive 'MSGP)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-do-chgrp-recursive (&optional ignore-marks-p)
    "Change the group of the marked (or next ARG) files."
    (interactive "P")
    (diredp-do-chxxx-recursive "Group" "chgrp" 'chgrp ignore-marks-p)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-do-chown-recursive (&optional ignore-marks-p)
    "Change the owner of the marked (or next ARG) files."
    (interactive "P")
    (diredp-do-chxxx-recursive "Owner" dired-chown-program 'chown ignore-marks-p)))

;;;###autoload
(defun diredp-do-touch-recursive (&optional ignore-marks-p)
  "Change the timestamp of marked files, including those in marked subdirs.
This calls `touch'.  Like `dired-do-touch', but act recursively on
subdirs.  The subdirs inserted are those that are marked in the
current Dired buffer, or all subdirs in the directory if none are
marked.  Marked subdirectories are handled recursively in the same
way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg)))
  (diredp-do-chxxx-recursive "Timestamp" (if (boundp 'dired-touch-program)
                                             dired-touch-program ; Emacs 22+
                                           "touch")
                             'touch
                             ignore-marks-p
                             (format-time-string "%Y%m%d%H%M.%S" (current-time))))

;;;###autoload
(defun diredp-do-redisplay-recursive (&optional msgp)
  "Redisplay marked file lines, including those in marked subdirs.
Like `dired-do-redisplay' with no args, but act recursively on
subdirs."
  (interactive (progn (diredp-ensure-mode)
                      (unless (y-or-n-p "Act on all marked file lines in and UNDER this dir? ")
                        (error "OK, canceled"))
                      (list t)))
  (when msgp (message "Redisplaying..."))
  (dolist (dir  (cons default-directory
                      (diredp-get-files nil #'file-directory-p 'INCLUDE-SUBDIRS 'DONT-ASK)))
    (with-current-buffer (dired-noselect dir)
      ;; `message' is much faster than making `dired-map-over-marks' show progress
      (dired-uncache (if (consp dired-directory) (car dired-directory) dired-directory))
      (dired-map-over-marks
       (let ((fname                    (dired-get-filename))
             ;; Postpone readin hook till we map over all marked files (Bug#6810).
             (dired-after-readin-hook  nil))
         (message "Redisplaying... %s" fname)
         (dired-update-file-line fname))
       nil)
      (run-hooks 'dired-after-readin-hook)
      (dired-move-to-filename)))
  (when msgp (message "Redisplaying...done")))


;;; `diredp-marked(-other-window)' tries to treat SWITCHES, but SWITCHES seems to be ignored
;;; by `dired' when the DIRNAME arg is a cons, at least on MS Windows.  I filed Emacs bug #952
;;; on 2008-09-10, but this doesn't work in Emacs 20, 21, 22, or 23, so I don't know if it will
;;; ever be fixed.  If it is declared a non-bug and it doesn't work on any platforms, then I'll
;;; remove SWITCHES here, alas.

;;;###autoload
(defun diredp-marked (dirname &optional n switches) ; Not bound
  "Open Dired on only the marked files or the next N files.
With a non-zero numeric prefix arg N, use the next abs(N) files.
A plain (`C-u'), zero, or negative prefix arg prompts for listing
switches as in command `dired'.

Note that the marked files can include files in inserted
subdirectories, so the Dired buffer that is opened can contain files
from multiple directories in the same tree."
  (interactive
   (progn
     (diredp-ensure-mode)
     (let ((num  (and current-prefix-arg (atom current-prefix-arg)
                      (not (zerop (prefix-numeric-value current-prefix-arg)))
                      (abs (prefix-numeric-value current-prefix-arg)))))
       (list (cons (generate-new-buffer-name (buffer-name)) (dired-get-marked-files t num))
             num
             (and current-prefix-arg    ; Switches
                  (or (consp current-prefix-arg) (< (prefix-numeric-value current-prefix-arg) 0))
                  (read-string "Dired listing switches: " dired-listing-switches))))))
  (unless (or n (save-excursion (goto-char (point-min))
                                (and (re-search-forward (dired-marker-regexp) nil t)
                                     (re-search-forward (dired-marker-regexp) nil t))))
    (error "No marked files"))
  (dired dirname switches))

;;;###autoload
(defun diredp-marked-other-window (dirname &optional n switches) ; Bound to `C-M-*'
  "Same as `diredp-marked', but uses a different window."
  (interactive
   (progn
     (diredp-ensure-mode)
     (let ((num  (and current-prefix-arg (atom current-prefix-arg)
                      (not (zerop (prefix-numeric-value current-prefix-arg)))
                      (abs (prefix-numeric-value current-prefix-arg)))))
       (list (cons (generate-new-buffer-name (buffer-name)) (dired-get-marked-files t num))
             num
             (and current-prefix-arg    ; Switches
                  (or (consp current-prefix-arg) (< (prefix-numeric-value current-prefix-arg) 0))
                  (read-string "Dired listing switches: " dired-listing-switches))))))
  (unless (or n (save-excursion (goto-char (point-min))
                                (and (re-search-forward (dired-marker-regexp) nil t)
                                     (re-search-forward (dired-marker-regexp) nil t))))
    (error "No marked files"))
  (dired-other-window dirname switches))


;; Similar to `dired-mark-extension' in `dired-x.el'.
;; The difference is that this uses prefix arg to unmark, not to determine the mark character.
;;;###autoload
(defun diredp-mark/unmark-extension (extension &optional unmark-p) ; Bound to `* .'
  "Mark all files with a certain EXTENSION for use in later commands.
A `.' is not automatically prepended to the string entered.
Non-nil prefix argument UNMARK-P means unmark instead of mark."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                                    "ing extension: "))
         current-prefix-arg))
  (or (listp extension)  (setq extension  (list extension)))
  (dired-mark-files-regexp (concat ".";; Do not match names with nothing but an extension
                                   "\\("
                                   (mapconcat 'regexp-quote extension "\\|")
                                   "\\)$")
                           (and current-prefix-arg ?\040)))

(defun diredp-mark-files-tagged-all/none (tags &optional none-p unmarkp prefix)
  "Mark or unmark files tagged with all or none of TAGS.
TAGS is a list of strings, the tag names.
NONEP non-nil means mark/unmark files that have none of the TAGS.
UNMARKP non-nil means unmark; nil means mark.
PREFIX non-nil is the prefix of the autofile bookmarks to check.

As a special case, if TAGS is empty, then mark or unmark the files
that have any tags at all, or if NONEP is non-nil then mark or unmark
those that have no tags at all."
  (let ((dired-marker-char  (if unmarkp ?\040 dired-marker-char)))
    (dired-mark-if (and (not (looking-at dired-re-dot))  (not (eolp))
                        (let* ((fname     (dired-get-filename nil t))
                               (bmk       (and fname (bmkp-get-autofile-bookmark
                                                      fname nil prefix)))
                               (btgs      (and bmk (bmkp-get-tags bmk)))
                               (presentp  nil)
                               (allp      (and btgs
                                               (catch 'diredp-m-f-t-an
                                                 (dolist (tag  tags)
                                                   (setq presentp  (assoc-default
                                                                    tag btgs nil t))
                                                   (unless (if none-p (not presentp) presentp)
                                                     (throw 'diredp-m-f-t-an nil)))
                                                 t))))
                          (if (null tags)
                              (if none-p (not btgs) btgs)
                            allp)))
                   (if none-p "no-tags-matching file" "all-tags-matching file"))))

(defun diredp-mark-files-tagged-some/not-all (tags &optional notallp unmarkp prefix)
  "Mark or unmark files tagged with any or not all of TAGS.
TAGS is a list of strings, the tag names.
NOTALLP non-nil means mark/unmark files that do not have all TAGS.
UNMARKP non-nil means unmark; nil means mark.
PREFIX non-nil is the prefix of the autofile bookmarks to check.

As a special case, if TAGS is empty, then mark or unmark the files
that have any tags at all, or if NOTALLP is non-nil then mark or
unmark those that have no tags at all."
  (let ((dired-marker-char  (if unmarkp ?\040 dired-marker-char)))
    (dired-mark-if (and (not (looking-at dired-re-dot))  (not (eolp))
                        (let* ((fname     (dired-get-filename nil t))
                               (bmk       (and fname (bmkp-get-autofile-bookmark
                                                      fname nil prefix)))
                               (btgs      (and bmk (bmkp-get-tags bmk)))
                               (presentp  nil)
                               (allp      (and btgs
                                               (catch 'diredp-m-f-t-sna
                                                 (dolist (tag  tags)
                                                   (setq presentp  (assoc-default
                                                                    tag btgs nil t))
                                                   (when (if notallp (not presentp) presentp)
                                                     (throw 'diredp-m-f-t-sna t)))
                                                 nil))))
                          (if (null tags)
                              (if notallp (not btgs) btgs)
                            allp)))
                   (if notallp "some-tags-not-matching file" "some-tags-matching file"))))

;;;###autoload
(defun diredp-mark-files-tagged-all (tags &optional none-p prefix) ; `T m *'
  "Mark all files that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, mark all that are *not* tagged with *any* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags none-p nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-none (tags &optional allp prefix) ; `T m ~ +'
  "Mark all files that are not tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 no tags at all.
With a prefix arg, mark all that are tagged with *each* tag in TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags (not allp) nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-some (tags &optional somenotp prefix) ; `T m +'
  "Mark all files that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, mark all that are *not* tagged with *all* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-some/not-all tags somenotp nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-not-all (tags &optional somep prefix) ; `T m ~ *'
  "Mark all files that are not tagged with *all* TAGS.
As a special case, if TAGS is empty, then mark the files that have
 no tags at all.
With a prefix arg, mark all that are tagged with *some* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-some/not-all tags (not somep) nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-regexp (regexp &optional notp prefix) ; `T m %'
  "Mark files that have at least one tag that matches REGEXP.
With a prefix arg, mark all that are tagged but have no matching tags.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (read-string "Regexp: ")
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (dired-mark-if (and (not (looking-at dired-re-dot))  (not (eolp))
                      (lexical-let* ((fname  (dired-get-filename nil t))
                                     (bmk    (and fname (bmkp-get-autofile-bookmark
                                                         fname nil prefix)))
                                     (btgs   (and bmk (bmkp-get-tags bmk)))
                                     (anyp   (and btgs  (bmkp-some
                                                         #'(lambda (tag)
                                                             (string-match regexp
                                                                           (bmkp-tag-name tag)))
                                                         btgs))))
                        (and btgs (if notp (not anyp) anyp))))
                 "some-tag-matching-regexp file"))

;;;###autoload
(defun diredp-unmark-files-tagged-regexp (regexp &optional notp prefix) ; `T u %'
  "Unmark files that have at least one tag that matches REGEXP.
With a prefix arg, unmark all that are tagged but have no matching tags.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (read-string "Regexp: ")
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (let ((dired-marker-char  ?\040))
    (dired-mark-if (and (not (looking-at dired-re-dot))  (not (eolp))
                        (lexical-let* ((fname  (dired-get-filename nil t))
                                       (bmk    (and fname (bmkp-get-autofile-bookmark
                                                           fname nil prefix)))
                                       (btgs   (and bmk (bmkp-get-tags bmk)))
                                       (anyp   (and btgs (bmkp-some
                                                          #'(lambda (tag)
                                                              (string-match regexp
                                                                            (bmkp-tag-name tag)))
                                                          btgs))))
                          (and btgs (if notp (not anyp) anyp))))
                   "some-tag-matching-regexp file")))

;;;###autoload
(defun diredp-unmark-files-tagged-all (tags &optional none-p prefix) ; `T u *'
  "Unmark all files that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, unmark all that are *not* tagged with *any* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags none-p 'UNMARK prefix))

;;;###autoload
(defun diredp-unmark-files-tagged-none (tags &optional allp prefix) ; `T u ~ +'
  "Unmark all files that are *not* tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 no tags at all.
With a prefix arg, unmark all that are tagged with *each* tag in TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags (not allp) 'UNMARK prefix))

;;;###autoload
(defun diredp-unmark-files-tagged-some (tags &optional somenotp prefix) ; `T u +'
  "Unmark all files that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 any tags at all.
With a prefix arg, unmark all that are *not* tagged with *all* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-some/not-all tags somenotp 'UNMARK prefix))

;;;###autoload
(defun diredp-unmark-files-tagged-not-all (tags &optional somep prefix) ; `T u ~ *'
  "Unmark all files that are *not* tagged with *all* TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 no tags at all.
With a prefix arg, unmark all that are tagged with *some* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive
   (list (and (fboundp 'bmkp-read-tags-completing) (bmkp-read-tags-completing))
         current-prefix-arg
         (and diredp-prompt-for-bookmark-prefix-flag
              (read-string "Prefix for autofile bookmark names: "))))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-some/not-all tags (not somep) 'UNMARK prefix))

;;;###autoload
(defun diredp-do-tag (tags &optional prefix arg) ; `T > +'
  "Tag the marked (or the next prefix argument) files.
You need library `bookmark+.el' to use this command.

Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.  Completion is lax: you are
not limited to existing tags.

TAGS is a list of strings.  PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (bmkp-read-tags-completing)
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for autofile bookmark name: "))
                            current-prefix-arg)))
  (dired-map-over-marks-check (lexical-let ((pref  prefix)) #'(lambda () (diredp-tag tags pref)))
                              arg 'tag (diredp-fewer-than-2-files-p arg)))

(defun diredp-tag (tags &optional prefix)
  "Add tags to the file or directory named on the current line.
You need library `bookmark+.el' to use this function.
The bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Return nil for success, file name otherwise."
  (bookmark-maybe-load-default-file)
  (let ((file  (dired-get-file-for-visit))
        failure)
    (condition-case err
        (bmkp-autofile-add-tags file tags nil prefix)
      (error (setq failure  (error-message-string err))))
    (if (not failure)
        nil                             ; Return nil for success.
      (dired-log failure)
      (dired-make-relative file))))     ; Return file name for failure.

;;;###autoload
(defun diredp-mouse-do-tag (event)      ; Not bound
  "In Dired, add some tags to this file.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-tag (bmkp-read-tags-completing) prefix))
                                1 'tag t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-do-untag (tags &optional prefix arg) ; `T > -'
  "Remove some tags from the marked (or the next prefix arg) files.
You need library `bookmark+.el' to use this command.

Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.  Completion is lax: you are
not limited to existing tags.

TAGS is a list of strings.  PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (bmkp-read-tags-completing)
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            current-prefix-arg)))
  (dired-map-over-marks-check (lexical-let ((pref  prefix))
                                #'(lambda () (diredp-untag tags pref)))
                              arg 'untag (diredp-fewer-than-2-files-p arg)))

(defun diredp-untag (tags &optional prefix)
  "Remove some tags from the file or directory named on the current line.
You need library `bookmark+.el' to use this function.
The bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Return nil for success, file name otherwise."
  (bookmark-maybe-load-default-file)
  (let ((file  (dired-get-file-for-visit))
        failure)
    (condition-case err
        (bmkp-autofile-remove-tags file tags nil prefix)
      (error (setq failure  (error-message-string err))))
    (if (not failure)
        nil                             ; Return nil for success.
      (dired-log failure)
      (dired-make-relative file))))     ; Return file name for failure.

;;;###autoload
(defun diredp-mouse-do-untag (event)    ; Not bound
  "In Dired, remove some tags from this file.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (lexical-let* ((bmk   (bmkp-get-autofile-bookmark  (dired-get-filename) nil prefix))
                   (btgs  (and bmk (bmkp-get-tags bmk))))
      (unless btgs (error "File has no tags to remove"))
      (dired-map-over-marks-check
       #'(lambda () (diredp-untag (bmkp-read-tags-completing btgs) prefix)) 1 'untag t)))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-do-remove-all-tags (&optional prefix arg) ; `T > 0'
  "Remove all tags from the marked (or the next prefix arg) files.
You need library `bookmark+.el' to use this command.

PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            current-prefix-arg)))
  (lexical-let ((pref  prefix))
    (dired-map-over-marks-check #'(lambda () (diredp-remove-all-tags pref)) arg 'remove-all-tags
                                (diredp-fewer-than-2-files-p arg))))

(defun diredp-remove-all-tags (&optional prefix)
  "Remove all tags from the file or directory named on the current line.
You need library `bookmark+.el' to use this function.
The bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Return nil for success, file name otherwise."
  (bookmark-maybe-load-default-file)
  (let ((file  (dired-get-file-for-visit))
        failure)
    (condition-case err
        (bmkp-remove-all-tags (bmkp-autofile-set file nil prefix))
      (error (setq failure  (error-message-string err))))
    (if (not failure)
        nil                             ; Return nil for success.
      (dired-log failure)
      (dired-make-relative file))))     ; Return file name for failure.

;;;###autoload
(defun diredp-mouse-do-remove-all-tags (event) ; Not bound
  "In Dired, remove all tags from the marked (or next prefix arg) files.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-remove-all-tags prefix))
                                1 'remove-all-tags t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-do-paste-add-tags (&optional prefix arg) ; `T > p', `T > C-y'
  "Add previously copied tags to the marked (or next prefix arg) files.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for autofile bookmark name: "))
                            current-prefix-arg)))
  (dired-map-over-marks-check (lexical-let ((pref  prefix))
                                #'(lambda () (diredp-paste-add-tags pref)))
                              arg 'paste-add-tags
                              (diredp-fewer-than-2-files-p arg)))

(defun diredp-paste-add-tags (&optional prefix)
  "Add previously copied tags to the file or directory on the current line.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this function.
The bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Return nil for success, file name otherwise."
  (bookmark-maybe-load-default-file)
  (let ((file  (dired-get-file-for-visit))
        failure)
    (condition-case err
        (bmkp-autofile-add-tags file bmkp-copied-tags nil prefix)
      (error (setq failure  (error-message-string err))))
    (if (not failure)
        nil                             ; Return nil for success.
      (dired-log failure)
      (dired-make-relative file))))     ; Return file name for failure.

;;;###autoload
(defun diredp-mouse-do-paste-add-tags (event) ; Not bound
  "In Dired, add previously copied tags to this file.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-paste-add-tags prefix))
                                1 'paste-add-tags t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-do-paste-replace-tags (&optional prefix arg) ; `T > q'
  "Replace tags for marked (or next prefix arg) files with copied tags.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for autofile bookmark name: "))
                            current-prefix-arg)))
  (dired-map-over-marks-check (lexical-let ((pref  prefix))
                                #'(lambda () (diredp-paste-replace-tags pref)))
                              arg 'paste-replace-tags (diredp-fewer-than-2-files-p arg)))

(defun diredp-paste-replace-tags (&optional prefix)
  "Replace tags for this file or dir with tags copied previously.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this function.
The bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Return nil for success, file name otherwise."
  (bookmark-maybe-load-default-file)
  (let ((file  (dired-get-file-for-visit))
        failure)
    (condition-case err
        (progn (bmkp-remove-all-tags (bmkp-autofile-set file nil prefix))
               (bmkp-autofile-add-tags file bmkp-copied-tags nil prefix))
      (error (setq failure  (error-message-string err))))
    (if (not failure)
        nil                             ; Return nil for success.
      (dired-log failure)
      (dired-make-relative file))))

;;;###autoload
(defun diredp-mouse-do-paste-replace-tags (event) ; Not bound
  "In Dired, replace tags for this file with tags copied previously.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-paste-replace-tags prefix))
                                1 'paste-replace-tags t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-do-set-tag-value (tag value &optional prefix arg) ; `T > v'
  "Set TAG value to VALUE, for the marked (or next prefix arg) files.
This does not change the TAG name.
You need library `bookmark+.el' to use this command.

PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (bmkp-read-tag-completing)
                            (read (read-string "Value: "))
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            current-prefix-arg)))
  (dired-map-over-marks-check (lexical-let ((tg    tag)
                                            (val   value)
                                            (pref  prefix))
                                #'(lambda () (diredp-set-tag-value tg val pref)))
                              arg 'set-tag-value (diredp-fewer-than-2-files-p arg)))

(defun diredp-set-tag-value (tag value &optional prefix)
  "Set TAG value to VALUE for this file or directory.
This does not change the TAG name.
You need library `bookmark+.el' to use this function.
The bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Return nil for success, file name otherwise."
  (bookmark-maybe-load-default-file)
  (let ((file  (dired-get-file-for-visit))
        failure)
    (condition-case err
        (bmkp-set-tag-value (bmkp-autofile-set file nil prefix) tag value)
      (error (setq failure  (error-message-string err))))
    (if (not failure)
        nil                             ; Return nil for success.
      (dired-log failure)
      (dired-make-relative file))))     ; Return file name for failure.

;;;###autoload
(defun diredp-mouse-do-set-tag-value (event) ; Not bound
  "In Dired, set the value of a tag for this file.
This does not change the tag name.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-set-tag-value (bmkp-read-tag-completing)
                                                                   (read (read-string "Value: "))
                                                                   prefix))
                                1 'set-tag-value t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-do-bookmark (&optional prefix arg) ; Bound to `M-b'
  "Bookmark the marked (or the next prefix argument) files.
Each bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Interactively, you are prompted for the PREFIX if
 `diredp-prompt-for-bookmark-prefix-flag' is non-nil.
The bookmarked position is the beginning of the file.
If you use library `bookmark+.el' then the bookmark is an autofile.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (progn (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            current-prefix-arg)))
  (dired-map-over-marks-check (lexical-let ((pref  prefix))
                                #'(lambda () (diredp-bookmark pref nil 'NO-MSG-P)))
                              arg 'bookmark (diredp-fewer-than-2-files-p arg)))

;;;###autoload
(defun diredp-mouse-do-bookmark (event) ; Not bound
  "In Dired, bookmark this file.  See `diredp-do-bookmark'."
  (interactive "e")
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-bookmark prefix nil 'NO-MSG-P))
                                nil 'bookmark t))
  (dired-previous-line 1))

(defun diredp-bookmark (&optional prefix file no-msg-p)
  "Bookmark the file or directory FILE.
If you use library `bookmark+.el' then the bookmark is an autofile.
Return nil for success or the file name otherwise.

The bookmark name is the (non-directory) file name, prefixed by
 optional arg PREFIX (a string) if non-nil.

FILE defaults to the file name on the current Dired line.

Non-nil optional arg NO-MSG-P means do not show progress messages."
  (bookmark-maybe-load-default-file)
  (let ((fil      (or file  (dired-get-file-for-visit)))
        (failure  nil))
    (condition-case err
        (if (fboundp 'bmkp-autofile-set)
            (bmkp-autofile-set fil nil prefix)
          (let ((bookmark-make-record-function
                 (cond ((and (require 'image nil t) (require 'image-mode nil t)
                             (condition-case nil (image-type fil) (error nil)))
                        ;; Last two lines of function are from `image-bookmark-make-record'.
                        ;; But don't use that directly, because it uses
                        ;; `bookmark-make-record-default', which gets nil for `filename'.

                        ;; NEED to keep this code sync'd with `bmkp-make-record-for-target-file'.
                        (lambda ()
                          `((filename   . ,fil)
                            (position   . 0)
                            (image-type . ,(image-type fil))
                            (handler    . image-bookmark-jump))))
                       (t
                        (lambda ()
                          `((filename . ,fil)
                            (position . 0)))))))
            (bookmark-store (concat prefix (file-name-nondirectory fil))
                            (cdr (bookmark-make-record))
                            nil
                            no-msg-p)))
      (error (setq failure  (error-message-string err))))
    (if (not failure)
        nil                             ; Return nil for success.
      (if (fboundp 'bmkp-autofile-set)
          (dired-log failure)
        (dired-log "Failed to create bookmark for `%s':\n%s\n" fil failure))
      (dired-make-relative fil))))      ; Return file name for failure.

;;;###autoload
(defun diredp-set-bookmark-file-bookmark-for-marked (bookmark-file ; Bound to `C-M-b'
                                                     &optional prefix arg)
  "Bookmark the marked files and create a bookmark-file bookmark for them.
The bookmarked position is the beginning of the file.
Jumping to the bookmark-file bookmark loads the set of file bookmarks.
You need library `bookmark+.el' to use this command.

Each bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Interactively, you are prompted for PREFIX if
 `diredp-prompt-for-bookmark-prefix-flag' is non-nil.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

You are also prompted for the bookmark file, BOOKMARK-FILE.  The
default is `.emacs.bmk' in the current directory, but you can enter
any file name, anywhere.

The marked-file bookmarks are added to file BOOKMARK-FILE, but this
command does not make BOOKMARK-FILE the current bookmark file.  To
make it current, just jump to the bookmark-file bookmark created by
this command.  That bookmark (which bookmarks BOOKMARK-FILE) is
defined in that current bookmark file.

Example:

 Bookmark file `~/.emacs.bmk' is current before invoking this command.
 The current (Dired) directory is `/foo/bar'.
 The marked files are bookmarked in the (possibly new) bookmark file
   `/foo/bar/.emacs.bmk'.
 The bookmarks for the marked files have names prefixed by `FOOBAR '.
 The name of the bookmark-file bookmark is `Foobar Files'.
 Bookmark `Foobar Files' is itself in bookmark file `~/.emacs.bmk'.
 Bookmark file `~/.emacs.bmk' is current after invoking this command.

You are prompted for the name of the bookmark-file bookmark, the
BOOKMARK-FILE for the marked-file bookmarks, and a PREFIX string for
each of the marked-file bookmarks.

See also command `diredp-do-bookmark-in-bookmark-file'."
  (interactive (diredp-read-bookmark-file-args))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-do-bookmark-in-bookmark-file bookmark-file prefix arg 'CREATE-BOOKMARK-FILE-BOOKMARK))

;;;###autoload
(defun diredp-do-bookmark-in-bookmark-file (bookmark-file ; Bound to `C-M-S-b' (`C-M-B')
                                            &optional prefix arg bfile-bookmarkp files)
  "Bookmark files in BOOKMARK-FILE and save BOOKMARK-FILE.
The files bookmarked are the marked files, by default.
The bookmarked position is the beginning of the file.
You are prompted for BOOKMARK-FILE.  The default is `.emacs.bmk' in
the current directory, but you can enter any file name, anywhere.
You need library `bookmark+.el' to use this command.

The marked files are bookmarked in file BOOKMARK-FILE, but this
command does not make BOOKMARK-FILE the current bookmark file.  To
make it current, use `\\[bmkp-switch-bookmark-file]' (`bmkp-switch-bookmark-file').

Each bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Interactively, you are prompted for PREFIX if
 `diredp-prompt-for-bookmark-prefix-flag' is non-nil.

Interactively, a prefix argument ARG specifies the files to use
instead of those marked.

 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

See also command `diredp-set-bookmark-file-bookmark-for-marked'.

Non-interactively:

 * Non-nil BFILE-BOOKMARKP means create a bookmark-file bookmark for
   BOOKMARK-FILE.
 * Non-nil FILES is the list of files to bookmark."
  (interactive (diredp-read-bookmark-file-args))
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (let ((bfile-exists-p  (file-readable-p bookmark-file)))
    (unless bfile-exists-p (bmkp-empty-file bookmark-file))
    (unless bmkp-current-bookmark-file (setq bmkp-current-bookmark-file  bookmark-default-file))
    (let ((old-bmkp-current-bookmark-file  bmkp-current-bookmark-file))
      (unwind-protect
           (progn (bmkp-switch-bookmark-file bookmark-file) ; Changes `*-current-bookmark-file'.
                  (if files
                      (dolist (file  files)  (diredp-bookmark prefix file 'NO-MSG-P))
                    (dired-map-over-marks-check
                     (lexical-let ((pref  prefix))
                       #'(lambda () (diredp-bookmark pref nil 'NO-MSG-P)))
                     arg 'bookmark (diredp-fewer-than-2-files-p arg)))
                  (bookmark-save)
                  (unless bfile-exists-p (revert-buffer)))
        (unless (bmkp-same-file-p old-bmkp-current-bookmark-file  bmkp-current-bookmark-file)
          (bmkp-switch-bookmark-file old-bmkp-current-bookmark-file 'NO-MSG))))
    (when bfile-bookmarkp (bmkp-set-bookmark-file-bookmark bookmark-file))))

(defun diredp-read-bookmark-file-args ()
  "Read args for `diredp-do-bookmark-in-bookmark-file' and similar."
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (diredp-ensure-mode)
  (list (let* ((insert-default-directory  t)
               (bmk-file                  (expand-file-name
                                           (read-file-name
                                            "Use bookmark file (default is in CURRENT dir): " nil
                                            (if (or (> emacs-major-version 23)
                                                    (and (= emacs-major-version 23)
                                                         (> emacs-minor-version 1)))
                                                (list ".emacs.bmk"
                                                      bookmark-default-file)
                                              ".emacs.bmk")))))
          bmk-file)
        (and diredp-prompt-for-bookmark-prefix-flag
             (read-string "Prefix for autofile bookmark names: "))
        current-prefix-arg))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Allows for consp `dired-directory' too.
;;
(defun dired-buffers-for-dir (dir &optional file)
  "Return a list of buffers that dired DIR (top level or in-situ subdir).
If FILE is non-nil, include only those whose wildcard pattern (if any)
matches FILE.
The list is in reverse order of buffer creation, most recent last.
As a side effect, killed dired buffers for DIR are removed from
`dired-buffers'."
  (setq dir (file-name-as-directory dir))
  (let (result buf)
    (dolist (elt  dired-buffers)
      (setq buf  (cdr elt))
      (cond ((null (buffer-name buf))   ; Buffer is killed - clean up.
             (setq dired-buffers  (delq elt dired-buffers)))
            ((dired-in-this-tree dir (car elt))
             (with-current-buffer buf
               (and (assoc dir dired-subdir-alist)
                    (or (null file)
                        (if (stringp dired-directory)
                            ;; Allow for consp `dired-directory' too.
                            (let ((wildcards  (file-name-nondirectory
                                               (if (consp dired-directory)
                                                   (car dired-directory)
                                                 dired-directory))))
                              (or (= 0 (length wildcards))
                                  (string-match (dired-glob-regexp wildcards) file)))
                          (member (expand-file-name file dir) (cdr dired-directory))))
                    (setq result  (cons buf result)))))))
    result))


;; These functions let you use the file on the current line as the default.
;; They are useful only in Emacs 22 or later.
;;
;; However, if you use library `files+.el', you need not use these commands explicitly,
;; because that library redefines `find-file-read-args' to do the same thing, in Dired mode.
;; These are provided here in case you want to bind them directly - for example, in case your
;; code does not use `find-file-read-args'.
;;
(when (fboundp 'dired-get-file-for-visit) ; Defined in Emacs 22.
  (defun diredp-find-a-file (filename &optional wildcards) ; Not bound
    "`find-file', but use file on current line as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file filename wildcards))

  (defun diredp-find-a-file-other-frame (filename &optional wildcards) ; Not bound
    "`find-file-other-frame', but use file under cursor as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file-other-frame filename wildcards))

  (defun diredp-find-a-file-other-window (filename &optional wildcards) ; Not bound
    "`find-file-other-window', but use file under cursor as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file-other-window filename wildcards))

  (defun diredp-find-a-file-read-args (prompt mustmatch) ; Not bound
    (list (lexical-let ((find-file-default  (abbreviate-file-name (dired-get-file-for-visit))))
            (minibuffer-with-setup-hook #'(lambda ()
                                            (setq minibuffer-default  find-file-default))
                                        (read-file-name prompt nil default-directory mustmatch)))
          t)))

;; Define these for Emacs 20 and 21.
(unless (fboundp 'dired-get-file-for-visit) ; Defined in Emacs 22.
  (defun dired-get-file-for-visit () ; Not bound
    "Get the current line's file name, with an error if file does not exist."
    (interactive)
    ;; We pass t for second arg so that we don't get error for `.' and `..'.
    (let ((raw  (dired-get-filename nil t))
          file-name)
      (unless raw (error "No file on this line"))
      (setq file-name  (file-name-sans-versions raw t))
      (if (file-exists-p file-name)
          file-name
        (if (file-symlink-p file-name)
            (error "File is a symlink to a nonexistent target")
          (error "File no longer exists; type `g' to update Dired buffer")))))

  (defun dired-find-alternate-file () ; Not bound
    "In Dired, visit this file or directory instead of the dired buffer."
    (interactive)
    (set-buffer-modified-p nil)
    (find-alternate-file (dired-get-file-for-visit))))

;;;###autoload
(defun diredp-find-file-reuse-dir-buffer () ; Not bound
  "Like `dired-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' if the target is not a directory."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((file  (dired-get-file-for-visit)))
    (if (file-directory-p file) (find-alternate-file file) (find-file file))))

;;;###autoload
(defun diredp-mouse-find-file-reuse-dir-buffer (event) ; Not bound
  "Like `diredp-mouse-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' if the target is not a directory."
  (interactive "e")
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion (goto-char (posn-point (event-end event)))
                      (setq file  (dired-get-file-for-visit))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (find-alternate-file (file-name-sans-versions file t))
      (find-file (file-name-sans-versions file t)))))

;;;###autoload
(defalias 'toggle-diredp-find-file-reuse-dir 'diredp-toggle-find-file-reuse-dir)
;;;###autoload
(defun diredp-toggle-find-file-reuse-dir (force-p) ; Not bound
  "Toggle whether Dired `find-file' commands reuse directories.
A prefix arg specifies directly whether or not to reuse.
 If its numeric value is non-negative then reuse; else do not reuse.

To set the behavior as a preference (default behavior), put this in
your ~/.emacs, where VALUE is 1 to reuse or -1 to not reuse:

 (diredp-toggle-find-file-reuse-dir VALUE)"
  (interactive "P")
  (if force-p                           ; Force.
      (if (natnump (prefix-numeric-value force-p))
          (diredp-make-find-file-keys-reuse-dirs)
        (diredp-make-find-file-keys-not-reuse-dirs))
    (if (where-is-internal 'dired-find-file dired-mode-map 'ascii)
        (diredp-make-find-file-keys-reuse-dirs)
      (diredp-make-find-file-keys-not-reuse-dirs))))

(defun diredp-make-find-file-keys-reuse-dirs ()
  "Make find-file keys reuse Dired buffers."
  (substitute-key-definition 'dired-find-file 'diredp-find-file-reuse-dir-buffer dired-mode-map)
  (substitute-key-definition 'diredp-mouse-find-file
                             'diredp-mouse-find-file-reuse-dir-buffer dired-mode-map)
  ;; These commands are defined in `w32-browser.el' (for use with MS Windows).
  (substitute-key-definition 'dired-w32-browser
                             'dired-w32-browser-reuse-dir-buffer dired-mode-map)
  (substitute-key-definition 'dired-mouse-w32-browser
                             'dired-mouse-w32-browser-reuse-dir-buffer dired-mode-map)
  (message "Reusing Dired buffers is now ON"))

(defun diredp-make-find-file-keys-not-reuse-dirs ()
  "Make find-file keys not reuse Dired buffers (i.e. act normally)."
  (substitute-key-definition 'diredp-find-file-reuse-dir-buffer 'dired-find-file dired-mode-map)
  (substitute-key-definition 'diredp-mouse-find-file-reuse-dir-buffer
                             'diredp-mouse-find-file dired-mode-map)
  ;; These commands are defined in `w32-browser.el' (for use with MS Windows).
  (substitute-key-definition 'dired-w32-browser-reuse-dir-buffer
                             'dired-w32-browser dired-mode-map)
  (substitute-key-definition 'dired-mouse-w32-browser-reuse-dir-buffer
                             'dired-mouse-w32-browser dired-mode-map)
  (message "Reusing Dired buffers is now OFF"))

;;;###autoload
(defun diredp-omit-marked ()            ; Not bound
  "Omit lines of marked files.  Return the number of lines omitted."
  (interactive)
  (let ((old-modified-p  (buffer-modified-p))
        count)
    (when (interactive-p) (message "Omitting marked lines..."))
    (setq count  (dired-do-kill-lines nil "Omitted %d line%s."))
    (set-buffer-modified-p old-modified-p) ; So no `%*' appear in mode-line.
    count))

;; `dired-do-toggle' was renamed to `dired-toggle-marks' after Emacs 20.
;; That's why we aliased it to `dired-toggle-marks' at the top of the file.
;;
;;;###autoload
(defun diredp-omit-unmarked ()          ; Not bound
  "Omit lines of unmarked files.  Return the number of lines omitted."
  (interactive)
  (let ((old-modified-p  (buffer-modified-p))
        count)
    (dired-do-toggle)
    (message "Omitting unmarked lines...")
    (setq count  (diredp-omit-marked))
    (dired-do-toggle)                   ; Marks all except `.', `..'
    (set-buffer-modified-p old-modified-p) ; So no `%*' appear in mode-line.
    count))

;;;###autoload
(defun diredp-ediff (file2)             ; Bound to `='
  "Compare file at cursor with file FILE2 using `ediff'.
FILE2 defaults to the file at the cursor as well.  If you enter just a
directory name for FILE2, then the file at the cursor is compared with
a file of the same name in that directory.  FILE2 is the second file
given to `ediff'; the file at the cursor is the first.

Try to guess a useful default value for FILE2, as follows:

* If the mark is active, use the file at mark.
* Else if the file at cursor is a autosave file or a backup file, use
  the corresponding base file.
* Else if there is any backup file for the file at point, use the
  newest backup file for it.
* Else use the file at point."
  (interactive
   (progn (require 'ediff)
          (list (ediff-read-file-name   ; In `ediff.el'.
                 (format "Compare %s with" (dired-get-filename t))
                 (dired-current-directory)
                 (let* ((file           (dired-get-filename))
                        (file-sans-dir  (file-name-nondirectory file))
                        (file-dir       (file-name-directory file))
                        (file-at-mark   (and (and transient-mark-mode  mark-active)
                                             (save-excursion (goto-char (mark t))
                                                             (dired-get-filename t t))))
                        (last-backup    (file-newest-backup file)))
                   (cond
                     (file-at-mark)
                     ((auto-save-file-name-p file-sans-dir)
                      (expand-file-name (substring file-sans-dir 1 -1) file-dir))
                     ((backup-file-name-p file-sans-dir)
                      (expand-file-name (file-name-sans-versions file-sans-dir) file-dir))
                     (last-backup)
                     (t file)))))))
  (ediff-files (dired-get-filename) file2)) ; In `ediff.el'.

(defun diredp-fewer-than-2-files-p (arg)
  "Return non-nil iff fewer than two files are to be treated by dired.
More precisely, return non-nil iff ARG is nil and fewer than two
files are marked, or ARG is -1, 0 or 1."
  (if arg
      (and (integerp arg) (< (abs arg) 2)) ; Next or previous file (or none).
    (not (save-excursion                ; Fewer than two marked files.
           (goto-char (point-min))
           (re-search-forward (dired-marker-regexp) nil t 2)))))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Redisplay only if at most one file is being treated.
;; 2. Doc string reflects Dired+'s version of `dired-map-over-marks-check'.
;;
;;;###autoload
(defun dired-do-compress (&optional arg) ; Bound to `Z'
  "Compress or uncompress marked (or next prefix argument) files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive "P")
  (dired-map-over-marks-check #'dired-compress arg 'compress (diredp-fewer-than-2-files-p arg)))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Redisplay only if at most one file is being treated.
;; 2. Doc string reflects Dired+'s version of `dired-map-over-marks-check'.
;;
;;;###autoload
(defun dired-do-byte-compile (&optional arg) ; Bound to `B'
  "Byte compile marked (or next prefix argument) Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive "P")
  (dired-map-over-marks-check #'dired-byte-compile arg 'byte-compile
                              (diredp-fewer-than-2-files-p arg)))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Redisplay only if at most one file is being treated.
;; 2. Doc string reflects Dired+'s version of `dired-map-over-marks-check'.
;;
;;;###autoload
(defun dired-do-load (&optional arg)    ; Bound to `L'
  "Load the marked (or next prefix argument) Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive "P")
  (dired-map-over-marks-check #'dired-load arg 'load (diredp-fewer-than-2-files-p arg)))

(defun diredp-do-grep (command-args)    ; Bound to `M-g'
  "Run `grep' on marked (or next prefix arg) files.
A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer."
  (interactive (progn (unless (if (< emacs-major-version 22)
                                  grep-command
                                (and grep-command  (or (not grep-use-null-device)
                                                       (eq grep-use-null-device t))))
                        (grep-compute-defaults))
                      (list (diredp-do-grep-1))))
  (grep command-args))

;; Optional arg FILES is no longer used.  It was used in `diredp-do-grep' before the
;; new `dired-get-marked-files'.
(defun diredp-do-grep-1 (&optional files)
  "Helper function for `diredp-do-grep'.
Non-nil optional arg FILES are the files to grep, overriding the files
choice described for `diredp-do-grep'."
  (let ((default  (and (fboundp 'grep-default-command) (grep-default-command))))
    (read-from-minibuffer
     "grep <pattern> <files> :  "
     (let ((up-to-files  (concat grep-command "   ")))
       (cons (concat up-to-files
                     (mapconcat #'identity
                                (or files
                                    (mapcar 'shell-quote-argument
                                            (dired-get-marked-files nil current-prefix-arg)))
                                " "))
             (- (length up-to-files) 2)))
     nil nil 'grep-history default)))

(when (memq system-type '(windows-nt ms-dos))
  (define-derived-mode diredp-w32-drives-mode fundamental-mode "Drives"
    "Open Dired for an MS Windows drive (local or remote)."
    (setq buffer-read-only  t)))

;; The next two commands were originally taken from Emacs Wiki, page WThirtyTwoBrowseNetDrives:
;; http://www.emacswiki.org/emacs/WThirtyTwoBrowseNetDrives.  They are referred to there as
;; commands `show-net-connections' and `netdir'.  I am hoping that the contributor (anonymous)
;; does not mind my adapting them and including them in Dired+.

(when (memq system-type '(windows-nt ms-dos))
  (defun diredp-w32-list-mapped-drives () ; Not bound
    "List network connection information for shared MS Windows resources.
This just invokes the Windows `NET USE' command."
    (interactive)
    (shell-command "net use")
    (display-buffer "*Shell Command Output*")))

(when (memq system-type '(windows-nt ms-dos))
  (defun diredp-w32-drives (&optional other-window-p) ; Not bound
    "Visit a list of MS Windows drives for use by Dired.
With a prefix argument use another window for the list.
In the list, use `mouse-2' or `RET' to open Dired for a given drive.

The drives listed are the remote drives currently available, as
determined by the Windows command `NET USE', plus the local drives
specified by option `diredp-w32-local-drives', which you can
customize.

Note: When you are in Dired at the root of a drive (e.g. directory
      `c:/'), command `dired-up-directory' invokes this command.
      So you can use `\\[dired-up-directory]' to go up to the list of drives."
    (interactive "P")
    (require 'widget)
    (let ((drive              (copy-sequence diredp-w32-local-drives))
          (inhibit-read-only  t))
      (with-temp-buffer
        (insert (shell-command-to-string "net use"))
        (goto-char (point-min))
        (while (re-search-forward "[A-Z]: +\\\\\\\\[^ ]+" nil t nil)
          (setq drive  (cons (split-string (match-string 0)) drive))))
      (if other-window-p
          (pop-to-buffer "*Windows Drives*")
        (switch-to-buffer "*Windows Drives*"))
      (erase-buffer)
      (widget-minor-mode 1)
      (dolist (drv  (sort drive (lambda (a b) (string-lessp (car a) (car b)))))
        (lexical-let ((drv  drv))
          (widget-create 'push-button
                         :notify (lambda (widget &rest ignore) (dired (car drv)))
                         (concat (car drv) "  " (cadr drv))))
        (widget-insert "\n"))
      (goto-char (point-min))
      (diredp-w32-drives-mode))))

;; $$$$$$ NO LONGER USED.  Was used in `dired-do-grep(-1)' before new `dired-get-marked-files'.
(defun diredp-all-files ()
  "List of all files shown in current Dired buffer.
Directories are not included."
  (let ((pos    (make-marker))
        (files  ())
        file)
    (save-excursion
      (goto-char (point-min)) (beginning-of-line)
      (while (not (eobp))
        (beginning-of-line)
        (while (and (not (eobp))  (dired-between-files))  (forward-line 1))
        (save-excursion (forward-line 1) (move-marker pos (1+ (point))))
        (setq file  (dired-get-filename nil t)) ; Non-nil second arg means "also . and ..".
        (when file                      ; Remove directory portion if in same directory.
          (setq file  (dired-get-filename (dired-in-this-tree file default-directory) t)))
        (unless (or (not file) (file-directory-p file))  (push file files))
        (goto-char pos))
      (move-marker pos nil))
    (setq files  (sort files (if (and (featurep 'ls-lisp)
                                      (not (symbol-value 'ls-lisp-use-insert-directory-program)))
                                 'ls-lisp-string-lessp
                               (if case-fold-search
                                   (lambda (s1 s2) (string-lessp (upcase s1) (upcase s2)))
                                 'string-lessp))))))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Use `diredp-this-subdir' instead of `dired-get-filename'.
;; 2. If on a subdir listing header line or a non-dir file in a subdir listing, go to
;;    the line for the subdirectory in the parent directory listing.
;; 3. Fit one-window frame after inserting subdir.
;;
;;;###autoload
(defun dired-maybe-insert-subdir (dirname &optional switches no-error-if-not-dir-p)
                                        ; Bound to `i'
  "Move to Dired subdirectory line or subdirectory listing.
This bounces you back and forth between a subdirectory line and its
inserted listing header line.  Using it on a non-directory line in a
subdirectory listing acts the same as using it on the subdirectory
header line.

* If on a subdirectory line, then go to the subdirectory's listing,
  creating it if not yet present.

* If on a subdirectory listing header line or a non-directory file in
  a subdirectory listing, then go to the line for the subdirectory in
  the parent directory listing.

* If on a non-directory file in the top Dired directory listing, do
  nothing.

Subdirectories are listed in the same position as for `ls -lR' output.

With a prefix arg, you can edit the `ls' switches used for this
listing.  Add `R' to the switches to expand the directory tree under a
subdirectory.

Dired remembers the switches you specify with a prefix arg, so
reverting the buffer does not reset them.  However, you might
sometimes need to reset some subdirectory switches after a
`dired-undo'.  You can reset all subdirectory switches to the
default value using \\<dired-mode-map>\\[dired-reset-subdir-switches].  See \
Info node
`(emacs)Subdir switches' for more details."
  (interactive (list (diredp-this-subdir)
                     (and current-prefix-arg
                          (read-string "Switches for listing: "
                                       (or (and (boundp 'dired-subdir-switches)
                                                dired-subdir-switches)
                                           dired-actual-switches)))))
  (let ((opoint    (point))
        (filename  dirname))
    (cond ((consp filename)             ; Subdir header line or non-directory file.
           (setq filename  (car filename))
           (if (assoc filename dired-subdir-alist)
               (dired-goto-file filename) ;  Subdir header line.
             (dired-insert-subdir (substring (file-name-directory filename) 0 -1))))
          (t
           ;; We don't need a marker for opoint as the subdir is always
           ;; inserted *after* opoint.
           (setq dirname  (file-name-as-directory dirname))
           (or (and (not switches)  (dired-goto-subdir dirname))
               (dired-insert-subdir dirname switches no-error-if-not-dir-p))
           ;; Push mark so that it's easy to go back.  Do this after the
           ;; insertion message so that the user sees the `Mark set' message.
           (push-mark opoint)
           (when (and (get-buffer-window (current-buffer)) ; Fit one-window frame.
                      (fboundp 'fit-frame-if-one-window))
             (fit-frame-if-one-window))))))

(defun diredp-this-subdir ()
  "This line's filename, if directory, or `dired-current-directory' list.
If on a directory line, then return the directory name.
Else return a singleton list of a directory name, which is as follows:
  If on a subdirectory header line (either of the two lines), then use
  that subdirectory name.  Else use the parent directory name."
  (or (let ((file  (dired-get-filename nil t)))
        (and file (file-directory-p file)
             (not (member (file-relative-name file (file-name-directory
                                                    (directory-file-name file)))
                          '("." ".." "./" "../")))
             file))
      (list (dired-current-directory))))


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; Fix the `interactive' spec.  This is the Emacs 24+ version, provided for earlier versions.
;;
(unless (> emacs-major-version 23)
  (defun dired-mark-unmarked-files (regexp msg &optional unflag-p localp)
    "Mark unmarked files matching REGEXP, displaying MSG.
REGEXP is matched against the entire file name.  When called
interactively, prompt for REGEXP.
With prefix argument, unflag all those files.

Non-interactively:
 Returns t if any work was done, nil otherwise.
 Optional fourth argument LOCALP is as in `dired-get-filename'."
    (interactive
     (list (dired-read-regexp
            "Mark unmarked files matching regexp (default all): ")
           nil current-prefix-arg nil))
    (let ((dired-marker-char (if unflag-p ?\   dired-marker-char)))
      (dired-mark-if
       (and
        ;; not already marked
        (looking-at " ")
        ;; uninteresting
        (let ((fn (dired-get-filename localp t)))
          (and fn (string-match regexp fn))))
       msg))))


;;; VISIT ALL MARKED FILES SIMULTANEOUSLY.

;;; Brief Description:
;;;
;;; `dired-do-find-marked-files' is bound to `F' by dired-x.el.
;;;
;;; * Use `dired-get-marked-files' to collect the marked files in the current
;;;   Dired Buffer into a list of filenames `FILE-LIST'.
;;;
;;; * Pass FILE-LIST to `dired-simultaneous-find-file' all with
;;;   `dired-do-find-marked-files''s prefix argument OPTION.
;;;
;;; * `dired-simultaneous-find-file' runs through FILE-LIST decrementing the
;;;   list each time.
;;;
;;; * If OPTION and `pop-up-frames' are both nil, then calculate the
;;; `size' of the window for each file by dividing the `window-height'
;;; by length of FILE-LIST.  Thus, `size' is cognizant of the
;;; window-configuration.
;;;
;;; * If `size' is too small abort, otherwise run `find-file' on each element
;;;   of FILE-LIST giving each a window of height `size'.


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; 1. Call `dired-get-marked-files' with original ARG, to get its multi-C-u behavior.
;; 2. Doc string updated to reflect change to `dired-simultaneous-find-file'.
;;
;;;###autoload
(defun dired-do-find-marked-files (&optional arg) ; Bound to `F'
  "Find marked files, displaying all of them simultaneously.
With a prefix ARG >= 0, just find files but do not select them.

If no prefix ARG, and variable `pop-up-frames' is non-nil, or
if prefix ARG < 0, then each file is displayed in a separate frame.

Otherwise (no prefix ARG and nil `pop-up-frames'), the current window
is split across all marked files, as evenly as possible.  Remaining
lines go to the bottom-most window.  The number of files that can be
displayed this way is restricted by the height of the current window
and `window-min-height'.

A prefix argument also behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer.

To keep the Dired buffer displayed, type \\[split-window-vertically] first.
To display just the marked files, type \\[delete-other-windows] first."
  (interactive "P")
  (dired-simultaneous-find-file (dired-get-marked-files nil arg)
                                (and arg (prefix-numeric-value arg))))


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; Use separate frames instead of windows if `pop-up-frames' is non-nil,
;; or if prefix arg is negative.
;;
(defun dired-simultaneous-find-file (file-list option)
  "Visit all files in list FILE-LIST and display them simultaneously.

With non-nil OPTION >= 0, the files are found but not selected.

If `pop-up-frames' is non-nil or OPTION < 0, use a separate frame
for each file.

Otherwise, the current window is split across all files in
FILE-LIST, as evenly as possible.  Remaining lines go to the
bottom-most window.  The number of files that can be displayed
this way is restricted by the height of the current window and
the variable `window-min-height'."
  ;; This is not interactive because it is usually too clumsy to
  ;; specify FILE-LIST interactively unless via dired.
  (let (size)
    (cond ((and option (natnump option))
           (while file-list (find-file-noselect (car file-list)) (pop file-list)))
          ((or pop-up-frames option)
           (while file-list (find-file-other-frame (car file-list)) (pop file-list)))
          (t
           (setq size  (/ (window-height) (length file-list)))
           (when (> window-min-height size)
             (error "Too many files to visit simultaneously.  Try C-u prefix."))
           (find-file (car file-list))
           (pop file-list)
           (while file-list
             ;; Vertically split off a window of desired size. Upper window will have SIZE lines.
             ;; Select lower (larger) window.  We split it again.
             (select-window (split-window nil size))
             (find-file (car file-list))
             (pop file-list))))))


;;;;;; REPLACE ORIGINAL in both `dired.el' and `dired-x.el':
;;;;;;
;;;;;; 1. This incorporates the `dired-x.el' change to the `dired.el'
;;;;;;    definition.  This version works with or without using dired-x.
;;;;;;    The `dired-x.el' version respects the var `dired-find-subdir'.
;;;;;;    When `dired-find-subdir' is non-nil, this version is the same
;;;;;;    as the `dired-x.el' version, except that a bug is corrected:
;;;;;;    Whenever the argument to `dired-find-buffer-nocreate' is a cons,
;;;;;;    the call to `dired-buffers-for-dir' gave a wrong type error.
;;;;;;    This has been avoided by not respecting `dired-find-subdir'
;;;;;;    whenever `dired-find-buffer-nocreate' is a cons.
;;;;;;    For the case when `dired-find-subdir' is nil, see #2, below.
;;;;;;
;;;;;; 2. Unless `dired-find-subdir' is bound and non-nil:
;;;;;;    If both DIRNAME and `dired-directory' are conses, then only
;;;;;;    compare their cars (directories), not their explicit file lists
;;;;;;    too.  If equal, then update `dired-directory's file list to that
;;;;;;    of DIRNAME.
;;;;;;
;;;;;;    This prevents `dired-internal-noselect' (which is currently
;;;;;;    `dired-find-buffer-nocreate's only caller) from creating a new
;;;;;;    buffer in this case whenever a different set of files is present
;;;;;;    in the cdr of DIRNAME and DIRNAME represents the same buffer as
;;;;;;    `dired-directory'.
;;;;;;
;;;;;;    If only one of DIRNAME and `dired-directory' is a cons, then
;;;;;;    this returns nil.
;;;;;;;###autoload
;;;;(defun dired-find-buffer-nocreate (dirname &optional mode)
;;;;  (let ((atomic-dirname-p (atom dirname)))
;;;;    (if (and (boundp 'dired-find-subdir) dired-find-subdir atomic-dirname-p)
;;;;        ;; This is the `dired-x.el' change:
;;;;        (let* ((cur-buf (current-buffer))
;;;;               (buffers (nreverse (dired-buffers-for-dir dirname)))
;;;;               (cur-buf-matches (and (memq cur-buf buffers)
;;;;                                     ;; Files list (wildcards) must match, too:
;;;;                                     (equal dired-directory dirname))))
;;;;          (setq buffers  (delq cur-buf buffers)) ; Avoid using same buffer---
;;;;          (or (car (sort buffers (function dired-buffer-more-recently-used-p)))
;;;;              (and cur-buf-matches cur-buf))) ; ---unless no other possibility.
;;;;      ;; Comment from `dired.el':
;;;;      ;;  This differs from `dired-buffers-for-dir' in that it doesn't consider
;;;;      ;;  subdirs of `default-directory' and searches for the first match only.
;;;;      (let ((blist dired-buffers)       ; was (buffer-list)
;;;;            found)
;;;;        (or mode (setq mode  'dired-mode))
;;;;        (while blist
;;;;          (if (null (buffer-name (cdr (car blist))))
;;;;              (setq blist  (cdr blist))
;;;;            (save-excursion
;;;;              (set-buffer (cdr (car blist)))
;;;;              (if (not (and (eq major-mode mode)
;;;;                            ;; DIRNAME and `dired-directory' have the same dir,
;;;;                            ;; and if either of them has an explicit file list,
;;;;                            ;; then both of them do.  In that case, update
;;;;                            ;; `dired-directory's file list from DIRNAME.
;;;;                            (if atomic-dirname-p
;;;;                                (and (atom dired-directory) ; Both are atoms.
;;;;                                     (string= (file-truename dirname)
;;;;                                              (file-truename dired-directory)))
;;;;                              (and (consp dired-directory) ; Both are conses.
;;;;                                   (string=
;;;;                                    (file-truename (car dirname))
;;;;                                    (file-truename (car dired-directory)))
;;;;                                   ;; Update `dired-directory's file list.
;;;;                                   (setq dired-directory  dirname)))))
;;;;                  (setq blist  (cdr blist))
;;;;                (setq found  (cdr (car blist)))
;;;;                (setq blist  nil)))))
;;;;        found))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Put `mouse-face' on whole line, not just file name.
;; 2. Add text property `dired-filename' to the file name (only).
;;
(defun dired-insert-set-properties (beg end)
  "Highlight entire dired line upon mouseover.
Add text property `dired-filename' to the file name."
  (let ((inhibit-field-text-motion  t)) ; Just in case.
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (condition-case nil
            (when (dired-move-to-filename)
              (add-text-properties (line-beginning-position) (line-end-position)
                                   '(mouse-face highlight
                                     help-echo "mouse-2: visit this file in other window"))
              (put-text-property (point) (save-excursion (dired-move-to-end-of-filename) (point))
                                 'dired-filename t))
          (error nil))
        (forward-line 1)))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Reset `mode-line-process' to nil.
;;
(when (< emacs-major-version 21)
  (or (fboundp 'old-dired-revert) (fset 'old-dired-revert (symbol-function 'dired-revert)))
  (defun dired-revert (&optional arg noconfirm)
    (setq mode-line-process  nil)        ; Set by, e.g., `find-dired'.
    (old-dired-revert arg noconfirm)))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; If at root on a Windows drive, go up to a list of available drives.
;;
;;;###autoload
(defun dired-up-directory (&optional other-window) ; Bound to `^'
  "Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary.

On MS Windows, if you already at the root directory, invoke
`diredp-w32-drives' to visit a navigable list of Windows drives."
  (interactive "P")
  (let* ((dir  (dired-current-directory))
         (up   (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)  (dired-goto-subdir up))
        (progn (if other-window (dired-other-window up) (dired up))
               (dired-goto-file dir))
        (and (memq system-type '(windows-nt ms-dos))  (diredp-w32-drives)))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Test also ./ and ../, in addition to . and .., for error "Cannot operate on `.' or `..'".
;;
(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In Dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
name in result.  A value of `verbatim' means to return the name exactly as
it occurs in the buffer, and a value of t means construct name relative to
`default-directory', which still may contain slashes if in a subdirectory.

Non-nil optional arg NO-ERROR-IF-NOT-FILEP means treat `.' and `..' as
regular filenames and return nil if no filename on this line.
Otherwise, an error occurs in these cases."
  (let (case-fold-search file p1 p2 already-absolute)
    (save-excursion (when (setq p1  (dired-move-to-filename (not no-error-if-not-filep)))
                      (setq p2  (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line but `no-error-if-not-filep' is t:
    (when (setq file  (and p1 p2 (buffer-substring p1 p2)))
      ;; Get rid of the mouse-face property that file names have.
      (set-text-properties 0 (length file) nil file)

      ;; Unquote names quoted by `ls' or by `dired-insert-directory'.
      ;; Prior to Emacs 23.3, this code was written using `read' (see commented code below),
      ;; because that is faster than substituting \007 (4 chars) -> ^G (1 char) etc. in a loop.
      ;; Unfortunately, that implementation required hacks such as dealing with filenames
      ;; with quotation marks in their names.
      (while (string-match (if (> emacs-major-version 21)
                               "\\(?:[^\\]\\|\\`\\)\\(\"\\)" ; Shy group: Emacs 22+.
                             "\\([^\\]\\|\\`\\)\\(\"\\)")
                           file)
        (setq file (replace-match "\\\"" nil t file 1)))

      ;; $$$ This was the code for that unquoting prior to Emacs 23.3:
      ;; (setq file  (read (concat "\"" ; Some `ls -b' do not escape quotes.  But GNU `ls' is OK.
      ;;                           (or (dired-string-replace-match
      ;;                                "\\([^\\]\\|\\`\\)\"" file "\\1\\\\\"" nil t)
      ;;                               file)
      ;;                           "\"")))

      ;; This sexp was added by Emacs 24, to fix bug #10469:
      ;; Unescape any spaces escaped by `ls -b'.
      ;; Other `-b' quotes, such as \t and \n, work transparently.
      (when (dired-switches-escape-p dired-actual-switches)
        (let ((start 0)
              (rep "")
              (shift -1))
          (when (eq localp 'verbatim)  (setq rep    "\\\\"
                                             shift  +1))
          (while (string-match "\\(\\\\\\) " file start)
            (setq file   (replace-match rep nil t file 1)
                  start  (+ shift (match-end 0))))))

      ;; $$$ This sexp was added by Emacs 23.3.
      (when (memq system-type '(windows-nt ms-dos))
        (save-match-data
          (let ((start  0))
            (while (string-match "\\\\" file start)
              (aset file (match-beginning 0) ?/)
              (setq start  (match-end 0))))))

      ;; $$$ This sexp was added by Emacs 23.3.
      ;; Hence we don't need to worry about converting `\\' back to `\'.
      (setq file (read (concat "\"" file "\"")))

      ;; Above `read' returns a unibyte string if FILE contains eight-bit-control/graphic chars.
      (when (and (fboundp 'string-to-multibyte) ; Emacs 22
                 enable-multibyte-characters (not (multibyte-string-p file)))
        (setq file  (string-to-multibyte file))))
    (and file (file-name-absolute-p file)
         ;; A relative file name can start with ~.  Do not treat it as absolute in this context.
         (not (eq (aref file 0) ?~))
         (setq already-absolute  t))
    (cond ((null file) nil)
          ((eq localp 'verbatim) file)
          ;; This is the essential Dired+ change: Added ./ and ../, not just . and ..
          ((and (not no-error-if-not-filep)
                (member file '("." ".." "./" "../")))
           (error "Cannot operate on `.' or `..'"))
          ((and (eq localp 'no-dir)  already-absolute)
           (file-name-nondirectory file))
          (already-absolute
           (let ((handler  (find-file-name-handler file nil)))
             ;; check for safe-magic property so that we won't
             ;; put /: for names that don't really need them.
             ;; For instance, .gz files when auto-compression-mode is on.
             (if (and handler  (not (get handler 'safe-magic)))
                 (concat "/:" file)
               file)))
          ((eq localp 'no-dir) file)
          ((equal (dired-current-directory) "/")
           (setq file  (concat (dired-current-directory localp) file))
           (let ((handler  (find-file-name-handler file nil)))
             ;; check for safe-magic property so that we won't
             ;; put /: for names that don't really need them.
             ;; For instance, .gz files when auto-compression-mode is on.
             (if (and handler (not (get handler 'safe-magic)))
                 (concat "/:" file)
               file)))
          (t
           (concat (dired-current-directory localp) file)))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Fixes Emacs bug #7126: Did not work with arbitrary file list (cons arg to `dired').
;; 2. Remove `/' from directory name before comparing with BASE.
;;
;;;###autoload
(defun dired-goto-file (file)           ; Bound to `j'
  "Go to line describing file FILE in this dired buffer."
  ;; Return value of point on success, else nil.
  ;; FILE must be an absolute file name.
  ;; Loses if FILE contains control chars like "\007" for which ls
  ;; either inserts "?" or "\\007" into the buffer, so we won't find
  ;; it in the buffer.
  (interactive
   (prog1                               ; Let push-mark display its message
       (list (expand-file-name (read-file-name "Goto file: " (dired-current-directory))))
     (push-mark)))
  (setq file  (directory-file-name file)) ; does no harm if no directory
  (let ((found  nil)
        case-fold-search dir)
    (setq dir  (or (file-name-directory file) (error "File name `%s' is not absolute" file)))

    ;; Dired+: Added this sexp.
    (save-excursion
      (goto-char (point-min))
      (let ((search-string  (replace-regexp-in-string "\^m" "\\^m" file nil t))
            (here           nil))
        (setq search-string  (replace-regexp-in-string "\\\\" "\\\\" search-string nil t))

        ;; Escape whitespace.  Added per Emacs 24 addition in `unless' code below:
        (when (and (dired-switches-escape-p dired-actual-switches)
                   (string-match "[ \t\n]" search-string))
          ;; FIXME: fix this for all possible file names (embedded control chars etc).
          ;;        Need to escape everything that `ls -b' escapes.
          (setq search-string  (replace-regexp-in-string " " "\\ "  search-string nil t)
                search-string  (replace-regexp-in-string "\t" "\\t" search-string nil t)
                search-string  (replace-regexp-in-string "\n" "\\n" search-string nil t)))

        ;; Use HERE to ensure we do not keep searching for a directory entry.
        (while (and (not (eobp))  (not found)  (not (equal here (point))))
          (setq here  (point))
          (if (search-forward (concat " " search-string) nil 'NO-ERROR)
              ;; Must move to filename since an (actually correct) match could have been
              ;; elsewhere on the line (e.g. "-" would match somewhere in permission bits).
              (setq found  (dired-move-to-filename))
            ;; If this isn't the right line, move forward to avoid trying this line again.
            (forward-line 1)))))

    (unless found
      (save-excursion
        ;; The difficulty here is to get the result of dired-goto-subdir without really
        ;; calling it if we don't have any subdirs.
        (if (if (string= dir (expand-file-name default-directory))
                (goto-char (point-min))
              (and (cdr dired-subdir-alist)  (dired-goto-subdir dir)))
            (let ((base      (file-name-nondirectory file))
                  (boundary  (dired-subdir-max))
                  search-string)
              (setq search-string  (replace-regexp-in-string "\^m" "\\^m" base nil t)
                    search-string  (replace-regexp-in-string "\\\\" "\\\\" search-string nil t))

              ;; Escape whitespace.  Sexp added by Emacs 24:
              (when (and (dired-switches-escape-p dired-actual-switches)
                         (string-match "[ \t\n]" search-string))
                ;; FIXME: fix this for all possible file names (embedded control chars etc).
                ;;        Need to escape everything that `ls -b' escapes.
                (setq search-string  (replace-regexp-in-string " " "\\ " search-string nil t)
                      search-string  (replace-regexp-in-string "\t" "\\t" search-string nil t)
                      search-string  (replace-regexp-in-string "\n" "\\n" search-string nil t)))

              (while (and (not found)
                          ;; Filenames are preceded by SPC.  This makes the search faster
                          ;; (e.g. for the filename "-"!).
                          (search-forward (concat " " search-string) boundary 'move))
                ;; Dired+: Remove / from filename, then compare with BASE.
                ;; Match could have BASE just as initial substring or
                ;; or in permission bits or date or not be a proper filename at all.
                (if (and (dired-get-filename 'no-dir t)
                         (equal base (directory-file-name (dired-get-filename 'no-dir t))))
                    ;; Must move to filename since an (actually correct) match could have been
                    ;; elsewhere on the line (e.g. "-" would match somewhere in permission bits).
                    (setq found  (dired-move-to-filename))
                  ;; If this is not the right line, move forward to avoid trying this line again.
                  (forward-line 1)))))))
    (and found (goto-char found))))     ; Return value of point.


;; REPLACE ORIGINAL in `dired.el':
;;
;; 1. Display a message to warn that flagged, not marked, files will be deleted.
;; 2. Use `diredp-internal-do-deletions', so it works with all Emacs versions.
;;
;;;###autoload
(defun dired-do-flagged-delete (&optional no-msg) ; Bound to `x'
  "In Dired, delete the files flagged for deletion.
NOTE: This deletes flagged, not marked, files.
If arg NO-MSG is non-nil, no message is displayed.

User option `dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  (interactive)
  (unless no-msg
    (ding)
    (message "NOTE: Deletion of files flagged `%c' (not those marked `%c')"
             dired-del-marker dired-marker-char)
    ;; Too slow/annoying, but without it the message is never seen: (sit-for 2)
    )
  (let* ((dired-marker-char  dired-del-marker)
         (regexp             (dired-marker-regexp))
         case-fold-search)
    (if (save-excursion (goto-char (point-min)) (re-search-forward regexp nil t))
        (diredp-internal-do-deletions
         ;; This cannot move point since last arg is nil.
         (dired-map-over-marks (cons (dired-get-filename) (point)) nil)
         nil
         'USE-TRASH-CAN)                ; This arg is for Emacs 24+ only.
      (unless no-msg (message "(No deletions requested.)")))))


;; REPLACE ORIGINAL in `dired.el':
;;
;; 1. Display a message to warn that marked, not flagged, files will be deleted.
;; 2. Use `diredp-internal-do-deletions', so it works with all Emacs versions.
;;
;;;###autoload
(defun dired-do-delete (&optional arg)  ; Bound to `D'
  "Delete all marked (or next ARG) files.
NOTE: This deletes marked, not flagged, files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  (interactive "P")
  ;; This is more consistent with the file-marking feature than
  ;; `dired-do-flagged-delete'.  But it can be confusing to the user,
  ;; especially since this is usually bound to `D', which is also the
  ;; `dired-del-marker'.  So offer this warning message:
  (unless arg
    (ding)
    (message "NOTE: Deletion of files marked `%c' (not those flagged `%c')."
             dired-marker-char dired-del-marker))
  (diredp-internal-do-deletions
   ;; This can move point if ARG is an integer.
   (dired-map-over-marks (cons (dired-get-filename) (point)) arg)
   arg
   'USE-TRASH-CAN))                     ; This arg is for Emacs 24+ only.

(defun diredp-internal-do-deletions (l arg &optional trash)
  "`dired-internal-do-deletions', but for any Emacs version."
  (if (> emacs-major-version 23)
      (dired-internal-do-deletions l arg trash)
    (dired-internal-do-deletions l arg)))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Put window point at bob.  Fixes bug #12281.
;;
(when (> emacs-major-version 22)
  (defun dired-pop-to-buffer (buf)
  "Pop up buffer BUF in a way suitable for Dired."
  (let ((split-window-preferred-function
         (lambda (window)
           (or (and (let ((split-height-threshold 0))
                      (window-splittable-p (selected-window)))
                    ;; Try to split the selected window vertically if
                    ;; that's possible.  (Bug#1806)
                    (if (fboundp 'split-window-below)
                        (split-window-below)
                      (split-window-vertically)))
               ;; Otherwise, try to split WINDOW sensibly.
               (split-window-sensibly window))))
        pop-up-frames)
    (pop-to-buffer (get-buffer-create buf)))
  (set-window-start (selected-window) (point-min))
  ;; If dired-shrink-to-fit is t, make its window fit its contents.
  (when dired-shrink-to-fit
    ;; Try to not delete window when we want to display less than
    ;; `window-min-height' lines.
    (fit-window-to-buffer (get-buffer-window buf) nil 1))))


;; REPLACE ORIGINAL in `dired.el':
;;
;; 1. Delete the window or frame popped up, afterward, and bury its buffer.
;;    Fixes Emacs bug #7533.
;;
;; 2, If buffer is shown in a separate frame, do not show a menu bar for that frame.
;;
(defun dired-mark-pop-up (bufname op-symbol files function &rest args)
  "Return FUNCTION's result on ARGS after showing which files are marked.
Displays the file names in a buffer named BUFNAME;
 nil gives \" *Marked Files*\".
This uses function `dired-pop-to-buffer' to do that.

FUNCTION should not manipulate files, just read input
 (an argument or confirmation).
The window is not shown if there is just one file or
 OP-SYMBOL is a member of the list in `dired-no-confirm'.
FILES is the list of marked files.  It can also be (t FILENAME)
in the case of one marked file, to distinguish that from using
just the current file."
  (or bufname (setq bufname  " *Marked Files*"))
  (let (result)
    (if (or (eq dired-no-confirm t)
            (memq op-symbol dired-no-confirm)
            ;; If FILES defaulted to the current line's file.
            (= (length files) 1))
        (setq result  (apply function args))
      (with-current-buffer (get-buffer-create bufname)
        (erase-buffer)
        ;; Handle (t FILE) just like (FILE), here.
        ;; That value is used (only in some cases), to mean
        ;; just one file that was marked, rather than the current line file.
        (dired-format-columns-of-files (if (eq (car files) t) (cdr files) files))
        (remove-text-properties (point-min) (point-max)
                                '(mouse-face nil help-echo nil)))
      (unwind-protect
           (save-window-excursion
             ;; Do not show menu bar, if buffer is popped up in a separate frame.
             (let ((special-display-frame-alist  (cons '(menu-bar-lines . 0)
                                                       special-display-frame-alist))
                   (default-frame-alist          (cons '(menu-bar-lines . 0)
                                                       default-frame-alist)))
               (dired-pop-to-buffer bufname))
             (setq result  (apply function args)))
        (save-excursion
          (condition-case nil           ; Ignore error if user already deleted window.
              (progn
                (select-window (get-buffer-window bufname 0))
                (if (one-window-p) (delete-frame) (delete-window)))
            (error nil)))
        (bury-buffer bufname)))
    result))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Push REGEXP onto `regexp-search-ring'.
;;
;;;###autoload
(defun dired-mark-files-regexp (regexp &optional marker-char)
  "Mark all files matching REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think.

REGEXP is added to `regexp-search-ring', for regexp search."
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (regexp): "))
	 (if current-prefix-arg ?\040)))
  (add-to-list 'regexp-search-ring regexp) ; Add REGEXP to `regexp-search-ring.
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename t t)))
	    (and fn (string-match regexp fn))))
     "matching file")))

;;;###autoload
(defun diredp-capitalize (&optional arg) ; Bound to `% c'
  "Rename all marked (or next ARG) files by capitalizing them.
Makes the first char of the name uppercase and the others lowercase."
  (interactive "P")
  (dired-rename-non-directory #'capitalize "Rename by capitalizing:" arg))


;;; Versions of `dired-do-*' commands for just this line's file.
;;;###autoload
(defun diredp-delete-this-file ()       ; Bound to `C-k', `delete'
  "In Dired, delete the file on the cursor line, upon confirmation."
  (interactive) (dired-do-delete 1))

;;;###autoload
(defun diredp-capitalize-this-file ()   ; Bound to `M-c'
  "In Dired, rename the file on the cursor line by capitalizing it.
Makes the first char of the name uppercase and the others lowercase."
  (interactive) (diredp-capitalize 1))

;;;###autoload
(defun diredp-downcase-this-file ()     ; Bound to `M-l'
  "In Dired, rename the file on the cursor line to lower case."
  (interactive) (dired-downcase 1))

;;;###autoload
(defun diredp-upcase-this-file ()       ; Bound to `M-u'
  "In Dired, rename the file on the cursor line to upper case."
  (interactive) (dired-upcase 1))

;;;###autoload
(defun diredp-rename-this-file ()       ; Bound to `r'
  "In Dired, rename the file on the cursor line."
  (interactive)
  (let ((use-file-dialog  nil))
    (dired-do-rename 1)))

;;;###autoload
(defun diredp-copy-this-file ()         ; Not bound
  "In Dired, copy the file on the cursor line."
  (interactive)
  (let ((use-file-dialog  nil))
    (dired-do-copy 1)))

;;;###autoload
(defun diredp-relsymlink-this-file ()   ; Bound to `y'
  "In Dired, make a relative symbolic link to file on cursor line."
  (interactive)
  (let ((use-file-dialog  nil))
    (and (fboundp 'dired-do-relsymlink) (dired-do-relsymlink 1))))

;;;###autoload
(defun diredp-symlink-this-file ()      ; Not bound
  "In Dired, make a symbolic link to the file on the cursor line."
  (interactive)
  (let ((use-file-dialog  nil))
    (dired-do-symlink 1)))

;;;###autoload
(defun diredp-hardlink-this-file ()     ; Not bound
  "In Dired, add a name (hard link) to the file on the cursor line."
  (interactive)
  (let ((use-file-dialog  nil))
    (dired-do-hardlink 1)))

;;;###autoload
(defun diredp-print-this-file ()        ; Bound to `M-p'
  "In Dired, print the file on the cursor line."
  (interactive) (dired-do-print 1))

;;;###autoload
(defun diredp-grep-this-file ()         ; Not bound
  "In Dired, grep the file on the cursor line."
  (interactive)
  (unless (and grep-command (or (< emacs-major-version 22)  (not grep-use-null-device)
                                (eq grep-use-null-device t)))
    (grep-compute-defaults))
  (grep (diredp-do-grep-1 (list (dired-get-filename t)))))

;;;###autoload
(defun diredp-compress-this-file ()     ; Bound to `z'
  "In Dired, compress or uncompress the file on the cursor line."
  (interactive) (dired-do-compress 1))

;;;###autoload
(defun diredp-shell-command-this-file (command) ; Not bound
  "In Dired, run a shell COMMAND on the file on the cursor line."
  (interactive
   (list (dired-read-shell-command (concat "! on " "%s: ") 1 (list (dired-get-filename t)))))
  (dired-do-shell-command command 1))

;;;###autoload
(defun diredp-bookmark-this-file (&optional prefix) ; Bound to `C-B' (`C-S-b')
  "In Dired, bookmark the file on the cursor line.
See `diredp-do-bookmark'."
  (interactive (progn (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: ")))))
  (diredp-do-bookmark prefix 1))

;;;###autoload
(defun diredp-tag-this-file (tags &optional prefix) ; Bound to `T +'
  "In Dired, add some tags to the file on the cursor line.
You need library `bookmark+.el' to use this command."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (bmkp-read-tags-completing)
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: ")))))
  (diredp-do-tag tags prefix 1))

;;;###autoload
(defun diredp-untag-this-file (tags &optional prefix arg) ; Bound to `T -'
  "In Dired, remove some tags from the file on the cursor line.
With a prefix arg, remove all tags from the file.
You need library `bookmark+.el' to use this command."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (let* ((pref  (and diredp-prompt-for-bookmark-prefix-flag
                                         (read-string "Prefix for bookmark name: ")))
                             (bmk   (bmkp-get-autofile-bookmark (dired-get-filename) nil pref))
                             (btgs  (and bmk (bmkp-get-tags bmk))))
                        (unless btgs (error "File has no tags to remove"))
                        (list (if current-prefix-arg btgs (bmkp-read-tags-completing btgs))
                              pref
                              current-prefix-arg))))
  (diredp-do-untag tags prefix 1))

;;;###autoload
(defun diredp-remove-all-tags-this-file (&optional prefix msgp) ; Bound to `T 0'
  "In Dired, remove all tags from this file.
You need library `bookmark+.el' to use this command."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            'MSG)))
  (bookmark-maybe-load-default-file)
  (diredp-do-remove-all-tags prefix 1))

;;;###autoload
(defun diredp-paste-add-tags-this-file (&optional prefix msgp) ; Bound to `T p', `T C-y'
  "In Dired, add previously copied tags to this file.
See `diredp-paste-add-tags'.
You need library `bookmark+.el' to use this command."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            'MSG)))
  (bookmark-maybe-load-default-file)
  (diredp-do-paste-add-tags prefix 1))

;;;###autoload
(defun diredp-paste-replace-tags-this-file (&optional prefix msgp) ; Bound to `T q'
  "In Dired, replace tags for this file with previously copied tags.
See `diredp-paste-replace-tags'.
You need library `bookmark+.el' to use this command."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            'MSG)))
  (bookmark-maybe-load-default-file)
  (diredp-do-paste-add-tags prefix 1))

;;;###autoload
(defun diredp-set-tag-value-this-file (tag value &optional prefix msgp) ; Bound to `T v'
  "In Dired, Set value of TAG to VALUE for this file.
See `diredp-set-tag-value'.
You need library `bookmark+.el' to use this command."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (bmkp-read-tag-completing)
                            (read (read-string "Value: "))
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            'MSG)))
  (bookmark-maybe-load-default-file)
  (diredp-do-set-tag-value tag value prefix 1))

;;;###autoload
(defun diredp-copy-tags-this-file (&optional prefix msgp) ; Bound to `T c', `T M-w'
  "In Dired, copy the tags from this file, so you can paste them to another.
See `diredp-copy-tags'.
You need library `bookmark+.el' to use this command."
  (interactive (progn (unless (require 'bookmark+ nil t)
                        (error "This command requires library `bookmark+.el'"))
                      (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            'MSG)))
  (bookmark-maybe-load-default-file)
  (let ((bmk  (bmkp-get-autofile-bookmark  (dired-get-filename) nil prefix)))
    (and bmk  (bmkp-copy-tags bmk msgp))))

;;;###autoload
(defun diredp-mouse-copy-tags (event)   ; Not bound
  "In Dired, copy the tags from this file, so you can paste them to another.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t)
        (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (diredp-copy-tags-this-file prefix 'MSG))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-describe-file (&optional internal-form-p) ; Bound to `C-h RET', `C-h C-RET'
  "In Dired, describe this file or directory.
You need library `help-fns+.el' to use this command.
If the file has an autofile bookmark and you use library `Bookmark+',
then show also the bookmark information (tags etc.).  In this case, a
prefix arg shows the internal form of the bookmark."
  (interactive "P")
  (unless (fboundp 'describe-file)
    (error "This command needs `describe-file' from library `help-fns+.el'"))
  (describe-file (dired-get-filename nil t) internal-form-p))

;;;###autoload
(defun diredp-mouse-describe-file (event &optional internal-form-p) ; Not bound
  "Describe the clicked file.
You need library `help-fns+.el' to use this command.
If the file has an autofile bookmark and you use library `Bookmark+',
then show also the bookmark information (tags etc.).  In this case, a
prefix arg shows the internal form of the bookmark."
  (interactive "e\nP")
  (unless (fboundp 'describe-file)
    (error "This command needs `describe-file' from library `help-fns+.el'"))
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename nil t))))
    (describe-file file internal-form-p)))

;;;###autoload
(defun diredp-byte-compile-this-file () ; Bound to `b'
  "In Dired, byte compile the (Lisp source) file on the cursor line."
  (interactive) (dired-do-byte-compile 1))

;;;###autoload
(defun diredp-load-this-file ()         ; Not bound
  "In Dired, load the file on the cursor line."
  (interactive) (dired-do-load 1))

;;;###autoload
(defun diredp-chmod-this-file ()        ; Bound to `M-m'
  "In Dired, change the mode of the file on the cursor line."
  (interactive) (dired-do-chmod 1))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-chgrp-this-file ()      ; Not bound
    "In Dired, change the group of the file on the cursor line."
    (interactive) (dired-do-chgrp 1)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-chown-this-file ()      ; Not bound
    "In Dired, change the owner of the file on the cursor line."
    (interactive) (dired-do-chown 1)))

(when (fboundp 'dired-do-touch)
  (defun diredp-touch-this-file ()        ; Not bound
    "In Dired, `touch' (change the timestamp of) the file on the cursor line."
    (interactive) (dired-do-touch 1)))


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; 1. Variable (symbol) `s' -> `blks'.
;; 2. Fixes to remove leading space from `uid' and allow `.' in `gid'.
;; 3. Cleaned up doc string and code a bit.
;;
;;;###autoload
(defun dired-mark-sexp (predicate &optional unmark-p) ; Bound to `M-(', `* ('
  "Mark files for which PREDICATE returns non-nil.
With non-nil prefix arg UNMARK-P, unmark those files instead.

PREDICATE is a lisp sexp that can refer to the following variables:

    `mode'   [string]  file permission bits, e.g. \"-rw-r--r--\"
    `nlink'  [integer] number of links to file
    `size'   [integer] file size in bytes
    `uid'    [string]  owner
    `gid'    [string]  group (If the gid is not displayed by `ls',
                       this will still be set (to the same as uid))
    `time'   [string]  the time that `ls' displays, e.g. \"Feb 12 14:17\"
    `name'   [string]  the name of the file
    `sym'    [string]  if file is a symbolic link, the linked-to name,
                       else \"\"
    `inode'  [integer] the inode of the file (only for `ls -i' output)
    `blks'   [integer] the size of the file for `ls -s' output
                       (ususally in blocks or, with `-k', in Kbytes)
Examples:
  Mark zero-length files: `(equal 0 size)'
  Mark files last modified on Feb 2: `(string-match \"Feb  2\" time)'
  Mark uncompiled Emacs Lisp files (`.el' file without a `.elc' file):
     First, dired just the source files: `dired *.el'.
     Then, use \\[dired-mark-sexp] with this sexp:
          (not (file-exists-p (concat name \"c\")))"

  ;; Using `sym' = "", instead of nil, for non-linked files avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Use `equal' instead of `=' in the example, as it works on integers and strings.
  (interactive "xVars: inode,blks,mode,nlink,uid,gid,size,time,name,sym -> \nP")
  (message "%s" predicate)
  (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char))
        (inode              nil)
        (blks               ())
        mode nlink uid gid size time name sym)
    (dired-mark-if
     (save-excursion
       (and
        ;; Sets vars INODE BLKS MODE NLINK UID GID SIZE TIME NAME and SYM
        ;; according to current file line.  Returns `t' for success, nil if
        ;; there is no file line.  Upon success, these vars are set, to either
        ;; nil or the appropriate value, so they need not be initialized.
        ;; Moves point within the current line.
        (dired-move-to-filename)
        (let ((mode-len             10) ; Length of mode string.
              ;; As in `dired.el', but with subexpressions \1=inode, \2=blks:
              (dired-re-inode-size  "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?")
              pos)
          (beginning-of-line)
          (forward-char 2)
          (when (looking-at dired-re-inode-size)
            (goto-char (match-end 0))
            (setq inode  (string-to-number (buffer-substring (match-beginning 1) (match-end 1)))
                  blks   (string-to-number (buffer-substring (match-beginning 2)
                                                             (match-end 2)))))
          (setq mode  (buffer-substring (point) (+ mode-len (point))))
          (forward-char mode-len)
          (setq nlink  (read (current-buffer)))
          (forward-char 1)              ; Fix: skip space.
          ;; Karsten Wenger <kw@cis.uni-muenchen.de> fixed uid.
          (setq uid  (buffer-substring (+ (point) 1) (progn (forward-word 1) (point))))
          (re-search-forward
           (if (< emacs-major-version 20)
               "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)"
             dired-move-to-filename-regexp))
          (goto-char (match-beginning 1))
          (forward-char -1)
          (setq size  (string-to-number (buffer-substring (save-excursion (backward-word 1)
                                                                          (setq pos  (point)))
                                                          (point))))
          (goto-char pos)
          (backward-word 1)
          ;; if no gid is displayed, gid will be set to uid
          ;; but user will then not reference it anyway in PREDICATE.
          (setq gid   (buffer-substring (save-excursion (forward-word 1) (point)) (point))
                time  (buffer-substring (match-beginning 1) (1- (dired-move-to-filename)))
                name  (buffer-substring (point) (or (dired-move-to-end-of-filename t) (point)))
                sym   (if (looking-at " -> ")
                          (buffer-substring (progn (forward-char 4) (point))
                                            (line-end-position))
                        "")))
        (eval predicate)))
     (format "'%s file" predicate))))

(defun diredp-this-file-marked-p (&optional mark-char)
  "Return non-nil if the file on this line is marked.
Optional arg MARK-CHAR is the type of mark to check.
 If nil, then if the file has any mark, including `D', it is marked."
  (and (dired-get-filename t t)
       (save-excursion (beginning-of-line)
                       (if mark-char
                           (looking-at (concat "^" (char-to-string mark-char)))
                         (not (looking-at "^ "))))))

(defun diredp-this-file-unmarked-p (&optional mark-char)
  "Return non-nil if the file on this line is unmarked.
Optional arg MARK-CHAR is the type of mark to check.
 If nil, then if the file has no mark, including `D', it is unmarked.
 If non-nil, then it is unmarked for MARK-CHAR if it has no mark or
 it has any mark except MARK-CHAR."
  (and (dired-get-filename t t)
       (save-excursion (beginning-of-line)
                       (if mark-char
                           (not (looking-at (concat "^" (char-to-string mark-char))))
                         (looking-at "^ ")))))

;;;###autoload
(defun diredp-mark-region-files (&optional unmark-p) ; Not bound
  "Mark all of the files in the current region (if it is active).
With non-nil prefix arg, unmark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (line-end-position)))
    (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (diredp-this-file-unmarked-p))
                     "region file"))))

;;;###autoload
(defun diredp-unmark-region-files (&optional mark-p) ; Not bound
  "Unmark all of the files in the current region (if it is active).
With non-nil prefix arg, mark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (line-end-position)))
    (let ((dired-marker-char  (if mark-p dired-marker-char ?\040)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (diredp-this-file-marked-p))
                     "region file"))))

;;;###autoload
(defun diredp-flag-region-files-for-deletion () ; Not bound
  "Flag all of the files in the current region (if it is active) for deletion."
  (interactive)
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (line-end-position)))
    (let ((dired-marker-char  dired-del-marker))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (diredp-this-file-unmarked-p ?\D))
                     "region file"))))

;;;###autoload
(defun diredp-toggle-marks-in-region (start end) ; Not bound
  "Toggle marks in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (if (not (fboundp 'dired-toggle-marks))
          ;; Pre-Emacs 22.  Use bol, eol.  If details hidden, show first.
          (let ((details-hidden-p  (and (boundp 'dired-details-state)
                                        (eq 'hidden dired-details-state))))
            (widen)
            (when details-hidden-p (dired-details-show))
            (goto-char start)
            (setq start  (line-beginning-position))
            (goto-char end)
            (setq end    (line-end-position))
            (narrow-to-region start end)
            (dired-do-toggle)
            (when details-hidden-p (dired-details-hide)))
        (narrow-to-region start end)
        (dired-toggle-marks))))
  (when (and (get-buffer-window (current-buffer)) (fboundp 'fit-frame-if-one-window))
    (fit-frame-if-one-window)))


;;; Mouse 3 menu.
;;;;;;;;;;;;;;;;;

(defvar diredp-file-line-overlay nil)

;;;###autoload
(defun diredp-mouse-3-menu (event)      ; Bound to `mouse-3'
  "Dired pop-up `mouse-3' menu, for files in selection or current line."
  (interactive "e")
  (if (not (and (fboundp 'mouse3-dired-use-menu)
                transient-mark-mode mark-active (not (eq (mark) (point)))))
      ;; No `mouse3.el' or no region.
      (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
          ;; Region
          (let ((reg-choice  (x-popup-menu
                              event
                              (list "Files in Region"
                                    (list ""
                                          '("Mark" . diredp-mark-region-files)
                                          '("Unmark" . diredp-unmark-region-files)
                                          '("Toggle Marked/Unmarked" .
                                            diredp-toggle-marks-in-region)
                                          '("Flag for Deletion" .
                                            diredp-flag-region-files-for-deletion))))))
            (when reg-choice (call-interactively reg-choice)))
        ;; Single file/dir (no region).
        (let ((mouse-pos                  (event-start event))
              ;; Do not use `save-excursion', because some commands will move point on purpose.
              ;; Just save original point and return to it unless MOVEP is set to non-nil.
              (opoint                     (point))
              (movep                      nil)
              (inhibit-field-text-motion  t) ; Just in case.
              choice bol  eol  file/dir-name)
          (with-current-buffer (window-buffer (posn-window mouse-pos))
            (goto-char (posn-point mouse-pos))
            (setq bol  (line-beginning-position)
                  eol  (line-end-position))
            (unwind-protect
                 (progn
                   (if diredp-file-line-overlay ; Don't re-create if exists.
                       (move-overlay diredp-file-line-overlay bol eol (current-buffer))
                     (setq diredp-file-line-overlay  (make-overlay bol eol))
                     (overlay-put diredp-file-line-overlay 'face 'region))
                   (setq file/dir-name  (and (not (eobp)) (dired-get-filename nil t)))
                   (when file/dir-name
                     (sit-for 0)
                     (let ((map
                            (easy-menu-create-menu
                             "This File"
                             ;; Stuff from `Mark' menu.
                             `(
                               ("Bookmark" :visible (featurep 'bookmark+)
                                ["Bookmark..." diredp-bookmark-this-file]
                                ["Tag..." diredp-tag-this-file
                                 :visible (featurep 'bookmark+)]
                                ["Untag..." diredp-untag-this-file
                                 :visible (featurep 'bookmark+)]
                                ["Remove All Tags" diredp-remove-all-tags-this-file
                                 :visible (featurep 'bookmark+)]
                                ["Copy Tags" diredp-copy-tags-this-file
                                 :visible (featurep 'bookmark+)]
                                ["Paste Tags (Add)" diredp-paste-add-tags-this-file
                                 :visible (featurep 'bookmark+)]
                                ["Paste Tags (Replace)" diredp-paste-replace-tags-this-file
                                 :visible (featurep 'bookmark+)]
                                ["Set Tag Value..." diredp-set-tag-value-this-file
                                 :visible (featurep 'bookmark+)]
                                )
                               ["Describe" diredp-describe-file]
                               ["Mark"  dired-mark
                                :visible (not (eql (dired-file-marker file/dir-name)
                                               dired-marker-char))]
                               ["Unmark" dired-unmark
                                :visible (dired-file-marker file/dir-name)]
                               ["Flag for Deletion" dired-flag-file-deletion
                                :visible (not (eql (dired-file-marker file/dir-name)
                                               dired-del-marker))]
                               ["Delete..." diredp-delete-this-file]
                               "--"     ; ------------------------------------------------------
                               ;; Stuff from `Single' / `Multiple' menus.
                               ["Open" dired-find-file]
                               ["Open in Other Window" dired-find-file-other-window]
                               ["Open in Other Frame" diredp-find-file-other-frame]
                               ["Open Associated Windows App" dired-w32-browser
                                :visible (featurep 'w32-browser)]
                               ["Open in Windows Explorer" dired-w32explore
                                :visible (featurep 'w32-browser)]
                               ["View (Read Only)" dired-view-file]
                               ["--" 'ignore ; -------------------------------------------------
                                :visible (or (atom (diredp-this-subdir)) ; Subdir line.
                                          (not (equal (dired-current-directory) ; Not top.
                                                default-directory)))]
                               ["Insert This Subdir"
                                (lambda () (interactive)
                                        (call-interactively #'dired-maybe-insert-subdir)
                                        (setq movep  t))
                                :visible (and (atom (diredp-this-subdir))
                                          (not (assoc (file-name-as-directory
                                                       (diredp-this-subdir))
                                                dired-subdir-alist)))
                                :enable (atom (diredp-this-subdir))]
                               ["Go To Inserted Subdir"
                                (lambda () (interactive)
                                        (call-interactively #'dired-maybe-insert-subdir)
                                        (setq movep  t))
                                :visible (and (atom (diredp-this-subdir))
                                          (assoc (file-name-as-directory (diredp-this-subdir))
                                           dired-subdir-alist))
                                :enable (atom (diredp-this-subdir))
                                :keys "i"]
                               ["Remove This Inserted Subdir" dired-kill-subdir
                                :visible (not (equal (dired-current-directory)
                                               default-directory))] ; In subdir, not top.
                               ["Dired This Inserted Subdir (Tear Off)"
                                (lambda () (interactive) (diredp-dired-this-subdir t))
                                :visible (not (equal (dired-current-directory)
                                               default-directory))] ; In subdir, not top.
                               "--"     ; ------------------------------------------------------
                               ["Compare..." diredp-ediff]
                               ["Diff..." dired-diff]
                               ["Diff with Backup" dired-backup-diff]

                               ["Bookmark..." diredp-bookmark-this-file
                                :visible (not (featurep 'bookmark+))]
                               "--"     ; ------------------------------------------------------
                               ["Copy to..." diredp-copy-this-file]
                               ["Rename to..." diredp-rename-this-file]
                               ["Capitalize" diredp-capitalize-this-file]
                               ["Upcase" diredp-upcase-this-file]
                               ["Downcase" diredp-downcase-this-file]
                               "--"     ; ------------------------------------------------------
                               ["Symlink to (Relative)..." diredp-relsymlink-this-file
                                :visible (fboundp 'dired-do-relsymlink)] ; In `dired-x.el'.
                               ["Symlink to..." diredp-symlink-this-file]
                               ["Hardlink to..." diredp-hardlink-this-file]
                               "--"     ; ------------------------------------------------------
                               ["Shell Command..." diredp-shell-command-this-file]
                               ["Print..." diredp-print-this-file]
                               ["Grep" diredp-grep-this-file]
                               ["Compress/Uncompress" diredp-compress-this-file]
                               ["Byte-Compile" diredp-byte-compile-this-file]
                               ["Load" diredp-load-this-file]
                               "--"     ; ------------------------------------------------------
                               ["Change Timestamp..." diredp-touch-this-file]
                               ["Change Mode..." diredp-chmod-this-file]
                               ["Change Group..." diredp-chgrp-this-file
                                :visible (fboundp 'diredp-chgrp-this-file)]
                               ["Change Owner..." diredp-chown-this-file
                                :visible (fboundp 'diredp-chown-this-file)]))))
                       (when diredp-file-line-overlay
                         (delete-overlay diredp-file-line-overlay))
                       (setq choice  (x-popup-menu event map))
                       (when choice
                         (call-interactively (lookup-key map (apply 'vector choice)))))))
              (unless movep (goto-char opoint))))))
    ;; `mouse3.el' and active region.
    (unless (eq mouse3-dired-function 'mouse3-dired-use-menu)
      (funcall #'mouse3-dired-use-menu)
      (revert-buffer))
    (let ((last-command  'mouse-save-then-kill))
      (mouse-save-then-kill event 'IGNORE))))


;; REPLACE ORIGINAL in `dired.el' for Emacs 20.
;;
;; Allow `.' and `..', by using non-nil second arg to `dired-get-filename'.
;;
(when (< emacs-major-version 21)
  (defun dired-find-file ()             ; Bound to `e', `RET'
    "In Dired, visit the file or directory named on this line."
    (interactive)
    (let* ((dgf-result  (or (dired-get-filename nil t) (error "No file on this line")))
           (file-name   (file-name-sans-versions dgf-result t)))
      (if (file-exists-p file-name)
          (find-file file-name)
        (if (file-symlink-p file-name)
            (error "File is a symlink to a nonexistent target")
          (error "File no longer exists; type `g' to update Dired buffer"))))))

;;;###autoload
(defun diredp-find-file-other-frame ()  ; Bound to `C-o'
  "In Dired, visit this file or directory in another frame."
  (interactive)
  (find-file-other-frame (file-name-sans-versions (dired-get-filename nil t) t)))

;;;###autoload
(defun diredp-mouse-find-file-other-frame (event) ; Bound to `M-mouse-2'
  "In Dired, visit file or directory clicked on in another frame."
  (interactive "e")
  (let ((pop-up-frames  t)) (dired-mouse-find-file-other-window event)))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Allow `.' and `..', by using non-nil second arg to `dired-get-filename'.
;;
;;;###autoload
(defun dired-mouse-find-file-other-window (event) ; Bound to `mouse-2'
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion (goto-char (posn-point (event-end event)))
                      (setq file  (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (find-file-other-window (file-name-sans-versions file t))))

;; But be aware that `dired-sort-menu.el' binds `S-mouse-2' to `dired-sort-menu-popup'.
;;
;;;###autoload
(defun diredp-mouse-find-file (event)   ; Bound to `S-mouse-2'
  "Replace dired in its window by this file or directory."
  (interactive "e")
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion (goto-char (posn-point (event-end event)))
                      (setq file  (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

;;;###autoload
(defun diredp-mouse-view-file (event)   ; Not bound
  "Examine this file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive "e")
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion (goto-char (posn-point (event-end event)))
                      (setq file  (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist) (dired-goto-subdir file))  (dired file))
      (view-file file))))               ; In `view.el'.

;;;###autoload
(defun diredp-mouse-ediff (event)       ; Not bound
  "Compare this file (pointed by mouse) with file FILE2 using `ediff'.
FILE2 defaults to this file as well.  If you enter just a directory
name for FILE2, then this file is compared with a file of the same
name in that directory.  FILE2 is the second file given to `ediff';
this file is the first given to it."
  (interactive "e")
  (require 'ediff)
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (call-interactively 'diredp-ediff)))

;;;###autoload
(defun diredp-mouse-diff (event &optional switches) ; Not bound
  "Compare this file (pointed by mouse) with file FILE2 using `diff'.
FILE2 defaults to the file at the mark.  This file is the first file
given to `diff'.  With prefix arg, prompt for second arg SWITCHES,
which are options for `diff'."
  (interactive "e")
  (let ((default    (and (mark t)  (save-excursion (goto-char (mark t))
                                                   (dired-get-filename t t))))
        (mouse-pos  (event-start event)))
    (require 'diff)
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (let ((file2  (read-file-name (format "Diff %s with: %s"
                                          (dired-get-filename t)
                                          (if default (concat "(default " default ") ") ""))
                                  (dired-current-directory) default t)))
      (setq switches  (and current-prefix-arg
                           (if (fboundp 'icicle-read-string-completing)
                               (icicle-read-string-completing
                                "Options for diff: "
                                (if (stringp diff-switches)
                                    diff-switches
                                  (mapconcat 'identity diff-switches " "))
                                (lambda (c) (string-match "switches" (symbol-name c))))
                             (read-string "Options for diff: "
                                          (if (stringp diff-switches)
                                              diff-switches
                                            (mapconcat 'identity diff-switches " "))))))
      (diff file2 (dired-get-filename t) switches))))

;;;###autoload
(defun diredp-mouse-backup-diff (event) ; Not bound
  "Diff this file with its backup file or vice versa.
Use the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for SWITCHES which are the options for `diff'."
  (interactive "e")
  (let ((switches   (and current-prefix-arg
                         (if (fboundp 'icicle-read-string-completing)
                             (icicle-read-string-completing
                              "Options for diff: "
                              (if (stringp diff-switches)
                                  diff-switches
                                (mapconcat 'identity diff-switches " "))
                              (lambda (c) (string-match "switches" (symbol-name c))))
                           (read-string "Options for diff: "
                                        (if (stringp diff-switches)
                                            diff-switches
                                          (mapconcat #'identity diff-switches " "))))))
        (mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (diff-backup (dired-get-filename) switches)))

;;;###autoload
(defun diredp-mouse-mark (event)        ; Not bound
  "In Dired, mark this file.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks,
and \\[dired-unmark] on a subdir to remove the marks in this subdir."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (if (and (cdr dired-subdir-alist) (dired-get-subdir))
      (save-excursion (dired-mark-subdir-files))
    (let (buffer-read-only)
      (dired-repeat-over-lines 1 #'(lambda () (delete-char 1) (insert dired-marker-char)))
      (dired-previous-line 1))))

;;;###autoload
(defun diredp-mouse-unmark (event)      ; Not bound
  "In Dired, unmark this file.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char  ?\040)) (dired-mark nil))
  (dired-previous-line 1))

;;; This can be bound to [C-down-mouse-1] to give behavior similar to Windows Explorer.
;;; However, Emacs generally uses [C-down-mouse-1] for `mouse-buffer-menu'.
;;;###autoload
(defun diredp-mouse-mark/unmark (event) ; Not bound
  "Mark/unmark file or directory at mouse EVENT."
  (interactive "e")
  (let* ((mouse-pos                  (event-start event))
         (inhibit-field-text-motion  t) ; Just in case.
         (file/dir-name              (with-current-buffer (window-buffer (posn-window mouse-pos))
                                       (save-excursion
                                         (goto-char (posn-point mouse-pos))
                                         (and (not (eobp)) (dired-get-filename nil t))))))
    ;; Return nil iff not on a file or directory name.
    (and file/dir-name (cond ((dired-file-marker file/dir-name)
                              (diredp-mouse-unmark event)
                              (message "Unmarked: %s" file/dir-name))
                             (t
                              (diredp-mouse-mark event)
                              (message "Marked: %s" file/dir-name))))))

;; This can be bound to [S-mouse-1] to give behavior similar to Windows Explorer.
;; If you do that, consider binding `diredp-mouse-mark/unmark' to `C-mouse-1'.
;; Alternatively, just bind `diredp-mouse-mark/unmark-mark-region-files' to [S-mouse-1].
;;;###autoload
(defun diredp-mouse-mark-region-files (event) ; Bound to `S-mouse-1'
  "Mark files between point and the mouse."
  (interactive "e")
  (call-interactively 'mouse-save-then-kill)
  (diredp-mark-region-files))

;; This can be bound to [S-mouse-1] to give behavior similar to Windows Explorer.
;; If you don't bind `diredp-mouse-mark/unmark' to, for instance, `C-mouse-1', then
;; Consider binding this to [S-mouse-1].
;;;###autoload
(defun diredp-mouse-mark/unmark-mark-region-files (event) ; Not bound
  "Mark/unmark file or mark files in region.
If the file the cursor is on is marked, then mark all files between it
 and the line clicked (included).
Otherwise (cursor's file is unmarked):
 If the file clicked is marked, then unmark it.
 If it is unmarked, then mark it."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    ;; If same click same line as cursor, or cursor's line is marked,
    ;; Then toggle the clicked line's mark.
    ;; Else mark all files in region between point and clicked line (included).
    (if (or (eq (count-lines (point-min) (posn-point mouse-pos))
              (count-lines (point-min) (point)))
            (equal dired-marker-char (dired-file-marker (dired-get-filename nil t))))
        (diredp-mouse-mark/unmark event)
      (call-interactively 'mouse-save-then-kill)
      (diredp-mark-region-files))))

;;;###autoload
(defun diredp-mouse-flag-file-deletion (event) ; Not bound
  "In Dired, flag this file for deletion.
If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char  dired-del-marker)) (dired-mark 1))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-copy (event)     ; Not bound
  "In Dired, copy this file.
This normally preserves the last-modified date when copying."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'copy #'dired-copy-file (if dired-copy-preserve-time "Copy [-p]" "Copy")
                         1 dired-keep-marker-copy))

;;;###autoload
(defun diredp-mouse-do-rename (event)   ; Not bound
  "In Dired, rename this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'move #'dired-rename-file "Move" 1 dired-keep-marker-rename "Rename"))

;;;###autoload
(defun diredp-mouse-upcase (event)      ; Not bound
  "In Dired, rename this file to upper case."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory #'upcase "Rename to uppercase:" nil))

;;;###autoload
(defun diredp-mouse-downcase (event)    ; Not bound
  "In Dired, rename this file to lower case."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory #'downcase "Rename to lowercase:" nil))

;;;###autoload
(defun diredp-mouse-do-delete (event)   ; Not bound
  "In Dired, delete this file, upon confirmation."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (diredp-internal-do-deletions (dired-map-over-marks (cons (dired-get-filename)
                                                            (point)) 1)
                                1
                                'USE-TRASH-CAN) ; This arg is for Emacs 24+ only.
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-shell-command (event) ; Not bound
  "Run a shell COMMAND on this file.
If there is output, it goes to a separate buffer.

No automatic redisplay of dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir."
  ;;Functions dired-run-shell-command and dired-shell-stuff-it do the
  ;;actual work and can be redefined for customization.
  (interactive "e")
  (lexical-let ((mouse-pos  (event-start event))
                (command    (dired-read-shell-command "! on %s: " nil
                                                      (dired-get-marked-files t nil))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-bunch-files (- 10000 (length command))
                       #'(lambda (&rest files)
                           (dired-run-shell-command (dired-shell-stuff-it command files t 1)))
                       nil
                       (dired-get-marked-files t 1))))

;;;###autoload
(defun diredp-mouse-do-symlink (event)  ; Not bound
  "Make symbolic link to this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'symlink #'make-symbolic-link "Symlink" 1 dired-keep-marker-symlink))

;;;###autoload
(defun diredp-mouse-do-hardlink (event) ; Not bound
  "Make hard link (alias) to this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'hardlink #'add-name-to-file "Hardlink" 1 dired-keep-marker-hardlink))

;;;###autoload
(defun diredp-mouse-do-print (event)    ; Not bound
  "Print this file.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let* ((file     (dired-get-filename))
         (command  (dired-mark-read-string "Print %s with: "
                                           (apply 'concat lpr-command " " lpr-switches)
                                           'print 1 (list file))))
    (dired-run-shell-command (dired-shell-stuff-it command (list file) nil))))

;;;###autoload
(defun diredp-mouse-do-grep (event)     ; Not bound
  "Run grep against this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (unless grep-command (grep-compute-defaults))
  (grep (diredp-do-grep-1 (list (dired-get-filename t)))))

;;;###autoload
(defun diredp-mouse-do-compress (event) ; Not bound
  "Compress or uncompress this file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-compress 1 'compress t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-byte-compile (event) ; Not bound
  "Byte compile this file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-byte-compile 1 'byte-compile t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-load (event)     ; Not bound
  "Load this Emacs Lisp file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-load 1 'load t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chmod (event)    ; Not bound
  "Change the mode of this file.
This calls chmod, so symbolic modes like `g+w' are allowed."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Mode" "chmod" 'chmod 1)
  (dired-previous-line 1))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-mouse-do-chgrp (event)  ; Not bound
    "Change the group of this file."
    (interactive "e")
    (let ((mouse-pos  (event-start event)))
      (select-window (posn-window mouse-pos))
      (goto-char (posn-point mouse-pos)))
    (dired-do-chxxx "Group" "chgrp" 'chgrp 1)
    (dired-previous-line 1)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-mouse-do-chown (event)  ; Not bound
    "Change the owner of this file."
    (interactive "e")
    (let ((mouse-pos  (event-start event)))
      (select-window (posn-window mouse-pos))
      (goto-char (posn-point mouse-pos)))
    (dired-do-chxxx "Owner" dired-chown-program 'chown 1)
    (dired-previous-line 1)))


;;; Dired+ Help

;;;###autoload
(defun diredp-describe-mode (&optional buffer)
  "Describe Dired mode, including Dired+ features.
This is `describe-mode' plus a description of Dired+ features.
For just the latter, use \\<dired-mode-map>`\\[diredp-dired-plus-help]'."
  (interactive "@")
  (unless (derived-mode-p 'dired-mode)
    (error "Use `diredp-dired-plus-help' if you want information about Dired+"))
  (with-current-buffer (or buffer (current-buffer)) (describe-mode))
  (with-current-buffer (get-buffer-create "*Help*")
    (save-excursion
      (goto-char (point-min))
      (diredp-dired-plus-help-link)
      (let ((buffer-read-only  nil)) (insert "\n"))
      (when (re-search-forward "Keybindings:\nkey\\s-+binding\n---\\s-+-------" nil t)
        (goto-char (match-beginning 0))
        (let ((buffer-read-only  nil))
          (insert "\f\n")
          (diredp-dired-plus-description+links)
          (insert "\f\n"))))))

;;;###autoload
(defun diredp-dired-plus-help ()
  "Describe Dired+."
  (interactive "@")
  (with-output-to-temp-buffer "*Help*" (diredp-dired-plus-description+links)))

(defun diredp-dired-plus-description+links ()
  "Insert Dired+ help text in `*Help*'."
  (with-current-buffer (get-buffer-create "*Help*")
    (let ((buffer-read-only  nil))
      (save-restriction
        (narrow-to-region (point) (point))
        (diredp-dired-plus-help-link)
        (insert (diredp-dired-plus-description))
        (goto-char (point-max))
        (insert "\n")
        (diredp-dired-plus-help-link)))))

(when (and (> emacs-major-version 21)
           (require 'help-mode nil t) (get 'help-xref 'button-category-symbol)) ; `button.el'
  (define-button-type 'diredp-help-button
      :supertype 'help-xref
      'help-function #'(lambda () (browse-url "http://www.emacswiki.org/cgi-bin/wiki/DiredPlus"))
      'help-echo
      (purecopy "mouse-2, RET: Dired+ documentation on the Emacs Wiki (requires \
Internet access)")))

(defun diredp-dired-plus-help-link ()
  "Add Web link for Dired+ help, and reminder about sending bug report."
  ;; Don't bother to do this for Emacs 21.3.  Its `help-insert-xref-button' is different.
  (when (and (> emacs-major-version 21)
             (require 'help-mode nil t) (fboundp 'help-insert-xref-button)) ; `help-mode.el'.
    (let ((buffer-read-only  nil))
      (help-insert-xref-button "[Dired+ Help on the Web]" 'diredp-help-button)
      (insert (substitute-command-keys
               "\t\tSend a Dired+ bug report:\n\t\t\t\t\t`\\[diredp-send-bug-report]'\n")))))

(defun diredp-dired-plus-description ()
  "Dired+ description."
  (substitute-command-keys
   (concat
    "\\<dired-mode-map>\
              Dired+ Features
              ---------------

Most keys listed here are in addition to those for vanilla Dired.

Menus
-----

Many Dired+ actions are available from the menu-bar menus and the
`mouse-3' context menu.  This may include commands shown here as not
being bound to keys (i.e., listed as `M-x ...').

General
-------

* \\[diredp-toggle-find-file-reuse-dir]\t- Toggle reuse of directories
"
    (and (fboundp 'diredp-w32-drives)
         "* \\[diredp-w32-drives]\t\t- Go up to a list of MS Windows drives
")

"
* \\[diredp-marked-other-window]\t\t\t\t- Open Dired on marked
* \\[diredp-fileset]\t\t- Open Dired on files in a fileset
* \\[diredp-dired-for-files]\t- Open Dired on specific files
* \\[diredp-dired-union]\t- Create union of some Dired buffers
* \\[diredp-dired-inserted-subdirs]\t- Dired each inserted subdir

Mouse
-----

* \\[diredp-mouse-3-menu]\t- Context-sensitive menu
"

    (and (where-is-internal 'diredp-mouse-describe-file dired-mode-map)
         "* \\[diredp-mouse-describe-file]\t- Describe this
")

    "* \\[diredp-mouse-mark-region-files]\t\t- Mark all in region
"

    (and (where-is-internal 'diredp-mouse-find-file dired-mode-map)
         "* \\[diredp-mouse-find-file]\t- Open in this window
")
    (and (where-is-internal 'diredp-mouse-find-file-reuse-dir-buffer dired-mode-map)
         "* \\[diredp-mouse-find-file-reuse-dir-buffer]\t- Open in this window
")

    (and (where-is-internal 'dired-mouse-find-file-other-window dired-mode-map)
         "* \\[dired-mouse-find-file-other-window]\t\t- Open in another window
")

    "* \\[diredp-mouse-find-file-other-frame]\t\t- Open in another frame
"

    (and (fboundp 'dired-mouse-w32-browser) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser dired-mode-map)
         "* \\[dired-mouse-w32-browser]\t- MS Windows `Open' action
")

    (and (fboundp 'dired-mouse-w32-browser-reuse-dir-buffer) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser-reuse-dir-buffer dired-mode-map)
         "* \\[dired-mouse-w32-browser-reuse-dir-buffer]\t- MS Windows `Open' action
")

    "
Marking
-------

* \\[dired-mark]\t\t- Mark this
* \\[dired-unmark]\t\t- Unmark this
* \\[dired-do-toggle]\t- Toggle marked/unmarked
* \\[dired-mark-sexp]\t\t- Mark all satisfying a predicate
* \\[dired-unmark-all-marks]\t\t- Unmark all
* \\[diredp-mark/unmark-extension]\t\t- Mark/unmark all that have a given extension
* \\[diredp-mark-files-tagged-regexp]\t\t- Mark those with a tag that matches a regexp
* \\[diredp-unmark-files-tagged-regexp]\t\t- Unmark those with a tag that matches a regexp
* \\[diredp-mark-files-tagged-all]\t\t- Mark those with all of the given tags
* \\[diredp-unmark-files-tagged-all]\t\t- Unmark those with all of the given tags
* \\[diredp-mark-files-tagged-some]\t\t- Mark those with some of the given tags
* \\[diredp-unmark-files-tagged-some]\t\t- Unmark those with some of the given tags
* \\[diredp-mark-files-tagged-not-all]\t- Mark those without some of the given tags
* \\[diredp-unmark-files-tagged-not-all]\t- Unmark those without some of the given tags
* \\[diredp-mark-files-tagged-none]\t- Mark those with none of the given tags
* \\[diredp-unmark-files-tagged-none]\t- Unmark those with none of the given tags
"

    "
Current file/subdir (current line)
----------------------------------

* \\[diredp-describe-file]\t- Describe
* \\[diredp-byte-compile-this-file]\t\t- Byte-compile
* \\[diredp-compress-this-file]\t\t- Compress/uncompress
* \\[diredp-print-this-file]\t\t- Print
* \\[diredp-relsymlink-this-file]\t\t- Create relative symlink
* \\[diredp-delete-this-file]\t\t- Delete (with confirmation)
* \\[diredp-rename-this-file]\t\t- Rename
* \\[diredp-capitalize-this-file]\t\t- Capitalize (rename)
* \\[diredp-upcase-this-file]\t\t- Rename to uppercase
* \\[diredp-downcase-this-file]\t\t- Rename to lowercase
* \\[diredp-ediff]\t\t- Ediff
"
    (and (fboundp 'diredp-tag-this-file) ; In `bookmark+-1.el'.
         "* \\[diredp-tag-this-file]\t\t- Add some tags to this
* \\[diredp-untag-this-file]\t\t- Remove some tags from this
* \\[diredp-remove-all-tags-this-file]\t\t- Remove all tags from this
* \\[diredp-copy-tags-this-file]\t\t- Copy the tags from this
* \\[diredp-paste-add-tags-this-file]\t\t- Paste (add) copied tags to this
* \\[diredp-paste-replace-tags-this-file]\t\t- Paste (replace) tags for this
* \\[diredp-set-tag-value-this-file]\t\t- Set a tag value for this
")

    (and (fboundp 'diredp-bookmark-this-file) ; In `bookmark+-1.el'.
         "* \\[diredp-bookmark-this-file]\t\t- Bookmark
")

    (and (fboundp 'dired-mouse-w32-browser) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser dired-mode-map)
         "* \\[dired-mouse-w32-browser]\t- MS Windows `Open' action
* \\[dired-w32explore]\t- MS Windows Explorer
")

    (and (fboundp 'dired-mouse-w32-browser-reuse-dir-buffer) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser-reuse-dir-buffer dired-mode-map)
         "* \\[dired-mouse-w32-browser-reuse-dir-buffer]\t- MS Windows `Open' action
* \\[dired-w32explore]\t- MS Windows Explorer
")

    "
Marked (or next prefix arg) files & subdirs here
------------------------------------------------

* \\[diredp-insert-subdirs]\t\t- Insert marked subdirectories
* \\[dired-copy-filename-as-kill]\t\t- Copy names for pasting
* \\[dired-do-find-marked-files]\t\t- Visit
* \\[dired-do-copy]\t\t- Copy
* \\[dired-do-rename]\t\t- Rename/move
* \\[diredp-do-grep]\t\t- Run `grep'
* \\[dired-do-search]\t\t- Search
"

    (if (fboundp 'dired-do-query-replace-regexp) ; Emacs 22+
         "* \\[dired-do-query-replace-regexp]\t\t- Query-replace
"
      "* \\[dired-do-query-replace]\t\t- Query-replace
")

    (and (fboundp 'dired-do-isearch)
         "* \\[dired-do-isearch]\t- Isearch
* \\[dired-do-isearch-regexp]\t- Regexp isearch
")

    "* \\[dired-do-shell-command]\t\t- Run shell command
* \\[diredp-marked-other-window]\t\t- Dired
* \\[dired-do-compress]\t\t- Compress
* \\[dired-do-byte-compile]\t\t- Byte-compile
* \\[dired-do-load]\t\t- Load (Emacs Lisp)

* \\[diredp-omit-marked]\t- Omit
* \\[diredp-omit-unmarked]\t- Omit unmarked
"

    (and (fboundp 'diredp-do-tag) ; In `bookmark+-1.el'.
         "
* \\[diredp-do-tag]\t\t- Add some tags to marked
* \\[diredp-do-untag]\t\t- Remove some tags from marked
* \\[diredp-do-remove-all-tags]\t\t- Remove all tags from marked
* \\[diredp-do-paste-add-tags]\t- Paste (add) copied tags to marked
* \\[diredp-do-paste-replace-tags]\t\t- Paste (replace) tags for marked
* \\[diredp-do-set-tag-value]\t\t- Set a tag value for marked
* \\[diredp-mark-files-tagged-regexp]\t\t- Mark those with a tag that matches a regexp
* \\[diredp-mark-files-tagged-all]\t\t- Mark those with all of the given tags
* \\[diredp-mark-files-tagged-some]\t\t- Mark those with some of the given tags
* \\[diredp-mark-files-tagged-not-all]\t- Mark those without some of the given tags
* \\[diredp-mark-files-tagged-none]\t- Mark those with none of the given tags
* \\[diredp-unmark-files-tagged-regexp]\t\t- Unmark those with a tag that matches a regexp
* \\[diredp-unmark-files-tagged-all]\t\t- Unmark those with all of the given tags
* \\[diredp-unmark-files-tagged-some]\t\t- Unmark those with some of the given tags
* \\[diredp-unmark-files-tagged-not-all]\t- Unmark those without some of the given tags
* \\[diredp-unmark-files-tagged-none]\t- Unmark those with none of the given tags
")

    (and (fboundp 'diredp-do-bookmark) ; In `bookmark+-1.el'.
         "
* \\[diredp-do-bookmark]\t\t- Bookmark
* \\[diredp-set-bookmark-file-bookmark-for-marked]\t\t- \
Bookmark and create bookmark-file bookmark
* \\[diredp-do-bookmark-in-bookmark-file]\t- Bookmark in specific bookmark file
")

    (and (fboundp 'dired-multiple-w32-browser) ; In `w32-browser.el'.
         "
* \\[dired-multiple-w32-browser]\t- MS Windows `Open' action
")

    "
Marked files here and below (in marked subdirs)
-----------------------------------------------

* \\[diredp-insert-subdirs-recursive]\t\t- Insert marked subdirectories
* \\[diredp-copy-filename-as-kill-recursive]\t\t- Copy names for pasting
* \\[diredp-do-find-marked-files-recursive]\t\t\t- Visit
* \\[diredp-do-copy-recursive]\t\t\t- Copy
* \\[diredp-do-move-recursive]\t\t\t- Move
* \\[diredp-do-grep-recursive]\t\t- `grep'
* \\[diredp-do-search-recursive]\t\t\t- Search
* \\[diredp-do-query-replace-regexp-recursive]\t\t\t- Query-replace
* \\[diredp-do-isearch-recursive]\t\t- Isearch
* \\[diredp-do-isearch-regexp-recursive]\t- Regexp isearch
* \\[diredp-do-shell-command-recursive]\t\t\t- Run shell command
* \\[diredp-marked-recursive-other-window]\t\t- Dired
* \\[diredp-list-marked-recursive]\t\t\t- List

* \\[diredp-do-bookmark-recursive]\t\t- Bookmark
* \\[diredp-do-bookmark-in-bookmark-file-recursive]\t\t- Bookmark in bookmark file
* \\[diredp-set-bookmark-file-bookmark-for-marked-recursive]\t\t- Create bookmark-file bookmark
"
    (and (fboundp 'dired-multiple-w32-browser) ; In `w32-browser.el'.
         "
* \\[diredp-multiple-w32-browser-recursive]\t- MS Windows `Open'
")


    (and (fboundp 'diredp-tag-this-file) ; In `bookmark+-1.el'.
         "
Tagging
-------

* \\[diredp-tag-this-file]\t\t- Add some tags to this
* \\[diredp-untag-this-file]\t\t- Remove some tags from this
* \\[diredp-remove-all-tags-this-file]\t\t- Remove all tags from this
* \\[diredp-copy-tags-this-file]\t\t- Copy the tags from this
* \\[diredp-paste-add-tags-this-file]\t\t- Paste (add) copied tags to this
* \\[diredp-paste-replace-tags-this-file]\t\t- Paste (replace) tags for this
* \\[diredp-set-tag-value-this-file]\t\t- Set a tag value for this
* \\[diredp-do-tag]\t\t- Add some tags to marked
* \\[diredp-do-untag]\t\t- Remove some tags from marked
* \\[diredp-do-remove-all-tags]\t\t- Remove all tags from marked
* \\[diredp-do-paste-add-tags]\t- Paste (add) copied tags to marked
* \\[diredp-do-paste-replace-tags]\t\t- Paste (replace) tags for marked
* \\[diredp-do-set-tag-value]\t\t- Set a tag value for marked
* \\[diredp-mark-files-tagged-regexp]\t\t- Mark those with a tag that matches a regexp
* \\[diredp-mark-files-tagged-all]\t\t- Mark those with all of the given tags
* \\[diredp-mark-files-tagged-some]\t\t- Mark those with some of the given tags
* \\[diredp-mark-files-tagged-not-all]\t- Mark those without some of the given tags
* \\[diredp-mark-files-tagged-none]\t- Mark those with none of the given tags
* \\[diredp-unmark-files-tagged-regexp]\t\t- Unmark those with a tag that matches a regexp
* \\[diredp-unmark-files-tagged-all]\t\t- Unmark those with all of the given tags
* \\[diredp-unmark-files-tagged-some]\t\t- Unmark those with some of the given tags
* \\[diredp-unmark-files-tagged-not-all]\t- Unmark those without some of the given tags
* \\[diredp-unmark-files-tagged-none]\t- Unmark those with none of the given tags
")

    (and (fboundp 'diredp-bookmark-this-file) ; In `bookmark+-1.el'.
         "
Bookmarking
-----------

* \\[diredp-bookmark-this-file]\t\t- Bookmark this
* \\[diredp-do-bookmark]\t\t- Bookmark marked
* \\[diredp-set-bookmark-file-bookmark-for-marked]\t\t- \
Bookmark marked and create bookmark-file bookmark
* \\[diredp-do-bookmark-in-bookmark-file]\t- Bookmark marked, in specific bookmark file
")
    )))

(when (> emacs-major-version 21)
  (defun diredp-nb-marked-in-mode-name ()
    "Add number of marked and flagged lines to mode name in the mode line.
\(Flagged means flagged for deletion.)
If the current line is marked/flagged and there are others
marked/flagged after it then show `N/M', where N is the number
marked/flagged through the current line and M is the total number
marked/flagged.

Also abbreviate `mode-name', using \"Dired/\" instead of \"Dired by\"."
    (let ((mname  (format-mode-line mode-name)))
      (unless (get-text-property 0 'dired+-mode-name mname) ; Do it only once.
        (save-match-data
          (setq mode-name
                `(,(propertize (if (string-match "^[dD]ired \\(by \\)?\\(.*\\)" mname)
                                   (format "Dired/%s" (match-string 2 mname))
                                 mname)
                               'dired+-mode-name t)
                  (:eval (let* ((dired-marker-char  (if (eq ?D dired-marker-char)
                                                        ?* ; `dired-do-flagged-delete' binds it.
                                                      dired-marker-char))
                                (marked-regexp      (dired-marker-regexp))
                                (nb-marked          (count-matches marked-regexp
                                                                   (point-min) (point-max))))
                           (if (not (> nb-marked 0))
                               ""
                             (propertize
                              (format " %s%d%c"
                                      (save-excursion
                                        (forward-line 0)
                                        (if (looking-at (concat marked-regexp ".*"))
                                            (format "%d/" (1+ (count-matches
                                                               marked-regexp
                                                               (point-min) (point))))
                                          ""))
                                      nb-marked dired-marker-char)
                              'face 'diredp-mode-line-marked 'dired+-mode-name t))))
                  (:eval (let* ((flagged-regexp  (let ((dired-marker-char  dired-del-marker))
                                                   (dired-marker-regexp)))
                                (nb-flagged      (count-matches flagged-regexp
                                                                (point-min) (point-max))))
                           (if (not (> nb-flagged 0))
                               ""
                             (propertize
                              (format " %s%dD"
                                      (save-excursion
                                        (forward-line 0)
                                        (if (looking-at (concat flagged-regexp ".*"))
                                            (format "%d/" (1+ (count-matches
                                                               flagged-regexp
                                                               (point-min) (point))))
                                          ""))
                                      nb-flagged)
                              'face 'diredp-mode-line-flagged))))))))))

  (defface diredp-mode-line-marked
      '((((background dark))
         (:foreground "#6B6BFFFF2C2C")) ; a bright green
        (t (:foreground "DarkViolet")))
    "*Face for marked number in mode line `mode-name' for Dired buffers."
    :group 'Dired-Plus :group 'font-lock-highlighting-faces)

  (defface diredp-mode-line-flagged
      '((t (:foreground "Red")))
    "*Face for flagged number in mode line `mode-name' for Dired buffers."
    :group 'Dired-Plus :group 'font-lock-highlighting-faces)


  (add-hook 'dired-after-readin-hook 'diredp-nb-marked-in-mode-name)
  ;; This one is needed for `find-dired', because it does not call `dired-readin'.
  (add-hook 'dired-mode-hook         'diredp-nb-marked-in-mode-name))

;;;###autoload
(defun diredp-send-bug-report ()
  "Send a bug report about a Dired+ problem."
  (interactive)
  (browse-url (format (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
Dired+ bug: \
&body=Describe bug below, using a precise recipe that starts with `emacs -Q' or `emacs -q'.  \
File `dired+.el' has a header `Update #' that you can use to identify it.\
%%0A%%0AEmacs version: %s.")
                      (emacs-version))))

;;;;;;;;;;;;

(setq diredp-loaded-p  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired+.el ends here

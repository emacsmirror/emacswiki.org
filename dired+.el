;;; dired+.el --- Extensions to Dired.
;;
;; Filename: dired+.el
;; Description: Extensions to Dired.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2023, Drew Adams, all rights reserved.
;; Created: Fri Mar 19 15:58:58 1999
;; Version: 2023.01.14
;; Package-Requires: ()
;; Last-Updated: Sat Jan 14 11:52:02 2023 (-0800)
;;           By: dradams
;;     Update #: 13434
;; URL: https://www.emacswiki.org/emacs/download/dired%2b.el
;; Doc URL: https://www.emacswiki.org/emacs/DiredPlus
;; Keywords: unix, mouse, directories, diredp, dired
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x, 27.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `auth-source', `autofit-frame', `avoid',
;;   `backquote', `bookmark', `bookmark+', `bookmark+-1',
;;   `bookmark+-bmu', `bookmark+-key', `bookmark+-lit', `button',
;;   `bytecomp', `cconv', `cl', `cl-generic', `cl-lib', `cl-macs',
;;   `cmds-menu', `col-highlight', `crosshairs', `custom', `dired',
;;   `dired+', `dired-aux', `dired-loaddefs', `dired-x', `doremi',
;;   `doremi-frm', `easymenu', `eieio', `eieio-core',
;;   `eieio-loaddefs', `epg-config', `facemenu', `facemenu+',
;;   `faces', `faces+', `fit-frame', `font-lock', `font-lock+',
;;   `font-lock-menus', `format-spec', `frame-cmds', `frame-fns',
;;   `gv', `help+', `help-fns', `help-fns+', `help-macro',
;;   `help-macro+', `help-mode', `hexrgb', `highlight', `hl-line',
;;   `hl-line+', `image', `image-dired', `image-file', `image-mode',
;;   `info', `info+', `kmacro', `macroexp', `menu-bar', `menu-bar+',
;;   `misc-cmds', `misc-fns', `mwheel', `naked', `package',
;;   `palette', `password-cache', `pp', `pp+', `radix-tree', `rect',
;;   `replace', `ring', `second-sel', `seq', `strings', `syntax',
;;   `tabulated-list', `text-mode', `thingatpt', `thingatpt+',
;;   `timer', `url-handlers', `url-parse', `url-vars', `vline',
;;   `w32-browser', `w32browser-dlgopen', `wid-edit', `wid-edit+',
;;   `widget', `zones'.
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
;;
;;  Wraparound Navigation
;;  ---------------------
;;
;;  In vanilla Dired, `dired-next-marked-file' (`M-}' or `* C-n') and
;;  `dired-previous-marked-file' (`M-{' or `* C-p') wrap around when
;;  you get to the end or the beginning of the Dired buffer.  Handy.
;;
;;  But the other navigation commands do not wrap around.  In `Dired+'
;;  they do, provided option `diredp-wrap-around-flag' is non-nil,
;;  which it is by default.  This means the following commands:
;;
;;    `diredp-next-line'     - `n', `C-n', `down', `SPC'
;;    `diredp-previous-line' - `p', `C-p', `up'
;;    `diredp-next-dirline'  - `>'
;;    `diredp-prev-dirline'  - `<'
;;    `diredp-next-subdir'   - `C-M-n'
;;    `diredp-prev-subdir'   - `C-M-p'
;;
;;
;;  Quick Viewing While Navigating
;;  ------------------------------
;;
;;  You can use key `C-down' or `C-up' to navigate to the next or
;;  previous file line, respectively, and at the same time show its
;;  file in another window.  The focus remains on the Dired buffer.
;;  A numeric prefix arg means move that many lines first.
;;
;;  Names of files and directories that match either of the options
;;  `diredp-visit-ignore-extensions' or `diredp-visit-ignore-regexps'
;;  are skipped.
;;
;;  You can use `e' to show the file of the current line.  If it is
;;  already shown in the same frame, and if Dired is the only other
;;  window there, then the file is hidden (its window is deleted).
;;
;;
;;  Font-Lock Highlighting
;;  ----------------------
;;
;;  If you want a maximum or minimum fontification for Dired mode,
;;  then customize option `font-lock-maximum-decoration'.  If you want
;;  a different fontification level for Dired than for other modes,
;;  you can do this too by customizing
;;  `font-lock-maximize-decoration'.
;;
;;  A few of the user options defined here have an effect on
;;  font-locking, and this effect is established only when Dired+ is
;;  loaded, which defines the font-lock keywords for Dired.  These
;;  options include `diredp-compressed-extensions',
;;  `diredp-ignore-compressed-flag', `dired-omit-extensions', and
;;  `diredp-omit-files-font-lock-regexp'.  This means that if you
;;  change the value of such an option then you will see the change
;;  only in a new Emacs session.
;;
;;  (You can see the effect in the same session if you use `C-M-x' on
;;  the `defvar' sexp for `diredp-font-lock-keywords-1', and then you
;;  toggle font-lock off and back on.)
;;
;;
;;  Act on All Files
;;  ----------------
;;
;;  Most of the commands (such as `C' and `C-M-g') that operate on the
;;  marked files have the added feature here that multiple `C-u' use
;;  not the files that are marked or the next or previous N files, but
;;  *all* of the files in the Dired buffer.  Just what "all" files
;;  means changes with the number of `C-u', as follows:
;;
;;    `C-u C-u'         - Use all files present, but no directories.
;;    `C-u C-u C-u'     - Use all files and dirs except `.' and `..'.
;;    `C-u C-u C-u C-u' - use all files and dirs, `.' and `..'.
;;
;;    (More than four `C-u' is the same as two.)
;;
;;  This feature can be particularly useful when you have a Dired
;;  buffer with files chosen from multiple directories.
;;
;;  Note that in most cases this behavior is described only in the doc
;;  string of function `dired-get-marked-files'.  It is generally
;;  *not* described in the doc strings of the various commands,
;;  because that would require redefining each command separately
;;  here.  Instead, we redefine macro `dired-map-over-marks' and
;;  function `dired-get-filename' in order to achieve this effect.
;;
;;  Commands such as `dired-do-load' for which it does not make sense
;;  to act on directories generally treat more than two `C-u' the same
;;  as two `C-u'.
;;
;;  Exceptions to the general behavior described here are called out
;;  in the doc strings.  In particular, the behavior of a prefix arg
;;  for `dired-do-query-replace-regexp' is different, so that you can
;;  use it also to specify word-delimited replacement.
;;
;;  Note too that if you have inserted subdir listings then 4 `C-u',
;;  which includes both `.' and `..', includes them for the main
;;  listing and for each inserted subdir listing.  That is, since the
;;  same directory is listed twice, as both parent and child, it is
;;  also included twice in the list returned by
;;  `dired-get-marked-files', typically with slightly different
;;  syntax.  If this is problematic for a given use of
;;  `dired-get-marked-files' then you'll want to remove duplicates
;;  (which, again, likely differ in syntax though they represent the
;;  same directory).
;;
;;  The same thing happens if you explicitly mark the same
;;  subdirectory in both its own listing and its parent directory.
;;  `dired-get-marked-files' includes both names.  (This is also true
;;  for vanilla Emacs.)
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
;;  any marked subdirectories, found recursively.  If such a
;;  descendant directory is listed in a Dired buffer then its marked
;;  files and subdirs are handled the same way.  If there is no Dired
;;  buffer that lists a given marked subdirectory then all of its
;;  files and subdirs are acted on.
;;
;;  For most such here-and-below commands, a prefix argument means
;;  ignore all marks.  The commands then act on all files in the
;;  current Dired buffer and all of its subdirectories, recursively.
;;
;;  But here-and-below commands that unmark or change marks act
;;  differently for different kinds of prefix argument:
;;
;;  * A non-positive prefix arg means ignore subdir markings and act
;;    instead on ALL subdirs.
;;
;;  * A non-negative prefix arg means do not change marks on subdirs
;;    themselves.
;;
;;  For example, `M-+ U' removes all marks, including from marked
;;  subdirs, recursively.  `C-- M-+ U' removes them from all files in
;;  all subdirs (marked or not), recursively.  `C-9 M-+ U' removes all
;;  marks, recursively, except the marks on subdirs themselves.  `C-0
;;  M-+ U' acts like those two combined: it descends everywhere,
;;  ignoring which subdirs are marked, but it does not remove marks
;;  from subdirs themselves.
;;
;;  All of the `diredp-*-recursive' commands are on prefix key `M-+',
;;  and most are available on submenu `Marked Here and Below' of the
;;  `Multiple' menu-bar menu.  The commands that unmark and change
;;  marks are also in submenu `Here and Below' of menu-bar menu
;;  `Marks'.
;;
;;  If you use library `Icicles' then you have the following
;;  additional commands/keys that act recursively on marked files.
;;  They are in the `Icicles' submenu of menu `Multiple' > `Marked
;;  Here and Below'.
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
;;  Dired Snapshots: Saving and Restoring Dired Listings
;;  ----------------------------------------------------
;;
;;  Suppose you use a command such as `find-name-dired', to generate a
;;  Dired buffer that lists files from various places.  The search
;;  part of that operation might take a long time.
;;
;;  And suppose that you later want to get back to such a listing,
;;  even if that buffer no longer exists.  In particular, maybe you
;;  want to get to it in another Emacs session.
;;
;;  And suppose that you don't want to pay the penalty of performing
;;  the `find' search again, and you're content with the set of file
;;  names found by the original search.  That is, you don't care
;;  whether that set of names is still 100% up-to-date.
;;
;;  In such a context you want, in effect, to create a Dired buffer
;;  snapshot of some sort - you want to record the set of names that
;;  your search found, and later use them again in Dired.
;;
;;  Dired+ gives you two ways to do this.  Both involve first creating
;;  a Dired buffer that's produced from an explicit set of file names,
;;  from anywhere, rather than one that's produced using `ls' or
;;  similar, and then saving that set of file names for re-creating
;;  such a Dired buffer later.
;;
;;  1. Use `C-M-*' (`diredp-marked-other-window') or `diredp-marked',
;;     to create a snapshot Dired buffer.  Then bookmark that buffer.
;;     Just jump to the bookmark to restore the snapshot buffer.
;;
;;  2. Use command `diredp-define-snapshot-dired-commands', to create
;;     two commands (for same-window and other-window) that will
;;     create a snapshot Dired buffer.  Save the `defun's of those
;;     commands to your init file, for persistent access.
;;
;;  The saved set of files, whether embedded in a bookmark or in a
;;  special Dired command, is a snapshot of the files available at a
;;  particular time.
;;
;;  When you later use Dired with that explicit set of file names,
;;  only those files are listed - if a name no longer corresponds to
;;  an existing file then it is ignored.  The resulting Dired buffer
;;  represents the current state of the file system, but only as far
;;  as the files it lists are concerned.
;;
;;  I think the first approach is generally preferable, but you might
;;  prefer the second.
;;
;;  If you use approach #1 then you also need my library Bookmark+:
;;
;;    https://www.emacswiki.org/emacs/BookmarkPlus
;;
;;  If you bookmark a Dired buffer without using Bookmark+ then the
;;  bookmark records only the Dired directory name.  It doesn't record
;;  the snapshot information - the explicit list of files to be
;;  restored.  (It also doesn't record the `ls' switches or which
;;  files were marked in the bookmarked snapshot listing, so you can't
;;  restore them.)
;;
;;  Both approaches, #1 and #2, use the marked files and dirs as the
;;  set to snapshot.  More precisely, they use the Dired+ version of
;;  function `dired-get-marked-files'.  That means that you can use a
;;  prefix arg to get a different set of files to snapshot, instead of
;;  those that are explicitly marked.  See the doc strings.
;;
;;  Note too that it is the full content of the original Dired buffer
;;  that's used to define the files to snapshot.  In particular,
;;  inserted subdir listings are included.
;;
;;
;;  Image Files
;;  -----------
;;
;;  `Dired+' provides several enhancements regarding image files.
;;  Most of these require standard library `image-dired.el'.  One of
;;  them, command `diredp-do-display-images', which displays all of
;;  the marked image files, requires standard library `image-file.el'.
;;
;;  `Dired+' loads these libraries automatically, if available, which
;;  means an Emacs version that supports image display (Emacs 22 or
;;  later).  (You must of course have installed whatever else your
;;  Emacs version needs to display images.)
;;
;;  Besides command `diredp-do-display-images', see the commands whose
;;  names have prefix `diredp-image-'.  And see options
;;  `diredp-image-preview-in-tooltip' and
;;  `diredp-auto-focus-frame-for-thumbnail-tooltip-flag'.
;;
;;
;;  Inserted Subdirs, Multiple Dired Buffers, Files from Anywhere,...
;;  -----------------------------------------------------------------
;;
;;  These three standard Dired features are worth pointing out.  The
;;  third in particular is little known because (a) it is limited in
;;  vanilla Dired and (b) you cannot use it interactively.
;;
;;   * You can pass a glob pattern with wildcards to `dired'
;;     interactively, as the file name.
;;
;;   * You can insert multiple subdirectory listings into a single
;;     Dired buffer using `i' on each subdir line.  Use `C-u i' to
;;     specify `ls' switches.  Specifying switch `R' inserts the
;;     inserted subdirectory's subdirs also, recursively.  You can
;;     also use `i' to bounce between a subdirectory line and its
;;     inserted-listing header line.  You can delete a subdir listing
;;     using `C-u k' on its header line.  You can hide/show an
;;     inserted subdir using `$'.  You can use `C-_' to undo any of
;;     these operations.
;;
;;   * You can open a Dired buffer for an arbitrary set of files from
;;     different directories.  You do this by invoking `dired'
;;     non-interactively, passing it a cons of a Dired buffer name and
;;     the file names.  Relative file names are interpreted relative
;;     to the value of `default-directory'.  Use absolute file names
;;     when appropriate.
;;
;;  `Dired+' makes these features more useful.
;;
;;  `$' is improved: It is a simple toggle - it does not move the
;;  cursor forward.  `M-$' advances the cursor, in addition to
;;  toggling like `$'.  `C-u $' does hide/show all (what `M-$' does in
;;  vanilla Dired).
;;
;;  `i' is improved in these ways:
;;
;;   * Once a subdir has been inserted, `i' bounces between the subdir
;;     listing and the subdir line in the parent listing.  If the
;;     parent dir is hidden, then `i' from a subdir opens the parent
;;     listing so it can move to the subdir line there (Emacs 24+).
;;
;;   * Vanilla Dired lets you create a Dired listing with files and
;;     directories from arbitrary locations, but you cannot insert
;;     (`i') such a directory if it is not in the same directory tree
;;     as the `default-directory' used to create the Dired buffer.
;;     `Dired+' removes this limitation; you can insert any non-root
;;     directories (that is, not `/', `c:/', etc.).
;;
;;  `Dired+' lets you create Dired buffers that contain arbitrary
;;  files and directories interactively, not just using Lisp.  Just
;;  use a non-positive prefix arg (e.g., `C--') when invoking `dired'.
;;
;;  You are then prompted for the Dired buffer name (anything you
;;  like, not necessarily a directory name) and the individual files
;;  and directories that you want listed.
;;
;;  A non-negative prefix arg still prompts you for the `ls' switches
;;  to use.  (So `C-0' does both: prompts for `ls' switches and for
;;  the Dired buffer name and the files to list.)
;;
;;  `Dired+' adds commands for combining and augmenting Dired
;;  listings:
;;
;;   * `diredp-add-to-dired-buffer', bound globally to `C-x D A', lets
;;     you add arbitrary file and directory names to an existing Dired
;;     buffer.
;;
;;   * `diredp-dired-union', bound globally to `C-x D U', lets you
;;     take the union of multiple Dired listings, or convert an
;;     ordinary Dired listing to an explicit list of absolute file
;;     names.  With a non-positive prefix arg, you can add extra file
;;     and directory names, just as for `diredp-add-to-dired-buffer'.
;;
;;  You can open an Emacs fileset in Dired mode, using `C-x D S' or
;;  `C-x 4 D S'.  See the Emacs manual, node Filesets, or
;;  https://www.emacswiki.org/emacs/FileSets, for info about filesets.
;;
;;  You can visit your recent files or directories in Dired mode,
;;  using `C-x D R' or `C-x D r'.  Like the other commands on prefix
;;  key `C-x D', these Dired listings are composed of arbitrary files;
;;  they're not the output of `ls'.
;;
;;  You can revert (using `g') or sort any Dired buffer that lists
;;  arbitrary files, which includes a buffer created with the commands
;;  on prefix keys `C-x D' and `C-x 4 D' and a buffer created with
;;  commands created using command
;;  `diredp-define-snapshot-dired-commands'.
;;
;;  You can also sort such a buffer in various ways, but you need to
;;  use `C-M-L' (aka `C-M-S-l') to do so - you can't use the ordinary
;;  Dired sort commands, such as `s'.  You're prompted for the sort
;;  order.  The default sort order for such buffers is determined by
;;  option `diredp-default-sort-arbitrary-function'.
;;
;;  When using a Dired buffer that lists arbitrary files, other than
;;  one composed of recent files, be aware that any operation that
;;  reverts the listing relists the same file names, and only those
;;  that correspond to currently existing files.  This means that:
;;
;;  1. If any of the files no longer exist then they will no longer be
;;     listed (which is likely what you would expect).
;;
;;  2. If any of the files has been renamed then it will no longer be
;;     listed (which is likely not what you would expect).  This
;;     applies to the use of WDired to rename files: the renamed files
;;     are not listed when you return to Dired from WDired.  It
;;     applies also to the use of `R' (`dired-do-rename').
;;
;;  (With Emacs prior to version 28 you can't use WDired on a
;;  recent-files buffer at all, because such a buffer uses a
;;  `revert-buffer-function' that updates the file list to show the
;;  currently recent files, and older versions of WDired hard-code the
;;  function used to revert back to Dired mode when you exit WDired.)
;;
;;  You can optionally add a header line to a Dired buffer using
;;  toggle command `diredp-breadcrumbs-in-header-line-mode'.  (A
;;  header line remains at the top of the window - no need to scroll
;;  to see it.)  If you want to show the header line automatically in
;;  all Dired buffers, you can do this:
;;
;;    (add-hook 'dired-before-readin-hook
;;              'diredp-breadcrumbs-in-header-line-mode)
;;
;;  Some other libraries, such as `Bookmark+' and `Icicles', make it
;;  easy to create or re-create Dired buffers that list specific files
;;  and have a particular set of markings.  `Bookmark+' records Dired
;;  buffers persistently, remembering `ls' switches, markings, subdir
;;  insertions, and hidden subdirs.  If you use `Icicles' then `dired'
;;  is a multi-command: you can open multiple Dired buffers with one
;;  `dired' invocation.
;;
;;  Dired can help you manage projects.  You might have multiple Dired
;;  buffers with quite specific contents.  You might have some
;;  subdirectories inserted in the same Dired buffer, and you might
;;  have separate Dired buffers for some subdirectories.  Sometimes it
;;  is useful to have both for the same subdirectory.  And sometimes
;;  it is useful to move from one presentation to the other.
;;
;;  This is one motivation for the `Dired+' `diredp-*-recursive'
;;  commands, which act on the marked files in marked subdirectories,
;;  recursively.  In one sense, these commands are an alternative to
;;  using a single Dired buffer with inserted subdirectories.  They
;;  let you use the same operations on the files in a set of Dired
;;  directories, without inserting those directories into an ancestor
;;  Dired buffer.
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
;;  Hide/Show Details
;;  -----------------
;;
;;  Starting with Emacs 24.4, listing details are hidden by default.
;;  Note that this is different from the vanilla Emacs behavior, which
;;  is to show details by default.
;;
;;  Use `(' anytime to toggle this hiding.  You can use option
;;  `diredp-hide-details-initially-flag' to change the default/initial
;;  state.  See also option `diredp-hide-details-propagate-flag'.
;;
;;  NOTE: If you do not want to hide details initially then you must
;;        either (1) change `diredp-hide-details-initially-flag' using
;;        Customize (recommended) or (2) set it to `nil' (e.g., using
;;        `setq') *BEFORE* loading `dired+.el'.
;;
;;  If you have an Emacs version older than 24.4, you can use library
;;  `dired-details+.el' (plus `dired-details.el') to get similar
;;  behavior.
;;
;;
;;  Mode-Line
;;  ---------
;;
;;  The number of files and dirs that are marked with `*', and the
;;  number that are flagged for deletion (marked `D') are indicated in
;;  the mode-line.  When the cursor is on such a line the indication
;;  tells you how many more there are.  For example, if the cursor is
;;  on the line of the third file that is marked `*', and there are
;;  seven of them total, then the mode-line shows `3/7*'.
;;
;;  The mode-line also indicates, for the current listing (which could
;;  be a subdir listing), how many files and dirs are listed.  If the
;;  cursor is on the 27th file in a listing of 78 files then the
;;  mode-line shows 27/78.
;;
;;  For counting files and dirs in a listing, option
;;  `diredp-count-.-and-..-flag' controls whether to count the lines
;;  for `.' and `..'.  By default it is nil, meaning they are not
;;  counted.
;;
;;
;;  If You Use Dired+ in Terminal Mode
;;  ----------------------------------
;;
;;  By default, Dired+ binds some keys that can be problematic in some
;;  terminals when you use Emacs in terminal mode (i.e., `emacs -nw').
;;  This is controlled by option
;;  `diredp-bind-problematic-terminal-keys'.
;;
;;  In particular, keys that use modifiers Meta and Shift together can
;;  be problematic.  If you use Dired+ in text-only terminal, and you
;;  find that your terminal does not support such keys, then you might
;;  want to customize the option to set the value to `nil', and then
;;  bind the commands to some other keys, which your terminal
;;  supports.
;;
;;  The problematic keys used by Dired+ include these:
;;
;;    `M-M'   (aka `M-S-m')   - `diredp-chmod-this-file'
;;    `M-O'   (aka `M-S-o')   - `diredp-chown-this-file'
;;    `M-T'   (aka `M-S-t')   - `diredp-touch-this-file'
;;    `C-M-B' (aka `C-M-S-b') - `diredp-do-bookmark-in-bookmark-file'
;;    `C-M-G' (aka `C-M-S-g') - `diredp-chgrp-this-file'
;;    `C-M-R' (aka `C-M-S-r') - `diredp-toggle-find-file-reuse-dir'
;;    `C-M-T' (aka `C-M-S-t') - `dired-do-touch'
;;    `M-+ M-B'   (aka `M-+ M-S-b') -
;;        `diredp-do-bookmark-dirs-recursive'
;;    `M-+ C-M-B' (aka `M-+ C-M-S-b') -
;;        `diredp-do-bookmark-in-bookmark-file-recursive'
;;    `M-+ C-M-T' (aka `M-+ C-M-S-t') - `diredp-do-touch-recursive'
;;
;;  (See also `(info "(org) TTY keys")' for more information about
;;  keys that can be problematic in a text-only terminal.)
;;
;;
;;  Faces defined here:
;;
;;    `diredp-autofile-name', `diredp-compressed-file-suffix',
;;    `diredp-date-time', `diredp-deletion',
;;    `diredp-deletion-file-name', `diredp-dir-heading',
;;    `diredp-dir-priv', `diredp-exec-priv', `diredp-executable-tag',
;;    `diredp-file-name', `diredp-file-suffix', `diredp-flag-mark',
;;    `diredp-flag-mark-line', `diredp-get-file-or-dir-name',
;;    `diredp-ignored-file-name', `diredp-link-priv',
;;    `diredp-mode-line-flagged', `diredp-mode-line-marked'
;;    `diredp-omit-file-name', `diredp-no-priv', `diredp-number',
;;    `diredp-other-priv', `diredp-rare-priv', `diredp-read-priv',
;;    `diredp-symlink', `diredp-tagged-autofile-name',
;;    `diredp-write-priv'.
;;
;;  Commands defined here:
;;
;;    `diredp-add-file-to-recentf', `diredp-add-this-to-recentf',
;;    `diredp-add-to-dired-buffer', `diredp-add-to-this-dired-buffer',
;;    `diredp-async-shell-command-this-file',
;;    `diredp-bookmark-this-file',
;;    `diredp-breadcrumbs-in-header-line-mode' (Emacs 22+),
;;    `diredp-byte-compile-this-file', `diredp-capitalize',
;;    `diredp-capitalize-recursive', `diredp-capitalize-this-file',
;;    `diredp-change-ls-switches', `diredp-change-marks-recursive'
;;    (Emacs 22+), `diredp-chgrp-this-file', `diredp-chmod-this-file',
;;    `diredp-chown-this-file',
;;    `diredp-compilation-files-other-window' (Emacs 24+),
;;    `diredp-compress-this-file',
;;    `diredp-copy-abs-filenames-as-kill',
;;    `diredp-copy-abs-filenames-as-kill-recursive',
;;    `diredp-copy-filename-as-kill-recursive',
;;    `diredp-copy-tags-this-file', `diredp-copy-this-file',
;;    `diredp-create-file-here', `diredp-decrypt-this-file',
;;    `diredp-define-snapshot-dired-commands',
;;    `diredp-delete-this-file', `diredp-describe-autofile',
;;    `diredp-describe-file', `diredp-describe-marked-autofiles',
;;    `diredp-describe-mode', `diredp-dired-for-files',
;;    `diredp-dired-for-files-other-window',
;;    `diredp-dired-inserted-subdirs', `diredp-dired-plus-help',
;;    `diredp-dired-recent-dirs',
;;    `diredp-dired-recent-dirs-other-window',
;;    `diredp-dired-recent-files',
;;    `diredp-dired-recent-files-other-window',
;;    `diredp-dired-this-subdir', `diredp-dired-union',
;;    `diredp-do-add-to-recentf',
;;    `diredp-do-aggregate-apply-to-marked',
;;    `diredp-do-aggregate-eval-in-marked',
;;    `diredp-do-apply/eval-marked',
;;    `diredp-do-apply/eval-marked-recursive',
;;    `diredp-do-apply-to-marked',
;;    `diredp-do-apply-to-marked-recursive',
;;    `diredp-do-async-shell-command-recursive', `diredp-do-bookmark',
;;    `diredp-do-bookmark-dirs-recursive',
;;    `diredp-do-bookmark-in-bookmark-file',
;;    `diredp-do-bookmark-in-bookmark-file-recursive',
;;    `diredp-do-bookmark-recursive', `diredp-do-chmod-recursive',
;;    `diredp-do-chgrp-recursive', `diredp-do-chown-recursive',
;;    `diredp-do-command-in-marked',
;;    `diredp-do-command-in-marked-recursive',
;;    `diredp-do-copy-recursive', `diredp-do-decrypt-recursive',
;;    `diredp-do-delete-recursive', `diredp-do-display-images' (Emacs
;;    22+), `diredp-do-encrypt-recursive', `diredp-do-eval-in-marked',
;;    `diredp-do-eval-in-marked-recursive',
;;    `diredp-do-find-marked-files-recursive', `diredp-do-grep',
;;    `diredp-do-grep-recursive', `diredp-do-hardlink-recursive',
;;    `diredp-do-isearch-recursive',
;;    `diredp-do-isearch-regexp-recursive',
;;    `diredp-do-move-recursive', `diredp-do-paste-add-tags',
;;    `diredp-do-paste-replace-tags', `diredp-do-print-recursive',
;;    `diredp-do-query-replace-regexp-recursive',
;;    `diredp-do-redisplay-recursive',
;;    `diredp-do-relsymlink-recursive', `diredp-do-remove-all-tags',
;;    `diredp-do-remove-from-recentf', `diredp-do-search-recursive',
;;    `diredp-do-set-tag-value', `diredp-do-shell-command-recursive',
;;    `diredp-do-sign-recursive', `diredp-do-symlink-recursive',
;;    `diredp-do-tag', `diredp-do-touch-recursive', `diredp-do-untag',
;;    `diredp-do-verify-recursive', `diredp-downcase-recursive',
;;    `diredp-downcase-this-file', `diredp-ediff',
;;    `diredp-encrypt-this-file', `diredp-fileset',
;;    `diredp-fileset-other-window', `diredp-find-a-file',
;;    `diredp-find-a-file-other-frame',
;;    `diredp-find-a-file-other-window',
;;    `diredp-find-file-other-frame',
;;    `diredp-find-file-reuse-dir-buffer',
;;    `diredp-find-line-file-other-window',
;;    `diredp-flag-auto-save-files-recursive',
;;    `diredp-flag-region-files-for-deletion',
;;    `diredp-grepped-files-other-window', `diredp-grep-this-file',
;;    `diredp-hardlink-this-file', `diredp-highlight-autofiles-mode',
;;    `diredp-image-dired-comment-file',
;;    `diredp-image-dired-comment-files-recursive',
;;    `diredp-image-dired-copy-with-exif-name',
;;    `diredp-image-dired-create-thumb',
;;    `diredp-image-dired-delete-tag',
;;    `diredp-image-dired-delete-tag-recursive',
;;    `diredp-image-dired-display-thumb',
;;    `diredp-image-dired-display-thumbs-recursive',
;;    `diredp-image-dired-edit-comment-and-tags',
;;    `diredp-image-dired-tag-file',
;;    `diredp-image-dired-tag-files-recursive',
;;    `diredp-image-show-this-file', `diredp-insert-as-subdir',
;;    `diredp-insert-subdirs', `diredp-insert-subdirs-recursive',
;;    `diredp-kill-this-tree', `diredp-list-marked-recursive',
;;    `diredp-load-this-file', `diredp-mark', `diredp-mark-autofiles',
;;    `diredp-marked', `diredp-marked-other-window',
;;    `diredp-marked-recursive',
;;    `diredp-marked-recursive-other-window',
;;    `diredp-mark-extension-recursive',
;;    `diredp-mark-files-containing-regexp-recursive',
;;    `diredp-mark-files-regexp-recursive',
;;    `diredp-mark-files-tagged-all', `diredp-mark-files-tagged-none',
;;    `diredp-mark-files-tagged-not-all',
;;    `diredp-mark-files-tagged-some',
;;    `diredp-mark-files-tagged-regexp', `diredp-mark-if-sexp',
;;    `diredp-mark-if-sexp-recursive', `diredp-mark-region-files',
;;    `diredp-mark-region-files-with-char',
;;    `diredp-mark-sexp-recursive' (Emacs 22+),
;;    `diredp-mark/unmark-autofiles', `diredp-mark/unmark-extension',
;;    `diredp-mark-with-char', `diredp-mouse-3-menu',
;;    `diredp-mouse-backup-diff', `diredp-mouse-copy-tags',
;;    `diredp-mouse-describe-autofile', `diredp-mouse-describe-file',
;;    `diredp-mouse-diff', `diredp-mouse-do-bookmark',
;;    `diredp-mouse-do-byte-compile', `diredp-mouse-do-chgrp',
;;    `diredp-mouse-do-chmod', `diredp-mouse-do-chown',
;;    `diredp-mouse-do-compress', `diredp-mouse-do-copy',
;;    `diredp-mouse-do-delete', `diredp-mouse-do-grep',
;;    `diredp-mouse-do-hardlink', `diredp-mouse-do-load',
;;    `diredp-mouse-do-print', `diredp-mouse-do-remove-all-tags',
;;    `diredp-mouse-do-rename', `diredp-mouse-do-set-tag-value',
;;    `diredp-mouse-do-shell-command', `diredp-mouse-do-symlink',
;;    `diredp-mouse-do-tag', `diredp-mouse-do-untag',
;;    `diredp-mouse-downcase', `diredp-mouse-ediff',
;;    `diredp-mouse-find-line-file-other-window',
;;    `diredp-mouse-find-file-other-frame',
;;    `diredp-mouse-find-file-reuse-dir-buffer',
;;    `diredp-mouse-flag-file-deletion', `diredp-mouse-mark',
;;    `diredp-mouse-mark-region-files', `diredp-mouse-mark/unmark',
;;    `diredp-mouse-unmark', `diredp-mouse-upcase',
;;    `diredp-mouse-view-file', `diredp-move-file' (Emacs 24+),
;;    `diredp-move-files-named-in-kill-ring', `diredp-move-this-file',
;;    `diredp-multiple-w32-browser-recursive',
;;    `diredp-nb-marked-in-mode-name', `diredp-next-dirline',
;;    `diredp-next-line', `diredp-next-subdir', `diredp-omit-marked',
;;    `diredp-omit-unmarked', `diredp-paste-add-tags-this-file',
;;    `diredp-paste-files', `diredp-paste-replace-tags-this-file',
;;    `diredp-prev-dirline', `diredp-previous-line',
;;    `diredp-prev-subdir', `diredp-print-this-file',
;;    `diredp-quit-window-kill' (Emacs 24+),
;;    `diredp-relsymlink-this-file',
;;    `diredp-remove-all-tags-this-file',
;;    `diredp-remove-file-from-recentf',
;;    `diredp-remove-inserted-subdirs',
;;    `diredp-remove-this-from-recentf', `diredp-rename-this-file',
;;    `diredp-restore-markings', `diredp-save-markings',
;;    `diredp-send-bug-report',
;;    `diredp-set-bookmark-file-bookmark-for-marked',
;;    `diredp-set-bookmark-file-bookmark-for-marked-recursive',
;;    `diredp-set-tag-value-this-file',
;;    `diredp-shell-command-this-file', `diredp-show-metadata',
;;    `diredp-show-metadata-for-marked', `diredp-sign-this-file',
;;    `diredp-sort-arbitrary-command', `diredp-symlink-this-file',
;;    `diredp-tag-this-file', `diredp-toggle-find-file-reuse-dir',
;;    `diredp-toggle-marks-in-region', `diredp-touch-this-file',
;;    `diredp-unmark-all-files-recursive' (Emacs 22+),
;;    `diredp-unmark-all-marks-recursive' (Emacs 22+),
;;    `diredp-unmark-autofiles', `diredp-unmark-files-tagged-all',
;;    `diredp-unmark-files-tagged-none',
;;    `diredp-unmark-files-tagged-not-all',
;;    `diredp-unmark-files-tagged-some', `diredp-unmark-region-files',
;;    `diredp-untag-this-file', `diredp-upcase-recursive',
;;    `diredp-up-directory', `diredp-up-directory-reuse-dir-buffer',
;;    `diredp-upcase-this-file', `diredp-verify-this-file',
;;    `diredp-visit-next-file', `diredp-visit-previous-file',
;;    `diredp-visit-this-file', `diredp-w32-drives',
;;    `diredp-w32-drives-mode', `diredp-yank-files',
;;    `global-dired-hide-details-mode' (Emacs 24.4+),
;;    `toggle-diredp-find-file-reuse-dir'.
;;
;;  User options defined here:
;;
;;    `diredp-auto-focus-frame-for-thumbnail-tooltip-flag',
;;    `diredp-bind-problematic-terminal-keys',
;;    `diredp-case-fold-search', `diredp-compressed-extensions',
;;    `diredp-count-.-and-..-flag' (Emacs 22+),
;;    `diredp-default-sort-arbitrary-function',
;;    `diredp-do-report-echo-limit', `diredp-dwim-any-frame-flag'
;;    (Emacs 22+), `diredp-image-preview-in-tooltip', `diff-switches',
;;    `diredp-hide-details-initially-flag' (Emacs 24.4+),
;;    `diredp-highlight-autofiles-mode',
;;    `diredp-hide-details-propagate-flag' (Emacs 24.4+),
;;    `diredp-ignore-compressed-flag',
;;    `diredp-image-show-this-file-use-frame-flag' (Emacs 22+),
;;    `diredp-list-file-attributes', `diredp-max-frames',
;;    `diredp-move-file-dirs' (Emacs 24+),
;;    `diredp-omit-files-font-lock-regexp',
;;    `diredp-omit-lines-regexp',
;;    `diredp-prompt-for-bookmark-prefix-flag',
;;    `diredp-recent-files-quit-kills-flag',
;;    `diredp-switches-in-mode-line',
;;    `diredp-toggle-dot+dot-dot-flag',
;;    `diredp-visit-ignore-extensions', `diredp-visit-ignore-regexps',
;;    `diredp-w32-local-drives', `diredp-wrap-around-flag'.
;;
;;  Non-interactive functions defined here:
;;
;;    `derived-mode-p' (Emacs < 22),
;;    `diredp--add-dired-to-invisibility-hook', `diredp-all-files',
;;    `diredp-ancestor-dirs', `diredp-apply-to-this-file',
;;    `diredp-bookmark', `diredp-copy-as-kill-from-clipboard',
;;    `diredp-create-files-non-directory-recursive',
;;    `diredp-define-snapshot-dired-commands-1', `diredp-delete-dups',
;;    `diredp-delete-if', `diredp-delete-if-not',
;;    `diredp-describe-file-1', `diredp-directories-within',
;;    `diredp-dired-plus-description',
;;    `diredp-dired-plus-description+links',
;;    `diredp-dired-plus-help-link', `diredp--dired-recent-files-1',
;;    `diredp-dired-union-1', `diredp-dired-union-interactive-spec',
;;    `diredp-display-image' (Emacs 22+), `diredp-do-chxxx-recursive',
;;    `diredp-do-create-files-recursive', `diredp-do-grep-1',
;;    `diredp-ensure-bookmark+', `diredp-ensure-fn-nonzero-arity',
;;    `diredp-ensure-fn-zero-arity', `diredp-ensure-mode',
;;    `diredp-eval-in-this-file', `diredp-existing-dired-buffer-p',
;;    `diredp-fewer-than-2-files-p',
;;    `diredp-fewer-than-echo-limit-files-p',
;;    `diredp-fewer-than-N-files-p', `diredp-fileset-1',
;;    `diredp-find-a-file-read-args',
;;    `diredp-file-content-description',
;;    `diredp-file-for-compilation-hit-at-point' (Emacs 24+),
;;    `diredp-files-within', `diredp-files-within-1',
;;    `diredp-fit-frame-unless-buffer-narrowed' (Emacs 24.4+),
;;    `diredp-fit-one-window-frame', `diredp-full-file-name-less-p',
;;    `diredp-full-file-name-more-p',
;;    `diredp-get-args-for-diredp-marked',
;;    `diredp-get-args-for-snapshot-cmd',
;;    `diredp-get-confirmation-recursive', `diredp-get-files',
;;    `diredp-get-files-for-dir', `diredp-get-image-filename',
;;    `diredp-get-subdirs', `diredp-hide-details-if-dired' (Emacs
;;    24.4+), `diredp-hide/show-details' (Emacs 24.4+),
;;    `diredp-highlight-autofiles', `diredp-image-dired-required-msg',
;;    `diredp-internal-do-deletions', `diredp-invoke-command',
;;    `diredp-invoke/eval-in-this-file', `diredp-invoke-in-this-file',
;;    `diredp-list-file', `diredp-list-files', `diredp-looking-at-p',
;;    `diredp-make-find-file-keys-reuse-dirs',
;;    `diredp-make-find-file-keys-not-reuse-dirs',
;;    `diredp-make-obsolete', `diredp-make-obsolete-variable',
;;    `diredp-maplist', `diredp-map-over-marks-and-report',
;;    `diredp-marked-here', `diredp-mark-files-tagged-all/none',
;;    `diredp-mark-files-tagged-some/not-all',
;;    `diredp-nondir-file-name-less-p',
;;    `diredp-nondir-file-name-more-p', `diredp-nonempty-region-p',
;;    `diredp-parent-dir', `diredp-paste-add-tags',
;;    `diredp-paste-replace-tags', `diredp-prefix-arg-all-files',
;;    `diredp-read-bookmark-file-args', `diredp-read-command',
;;    `diredp-read-expression' (Emacs 22+),
;;    `diredp-read-include/exclude', `diredp-read-regexp',
;;    `diredp-recent-dirs', `diredp-recent-files-buffer',
;;    `diredp-refontify-buffer', `diredp-remove-if',
;;    `diredp-remove-if-not', `diredp-replace-dir-sep-in-string',
;;    `diredp-report-file-result', `diredp-report-summary',
;;    `diredp-revert-displayed-recentf-buffers',
;;    `diredp--reuse-dir-buffer-helper', `diredp-root-directory-p',
;;    `diredp-set-header-line-breadcrumbs' (Emacs 22+),
;;    `diredp-set-tag-value', `diredp-set-union',
;;    `diredp--set-up-font-locking', `diredp--snapshot-cmd-name-time',
;;    `diredp-sort-arbitrary', `diredp-string-less-p',
;;    `diredp-string-match-p', `diredp-tag',
;;    `diredp-this-file-marked-p', `diredp-this-file-unmarked-p',
;;    `diredp-this-subdir', `diredp-untag',
;;    `diredp-visit-ignore-regexp', `diredp-y-or-n-files-p'.
;;
;;  Variables defined here:
;;
;;    `diredp-bookmark-menu', `diredp-dir-sep-replacement',
;;    `diredp-file-line-overlay', `diredp-filename-separator',
;;    `diredp-files-within-dirs-done', `diredp-font-lock-keywords-1',
;;    `diredp-hide-details-last-state' (Emacs 24.4+),
;;    `diredp-hide-details-toggled' (Emacs 24.4+),
;;    `diredp-hide/show-menu', `diredp-images-recursive-menu',
;;    `diredp-last-copied-filenames', `diredp-list-files-map',
;;    `diredp-loaded-p', `diredp-marks-recursive-menu',
;;    `diredp-menu-bar-dir-menu', `diredp-menu-bar-marks-menu',
;;    `diredp-menu-bar-multiple-menu', `diredp-menu-bar-regexp-menu',
;;    `diredp-menu-bar-single-menu', `diredp-multiple-bookmarks-menu',
;;    `diredp-multiple-delete-menu', `diredp-multiple-dired-menu',
;;    `diredp-multiple-images-menu',
;;    `diredp-multiple-encryption-menu',
;;    `diredp-multiple-move-copy-link-menu',
;;    `diredp-multiple-omit-menu', `diredp-multiple-recursive-menu',
;;    `diredp-multiple-rename-menu', `diredp-multiple-search-menu',
;;    `diredp-navigate-menu', `diredp-recent-files-map',
;;    `diredp-regexp-recursive-menu', `diredp-re-no-dot',
;;    `diredp-saved-markings', `diredp-single-bookmarks-menu',
;;    `diredp-single-encryption-menu', `diredp-single-image-menu',
;;    `diredp-single-move-copy-link-menu', `diredp-single-open-menu',
;;    `diredp-single-rename-menu',
;;    `diredp-snapshot-cmd-buffer-name-format',
;;    `diredp-snapshot-cmd-time-format', `diredp-w32-drives-mode-map'.
;;
;;  Macros defined here:
;;
;;    `diredp-mark-if', `diredp-user-error',
;;    `diredp-with-help-window'.
;;
;;
;;  ***** NOTE: The following macro defined in `subr.el' has
;;              been REDEFINED HERE:
;;
;;  `with-silent-modifications' - Adapt for older Emacs also.
;;
;;  ***** NOTE: The following macros defined in `dired.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-map-over-marks'    - Treat multiple `C-u' specially.
;;
;;
;;  ***** NOTE: The following functions defined in `dired.el' have
;;              been REDEFINED or ADVISED HERE:
;;
;;  `dired'                   - Handle non-positive prefix arg.
;;  `dired-do-delete'         - Display message to warn that marked,
;;                              not flagged, files will be deleted.
;;  `dired-do-flagged-delete' - Display message to warn that flagged,
;;                              not marked, files will be deleted.
;;  `dired-dwim-target-directory' - Uses `diredp-dwim-any-frame-flag'.
;;  `dired-find-file'         - Allow `.' and `..' (Emacs 20 only).
;;  `dired-get-filename'      - Test `./' and `../' (like `.', `..').
;;  `dired-get-marked-files'  - Can include `.' and `..'.
;;                              Allow FILTER + DISTINGUISH-ONE-MARKED.
;;  `dired-goto-file'         - Respect `diredp-case-fold-search'.
;;                              Prefix arg toggles that.
;;                              Open an enclosing hidden parent dir.
;;                              Expand input per current subdir list.
;;  `dired-goto-file-1'       - Use `compare-strings', Emacs 25+.
;;                              Added optional arg OPEN-HIDDEN-DIR-P.
;;  `dired-hide-details-mode' - Respect new user options:
;;                              * `diredp-hide-details-initially-flag'
;;                              * `diredp-hide-details-propagate-flag'
;;                              (Emacs 24.4+)
;;  `dired-insert-directory'  - Compute WILDCARD arg for
;;                              `insert-directory' for individual file
;;                              (don't just use nil). (Emacs 23+, and
;;                              only for MS Windows)
;;  `dired-insert-set-properties' - `mouse-face' on whole line.
;;  `dired-flag-auto-save-files', `dired-mark-directories',
;;  `dired-mark-executables', `dired-mark-files-containing-regexp',
;;  `dired-mark-files-regexp', `dired-mark-symlinks'
;;                            - Use `diredp-mark-if', not `dired-mark-if'.
;;  `dired-mark-files-regexp' - Add regexp to `regexp-search-ring'.
;;                              More matching possibilities.
;;                              Added optional arg LOCALP.
;;  `dired-mark-pop-up'       - Delete the window or frame popped up,
;;                              afterward, and bury its buffer. Do not
;;                              show a menu bar for pop-up frame.
;;  `dired-mark-remembered'   - File names in ALIST need not be absolute.
;;  `dired-move-to-filename'  - Made it a command.
;;  `dired-other-frame'       - Handle non-positive prefix arg.
;;  `dired-other-window'      - Handle non-positive prefix arg.
;;  `dired-pop-to-buffer'     - Restore (Emacs 29+).
;;                              Put window point at bob (bug #12281)
;;                              (Emacs 22-24.1).
;;  `dired-read-dir-and-switches' - Non-positive prefix arg behavior.
;;
;;; NOT YET:
;;; ;;  `dired-readin-insert'     - Use t as WILDCARD arg to
;;; ;;                              `dired-insert-directory'.  (Emacs 23+,
;;; ;;                              and only for MS Windows)
;;
;;  `dired-remember-marks'    - Added optional arg LOCALP.
;;  `dired-repeat-over-lines' - Skip dir header line (bug #48883).
;;  `dired-revert'            - Reset `mode-line-process' to nil.
;;  `dired-sort-set-mode-line' - Respect `diredp-switches-in-mode-line'.
;;  `dired-switches-escape-p' - Made compatible with Emacs 20, 21.
;;
;;
;;  ***** NOTE: The following functions are included here with little
;;              or no change to their definitions.  They are here to
;;              take advantage of the new definition of macro
;;              `dired-map-over-marks':
;;
;;  `dired-do-redisplay', `dired-map-over-marks-check',
;;  `image-dired-dired-insert-marked-thumbs',
;;  `image-dired-dired-toggle-marked-thumbs'.
;;
;;
;;  ***** NOTE: The following functions are included here with little
;;              or no change to their definitions.  They are here for
;;              older Emacs versions, to take advantage of the Emacs
;;              27+ way of hiding subdir listings:
;;
;;  `dired--find-hidden-pos', `dired--hidden-p', `dired--hide',
;;  `dired--unhide', `dired-unhide-subdir', `dired-subdir-hidden-p',
;;  `dired-add-entry', `dired-remember-hidden',
;;  `dired-move-to-end-of-filename', `dired-check-switches',
;;  `dired-hide-subdir', `dired-hide-all'.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-aux.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-do-byte-compile', `dired-do-compress', `dired-do-load' -
;;     Redisplay only if at most one file is being treated.
;;  `dired-do-find-regexp', `dired-do-find-regexp-and-replace' -
;;     Prefix arg lets you act on files other than those marked.
;;  `dired-do-isearch', `dired-do-isearch-regexp',
;;     `dired-do-query-replace-regexp', `dired-do-search' -
;;        Use new `dired-get-marked-files'.
;;  `dired-insert-subdir-newpos' - If not a descendant, put at eob.
;;  `dired-insert-subdir-validate' - Do nothing: no restrictions.
;;  `dired-do-touch' - Zero prefix arg creates empty file.
;;  `dired-do-kill-lines' - Added optional arg INIT-COUNT.
;;  `dired-maybe-insert-subdir' - Go back to subdir line if in listing.
;;  `dired-handle-overwrite' - Added optional arg FROM, for listing.
;;  `dired-copy-file(-recursive)', `dired-hardlink', `dired-query',
;;     `dired-rename-file' - You can list (`l') the files involved.
;;
;;
;;  ***** NOTE: The following functions defined in `dired-x.el' have
;;              been REDEFINED HERE:
;;
;;  `dired-copy-filename-as-kill' -
;;     Use `diredp-filename-separator', not SPC, as the separator.
;;     Put file names also in var `diredp-last-copied-filenames'.
;;  `dired-do-find-marked-files' -
;;     Call `dired-get-marked-files' with original ARG.
;;     Added optional arg INTERACTIVEP - no error if nil and no files.
;;  `dired-do-run-mail' - Require confirmation (Emacs < 25 only).
;;  `dired-jump' - Open destination directory listing if hidden.
;;  `dired-mark-sexp' - 1. Variable `s' -> `blks'.
;;                      2. Fixes to `uid' and `gid'.
;;  `dired-mark-unmarked-files' (Emacs < 24 only) - Emacs 24+ version.
;;  `dired-omit-expunge' - Added optional args LINEP and INIT-COUNT.
;;  `dired-omit-mode' -  Call `dired-omit-expunge' with arg LINEP.
;;  `dired-simultaneous-find-file' -
;;     Use separate frames instead of windows if `pop-up-frames' is
;;     non-nil, or if prefix arg < 0.
;;
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
;; 2023/01/14 dadams
;;     Added redefinitions of vanilla dired-remember-marks, dired-mark-remembered.
;;     Added: diredp-saved-markings, diredp-save-markings, diredp-restore-markings.  Bind: * S, * R and Marks menu.
;;     Added: diredp-dir-sep-replacement, diredp-replace-dir-sep-in-string.  Use for constructed Dired buffer names,
;;            to prevent error for inaccessible or nonexistent directory from dired-readin-insert.
;;     dired-read-dir-and-switches, diredp-dired-recent-(dirs|files)(-other-window):
;;       Use diredp-replace-dir-sep-in-string, to protect constructed Dired buffer names
;; 2022/12/29 dadams
;;     Restore defvar of dired-shrink-to-fit - removed from vanilla Emacs 29+.
;; 2022/12/28 dadams
;;     Restore dired-pop-to-buffer - removed from vanilla Emacs 29+.
;; 2022/11/04 dadams
;;     Added: diredp-describe-file-1, diredp-file-content-description.  Former uses latter.
;;       diredp-file-content-description: Includes info from dired-show-file-type.
;;     diredp-list-file: File Type -> File type.
;;     diredp(-mouse)-describe-file: Non-negative prefix arg means describe symlink.  Use diredp-describe-file-1.
;;     diredp-list-file-attributes: Bug fix (typo): Removed quote before "all".
;; 2022/09/07 dadams
;;     diredp-do-command-in-marked(-recursive), diredp-invoke-command: COMMAND arg can be a keyboard macro.
;; 2022/08/15 dadams
;;     dired-switches-escape-p: Typo: dired-switches-check -> dired-check-switches.  Ensure SWITCHES is a string.
;; 2022/07/25 dadams
;;     Added: diredp-uninserted-subdirs.
;;     dired-maybe-insert-subdir: C-u C-u: remove all, <= 0: prompt for subdir, >= 0: prompt for switches.
;;     dired-do-run-mail: Redefine only for Emacs < 25 (bug #13561).
;; 2022/07/23 dadams
;;     Added redefinition of dired-jump.
;;     diredp-dired-this-subdir, diredp-(dired|remove)-inserted-subdirs:
;;       Wrap default-directory with expand-file-name (for string= test).
;;     dired-goto-file:
;;       Added optional arg OPEN-HIDDEN-DIR-P.
;;       Pass non-nil OPEN-HIDDEN-DIR-P to dired-goto-file-1 (don't use its code here).
;;       Show a message whether found or not.  (Don't show Mark set message from push-mark.)
;;     dired-goto-file-1: Use compare-strings, not string-collate-equalp.
;;                        Added optional arg OPEN-HIDDEN-DIR-P.
;; 2022/07/22 dadams
;;     Added: dired-goto-file-1 redefinition (Emacs 25+): use string-collate-equalp.
;;     dired-goto-file: Added version for Emacs 25+ (needs string-collate-equalp):
;;                       Respect diredp-case-fold-search.  Prefix arg toggles that.
;;                       Expand input file name relative to current subdir listing, not default-directory.
;; 2022/07/20 dadams
;;     diredp-create-file-here: Add missing FILE arg to error.
;;     Restore defvar for directory-listing-before-filename-regexp, for Emacs 20.
;;     diredp-image-dired-edit-comment-and-tags: Protect widget-keymap with boundp, for Emacs 20.
;;     Added more vacuous defvars to quiet byte-compiler for Emacs 20.
;; 2022/07/17 dadams
;;     Removed: diredp-cannot-revert.
;;     diredp-dired-for-files(-other-window): Removed setting revert-buffer-function to diredp-cannot-revert.
;; 2022/07/10 dadams
;;     diredp-menu-bar-dir-menu, item diredp-add-to-this-dired-buffer: Removed :keys.
;; 2022/06/25 dadams
;;     dired-omit-mode, dired-hide-details-mode: Removed optional args for define-minor-mode - use only keywords.
;; 2022/06/03 dadams
;;     diredp-multiple-dired-menu, diredp-menu-bar-dir-menu: Added diredp-define-snapshot-dired-commands.
;; 2022/06/02 dadams
;;     Added: diredp-get-args-for-diredp-marked.
;;     diredp-marked(-other-window): Can now use multiple C-u to get all files etc.  Prefix arg changed generally.
;;     dired-insert-directory (with ls-lisp+.el):
;;       Don't handle wildcards in file name if a file with that name really exists.
;;     Updated Commentary for the case of marking the same directory in multiple listings in the same buffer,
;;       including the case of 4 C-u.
;; 2022/05/29 dadams
;;     diredp-define-snapshot-dired-commands, diredp-get-args-for-snapshot-cmd, diredp-add-to-this-dired-buffer,
;;       diredp-breadcrumbs-in-header-line-mode:
;;         Use diredp-ensure-mode.
;; 2022/05/28 dadams
;;     diredp-define-snapshot-dired-commands: Display defuns of the created commands.
;;     diredp-define-snapshot-dired-commands-1 (minor doc string change).
;; 2022/05/27 dadams
;;     Added: diredp-define-snapshot-dired-commands, diredp-define-snapshot-dired-commands-1,
;;            diredp-get-args-for-snapshot-cmd, diredp-snapshot-cmd-buffer-name-format,
;;            diredp-snapshot-cmd-time-format, diredp--snapshot-cmd-name-time.
;;     diredp-get-file-or-dir-name: Emacs 20 bug fix - use ./ and ../, not . and ..
;; 2022/05/24 dadams
;;     dired-read-dir-and-switches:
;;       Use DIRED-BUFFER also if positive prefix arg.  Better doc.  Added autoload cookie.
;; 2022/05/23 dadams
;;     dired-find-file: Show erroneous file name in error msg.  Redefine for all Emacs versions now, not just 20.
;; 2022/05/22 dadams
;;     dired-read-dir-and-switches: Prevent any additional reads from changing raw prefix arg used for later tests.
;; 2022/05/21 dadams
;;     diredp-mouse-3-menu: Corrected This File entry for Describe.
;; 2022/05/15 dadams
;;     Added alias diredp-move-this-file.
;;     diredp-mouse-3-menu: Added diredp-rename-this-file.
;;     diredp-single-move-copy-link-menu: Renamed item diredp-rename-this-file.
;;     diredp-menu-bar-regexp-menu: Renamed item dired-do-rename-regexp.
;; 2022/03/31 dadams
;;     diredp-move-files-named-in-kill-ring: Use diredp-filename-separator for split-string.
;; 2022/03/27 dadams
;;     Added (aliases) diredp-mark-if-sexp, diredp-mark-if-sexp-recursive.
;; 2022/01/28 dadams
;;     dired-buffers-for-dir:
;;       Revert to using dired-in-this-tree, not file-in-directory-p.  (Previous Emacs 28 change was wrong.)
;;       Apply expand-file-name to DIR, not to result of file-name-as-directory.
;;     dired-hide-details-mode: Update for Emacs 28 - include wdired-mode for derived-mode-p.
;;     Everywhere: No need any more to apply expand-file-name to arg of dired-buffers-for-dir.
;; 2021/12/11 dadams
;;     diredp-display-image: Removed unneeded binding of inhibit-changing-match-data (deprecated in Emacs 29).
;; 2021/12/10 dadams
;;     Added: diredp-create-file-here, redefinition of dired-do-touch.
;;     diredp-menu-bar-dir-menu: Added diredp-create-file-here.
;;                               Moved it and New Directory after dired-maybe-insert-subdir and a separator.
;;     dired-do-delete(-recursive): Zero ARG now deletes marked, but with delete-by-moving-to-trash flipped.
;;     Protect delete-by-moving-to-trash with boundp everywhere.
;; 2021/12/09 dadams
;;     dired-buffers-for-dir: Expand DIR arg with expand-file-name (fixes Emacs bug #52395).
;; 2021/11/30 dadams
;;     diredp-do-apply/eval-marked-recursive:
;;       Typo in body: current-prefix-arg -> arg.  Corrected first argument to diredp-get-files.
;; 2021/10/03 dadams
;;     Added: diredp-toggle-dot+dot-dot-flag.
;;     dired-toggle-marks: Added optional arg FLIP.  Respect diredp-toggle-dot+dot-dot-flag.
;; 2021/09/23 dadams
;;     diredp-menu-bar-multiple-menu: Removed items that are anyway on Apply (Map) submenu.
;; 2021/07/22 dadams
;;     Added: diredp-remove-inserted-subdirs, diredp-fit-one-window-frame.
;;     dired-maybe-insert-subdir: With negative prefix arg, remove all inserted subdir listings.
;;     diredp-fit-frame-unless-buffer-narrowed, diredp-toggle-marks-in-region: Use diredp-fit-one-window-frame.
;; 2021/06/21 dadams
;;     dired-buffers-for-dir: Updated for Emacs 28: Added optional arg SUBDIRS.
;;     diredp-dired-inserted-subdirs, diredp-next-subdir: dired-get-subdir-min -> cdr (Emacs 27 removed it).
;;     dired-get-filename: Updated per Emacs 27: Removed Emacs 22-26 string-to-multibyte hack.
;;     dired-do(-flagged)-delete: Applied fix for bug #48805.
;;     dired-mark-pop-up: Updated per Emacs 27+: Reset tab-line-exclude to nil.
;; 2021/06/16 dadams
;;     diredp--dired-recent-files-1: Bug fix in generated rev-fn: use BUFNAME, not BUFFER.
;; 2021/06/07 dadams
;;     Added redefinitions of dired-repeat-over-lines and dired-toggle-marks (bug #48883).
;; 2021/05/10 dadams
;;     diredp-copy-filename-as-kill-recursive: use diredp-filename-separator, not SPC.
;;     dired-copy-filename-as-kill, diredp-yank-files: Mention diredp-filename-separator in doc string.
;;     diredp-copy-abs-filenames-as-kill-recursive: Corrected doc string to mention diredp-last-copied-filenames.
;; 2021/04/20 dadams
;;     Added: dired-move-to-filename, and made it a command.
;; 2021/04/14 dadams
;;     diredp-menu-bar-dir-menu: Added dired-undo.
;; 2021/03/20 dadams
;;     Added: diredp--dired-recent-files-1.
;;     diredp-recent-files-buffer is for dirs also now.
;;     diredp-dired-recent-(files|dirs)(-other-window):
;;       Added FILES arg.  Use diredp--dired-recent-files-1.  Revert-buffer respects prefix arg, else relists same.
;;     diredp-read-include/exclude: Added optional arg keep-duplicates-p.  Delete dups by default.
;; 2021/03/19 dadams
;;     Added: diredp-case-fold-search, diredp-default-sort-arbitrary-function, diredp-sort-arbitrary-command,
;;            diredp-sort-arbitrary, diredp-string-less-p, diredp-(full|nondir)-file-name-(less|more)-p.
;;     diredp-dired-recent-(files|dirs)(-other-window): Apply diredp-sort-arbitrary.
;;     diredp-dired-recent-dirs(-other-window): Made revert-buffer-function a local var (forgot this).
;;     diredp-sort-arbitrary-command: Add to diredp-menu-bar-dir-menu and bind to C-M-L.
;; 2021/03/10 dadams
;;     Added: diredp-copy-as-kill-from-clipboard.
;;     diredp-yank-files: You can now also yank the file names from the clipboard.
;; 2021/03/03 dadams
;;     Renamed: diredp-omit-files-regexp to diredp-omit-files-font-lock-regexp,
;;              diredp-omit-line-regexp to diredp-omit-lines-regexp.
;;     diredp-omit-lines-regexp: Value can also be a variable whose value is a regexp.
;;     dired-omit-expunge: If REGEXP is a variable, use its value.
;; 2021/03/02 dadams
;;     Added: diredp-omit-line-regexp, redefinitions of dired-do-kill-lines, dired-omit-expunge, dired-omit-mode.
;; 2021/02/07 dadams
;;     Added: diredp-mark.  Give it dired-mark's keys.
;;     diredp-mark-region-files: Non-negative prefix arg now prompts for the CHAR to mark with.
;; 2021/02/04 dadams
;;     diredp-mark(-region-files)-with-char: Updated since bug #46243 is now fixed (for Emacs 28+).
;; 2021/02/01 dadams
;;     Added: diredp-mark-region-files-with-char, diredp-mark-with-char.
;;     diredp-marks-mark-menu, diredp-mouse-3-menu: Added items for diredp-mark(-region-files)-with-char.
;; 2021/01/02 dadams
;;     Updates for Emacs 27.1.
;;       dired-map-over-marks-check: Use ngettext for Emacs 27.
;;       dired-do-search, dired-do-query-replace-regexp: Use fileloop-initialize-(search|replace) for Emacs 27.
;;       dired-do-find-regexp: Require xref.el.  Map over rgrep-find-ignored-directories.
;;                             For Emacs 27+, xref--show-xrefs needs a fetcher function, not a file list.
;; 2020/12/27 dadams
;;     dired(p)-do-find-marked-files(-recursive): Don't pass plain C-u as arg to dired-simultaneous-find-file.
;; 2020/12/05 dadams
;;     diredp-re-no-dot:
;;       Changed value to be that of directory-files-no-dot-files-regexp.  See comment in code.
;;       This value is OK for use by directory-files, at least.
;;       See https://lists.gnu.org/archive/html/emacs-devel/2020-04/msg00764.html and followups.
;; 2020/12/01 dadams
;;     dired-mark-files-regexp: For Lisp, swapped nil and non-nil (other than no-dir) cases of
;;       last arg, to be compatible with vanilla Emacs.  (No change to interactive behavior.)
;;       So it's no longer true that the last arg is just passed to dired-get-filename as LOCALP.
;;     diredp-menu-bar-regexp-menu, diredp-marks-(un)mark-menu: Adjusted last arg to dired-mark-files-regexp.
;; 2020/10/12 dadams
;;     diredp--add-dired-to-invisibility-hook: Correction for Emacs 20.
;; 2020/10/06 dadams
;;     Changes to use the Emacs 27+ way of hiding subdir listings.
;;       Added: macro with-silent-modifications (with changes to work with older Emacs).
;;       Added: dired--find-hidden-pos, dired--hidden-p, dired--hide, dired--unhide, dired-remember-marks,
;;              dired-unhide-subdir, dired-subdir-hidden-p, dired-add-entry, dired-remember-hidden,
;;              dired-move-to-end-of-filename, dired-check-switches, dired-hide-subdir, dired-hide-all.
;; 2020/09/04 dadams
;;     dired-rename-file: Applied fix for bug #32899.
;; 2020/06/24 dadams
;;     dired-do-find-regexp-and-replace: Bug fix: let -> let*.
;; 2020/03/16 dadams
;;     diredp-(un)mark-region-files, diredp-flag-region-files-for-deletion, diredp-toggle-marks-in-region:
;;       Added (when (and (bolp) (> end beg)) (backward-char)) for end pos (Emacs bug #39902).
;; 2020/01/12 dadams
;;     Added:diredp-make-obsolete, diredp-make-obsolete-variable.  Use everywhere.
;; 2020/01/05 dadams
;;     diredp-font-lock-keywords-1: Fixed typo: Second diredp-rare-priv occurrence -> diredp-link-priv.
;; 2020/01/04 dadams
;;     diredp-font-lock-keywords-1: Removed .* after dired-move-to-filename-regexp.  Thx to Axel Svensson.
;; 2019/12/28 dadams
;;     diredp-y-or-n-files-p: Restore echo area when done.
;; 2019/12/26 dadams
;;     Added: diredp-maybe-save-visited.
;;     Renamed: diredp-do-apply/eval(-recursive) to diredp-do-apply/eval-marked(-recursive).
;;     diredp-do-(apply-to|eval-in|invoke-in|command-in|apply/eval)-marked(-recursive): Use diredp-maybe-save-visited
;; 2019/12/23 dadams
;;     Added: diredp-do-aggregate-apply-to-marked-recursive, diredp-ensure-fn-nonzero-arity,
;;            diredp-ensure-fn-zero-arity.
;;     diredp-get-file-or-dir-name: Added optional LOCALP arg.  No longer local (by default).
;;     diredp-do-(apply-to|eval-in|invoke-in|apply/eval)-marked-recursive: Return list of results.
;;     diredp-(apply-to|invoke-in|eval-in|invoke/eval-in)-this-file, diredp-invoke-command:
;;         Get absolute, not relative, file name with dired-get-filename.
;; 2019/12/22 dadams
;;     Added: diredp-do-aggregate-apply-to-marked, diredp-do-aggregate-invoke-in-marked,
;;            diredp-do-aggregate-eval-in-marked, diredp-do-command/macro-in-marked-recursive, diredp-report-summary.
;;     Renamed: diredp-do-emacs-command to diredp-do-command/macro-in-marked,
;;              diredp-invoke-emacs-command to diredp-invoke-command.
;;     diredp-report-file-result: Return RESULT (input arg).
;;     Bound to keys and added to new submenus: diredp-do-(apply-to|invoke-in|eval-in|command)-marked(-recursive).
;;     Removed bindings for diredp-do-apply/eval(-recursive).
;;     Updated diredp-dired-plus-description.
;; 2019/12/19 dadams
;;     Added: diredp-do-eval-in-marked-recursive.
;;     Renamed diredp-eval-lisp-sexp to diredp-eval-in-this-file,
;;             diredp-do-lisp-sexp   to diredp-do-eval-in-marked.
;;     diredp-do-apply/eval-recursive: Pass ARG to diredp-get-files, unless single C-u.
;;     diredp-do-(apply-to|invoke-in|apply/eval)-marked(-recursive),
;;       diredp-(apply-to|invoke-in|invoke/eval)-this-file: Raise error if wrong-arity function.
;;     diredp-(apply-to|invoke-in|invoke/eval|eval)-this-file:
;;       Added ARG, passed to diredp-get-file-or-dir-name if multiple C-u .
;; 2019/12/18 dadams
;;     Added: diredp-filename-separator.  Use in dired-copy-filename-as-kill, diredp-yank-files, to allow spaces
;;            etc. in file names.  (Suggested by Alex Bernhofer.)
;;     Renamed: diredp-apply-function-to-file-name to diredp-apply-to-this-file.
;; 2019/12/14 dadams
;;     Added: diredp-prefix-arg-all-files, diredp-do-apply-to-marked, diredp-do-apply-to-marked-recursive,
;;            diredp-do-invoke-in-marked, diredp-do-invoke-in-marked-recursive.
;;     dired-map-over-marks: Fixed for ARG = 256: use diredp-prefix-arg-all-files.
;;     diredp-apply-function-to-file-name, diredp-invoke-in-this-file:
;;       Added second arg (raw prefix arg) - pass it to diredp-get-file-or-dir-name.
;; 2019/12/08 dadams
;;     Renamed: diredp-invoke-function-no-args to diredp-invoke-in-this-file.
;; 2019/12/05 dadams
;;     Added: diredp-invoke/eval-in-this-file.
;;     Renamed: diredp-do-apply-function(-recursive) to diredp-do-apply/eval(-recursive).
;;     diredp-do-apply/eval(-recursive): With C-u, can handle a sexp, not just a function name.
;;     diredp-do-apply/eval-recursive: Pass nil as first arg to diredp-get-files, for non-C-u case.
;;     diredp-(do|eval)-lisp-sexp: No longer only for Emacs 22+.
;;     diredp-read-expression: Usable with any Emacs version.  No longer aliased to read--expression.
;;     diredp-dired-plus-description: Updated for command renamings.  Remove mention of diredp-do-lisp-sexp.
;; 2019/10/22 dadams
;;     Added: diredp-recent-files-map, diredp-recent-files-quit-kills-flag, diredp-quit-window-kill.
;;     diredp-dired-recent-files(-other-window):
;;       (use-local-map diredp-recent-files-map)
;;       (set (make-local-variable 'revert-buffer-function)...), not setq.
;; 2019/10/21 dadams
;;     Added: diredp-revert-displayed-recentf-buffers, diredp-recent-files-buffer.
;;     diredp-list-file-attributes: Corrected - either a list of integers or a non-list.
;;     diredp-list-marked: Pass nil to dired-get-marked-files for DISTINGUISH-ONE-MARKED arg.
;;     diredp-dired-recent-files(-other-window): (set (make-local-variable 'diredp-recent-files-buffer) bufname)
;;     diredp-do-(add-to|remove-from)-recentf: Call diredp-revert-displayed-recentf-buffers afterward.
;;     diredp-add-file-to-recentf, diredp-remove-file-from-recentf:
;;       Make interactive (commandp).  Use code from recentf.el to add/remove.
;;       Call diredp-revert-displayed-recentf-buffers afterward.
;;     diredp-print-this-file, diredp-compress-this-file, diredp-(add|remove)-this-(to|from)-recentf:
;;       Bind use-file-dialog to nil so get correct file name.
;; 2019/10/18 dadams
;;     Added: diredp-add-this-to-recentf, diredp-remove-this-from-recentf.
;;     diredp-dired-recent-dirs(-other-window): Use diredp-dired-recent-dirs as revert function.
;;     diredp-dired-union-1, diredp-fileset-1, diredp-compilation-files-other-window:
;;       Set dired-sort-inhibit to t.
;;     diredp-mouse-3-menu: Added items for diredp-(add|remove)-this-(to|from)-recentf.
;; 2019/10/16 dadams
;;     Added: diredp-do-add-to-recentf,  diredp-do-remove-from-recentf, diredp-add-file-to-recentf,
;;            diredp-remove-file-from-recentf.
;;     diredp-dired-plus-description: Add diredp-do-(add-to|remove-from)-recentf to doc.
;;     diredp-menu-bar-multiple-menu: Add diredp-do-(add-to|remove-from)-recentf.
;;     diredp-dired-recent-files(-other-window): Provide revert function that keeps listing in recentf-list order.
;; 2019/10/15 dadams
;;     Set dired-sort-inhibit to t wherever set revert-buffer-function to diredp-cannot-revert.
;; 2019/10/13 dadams
;;     Added: diredp-cannot-revert, diredp-recent-files, diredp-dired-recent-files,
;;            diredp-dired-recent-files-other-window.
;;     diredp-dired-for-dirs(-other-window): Bind revert-buffer-function to diredp-cannot-revert.
;;     Bind diredp-dired-for-dirs to C-x D r, diredp-dired-recent-files to C-x D R.
;;     diredp-menu-bar-regexp-menu, diredp-marks-(un)mark-menu:
;;       Added items for the different dired-mark-files-regexp behaviors.
;; 2019/10/12 dadams
;;     dired-mark-files-regexp: Fixed prefix arg for LOCALP, to correspond to doc string:
;;       none, C-u (no dir),  M-9, C-u C-u (rel to default dir), M--, M-0 (absolute)
;; 2019/07/03 dadams
;;     dired-mark-unmarked-files: Apply fix for Emacs bug #27465.
;;     diredp-mark-if, diredp-mark-sexp(-recursive), dired-mark-unmarked-files:
;;       Use char-after, not diredp-looking-at-p.
;; 2019/07/19 dadams
;;     diredp-change-marks-recursive, diredp-unmark-all-files-recursive,
;;       diredp-mark-files(-containing)-regexp-recursive, diredp-mark-sexp-recursive, diredp-mark-recursive-1:
;;         Added missing PREDICATE arg in calls to diredp-get-subdirs.
;; 2019/06/25 dadams
;;     diredp-mark-if, diredp-this-file-(un)marked-p: Use regexp-quote for marker char.
;; 2019/06/03 dadams
;;     Removed autoload cookie for diredp-omit-files-regexp - it evaluates dired-omit-files, from dired-x.el.
;;     Hard-require dired-x.el.  (No reason not to.)  Removed fboundp guards for it.
;; 2019/04/22 dadams
;;     Added diredp-move-files-named-in-kill-ring.  Bound to C-w.
;; 2019/04/21 dadams
;;     Added redefinitions of dired-do-find-regexp, dired-do-find-regexp-and-replace.
;;     diredp-multiple-search-menu: Added "Using TAGS Table" for dired-do-(query-replace|search).
;; 2019/04/20 dadams
;;     Added:
;;       diredp-map-over-marks-and-report, diredp-do-emacs-command, diredp-invoke-emacs-command,
;;       diredp-read-command, diredp-do-lisp-sexp, diredp-eval-lisp-sexp, diredp-report-file-result,
;;       diredp-do-report-echo-limit, diredp-fewer-than-N-files-p, diredp-fewer-than-echo-limit-files-p,
;;       diredp-apply-function-to-file-name, diredp-invoke-function-no-args, diredp-list-file-attributes.
;;     diredp-do-apply-function: Redefine to use diredp-map-over-marks-and-report.
;;     diredp-dired-plus-description, diredp-menu-bar-multiple-menu:
;;       Added diredp-do-emacs-command, diredp-do-lisp-sexp.
;;     diredp-menu-bar-multiple-menu: Reordered items.
;;     diredp-list-marked, diredp-*-recursive, diredp-describe-marked-autofiles:
;;       Use diredp-list-file-attributes for DETAILS arg interactively.
;;     diredp-yank-files, dired-query: Use diredp-list-file-attributes, not hardcoded list (5 8).
;;     diredp-set-bookmark-file-bookmark-for-marked-recursive: Corrected interactive spec.
;; 2019/04/16 dadams
;;     Added: diredp-delete-if.
;;     dired-map-over-marks-check: Added &rest argument FUN-ARGS, so FUN can accept arguments.
;; 2019/04/12 dadams
;;     dired-get-marked-files: Do not add t to RESULT.  Thx to Jeff Spencer for bug report.
;;                             If all marked is (t) for some reason reset it to nil, per vanilla Emacs 24+.
;;     diredp-compressed-extensions: Added .rar, .rev.
;; 2019/04/10 dadams
;;     Added diredp-read-expression (forgot it when added diredp-mark-sexp-recursive).
;;     diredp-mark-sexp-recursive is thus only for Emacs 22+.
;; 2019/03/20 dadams
;;     Added option diredp-omit-files-regexp.
;;     Face diredp-omit-file-name: Added strike-through.
;;     diredp-font-lock-keywords-1, for face diredp-omit-file-name:
;;       Move to file name.  Use diredp-omit-files-regexp.  Append * for executable flag.  Highlight whole line.
;; 2019/03/17 dadams
;;     diredp-font-lock-keywords-1:
;;       Use just dired-omit-files as regexp - its components already have ^...$.
;;       Removed superfluous execute *'s in regexps and superfluous concat for compressed extensions.
;;     Face diredp-omit-file-name: Removed :strike-through for default value.
;; 2019/03/16 dadms
;;     Added face diredp-omit-file-name.
;;     diredp-font-lock-keywords-1: Use face diredp-omit-file-name for dired-omit-files matches.
;; 2019/03/15 dadams
;;     diredp-font-lock-keywords-1: Treat dired-omit-files like dired-omit-extensions.
;; 2019/01/27 dadams
;;     Added: diredp-mark-files-containing-regexp-recursive.
;;              Bound to M-+ % g.  Added to diredp-marks-recursive-menu, diredp-regexp-recursive-menu.
;; 2019/01/17 dadams
;;     Added: diredp-mark-sexp-recursive.  Bound to M-+ M-(, M-+ * (.  Added to diredp-marks-recursive-menu.
;;     dired-query: Use dired-query-alist only when available.
;;     diredp-move-file: Fix format string in error call.
;;     diredp-mark-symlinks-recursive: Added missing DETAILS arg for diredp-mark-recursive-1.
;; 2019/01/01 dadams
;;     Added: diredp-list-file.
;;     Added redefinitions of dired-query, dired-handle-overwrite, dired-copy-file(-recursive), dired-rename-file,
;;           dired-hardlink.
;;     Added optional arg DETAILS to these functions: diredp-get-(subdirs|files), diredp-y-or-n-files-p,
;;       diredp-list-(marked|files), diredp-yank-files, diredp-describe-marked-autofiles, plus all functions with
;;       "recursive" in their name except diredp-get-confirmation-recursive.
;;       Added optional arg DETAILS.
;;     diredp-get-(subdirs|files), diredp-y-or-n-files-p, diredp-list-(marked|files), diredp-yank-files,
;;       diredp-describe-marked-autofiles:
;;         Added optional arg DETAILS.
;;     diredp-list-files: Use dired-list-file, to optionally show details.
;;     diredp-yank-files: Non-positive prefix arg shows details now.
;; 2018/12/02 dadams
;;     dired-mark-pop-up: Work around Emacs 22 bug in dired-pop-to-buffer which can exit in Dired buffer.
;; 2018/10/17 dadams
;;     dired-read-dir-and-switches: Removed mention of icicle-file-sort-first-time-p (no longer used in Icicles).
;; 2018/09/21 dadams
;;     diredp-image-dired-edit-comment-and-tags, diredp-w32-drives:
;;       Use pop-to-buffer-same-window, not switch-to-buffer.
;; 2018/09/14 dadams
;;     Added: diredp-move-file-dirs, diredp-move-file.
;; 2018/06/30 dadams
;;     Added: diredp-delete-if-not.
;; 2018/06/16 dadams
;;     Added: diredp-visit-ignore-extensions, diredp-visit-ignore-regexps, diredp-visit-next-file,
;;            diredp-visit-previous-file, diredp-visit-this-file, diredp-visit-ignore-regexp.
;;     Bind the commands to C-down, C-up, e.
;; 2018/03/25 dadams
;;     Added: diredp-user-error.
;;     Updated for Emacs 27-pretest-2 change in dired-get-marked-files signature.
;;       dired-get-marked-files: Added optional arg ERROR-IF-NONE-P.
;;       diredp-list-marked, diredp-insert-subdirs, dired-do-(i)search(-regexp), dired-do-query-replace-regexp,
;;         dired-do-find-marked-files, diredp-describe-marked-autofiles:
;;           Added optional arg INTERACTIVEP.
;;           Pass non-nil ERROR-IF-NONE-P to dired-get-marked-files when INTERACTIVEP.  (See Emacs bug #30938.)
;; 2018/03/23 dadams
;;     Added diredp-mark-if.  Removed: redefinition of dired-mark-if.
;;       Differences: msg and return value include both number of matches and number of changes.
;;     Added redefinitions (use diredp-mark-if) of dired-flag-auto-save-files,
;;           dired-mark-(files-containing-regexp|symlinks|directories|executables).
;;     Everywhere: Use diredp-mark-if, not dired-mark-if.
;; 2018/03/03 dadams
;;     diredp-delete-dups: defalias the symbol, not its symbol-function (dunno why I did the latter).
;; 2018/02/28 dadams
;;     Added: diredp-last-copied-filenames, diredp-copy-abs-filenames-as-kill-recursive,
;;            and redefinition of vanilla diredp-last-copied-filenames.
;;     diredp-copy-abs-filenames-as-kill: Use diredp-ensure-mode in interactive spec.
;;     diredp-copy-filename-as-kill-recursive: Update diredp-last-copied-filenames with filenames string.
;;     diredp-yank-files: Require confirmation for pasting, using diredp-y-or-n-files-p.
;;                        Get file names from variable diredp-last-copied-filenames, not kill-ring.
;;                        Added NO-CONFIRM-P arg.
;;     diredp-ensure-mode: Added doc string.
;;     diredp-do-grep, diredp-do-grep-recursive: Changed bindings to C-M-G and M-+ C-M-G, due to M-g conflict.
;; 2018/02/27 dadams
;;     Added: diredp-copy-abs-filenames-as-kill, diredp-yank-files (aka diredp-paste-files) (bound to C-y).
;;     diredp-menu-bar-multiple-menu: Added diredp-copy-abs-filenames-as-kill.
;;     diredp-menu-bar-dir-menu: Added diredp-yank-files.
;; 2018/01/11 dadams
;;     diredp-get-files:
;;       Set IGNORE-MARKS-P to non-nil if nothing marked here.  (It was not getting all if nothing marked.)
;;     diredp-marked-recursive(-other-window):
;;       Corrected interactive spec, which was missing nil DIRNAME arg.  Corrected body: use DIRNAME.
;;     diredp-get-files-for-dir, diredp-do-bookmark-dirs-recursive, diredp-change-marks-recursive,
;;       diredp-unmark-all-files-recursive, diredp-mark-files-regexp-recursive, diredp-mark-recursive-1,
;;       diredp-do-delete-recursive: Factor out (dired-buffers-for-dir (expand-file-name directory)).
;; 2018/01/03 dadams
;;     dired-mark-files-regexp: Corrected doc string wrt prefix args.  Thx to John Mastro.
;;     diredp-do-grep-recursive: Removed unused optional arg IGNORE-MARKS-P.
;;     diredp-marked-recursive(-other-window): Moved handling of optional arg from interactive spec to body.
;; 2018/01/02 dadams
;;     Added: diredp-flag-auto-save-files-recursive.  Bound to M-+ #.
;;     diredp-get-file-or-dir-name, diredp-marked-here: Doubled backslashes to escape dots.
;;     diredp-marked-here: Fixed regexp to match only double-dot, not single-dot.
;;     diredp-flag-auto-save-files-recursive: Updated to include more M-+ keys.
;;     diredp-marks-recursive-menu: Added diredp-flag-auto-save-files-recursive.
;; 2017/12/31 dadams
;;     diredp-get-files-for-dir: Pass non-nil NO-DOT-DOT-P arg to diredp-marked-here.
;;     dired-get-marked-files: Allow use of FILTER and DISTINGUISH-ONE-MARKED together.
;;     diredp-marked-here: Added optional arg NO-DOT-DOT-P.
;;     diredp-change-marks-recursive, diredp-unmark-all-files-recursive: Removed unused vars include-dirs, files.
;; 2017/12/30 dadams
;;     Added: diredp-change-marks-recursive, diredp-unmark-all-files-recursive, diredp-unmark-all-marks-recursive.
;;            Bound to M-+ * c, M-+ M-DEL, M-+ U, respectively.
;;     diredp-menu-bar-marks-menu: Rename item Change Marks to Change Mark.
;;     diredp-marks-recursive-menu, diredp-multiple-recursive-menu:
;;       Added diredp-change-marks-recursive, diredp-unmark-all-(files|marks)-recursive.
;; 2017/12/21 dadams
;;     Added: diredp-mark-recursive-1.  Forgot to add it last June.
;; 2017/12/17 dadams
;;     Removed: diredp-display-graphic-p.
;;     Do not use diredp-display-graphic-p to allow binding diredp-bind-problematic-terminal-keys by default.
;; 2017/11/25 dadams
;;     diredp-nb-marked-in-mode-name: Wrap last :eval sexp in save-excursion.
;;                                    Protect Call dired-current-directory only when dired-subdir-alist.
;; 2017/10/23 dadams
;;     Added: diredp-count-.-and-..-flag, diredp--reuse-dir-buffer-helper.
;;     Removed: diredp-mouse-find-file.
;;     diredp-find-file-reuse-dir-buffer, diredp-mouse-find-file-reuse-dir-buffer,
;;       diredp-up-directory-reuse-dir-buffer:
;;         Use diredp--reuse-dir-buffer-helper.
;;     diredp-find-file-reuse-dir-buffer: Changed logic: do find-alternate-file only if target is a dir not in
;;                                        Dired and current Dired buffer is in only this window.
;;     diredp-mouse-find-file-reuse-dir-buffer: Added optional args FIND-FILE-FUNC and  FIND-DIR-FUNC.
;;     diredp-up-directory, diredp-up-directory-reuse-dir-buffer: Pass OTHER-WINDOW arg to diredp-w32-drives.
;;     diredp-nb-marked-in-mode-name: Show also number of lines in current listing, and listing-relative lineno,
;;                                    respecting diredp-count-.-and-..-flag.
;;     diredp-find-a-file*: Added autoload cookies.
;; 2017/08/18 dadams
;;     Fixed emacswiki URLs everywhere.  They changed the locations and changed http to https.
;; 2017/06/30 dadams
;;     Added: diredp-bind-problematic-terminal-keys, diredp-display-graphic-p.
;;     Guard bindings of problematic keys with diredp-display-graphic-p & diredp-bind-problematic-terminal-keys.
;;     Documented problematic keys for terminal mode in commentary.
;; 2017/06/23 dadams
;;     Added: diredp-read-regexp (removed alias to read-regexp), diredp-marks-recursive-menu,
;;       diredp-mark-executables-recursive (bound to M-+ * *),
;;       diredp-mark-directories-recursive (bound to M-+ * /),
;;       diredp-mark-extension-recursive (bound to M-+ * .),
;;       diredp-mark-autofiles-recursive (bound to M-+ * B),
;;       diredp-mark-executables-recursive (bound to M-+ * *),
;;       diredp-mark-directories-recursive (bound to M-+ * /),
;;       diredp-mark-symlinks-recursive (bound to M-+ * @),
;;     Bind diredp-mark-autofiles to * B.
;;     diredp-marked-here: Bind dired-marker-char to ?*.
;;     diredp-mark-files-regexp-recursive: Better msgs - show total count.
;;     Everywhere: Use diredp-looking-at, not looking-at.  Use diredp-read-regexp, not dired-read-regexp.
;; 2017/05/30 dadams
;;     Fixed typo: direp--set-up-font-locking -> diredp--set-up-font-locking.
;; 2017/05/22 dadams
;;     Added: direp--set-up-font-locking.
;;     Use direp--set-up-font-locking instead of lambda in dired-mode-hook.
;; 2017/04/09 dadams
;;     Version 2017.04.09.
;;     Added: diredp-multiple-move-copy-link-menu, diredp-multiple-rename-menu, diredp-multiple-dired-menu,
;;            diredp-multiple-omit-menu, diredp-multiple-delete-menu, diredp-single-bookmarks-menu,
;;            diredp-single-encryption-menu, diredp-single-image-menu, diredp-single-open-menu,
;;            diredp-single-move-copy-link-menu, diredp-single-rename-menu.
;;            Moved single menu items there.
;;     Renamed: diredp-menu-bar-encryption-menu        to diredp-multiple-encryption-menu,
;;              diredp-menu-bar-mark-menu              to diredp-menu-bar-marks-menu,
;;              diredp-menu-bar-operate-menu           to diredp-menu-bar-multiple-menu,
;;              diredp-menu-bar-operate-bookmarks-menu to diredp-multiple-bookmarks-menu,
;;              diredp-menu-bar-operate-recursive-menu to diredp-multiple-recursive-menu,
;;              diredp-menu-bar-operate-search-menu    to diredp-multiple-search-menu,
;;              diredp-menu-bar-images-menu            to diredp-multiple-images-menu,
;;              diredp-menu-bar-images-recursive-menu  to diredp-images-recursive-menu,
;;              diredp-menu-bar-immediate-menu         to diredp-menu-bar-single-menu,
;;              diredp-menu-bar-regexp-recursive-menu  to diredp-regexp-recursive-menu,
;;              diredp-menu-bar-subdir-menu            to diredp-menu-bar-dir-menu.
;;     Added dired-do-rename to diredp-multiple-rename-menu.
;;     diredp-nonempty-region-p: Ensure (mark) also.
;; 2017/03/30 dadams
;;     Moved key bindings to end of file.  Moved defgroup before defcustoms.
;;     Bind dired-multiple-w32-browser to C-M-RET, diredp-multiple-w32-browser-recursive to M-+ C-M-RET.
;; 2017/03/29 dadams
;;     Added: diredp-dired-union-other-window, diredp-add-to-dired-buffer-other-window.
;;     diredp-dired-union-1: Added optional arg OTHERWIN.
;;     diredp-dired-plus-description: Updated doc string.
;;     diredp-menu-bar-subdir-menu: Added diredp-dired-for-files.
;;     Bind diredp-w32-drives to :/, diredp-dired-inserted-subdirs to C-M-i.
;;     Bind diredp-add-to-dired-buffer to C-x D A (not C-x E), diredp-dired-union to C-x D U (not C-x D),
;;          diredp-fileset to C-x D S (not C-M-f), diredp-dired-recent-dirs to C-x D R (not C-x R),
;;          diredp-dired-for-files to C-x D F, plus other-window versions.
;; 2017/03/24 dadams
;;     Added defalias for dired-read-regexp.
;;     diredp-mouse-3-menu: Removed second arg to mouse-save-then-kill.
;; 2017/02/20 dadams
;;     diredp-(next|previous)-line, diredp-(next|prev)-dirline, diredp-(next|prev)-subdir:
;;       Update interactive spec to use (in effect) ^p for prefix arg (for shift-select-mode).
;; 2017/01/12 dadams
;;     dired-mark-files-regexp: Swapped prefix-arg behavior for relative and absolute name matching.
;; 2017/01/01 dadams
;;     dired-mark-files-regexp: Fix to prompt for no prefix arg.
;; 2016/12/28 dadams
;;     dired-mark-files-regexp: Corrected prompt string for Mark/UNmark.  Thx to Tino Calancha.
;; 2016/11/20 dadams
;;     diredp-menu-bar-operate-search-menu: Added dired-do-find-regexp and dired-do-find-regexp-and-replace.
;;     Bind dired-do-search to M-a and dired-do-query(-replace)-regexp to M-q.
;;     diredp-dired-plus-description: Added dired-do-find-regexp and dired-do-find-regexp-and-replace.
;; 2016/10/12 dadams
;;     diredp-compressed-extensions: Added extensions .xz and .lzma.  Thx to xuhdev (https://www.topbug.net/).
;; 2016/09/20 dadams
;;     Emacs 25.1: Bind M-z to dired-do-compress-to (replaces c).  (Emacs bug #24484.)
;;                 diredp-menu-bar-operate-menu: Added item: Compress to (dired-do-compress-to).
;; 2016/09/15 dadams
;;     Added: diredp-max-frames.
;;     dired-do-find-marked-files: Pass non-nil ARG to dired-get-marked-files only if it is a cons.
;;                                 Clarified doc string wrt prefix arg.
;;     dired-simultaneous-find-file: Require confirmation if more files than diredp-max-frames.
;;     diredp-do-find-marked-files-recursive: Clarified doc string wrt prefix arg.
;;     Thx to Tino Calancha.
;; 2016/09/14 dadams
;;     diredp-dired-plus-description: Added entry for dired-hide-details-mode - ( key.
;; 2016/08/26 dadams
;;     diredp-y-or-n-files-p: pop-to-buffer only when the buffer was created.
;;                            Update wrt vanilla (scroll actions).
;;     diredp-do-query-replace-regexp-recursive:
;;       Call diredp-get-confirmation-recursive.
;;       Use only diredp-get-files, not dired-get-marked-files.
;;       Non-positive prefix arg means DELIMITED.
;; 2016/08/08 dadams
;;     diredp-menu-bar-mark-menu:
;;       Added: dired-mark-files-containing-regexp, dired-mark-sexp, image-dired-mark-tagged-files,
;; 2016/05/28 dadams
;;     diredp-mark-files-regexp-recursive: Use nil for dired-get-filename LOCALP arg.
;;     dired-mark-files-regexp: Corrected doc string: absolute filename matching by default.
;; 2016/05/24 dadams
;;     dired-mark-files-regexp: Added optional arg LOCALP, so can mark/unmark matching different file-name forms.
;; 2016/05/15 dadams
;;     Added: diredp-bookmark-menu, diredp-hide/show-menu, diredp-navigate-menu.
;;     Move insert after revert and rename it to Insert/Move-To This Subdir.  Move create-directory before revert.
;; 2016/04/29 dadams
;;     diredp-next-line: Respect goal-column.
;; 2016/01/24 dadams
;;     Added: diredp-ensure-bookmark+, diredp-mark-autofiles, diredp-unmark-autofiles,
;;            diredp-mark/unmark-autofiles, diredp-describe-autofile, diredp-show-metadata,
;;            diredp-mouse-describe-autofile, diredp-describe-marked-autofiles, diredp-show-metadata-for-marked
;;     Soft-require help-fns+.el (Emacs 22+) or help+20.el (Emacs 20-21).
;;     Add to menu-bar menus:
;;       diredp-(un)mark-autofiles, diredp-describe-autofile, diredp-describe-marked-autofiles.
;;     diredp-menu-bar-immediate-menu: Add diredp-describe-file only if defined.
;;     Bind diredp-describe-file to keys only if defined.
;;     Use diredp-ensure-bookmark+ everywhere, instead of its definition.
;;     diredp(-mouse)-describe-file: Define only if describe-file is defined.  Removed raising error if not.
;;     diredp-mouse-3-menu: Use  diredp-describe-autofile if diredp-describe-file is not defined.
;;     diredp-dired-plus-description: Add diredp-mouse-describe-autofile, when bound.
;;     dired-mark-if: Do not count non-changes.
;; 2015/12/15 dadams
;;     diredp-font-lock-keywords-1: Follow # with optional [/ ], for face diredp-number.  Thx to Tino Calancha.
;; 2015/11/10 dadams
;;     diredp-fileset(-other-window): Separate error msgs for unloaded filesets.el and empty filesets-data.
;; 2015/10/02 dadams
;;     dired-mark-sexp: Like vanilla, skip extended attributes marker before setting NLINK.  Thx to Tino Calancha.
;; 2015/09/29 dadams
;;     diredp-delete-this-file: Redefined to use delete-file instead of dired-do-delete.
;; 2015/09/07 dadams
;;     diredp-font-lock-keywords-1: Do not test diredp-ignore-compressed-flag when highlighting file names.
;;                                  Use separate entries for compressed and non-compressed file names.
;;                                  Added missing \\| before ignored compressed extensions.
;; 2015/09/06 dadams
;;     diredp-compressed-extensions: Added .tgz.  Removed duplicate .gz.
;;     diredp-font-lock-keywords-1: Use regexp-opt where possible, instead of mapcar regexp-quote.
;; 2015/09/05 dadams
;;     Added: diredp-compressed-extensions, diredp-ignore-compressed-flag, diredp-compressed-file-name,
;;            diredp-dir-name.
;;     diredp-font-lock-keywords-1:
;;       Allow spaces in symlink names.  Highlight compressed-file names, if diredp-ignore-compressed-flag.
;;       Use diredp-compressed-extensions instead of hardcoding extensions.
;;     Highlight d with diredp-dir-priv (fix).
;;     Treat l in third column the same as - and d there.
;;     Highlight whole line for D-flagged files, with face diredp-deletion-file-name.
;;     Thx to Nick Helm.
;; 2015/08/30 dadams
;;     dired-mark-sexp: Updated per Emacs 25 code.
;; 2015/07/30 dadams
;;     diredp-fileset(-other-window): Changed key binding from C-x F to C-x C-M-f (conflicted with find-function).
;; 2015/06/24 dadams
;;     Added: diredp-parent-dir, diredp-breadcrumbs-in-header-line-mode, diredp-set-header-line-breadcrumbs.
;; 2015/06/06 dadams
;;     Added dired-other-(frame|window).
;;     diredp-font-lock-keywords-1:
;;       Use dired-re-maybe-mark and dired-re-inode-size for permission matchings and directory names.
;;     dired(-other-(frame|window)) advice:
;;       Add interactive spec, to handle arg <= 0 (broken by change to dired-read-dir-and-switches 2015/02/02).
;;     diredp-dired-for-files: Typo: pass empty string.
;; 2015/06/05 dadams
;;     Added: diredp-grepped-files-other-window as alias for diredp-compilation-files-other-window.
;;     diredp-compilation-files-other-window: Added SWITCHES optional arg (prefix arg).
;; 2015/06/04 dadams
;;     diredp-dired-for-files(-other-window):
;;       Updated to fit change to dired-read-dir-and-switches made 2015/02/02: addition of READ-EXTRA-FILES-P.
;;       Use prefix arg to prompt for switches.
;; 2015/05/31 dadams
;;     Added: diredp-image-show-this-file,diredp-image-show-this-file-use-frame-flag, diredp-get-image-filename.
;;     image-dired-dired-toggle-marked-thumbs, diredp-menu-bar-immediate-menu [image]:
;;       Use diredp-get-image-filename.
;;     Bound diredp-image-show-this-file to C-t I.
;;     diredp-menu-bar-immediate-image-menu: Added diredp-image-show-this-file and dired-find-file.
;;     Added autoload cookies for image commands.
;; 2015/04/16 dadams
;;     Added: diredp-do-apply-function, diredp-do-apply-function-recursive.  Added to menus.  Bind to @, M-+ @.
;;     dired-do-query-replace-regexp: Handle nil ARG properly.
;; 2015/03/26 dadams
;;     Added: redefinitions of dired-do-isearch, dired-do-isearch-regexp, dired-do-query-replace-regexp,
;;            dired-do-search, to handle multi-C-u.
;;     Added: dired-nondirectory-p (Emacs 20), diredp-refontify-buffer.
;;     dired-do-byte-compile, dired-do-load, : Corrected interactive spec, to treat more than two C-u as two.
;;     dired-after-readin-hook: Add diredp-refontify-buffer.  In particular, this ensures that reverting Dired
;;       for a listing of explicit file names gets refontified.  (Just turn-on-font-lock does not refontify.)
;; 2015/03/24 dadams
;;     Added: diredp-compilation-files-other-window, diredp-file-for-compilation-hit-at-point.
;; 2015/03/06 dadams
;;     Renamed: diredp-menu-bar-recursive-marked-menu to diredp-menu-bar-operate-recursive-menu.
;;     Added: diredp-do-delete-recursive: M-+ D.  Added to diredp-menu-bar-operate-recursive-menu.
;;     Added: diredp-mark-files-regexp-recursive: M-+ % m.  Added to diredp-menu-bar-regexp-recursive-menu.
;; 2015/03/04 dadams
;;     Added: diredp-dwim-any-frame-flag, (redefinition of) dired-dwim-target-directory.
;; 2015/02/22 dadams
;;     diredp-bookmark: Corrected for use without Bookmark+ - bookmark-store signature.
;;                      Pass correct value to bmkp-autofile-set for its MSG-P arg.
;;     diredp-mouse-do-bookmark: Do not pass non-nil NO-MSG-P arg to diredp-bookmark.
;; 2015/02/03 dadams
;;     Added: diredp-add-to-this-dired-buffer.
;;     Removed: diredp-add-to-dired-buffer-other-window, diredp-dired-union-other-window.
;;     diredp-dired-union-1: Removed optional arg OTHER-WINDOW-P.
;;     diredp-menu-bar-subdir-menu: Added diredp-add-to-this-dired-buffer.
;;     dired-read-dir-and-switches, diredp-dired-union-interactive-spec:
;;       Added optional arg DIRED-BUFFER.  If nil, use current buffer name as default when reading buffer name.
;; 2015/02/02 dadams
;;     Added: diredp-add-to-dired-buffer, diredp-add-to-dired-buffer-other-window, diredp-set-union,
;;            diredp-existing-dired-buffer-p.
;;     Bind diredp-add-to-dired-buffer(-other-window) globally to C-x E, C-x 4 E.
;;     diredp-dired-union(-other-window):
;;       Added args DIRNAME and EXTRA.  Pass them to diredp-dired-union-1.  Moved "UNION" to *-interactive-spec.
;;       Pass values for new args NO-DIRED-BUFS and READ-EXTRA-FILES-P to diredp-dired-union-interactive-spec.
;;     diredp-dired-union-interactive-spec:
;;       Added args NO-DIRED-BUFS and READ-EXTRA-FILES-P.  Use (updated) dired-read-dir-and-switches.
;;       Delete dead buffers from dired-buffers.  Remove DIRNAME buffer as candidate.
;;       Apply expand-file-name to default-directory.  Return list of DIRNAME BUFS SWITCHES EXTRA-FILES.
;;     diredp-dired-union-1:
;;       Added args DIRED-NAME and EXTRA.
;;       For existing Dired buffer whose dired-directory is a cons:
;;         Include its current listing.  Replace buffer with new one of same name, after deleting its window.
;;     dired-read-dir-and-switches:
;;       Added arg READ-EXTRA-FILES-P.
;;       If chosen Dired buffer exists and is an ordinary listing then start out with its directory-files.
;;     diredp-dired-union, diredp-fileset, diredp-dired-recent-dirs: Bind globally, not just in Dired mode.
;; 2015/01/30 dadams
;;     dired-read-dir-and-switches: Remove any killed buffers from dired-buffers, before using for completion.
;; 2014/10/25 dadams
;;     diredp-dired-union-interactive-spec: Typo: quote buffer-name-history.  Pass other-window STRING.
;;     diredp-dired-union-other-window: Pass other-window STRING.
;;     dired-read-dir-and-switches: Include STRING for reading buffer name also.
;;     dired (defadvice): Corrected doc string for prefix arg >= and <= 0.
;; 2014/10/15 dadams
;;     diredp-hide-details-initially-flag:
;;       Added :set, to ensure that diredp-hide-details-last-state is kept up-to-date.
;; 2014/09/28 dadams
;;     Added: diredp-recent-dirs, diredp-read-include/exclude, diredp-root-directory-p, diredp-remove-if.
;;     diredp-dired-recent-dirs(-other-window): Added optional ARG.  Use diredp-recent-dirs.  Pass SWITCHES.
;;     dired-read-dir-and-switches: Use diredp-root-directory-p.
;;     Bound diredp-dired-recent-dirs(-other-window) to C-x R and C-x 4 R.
;;     Added diredp-dired-recent-dirs to Dir menu.
;; 2014/09/27 dadams
;;     Added: diredp-dired-recent-dirs, diredp-dired-recent-dirs-other-window, diredp-delete-dups.
;; 2014/09/26 dadams
;;     diredp-mouseover-help: dired-get-filename etc. has to be inside the save-excursion.
;;     diredp-image-dired-create-thumb: Added FILE arg.  Use numeric prefix arg as the new thumbnail size.
;; 2014/09/22 dadams
;;     diredp-mouse-3-menu: Do not place overlay unless on a file/dir name (i.e., dired-get-filename).
;; 2014/09/15 dadams
;;     dired-read-dir-and-switches: Made it (thus dired too) an Icicles multi-command.
;;     dired (defadvice): Added doc about using it with Icicles.
;; 2014/09/14 dadams
;;     Added: diredp-kill-this-tree.
;;     Removed: diredp-dired-files(-other-window), diredp-dired-files-interactive-spec.
;;     dired-read-dir-and-switches:
;;       Based on diredp-dired-files-interactive-spec implementation now, but:
;;        Moved unwind-protect outside call to list.  completing-read, not read-string, for DIRBUF.
;;        Do not allow inclusion of root directories.  Protected icicle-sort-comparer with boundp.
;;     dired-insert-subdir-validate: Make it a no-op.
;;     dired advice (doc string): Mention wildcards, Icicles.
;;     diredp-dired-for-files(-other-window):
;;       Use dired-read-dir-and-switches and dired, not diredp-dired-files-interactive-spec and
;;       diredp-dired-files.
;;     diredp-menu-bar-immediate-menu, diredp-mouse-3-menu:
;;       Added item for diredp-kill-this-tree.
;;       Corrected visible condition: expand-file-name, so ~/ compares with its expansion.
;;     diredp-font-lock-keywords-1: Include period (.) for diredp(-compressed)-file-suffix.
;; 2014/09/09 dadams
;;     Added: dired-read-dir-and-switches redefinition.
;;     Advise dired, for doc string.
;;     dired-get-filename: Hack for Emacs 20-22, to expand ~/...
;; 2014/09/07 dadams
;;     Added: redefinitions of dired-insert-subdir-newpos, dired-insert-subdir-validate.
;; 2014/07/26 dadams
;;     diredp-do-find-marked-files-recursive:
;;       Only ARG >= 0 ignores marks now.  And ARG <= 0 means find but do not display.
;; 2014/07/13 dadams
;;     diredp-mouseover-help: Wrap (goto-char pos) in save-excursion (Emacs bug #18011).
;; 2014/07/12 dadams
;;     Faces diredp(-tagged)-autofile-name: Made paler/darker (less saturated).
;;     Moved diredp-highlight-autofiles before diredp-highlight-autofiles-mode, so will be
;;      defined for first revert.
;;     diredp-mouse-3-menu: Renamed items Tag, Untag to Add Tags, Remove Tags.
;;     diredp-dired-plus-description: Updated.
;; 2014/07/11 dadams
;;     Added: diredp-highlight-autofiles-mode, diredp-highlight-autofiles,
;;            diredp-autofile-name, diredp-tagged-autofile-name.
;;     Soft-require bookmark+.el.  Soft-require highlight.el if bookmark+.el is provided.
;;     diredp-menu-bar-subdir-menu: Added item Toggle Autofile Highlighting.
;;     Removed unused face: diredp-display-msg.
;; 2014/06/29 dadams
;;     dired-get-marked-files, diredp-internal-do-deletions:
;;       Remove nils from dired-map-over-marks result.
;; 2014/05/28 dadams
;;     diredp-mode-line-marked: Use DarkViolet for both light and dark background modes.
;; 2014/05/23 dadams
;;     Added: diredp-with-help-window.
;;     diredp-list-files, diredp-dired-plus-help:
;;       Use diredp-with-help-window, not with-output-to-temp-buffer.  See Emacs bug #17109.
;; 2014/05/06 dadams
;;     Added: diredp-image-dired-required-msg, diredp-list-files-map,
;;            diredp-find-line-file-other-window, diredp-mouse-find-line-file-other-window,
;;            image-dired-dired-toggle-marked-thumbs, diredp-list-marked.
;;     Soft-require image-dired.el and image-file.el.
;;     diredp-image-dired-create-thumb: Define unconditionally.
;;     image-dired-dired-insert-marked-thumbs, diredp-image-dired-comment-file,
;;       diredp-image-dired-tag-file, diredp-image-dired-delete-tag,
;;       diredp-image-dired-display-thumb, diredp-image-dired-copy-with-exif-name,
;;       diredp-image-dired-edit-comment-and-tags, diredp-do-display-images:
;;         Define unconditionally and raise error if no image-(dired|file).el.
;;     diredp-menu-bar-immediate-image-menu, diredp-menu-bar-images-menu,
;;       diredp-menu-bar-images-recursive-menu, image-dired-mark-tagged-files:
;;         Define unconditionally and use :enable.
;;     diredp-menu-bar-images-menu, diredp-menu-bar-images-recursive-menu:
;;       Add defalias so can use menu-item with :enable.
;;     diredp-list-files: Add properties mouse-face, keymap, and help-echo.
;;     diredp-mouseover-help: Make it work also for diredp-list-files listings.
;;     image-dired-dired-insert-marked-thumbs: Add autoload cookie.
;;     dired-get-marked-files: Pass non-nil 2nd arg to dired-get-filename, to include . and .. .
;;     Bind diredp-list-marked to C-M-l and diredp-list-marked-recursive to M+ C-M-l.
;;     diredp-insert-subdirs: Exclude . and .., as dired-get-marked-files can now include them.
;;     diredp-menu-bar-operate-menu: Add diredp-menu-bar-operate-menu to menu.
;;     diredp-dired-plus-description: Mention diredp-list-marked*.
;; 2014/05/03 dadams
;;     dired-switches-escape-p: Use dired-switches-check if available, based on bug #17218 fix.
;; 2014/04/25 dadams
;;     diredp-image-dired-create-thumb:
;;       Do not call diredp-image-dired-create-thumb twice: reuse THUMB-NAME.
;; 2014/04/24 dadams
;;     Added: diredp-mouseover-help, diredp-auto-focus-frame-for-thumbnail-tooltip-flag,
;;            diredp-image-preview-in-tooltip.
;;     dired-insert-set-properties: Show image-file preview in tooltip.
;;     diredp-image-dired-create-thumb: Return thumbnail file name or nil.
;; 2014/04/23 dadams
;;     Added: diredp-looking-at-p.
;;     dired-insert-set-properties: Applied fix for bug #17228.
;; 2014/04/05 dadams
;;     Added: diredp-do-bookmark-dirs-recursive.
;;            Renamed from bmkp-create-dired-bookmarks-recursive in bookmark+-1.el (removed).
;;       Bound to M-B (aka M-S-b).
;;       Added to menus *-subdir-menu, *-operate-bookmarks-menu, *-bookmarks-menu.
;;     diredp-get-confirmation-recursive: Added optional TYPE arg.
;;     diredp-insert-subdirs-recursive: Call diredp-get-confirmation-recursive with TYPE arg.
;; 2014/02/16 dadams
;;     dired-pop-to-buffer: Do not redefine for Emacs > 24.1.
;;     dired-mark-pop-up: Updated doc string.
;; 2014/02/13 dadams
;;     Added: diredp-fileset-other-window, diredp-fileset-1.
;;     diredp-fileset: Use diredp-fileset-1.
;;     Bind diredp-dired-union(-other-window) to C-x D, C-x 4 D,
;;          diredp-fileset(-other-window)     to C-x F, C-x 4 F.
;;     Use diredp-fileset-other-window, not diredp-fileset, in menu.
;; 2014/02/03 dadams
;;     Added: diredp-hide-subdir-nomove.
;;     Added: dired-goto-file for Emacs 24+ - open hidden parent dir, so can goto destination.
;;     Replace bindings for dired-hide-subdir with diredp-hide-subdir-nomove.
;;     Bind dired-hide-subdir to M-$ (not $).
;; 2014/02/02 dadams
;;     dired-goto-file: Redefine only for Emacs < 24.
;; 2014/01/15 dadams
;;     Bind diredp-toggle-find-file-reuse-dir to C-M-R (aka C-M-S-r).
;; 2014/01/05 dadams
;;     Bind dired-omit-mode (aka dired-omit-toggle) to C-x M-o.
;; 2013/12/05 dadams
;;     diredp-do-grep-1: Call grep-default-command with arg, if grep+.el is loaded.
;; 2013/11/05 dadams
;;     Added: diredp-get-subdirs.
;;     diredp-get-files, diredp-get-files-for-dir, diredp-marked-here: Added optional arg NIL-IF-NONE-P.
;;     diredp-get-files: Pass INCLUDE-DIRS-P to diredp-files-within.
;; 2013/11/04 dadams
;;     Renamed Bookmarks submenus to Bookmark.
;;     Added Bookmark Dired Buffer to Dir menu.
;;     Alias dired-toggle-marks to dired-do-toggle for Emacs 20, instead of backwards for others.
;;     Use dired-toggle-marks everywhere instead of dired-do-toggle.
;; 2013/11/03 dadams
;;     Created submenus of Multiple menu: Bookmarks, Search.
;;     Created submenus of Multiple > Marked Here and Below menu:
;;       Images, Encryption, Search, Bookmarks.
;;     Reordered menus.
;; 2013/09/26 dadams
;;     diredp-next-line: Use let*, so line-move sees let bindings.
;; 2013/08/11 dadams
;;     diredp-dired-files-interactive-spec:
;;       Protect icicle-file-sort with boundp.  Thx to Vladimir Lomov.
;; 2013/08/06 dadams
;;     diredp-display-image,diredp-menu-bar-immediate-image-menu (:enable's):
;;       Protect diredp-string-match-p from nil argument.
;; 2013/07/24 dadams
;;     Added: diredp-nonempty-region-p.  Use everywhere, in place of its definition.
;; 2013/07/21 dadams
;;     Added: diredp-image-dired-(comment-file|copy-with-exif-name|(create|display)-thumb|
;;                                delete-tag|edit-comment-and-tags|tag-file),
;;            diredp-string-match-p, diredp-menu-bar-immediate-image-menu.
;;     Put this-file image commands on new menu diredp-menu-bar-immediate-image-menu.
;;     diredp-menu-bar-images-menu: Added diredp-do-display-images.
;;     Use diredp-string-match-p instead of string-match where appropriate.
;;     diredp-find-a-file-read-args: Removed #' from lambda.
;; 2013/07/19 dadams
;;     Added redefinition of dired-hide-details-mode.
;;     Added: diredp-hide-details-propagate-flag, diredp-hide-details-initially-flag,
;;            diredp-hide-details-last-state, diredp-hide-details-toggled,
;;            diredp-hide-details-if-dired, global-dired-hide-details-mode,
;;            diredp-fit-frame-unless-buffer-narrowed, diredp-hide/show-details,
;;            diredp-do-display-images, diredp-display-image.
;;     On dired-after-readin-hook: diredp-hide/show-details.
;;     On dired-hide-details-mode-hook: diredp-fit-frame-unless-buffer-narrowed.
;;     diredp-maplist: Use diredp-maplist, not maplist, in recursive call.
;;     diredp-next-line: Added bobp test for negative ARG.
;;                       Emacs 20 line-move returns nil, so use (progn ... t).
;;     Soft-require autofit-frame.el.
;; 2013/07/18 dadams
;;     diredp-next-line: Protect visible-p with fboundp for Emacs 20.
;; 2013/07/17 dadams
;;     Added: diredp-menu-bar-encryption-menu, diredp-menu-bar-images-menu,
;;            diredp-menu-bar-immediate-encryption-menu,
;;            diredp-(decrypt|verify|sign|encrypt)-this-file.
;;     Added diredp-(decrypt|verify|sign|encrypt)-this-file to *-immediate-encryption-menu.
;;     Moved encryption and image-dired items to the new Multiple submenus from Multiple menu.
;; 2013/07/15 dadams
;;     Added: diredp-async-shell-command-this-file, diredp-do-async-shell-command-recursive.
;;            Added them to menus.  Bind diredp-do-async-shell-command-recursive to M-+ &.
;;     diredp-menu-bar-mark-menu, diredp-dired-plus-description: Added dired-mark-omitted.
;;     diredp-menu-bar-subdir-menu: Added dired-omit-mode, dired-hide-details-mode.
;;     diredp-menu-bar-regexp-menu: Added image-dired-mark-tagged-files.
;;     diredp-menu-bar-subdir-menu: Added dired-hide-details-mode.
;;     diredp-shell-command-this-file: Corrected: provide file list to dired-do-shell-command.
;; 2013/07/13 dadams
;;     diredp-font-lock-keywords-1:
;;       Ensure diredp-dir-priv is not used for directory header of d:/... (Windows drive name).
;;     dired-insert-directory:
;;       Update wrt Emacs 24.4: Do dired-insert-set-properties last; use saved CONTENT-POINT.
;;     dired-insert-set-properties: Updated for Emacs 24.4, for dired-hide-details-mode.
;;     Add frame-fitting to dired-hide-details-mode-hook.
;;     dired-mouse-find-file(-other-window): Error msg if click off a file name.
;; 2013/07/12 dadams
;;     Added: diredp-wrap-around-flag, diredp-(next|previous)-(subdir|(dir)line).
;;     Renamed dired-up-directory to diredp-up-directory.
;;     Replaced vanilla commands by these new commands everywhere.
;; 2013/07/11 dadams
;;     Added: diredp-up-directory-reuse-dir-buffer.
;;     diredp-make-find-file-keys(-not)-reuse-dirs: Added diredp-up-directory-reuse-dir-buffer.
;; 2013/02/06 dadams
;;     dired-mark-pop-up: goto point-min, so show start of file list.  Thx to Michael Heerdegen.
;; 2013/01/28 dadams
;;     Added redefinition of dired-do-run-mail.  Fixes Emacs bug #13561.
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
;;     diredp-dired-inserted-subdirs: Do dired-(remember-marks|mark-remembered) in this-buff.
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
;;              Bound to M-+ M-b, M-+ C-M-B (aka C-M-S-b), M-+ C-M-b, respectively.  Added to menus.
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
;;     diredp-menu-bar-subdir-menu: Add image-dired-dired-toggle-marked-thumbs.
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
;;            diredp-do-bookmark-in-bookmark-file (C-M-B, aka C-M-S-b), diredp-read-bookmark-file-args.
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
(require 'dired-x) ;; dired-do-relsymlink
(require 'autofit-frame nil t) ;; (no error if not found) fit-frame-if-one-window
(require 'bookmark+ nil t) ;; (no error if not found)
 ;; bmkp-autofile-add-tags, bmkp-autofile-remove-tags, bmkp-autofile-set, bmkp-copied-tags,
 ;; bmkp-current-bookmark-file, bmkp-describe-bookmark, bmkp-empty-file, bmkp-get-autofile-bookmark,
 ;; bmkp-get-bookmark-in-alist, bmkp-get-tags, bmkp-read-tag-completing,
 ;; bmkp-read-tags-completing, bmkp-refresh/rebuild-menu-list, bmkp-remove-all-tags,
 ;; bmkp-same-file-p, bmkp-set-bookmark-file-bookmark, bmkp-set-sequence-bookmark,
 ;; bmkp-set-tag-value, bmkp-some, bmkp-switch-bookmark-file, bmkp-tag-name

;; For now at least, `highlight.el' is needed only if you use `bookmark+.el'.
(when (featurep 'bookmark+) (require 'highlight nil t)) ;; (no error if not found):
 ;; hlt-highlight-region

(if (> emacs-major-version 21) (require 'help-fns+ nil t) (require 'help+20 nil t))  ;; (no error if not found):
 ;; describe-file

(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(require 'image-file nil t) ;; (no error if not found): image-file-name-regexp
(require 'image-dired nil t) ;; (no error if not found):
 ;; image-dired-create-thumb, image-dired-create-thumbnail-buffer,
 ;; image-dired-dired-after-readin-hook, image-dired-delete-tag, image-dired-dired-comment-files,
 ;; image-dired-dired-display-external, image-dired-dired-display-image,
 ;; image-dired-display-thumbs, image-dired-get-comment, image-dired-get-exif-file-name,
 ;; image-dired-get-thumbnail-image, image-dired-insert-thumbnail, image-dired-line-up,
 ;; image-dired-line-up-dynamic, image-dired-line-up-interactive, image-dired-line-up-method,
 ;; image-dired-list-tags, image-dired-main-image-directory, image-dired-mark-tagged-files,
 ;; image-dired-read-comment, image-dired-remove-tag, image-dired-save-information-from-widgets,
 ;; image-dired-tag-files, image-dired-thumb-height, image-dired-thumbnail-buffer,
 ;; image-dired-thumb-name, image-dired-thumb-size, image-dired-thumb-width,
 ;; image-dired-widget-list, image-dired-write-comments, image-dired-write-tags
(when (memq system-type '(windows-nt ms-dos))
  ;; (no error if not found):
  (require 'w32-browser nil t));; dired-w32explore, dired-w32-browser, dired-mouse-w32-browser,
                               ;; dired-multiple-w32-browser
(when (< emacs-major-version 21) (require 'subr-21)) ;; replace-regexp-in-string, add-to-list

;; Provide macro for code byte-compiled using Emacs < 22.
(eval-when-compile
  (when (< emacs-major-version 22)
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
             (remove-hook 'minibuffer-setup-hook ,hook))))))
  )

;; Provide macro for code byte-compiled using Emacs < 23.
(eval-and-compile
  (when (< emacs-major-version 23)


    ;; REPLACE ORIGINAL in `subr.el'.  Needed for byte-compiling.
    ;; Almost the same as vanilla definition.
    ;; The only difference is to use `set-buffer-modified-p' if `restore-buffer-modified-p' is  not defined.
    (defmacro with-silent-modifications (&rest body)
      "Execute BODY, pretending it does not modify the buffer.
This macro is typically used around modifications of
text properties that do not really affect the buffer's content.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.

This macro will run BODY normally, but doesn't count its buffer
modifications as being buffer modifications.  This affects things
like `buffer-modified-p', checking whether the file is locked by
someone else, running buffer modification hooks, and other things
of that nature."
      ;; (declare (debug t) (indent 0))
      (let ((modified  (make-symbol "modified")))
        `(let* ((,modified                   (buffer-modified-p))
                (buffer-undo-list            t)
                (inhibit-read-only           t)
                (inhibit-modification-hooks  t))
           (unwind-protect
               (progn ,@body)
             (unless ,modified
               (if (fboundp 'restore-buffer-modified-p)
                   (restore-buffer-modified-p nil)
                 (set-buffer-modified-p nil)))))))))

(defmacro diredp-user-error (&rest args)
  `(if (fboundp 'user-error) (user-error ,@args) (error ,@args)))

;; Define these for Emacs 20 and 21.
(unless (fboundp 'dired-get-file-for-visit) ; Emacs 22+
  (defun dired-get-file-for-visit ()    ; Not bound
    "Get the current line's file name, with an error if file does not exist."
    (interactive)
    (let ((raw  (dired-get-filename nil 'NO-ERROR)) ; No error for `.' and `..'.
          file-name)
      (unless raw (error "No file on this line"))
      (setq file-name  (file-name-sans-versions raw t))
      (if (file-exists-p file-name)
          file-name
        (if (file-symlink-p file-name)
            (error "File is a symlink to a nonexistent target")
          (error "File no longer exists; type `g' to update Dired buffer")))))

  (defun dired-find-alternate-file ()   ; Not bound
    "In Dired, visit this file or directory instead of the Dired buffer."
    (interactive)
    (set-buffer-modified-p nil)
    (find-alternate-file (dired-get-file-for-visit))))

(defun diredp-make-obsolete (obsolete-name current-name &optional when)
  "Same as `make-obsolete', but usable also prior to Emacs 23."
  (if (< emacs-major-version 23)
      (make-obsolete obsolete-name current-name)
    (make-obsolete obsolete-name current-name when)))

(defun diredp-make-obsolete-variable (obsolete-name current-name &optional when access-type)
  "Same as `make-obsolete-variable', but usable also prior to Emacs 23."
  (if (< emacs-major-version 23)
      (make-obsolete-variable obsolete-name current-name)
    (make-obsolete-variable obsolete-name current-name when)))


;;;;;;;;;;;;;;;;;;;;;;;


(provide 'dired+)
(require 'dired+)                       ; Ensure loaded before compile this.

;; Quiet the byte-compiler.
(defvar bmkp-copied-tags)                         ; In `bookmark+-1.el'
(defvar bmkp-current-bookmark-file)               ; In `bookmark+-1.el'
(defvar bookmark-default-file)                    ; In `bookmark.el'
(defvar compilation-current-error)                ; In `compile.el'
(defvar delete-by-moving-to-trash)                ; Built-in, Emacs 23+
(defvar dired-always-read-filesystem)             ; In `dired.el', Emacs 26+
(defvar dired-auto-revert-buffer)                 ; In `dired.el', Emacs 23+
(defvar dired-create-files-failures)              ; In `dired-aux.el', Emacs 22+
(defvar dired-details-state)                      ; In `dired-details+.el'
(defvar dired-keep-marker-hardlink)               ; In `dired-x.el'
(defvar dired-omit-mode)                          ; In `dired.el', Emacs 22+
(defvar dired-overwrite-confirmed)                ; In `dired-aux.el'
(defvar dired-query-alist)                        ; In `dired-aux.el', Emacs < 24
(defvar dired-recursive-copies)                   ; In `dired-aux.el', Emacs 22+
(defvar dired-recursive-deletes)                  ; In `dired.el', Emacs 22+
;; (defvar dired-shrink-to-fit)                      ; In `dired.el'
(defvar dired-switches-alist)                     ; In `dired.el'
(defvar dired-subdir-switches)                    ; In `dired.el'
(defvar dired-touch-program)                      ; Emacs 22+
(defvar dired-use-ls-dired)                       ; Emacs 22+
(defvar diredp-count-.-and-..-flag)               ; Here, Emacs 22+
(defvar diredp-hide-details-initially-flag)       ; Here, Emacs 24.4+
(defvar diredp-hide-details-last-state)           ; Here, Emacs 24.4+
(defvar diredp-hide-details-propagate-flag)       ; Here, Emacs 24.4+
(defvar diredp-hide-details-toggled)              ; Here, Emacs 24.4+
(defvar diredp-highlight-autofiles-mode)          ; Here, Emacs 22+
(defvar diredp-menu-bar-encryption-menu)          ; Here, Emacs 23+
(defvar diredp-menu-bar-images-recursive-menu)    ; Here (old name)
(defvar diredp-menu-bar-regexp-recursive-menu)    ; Here (old name)
(defvar diredp-menu-bar-subdir-menu)              ; Here (old name)
(defvar diredp-move-file-dirs)                    ; Here, Emacs 24+
(defvar diredp-single-bookmarks-menu)             ; Here, if Bookmark+ is available
(defvar filesets-data)                            ; In `filesets.el'
(defvar font-lock-mode)                           ; In `fontcore.el'
(defvar grep-command)                             ; In `grep.el'
(defvar grep-use-null-device)                     ; In `grep.el'
(defvar header-line-format)                       ; Emacs 22+
(defvar icicle-candidate-alt-action-fn)           ; In `icicles-var.el'
(defvar icicle-default-value)                     ; In `icicles-opt.el'
(defvar icicle-file-extras)                       ; In `icicles-opt.el'
(defvar icicle-file-match-regexp)                 ; In `icicles-opt.el'
(defvar icicle-file-no-match-regexp)              ; In `icicles-opt.el'
(defvar icicle-file-predicate)                    ; In `icicles-opt.el'
(defvar icicle-file-require-match-flag)           ; In `icicles-opt.el'
(defvar icicle-file-sort)                         ; In `icicles-opt.el'
;; $$$$ (defvar icicle-file-sort-first-time-p)      ; In `icicles-var.el'
(defvar icicle-files-ido-like-flag)               ; In `icicles-opt.el'
(defvar icicle-ignored-directories)               ; In `icicles-opt.el'
(defvar icicle-show-Completions-initially-flag)   ; In`icicles-opt.el'
(defvar icicle-sort-comparer)                     ; In `icicles-opt.el'
(defvar icicle-sort-orders-alist)                 ; In `icicles-opt.el'
(defvar icicle-top-level-when-sole-completion-flag) ; In `icicles-opt.el'
(defvar image-dired-display-image-buffer)         ; In `image-dired.el'
(defvar image-dired-line-up-method)               ; In `image-dired.el'
(defvar image-dired-main-image-directory)         ; In `image-dired.el'
(defvar image-dired-thumbnail-buffer)             ; In `image-dired.el'
(defvar image-dired-thumb-height)                 ; In `image-dired.el'
(defvar image-dired-thumb-width)                  ; In `image-dired.el'
(defvar image-dired-widget-list)                  ; In `image-dired.el'
(defvar ls-lisp-use-insert-directory-program)     ; In `ls-lisp.el'
(defvar minibuffer-default-add-function)          ; In `simple.el', Emacs 23+
(defvar mouse3-dired-function)                    ; In `mouse3.el'
(defvar pp-read-expression-map)                   ; In `pp+.el'
(defvar read-file-name-completion-ignore-case)    ; In `minibuffer.el', Emacs 23+.  In C code, Emacs 22.
(defvar recentf-list)                             ; In `recentf.el'
;; Really a function, not a var - this quiets Emacs 20 byte-compiler, which doesn't recognize `declare-function'.
;; (defvar rgrep-find-ignored-directories)
(defvar save-some-buffers-action-alist)           ; In `files.el'
(defvar switch-to-buffer-preserve-window-point)   ; In `window.el', Emacs 24+
(defvar tab-line-exclude)                         ; In `tab-line.el', Emacs 27+
(defvar tar-subfile-mode)                         ; In `tar-mode.el'.
(defvar tar-superior-buffer)                      ; In `tar-mode.el'.
(defvar tooltip-mode)                             ; In `tooltip.el'
(defvar vc-directory-exclusion-list)              ; In `vc'
(defvar w32-browser-wait-time)                    ; In `w32-browser.el'
(defvar widget-keymap)                            ; In `wid-edit.el'

;;;;;;;;;;;;;;;;;;;;;;;

(defgroup Dired-Plus nil
  "Various enhancements to Dired."
  :prefix "diredp-" :group 'dired
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
dired+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/dired%2b.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/DiredPlus")
  :link '(emacs-commentary-link :tag "Commentary" "dired+"))
 
;;; Variables

;; `dired-do-toggle' was renamed to `dired-toggle-marks' after Emacs 20.
(unless (fboundp 'dired-toggle-marks) (defalias 'dired-toggle-marks 'dired-do-toggle))

;;; This is duplicated in `diff.el' and `vc.el'.
;;;###autoload
(defcustom diff-switches "-c"
  "*A string or list of strings specifying switches to be passed to diff."
  :type '(choice string (repeat string))
  :group 'dired :group 'diff)

;;;###autoload
(defcustom diredp-auto-focus-frame-for-thumbnail-tooltip-flag nil
  "*Non-nil means automatically focus the frame for a thumbnail tooltip.
If nil then you will not see a thumbnail image tooltip when you
mouseover an image-file name in Dired, unless you first give the frame
the input focus (e.g., by clicking its title bar).

This option has no effect if `diredp-image-preview-in-tooltip' is nil.
It also has no effect for Emacs versions prior to Emacs 22."
  :type 'boolean :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-bind-problematic-terminal-keys t
  "*Non-nil means bind some keys that might not work in a text-only terminal.
This applies to keys that use modifiers Meta and Shift together.
If you use Emacs in text-only terminal and your terminal does not
support the use of such keys then customize this option to nil."
  :type 'boolean :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-case-fold-search (if (boundp 'recentf-case-fold-search)
                                       recentf-case-fold-search
                                     (memq system-type '(windows-nt cygwin)))
  "Non-nil means some Dired+ functions ignore case.
The doc of such functions calls out the use of this option."
  :type '(choice
          (restricted-sexp :tag "Ignore case" :match-alternatives (identity)
                           :value (memq system-type '(windows-nt cygwin)))
          (const :tag "Respect case" nil))
  :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-compressed-extensions '(".tar" ".taz" ".tgz" ".arj" ".lzh"
                                          ".lzma" ".xz" ".zip" ".z" ".Z" ".gz" ".bz2" ".rar" ".rev")
  "*List of compressed-file extensions, for highlighting.

Note: If you change the value of this option then you need to restart
Emacs to see the effect of the new value on font-locking."
  :type '(repeat string) :group 'Dired-Plus)

(when (> emacs-major-version 21)        ; Emacs 22+
  (defcustom diredp-count-.-and-..-flag nil
    "Non-nil means count `.' and `..' when counting files for mode-line."
    :type 'boolean :group 'Dired-Plus))

;;;###autoload
(defcustom diredp-default-sort-arbitrary-function nil
  "Sorting used by default for sorting recent-files or dirs
You can re-sort the buffer using \\<dired-mode-map>`\\[diredp-sort-arbitrary-command]'."
  :type '(choice
          (const :tag "Access order, most recent first"   nil)
          (const :tag "Access order, least recent first"  t)
          (function :tag "File name, A-Z"          :value diredp-nondir-file-name-less-p)
          (function :tag "File name, Z-A"          :value diredp-nondir-file-name-more-p)
          (function :tag "Full file name, A-Z"     :value diredp-full-file-name-less-p)
          (function :tag "Full file name, Z-A"     :value diredp-full-file-name-more-p))
  :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-do-report-echo-limit 5
  "Echo result for each file, for fewer than this many files.
If more than this many files are acted on then there is no echoing.

Used by some commands, such as `diredp-do-apply-to-marked'.  Results
that are not echoed are anyway reported by `dired-log', so you can
show them with `?' in the Dired buffer."
  :type '(restricted-sexp :match-alternatives (wholenump)) :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-dwim-any-frame-flag pop-up-frames
  "*Non-nil means the target directory can be in a window in another frame.
Only visible frames are considered.
This is used by ``dired-dwim-target-directory'.
This option has no effect for Emacs versions before Emacs 22."
  :type 'boolean :group 'Dired-Plus)

(when (fboundp 'dired-hide-details-mode) ; Emacs 24.4+
  (defcustom diredp-hide-details-initially-flag t
    "*Non-nil means hide details in Dired from the outset."
    :type 'boolean :group 'Dired-Plus
    :set (lambda (sym defs)
           (custom-set-default sym defs)
           (setq diredp-hide-details-last-state  diredp-hide-details-initially-flag)))

  (defcustom diredp-hide-details-propagate-flag t
    "*Non-nil means display the next Dired buffer the same way as the last.
The last `dired-hide-details-mode' value set is used by the next Dired
buffer created."
    :type 'boolean :group 'Dired-Plus))

;; Emacs 20 only.
;;;###autoload
(unless (fboundp 'define-minor-mode)
  (defcustom diredp-highlight-autofiles-mode t
    "*Non-nil means highlight names of files that are autofile bookmarks.
Autofiles that have tags are highlighted using face
`diredp-tagged-autofile-name'.  Those with no tags are highlighted
using face `diredp-autofile-name'.

Setting this option directly does not take effect; use either
\\[customize] or command `diredp-highlight-autofiles-mode'.

NOTE: When `dired+.el' is loaded (for the first time per Emacs
session), the highlighting is turned ON, regardless of the option
value.  To prevent this and have the highlighting OFF by default, you
must do one of the following:

 * Put (diredp-highlight-autofiles-mode -1) in your init file, AFTER
   it loads `dired+.el'.

 * Customize the option to `nil', AND ensure that your `custom-file'
   (or the `custom-saved-variables' part of your init file) is
   evaluated before `dired+.el' is loaded.

This option has no effect unless you use libraries `Bookmark and
`highlight.el'."
    :set        (lambda (symbol value) (diredp-highlight-autofiles-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'Dired-Plus :require 'dired+))

;;;###autoload
(defcustom diredp-ignore-compressed-flag t
  "*Non-nil means to font-lock names of compressed files as ignored files.
This applies to filenames whose extensions are in
`diredp-compressed-extensions'.  If nil they are highlighted using
face `diredp-compressed-file-name'.

Note: If you change the value of this option then you need to restart
Emacs to see the effect of the new value on font-locking."
  :type 'boolean :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-image-preview-in-tooltip (or (and (boundp 'image-dired-thumb-size)  image-dired-thumb-size)
                                               100)
  "*Whether and what kind of image preview to show in a tooltip.
The possible values are:

 `nil'       : do not show a tooltip preview
 integer N>0 : show a thumbnail preview of that size
 `full'      : show a full-size preview of the image

To enable tooltip image preview you must turn on `tooltip-mode' and
load library `image-dired.el'.  See also option
`diredp-auto-focus-frame-for-thumbnail-tooltip-flag'.

This option has no effect for Emacs versions prior to Emacs 22."
  :type '(choice
          (restricted-sexp :tag "Show a thumnail image of size"
           :match-alternatives ((lambda (x) (and (wholenump x)  (not (zerop x))))))
          (const :tag "Show a full-size image preview"      full)
          (const :tag "OFF: Do not show an image preview"   nil))
  :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-image-show-this-file-use-frame-flag t
  "Non-nil means `diredp-image-show-this-file' uses another frame.
If nil then it uses another window.  Using another frame means you
have more control over the image size when you use a prefix arg.

If it uses another window then the prefix arg controls only the
minimum window height, not necessarily the image scale (height).

\(If the buffer displaying the image is already considered a
special-display buffer by your Emacs setup, then a nil value of this
option has no effect.)"
  :type 'boolean :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-list-file-attributes (list 5 8)
  "Which file attributes `diredp-list-file' uses, and when.
A list of file attribute numbers means use only the values of those
attributes.
A non-list means use all attribute values."
  :group 'Dired-Plus :type '(choice
                             (repeat (integer :tag "Use attribute with number"))
                             (const :tag "Use all attributes" all)))

;;;###autoload
(defcustom diredp-max-frames 200
  "*Max number of frames, for commands that find files in separate frames.
These commands are `dired-do-find-marked-files' and
`diredp-do-find-marked-files-recursive'.  See their descriptions for
the circumstances in which they show the files in separate frames."
  :type '(restricted-sexp :match-alternatives ((lambda (x) (and (wholenump x)  (not (zerop x))))))
  :group 'Dired-Plus)

(when (fboundp 'file-equal-p)           ; Emacs 24+
  (defcustom diredp-move-file-dirs ()
    "Alist of names of files and preferred directories to move them to.
File names should be relative (no directory component).
Target directory names should be absolute."
    :group 'files :type '(alist :key-type file :value-type directory)))

;; (Not used - just use the body directly in the option default value.
;; (defun diredp-omit-files-font-lock-regexp ()
;;   "Return regexp to use for font-locking, using `dired-omit-files' as base."
;;   (let* ((strg  dired-omit-files)
;;          (strg  (if (eq ?^ (aref strg 0)) (substring strg 1) strg)) ; Remove initial ^
;;          (strg  (replace-regexp-in-string "\\(\\\\[|]\\)\\^" "\\1" strg 'FIXEDCASE nil)) ; Remove other ^'s
;;          (strg  (replace-regexp-in-string "\\([$]\\)" "" strg 'FIXEDCASE nil))) ; Remove $'s
;;     strg))

(diredp-make-obsolete-variable 'diredp-omit-files-regexp 'diredp-omit-files-font-lock-regexp "2021-03-03")
(defcustom diredp-omit-files-font-lock-regexp "\\.?#.*#\\|\\.\\|\\.\\."
  ;;
  ;; This was no good for `#...#'.  There's no rule good enough to work generally.
  ;; Can't just transform `dired-omit-files'.
  ;;
  ;; (let* ((strg  dired-omit-files)
  ;;        (strg  (if (eq ?^ (aref strg 0)) ; Remove initial ^
  ;;                   (substring strg 1)
  ;;                 strg))
  ;;        (strg  (replace-regexp-in-string "\\(\\\\[|]\\)\\^" ; Remove other ^'s
  ;;                                         "\\1"
  ;;                                         strg
  ;;                                         'FIXEDCASE
  ;;                                         nil))
  ;;        (strg  (replace-regexp-in-string "\\([$]\\)" ; Remove $'s
  ;;                                         ""
  ;;                                         strg
  ;;                                         'FIXEDCASE
  ;;                                         nil)))
  ;;   strg)
  "Regexp for font-locking file names to be omitted by `dired-omit-mode'.
The regexp is matched only against the file name, but the entire line
is highlighted (with face `diredp-omit-file-name').

The default value corresponds to the default value of option
`dired-omit-files': auto-save files, `.', and `..'.

The option value is wrapped with `^.*' and `[*]?$', to match the full
line, including a possible executable tag, `*', at the end.  The
option value should not, therefore, itself contain ^, $, \\`, or \\'.

This option is for font-locking, whereas `dired-omit-files' is used
only for recognizing a file name, to act on the file.  The latter does
not need to match the whole (nondirectory) file name, so its default
value includes, for example, just \\.?#, not \\.?#.*#.  Also, the
latter wraps ^ and $ around the regexp.

Note: If you change the value of this option then you need to restart
Emacs to see the effect of the new value on font-locking."
  :group 'Dired-Plus :type 'regexp)

;;;###autoload
(defcustom diredp-omit-lines-regexp nil
  "Regexp matching lines to be omitted by `dired-omit-mode'.
The value can also be a variable whose value is such a regexp.
The value can also be nil, which means do no line matching.

See command `dired-omit-mode' (\\[dired-omit-mode]).

Some predefined regexp variables for Dired, which you can use as the
option value:

* `dired-re-inode-size'
* `dired-re-mark'
* `dired-re-maybe-mark'
* `dired-re-dir'
* `dired-re-sym'
* `dired-re-exe'
* `dired-re-perms'
* `dired-re-dot'
* `dired-re-no-dot'
* `diredp-re-no-dot'"
  :type `(choice (const  :tag "Do not match lines to omit" nil)
                 (regexp :tag "Regexp to match lines to omit (default omits executables)" :value ,dired-re-exe)
                 (restricted-sexp :tag "Variable with regexp value (default: `dired-re-exe')"
                                  :match-alternatives ((lambda (obj) (and (symbolp obj) (boundp obj))))
                                  :value dired-re-exe))
  :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-prompt-for-bookmark-prefix-flag nil
  "*Non-nil means prompt for a prefix string for bookmark names."
  :type 'boolean :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-visit-ignore-regexps ()
  "Regexps matching file names for `diredp-visit-(next|previous)' to skip.
A file or directory name matching one of these regexps is skipped,
along with those with an extension in `diredp-visit-ignore-extensions'."
  :type '(repeat regexp) :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-switches-in-mode-line nil
  "How to indicate `dired-actual-switches' in mode-line.
Possible values:
 * `nil':    Indicate name-or-date sort order, if possible.
             Else show full switches.
 * `as-is':  Show full switches.
 * Integer:  Show only the first N chars of full switches.
 * Function: Pass `dired-actual-switches' as arg and show result."
  :group 'Dired-Plus
  :type '(choice
          (const    :tag "Indicate by name or date, else full"   nil)
          (const    :tag "Show full switches"                    as-is)
          (integer  :tag "Show first N chars of switches" :value 10)
          (function :tag "Format with function"           :value identity)))

;;;###autoload
(defcustom diredp-toggle-dot+dot-dot-flag t
  "Non-nil means `dired-toggle-marks' acts also on `.' and `..'."
  :type 'boolean :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-visit-ignore-extensions '("elc")
  "Extensions of file names for `diredp-visit-(next|previous)' to skip.
A file name with one of these extensions is skipped, along with those
matching a regexp in `diredp-visit-ignore-regexps'."
  :type '(repeat string) :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-w32-local-drives '(("C:" "Local disk"))
  "*Local MS Windows drives that you want to use for `diredp-w32-drives'.
Each entry is a list (DRIVE DESCRIPTION), where DRIVE is the drive
name and DESCRIPTION describes DRIVE."
  :type '(alist
          :key-type   (string        :tag "Drive name")
          :value-type (group (string :tag "Drive description")))
  :group 'Dired-Plus)

;;;###autoload
(defcustom diredp-wrap-around-flag t
  "*Non-nil means Dired \"next\" commands wrap around to buffer beginning."
  :type 'boolean :group 'Dired-Plus)

(when (fboundp 'dired-hide-details-mode) ; Emacs 24.4+
  (defvar diredp-hide-details-last-state diredp-hide-details-initially-flag
    "Last `dired-hide-details-mode' value.
Initialized to the value of option `diredp-hide-details-initially-flag'.")

  (defvar diredp-hide-details-toggled nil
    "Non-nil means you have already toggled hiding details in this buffer.")
  (make-variable-buffer-local 'diredp-hide-details-toggled))

(defvar diredp-dir-sep-replacement "|"
  "String to replace char `/' in constructed Dired buffer names.")

(defun diredp-replace-dir-sep-in-string (string)
  "Replace `/` chars in STRING with `diredp-dir-sep-replacement'."
  (replace-regexp-in-string "/" diredp-dir-sep-replacement string nil 'LITERAL))

;; 2020-12-05.
;;
;; This was previously "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*".  Changed to be the same as
;; `directory-files-no-dot-files-regexp' in `files.el', available for Emacs 23+, but changed in
;; Emacs 27+ to this value.  This value is a bit quicker than the previous value, and it can
;; match file names containing ^J.
;;
;; This value is OK, as long as no use is made of the match data.  In particular, as long as
;; the match data is not used to capture the file name.  That's the case (it's OK) for use by
;; `directory-files', which is currently the only use here.
;;
;; See https://lists.gnu.org/archive/html/emacs-devel/2020-04/msg00764.html, msg01247, msg01305
;;
;; Same as the default value of `icicle-re-no-dot', in `icicles-var.el'.
;;
(defvar diredp-re-no-dot "[^.]\\|\\.\\.\\."
  "Regexp that matches any nonempty file name except `.' and `..'.
Default value is same as `directory-files-no-dot-files-regexp'.")

(defvar diredp-recent-files-buffer nil
  "Non-nil means this buffer is a Dired listing of recent files or dirs.")
(make-variable-buffer-local 'diredp-recent-files-buffer)

(defvar diredp-w32-drives-mode-map (let ((map  (make-sparse-keymap)))
                                     (define-key map "q"       'bury-buffer)
                                     (define-key map "\r"      'widget-button-press)
                                     (define-key map [mouse-2] 'widget-button-click)
                                     map)
  "Keymap for `diredp-w32-drives-mode'.")

(unless (boundp 'directory-listing-before-filename-regexp) ; Emacs 20 (doesn't have `defvaralias').
  (defvar directory-listing-before-filename-regexp dired-move-to-filename-regexp
    "Regular expression to match up to the file name in a directory listing.
The default value is designed to recognize dates and times
regardless of the language."))
 
;;; Macros


;; Unlike `dired-mark-if':
;;
;; 1. Value returned and message indicate both the number matched and the number changed.
;; 2. Added optional arg PLURAL, for irregular plurals (e.g. "directories").
;;
(defmacro diredp-mark-if (predicate singular &optional plural)
  "Mark files for PREDICATE, according to `dired-marker-char'.
PREDICATE is evaluated on each line, with point at beginning of line.
SINGULAR is a singular noun phrase for the type of files being marked.
Optional arg PLURAL is a plural noun phrase for the type of files
 being marked.
If PLURAL is nil then SINGULAR should end with a noun that can be
pluralized by adding `s'.

Return nil if no files matched PREDICATE.
Otherwise return a cons (CHANGED . MATCHED), where:
 CHANGED is the number of markings that were changed by the operation.
 MATCHED is the number of files that matched PREDICATE."
  `(let ((inhibit-read-only  t)
         changed matched)
    (save-excursion
      (setq matched  0
            changed  0)
      (when ,singular (message "%s %s%s..."
                               (cond ((eq dired-marker-char ?\040)            "Unmarking")
                                     ((eq dired-del-marker dired-marker-char) "Flagging")
                                     (t                                       "Marking"))
                               (or ,plural  (concat ,singular "s"))
                               (if (eq dired-del-marker dired-marker-char) " for deletion" "")))
      (goto-char (point-min))
      (while (not (eobp))
        (when ,predicate
          (setq matched  (1+ matched))
          (unless (eq dired-marker-char (char-after))
            (delete-char 1) (insert dired-marker-char) (setq changed  (1+ changed))))
        (forward-line 1))
      (when ,singular (message "%s %s%s%s newly %s%s"
                               matched
                               (if (= matched 1) ,singular (or ,plural  (concat ,singular "s")))
                               (if (not (= matched changed)) " matched, " "")
                               (if (not (= matched changed)) changed "")
                               (if (eq dired-marker-char ?\040) "un" "")
                               (if (eq dired-marker-char dired-del-marker) "flagged" "marked"))))
    (and (> matched 0)  (cons changed matched))))


;; Just a helper function for `dired-map-over-marks'.
(defun diredp-get-file-or-dir-name (arg &optional localp)
  "Return name of next file or directory or nil if none.
Argument ARG:
 `all-files-no-dirs' or nil means skip directories.
 `all-files-no-dots' means skip `.' and `..'.
Optional arg LOCALP as in `dired-get-filename'."
  (let* ((fname        nil)
         ;; With Emacs 20 `dired-get-filename' returns `./' and `../' instead of `./' and `../'.
         (emacs20      (< emacs-major-version 22))
         (dots=        (if emacs20 '("./" "../") '("." "..")))
         (dots-suffix  (if emacs20 "/\\.\\.?/$" "/\\.\\.?$")))
    (while (and (not fname)  (not (eobp)))
      (setq fname  (dired-get-filename localp 'NO-ERROR))
      (when (and fname  (or (not arg)  (eq arg 'all-files-no-dirs))  (file-directory-p fname))
        (setq fname  nil))
      (when (and fname  (eq arg 'all-files-no-dots)
                 (or (member fname dots=)
                     (diredp-string-match-p dots-suffix fname)))
        (setq fname  nil))
      (forward-line 1))
    (forward-line -1)
    fname))

(defun diredp-prefix-arg-all-files (arg)
  "Return symbol for kind of \"all-files\" behavior, or nil for none.
ARG is a raw prefix arg.
Return value depends on the number of plain `C-u' used:
 * nil                 if 0 or 1 `C-u'
 * `all-files-no-dirs' if 2 or more than 4
 * `all-files-no-dots' if 3
 * `all-files'         if 4"
  (and (consp arg)
       (> (prefix-numeric-value arg) 4)
       (case (prefix-numeric-value arg)
         (16   'all-files-no-dirs)      ; `C-u C-u'
         (64   'all-files-no-dots)      ; `C-u C-u C-u'
         (256  'all-files)              ; `C-u C-u C-u C-u'
         (t    'all-files-no-dirs))))


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
   Point is advanced accordingly.  This is so that commands for the
   next ARG (instead of the marked) files can be easily chained.
 If ARG is a cons with element 16, 64, or 256, corresponding to
   `C-u C-u', `C-u C-u C-u', or `C-u C-u C-u C-u', then use all files
   in the Dired buffer, where:
     16 includes NO directories (including `.' and `..')
     64 includes directories EXCEPT `.' and `..'
    256 includes ALL directories (including `.' and `..')
 If ARG is otherwise non-nil, use the current file.
If optional third arg SHOW-PROGRESS evaluates to non-nil,
 redisplay the Dired buffer after each file is processed.

No guarantee is made about the position on the marked line.  BODY must
ensure this itself, if it depends on this.

Search starts at the beginning of the buffer, thus the car of the list
corresponds to the line nearest the end of the buffer.  This is also
true for (positive and negative) integer values of ARG.

BODY should not be too long, because it is expanded four times.

If DISTINGUISH-ONE-MARKED is non-nil, then return (t FILENAME) instead
 of (FILENAME), if only one file is marked."
  ;; WARNING: BODY must not add new lines before point - this may cause an
  ;; endless loop.  This warning should not apply any longer, sk  2-Sep-1991 14:10.
  `(prog1
       (let ((inhibit-read-only  t)
             (multi-C-u          (diredp-prefix-arg-all-files ,arg))
             case-fold-search
             found
             results)
         (if (and ,arg  (not multi-C-u))
             (if (integerp ,arg)
                 (progn     ; No `save-excursion', want to move point.
                   (dired-repeat-over-lines ,arg #'(lambda ()
                                                    (when ,show-progress (sit-for 0))
                                                    (setq results  (cons ,body results))))
                   (if (< ,arg 0) (nreverse results) results))
               ;; Non-nil, non-integer ARG means use current file:
               (list ,body))
           (let ((regexp  (dired-marker-regexp))
                 next-position)
             (save-excursion
               (goto-char (point-min))
               ;; Remember position of next marked file before BODY can insert lines before the
               ;; just found file, confusing us by finding the same marked file again and again...
               (setq next-position  (and (if multi-C-u
                                             (diredp-get-file-or-dir-name multi-C-u)
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
                                               (diredp-get-file-or-dir-name multi-C-u)
                                             (re-search-forward regexp nil t))
                                           (point-marker)))))
             (when (and ,distinguish-one-marked  (= (length results) 1))
               (setq results  (cons t results)))
             (if found results (list ,body)))))
     ;; `save-excursion' loses, again
     (dired-move-to-filename)))

;; Same as `icicle-with-help-window' in `icicles-mac.el'
;; and `bmkp-with-help-window' in `bookmark+-mac.el'.
(defmacro diredp-with-help-window (buffer &rest body)
  "`with-help-window', if available; else `with-output-to-temp-buffer'."
  (if (fboundp 'with-help-window)
      `(with-help-window ,buffer ,@body)
    `(with-output-to-temp-buffer ,buffer ,@body)))

(put 'diredp-with-help-window 'common-lisp-indent-function '(4 &body))
 
;;; Utility functions

;; Same as `imenup-delete-if-not'.
;;
(defun diredp-delete-if-not (predicate xs)
  "Remove all elements of list XS that do not satisfy PREDICATE.
This operation is destructive, reusing conses of XS whenever possible."
  (while (and xs  (not (funcall predicate (car xs))))
    (setq xs  (cdr xs)))
  (let ((cl-p  xs))
    (while (cdr cl-p)
      (if (not (funcall predicate (cadr cl-p))) (setcdr cl-p (cddr cl-p)) (setq cl-p  (cdr cl-p)))))
  xs)

;; Same as `imenup-delete-if'.
;;
(defun diredp-delete-if (predicate xs)
  "Remove all elements of list XS that satisfy PREDICATE.
This operation is destructive, reusing conses of XS whenever possible."
  (while (and xs  (funcall predicate (car xs)))
    (setq xs  (cdr xs)))
  (let ((cl-p  xs))
    (while (cdr cl-p)
      (if (funcall predicate (cadr cl-p))
          (setcdr cl-p (cddr cl-p))
        (setq cl-p  (cdr cl-p)))))
  xs)

;; Define this to avoid requiring `cl.el' at runtime.  Same as `frcmds-set-difference' in `frame-cmds.el'.
(defun diredp-set-difference (list1 list2 &optional key)
  "Combine LIST1 and LIST2 using a set-difference operation.
Optional arg KEY is a function used to extract the part of each list
item to compare.

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

;; Same as `tap-string-match-p' in `thingatpt+.el'.
(if (fboundp 'string-match-p)
    (defalias 'diredp-string-match-p 'string-match-p) ; Emacs 23+
  (defun diredp-string-match-p (regexp string &optional start)
    "Like `string-match', but this saves and restores the match data."
    (save-match-data (string-match regexp string start))))

(if (fboundp 'looking-at-p)
    (defalias 'diredp-looking-at-p 'looking-at-p) ; Emacs 23+
  (defun diredp-looking-at-p (regexp)
    "Like `looking-at', but this saves and restores the match data."
    (save-match-data (looking-at regexp))))

;; `dired-read-regexp' does not accept DEFAULT and HISTORY for older Emacsen, so use this.
(defun diredp-read-regexp (prompt &optional default history)
  "Read a regexp.
HISTORY defaults to `dired-regexp-history'."
  (setq history  (or history 'dired-regexp-history))
  (if (fboundp 'read-regexp)
      (read-regexp prompt default history)
    (read-from-minibuffer prompt nil nil nil history default)))

(if (fboundp 'delete-dups)
    (defalias 'diredp-delete-dups 'delete-dups)
  (defun diredp-delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
        (setcdr tail (delete (car tail) (cdr tail)))
        (setq tail (cdr tail))))
    list))

(defun diredp-nonempty-region-p ()
  "Return non-nil if region is active and non-empty."
  (and transient-mark-mode  mark-active  (mark)  (> (region-end) (region-beginning))))

(defun diredp-get-image-filename (&optional localp no-error-if-not-filep)
  "Return the image-file name on this line, or nil if no image file.
If not in Dired (or a mode derived from Dired), then test the entire
text of the current line as the file name.

The optional args are the same as for `dired-get-filename'.  They are
ignored if not in a Dired mode.

\(Prior to Emacs 22, this function just returns nil.)"
  (let ((file  (if (derived-mode-p 'dired-mode)
                   (dired-get-filename localp no-error-if-not-filep)
                 ;; Make it work also for `diredp-list-files' listings.
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (and file
         (fboundp 'image-file-name-regexp) ; Emacs 22+, `image-file.el'.
         (diredp-string-match-p (image-file-name-regexp) file)
         file)))

(defun diredp-root-directory-p (file)
  "Return non-nil if FILE is a root directory."
  (if (fboundp 'ange-ftp-root-dir-p)
      (ange-ftp-root-dir-p (file-name-as-directory file))
    ;; This is essentially `ange-ftp-root-dir-p' applied to `file-name-as-directory'.
    ;; If `ange-ftp-root-dir-p' changes, update this code.
    (or (and (eq system-type 'windows-nt)  (diredp-string-match-p "\\`[a-zA-Z]:[/\\]\\'"
                                                                  (file-name-as-directory file)))
        (string= "/" file))))

(defun diredp-parent-dir (file &optional relativep)
  "Return the parent directory of FILE, or nil if none.
Optional arg RELATIVEP non-nil means return a relative name, that is,
just the parent component."
  (let ((parent  (file-name-directory (directory-file-name (expand-file-name file))))
        relparent)
    (when relativep (setq relparent  (file-name-nondirectory (directory-file-name parent))))
    (and (not (equal parent file))  (or relparent  parent))))

(unless (fboundp 'derived-mode-p)       ; Emacs 20, 21.
  (defun derived-mode-p (&rest modes)
    "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
    (let ((parent  major-mode))
      (while (and (not (memq parent modes))  (setq parent  (get parent 'derived-mode-parent))))
      parent)))

(defun diredp-ensure-mode ()
  "Raise an error if not in Dired or a mode derived from it."
  (unless (derived-mode-p 'dired-mode)
    (error "You must be in Dired or a mode derived from it to use this command")))

(defun diredp-ensure-fn-nonzero-arity (obj)
  "Raise an error if FN is not a function that can accept 1 argument."
  (unless (functionp obj) (error "Not a function: `%s'" obj))
  (unless (or (not (fboundp 'func-arity)) ; Emacs < 26
              (let ((max  (cdr (func-arity obj))))
                (or (eq max 'many)  (> max 0))))
    (error "Function `%s' cannot accept any args" obj)))

(defun diredp-ensure-fn-zero-arity (obj)
  "Raise an error if FN is not a function that can accept 0 arguments."
  (unless (functionp obj) (error "Not a function: `%s'" obj))
  (unless (or (not (fboundp 'func-arity)) ; Emacs < 26
              (= 0 (car (func-arity obj))))
    (error "Function `%s' cannot accept zero args" obj)))

(defun diredp-ensure-bookmark+ ()
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'")))

;; Same as `recentf-string-less-p'.
;;
(defun diredp-string-less-p (string1 string2)
  "Return non-nil if STRING1 is less than STRING2 in lexicographic order.
Ignore case if `diredp-case-fold-search' is non-nil."
  (if diredp-case-fold-search
      (string-lessp (downcase string1) (downcase string2))
    (string-lessp string1 string2)))

(defalias 'diredp-full-file-name-less-p 'diredp-string-less-p)

(defun diredp-full-file-name-more-p (file1 file2)
  "Return non-nil if full name of FILE1 comes after that of FILE2.
Comparison is lexicographic.
Ignore case if `diredp-case-fold-search' is non-nil."
  (diredp-string-less-p file2 file1))

(defun diredp-nondir-file-name-less-p (file1 file2)
  "Return non-nil if nondir name of FILE1 comes before that of FILE2.
Comparison is lexicographic.
Ignore case if `diredp-case-fold-search' is non-nil."
  (diredp-string-less-p (file-name-nondirectory file1) (file-name-nondirectory file2)))

(defun diredp-nondir-file-name-more-p (file1 file2)
  "Return non-nil if nondir name of FILE1 comes after that of FILE2.
Comparison is lexicographic.
Ignore case if `diredp-case-fold-search' is non-nil."
  (diredp-string-less-p (file-name-nondirectory file2) (file-name-nondirectory file1)))

(defun diredp-sort-arbitrary (function &optional arg)
  "Sort recent files or dirs with `diredp-default-sort-arbitrary-function'.
FUNCTION shoud be `diredp-recent-files' or `diredp-recent-dirs'.
ARG is as in `diredp-dired-recent-files'."
  (if diredp-default-sort-arbitrary-function
      (if (functionp diredp-default-sort-arbitrary-function)
          (sort (copy-sequence (funcall function arg)) diredp-default-sort-arbitrary-function)
        (nreverse (funcall function arg)))
    (funcall function arg)))
 

(unless (fboundp 'dired-nondirectory-p) ; Emacs 20, 21.
  (defun dired-nondirectory-p (file)
    "Return non-nil if FILE is not a directory."
    (not (file-directory-p file))))


;;; Some of the redefinitions that follow are essentially unaltered vanilla Emacs code to be
;;; reloaded, to use the new definition of `dired-map-over-marks' here.



;; REPLACE ORIGINAL in `dired.el'.
;;
;; Added optional arg LOCALP.
;;
(defun dired-remember-marks (beg end &optional localp)
  "Return alist of files and their marks, from BEG to END.
Non-nil LOCALP means save only the nondir part of the file names."
  (if (dired--find-hidden-pos (point-min) (point-max))
      (dired--unhide (point-min) (point-max))) ; Must unhide to make this work.
  (let (fil chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dired-re-mark end t)
	(if (setq fil  (dired-get-filename localp t))
	    (setq chr    (preceding-char)
		  alist  (cons (cons fil chr) alist)))))
    alist))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; File names in ALIST need not be absolute.  If not absolute they're expanded in `default-directory'.
;;
(defun dired-mark-remembered (alist)
  "Mark all files remembered in ALIST.
Each element of ALIST looks like (FILE . MARKERCHAR)."
  (let (elt file chr)
    (save-excursion
      (while alist
	(setq elt    (car alist)
	      alist  (cdr alist)
	      file   (car elt)
	      chr    (cdr elt))
        (unless (file-name-absolute-p file) (setq file (expand-file-name file)))
	(when (dired-goto-file file)
	  (beginning-of-line)
	  (delete-char 1)
	  (insert chr))))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Respect option `diredp-switches-in-mode-line'.
;;
(defun dired-sort-set-mode-line ()
  "Set mode-line according to option `diredp-switches-in-mode-line'."
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let ((case-fold-search  nil))
            (if diredp-switches-in-mode-line
                (concat "Dired"
                        (cond ((integerp diredp-switches-in-mode-line)
                               (let* ((l1  (length dired-actual-switches))
                                      (xs  (substring dired-actual-switches 0 (min l1 diredp-switches-in-mode-line)))
                                      (l2  (length xs)))
                                 (if (zerop l2)
                                     xs
                                   (concat " " xs (and (< l2  l1) "...")))))
                              ((functionp diredp-switches-in-mode-line)
                               (format " %s" (funcall diredp-switches-in-mode-line
                                                     dired-actual-switches)))
                              (t (concat " " dired-actual-switches))))
              (cond ((string-match-p dired-sort-by-name-regexp dired-actual-switches)
                     "Dired by name")
                    ((string-match-p dired-sort-by-date-regexp dired-actual-switches)
                     "Dired by date")
                    (t (concat "Dired " dired-actual-switches))))))
    (force-mode-line-update)))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Pass non-nil second arg to `dired-get-filename' so we can include `.' and `..'.
;; 2. Doc string is updated to reflect the new ARG behavior.
;; 3. Allow, unlike vanilla Emacs, use of FILTER and DISTINGUISH-ONE-MARKED together.
;;
(defun dired-get-marked-files (&optional localp arg filter distinguish-one-marked error-if-none-p)
  "Return names of the marked files and directories as a list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Values returned are normally absolute file names.
Optional arg LOCALP as in `dired-get-filename'.
Optional second argument ARG specifies files to use instead of marked.
 Usually ARG comes from the command's prefix arg.
 If ARG is an integer, use the next ARG files (previous -ARG, if < 0).
  (1 means file on current line.  -1 means file on previous line.)
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
 of (FILENAME) if only one file is marked (after any filtering by
 FILTER).
If ERROR-IF-NONE-P is non-nil, signal an error if the list of files is
 empty.  If ERROR-IF-NONE-P is a string then it is the error message.

Note that the Dired+ version of this function differs from the vanilla
version in these respects:

* There are more possibilities for argument ARG (prefix argument).
* Directories `.' and `..' can be included as marked.
* You can use arguments FILTER and DISTINGUISH-ONE-MARKED together."
  (let ((all  (delq nil (save-excursion (dired-map-over-marks (dired-get-filename localp 'NO-ERROR)
                                                              arg
                                                              nil
                                                              distinguish-one-marked))))
        result)
    (when (equal all '(t)) (setq all  nil)) ; Added by vanilla Emacs 24+.
    (if (and distinguish-one-marked  (eq (car all) t))
        (if (not filter)
            all
          (and (funcall filter (cadr all))  (list t (cadr all))))
      (dolist (file  all)
        (when (or (not filter)  (funcall filter file)) (push file result)))
      (when (and (null result)  error-if-none-p)
        (diredp-user-error (if (stringp error-if-none-p) error-if-none-p "No files specified")))
      result)))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; Zero prefix arg means create empty file, prompting for file name.
;;
;;;###autoload
(defun dired-do-touch (&optional arg)
  "Change the timestamp of the marked (or next ARG) files.
As an exception, if ARG is zero then create a single new (empty) file
instead.  You are prompted for the file name.

This invokes system program `touch'.

Use `M-n' to pull the file attributes of the file at point
into the minibuffer."
  (interactive "P")
  (if (zerop (prefix-numeric-value arg))
      (call-interactively #'diredp-create-file-here)
    (dired-do-chxxx "Timestamp" dired-touch-program 'touch arg)))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Added optional arg INIT-COUNT.
;; 2. Changed doc to speak of removing, not killing.
;;
(defun dired-do-kill-lines (&optional arg fmt init-count)
  "Remove all marked lines, or the next ARG lines.
The files or directories listed on those lines are _not_ deleted.
Only the Dired listing is affected.  To restore the removals, use \\<dired-mode-map>`\\[revert-buffer]'.

With a numeric prefix arg, remove that many lines going forward,
starting with the current line.  (A negative prefix arg removes lines
going backward.)

If you use a prefix arg to remove the line in a parent listing for a
subdir whose listing you have inserted into the Dired buffer, then
that inserted subdir listing is also removed.

To remove a subdir listing _without_ removing the subdir's line in its
parent listing, go to the header line of the subdir listing and use
this command with any prefix arg.

When called from Lisp, non-nil INIT-COUNT is added to the number of
lines removed by this invocation, for the reporting message."
  ;; Returns count of killed lines.  FMT="" suppresses message.
  (interactive "P")
  (if arg
      (if (dired-get-subdir) (dired-kill-subdir) (dired-kill-line arg))
    (save-excursion
      (goto-char (point-min))
      (let ((count   (or init-count  0))
            (regexp  (dired-marker-regexp))
            buffer-read-only)
        (while (and (not (eobp))  (re-search-forward regexp nil t))
          (setq count  (1+ count))
          (delete-region (line-beginning-position) (progn (forward-line 1) (point))))
        (unless (equal "" fmt) (message (or fmt "Killed %d line%s.") count (dired-plural-s count)))
        count))))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Define here to make use of my `dired-map-over-marks'.
;; 2. Added &rest arg FUN-ARGS.
;; 3. Added doc string.
;;
(defun dired-map-over-marks-check (fun mark-arg op-symbol &optional show-progress &rest fun-args)
  "Map FUN over marked lines and report failures.
FUN should return nil for success and non-nil (the offending object,
e.g. the short form of the filename) for a failure.  FUN can log a
detailed error explanation using `dired-log'.

MARK-ARG is as the second argument of `dired-map-over-marks'.

OP-SYMBOL is a symbol describing the operation performed (e.g.
`compress').  It is used with `dired-mark-pop-up' to prompt the user
\(e.g. with `Compress * [2 files]? ') and to display errors (e.g.
`Failed to compress 1 of 2 files - type ? for details (\"foo\")')

SHOW-PROGRESS if non-nil means redisplay Dired after each file.

FUN-ARGS is the list of any remaining args to
`dired-map-over-marks-check'.  Function FUN is applied to these
arguments.

To map a function whose return values you are interested in, that is,
a function that does not just return nil for success, use function
`diredp-map-over-marks-and-report'."
  (and (dired-mark-confirm op-symbol mark-arg)
       (let* ((results     (dired-map-over-marks (apply fun fun-args) mark-arg show-progress)) ; FUN return vals.
              (nb-results  (length results))
              (failures    (delq nil results))
              (nb-fail     (length failures))
              (op-strg     (if (eq op-symbol 'compress) "Compress or uncompress" (capitalize
                                                                                  (symbol-name op-symbol)))))
         (if (null failures)
             (if (fboundp 'ngettext)    ; Emacs 27+
                 (message (ngettext "%s: %d file." "%s: %d files." nb-results) op-strg nb-results)
               (message "%s: %d file%s." op-strg nb-results (dired-plural-s nb-results)))
           (dired-log-summary
            (if (fboundp 'ngettext)    ; Emacs 27+
                (format (ngettext "Failed to %s %d of %d file" "Failed to %s %d of %d files" nb-results)
                        (downcase op-strg) nb-fail nb-results)
              (format "Failed to %s %d of %d file%s"
                      (downcase op-strg) nb-fail nb-results (dired-plural-s nb-results)))
            failures)))))

(defun diredp-map-over-marks-and-report (fun mark-arg op-symbol &optional show-progress &rest fun-args)
  "Map FUN over marked lines and report the results.
Return the results as a list.

This is like `dired-map-over-marks-check', but use this if you are
interested in the values returned by FUN:
 * FUN can return anything.  There is no notion of failure, and
   returning nil does not indicate success.
 * FUN can log any information it wants using `dired-log'.
 * `diredp-report-summary' is called to display the log, instead of
   `dired-log-summary'.  (It is not used to report failure.)

MARK-ARG is as the second argument of `dired-map-over-marks'.

OP-SYMBOL is a symbol describing the operation performed (e.g.
`compress').  It is used with `dired-mark-pop-up' to prompt the user
\(e.g. with `Compress * [2 files]? ') and to display errors (e.g.
`Failed to compress 1 of 2 files - type ? to see why (\"foo\")')

SHOW-PROGRESS if non-nil means redisplay Dired after each file.

FUN-ARGS is the list of any remaining args to
`diredp-map-over-marks-and-report'.  Function FUN is applied to these
arguments."
  (and (dired-mark-confirm op-symbol mark-arg)
       (let* ((results     (dired-map-over-marks (apply fun fun-args) mark-arg show-progress)) ; FUN return vals.
              (nb-results  (length results))
              (op-strg     (capitalize (symbol-name op-symbol))))
         (diredp-report-summary (format "%s for %d file%s" op-strg nb-results (dired-plural-s nb-results)))
         results)))                     ; Return list of results.

(defun diredp-report-summary (string)
  "Report a summary for a command that maps over Dired lines.
Report in the echo area and display a log buffer."
  (message "%s--type ? for details" string)
  (dired-log (concat "\n" string "\n"))
  (dired-log t))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
(when (boundp 'dired-subdir-switches)   ; Emacs 22+
  (defun dired-do-redisplay (&optional arg test-for-subdir) ; Bound to `l'
    "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing.

Dired remembers switches specified with a prefix arg, so reverting the
buffer does not reset them.  However, you might sometimes need to
reset some subdirectory switches after using \\<dired-mode-map>`\\[dired-undo]'.  You can reset all
subdirectory switches to the default value using
`\\[dired-reset-subdir-switches]'.
See Info node `(emacs) Subdir switches' for more details."
    ;; Moves point if the next ARG files are redisplayed.
    (interactive "P\np")
    (if (and test-for-subdir  (dired-get-subdir))
        (let* ((dir       (dired-get-subdir))
               (switches  (cdr (assoc-string dir dired-switches-alist))))
          (dired-insert-subdir dir (and arg  (read-string "Switches for listing: "
                                                          (or switches
                                                              dired-subdir-switches
                                                              dired-actual-switches)))))
      (message "Redisplaying...")
      ;; `message' is much faster than making `dired-map-over-marks' show progress
      (dired-uncache (if (consp dired-directory) (car dired-directory) dired-directory))
      (dired-map-over-marks (let ((fname                    (dired-get-filename))
                                  ;; Postpone readin hook map over all marked files (Bug#6810).
                                  (dired-after-readin-hook  nil))
                              (message "Redisplaying... `%s'" fname)
                              (dired-update-file-line fname))
                            arg)
      (run-hooks 'dired-after-readin-hook)
      (dired-move-to-filename)
      (message "Redisplaying...done"))))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
(unless (boundp 'dired-subdir-switches) ; Emacs 20, 21
  (defun dired-do-redisplay (&optional arg test-for-subdir) ; Bound to `l'
    "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing."
    ;; Moves point if the next ARG files are redisplayed.
    (interactive "P\np")
    (if (and test-for-subdir  (dired-get-subdir))
        (dired-insert-subdir (dired-get-subdir)
                             (and arg  (read-string "Switches for listing: " dired-actual-switches)))
      (message "Redisplaying...")
      ;; `message' is much faster than making dired-map-over-marks show progress
      (dired-uncache (if (consp dired-directory) (car dired-directory) dired-directory))
      (dired-map-over-marks (let ((fname  (dired-get-filename)))
                              (message "Redisplaying... `%s'" fname)
                              (dired-update-file-line fname))
                            arg)
      (dired-move-to-filename)
      (message "Redisplaying...done"))))


;; REPLACE ORIGINAL in `dired.el'.
;;
(when (fboundp 'get-window-with-predicate) ; Emacs 22+
  (defun dired-dwim-target-directory ()
    "Guess a target directory to use for Dired.
If there is a Dired buffer displayed in another window, use its
current subdir, else use current subdir of this Dired buffer."
    (let ((this-dir  (and (eq major-mode 'dired-mode)  (dired-current-directory))))
      ;; Non-dired buffer may want to profit from this function, e.g. `vm-uudecode'.
      (if dired-dwim-target
          (let* ((other-win  (get-window-with-predicate (lambda (window)
                                                          (with-current-buffer (window-buffer window)
                                                            (eq major-mode 'dired-mode)))
                                                        nil
                                                        (and diredp-dwim-any-frame-flag  'visible)))
                 (other-dir  (and other-win  (with-current-buffer (window-buffer other-win)
                                               (and (eq major-mode 'dired-mode)  (dired-current-directory))))))
            (or other-dir  this-dir))
        this-dir))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Added optional args READ-EXTRA-FILES-P and DIRED-BUFFER.
;;
;; 2. Behavior for non-positive prefix arg:
;;
;;    * Construct a cons DIRNAME arg.
;;    * If READ-EXTRA-FILES-P is non-nil then read any number of file and dir names, to be included as its cdr.
;;    * If chosen Dired buffer exists and is an ordinary listing then start out with its `directory-files'.
;;
;; 2. If you use Icicles then this is a multi-command - see doc for `dired' defadvice.
;;
;;;###autoload
(defun dired-read-dir-and-switches (string &optional read-extra-files-p dired-buffer)
  "Read arguments for `dired' commands.
Add STRING to the prompt after \"Dired \".  If it is not \"\" then it
should end with a space.

Return a list of arguments for `dired': (DIRNAME SWITCHES).  DIRNAME
here has the same form as `dired-directory'.

With a non-negative prefix arg, read the `ls' switches.

With a non-positive prefix arg:

 * DIRNAME is a cons of the Dired buffer name (see below) and the list
   of file names.

 * With non-nil READ-EXTRA-FILES-P, read any number of directory or
   file names, to make up the Dired arbitrary-files listing (the cdr
   of DIRNAME).  Use `C-g' when done entering the files and
   directories to list.  You can use file-name wildcards (i.e., `*'
   for globbing), to include the matching files and directories.

Optional arg DIRED-BUFFER:

 * If non-nil then it should be a string.  Use it as the Dired buffer
   name.  Because `dired-readin' tries to use it as a directory name,
   all `/' chars in arg DIRED-BUFFER are replaced with the value of
   `diredp-dir-sep-replacement'.

 * If nil, read the buffer name (it is not necessarily a directory
   name).  If in Dired now, the current buffer name is the default.
   With a positive prefix arg, read the buffer name as a directory
   name, defaulting to the `default-directory'.

If you use Icicles then reading uses Icicles completion, with
additional multi-command keys.  See `dired' (defadvice doc)."
  (setq dired-buffer  (and dired-buffer  (diredp-replace-dir-sep-in-string dired-buffer)))
  (let* ((raw-parg  current-prefix-arg)
         (switchs   (and current-prefix-arg
                         (natnump (prefix-numeric-value current-prefix-arg))
                         (read-string "Dired listing switches: " dired-listing-switches)))
         (icicle-candidate-action-fn
          (lambda (cand)
            (dired-other-window cand (and raw-parg  (read-string "Dired listing switches: "
                                                                 dired-listing-switches)))
            (select-window (minibuffer-window))
            (select-frame-set-input-focus (selected-frame))))
;;; $$$$$$ Alternative: Could choose no-op for non-dir candidate.
;;;          (icicle-candidate-action-fn
;;;           (lambda (cand)
;;;             (cond ((file-directory-p cand)
;;;                    (dired-other-window cand (and raw-parg  (read-string "Dired listing switches: "
;;;                                                                         dired-listing-switches)))
;;;                    (select-window (minibuffer-window))
;;;                    (select-frame-set-input-focus (selected-frame)))
;;;                   (t
;;;                    (message "Not a directory: `%s'" cand) (sit-for 2)))))
         (icicle-all-candidates-list-alt-action-fn ; M-|'
          (lambda (files)
            (let ((enable-recursive-minibuffers  t))
              (dired-other-window (cons (diredp-replace-dir-sep-in-string
                                         (read-string (format "Dired %s(buffer name): " string)))
                                        files)))))
         (icicle-sort-comparer                        (or (and (boundp 'icicle-file-sort) ; If not reading files
                                                               icicle-file-sort) ; then dirs first.
                                                          (and (> (prefix-numeric-value raw-parg) 0)
                                                               'icicle-dirs-first-p)
                                                          (and (boundp 'icicle-sort-comparer)
                                                               icicle-sort-comparer)))

         ;; The rest of the bindings are from `icicle-file-bindings', in `icicles-mac.el'.
         (completion-ignore-case
          (or (and (boundp 'read-file-name-completion-ignore-case)  read-file-name-completion-ignore-case)
              completion-ignore-case))
         (icicle-show-Completions-initially-flag      (and (boundp 'icicle-show-Completions-initially-flag)
                                                           (or icicle-show-Completions-initially-flag
                                                               icicle-files-ido-like-flag)))
         (icicle-top-level-when-sole-completion-flag  (and (boundp 'icicle-top-level-when-sole-completion-flag)
                                                           (or icicle-top-level-when-sole-completion-flag
                                                               icicle-files-ido-like-flag)))
         (icicle-default-value                        (and (boundp 'icicle-default-value)
                                                           (if (and icicle-files-ido-like-flag
                                                                    icicle-default-value)
                                                               icicle-files-ido-like-flag
                                                             ;;  Get default via `M-n', but do not insert it.
                                                             (and (memq icicle-default-value '(t nil))
                                                                  icicle-default-value))))
         (icicle-must-match-regexp                    (and (boundp 'icicle-file-match-regexp)
                                                           icicle-file-match-regexp))
         (icicle-must-not-match-regexp                (and (boundp 'icicle-file-no-match-regexp)
                                                           icicle-file-no-match-regexp))
         (icicle-must-pass-after-match-predicate      (and (boundp 'icicle-file-predicate)
                                                           icicle-file-predicate))
         (icicle-require-match-flag                   (and (boundp 'icicle-file-require-match-flag)
                                                           icicle-file-require-match-flag))
         (icicle-file-completing-p                    t)
         (icicle-extra-candidates                     (and (boundp 'icicle-file-extras)  icicle-file-extras))
         (icicle-transform-function                   'icicle-remove-dups-if-extras)
         ;; Put `icicle-file-sort' first.  If already in the list, move it, else add it, to beginning.
         (icicle--temp-orders                         (and (boundp 'icicle-sort-orders-alist)
                                                           (copy-sequence icicle-sort-orders-alist)))
         (icicle-candidate-help-fn                    (lambda (cand)
                                                        (icicle-describe-file cand raw-parg t)))
         (icicle-candidate-alt-action-fn              (and (boundp 'icicle-candidate-alt-action-fn)
                                                           (or icicle-candidate-alt-action-fn
                                                               (icicle-alt-act-fn-for-type "file"))))
         (icicle-delete-candidate-object              'icicle-delete-file-or-directory)
         (icicle-sort-orders-alist
          (and (boundp 'icicle-sort-orders-alist)
               (progn (when t ; $$$$ (and icicle-file-sort-first-time-p  icicle-file-sort)
                        (setq icicle-sort-comparer  icicle-file-sort))
                      ;; $$$$ (setq icicle-file-sort-first-time-p  nil))
                      (if icicle-file-sort
                          (let ((already-there  (rassq icicle-file-sort icicle--temp-orders)))
                            (if already-there
                                (cons already-there (setq icicle--temp-orders  (delete already-there
                                                                                       icicle--temp-orders)))
                              (cons `("by `icicle-file-sort'" ,@icicle-file-sort) icicle--temp-orders)))
                        icicle--temp-orders)))))
    (when (fboundp 'icicle-bind-file-candidate-keys) (icicle-bind-file-candidate-keys))
    (unwind-protect
        (list
         (if (> (prefix-numeric-value raw-parg) 0)
             (or dired-buffer
                 ;; If a dialog box is about to be used, call `read-directory-name' so the dialog
                 ;; code knows we want directories.  Some dialog boxes can only select directories
                 ;; or files when popped up, not both.  If no dialog box is used, call `read-file-name'
                 ;; because the user may want completion of file names for use in a wildcard pattern.
                 (funcall (if (and (fboundp 'read-directory-name)  (next-read-file-uses-dialog-p))
                              #'read-directory-name
                            #'read-file-name)
                          (format "Dired %s(directory): " string) nil default-directory nil))
           (dolist (db  dired-buffers) ; Remove any killed buffers from `dired-buffers' (even if DIRED-BUFFER).
             (unless (buffer-name (cdr db)) (setq dired-buffers  (delq db dired-buffers))))
           (let* ((dbufs   (and (not dired-buffer)
                                (mapcar (lambda (db) (list (buffer-name (cdr db)))) dired-buffers)))
                  (dirbuf  (or dired-buffer
                               (diredp-replace-dir-sep-in-string
                                (completing-read (format "Dired %s(buffer name): " string)
                                                 dbufs nil nil nil nil (and (derived-mode-p 'dired-mode)
                                                                            (buffer-name))))))
                  (files   (and (diredp-existing-dired-buffer-p dirbuf)
                                (with-current-buffer (get-buffer dirbuf)
                                  (and (not (consp dired-directory))
                                       (directory-files dired-directory 'FULL diredp-re-no-dot)))))
                  file)
             (when read-extra-files-p
               (while (condition-case nil ; Use lax completion, to allow wildcards.
                          (setq file  (read-file-name "File or dir (C-g when done): "))
                        (quit nil))
                 ;; Do not allow root dir (`/' or a Windows drive letter, e.g. `c:/').
                 (if (diredp-root-directory-p file)
                     (progn (message "Cannot choose root directory (`%s')" file) (sit-for 1))
                   (push file files))))
             (cons dirbuf files)))
         switchs)
      (when (fboundp 'icicle-unbind-file-candidate-keys) (icicle-unbind-file-candidate-keys)))))


;;; $$$$$$$$ An alternative implementation - different behavior.
;;;
;;; ;; REPLACE ORIGINAL in `dired.el'.
;;; ;;
;;; ;; Non-positive prefix arg means construct cons DIRNAME arg: Read Dired name and files/dirs.
;;; ;;
;;; (defun dired-read-dir-and-switches (string)
;;;   "Read arguments for `dired'.
;;; With a non-negative prefix arg, prompt first for `ls' switches.
;;; With a non-positive prefix arg, read the Dired buffer name and then
;;;  read any number of dir or file names, to make up the Dired listing.

;;; STRING is appended to the prompt, unless prefix arg is non-positive.
;;; If non-empty, STRING should begin with a SPC."
;;;   (let ((switches    (and current-prefix-arg
;;;                           (>= (prefix-numeric-value current-prefix-arg) 0)
;;;                           (read-string "Dired listing switches: " dired-listing-switches)))
;;;         (formt       (format "Dired %s(directory): " string))
;;;         (entries     ())
;;;         (curr-entry  ""))
;;;     (when (and current-prefix-arg  (<= (prefix-numeric-value current-prefix-arg) 0))
;;;       (push (completing-read "Dired buffer name: " dired-buffers) entries)
;;;       (setq curr-entry  (read-file-name (format "Dir or file: ") nil "" 'MUST-MATCH))
;;;       (while (not (equal "" curr-entry))
;;;         (push curr-entry entries)
;;;         (setq curr-entry  (read-file-name (format "Dir or file: ") nil "" 'MUST-MATCH)))
;;;       (unless (cadr entries) (push default-directory entries)))
;;;     (list (or (nreverse entries)  (if (and (fboundp 'next-read-file-uses-dialog-p)
;;;                                            (next-read-file-uses-dialog-p))
;;;                                       (read-directory-name formt nil default-directory nil)
;;;                                     (read-file-name formt nil default-directory nil)))
;;;           switches)))


;; ADVISE ORIGINAL in `dired.el'.
;;
;; Add to doc string, to document non-positive prefix arg.
;;
(defadvice dired (before diredp-doc-cons-arg activate)
  "Interactively, a prefix argument changes the behavior as follows:

* If >= 0, you are first prompted for the `ls' switches to use.

* If <= 0, you are prompted first for the name of the Dired  buffer.
  Then you are prompted repeatedly for the names of the directories
  or files to list in the buffer.  You can use file-name wildcards
  (i.e., `*' for globbing), to include the matching files and
  directories.  Use `C-g' to end.

  In other words, instead of listing a single directory, the Dired
  buffer can list any number of directories and file names, which can
  even belong to different directory trees.

The rest of this description applies only if you use Icicles.

In Icicle mode this is a multi-command: You can cycle among file-name
completion candidates and act individually on those that name
directories.  The action is to open Dired for the directory.  While
cycling, these keys are active:

\\<minibuffer-local-completion-map>\
`C-mouse-2', `C-return' - Act on current completion candidate only
`C-down', `C-wheel-down' - Move to next completion candidate and act
`C-up', `C-wheel-up' - Move to previous completion candidate and act
`C-next'  - Move to next apropos-completion candidate and act
`C-prior' - Move to previous apropos-completion candidate and act
`C-end'   - Move to next prefix-completion candidate and act
`C-home'  - Move to previous prefix-completion candidate and act
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively (careful!)
`\\[icicle-all-candidates-list-alt-action]'     - Open Dired on all candidates

When candidate action and cycling are combined (e.g. `C-next'), user
option `icicle-act-before-cycle-flag' determines which occurs first.

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about candidates.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate, or
`C-g' to quit.

These keys are also bound in the minibuffer during completion (`*'
means the key requires library `Bookmark+'):

   S-delete    - Delete candidate file or (empty) dir
   C-c +       - Create a new directory
   C-backspace - Go up one directory level
 * C-x C-t *   - Narrow to files with all of the tags you specify
 * C-x C-t +   - Narrow to files with some of the tags you specify
 * C-x C-t % * - Narrow to files with all tags matching a regexp
 * C-x C-t % + - Narrow to files with some tags  matching a regexp
 * C-x a +     - Add tags to the current-candidate file
 * C-x a -     - Remove tags from the current-candidate file
 * C-x m       - Access file bookmarks (not just autofiles)"
  (interactive (dired-read-dir-and-switches "" 'READ-EXTRA-FILES-P)))


;; ADVISE ORIGINAL in `dired.el'.
;;
;; Add to doc string, to document non-positive prefix arg.
;;
(defadvice dired-other-window (before diredp-doc-cons-arg activate)
  "Interactively, a prefix argument changes the behavior.
A non-positive prefix arg lets you choose an explicit set of files and
directories to list.  See the advice for `dired' for more information."
  (interactive (dired-read-dir-and-switches "" 'READ-EXTRA-FILES-P)))


;; ADVISE ORIGINAL in `dired.el'.
;;
;; Add to doc string, to document non-positive prefix arg.
;;
(defadvice dired-other-frame (before diredp-doc-cons-arg activate)
  "Interactively, a prefix argument changes the behavior.
A non-positive prefix arg lets you choose an explicit set of files and
directories to list.  See the advice for `dired' for more information."
  (interactive (dired-read-dir-and-switches "" 'READ-EXTRA-FILES-P)))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Made compatible with Emacs 20, 21, which do not have [:alnum:].
;; Also, this is defined here because it is used elsewhere in the file.
;;
(defun dired-switches-escape-p (switches)
  "Return non-nil if the string SWITCHES contains `-b' or `--escape'."
  (if (fboundp 'dired-check-switches) ; Emacs 24.4+ - see Emacs bug #17218.
      (dired-check-switches switches "b" "escape")
    (and (stringp switches)
         ;; Don't match things like "--block-size" that happen to contain "b".
         (diredp-string-match-p (if (> emacs-major-version 21)
                                    "\\(\\`\\| \\)-[[:alnum:]]*b\\|--escape\\>"
                                  "\\(\\`\\| \\)-\\(\w\\|[0-9]\\)*b\\|--escape\\>")
                                switches))))

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
;;;     (let (dir  file-list)
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


  ;; REPLACE ORIGINAL in `dired.el'.
  ;;
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
              ;; Compute WILDCARD arg for file, but only if there's no actual file whose name contains wildcards.
              (insert-directory f
                                switches
                                (and (diredp-string-match-p "[[?*]" f)  (not (file-exists-p f)))
                                nil)
              ;; Re-align fields, if necessary.
              (dired-align-file beg (point))))
        (insert-directory dir switches wildcard (not wildcard)))
      ;; Quote certain characters, unless `ls' quoted them for us.
      (unless (dired-switches-escape-p dired-actual-switches)
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
      ;; If we used `--dired' and it worked, the lines are already indented.  Else indent them.
      (unless (save-excursion (goto-char opoint) (diredp-looking-at-p "  "))
        (let ((indent-tabs-mode  nil)) (indent-rigidly opoint (point) 2)))
      ;; Insert text at the beginning to standardize things.
      (let ((content-point opoint))
        (save-excursion
          (goto-char opoint)
          (when (and (or hdr  wildcard)  (not (and (looking-at "^  \\(.*\\):$")
                                                   (file-name-absolute-p (match-string 1)))))
            ;; `dired-build-subdir-alist' will replace the name by its expansion, so it does not
            ;; matter whether what we insert here is fully expanded, but it should be absolute.
            (insert "  " (directory-file-name (file-name-directory dir)) ":\n")
            (setq content-point (point)))
          (when wildcard
            ;; Insert "wildcard" line where "total" line would be for a full dir.
            (insert "  wildcard " (file-name-nondirectory dir) "\n")))
        (dired-insert-set-properties content-point (point))))))


;;; Image stuff.

(defun diredp-image-dired-required-msg ()
  "Raise an error if `image-dired.el' is not loaded."
  (unless (require 'image-dired nil t) (error "This command requires library `image-dired.el'")))

;; See `image-dired-create-thumb'.
;; Define this even if `image-dired.el' is not loaded.
;; Do NOT raise an error if not loaded, because this is used in `diredp-mouseover-help'.
;;;###autoload
(defun diredp-image-dired-create-thumb (file &optional arg)
  "Create thumbnail image file for FILE (default: file on current line).
With a prefix arg, replace any existing thumbnail for FILE.
With a numeric prefix arg (not a cons), use it as the thumbnail size.
Return the name of the thumbnail image file, or nil if none."
  (interactive (list (if (derived-mode-p 'dired-mode)
                         (dired-get-filename nil 'NO-ERROR)
                       ;; Make it work also for `diredp-list-files' listings.
                       (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                     current-prefix-arg))
  (and (fboundp 'image-dired-thumb-name) ; No-op (return nil) if `image-dired.el' not loaded.
       (let ((thumb-name  (image-dired-thumb-name file)))
         (when arg (clear-image-cache))
         (when (or arg  (not (file-exists-p thumb-name)))
           (let ((image-dired-thumb-width   (or (and arg  (atom arg)  arg)  image-dired-thumb-width))
                 (image-dired-thumb-height  (or (and arg  (atom arg)  arg)  image-dired-thumb-height)))
             (unless (zerop (image-dired-create-thumb file thumb-name))
               (error "Thumbnail image file could not be created"))))
         (and (file-exists-p thumb-name)  thumb-name))))


;; REPLACE ORIGINAL in `image-dired.el' (Emacs 22-23).
;;
;; 1. Raise an error if `image-dired.el' is not available.
;; 2. Repro it here so it picks up `Dired+' version of `dired-map-over-marks'.
;;
;;;###autoload
(defun image-dired-dired-insert-marked-thumbs () ; Bound to `C-t C-t' (Emacs 22-23)
  "Insert thumbnails before file names of marked files in the Dired buffer."
  (interactive (progn (diredp-image-dired-required-msg) ()))
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
       (setq overlay  (car (delq nil (mapcar (lambda (ov) (and (overlay-get ov 'put-image)  ov))
                                             (overlays-in (point) (1+ (point)))))))
       (overlay-put overlay 'image-file image-file)
       (overlay-put overlay 'thumb-file thumb-file)))
   nil)
  (add-hook 'dired-after-readin-hook 'image-dired-dired-after-readin-hook nil t))


;; REPLACE ORIGINAL in `image-dired.el' (Emacs 24+).
;;
;; 1. Raise an error if `image-dired.el' is not available.
;; 2. Repro it here so it picks up `Dired+' version of `dired-map-over-marks'.
;;
;;;###autoload
(defun image-dired-dired-toggle-marked-thumbs (&optional arg) ; Bound to `C-t C-t' (Emacs 24+)
  "Toggle thumbnails in front of file names in Dired.
If no files are marked, insert or hide thumbnails on the current line.
With a numeric prefix arg N, ignore marked files and act on the next N
files (previous -N files, if N < 0)."
  (interactive (progn (diredp-image-dired-required-msg) (list current-prefix-arg)))
  (dired-map-over-marks
   (let* ((image-pos   (dired-move-to-filename))
          (image-file  (diredp-get-image-filename nil 'NO-ERROR))
          thumb-file  overlay)
     (when image-file
       (setq thumb-file  (image-dired-get-thumbnail-image image-file))
       ;; If image is not already added, then add it.
       (let* ((cur-ovs   (overlays-in (point) (1+ (point))))
              (thumb-ov  (car (diredp-remove-if-not (lambda (ov) (overlay-get ov 'thumb-file))
                                                    cur-ovs))))
         (if thumb-ov
             (delete-overlay thumb-ov)
           (put-image thumb-file image-pos)
           (setq overlay (car (delq nil (mapcar (lambda (ov) (and (overlay-get ov 'put-image)  ov))
                                                (overlays-in (point) (1+ (point)))))))
           (overlay-put overlay 'image-file image-file)
           (overlay-put overlay 'thumb-file thumb-file)))))
   arg
   'SHOW-PROGRESS)
  (add-hook 'dired-after-readin-hook 'image-dired-dired-after-readin-hook nil t))

;; Corresponds to `image-dired-dired-comment-files'.
;;;###autoload
(defun diredp-image-dired-comment-file ()
  "Add comment to this image file."
  (interactive (progn (diredp-image-dired-required-msg) ()))
  (image-dired-write-comments (cons (dired-get-filename) (image-dired-read-comment))))

;; Corresponds to `image-dired-tag-files'.
;;;###autoload
(defun diredp-image-dired-tag-file ()
  "Tag this image file with an `image-dired' tag."
  (interactive (progn (diredp-image-dired-required-msg) ()))
  (image-dired-write-tags (cons (dired-get-filename)
                                (read-string "Tags to add (use `;' to separate): "))))

;; Corresponds to `image-dired-delete-tag'.
;;;###autoload
(defun diredp-image-dired-delete-tag ()
  "Remove an `image-dired' tag from  this image file."
  (interactive (progn (diredp-image-dired-required-msg) ()))
  (image-dired-remove-tag (list (dired-get-filename)) (read-string "Tag to remove: ")))

;; Corresponds to `image-dired-display-thumbs'.
;;;###autoload
(defun diredp-image-dired-display-thumb (&optional append)
  "Pop to thumbnail of this image file, in `image-dired-thumbnail-buffer'.
If a thumbnail image does not yet exist for this file, create it.
With a prefix arg, append the thumbnail to the thumbnails buffer,
instead of clearing the buffer first."
  (interactive (progn (diredp-image-dired-required-msg) (list current-prefix-arg)))
  (let* ((dired-buf   (current-buffer))
         (curr-file   (dired-get-filename))
         (thumb-name  (image-dired-thumb-name curr-file)))
    (with-current-buffer (image-dired-create-thumbnail-buffer)
      (let ((inhibit-read-only  t))
        (if (not append) (erase-buffer) (goto-char (point-max)))
        (if (and (not (file-exists-p thumb-name))
                 (not (zerop (image-dired-create-thumb curr-file thumb-name))))
            (message "Cannot create thumbnail image for file `%s'" curr-file)
          (image-dired-insert-thumbnail thumb-name curr-file dired-buf)))
      (cond ((eq 'dynamic image-dired-line-up-method)     (image-dired-line-up-dynamic))
            ((eq 'fixed image-dired-line-up-method)       (image-dired-line-up))
            ((eq 'interactive image-dired-line-up-method) (image-dired-line-up-interactive))
            ((eq 'none image-dired-line-up-method)        nil)
            (t                                            (image-dired-line-up-dynamic))))
    (pop-to-buffer image-dired-thumbnail-buffer)))

;; Corresponds to `image-dired-copy-with-exif-file-name'.
;;;###autoload
(defun diredp-image-dired-copy-with-exif-name ()
  "Copy this image file to your main image directory.
Uses `image-dired-get-exif-file-name' to name the new file."
  (interactive (progn (diredp-image-dired-required-msg) ()))
  (let* ((curr-file  (dired-get-filename))
         (new-name   (format "%s/%s" (file-name-as-directory
                                      (expand-file-name image-dired-main-image-directory))
                             (image-dired-get-exif-file-name curr-file))))
    (message "Copying `%s' to `%s'..." curr-file new-name)
    (copy-file curr-file new-name)
    (message "Copying `%s' to `%s'...done" curr-file new-name)))

;; Corresponds to `image-dired-dired-edit-comment-and-tags'.
;;;###autoload
(defun diredp-image-dired-edit-comment-and-tags ()
  "Edit comment and tags for this image file."
  (interactive (progn (diredp-image-dired-required-msg) ()))
  (setq image-dired-widget-list  ())
  (let ((file  (dired-get-filename)))
    (if (fboundp 'pop-to-buffer-same-window)
        (pop-to-buffer-same-window "*Image-Dired Edit Meta Data*")
      (switch-to-buffer "*Image-Dired Edit Meta Data*"))
    (kill-all-local-variables)
    (make-local-variable 'widget-example-repeat)
    (let ((inhibit-read-only  t))
      (erase-buffer)
      (remove-overlays)
      (widget-insert
       "\nEdit comment and tags for the image.  Separate multiple tags
with a comma (`,').  Move forward among fields using `TAB' or `RET'.
Move backward using `S-TAB'.  Click `Save' to save your edits or
`Cancel' to abandon them.\n\n")
      (let* ((thumb-file  (image-dired-thumb-name file))
             (img         (create-image thumb-file))
             comment-widget  tag-widget)
        (insert-image img)
        (widget-insert "\n\nComment: ")
        (setq comment-widget  (widget-create 'editable-field :size 60 :format "%v "
                                             :value (or (image-dired-get-comment file)  "")))
        (widget-insert "\nTags:    ")
        (setq tag-widget  (widget-create 'editable-field :size 60 :format "%v "
                                         :value (or (mapconcat #'identity (image-dired-list-tags file) ",")  "")))
        ;; Save info in widgets to use when the user saves the form.
        (setq image-dired-widget-list  (append image-dired-widget-list
                                               (list (list file comment-widget tag-widget))))
        (widget-insert "\n\n")))
    (widget-insert "\n")
    (widget-create 'push-button :notify (lambda (&rest _ignore)
                                          (image-dired-save-information-from-widgets)
                                          (bury-buffer)
                                          (message "Done"))
                   "Save")
    (widget-insert " ")
    (widget-create 'push-button :notify (lambda (&rest _ignore)
                                          (bury-buffer)
                                          (message "Operation canceled"))
                   "Cancel")
    (widget-insert "\n")
    (when (boundp 'widget-keymap) (use-local-map widget-keymap)) ; Emacs 22+
    (widget-setup)
    (widget-forward 1)))                ; Jump to the first widget.

;;;###autoload
(defun diredp-do-display-images (&optional arg)
  "Display the marked image files.
A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any files are marked).
 More than one `C-u' means use all files in the Dired buffer, as if
 they were all marked."
  (interactive (progn (unless (require 'image-file nil t)
                        (error "This command requires library `image-file.el'"))
                      (diredp-ensure-mode)
                      (list current-prefix-arg)))
  (dired-map-over-marks-check #'diredp-display-image arg 'display\ image (diredp-fewer-than-2-files-p arg)))

(defun diredp-display-image ()
  "Display image file at point.  Log an error using `dired-log'."
  (let ((file     (dired-get-filename t 'NO-ERROR))
        (failure  nil))
    (save-excursion
      (if (and file  (diredp-string-match-p (image-file-name-regexp) file))
          (condition-case err
              (let ((find-file-run-dired  nil)) (find-file-other-window file))
            (error (setq failure  (error-message-string err))))
        (dired-log (format "Not an image file: `%s'" file))
        (setq failure  t)))
    (and failure                    ; Return nil for success.
         (prog1 file                ; Return file name for failure.
           (unless (eq t failure) (dired-log "Cannot display image file `%s':\n%s\n" file failure)  t)))))

;;;###autoload
(defun diredp-image-show-this-file (&optional arg)
  "Show the image file named on this line in another frame or window.
Option `diredp-image-show-this-file-use-frame-flag' which is used.

With a prefix arg, shrink the image to fit a frame that many lines
high or a window at least that many lines high.
Otherwise, show the image full size.
Note:
 * To show the image full size, you can also use `\\<dired-mode-map>\\[dired-find-file]'.
 * To show the image in another window, at whatever scale fits there,
   you can use `\\[image-dired-dired-display-image]'."
  (interactive (progn (diredp-image-dired-required-msg) (list current-prefix-arg)))
  (image-dired-create-display-image-buffer)
  (let ((fit-frame-inhibit-fitting-flag  t) ; In `fit-frame.el'.
        (img-file                        (diredp-get-image-filename)))
    (if img-file
        (with-current-buffer image-dired-display-image-buffer
          (let* ((window-min-height  (if arg
                                         (prefix-numeric-value arg)
                                       (ceiling (cdr (image-size (create-image img-file))))))
                 (special-display-frame-alist   (if diredp-image-show-this-file-use-frame-flag
                                                    (cons `(height . ,window-min-height)
                                                          special-display-frame-alist)
                                                  special-display-frame-alist))
                 (special-display-buffer-names  (if diredp-image-show-this-file-use-frame-flag
                                                    (cons image-dired-display-image-buffer
                                                          special-display-buffer-names)
                                                  special-display-buffer-names)))
            (display-buffer image-dired-display-image-buffer)
            (image-dired-display-image img-file (not arg))))
      (message "No image file here")))) ; An error is handled by `diredp-get-image-filename'.

(defun diredp-report-file-result (file result failure echop)
  "Report failure (error) or success using `message' and logging.
Return RESULT."
  (let ((msg  (if failure
                  (format "Error for %s:\n%s\n" file failure)
                (format "Result for %s:\n%s\n" file result))))
    (when echop (message msg) (sit-for 1))
    (dired-log msg))
  result)

(defun diredp-read-command (&optional prompt default)
  "Read the name of a command and return a symbol with that name.
\(A command is anything that satisfies predicate `commandp'.)
Prompt with PROMPT, which defaults to \"Command: \".
By default, return the command named DEFAULT (or, with Emacs 23+, its
first element if DEFAULT is a list).  (If DEFAULT does not name a
command then it is ignored.)"
  (setq prompt  (or prompt  "Emacs command: "))
  (let ((name  (completing-read prompt obarray #'commandp t nil
                                'extended-command-history default)))
    (while (string= "" name)
      (setq name  (completing-read prompt obarray #'commandp t nil
                                   'extended-command-history default)))
    (intern name)))
 
;;; Face Definitions

(defface diredp-autofile-name
    '((((background dark)) (:background "#111313F03181")) ; Very dark blue
      (t                   (:background "#EEECEC0FCE7E"))) ; Very pale goldenrod
  "*Face used in Dired for names of files that are autofile bookmarks."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-autofile-name 'diredp-autofile-name)

(defface diredp-compressed-file-name
    '((((background dark)) (:foreground "Blue"))
      (t                   (:foreground "Brown")))
  "*Face used for compressed file names."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-compressed-file-name 'diredp-compressed-file-name)

(defface diredp-compressed-file-suffix
    '((((background dark)) (:foreground "Blue"))
      (t                   (:foreground "Yellow")))
  "*Face used for compressed file suffixes in Dired buffers.
This means the `.' plus the file extension.  Example: `.zip'."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-compressed-file-suffix 'diredp-compressed-file-suffix)

(defface diredp-date-time
    '((((background dark)) (:foreground "#74749A9AF7F7")) ; ~ med blue
      (t                   (:foreground "DarkGoldenrod4")))
  "*Face used for date and time in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-date-time 'diredp-date-time)

(defface diredp-deletion
    '((t (:foreground "Yellow" :background "Red")))
  "*Face used for deletion flags (D) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-deletion 'diredp-deletion)

(defface diredp-deletion-file-name
    '((t (:foreground "Red")))
  "*Face used for names of deleted files in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-deletion-file-name 'diredp-deletion-file-name)

(defface diredp-dir-heading
    '((((background dark)) (:foreground "Yellow" :background "#00003F3F3434")) ; ~ dark green
      (t                   (:foreground "Blue" :background "Pink")))
  "*Face used for directory headings in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-dir-heading 'diredp-dir-heading)

(defface diredp-dir-name
    '((((background dark))
       (:foreground "#7474FFFFFFFF" :background "#2C2C2C2C2C2C")) ; ~ cyan, dark gray
      (t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory names."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-dir-name 'diredp-dir-name)

(defface diredp-dir-priv
    '((((background dark))
       (:foreground "#7474FFFFFFFF" :background "#2C2C2C2C2C2C")) ; ~ cyan, dark gray
      (t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory privilege indicator (d) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-dir-priv 'diredp-dir-priv)

(defface diredp-exec-priv
    '((((background dark)) (:background "#4F4F3B3B2121")) ; ~ dark brown
      (t                   (:background "LightSteelBlue")))
  "*Face used for execute privilege indicator (x) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-exec-priv 'diredp-exec-priv)

;; For this to show up, you need `F' among the options in `dired-listing-switches'.
;; For example, I use "-alF" for `dired-listing-switches'.
(defface diredp-executable-tag
    '((t (:foreground "Red")))
  "*Face used for executable tag (*) on file names in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-executable-tag 'diredp-executable-tag)

(defface diredp-file-name
    '((((background dark)) (:foreground "Yellow"))
      (t                   (:foreground "Blue")))
  "*Face used for file names (without suffixes) in Dired buffers.
This means the base name.  It does not include the `.'."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-name 'diredp-file-name)

(defface diredp-file-suffix
    '((((background dark)) (:foreground "#7474FFFF7474")) ; ~ light green
      (t                   (:foreground "DarkMagenta")))
  "*Face used for file suffixes in Dired buffers.
This means the `.' plus the file extension.  Example: `.elc'."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-suffix 'diredp-file-suffix)

(defface diredp-flag-mark
    '((((background dark)) (:foreground "Blue" :background "#7575D4D41D1D")) ; ~ olive green
      (t                   (:foreground "Yellow" :background "Blueviolet")))
  "*Face used for flags and marks (except D) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark 'diredp-flag-mark)

(defface diredp-flag-mark-line
    '((((background dark)) (:background "#787831311414")) ; ~ dark red brown
      (t                   (:background "Skyblue")))
  "*Face used for flagged and marked lines in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark-line 'diredp-flag-mark-line)

(defface diredp-ignored-file-name
  '((((background dark)) (:foreground "#C29D6F156F15"))    ; ~ salmon
    (t                   (:foreground "#00006DE06DE0"))) ; ~ dark cyan
  "*Face used for files whose names are omitted based on the extension.
See also face `diredp-omit-file-name'."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-ignored-file-name 'diredp-ignored-file-name)

(defface diredp-link-priv
    '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
      (t                   (:foreground "DarkOrange")))
  "*Face used for link privilege indicator (l) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-link-priv 'diredp-link-priv)

(when (> emacs-major-version 21)
  (defface diredp-mode-line-marked
      '((t (:foreground "DarkViolet")))
    "*Face for marked number in mode-line `mode-name' for Dired buffers."
    :group 'Dired-Plus :group 'font-lock-highlighting-faces)

  (defface diredp-mode-line-flagged
      '((t (:foreground "Red")))
    "*Face for flagged number in mode-line `mode-name' for Dired buffers."
    :group 'Dired-Plus :group 'font-lock-highlighting-faces))

(defface diredp-no-priv
    '((((background dark)) (:background "#2C2C2C2C2C2C")) ; ~ dark gray
      (t                   (:background "LightGray")))
  "*Face used for no privilege indicator (-) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-no-priv 'diredp-no-priv)

(defface diredp-number
    '((((background dark)) (:foreground "#FFFFFFFF7474")) ; ~ light yellow
      (t                   (:foreground "DarkBlue")))
  "*Face used for numerical fields in Dired buffers.
In particular, inode number, number of hard links, and file size."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-number 'diredp-number)

(defface diredp-omit-file-name
  (if (assq :inherit custom-face-attributes) ; Emacs 22+
      '((((background dark)) (:inherit diredp-ignored-file-name :strike-through "#555555555555")) ; ~ dark gray
        (t                   (:inherit diredp-ignored-file-name :strike-through "#AAAAAAAAAAAA"))) ; ~ light gray
    '((((background dark)) (:foreground "#C29D6F156F15")) ; ~ salmon
      (t                   (:foreground "#00006DE06DE0")))) ; ~ dark cyan
  "*Face used for files whose names will be omitted in `dired-omit-mode'.
This means file names that match regexp `diredp-omit-files-font-lock-regexp'.
\(File names matching `dired-omit-extensions' are highlighted with face
`diredp-ignored-file-name' instead.)"
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-omit-file-name 'diredp-omit-file-name)

(defface diredp-other-priv
    '((((background dark)) (:background "#111117175555")) ; ~ dark blue
      (t                   (:background "PaleGoldenrod")))
  "*Face used for l,s,S,t,T privilege indicators in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-other-priv 'diredp-other-priv)

(defface diredp-rare-priv
    '((((background dark)) (:foreground "Green" :background "#FFFF00008080")) ; ~ hot pink
      (t                   (:foreground "Magenta" :background "SpringGreen")))
  "*Face used for rare privilege indicators (b,c,s,m,p,S) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-rare-priv 'diredp-rare-priv)

(defface diredp-read-priv
    '((((background dark)) (:background "#999932325555")) ; ~ burgundy / dark magenta
      (t                   (:background "MediumAquamarine")))
  "*Face used for read privilege indicator (w) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-read-priv 'diredp-read-priv)

(defface diredp-symlink
    '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
      (t                   (:foreground "DarkOrange")))
  "*Face used for symbolic links in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-symlink 'diredp-symlink)

(defface diredp-tagged-autofile-name
    '((((background dark)) (:background "#328C0411328C")) ; Very dark magenta
      (t                   (:background "#CD73FBEECD73"))) ; Very pale green
  "*Face used in Dired for names of files that are autofile bookmarks."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-tagged-autofile-name 'diredp-tagged-autofile-name)

(defface diredp-write-priv
    '((((background dark)) (:background "#25258F8F2929")) ; ~ dark green
      (t                   (:background "Orchid")))
  "*Face used for write privilege indicator (w) in Dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-write-priv 'diredp-write-priv)

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
   '("^  wildcard.*$" 0 'default)   ; Override others, e.g. `l' for `diredp-other-priv'.
   '("^  (No match).*$" 0 'default) ; Override others, e.g. `t' for `diredp-other-priv'.
   '("[^ .]\\(\\.[^. /]+\\)$" 1 diredp-file-suffix) ; Suffix, including `.'.
   '("\\([^ ]+\\) -> .+$" 1 diredp-symlink)         ; Symbolic links

   ;; 1) Date/time and 2) filename w/o suffix.
   ;;    This is a bear, and it is fragile - Emacs can change `dired-move-to-filename-regexp'.
   (if (or (not (fboundp 'version<))  (version< emacs-version "23.2"))
       (list dired-move-to-filename-regexp
             (list 1 'diredp-date-time t t) ; Date/time
             (list (concat "\\(.+\\)\\(" (concat (funcall #'regexp-opt diredp-compressed-extensions)
                                                 "\\)[*]?$")) ; Compressed-file name
                   nil nil (list 0 diredp-compressed-file-name 'keep t)))
     `(,dired-move-to-filename-regexp
       (7 diredp-date-time t t) ; Date/time, locale (western or eastern)
       (2 diredp-date-time t t) ; Date/time, ISO
       (,(concat "\\(.+\\)\\(" (concat (funcall #'regexp-opt diredp-compressed-extensions)
                                       "\\)[*]?$"))
        nil nil (0 diredp-compressed-file-name keep t)))) ; Compressed-file suffix
   (if (or (not (fboundp 'version<))  (version< emacs-version "23.2"))
       (list dired-move-to-filename-regexp
             (list 1 'diredp-date-time t t) ; Date/time
             (list "\\(.+\\)$" nil nil (list 0 diredp-file-name 'keep t))) ; Filename
     `(,dired-move-to-filename-regexp
       (7 diredp-date-time t t) ; Date/time, locale (western or eastern)
       (2 diredp-date-time t t) ; Date/time, ISO
       ("\\(.+\\)$" nil nil (0 diredp-file-name keep t)))) ; Filename (not a compressed file)

   ;; Files to ignore.
   ;;   Use face `diredp-ignored-file-name' for omission by file-name extension.
   ;;   Use face `diredp-omit-file-name' for omission by entire file name.
   (let* ((omit-exts   (or (and (boundp 'dired-omit-extensions)  dired-omit-extensions)
                           completion-ignored-extensions))
          (omit-exts   (and omit-exts  (mapconcat #'regexp-quote omit-exts "\\|")))
          (compr-exts  (and diredp-ignore-compressed-flag
                            (concat "\\|" (mapconcat #'regexp-quote diredp-compressed-extensions "\\|")))))
     (list (concat "^  \\(.*\\(" omit-exts compr-exts "\\)[*]?\\)$") ; [*]? allows for executable flag (*).
           1 diredp-ignored-file-name t))
   `(,(concat "^.*" dired-move-to-filename-regexp
              "\\(" diredp-omit-files-font-lock-regexp "\\)[*]?$") ; [*]? allows for executable flag (*).
     (0 diredp-omit-file-name t))

   ;; Compressed-file (suffix)
   (list (concat "\\(" (funcall #'regexp-opt diredp-compressed-extensions) "\\)[*]?$")
         1 diredp-compressed-file-suffix t)
   '("\\([*]\\)$" 1 diredp-executable-tag t) ; Executable (*)

   ;; Inode, hard-links, & file size (. and , are for the decimal point, depending on locale)
   ;; See comment for `directory-listing-before-filename-regexp' in `files.el' or `files+.el'.
   '("\\(\\([0-9]+\\([.,][0-9]+\\)?\\)[BkKMGTPEZY]?[ /]?\\)" 1 diredp-number)

   ;; Directory names - exclude d:/..., Windows drive letter in a dir heading.
   (list (concat dired-re-maybe-mark dired-re-inode-size "\\(d\\)[^:]")
         '(1 diredp-dir-priv t) '(".+" (dired-move-to-filename) nil (0 diredp-dir-name t)))

   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\(x\\)") ; o x
         '(1 diredp-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([lsStT]\\)") ; o misc
         '(1 diredp-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\(w\\).") ; o w
         '(1 diredp-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\(r\\)..") ; o r
         '(1 diredp-read-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\(x\\)...") ; g x
         '(1 diredp-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([lsStT]\\)...") ; g misc
         '(1 diredp-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\(w\\)....") ; g w
         '(1 diredp-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\(r\\).....") ; g r
         '(1 diredp-read-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\(x\\)...") ; u x
         '(1 diredp-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([lsStT]\\)...") ; u misc
         '(1 diredp-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\(w\\)....") ; u w
         '(1 diredp-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\(r\\).....") ; u r
         '(1 diredp-read-priv))

   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([-rwxlsStT]\\)") ; o -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\([-rwxlsStT]\\).") ; g -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\([-rwxlsStT]\\)..") ; u -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([-rwxlsStT]\\)...") ; o -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\([-rwxlsStT]\\)....") ; g -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\([-rwxlsStT]\\).....") ; u -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([-rwxlsStT]\\)......") ; o -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\([-rwxlsStT]\\).......") ; g -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\([-rwxlsStT]\\)........") ; u -
         '(1 diredp-no-priv keep))

   (list (concat dired-re-maybe-mark dired-re-inode-size "\\([bcsmpS]\\)") ; (rare)
         '(1 diredp-rare-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "\\(l\\)[-rwxlsStT]") ; l
         '(1 diredp-link-priv keep))

   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "].*$\\)")
         1 diredp-flag-mark-line t)     ; Flag/mark lines
   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "]\\)") ; Flags, marks (except D)
         1 diredp-flag-mark t)

   (list (concat "^\\([" (char-to-string dired-del-marker) "].*$\\)") ; Deletion-flagged lines
         1 diredp-deletion-file-name t)
   (list (concat "^\\([" (char-to-string dired-del-marker) "]\\)") ; Deletion flags (D)
         1 diredp-deletion t)

   ) "2nd level of Dired highlighting.  See `font-lock-maximum-decoration'.")


(defun diredp--set-up-font-locking ()
  "Add this to `dired-mode-hook' to provide for second-level fontifying."
  (set (make-local-variable 'font-lock-defaults)
       ;; Two levels.  Use 3-element list, since it is standard to have one more
       ;; than the number of levels.  This is necessary for it to work with
       ;; `font(-lock)-menus.el'.
       '((dired-font-lock-keywords
          dired-font-lock-keywords
          diredp-font-lock-keywords-1)
         t nil nil beginning-of-line))
  ;; Refresh `font-lock-keywords' from `font-lock-defaults'
  (when (fboundp 'font-lock-refresh-defaults) (font-lock-refresh-defaults)))

;;; Provide for the second level of fontifying.
(add-hook 'dired-mode-hook 'diredp--set-up-font-locking)

;; Ensure that Dired buffers are refontified when you use `g' or otherwise read in the file list.
(defun diredp-refontify-buffer ()
  "Turn `font-lock-mode' off, then on."
  (setq font-lock-mode  nil)
  (font-lock-mode))
(add-hook 'dired-after-readin-hook 'diredp-refontify-buffer)
 
;;; Function Definitions

;;; $$$$$$$$
;;; (defun diredp-dired-files (arg &optional switches) ; Not bound
;;;   "Like `dired', but non-positive prefix arg prompts for files to list.
;;; This is like `dired' unless you use a non-positive prefix arg.
;;; In that case, you are prompted for names of files and directories to
;;; list, and then you are prompted for the name of the Dired buffer that
;;; lists them.  Use `C-g' when you are done entering file names to list.

;;; In all cases, when inputting a file or directory name you can use
;;; shell wildcards.

;;; If you use Icicles, then in Icicle mode the following keys are bound
;;; in the minibuffer during completion (`*' means the key requires
;;; library `Bookmark+'):

;;;   M-|       - Open Dired on the file names matching your input
;;;   C-c +     - Create a new directory
;;;  *C-x a +   - Add tags to the current-candidate file
;;;  *C-x a -   - Remove tags from the current-candidate file
;;;  *C-x m     - Access file bookmarks (not just autofiles)"
;;;   (interactive (diredp-dired-files-interactive-spec ""))
;;;   (when (consp arg)
;;;     (let ((buf  (dired-find-buffer-nocreate (car arg)))) ; Respect file list.
;;;       (when buf (kill-buffer buf))))
;;;   (if (fboundp 'pop-to-buffer-same-window)
;;;       (pop-to-buffer-same-window (dired-noselect arg switches))
;;;     (switch-to-buffer (dired-noselect arg switches))))

;;; (defun diredp-dired-files-other-window (arg &optional switches) ; Not bound
;;;   "Same as `diredp-dired-files' except uses another window."
;;;   (interactive (diredp-dired-files-interactive-spec "in other window "))
;;;   (when (consp arg)
;;;     (let ((buf  (dired-find-buffer-nocreate (car arg)))) ; Respect file list.
;;;       (when buf (kill-buffer buf))))
;;;   (dired-other-window arg switches))

;;;###autoload
(defun diredp-dired-for-files (arg &optional switches) ; Bound to `C-x D F'
  "Dired the file names that you enter, in a Dired buffer that you name.
You are prompted for the name of the Dired buffer to use.
You are then prompted for names of files and directories to list,
 which can be located anywhere.
Use `C-g' when you are done.

With a prefix arg you are first prompted for the `ls' switches to use.

See also `dired' (including the advice)."
  (interactive (let ((current-prefix-arg  (if current-prefix-arg 0 -1)))
                 (dired-read-dir-and-switches "" 'READ-EXTRA-FILES-P)))
  (dired arg switches)
  (with-current-buffer (car arg)
    (when (boundp 'dired-sort-inhibit) (set (make-local-variable 'dired-sort-inhibit) t))))

;;;###autoload
(defun diredp-dired-for-files-other-window (arg &optional switches) ; Bound to `C-x 4 D F'
  "Same as `diredp-dired-for-files', except uses another window."
  (interactive (let ((current-prefix-arg  (if current-prefix-arg 0 -1)))
                 (dired-read-dir-and-switches "in other window " 'READ-EXTRA-FILES-P)))
  (dired-other-window arg switches)
  (with-current-buffer (car arg)
    (when (boundp 'dired-sort-inhibit) (set (make-local-variable 'dired-sort-inhibit) t))))

;;;###autoload
(defun diredp-define-snapshot-dired-commands (cmd-name directory &optional files msg-p)
  "Define Dired commands for the marked files in current Dired buffer.
Define both a same-window and an other-window command.

Display the command definitions in a separate window.  You can use
`\\[write-file]' (`write-file') or `\\[append-to-file]' to save them.

A prefix ARG specifies files to use instead of the marked files:

 If ARG is an integer, use the files and dirs named on the next ARG
  lines (previous -ARG, if < 0).  E.g., 1: use file on current line;
  -1: use file on previous line.

 If ARG is `C-u C-u', `C-u C-u C-u', or `C-u C-u C-u C-u', then use
  all files in the Dired buffer, where:
    `C-u C-u' includes only files - NO directories
    `C-u C-u C-u' includes directories EXCEPT `.' and `..'
    `C-u C-u C-u C-u' includes ALL directories (even `.' and `..')

 If ARG is otherwise non-nil (e.g. `M--' or a single `C-u'), use the
  file or dir of the current line.  This is the same as ARG = 1.

When called from Lisp:

 CMD-NAME is the name of the new command, a symbol.

 DIRECTORY is the directory/buffer name of the Dired buffer that the
  new command shows.  It is the car of the list returned by
  `dired-read-dir-and-switches'.

 FILES is the list of file and directory names to be shown in the
  Dired buffer.

 Non-nil MSG-P means echo the name of the new command.

___

The defined commands produce a Dired buffer that lists the files and
directories chosen when they were defined, as absolute file names.
This set of files never changes, regardless of any changes to the
content of the directory in which you created the commands.

Using any prefix arg with the commands prompts for the `ls' switches
to use.

Such commands are particularly useful to dired a set of files and dirs
that results from some possibly long-lasting command, such as
`find-dired'.  This can be quicker than recalculating the set of files
to Dired, but the set of files used is not necessarily up-to-date with
respect to what a new calculation would produce.

You can change the values of variables
`diredp-snapshot-cmd-time-format'
`diredp-snapshot-cmd-buffer-name-format', to change format of the
Dired buffer name.
___

See also commands `diredp-marked(-other-window)', which you can also
use to get snapshot Dired buffers.  Use library Bookmark+ to bookmark
such a buffer for persistent access."
  (interactive (diredp-get-args-for-snapshot-cmd))
  (diredp-ensure-mode)
  (let ((cmd-same   (diredp-define-snapshot-dired-commands-1
                     cmd-name directory files nil))
        (cmd-other  (diredp-define-snapshot-dired-commands-1
                     (intern (format "%s-other-window" cmd-name)) directory files 'OTHER-WIN)))
    (when msg-p                         ; Display definitions, with comments on how to save them.
      (let ((buf  (get-buffer-create (format "*DEFUN `%s(-other-window)'*" cmd-same))))
        (with-current-buffer buf
          (insert
           (substitute-command-keys
            (format (concat ";; Commands `%s(-other-window)'.\n"
                            ";; Use `\\[write-file]' (`write-file') or `\\[append-to-file]' to save to a file.\n"
                            "(symbol-function cmd-same)\n")
                    cmd-same)))
          (pp-eval-last-sexp 'INSERT)
          (backward-sexp 2)
          (kill-sexp)
          (goto-char (point-min))
          (forward-line 3)
          (insert (format "(defun %s (switches)\n" cmd-same))
          (kill-line 2)
          (forward-line -1)
          (forward-sexp 1)
          (insert "\n(symbol-function cmd-other)\n")
          (pp-eval-last-sexp 'INSERT)
          (backward-sexp 2)
          (kill-sexp)
          (forward-line 1)
          (insert (format "(defun %s (switches)\n" cmd-other))
          (kill-line 2)
          (emacs-lisp-mode)
          (set-buffer-modified-p nil))
        (display-buffer buf))
      (when msg-p (message "Commands defined: `%s(-other-window)'" cmd-same))
      cmd-other)))

(defvar diredp-snapshot-cmd-time-format "%F %R %z"
  "Time format for `diredp-define-snapshot-dired-commands-1'.
Used as arg to `format-time-string'.")

(defun diredp--snapshot-cmd-name-time ()
  "Replace SPC chars in time part of snapshot command names.
Uses `diredp-snapshot-cmd-time-format' for the time part to fix."
  (replace-regexp-in-string
   (regexp-quote " ") "_" (format-time-string diredp-snapshot-cmd-time-format)))

(defvar diredp-snapshot-cmd-buffer-name-format "Files on %s"
  "Buffer name format for `diredp-define-snapshot-dired-commands-1'.
Should accept a single value, which by default is a time string.")

(defun diredp-get-args-for-snapshot-cmd ()
  "Get args for `diredp-define-snapshot-dired-commands'.
Read command name.
Get directory/buffer name with `dired-read-dir-and-switches'.
Get file list with `dired-get-marked-files' and `current-prefix-arg'."
  (diredp-ensure-mode)
  (let* ((dbufname  (format diredp-snapshot-cmd-buffer-name-format (diredp--snapshot-cmd-name-time)))
         (dir       (let ((current-prefix-arg  nil))
                      (car (dired-read-dir-and-switches "" nil dbufname))))
         (fils      (dired-get-marked-files nil current-prefix-arg)))
    (list (intern (read-string "Define command: "
                               (format "dired-files-in-%s-on-%s"
                                       default-directory (diredp--snapshot-cmd-name-time))))
          dir
          fils
          'MSGP)))

(defun diredp-define-snapshot-dired-commands-1 (cmd-name directory files other-window-p)
  "Helper for `diredp-define-snapshot-dired-commands', which see for args."

  (defalias cmd-name `(lambda (switches)
                        ,(format "Dired files chosen from a Dired buffer at a given time%s.
The set of files listed does not change, regardless of any changes
to the content of the directory in which the command was created.

A prefix arg prompts you for the `ls' switches to use."
                                 (if other-window-p ", in other window" ""))
                        (interactive (list (and current-prefix-arg
                                                (read-string "Dired listing switches: "
                                                             dired-listing-switches))))
                        (funcall ,(if other-window-p
                                      '#'dired-other-window
                                    '#'dired)
                                 ',`(,directory ,@files)
                                 switches)))
  cmd-name)

(when (fboundp 'quit-restore-window)    ; Emacs 24+

  ;; Use this for `q' because `diredp-dired-recent-files' creates a new unique buffer name.
  ;;
  (defun diredp-quit-window-kill (&optional window)
    "Quit WINDOW, deleting it, and kill its buffer.
WINDOW must be a live window and defaults to the selected one.

This is similar to the version of `quit-window' that Emacs had before
the introduction of `quit-restore-window'.  It ignores the information
stored in WINDOW's `quit-restore' window parameter.

It deletes the WINDOW more often, rather than switching to another
buffer in it.  If WINDOW is alone in its frame then the frame is
deleted or iconified, according to option `frame-auto-hide-function'."
    (interactive "P")
    (set-window-parameter window 'quit-restore `(frame frame nil ,(current-buffer)))
    (quit-restore-window window 'kill))

  )

(defvar diredp-recent-files-map (let ((map  (make-sparse-keymap)))
                                        (set-keymap-parent map dired-mode-map)
                                        map)
  "Keymap for Dired buffer listing recently visited files.
By default this is the same as its parent, `dired-mode-map', except
that if `diredp-recent-files-quit-kills-flag' is non-nil then `q' is
bound to `diredp-quit-window-kill' (Emacs 24+).")

(defcustom diredp-recent-files-quit-kills-flag t
  "Non-nil means `q' in Dired recently visited files buffer kills buffer.
Non-nil is convenient if you repeat a command that creates such a
buffer, as it won't add yet another one.  But nil is convenient if you
frequently use `C-x b' to revisit an existing such buffer.

Do not set this option using `setq' or similar.  Use
`customize-option' or `customize-set-variable'."
  :type 'boolean :group 'Dired-Plus
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (when (fboundp 'diredp-quit-window-kill)
           (define-key diredp-recent-files-map [remap quit-window] (and (symbol-value sym)
                                                                        'diredp-quit-window-kill)))))

;;;###autoload
(defun diredp-dired-recent-files (buffer &optional arg files) ; Bound to `C-x D R'
  "Open Dired in BUFFER, showing recently visited files and directories.
You are prompted for BUFFER (default: `Recently Visited Files').

With a numeric prefix arg you can enter names of recent files to
include or exclude.

No prefix arg or a plain prefix arg (`C-u', `C-u C-u', etc.) means
list all of the recently used files.

With a prefix arg:
* If 0, `-', or plain (`C-u') then you are prompted for the `ls'
  switches to use.
* If not plain (`C-u') then:
  * If >= 0 then the files to include are read, one by one.
  * If  < 0 then the files to exclude are read, one by one.

When entering files to include or exclude, use `C-g' to end.

The file listing is sorted by option
`diredp-default-sort-arbitrary-function', if non-nil.  If nil (the
default) then the listing is in reverse chronological order of opening
or writing files you access.

Use \\<dired-mode-map>`\\[revert-buffer]' to revert the buffer, as usual.  If you use it without a
prefix arg then the same files are relisted.  A prefix arg is handled
as for `\\[diredp-dired-recent-files] itself.

When called from Lisp:
 * ARG corresponds to the raw prefix arg.
 * FILES is passed to `diredp--dired-recent-files-1'.  It is used only
   when the command is used as part of the `revert-buffer-function'."
  (interactive (list (completing-read (diredp-replace-dir-sep-in-string "Dired (buffer name): ")
                                      dired-buffers nil nil nil nil "Recently Visited Files")
                     current-prefix-arg))
  (diredp--dired-recent-files-1 buffer arg files))

;;;###autoload
(defun diredp-dired-recent-files-other-window (buffer &optional arg files) ; Bound to `C-x 4 D R'
  "Same as `diredp-dired-recent-files', but use other window."
  (interactive (list (completing-read (diredp-replace-dir-sep-in-string "Dired in other window (buffer name): ")
                                      dired-buffers nil nil nil nil "Recently Visited Files")
                     current-prefix-arg))
  (diredp--dired-recent-files-1 buffer arg files 'OTHER-WINDOW-P))

(defun diredp--dired-recent-files-1 (buffer arg files &optional other-window-p dirs-p)
  "Helper for `diredp-dired-recent-files(-other-window)."
  (unless (require 'recentf nil t) (error "This command requires library `recentf.el'"))
  (let* ((switches  (and (or (zerop (prefix-numeric-value arg))  (consp arg)  (eq '- arg))
                         (read-string "Dired listing switches: " dired-listing-switches)))
         (bufname   (generate-new-buffer-name buffer))
         (fils      (or files  (diredp-sort-arbitrary (if dirs-p #'diredp-recent-dirs #'diredp-recent-files) arg)))
         (rev-fn    `(lambda (_ __)
                       (message "Reverting...")
                       (kill-buffer)
                       (funcall ,(if dirs-p '#'diredp-dired-recent-dirs '#'diredp-dired-recent-files)
                                ',bufname current-prefix-arg (and (not current-prefix-arg)  ',fils))
                       (message "Reverting...done"))))
    (funcall (if other-window-p #'dired-other-window #'dired) (cons bufname fils) switches)
    (with-current-buffer bufname
      (setq diredp-recent-files-buffer  bufname)
      (use-local-map diredp-recent-files-map)
      (when (boundp 'dired-sort-inhibit) (set (make-local-variable 'dired-sort-inhibit) t))
      (set (make-local-variable 'revert-buffer-function) rev-fn))))

(defun diredp-recent-files (arg)
  "Return a list of recently used files and directories.
ARG is as for `diredp-dired-recent-files'."
  (let ((recent-files  (diredp-remove-if #'diredp-root-directory-p (diredp-delete-dups recentf-list))))
    (if (and arg  (atom arg))
        (diredp-read-include/exclude 'File recent-files (not (natnump (prefix-numeric-value arg))))
      recent-files)))

;;;###autoload
(defun diredp-dired-recent-dirs (buffer &optional arg files) ; Bound to `C-x D r'
  "Open Dired in BUFFER, showing recently visited directories.
Like `diredp-dired-recent-files', but limited to recent directories.
A directory is recent if any of its files is recent."
  (interactive (list (completing-read (diredp-replace-dir-sep-in-string "Dired (buffer name): ")
                                      dired-buffers nil nil nil nil "Recently Visited Directories")
                     current-prefix-arg))
  (diredp--dired-recent-files-1 buffer arg files nil 'DIRS-P))

;;;###autoload
(defun diredp-dired-recent-dirs-other-window (buffer &optional arg files) ; Bound to `C-x 4 D r'
  "Same as `diredp-dired-recent-dirs', but use other window."
  (interactive (list (completing-read (diredp-replace-dir-sep-in-string "Dired in other window (buffer name): ")
                                      dired-buffers nil nil nil nil
                                      "Recently Visited Directories")
                     current-prefix-arg))
  (diredp--dired-recent-files-1 buffer arg files 'OTHER-WINDOW-P 'DIRS-P))

(defun diredp-recent-dirs (arg)
  "Return a list of recently used directories.
ARG is as for `diredp-dired-recent-dirs'."
  (let ((recent-dirs  (diredp-remove-if #'diredp-root-directory-p
                                        (diredp-delete-dups
                                         (mapcar (lambda (f/d)
                                                   (if (file-directory-p f/d) f/d (file-name-directory f/d)))
                                                 recentf-list)))))
    (if (and arg  (atom arg))
        (diredp-read-include/exclude 'Dir recent-dirs (not (natnump (prefix-numeric-value arg))))
      recent-dirs)))

;;;###autoload
(defun diredp-do-add-to-recentf (&optional arg) ; Not bound by default
  "Add all marked (or next ARG) files to list of recently used files.
That is, add them from variable `recentf-list'.

\(This does not refresh any Dired buffer listing the recently visited
files.  You can refresh it manually using `\\[revert-buffer]'."
  (interactive "P")
  (unless (require 'recentf nil t) (error "This command requires library `recentf.el'"))
  (diredp-ensure-mode)
  (dired-map-over-marks-check #'diredp-add-file-to-recentf arg 'add\ to\ recentf
                              (diredp-fewer-than-2-files-p arg))
  (diredp-revert-displayed-recentf-buffers))

;;;###autoload
(defun diredp-do-remove-from-recentf (&optional arg) ; Not bound by default
  "Remove all marked (or next ARG) files from list of recently used files.
That is, remove them from variable `recentf-list'.

\(This does not refresh any Dired buffer listing the recently visited
files.  You can refresh it manually using `\\[revert-buffer]'."
  (interactive "P")
  (unless (require 'recentf nil t) (error "This command requires library `recentf.el'"))
  (diredp-ensure-mode)
  (dired-map-over-marks-check #'diredp-remove-file-from-recentf arg 'remove\ from\ recentf
                              (diredp-fewer-than-2-files-p arg))
  (diredp-revert-displayed-recentf-buffers))

;;;###autoload
(defun diredp-add-this-to-recentf ()
  "In Dired, add this file to the list of recently used files."
  (interactive)
  (let ((use-file-dialog  nil)) (diredp-do-add-to-recentf 1)))

;;;###autoload
(defun diredp-remove-this-from-recentf ()
  "In Dired, remove this file from the list of recently used files."
  (interactive)
  (let ((use-file-dialog  nil)) (diredp-do-remove-from-recentf 1)))

;;;###autoload
(defun diredp-add-file-to-recentf (&optional file interactivep) ; Not bound by default
  "Add FILE to the front of `recentf-list'.
If FILE is already present, move it to the front of the list.
In Dired, FILE defaults to the file of the current Dired line."
  (interactive "fFile: \np")
  (setq file  (or file  (and (derived-mode-p 'dired-mode)  (dired-get-file-for-visit))))
  (unless file (error "No file"))
  (recentf-push (recentf-expand-file-name file))
  (when interactivep (diredp-revert-displayed-recentf-buffers))
  nil)                                  ; Return nil for success (cannot fail).

;;;###autoload
(defun diredp-remove-file-from-recentf (&optional file interactivep) ; Not bound by default
  "Remove FILE from `recentf-list'.
In Dired, FILE defaults to the file of the current Dired line.

Interactively: After removing, revert any displayed buffers showing
`recentf-list' (or part of it)."
  (interactive "fFile: \np")
  (setq file  (or file  (and (derived-mode-p 'dired-mode)  (dired-get-file-for-visit))))
  (unless file (error "No file"))
  (let ((memb  (recentf-string-member (recentf-expand-file-name file) recentf-list)))
    (and memb  (setq recentf-list  (delq (car memb) recentf-list))))
  (when interactivep (diredp-revert-displayed-recentf-buffers))
  nil)                                  ; Return nil for success (cannot fail).

(defun diredp-revert-displayed-recentf-buffers ()
  "Revert displayed Dired buffers showing `recentf-list' (or part of it)."
  (let (win)
    (dolist (buf  (buffer-list))
      (when (setq win  (get-buffer-window buf 0))
        (select-window win)
        (when (and (derived-mode-p 'dired-mode)
                   (local-variable-if-set-p 'diredp-recent-files-buffer)
                   diredp-recent-files-buffer)
          (revert-buffer))))))

(defun diredp-read-include/exclude (thing things &optional exclude keep-duplicates-p)
  "Read which THINGs to include (or to EXCLUDE, if non-nil) from list THINGS.
The things are read one by one.  `C-g' stops reading.

THING is a string or symbol naming the type of thing to read, e.g.,
`File' or `Directory'.  It is used only in the prompt, which is THING
followed by \" to exclude\" or \" to include\" and a reminder about `C-g'.

A new list is returned - list THINGS is not modified.

Unless optional arg KEEP-DUPLICATES-P is non-nil, remove duplicates,
keeping only the first of a set of `equal' THINGS."
  (let* ((thgs                    (if exclude (copy-sequence things) ()))
         (prompt                  (format "%s to %s (C-g when done): " thing (if exclude 'EXCLUDE 'INCLUDE)))
         (completion-ignore-case  (or (and (boundp 'read-file-name-completion-ignore-case)
                                           (memq thing '(Dir Directory File "Dir" "Directory" "File")) ; Hack
                                           read-file-name-completion-ignore-case)
                                      completion-ignore-case))
         thing)
    (while (condition-case nil
               (setq thing  (completing-read prompt (mapcar #'list things) nil t))
             (quit nil))
      (if exclude (delete thing thgs)
        (push thing thgs)))
    (unless keep-duplicates-p (setq thgs  (diredp-delete-dups thgs)))
    thgs))

;;; $$$$$$$$
;;; (defun diredp-dired-files-interactive-spec (str)
;;;   "`interactive' spec for `diredp-dired-files' commands.
;;; STR is a string appended to the prompt.
;;; With non-negative prefix arg, read switches.
;;; With non-positive prefix arg, read files and dirs to list and then the
;;;  Dired buffer name.  User uses `C-g' when done reading files and dirs.

;;; If you use Icicles, then in Icicle mode the following keys are bound
;;; in the minibuffer during completion (`*' means the key requires
;;; library `Bookmark+'):

;;;   M-|       - Open Dired on the file names matching your input
;;;   C-c +     - Create a new directory
;;;  *C-x a +   - Add tags to the current-candidate file
;;;  *C-x a -   - Remove tags from the current-candidate file
;;;  *C-x m     - Access file bookmarks (not just autofiles)"
;;;   (list
;;;    (unwind-protect
;;;         (let ((icicle-sort-comparer  (or (and (boundp 'icicle-file-sort) ;; If not reading files
;;;                                               icicle-file-sort)          ;; then dirs first.
;;;                                          (and (> (prefix-numeric-value current-prefix-arg) 0)
;;;                                               'icicle-dirs-first-p)
;;;                                          icicle-sort-comparer))
;;;               (icicle-all-candidates-list-alt-action-fn ; M-|'
;;;                (lambda (files)
;;;                  (let ((enable-recursive-minibuffers  t))
;;;                    (dired-other-window (cons (read-string "Dired buffer name: ") files))))))
;;;           (when (fboundp 'icicle-bind-file-candidate-keys) (icicle-bind-file-candidate-keys))
;;;           (if (> (prefix-numeric-value current-prefix-arg) 0)
;;;               ;; If a dialog box is about to be used, call `read-directory-name' so the dialog
;;;               ;; code knows we want directories.  Some dialog boxes can only select directories
;;;               ;; or files when popped up, not both.
;;;               (if (and (fboundp 'read-directory-name)  (next-read-file-uses-dialog-p))
;;;                   (read-directory-name (format "Dired %s(directory): " str) nil
;;;                                        default-directory nil)
;;;                 (read-file-name (format "Dired %s(directory): " str) nil default-directory nil))
;;;             (let ((insert-default-directory  nil)
;;;                   (files                     ())
;;;                   file)
;;;               (while (condition-case nil ; Use lax completion, to allow wildcards.
;;;                          (setq file  (read-file-name "File or dir (C-g when done): "))
;;;                        (quit nil))
;;;                 (push file files))
;;;               (cons (read-string "Dired buffer name: " nil nil default-directory) files))))
;;;      (when (fboundp 'icicle-unbind-file-candidate-keys)
;;;        (icicle-unbind-file-candidate-keys)))
;;;    (and current-prefix-arg  (natnump (prefix-numeric-value current-prefix-arg))
;;;         (read-string "Dired listing switches: " dired-listing-switches))))

;;;###autoload
(defun diredp-sort-arbitrary-command (sort-function)
  "Sort the current Dired listing of arbitary files.

NOTE:
 This is intended for sorting a list of arbitrary file names, not an
 ordinary Dired listing such as produced by `ls'.

 If the current Dired buffer is *not* already a list of arbitrary file
 names then you are prompted to confirm converting it to such a list.
 If you do so it will no longer use `ls' or `dired-listing-switches'.
 Don't confirm, if you don't want this conversion."
  (interactive
   (if (and (atom dired-directory)
            (not (y-or-n-p "Convert Dired buffer to a list of arbitrary file names? ")))
       (error "OK, canceled")
     (let* ((cands  '(("2: Reverse"              . t)
                      ("3: Nondir Name, A-to-Z"  . diredp-nondir-file-name-less-p)
                      ("4: Nondir Name, Z-to-A"  . diredp-nondir-file-name-more-p)
                      ("5: Full Name, A-to-Z"    . diredp-full-file-name-less-p)
                      ("6: Full Name, Z-to-A"    . diredp-full-file-name-more-p)))
            (order  (completing-read "Sort order: " cands nil t)))
       (list (cdr (assoc order cands))))))
  (let ((buf    (buffer-name))
        (files  (copy-sequence (dired-get-marked-files nil '(64)))))
    (kill-buffer)
    (dired (cons buf (if sort-function
                         (if (functionp sort-function)
                             (sort files sort-function)
                           (nreverse files))
                       files))
           dired-actual-switches)))

;;;###autoload
(defun diredp-dired-union (dired-name dirbufs &optional switches extra) ; Bound to `C-x D U'
  "Create a Dired buffer that is the union of some existing Dired buffers.
With a non-negative prefix arg, you are prompted for `ls' switches.
With a non-positive prefix arg, you are prompted for file and dir
names to add to the listing - see below.

You are prompted for the name of the Dired union buffer.  Completion
against names of existing Dired buffers is available, but you can
enter any other name to create a new Dired buffer of that name.

If the union buffer name you choose names an existing Dired buffer,
then what happens depends on whether that buffer is an ordinary Dired
directory listing or a list of arbitrary file names.  That is, it
depends on whether `dired-directory' is a directory name or a cons of
a Dired buffer name plus file names.

* If the buffer is an ordinary Dired listing, then it is converted to
  an explicit list of absolute file names, just as if these had been
  chosen individually.  The existing buffer and window are replaced by
  new ones that show the explicit listing.  (This replacement is
  necessary because the list of files contained in an ordinary Dired
  listing cannot be modified.)

* If the buffer lists arbitrary file names explicitly, then it is
  updated to include also the files from any Dired buffers and any
  additional files that you specify.

If the union buffer name you choose does not name an existing Dired
buffer, then its `default-directory' is the same as the
`default-directory' before invoking the command.

If you use a non-positive prefix arg, then you can next choose
additional file and directory names to add to the listing.  Use `C-g'
when done choosing them.

Any directory names you choose this way are included as single entries
in the listing - the directory contents are not included (these
directories are not unioned).  To instead include the contents of a
directory chosen this way, use a glob pattern: `/*' after the
directory name.

You are then prompted for the Dired buffers to union.  Use `C-g' when
done choosing them.  These Dired listings to union are included in the
order that you chose them, and each entry is listed only once in the
new Dired buffer.

The new Dired listing respects the markings, subdirectory insertions,
and hidden subdirectories of the selected Dired listings.  However, in
case of conflict between marked or unmarked status for the same entry,
the entry is marked.  Similarly, in case of conflict over an included
subdirectory between it being hidden or shown, it is hidden, but its
contained files are also listed.

See also command `diredp-add-to-dired-buffer'.

From Lisp:
 DIRED-NAME is the name of the resulting Dired union buffer.
 DIRBUFS is a list of the names of Dired buffers to union.
 SWITCHES is a string of `ls' switches.
 EXTRA is a list of files & directories to be included in the listing."
  (interactive (diredp-dired-union-interactive-spec "UNION "
                                                    nil
                                                    (and current-prefix-arg
                                                         (<= (prefix-numeric-value current-prefix-arg) 0))))
  (diredp-dired-union-1 dired-name dirbufs switches extra))

;;;###autoload
(defun diredp-dired-union-other-window (dired-name dirbufs &optional switches extra) ; Bound to `C-x 4 D U'
  "Same as `diredp-dired-union', except use other window."
  (interactive (diredp-dired-union-interactive-spec "UNION "
                                                    nil
                                                    (and current-prefix-arg
                                                         (<= (prefix-numeric-value current-prefix-arg) 0))))
  (diredp-dired-union-1 dired-name dirbufs switches extra 'OTHERWIN))

;;;###autoload
(defun diredp-add-to-dired-buffer (dired-name to-add &optional switches) ; Bound to `C-x D A'
  "Add individual file and directory names to a Dired buffer.
You are prompted for the buffer name.
With a prefix arg, you are also prompted for the `ls' switches.

The buffer must either not exist yet or must list arbitrary file and
directory names.  That is, it cannot be an ordinary Dired directory
listing - those cannot be modified.

Any directory names you choose this way are included as single entries
in the listing - the directory contents are not included (these
directories are not unioned).  To instead include the contents of a
directory chosen this way, use a glob pattern: `/*' after the
directory name.

See also command `diredp-dired-union'.

From Lisp:
 DIRED-NAME is the name of the Dired buffer to modify.
 TO-ADD is the list of files and dirs to add to it.
 SWITCHES is the string of `ls' switches."
  ;; Bind `current-prefix-arg' to force reading file/dir names.
  ;; Read `ls' switches too, if user used prefix arg.
  (interactive
   (let* ((current-prefix-arg  (if current-prefix-arg 0 -1))
          (all                 (diredp-dired-union-interactive-spec "add files/dirs "
                                                                    'NO-DIRED-BUFS
                                                                    'READ-EXTRA-FILES-P)))
     (list (nth 0 all) (nth 3 all) (nth 2 all))))
  (diredp-dired-union-1 dired-name () switches to-add))

;;;###autoload
(defun diredp-add-to-dired-buffer-other-window (dired-name to-add &optional switches) ; Bound to `C-x 4 D A'
  "Same as `diredp-add-to-dired-buffer', except use other window."
  ;; Bind `current-prefix-arg' to force reading file/dir names.
  ;; Read `ls' switches too, if user used prefix arg.
  (interactive
   (let* ((current-prefix-arg  (if current-prefix-arg 0 -1))
          (all                 (diredp-dired-union-interactive-spec "add files/dirs "
                                                                    'NO-DIRED-BUFS
                                                                    'READ-EXTRA-FILES-P)))
     (list (nth 0 all) (nth 3 all) (nth 2 all))))
  (diredp-dired-union-1 dired-name () switches to-add 'OTHERWIN))

;;;###autoload
(defun diredp-add-to-this-dired-buffer (dired-name to-add &optional switches) ; Not bound by default
  "Same as `diredp-add-to-dired-buffer' for this Dired buffer."
  ;; Bind `current-prefix-arg' to force reading file/dir names.
  ;; Read `ls' switches too, if user used prefix arg.
  (interactive
   (progn (diredp-ensure-mode)
          (let* ((current-prefix-arg  (if current-prefix-arg 0 -1))
                 (all                 (diredp-dired-union-interactive-spec "add files/dirs here "
                                                                           'NO-DIRED-BUFS
                                                                           'READ-EXTRA-FILES-P
                                                                           (buffer-name))))
            (list (nth 0 all) (nth 3 all) (nth 2 all)))))
  (diredp-dired-union-1 dired-name () switches to-add))

(defun diredp-dired-union-1 (dired-name dirbufs switches extra &optional otherwin)
  "Helper for `diredp-dired-union' and `diredp-add-to-dired-buffer'.
Non-nil optional OTHERWIN means use other window for the Dired buffer.
See `diredp-dired-union' for the other argument descriptions."
  (let ((dbuf         (get-buffer dired-name))
        (files        extra)
        (marked       ())
        (subdirs      ())
        (hidden-dirs  ())
        hid-here files-here)
    (dolist (buf  (reverse dirbufs))
      (with-current-buffer buf
        (unwind-protect
            (progn (setq hid-here    (save-excursion (dired-remember-hidden))
                         files-here  (if (consp dired-directory)
                                         (reverse (cdr dired-directory)) ; Reverse bc will push.
                                       ()))
                   (unless files-here
                     (save-excursion ; This bit is more or less from `dired-toggle-marks'.
                       (goto-char (point-min))
                       (while (not (eobp))
                         (or (diredp-looking-at-p dired-re-dot)
                             (push (dired-get-filename nil 'NO-ERROR) files-here))
                         (forward-line 1)))
                     (setq files-here  (delq nil files-here)))
                   (dolist (hid-here  hid-here) (push hid-here hidden-dirs))
                   (dolist (sub  (cdr (reverse dired-subdir-alist)))
                     (push (list (car sub)) subdirs))
                   (dolist (mkd  (dired-remember-marks (point-min) (point-max))) ; This unhides.
                     (push (car mkd) marked))
                   (dolist (file  files-here)
                     (when (or (not (file-name-absolute-p file))  (not (member file files)))
                       (push file files))))
          (save-excursion             ; Hide subdirs that were hidden.
            (dolist (dir  hid-here)  (when (dired-goto-subdir dir) (dired-hide-subdir 1)))))))
    ;; For an existing Dired buffer having this name whose `dired-directory' is a cons:
    ;; 1. Include the files and dirs already listed there.
    ;; 2. Kill the current buffer and delete its window.  A new buffer of the same name is created and shown.
    (when dbuf
      (with-current-buffer dbuf
        (when (consp dired-directory) (setq files  (diredp-set-union (cdr dired-directory) files)))
        (let ((win  (get-buffer-window dbuf 0))) (when win (delete-window win)))
        (kill-buffer dbuf)))
    (setq dbuf  (dired-other-window (cons dired-name files) switches))
    (with-current-buffer dbuf
      (let ((inhibit-read-only  t))
        (dired-insert-old-subdirs subdirs)
        (dired-mark-remembered ; Don't really need `expand-file-name' - already abs.
         (mapcar (lambda (mf) (cons (expand-file-name mf dired-directory) 42)) marked))
        (save-excursion
          (dolist (hdir  hidden-dirs)  (when (dired-goto-subdir hdir) (dired-hide-subdir 1))))
        (when (boundp 'dired-sort-inhibit) (set (make-local-variable 'dired-sort-inhibit) t))))))

(defun diredp-dired-union-interactive-spec (string &optional no-dired-bufs read-extra-files-p dired-buffer)
  "Read arguments for `diredp-dired-union' and `diredp-add-to-dired-buffer'.
STRING is appended to the prompt for the listing buffer name.
Non-nil NO-DIRED-BUFS means do not read Dired buffers to union.
Non-nil READ-EXTRA-FILES-P is passed to `dired-read-dir-and-switches',
 and means read extra files to add to the listing.
Non-nil DIRED-BUFFER is passed to `dired-read-dir-and-switches'.
  It is the name of the Dired union buffer."
  (let* ((current-prefix-arg  -1)
         (dir+switches        (dired-read-dir-and-switches string read-extra-files-p dired-buffer))
         (dirname             (car  dir+switches))
         (switches            (cadr dir+switches))
         (dirbufs             ())
         (bufs                ())
         (extra-files         ())
         buf)
    (when (consp dirname) (setq extra-files  (cdr dirname)
                                dirname      (car dirname)))
    (unless no-dired-bufs
      ;; Remove any killed buffers from `dired-buffers'.  Then use all but the target buffer as candidates.
      (dolist (db  dired-buffers)
        (if (buffer-live-p (cdr db))
            (unless (equal dirname (buffer-name (cdr db)))
              (push (cons (buffer-name (cdr db)) (car db)) dirbufs))
          (setq dired-buffers  (delq db dired-buffers))))
      (while (and dirbufs  (condition-case nil
                               (setq buf  (completing-read "Existing Dired buffer to include (C-g when done): "
                                                           dirbufs nil t nil 'buffer-name-history
                                                           (and dirbufs  (car (assoc (buffer-name) dirbufs)))))
                             (quit nil)))
        (push buf bufs)
        (setq dirbufs  (delete (cons buf (with-current-buffer buf (expand-file-name default-directory)))
                               dirbufs)))
      (setq bufs  (nreverse bufs)))
    (list dirname bufs switches extra-files)))


(defvar diredp-saved-markings ()
  "Markings saved from a Dired buffer.
This includes markings with any character, not just `*'.")

(defun diredp-save-markings (&optional localp msgp)
  "Save all markings in this Dired buffer.
With a prefix arg, save only the nondir file names."
  (interactive "P\np")
  (diredp-ensure-mode)
  (setq diredp-saved-markings  (dired-remember-marks (point-min) (point-max) localp))
  (when msgp (message "Markings saved%s" (if localp " with nondir file names" ""))))

(defun diredp-restore-markings (&optional clear-first-p msgp)
  "Restore markings in this Dired buffer from those last saved.
With a prefix arg, *remove* all markings here first, so the only
markings are those that were saved.

Ignore any saved markings for files that are not in the current listing."
  (interactive "P\np")
  (diredp-ensure-mode)
  (let ((inhibit-read-only  t))
    (when clear-first-p (dired-unmark-all-marks))
    (dired-mark-remembered diredp-saved-markings))
  (when msgp (message "Saved markings restored%s" (if clear-first-p ", after removing all"))))


(when (> emacs-major-version 23)        ; `compilation--loc->file-struct'

  (defalias 'diredp-grepped-files-other-window 'diredp-compilation-files-other-window)
  (defun diredp-compilation-files-other-window (&optional switches)
    "Open Dired on the files indicated by compilation (e.g., `grep') hits.
Applies to any `compilation-mode'-derived buffer, such as `*grep*'.
You are prompted for the name of the new Dired buffer.
With a prefix arg you are first prompted for the `ls' switches.

\(However, Emacs bug #20739 means that the switches are ignored.)"
    (interactive (list (and current-prefix-arg  (read-string "Dired listing switches: " dired-listing-switches))))
    (unless (compilation-buffer-p (current-buffer)) (error "Not in a buffer derived from `compilation-mode'"))
    (let ((files  ())
          bufname)
      (save-excursion (goto-char (point-min))
                      (while (condition-case nil (compilation-next-file 1) (error nil))
                        (setq compilation-current-error  (point))
                        (push (diredp-file-for-compilation-hit-at-point) files)))
      (setq files  (nreverse files))
      (dired-other-window
       (cons (setq bufname  (read-string "Dired buffer name: "
                                         nil nil (generate-new-buffer-name default-directory)))
             files)
       switches)
      (with-current-buffer bufname
        (when (boundp 'dired-sort-inhibit) (set (make-local-variable 'dired-sort-inhibit) t)))))

  (defun diredp-file-for-compilation-hit-at-point ()
    "Return the name of the file for the compilation hit at point.
The name is expanded in the directory for the last directory change."
    (let* ((msg         (compilation-next-error 0))
           (loc         (compilation--message->loc msg))
           (filestruct  (compilation--loc->file-struct loc))
           (file        (caar filestruct))
           (dir         (cadr (car filestruct))))
      (when dir (setq file  (expand-file-name file dir)))
      file))
  )

;;;###autoload
(defun diredp-fileset (flset-name)      ; Bound to `C-x D S'
  "Open Dired on the files in fileset FLSET-NAME."
  (interactive
   (progn (unless (require 'filesets nil t) (error "Feature `filesets' not provided"))
          (unless filesets-data (error "`filesets-data' is empty"))
          (list (completing-read "Open Dired on fileset: " filesets-data))))
  (diredp-fileset-1 flset-name))

;;;###autoload
(defun diredp-fileset-other-window (flset-name) ; Bound to `C-x 4 D S'
  "Open Dired in another window on the files in fileset FLSET-NAME."
  (interactive
   (progn (unless (require 'filesets nil t) (error "Feature `filesets' not provided"))
          (unless filesets-data (error "`filesets-data' is empty"))
          (list (completing-read "Open Dired on fileset, in other window: " filesets-data))))
  (diredp-fileset-1 flset-name 'OTHER-WINDOW))

(defun diredp-fileset-1 (flset-name &optional other-window-p)
  "Helper for `diredp-fileset(-other-window)'."
  (let ((flset   (filesets-get-fileset-from-name flset-name))
        (files   ())
        (mode    nil)
        (dirfun  (if other-window-p #'dired-other-window #'dired))
        bufname)
    (unless (or (setq mode  (filesets-entry-mode flset)) ; ("my-fs" (:files "a" "b"))
                (setq flset  (cons "dummy" flset) ; (:files "a" "b")
                      mode   (filesets-entry-mode flset)))
      (error "Bad fileset: %S" flset-name))
    (message "Gathering file names...")
    (dolist (file  (filesets-get-filelist flset mode)) (push file files))
    (funcall dirfun (cons (setq bufname  (generate-new-buffer-name flset-name))
                          (nreverse (mapcar (lambda (file)
                                              (if (file-name-absolute-p file)
                                                  (expand-file-name file)
                                                file))
                                            files))))
    (with-current-buffer bufname
      (when (boundp 'dired-sort-inhibit) (set (make-local-variable 'dired-sort-inhibit) t)))))

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
  (let* ((this-dir       (expand-file-name default-directory))
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
(defun diredp-dired-inserted-subdirs (&optional no-show-p msgp) ; Bound to `C-M-i'
  "Open Dired for each of the subdirs inserted in this Dired buffer.
A separate Dired buffer is used for each of them.
With a prefix arg, create the Dired buffers but do not display them.
Markings and current Dired switches are preserved."
  (interactive "P\np")
  (diredp-ensure-mode)
  (let ((this-dir    (expand-file-name default-directory))
        (this-buff   (current-buffer))
        (this-frame  (selected-frame))
        marked)
    (unwind-protect
         (save-selected-window
           (dolist (entry  dired-subdir-alist)
             (unless (string= (car entry) this-dir)
               (setq marked  (with-current-buffer this-buff
                               (dired-remember-marks (cdr entry) (dired-get-subdir-max entry))))
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

(defun diredp-get-subdirs (&optional ignore-marks-p predicate details)
  "Return subdirs from this Dired buffer and from marked subdirs, recursively.
If optional arg IGNORE-MARKS-P is non-nil then include all
subdirectories.  Otherwise, include only those that are marked.

Non-nil optional arg PREDICATE means include only subdirectory names
for which the PREDICATE returns non-nil.  PREDICATE must accept a file
name as its only required argument.

Optional arg DETAILS is passed to `diredp-get-files'."
  (diredp-get-files ignore-marks-p (if predicate
                                       `(lambda (name) (and (file-directory-p name)  (funcall ,predicate name)))
                                     #'file-directory-p)
                    'INCLUDE-DIRS-P 'DONT-ASKP 'ONLY-MARKED-P details))

(defun diredp-get-files (&optional ignore-marks-p predicate include-dirs-p dont-askp only-marked-p details)
  "Return file names from this Dired buffer and subdirectories, recursively.
The names are those that are marked in the current Dired buffer, or
all files in the directory if none are marked.  Marked subdirectories
are handled recursively in the same way.

If there is some included subdirectory that has a Dired buffer with
marked files, then (unless DONT-ASKP is non-nil) this asks you whether
to use the marked files in Dired buffers, as opposed to using all of
the files in included directories.  To this y-or-n question you can
hit `l' to see the list of files that will be included (using
`diredp-list-files').  In that `l' listing you can mouseover to see
image-file previews or use `RET' or `mouse-2' to visit files.

\(Directories in `icicle-ignored-directories' are skipped, if you use
Icicles.  Otherwise, directories in `vc-directory-exclusion-list' are
skipped.)

Non-nil IGNORE-MARKS-P means ignore all Dired markings: just get all
of the files in the current directory (and all of the subdirectories,
if INCLUDE-DIRS-P is non-nil).

Non-nil PREDICATE means include only file names for which the
PREDICATE returns non-nil.  PREDICATE must accept a file name as its
only required argument.

Non-nil INCLUDE-DIRS-P means include marked subdirectory names (but
also handle those subdirs recursively, picking up their marked files
and subdirs).

Non-nil DONT-ASKP means do not ask the user whether to use marked
instead of all.  Act as if the user was asked and replied `y'.

Non-nil optional arg ONLY-MARKED-P means collect only marked files,
instead of collecting all files if none are marked.  This argument is
ignored if IGNORE-MARKS-P is non-nil.

Optional arg DETAILS is passed to `diredp-y-or-n-files-p'."
  (let ((askp  (list nil)))             ; The cons's car will be set to `t' if need to ask user.
    (if ignore-marks-p
        (diredp-files-within (directory-files default-directory 'FULL diredp-re-no-dot)
                             () nil include-dirs-p predicate)
      ;; Pass FILES and ASKP to `diredp-get-files-for-dir', so we don't have to use them as
      ;; free vars there.  But that means that they each need to be a cons cell that we can
      ;; modify, so we can get back the updated info.
      (let ((files  (list 'DUMMY)))     ; The files picked up will be added to this list.
        (diredp-get-files-for-dir default-directory files askp include-dirs-p only-marked-p)
        (setq files  (cdr files))       ; Remove `DUMMY' from the modifed list.
        (if (or dont-askp
                (not (car askp))
                (diredp-y-or-n-files-p "Use marked (instead of all) in subdir Dired buffers? "
                                       files predicate details))
            (if predicate (diredp-remove-if-not predicate files) files)
          (setq files  ())
          (dolist (file  (diredp-marked-here))
            (if (not (file-directory-p file))
                (when (or (not predicate)  (funcall predicate file))
                  (add-to-list 'files file))
              (when include-dirs-p (setq files  (nconc files (list file))))
              (setq files  (nconc files (diredp-files-within (directory-files file 'FULL diredp-re-no-dot)
                                                             () nil include-dirs-p predicate)))))
          (nreverse files))))))

(defun diredp-get-files-for-dir (directory accum askp &optional include-dirs-p only-marked-p)
  "Return marked file names for DIRECTORY and subdirectories, recursively.
Pick up names of all marked files in DIRECTORY if it has a Dired
buffer, or all files in DIRECTORY if not.  Handle subdirs recursively
\(only marked subdirs, if Dired).

ACCUM is an accumulator list: the files picked up in this call are
nconc'd to it.

ASKP is a one-element list, the element indicating whether to ask the
user about respecting Dired markings.  It is set here to `t' if there
is a Dired buffer for DIRECTORY.

Non-nil optional arg INCLUDE-DIRS-P means include marked subdirectory
names (but also handle those subdirs recursively).

Non-nil optional arg ONLY-MARKED-P means collect only marked files,
instead of collecting all files if none are marked.

If there is more than one Dired buffer for DIRECTORY then raise an
error."
  (let ((dbufs  (dired-buffers-for-dir directory)))
    (dolist (file  (if (not dbufs)
                       (and (not only-marked-p)  (directory-files directory 'FULL diredp-re-no-dot))
                     (when (cadr dbufs) (error "More than one Dired buffer for `%s'" directory))
                     (unless (equal directory default-directory) (setcar askp  t))
                     (with-current-buffer (car dbufs) (diredp-marked-here only-marked-p 'NO-DOT-DOT))))
      (if (not (file-directory-p file))
          (setcdr (last accum) (list file))
        (when include-dirs-p (setcdr (last accum) (list file)))
        (diredp-get-files-for-dir file accum askp include-dirs-p only-marked-p)))))

(defun diredp-marked-here (&optional only-marked-p no-dot-dot-p)
  "Marked files and subdirs in this Dired buffer, or all if none are marked.
Non-nil optional arg ONLY-MARKED-P means return nil if none are
marked.
Non-nil optional arg NO-DOT-DOT-P means do not include marked `..'."
  ;; If no file is marked, exclude `(FILENAME)': the unmarked file at cursor.
  ;; If there are no marked files as a result, return all files and subdirs in the dir.
  (let* ((dired-marker-char  ?*)
         (ff                 (condition-case nil ; Ignore error if on `.' or `..' and no file is marked.
                                 (dired-get-marked-files
                                  nil nil (and no-dot-dot-p
                                               (lambda (mf) (not (diredp-string-match-p "/\\.\\.$" mf))))
                                  'DISTINGUISH-ONE-MARKED)
                               (error nil))))
    (cond ((eq t (car ff))  (cdr ff))   ; Single marked
          ((cadr ff)        ff)         ; Multiple marked
          (t                (and (not only-marked-p) ; None marked
                                 (directory-files default-directory 'FULL diredp-re-no-dot 'NOSORT))))))

(defun diredp-y-or-n-files-p (prompt files &optional predicate details)
  "PROMPT user with a \"y or n\" question about a list of FILES.
Return t if answer is \"y\".  Otherwise, return nil.

Like `y-or-n-p', but you can also hit `l' to display the list of files
that the confirmation is for, in buffer `*Files'.  In that `'l'
listing you can mouseover to see image-file previews or use `RET' or
`mouse-2' to visit files.

When finished, buffer `*Files*' is killed if it was never shown, or is
hidden and buried otherwise.  Thus, if it was shown then it is still
available to revisit afterward (even if you quit using `C-g').

PREDICATE is passed to `diredp-list-files', to list only file names
for which it returns non-nil.

DETAILS is passed to `diredp-list-files', to show details about FILES."
  (let ((answer     'recenter)
        (orig-echo  (current-message)))
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
                   (while (let* ((reprompt-actions  '(recenter scroll-up scroll-down
                                                               scroll-other-window scroll-other-window-down))
                                 (key               (let ((cursor-in-echo-area  t))
                                                      (when minibuffer-auto-raise
                                                        (raise-frame (window-frame (minibuffer-window))))
                                                      (if (fboundp 'read-key)
                                                          (read-key (propertize
                                                                     (if (memq answer reprompt-actions)
                                                                         prompt
                                                                       (concat "Please answer y or n.  " prompt))
                                                                     'face 'minibuffer-prompt))
                                                        (read-char-exclusive
                                                         (if (memq answer reprompt-actions)
                                                             prompt
                                                           (concat "Please answer y or n.  " prompt)))))))
                            (setq answer  (lookup-key query-replace-map (vector key) t))
                            (case answer
                              ((skip  act)              nil)
                              (recenter                 (recenter) t)
                              (show                     (diredp-list-files files nil list-buf predicate details)
                                                        (setq list-was-shown  t)) ; Record showing it.
                              (help                     (message "Use `l' to show file list") (sit-for 1))
                              (scroll-up                (condition-case nil (scroll-up-command) (error nil)) t)
                              (scroll-down              (condition-case nil (scroll-down-command) (error nil)) t)
                              (scroll-other-window      (condition-case nil (scroll-other-window) (error nil)) t)
                              (scroll-other-window-down (condition-case nil (scroll-other-window-down nil)
                                                          (error nil)) t)
                              ((exit-prefix  quit)      (signal 'quit nil) t)
                              (t (or (not (eq key ?\e))  (progn (signal 'quit nil) t)))))
                     (ding)
                     (discard-input)))
               (when (get-buffer list-buf)
                 (save-window-excursion (pop-to-buffer list-buf)
                                        (condition-case nil ; Ignore error if user already deleted.
                                            (if (one-window-p) (delete-frame) (delete-window))
                                          (error nil))
                                        (if list-was-shown (bury-buffer list-buf) (kill-buffer list-buf))))
               (define-key query-replace-map "l" nil)))))
    (let ((ret  (eq answer 'act)))
      (unless noninteractive
        (message "%s%c" prompt (if ret ?y ?n)) (sit-for 1)
        (let ((message-log-max  nil))
          (if orig-echo  (message "%s" orig-echo) (message ""))))
      ret)))

(defvar diredp-list-files-map
  (let ((map  (make-sparse-keymap)))
    (define-key map "q"       'quit-window)
    (define-key map "\r"      'diredp-find-line-file-other-window)
    (define-key map [mouse-2] 'diredp-mouse-find-line-file-other-window)
    map)
  "Keymap for `diredp-list-files' output.")
(fset 'diredp-list-files-map diredp-list-files-map)

;;;###autoload
(defun diredp-find-line-file-other-window ()
  "Visit file named by current line, in another window.
The full text of the line is used as the file name."
  (interactive)
  (let ((file  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (when file (find-file-other-window file))))

;;;###autoload
(defun diredp-mouse-find-line-file-other-window (e)
  "Visit file named by clicked line, in another window.
The full text of the line is used as the file name."
  (interactive "e")
  (save-excursion (mouse-set-point e) (diredp-find-line-file-other-window)))

;;;###autoload
(defun diredp-list-marked (&optional arg predicate interactivep details) ; Bound to `C-M-l'
  "List the marked files in this Dired buffer.
A prefix arg specifies files to use instead of the marked files:

 * Numeric prefix arg N: The next N files (previous -N, if < 0).
 * C-u C-u: All files, but no directories.
 * C-u C-u C-u: All files and directories, except `.' and `..'
 * C-u C-u C-u C-u: All files and directories, including `.' and `..'
 * Any other prefix arg: The current line's file only.

You can use `RET' or `mouse-2' to visit any of the files.
If `tooltip-mode' is on then moving the mouse over image-file names
shows image previews.

When called from Lisp:
 Non-nil optional arg PREDICATE is a file-name predicate.  List only
  the files for which it returns non-nil.
 Non-nil optional arg DETAILS is passed to `diredp-list-files'."
  (interactive (progn (diredp-ensure-mode) (list current-prefix-arg nil t diredp-list-file-attributes)))
  (let ((files  (dired-get-marked-files nil arg predicate nil interactivep)))
    (diredp-list-files files nil nil nil details)))

(defun diredp-list-files (files &optional dir bufname predicate details)
  "Display FILES, a list of file names.  Wildcard patterns are expanded.
The files are shown in a new buffer, `*Files*' by default.

Optional arg DIR serves as the default directory for expanding file
 names that are not absolute.  It defaults to `default-directory'.

Optional arg BUFNAME is the name of the buffer for the display.
 It defaults to `*Files*' (or `*Files*<N>' if `*Files*' exists).

Optional arg PREDICATE is a predicate used to filter FILES: only files
 satisfying PREDICATE are listed.

Non-nil arg DETAILS means show details about each file, in addition to
the file name.  It is passed to `diredp-list-file' (which see).

File names listed are absolute.  Mouseover gives help or an image-file
preview, and you can use `RET' or `mouse-2' to visit files."
  (unless bufname (setq bufname  (generate-new-buffer-name "*Files*")))
  (diredp-with-help-window
   bufname
   (princ "Files\n-----\n\n")
   (let ((all-files-no-wildcards  ())
         file-alist file-dir)
     (dolist (file  files)
       (unless (or (string= file "")    ; Ignore empty file names.
                   (and predicate  (not (funcall predicate file))))
         (if (not (diredp-string-match-p "[[?*]" file))
             (add-to-list 'all-files-no-wildcards (diredp-list-file file details))
           (setq file-dir    (or (file-name-directory file)  default-directory)
                 file-alist  (directory-files-and-attributes file-dir 'FULL "[[?*]" 'NOSORT))
           (dolist (ff  file-alist)
             (add-to-list 'all-files-no-wildcards (diredp-list-file file details))))))
     (save-excursion (dolist (fff  (nreverse all-files-no-wildcards))
                       (princ fff) (terpri)))))
  (with-current-buffer bufname
    (let ((buffer-read-only  nil))
      (save-excursion
        (goto-char (point-min))
        (forward-line 3)
        (while (not (eobp))
          (add-text-properties (line-beginning-position) (line-end-position)
                               '(mouse-face highlight help-echo diredp-mouseover-help dired-filename t
                                            ;; `keymap' does not work for Emacs 20.  Could use `local-map'
                                            ;; but that still leaves `RET' bound to `help-follow'.
                                            keymap diredp-list-files-map))
          (forward-line 1))))
    (set-buffer-modified-p nil)
    (setq buffer-read-only  t)
    (buffer-enable-undo)))

(defun diredp-list-file (file &optional details)
  "Return FILE name, expanded.
Non-nil optional arg DETAILS means append details about FILE to the
returned string.

If DETAILS is a list of file attribute numbers then include only the
values of those attributes.  Otherwise, include all attribute values."
  (let ((file-dir  (and details  (or (file-name-directory file)  default-directory)))
        attrs)
    (setq file  (expand-file-name file file-dir))
    (when (and details  (atom details)) (setq details  '(0 1 2 3 4 5 6 7 8 9 10 11)))
    (concat
     file
     (and details
          (setq attrs  (file-attributes file))
          (concat
           "\n"
           (and (memq 0 details)
                (format " File type:                  %s\n"
                        (cond ((eq t (nth 0 attrs))  "Directory")
                              ((stringp (nth 0 attrs))  (format "Symbolic link to `%s'" (nth 0 attrs)))
                              (t  "Normal file"))))
           (and (memq 8 details)
                (format " Permissions:                %s\n" (nth 8 attrs)))
           (and (memq 7 details)  (not (eq t (nth 0 attrs)))
                (format " Size in bytes:              %g\n" (nth 7 attrs)))
           (and (memq 4 details)
                (format-time-string " Time of last access:        %a %b %e %T %Y (%Z)\n" (nth 4 attrs)))
           (and (memq 5 details)
                (format-time-string " Time of last modification:  %a %b %e %T %Y (%Z)\n" (nth 5 attrs)))
           (and (memq 6 details)
                (format-time-string " Time of last status change: %a %b %e %T %Y (%Z)\n" (nth 6 attrs)))
           (and (memq 1 details)
                (format " Number of links:            %d\n" (nth 1 attrs)))
           (and (memq 2 details)
                (format " User ID (UID):              %s\n" (nth 2 attrs)))
           (and (memq 3 details)
                (format " Group ID (GID):             %s\n" (nth 3 attrs)))
           (and (memq 10 details)
                (format " Inode:                      %S\n" (nth 10 attrs)))
           (and (memq 11 details)
                (format " Device number:              %s\n" (nth 11 attrs))))))))

(defvar diredp-files-within-dirs-done ()
  "Directories already processed by `diredp-files-within'.")

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

;; Args INCLUDE-DIRS-P and PREDICATE are not used in the `Dired+' code yet
;; (except in `diredp-directories-within', which also is not used yet).
;;
(defun diredp-files-within (file-list accum &optional no-symlinks-p include-dirs-p predicate)
  "List of readable files in FILE-LIST, handling directories recursively.
FILE-LIST is a list of file names or a function that returns such.
If a function, then invoke it with no args to get the list of files.

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
  ;; Bind `diredp-files-within-dirs-done' for use as a free var in `diredp-files-within-1'.
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
              (setq res  (diredp-files-within-1 (or (and (functionp file-list)
                                                         (dired-buffers-for-dir file) ; Removes killed buffers.
                                                         (with-current-buffer
                                                             (cdr (assoc (file-name-as-directory file)
                                                                         dired-buffers))
                                                           (funcall file-list)))
                                                    (directory-files file 'FULL diredp-re-no-dot))
                                                res no-symlinks-p include-dirs-p predicate))
              (when include-dirs-p (push file res))
              (push (file-truename file) diredp-files-within-dirs-done))
          (when (file-readable-p file) (push file res))))
      (pop files))
    res))

(defun diredp-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (let ((result  ()))
    (dolist (x xs) (unless (funcall pred x) (push x result)))
    (nreverse result)))

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
does the same thing, but it lets you insert any number of descendant
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
    (let ((child-dired-buf  (if in-dired-now-p (current-buffer) (dired-buffers-for-dir child)))
          (switches         ())
          (marked           ()))
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
  (and list  (cons (funcall function list) (diredp-maplist function (cdr list)))))

(defun diredp-existing-dired-buffer-p (buffer-name)
  "Return non-nil if BUFFER-NAME names a live, existing Dired buffer."
  (let ((dbuf  (get-buffer buffer-name)))
    (and dbuf  (buffer-live-p dbuf)  (rassq dbuf dired-buffers))))

;; From `cl-seq.el', function `union', without keyword treatment.
;; (Same as `icicle-set-union' in `icicles-fn.el'.)
(defun diredp-set-union (list1 list2)
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

(when (fboundp 'file-equal-p)           ; Emacs 24+
  (defun diredp-move-file (file &optional prompt-anyway)
    "Move FILE to associated directory in `diredp-move-file-dirs'.
If no association, or if you use a prefix arg, prompt for directory."
    (interactive (list (dired-get-filename) current-prefix-arg))
    (unless file (error "No file specified"))
    (let* ((file-sans  (file-name-nondirectory file))
           (dir        (file-name-as-directory
                        (or (and (not prompt-anyway)
                                 (cdr (assoc file-sans diredp-move-file-dirs)))
                            (read-directory-name "Move to: ")))))
      (when (file-equal-p dir (file-name-directory file))
        (error "Cannot move to same directory: %s" dir))
      (dired-rename-file file dir nil)
      (dired-add-file (expand-file-name file-sans dir))
      (message "Moved `%s' to `%s'" file-sans dir))))

(defvar diredp-last-copied-filenames ()
  "String list of file names last copied to the `kill-ring'.
Copying is done by `dired-copy-filename-as-kill' and related commands.")

(defvar diredp-filename-separator (copy-sequence "\000") ; "^@"
  ;; Should contain only chars that are invalid in a file name.
  "String used to separate file names in a `kill-ring' entry.")


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Use `diredp-filename-separator', not a space char, as the separator.
;; 2. Put text copied to kill ring in variable `diredp-last-copied-filenames'.
;;
(defun dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by the value of variable
`diredp-filename-separator'.

With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the Dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank].

The value of global variable `diredp-last-copied-filenames' is updated
to the string list of file name(s), so you can obtain it even after
the kill ring is modified."
  (interactive "P")
  (let* ((num-arg  (prefix-numeric-value arg))
         (string  (or (dired-get-subdir)
                      (mapconcat #'identity
                                 (cond ((not arg)       (dired-get-marked-files 'no-dir))
                                       ((zerop num-arg) (dired-get-marked-files))
                                       ((consp arg)     (dired-get-marked-files t))
                                       (t               (dired-get-marked-files 'no-dir num-arg)))
                                 diredp-filename-separator))))
    (unless (string= "" string)
      (if (eq last-command 'kill-region) (kill-append string nil) (kill-new string))
      (setq diredp-last-copied-filenames  (car kill-ring-yank-pointer))
      (message "%s" string))))

(defun diredp-copy-abs-filenames-as-kill () ; Not bound.
  "Copy absolute names of marked files in Dired to the kill ring.
Also set variable `diredp-last-copied-filenames' to the string that
lists the file names.

This is the same as using a zero prefix arg with command
`dired-copy-filename-as-kill', that is, \\<dired-mode-map>`M-0 \\[dired-copy-filename-as-kill]'."
  (interactive (diredp-ensure-mode))
  (dired-copy-filename-as-kill 0))

(defun diredp-copy-as-kill-from-clipboard (files)
  "Copy file names from clipboard."
  (when (stringp files)
    (setq files  (if (diredp-string-match-p "\n" files)
                     (split-string files "\n" t "\"")
                   (setq files (split-string files "\"" t "\"")))))
  (let ((files-string  (mapconcat
                        (lambda (file)
                          (unless (file-exists-p file) (message (format "No such file: `%s'" file)))
                          file)
                        files
                        diredp-filename-separator)))
    (kill-new files-string)
    (setq diredp-last-copied-filenames  files-string)))

;;;###autoload
(defalias 'diredp-paste-files 'diredp-yank-files) ; Bound to `C-y'.
;;;###autoload
(defun diredp-yank-files (&optional dir no-confirm-p details)
  "Yank (paste) files to the current directory.
With a non-negative prefix arg you are instead prompted for the target
 directory.
With a non-positive prefix arg you can see details about the files if
 you hit `l' when prompted to confirm pasting.  Otherwise you see only
 the file names.  The details you see are defined by option
 `diredp-list-file-attributes'.

The absolute names of the files to be yanked are taken from the
clipboard or, if that's empty, from names you've copied to the kill
ring using \\<dired-mode-map>\ `M-0 \\[dired-copy-filename-as-kill]' or \
\\[diredp-copy-abs-filenames-as-kill].

Those copy-filename commands also:
 * Use the value of option `diredp-filename-separator' to separate the
   copied file names.
 * Set variable `diredp-last-copied-filenames' to the same string.
   `diredp-yank-files' uses the value of that variable, not whatever
   is currently at the head of the kill ring.

\(To copy file names to the clipboard on MS Windows, you can use Windows
Explorer: Select the file names, then hold `Shift', right-click, and
choose `Copy as Path' from the menu.)

When called from Lisp:

Optional arg NO-CONFIRM-P means do not ask for confirmation to copy.
Optional arg DETAILS is passed to `diredp-y-or-n-files-p'."
  (interactive (list (and current-prefix-arg  (natnump (prefix-numeric-value current-prefix-arg))
                          (expand-file-name (read-directory-name "Yank files to directory: ")))
                     nil
                     (and current-prefix-arg
                          (<= (prefix-numeric-value current-prefix-arg) 0)
                          diredp-list-file-attributes)))
  (setq dir  (or dir  (and (derived-mode-p 'dired-mode)  (dired-current-directory))))
  (unless (file-directory-p dir) (error "Not a directory: `%s'" dir))
  (let* ((ipf-files  (funcall interprogram-paste-function))
         (files      (or ipf-files  diredp-last-copied-filenames)))
    (unless (stringp files)  (error "No copied file names"))
    (when ipf-files (setq files  (diredp-copy-as-kill-from-clipboard files)))
    (setq files  (diredp-delete-if-not (lambda (file) (file-name-absolute-p file))
                                       (split-string files diredp-filename-separator)))
    (unless files  (error "No copied *absolute* file names (Did you use `M-0 w'?)"))
    (if (and (not no-confirm-p)
             (diredp-y-or-n-files-p "Yank files whose names you copied? " files nil details))
        (dired-create-files #'dired-copy-file "Copy" files
                            (lambda (from) (expand-file-name (file-name-nondirectory from) dir)))
      (message "OK, file-yanking canceled"))))

;;;###autoload
(defun diredp-move-files-named-in-kill-ring (&optional dir no-confirm-p details) ; Bound to `C-w'
  "Move files, whose absolute names you copied, to the current directory.
With a non-negative prefix arg you are instead prompted for the target
 directory.
With a non-positive prefix arg you can see details about the files if
 you hit `l' when prompted to confirm pasting.  Otherwise you see only
 the file names.  The details you see are defined by option
 `diredp-list-file-attributes'.

You should have copied the list of file names as a string to the kill
ring using \\<dired-mode-map>`M-0 \\[dired-copy-filename-as-kill]' or \
\\[diredp-copy-abs-filenames-as-kill].
Those commands also set variable `diredp-last-copied-filenames' to the
same string.  `diredp-move-files-named-in-kill-ring' uses the value of
that variable, not whatever is currently at the head of the kill ring.

When called from Lisp:

Optional arg NO-CONFIRM-P means do not ask for confirmation to move.
Optional arg DETAILS is passed to `diredp-y-or-n-files-p'."
  (interactive (list (and current-prefix-arg  (natnump (prefix-numeric-value current-prefix-arg))
                          (expand-file-name (read-directory-name "Move files to directory: ")))
                     nil
                     (and current-prefix-arg
                          (<= (prefix-numeric-value current-prefix-arg) 0)
                          diredp-list-file-attributes)))
  (setq dir  (or dir  (and (derived-mode-p 'dired-mode)  (dired-current-directory))))
  (unless (file-directory-p dir) (error "Not a directory: `%s'" dir))
  (let ((files  diredp-last-copied-filenames))
    (unless (stringp files)  (error "No copied file names"))
    (setq files  (diredp-delete-if-not (lambda (file) (file-name-absolute-p file))
                                       (split-string files (regexp-quote diredp-filename-separator))))
    (unless files  (error "No copied (absolute* file names (Did you use `M-0 w'?)"))
    (if (and (not no-confirm-p)
             (diredp-y-or-n-files-p "MOVE files whose names you copied? " files nil details))
        (dired-create-files #'dired-rename-file "Move" files
                            (lambda (from) (expand-file-name (file-name-nondirectory from) dir)))
      (message "OK, file-moves canceled"))))


;;; Commands operating on marked at all levels below (recursively)

(defun diredp-get-confirmation-recursive (&optional type)
  "Get confirmation from user to act on all TYPE here and below.
If TYPE is nil use \"files\" in the confirmation prompt, else use TYPE.
Raise an error if not confirmed.
Raise an error first if not in Dired mode."
  (diredp-ensure-mode)
  (unless (y-or-n-p (format "Act on ALL %s (or all marked if any) in and UNDER this dir? "
                            (or type 'files)))
    (error "OK, canceled")))

;;;###autoload
(when (> emacs-major-version 21)        ; Emacs 22+ has KILL-ROOT parameter.
  (defun diredp-kill-this-tree ()
    "Remove this subdir listing and lower listings."
    (interactive)
    (dired-kill-tree (dired-current-directory) nil 'KILL-ROOT)))

;;;###autoload
(defun diredp-insert-subdirs (&optional switches interactivep) ; Bound to `M-i'
  "Insert the marked subdirectories.
Like using \\<dired-mode-map>`\\[dired-maybe-insert-subdir]' at each marked directory line."
  (interactive (list (and current-prefix-arg
                          (read-string "Switches for listing: "
                                       (or (and (boundp 'dired-subdir-switches)  dired-subdir-switches)
                                           dired-actual-switches)))
                     t))
  (dolist (subdir  (dired-get-marked-files nil
                                           nil
                                           (lambda (fl) (and (file-directory-p fl) ; Exclude `.' and `..'
                                                             (not (diredp-string-match-p "/[.][.]?\\'" fl))))
                                           nil
                                           interactivep))
    (dired-maybe-insert-subdir subdir switches)))

;;;###autoload
(defun diredp-insert-subdirs-recursive (&optional ignore-marks-p details) ; Bound to `M-+ M-i'
  "Insert the marked subdirs, including those in marked subdirs.
Like `diredp-insert-subdirs', but act recursively on subdirs.
The subdirs inserted are those that are marked in the current Dired
buffer, or ALL subdirs in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way (their marked
subdirs are inserted...).

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive 'subdirs)
                      (list current-prefix-arg diredp-list-file-attributes)))
  (dolist (subdir  (diredp-get-files ignore-marks-p #'file-directory-p 'INCLUDE-SUBDIRS-P nil nil details))
    (dired-maybe-insert-subdir subdir)))

;;;###autoload
(defun diredp-do-shell-command-recursive (command &optional ignore-marks-p details) ; Bound to `M-+ !'
  "Run shell COMMAND on the marked files, including those in marked subdirs.
Like `dired-do-shell-command', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
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
            (list cmd current-prefix-arg diredp-list-file-attributes))))
  (dired-do-shell-command command nil (diredp-get-files ignore-marks-p nil nil nil nil details)))

(when (fboundp 'dired-do-async-shell-command) ; Emacs 23+

  (defun diredp-do-async-shell-command-recursive (command &optional ignore-marks-p details) ; Bound to `M-+ &'
    "Run async shell COMMAND on marked files, including in marked subdirs.
Like `dired-do-async-shell-command', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
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
              (list cmd current-prefix-arg diredp-list-file-attributes))))
    (dired-do-async-shell-command command nil (diredp-get-files ignore-marks-p nil nil nil nil details))))

;;;###autoload
(defun diredp-do-symlink-recursive (&optional ignore-marks-p details) ; Bound to `M-+ S'
  "Make symbolic links to marked files, including those in marked subdirs.
Like `dired-do-symlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-create-files-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-do-create-files-recursive #'make-symbolic-link "Symlink" ignore-marks-p details))

(defun diredp-do-relsymlink-recursive (&optional ignore-marks-p details) ; Bound to `M-+ Y'
  "Relative symlink all marked files, including those in marked subdirs into a dir.
Like `dired-do-relsymlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

For absolute symlinks, use \\[diredp-do-symlink-recursive].

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-create-files-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-do-create-files-recursive #'dired-make-relative-symlink "RelSymLink" ignore-marks-p details))

;;;###autoload
(defun diredp-do-hardlink-recursive (&optional ignore-marks-p details) ; Bound to `M-+ H'
  "Add hard links for marked files, including those in marked subdirs.
Like `dired-do-hardlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-create-files-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-do-create-files-recursive #'dired-hardlink "Hardlink" ignore-marks-p details))

;;;###autoload
(defun diredp-do-print-recursive (&optional ignore-marks-p details) ; Bound to `M-+ P'
  "Print the marked files, including those in marked subdirs.
Like `dired-do-print', but act recursively on subdirs to pick up the
files to print.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (let* ((file-list  (diredp-get-files ignore-marks-p nil nil nil nil details))
         (command    (dired-mark-read-string
                      "Print %s with: "
                      (mapconcat #'identity
                                 (cons lpr-command (if (stringp lpr-switches) (list lpr-switches) lpr-switches))
                                 " ")
                      'print nil file-list)))
    (dired-run-shell-command (dired-shell-stuff-it command file-list nil))))

;;;###autoload
(defun diredp-image-dired-display-thumbs-recursive (&optional ignore-marks-p append do-not-pop details)
                                        ; Bound to `M-+ C-t d'
  "Display thumbnails of marked files, including those in marked subdirs.
Like `image-dired-display-thumbs', but act recursively on subdirs.
Optional arguments APPEND and DO-NOT-POP are as for
`image-dired-display-thumbs'.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-image-dired-required-msg)
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg nil nil diredp-list-file-attributes)))
  (let ((buf  (image-dired-create-thumbnail-buffer))
        thumb-name files dired-buf)
    (setq files      (diredp-get-files ignore-marks-p nil nil nil nil details)
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
(defun diredp-image-dired-tag-files-recursive (&optional ignore-marks-p details) ; Bound to `M-+ C-t t'
  "Tag marked files with an `image-dired' tag, including in marked subdirs.
Like `image-dired-tag-files', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-image-dired-required-msg)
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg diredp-list-file-attributes)))
  (let ((tag  (read-string "Tags to add (separate tags with a semicolon): ")))
    (image-dired-write-tags (mapcar (lambda (x) (cons x tag))
                                    (diredp-get-files ignore-marks-p nil nil nil nil details)))))

;;;###autoload
(defun diredp-image-dired-delete-tag-recursive (&optional ignore-marks-p details) ; Bound to `M-+ C-t r'
  "Remove `image-dired' tag for marked files, including in marked subdirs.
Like `image-dired-delete-tag', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-image-dired-required-msg)
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg diredp-list-file-attributes)))
  (image-dired-remove-tag (diredp-get-files ignore-marks-p nil nil nil nil details)
                          (read-string "Tag to remove: ")))

;;;###autoload
(defun diredp-image-dired-comment-files-recursive (&optional ignore-marks-p details) ; Bound to `M-+ C-t c'
  "Add comment to marked files in dired, including those in marked subdirs.
Like `image-dired-dired-comment-files' but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-image-dired-required-msg)
                      (diredp-get-confirmation-recursive)
                      (list current-prefix-arg diredp-list-file-attributes)))
  (let ((comment  (image-dired-read-comment)))
    (image-dired-write-comments (mapcar (lambda (curr-file) (cons curr-file comment))
                                        (diredp-get-files ignore-marks-p nil nil nil nil details)))))

(when (> emacs-major-version 22)

  (defun diredp-do-decrypt-recursive (&optional ignore-marks-p details) ; Bound to `M-+ : d'
    "Decrypt marked files, including those in marked subdirs.
Like `epa-dired-do-decrypt', but act recursively on subdirs to pick up
the files to decrypt.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (dolist (file  (diredp-get-files ignore-marks-p nil nil nil nil details))
      (epa-decrypt-file (expand-file-name file)))
    (revert-buffer))

  (defun diredp-do-verify-recursive (&optional ignore-marks-p details) ; Bound to `M-+ : v'
    "Verify marked files, including those in marked subdirs.
Like `epa-dired-do-verify', but act recursively on subdirs to pick up
the files to verify.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (dolist (file  (diredp-get-files ignore-marks-p nil nil nil nil details))
      (epa-verify-file (expand-file-name file)))
    (revert-buffer))

  (defun diredp-do-sign-recursive (&optional ignore-marks-p details) ; Bound to `M-+ : s'
    "Sign marked files, including those in marked subdirs.
Like `epa-dired-do-sign', but act recursively on subdirs to pick up
the files to sign.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (dolist (file  (diredp-get-files ignore-marks-p nil nil nil nil details))
      (epa-sign-file (expand-file-name file)
                     (epa-select-keys (epg-make-context) "Select keys for signing.
If none are selected, the default secret key is used.  ")
                     (y-or-n-p "Make a detached signature? ")))
    (revert-buffer))

  (defun diredp-do-encrypt-recursive (&optional ignore-marks-p details) ; Bound to `M-+ : e'
    "Encrypt marked files, including those in marked subdirs.
Like `epa-dired-do-encrypt', but act recursively on subdirs to pick up
the files to encrypt.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (dolist (file  (diredp-get-files ignore-marks-p nil nil nil nil details))
      (epa-encrypt-file (expand-file-name file)
                        (epa-select-keys (epg-make-context) "Select recipients for encryption.
If none are selected, symmetric encryption is performed.  ")))
    (revert-buffer)))

;;;###autoload
(defun diredp-do-bookmark-recursive (&optional ignore-marks-p prefix details) ; Bound to `M-+ M-b'
  "Bookmark the marked files, including those in marked subdirs.
Like `diredp-do-bookmark', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list current-prefix-arg
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            diredp-list-file-attributes)))
  (dolist (file  (diredp-get-files ignore-marks-p nil nil nil nil details))
    (diredp-bookmark prefix file 'NO-MSG-P)))

;;;###autoload
(defun diredp-do-bookmark-dirs-recursive (ignore-marks-p &optional details msgp)
  "Bookmark this Dired buffer and marked subdirectory Dired buffers, recursively.
Create a Dired bookmark for this directory and for each of its marked
subdirectories.  Handle each of the marked subdirectory similarly:
bookmark it and its marked subdirectories, and so on, recursively.
Name each of these Dired bookmarks with the Dired buffer name.

After creating the Dired bookmarks, create a sequence bookmark, named
`DIRBUF and subdirs', where DIRBUF is the name of the original buffer.
This bookmark represents the whole Dired tree rooted in the directory
where you invoked the command.  Jumping to this sequence bookmark
restores all of the Dired buffers making up the tree, by jumping to
each of their bookmarks.

With a prefix arg, bookmark the marked and unmarked subdirectory Dired
buffers, recursively, that is, ignore markings.

Note:

* If there is more than one Dired buffer for a given subdirectory then
  only the first such is used.

* This command creates new bookmarks.  It never updates or overwrites
  an existing bookmark.

You need library `Bookmark+' for this command.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-subdirs'."
  (interactive (progn (unless (featurep 'bookmark+)
                        (error "You need library `Bookmark+' for this command"))
                      (diredp-get-confirmation-recursive 'subdirs)
                      (list current-prefix-arg diredp-list-file-attributes t)))
  (diredp-ensure-mode)
  (let ((sdirs   (diredp-get-subdirs ignore-marks-p nil details))
        (snames  ())
        dbufs)
    (when (and msgp  sdirs) (message "Checking descendant directories..."))
    (dolist (dir  (cons default-directory sdirs))
      (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
        (with-current-buffer (car dbufs)
          (let ((bname  (bookmark-buffer-name))
                (count  2))
            (while (and (bmkp-get-bookmark-in-alist bname 'NOERROR)  (setq bname  (format "%s[%d]" bname count))))
            (bookmark-set bname nil nil 'NO-UPDATE-P) ; Inhibit updating displayed list.
            (push bname snames)))))
    (let ((bname  (format "%s and subdirs" (bookmark-buffer-name)))
          (count  2))
      (while (and (bmkp-get-bookmark-in-alist bname 'NOERROR)  (setq bname  (format "%s[%d]" bname count))))
      (bmkp-set-sequence-bookmark bname (nreverse snames) -1 'MSGP))
    (bmkp-refresh/rebuild-menu-list nil)))

;;;###autoload
(defun diredp-do-bookmark-in-bookmark-file-recursive (bookmark-file ; Bound to `M-+ C-M-B', aka `M-+ C-M-S-b')
                                                      &optional prefix ignore-marks-p bfile-bookmarkp details)
  "Bookmark files here and below in BOOKMARK-FILE and save BOOKMARK-FILE.
Like `diredp-do-bookmark-in-bookmark-file', but act recursively on
subdirs.  The files included are those that are marked in the current
Dired buffer, or all files in the directory if none are marked.
Marked subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp:
 * Optional arg BFILE-BOOKMARKP non-nil means create a bookmark-file
   bookmark for BOOKMARK-FILE.
 * Optional arg DETAILS is passed to `diredp-get-files'."
  (interactive
   (progn (diredp-get-confirmation-recursive)
          (let ((d-r-b-f-args  (diredp-read-bookmark-file-args)))
            (list (car d-r-b-f-args)
                  (cadr d-r-b-f-args)
                  (car (cddr d-r-b-f-args))
                  nil
                  diredp-list-file-attributes))))
  (diredp-do-bookmark-in-bookmark-file bookmark-file prefix nil bfile-bookmarkp
                                       (diredp-get-files ignore-marks-p nil nil nil nil details)))

;;;###autoload
(defun diredp-set-bookmark-file-bookmark-for-marked-recursive (bookmark-file
                                                               &optional prefix ignore-marks-p details)
                                        ; Bound to `M-+ C-M-b'
  "Bookmark the marked files and create a bookmark-file bookmark for them.
Like `diredp-set-bookmark-file-bookmark-for-marked', but act
recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-bookmark-in-bookmark-file-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (let ((d-r-b-f-args  (diredp-read-bookmark-file-args)))
                        (list (car d-r-b-f-args)
                              (cadr d-r-b-f-args)
                              (car (cddr d-r-b-f-args))
                              diredp-list-file-attributes))))
  (diredp-ensure-bookmark+)
  (diredp-do-bookmark-in-bookmark-file-recursive
   bookmark-file prefix ignore-marks-p 'CREATE-BOOKMARK-FILE-BOOKMARK details))

;;;###autoload
(defun diredp-do-find-marked-files-recursive (&optional arg details) ; Bound to `M-+ F'
  "Find marked files simultaneously, including those in marked subdirs.
Like `dired-do-find-marked-files', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With (explicit) numeric prefix ARG >= 0, find the files but do not
display them.

With numeric prefix ARG <= 0, ignore all marks - include all files in
this Dired buffer and all subdirs, recursively.

Note that prefix-argument behavior is different for this command than
for `dired-do-find-marked-files'.  In particular, a negative numeric
prefix arg does not cause the files to be shown in separate frames.
Only non-nil `pop-up-frames' (or equivalent configuration) causes
the files to be shown in separate frames.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list current-prefix-arg diredp-list-file-attributes)))
  (let ((narg  (prefix-numeric-value arg)))
    (dired-simultaneous-find-file (diredp-get-files (<= narg 0) nil nil nil nil details)
                                  (and arg  (not (consp arg))  (>= narg 0)  narg))))

(when (fboundp 'dired-do-isearch-regexp) ; Emacs 23+

  (defun diredp-do-isearch-recursive (&optional ignore-marks-p details) ; Bound to `M-+ M-s a C-s'
    "Isearch the marked files, including those in marked subdirs.
With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (multi-isearch-files (diredp-get-files ignore-marks-p nil nil nil nil details)))

  (defun diredp-do-isearch-regexp-recursive (&optional ignore-marks-p details) ; `M-+ M-s a C-M-s'
    "Regexp-Isearch the marked files, including those in marked subdirs.
With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (multi-isearch-files-regexp (diredp-get-files ignore-marks-p nil nil nil nil details))))

(defun diredp-do-search-recursive (regexp &optional ignore-marks-p details) ; Bound to `M-+ A'
  "Regexp-search the marked files, including those in marked subdirs.
With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

Stops when a match is found.
To continue searching for the next match, use `\\[tags-loop-continue]'.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list (read-string "Search marked files (regexp): ")
                            current-prefix-arg
                            diredp-list-file-attributes)))
  (tags-search regexp '(diredp-get-files ignore-marks-p nil nil nil nil details)))

;;;###autoload
(defun diredp-do-query-replace-regexp-recursive (from to &optional arg details) ; Bound to `M-+ Q'
  "Do `query-replace-regexp' on marked files, including in marked subdirs.
Query-replace FROM with TO.

Like `dired-do-query-replace-regexp', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With an (explicit) numeric prefix argument:

* >= 0 means ignore all marks - include ALL files in this Dired buffer
  and all subdirs, recursively.

* <= 0 means replace only word-delimited matches.

If you exit (`\\[keyboard-quit]', `RET' or `q'), you can resume the query replacement
using `\\[tags-loop-continue]'.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (let ((common  (query-replace-read-args "Query replace regexp in marked files" t t)))
                        (list (nth 0 common)
                              (nth 1 common)
                              current-prefix-arg
                              diredp-list-file-attributes))))
  (let* ((narg                  (and arg  (prefix-numeric-value arg)))
         (delimited             (and narg  (<= narg 0)))
         (ignore-marks-p        (and narg  (>= narg 0)))
         (files                 (diredp-get-files ignore-marks-p nil nil nil nil details))
         (fit-frame-min-width   30)
         (fit-frame-min-height  15))
    (dolist (file  files)
      (let ((buffer  (get-file-buffer file)))
        (when (and buffer  (with-current-buffer buffer buffer-read-only))
          (error "File `%s' is visited read-only" file))))
    (tags-query-replace from to delimited `',files)))

;;;###autoload
(defun diredp-do-grep-recursive (command-args &optional details) ; Bound to `M+ C-M-g'
  "Run `grep' on marked files, including those in marked subdirs.
Like `diredp-do-grep', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (unless (if (< emacs-major-version 22)
                                  grep-command
                                (and grep-command  (or (not grep-use-null-device)  (eq grep-use-null-device t))))
                        (grep-compute-defaults))
                      (list (diredp-do-grep-1
                             (diredp-get-files current-prefix-arg nil nil nil nil diredp-list-file-attributes)))))
  (grep command-args))

;;;###autoload
(defun diredp-marked-recursive (dirname &optional ignore-marks-p details) ; Not bound to a key
  "Open Dired on marked files, including those in marked subdirs.
Like `diredp-marked', but act recursively on subdirs.

See `diredp-do-find-marked-files-recursive' for a description of the
files included.  In particular, if no files are marked here or in a
marked subdir, then all files in the directory are included.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, DIRNAME here must be a string, not a cons.  It
is used as the name of the new Dired buffer.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list nil current-prefix-arg diredp-list-file-attributes)))
  (dired (cons (or dirname  (generate-new-buffer-name (buffer-name)))
               (diredp-get-files ignore-marks-p nil nil nil nil details))))

;;;###autoload
(defun diredp-marked-recursive-other-window (dirname &optional ignore-marks-p details) ; Bound to `M-+ C-M-*'
  "Same as `diredp-marked-recursive', but uses a different window.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list nil current-prefix-arg diredp-list-file-attributes)))
  (dired-other-window
   (cons (or dirname  (generate-new-buffer-name (buffer-name)))
         (diredp-get-files ignore-marks-p nil nil nil nil details))))

;;;###autoload
(defun diredp-list-marked-recursive (&optional ignore-marks-p predicate details) ; Bound to `M-+ C-M-l'
  "List the files marked here and in marked subdirs, recursively.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, all marks are ignored: all files in this Dired
buffer and all descendant directories are included.

You can use `RET' or `mouse-2' to visit any of the files.
If `tooltip-mode' is on then moving the mouse over image-file names
shows image previews.

When called from Lisp:
 Non-nil optional arg IGNORE-MARKS-P means ignore marks.
 Non-nil optional arg PREDICATE is a file-name predicate.  List only
  the files for which it returns non-nil.
 Non-nil optional arg DETAILS is passed to `diredp-list-files'."
  (interactive ; No need for `diredp-get-confirmation-recursive' here.
   (progn (diredp-ensure-mode) (list current-prefix-arg nil diredp-list-file-attributes)))
  (let ((files  (diredp-get-files ignore-marks-p predicate))) (diredp-list-files files nil nil nil details)))

;;;###autoload
(defun diredp-flag-auto-save-files-recursive (&optional arg details) ; Bound to `M-+ #'
  "Flag all auto-save files for deletion, including in marked subdirs.
A non-negative prefix arg means to unmark (unflag) them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, flag all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'."
  (interactive (list current-prefix-arg diredp-list-file-attributes))
  (let ((dired-marker-char  dired-del-marker))
    (diredp-mark-recursive-1 arg "auto-save files" "auto-save file" '(diredp-looking-at-p "^.* #.+#$") details)))

(when (fboundp 'char-displayable-p)     ; Emacs 22+

  (defun diredp-change-marks-recursive (old new &optional arg predicate details) ; Bound to `M-+ * c'
    "Change all OLD marks to NEW marks, including those in marked subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

* A non-positive prefix arg means ignore subdir markings and act
  instead on ALL subdirs.

* A non-negative prefix arg means do not change marks on subdirs
  themselves.

Note: If there is more than one Dired buffer for a given subdirectory
then only the first such is used.

When called from Lisp:
 Non-nil arg PREDICATE is a file-name predicate.  Act on only the
  files for which it returns non-nil.
 DETAILS is passed to `diredp-get-subdirs'."
    (interactive
     (progn (diredp-get-confirmation-recursive)
            (let* ((cursor-in-echo-area  t)
                   (old                  (progn (message "Change (old mark): ") (read-char)))
                   (new                  (progn (message "Change `%c' marks to (new mark): " old) (read-char))))
              (list old new current-prefix-arg nil diredp-list-file-attributes))))
    (let* ((numarg             (and arg  (prefix-numeric-value arg)))
           (nosubs             (natnump numarg))
           (ignore-marks       (and numarg  (<= numarg 0)))
           (dired-marker-char  new)
           (sdirs              (diredp-get-subdirs ignore-marks predicate details))
           (old-strg           (format "\n%c" old))
           (count              0)
           dbufs)
      (unless (char-displayable-p old) (error "Not a displayable character: `%c'" old))
      (unless (char-displayable-p new) (error "Not a displayable character: `%c'" new))
      (message "Changing mark `%c' to `%c'..." old new)
      (dolist (dir  (cons default-directory sdirs))
        (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
          (with-current-buffer (car dbufs)
            (let ((inhibit-read-only  t)
                  (file               nil))
              (save-excursion
                (goto-char (point-min))
                (while (search-forward old-strg nil t)
                  (save-match-data (setq file  (dired-get-filename 'no-dir 'NO-ERROR)))
                  ;; Do nothing if changing from UNmarked and not on a file or dir name.
                  (unless (and (= old ?   )  (not file))
                    ;; Do nothing if marked subdir and not changing subdir marks.
                    (unless (and nosubs  file  (file-directory-p file))
                      (subst-char-in-region (match-beginning 0) (match-end 0) old new)
                      (setq count  (1+ count))))))))))
      (message "%d mark%s changed from `%c' to `%c'" count (dired-plural-s count) old new)))

  (defun diredp-unmark-all-marks-recursive (&optional arg details) ; Bound to `M-+ U'
    "Remove ALL marks everywhere, including in marked subdirs.
A prefix arg is as for `diredp-unmark-all-files-recursive'.
Note that a negative prefix arg (e.g. `C--') removes all marks from
this Dired buffer and then does the same recursively for each of its
subdirs.

When called from Lisp, optional arg DETAILS is passed to
`diredp-unmark-all-files-recursive'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (diredp-unmark-all-files-recursive ?\r arg details))

  (defun diredp-unmark-all-files-recursive (mark &optional arg predicate details) ; Bound to `M-+ M-DEL'
    "Remove a given mark (or ALL) everywhere, including in marked subdirs.
You are prompted for the mark character to remove.  If you hit `RET'
instead then ALL mark characters are removed.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

* A non-positive prefix arg means ignore subdir markings and act
  instead on ALL subdirs.

* A non-negative prefix arg means do not change marks on subdirs
  themselves.

Note: If there is more than one Dired buffer for a given subdirectory
then only the first such is used.

When called from Lisp:
 Non-nil arg PREDICATE is a file-name predicate.  Act on only the
  files for which it returns non-nil.
 DETAILS is passed to `diredp-get-subdirs'."
    (interactive
     (progn (diredp-get-confirmation-recursive)
            (let* ((cursor-in-echo-area  t)
                   (mrk                  (progn (message "Remove marks (RET means all): ") (read-char))))
              (list mrk current-prefix-arg nil diredp-list-file-attributes))))
    (let* ((numarg             (and arg  (prefix-numeric-value arg)))
           (nosubs             (natnump numarg))
           (ignore-marks       (and numarg  (<= numarg 0)))
           (dired-marker-char  ?\  )    ; Unmark
           (sdirs              (diredp-get-subdirs ignore-marks predicate details))
           (mrk-strg           (format "\n%c" mark))
           (count              0)
           dbufs)
      (unless (char-displayable-p mark) (error "Not a displayable character: `%c'" mark))
      (if (eq mark ?\r)
          (message "Unmarking ALL marks here and below...")
        (message "Unmarking mark `%c' here and below..." mark))
      (dolist (dir  (cons default-directory sdirs))
        (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
          (with-current-buffer (car dbufs)
            (let ((inhibit-read-only  t)
                  (file               nil))
              (save-excursion
                (goto-char (point-min))
                (while (if (eq mark ?\r)
                           (re-search-forward dired-re-mark nil t)
                         (search-forward mrk-strg nil t))
                  (save-match-data (setq file  (dired-get-filename 'no-dir 'NO-ERROR)))
                  ;; Do nothing if marked subdir and not changing subdir marks.
                  (unless (and nosubs  file  (file-directory-p file))
                    (subst-char-in-region (match-beginning 0) (match-end 0) (preceding-char) ?\   ))
                  (setq count  (1+ count))))))))
      (message "%d mark%s UNmarked" count (dired-plural-s count))))

  )

(when (and (memq system-type '(windows-nt ms-dos))  (fboundp 'w32-browser))

  (defun diredp-multiple-w32-browser-recursive (&optional ignore-marks-p details)
    "Run Windows apps for with marked files, including those in marked subdirs.
Like `dired-multiple-w32-browser', but act recursively on subdirs.

See `diredp-do-find-marked-files-recursive' for a description of the
files included.  In particular, if no files are marked here or in a
marked subdir, then all files in the directory are included.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
    (interactive (progn (diredp-get-confirmation-recursive)
                        (list current-prefix-arg diredp-list-file-attributes)))
    (let ((files  (diredp-get-files ignore-marks-p nil nil nil nil details)))
      (while files
        (w32-browser (car files))
        (sleep-for w32-browser-wait-time)
        (setq files  (cdr files)))))

  )

;;;###autoload
(defun diredp-copy-filename-as-kill-recursive (&optional arg details) ; Bound to `M-+ M-w'
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
subdirectories are handled recursively in the same way.

The names are copied to the kill ring and to variable
`diredp-last-copied-filenames'.

When called from Lisp:
* ARG is a raw prefix arg
* DETAILS is passed to `diredp-get-files'."
  (interactive ; No need for `diredp-get-confirmation-recursive' here.
   (progn (diredp-ensure-mode) (list current-prefix-arg diredp-list-file-attributes)))
  (let* ((files   (mapcar (cond ((zerop (prefix-numeric-value arg)) #'identity)
                                ((consp arg) (lambda (fn) (concat (dired-current-directory t)
                                                             (file-name-nondirectory fn))))
                                (t (lambda (fn) (file-name-nondirectory fn))))
                          (diredp-get-files nil nil nil nil nil details)))
         (string  (mapconcat #'identity files diredp-filename-separator)))
    (unless (string= "" string)
      (if (eq last-command 'kill-region) (kill-append string nil) (kill-new string))
      (setq diredp-last-copied-filenames  (car kill-ring-yank-pointer)))
    (message "%s" string)))

;;;###autoload
(defun diredp-copy-abs-filenames-as-kill-recursive (&optional ignore-marks-p details) ; Not bound.
  "Copy absolute names of files marked here and in marked subdirs, recursively.
The names are copied to the kill ring and to variable
`diredp-last-copied-filenames'.

The files whose names are copied are those that are marked in the
current Dired buffer, or all files in the directory if none are
marked.  Marked subdirectories are handled recursively in the same
way.

When called from Lisp, optional arg DETAILS is passed to
`diredp-copy-filename-as-kill-recursive'."
  (interactive                          ; No need for `diredp-get-confirmation-recursive' here.
   (progn (diredp-ensure-mode) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-copy-filename-as-kill-recursive 0 details)
  (setq diredp-last-copied-filenames  (car kill-ring-yank-pointer)))

;;;###autoload
(defun diredp-mark-files-regexp-recursive (regexp
                                           &optional marker-char ignore-marks-p details) ; Bound to `M-+ % m'
  "Mark all files matching REGEXP, including those in marked subdirs.
Like `dired-mark-files-regexp' but act recursively on marked subdirs.

The file names to be matched by this command are always absolute -
they include the full directory.  Note that this does NOT correspond
to the default behavior for `dired-mark-files-regexp'.  The other
matching possibilities offered by `dired-mark-files-regexp' are not
available for this command.

Directories `.' and `..' are never marked.

A non-negative prefix arg means to UNmark the files instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all matching files in this
directory and all descendant directories.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think.

REGEXP is added to `regexp-search-ring', for regexp search.

Note: If there is more than one Dired buffer for a given subdirectory
then only the first such is used.

When called from Lisp, DETAILS is passed to `diredp-get-subdirs'."
  (interactive (let* ((numarg   (and current-prefix-arg  (prefix-numeric-value current-prefix-arg)))
                      (unmark   (and numarg  (>= numarg 0)))
                      (ignorep  (and numarg  (<= numarg 0))))
                 (list (diredp-read-regexp (concat (if unmark "UNmark" "Mark") " files (regexp): "))
                       (and unmark  ?\040)
                       ignorep
                       diredp-list-file-attributes)))
  (add-to-list 'regexp-search-ring regexp) ; Add REGEXP to `regexp-search-ring'.
  (let ((dired-marker-char  (or marker-char  dired-marker-char))
        (sdirs              (diredp-get-subdirs ignore-marks-p nil details))
        (matched            0)
        (changed            0)
        dbufs chg.mtch)
    (message "%s files..." (if (eq ?\040 dired-marker-char) "UNmarking" "Marking"))
    (dolist (dir  (cons default-directory sdirs))
      (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
        (with-current-buffer (car dbufs)
          (setq chg.mtch  (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))
                                               (not (eolp)) ; Empty line
                                               (let ((fn  (dired-get-filename nil 'NO-ERROR)))
                                                 (and fn  (diredp-string-match-p regexp fn))))
                                          "file")
                changed   (+ changed (or (car chg.mtch)  0))
                matched   (+ matched (or (cdr chg.mtch)  0))))))
    (message "%s file%s%s%s newly %s"
             matched
             (dired-plural-s matched)
             (if (not (= matched changed)) " matched, " "")
             (if (not (= matched changed)) changed "")
             (if (eq ?\040 dired-marker-char) "unmarked" "marked"))))

;;;###autoload
(defun diredp-mark-files-containing-regexp-recursive (regexp
                                                      &optional marker-char ignore-marks-p details) ; `M-+ % g'
  "Mark files with contents containing a REGEXP match, including in marked subdirs.
Like `dired-mark-files-containing-regexp' but act recursively on
marked subdirs.

A non-negative prefix arg means to UNmark the files instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all matching files in this
directory and all descendant directories.

REGEXP is added to `regexp-search-ring', for regexp search.

Note: If there is more than one Dired buffer for a given subdirectory
then only the first such is used.

If a file is visited in a buffer and `dired-always-read-filesystem' is
nil, this looks in the buffer without revisiting the file, so the
results might be inconsistent with the file on disk if its contents
have changed since it was last visited.

When called from Lisp, DETAILS is passed to `diredp-get-subdirs'."

  (interactive (let* ((numarg   (and current-prefix-arg  (prefix-numeric-value current-prefix-arg)))
                      (unmark   (and numarg  (>= numarg 0)))
                      (ignorep  (and numarg  (<= numarg 0))))
                 (list (diredp-read-regexp (concat (if unmark "UNmark" "Mark") " files containing (regexp): "))
                       (and unmark  ?\040)
                       ignorep
                       diredp-list-file-attributes)))
  (add-to-list 'regexp-search-ring regexp) ; Add REGEXP to `regexp-search-ring'.
  (let ((dired-marker-char  (or marker-char  dired-marker-char))
        (sdirs              (diredp-get-subdirs ignore-marks-p nil details))
        (matched            0)
        (changed            0)
        dbufs chg.mtch)
    (message "%s files..." (if (eq ?\040 dired-marker-char) "UNmarking" "Marking"))
    (dolist (dir  (cons default-directory sdirs))
      (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
        (with-current-buffer (car dbufs)
          (setq chg.mtch
                (diredp-mark-if
                 (and (not (diredp-looking-at-p dired-re-dot))
                      (not (eolp))
                      (let ((fname  (dired-get-filename nil 'NO-ERROR)))
                        (and fname
                             (file-readable-p fname)
                             (not (file-directory-p fname))
                             (let ((prebuf  (get-file-buffer fname)))
                               (message "Checking %s" fname)
                               ;; For now, do it inside Emacs.  Grep might be better if there are lots of files.
                               (if (and prebuf  (or (not (boundp 'dired-always-read-filesystem))
                                                    (not dired-always-read-filesystem))) ; Emacs 26+
                                   (with-current-buffer prebuf
                                     (save-excursion (goto-char (point-min)) (re-search-forward regexp nil t)))
                                 (with-temp-buffer
                                   (insert-file-contents fname)
                                   (goto-char (point-min))
                                   (re-search-forward regexp nil t)))))))
                 "file")
                changed   (+ changed (or (car chg.mtch)  0))
                matched   (+ matched (or (cdr chg.mtch)  0))))))
    (message "%s file%s%s%s newly %s"
             matched
             (dired-plural-s matched)
             (if (not (= matched changed)) " matched, " "")
             (if (not (= matched changed)) changed "")
             (if (eq ?\040 dired-marker-char) "unmarked" "marked"))))

(defun diredp-mark-extension-recursive (extension &optional arg details) ; Bound to `M-+ * .'
  "Mark all files with a certain EXTENSION, including in marked subdirs.
A `.' is not automatically prepended to the string entered.

This is like `diredp-mark/unmark-extension', but this acts recursively
on marked subdirs, and a non-positive prefix arg acts differently.

A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

Non-interactively, EXTENSION is the extension (a string).  It can also
be a list of extension strings.
Optional argument ARG is the prefix arg.

When called from Lisp, DETAILS is passed to `diredp-mark-files-regexp-recursive'."
  (interactive (let* ((numarg  (and current-prefix-arg  (prefix-numeric-value current-prefix-arg)))
                      (unmark  (and numarg  (>= numarg 0))))
                 (list (diredp-read-regexp (concat (if unmark "UNmark" "Mark") " extension: "))
                       current-prefix-arg
                       diredp-list-file-attributes)))
  (let* ((numarg   (and arg  (prefix-numeric-value arg)))
         (unmark   (and numarg  (>= numarg 0)))
         (ignorep  (and numarg  (<= numarg 0))))
    (or (listp extension)  (setq extension  (list extension)))
    (diredp-mark-files-regexp-recursive (concat ".+[.]\\("
                                                (mapconcat #'regexp-quote extension "\\|")
                                                "\\)$")
                                        (if unmark ?\040 dired-marker-char)
                                        ignorep
                                        details)))

;; This is `read--expression' (from Emacs 24.4+), except that it uses `pp-read-expression-map' if available.
(defun diredp-read-expression (prompt &optional initial-contents)
  (if (fboundp 'minibuffer-with-setup-hook) ; Emacs 22+
      (let ((minibuffer-completing-symbol  t))
        (minibuffer-with-setup-hook
            (lambda ()       ; Vanilla Emacs FIXME: call `emacs-lisp-mode'?
              (when (fboundp 'add-function) ; Emacs 24+
                (add-function :before-until (local 'eldoc-documentation-function)
                              #'elisp-eldoc-documentation-function)
                (eldoc-mode 1))
              (add-hook 'completion-at-point-functions #'elisp-completion-at-point nil t)
              (run-hooks 'eval-expression-minibuffer-setup-hook))
          (read-from-minibuffer prompt initial-contents (if (boundp 'pp-read-expression-map) ;In  `pp+.el'
                                                            pp-read-expression-map
                                                          read-expression-map)
                                t 'read-expression-history)))
    (read-from-minibuffer "Function or sexp: " nil nil nil 'read-expression-history))) ; Emacs 20-21

;; FIXME: Factor out code that is common with `dired-mark-sexp'.
;;
(when (fboundp 'minibuffer-with-setup-hook) ; Emacs 22+

  ;; Just to help discovery.  And can't use the name `dired-mark-if', since that's a macro.
  ;;
  (defalias 'diredp-mark-if-sexp-recursive 'dired-mark-sexp-recursive)
  ;;
  (defun diredp-mark-sexp-recursive (predicate &optional arg details) ; Bound to `M-+ M-(', `M-+ * ('
    "Mark files here and below for which PREDICATE returns non-nil.
Like `diredp-mark-sexp', but act recursively on subdirs.

A non-negative prefix arg means to unmark those files instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

PREDICATE is a lisp sexp that can refer to the following symbols as
variables:

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
     First, Dired just the source files: `dired *.el'.
     Then, use \\[diredp-mark-sexp-recursive] with this sexp:
          (not (file-exists-p (concat name \"c\")))

There's an ambiguity when a single integer not followed by a unit
prefix precedes the file mode: It is then parsed as inode number
and not as block size (this always works for GNU coreutils ls).

Another limitation is that the uid field is needed for the
function to work correctly.  In particular, the field is not
present for some values of `ls-lisp-emulation'.

This function operates only on the Dired buffer content.  It does not
refer at all to the underlying file system.  Contrast this with
`find-dired', which might be preferable for the task at hand.

When called from Lisp, DETAILS is passed to `diredp-get-subdirs'."
    ;; Using `sym' = "", instead of nil, for non-linked files avoids the trap of
    ;; (string-match "foo" sym) into which a user would soon fall.
    ;; Use `equal' instead of `=' in the example, as it works on integers and strings.
    ;; (interactive "xMark if (vars: inode,blks,mode,nlink,uid,gid,size,time,name,sym): \nP")

    (interactive
     (let* ((numarg  (and current-prefix-arg  (prefix-numeric-value current-prefix-arg)))
            (unmark  (and numarg  (>= numarg 0))))
       (diredp-get-confirmation-recursive)
       (list (diredp-read-expression (format "%s if (Lisp expr): " (if current-prefix-arg "UNmark" "Mark")))
             current-prefix-arg
             diredp-list-file-attributes)))
    (message "%s" predicate)
    (let* ((numarg             (and arg  (prefix-numeric-value arg)))
           (unmark             (and numarg  (>= numarg 0)))
           (ignorep            (and numarg  (<= numarg 0)))
           (dired-marker-char  (if unmark ?\040 dired-marker-char))
           (inode              nil)
           (blks               ())
           (matched            0)
           (changed            0)
           dbufs chg.mtch mode nlink uid gid size time name sym)
      (dolist (dir  (cons default-directory (diredp-get-subdirs ignorep nil details)))
        (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
          (with-current-buffer (car dbufs)
            (setq chg.mtch
                  (diredp-mark-if
                   (save-excursion
                     (and
                      ;; Sets vars INODE BLKS MODE NLINK UID GID SIZE TIME NAME and SYM
                      ;; according to current file line.  Returns `t' for success, nil if
                      ;; there is no file line.  Upon success, these vars are set, to either
                      ;; nil or the appropriate value, so they need not be initialized.
                      ;; Moves point within the current line.
                      (dired-move-to-filename)
                      (let ((mode-len             10) ; Length of `mode' string.
                            ;; As in `dired.el', but with subexpressions \1=inode, \2=blks:
                            ;; GNU `ls -hs' suffixes the block count with a unit and prints it as a float
                            ;; FreeBSD does neither.
                            ;; $$$$$$ (dired-re-inode-size  "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?")
                            (dired-re-inode-size  (if (> emacs-major-version 24)
                                                      "\\=\\s *\\([0-9]+\\s +\\)?\
\\(?:\\([0-9]+\\(?:\\.[0-9]*\\)?[BkKMGTPEZY]?\\)? ?\\)"
                                                    "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?"))
                            pos)
                        (beginning-of-line)
                        (forward-char 2)
                        (search-forward-regexp dired-re-inode-size nil t)
                        ;; `INODE', `BLKS', `MODE'
                        ;; XXX Might be a size not followed by a unit prefix.
                        ;; Could set `blks' to `inode' if it were otherwise nil, with similar reasoning
                        ;; as for setting `gid' to `uid', but it would be even more whimsical.
                        (setq inode  (and (match-string 1)  (string-to-number (match-string 1)))
                              blks   (and (match-string 2)  (if (fboundp 'dired-x--string-to-number) ; Emacs 25+
                                                                (dired-x--string-to-number (match-string 2))
                                                              (string-to-number (match-string 2))))
                              mode   (buffer-substring (point) (+ mode-len (point))))
                        (forward-char mode-len)
                        ;; Skip any extended attributes marker ("." or "+").
                        (unless (eq (char-after) ?\   ) (forward-char 1))
                        (setq nlink  (read (current-buffer))) ; `NLINK'

                        ;; `UID'
                        ;; Another issue is that GNU `ls -n' right-justifies numerical UIDs and GIDs,
                        ;; while FreeBSD left-justifies them, so do not rely on a specific whitespace
                        ;; layout.  Both of them right-justify all other numbers, though.
                        ;; XXX Return a number if the `uid' or `gid' seems to be numerical?
                        ;; $$$$$$ (setq uid  (buffer-substring (+ (point) 1) (progn (forward-word 1) (point))))
                        (setq uid  (buffer-substring (progn (skip-chars-forward " \t")  (point))
                                                     (progn (skip-chars-forward "^ \t") (point))))
                        (cond ((> emacs-major-version 24)
                               (dired-move-to-filename)
                               (save-excursion
                                 (setq time ; `TIME'
                                       ;; The regexp below tries to match from the last digit of the size
                                       ;; field through a space after the date.  Also, dates may have
                                       ;; different formats depending on file age, so the date column need
                                       ;; not be aligned to the right.
                                       (buffer-substring
                                        (save-excursion (skip-chars-backward " \t") (point))
                                        (progn (re-search-backward directory-listing-before-filename-regexp)
                                               (skip-chars-forward "^ \t")
                                               (1+ (point))))

                                       size ; `SIZE'
                                       (dired-x--string-to-number
                                        ;; We know that there's some kind of number before point because
                                        ;; the regexp search above succeeded.  Not worth doing an extra
                                        ;; check for leading garbage.
                                        (buffer-substring (point) (progn (skip-chars-backward "^ \t") (point))))
                                       ;; If no `gid' is displayed, `gid' will be set to `uid' but user
                                       ;; will then not reference it anyway in PREDICATE.

                                       gid ; `GID'
                                       (buffer-substring (progn (skip-chars-backward " \t") (point))
                                                         (progn (skip-chars-backward "^ \t") (point)))))
                               ;; `NAME', `SYM'
                               (setq name  (buffer-substring (point)
                                                             (or (dired-move-to-end-of-filename t)  (point)))
                                     sym   (if (diredp-looking-at-p " -> ")
                                               (buffer-substring (progn (forward-char 4) (point))
                                                                 (line-end-position))
                                             "")))
                              (t
                               (re-search-forward
                                (if (< emacs-major-version 20)
                                    "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)"
                                  dired-move-to-filename-regexp))
                               (goto-char (match-beginning 1))
                               (forward-char -1)
                               (setq size ; `SIZE'
                                     (string-to-number (buffer-substring (save-excursion (backward-word 1)
                                                                                         (setq pos  (point)))
                                                                         (point))))
                               (goto-char pos)
                               (backward-word 1)
                               ;; `GID', `TIME', `NAME', `SYM'
                               ;; if no `gid' is displayed, `gid' will be set to `uid' but user will then
                               ;; not reference it anyway in PREDICATE.
                               (setq gid   (buffer-substring (save-excursion (forward-word 1) (point)) (point))
                                     time  (buffer-substring (match-beginning 1) (1- (dired-move-to-filename)))
                                     name  (buffer-substring (point) (or (dired-move-to-end-of-filename t)
                                                                         (point)))
                                     sym   (if (diredp-looking-at-p " -> ")
                                               (buffer-substring (progn (forward-char 4) (point))
                                                                 (line-end-position))
                                             "")))))
                      ;; Vanilla Emacs uses `lexical-binding' = t, and it passes bindings to `eval'
                      ;; as a second arg.  We use `lexical-binding' = nil, and anyway there should
                      ;; be no need to pass the bindings.
                      (eval predicate)))
                   (format "'%s file" predicate)))
            (setq changed   (+ changed (or (car chg.mtch)  0))
                  matched   (+ matched (or (cdr chg.mtch)  0))))))
      (message "%s file%s%s%s newly %s" matched (dired-plural-s matched)
               (if (not (= matched changed)) " matched, " "")
               (if (not (= matched changed)) changed "")
               (if (eq ?\040 dired-marker-char) "unmarked" "marked"))))

  )

;;;###autoload
(defun diredp-mark-autofiles-recursive (&optional arg details) ; Bound to `M-+ * B'
  "Mark all autofiles, including in marked subdirs.
Autofiles are files that have an autofile bookmark.
A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'."
  (interactive (list current-prefix-arg diredp-list-file-attributes))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-recursive-1 arg "autofiles" "autofile"
                           '(and (not (diredp-looking-at-p dired-re-dot))  (not (eolp))
                             (let ((fname  (dired-get-filename nil 'NO-ERROR)))
                               (and fname  (bmkp-get-autofile-bookmark fname))))
                           details))

;;;###autoload
(defun diredp-mark-executables-recursive (&optional arg details) ; Bound to `M-+ * *'
  "Mark all executable files, including in marked subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'."
  (interactive (list current-prefix-arg diredp-list-file-attributes))
  (diredp-mark-recursive-1 arg "executable files" "executable file" '(diredp-looking-at-p dired-re-exe) details))

;;;###autoload
(defun diredp-mark-directories-recursive (&optional arg details) ; Bound to `M-+ * /'
  "Mark all directories except `.' and `..', including in marked subdirs.
The directories included are those that are marked in the current
Dired buffer, or all subdirs in the directory if none are marked.
Marked subdirectories are handled recursively in the same way.

A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'."
  (interactive (list current-prefix-arg diredp-list-file-attributes))
  (diredp-mark-recursive-1 arg "directories" "directory" '(and (diredp-looking-at-p dired-re-dir)
                                                           (not (diredp-looking-at-p dired-re-dot)))
                           details))
;;;###autoload
(defun diredp-mark-symlinks-recursive (&optional arg details) ; Bound to `M-+ * @'
  "Mark all symbolic links, including in marked subdirs.
The symlinks included are those that are marked in the current Dired
buffer, or all symlinks in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-subdirs'."
  (interactive (list current-prefix-arg diredp-list-file-attributes))
  (diredp-mark-recursive-1 arg "symlinks" "symbolic link" '(diredp-looking-at-p dired-re-sym) details))

(defun diredp-mark-recursive-1 (arg plural singular predicate-sexp details)
  "Helper for `diredp-mark-*-recursive' commands."
  (let* ((numarg             (and arg  (prefix-numeric-value arg)))
         (unmark             (and numarg  (>= numarg 0)))
         (ignorep            (and numarg  (<= numarg 0)))
         (dired-marker-char  (if unmark ?\040 dired-marker-char))
         (sdirs              (diredp-get-subdirs ignorep nil details))
         (changed            0)
         (matched            0)
         dbufs chg.mtch)
    (message "%s %s..." (if (eq ?\040 dired-marker-char) "UNmarking" "Marking") plural)
    (dolist (dir  (cons default-directory sdirs))
      (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
        (with-current-buffer (car dbufs)
          (setq chg.mtch  (diredp-mark-if (eval predicate-sexp) singular)
                changed   (+ changed (or (car chg.mtch)  0))
                matched   (+ matched (or (cdr chg.mtch)  0))))))
    (message "%s %s%s%s newly %s"
             matched
             (if (= 1 matched) singular plural)
             (if (not (= matched changed)) " matched, " "")
             (if (not (= matched changed)) changed "")
             (if (eq ?\040 dired-marker-char) "unmarked" "marked"))))

;;;###autoload
(defun diredp-capitalize-recursive (&optional ignore-marks-p details) ; Bound to `M-+ % c'
  "Rename marked files, including in marked subdirs, by capitalizing them.
Like `diredp-capitalize', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-create-files-non-directory-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-create-files-non-directory-recursive
   #'dired-rename-file #'capitalize "Rename by capitalizing:" ignore-marks-p details))

;;;###autoload
(defun diredp-upcase-recursive (&optional ignore-marks-p details) ; Bound to `M-+ % u'
  "Rename marked files, including in marked subdirs, making them uppercase.
Like `dired-upcase', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-create-files-non-directory-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-create-files-non-directory-recursive
   #'dired-rename-file #'upcase "Rename to uppercase:" ignore-marks-p details))

;;;###autoload
(defun diredp-downcase-recursive (&optional ignore-marks-p details) ; Bound to `M-+ % l'
  "Rename marked files, including in marked subdirs, making them lowercase.
Like `dired-downcase', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-create-files-non-directory-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-create-files-non-directory-recursive
   #'dired-rename-file #'downcase "Rename to lowercase:" ignore-marks-p details))

;;;###autoload
(defun diredp-do-aggregate-apply-to-marked-recursive (aggregate-fun apply-fun &optional ignore-marks-p details msgp)
  "Aggregate function applications to marked files, including in marked subdirs.
This command prompts you for the aggregate function, AGGREGATE-FUN,
and for the function to apply to each file, APPLY-FUN.

Then it calls `diredp-do-apply-to-marked-recursive', which applies
FILE-FUN to the name of each marked file or dir.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive
   (progn
     (diredp-get-confirmation-recursive)
     (list (read (completing-read "Aggregate function: " obarray 'functionp nil nil
                                  (and (boundp 'function-name-history)  'function-name-history)))
           (read (completing-read "Apply function (to file): " obarray 'functionp nil nil
                                  (and (boundp 'function-name-history)  'function-name-history)))
           current-prefix-arg
           diredp-list-file-attributes
           t)))
  (diredp-ensure-fn-nonzero-arity aggregate-fun)
  (let ((result  (apply aggregate-fun (diredp-do-apply-to-marked-recursive apply-fun ignore-marks-p details))))
    (when msgp (let ((print-length  nil)
                     (print-level   nil))
                 (message "Result: %S" result)))))

;;; (defun diredp-do-aggregate-invoke-in-marked-recursive (aggregate-fun invoke-fun
;;;                                                                      &optional ignore-marks-p details msgp)
;;;   "Aggregate results of invoking a function in the marked files and dirs.
;;; This command prompts you for the aggregate function, AGGREGATE-FUN,
;;; and for the function to invoke in each marked file or dir, INVOKE-FUN.
;;; 
;;; Then it calls `diredp-do-invoke-in-marked-recursive', which calls
;;; INVOKE-FUN at the start of the file, with no args.
;;; 
;;; The files included are those that are marked in the current Dired
;;; buffer, or all files in the directory if none are marked.  Marked
;;; subdirectories are handled recursively in the same way.
;;; 
;;; With a prefix argument, ignore all marks - include all files in this
;;; Dired buffer and all subdirs, recursively.
;;; 
;;; When called from Lisp, optional arg DETAILS is passed to
;;; `diredp-get-files'."
;;;   (interactive
;;;    (progn
;;;      (diredp-get-confirmation-recursive)
;;;      (list (read (completing-read "Aggregate function: " obarray 'functionp nil nil
;;;                                   (and (boundp 'function-name-history)  'function-name-history)))
;;;            (read (completing-read "Invoke function (in file): " obarray 'functionp nil nil
;;;                                   (and (boundp 'function-name-history)  'function-name-history)))
;;;            current-prefix-arg
;;;            diredp-list-file-attributes
;;;            t)))
;;;   (diredp-ensure-fn-zero-arity aggregate-fun)
;;;   (let ((result  (apply aggregate-fun (diredp-do-invoke-in-marked-recursive invoke-fun ignore-marks-p details))))
;;;     (when msgp (let ((print-length  nil)
;;;                      (print-level   nil))
;;;                  (message "Result: %S" result)))))

;;;###autoload
(defun diredp-do-aggregate-eval-in-marked-recursive (aggregate-fun sexp &optional ignore-marks-p details msgp)
  "Aggregate results of invoking a function in the marked files and dirs.
This command prompts you for the aggregate function, AGGREGATE-FUN,
and for the sexp to evaluate in each marked file or dir, INVOKE-FUN.

Then it calls `diredp-do-eval-in-marked-recursive', which evaluates
SEXP at the start of the file.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive
   (progn
     (diredp-get-confirmation-recursive)
     (list (read (completing-read "Aggregate function: " obarray 'functionp nil nil
                                  (and (boundp 'function-name-history)  'function-name-history)))
           (diredp-read-expression "Sexp: ")
           current-prefix-arg
           diredp-list-file-attributes
           t)))
  (diredp-ensure-fn-zero-arity aggregate-fun)
  (let ((result  (apply aggregate-fun (diredp-do-eval-in-marked-recursive sexp ignore-marks-p details))))
    (when msgp (let ((print-length  nil)
                     (print-level   nil))
                 (message "Result: %S" result)))))

(defun diredp-maybe-save-visited (files &optional predicate details)
  "Prompt to save some (or all) modified buffers visiting a file in FILES.
Optional args PREDICATE and DETAILS are passed to
 `diredp-y-or-n-files-p'."
  (let ((modified-bufs  (delq nil (mapcar (lambda (ff)
                                            (if (> emacs-major-version 21)
                                                (find-buffer-visiting ff (lambda (bb) (buffer-modified-p bb)))
                                              (find-buffer-visiting ff)))
                                          files))))
    (when (and modified-bufs
               (diredp-y-or-n-files-p
                "Save modified file buffers now? "
                (setq modified-bufs  (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) modified-bufs)))
                predicate
                details))
      (map-y-or-n-p (lambda (bb) (format "Save `%s'? " (buffer-file-name bb)))
                    (lambda (bb) (with-current-buffer bb (save-buffer)))
                    modified-bufs
                    '("buffer" "buffers" "save")
                    (and (boundp 'save-some-buffers-action-alist)
                         save-some-buffers-action-alist)))))

;;;###autoload
(defun diredp-do-apply-to-marked-recursive (function &optional ignore-marks-p details) ; Bound to `M-+ @ @'
  "Apply FUNCTION to names of marked files, including in marked subdirs.
The absolute file name is passed to FUNCTION.

Like `diredp-do-apply-to-marked' but act recursively on subdirs, and
do no result-logging, error-logging, or echoing.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive
   (progn
     (diredp-get-confirmation-recursive)
     (list (read (completing-read "Apply function: " obarray 'functionp nil nil
                                  (and (boundp 'function-name-history)  'function-name-history)))
           current-prefix-arg
           diredp-list-file-attributes)))
  (diredp-ensure-fn-nonzero-arity function)
  (let* ((files     (diredp-get-files ignore-marks-p nil nil nil nil details))
         (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
         (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
         (new-bufs  ())
         (result    ()))
    (diredp-maybe-save-visited files nil details)
    (setq result    (mapcar function files)
          new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
    (message nil) ; Clear echo area of anything inserted by FUNCTION.
    (when (and new-bufs
               (diredp-y-or-n-files-p
                "New buffers of visited files.  Kill some? "
                (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                nil
                details))
      (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                    #'kill-buffer
                    new-bufs
                    '("buffer" "buffers" "kill")
                    save-some-buffers-action-alist))
    result))

;;;###autoload
(defun diredp-do-command-in-marked-recursive (command &optional arg details) ; Bound to `M-+ @ M-x'
  "Invoke Emacs COMMAND in each marked file, including in marked subdirs.
Like ‘diredp-do-command-in-marked’ but act recursively on subdirs.
Errors invoking COMMAND are reported.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

A plain prefix arg (`C-u') means ignore all marks - include all files
in this Dired buffer and all subdirs, recursively.

Any other prefix arg is passed to COMMAND as its prefix arg.  For
example, if COMMAND uses a numeric prefix arg N for repetition then
the effect of COMMAND is repeated N times.

Other than the prefix arg, no arguments are passed to COMMAND.

When called from Lisp, optional arg DETAILS is passed to
‘diredp-get-files’.

Be aware that COMMAND could be invoked in a directory (Dired) buffer.
You may not want to do this if COMMAND modifies the buffer text.
\(But generally this will have little lasting effect - you can just
use `g' in that buffer to revert the listing.)"
  (interactive (progn (diredp-get-confirmation-recursive)
                      (list (diredp-read-command "Invoke Emacs command: ")
                            current-prefix-arg
                            diredp-list-file-attributes)))
  (unless (commandp command) (error "Not a command: `%s'" command))
  (unless (or (and (fboundp 'kmacro-keyboard-macro-p) ; Emacs 27+
                   (kmacro-keyboard-macro-p command)) ; Keyboard macro
              (arrayp (symbol-function command)) ; Keyboard macro
              (not (fboundp 'func-arity))        ; Emacs < 26
              (= 0 (car (func-arity command))))
    (error "Command `%s' cannot accept zero args" command))
  (let* ((ignore-marks-p  (and (consp arg)  (< (car arg) 16)))
         (files           (diredp-get-files ignore-marks-p nil nil nil nil details))
         (count           (length files))
         (fbufs           (delq nil (mapcar #'find-buffer-visiting files)))
         (mod-bufs        (diredp-remove-if-not #'buffer-modified-p fbufs))
         (new-bufs        ())
         (failures        ()))
    (diredp-maybe-save-visited files nil details)
    (dolist (file  files)
      (save-selected-window
        (condition-case err
            (with-current-buffer (find-file-noselect file)
              (save-excursion (goto-char (point-min))
                              (let ((current-prefix-arg  (and (not ignore-marks-p)  arg)))
                                (command-execute command))))
          (error (dired-log "File: %s\n  %s\n" file (error-message-string err))
                 (setq failures  (cons file failures))))))
    (message nil) ; Clear echo area of anything inserted there by COMMAND.
    (when failures
      (dired-log-summary (format "%d of %d failed" (length failures) count) failures))
    (setq new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
    (when (and new-bufs
               (diredp-y-or-n-files-p
                "New buffers of visited files.  Kill some? "
                (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                nil
                details))
      (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                    #'kill-buffer
                    new-bufs
                    '("buffer" "buffers" "kill")
                    save-some-buffers-action-alist))))

;;;; (defun diredp-do-invoke-in-marked-recursive (function &optional ignore-marks-p details) ; Bound to `M-+ @ M-@'
;;;;   "Invoke FUNCTION in each of the marked files, including in marked subdirs.
;;;; No arguments are passed to FUNCTION.
;;;;
;;;; Like `diredp-do-invoke-in-marked' but act recursively on subdirs, and
;;;; do no result-logging, error-logging, or echoing.
;;;;
;;;; The files included are those that are marked in the current Dired
;;;; buffer, or all files in the directory if none are marked.  Marked
;;;; subdirectories are handled recursively in the same way.
;;;;
;;;; With a prefix argument, ignore all marks - include all files in this
;;;; Dired buffer and all subdirs, recursively.
;;;;
;;;; When called from Lisp, optional arg DETAILS is passed to
;;;; `diredp-get-files'."
;;;;   (interactive
;;;;    (progn
;;;;      (diredp-get-confirmation-recursive)
;;;;      (list (read (completing-read "Invoke function: " obarray 'functionp nil nil
;;;;                                   (and (boundp 'function-name-history)  'function-name-history)))
;;;;            current-prefix-arg
;;;;            diredp-list-file-attributes)))
;;;;   (diredp-ensure-fn-zero-arity function)
;;;;   (let* ((files     (diredp-get-files ignore-marks-p nil nil nil nil details))
;;;;          (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
;;;;          (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
;;;;          (new-bufs  ())
;;;;          (result    ()))
;;;;     (diredp-maybe-save-visited files nil details)
;;;;     (setq result    (mapcar (lambda (file)
;;;;                               (with-current-buffer (find-file-noselect file)
;;;;                                 (save-excursion (goto-char (point-min)) (funcall function))))
;;;;                             files)
;;;;           new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
;;;;     (message nil) ; Clear echo area of anything inserted by invoking FUNCTION.
;;;;     (when (and new-bufs
;;;;                (diredp-y-or-n-files-p
;;;;                 "New buffers of visited files.  Kill some? "
;;;;                 (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
;;;;                 nil
;;;;                 details))
;;;;       (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
;;;;                     #'kill-buffer
;;;;                     new-bufs
;;;;                     '("buffer" "buffers" "kill")
;;;;                     save-some-buffers-action-alist))
;;;;     result))

;;;###autoload
(defun diredp-do-eval-in-marked-recursive (sexp &optional ignore-marks-p details) ; Bound to `M-+ @ M-:'
  "Evaluate SEXP in each of the marked files, including in marked subdirs.
Like `diredp-do-eval-in-marked' but act recursively on subdirs, and do
no result-logging, error-logging, or echoing.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive
   (progn (diredp-get-confirmation-recursive)
          (list (diredp-read-expression "Eval sexp: ")
                current-prefix-arg
                diredp-list-file-attributes)))
  (let* ((files     (diredp-get-files ignore-marks-p nil nil nil nil details))
         (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
         (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
         (new-bufs  ())
         (result    ()))
    (diredp-maybe-save-visited files nil details)
    (setq result    (mapcar (lambda (file)
                              (with-current-buffer (find-file-noselect file)
                                (save-excursion (goto-char (point-min)) (eval-expression sexp))))
                            files)
          new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
    (message nil)       ; Clear echo area of anything inserted by evaluating SEXP.
    (when (and new-bufs
               (diredp-y-or-n-files-p
                "New buffers of visited files.  Kill some? "
                (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                nil
                details))
      (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                    #'kill-buffer
                    new-bufs
                    '("buffer" "buffers" "kill")
                    save-some-buffers-action-alist))
    result))

;;;###autoload
(defun diredp-do-apply/eval-marked-recursive (fun/sexp &optional arg details) ; Not bound
  "Apply a function to marked files, or evaluate a sexp in them.
Like `diredp-do-apply/eval-marked' but act recursively on subdirs, and do no
result--logging, error-logging or echoing.

The files acted on are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a plain prefix ARG (`C-u'), visit each file and eval the sexp or
 invoke the function there.  (The function is passed no arguments.)
Otherwise, apply the function to each file name.

With any other prefix arg, ignore all marks - apply the function to
each file in this Dired buffer and all subdirs, recursively.

When called from Lisp:
 * If ARG is `(4)' then invoke the function, or eval the sexp, while
   visiting each file.
 * Optional arg DETAILS is passed to `diredp-get-files'."
  (interactive
   (let ((use-no-args-p  (and (consp current-prefix-arg)  (< (car current-prefix-arg) 16))))
     (diredp-get-confirmation-recursive)
     (list (if use-no-args-p
               (diredp-read-expression "Invoke function or eval sexp: ")
             (read (completing-read "Apply function: " obarray 'functionp nil nil
                                    (and (boundp 'function-name-history)  'function-name-history))))
           current-prefix-arg
           diredp-list-file-attributes)))
  (let ((use-no-args-p  (and (consp arg)  (< (car arg) 16))))
    (if use-no-args-p
        (when (functionp fun/sexp)
          (unless (or (not (fboundp 'func-arity)) ; Emacs < 26
                      (= 0 (car (func-arity fun/sexp))))
            (error "Function `%s' cannot accept zero args" fun/sexp))) ; Function to invoke in visited file.
      (unless (or (not (fboundp 'func-arity)) ; Emacs < 26
                  (let ((max  (cdr (func-arity fun/sexp))))
                    (or (eq max 'many)  (> max 0))))
        (error "Function `%s' cannot accept any args" fun/sexp))) ; Function to apply to file name.
    (let* ((files     (diredp-get-files (and (not use-no-args-p)  arg) nil nil nil nil details))
           (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
           (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
           (new-bufs  ())
           (result    ()))
      (diredp-maybe-save-visited files nil details)
      (setq result    (if use-no-args-p
                          (mapcar (lambda (file)
                                    (with-current-buffer (find-file-noselect file)
                                      (save-excursion (goto-char (point-min))
                                                      (if (functionp fun/sexp)
                                                          (funcall fun/sexp)
                                                        (eval-expression fun/sexp)))))
                                  files)
                        (mapcar (lambda (file) (funcall fun/sexp file)) files))
            new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
      (message nil) ; Clear echo area of anything inserted by FUN/SEXP.
      (when (and new-bufs
                 (diredp-y-or-n-files-p
                  "New buffers of visited files.  Kill some? "
                  (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                  nil
                  details))
        (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                      #'kill-buffer
                      new-bufs
                      '("buffer" "buffers" "kill")
                      save-some-buffers-action-alist))
      result)))

;;;###autoload
(defun diredp-do-delete-recursive (arg &optional details) ; Bound to `M-+ D'
  "Delete marked (not flagged) files, including in marked subdirs.
Like `dired-do-delete' but act recursively on subdirs.

The files to be deleted are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files' and `diredp-get-subdirs'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (let* ((flip                       (zerop (prefix-numeric-value arg)))
         (delete-by-moving-to-trash  (and (boundp 'delete-by-moving-to-trash)
                                          (if flip
                                              (not delete-by-moving-to-trash)
                                            delete-by-moving-to-trash))))
    (when flip (setq arg  nil))
    (unless arg
      (ding)
      (message "NOTE: Deletion of files marked `%c' (not those flagged `%c')."
               dired-marker-char dired-del-marker))
    (let* ((files  (diredp-get-files nil nil nil nil 'ONLY-MARKED-P details))
           (count  (length files))
           (succ   0))
      (if (dired-mark-pop-up
           " *Deletions*" 'delete files dired-deletion-confirmer
           (format "%s %s " (if delete-by-moving-to-trash "Trash" "Delete") (dired-mark-prompt arg files)))
          (let ((progress-reporter  (and (fboundp 'make-progress-reporter)
                                         (make-progress-reporter
                                          (if delete-by-moving-to-trash "Trashing..." "Deleting...")
                                          succ
                                          count)))
                (failures           ()))
            (unless progress-reporter (message "Deleting..."))
            (dolist (file  files)
              (condition-case err
                  (progn (if (fboundp 'dired-delete-file) ; Emacs 22+
                             (dired-delete-file file dired-recursive-deletes delete-by-moving-to-trash)
                           ;; This test is equivalent to (and (file-directory-p file)  (not (file-symlink-p file)))
                           ;; but more efficient.
                           (if (eq t (car (file-attributes file))) (delete-directory file) (delete-file file)))
                         (setq succ  (1+ succ))
                         (when (fboundp 'progress-reporter-update)
                           (progress-reporter-update progress-reporter succ)))
                (error (dired-log "%s\n" err) ; Catch errors from failed deletions.
                       (setq failures  (cons file failures))))
              (dired-clean-up-after-deletion file))
            (if failures
                (dired-log-summary (format "%d of %d deletion%s failed"
                                           (length failures) count (dired-plural-s count))
                                   failures)
              (if (fboundp 'progress-reporter-done)
                  (progress-reporter-done progress-reporter)
                (message "Deleting...done")))
            (let ((sdirs  (diredp-get-subdirs nil nil details))
                  dbufs)
              (dolist (dir  (cons default-directory sdirs))
                (when (setq dbufs  (dired-buffers-for-dir dir)) ; Dirs with Dired buffers only.
                  (with-current-buffer (car dbufs) (dired-revert))))))
        (message "OK. NO deletions performed")))))

;;;###autoload
(defun diredp-do-move-recursive (&optional ignore-marks-p details) ; Bound to `M-+ R'
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
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-do-create-files-recursive #'dired-rename-file "Move" ignore-marks-p details))

;;;###autoload
(defun diredp-do-copy-recursive (&optional ignore-marks-p details) ; Bound to `M-+ C'
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
command `cp -d'.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-create-files-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (let ((dired-recursive-copies  nil))  ; Doesn't have to be nil, but let's not go overboard now.
    (diredp-do-create-files-recursive #'dired-copy-file "Copy" ignore-marks-p details)))

(defun diredp-do-create-files-recursive (file-creator operation ignore-marks-p &optional details)
  "Create a new file for each marked file, including those in marked subdirs.
Like `dired-do-create-files', but act recursively on subdirs, and
always keep markings.
Prompts for the target directory, in which to create the files.
FILE-CREATOR and OPERATION are as in `dired-create-files'.
Non-nil IGNORE-MARKS-P means ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (lexical-let* ((fn-list     (diredp-get-files ignore-marks-p nil nil nil nil details))
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

(defun diredp-create-files-non-directory-recursive (file-creator basename-constructor operation
                                                    &optional ignore-marks-p details)
  "Apply FILE-CREATOR + BASENAME-CONSTRUCTOR to non-dir part of marked names.
Like `dired-create-files-non-directory', but act recursively on subdirs.

The files acted on are those marked in the current Dired buffer, or
all files in the directory if none are marked.  Marked subdirectories
are handled recursively in the same way.

With non-nil IGNORE-MARKS-P, ignore all marks - include all files in
this Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (let (rename-non-directory-query)
    (dired-create-files
     file-creator
     operation
     (diredp-get-files ignore-marks-p nil nil nil nil details)
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

(defun diredp-do-chxxx-recursive (attribute-name program op-symbol &optional ignore-marks-p default details)
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
 Dired buffer and all subdirs, recursively.
DEFAULT is the default value for reading the mark string.
DETAILS is passed to `diredp-get-files' and
 `diredp-do-redisplay-recursive'."
  (let* ((this-buff      (current-buffer))
         (files          (diredp-get-files ignore-marks-p nil nil nil nil details))
         (prompt         (concat "Change " attribute-name " of %s to: "))
         (new-attribute  (if (> emacs-major-version 22)
                             (dired-mark-read-string prompt nil op-symbol ignore-marks-p files default)
                           (dired-mark-read-string prompt nil op-symbol ignore-marks-p files)))
         (operation      (concat program " " new-attribute))
         failures)
    (setq failures  (dired-bunch-files 10000 (function dired-check-process)
                                       (append (list operation program)
                                               (unless (string-equal new-attribute "")
                                                 (if (equal attribute-name "Timestamp")
                                                     (list "-t" new-attribute)
                                                   (list new-attribute)))
                                               (and (diredp-string-match-p "gnu" system-configuration)
                                                    '("--"))) ; --------------------------------
                                       files))
    (with-current-buffer this-buff (diredp-do-redisplay-recursive details 'MSGP))
    (when failures (dired-log-summary (format "%s: error" operation) nil))))

;;;###autoload
(defun diredp-do-chmod-recursive (&optional ignore-marks-p details) ; Bound to `M-+ M'
  "Change the mode of the marked files, including those in marked subdirs.
Symbolic modes like `g+w' are allowed.

Note that marked subdirs are not changed.  Their markings are used only
to indicate that some of their files are to be changed.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files' and `diredp-do-redisplay-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (let* ((files    (diredp-get-files ignore-marks-p nil nil nil nil details))
         (modestr  (and (stringp (car files))  (nth 8 (file-attributes (car files)))))
         (default  (and (stringp modestr)
                        (string-match "^.\\(...\\)\\(...\\)\\(...\\)$" modestr)
                        (replace-regexp-in-string "-" "" (format "u=%s,g=%s,o=%s"
                                                                 (match-string 1 modestr)
                                                                 (match-string 2 modestr)
                                                                 (match-string 3 modestr)))))
         (modes    (if (> emacs-major-version 22)
                       (dired-mark-read-string
                        "Change mode of marked files here and below to: " nil 'chmod nil files default)
                     (dired-mark-read-string
                      "Change mode of marked files here and below to: " nil 'chmod nil files))))
    (when (equal modes "") (error "No file mode specified"))
    (dolist (file  files)
      (set-file-modes file (or (and (diredp-string-match-p "^[0-7]+" modes)  (string-to-number modes 8))
                               (file-modes-symbolic-to-number modes (file-modes file)))))
    (diredp-do-redisplay-recursive details 'MSGP)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-do-chgrp-recursive (&optional ignore-marks-p details)
    "Change the group of the marked (or next ARG) files.
With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-chxxx-recursive'."
    (interactive (list current-prefix-arg diredp-list-file-attributes))
    (diredp-do-chxxx-recursive "Group" "chgrp" 'chgrp ignore-marks-p nil details)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-do-chown-recursive (&optional ignore-marks-p details)
    "Change the owner of the marked (or next ARG) files.
With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-chxxx-recursive'."
    (interactive (list current-prefix-arg diredp-list-file-attributes))
    (diredp-do-chxxx-recursive "Owner" dired-chown-program 'chown ignore-marks-p nil details)))

;;;###autoload
(defun diredp-do-touch-recursive (&optional ignore-marks-p details)
  "Change the timestamp of marked files, including those in marked subdirs.
This calls `touch'.  Like `dired-do-touch', but act recursively on
subdirs.  The subdirs inserted are those that are marked in the
current Dired buffer, or all subdirs in the directory if none are
marked.  Marked subdirectories are handled recursively in the same
way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-chxxx-recursive'."
  (interactive (progn (diredp-get-confirmation-recursive) (list current-prefix-arg diredp-list-file-attributes)))
  (diredp-do-chxxx-recursive "Timestamp" (if (boundp 'dired-touch-program)
                                             dired-touch-program ; Emacs 22+
                                           "touch")
                             'touch
                             ignore-marks-p
                             (format-time-string "%Y%m%d%H%M.%S" (current-time))
                             details))

;;;###autoload
(defun diredp-do-redisplay-recursive (&optional details msgp)
  "Redisplay marked file lines, including those in marked subdirs.
Non-nil MSGP means show status messages.
Like `dired-do-redisplay' with no args, but act recursively on
subdirs.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (progn (diredp-ensure-mode)
                      (unless (y-or-n-p "Act on all marked file lines in and UNDER this dir? ")
                        (error "OK, canceled"))
                      (list diredp-list-file-attributes t)))
  (when msgp (message "Redisplaying..."))
  (dolist (dir  (cons default-directory
                      (diredp-get-files nil #'file-directory-p 'INCLUDE-SUBDIRS 'DONT-ASK nil details)))
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

;;;###autoload
(defun diredp-change-ls-switches (switches)
  "Change `ls' switches for the current Dired buffer.
The prompt for the new switches shows the current switches, for info.
Possible default values (`M-n') are, in order:
 1. `dired-listing-switches' - your default switches for Dired
 2. `dired-subdir-switches' - your switches for subdir listings
 3. Current switches for this buffer (just reverts the listing)"
  (interactive
   (list (read-string
          (format "New ls switches - must contain -l (current: %s): " dired-actual-switches)
          nil
          'diredp-switches-history
          (diredp-delete-dups (delq nil (list dired-listing-switches dired-subdir-switches dired-actual-switches )))
          t)))
  (dired-sort-other switches))


;;; `diredp-marked(-other-window)' tries to treat SWITCHES, but SWITCHES seems to be ignored
;;; by `dired' when the DIRNAME arg is a cons, at least on MS Windows.  I filed Emacs bug #952
;;; on 2008-09-10, but this doesn't work in Emacs 20, 21, 22, or 23, so I don't know if it will
;;; ever be fixed.  If it is declared a non-bug and it doesn't work on any platforms, then I'll
;;; remove SWITCHES here, alas.

;;;###autoload
(defun diredp-marked (dirname &optional arg switches) ; Not bound
  "Open Dired on the marked files and directories.
A prefix ARG that is non-positive or a single `C-u' means prompt for
  `ls' listing switches, as in command `dired'.

A prefix ARG also specifies the files to use instead of the marked
  files (any markings are ignored), as follows:

 If ARG is positive or negative, use the files named on the next ARG
  lines.  E.g., 1 or -1: use the file on the current line; 3 or -3:
  use the file on the current line and those on the following 2 lines.

 If ARG is `C-u C-u', `C-u C-u C-u', or `C-u C-u C-u C-u', use _all_
  files in the Dired buffer, where:

    `C-u C-u' includes only files - NO directories
    `C-u C-u C-u' includes directories EXCEPT `.' and `..'
    `C-u C-u C-u C-u' includes ALL directories (even `.' and `..')

 If ARG is otherwise non-nil (e.g. 0, `M--' or a single `C-u'), use
  the file or dir of the current line (same as ARG = -1).

Use library Bookmark+ to bookmark the resulting Dired buffer, for
persistent access.

This command is particularly useful to dired a set of files and dirs
that results from some possibly long-lasting command, such as
`find-dired'.  This can be quicker than recalculating the set of files
to Dired, but the set of files used is not necessarily up-to-date with
respect to what a new calculation would produce.
___

See also command `diredp-define-snapshot-dired-commands'.  It defines
Dired commands (same-window and other-window) that, like
`diredp-marked', create a snapshot Dired buffer.  Put the command
defuns in your init file, for persistent access."
  (interactive (diredp-get-args-for-diredp-marked))
  (unless (or arg  (save-excursion (goto-char (point-min))
                                   (re-search-forward (dired-marker-regexp) nil t)))
    (error "No marked files"))
  (dired dirname switches))

;;;###autoload
(defun diredp-marked-other-window (dirname &optional arg switches) ; Bound to `C-M-*'
  "Same as `diredp-marked', but uses a different window."
  (interactive (diredp-get-args-for-diredp-marked))
  (unless (or arg  (save-excursion (goto-char (point-min))
                                   (re-search-forward (dired-marker-regexp) nil t)))
    (error "No marked files"))
  (dired-other-window dirname switches))

(defun diredp-get-args-for-diredp-marked ()
  "Get args for `diredp-marked(-other-window)'."
  (progn (diredp-ensure-mode)
         (let* ((raw    current-prefix-arg)
                (narg   (prefix-numeric-value raw))
                (non-0  (and raw  (not (zerop narg))))
                (+/-    (and raw  (atom raw)  non-0  (abs narg))) ; Use only forward direction
                (files  (or +/-  raw)))
           (when (and raw  (zerop narg)) (setq files  1))
           (list (cons (generate-new-buffer-name (buffer-name))
                       (dired-get-marked-files t files))
                 files
                 (and (or (equal raw '(4))  (< narg 1))  ; `C-u', negative, or 0
                      (read-string "Dired listing switches: " dired-listing-switches))))))

;; Similar to `dired-mark-extension' in `dired-x.el'.
;; The difference is that this uses prefix arg to unmark, not to determine the mark character.
;;;###autoload
(defun diredp-mark/unmark-extension (extension &optional unmark-p) ; Bound to `* .'
  "Mark all files with a certain EXTENSION for use in later commands.
A `.' is not automatically prepended to the string entered.
Non-nil prefix argument UNMARK-P means unmark instead of mark.

Non-interactively, EXTENSION is the extension (a string).  It can also
  be a list of extension strings.
Optional argument UNMARK-P is the prefix arg."
  (interactive (list (diredp-read-regexp (concat (if current-prefix-arg "UNmark" "Mark") "ing extension: "))
                     current-prefix-arg))
  (or (listp extension)  (setq extension  (list extension)))
  (dired-mark-files-regexp (concat ".";; Do not match names with nothing but an extension
                                   "\\("
                                   (mapconcat #'regexp-quote extension "\\|")
                                   "\\)$")
                           (and current-prefix-arg  ?\040)))

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
    (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))  (not (eolp))
                         (let* ((fname     (dired-get-filename nil 'NO-ERROR))
                                (bmk       (and fname  (bmkp-get-autofile-bookmark fname nil prefix)))
                                (btgs      (and bmk  (bmkp-get-tags bmk)))
                                (presentp  nil)
                                (allp      (and btgs  (catch 'diredp-m-f-t-an
                                                        (dolist (tag  tags)
                                                          (setq presentp  (assoc-default tag btgs nil t))
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
    (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))  (not (eolp))
                         (let* ((fname     (dired-get-filename nil 'NO-ERROR))
                                (bmk       (and fname
                                                (bmkp-get-autofile-bookmark fname nil prefix)))
                                (btgs      (and bmk  (bmkp-get-tags bmk)))
                                (presentp  nil)
                                (allp      (and btgs  (catch 'diredp-m-f-t-sna
                                                        (dolist (tag  tags)
                                                          (setq presentp  (assoc-default tag btgs nil t))
                                                          (when (if notallp (not presentp) presentp)
                                                            (throw 'diredp-m-f-t-sna t)))
                                                        nil))))
                           (if (null tags) (if notallp (not btgs) btgs) allp)))
                    (if notallp "some-tags-not-matching file" "some-tags-matching file"))))

;;;###autoload
(defun diredp-mark-files-tagged-all (tags &optional none-p prefix) ; `T m *'
  "Mark all files that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, mark all that are *not* tagged with *any* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags none-p nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-none (tags &optional allp prefix) ; `T m ~ +'
  "Mark all files that are not tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 no tags at all.
With a prefix arg, mark all that are tagged with *each* tag in TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags (not allp) nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-some (tags &optional somenotp prefix) ; `T m +'
  "Mark all files that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, mark all that are *not* tagged with *all* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-some/not-all tags somenotp nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-not-all (tags &optional somep prefix) ; `T m ~ *'
  "Mark all files that are not tagged with *all* TAGS.
As a special case, if TAGS is empty, then mark the files that have
 no tags at all.
With a prefix arg, mark all that are tagged with *some* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-some/not-all tags (not somep) nil prefix))

;;;###autoload
(defun diredp-mark-files-tagged-regexp (regexp &optional notp prefix) ; `T m %'
  "Mark files that have at least one tag that matches REGEXP.
With a prefix arg, mark all that are tagged but have no matching tags.
You need library `bookmark+.el' to use this command."
  (interactive (list (read-string "Regexp: ")
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))  (not (eolp))
                       (lexical-let* ((fname  (dired-get-filename nil 'NO-ERROR))
                                      (bmk    (and fname
                                                   (bmkp-get-autofile-bookmark fname nil prefix)))
                                      (btgs   (and bmk  (bmkp-get-tags bmk)))
                                      (anyp   (and btgs  (bmkp-some #'(lambda (tag)
                                                                        (diredp-string-match-p
                                                                         regexp
                                                                         (bmkp-tag-name tag)))
                                                                    btgs))))
                         (and btgs  (if notp (not anyp) anyp))))
                  "some-tag-matching-regexp file"))

;;;###autoload
(defun diredp-unmark-files-tagged-regexp (regexp &optional notp prefix) ; `T u %'
  "Unmark files that have at least one tag that matches REGEXP.
With a prefix arg, unmark all that are tagged but have no matching tags.
You need library `bookmark+.el' to use this command."
  (interactive (list (read-string "Regexp: ")
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (let ((dired-marker-char  ?\040))
    (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))  (not (eolp))
                         (lexical-let* ((fname  (dired-get-filename nil 'NO-ERROR))
                                        (bmk    (and fname  (bmkp-get-autofile-bookmark fname nil prefix)))
                                        (btgs   (and bmk  (bmkp-get-tags bmk)))
                                        (anyp   (and btgs (bmkp-some #'(lambda (tag)
                                                                         (diredp-string-match-p
                                                                          regexp
                                                                          (bmkp-tag-name tag)))
                                                                     btgs))))
                           (and btgs  (if notp (not anyp) anyp))))
                    "some-tag-matching-regexp file")))

;;;###autoload
(defun diredp-unmark-files-tagged-all (tags &optional none-p prefix) ; `T u *'
  "Unmark all files that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, unmark all that are *not* tagged with *any* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags none-p 'UNMARK prefix))

;;;###autoload
(defun diredp-unmark-files-tagged-none (tags &optional allp prefix) ; `T u ~ +'
  "Unmark all files that are *not* tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 no tags at all.
With a prefix arg, unmark all that are tagged with *each* tag in TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-all/none tags (not allp) 'UNMARK prefix))

;;;###autoload
(defun diredp-unmark-files-tagged-some (tags &optional somenotp prefix) ; `T u +'
  "Unmark all files that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 any tags at all.
With a prefix arg, unmark all that are *not* tagged with *all* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark-files-tagged-some/not-all tags somenotp 'UNMARK prefix))

;;;###autoload
(defun diredp-unmark-files-tagged-not-all (tags &optional somep prefix) ; `T u ~ *'
  "Unmark all files that are *not* tagged with *all* TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 no tags at all.
With a prefix arg, unmark all that are tagged with *some* TAGS.
You need library `bookmark+.el' to use this command."
  (interactive (list (and (fboundp 'bmkp-read-tags-completing)  (bmkp-read-tags-completing))
                     current-prefix-arg
                     (and diredp-prompt-for-bookmark-prefix-flag
                          (read-string "Prefix for autofile bookmark names: "))))
  (diredp-ensure-bookmark+)
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
  (interactive (progn (diredp-ensure-bookmark+)
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
Like `diredp-do-tag', but invoked using the mouse.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-tag (bmkp-read-tags-completing) prefix)) 1 'tag t))
  (diredp-previous-line 1))

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
  (interactive (progn (diredp-ensure-bookmark+)
                      (diredp-ensure-mode)
                      (list (bmkp-read-tags-completing)
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: "))
                            current-prefix-arg)))
  (dired-map-over-marks-check (lexical-let ((pref  prefix)) #'(lambda () (diredp-untag tags pref)))
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
Like `diredp-do-untag', but invoked using the mouse.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (lexical-let* ((bmk   (bmkp-get-autofile-bookmark  (dired-get-filename) nil prefix))
                   (btgs  (and bmk  (bmkp-get-tags bmk))))
      (unless btgs (error "File has no tags to remove"))
      (dired-map-over-marks-check #'(lambda () (diredp-untag (bmkp-read-tags-completing btgs) prefix)) 1 'untag t)))
  (diredp-previous-line 1))

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
  (interactive (progn (diredp-ensure-bookmark+)
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
Like `diredp-do-remove-all-tags', but invoked using the mouse.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-remove-all-tags prefix)) 1 'remove-all-tags t))
  (diredp-previous-line 1))

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
  (interactive (progn (diredp-ensure-bookmark+)
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
Like `diredp-do-paste-add-tags', but invoked using the mouse.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-paste-add-tags prefix))
                                1 'paste-add-tags t))
  (diredp-previous-line 1))

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
  (interactive (progn (diredp-ensure-bookmark+)
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
      (dired-make-relative file))))     ; Return file name for failure.

;;;###autoload
(defun diredp-mouse-do-paste-replace-tags (event) ; Not bound
  "In Dired, replace tags for this file with tags copied previously.
The tags were previously copied from a file to `bmkp-copied-tags'.
Like `diredp-do-paste-replace-tags', but invoked using the mouse.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-paste-replace-tags prefix))
                                1 'paste-replace-tags t))
  (diredp-previous-line 1))

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
  (interactive (progn (diredp-ensure-bookmark+)
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
Like `diredp-do-set-tag-value', but invoked using the mouse.
You need library `bookmark+.el' to use this command."
  (interactive "e")
  (diredp-ensure-bookmark+)
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
  (diredp-previous-line 1))


;; Define these even if `Bookmark+' is not loaded.
;;;###autoload
(defun diredp-mark-autofiles ()         ; Bound to `* B'
  "Mark all autofiles, that is, files that have an autofile bookmark."
  (interactive)
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark/unmark-autofiles))

;;;###autoload
(defun diredp-unmark-autofiles ()
  "Unmark all autofiles, that is, files that have an autofile bookmark."
  (interactive)
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (diredp-mark/unmark-autofiles t))

;;;###autoload
(defun diredp-mark/unmark-autofiles (&optional unmarkp)
  "Mark all autofiles, or unmark if UNMARKP is non-nil."
  (let ((dired-marker-char  (if unmarkp ?\040 dired-marker-char)))
    (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))  (not (eolp))
                         (let ((fname  (dired-get-filename nil 'NO-ERROR)))
                           (and fname  (bmkp-get-autofile-bookmark fname))))
                    "autofile")))

(when (and (fboundp 'bmkp-get-autofile-bookmark) ; Defined in `bookmark+-1.el'.
           (fboundp 'hlt-highlight-region)) ; Defined in `highlight.el'.

  (defun diredp-highlight-autofiles ()
    "Highlight files that are autofile bookmarks.
Highlighting uses face `diredp-autofile-name'."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward dired-move-to-filename-regexp nil t)
        ;; If Dired details are hidden the match data gets changed.
        (let* ((bmk    (save-match-data
                         (bmkp-get-autofile-bookmark (buffer-substring (match-end 0) (line-end-position)))))
               (tags  (and bmk  (bmkp-get-tags bmk))))
          (when bmk
            (hlt-highlight-region (match-end 0) (line-end-position)
                                  (if tags
                                      'diredp-tagged-autofile-name
                                    'diredp-autofile-name)))))))

  (cond ((fboundp 'define-minor-mode)
         ;; Emacs 21+.  Use `eval' so that even if the library is byte-compiled with Emacs 20,
         ;; loading it into Emacs 21+ will define variable `diredp-highlight-autofiles-mode'.
         (eval '(define-minor-mode diredp-highlight-autofiles-mode
                 "Toggle automatic highlighting of autofile bookmarks.
When you turn this on, it ensures that your bookmark file is loaded.

NOTE: This mode is ON BY DEFAULT.  More precisely, when `dired+.el' is
loaded (for the first time per Emacs session), the mode is turned ON.
To prevent this and have the mode OFF by default, you must do one of
the following:

 * Put (diredp-highlight-autofiles-mode -1) in your init file, AFTER
   it loads `dired+.el'.

 * Customize option `diredp-highlight-autofiles-mode' to `nil', AND
   ensure that your `custom-file' (or the `custom-saved-variables'
   part of your init file) is evaluated before `dired+.el' is loaded.

You need libraries `Bookmark and `highlight.el' for this command."
                 :init-value t :global t :group 'Dired-Plus :require 'dired+
                 (if (not diredp-highlight-autofiles-mode)
                     (remove-hook 'dired-after-readin-hook #'diredp-highlight-autofiles)
                   (add-hook 'dired-after-readin-hook #'diredp-highlight-autofiles)
                   (bookmark-maybe-load-default-file))
                 (when (derived-mode-p 'dired-mode) (dired-revert nil nil))
                 (when (interactive-p)
                   (message "Dired highlighting of autofile bookmarks is now %s"
                            (if diredp-highlight-autofiles-mode "ON" "OFF"))))))
        (t;; Emacs 20.
         (defun diredp-highlight-autofiles-mode (&optional arg)
           "Toggle automatic highlighting of autofile bookmarks.
When you turn this on, it ensures that your bookmark file is loaded.

NOTE: This mode is ON BY DEFAULT.  More precisely, when `dired+.el' is
loaded (for the first time per Emacs session), the mode is turned ON.
To prevent this and have the mode OFF by default, you must do one of
the following:

 * Put (diredp-highlight-autofiles-mode -1) in your init file, AFTER
   it loads `dired+.el'.

 * Customize option `diredp-highlight-autofiles-mode' to `nil', AND
   ensure that your `custom-file' (or the `custom-saved-variables'
   part of your init file) is evaluated before `dired+.el' is loaded.

You need libraries `Bookmark and `highlight.el' for this command."
           (interactive (list (or current-prefix-arg  'toggle)))
           (setq diredp-highlight-autofiles-mode  (if (eq arg 'toggle)
                                                      (not diredp-highlight-autofiles-mode)
                                                    (> (prefix-numeric-value arg) 0)))
           (if (not diredp-highlight-autofiles-mode)
               (remove-hook 'dired-after-readin-hook #'diredp-highlight-autofiles)
             (add-hook 'dired-after-readin-hook #'diredp-highlight-autofiles)
             (bookmark-maybe-load-default-file))
           (when (derived-mode-p 'dired-mode) (dired-revert nil nil))
           (when (interactive-p) (message "Dired highlighting of autofile bookmarks is now %s"
                                          (if diredp-highlight-autofiles-mode "ON" "OFF"))))))

  ;; Turn it ON BY DEFAULT.
  (unless (or (boundp 'diredp-loaded-p)  (get 'diredp-highlight-autofiles-mode 'saved-value))
    (diredp-highlight-autofiles-mode 1))
  )

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
  "In Dired, bookmark this file.  See `diredp-do-bookmark'.
Like `diredp-do-bookmark', but invoked using the mouse."
  (interactive "e")
  (lexical-let ((mouse-pos         (event-start event))
                (dired-no-confirm  t)
                (prefix            (and diredp-prompt-for-bookmark-prefix-flag
                                        (read-string "Prefix for bookmark name: "))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-bookmark prefix nil)) nil 'bookmark t))
  (diredp-previous-line 1))

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
        (if (fboundp 'bmkp-autofile-set) ; Bookmark+ - just set an autofile bookmark.
            (bmkp-autofile-set fil nil prefix nil (not no-msg-p))
          ;; Vanilla `bookmark.el' (or very old Bookmark+ version).
          (let ((bookmark-make-record-function
                 (cond ((and (require 'image nil t)  (require 'image-mode nil t)
                             (condition-case nil (image-type fil) (error nil)))
                        ;; Last two lines of function are from `image-bookmark-make-record'.
                        ;; But don't use that directly, because it uses
                        ;; `bookmark-make-record-default', which gets nil for `filename'.
                        (lambda ()
                          `((filename   . ,fil)
                            (position   . 0)
                            ;; NEED to keep this part of code sync'd with `bmkp-make-record-for-target-file'.
                            (image-type . ,(image-type fil))
                            (handler    . image-bookmark-jump)))) ; In `image-mode.el'.
                       (t
                        (lambda ()
                          `((filename . ,fil)
                            (position . 0)))))))
            (bookmark-store (concat prefix (file-name-nondirectory fil)) (cdr (bookmark-make-record)) nil)))
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
  (diredp-ensure-bookmark+)
  (diredp-do-bookmark-in-bookmark-file bookmark-file prefix arg 'CREATE-BOOKMARK-FILE-BOOKMARK))

;;;###autoload
(defun diredp-do-bookmark-in-bookmark-file (bookmark-file ; Bound to `C-M-B' (aka `C-M-S-b')
                                            &optional prefix arg bfile-bookmarkp files)
  "Bookmark marked files in BOOKMARK-FILE and save BOOKMARK-FILE.
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
  (diredp-ensure-bookmark+)
  (let ((bfile-exists-p  (file-readable-p bookmark-file)))
    (unless bfile-exists-p (bmkp-empty-file bookmark-file))
    (unless bmkp-current-bookmark-file (setq bmkp-current-bookmark-file  bookmark-default-file))
    (let ((old-bmkp-current-bookmark-file  bmkp-current-bookmark-file))
      (unwind-protect
           (progn (bmkp-switch-bookmark-file bookmark-file) ; Changes `*-current-bookmark-file'.
                  (if files
                      (dolist (file  files)  (diredp-bookmark prefix file 'NO-MSG-P))
                    (dired-map-over-marks-check
                     (lexical-let ((pref  prefix)) #'(lambda () (diredp-bookmark pref nil 'NO-MSG-P)))
                     arg 'bookmark (diredp-fewer-than-2-files-p arg)))
                  (bookmark-save)
                  (unless bfile-exists-p (revert-buffer)))
        (unless (bmkp-same-file-p old-bmkp-current-bookmark-file  bmkp-current-bookmark-file)
          (bmkp-switch-bookmark-file old-bmkp-current-bookmark-file 'NO-MSG))))
    (when bfile-bookmarkp (bmkp-set-bookmark-file-bookmark bookmark-file))))

(defun diredp-read-bookmark-file-args ()
  "Read args for `diredp-do-bookmark-in-bookmark-file' and similar."
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (list (let* ((insert-default-directory  t)
               (bmk-file                  (expand-file-name
                                           (read-file-name
                                            "Use bookmark file (default is in CURRENT dir): " nil
                                            (if (or (> emacs-major-version 23)
                                                    (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                                                (list ".emacs.bmk" bookmark-default-file)
                                              ".emacs.bmk")))))
          bmk-file)
        (and diredp-prompt-for-bookmark-prefix-flag  (read-string "Prefix for autofile bookmark names: "))
        current-prefix-arg))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Allow for consp `dired-directory' too.
;; 2. Updated for Emacs 28: Added optional arg SUBDIRS-P.
;; 3. Fix for Emacs bug #52395.  Expand DIR with `default-directory', so `file-name-as-directory' gets applied to
;;    absolute name.  Otherwise, (dired-buffers-for-dir "~/some/dir/") returns nil, because the element in
;;    `dired-subdir-alist' is ("/the/home/dir/some/dir/" . #<buffer dir>), not ("~/some/dir/" . #<buffer dir>).
;;
(defun dired-buffers-for-dir (dir &optional file subdirs-p)
  "Return a list of buffers that Dired DIR (top level or in-situ subdir).
DIR is automatically expanded with `expand-file-name' using the
 `default-directory'.  If you need expansion relative to some other
 directory then do that before calling `dired-buffers-for-dir'.
If FILE is non-nil, include only those whose wildcard pattern (if any)
 matches FILE.
If SUBDIRS-P is non-nil, also include the dired buffers of directories
 below DIR.

The list is in reverse order of buffer creation, most recent last.
As a side effect, killed Dired buffers for DIR are removed from
 `dired-buffers'."
  (setq dir  (file-name-as-directory (expand-file-name dir)))
  (let (result buf)
    (dolist (elt  dired-buffers)
      (setq buf  (cdr elt))
      (cond ((null (buffer-name buf))   ; Buffer is killed - clean up.
             (setq dired-buffers  (delq elt dired-buffers)))
            ((dired-in-this-tree (car elt) dir)
             (with-current-buffer buf
               (and (or subdirs-p  (assoc dir dired-subdir-alist))
                    (or (null file)
                        (if (stringp dired-directory)
                            ;; Allow for consp `dired-directory' too.
                            (let ((wildcards  (file-name-nondirectory (if (consp dired-directory)
                                                                          (car dired-directory)
                                                                        dired-directory))))
                              (or (zerop (length wildcards))
                                  (diredp-string-match-p (dired-glob-regexp wildcards) file)))
                          (member (expand-file-name file dir) (cdr dired-directory))))
                    (setq result  (cons buf result)))))))
    result))


;; If you use library `files+.el', you need not use these commands
;; explicitly, because that library redefines `find-file-read-args' to
;; do the same thing, in Dired mode.  These are provided here in case
;; you want to bind them directly - for example, in case your code
;; does not use `find-file-read-args'.
;;
;;;###autoload
(defun diredp-find-a-file (filename &optional wildcards) ; Not bound
  "`find-file', but use file on current line as default (`M-n')."
  (interactive (diredp-find-a-file-read-args "Find file: " nil))
  (find-file filename wildcards))

;;;###autoload
(defun diredp-find-a-file-other-frame (filename &optional wildcards) ; Not bound
  "`find-file-other-frame', but use file under cursor as default (`M-n')."
  (interactive (diredp-find-a-file-read-args "Find file: " nil))
  (find-file-other-frame filename wildcards))

;;;###autoload
(defun diredp-find-a-file-other-window (filename &optional wildcards) ; Not bound
  "`find-file-other-window', but use file under cursor as default (`M-n')."
  (interactive (diredp-find-a-file-read-args "Find file: " nil))
  (find-file-other-window filename wildcards))

;;;###autoload
(defun diredp-find-a-file-read-args (prompt mustmatch) ; Not bound
  (list (lexical-let ((find-file-default  (abbreviate-file-name (dired-get-file-for-visit))))
          (minibuffer-with-setup-hook (lambda ()
                                        (setq minibuffer-default  find-file-default))
                                      (read-file-name prompt nil default-directory mustmatch)))
        t))

;;;###autoload
(defun diredp-find-file-reuse-dir-buffer () ; Not bound
  "Like `dired-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' unless (1) the target is a directory that is not
yet visited as a Dired buffer, and (2) the current (Dired) buffer is
not visited also in some other window (possibly in an iconified
frame)."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((file  (dired-get-file-for-visit)))
    (diredp--reuse-dir-buffer-helper file)))

;;;###autoload
(defun diredp-mouse-find-file-reuse-dir-buffer (event &optional find-file-func find-dir-func) ; Not bound
  "Like `dired-mouse-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' unless (1) the target is a directory that is not
yet visited as a Dired buffer, and (2) the current (Dired) buffer is
not visited also in some other window (possibly in an iconified
frame).

Non-nil optional args FIND-FILE-FUNC and FIND-DIR-FUNC specify
functions to visit the file and directory, respectively.
Defaults: `find-file' and `dired', respectively."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window  (posn-window (event-end event))
            pos     (posn-point (event-end event)))
      (unless (windowp window) (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file  (dired-get-file-for-visit)))
    (select-window window)
    (diredp--reuse-dir-buffer-helper file find-file-func find-dir-func)))

(defun diredp--reuse-dir-buffer-helper (file &optional find-file-func find-dir-func other-window)
  "Helper for commands `diredp-*-reuse-dir-buffer' commands.
Non-nil optional args FIND-FILE-FUNC and FIND-DIR-FUNC specify
functions to visit the file and directory, respectively.
Defaults: `find-file' and `dired', respectively.

Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' unless (1) the target is a directory that is not
yet visited as a Dired buffer, and (2) the current (Dired) buffer is
not visited also in some other window (possibly in an iconified
frame)."
  (setq find-file-func  (or find-file-func (if other-window #'find-file-other-window #'find-file))
        find-dir-func   (or find-dir-func  (if other-window #'dired-other-window #'dired)))
  (let (;; This binding prevents problems with preserving point in windows displaying Dired buffers, because
        ;; reverting a Dired buffer empties it, which changes the places where the markers used by
        ;; `switch-to-buffer-preserve-window-point' point.
        (switch-to-buffer-preserve-window-point  (and (boundp 'switch-to-buffer-preserve-window-point) ; Emacs 24+
                                                      (or (not (boundp 'dired-auto-revert-buffer))
                                                          (not dired-auto-revert-buffer))
                                                      switch-to-buffer-preserve-window-point))
        (find-file-run-dired                     t)
        (wins                                    ())
        (alt-find-file-func                      (if other-window
                                                     #'find-alternate-file-other-window
                                                   #'find-alternate-file))
        dir-bufs)
    (if (or (not (file-directory-p file)) ; New is a not a directory
            (dired-buffers-for-dir file) ; or there is a Dired buffer for it, even as a subdir.
            (and (setq dir-bufs  (dired-buffers-for-dir default-directory)) ; Dired bufs for current (old).
                 (progn
                   (dolist (buf  dir-bufs)
                     (setq wins  (append wins (get-buffer-window-list buf 'NOMINI 0))))
                   (setq wins  (delq nil wins))
                   (cdr wins))))        ; More than one window showing current Dired buffer.
        (if (file-directory-p file)
            (or (and (cdr dired-subdir-alist)  (dired-goto-subdir file)) ; New is a subdir inserted in current
                (funcall find-dir-func file))
          (funcall find-file-func (file-name-sans-versions file t)))
      (funcall alt-find-file-func (file-name-sans-versions file t)))))

;;;###autoload
(defalias 'toggle-diredp-find-file-reuse-dir 'diredp-toggle-find-file-reuse-dir)
;;;###autoload
(defun diredp-toggle-find-file-reuse-dir (force-p) ; Bound to `C-M-R' (aka `C-M-S-r')
  "Toggle whether Dired `find-file' commands reuse directories.
This applies also to `dired-w32-browser' commands and
`diredp-up-directory'.

A prefix arg specifies directly whether or not to reuse.
 If its numeric value is non-negative then reuse; else do not reuse.

To set the behavior as a preference (default behavior), put this in
your ~/.emacs, where VALUE is 1 to reuse or -1 to not reuse:

 (diredp-toggle-find-file-reuse-dir VALUE)

Note: This affects only these commands:

  `dired-find-file'
  `dired-mouse-find-file'

It does not affect the corresponding `-other-window' commands.  Note
too that, by default, mouse clicks to open files or directories open
in another window: command `diredp-mouse-find-file-other-window', not
`dired-mouse-find-file'.  If you want a mouse click to reuse a
directory then bind `mouse-2' to `dired-mouse-find-file' instead."
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
  (substitute-key-definition 'diredp-up-directory 'diredp-up-directory-reuse-dir-buffer dired-mode-map)
  (substitute-key-definition 'dired-find-file 'diredp-find-file-reuse-dir-buffer dired-mode-map)
  (substitute-key-definition 'dired-mouse-find-file 'diredp-mouse-find-file-reuse-dir-buffer dired-mode-map)
  ;; These commands are defined in `w32-browser.el' (for use with MS Windows).
  (substitute-key-definition 'dired-w32-browser 'dired-w32-browser-reuse-dir-buffer dired-mode-map)
  (substitute-key-definition 'dired-mouse-w32-browser 'dired-mouse-w32-browser-reuse-dir-buffer dired-mode-map)
  (message "Reusing Dired buffers is now ON"))

(defun diredp-make-find-file-keys-not-reuse-dirs ()
  "Make find-file keys not reuse Dired buffers (i.e. act normally)."
  (substitute-key-definition 'diredp-up-directory-reuse-dir-buffer 'diredp-up-directory dired-mode-map)
  (substitute-key-definition 'diredp-find-file-reuse-dir-buffer 'dired-find-file dired-mode-map)
  (substitute-key-definition 'diredp-mouse-find-file-reuse-dir-buffer 'dired-mouse-find-file dired-mode-map)
  ;; These commands are defined in `w32-browser.el' (for use with MS Windows).
  (substitute-key-definition 'dired-w32-browser-reuse-dir-buffer 'dired-w32-browser dired-mode-map)
  (substitute-key-definition 'dired-mouse-w32-browser-reuse-dir-buffer 'dired-mouse-w32-browser dired-mode-map)
  (message "Reusing Dired buffers is now OFF"))

;;;###autoload
(defun diredp-create-file-here (file)   ; Not bound
  "Create new FILE in the directory of this Dired listing.
You are prompted for the (relative) file name, which is relative to
the directory of the current listing.

If called from Lisp, the buffer must be in Dired (or a derived) mode,
and FILE is expanded in `default-directory'."
  (interactive
   (let* ((_IGNORE        (diredp-ensure-mode))
          (this-dir       default-directory)
          (this-subdir    (diredp-this-subdir))
          (on-dir-line-p  (atom this-subdir)))
     (unless on-dir-line-p ; Subdir header line or non-directory file.
       (setq this-subdir  (car this-subdir)))
     (let* ((input  (read-string (format "Create file in `%s': " this-subdir)))
            (file   (expand-file-name input this-subdir)))
       (while (or  (string= input "")  (file-exists-p file))
         (setq input  (read-string (if (string= input "")
                                       "Empty input.  Create file: "
                                     (format "`%s' already exists.  Create file: " file)))
               file   (expand-file-name input this-subdir)))
       (list file))))
  (unless (if (or (> emacs-major-version 23)
                  (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
              (called-interactively-p 'interactive)
            (interactive-p))
    (diredp-ensure-mode)
    (when (file-exists-p (expand-file-name file)) (error "File `%s' already exists" file)))
  (let ((failures  (dired-bunch-files
                    2
		    #'dired-check-process
		    (append
		     (list (concat dired-touch-program " " file) dired-touch-program)
		     (if (string-match-p "gnu" system-configuration) '("--") nil))
		    (list file))))
    (if failures
        (dired-log-summary (format "%s: error" 'touch) nil)
      (dired-relist-entry (expand-file-name file)))))

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

;;;###autoload
(defun diredp-omit-unmarked ()          ; Not bound
  "Omit lines of unmarked files.  Return the number of lines omitted."
  (interactive)
  (let ((old-modified-p  (buffer-modified-p))
        count)
    (dired-toggle-marks)
    (message "Omitting unmarked lines...")
    (setq count  (diredp-omit-marked))
    (dired-toggle-marks)                ; Marks all except `.', `..'
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
  (interactive (progn (require 'ediff)
                      (list (ediff-read-file-name ; In `ediff-util.el'.
                             (format "Compare %s with" (dired-get-filename t))
                             (dired-current-directory)
                             (let* ((file           (dired-get-filename))
                                    (file-sans-dir  (file-name-nondirectory file))
                                    (file-dir       (file-name-directory file))
                                    (file-at-mark   (and transient-mark-mode
                                                         mark-active
                                                         (save-excursion (goto-char (mark t))
                                                                         (dired-get-filename t 'NO-ERROR))))
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

(defun diredp-fewer-than-N-files-p (arg n)
  "Return non-nil iff fewer than N files are to be treated by dired.
More precisely, return non-nil iff ARG is nil and fewer than N
files are marked, or the absolute value of ARG is less than N."
  (if arg
      (and (integerp arg)  (< (abs arg) n)) ; Next or previous file (or none).
    (not (save-excursion                ; Fewer than two marked files.
           (goto-char (point-min))
           (re-search-forward (dired-marker-regexp) nil t n)))))

(defun diredp-fewer-than-2-files-p (arg)
  "Return non-nil iff fewer than two files are to be treated by dired.
More precisely, return non-nil iff ARG is nil and fewer than two
files are marked, or ARG is -1, 0 or 1."
  (diredp-fewer-than-N-files-p arg 2))

(defun diredp-fewer-than-echo-limit-files-p (arg)
  "Return non-nil iff < `diredp-do-report-echo-limit' files marked.
More precisely, return non-nil iff ARG is nil and fewer than two
files are marked, or ARG is -1, 0 or 1."
  (diredp-fewer-than-N-files-p arg diredp-do-report-echo-limit))

;;;###autoload
(defun diredp-do-aggregate-apply-to-marked (aggregate-fun &optional mark-arg msgp)
  "Aggregate results of applying a function to the marked files and dirs.
This command prompts you for the aggregate function, AGGREGATE-FUN.

Then it calls `diredp-do-apply-to-marked', which prompts you for a
function to apply to the name of each marked file or dir.

A prefix arg acts like the ARG argument of `dired-get-marked-files'.
In particular, `C-u C-u' operates on all files in the Dired buffer."
  (interactive
   (progn
     (diredp-ensure-mode)
     (list (read (completing-read "Aggregate function: " obarray 'functionp nil nil
                                  (and (boundp 'function-name-history)  'function-name-history)))
           current-prefix-arg
           t)))
  (diredp-ensure-fn-nonzero-arity aggregate-fun)
  (let ((result  (apply aggregate-fun (let ((current-prefix-arg  mark-arg))
                                        (call-interactively #'diredp-do-apply-to-marked)))))
    (when msgp (message "Result: %S" result))))

;;; (defun diredp-do-aggregate-invoke-in-marked (aggregate-fun &optional mark-arg msgp)
;;;   "Aggregate results of invoking a function in the marked files and dirs.
;;; This command prompts you for the aggregate function, AGGREGATE-FUN.
;;; 
;;; Then it calls `diredp-do-invoke-in-marked', which prompts you for a
;;; function to invoke in each marked file or dir.
;;; 
;;; A prefix arg acts like the ARG argument of `dired-get-marked-files'.
;;; In particular, `C-u C-u' operates on all files in the Dired buffer."
;;;   (interactive
;;;    (progn
;;;      (diredp-ensure-mode)
;;;      (list (read (completing-read "Aggregate function: " obarray 'functionp nil nil
;;;                                   (and (boundp 'function-name-history)  'function-name-history)))
;;;            current-prefix-arg
;;;            t)))
;;;   (diredp-ensure-fn-nonzero-arity aggregate-fun)
;;;   (let ((result  (apply aggregate-fun (let ((current-prefix-arg  mark-arg))
;;;                                         (call-interactively #'diredp-do-invoke-in-marked)))))
;;;     (when msgp (message "Result: %S" result))))

;;;###autoload
(defun diredp-do-aggregate-eval-in-marked (aggregate-fun &optional mark-arg msgp)
  "Aggregate results of evaluating a sexp in the marked files and dirs.
This command prompts you for the aggregate function, AGGREGATE-FUN.

Then it calls `diredp-do-eval-in-marked', which prompts you for a Lisp
sexp to evaluate in each marked file or dir.

A prefix arg acts like the ARG argument of `dired-get-marked-files'.
In particular, `C-u C-u' operates on all files in the Dired buffer."
  (interactive
   (progn
     (diredp-ensure-mode)
     (list (read (completing-read "Aggregate function: " obarray 'functionp nil nil
                                  (and (boundp 'function-name-history)  'function-name-history)))
           current-prefix-arg
           t)))
  (diredp-ensure-fn-nonzero-arity aggregate-fun)
  (let ((result  (apply aggregate-fun (let ((current-prefix-arg  mark-arg))
                                        (call-interactively #'diredp-do-eval-in-marked)))))
    (when msgp (message "Result: %S" result))))

;;;###autoload
(defun diredp-do-apply-to-marked (function &optional arg) ; Bound to `@ @'
  "Apply FUNCTION to the absolute name of each marked file.
Return a list of the results.

You are prompted for FUNCTION.

A prefix arg behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer.

If you use multiple `C-u' as prefix arg then many files might be acted
on, and some of them might already be visited in modified buffers.  If
the function you apply modifies file content, and you want to save any
such buffers before using this command, then use \\[save-some-buffers]
first.

The result returned for each file is logged by `dired-log'.  Use `?'
to see all such results and any error messages.  If there are fewer
marked files than `diredp-do-report-echo-limit' then each result is
also echoed momentarily."
  (interactive
   (progn
     (diredp-ensure-mode)
     (list (read (completing-read "Apply function: " obarray 'functionp nil nil
                                  (and (boundp 'function-name-history)  'function-name-history)))
           current-prefix-arg)))
  (diredp-ensure-fn-nonzero-arity function)
  (let* ((files     (dired-get-marked-files nil arg nil nil 'NO-ERROR))
         (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
         (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
         (new-bufs  ())
         (result    ()))
    ;; FIXME
    ;; We do this for recursive version.  But here it means an additional `dired-map-over-marks'.  Optimize?
    (diredp-maybe-save-visited files nil diredp-list-file-attributes)
    (setq result    (save-selected-window
                      (diredp-map-over-marks-and-report #'diredp-apply-to-this-file
                                                        arg
                                                        'apply\ function\ to
                                                        (diredp-fewer-than-2-files-p arg)
                                                        function
                                                        (or (diredp-prefix-arg-all-files arg)  arg)
                                                        (diredp-fewer-than-echo-limit-files-p arg)))
          new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
    (message nil) ; Clear echo area of anything inserted by evaluating SEXP.
    (when (and new-bufs
               (diredp-y-or-n-files-p
                "New buffers of visited files.  Kill some? "
                (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                nil
                diredp-list-file-attributes))
      (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                    #'kill-buffer
                    new-bufs
                    '("buffer" "buffers" "kill")
                    save-some-buffers-action-alist))
    result))

;;;###autoload
(defun diredp-do-command-in-marked (command &optional arg) ; Bound to `@ M-x'
  "Invoke Emacs COMMAND in each marked file.
Visit each marked file at its beginning, then invoke COMMAND.
You are prompted for the COMMAND.
COMMAND can be the name of a command or a keyboard macro.

Only explicitly marked files are used.  A prefix arg has no effect on
which files are used.

A prefix arg is passed to COMMAND as its prefix arg.  For example, if
COMMAND uses a numeric prefix arg N for repetition then the effect of
COMMAND is repeated N times.

Other than the prefix arg, no arguments are passed to COMMAND.

If any of the marked files are already visited in modified buffers
then you are prompted to save them first.

If after invoking COMMAND in all of the marked files new buffers have
been created to visit some of them then you are prompted to kill those
buffers.

Any errors are logged by `dired-log'.  Use `?'  to see the error
messages.

Be aware that COMMAND could be invoked in a directory (Dired) buffer.
You may not want to do this if COMMAND modifies the buffer text.
\(But generally this will have little lasting effect - you can just
use `g' in that buffer to revert the listing.)"
  (interactive (progn (diredp-ensure-mode)
                      (list (diredp-read-command "Invoke command: ") current-prefix-arg)))
  (unless (commandp command) (error "Not a command: `%s'" command))
  (unless (or (and (fboundp 'kmacro-keyboard-macro-p) ; Emacs 27+
                   (kmacro-keyboard-macro-p command)) ; Keyboard macro
              (arrayp (symbol-function command)) ; Keyboard macro
              (not (fboundp 'func-arity))        ; Emacs < 26
              (= 0 (car (func-arity command))))
    (error "Command `%s' cannot accept zero args" command))
  (let* ((files     (dired-get-marked-files nil nil nil nil 'NO-ERROR)) ; nil for 2nd argument
         (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
         (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
         (new-bufs  ()))
    ;; FIXME
    ;; We do this for recursive version.  But here it means an additional `dired-map-over-marks'.  Optimize?
    (diredp-maybe-save-visited files nil diredp-list-file-attributes)
    (save-selected-window
      (dired-map-over-marks-check #'diredp-invoke-command
                                  nil   ; nil for 2nd argument
                                  'invoke\ command\ within
                                  (diredp-fewer-than-2-files-p nil) ; nil for argument
                                  command
                                  arg))
    (setq new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
    (sit-for 2) ; Show echo area message from `dired-map-over-marks-check'
    (when (and new-bufs
               (diredp-y-or-n-files-p
                "New buffers of visited files.  Kill some? "
                (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                nil
                diredp-list-file-attributes))
      (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                    #'kill-buffer
                    new-bufs
                    '("buffer" "buffers" "kill")
                    save-some-buffers-action-alist))))

;;; (defun diredp-do-invoke-in-marked (function &optional arg) ; Bound to `@ M-@'
;;;   "Invoke FUNCTION in each marked file, passing it no arguments.
;;; Return a list of the results.
;;;
;;; You are prompted for FUNCTION.
;;;
;;; A prefix arg behaves according to the ARG argument of
;;; `dired-get-marked-files'.  In particular, `C-u C-u' operates on all
;;; files in the Dired buffer.
;;;
;;; If you use multiple `C-u' as prefix arg then many files might be acted
;;; on, and some of them might already be visited in modified buffers.  If
;;; the function you apply modifies file content, and you want to save any
;;; such buffers before using this command, then use \\[save-some-buffers]
;;; first.
;;;
;;; The result returned for each file is logged by `dired-log'.  Use `?'
;;; to see all such results and any error messages.  If there are fewer
;;; marked files than `diredp-do-report-echo-limit' then each result is
;;; also echoed momentarily."
;;;   (interactive
;;;    (progn
;;;      (diredp-ensure-mode)
;;;      (list (read (completing-read "Invoke function: " obarray 'functionp nil nil
;;;                                   (and (boundp 'function-name-history)  'function-name-history)))
;;;            current-prefix-arg)))
;;;   (diredp-ensure-fn-zero-arity function)
;;;   (let* ((files     (dired-get-marked-files nil arg nil nil 'NO-ERROR))
;;;          (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
;;;          (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
;;;          (new-bufs  ())
;;;          (result    ()))
;;;     ;; FIXME
;;;     ;; We do this for recursive version.  But here it means an additional `dired-map-over-marks'.  Optimize?
;;;     (diredp-maybe-save-visited files nil diredp-list-file-attributes)
;;;     (setq result  (save-selected-window
;;;                     (diredp-map-over-marks-and-report #'diredp-invoke/eval-in-this-file
;;;                                                       arg
;;;                                                       'invoke\ function\ within
;;;                                                       (diredp-fewer-than-2-files-p arg)
;;;                                                       function
;;;                                                       (diredp-fewer-than-echo-limit-files-p arg)))
;;;           new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
;;;     (message nil) ; Clear echo area of anything inserted by evaluating SEXP.
;;;     (when (and new-bufs
;;;                (diredp-y-or-n-files-p
;;;                 "New buffers of visited files.  Kill some? "
;;;                 (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
;;;                 nil
;;;                 diredp-list-file-attributes))
;;;       (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
;;;                     #'kill-buffer
;;;                     new-bufs
;;;                     '("buffer" "buffers" "kill")
;;;                     save-some-buffers-action-alist))
;;;     result))

;;;###autoload
(defun diredp-do-eval-in-marked (sexp &optional arg) ; Bound to `@ M-:'
  "Evaluate an Emacs-Lisp SEXP in each marked file.
Visit each marked file at its beginning, then evaluate SEXP.
Return a list of the results.

You are prompted for the SEXP.

The result returned for each file is logged by `dired-log'.  Use `?'
to see all such results and any error messages.  If there are fewer
marked files than `diredp-do-report-echo-limit' then each result is
also echoed momentarily.

A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer."
  (interactive (progn (diredp-ensure-mode)
                      (list (diredp-read-expression "Eval sexp: ") current-prefix-arg)))
  (let* ((files     (dired-get-marked-files nil arg nil nil 'NO-ERROR))
         (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
         (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
         (new-bufs  ())
         (result    ()))
    ;; FIXME
    ;; We do this for recursive version.  But here it means an additional `dired-map-over-marks'.  Optimize?
    (diredp-maybe-save-visited files nil diredp-list-file-attributes)
    (setq result    (save-selected-window
                      (diredp-map-over-marks-and-report #'diredp-eval-in-this-file
                                                        arg
                                                        'eval\ lisp\ sexp
                                                        (diredp-fewer-than-2-files-p arg)
                                                        sexp
                                                        (diredp-fewer-than-echo-limit-files-p arg)))
          new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
    (message nil) ; Clear echo area of anything inserted by evaluating SEXP.
    (when (and new-bufs
               (diredp-y-or-n-files-p
                "New buffers of visited files.  Kill some? "
                (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                nil
                diredp-list-file-attributes))
      (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                    #'kill-buffer
                    new-bufs
                    '("buffer" "buffers" "kill")
                    save-some-buffers-action-alist))
    result))

;;;###autoload
(defun diredp-do-apply/eval-marked (fun/sexp &optional arg) ; Not bound
  "Apply a function to the marked files, or evaluate a sexp in them.
Return a list of the results.

You are prompted for the function or sexp.

With a plain prefix ARG (`C-u'), visit each marked file and eval the
 sexp or invoke the function there.  (The function is passed no
 arguments.)
Otherwise, apply the function to each file name.

Any prefix arg other than a single `C-u' behaves according to the ARG
argument of `dired-get-marked-files'.  In particular:
 * `C-u C-u' operates on all files in the Dired buffer.
 * To apply a function to just the file of the current line, ignoring
   ally marks, use a numeric prefix arg of 1 (e.g. `M-1`).

If you use multiple `C-u' as prefix arg then many files might be acted
on, and some of them might already be visited in modified buffers.  If
the function you apply modifies file content, and you want to save any
such buffers before using this command, then use \\[save-some-buffers]
first.

The result returned for each file is logged by `dired-log'.  Use `?'
to see all such results and any error messages.  If there are fewer
marked files than `diredp-do-report-echo-limit' then each result is
also echoed momentarily.

When called from Lisp, if ARG is `(4)' then eval the sexp or invoke
the function while visiting each file."
  (interactive
   (let ((use-no-args-p  (and (consp current-prefix-arg)  (< (car current-prefix-arg) 16))))
     (diredp-ensure-mode)
     (list (if use-no-args-p
               (diredp-read-expression "Invoke function or eval sexp: ")
             (read (completing-read "Apply function: " obarray 'functionp nil nil
                                    (and (boundp 'function-name-history)  'function-name-history))))
           current-prefix-arg)))
  (let ((use-no-args-p  (and (consp arg)  (< (car arg) 16))))
    (if (not use-no-args-p)
        (unless (or (not (fboundp 'func-arity)) ; Emacs < 26
                    (let ((max  (cdr (func-arity fun/sexp))))
                      (or (eq max 'many)  (> max 0))))
          (error "Function `%s' cannot accept any args" fun/sexp))
      (setq arg  nil)
      (when (functionp fun/sexp)
        (unless (or (not (fboundp 'func-arity)) ; Emacs < 26
                    (= 0 (car (func-arity fun/sexp))))
          (error "Function `%s' cannot accept zero args" fun/sexp))))
    (let* ((files     (dired-get-marked-files nil arg nil nil 'NO-ERROR))
           (fbufs     (delq nil (mapcar #'find-buffer-visiting files)))
           (mod-bufs  (diredp-remove-if-not #'buffer-modified-p fbufs))
           (new-bufs  ())
           (result    ()))
      ;; FIXME
      ;; We do this for recursive version.  But here it means an additional `dired-map-over-marks'.  Optimize?
      (diredp-maybe-save-visited files nil diredp-list-file-attributes)
      (setq result    (save-selected-window
                        (diredp-map-over-marks-and-report
                         (if use-no-args-p
                             #'diredp-invoke/eval-in-this-file
                           #'diredp-apply-to-this-file)
                         arg
                         (if use-no-args-p 'invoke/eval\ within 'apply\ function\ to)
                         (diredp-fewer-than-2-files-p arg)
                         fun/sexp
                         (diredp-fewer-than-echo-limit-files-p arg)))
            new-bufs  (diredp-set-difference (delq nil (mapcar #'find-buffer-visiting files)) fbufs nil))
      (message nil) ; Clear echo area of anything inserted by evaluating SEXP.
      (when (and new-bufs
                 (diredp-y-or-n-files-p
                  "New buffers of visited files.  Kill some? "
                  (delq nil (mapcar (lambda (bb) (buffer-file-name bb)) new-bufs))
                  nil
                  diredp-list-file-attributes))
        (map-y-or-n-p (lambda (bb) (format "Kill new buffer `%s'? " (buffer-file-name bb)))
                      #'kill-buffer
                      new-bufs
                      '("buffer" "buffers" "kill")
                      save-some-buffers-action-alist))
      result)))

(defun diredp-apply-to-this-file (function arg &optional echop)
  "Apply FUNCTION to the file or directory name on this line.
Log the result returned or any error.
ARG is a raw prefix arg.  If prefix arg is multiple `C-u' then ARG is
 passed to `diredp-get-file-or-dir-name'.
Non-nil optional arg ECHOP means also echo the result."
  (diredp-ensure-fn-nonzero-arity function)
  (let ((file     (or (dired-get-filename nil 'NO-ERROR) ; Explicitly marked or integer ARG
                      (diredp-get-file-or-dir-name arg))) ; Multi C-u
        (failure  nil)
        result)
    (condition-case err
        (setq result  (funcall function file))
      (error (setq failure  (error-message-string err))))
    (diredp-report-file-result file result failure echop)))

(defun diredp-invoke-command (command arg)
  "Visit the file or directory of this line, and invoke COMMAND.
COMMAND can be the name of a command or a keyboard macro.

COMMAND is invoked at the beginning of the buffer.  It is passed only
ARG, a raw prefix arg.

Be aware that COMMAND could be invoked in a directory (Dired) buffer.
You may not want to do this if COMMAND modifies the buffer text.
\(But generally this will have little lasting effect - you can just
use `g' in that buffer to revert the listing.)"
  (unless (commandp command) (error "Not a command: `%s'" command))
  (unless (or (and (fboundp 'kmacro-keyboard-macro-p) ; Emacs 27+
                   (kmacro-keyboard-macro-p command)) ; Keyboard macro
              (arrayp (symbol-function command)) ; Keyboard macro
              (not (fboundp 'func-arity))        ; Emacs < 26
              (= 0 (car (func-arity command))))
    (error "Command `%s' cannot accept zero args" command))
  (let* ((file     (dired-get-filename nil 'NO-ERROR)) ; Explicitly marked or integer ARG
         (failure  (not (file-exists-p file))))
    (unless failure
      (condition-case err
          (save-selected-window
            (with-current-buffer (find-file-noselect file)
              (save-excursion (goto-char (point-min))
                              (let ((current-prefix-arg  arg)) (command-execute command)))))
        (error (setq failure  (format "File: %s\n  %s\n" file (error-message-string err))))))
    (if (not failure)
        nil                             ; Return nil for success.
      (dired-log failure)
      (dired-make-relative file))))     ; Return file name for failure.

;; This function is not used.
;;
(defun diredp-invoke-in-this-file (function arg &optional echop)
  "Visit the file or directory of this line, and invoke FUNCTION.
Log the result returned or any error.
FUNCTION is invoked at the file beginning.  It is passed no args.
ARG is a raw prefix arg.  If prefix arg is multiple `C-u' then ARG is
 passed to `diredp-get-file-or-dir-name'.
Non-nil optional arg ECHOP means also echo the result."
  (diredp-ensure-fn-zero-arity function)
  (let* ((file     (or (dired-get-filename nil 'NO-ERROR) ; Explicitly marked or integer ARG
                       (diredp-get-file-or-dir-name arg))) ; Multi C-u
         (failure  (not (file-exists-p file)))
         result)
    (unless failure
      (condition-case err
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char (point-min))
              (setq result  (funcall function))))
        (error (setq failure  (error-message-string err)))))
    (diredp-report-file-result file result failure echop)))

(defun diredp-eval-in-this-file (sexp arg &optional echop)
  "Visit file or directory of this line, and evaluate Lisp SEXP.
Log the result returned or any error.
SEXP is evaluated at the beginning of the file.
ARG is a raw prefix arg.  If prefix arg is multiple `C-u' then ARG is
 passed to `diredp-get-file-or-dir-name'.
Non-nil optional arg ECHOP means also echo the result."
  (let* ((file     (or (dired-get-filename nil 'NO-ERROR) ; Explicitly marked or integer ARG
                       (diredp-get-file-or-dir-name arg))) ; Multi C-u
         (failure  (not (file-exists-p file)))
         result)
    (unless failure
      (condition-case err
          (with-current-buffer (find-file-noselect file)
            (save-excursion (goto-char (point-min))
                            (setq result  (eval-expression sexp))))
        (error (setq failure  (error-message-string err)))))
    (diredp-report-file-result file result failure echop)))

(defun diredp-invoke/eval-in-this-file (fun/sexp arg &optional echop)
  "Invoke/eval FUN/SEXP while visiting file of this line at position 1.
Log the result returned or any error.
FUN/SEXP is a function or a sexp.  The function is passed no args.
ARG is a raw prefix arg.  If prefix arg is multiple `C-u' then ARG is
 passed to `diredp-get-file-or-dir-name'.
Non-nil optional arg ECHOP means also echo the result."
  (when (functionp fun/sexp)
    (unless (or (not (fboundp 'func-arity)) ; Emacs < 26
                (= 0 (car (func-arity fun/sexp))))
      (error "Function `%s' cannot accept zero args" fun/sexp)))
  (let* ((file     (or (dired-get-filename nil 'NO-ERROR) ; Explicitly marked or integer ARG
                       (diredp-get-file-or-dir-name arg)))
         (failure  (not (file-exists-p file)))
         result)
    (unless failure
      (condition-case err
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char (point-min))
              (setq result  (if (functionp fun/sexp) (funcall fun/sexp) (eval-expression fun/sexp)))))
        (error (setq failure  (error-message-string err)))))
    (diredp-report-file-result file result failure echop)))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Redisplay only if at most one file is being treated.
;; 2. Doc string reflects `Dired+'s version of `dired-map-over-marks-check'.
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
;; 2. Doc string reflects `Dired+'s version of `dired-map-over-marks-check'.
;;
;;;###autoload
(defun dired-do-byte-compile (&optional arg) ; Bound to `B'
  "Byte compile marked Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (let* ((arg  current-prefix-arg)
                      (C-u  (and (consp arg)  arg)))
                 (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                 (list arg)))
  (dired-map-over-marks-check #'dired-byte-compile arg 'byte-compile (diredp-fewer-than-2-files-p arg)))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Redisplay only if at most one file is being treated.
;; 2. Doc string reflects `Dired+' version of `dired-map-over-marks-check'.
;;
;;;###autoload
(defun dired-do-load (&optional arg)    ; Bound to `L'
  "Load the marked Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (let* ((arg  current-prefix-arg)
                      (C-u  (and (consp arg)  arg)))
                 (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                 (list arg)))
  (dired-map-over-marks-check #'dired-load arg 'load (diredp-fewer-than-2-files-p arg)))


(when (fboundp 'multi-isearch-files)

  ;; REPLACE ORIGINAL in `dired.el':
  ;;
  ;; 1. Added optional arg ARG, so you can act on next ARG files or on all files.
  ;; 2. Added optional arg INTERACTIVEP.
  ;; 3. Do not raise error if no files when not INTERACTIVEP.
  ;;
  (defun dired-do-isearch (&optional arg interactivep)
    "Search for a string through all marked files using Isearch.
A prefix argument ARG specifies files to use instead of those marked.
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.
When invoked interactively, raise an error if no files are marked."
    (interactive (let* ((arg  current-prefix-arg)
                        (C-u  (and (consp arg)  arg)))
                   (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                   (list arg t)))
    (multi-isearch-files (dired-get-marked-files nil arg 'dired-nondirectory-p nil interactivep)))


  ;; REPLACE ORIGINAL in `dired.el':
  ;;
  ;; 1. Added optional arg ARG, so you can act on next ARG files or on all files.
  ;; 2. Added optional arg INTERACTIVEP.
  ;; 3. Do not raise error if no files when not INTERACTIVEP.
  ;;
  (defun dired-do-isearch-regexp (&optional arg interactivep)
    "Search for a regexp through all marked files using Isearch.
A prefix arg behaves as follows:
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.
When invoked interactively, raise an error if no files are marked."
    (interactive (let* ((arg  current-prefix-arg)
                        (C-u  (and (consp arg)  arg)))
                   (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                   (list arg t)))
    (multi-isearch-files-regexp (dired-get-marked-files nil arg 'dired-nondirectory-p nil interactivep)))

  )


;; REPLACE ORIGINAL in `dired-aux.el':
;;
;; 1. Added optional arg ARG, so you can act on next ARG files or on all files.
;; 2. Added optional arg INTERACTIVEP.
;; 3. Do not raise error if no files when not INTERACTIVEP.
;; 4. Works also for Emacs < 27.
;;
;;;###autoload
(defun dired-do-search (regexp &optional arg interactivep)
  "Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use:
 * `\\[fileloop-continue]'  (Emacs 27+) or
 * `\\[tags-loop-continue]' (older Emacs versions)

A prefix arg behaves as follows:
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.

When invoked interactively, raise an error if no files are marked."
  (interactive (let* ((arg  current-prefix-arg)
                      (C-u  (and (consp arg)  arg)))
                 (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                 (list (diredp-read-regexp "Search marked files (regexp): ")
                       arg
                       t)))
  (if (< emacs-major-version 27)
      (tags-search regexp `(dired-get-marked-files nil ',arg #'dired-nondirectory-p nil ,interactivep))
    (fileloop-initialize-search
     regexp `(dired-get-marked-files nil ',arg #'dired-nondirectory-p nil ,interactivep) 'default)
    (fileloop-continue)))


;; REPLACE ORIGINAL in `dired-aux.el':
;;
;; 1. Added optional arg ARG, so you can act on next ARG files or on all files.
;; 2. Added optional arg INTERACTIVEP.
;; 3. Do not raise error if no files when not INTERACTIVEP.
;;
;;;###autoload
(defun dired-do-query-replace-regexp (from to &optional arg interactivep)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
NOTE: A prefix arg for this command acts differently than for other
commands, so that you can use it to request word-delimited matches.

With a prefix argument:
 * An odd number of plain `C-u': act on the marked files, but replace
   only word-delimited matches.
 * More than one plain `C-u': act on all files, ignoring whether any
   are marked.
 * Any other prefix arg: Act on the next numeric-prefix files.

So for example:
 * `C-u C-u C-u': act on all files, replacing word-delimited matches.
 * `C-u 4': act on the next 4 files.  `C-4' means the same thing.
 * `C-u': act on the marked files, replacing word-delimited matches.

When invoked interactively, raise an error if no files are marked.

If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[fileloop-continue] (\\[tags-loop-continue] for
Emacs 26 or prior)."
  (interactive (let ((common  (query-replace-read-args "Query replace regexp in marked files" t t)))
                 (list (nth 0 common)
                       (nth 1 common)
                       current-prefix-arg
                       t)))
  (let* ((argnum     (and (consp arg)  (prefix-numeric-value arg)))
         (delimited  (and argnum  (eq (logand (truncate (log argnum 4)) 1) 1))) ; Odd number of plain `C-u'.
         (all        (and argnum  (> argnum 4))) ; At least 3 plain `C-u'.
         (dgmf-arg   (dired-get-marked-files nil
                                             (if (and arg  (atom arg)) (abs arg) (and all  '(16)))
                                             'dired-nondirectory-p
                                             nil
                                             interactivep)))
    (dolist (file  dgmf-arg)
      (let ((buffer  (get-file-buffer file)))
        (when (and buffer  (with-current-buffer buffer buffer-read-only))
          (error "File `%s' is visited read-only" file))))
    (if (< emacs-major-version 27)
        (tags-query-replace from to delimited `',dgmf-arg)
      (fileloop-initialize-replace from to `',dgmf-arg (and (not (equal from (downcase from)))  'default) delimited)
      (fileloop-continue))))


(when (fboundp 'xref-collect-matches)   ; Emacs 25+


  ;; REPLACE ORIGINAL in `dired-aux.el':
  ;;
  ;; 1. Added optional arg ARG, so you can act on next ARG files or on all files.
  ;; 2. Added optional arg INTERACTIVEP.
  ;; 3. Do not raise error if no files when not INTERACTIVEP.
  ;; 4. Works also for Emacs < 27.
  ;;
  (defun dired-do-find-regexp (regexp &optional arg interactivep)
    "Find all matches for REGEXP in all marked files.
For any marked directory, all of its files are searched recursively.
However, files matching `grep-find-ignored-files' and subdirectories
matching `grep-find-ignored-directories' are skipped in the marked
directories.

A prefix arg behaves as follows:
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.

When invoked interactively, raise an error if no files are marked.

REGEXP should use constructs supported by your local `grep' command."
    (interactive (let* ((arg  current-prefix-arg)
                        (C-u  (and (consp arg)  arg)))
                   (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                   (list (diredp-read-regexp "Search marked files (regexp): ")
                         arg
                         t)))
    (require 'grep)
    (when (> emacs-major-version 26) (require 'xref)) ; Emacs 27+
    (defvar grep-find-ignored-files)
    (if (> emacs-major-version 26)
        (declare-function rgrep-find-ignored-directories "grep" (dir))
      (defvar grep-find-ignored-directories))
    (let* ((marked    (dired-get-marked-files nil arg nil nil interactivep))
           (ignores   (nconc (mapcar #'file-name-as-directory
                                     (if (> emacs-major-version 26)
                                         (rgrep-find-ignored-directories default-directory)
                                       grep-find-ignored-directories))
                             grep-find-ignored-files))
           (xrefs-26  (and (< emacs-major-version 27)
                           (mapcan (lambda (file)
                                     (xref-collect-matches
                                      regexp "*" file (and (file-directory-p file)  ignores)))
                                   marked)))
           (fetcher   (and (> emacs-major-version 26)
                           `(lambda ()
                              (let ((files  ())
                                    (xrefs  ()))
                                (mapc (lambda (file)
                                        (if (file-directory-p file)
                                            (setq files  (nconc (project--files-in-directory file ignores "*")
                                                                files))
                                          (push file files)))
                                      (nreverse ',marked))
                                (setq xrefs  (xref-matches-in-files regexp files))
                                (when (and (null xrefs)  `,interactivep)
                                  (diredp-user-error "No matches for: %s" regexp))
                                xrefs)))))
      (if fetcher
          (xref--show-xrefs fetcher nil) ; Emacs 27+
        (if xrefs-26                     ; Emacs < 27
            (xref--show-xrefs xrefs-26 nil t)
          (when interactivep (diredp-user-error "No matches for: %s" regexp))))))


  ;; REPLACE ORIGINAL in `dired-aux.el':
  ;;
  ;; 1. Added optional arg ARG, so you can act on next ARG files or on all files.
  ;; 2. Added optional arg INTERACTIVEP.
  ;; 3. Do not raise error if no files when not INTERACTIVEP.
  ;;
;;;###autoload
  (defun dired-do-find-regexp-and-replace (from to &optional arg interactivep)
    "Replace matches of FROM with TO, in all marked files.
For any marked directory, matches in all of its files are replaced,
recursively.  However, files matching `grep-find-ignored-files'
and subdirectories matching `grep-find-ignored-directories' are skipped
in the marked directories.

A prefix arg behaves as follows:
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.

When invoked interactively, raise an error if no files are marked.

REGEXP should use constructs supported by your local `grep' command."
    (interactive (let* ((common  (query-replace-read-args "Query replace regexp in marked files" t t))
                        (arg     current-prefix-arg)
                        (C-u     (and (consp arg)  arg)))
                   (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                   (list (nth 0 common)
                         (nth 1 common)
                         arg
                         t)))
    (with-current-buffer (dired-do-find-regexp from arg interactivep)
      (xref-query-replace-in-results from to)))

  )

;;;###autoload
(defun diredp-do-grep (command-args)    ; Bound to `C-M-g'
  "Run `grep' on marked (or next prefix arg) files.
A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer."
  (interactive (progn (unless (if (< emacs-major-version 22)
                                  grep-command
                                (and grep-command  (or (not grep-use-null-device)  (eq grep-use-null-device t))))
                        (grep-compute-defaults))
                      (list (diredp-do-grep-1))))
  (grep command-args))

;; Optional arg FILES is no longer used.  It was used in `diredp-do-grep' before the
;; new `dired-get-marked-files'.
(defun diredp-do-grep-1 (&optional files)
  "Helper function for `diredp-do-grep'.
Non-nil optional arg FILES are the files to grep, overriding the files
choice described for `diredp-do-grep'."
  (let ((default  (and (fboundp 'grep-default-command)
                       (if (fboundp 'grepp-default-regexp-fn) ; In `grep+.el'.
                           (grep-default-command (funcall (grepp-default-regexp-fn)))
                         (grep-default-command)))))
    (read-from-minibuffer
     "grep <pattern> <files> :  "
     (let ((up-to-files  (concat grep-command "   ")))
       (cons (concat up-to-files
                     (mapconcat #'identity
                                (or files  (mapcar 'shell-quote-argument
                                                   (dired-get-marked-files nil current-prefix-arg)))
                                " "))
             (- (length up-to-files) 2)))
     nil nil 'grep-history default)))

(when (memq system-type '(windows-nt ms-dos))
  (define-derived-mode diredp-w32-drives-mode fundamental-mode "Drives"
    "Mode for Dired buffer listing MS Windows drives (local or remote)."
    (setq buffer-read-only  t)))

;; The next two commands were originally taken from Emacs Wiki, page WThirtyTwoBrowseNetDrives:
;; https://www.emacswiki.org/emacs/WThirtyTwoBrowseNetDrives.  They are referred to there as
;; commands `show-net-connections' and `netdir'.  I am hoping that the contributor (anonymous)
;; does not mind my adapting them and including them in `Dired+'.

(when (memq system-type '(windows-nt ms-dos))
  (defun diredp-w32-list-mapped-drives () ; Not bound
    "List network connection information for shared MS Windows resources.
This just invokes the Windows `NET USE' command."
    (interactive)
    (shell-command "net use")
    (display-buffer "*Shell Command Output*")))

(when (memq system-type '(windows-nt ms-dos))
  (defun diredp-w32-drives (&optional other-window-p) ; Bound to `:/'
    "Visit a list of MS Windows drives for use by Dired.
With a prefix argument use another window for the list.
In the list, use `mouse-2' or `RET' to open Dired for a given drive.

The drives listed are the remote drives currently available, as
determined by the Windows command `NET USE', plus the local drives
specified by option `diredp-w32-local-drives', which you can
customize.

Note: When you are in Dired at the root of a drive (e.g. directory
      `C:/'), command `diredp-up-directory' invokes this command.
      So you can use `\\[diredp-up-directory]' to go up to the list of drives."
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
        (if (fboundp 'pop-to-buffer-same-window)
            (pop-to-buffer-same-window "*Windows Drives*")
          (switch-to-buffer "*Windows Drives*")))
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

;; $$$$$$ NO LONGER USED.  Was used in `diredp-do-grep(-1)' before new `dired-get-marked-files'.
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
        (setq file  (dired-get-filename nil 'NO-ERROR)) ; Non-nil second arg means "also . and ..".
        (when file                      ; Remove directory portion if in same directory.
          (setq file  (dired-get-filename (dired-in-this-tree file default-directory) 'NO-ERROR)))
        (unless (or (not file)  (file-directory-p file))  (push file files))
        (goto-char pos))
      (move-marker pos nil))
    (setq files  (sort files (if (and (featurep 'ls-lisp)
                                      (not (symbol-value 'ls-lisp-use-insert-directory-program)))
                                 'ls-lisp-string-lessp
                               (if case-fold-search
                                   (lambda (s1 s2) (string-lessp (upcase s1) (upcase s2)))
                                 'string-lessp))))))

(when (fboundp 'read-char-choice)       ; Emacs 24+


  ;; REPLACE ORIGINAL in `dired-aux.el'
  ;;
  ;; `l' lists the files involved and prompts again.
  ;;
  (defun dired-query (sym prompt &rest args)
    "Format PROMPT with ARGS, query user, and store the result in SYM.
The return value is either nil or t.

The user can type:
 `y' or `SPC' to accept once
 `n' or `DEL' to skip once
 `!' to accept this and subsequent queries
 `l' list the files, showing details per `diredp-list-file-attributes'
 `q' or `ESC' to decline this and subsequent queries

If SYM is already bound to a non-nil value, this function may return
automatically without querying the user.  If SYM is `!', return t; if
SYM is `q' or ESC, return nil."
    (let* ((char            (symbol-value sym))
           (char-choices    '(?y ?\   ?n ?\177 ?! ?l ?q ?\e)) ; Use ?\  , not ?\s, for Emacs 20 byte-compiler.
           (list-buf        (generate-new-buffer-name "*Files*"))
           (list-was-shown  nil))
      (unwind-protect
           (cond ((eq char ?!) t)       ; Accept, and don't ask again.
                 ((memq char '(?q ?\e)) nil) ; Skip, and don't ask again.
                 (t                     ; No previous answer - ask now
                  (setq prompt  (concat (apply (if (fboundp 'format-message) #'format-message #'format)
                                               prompt
                                               args)
                                        (if help-form
                                            (format " [Type ynlq! or %s] " (key-description (vector help-char)))
                                          " [Type y, n, l, q or !] ")))
                  (set sym (setq char  (read-char-choice prompt char-choices)))
                  (when (eq char ?l)    ; List files and prompt again.
                    (diredp-list-files args nil nil nil diredp-list-file-attributes)
                    (set sym (setq char  (read-char-choice prompt char-choices))))
                  (and (memq char '(?y ?\   ?!))  t))) ; Use ?\  , not ?\s, for Emacs 20.
        (when (get-buffer list-buf)
          (save-window-excursion
            (pop-to-buffer list-buf)
            (condition-case nil         ; Ignore error if user already deleted.
                (if (one-window-p) (delete-frame) (delete-window))
              (error nil))
            (if list-was-shown (bury-buffer list-buf) (kill-buffer list-buf)))))))

  )

(unless (fboundp 'read-char-choice)     ; Emacs 20-23 (modified the Emacs 23 version).  Needs `dired-query-alist'.


  ;; REPLACE ORIGINAL in `dired-aux.el'
  ;;
  ;; 1. `l' lists the files involved and prompts again.
  ;; 2. Compatible with older Emacs versions (before Emacs 24): can use `dired-query-alist'.
  ;;
  (defun dired-query (qs-var qs-prompt &rest qs-args)
    "Query user and return nil or t.
The user can type:
 `y' or `SPC' to accept once
 `n' or `DEL' to skip once
 `!' to accept this and subsequent queries
 `l' list the files, showing details per `diredp-list-file-attributes'
 `q' or `ESC' to decline this and subsequent queries

Store answer in symbol VAR (which must initially be bound to nil).
Format PROMPT with ARGS.
Binding variable `help-form' will help the user who types the help key."
    (let* ((char               (symbol-value qs-var))
           (dired-query-alist  (cons '(?l . l) dired-query-alist))
           (action             (cdr (assoc char dired-query-alist))))
      (cond ((eq 'yes action) t)        ; Accept, and don't ask again.
            ((eq 'no action) nil)       ; Skip, and don't ask again.
            (t                          ; No lasting effects from last time we asked - ask now.
             (let ((cursor-in-echo-area  t)
                   (executing-kbd-macro  executing-kbd-macro)
                   (qprompt              (concat qs-prompt
                                                 (if help-form
                                                     (format " [Type ynl!q or %s] "
                                                             (key-description (char-to-string help-char)))
                                                   " [Type y, n, l, q or !] ")))
                   done result elt)
               (while (not done)
                 (apply #'message qprompt qs-args)
                 (setq char  (set qs-var (read-event)))
                 (when (eq char ?l)     ; List files and prompt again.
                   (diredp-list-files qs-args nil nil nil diredp-list-file-attributes)
                   (apply #'message qprompt qs-args)
                   (setq char  (set qs-var (read-event))))
                 (if (numberp char)
                     (cond ((and executing-kbd-macro (= char -1))
                            ;; `read-event' returns -1 if we are in a keyboard macro and there are no more
                            ;; events in the macro.  Try to get an event interactively.
                            (setq executing-kbd-macro  nil))
                           ((eq (key-binding (vector char)) 'keyboard-quit) (keyboard-quit))
                           (t (setq done  (setq elt  (assoc char dired-query-alist)))))))
               ;; Display the question with the answer.
               (message "%s" (concat (apply #'format qprompt qs-args) (char-to-string char)))
               (memq (cdr elt) '(t y yes)))))))

  )


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; 1. Use `diredp-this-subdir' instead of `dired-get-filename'.
;; 2. If on a subdir listing header line or a non-dir file in a subdir listing, go to
;;    the line for the subdirectory in the parent directory listing.
;; 3. Prefix arg `C-u C-u' means remove all inserted subdir listings.
;; 4. Non-positive prefix arg means prompt for the subdir to insert.
;; 5. Fit one-window frame after inserting subdir.
;;
;;;###autoload
(defun dired-maybe-insert-subdir (dirname &optional arg no-error-if-not-dir-p) ; Bound to `i'
  "Insert a subdirectory listing or move to a subdir line or listing.
Inserted subdirs are listed in the same positions as with `ls -lR'.

This bounces you back and forth between a subdir line and its inserted
listing header line.  Using it on a non-directory line in a subdir
listing acts the same as using it on the subdir header line.

* If on a subdir line, then go to the subdir's listing, creating it if
  not yet present.

* If on a subdir listing header line or a non-directory file in a
  subdir listing, then go to the line for the subdir in the parent
  directory listing.

* If on a non-directory file in the top Dired directory listing, do
  nothing.

A prefix arg changes the behavior, as follows:

* A non-negative numeric prefix prefix arg prompts you for the `ls'
  switches to use for the subdir listing.  `R' as a switch expands the
  directory tree.

* A non-positive numeric prefix prefix arg prompts you for the subdir
  to insert.  Candidates are the directories within (below) the
  listing surrounding point.

* `C-u C-u' just removes all inserted subdir listings.

Dired remembers switches specified with a prefix arg, so reverting the
buffer does not reset them.

However, you might sometimes need to reset some switches for a subdir
after using undo (\\<dired-mode-map>`\\[dired-undo]').  You can reset all subdir switches to the
default value using `\\[dired-reset-subdir-switches]'.  See Info node
`(emacs) Subdir Switches' for more details.

If called from Lisp, pass symbol `REMOVE' as ARG to remove inserted
subdirs.  Otherwise, the args are as for `dired-insert-subdir'."
  (interactive (let ((num  (prefix-numeric-value current-prefix-arg))
                     (raw  current-prefix-arg))
                 (let ((subdir    (or (and raw  (atom raw)  (<= num 0)
                                           (completing-read "Insert subdir: "
                                                            (diredp-uninserted-subdirs (dired-current-directory))
                                                            nil t))
                                      (diredp-this-subdir)))
                       (switches  (and raw  (atom raw)  (natnump num)
                                       (read-string
                                        "Switches for listing: "
                                        (or (and (boundp 'dired-subdir-switches)  dired-subdir-switches)
                                            dired-actual-switches))))
                       (remove    (and (consp raw)  (= 16 num)  'REMOVE)))
                   (unless (or remove  (let ((subd  (if (consp subdir) (car subdir) subdir)))
                                         (not (string= subd (if (consp dired-directory)
                                                                (car dired-directory)
                                                              dired-directory)))))
                     (error "No subdir here"))
                   (list subdir (or switches  remove)))))
  (if (eq arg 'REMOVE)
      (diredp-remove-inserted-subdirs)
    (let ((opoint    (point)) ; No need for a marker for OPOINT, as subdir is always inserted after OPOINT.
          (filename  dirname))
      (cond ((consp filename) ; Subdir header line or non-directory file.
             (setq filename  (car filename))
             (if (assoc filename dired-subdir-alist)
                 (dired-goto-file filename) ;  On subdir header line.  Go to subdir line in parent listing.
               (dired-insert-subdir (substring (file-name-directory filename) 0 -1))))
            (t
             (setq dirname  (file-name-as-directory dirname))
             (or (and (not arg)  (dired-goto-subdir dirname))
                 (dired-insert-subdir dirname arg no-error-if-not-dir-p))
             (push-mark opoint) ; Do this after inserting message so user sees `Mark set'.
             (diredp-fit-one-window-frame))))))

(defun diredp-uninserted-subdirs (dir &optional full)
  "Subdirs of DIR not yet inserted in this Dired buffer.
This includes all directories under DIR, not just direct (child)
subdirectories.

Non-nil optional arg FULL means the directory names are absolute.
Otherwise (default) they are relative to `dired-directory'."
  (let* ((uninserted   (diredp-remove-if
                        (lambda (dir) (assoc (file-name-as-directory (expand-file-name dir)) dired-subdir-alist))
                        (diredp-directories-within dir))))
    (if full
        uninserted
      (mapcar #'file-relative-name uninserted))))

(defun diredp-remove-inserted-subdirs () ; Not bound
  "Remove all inserted subdir listings.
Note that `C-x C-v' followed by accepting the default directory does
this, as it rereads the main directory.  However, that also removes
all markings etc."
  (interactive)
  (diredp-ensure-mode)
  (dolist (entry  dired-subdir-alist)
    (unless (string= (car entry) (expand-file-name default-directory))
      ;; Could just use (dired-goto-subdir (car entry)) but next two lines are the relevant part.
      (goto-char (cdr entry))
      (skip-chars-forward "^\r\n")
      (dired-kill-subdir))))

(defun diredp-fit-one-window-frame ()
  "Fit one-window selected frame to its buffer."
  (when (and (get-buffer-window (current-buffer))  (fboundp 'fit-frame-if-one-window)) ; In `autofit-frame.el'.
    (fit-frame-if-one-window)))

(defun diredp-this-subdir ()
  "This line's filename, if directory, or `dired-current-directory' list.
If on a directory line, then return the directory name.
Else return a singleton list of a directory name, which is as follows:
  If on a subdirectory header line (either of the two lines), then use
  that subdirectory name.  Else use the parent directory name."
  (or (let ((file  (dired-get-filename nil 'NO-ERROR)))
        (and file
             (file-directory-p file)
             (not (member (file-relative-name file (file-name-directory (directory-file-name file)))
                          '("." ".." "./" "../")))
             file))
      (list (dired-current-directory))))


;; REPLACE ORIGINAL in `dired-aux.el'
;;
;; 1. Added optional arg FROM, which is also listed by `l' when prompted.
;; 2. Added missing doc string.
;;
(defun dired-handle-overwrite (to &optional from)
  "Save old version of file TO that is to be overwritten.
`dired-overwrite-confirmed' and `overwrite-backup-query' are fluid vars
from `dired-create-files'.

Optional arg FROM is a file being copied or renamed to TO.  It is used
only when a user hits `l' to list files when asked whether to
overwrite."
  (let (backup)
    (when (and dired-backup-overwrite
               dired-overwrite-confirmed
               (setq backup (car (find-backup-file-name to)))
               (or (eq 'always dired-backup-overwrite)
                   (dired-query 'overwrite-backup-query "Make backup for existing file `%s'? " to from)))
      (rename-file to backup 0)         ; Confirm overwrite of old backup.
      (dired-relist-entry backup))))


(when (fboundp 'dired-copy-file-recursive) ; Emacs 22+


  ;; REPLACE ORIGINAL in `dired-aux.el'
  ;;
  ;; 1. Pass also FROM to `dired-handle-overwrite', so `l' lists it too.
  ;; 2. Added missing doc string.
  ;;
  (defun dired-copy-file (from to ok-if-already-exists)
    "Copy file FROM to location TO.
Non-nil arg OK-IF-ALREADY-EXISTS is passed to `copy-file' or
 `make-symbolic-link'.
Preserves the last-modified date when copying, unless
`dired-copy-preserve-time' is nil."
    (dired-handle-overwrite to from)
    (dired-copy-file-recursive from to ok-if-already-exists dired-copy-preserve-time t dired-recursive-copies))


  ;; REPLACE ORIGINAL in `dired-aux.el'
  ;;
  ;; 1. Pass also FROM to `dired-handle-overwrite', so `l' lists it too.
  ;; 2. Added missing doc string.
  ;;
  (defun dired-copy-file-recursive (from to ok-if-already-exists &optional keep-time top recursive)
    "Copy file FROM to location TO, handling directories in FROM recursively.
Non-nil arg OK-IF-ALREADY-EXISTS is passed to `copy-file' or
 `make-symbolic-link'.
Non-nil optional arg KEEP-TIME is passed to `copy-file' or
 `copy-directory'.
Non-nil optional arg TOP means do not bother with `dired-handle-overwrite'.
Non-nil optional arg RECURSIVE means recurse on any directories in
 FROM, after confirmation if RECURSIVE is not `always'."
    (when (and (eq t (car (file-attributes from)))  (file-in-directory-p to from))
      (error "Cannot copy `%s' into its subdirectory `%s'" from to))
    (let ((attrs  (file-attributes from)))
      (if (and recursive
               (eq t (car attrs))
               (or (eq recursive 'always)  (yes-or-no-p (format "Recursive copies of %s? " from))))
          (copy-directory from to keep-time)
        (or top  (dired-handle-overwrite to from))
        (condition-case err
            (if (stringp (car attrs))   ; It is a symlink
                (make-symbolic-link (car attrs) to ok-if-already-exists)
              (copy-file from to ok-if-already-exists keep-time))
          (file-date-error
           (push (dired-make-relative from) dired-create-files-failures)
           (dired-log "Can't set date on %s:\n%s\n" from err))))))

  )


;; REPLACE ORIGINAL in `dired-aux.el'
;;
;; 1. Pass also FILE to `dired-handle-overwrite', so `l' lists it too.
;; 2. Added missing doc string.
;;
(defun dired-rename-file (file newname ok-if-already-exists)
  "Rename FILE to NEWNAME.
Non-nil arg OK-IF-ALREADY-EXISTS is passed to `rename-file'."
  (dired-handle-overwrite newname file)
  (when (fboundp 'dired-maybe-create-dirs) (dired-maybe-create-dirs (file-name-directory newname))) ; Emacs 27+
  (rename-file file newname ok-if-already-exists) ; Error is caught in `-create-files'.
  ;; Silently rename the visited file of any buffer visiting this file.
  (and (get-file-buffer file)  (with-current-buffer (get-file-buffer file) (set-visited-file-name newname nil t)))
  (dired-remove-file file)
  ;; See if it's an inserted subdir, and rename that, too.
  (when (file-directory-p file)         ; Emacs bug #32899
    (dired-rename-subdir file newname)))


;; REPLACE ORIGINAL in `dired-aux.el'
;;
;; Pass also FILE to `dired-handle-overwrite', so `l' lists it too.
;;
(defun dired-hardlink (file newname &optional ok-if-already-exists)
  "Give FILE additional name NEWNAME.
Non-nil arg OK-IF-ALREADY-EXISTS is passed to `add-name-to-file'."
  (dired-handle-overwrite newname file)
  (add-name-to-file file newname ok-if-already-exists) ; Error is caught in -create-files'.
  (dired-relist-file file))             ; Update the link count.


;; REPLACE ORIGINAL in `dired.el'.
;;
;; No-op: does nothing now.
;;
(defun dired-insert-subdir-validate (dirname &optional switches))


;;; $$$$$$$$
;;; ;; REPLACE ORIGINAL in `dired-aux.el'.
;;; ;;
;;; ;; 1. Do not require that DIRNAME be in the current directory tree (no error if not).
;;; ;; 2. Use `dolist' instead of `mapcar'.
;;; ;;
;;; (defun dired-insert-subdir-validate (dirname &optional switches)
;;;   "Raise an error if it is invalid to insert DIRNAME with SWITCHES."
;;; ;;; (or (dired-in-this-tree dirname (expand-file-name default-directory)) ; REMOVED
;;; ;;;     (error  "%s: not in this directory tree" dirname))
;;;   (let ((real-switches  (or switches  (and (boundp 'dired-subdir-switches) ; Emacs 22+
;;;                                            dired-subdir-switches))))
;;;     (when real-switches
;;;       (let (case-fold-search)
;;;         (dolist (switchs  '("F" "b"))   ; Switches that matter for `dired-get-filename'.
;;;           (unless (eq (null (diredp-string-match-p switchs real-switches))
;;;                       (null (diredp-string-match-p switchs dired-actual-switches)))
;;;             (error "Can't have dirs with and without `-%s' switches together" switchs)))))))


;; REPLACE ORIGINAL in `dired-aux.el'.
;;
;; If NEW-DIR is not a descendant of a directory in the buffer, put it at eob.
;;
(defun dired-insert-subdir-newpos (new-dir)
  "Move to the proper position for inserting NEW-DIR, and return it.
Respect the order within each directory tree.  But if NEW-DIR is not a
descendant of any directory in the buffer, then put it at the end."
  (let ((alist  dired-subdir-alist)
        elt dir new-pos)
    (while alist
      (setq elt    (car alist)
            alist  (cdr alist)
            dir    (car elt))
      (if (dired-tree-lessp dir new-dir)
          (setq new-pos  (dired-get-subdir-max elt) ; Position NEW-DIR after DIR.
                alist    ())
        (setq new-pos  (point-max))))
    (goto-char new-pos))
  (unless (eobp) (forward-line -1))
  (insert "\n")
  (point))


;; Provide Emacs 27+ handling of hiding subdir listings.
;;
;; Except for `diredp--add-dired-to-invisibility-hook', these are from vanilla Emacs 27.1, `dired.el' and
;; `dired-aux.el' - no changes, except as noted.
;;
;; The non-"internal" ones here (no `--') OVERRIDE any versions from earlier Emacs versions.  Earlier Emacs versions
;; used selective display, which is extremely slow (pretty much hangs Emacs) for a large subdir listing.
;;
(when (< emacs-major-version 27)

  (defun diredp--add-dired-to-invisibility-hook ()
    (when (and (< emacs-major-version 22)  (eq buffer-invisibility-spec t))
      (setq buffer-invisibility-spec (list t)))
    (add-to-invisibility-spec '(dired . t)))
  (add-hook 'dired-mode-hook 'diredp--add-dired-to-invisibility-hook)

  (defun dired--find-hidden-pos (start end) ; Emacs 27.1 `dired.el'
    (text-property-any start end 'invisible 'dired))

  (defun dired--hidden-p (&optional pos) ; Emacs 27.1 `dired.el'
    (eq (get-char-property (or pos  (point)) 'invisible) 'dired))

  (defun dired--hide (start end)        ; Emacs 27.1 `dired.el'
    ;; The old code used selective-display which only works at
    ;; a line-granularity, so it used start and end positions that were
    ;; approximate ("anywhere on the line is fine").
    (save-excursion
      (put-text-property (progn (goto-char start) (line-end-position))
                         (progn (goto-char end)   (line-end-position))
                         'invisible 'dired)))

  ;; Fixed to support older Emacs versions, which don't have `remove-list-of-text-properties'.
  (defun dired--unhide (start end)      ; From Emacs 27.1 `dired.el'.
    ;; The Emacs < 27 code used selective-display which only works at a line-granularity, so it used start and end
    ;; positions that were approximate ("anywhere on the line is fine").
    ;;
    ;; FIXME: This also removes other invisible properties!
    (save-excursion
      (remove-text-properties (progn (goto-char start) (line-end-position))
                              (progn (goto-char end) (line-end-position))
                              '(invisible _DUMMY))))

  (defun dired-unhide-subdir ()         ; Emacs 27.1 `dired-aux.el'
    (with-silent-modifications (dired--unhide (dired-subdir-min) (dired-subdir-max))))

  (defun dired-subdir-hidden-p (dir)    ; Emacs 27.1 `dired-aux.el'
    (save-excursion (dired-goto-subdir dir) (dired--hidden-p)))

  ;; Fixed to support older Emacs versions.
  ;;
  (defun dired-add-entry (filename &optional marker-char relative) ; Emacs 27.1 `dired-aux.el'
    "Add a new dired entry for FILENAME.
Optionally mark it with MARKER-CHAR (a character, else uses
`dired-marker-char').  Note that this adds the entry `out of order'
if files are sorted by time, etc.
Skips files that match `dired-trivial-filenames'.
Exposes hidden subdirectories if a file is added there.

If `dired-x' is loaded and `dired-omit-mode' is enabled, skips
files matching `dired-omit-regexp'."
    (if (or (not (featurep 'dired-x))
            (and (boundp 'dired-omit-mode) ; Emacs 22+.
                 (not dired-omit-mode))
            ;; Avoid calling `ls' for files that are going to be omitted anyway.
            (let ((omit-re  (dired-omit-regexp)))
              (or (string= omit-re "")
                  (not (diredp-string-match-p omit-re (cond ((eq 'no-dir dired-omit-localp) filename)
                                                            ((eq t dired-omit-localp) (dired-make-relative filename))
                                                            (t (dired-make-absolute
                                                                filename (file-name-directory filename)))))))))
        ;; Do it!
        (progn
          (setq filename  (directory-file-name filename))
          ;; Entry is always for files, even if they happen to also be directories.
          (let* ((opoint     (point))
                 (cur-dir    (dired-current-directory))
                 (directory  (if relative cur-dir (file-name-directory filename)))
                 reason)
            (setq filename  (if relative (file-relative-name filename directory) (file-name-nondirectory filename))
                  reason    (catch 'not-found
                              (if (string= directory cur-dir)
                                  (progn
                                    (end-of-line)
                                    (when (dired--hidden-p) (dired-unhide-subdir))
                                    ;; Where we should be, unless point is before subdir line or its total line.
                                    (let ((p  (dired-after-subdir-garbage cur-dir)))
                                      (when (< (point) p) (goto-char p))))
                                ;; Else try to find correct place to insert
                                (if (dired-goto-subdir directory)
                                    (progn                    ; Unhide if necessary
                                      (when (dired--hidden-p) ; Point is at end of subdir line.
                                        (dired-unhide-subdir))
                                      ;; Found - skip subdir and `total' line and uninteresting files like . and ..
                                      ;; This had better not move into the next subdir!
                                      (dired-goto-next-nontrivial-file))
                                  ;; Not found
                                  (throw 'not-found "Subdir not found")))
                              (let (buffer-read-only  opoint)
                                (beginning-of-line)
                                (setq opoint  (point))
                                ;; Don't expand `.'.  Show just the file name within directory.
                                (let ((default-directory  directory))
                                  (dired-insert-directory
                                   directory (concat dired-actual-switches " -d") (list filename)))
                                (goto-char opoint)
                                ;; Put in desired marker char.
                                (when marker-char
                                  (let ((dired-marker-char  (if (integerp marker-char)
                                                                marker-char
                                                              dired-marker-char)))
                                    (dired-mark nil)))
                                ;; Compensate for a bug in `ange-ftp'.  It inserts the file's absolute name, rather
                                ;; than the relative one.  That may be hard to fix since it is probably
                                ;; controlled by something in FTP.
                                (goto-char opoint)
                                (let ((inserted-name  (dired-get-filename 'verbatim)))
                                  (if (file-name-directory inserted-name)
                                      (let (props)
                                        (end-of-line)
                                        (forward-char (- (length inserted-name)))
                                        (setq props  (text-properties-at (point)))
                                        (delete-char (length inserted-name))
                                        (let ((pt  (point)))
                                          (insert filename)
                                          (set-text-properties pt (point) props))
                                        (forward-char 1))
                                    (forward-line 1)))
                                (forward-line -1)
                                (if dired-after-readin-hook
                                    ;; The subdir-alist is not affected...
                                    (save-excursion ; ...so we can run it right now:
                                      (save-restriction
                                        (beginning-of-line)
                                        (narrow-to-region (point) (line-beginning-position 2))
                                        (run-hooks 'dired-after-readin-hook))))
                                (dired-move-to-filename))
                              ;; Return nil if all went well
                              nil))
            (when reason (goto-char opoint)) ; don't move away on failure
            (not reason)))                   ; return t on success, else nil
      ;; Don't do it (`dired-omit-mode').
      ;; Return t for success (perhaps we should return `file-exists-p').
      t))

  ;; Fixed to support older Emacs versions: Don't use `pcase-dolist'.
  ;;
  (defun dired-remember-hidden ()       ; Emacs 27.1 `dired.el'
    "Return a list of names of subdirs currently hidden."
    (let (result)
      ;; ORIGINAL:
      ;;       (pcase-dolist (`(,dir . ,pos) dired-subdir-alist)
      ;;         (goto-char pos)
      ;;         (end-of-line)
      ;;         (when (dired--hidden-p) (push dir result)))
      ;;       result))
      (let (dir pos)
        (dolist (dir.pos  dired-subdir-alist)
          (setq dir  (car dir.pos)
                pos  (cdr dir.pos))
          (goto-char pos)
          (end-of-line)
          (when (dired--hidden-p) (push dir result)))
        result)))

  (defun dired-move-to-end-of-filename (&optional no-error) ; Emacs 27.1 `dired.el'
    ;; Assumes point is at beginning of filename,
    ;; thus the rwx bit re-search-backward below will succeed in *this*
    ;; line if at all.  So, it should be called only after
    ;; (dired-move-to-filename t).
    ;; On failure, signals an error (with non-nil NO-ERROR just returns nil).
    ;; This is the UNIX version.
    (if (get-text-property (point) 'dired-filename)
        (goto-char (next-single-property-change (point) 'dired-filename))
      (let ((opoint  (point))
            (used-F  (dired-check-switches dired-actual-switches "F" "classify"))
            (eol     (line-end-position))
            (hidden  (dired--hidden-p))
            file-type executable symlink)
        (if hidden
            nil
          (save-excursion               ; Find out what kind of file this is.
            ;; Restrict permission bits to non-blank, otherwise this matches one char too early (looking backward):
            ;; "l---------" (some systems make symlinks that way)
            ;; "----------" (plain file with zero perms)
            (if (re-search-backward dired-permission-flags-regexp nil t)
                (setq file-type   (char-after (match-beginning 1))
                      symlink     (eq file-type ?l)
                      ;; Only with -F we need to know whether it's an executable
                      executable  (and used-F  (string-match
                                                "[xst]" ;; execute bit set anywhere?
                                                (concat (match-string 2) (match-string 3) (match-string 4)))))
              (unless no-error (error "No file on this line"))))
          ;; Move point to end of name:
          (if symlink
              (when (search-forward " -> " eol t)
                (forward-char -4)
                (and used-F  dired-ls-F-marks-symlinks  (eq (preceding-char) ?@) ; `ls' really marked the link
                     (forward-char -1)))
            ;; Else not a symbolic link, `ls -lF' marks dirs, sockets, fifos and executables with exactly one
            ;; trailing char.  (Executable bits on symlinks don't mean a thing, even to `ls', but we know it's
            ;; not a symlink.)
            (goto-char eol)
            (and used-F  (or (memq file-type '(?d ?s ?p))  executable)
                 (forward-char -1))))
        (or no-error
            (not (eq opoint (point)))
            (error "%s" (if hidden
                            (substitute-command-keys "File line is hidden, type \\[dired-hide-subdir] to unhide")
                          "No file on this line")))
        (if (eq opoint (point)) nil (point)))))

  (unless (fboundp 'dired-check-switches)                       ; < Emacs 24
    (defun dired-check-switches (switches short &optional long) ; Emacs 27.1 `dired.el'
      "Return non-nil if the string SWITCHES matches LONG or SHORT format."
      (let (case-fold-search)
        (and (stringp switches)
             (diredp-string-match-p (concat "\\(\\`\\| \\)-[[:alnum:]]*" short
                                            (if long (concat "\\|--" long "\\>") ""))
                                    switches)))))

  (defun dired-hide-subdir (arg)        ; Emacs 27.1 `dired-aux.el'
    "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories."
    (interactive "p")
    (with-silent-modifications
      (while (>=  (setq arg  (1- arg)) 0)
        (let* ((cur-dir   (dired-current-directory))
               (hidden-p  (dired-subdir-hidden-p cur-dir))
               (elt       (assoc cur-dir dired-subdir-alist))
               (end-pos   (1- (dired-get-subdir-max elt)))
               buffer-read-only)
          ;; Keep header line visible, hide rest
          (goto-char (cdr elt))
          (end-of-line)
          (if hidden-p (dired--unhide (point) end-pos) (dired--hide (point) end-pos)))
        (dired-next-subdir 1 t))))

  (defun dired-hide-all (&optional ignored) ; Emacs 27.1 `dired-aux.el'
    "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory."
    (interactive "P")
    (with-silent-modifications
      (if (text-property-any (point-min) (point-max) 'invisible 'dired)
          (dired--unhide (point-min) (point-max))
        (let ((pos  (point-max)))        ; Position of end of last directory
          (dolist (subdir dired-subdir-alist)
            (let ((start  (cdr subdir))  ; Position of prev dir
                  (end    (save-excursion
                            (goto-char pos) ; Current dir.  We're somewhere on current dir's line.
                            (forward-line -1)
                            (point))))
              (dired--hide start end))
            (setq pos  (cdr subdir))))))) ; Previous dir gets current dir
  )

;; This is like `dired-hide-subdir' in `dired-aux.el', except:
;;
;; 1. Plain prefix arg means invoke `dired-hide-all'.  Added optional arg NEXT.
;; 2. Do not move to the next subdir.
;;
;;;###autoload
(defun diredp-hide-subdir-nomove (arg &optional next) ; Bound to `$'
    "Hide or unhide the current directory.
Unlike `dired-hide-subdir', this does not advance the cursor to the
next directory header line.

With a plain prefix arg (`C-u'), invoke `dired-hide-all' to hide or
 show everything.
With a numeric prefix arg N, hide this subdirectory and the next N-1
 subdirectories."
    (interactive "P")
    (if (consp arg)
        (dired-hide-all 'IGNORED) ; Arg needed for older Emacs versions.
      (setq arg  (prefix-numeric-value arg))
      (with-silent-modifications
        (while (>=  (setq arg  (1- arg)) 0)
          (let* ((cur-dir   (dired-current-directory))
                 (hidden-p  (dired-subdir-hidden-p cur-dir))
                 (elt       (assoc cur-dir dired-subdir-alist))
                 (end-pos   (1- (dired-get-subdir-max elt)))
                 buffer-read-only)
            (goto-char (cdr elt)) ; Keep header line visible, hide rest
            (end-of-line)
            (if hidden-p
                (dired--unhide (point) end-pos)
              (dired--hide (point) end-pos)))
          (when next (dired-next-subdir 1 t))))))

;;; ----------------------
;;; If we instead renamed `diredp-hide-subdir-nomove' to `dired-hide-subdir' as a replacement,
;;; then we would define things this way:
;;;
;;;
;;; ;; REPLACE ORIGINAL in `dired-aux.el'.
;;; ;;
;;; ;; 1. Plain prefix arg means invoke `dired-hide-all'.  Added optional arg NEXT.
;;; ;;
;;; ;; 2. Do not move to the next subdir.
;;; ;;
;;; ;; 3. Modified to work with also with older Emacs versions.
;;; ;;
;;; (defun dired-hide-subdir (arg &optional next)
;;;   "Hide or unhide the current directory.
;;; Unlike `diredp-hide-subdir-goto-next', this does not advance the
;;; cursor to the next directory header line.
;;;
;;; With a plain prefix arg (`C-u'), invoke `dired-hide-all' to hide or
;;;  show everything.
;;; With a numeric prefix arg N, hide this subdirectory and the next N-1
;;;  subdirectories."
;;;   (interactive "P")
;;;   (dired-hide-check)
;;;   (if (consp arg)
;;;       (dired-hide-all 'IGNORED)         ; Arg needed for older Emacs versions.
;;;     (setq arg  (prefix-numeric-value arg))
;;;     (let ((modflag  (buffer-modified-p)))
;;;       (while (>=  (setq arg  (1- arg)) 0)
;;;         (let* ((cur-dir   (dired-current-directory))
;;;                (hidden-p  (dired-subdir-hidden-p cur-dir))
;;;                (elt       (assoc cur-dir dired-subdir-alist))
;;;                (end-pos   (1- (dired-get-subdir-max elt)))
;;;                buffer-read-only)
;;;           (goto-char (cdr elt)) ; Keep header line visible, hide rest
;;;           (skip-chars-forward "^\n\r")
;;;           (if hidden-p
;;;               (subst-char-in-region (point) end-pos ?\r ?\n)
;;;             (subst-char-in-region (point) end-pos ?\n ?\r)))
;;;         (when next (dired-next-subdir 1 t)))
;;;       (if (fboundp 'restore-buffer-modified-p)
;;;           (restore-buffer-modified-p modflag)
;;;         (set-buffer-modified-p modflag)))))
;;;
;;; (defun diredp-hide-subdir-goto-next (arg)
;;;   "Hide or unhide current directory and move to next directory header line."
;;;   (interactive "P")
;;;   (dired-hide-subdir arg 'NEXT))
;;; ----------------------



;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; When `diredp-omit-lines-regexp' is non-nil, call `dired-omit-expunge' again to omit matching lines.
;;
(when (fboundp 'define-minor-mode)      ; Emacs 22+

  ;; Macro `define-minor-mode' is not defined in Emacs 20, so in order to be able to byte-compile
  ;; this file in Emacs 20, prohibit byte-compiling of the `define-minor-mode' call.
  ;;
  (eval '(define-minor-mode dired-omit-mode
           "Toggle omission of uninteresting files in Dired (Dired-Omit mode).
With prefix argument ARG, enable Dired-Omit mode if ARG is positive,
and disable it otherwise.

If called from Lisp, enable the mode if ARG is omitted or nil.

Dired-Omit mode is a buffer-local minor mode.

When enabled in a Dired buffer, Dired does not list files whose
filenames match regexp `dired-omit-files', files ending with
extensions in `dired-omit-extensions', or files listed on lines
matching `diredp-omit-lines-regexp'.

To enable omitting in every Dired buffer, you can put this in
your init file:

  (add-hook \\='dired-mode-hook (lambda () (dired-omit-mode)))

See Info node `(dired-x) Omitting Variables' for more information."
           :init-value nil :lighter nil :keymap nil :group 'Dired-Plus
           (if (not dired-omit-mode)
               (revert-buffer)
             (let ((dired-omit-size-limit  nil)
                   (file-count             0))
               ;; Omit by file-name match, then omit by line match.
               ;; Use count of file-name match as INIT-COUNT for line match.
               ;; Return total count.  (Return value is not used anywhere, so far).
               (setq file-count  (dired-omit-expunge))
               (when diredp-omit-lines-regexp (dired-omit-expunge diredp-omit-lines-regexp 'LINEP file-count))))))

  (unless (boundp 'dired-omit-verbose)  ; Not in Emacs < 24.
    (defvar dired-omit-verbose t
      "When non-nil, show messages when omitting files.
When nil, don't show messages."))


  ;; REPLACE ORIGINAL in `dired-x.el'.
  ;;
  ;; Added optional args LINEP and INIT-COUNT.
  ;;
  (defun dired-omit-expunge (&optional regexp linep init-count)
    "Erase all unmarked files whose names match REGEXP.
With a prefix arg (non-nil LINEP when called from Lisp), match REGEXP
against the whole line.  Otherwise, match it against the file name.

If REGEXP is nil, use `dired-omit-files', and also omit file names
ending in `dired-omit-extensions'.

Do nothing if REGEXP is the empty string, `dired-omit-mode' is nil, or
if called from Lisp and buffer is bigger than `dired-omit-size-limit'.

Optional arg INIT-COUNT is an initial count that is added to the
number of lines omitted by this invocation of `dired-omit-expunge', in
the status message."
    (interactive "sOmit files (regexp): \nP")
    (when (and (symbolp regexp)  (boundp regexp)) (setq regexp  (symbol-value regexp)))
    ;; Bind `dired-marker-char' to `dired-omit-marker-char', then call `dired-do-kill-lines'.
    (if (and dired-omit-mode
             (or (called-interactively-p 'interactive)
                 (not dired-omit-size-limit)
                 (< (buffer-size) dired-omit-size-limit)
                 (progn
                   (when dired-omit-verbose
                     (message "Not omitting: directory larger than %d characters." dired-omit-size-limit))
                   (setq dired-omit-mode  nil)
                   nil)))
        (let ((omit-re         (or regexp  (dired-omit-regexp)))
              (old-modified-p  (buffer-modified-p))
              (count           (or init-count  0)))
          (unless (string= omit-re "")
            (let ((dired-marker-char  dired-omit-marker-char))
              (when dired-omit-verbose (message "Omitting..."))
              (if (not (if linep
                           (dired-mark-if (and (= (following-char) ?\   ) ; Not already marked
                                               (string-match-p omit-re (buffer-substring
                                                                        (line-beginning-position)
                                                                        (line-end-position))))
                                          nil)
                         (dired-mark-unmarked-files
                          omit-re nil nil dired-omit-localp
                          (dired-omit-case-fold-p (if (stringp dired-directory)
                                                      dired-directory
                                                    (car dired-directory))))))
                  (when dired-omit-verbose (message "(Nothing to omit)"))
                (setq count  (+ count
                                (dired-do-kill-lines nil (if dired-omit-verbose "Omitted %d line%s" "") init-count)))
                (force-mode-line-update))))
          ;; Try to preserve modified state of buffer, so `%*' doesn't appear in `mode-line'.
          (set-buffer-modified-p (and old-modified-p  (save-excursion (goto-char (point-min))
                                                                      (re-search-forward dired-re-mark nil t))))
          count))))


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
    (interactive (list (diredp-read-regexp "Mark unmarked files matching regexp (default all): ")
                       nil
                       current-prefix-arg
                       nil))
    (let ((dired-marker-char  (if unflag-p ?\   dired-marker-char))
          (unmarkedp          (eq (char-after) ?\   )))
      (diredp-mark-if (and (if unflag-p (not unmarkedp) unmarkedp) ; Fixes Emacs bug #27465.
                           (let ((fn  (dired-get-filename localp 'NO-ERROR))) ; Uninteresting
                             (and fn  (diredp-string-match-p regexp fn))))
                      msg))))


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; 1. Call `dired-get-marked-files' with original ARG, to get its multi-`C-u' behavior.
;; 2. Doc string updated to reflect change to `dired-simultaneous-find-file'.
;; 3. Added optional arg INTERACTIVEP.
;; 4. Do not raise error if no files when not INTERACTIVEP.
;;
;;;###autoload
(defun dired-do-find-marked-files (&optional arg interactivep) ; Bound to `F'
  "Find marked files, displaying all of them simultaneously.
With no prefix argument:

* If `pop-up-frames' is nil then split the current window across all
  marked files, as evenly as possible.  Remaining lines go to the
  bottom-most window.  The number of files that can be displayed this
  way is restricted by the height of the current window and
  `window-min-height'.

* If `pop-up-frames' is non-nil then show each marked file in a
  separate frame (not window).

With a prefix argument:

* One or more plain `C-u' behaves as for `dired-get-marked-files'.
  In particular, `C-u C-u' means ignore any markings and operate on
  ALL files and directories (except `.' and `..') in the Dired buffer.

* A numeric prefix arg >= 0 means just find (visit) the marked files -
  do not show them.

* A numeric prefix arg < 0 means show each marked file in a separate
  frame (not window).  (This is the same behavior as no prefix arg
  with non-nil `pop-up-frames'.)

Note that a numeric prefix argument acts differently with this command
than it does with other `dired-do-*' commands: it does NOT act on the
next or previous (abs ARG) files, ignoring markings.

To keep the Dired buffer displayed, split the window (e.g., `C-x 2')
first.  To show only the marked files, type `\\[delete-other-windows]' first.

When invoked interactively, raise an error if no files are marked."
  (interactive "P\np")
  (dired-simultaneous-find-file
   (dired-get-marked-files nil (and (consp arg)  arg) nil nil interactivep)
   (and arg  (not (consp arg))  (prefix-numeric-value arg))))


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; Use separate frames instead of windows if `pop-up-frames' is non-nil,
;; or if prefix arg is negative.
;;
(defun dired-simultaneous-find-file (file-list option)
  "Visit all files in list FILE-LIST and display them simultaneously.
With non-nil OPTION >= 0, the files are found (visited) but not shown.

If `pop-up-frames' is non-nil or if OPTION < 0, use a separate frame
for each file.  (See also option `diredp-max-frames'.)

Otherwise, the current window is split across all files in FILE-LIST,
as evenly as possible.  Remaining lines go to the bottom-most window.
The number of files that can be displayed this way is restricted by
the height of the current window and the value of variable
`window-min-height'."
  ;; This is not interactive because it is usually too clumsy to specify FILE-LIST interactively unless via dired.
  (let (size)
    (cond ((and option  (natnump option))
           (while file-list (find-file-noselect (car file-list)) (pop file-list)))
          ((or pop-up-frames  option)
           (let ((nb-files  (length file-list)))
             (when (and (> nb-files diredp-max-frames)
                        (not (y-or-n-p (format "Really show %d files in separate frames? " nb-files))))
               (error "OK, canceled"))
             (while file-list (find-file-other-frame (car file-list)) (pop file-list))))
          (t
           (setq size  (/ (window-height) (length file-list)))
           (when (> window-min-height size) (error "Too many files to show simultaneously"))
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
;;;;  (let ((atomic-dirname-p  (atom dirname)))
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


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; Require confirmation.  Fixes Emacs bug #13561.
;;
(when (< emacs-major-version 25)
  (defun dired-do-run-mail ()
    "If `dired-bind-vm' is non-nil, call `dired-vm', else call `dired-rmail'."
    (interactive)
    (unless (y-or-n-p "Read all marked mail folders? ") (error "OK, canceled"))
    (if dired-bind-vm
        ;; Read mail folder using vm.
        (dired-vm)
      ;; Read mail folder using rmail.
      (dired-rmail))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Put `mouse-face' on whole line, not just file name.
;; 2. Add text property `dired-filename' to only the file name.
;; 3. Show image-file preview on mouseover, if `tooltip-mode'
;;    and if `diredp-image-preview-in-tooltip'.
;;
(defun dired-insert-set-properties (beg end)
  "Add various text properties to the lines in the region.
Highlight entire line upon mouseover.
Add text property `dired-filename' to the file name.
Handle `dired-hide-details-mode' invisibility spec (Emacs 24.4+)."
  (let ((inhibit-field-text-motion  t)) ; Just in case.
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (condition-case nil
            (cond ((dired-move-to-filename)
                   (add-text-properties (line-beginning-position) (line-end-position)
                                        '(mouse-face highlight help-echo diredp-mouseover-help))
                   (put-text-property
                    (point) (save-excursion (dired-move-to-end-of-filename) (point))
                    'dired-filename t)
                   (when (fboundp 'dired-hide-details-mode) ; Emacs 24.4+
                     (put-text-property (+ (line-beginning-position) 1) (1- (point))
                                        'invisible 'dired-hide-details-detail)
                     (dired-move-to-end-of-filename)
                     (when (< (+ (point) 4) (line-end-position))
                       (put-text-property (+ (point) 4) (line-end-position)
                                          'invisible 'dired-hide-details-link))))
                  ((fboundp 'dired-hide-details-mode) ; Emacs 24.4+
                   (unless (or (diredp-looking-at-p "^$")  (diredp-looking-at-p dired-subdir-regexp))
                     (put-text-property (line-beginning-position) (1+ (line-end-position))
                                        'invisible 'dired-hide-details-information))))
          (error nil))
        (forward-line 1)))))

(defun diredp-mouseover-help (window buffer pos)
  "Show `help-echo' help for a file name, in Dired.
If `tooltip-mode' is on and `diredp-image-preview-in-tooltip' says to
show an image preview, then do so.  Otherwise, show text help."
  (let ((image-dired-thumb-width   (or (and (wholenump diredp-image-preview-in-tooltip)
                                            diredp-image-preview-in-tooltip)
                                       image-dired-thumb-width))
        (image-dired-thumb-height  (or (and (wholenump diredp-image-preview-in-tooltip)
                                            diredp-image-preview-in-tooltip)
                                       image-dired-thumb-height))
        file)
    (or (and (boundp 'tooltip-mode)  tooltip-mode
             (fboundp 'image-file-name-regexp) ; Emacs 22+, `image-file.el'.
             diredp-image-preview-in-tooltip
             (condition-case nil
                 (and (with-current-buffer buffer
                        (save-excursion (goto-char pos)
                                        (diredp-string-match-p
                                         (image-file-name-regexp)
                                         (setq file  (if (derived-mode-p 'dired-mode)
                                                         (dired-get-filename nil 'NO-ERROR)
                                                       ;; Make it work also for `diredp-list-files' listings.
                                                       (buffer-substring-no-properties (line-beginning-position)
                                                                                       (line-end-position)))))))
                      (or (not diredp-auto-focus-frame-for-thumbnail-tooltip-flag)
                          (progn (select-frame-set-input-focus (window-frame window)) t))
                      (let ((img-file  (if (eq 'full diredp-image-preview-in-tooltip)
                                           file
                                         (diredp-image-dired-create-thumb file))))
                        (propertize " " 'display (create-image img-file))))
               (error nil)))
        (if (fboundp 'describe-file)    ; Library `help-fns+.el'
            "mouse-2: visit in another window, C-h RET: describe"
          "mouse-2: visit this file/dir in another window"))))

;; `dired-hide-details-mode' enhancements.
(when (fboundp 'dired-hide-details-mode) ; Emacs 24.4+

  (defun diredp-hide-details-if-dired ()
    "In Dired mode hide details.  Outside Dired, do nothing."
    (when (derived-mode-p 'dired-mode) (dired-hide-details-mode 1)))

  ;; Use `eval' of list so file byte-compiled in Emacs 20 will be OK in later versions.
  (eval '(define-globalized-minor-mode global-dired-hide-details-mode
          dired-hide-details-mode diredp-hide-details-if-dired))

  (eval '(define-minor-mode dired-hide-details-mode
          "Hide details in Dired mode."
          :init-value (and diredp-hide-details-propagate-flag  diredp-hide-details-last-state)
          :group 'dired
          (unless (derived-mode-p 'dired-mode 'wdired-mode) (error "Not a Dired buffer"))
          (dired-hide-details-update-invisibility-spec)
          (setq diredp-hide-details-toggled  t)
          (when diredp-hide-details-propagate-flag
            (setq diredp-hide-details-last-state  dired-hide-details-mode))
          (if dired-hide-details-mode
              (add-hook 'wdired-mode-hook 'dired-hide-details-update-invisibility-spec nil t)
            (remove-hook 'wdired-mode-hook 'dired-hide-details-update-invisibility-spec t))))

  (defun diredp-hide/show-details ()
    "Hide/show details according to user options.
If `diredp-hide-details-propagate-flag' is non-nil and details have
never been hidden in the buffer, then hide/show according to your last
hide/show choice in any other Dired buffer or, if no last choice,
according to option `diredp-hide-details-initially-flag'."
    (unless (or diredp-hide-details-toggled ; No op if hide/show already set.
                (buffer-narrowed-p))    ; No-op when showing just newly copied file etc.
      (cond (diredp-hide-details-propagate-flag
             (dired-hide-details-mode (if diredp-hide-details-last-state 1 -1)))
            (diredp-hide-details-initially-flag
             (dired-hide-details-mode 1)))))

  (add-hook 'dired-after-readin-hook #'diredp-hide/show-details)

  (defun diredp-fit-frame-unless-buffer-narrowed ()
    "Fit frame unless Dired buffer is narrowed.
Requires library `autofit-frame.el'."
    (unless (buffer-narrowed-p) (diredp-fit-one-window-frame)))

  ;; Fit frame only if not narrowed.  Put it on this hook because `dired-hide-details-mode' is
  ;; invoked from `dired-after-readin-hook' via `diredp-hide/show-details', even for an update
  ;; such as copying a file, where buffer is narrowed when invoked.
  (when (fboundp 'fit-frame-if-one-window) ; In `autofit-frame.el'.
    (add-hook 'dired-hide-details-mode-hook #'diredp-fit-frame-unless-buffer-narrowed)))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Reset `mode-line-process' to nil.
;;
(when (< emacs-major-version 21)
  (or (fboundp 'old-dired-revert)  (fset 'old-dired-revert (symbol-function 'dired-revert)))
  (defun dired-revert (&optional arg noconfirm)
    (setq mode-line-process  nil)        ; Set by, e.g., `find-dired'.
    (old-dired-revert arg noconfirm)))

;; Like `dired-up-directory', but go up to MS Windows drive if in top-level directory.
;;
;;;###autoload
(defun diredp-up-directory (&optional other-window) ; Bound to `^'
  "Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary.

With a prefix arg, Dired the parent directory in another window.

On MS Windows, if you are already at the root directory, invoke
`diredp-w32-drives' to visit a navigable list of Windows drives."
  (interactive "P")
  (let* ((dir  (dired-current-directory))
         (up   (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try `dired-goto-subdir' if buffer has more than one dir.
        (and (cdr dired-subdir-alist)  (dired-goto-subdir up))
        (progn (if other-window (dired-other-window up) (dired up))
               (dired-goto-file dir))
        (and (memq system-type '(windows-nt ms-dos))  (diredp-w32-drives other-window)))))

;;;###autoload
(defun diredp-up-directory-reuse-dir-buffer (&optional other-window) ; Not bound
  "Like `diredp-up-directory', but reuse Dired buffers.
With a prefix arg, Dired the parent directory in another window.

On MS Windows, moving up from a root Dired buffer does not kill that
buffer (the Windows drives buffer is not really a Dired buffer)."
  (interactive "P")
  (let* ((dir      (dired-current-directory))
         (dirfile  (directory-file-name dir))
         (up       (file-name-directory dirfile)))
    (or (dired-goto-file dirfile)
        ;; Only try `dired-goto-subdir' if buffer has more than one dir.
        (and (cdr dired-subdir-alist)  (dired-goto-subdir up)) ; It is a subdir inserted in current Dired.
        (progn (diredp--reuse-dir-buffer-helper up nil nil other-window)
               (dired-goto-file dir))
        (and (memq system-type '(windows-nt ms-dos))  (diredp-w32-drives other-window)))))

;; Differs from `dired-next-line' in both wraparound and respect of `goal-column'.
;;
;;;###autoload
(defun diredp-next-line (arg)           ; Bound to `SPC', `n', `C-n', `down'
  "Move down lines then position cursor at filename.
If `goal-column' is non-nil then put the cursor at that column.
Optional prefix ARG says how many lines to move; default is one line.

If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer end (buffer beginning, if ARG is negative).
Otherwise, just move to the buffer limit."
  (interactive (let ((narg  (prefix-numeric-value current-prefix-arg)))
                 (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
                 (list narg)))          ; Equivalent to "^p"
  (let* ((line-move-visual  nil)
         ;; (goal-column       nil)

         ;; Use `condition-case' and `(progn... t)' because Emacs < 22 `line-move' has no
         ;; NO-ERROR arg and it always returns nil.
         (no-more           (or (not (condition-case nil (progn (line-move arg) t) (error nil)))
                                (if (< arg 0) (bobp) (eobp)))))
    (when (and diredp-wrap-around-flag  no-more)
      (let ((diredp-wrap-around-flag  nil))
        (goto-char (if (< arg 0) (point-max) (point-min)))
        (diredp-next-line arg)))
    ;; We never want to move point into an invisible line.
    (while (and (fboundp 'invisible-p)  ; Emacs 22+
                (invisible-p (point))
                (not (if (and arg  (< arg 0)) (bobp) (eobp))))
      (forward-char (if (and arg  (< arg 0)) -1 1)))
    (unless goal-column (dired-move-to-filename))))

;; In Emacs < 22, `C-p' does not wrap around, because it never moves to the first header line.
;;;###autoload
(defun diredp-previous-line (arg)       ; Bound to `p', `C-p', `up'
  "Move up lines then position cursor at filename.
If `goal-column' is non-nil then put the cursor at that column.
Optional prefix ARG says how many lines to move; default is one line.

If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer beginning (buffer end, if ARG is negative).
Otherwise, just move to the buffer limit."
  (interactive (let ((narg  (prefix-numeric-value current-prefix-arg)))
                 (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
                 (list narg)))          ; Equivalent to "^p"
  (diredp-next-line (- (or arg  1))))

;;;###autoload
(defun diredp-next-dirline (arg &optional opoint) ; Bound to `>'
  "Goto ARGth next directory file line.
If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer beginning (buffer end, if ARG is negative).
Otherwise, raise an error or, if NO-ERROR-IF-NOT-FOUND is nil, return
nil."
  (interactive (let ((narg  (prefix-numeric-value current-prefix-arg)))
                 (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
                 (list narg)))          ; Equivalent to "^p"
  (or opoint  (setq opoint  (point)))
  (if (if (> arg 0)
          (re-search-forward dired-re-dir nil t arg)
        (beginning-of-line)
        (re-search-backward dired-re-dir nil t (- arg)))
      (dired-move-to-filename)          ; user may type `i' or `f'
    (if diredp-wrap-around-flag
        (let ((diredp-wrap-around-flag  nil))
          (goto-char (if (< arg 0) (point-max) (point-min)))
          (diredp-next-dirline arg opoint))
      (goto-char opoint)
      (error "No more subdirectories"))))

;;;###autoload
(defun diredp-prev-dirline (arg)        ; Bound to `<'
  "Goto ARGth previous directory file line."
  (interactive (let ((narg  (prefix-numeric-value current-prefix-arg)))
                 (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
                 (list narg)))          ; Equivalent to "^p"
  (diredp-next-dirline (- arg)))

;;;###autoload
(defun diredp-next-subdir (arg &optional no-error-if-not-found no-skip) ; Bound to `C-M-n'
  "Go to the Nth next subdirectory, regardless of level.
N is the numeric prefix arg (defaults to 1).
If N = 0 then go to this directory's header line.

If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer end (buffer beginning, if ARG is negative).
Otherwise, raise an error or, if NO-ERROR-IF-NOT-FOUND is nil, return
nil.

Non-nil NO-SKIP means do not move to end of header line, and return
the position moved to so far."
  (interactive (let ((narg  (prefix-numeric-value current-prefix-arg)))
                 (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
                 (list narg)))          ; Equivalent to "^p"
  (let ((this-dir  (dired-current-directory))
        pos index)
    ;; `nth' with negative arg does not return nil but the first element
    (setq index  (if diredp-wrap-around-flag
                     (mod (- (dired-subdir-index this-dir) arg) (length dired-subdir-alist))
                   (- (dired-subdir-index this-dir) arg))
          pos    (and (>= index 0)  (cdr (nth index dired-subdir-alist))))
    (if pos
        (progn (goto-char pos)
               (or no-skip  (skip-chars-forward "^\n\r"))
               (point))
      (if no-error-if-not-found
          nil                           ; Return nil if not found
        (error "%s directory" (if (> arg 0) "Last" "First"))))))

;;;###autoload
(defun diredp-prev-subdir (arg &optional no-error-if-not-found no-skip) ; Bound to `C-M-p'
  "Go to the Nth previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line.
Otherwise, this is a mirror image of `diredp-next-subdir'."
  ;;(interactive "^p")
  (interactive
   (list (if current-prefix-arg
             (let ((narg  (prefix-numeric-value current-prefix-arg)))
               (when (and (boundp 'shift-select-mode)  shift-select-mode) (handle-shift-selection)) ; Emacs 23+
               narg)                    ; Equivalent to "^p"
           ;; If on subdir start already then do not stay there.
           (if (dired-get-subdir) 1 0))))
  (diredp-next-subdir (- arg) no-error-if-not-found no-skip))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Test also ./ and ../, in addition to . and .., for error "Cannot operate on `.' or `..'".
;; 2. Hack for Emacs 20-22, to expand `~/...'.
;;
(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In Dired, return name of file mentioned on this line.
Value returned normally includes the directory name.

Optional arg LOCALP:
 nil:           return the absolute file name.
 `no-dir':      do not include directory name in result.
 `verbatim':    return the name exactly as it occurs in the buffer.
 Other non-nil: construct the name relative to `default-directory',
  which still might contain slashes if point is in a subdirectory.

Non-nil optional arg NO-ERROR-IF-NOT-FILEP means treat `.' and `..' as
regular filenames and return nil if there is no filename on this line.
Otherwise, an error occurs in these cases."
  (let ((case-fold-search  nil)
        (already-absolute  nil)
        file p1 p2)
    (save-excursion (when (setq p1  (dired-move-to-filename (not no-error-if-not-filep)))
                      (setq p2  (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line but `no-error-if-not-filep' is t:
    (when (setq file  (and p1  p2  (buffer-substring p1 p2)))
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
        (setq file  (replace-match "\\\"" nil t file 1)))

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
        (let ((start  0)
              (rep    "")
              (shift  -1))
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
      (setq file  (read (concat "\"" file "\"")))

      ;; Emacs 22-26 only.
      ;; Above `read' returns a unibyte string if FILE contains eight-bit-control/graphic chars.
      (when (and (fboundp 'string-to-multibyte) ; Emacs 22
                 (< emacs-major-version 27)
                 enable-multibyte-characters
                 (not (multibyte-string-p file)))
        (setq file  (string-to-multibyte file))))
    (and file
         (file-name-absolute-p file)
         ;; A relative file name can start with ~.  Do not treat it as absolute in this context.
         (not (eq (aref file 0) ?~))
         (setq already-absolute  t))
    (cond ((null file) nil)
          ((eq localp 'verbatim) file)
          ;; This is the essential `Dired+' change: Added ./ and ../, not just . and ..
          ((and (not no-error-if-not-filep)  (member file '("." ".." "./" "../")))
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
             (if (and handler  (not (get handler 'safe-magic)))
                 (concat "/:" file)
               file)))
          ;; Ugly hack for Emacs < 23, for which `ls-lisp-insert-directory' can insert a subdir
          ;; using `~/...'.  Expand `~/' for return value.
          ((and (< emacs-major-version 23)  file  (file-name-absolute-p file)
                (eq (aref file 0) ?~))
           (expand-file-name file))
          (t
           (concat (dired-current-directory localp) file)))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Made it a command.
;; 2. Use `forward-line' instead of `beginning-of-line'.
;;
(defun dired-move-to-filename (&optional raise-error eol)
  "Move to the beginning of the file name on the current line.
Return the position of the file-name beginning, or nil if none found.

Non-nil RAISE-ERROR means raise an error if no file name is found.
Non-nil EOL is the search limit.  Default: `line-end-position'."
  (interactive)
  (unless eol (setq eol  (line-end-position)))
  (forward-line 0)
  (let ((change (next-single-property-change (point) 'dired-filename nil eol)))
    (cond ((and change (< change eol))
           (goto-char change))
          ((re-search-forward directory-listing-before-filename-regexp eol t)
           (goto-char (match-end 0)))
          ((re-search-forward dired-permission-flags-regexp eol t)
           ;; There *is* a file.  Our regexp-from-hell just failed to find it.
           (when raise-error (error "Unrecognized line!  Check `directory-listing-before-filename-regexp'"))
           (beginning-of-line)
           nil)
          (raise-error (error "No file on this line")))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Fixes Emacs bug #7126: Did not work with arbitrary file list (cons arg to `dired').
;; 2. Remove `/' from directory name before comparing with BASE.
;;
(when (< emacs-major-version 24)
  (defun dired-goto-file (file)         ; Bound to `j'
    "Go to line describing file FILE in this Dired buffer.
FILE must be an absolute file name.
Return buffer position on success, else nil."
    ;; Loses if FILE contains control chars like "\007" for which `ls' inserts "?" or "\\007"
    ;; into the buffer, so we won't find it in the buffer.
    (interactive (prog1 (list (expand-file-name (read-file-name "Goto file: " (dired-current-directory))))
                   (push-mark))) ; Let `push-mark' display its message.
    (unless (file-name-absolute-p file) (error "File name `%s' is not absolute" file))
    (setq file  (directory-file-name file)) ; does no harm if no directory
    (let* ((case-fold-search  nil)
           (dir               (file-name-directory file))
           (found             nil))
      ;; `Dired+': Added this sexp.
      (save-excursion
        (goto-char (point-min))
        (let ((search-string  (replace-regexp-in-string "\^m" "\\^m" file nil t))
              (here           nil))
          (setq search-string  (replace-regexp-in-string "\\\\" "\\\\" search-string nil t))

          ;; Escape whitespace.  Added per Emacs 24 addition in `unless' code below:
          (when (and (dired-switches-escape-p dired-actual-switches)
                     (diredp-string-match-p "[ \t\n]" search-string))
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
          ;; The difficulty here is to get the result of `dired-goto-subdir' without really
          ;; calling it, if we don't have any subdirs.
          (when (if (string= dir (expand-file-name default-directory))
                    (goto-char (point-min))
                  (and (cdr dired-subdir-alist)  (dired-goto-subdir dir)))
            (let ((base      (file-name-nondirectory file))
                  (boundary  (dired-subdir-max))
                  search-string)
              (setq search-string  (replace-regexp-in-string "\^m" "\\^m" base nil t)
                    search-string  (replace-regexp-in-string "\\\\" "\\\\" search-string nil t))
              ;; Escape whitespace.  Sexp added by Emacs 24:
              (when (and (dired-switches-escape-p dired-actual-switches)
                         (diredp-string-match-p "[ \t\n]" search-string))
                ;; FIXME: fix this for all possible file names (embedded control chars etc).
                ;;        Need to escape everything that `ls -b' escapes.
                (setq search-string  (replace-regexp-in-string " " "\\ " search-string nil t)
                      search-string  (replace-regexp-in-string "\t" "\\t" search-string nil t)
                      search-string  (replace-regexp-in-string "\n" "\\n" search-string nil t)))
              (while (and (not found)
                          ;; Filenames are preceded by SPC.  This makes the search faster
                          ;; (e.g. for the filename "-"!).
                          (search-forward (concat " " search-string) boundary 'move))
                ;; `Dired+': Remove `/' from filename, then compare with BASE.
                ;; Match could have BASE just as initial substring or
                ;; or in permission bits or date or not be a proper filename at all.
                (if (and (dired-get-filename 'no-dir 'NO-ERROR)
                         (equal base (directory-file-name (dired-get-filename 'no-dir 'NO-ERROR))))
                    ;; Must move to filename since an (actually correct) match could have been
                    ;; elsewhere on the line (e.g. "-" would match somewhere in permission bits).
                    (setq found  (dired-move-to-filename))
                  ;; If this is not the right line, move forward to avoid trying this line again.
                  (forward-line 1)))))))
      (and found  (goto-char found))))) ; Return buffer position, or nil if not found.


;; REPLACE ORIGINAL in `dired.el'.
;;
;; If destination is in a hidden dir listing then open that listing.
;;
(when (= emacs-major-version 24)
  (defun dired-goto-file (file)
    "Go to line describing file FILE in this Dired buffer.
FILE must be an absolute file name.
Return buffer position on success, else nil."
    ;; Loses if FILE contains control chars like "\007" for which `ls' inserts "?" or "\\007"
    ;; into the buffer, so we won't find it in the buffer.
    (interactive (prog1 (list (expand-file-name (read-file-name "Goto file: " (dired-current-directory))))
                   (push-mark)))  ; Let push-mark display its message.
    (unless (file-name-absolute-p file) (error "File name `%s' is not absolute" file))
    (setq file  (directory-file-name file)) ; Does no harm if not a directory
    (let* ((case-fold-search  nil)
           (dir               (file-name-directory file))
           (found
            (or
             ;; First, look for a listing under the absolute name.
             (save-excursion (goto-char (point-min)) (dired-goto-file-1 file file (point-max)))
             ;; Else look for it as a relative name.  The difficulty is to get the result
             ;; of `dired-goto-subdir' without calling it, if we don't have any subdirs.
             (save-excursion
               (when (if (string= dir (expand-file-name default-directory))
                         (goto-char (point-min))
                       (and (cdr dired-subdir-alist)  (dired-goto-subdir dir)))
                 (when (dired-subdir-hidden-p (dired-current-directory))
                   (diredp-hide-subdir-nomove 1)) ; Open hidden directory.
                 (dired-goto-file-1 (file-name-nondirectory file) file (dired-subdir-max)))))))
      (and found  (goto-char found)))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Expand input file name relative to current subdir listing, not `default-directory'.
;; 2. Respect option `diredp-case-fold-search'.  Prefix arg means respect its complement instead.
;; 3. If destination is in a hidden dir listing then open that listing.
;;
(when (>= emacs-major-version 25)

  ;; Vanilla comment: Loses if FILE contains control chars like "\007" for which `ls' inserts "?" or "\\007"
  ;; into the buffer, so we won't find it in the buffer.
  (defun dired-goto-file (file &optional toggle-case-fold-p open-hidden-dir-p) ; Bound to `j'
    "Go to line describing file FILE in this Dired buffer.
Respect option `diredp-case-fold-search'.
But with a prefix arg, respect its complement instead.

When you enter FILE, it is expanded relative to the current subdir
listing.  (The directory name in the minibuffer before point.)

When called from Lisp:
  FILE must be an absolute file name.
  Non-nil TOGGLE-CASE-FOLD-P means act as if `diredp-case-fold-search'
    is toggled.
  Non-nil OPEN-HIDDEN-DIR-P means open current subdir if hidden.

Return buffer position on success, else nil."
    ;; Unlike vanilla Dired, expand input name in current subdir listing.
    (interactive (prog1 (list (let ((curr-listing-dir  (dired-current-directory)))
                                (expand-file-name (read-file-name "Goto file: " curr-listing-dir) curr-listing-dir))
;;; $$$$$ WAS THIS: (expand-file-name (read-file-name "Goto file: " (dired-current-directory)))
                              current-prefix-arg
                              t)
                   (push-mark nil t)))
    (unless (file-name-absolute-p file) (error "File name `%s' is not absolute" file))
    (setq file  (directory-file-name file)) ; Does no harm if not a directory
    (let* ((case-fold-search  (if toggle-case-fold-p (not diredp-case-fold-search) diredp-case-fold-search))
           (dir               (file-name-directory file))
           (found             (or
                               ;; Absolute name.
                               (save-excursion
                                 (goto-char (point-min))
                                 (dired-goto-file-1 file file (point-max) open-hidden-dir-p))
                               ;; Relative name with leading subdirs.  (E.g. produced by `find-dired'.)
                               (save-excursion
                                 (goto-char (point-min))
                                 (dired-goto-file-1 (file-relative-name file default-directory) file
                                                    (point-max) open-hidden-dir-p))
                               ;; Base name only.
                               ;; Get effect of `dired-goto-subdir', but without calling it if we have no subdirs.
                               (save-excursion
                                 (when (if (string= dir (expand-file-name default-directory))
                                           (goto-char (point-min))
                                         (and (cdr dired-subdir-alist)  (dired-goto-subdir dir)))
                                   (dired-goto-file-1 (file-name-nondirectory file) file
                                                      (dired-subdir-max)  open-hidden-dir-p)))))
           (result            (and found  (goto-char found)))) ; Return buffer position, or nil if not found.
      (when (called-interactively-p 'interactive)
        (if result
            (message (substitute-command-keys "Found - (`\\[exchange-point-and-mark] C-g' to go back)"))
          (pop-mark)
          (message "Not found")))
      result))


  ;; REPLACE ORIGINAL in `dired.el':
  ;;
  ;; Respect `case-fold-search'.
  ;;
  (defun dired-goto-file-1 (file full-name limit &optional open-hidden-dir-p)
    "Advance to the Dired listing labeled by FILE; return its position.
Return nil if the listing is not found.

If FILE contains chars that would not appear in a Dired buffer then
search using the quoted forms of those chars

FULL-NAME specifies the actual file name the listing must have, as
 returned by `dired-get-filename'.
LIMIT is the search limit.
Non-nil OPEN-HIDDEN-DIR-P means open current subdir listing if hidden."
    (let (str)
      (setq str  (replace-regexp-in-string "\^m" "\\^m"  file nil t)
            str  (replace-regexp-in-string "\\\\" "\\\\" str  nil t))
      (and (dired-switches-escape-p dired-actual-switches)
	   (string-match-p "[ \t\n]" str)
           ;; FIXME: to fix this for embedded control characters etc, we should escape everything that `ls -b' does.
	   (setq str  (replace-regexp-in-string " " "\\ "  str nil t)
	         str  (replace-regexp-in-string "\t" "\\t" str nil t)
	         str  (replace-regexp-in-string "\n" "\\n" str nil t)))
      (let ((found          nil)
	    (search-string  (concat " " str))) ; This makes search faster (e.g. for the filename "-").
        (while (and (not found)  (search-forward search-string limit 'move))
          ;; Check that we are in the right place.
          ;; Match could have BASE just as initial substring or in permission bits etc.
          ;;
          ;; Comparison respects `case-fold-search' (Emacs 25+).
	  (if (let ((fil  (dired-get-filename nil t)))
                ;; Not doing this.  `string-collate-equalp' doc says not to use it for file names.
                ;;  (and full-name  fil  (string-collate-equalp full-name fil nil case-fold-search)))
                (and full-name  fil  (eq t (compare-strings full-name 0 nil fil 0 nil t))))
	      (setq found  (dired-move-to-filename))
	    (forward-line 1)))
        (when (and open-hidden-dir-p  found  (dired-subdir-hidden-p (dired-current-directory)))
          (diredp-hide-subdir-nomove 1)) ; Open hidden directory.
        found)))


  ;; REPLACE ORIGINAL in `dired-x.el':
  ;;
  ;; 1. Respect option `diredp-case-fold-search'.
  ;; 2. If destination in Dired is in a hidden dir listing, open that listing.
  ;;
  (defun dired-jump (&optional other-window file-name open-hidden-dir-p)
    "Jump to name of current buffer's visited file in Dired.
With a prefix arg you are prompted for the file name instead.
If the file line can't be found in Dired, refresh and try again.

If in Dired already, pop up a level and go to directory's line.

When called from Lisp:
 Non-nil OTHER-WINDOW means jump to Dired buffer in other window.
 Non-nil FILE-NAME means jump to its line in Dired.
 Non-nil OPEN-HIDDEN-DIR-P means open current subdir if hidden."
    (interactive (list nil
                       (and current-prefix-arg  (read-file-name "Jump to Dired for file: "))
                       t))
    (if (bound-and-true-p tar-subfile-mode)
        (switch-to-buffer tar-superior-buffer)
      ;; Expand name for `dired-goto-file' - `read-file-name' can give abbreviated name (Bug#24409).
      (let* ((file   (or (and file-name  (expand-file-name file-name))  buffer-file-name))
             (dir    (if file (file-name-directory file) default-directory))
             (found  nil))
        (if (and (eq major-mode 'dired-mode)  (null file-name))
            (progn (setq dir  (dired-current-directory))
                   (dired-up-directory other-window)
                   (setq found  (dired-goto-file dir))
                   (unless found
                     (dired-insert-subdir (file-name-directory dir)) ; Refresh and try again
                     (setq found  (dired-goto-file dir))))
          (if other-window (dired-other-window dir) (dired dir))
          (when (and file  (not (setq found  (dired-goto-file file))))
            (dired-insert-subdir (file-name-directory file)) ; Refresh and try again
            (setq found  (dired-goto-file file))
            (when dired-omit-mode ; Toggle omitting, if it is on, and try again.
              (dired-omit-mode)
              (setq found  (dired-goto-file file)))))
        (when (and open-hidden-dir-p  found  (dired-subdir-hidden-p (dired-current-directory)))
          (diredp-hide-subdir-nomove 1)) ; Open hidden directory.
        found)))        ; Return buffer position, or nil if not found.

  )


;; REPLACE ORIGINAL in `dired.el':
;;
;; 1. Display a message to warn that flagged, not marked, files will be deleted.
;; 2. Use `diredp-internal-do-deletions', so it works with all Emacs versions.
;; 3. Applies fix for bug #48805.
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
         (case-fold-search   nil)
         (markers            ()))
    (if (save-excursion (goto-char (point-min)) (re-search-forward regexp nil t))
        (diredp-internal-do-deletions
         (nreverse
          ;; This cannot move point since last arg is nil.
          (dired-map-over-marks (cons (dired-get-filename) (let ((mk  (point-marker)))
                                                             (push mk markers)
                                                             mk))
                                nil))
         nil
         'USE-TRASH-CAN)             ; This arg is for Emacs 24+ only.
      (dolist (mk  markers) (set-marker mk nil))
      (unless no-msg (message "(No deletions requested.)")))))


;; REPLACE ORIGINAL in `dired.el':
;;
;; 1. Display a message to warn that marked, not flagged, files will be deleted.
;; 2. Use `diredp-internal-do-deletions', so it works with all Emacs versions.
;; 3. Applies fix for bug #48805.
;; 4. Zero ARG deletes marked, but with `delete-by-moving-to-trash' flipped.
;;
;;;###autoload
(defun dired-do-delete (&optional arg)  ; Bound to `D'
  "Delete all marked (or next ARG) files.
NOTE: This deletes the marked (`*'), not the flagged (`D'), files.

User option `dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed.

ARG is the prefix argument.

As an exception, if ARG is zero then delete the marked files, but with
the behavior specified by option `delete-by-moving-to-trash' flipped."
  (interactive "P")
  (let* ((flip                       (zerop (prefix-numeric-value arg)))
         (delete-by-moving-to-trash  (and (boundp 'delete-by-moving-to-trash)  (if flip
                                                                                   (not delete-by-moving-to-trash)
                                                                                 delete-by-moving-to-trash)))
         (markers                    ()))
    (when flip (setq arg  nil))
    ;; This is more consistent with the file-marking feature than
    ;; `dired-do-flagged-delete'.  But it can be confusing to the user,
    ;; especially since this is usually bound to `D', which is also the
    ;; `dired-del-marker'.  So offer this warning message.
    (unless arg
      (ding)
      (message "NOTE: Deletion of files marked `%c' (not those flagged `%c')."
               dired-marker-char dired-del-marker))
    (diredp-internal-do-deletions
     (nreverse
      ;; This can move point if ARG is an integer.
      (dired-map-over-marks (cons (dired-get-filename) (let ((mk  (point-marker)))
                                                         (push mk markers)
                                                         mk))
                            arg))
     arg
     t)          ; Gets ANDed anyway with `delete-by-moving-to-trash'.
    (dolist (mk  markers) (set-marker mk nil))))

(defun diredp-internal-do-deletions (file-alist arg &optional trash)
  "`dired-internal-do-deletions', but for any Emacs version.
FILE-ALIST is an alist of files to delete, with their buffer positions.
 Filenames are absolute.
ARG is normally the raw prefix arg for the calling command.
Non-nil TRASH means use the trash can."
  ;; \(car FILE-ALIST) *must* be the *last* (bottommost) file in the dired
  ;; buffer.  That way as changes are made in the buffer they do not shift
  ;; the lines still to be changed, so the (point) values in FILE-ALIST
  ;; stay valid.  Also, for subdirs in natural order, a subdir's files are
  ;; deleted before the subdir itself - the other way around would not work."
  (setq file-alist  (delq nil file-alist)) ; nils could come from `dired-map-over-marks'.
  (if (> emacs-major-version 23)
      (dired-internal-do-deletions file-alist arg trash)
    (dired-internal-do-deletions file-alist arg)))


;; RESTORE ORIGINAL from Emacs prior to Emacs 29.  Used only in `dired-pop-to-buffer'
;;
;;
(defvar dired-shrink-to-fit t
  ;; I see no reason ever to make this nil -- rms.
  ;;  (> baud-rate search-slow-speed)
  "Non-nil means Dired shrinks the display buffer to fit the marked files.")
;; (make-obsolete-variable 'dired-shrink-to-fit
;; 			"use the Customization interface to add a new rule
;; to `display-buffer-alist' where condition regexp is \"^ \\*Marked Files\\*$\",
;; action argument symbol is `window-height' and its value is nil." "24.3")


;; REPLACE ORIGINAL in `dired.el':
;;
;; Emacs 23-28: Fix bug #12281.
;; Emacs 29+: Restore the function (not used by vanilla `dired.el' anymore).
;;
(when (> emacs-major-version 22)
  (defun dired-pop-to-buffer (buf)
    "Pop up buffer BUF in a way suitable for Dired."
    (let ((split-window-preferred-function
           (lambda (window)
             (or (and (let ((split-height-threshold  0)) (window-splittable-p (selected-window)))
                      ;; Try to split the selected window vertically if that's possible.  (Bug#1806)
                      (if (fboundp 'split-window-below) (split-window-below) (split-window-vertically)))
                 (split-window-sensibly window))))
          pop-up-frames)
      (pop-to-buffer (get-buffer-create buf)))
    (set-window-start (selected-window) (point-min))
    (when dired-shrink-to-fit
      ;; Try to not delete window when we want to display less than `window-min-height' lines.
      (cond ((> emacs-major-version 24)
             (fit-window-to-buffer (get-buffer-window buf) nil 1 nil nil t))
            ((= emacs-major-version 24)
             (fit-window-to-buffer (get-buffer-window buf) nil 1 nil nil))
            ((= emacs-major-version 23)
             (fit-window-to-buffer (get-buffer-window buf) nil 1))))))


;; REPLACE ORIGINAL in `dired.el':
;;
;; 1. Delete the window or frame popped up, afterward, and bury its buffer.
;;    Fixes Emacs bug #7533.
;;
;; 2, If buffer is shown in a separate frame, do not show a menu bar for that frame.
;;
(defun dired-mark-pop-up (buffer-or-name op-symbol files function &rest args)
  "Return FUNCTION's result on ARGS after showing which files are marked.
Displays the file names in a buffer named BUFFER-OR-NAME, the default
name being \" *Marked Files*\".  The buffer is not shown if there is
just one file, `dired-no-confirm' is t, or OP-SYMBOL is a member of
the list in `dired-no-confirm'.  Uses function `dired-pop-to-buffer'
to show the buffer.

The window is not shown if there is just one file, `dired-no-confirm'
is `t', or OP-SYMBOL is a member of `dired-no-confirm'.

FILES is the list of marked files.  It can also be (t FILENAME)
in the case of one marked file, to distinguish that from using
just the current file.

FUNCTION should not manipulate the files.  It should just read input
\(an argument or confirmation)."
  (unless buffer-or-name (setq buffer-or-name  " *Marked Files*"))
  (let (result)
    (if (or (eq dired-no-confirm t)
            (memq op-symbol dired-no-confirm)
            ;; If FILES defaulted to the current line's file.
            (= (length files) 1))
        (setq result  (apply function args))
      (with-current-buffer (get-buffer-create buffer-or-name)
        (erase-buffer)
        ;; Handle (t FILE) just like (FILE), here.  That value is used (only in some cases),
        ;; to mean just one file that was marked, rather than the current-line file.
        (dired-format-columns-of-files (if (eq (car files) t) (cdr files) files))
        (remove-text-properties (point-min) (point-max)
                                '(mouse-face nil help-echo nil))
        (when (boundp 'tab-line-exclude) (setq tab-line-exclude  nil))) ; Emacs 27+
      (unwind-protect
           (save-window-excursion
             ;; Do not show menu bar, if buffer is popped up in a separate frame.
             (let ((special-display-frame-alist  (cons '(menu-bar-lines . 0)
                                                       special-display-frame-alist))
                   (default-frame-alist          (cons '(menu-bar-lines . 0)
                                                       default-frame-alist)))
               (dired-pop-to-buffer buffer-or-name)
               ;; Work around Emacs 22 bug in `dired-pop-to-buffer', which can exit with Dired buffer current.
               (set-buffer buffer-or-name)
               (goto-char (point-min)))
             (setq result  (apply function args)))
        (save-excursion
          (condition-case nil           ; Ignore error if user already deleted window.
              (progn (select-window (get-buffer-window buffer-or-name 0))
                     (if (one-window-p) (delete-frame) (delete-window)))
            (error nil)))
        (bury-buffer buffer-or-name)))
    result))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Do not mark dir header line (see bug #48883).
;;
;;;###autoload
(defun dired-repeat-over-lines (arg function)
  "Repeat FUNCTION over this line and the next ARG lines.
\(Negative ARG means previous, not next.)
Non-file lines are skipped."
  (let ((pos  (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0)  (not (eobp)))
      (setq arg  (1- arg))
      (beginning-of-line)
      (while (and (not (eobp)) (dired-between-files)) (forward-line 1))
      (save-excursion
	(forward-line 1)
	(move-marker pos (1+ (point))))
      (save-excursion (funcall function))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0)  (not (bobp)))
      (setq arg  (1+ arg))
      (forward-line -1)
      (while (and (not (bobp))  (dired-between-files)) (forward-line -1))
      (beginning-of-line)
      (when (dired-get-filename nil t) (save-excursion (funcall function))))
    (move-marker pos nil)
    (dired-move-to-filename)))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Toggle also `.' and `..', according to option `diredp-toggle-dot+dot-dot-flag'.
;; See also Emacs bug #48883.
;;
(defun dired-toggle-marks (&optional flip)
  "Toggle marks: marked files become unmarked, and vice versa.
Marks (such as `C' and `D') other than `*' are not affected.
Hidden subdirs are also not affected.

Whether `.' and `..' are toggled is controlled by option
`diredp-toggle-dot+dot-dot-flag'.  A prefix arg acts as if the option
had the opposite value."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only     t))
      (while (not (eobp))
        (or (dired-between-files)
            (and (if flip diredp-toggle-dot+dot-dot-flag (not diredp-toggle-dot+dot-dot-flag))
                 (member (dired-get-filename t t) '("." "..")))
            (apply 'subst-char-in-region (point)
                   (1+ (point))
                   (if (eq ?\   (following-char))
                       (list ?\   dired-marker-char)
                     (list dired-marker-char ?\  ))))
        (forward-line 1)))))


;; REPLACE ORIGINAL in `dired.el':
;;
;; 1. Prefix arg has more possibilities.
;; 2, Added optional arg NAME-FORM, so you can mark/unmark matching different file-name forms.
;; 3. Push REGEXP onto `regexp-search-ring'.
;;
;;;###autoload
(defun dired-mark-files-regexp (regexp &optional marker-char name-form)
  "Mark all file names matching REGEXP for use in later commands.
`.' and `..' are never marked or unmarked by this command.

Whether to mark or unmark, and what form of file name to match, are
governed by the prefix argument.  For this, a plain (`C-u') or a
double-plain (`C-u C-u') prefix arg is considered only as such - it is
not considered numerically.

Whether to mark or unmark:

 - No prefix arg, a positive arg, or a negative arg means MARK.

 - Plain (`C-u'), double-plain (`C-u C-u'), or zero (e.g. `M-0' means
   UNMARK.

The form of the file names used for matching:

 - No prefix arg (to mark) or a plain prefix arg (`C-u', to unmark)
   means use the relative file name (no directory part).

 - A positive arg (e.g. `M-9', to mark) or a double plain arg (`C-u
   C-u', to unmark) means construct the file name relative to
   `default-directory'.

   For an entry in an inserted subdir listing, this means prefix the
   relative file name (no directory part) with the subdir name
   relative to `default-directory'.

 - A negative arg (e.g. `M--', to mark) or a zero arg (e.g. `M-0'), to
   unmark) means use the absolute file name, that is, including all
   directory components.

Note that the default matching behavior of this command is different
for Dired+ than it is for vanilla Emacs.  Using a positive prefix arg
or a double plain prefix arg (`C-u C-u') gives you the same behavior
as vanilla Emacs (marking or unmarking, respectively): matching
against names that are relative to the `default-directory'.

What Dired+ offers in addition is the possibility to match against
names that are relative (have no directory part - e.g. no prefix arg
or a single `C-u', to mark & unmark, respectively), or absolute (`M--'
or `M-0', respectively).

The default behavior uses relative names because this
is likely to be the more common use case.  But matching against
absolute names gives you more flexibility.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$'
for object files--just `.o' might mark more than you might expect.
REGEXP is added to `regexp-search-ring', for subsequent regexp search.

When called from Lisp:

 MARKER-CHAR is the marker character - used for `dired-marker-char'.

 NAME-FORM determines the form of file name matched against REGEXP:

   * nil:           names that are relative to `default-directory'
   * `no-dir':      relative names (no directory part)
   * Other non-nil: absolute names

   (NAME-FORM differs from the LOCALP arg of `dired-get-filename' in
   that the nil and other non-nil cases are swapped.  This is
   unfortunate, but it is to keep nil NAME-FORM compatible with the
   vanilla Emacs behavior, where this arg does not exist.)"
  (interactive (let* ((raw      current-prefix-arg)
                      (C-u      (and (consp raw)  (= 4 (car raw))))
                      (C-u-C-u  (and (consp raw)  (= 16 (car raw))))
                      (num      (and raw  (prefix-numeric-value raw)))
                      (type     (cond ((or (not raw)  C-u)  'no-dir) ; no prefix arg, `C-u' (no dir)
                                      ((> num 0)            nil) ; `M-9', `C-u C-u' (rel to default dir)
                                      (t                    t)))) ; `M--', `M-0' (absolute)
                 (list (diredp-read-regexp (concat (if (or (consp raw)  (and num  (zerop num)))
                                                       "UNmark"
                                                     "Mark")
                                                   (format " files (regexp matching %s): "
                                                           (case type
                                                             ((nil)   "names with default dir")
                                                             (no-dir  "relative names - no dir")
                                                             (t       "absolute names")))))
                       (and raw  (or C-u  C-u-C-u  (zerop num))  ?\040)
                       type)))
  (add-to-list 'regexp-search-ring regexp) ; Add REGEXP to `regexp-search-ring'.
  (let ((dired-marker-char  (or marker-char  dired-marker-char)))
    (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))
                         (not (eolp))   ; Empty line
                         (let ((fn  (dired-get-filename (case name-form
                                                          ((nil)   t)
                                                          (no-dir  'no-dir)
                                                          (t       nil))
                                                        'NO-ERROR)))
                           (and fn  (diredp-string-match-p regexp fn))))
                    "file")))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Use `diredp-mark-if', not `dired-mark-if'.
;;
;;;###autoload
(defun dired-mark-files-containing-regexp (regexp &optional marker-char)
  "Mark files with contents containing a REGEXP match.
A prefix argument means unmark them instead.
`.' and `..' are never marked.

If a file is visited in a buffer and `dired-always-read-filesystem' is
nil, this looks in the buffer without revisiting the file, so the
results might be inconsistent with the file on disk if its contents
have changed since it was last visited."
  (interactive
   (list (diredp-read-regexp (concat (if current-prefix-arg "Unmark" "Mark") " files containing (regexp): ")
                             nil 'dired-regexp-history)
         (and current-prefix-arg  ?\040)))
  (let ((dired-marker-char  (or marker-char  dired-marker-char)))
    (diredp-mark-if (and (not (diredp-looking-at-p dired-re-dot))
                         (not (eolp))
                         (let ((fname  (dired-get-filename nil 'NO-ERROR)))
                           (when (and fname  (file-readable-p fname)  (not (file-directory-p fname)))
                             (let ((prebuf  (get-file-buffer fname)))
                               (message "Checking %s" fname)
                               ;; For now, do it inside Emacs.  Grep might be better if there are lots of files.
                               (if (and prebuf  (or (not (boundp 'dired-always-read-filesystem))
                                                    (not dired-always-read-filesystem))) ; Emacs 26+
                                   (with-current-buffer prebuf
                                     (save-excursion (goto-char (point-min)) (re-search-forward regexp nil t)))
                                 (with-temp-buffer
                                   (insert-file-contents fname)
                                   (goto-char (point-min))
                                   (re-search-forward regexp nil t)))))))
                    "file")))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Use `diredp-mark-if', not `dired-mark-if'.
;;
;;;###autoload
(defun dired-mark-symlinks (unflag-p)
  "Mark all symbolic links.
With prefix argument, unmark or unflag all those files."
  (interactive "P")
  (let ((dired-marker-char  (if unflag-p ?\040 dired-marker-char)))
    (diredp-mark-if (diredp-looking-at-p dired-re-sym) "symbolic link")))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Use `diredp-mark-if', not `dired-mark-if'.
;;
;;;###autoload
(defun dired-mark-directories (unflag-p)
  "Mark all directory file lines except `.' and `..'.
With prefix argument, unmark or unflag the files instead."
  (interactive "P")
  (let ((dired-marker-char  (if unflag-p ?\040 dired-marker-char)))
    (diredp-mark-if (and (diredp-looking-at-p dired-re-dir)  (not (diredp-looking-at-p dired-re-dot)))
                    "directory" "directories")))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Use `diredp-mark-if', not `dired-mark-if'.
;;
;;;###autoload
(defun dired-mark-executables (unflag-p)
  "Mark all executable files.
With prefix argument, unmark or unflag the files instead."
  (interactive "P")
  (let ((dired-marker-char  (if unflag-p ?\040 dired-marker-char)))
    (diredp-mark-if (diredp-looking-at-p dired-re-exe) "executable file")))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Use `diredp-mark-if', not `dired-mark-if'.
;;
;;;###autoload
(defun dired-flag-auto-save-files (&optional unflag-p)
  "Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unmark or unflag the files instead."
  (interactive "P")
  (let ((dired-marker-char  (if unflag-p ?\040 dired-del-marker)))
    (diredp-mark-if
     ;; It is less than general to check for # here, but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
                          (or (eq (preceding-char) ?#)
                              ;; Handle executables in case of -F option.  Need not worry about the other kinds
                              ;; of markings that -F makes, since they won't appear on real auto-save files.
                              (and (eq (preceding-char) ?*)
                                   (progn (forward-char -1) (eq (preceding-char) ?#)))))
          (not (diredp-looking-at-p dired-re-dir))
          (let ((fname  (dired-get-filename t 'NO-ERROR)))
            (and fname  (auto-save-file-name-p (file-name-nondirectory fname)))))
     "auto-save file")))

;;;###autoload
(defun diredp-capitalize (&optional arg) ; Bound to `% c'
  "Rename all marked (or next ARG) files by capitalizing them.
Makes the first char of the name uppercase and the others lowercase."
  (interactive "P")
  (dired-rename-non-directory #'capitalize "Rename by capitalizing:" arg))

;; This is more useful than a single-file version of `dired-do-delete'.
;;;###autoload
(defun diredp-delete-this-file (&optional use-trash-can) ; Bound to `C-k', `delete'
  "In Dired, delete this file, upon confirmation.
This uses `delete-file'.
If the file is a symlink, remove the symlink.  If the file has
multiple names, it continues to exist with the other names.

For Emacs 24 and later, a prefix arg means that if
`delete-by-moving-to-trash' is non-nil then trash the file instead of
deleting it."
  (interactive "P")
  (let ((file  (dired-get-filename)))
    (if (not (yes-or-no-p (format "%s file `%s'? " (if (and use-trash-can
                                                            (boundp 'delete-by-moving-to-trash)
                                                            delete-by-moving-to-trash)
                                                       "Trash"
                                                     "Permanently delete")
                                  file)))
        (message "OK - canceled")
      (if (> emacs-major-version 23) (delete-file file use-trash-can) (delete-file file))
      (revert-buffer))))

;;; Versions of `dired-do-*' commands for just this line's file.
;;;###autoload
(defun diredp-capitalize-this-file ()   ; Bound to `M-c'
  "In Dired, rename this file by capitalizing it.
Makes the first char of the name uppercase and the others lowercase."
  (interactive) (diredp-capitalize 1))

;;;###autoload
(defun diredp-downcase-this-file ()     ; Bound to `M-l'
  "In Dired, rename this file to lower case."
  (interactive) (dired-downcase 1))

;;;###autoload
(defun diredp-upcase-this-file ()       ; Bound to `M-u'
  "In Dired, rename this file to upper case."
  (interactive) (dired-upcase 1))

;;;###autoload
(defalias 'diredp-move-this-file 'diredp-rename-this-file)
;;;###autoload
(defun diredp-rename-this-file ()       ; Bound to `r'
  "In Dired, rename this file or move it to a different directory.
To move it, rename the directory part of its absolute name."
  (interactive)
  (let ((use-file-dialog  nil)) (dired-do-rename 1)))

(when (fboundp 'epa-dired-do-encrypt)   ; Emacs 23+
  (defun diredp-decrypt-this-file ()
    "In Dired, decrypt this file."
    (interactive)
    (let ((use-file-dialog  nil)) (epa-dired-do-decrypt 1)))

  (defun diredp-encrypt-this-file ()
    "In Dired, encrypt this file."
    (interactive)
    (let ((use-file-dialog  nil)) (epa-dired-do-encrypt 1)))

  (defun diredp-verify-this-file ()
    "In Dired, verify this file."
    (interactive)
    (let ((use-file-dialog  nil)) (epa-dired-do-verify 1)))

  (defun diredp-sign-this-file ()
    "In Dired, sign this file."
    (interactive)
    (let ((use-file-dialog  nil)) (epa-dired-do-sign 1))))

;;;###autoload
(defun diredp-copy-this-file ()         ; Not bound
  "In Dired, copy this file."
  (interactive)
  (let ((use-file-dialog  nil)) (dired-do-copy 1)))

;;;###autoload
(defun diredp-relsymlink-this-file ()   ; Bound to `y'
  "In Dired, make a relative symbolic link to file on cursor line."
  (interactive)
  (let ((use-file-dialog  nil)) (dired-do-relsymlink 1)))

;;;###autoload
(defun diredp-symlink-this-file ()      ; Not bound
  "In Dired, make a symbolic link to this file."
  (interactive)
  (let ((use-file-dialog  nil)) (dired-do-symlink 1)))

;;;###autoload
(defun diredp-hardlink-this-file ()     ; Not bound
  "In Dired, add a name (hard link) to this file."
  (interactive)
  (let ((use-file-dialog  nil)) (dired-do-hardlink 1)))

;;;###autoload
(defun diredp-print-this-file ()        ; Bound to `M-p'
  "In Dired, print this file."
  (interactive)
  (let ((use-file-dialog  nil)) (dired-do-print 1)))

;;;###autoload
(defun diredp-grep-this-file ()         ; Not bound
  "In Dired, grep this file."
  (interactive)
  (unless (and grep-command  (or (< emacs-major-version 22)
                                 (not grep-use-null-device)
                                 (eq grep-use-null-device t)))
    (grep-compute-defaults))
  (grep (diredp-do-grep-1 (list (dired-get-filename t)))))

;;;###autoload
(defun diredp-compress-this-file ()     ; Bound to `z'
  "In Dired, compress or uncompress this file."
  (interactive)
  (let ((use-file-dialog  nil)) (dired-do-compress 1)))

;;;###autoload
(defun diredp-async-shell-command-this-file (command filelist) ; Not bound
  "Run a shell COMMAND asynchronously on the file on the Dired cursor line.
Like `diredp-shell-command-this-file', but adds `&' at the end of
COMMAND to execute it asynchronously.  The command output appears in
buffer `*Async Shell Command*'."
  (interactive (list (dired-read-shell-command (concat "& on " "%s: ") 1 (list (dired-get-filename t)))
                     (list (dired-get-filename t))))
  (unless (diredp-string-match-p "&[ \t]*\\'" command) (setq command  (concat command " &")))
  (dired-do-shell-command command 1 filelist))

;;;###autoload
(defun diredp-shell-command-this-file (command filelist) ; Not bound
  "In Dired, run a shell COMMAND on this file."
  (interactive (list (dired-read-shell-command (concat "! on " "%s: ") 1 (list (dired-get-filename t)))
                     (list (dired-get-filename t))))
  (dired-do-shell-command command 1 filelist))

;;;###autoload
(defun diredp-bookmark-this-file (&optional prefix) ; Bound to `C-B' (`C-S-b')
  "In Dired, bookmark this file.
See `diredp-do-bookmark'."
  (interactive (progn (diredp-ensure-mode)
                      (list (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: ")))))
  (diredp-do-bookmark prefix 1))

;;;###autoload
(defun diredp-tag-this-file (tags &optional prefix) ; Bound to `T +'
  "In Dired, add some tags to this file.
You need library `bookmark+.el' to use this command."
  (interactive (progn (diredp-ensure-bookmark+)
                      (diredp-ensure-mode)
                      (list (bmkp-read-tags-completing)
                            (and diredp-prompt-for-bookmark-prefix-flag
                                 (read-string "Prefix for bookmark name: ")))))
  (diredp-do-tag tags prefix 1))

;;;###autoload
(defun diredp-untag-this-file (tags &optional prefix arg) ; Bound to `T -'
  "In Dired, remove some tags from this file.
With a prefix arg, remove all tags from the file.
You need library `bookmark+.el' to use this command."
  (interactive (progn (diredp-ensure-bookmark+)
                      (diredp-ensure-mode)
                      (let* ((pref  (and diredp-prompt-for-bookmark-prefix-flag
                                         (read-string "Prefix for bookmark name: ")))
                             (bmk   (bmkp-get-autofile-bookmark (dired-get-filename) nil pref))
                             (btgs  (and bmk  (bmkp-get-tags bmk))))
                        (unless btgs (error "File has no tags to remove"))
                        (list (if current-prefix-arg btgs (bmkp-read-tags-completing btgs))
                              pref
                              current-prefix-arg))))
  (diredp-do-untag tags prefix 1))

;;;###autoload
(defun diredp-remove-all-tags-this-file (&optional prefix msgp) ; Bound to `T 0'
  "In Dired, remove all tags from this file.
You need library `bookmark+.el' to use this command."
  (interactive (progn (diredp-ensure-bookmark+)
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
  (interactive (progn (diredp-ensure-bookmark+)
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
  (interactive (progn (diredp-ensure-bookmark+)
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
  (interactive (progn (diredp-ensure-bookmark+)
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
  (interactive (progn (diredp-ensure-bookmark+)
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
  (diredp-previous-line 1))

(when (fboundp 'describe-file)          ; In `help-fns+.el'.

  (defun diredp-describe-file (&optional arg) ; Bound to `C-h RET', `C-h C-RET'
    "In Dired, describe this file or directory.
You need library `help-fns+.el' to use this command.

A non-negative prefix arg means that if the targeted file or dir is a
symlink then describe the symlinked file or dir instead.

If the file has an autofile bookmark and you use library `Bookmark+',
then show also the bookmark information (tags etc.).  In this case, a
non-positive prefix arg shows the internal form of the bookmark."
    (interactive "P")
    (diredp-ensure-mode)
    (diredp-describe-file-1 (dired-get-filename nil 'NO-ERROR) arg))

  (defun diredp-mouse-describe-file (event &optional arg) ; Not bound
    "Describe the clicked file.
You need library `help-fns+.el' to use this command.

A non-negative prefix arg means that if the targeted file or dir is a
symlink then describe the symlinked file or dir instead.

If the file has an autofile bookmark and you use library `Bookmark+',
then show also the bookmark information (tags etc.).  In this case, a
non-positive prefix arg shows the internal form of the bookmark."
    (interactive "e\nP")
    (let (file)
      (with-current-buffer (window-buffer (posn-window (event-end event)))
        (diredp-ensure-mode)
        (save-excursion (goto-char (posn-point (event-end event)))
                        (setq file  (dired-get-filename nil 'NO-ERROR))))
      (diredp-describe-file-1 file arg)))

  (defun diredp-describe-file-1 (file arg)
    "Helper for `diredp(-mouse)-describe-file."
    (let* ((numarg            (and current-prefix-arg  (prefix-numeric-value current-prefix-arg)))
           (follow-symlink-p  (and numarg  (natnump numarg)))
           (bmk-internal-p    (and numarg  (<= numarg 0)))
           (content-desc      (diredp-file-content-description file follow-symlink-p))
           (content-desc      (if (diredp-string-match-p "directory\n" content-desc)
                                  ""
                                (concat "Content type:"
                                        (if (> (length content-desc) 43)
                                            "\n "
                                          "               ")
                                        content-desc))))
      (describe-file file bmk-internal-p)
      (when content-desc
        (with-current-buffer "*Help*"
          (let ((inhibit-read-only  t))
            (save-excursion
              (goto-char (point-min))
              (search-forward "File type:")
              (forward-line 1)
              (insert content-desc)))))))

  ;; This is from `dired-show-file-type'.
  ;;
  (defun diredp-file-content-description (file &optional follow-symlink-p)
    "Return the `dired-show-file-type' description as a string.
The string doesn't include the file name, and it ends with a newline."
    (unless file (setq file  (if (derived-mode-p 'dired-mode)
                                 (dired-get-filename t)
                               (read-file-name "File: "))))
    (let ((process-file-side-effects  nil))
      (with-temp-buffer
        (if follow-symlink-p
            (process-file "file" nil t t "-L" "--" file)
          (process-file "file" nil t t "--" file))
        (when (bolp) (backward-delete-char 1))
        (filter-buffer-substring (point-min)
                                 (save-excursion (goto-char (point-min))
                                                 (search-forward (concat file ": ") (line-end-position)))
                                 'DELETE)
        (concat (buffer-string) "\n"))))

  )

;; Define these even if `Bookmark+' is not loaded.
;;;###autoload
(defalias 'diredp-show-metadata 'diredp-describe-autofile)
;;;###autoload
(defun diredp-describe-autofile (&optional internal-form-p)
  "Show the metadata for the file of the current line.
The file must name an autofile bookmark.  The metadata is the bookmark
information.

With a prefix argument, show the internal definition of the bookmark.

You need library `bookmark+.el' for this command."
  (interactive "P")
  (diredp-ensure-bookmark+)
  (diredp-ensure-mode)
  (let ((bmk  (save-match-data
                (bmkp-get-autofile-bookmark (dired-get-filename nil 'NO-ERROR)))))
    (unless bmk (error "Not on an autofile bookmark"))
    (save-selected-window (if internal-form-p
                              (bmkp-describe-bookmark-internals bmk)
                            (bmkp-describe-bookmark bmk)))))

(defun diredp-mouse-describe-autofile (event &optional internal-form-p) ; Not bound
  "Show the metadata for the file whose name you click.
The file must name an autofile bookmark.  The metadata is the bookmark
information.

With a prefix argument, show the internal definition of the bookmark.

You need library `bookmark+.el' for this command."
  (interactive "e\nP")
  (diredp-ensure-bookmark+)
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (diredp-ensure-mode)
      (save-excursion (goto-char (posn-point (event-end event)))
                      (setq file  (dired-get-filename nil 'NO-ERROR))))
    (let ((bmk  (save-match-data (bmkp-get-autofile-bookmark file))))
      (unless bmk (error "Not an autofile bookmark"))
      (save-selected-window (if internal-form-p
                                (bmkp-describe-bookmark-internals bmk)
                              (bmkp-describe-bookmark bmk))))))

;;;###autoload
(defalias 'diredp-show-metadata-for-marked 'diredp-describe-marked-autofiles)
;;;###autoload
(defun diredp-describe-marked-autofiles (&optional internal-form-p interactivep details)
  "Show metadata for the marked files.
If no file is marked, describe ALL autofiles in this directory.
With a prefix argument, show the internal (Lisp) form of the metadata.
When invoked interactively, raise an error if no files are marked.
You need library `bookmark+.el' for this command.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'."
  (interactive (list current-prefix-arg t diredp-list-file-attributes))
  (diredp-ensure-bookmark+)
  (let ((help-xref-following  nil))
    (help-setup-xref (list `(lambda (_buf)
                             (with-current-buffer ,(current-buffer) (diredp-describe-marked-autofiles)))
                           internal-form-p)
                     (if (or (> emacs-major-version 23)
                             (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                         (called-interactively-p 'interactive)
                       (interactive-p))))
  (diredp-with-help-window "*Help*"
    (let ((marked  (dired-get-marked-files nil nil nil 'DISTINGUISH-ONE-MARKED interactivep)))
      (unless (cdr marked)
        (message "Describing ALL autofiles here (none are marked)...")
        (setq marked  (diredp-get-files 'IGNORE-MARKS-P nil nil nil nil details)))
      (if (eq t (car marked))
          (diredp-describe-autofile internal-form-p)
        (dolist (bmk  (delq nil (mapcar #'bmkp-get-autofile-bookmark marked)))
          (if internal-form-p
              (let* ((bname      (bmkp-bookmark-name-from-record bmk))
                     (help-text  (format "%s\n%s\n\n%s"
                                         bname (make-string (length bname) ?-) (pp-to-string bmk))))
                (princ help-text) (terpri))
            (princ (bmkp-bookmark-description bmk)) (terpri)))))))

;;;###autoload
(defun diredp-byte-compile-this-file () ; Bound to `b'
  "In Dired, byte compile the (Lisp source) file on the cursor line."
  (interactive) (dired-do-byte-compile 1))

;;;###autoload
(defun diredp-load-this-file ()         ; Not bound
  "In Dired, load this file."
  (interactive) (dired-do-load 1))

;;;###autoload
(defun diredp-chmod-this-file ()        ; Bound to `M-m'
  "In Dired, change the mode of this file."
  (interactive) (dired-do-chmod 1))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-chgrp-this-file ()      ; Not bound
    "In Dired, change the group of this file."
    (interactive) (dired-do-chgrp 1)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-chown-this-file ()      ; Not bound
    "In Dired, change the owner of this file."
    (interactive) (dired-do-chown 1)))

(when (fboundp 'dired-do-touch)
  (defun diredp-touch-this-file ()        ; Not bound
    "In Dired, `touch' (change the timestamp of) this file."
    (interactive) (dired-do-touch 1)))


;; Just to help discovery.  And can't use the name `dired-mark-if', since that's a macro.
;;
;;;###autoload
(defalias 'diredp-mark-if-sexp 'dired-mark-sexp)
;;
;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; 1. Variable (symbol) `s' -> `blks'.
;; 2. Fixes to remove leading space from `uid' and allow `.' in `gid'.
;; 3. Cleaned up doc string and code a bit.
;;
;;;###autoload
(defun dired-mark-sexp (predicate &optional unmark-p) ; Bound to `M-(', `* ('
  "Mark files for which PREDICATE returns non-nil.
With a prefix arg, unmark or unflag those files instead.

PREDICATE is a lisp sexp that can refer to the following symbols as
variables:

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
     First, Dired just the source files: `dired *.el'.
     Then, use \\[dired-mark-sexp] with this sexp:
          (not (file-exists-p (concat name \"c\")))

There's an ambiguity when a single integer not followed by a unit
prefix precedes the file mode: It is then parsed as inode number
and not as block size (this always works for GNU coreutils ls).

Another limitation is that the uid field is needed for the
function to work correctly.  In particular, the field is not
present for some values of `ls-lisp-emulation'.

This function operates only on the Dired buffer content.  It does not
refer at all to the underlying file system.  Contrast this with
`find-dired', which might be preferable for the task at hand."
  ;; Using `sym' = "", instead of nil, for non-linked files avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Use `equal' instead of `=' in the example, as it works on integers and strings.
  (interactive "xMark if (vars: inode,blks,mode,nlink,uid,gid,size,time,name,sym): \nP")
  (message "%s" predicate)
  (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char))
        (inode              nil)
        (blks               ())
        mode nlink uid gid size time name sym)
    (diredp-mark-if
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
              ;; GNU `ls -hs' suffixes the block count with a unit and prints it as a float; FreeBSD does neither.
              ;; $$$$$$ (dired-re-inode-size  "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?")
              (dired-re-inode-size (if (> emacs-major-version 24)
                                       "\\=\\s *\\([0-9]+\\s +\\)?\
\\(?:\\([0-9]+\\(?:\\.[0-9]*\\)?[BkKMGTPEZY]?\\)? ?\\)"
                                     "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?"))
              pos)
          (beginning-of-line)
          (forward-char 2)
          (search-forward-regexp dired-re-inode-size nil t)
          ;; XXX Might be a size not followed by a unit prefix.  Could set `blks' to `inode' if it were otherwise
          ;; nil, with similar reasoning as for setting `gid' to `uid', but it would be even more whimsical.
          (setq inode  (and (match-string 1)  (string-to-number (match-string 1)))
                blks   (and (match-string 2)  (if (fboundp 'dired-x--string-to-number)
                                                  (dired-x--string-to-number (match-string 2)) ; Emacs 25+
                                                (string-to-number (match-string 2))))
                mode   (buffer-substring (point) (+ mode-len (point))))
          (forward-char mode-len)
          (unless (eq (char-after) ?\   ) (forward-char 1)) ; Skip any extended attributes marker ("." or "+").
          (setq nlink  (read (current-buffer)))
          ;; Karsten Wenger <kw@cis.uni-muenchen.de> fixed uid.

          ;; Another issue is that GNU `ls -n' right-justifies numerical UIDs and GIDs, while FreeBSD
          ;; left-justifies them, so do not rely on a specific whitespace layout.  Both of them right-justify all
          ;; other numbers, though.
          ;; XXX Return a number if the `uid' or `gid' seems to be numerical?
          ;; $$$$$$ (setq uid  (buffer-substring (+ (point) 1) (progn (forward-word 1) (point))))
          (setq uid  (buffer-substring (progn (skip-chars-forward " \t")  (point))
                                       (progn (skip-chars-forward "^ \t") (point))))
          (cond ((> emacs-major-version 24)
                 (dired-move-to-filename)
                 (save-excursion
                   (setq time
                         ;; The regexp below tries to match from the last digit of the size field through a
                         ;; space after the date.  Also, dates may have different formats depending on file age,
                         ;; so the date column need not be aligned to the right.
                         (buffer-substring (save-excursion (skip-chars-backward " \t") (point))
                                           (progn (re-search-backward directory-listing-before-filename-regexp)
                                                  (skip-chars-forward "^ \t")
                                                  (1+ (point))))
                         size
                         (dired-x--string-to-number
                          ;; We know that there's some kind of number before point because the regexp search
                          ;; above succeeded.  Not worth doing an extra check for leading garbage.
                          (buffer-substring (point) (progn (skip-chars-backward "^ \t") (point))))
                         ;; If no `gid' is displayed, `gid' will be set to `uid' but user will then not reference
                         ;; it anyway in PREDICATE.
                         gid
                         (buffer-substring (progn (skip-chars-backward " \t") (point))
                                           (progn (skip-chars-backward "^ \t") (point)))))
                 (setq name  (buffer-substring (point) (or (dired-move-to-end-of-filename t)  (point)))
                       sym   (if (diredp-looking-at-p " -> ")
                                 (buffer-substring (progn (forward-char 4) (point)) (line-end-position))
                               "")))
                (t
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
                 ;; if no `gid' is displayed, `gid' will be set to `uid' but user will then not reference
                 ;; it anyway in PREDICATE.
                 (setq gid   (buffer-substring (save-excursion (forward-word 1) (point)) (point))
                       time  (buffer-substring (match-beginning 1) (1- (dired-move-to-filename)))
                       name  (buffer-substring (point) (or (dired-move-to-end-of-filename t)  (point)))
                       sym   (if (diredp-looking-at-p " -> ")
                                 (buffer-substring (progn (forward-char 4) (point)) (line-end-position))
                               "")))))
        ;; Vanilla Emacs uses `lexical-binding' = t, and it passes bindings to `eval' as a second arg.
        ;; We use `lexical-binding' = nil, and anyway there should be no need to pass the bindings.
        (eval predicate)))
     (format "'%s file" predicate))))

(defun diredp-this-file-marked-p (&optional mark-char)
  "Return non-nil if the file on this line is marked.
Optional arg MARK-CHAR is the type of mark to check.
 If nil, then if the file has any mark, including `D', it is marked."
  (and (dired-get-filename t 'NO-ERROR)  (save-excursion
                                           (beginning-of-line)
                                           (if mark-char
                                               (diredp-looking-at-p
                                                (concat "^" (regexp-quote (char-to-string mark-char))))
                                             (not (diredp-looking-at-p "^ "))))))

(defun diredp-this-file-unmarked-p (&optional mark-char)
  "Return non-nil if the file on this line is unmarked.
Optional arg MARK-CHAR is the type of mark to check.
 If nil, then if the file has no mark, including `D', it is unmarked.
 If non-nil, then it is unmarked for MARK-CHAR if it has no mark or
 it has any mark except MARK-CHAR."
  (and (dired-get-filename t 'NO-ERROR)  (save-excursion
                                           (beginning-of-line)
                                           (if mark-char
                                               (not (diredp-looking-at-p
                                                     (concat "^" (regexp-quote (char-to-string mark-char)))))
                                             (diredp-looking-at-p "^ ")))))

(put 'diredp-mark 'interactive-only t)
;;;###autoload
(defun diredp-mark (arg &optional char) ; Bound to `m', `* m'
  "Mark current line, lines in active region, or lines in a listing.
If the region is active and nonempty:
 * Mark the lines in the region.
 * With a prefix arg, you are prompted for the CHAR to mark with.

If cursor is on a listing header line (main dir or a subdir):
 * Mark all lines in that listing except dirs `.' and `..'.
 * With a prefix arg, you are prompted for the CHAR to mark with.

Otherwise, with numeric prefix arg N, mark the next N lines.

Use \\<dired-mode-map>`\\[dired-unmark-all-files]' to remove marks everywhere, \
or `\\[dired-unmark]' on a listing header
line to remove marks in the listing."
  (interactive "P")
  (let ((dired-marker-char  dired-marker-char))
    (cond ((diredp-nonempty-region-p)
           (when arg
             (when (< emacs-major-version 28) (message nil)) ; Workaround for bug #46243.
             (setq dired-marker-char  (read-char "Mark region lines with char: ")))
           (diredp-mark-region-files))
          ((dired-get-subdir)
           (when arg
             (when (< emacs-major-version 28) (message nil)) ; Workaround for bug #46243.
             (setq dired-marker-char  (read-char "Mark subdir lines with char: ")))
           (save-excursion (dired-mark-subdir-files)))
          (t
           (let ((inhibit-read-only  t))
             (dired-repeat-over-lines
              (prefix-numeric-value arg)
              (lambda () (delete-char 1) (insert dired-marker-char))))))))

;;;###autoload
(defun diredp-mark-with-char (char &optional arg) ; Not bound, except in menus
  "Mark this line with CHAR.
With numeric prefix arg N, mark the next N lines."
  ;; Need workaround for Emacs < 28 - see Emacs bug #46243.
  ;; (interactive "cMark this line with char: \np")
  (interactive
   (progn (when (< emacs-major-version 28) (message nil)) ; Workaround for bug #46243.
          (list (read-char "Mark this line with char: ")
                (prefix-numeric-value current-prefix-arg))))
  (let ((dired-marker-char  char)) (dired-mark arg)))

;;;###autoload
(defun diredp-mark-region-files-with-char (char &optional unmark-p) ; Not bound, except in menus
  "Mark lines in active region with CHAR.
With non-nil prefix arg, unmark CHAR instead."
  ;; Need workaround for Emacs < 28 - see Emacs bug #46243.
  ;;(interactive "cMark region with char: \nP")
  (interactive
   (progn (when (< emacs-major-version 28) (message nil)) ; Workaround for bug #46243.
          (list (read-char "Mark region with char: ") current-prefix-arg)))
  (let ((dired-marker-char  char)
        (beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (when (and (bolp) (> end beg)) (backward-char)) (line-end-position)))
    (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char)))
      (diredp-mark-if (and (<= (point) end)  (>= (point) beg)  (diredp-this-file-unmarked-p)) "region file"))))

;;;###autoload
(defun diredp-mark-region-files (&optional unmark-p) ; Not bound, except in menus
  "Mark all of the files in the current region (if it is active).
With non-nil prefix arg, unmark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (when (and (bolp) (> end beg)) (backward-char)) (line-end-position)))
    (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char)))
      (diredp-mark-if (and (<= (point) end)  (>= (point) beg)  (diredp-this-file-unmarked-p)) "region file"))))

;;;###autoload
(defun diredp-unmark-region-files (&optional mark-p) ; Not bound, except in menus
  "Unmark all of the files in the current region (if it is active).
With non-nil prefix arg, mark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (when (and (bolp) (> end beg)) (backward-char)) (line-end-position)))
    (let ((dired-marker-char  (if mark-p dired-marker-char ?\040)))
      (diredp-mark-if (and (<= (point) end)  (>= (point) beg)  (diredp-this-file-marked-p)) "region file"))))

;;;###autoload
(defun diredp-flag-region-files-for-deletion () ; Not bound
  "Flag all of the files in the current region (if it is active) for deletion."
  (interactive)
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (line-beginning-position))
          end  (save-excursion (goto-char end) (when (and (bolp) (> end beg)) (backward-char)) (line-end-position)))
    (let ((dired-marker-char  dired-del-marker))
      (diredp-mark-if (and (<= (point) end)  (>= (point) beg)  (diredp-this-file-unmarked-p ?\D))
                      "region file"))))

;;;###autoload
(defun diredp-toggle-marks-in-region (beg end) ; Not bound
  "Toggle marks in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (if (not (fboundp 'dired-toggle-marks))
          ;; Pre-Emacs 22.  Use bol, eol.  If details hidden, show first.
          (let ((details-hidden-p  (and (boundp 'dired-details-state)  (eq 'hidden dired-details-state))))
            (widen)
            (when details-hidden-p (dired-details-show))
            (goto-char beg)
            (setq beg  (line-beginning-position))
            (goto-char end)
            (when (and (bolp) (> end beg)) (backward-char))
            (setq end  (line-end-position))
            (narrow-to-region beg end)
            (dired-toggle-marks)
            (when details-hidden-p (dired-details-hide)))
        (narrow-to-region beg end)
        (dired-toggle-marks))))
  (diredp-fit-one-window-frame))


;;; Mouse 3 menu.
;;;;;;;;;;;;;;;;;

(defvar diredp-file-line-overlay nil)

;;;###autoload
(defun diredp-mouse-3-menu (event)      ; Bound to `mouse-3'
  "Dired pop-up `mouse-3' menu, for files in selection or current line."
  (interactive "e")
  (if (not (and (fboundp 'mouse3-dired-use-menu)  (diredp-nonempty-region-p)))
      ;; No `mouse3.el' or no region.
      (if (diredp-nonempty-region-p)
          ;; Region
          (let ((reg-choice  (x-popup-menu
                              event
                              (list "Files in Region"
                                    (list ""
                                          '("Mark" . diredp-mark-region-files)
                                          '("Mark with Char" . diredp-mark-region-files-with-char) ; But bug #46243.
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
                (when (setq file/dir-name  (and (not (eobp))  (dired-get-filename nil 'NO-ERROR)))
                  (if diredp-file-line-overlay ; Don't re-create if exists.
                      (move-overlay diredp-file-line-overlay bol eol (current-buffer))
                    (setq diredp-file-line-overlay  (make-overlay bol eol))
                    (overlay-put diredp-file-line-overlay 'face 'region))
                  (sit-for 0)
                  (let ((map
                         (easy-menu-create-menu
                          "This File"
                          `(
                            ("Bookmark" :visible (featurep 'bookmark+)
                             ["Bookmark..." diredp-bookmark-this-file]
                             ["Add Tags..." diredp-tag-this-file
                              :visible (featurep 'bookmark+)]
                             ["Remove Tags..." diredp-untag-this-file
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
                            ["Describe" (if (if (> emacs-major-version 21)
                                                (require 'help-fns+ nil t)
                                              (require 'help+20 nil t))
                                            (diredp-describe-file file/dir-name)
                                          (diredp-describe-autofile file/dir-name))] ; Requires `bookmark+.el'
                            ;; Stuff from `Marks' menu.
                            ["Mark"  diredp-mark
                             :visible (not (eql (dired-file-marker file/dir-name)
                                                dired-marker-char))]
                            ["Mark with Char..."  diredp-mark-with-char] ; But see bug #46243.
                            ["Unmark" dired-unmark
                             :visible (dired-file-marker file/dir-name)]
                            ["Flag for Deletion" dired-flag-file-deletion
                             :visible (not (eql (dired-file-marker file/dir-name)
                                                dired-del-marker))]
                            ["Delete..." diredp-delete-this-file]
                            "--" ; ------------------------------------------------------
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
                                          (not (equal (expand-file-name (dired-current-directory))
                                                      (expand-file-name default-directory))))] ; Not top.
                            ["Insert This Subdir"
                             (lambda () (interactive)
                               (call-interactively #'dired-maybe-insert-subdir)
                               (setq movep  t))
                             :visible (and (atom (diredp-this-subdir))
                                           (not (assoc (file-name-as-directory (diredp-this-subdir))
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
                             :visible (not (equal
                                            (expand-file-name (dired-current-directory))
                                            (expand-file-name default-directory)))] ; In subdir, not top.
                            ["Remove This Inserted Subdir and Lower" diredp-kill-this-tree
                             :visible (and (fboundp 'diredp-kill-this-tree)
                                           (not (equal
                                                 (expand-file-name (dired-current-directory))
                                                 (expand-file-name default-directory))))] ; In subdir, not top.
                            ["Dired This Inserted Subdir (Tear Off)"
                             (lambda () (interactive) (diredp-dired-this-subdir t))
                             :visible (not (equal (expand-file-name (dired-current-directory))
                                                  (expand-file-name default-directory)))] ; In subdir, not top.
                            "--" ; ------------------------------------------------------
                            ["Compare..." diredp-ediff]
                            ["Diff..." dired-diff]
                            ["Diff with Backup" dired-backup-diff]

                            ["Bookmark..." diredp-bookmark-this-file
                             :visible (not (featurep 'bookmark+))]
                            "--" ; ------------------------------------------------------
                            ["Rename to..." diredp-rename-this-file]
                            ["Capitalize" diredp-capitalize-this-file]
                            ["Upcase" diredp-upcase-this-file]
                            ["Downcase" diredp-downcase-this-file]
                            "--" ; ------------------------------------------------------
                            ["Copy to..." diredp-copy-this-file]
                            ["Move/Rename to..." diredp-rename-this-file]
                            ["Symlink to (Relative)..." diredp-relsymlink-this-file]
                            ["Symlink to..." diredp-symlink-this-file]
                            ["Hardlink to..." diredp-hardlink-this-file]
                            "--" ; ------------------------------------------------------
                            ["Load" diredp-load-this-file]
                            ["Byte-Compile" diredp-byte-compile-this-file]
                            ["Asynchronous Shell Command..."
                             diredp-async-shell-command-this-file]
                            ["Shell Command..." diredp-shell-command-this-file]
                            ["Compress/Uncompress" diredp-compress-this-file]
                            ["Print..." diredp-print-this-file]
                            ["Add To Recent Visits" diredp-add-this-to-recentf
                             :visible (featurep 'recentf)]
                            ["Remove From Recent Visits" diredp-remove-this-from-recentf
                             :visible (featurep 'recentf)]
                            ["Grep" diredp-grep-this-file]
                            "--" ; ------------------------------------------------------
                            ["Change Timestamp..." diredp-touch-this-file]
                            ["Change Mode..." diredp-chmod-this-file]
                            ["Change Group..." diredp-chgrp-this-file
                             :visible (fboundp 'diredp-chgrp-this-file)]
                            ["Change Owner..." diredp-chown-this-file
                             :visible (fboundp 'diredp-chown-this-file)]))))
                    (when diredp-file-line-overlay
                      (delete-overlay diredp-file-line-overlay))
                    (setq choice  (x-popup-menu event map))
                    (when choice (call-interactively (lookup-key map (apply 'vector choice))))))
              (unless movep (goto-char opoint))))))
    ;; `mouse3.el' and active region.
    (unless (eq mouse3-dired-function 'mouse3-dired-use-menu)
      (funcall #'mouse3-dired-use-menu)
      (revert-buffer))
    (let ((last-command  'mouse-save-then-kill)) (mouse-save-then-kill event))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Show erroneous file name in error message.
;; 2. (Needed only for Emacs 20) Allow `.' and `..', by using non-nil second arg to `dired-get-filename'.
;;
(defun dired-find-file ()               ; Bound to `RET'
  "In Dired, visit the file or directory named on this line."
  (interactive)
  (let* ((dgf-result  (or (dired-get-filename nil 'NO-ERROR)  (error "No file on this line")))
         (file-name   (file-name-sans-versions dgf-result t)))
    (if (file-exists-p file-name)
        (find-file file-name)
      (error (if (file-symlink-p file-name)
                 "File `%s' is a symlink to a nonexistent target"
               "File `%s' no longer exists; type `g' to update Dired buffer")
             file-name))))

;;;###autoload
(defun diredp-find-file-other-frame ()  ; Bound to `C-o'
  "In Dired, visit this file or directory in another frame."
  (interactive)
  (find-file-other-frame (file-name-sans-versions (dired-get-filename nil 'NO-ERROR) t)))

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
                      (setq file  (dired-get-filename nil 'NO-ERROR))))
    (unless (stringp file) (error "No file here"))
    (select-window (posn-window (event-end event)))
    (find-file-other-window (file-name-sans-versions file t))))

;;;###autoload
(defun diredp-mouse-view-file (event)   ; Not bound
  "Examine this file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive "e")
  (let (file)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion (goto-char (posn-point (event-end event)))
                      (setq file  (dired-get-filename nil 'NO-ERROR))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)  (dired-goto-subdir file))  (dired file))
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
                                                   (dired-get-filename t 'NO-ERROR))))
        (mouse-pos  (event-start event)))
    (require 'diff)
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (let ((file2  (read-file-name (format "Diff %s with: %s"
                                          (dired-get-filename t)
                                          (if default (concat "(default " default ") ") ""))
                                  (dired-current-directory) default t)))
      (setq switches  (and current-prefix-arg
                           (if (fboundp 'icicle-read-string-completing) ; In `icicles-fn.el'
                               (icicle-read-string-completing "Options for diff: "
                                                              (if (stringp diff-switches)
                                                                  diff-switches
                                                                (mapconcat #'identity diff-switches " "))
                                                              (lambda (c)
                                                                (diredp-string-match-p "switches"
                                                                                       (symbol-name c))))
                             (read-string "Options for diff: " (if (stringp diff-switches)
                                                                   diff-switches
                                                                 (mapconcat #'identity diff-switches " "))))))
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
                         (if (fboundp 'icicle-read-string-completing) ; In `icicles-fn.el'
                             (icicle-read-string-completing "Options for diff: "
                                                            (if (stringp diff-switches)
                                                                diff-switches
                                                              (mapconcat #'identity diff-switches " "))
                                                            (lambda (c)
                                                              (diredp-string-match-p "switches"
                                                                                     (symbol-name c))))
                           (read-string "Options for diff: " (if (stringp diff-switches)
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
  (if (and (cdr dired-subdir-alist)  (dired-get-subdir))
      (save-excursion (dired-mark-subdir-files))
    (let ((buffer-read-only  nil))
      (dired-repeat-over-lines 1 #'(lambda () (delete-char 1) (insert dired-marker-char)))
      (diredp-previous-line 1))))

;;;###autoload
(defun diredp-mouse-unmark (event)      ; Not bound
  "In Dired, unmark this file.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char  ?\040)) (dired-mark nil))
  (diredp-previous-line 1))

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
                                         (and (not (eobp))  (dired-get-filename nil 'NO-ERROR))))))
    ;; Return nil iff not on a file or directory name.
    (and file/dir-name  (cond ((dired-file-marker file/dir-name)
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
            (equal dired-marker-char (dired-file-marker (dired-get-filename nil 'NO-ERROR))))
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
  (diredp-previous-line 1))

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
  (diredp-internal-do-deletions (dired-map-over-marks (cons (dired-get-filename) (point)) 1)
                                1
                                'USE-TRASH-CAN) ; This arg is for Emacs 24+ only.
  (diredp-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-shell-command (event) ; Not bound
  "Run a shell COMMAND on this file.
If there is output, it goes to a separate buffer.

No automatic redisplay of Dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir."
  ;;Functions dired-run-shell-command and dired-shell-stuff-it do the
  ;;actual work and can be redefined for customization.
  (interactive "e")
  (lexical-let ((mouse-pos  (event-start event))
                (command    (dired-read-shell-command "! on %s: " nil (dired-get-marked-files t nil))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-bunch-files (- 10000 (length command))
                       (lambda (&rest files) (dired-run-shell-command (dired-shell-stuff-it command files t 1)))
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
         (command  (dired-mark-read-string
                    "Print %s with: " (apply 'concat lpr-command " " lpr-switches) 'print 1 (list file))))
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
  (diredp-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-byte-compile (event) ; Not bound
  "Byte compile this file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-byte-compile 1 'byte-compile t))
  (diredp-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-load (event)     ; Not bound
  "Load this Emacs Lisp file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-load 1 'load t))
  (diredp-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chmod (event)    ; Not bound
  "Change the mode of this file.
This calls chmod, so symbolic modes like `g+w' are allowed."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Mode" "chmod" 'chmod 1)
  (diredp-previous-line 1))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-mouse-do-chgrp (event)  ; Not bound
    "Change the group of this file."
    (interactive "e")
    (let ((mouse-pos  (event-start event)))
      (select-window (posn-window mouse-pos))
      (goto-char (posn-point mouse-pos)))
    (dired-do-chxxx "Group" "chgrp" 'chgrp 1)
    (diredp-previous-line 1)))

(unless (memq system-type '(windows-nt ms-dos))
  (defun diredp-mouse-do-chown (event)  ; Not bound
    "Change the owner of this file."
    (interactive "e")
    (let ((mouse-pos  (event-start event)))
      (select-window (posn-window mouse-pos))
      (goto-char (posn-point mouse-pos)))
    (dired-do-chxxx "Owner" dired-chown-program 'chown 1)
    (diredp-previous-line 1)))


;;; Breadcrumbs

(when (fboundp 'define-minor-mode)      ; Emacs 22+

  ;; Macro `define-minor-mode' is not defined in Emacs 20, so in order to be able to byte-compile
  ;; this file in Emacs 20, prohibit byte-compiling of the `define-minor-mode' call.
  ;;
  (eval '(define-minor-mode diredp-breadcrumbs-in-header-line-mode
          "Toggle the use of breadcrumbs in Dired header line.
With arg, show breadcrumbs iff arg is positive."
          :init-value nil :group 'header-line :group 'Dired-Plus
          (diredp-ensure-mode)
          (if diredp-breadcrumbs-in-header-line-mode
              (diredp-set-header-line-breadcrumbs)
            (setq header-line-format  (default-value 'header-line-format)))))

  (defun diredp-set-header-line-breadcrumbs ()
    "Show a header line with breadcrumbs to parent directories."
    (let ((parent  (diredp-parent-dir default-directory))
          (dirs    ())
          (text    ""))
      (while parent
        (push parent dirs)
        (setq parent  (diredp-parent-dir parent)))
      (dolist (dir  dirs)
        (let* ((crumbs-map    (make-sparse-keymap))
               (menu-map      (make-sparse-keymap "Breadcrumbs in Header Line"))
               ;; The next three are for showing the root as absolute and the rest as relative.
               (rootp         (diredp-root-directory-p dir))
               (parent-rootp  (and (not rootp)  (diredp-root-directory-p (diredp-parent-dir dir))))
               (rdir          dir))
          ;; (define-key crumbs-map [header-line mouse-3] menu-map)
          (unless rootp (setq rdir  (file-name-nondirectory (directory-file-name dir))))
          (when dir
            (setq rdir  (propertize rdir
                                    'local-map (progn (define-key crumbs-map [header-line mouse-1]
                                                        `(lambda () (interactive)
                                                          (dired ,dir dired-actual-switches)))
                                                      (define-key crumbs-map [header-line mouse-2]
                                                        `(lambda () (interactive)
                                                          (dired-other-window ,dir dired-actual-switches)))
                                                      crumbs-map)
                                    'mouse-face 'mode-line-highlight
                                    ;;'help-echo "mouse-1: Dired; mouse-2: Dired in other window; mouse-3: Menu"))
                                    'help-echo "mouse-1: Dired; mouse-2: Dired in other window"))
            (setq text  (concat text (if (or rootp  parent-rootp) " "  " / ") rdir)))))
      (make-local-variable 'header-line-format)
      (setq header-line-format  text)))

  ;; Users can do this.
  ;;
  ;; (add-hook 'dired-before-readin-hook 'diredp-breadcrumbs-in-header-line-mode)

  )


;;; `Dired+' Help

;;;###autoload
(defun diredp-describe-mode (&optional buffer)
  "Describe Dired mode, including Dired+ features.
This is `describe-mode' plus a description of Dired+ features.
For just the latter, use \\<dired-mode-map>`\\[diredp-dired-plus-help]'."
  (interactive "@")
  (unless (derived-mode-p 'dired-mode)
    (error "Use `diredp-dired-plus-help' if you want information about Dired+"))
  (with-current-buffer (or buffer  (current-buffer)) (describe-mode))
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
  (diredp-with-help-window "*Help*" (diredp-dired-plus-description+links)))

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
           (require 'help-mode nil t)
           (get 'help-xref 'button-category-symbol)) ; `button.el'
  (define-button-type 'diredp-help-button
      :supertype 'help-xref
      'help-function #'(lambda () (browse-url "https://www.emacswiki.org/emacs/DiredPlus"))
      'help-echo
      (purecopy "mouse-2, RET: Dired+ documentation on the Emacs Wiki (requires \
Internet access)")))

(defun diredp-dired-plus-help-link ()
  "Add Web link for Dired+ help, and reminder about sending bug report."
  ;; Don't bother to do this for Emacs 21.3.  Its `help-insert-xref-button' is different.
  (when (and (> emacs-major-version 21)
             (require 'help-mode nil t)
             (fboundp 'help-insert-xref-button)) ; `help-mode.el'.
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

To see or customize the  Dired+ options or faces, use
`M-x customize-option diredp TAB' or `M-x customize-face diredp TAB'.

Most keys listed here are in addition to those for vanilla Dired.

Menus
-----

Many Dired+ actions are available from the menu-bar menus and the
`mouse-3' context menu.  This may include commands shown here as not
being bound to keys (i.e., listed as `M-x ...').

General Here
------------

"
    (and (fboundp 'diredp-w32-drives)
         "  \\[diredp-w32-drives]\t\t- Go up to a list of MS Windows drives
")
    (and (fboundp 'dired-hide-details-mode)
         "  \\[dired-hide-details-mode]\t\t- Hide/show details
")

    "  \\[revert-buffer]\t\t- Refresh (sync and show all)
  \\[diredp-toggle-find-file-reuse-dir]\t- Toggle reusing directories
"
    "  \\[diredp-marked-other-window]\t\t- Open Dired on marked files here
  \\[diredp-dired-inserted-subdirs]\t\t- Dired separately each subdir inserted here
"
    (and (featurep 'bookmark+)
         "  \\[diredp-highlight-autofiles-mode]\t- Toggle autofile highlighting

")

    "General Globally
----------------

\\<global-map>\
  \\[diredp-add-to-dired-buffer]\t- Add files to a Dired buffer
  \\[diredp-fileset]\t- Open Dired on files in a fileset
  \\[diredp-dired-recent-files]\t- Open Dired on recently used files and dirs
  \\[diredp-dired-recent-dirs]\t- Open Dired on recently used dirs
  \\[diredp-dired-union]\t- Create union of some Dired buffers
  \\[diredp-dired-for-files]\t- Open Dired on files located anywhere
\\<dired-mode-map>\

Mouse
-----

  \\[diredp-mouse-3-menu]\t- Context-sensitive menu
"

    (and (where-is-internal 'diredp-mouse-describe-file dired-mode-map)
         "  \\[diredp-mouse-describe-file]\t- Describe file
")

    (and (where-is-internal 'diredp-mouse-describe-autofile dired-mode-map)
         "  \\[diredp-mouse-describe-autofile]\t- Describe autofile
")

    "  \\[diredp-mouse-mark-region-files]\t\t- Mark all in region
"

    (and (fboundp 'dired-mouse-w32-browser) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser dired-mode-map)
         "  \\[dired-mouse-w32-browser]\t\t- MS Windows `Open' action
")
    (and (fboundp 'dired-mouse-w32-browser-reuse-dir-buffer) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser-reuse-dir-buffer dired-mode-map)
         "  \\[dired-mouse-w32-browser-reuse-dir-buffer]\t- MS Windows `Open' action
")

    (and (where-is-internal 'dired-mouse-find-file dired-mode-map)
         "  \\[dired-mouse-find-file]\t- Open in this window
")
    (and (where-is-internal 'diredp-mouse-find-file-reuse-dir-buffer dired-mode-map)
         "  \\[diredp-mouse-find-file-reuse-dir-buffer]\t- Open in this window
")

    (and (where-is-internal 'dired-mouse-find-file-other-window dired-mode-map)
         "  \\[dired-mouse-find-file-other-window]\t\t- Open in another window
")

    "  \\[diredp-mouse-find-file-other-frame]\t\t- Open in another frame
"

    "
Marking
-------

  \\[diredp-mark]\t\t- Mark this line or those in region or subdir listing
  \t\t  Prefix arg (region/subdir): mark with a given char
  \\[dired-unmark]\t\t- Unmark this line
  \\[dired-toggle-marks]\t\t- Toggle marked/unmarked
  \\[dired-mark-sexp]\t\t- Mark all satisfying a predicate
  \\[dired-unmark-all-marks]\t\t- Unmark all
  \\[diredp-mark/unmark-extension]\t\t- Mark/unmark all that have a given extension
  \\[dired-mark-files-regexp]\t\t- Mark/unmark files with names matching a regexp
  \\[dired-mark-files-containing-regexp]\t\t- Mark/unmark files containing a regexp match
"

    (and (fboundp 'dired-mark-omitted)  ; In `dired-x.el' Emacs 22+.
         "  \\[dired-mark-omitted]\t\t- Mark omitted
")

    "  \\[diredp-mark-files-tagged-regexp]\t\t- Mark those with a tag that matches a regexp
  \\[diredp-unmark-files-tagged-regexp]\t\t- Unmark those with a tag that matches a regexp
  \\[diredp-mark-files-tagged-all]\t\t- Mark those with all of the given tags
  \\[diredp-unmark-files-tagged-all]\t\t- Unmark those with all of the given tags
  \\[diredp-mark-files-tagged-some]\t\t- Mark those with some of the given tags
  \\[diredp-unmark-files-tagged-some]\t\t- Unmark those with some of the given tags
  \\[diredp-mark-files-tagged-not-all]\t- Mark those without some of the given tags
  \\[diredp-unmark-files-tagged-not-all]\t- Unmark those without some of the given tags
  \\[diredp-mark-files-tagged-none]\t- Mark those with none of the given tags
  \\[diredp-unmark-files-tagged-none]\t- Unmark those with none of the given tags

  \\[diredp-save-markings]\t\t- Save all markings (not just `*')
  \\[diredp-restore-markings]\t\t- Restore markings last saved with `diredp-save-markings'
"

    "
Current file/subdir (current line)
----------------------------------

  \\[diredp-describe-file]\t- Describe
  \\[dired-find-file]\t\t- Open
"
    (and (fboundp 'dired-mouse-w32-browser) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser dired-mode-map)
         "  \\[dired-mouse-w32-browser]\t- MS Windows `Open' action
  \\[dired-w32explore]\t- MS Windows Explorer
")

    "  \\[diredp-byte-compile-this-file]\t\t- Byte-compile
  \\[diredp-compress-this-file]\t\t- Compress/uncompress
  \\[diredp-print-this-file]\t\t- Print
  \\[diredp-relsymlink-this-file]\t\t- Create relative symlink
  \\[diredp-delete-this-file]\t\t- Delete (with confirmation)
  \\[diredp-rename-this-file]\t\t- Rename
  \\[diredp-capitalize-this-file]\t\t- Capitalize (rename)
  \\[diredp-upcase-this-file]\t\t- Rename to uppercase
  \\[diredp-downcase-this-file]\t\t- Rename to lowercase
  \\[diredp-ediff]\t\t- Ediff
  \\[diredp-bookmark-this-file]\t\t- Bookmark
"
    (and (featurep 'bookmark+)
         "  \\[diredp-tag-this-file]\t\t- Add some tags to this file/dir
  \\[diredp-untag-this-file]\t\t- Remove some tags from this file/dir
  \\[diredp-remove-all-tags-this-file]\t\t- Remove all tags from this file/dir
  \\[diredp-copy-tags-this-file]\t\t- Copy the tags from this file/dir
  \\[diredp-paste-add-tags-this-file]\t\t- Paste (add) copied tags to this file/dir
  \\[diredp-paste-replace-tags-this-file]\t\t- Paste (replace) tags for this file/dir
  \\[diredp-set-tag-value-this-file]\t\t- Set a tag value for this file/dir
")

    (and (fboundp 'dired-mouse-w32-browser-reuse-dir-buffer) ; In `w32-browser.el'.
         (where-is-internal 'dired-mouse-w32-browser-reuse-dir-buffer dired-mode-map)
         "  \\[dired-mouse-w32-browser-reuse-dir-buffer]\t- MS Windows `Open' action
  \\[dired-w32explore]\t- MS Windows Explorer
")

    "
Marked (or next prefix arg) files & subdirs here
------------------------------------------------
"
    (and (fboundp 'dired-multiple-w32-browser) ; In `w32-browser.el'.
         "
  \\[dired-multiple-w32-browser]\t- MS Windows `Open' action
")


    "  \\[diredp-marked-other-window]\t\t- Dired marked files and directories
  \\[diredp-list-marked]\t\t- List marked files and directories
  \\[diredp-insert-subdirs]\t\t- Insert marked subdirectories

  \\[dired-copy-filename-as-kill]\t\t- Copy names for pasting
  M-0 \\[dired-copy-filename-as-kill]\t\t- Copy absolute names for pasting
  \\[diredp-yank-files]\t\t- Paste files whose absolute names you copied
  \\[dired-do-find-marked-files]\t\t- Visit
  \\[dired-do-print]\t\t- Print
  \\[dired-do-copy]\t\t- Copy
  \\[dired-do-rename]\t\t- Rename/move
  \\[dired-do-touch]\t- Touch (update timestamp)
  \\[dired-do-chmod]\t\t- Change mode

  \\[diredp-do-grep]\t\t- Run `grep'
  \\[dired-do-search]\t\t- Search
"
    (and (fboundp 'dired-do-find-regexp) ; Emacs 25+
         "  \\[dired-do-find-regexp]\t\t- Search using `find'
")

    (if (fboundp 'dired-do-query-replace-regexp) ; Emacs 22+
        "  \\[dired-do-query-replace-regexp]\t\t- Query-replace
"
      "  \\[dired-do-query-replace]\t\t- Query-replace
")

    (and (fboundp 'dired-do-find-regexp-and-replace)
         "  \\[dired-do-find-regexp-and-replace]\t\t- Query-replace using `find'
")

    (and (fboundp 'dired-do-isearch)
         "  \\[dired-do-isearch]\t- Isearch
  \\[dired-do-isearch-regexp]\t- Regexp isearch
")

    "  \\[dired-do-compress]\t\t- Compress
  \\[dired-do-byte-compile]\t\t- Byte-compile
  \\[dired-do-load]\t\t- Load (Emacs Lisp)
"

    (and (fboundp 'dired-do-async-shell-command)
         "  \\[dired-do-async-shell-command]\t\t- Run shell command asynchronously
")

    "  \\[dired-do-shell-command]\t\t- Run shell command

  \\[diredp-do-apply-to-marked]\t\t- Apply Lisp function to file or dir name
  \\[diredp-do-command-in-marked]\t\t- Invoke command or keyboard macro in file or dir
  \\[diredp-do-eval-in-marked]\t\t- Eval Lisp sexp in file or dir

  \\[diredp-omit-marked]\t- Omit
  \\[diredp-omit-unmarked]\t- Omit unmarked
  \\[diredp-do-add-to-recentf]\t\t- Add to recently visited
  \\[diredp-do-remove-from-recentf]\t- Remove from recently visited
"

    (and (featurep 'bookmark+)
         "
  \\[diredp-do-tag]\t\t- Add some tags to marked
  \\[diredp-do-untag]\t\t- Remove some tags from marked
  \\[diredp-do-remove-all-tags]\t\t- Remove all tags from marked
  \\[diredp-do-paste-add-tags]\t- Paste (add) copied tags to marked
  \\[diredp-do-paste-replace-tags]\t\t- Paste (replace) tags for marked
  \\[diredp-do-set-tag-value]\t\t- Set a tag value for marked
  \\[diredp-mark-files-tagged-regexp]\t\t- Mark those with a tag that matches a regexp
  \\[diredp-mark-files-tagged-all]\t\t- Mark those with all of the given tags
  \\[diredp-mark-files-tagged-some]\t\t- Mark those with some of the given tags
  \\[diredp-mark-files-tagged-not-all]\t- Mark those without some of the given tags
  \\[diredp-mark-files-tagged-none]\t- Mark those with none of the given tags
  \\[diredp-unmark-files-tagged-regexp]\t\t- Unmark those with a tag that matches a regexp
  \\[diredp-unmark-files-tagged-all]\t\t- Unmark those with all of the given tags
  \\[diredp-unmark-files-tagged-some]\t\t- Unmark those with some of the given tags
  \\[diredp-unmark-files-tagged-not-all]\t- Unmark those without some of the given tags
  \\[diredp-unmark-files-tagged-none]\t- Unmark those with none of the given tags")

    "

  \\[diredp-do-bookmark]\t\t- Bookmark
"

    (and (featurep 'bookmark+)
         "  \\[diredp-set-bookmark-file-bookmark-for-marked]\t\t- \
Bookmark and create bookmark-file bookmark
  \\[diredp-do-bookmark-in-bookmark-file]\t- Bookmark in specific bookmark file
")

    "
Here and below (in marked subdirs)
----------------------------------
"
    (and (fboundp 'dired-multiple-w32-browser) ; In `w32-browser.el'.
         "
  \\[diredp-multiple-w32-browser-recursive]\t- MS Windows `Open' action
")

    "  \\[diredp-marked-recursive-other-window]\t\t- Dired
  \\[diredp-list-marked-recursive]\t\t- List marked files and directories
  \\[diredp-insert-subdirs-recursive]\t\t- Insert marked subdirectories

  \\[diredp-copy-filename-as-kill-recursive]\t\t- Copy names for pasting
  \\[diredp-do-find-marked-files-recursive]\t\t\t- Visit
  \\[diredp-do-print-recursive]\t\t\t- Print
  \\[diredp-do-copy-recursive]\t\t\t- Copy
  \\[diredp-do-move-recursive]\t\t\t- Move
  \\[diredp-do-touch-recursive]\t\t- Touch (update timestamp)
  \\[diredp-do-chmod-recursive]\t\t\t- Change mode

  \\[diredp-do-symlink-recursive]\t\t\t- Add symbolic links
  \\[diredp-do-relsymlink-recursive]\t\t\t- Add relative symbolic links
  \\[diredp-do-hardlink-recursive]\t\t\t- Add hard links

  \\[diredp-capitalize-recursive]\t\t- Capitalize
  \\[diredp-downcase-recursive]\t\t- Downcase
  \\[diredp-upcase-recursive]\t\t- Upcase
"
  (and (fboundp 'epa-dired-do-encrypt)   ; Emacs 23+
       "
  \\[diredp-do-encrypt-recursive]\t\t- Encrypt
  \\[diredp-do-decrypt-recursive]\t\t- Decrypt
  \\[diredp-do-sign-recursive]\t\t- Sign
  \\[diredp-do-verify-recursive]\t\t- Verify
")

  "
  \\[diredp-do-grep-recursive]\t\t- `grep'
  \\[diredp-do-search-recursive]\t\t\t- Search
  \\[diredp-do-query-replace-regexp-recursive]\t\t\t- Query-replace
  \\[diredp-do-isearch-recursive]\t\t- Isearch
  \\[diredp-do-isearch-regexp-recursive]\t- Regexp isearch
"
    (and (fboundp 'diredp-do-async-shell-command-recursive) ; Emacs 23+
         "
  \\[diredp-do-async-shell-command-recursive]\t\t\t- Run shell command asynchronously
")

    "  \\[diredp-do-shell-command-recursive]\t\t\t- Run shell command

  \\[diredp-do-apply-to-marked-recursive]\t\t\t- Apply Lisp function to file or dir name
  \\[diredp-do-command-in-marked-recursive]\t\t- Invoke command or keyboard macro in file/dir
  \\[diredp-do-eval-in-marked-recursive]\t\t\t- Eval Lisp sexp in file or dir

  \\[diredp-image-dired-comment-files-recursive]\t\t- Add image comment
  \\[diredp-image-dired-display-thumbs-recursive]\t\t- Show thumbnail images
  \\[diredp-image-dired-tag-files-recursive]\t\t- Tag images
  \\[diredp-image-dired-delete-tag-recursive]\t\t- Delete image tags

  \\[diredp-do-bookmark-recursive]\t\t- Bookmark
"
    (and (featurep 'bookmark+)
         "  \\[diredp-do-bookmark-in-bookmark-file-recursive]\t\t- Bookmark in bookmark file
  \\[diredp-set-bookmark-file-bookmark-for-marked-recursive]\t\t- Create bookmark-file bookmark
")

    "
  \\[diredp-mark-directories-recursive]\t\t- Mark directories
  \\[diredp-mark-executables-recursive]\t\t- Mark executables
  \\[diredp-mark-symlinks-recursive]\t\t- Mark symbolic links
  \\[diredp-mark-files-containing-regexp-recursive]\t\t- Mark content regexp matches
  \\[diredp-mark-files-regexp-recursive]\t\t- Mark filename regexp matches
"
    (and (featurep 'bookmark+)
         "  \\[diredp-mark-autofiles-recursive]\t\t- Mark autofiles
")
    "  \\[diredp-flag-auto-save-files-recursive]\t\t\t- Flag auto-save
  \\[diredp-do-delete-recursive]\t\t\t- Delete marked (not flagged)
  \\[diredp-change-marks-recursive]\t\t- Change marks
  \\[diredp-unmark-all-files-recursive]\t\t- Remove a given mark
  \\[diredp-unmark-all-marks-recursive]\t\t\t- Remove all marks
"
    (and (featurep 'bookmark+)
"

Tagging
-------

  \\[diredp-tag-this-file]\t\t- Add some tags to this file/dir
  \\[diredp-untag-this-file]\t\t- Remove some tags from this file/dir
  \\[diredp-remove-all-tags-this-file]\t\t- Remove all tags from this file/dir
  \\[diredp-copy-tags-this-file]\t\t- Copy the tags from this file/dir
  \\[diredp-paste-add-tags-this-file]\t\t- Paste (add) copied tags to this file/dir
  \\[diredp-paste-replace-tags-this-file]\t\t- Paste (replace) tags for this file/dir
  \\[diredp-set-tag-value-this-file]\t\t- Set a tag value for this file/dir
  \\[diredp-do-tag]\t\t- Add some tags to marked
  \\[diredp-do-untag]\t\t- Remove some tags from marked
  \\[diredp-do-remove-all-tags]\t\t- Remove all tags from marked
  \\[diredp-do-paste-add-tags]\t- Paste (add) copied tags to marked
  \\[diredp-do-paste-replace-tags]\t\t- Paste (replace) tags for marked
  \\[diredp-do-set-tag-value]\t\t- Set a tag value for marked
  \\[diredp-mark-files-tagged-regexp]\t\t- Mark those with a tag that matches a regexp
  \\[diredp-mark-files-tagged-all]\t\t- Mark those with all of the given tags
  \\[diredp-mark-files-tagged-some]\t\t- Mark those with some of the given tags
  \\[diredp-mark-files-tagged-not-all]\t- Mark those without some of the given tags
  \\[diredp-mark-files-tagged-none]\t- Mark those with none of the given tags
  \\[diredp-unmark-files-tagged-regexp]\t\t- Unmark those with a tag that matches a regexp
  \\[diredp-unmark-files-tagged-all]\t\t- Unmark those with all of the given tags
  \\[diredp-unmark-files-tagged-some]\t\t- Unmark those with some of the given tags
  \\[diredp-unmark-files-tagged-not-all]\t- Unmark those without some of the given tags
  \\[diredp-unmark-files-tagged-none]\t- Unmark those with none of the given tags
")

    "
Bookmarking
-----------

  \\[diredp-bookmark-this-file]\t\t- Bookmark this file/dir
  \\[diredp-do-bookmark]\t\t- Bookmark marked"

    (and (featurep 'bookmark+)
         "
  \\[diredp-set-bookmark-file-bookmark-for-marked]\t\t- \
Bookmark marked and create bookmark-file bookmark
  \\[diredp-do-bookmark-in-bookmark-file]\t- Bookmark marked, in specific bookmark file
")

    "  \\[diredp-do-bookmark-recursive]\t- Bookmark marked, here and below
"
    (and (featurep 'bookmark+)
         "  \\[diredp-do-bookmark-in-bookmark-file-recursive]\t- \
Bookmark marked, here and below, in specific file
  \\[diredp-set-bookmark-file-bookmark-for-marked-recursive]\t- \
Set bookmark-file bookmark for marked here and below
")

    )))

(when (> emacs-major-version 21)
  (defun diredp-nb-marked-in-mode-name ()
    "Show number of marked, flagged, and current-list lines in mode-line.
\(Flagged means flagged for deletion.)
If the current line is marked/flagged and there are others
marked/flagged after it then show `N/M', where `N' is the number
marked/flagged through the current line and `M' is the total number
marked/flagged.

If the current line is for a file then show `L/T', where `L' is the
line number in the current listing and `T' is the number of files in
that listing.  If option `diredp-count-.-and-..-flag' is non-nil then
count also `.' and `..'.

Also abbreviate `mode-name', using \"Dired/\" instead of \"Dired by\"."
    (let ((mname  (format-mode-line mode-name)))
      ;; Property `dired+-mode-name' indicates whether `mode-name' has been changed.
      (unless (get-text-property 0 'dired+-mode-name mname)
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
                                        (if (diredp-looking-at-p (concat marked-regexp ".*"))
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
                                        (if (diredp-looking-at-p (concat flagged-regexp ".*"))
                                            (format "%d/" (1+ (count-matches
                                                               flagged-regexp
                                                               (point-min) (point))))
                                          ""))
                                      nb-flagged)
                              'face 'diredp-mode-line-flagged))))
                  (:eval (save-excursion
                           (let ((this   0)
                                 (total  0)
                                 (o-pt   (line-beginning-position))
                                 (e-pt   (or (condition-case nil
                                                 (let ((diredp-wrap-around-flag  nil))
                                                   (save-excursion
                                                     (diredp-next-subdir 1)
                                                     (line-beginning-position)))
                                               (error nil))
                                             (save-excursion (goto-char (point-max)) (line-beginning-position)))))
                             (when dired-subdir-alist (dired-goto-subdir (dired-current-directory)))
                             (while (and (<= (point) e-pt)
                                         (< (point) (point-max))) ; Hack to work around Emacs display-engine bug.
                               (when (condition-case nil
                                         (dired-get-filename nil diredp-count-.-and-..-flag)
                                       (error nil))
                                 (when (<= (line-beginning-position) o-pt) (setq this  (1+ this)))
                                 (setq total  (1+ total)))
                               (forward-line 1))
                             (if (not (> this 0)) (format " %d" total) (format " %d/%d" this total)))))))))))

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

(defun diredp-visit-ignore-regexp ()    ; Taken from `image-file-name-regexp'.
  "Return a regular expression matching file names to skip.
This is used by `diredp-visit-(next|previous)'."
  (let ((exts-regexp  (and diredp-visit-ignore-extensions
                           (concat "\\." (regexp-opt (nconc (mapcar #'upcase diredp-visit-ignore-extensions)
                                                            diredp-visit-ignore-extensions)
                                                     t)
                                   "\\'"))))
    (if diredp-visit-ignore-regexps
        (mapconcat #'identity (if exts-regexp
                                  (cons exts-regexp diredp-visit-ignore-regexps)
                                diredp-visit-ignore-regexps)
                   "\\|")
      exts-regexp)))

;;;###autoload
(defun diredp-visit-next-file (&optional arg) ; Bound to `C-down'
  "Move down a line and visit its file in another window.
With numeric prefix arg N, move down N-1 lines first.

After moving N lines, skip any lines with file names that match either
`diredp-visit-ignore-extensions' or `diredp-visit-ignore-regexps'.

Kill the last buffer visited by a `diredp-visit-*' command."
  (interactive "p")
  (dired-next-line arg)
  (while (diredp-string-match-p (diredp-visit-ignore-regexp) (dired-get-file-for-visit))
    (dired-next-line 1))
  (diredp-visit-this-file))

;;;###autoload
(defun diredp-visit-previous-file (&optional arg) ; Bound to `C-up'
  "Move up a line and visit its file in another window.
With numeric prefix arg N, move up N-1 lines first.

After moving N lines, skip any lines with file names that match either
`diredp-visit-ignore-extensions' or `diredp-visit-ignore-regexps'.

Kill the last buffer visited by a `diredp-visit-*' command."
  (interactive "p")
  (dired-previous-line arg)
  (while (diredp-string-match-p (diredp-visit-ignore-regexp) (dired-get-file-for-visit))
    (dired-previous-line 1))
  (diredp-visit-this-file))

;;;###autoload
(defun diredp-visit-this-file ()        ; Bound to `e' (replaces `dired-find-file' binding)
  "View the file on this line in another window in the same frame.
If it was not already shown there then kill the previous buffer
visited by a `diredp-visit-*' command.

If it was already shown there, and if it and Dired are the only
windows there, then delete its window (toggle : show/hide the file)."
  (interactive)
  (let ((file   (dired-get-file-for-visit))
        (obuf   (current-buffer))
        (shown  nil)
        fwin)
    (unless (or (and (fboundp 'window-parent)  (window-parent))
                (not (one-window-p 'NOMINI)))
      (split-window))
    (save-selected-window
      (other-window 1)
      (setq fwin  (selected-window))
      (unless (or (setq shown  (or (equal (current-buffer) (get-file-buffer file))
                                   (memq (current-buffer) (dired-buffers-for-dir file))))
                  (equal obuf (current-buffer)))
        (kill-buffer (current-buffer))))
    (if shown
        (when (= 2 (count-windows 'NOMINI)) (delete-window fwin))
      (set-window-buffer fwin (find-file-noselect file)))))
 
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
(defvar diredp-menu-bar-single-menu (make-sparse-keymap "Single"))
(define-key dired-mode-map [menu-bar immediate] (cons "Single" diredp-menu-bar-single-menu))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-immediate-menu 'diredp-menu-bar-single-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-immediate-menu 'diredp-menu-bar-single-menu "2017-04-09")

(if (fboundp 'diredp-describe-file)
    (define-key diredp-menu-bar-single-menu [diredp-describe-file]
      '(menu-item "Describe" diredp-describe-file
        :help "Describe the file or directory at cursor"))
  (define-key diredp-menu-bar-single-menu [diredp-describe-autofile]
    '(menu-item "Describe" diredp-describe-autofile
      :help "Describe the autofile at cursor"
      :enable (featurep 'bookmark+))))
(define-key diredp-menu-bar-single-menu [separator-describe] '("--")) ; ---------------------

(when (fboundp 'diredp-chown-this-file)
  (define-key diredp-menu-bar-single-menu [chown]
    '(menu-item "Change Owner..." diredp-chown-this-file
      :help "Change the owner of file at cursor")))
(when (fboundp 'diredp-chgrp-this-file)
  (define-key diredp-menu-bar-single-menu [chgrp]
    '(menu-item "Change Group..." diredp-chgrp-this-file
      :help "Change the group of file at cursor")))
(define-key diredp-menu-bar-single-menu [chmod]
  '(menu-item "Change Mode..." diredp-chmod-this-file
    :help "Change mode (attributes) of file at cursor"))
(when (fboundp 'dired-do-touch)         ; Emacs 22+
  (define-key diredp-menu-bar-single-menu [touch]
    '(menu-item "Change Timestamp (`touch')..." diredp-touch-this-file
      :help "Change the timestamp of file at cursor, using `touch'")))
(define-key diredp-menu-bar-single-menu [separator-change] '("--")) ; -----------------------

(define-key diredp-menu-bar-single-menu [print]
  '(menu-item "Print..." diredp-print-this-file
    :help "Print file at cursor, supplying print command"))
(define-key diredp-menu-bar-single-menu [grep]
  '(menu-item "Grep..." diredp-grep-this-file :help "Grep file at cursor"))
(define-key diredp-menu-bar-single-menu [compress]
  '(menu-item "Compress/Uncompress" diredp-compress-this-file
    :help "Compress/uncompress file at cursor"))
(define-key diredp-menu-bar-single-menu [command]
  '(menu-item "Shell Command..." diredp-shell-command-this-file
    :help "Run a shell command on file at cursor"))
(define-key diredp-menu-bar-single-menu [diredp-async-shell-command-this-file]
  '(menu-item "Asynchronous Shell Command..." diredp-async-shell-command-this-file
    :help "Run a shell command asynchronously on file at cursor"))
(define-key diredp-menu-bar-single-menu [compile]
  '(menu-item "Byte Compile" diredp-byte-compile-this-file
    :help "Byte-compile this Emacs Lisp file"))
(define-key diredp-menu-bar-single-menu [load]
  '(menu-item "Load" diredp-load-this-file
    :help "Load this Emacs Lisp file"))

(when (fboundp 'mkhtml-dired-files)     ; In `mkhtml.el'.
  (define-key diredp-menu-bar-single-menu [mkhtml-dired-files]
    '(menu-item "Create HTML" mkhtml-dired-files
      :help "Create an HTML file corresponding to file at cursor")))
(define-key diredp-menu-bar-single-menu [separator-misc] '("--")) ; -------------------------

(define-key diredp-menu-bar-single-menu [delete]
  '(menu-item "Delete" diredp-delete-this-file :help "Delete file at cursor"))
(define-key diredp-menu-bar-single-menu [separator-delete] '("--")) ; -----------------------

(define-key diredp-menu-bar-single-menu [backup-diff]
  '(menu-item "Diff with Backup" dired-backup-diff
    :help "Diff file at cursor with its latest backup"))
(define-key diredp-menu-bar-single-menu [diff]
  '(menu-item "Diff..." dired-diff
    :help "Compare file at cursor with another file using `diff'"))
(define-key diredp-menu-bar-single-menu [ediff]
  '(menu-item "Compare..." diredp-ediff :help "Compare file at cursor with another file"))
(define-key diredp-menu-bar-single-menu [separator-diff] '("--")) ; -------------------------

(define-key diredp-menu-bar-single-menu [diredp-kill-this-tree]
  '(menu-item "Remove This Inserted Subdir and Lower" diredp-kill-this-tree
    :visible (and (fboundp 'diredp-kill-this-tree)
              (not (equal
                    (expand-file-name (dired-current-directory))
                    (expand-file-name default-directory)))))) ; In subdir, not top.
(define-key diredp-menu-bar-single-menu [dired-kill-subdir]
  '(menu-item "Remove This Inserted Subdir" dired-kill-subdir
    :visible (not (equal (expand-file-name (dired-current-directory))
                         (expand-file-name default-directory))))) ; In subdir, not top.
(define-key diredp-menu-bar-single-menu [diredp-dired-this-subdir]
  '(menu-item "Dired This Inserted Subdir (Tear Off)"
    (lambda () (interactive) (diredp-dired-this-subdir t))
    :visible (and (cdr dired-subdir-alist) ; First is current dir.  Must have at least one more.
              (not (equal (expand-file-name (dired-current-directory))
                          (expand-file-name default-directory)))) ; Must be sub, not top.
    :help "Open Dired for subdir at or above point, tearing it off if inserted"))
(define-key diredp-menu-bar-single-menu [insert-subdir]
  '(menu-item "Insert This Subdir" dired-maybe-insert-subdir
    :visible (and (atom (diredp-this-subdir))
              (not (assoc (file-name-as-directory (diredp-this-subdir)) dired-subdir-alist)))
    :enable (atom (diredp-this-subdir))
    :help "Insert a listing of this subdirectory"))
(define-key diredp-menu-bar-single-menu [goto-subdir]
  '(menu-item "Go To Inserted Subdir" dired-maybe-insert-subdir
    :visible (and (atom (diredp-this-subdir))
              (assoc (file-name-as-directory (diredp-this-subdir)) dired-subdir-alist))
    :enable (atom (diredp-this-subdir))
    :help "Go to the inserted listing of this subdirectory"))
(define-key diredp-menu-bar-single-menu [separator-subdir] '("--" ; ------------------------
            :visible (or (atom (diredp-this-subdir)) ; Subdir line.
                         (not (equal (expand-file-name (dired-current-directory))
                                     (expand-file-name default-directory)))))) ; Not top.

(define-key diredp-menu-bar-single-menu [view]
  '(menu-item "View (Read Only)" dired-view-file
    :help "Examine file at cursor in read-only mode"))
(define-key diredp-menu-bar-single-menu [display]
  '(menu-item "Display in Other Window" dired-display-file
    :help "Display file at cursor in a different window"))


;; `Single' > `Open' menu.
;;
(defvar diredp-single-open-menu (make-sparse-keymap "Rename")
  "`Open' submenu for Dired menu-bar `Single' menu.")
(define-key diredp-menu-bar-single-menu [multiple-open] (cons "Open" diredp-single-open-menu))

;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(progn
    (define-key diredp-single-open-menu [dired-w32-browser]
      '(menu-item "Open Associated Windows App" dired-w32-browser
        :help "Open file using the Windows app associated with its file type"))
    (define-key diredp-single-open-menu [dired-w32explore]
      '(menu-item "Open in Windows Explorer" dired-w32explore
        :help "Open file in Windows Explorer"))))
(define-key diredp-single-open-menu [find-file-other-frame]
  '(menu-item "Open in Other Frame" diredp-find-file-other-frame
    :help "Edit file at cursor in a different frame"))
(define-key diredp-single-open-menu [find-file-other-window]
  '(menu-item "Open in Other Window" dired-find-file-other-window
    :help "Edit file at cursor in a different window"))
(define-key diredp-single-open-menu [find-file]
  '(menu-item "Open" dired-find-file :help "Edit file at cursor"))


;; `Single' > `Rename' menu.
;;
(defvar diredp-single-rename-menu (make-sparse-keymap "Rename")
  "`Rename' submenu for Dired menu-bar `Single' menu.")
(define-key diredp-menu-bar-single-menu [multiple-case] (cons "Rename" diredp-single-rename-menu))

(define-key diredp-single-rename-menu [single-rename-capitalize]
  '(menu-item "Capitalize" diredp-capitalize-this-file
    :help "Capitalize (initial caps) name of file at cursor"))
(define-key diredp-single-rename-menu [single-rename-downcase]
  '(menu-item "Downcase" diredp-downcase-this-file
    ;; When running on plain MS-DOS, there is only one letter-case for file names.
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename file at cursor to a lower-case name"))
(define-key diredp-single-rename-menu [single-rename-upcase]
  '(menu-item "Upcase" diredp-upcase-this-file
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename file at cursor to an upper-case name"))


;; `Single' > `Move / Copy / Link' menu.
;;
(defvar diredp-single-move-copy-link-menu (make-sparse-keymap "Move / Copy / Link")
  "`Move / Copy / Link' submenu for Dired menu-bar `Single' menu.")
(define-key diredp-menu-bar-single-menu [multiple-move-copy-link]
  (cons "Move / Copy / Link" diredp-single-move-copy-link-menu))

(define-key diredp-single-move-copy-link-menu [single-hardlink]
  '(menu-item "Hardlink to..." diredp-hardlink-this-file
    :help "Make hard links for current or marked files"))
(define-key diredp-single-move-copy-link-menu [single-symlink]
  '(menu-item "Symlink to (Absolute)..." diredp-symlink-this-file
              :help "Make absolute symbolic link for file at cursor"))
(define-key diredp-single-move-copy-link-menu [single-relsymlink]
  '(menu-item "Symlink to (Relative)..." diredp-relsymlink-this-file
              :help "Make relative symbolic link for file at cursor"))
(define-key diredp-single-move-copy-link-menu [single-copy]
  '(menu-item "Copy to..." diredp-copy-this-file :help "Copy file at cursor"))
(define-key diredp-single-move-copy-link-menu [single-rename]
  '(menu-item "Move/Rename to..." diredp-rename-this-file
    :help "Rename file at cursor, or move it to a different directory"))


;; `Single' > `Image' menu.
;;
(defvar diredp-single-image-menu (make-sparse-keymap "Image"))
(defalias 'diredp-single-image-menu diredp-single-image-menu)
(define-key diredp-menu-bar-single-menu [image]
  '(menu-item "Image" diredp-single-image-menu
    :enable (let ((img-file  (diredp-get-image-filename 'LOCALP 'NO-ERROR)))
              (and (fboundp 'image-dired-dired-display-image)  img-file))))

(define-key diredp-single-image-menu [diredp-image-dired-display-thumb]
  '(menu-item "Go To Thumbnail" diredp-image-dired-display-thumb
    :help "Pop to buffer showing the thumbnail of this image file"))
(define-key diredp-single-image-menu [diredp-image-dired-create-thumb]
  '(menu-item "Create Thumbnail" diredp-image-dired-create-thumb
    :help "Create a thumbnail image for this image file"))
(define-key diredp-single-image-menu [diredp-image-dired-edit-comment-and-tags]
  '(menu-item "Edit Comment and Tags..." diredp-image-dired-edit-comment-and-tags
    :help "Edit comment and tags for this image file"))
(define-key diredp-single-image-menu [diredp-image-dired-delete-tag]
  '(menu-item "Delete Image Tag..." diredp-image-dired-delete-tag
    :help "Remove an `image-dired' tag from this image file"))
(define-key diredp-single-image-menu [diredp-image-dired-tag-file]
  '(menu-item "Add Tags..." diredp-image-dired-tag-file
    :help "Add tags to this image file"))
(define-key diredp-single-image-menu [diredp-image-dired-comment-file]
  '(menu-item "Add Comment..." diredp-image-dired-comment-file
    :help "Add a comment to this image file"))
(define-key diredp-single-image-menu [diredp-image-dired-copy-with-exif-name]
  '(menu-item "Copy with EXIF Name" diredp-image-dired-copy-with-exif-name
    :help "Copy this image file to main image dir using EXIF name"))
(define-key diredp-single-image-menu [image-dired-dired-display-external]
  '(menu-item "Display Externally" image-dired-dired-display-external
    :help "Display image using external viewer"))
(define-key diredp-single-image-menu [image-dired-dired-display-image]
  '(menu-item "Display to Fit Other Window" image-dired-dired-display-image
    :help "Display scaled image to fit a separate window"))
(define-key diredp-single-image-menu [diredp-image-show-this-file]
  '(menu-item "Display Full Size Or Smaller" diredp-image-show-this-file
    :help "Display image full size or at least prefix-arg lines high"))
(define-key diredp-single-image-menu [dired-find-file]
  '(menu-item "Display Full Size" dired-find-file
    :help "Display image full size"))


;; `Single' > `Encryption' menu.
;;
(when (fboundp 'epa-dired-do-encrypt)   ; Emacs 23+
  (defvar diredp-single-encryption-menu (make-sparse-keymap "Encryption"))
  (define-key diredp-menu-bar-single-menu [encryption]
    (cons "Encryption" diredp-single-encryption-menu))

  (define-key diredp-single-encryption-menu [diredp-decrypt-this-file]
    '(menu-item "Decrypt..." (lambda ()
                               (interactive)
                               (epa-decrypt-file (expand-file-name (dired-get-filename nil 'NO-ERROR))))
      :help "Decrypt this file"))
  (define-key diredp-single-encryption-menu [diredp-verify-this-file]
    '(menu-item "Verify..." (lambda ()
                              (interactive)
                              (epa-verify-file (expand-file-name (dired-get-filename nil 'NO-ERROR))))
      :help "Verify this file"))
  (define-key diredp-single-encryption-menu [diredp-sign-this-file]
    '(menu-item "Sign..." (lambda ()
                            (interactive)
                            (epa-sign-file (expand-file-name (dired-get-filename nil 'NO-ERROR))
                                           (epa-select-keys (epg-make-context)
                                                            "Select keys for signing.
If no one is selected, default secret key is used.  "
                                                            nil
                                                            t)))
      :help "Encrypt this file"))
  (define-key diredp-single-encryption-menu [diredp-encrypt-this-file]
    '(menu-item "Encrypt..." (lambda ()
                               (interactive)
                               (epa-encrypt-file (expand-file-name (dired-get-filename nil 'NO-ERROR))
                                                 (epa-select-keys
                                                  (epg-make-context)
                                                  "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
                                                  nil t)))
      :help "Sign this file")))


;; `Single' > `Bookmark' menu.
;;
(when (require 'bookmark+ nil t)
  (defvar diredp-single-bookmarks-menu (make-sparse-keymap "Bookmark"))
  (define-key diredp-menu-bar-single-menu [bookmark]
    (cons "Bookmark" diredp-single-bookmarks-menu))

  (define-key diredp-single-bookmarks-menu [diredp-set-tag-value-this-file]
    '(menu-item "Set Tag Value..." diredp-set-tag-value-this-file
      :help "Set the value (not the name) of a given tag for this file"))
  (define-key diredp-single-bookmarks-menu [diredp-paste-replace-tags-this-file]
    '(menu-item "Paste Tags (Replace)" diredp-paste-replace-tags-this-file
      :help "Replace tags for this file with previously copied tags"))
  (define-key diredp-single-bookmarks-menu [diredp-paste-add-tags-this-file]
    '(menu-item "Paste Tags (Add)" diredp-paste-add-tags-this-file
      :help "Add previously copied tags to this file"))
  (define-key diredp-single-bookmarks-menu [diredp-copy-tags-this-file]
    '(menu-item "Copy Tags" diredp-copy-tags-this-file
      :help "Copy the tags from this file, so you can paste them to another"))
  (define-key diredp-single-bookmarks-menu [diredp-remove-all-tags-this-file]
    '(menu-item "Remove All Tags" diredp-remove-all-tags-this-file
      :help "Remove all tags from the file at cursor"))
  (define-key diredp-single-bookmarks-menu [diredp-untag-this-file]
    '(menu-item "Remove Tags..." diredp-untag-this-file
      :help "Remove some tags from the file at cursor (`C-u': remove all tags)"))
  (define-key diredp-single-bookmarks-menu [diredp-tag-this-file]
    '(menu-item "Add Tags..." diredp-tag-this-file :help "Add some tags to the file at cursor"))
  (define-key diredp-single-bookmarks-menu [diredp-bookmark-this-file]
    '(menu-item "Bookmark..." diredp-bookmark-this-file
      :help "Bookmark the file at cursor (create/set autofile)")))


;; `Multiple' menu.
;;
;; REPLACE ORIGINAL "Operate" menu in `dired.el'.
;;
(defvar diredp-menu-bar-multiple-menu (make-sparse-keymap "Multiple"))
(define-key dired-mode-map [menu-bar operate] (cons "Multiple" diredp-menu-bar-multiple-menu))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-operate-menu 'diredp-menu-bar-multiple-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-operate-menu 'diredp-menu-bar-multiple-menu "2017-04-09")

(define-key diredp-menu-bar-multiple-menu [diredp-describe-marked-autofiles]
  '(menu-item "Describe Marked Autofiles" diredp-describe-marked-autofiles
    :help "Show the metadata for the marked files that are autofiles"
    :enable (featurep 'bookmark+)))
(define-key diredp-menu-bar-multiple-menu [separator-describe] '("--")) ; -----------------------

(unless (memq system-type '(windows-nt ms-dos))
  (define-key diredp-menu-bar-multiple-menu [chown]
    '(menu-item "Change Owner..." dired-do-chown
      :help "Change the owner of marked files")))
(unless (memq system-type '(windows-nt ms-dos))
  (define-key diredp-menu-bar-multiple-menu [chgrp]
    '(menu-item "Change Group..." dired-do-chgrp
      :help "Change the owner of marked files")))
(define-key diredp-menu-bar-multiple-menu [chmod]
  '(menu-item "Change Mode..." dired-do-chmod
    :help "Change mode (attributes) of marked files"))
(when (fboundp 'dired-do-touch)         ; Emacs 22+
  (define-key diredp-menu-bar-multiple-menu [touch]
    '(menu-item "Change Timestamp (`touch')..." dired-do-touch
      :help "Change the timestamp of the marked files, using `touch'")))
(define-key diredp-menu-bar-multiple-menu [separator-change] '("--")) ; -------------------------

(define-key diredp-menu-bar-multiple-menu [diredp-do-remove-from-recentf]
    '(menu-item "Remove Marked Files From Recent Visits" diredp-do-remove-from-recentf
      :help "Remove the files marked here from the list of recently visited files"
      :enable (featurep 'recentf)))
(define-key diredp-menu-bar-multiple-menu [diredp-do-add-to-recentf]
    '(menu-item "Add Marked Files To Recent Visits" diredp-do-add-to-recentf
      :help "Add the files marked here to the list of recently visited files"
      :enable (featurep 'recentf)))
;;;; (define-key diredp-menu-bar-multiple-menu [diredp-do-eval-in-marked]
;;;;   '(menu-item "Eval Sexp In..." diredp-do-eval-in-marked
;;;;               :help "Evaluate a Lisp sexp in each marked file"))
;;;; ;;; (define-key diredp-menu-bar-multiple-menu [diredp-do-invoke-in-marked]
;;;; ;;;   '(menu-item "Invoke Function In..." diredp-do-invoke-in-marked
;;;; ;;;               :help "Invoke a Lisp function in each marked file"))
;;;; (define-key diredp-menu-bar-multiple-menu [diredp-do-apply-to-marked]
;;;;   '(menu-item "Apply Function To..." diredp-do-apply-to-marked
;;;;               :help "Apply a Lisp function to each marked file name"))
;;;; (define-key diredp-menu-bar-multiple-menu [diredp-do-command-in-marked]
;;;;     '(menu-item "Invoke Command/Macro In..." diredp-do-command-in-marked
;;;;       :help "Invoke an Emacs command or keyboard macro in each marked file"))
(define-key diredp-menu-bar-multiple-menu [print]
  '(menu-item "Print..." dired-do-print :help "Print marked files, supplying print command"))
(define-key diredp-menu-bar-multiple-menu [compress]
  '(menu-item "Compress/Uncompress" dired-do-compress :help "Compress/uncompress marked files"))
(when (fboundp 'dired-do-compress-to)
  (define-key diredp-menu-bar-multiple-menu [compress-to]
    '(menu-item "Compress to..." dired-do-compress-to
      :help "Compress marked files and dirs together, in the same archive")))
(define-key diredp-menu-bar-multiple-menu [command]
  '(menu-item "Shell Command..." dired-do-shell-command
    :help "Run a shell command on each marked file"))
(when (fboundp 'dired-do-async-shell-command) ; Emacs 23+
  (define-key diredp-menu-bar-multiple-menu [async-command]
    '(menu-item "Asynchronous Shell Command..." dired-do-async-shell-command
      :help "Run a shell command asynchronously on each marked file")))
(define-key diredp-menu-bar-multiple-menu [compile]
  '(menu-item "Byte Compile" dired-do-byte-compile :help "Byte-compile marked Emacs Lisp files"))
(define-key diredp-menu-bar-multiple-menu [load]
  '(menu-item "Load" dired-do-load :help "Load marked Emacs Lisp files"))

(unless (require 'bookmark+ nil t)
  (define-key diredp-menu-bar-multiple-menu [diredp-bookmark-this-file]
    '(menu-item "Bookmark..." diredp-bookmark-this-file :help "Bookmark the file at cursor")))
(when (fboundp 'mkhtml-dired-files)     ; In `mkhtml.el'.
  (define-key diredp-menu-bar-multiple-menu [mkhtml-dired-files]
    '(menu-item "Create HTML" mkhtml-dired-files
      :help "Create HTML files corresponding to marked files")))
(define-key diredp-menu-bar-multiple-menu [separator-misc] '("--")) ; ---------------------------

(define-key diredp-menu-bar-multiple-menu [diredp-copy-abs-filenames-as-kill]
  '(menu-item "Copy Marked Names as Absolute" diredp-copy-abs-filenames-as-kill
    :help "Copy absolute names of marked files to the kill ring"
    :keys "M-0 w"))
(define-key diredp-menu-bar-multiple-menu [kill-ring]
  '(menu-item "Copy Marked Names" dired-copy-filename-as-kill
    :help "Copy names of marked files to the kill ring, for pasting"))
(define-key diredp-menu-bar-multiple-menu [diredp-list-marked]
    '(menu-item "List Marked Files" diredp-list-marked
      :help "List the files marked here (C-u C-u: all, C-u C-u C-u: all + dirs)"))
(define-key diredp-menu-bar-multiple-menu [diredp-insert-subdirs]
  '(menu-item "Insert Subdirs" diredp-insert-subdirs
    :help "Insert the marked subdirectories - like using `i' at each marked dir"))
;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(define-key diredp-menu-bar-multiple-menu [dired-multiple-w32-browser]
      '(menu-item "Open Associated Windows Apps" dired-multiple-w32-browser
        :help "Open files using the Windows apps associated with their file types")))
(when (fboundp 'dired-do-find-marked-files)
  (define-key diredp-menu-bar-multiple-menu [find-files]
    '(menu-item "Open" dired-do-find-marked-files ; In `dired-x.el'.
      :help "Open each marked file for editing")))


;; `Multiple' > `Apply (Map)' menu.
;;
(defvar diredp-apply-multiple-menu (make-sparse-keymap "Apply (Map)"))
(defalias 'diredp-apply-multiple-menu diredp-apply-multiple-menu)
(define-key diredp-menu-bar-multiple-menu [apply]
  (cons "Apply (Map)" diredp-apply-multiple-menu))

(define-key diredp-apply-multiple-menu [diredp-do-eval-in-marked]
  '(menu-item "Eval Sexp In..." diredp-do-eval-in-marked
              :help "Evaluate a Lisp sexp in each marked file or dir"))
(define-key diredp-apply-multiple-menu [diredp-do-command-in-marked]
  '(menu-item "Invoke Command/Macro In..." diredp-do-command-in-marked
              :help "Invoke Emacs command or keyboard macro in each marked file or dir"))
;;; (define-key diredp-apply-multiple-menu [diredp-do-invoke-in-marked]
;;;   '(menu-item "Invoke Function In..." diredp-do-invoke-in-marked
;;;               :help "Invoke a Lisp function in each marked file or dir"))
(define-key diredp-apply-multiple-menu [diredp-do-apply-to-marked]
  '(menu-item "Apply Function To..." diredp-do-apply-to-marked
              :help "Apply a Lisp function to each marked file or dir name"))


;; `Multiple' > `Dired' menu.
;;
(defvar diredp-multiple-dired-menu (make-sparse-keymap "Dired")
  "`Dired' submenu for Dired menu-bar `Multiple' menu.")
(define-key diredp-menu-bar-multiple-menu [multiple-dired]
  `(menu-item "Dired" ,diredp-multiple-dired-menu
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files and dirs only"))

(define-key diredp-multiple-dired-menu [diredp-define-snapshot-dired-commands]
  '(menu-item "Define Dired Commands for Marked" diredp-define-snapshot-dired-commands
    :help "Define commands to dired the marked files and dirs"))
(define-key diredp-multiple-dired-menu [diredp-marked-other-window]
  '(menu-item "Dired Marked in Other Window" diredp-marked-other-window
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files and dirs only, in other window"))
(define-key diredp-multiple-dired-menu [diredp-marked]
  '(menu-item "Dired Marked" diredp-marked
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files and dirs only"))


;; `Multiple' > `Omit' menu.
;;
(defvar diredp-multiple-omit-menu (make-sparse-keymap "Omit")
  "`Omit' submenu for Dired menu-bar `Multiple' menu.")
(define-key diredp-menu-bar-multiple-menu [multiple-omit] (cons "Omit" diredp-multiple-omit-menu))

(define-key diredp-multiple-omit-menu [omit-unmarked]
  '(menu-item "Omit Unmarked" diredp-omit-unmarked :help "Hide lines of unmarked files"))
(define-key diredp-multiple-omit-menu [omit-marked]
  '(menu-item "Omit Marked" diredp-omit-marked :help "Hide lines of marked files"))


;; `Multiple' > `Delete' menu.
;;
(defvar diredp-multiple-delete-menu (make-sparse-keymap "Delete")
  "`Delete' submenu for Dired menu-bar `Multiple' menu.")
(define-key diredp-menu-bar-multiple-menu [multiple-delete] (cons "Delete" diredp-multiple-delete-menu))

(define-key diredp-multiple-delete-menu [delete-flagged]
  '(menu-item "Delete Flagged" dired-do-flagged-delete
    :help "Delete all files flagged for deletion (D)"))
(define-key diredp-multiple-delete-menu [delete]
  '(menu-item "Delete Marked (not Flagged)" dired-do-delete
    :help "Delete current file or all marked files (not flagged files)"))


;; `Multiple' > `Rename' menu.
;;
(defvar diredp-multiple-rename-menu (make-sparse-keymap "Rename")
  "`Rename' submenu for Dired menu-bar `Multiple' menu.")
(define-key diredp-menu-bar-multiple-menu [multiple-case] (cons "Rename" diredp-multiple-rename-menu))

(define-key diredp-multiple-rename-menu [multiple-rename-rename]
  '(menu-item "Move to Dir... / Rename This..." dired-do-rename
    :help "Move marked (or next N) files, or rename current file"))

(define-key diredp-multiple-rename-menu [multiple-rename-capitalize]
  '(menu-item "Capitalize" diredp-capitalize
    :help "Capitalize (initial caps) the names of all marked files"))
(define-key diredp-multiple-rename-menu [multiple-rename-downcase]
  '(menu-item "Downcase" dired-downcase
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename marked files to lowercase names"))
(define-key diredp-multiple-rename-menu [multiple-rename-upcase]
  '(menu-item "Upcase" dired-upcase
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename marked files to uppercase names"))


;; `Multiple' > `Move / Copy / Link' menu.
;;
(defvar diredp-multiple-move-copy-link-menu (make-sparse-keymap "Move / Copy / Link")
  "`Move / Copy / Link' submenu for Dired menu-bar `Multiple' menu.")
(define-key diredp-menu-bar-multiple-menu [multiple-move-copy-link]
  (cons "Move / Copy / Link" diredp-multiple-move-copy-link-menu))

(define-key diredp-multiple-move-copy-link-menu [multiple-move-copy-link-hardlink]
  '(menu-item "Hardlink to..." dired-do-hardlink
    :help "Make hard links for current or marked files"))
(define-key diredp-multiple-move-copy-link-menu [multiple-move-copy-link-symlink]
  '(menu-item "Symlink to (Absolute)..." dired-do-symlink ; In `dired-aux.el'.
              :help "Make absolute symbolic links for current or marked files"))
(define-key diredp-multiple-move-copy-link-menu [multiple-move-copy-link-relsymlink]
  '(menu-item "Symlink to (Relative)..." dired-do-relsymlink ; In `dired-x.el'.
              :help "Make relative symbolic links for current or marked files"))
(define-key diredp-multiple-move-copy-link-menu [multiple-move-copy-link-copy]
  '(menu-item "Copy to..." dired-do-copy :help "Copy current file or all marked files"))
(define-key diredp-multiple-move-copy-link-menu [multiple-move-copy-link-rename]
  '(menu-item "Move to..." dired-do-rename :help "Rename current file or move marked files"))


;; `Multiple' > `Images' menu.
;;
(defvar diredp-multiple-images-menu (make-sparse-keymap "Images"))
(defalias 'diredp-multiple-images-menu diredp-multiple-images-menu)
(define-key diredp-menu-bar-multiple-menu [images]
  '(menu-item "Images" diredp-multiple-images-menu
    :enable (fboundp 'image-dired-display-thumbs)))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-images-menu 'diredp-multiple-images-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-images-menu 'diredp-multiple-images-menu "2017-04-09")

;; Remove the items from `Multiple' menu.
(define-key diredp-menu-bar-multiple-menu [image-dired-delete-tag] nil)
(define-key diredp-menu-bar-multiple-menu [image-dired-tag-files] nil)
(define-key diredp-menu-bar-multiple-menu [image-dired-dired-comment-files] nil)
(define-key diredp-menu-bar-multiple-menu [image-dired-display-thumbs] nil)

;; Add them to `Multiple' > `Images' menu.
(define-key diredp-multiple-images-menu [image-dired-delete-tag]
  '(menu-item "Delete Tag..." image-dired-delete-tag
    :help "Delete tag from marked image files"))
(define-key diredp-multiple-images-menu [image-dired-tag-files]
  '(menu-item "Add Tags..." image-dired-tag-files
    :help "Add tags to marked image files"))
(define-key diredp-multiple-images-menu [image-dired-dired-comment-files]
  '(menu-item "Add Comment..." image-dired-dired-comment-files
    :help "Add comment to marked image files"))
(define-key diredp-multiple-images-menu [image-dired-display-thumbs]
  '(menu-item "Display Thumbnails" image-dired-display-thumbs
    :help "Display thumbnails for marked image files"))
(define-key diredp-multiple-images-menu [diredp-do-display-images]
  '(menu-item "Display" diredp-do-display-images
    :help "Display the marked image files"))


;; `Multiple' > `Encryption' menu.
;;
(when (fboundp 'epa-dired-do-encrypt)   ; Emacs 23+
  (defvar diredp-multiple-encryption-menu (make-sparse-keymap "Encryption"))
  (define-key diredp-menu-bar-multiple-menu [encryption]
    (cons "Encryption" diredp-multiple-encryption-menu))

  ;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
  ;; works for newer Emacs too.
  (when (fboundp 'defvaralias)          ; Emacs 22+
    (defvaralias 'diredp-menu-bar-encryption-menu 'diredp-multiple-encryption-menu))
  (diredp-make-obsolete-variable 'diredp-menu-bar-encryption-menu 'diredp-multiple-encryption-menu "2017-04-09")

  (when (boundp 'diredp-menu-bar-encryption-menu)
    (defalias 'diredp-menu-bar-encryption-menu diredp-menu-bar-encryption-menu))
  (diredp-make-obsolete 'diredp-menu-bar-encryption-menu 'diredp-multiple-encryption-menu "2017-04-09")

  ;; Remove the items from `Multiple' menu.
  (define-key diredp-menu-bar-multiple-menu [epa-dired-do-decrypt] nil)
  (define-key diredp-menu-bar-multiple-menu [epa-dired-do-verify] nil)
  (define-key diredp-menu-bar-multiple-menu [epa-dired-do-sign] nil)
  (define-key diredp-menu-bar-multiple-menu [epa-dired-do-encrypt] nil)

  ;; Add them to `Multiple' > `Encryption' menu.
  (define-key diredp-multiple-encryption-menu [epa-dired-do-decrypt]
    '(menu-item "Decrypt..." epa-dired-do-decrypt :help "Decrypt the marked files"))
  (define-key diredp-multiple-encryption-menu [epa-dired-do-verify]
    '(menu-item "Verify..." epa-dired-do-verify :help "Verify the marked files"))
  (define-key diredp-multiple-encryption-menu [epa-dired-do-sign]
    '(menu-item "Sign..." epa-dired-do-sign :help "Sign the marked files"))
  (define-key diredp-multiple-encryption-menu [epa-dired-do-encrypt]
    '(menu-item "Encrypt..." epa-dired-do-encrypt :help "Encrypt the marked files")))


;; `Multiple' > `Search' menu.
;;
(defvar diredp-multiple-search-menu (make-sparse-keymap "Search"))
(define-key diredp-menu-bar-multiple-menu [search]
  (cons "Search" diredp-multiple-search-menu))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-operate-search-menu 'diredp-multiple-search-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-operate-search-menu 'diredp-multiple-search-menu "2017-04-09")

(when (fboundp 'dired-do-isearch-regexp) ; Emacs 23+
  (define-key diredp-multiple-search-menu [isearch-regexp]
    '(menu-item "Isearch Regexp Files..." dired-do-isearch-regexp
      :help "Incrementally search marked files for regexp"))
  (define-key diredp-multiple-search-menu [isearch]
    '(menu-item "Isearch Files..." dired-do-isearch
      :help "Incrementally search marked files for string")))
(when (fboundp 'dired-do-find-regexp-and-replace)
  (define-key diredp-multiple-search-menu [find-query-replace]
    '(menu-item "Query Replace Using `find'..." dired-do-find-regexp-and-replace
      :help "Replace regexp in marked files using `find'")))
(define-key diredp-multiple-search-menu [query-replace]
  (if (< emacs-major-version 21)
      '(menu-item "Query Replace Using TAGS Table..." dired-do-query-replace)
    '(menu-item "Query Replace Using TAGS Table..." dired-do-query-replace-regexp
      :help "Replace regexp in marked files using tags in a TAGS table")))
(when (fboundp 'dired-do-find-regexp)
  (define-key diredp-multiple-search-menu [find-regexp]
    '(menu-item "Search Files Using `find'..." dired-do-find-regexp
      :help "Search marked files for regexp using `find'")))
(define-key diredp-multiple-search-menu [search]
  '(menu-item "Search Files Using TAGS Table..." dired-do-search
              :help "Search marked files for regexp using tags in a TAGS table"))
(define-key diredp-multiple-search-menu [grep]
  '(menu-item "Grep..." diredp-do-grep :help "Grep marked, next N, or all files shown"))


;; `Multiple' > `Bookmark' menu.
;;
(defvar diredp-multiple-bookmarks-menu (make-sparse-keymap "Bookmark"))
(define-key diredp-menu-bar-multiple-menu [bookmark]
  (cons "Bookmark" diredp-multiple-bookmarks-menu))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-operate-bookmarks-menu 'diredp-multiple-bookmarks-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-operate-bookmarks-menu 'diredp-multiple-bookmarks-menu "2017-04-09")

(when (require 'bookmark+ nil t)
  (define-key diredp-multiple-bookmarks-menu [diredp-do-set-tag-value]
    '(menu-item "Set Tag Value..." diredp-do-set-tag-value
      :help "Set the value of a given tag for the marked or next N files"))
  (define-key diredp-multiple-bookmarks-menu [diredp-do-paste-replace-tags]
    '(menu-item "Paste Tags (Replace)" diredp-do-paste-replace-tags
      :help "Replace tags for the marked or next N files with copied tags"))
  (define-key diredp-multiple-bookmarks-menu [diredp-do-paste-add-tags]
    '(menu-item "Paste Tags (Add)" diredp-do-paste-add-tags
      :help "Add previously copied tags to the marked or next N files"))
  (define-key diredp-multiple-bookmarks-menu [diredp-do-remove-all-tags]
    '(menu-item "Remove All Tags" diredp-do-remove-all-tags
      :help "Remove all tags from the marked or next N files"))
  (define-key diredp-multiple-bookmarks-menu [diredp-do-untag]
    '(menu-item "Remove Tags..." diredp-do-untag
      :help "Remove some tags from the marked or next N files"))
  (define-key diredp-multiple-bookmarks-menu [diredp-do-tag]
    '(menu-item "Add Tags..." diredp-do-tag
      :help "Add some tags to the marked or next N files"))
  (define-key diredp-multiple-bookmarks-menu [separator-book-2] '("--"))) ; ------------

(define-key diredp-multiple-bookmarks-menu
    [diredp-do-bookmark-in-bookmark-file-recursive]
  '(menu-item "Bookmark in Bookmark File (Here and Below)..."
    diredp-do-bookmark-in-bookmark-file-recursive
    :help "Bookmark marked files (including in marked subdirs) in bookmark file and save it"))
(define-key diredp-multiple-bookmarks-menu
    [diredp-set-bookmark-file-bookmark-for-marked-recursive]
  '(menu-item "Create Bookmark-File Bookmark (Here and Below)..."
    diredp-set-bookmark-file-bookmark-for-marked-recursive
    :help "Create a bookmark-file bookmark for marked files, including in marked subdirs"))
(define-key diredp-multiple-bookmarks-menu [diredp-do-bookmark-dirs-recursive]
    '(menu-item "Bookmark Dirs (Here and Below)..." diredp-do-bookmark-dirs-recursive
      :help "Bookmark this Dired buffer and marked subdirectory Dired buffers, recursively."))
(define-key diredp-multiple-bookmarks-menu [diredp-do-bookmark-recursive]
  '(menu-item "Bookmark (Here and Below)..." diredp-do-bookmark-recursive
    :help "Bookmark the marked files, including those in marked subdirs"))
(define-key diredp-multiple-bookmarks-menu [separator-book-1] '("--")) ; ---------------

(define-key diredp-multiple-bookmarks-menu [diredp-do-bookmark-in-bookmark-file]
  '(menu-item "Bookmark in Bookmark File..." diredp-do-bookmark-in-bookmark-file
    :help "Bookmark the marked files in BOOKMARK-FILE and save BOOKMARK-FILE"))
(define-key diredp-multiple-bookmarks-menu [diredp-set-bookmark-file-bookmark-for-marked]
  '(menu-item "Create Bookmark-File Bookmark..." diredp-set-bookmark-file-bookmark-for-marked
    :help "Create a bookmark-file bookmark, and bookmark the marked files in it"))
(define-key diredp-multiple-bookmarks-menu [diredp-do-bookmark]
  '(menu-item "Bookmark..." diredp-do-bookmark :help "Bookmark the marked or next N files"))


;; `Multiple' > `Marked Here and Below' menu.
;;
(defvar diredp-multiple-recursive-menu (make-sparse-keymap "Marked Here and Below"))
(define-key diredp-menu-bar-multiple-menu [operate-recursive]
  (cons "Marked Here and Below" diredp-multiple-recursive-menu))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-operate-recursive-menu 'diredp-multiple-recursive-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-operate-recursive-menu 'diredp-multiple-recursive-menu "2017-04-09")

(when (fboundp 'diredp-do-chown-recursive)
  (define-key diredp-multiple-recursive-menu [chown]
    '(menu-item "Change Owner..." diredp-do-chown-recursive
      :help "Change the owner of marked files, including those in marked subdirs")))
(when (fboundp 'diredp-do-chgrp-recursive)
  (define-key diredp-multiple-recursive-menu [chgrp]
    '(menu-item "Change Group..." diredp-do-chgrp-recursive
      :help "Change the owner of marked files, including those in marked subdirs")))
(define-key diredp-multiple-recursive-menu [chmod]
  '(menu-item "Change Mode..." diredp-do-chmod-recursive
    :help "Change mode (attributes) of marked files, including those in marked subdirs"))
(when (fboundp 'dired-do-touch)         ; Emacs 22+
  (define-key diredp-multiple-recursive-menu [touch]
    '(menu-item "Change Timestamp (`touch')..." diredp-do-touch-recursive
      :help "Change timestamp of marked files, including those in marked subdirs")))
(define-key diredp-multiple-recursive-menu [separator-change] '("--")) ; ----------------

(define-key diredp-multiple-recursive-menu [diredp-do-print-recursive]
    '(menu-item "Print..." diredp-do-print-recursive
      :help "Print the marked files, including those in marked subdirs"))
(define-key diredp-multiple-recursive-menu [diredp-do-shell-command-recursive]
    '(menu-item "Shell Command..." diredp-do-shell-command-recursive
      :help "Run shell command on the marked files, including those in marked subdirs"))
(when (fboundp 'dired-do-async-shell-command) ; Emacs 23+
  (define-key diredp-multiple-recursive-menu [diredp-do-async-shell-command-recursive]
    '(menu-item "Asynchronous Shell Command..." diredp-do-async-shell-command-recursive
      :help "Run shell command asynchronously on marked files, including in marked subdirs")))

(when (fboundp 'diredp-unmark-all-marks-recursive) ; Emacs 22+
  (define-key diredp-multiple-recursive-menu [separator-1] '("--")) ; ------------
  (define-key diredp-multiple-recursive-menu [diredp-change-marks-recursive]
    '(menu-item "Change Mark..." diredp-change-marks-recursive
      :help "Change all OLD marks to NEW marks, including those in marked subdirs"))
  (define-key diredp-multiple-recursive-menu [diredp-unmark-all-files-recursive]
    '(menu-item "Unmark Marked-With..." diredp-unmark-all-files-recursive
      :help "Remove a given mark everywhere, including in marked subdirs"))
  (define-key diredp-multiple-recursive-menu [diredp-unmark-all-marks-recursive]
    '(menu-item "Unmark All..." diredp-unmark-all-marks-recursive
      :help "Remove ALL marks everywhere, including in marked subdirs")))

(define-key diredp-multiple-recursive-menu [separator-misc] '("--")) ; ------------------

(define-key diredp-multiple-recursive-menu [diredp-do-delete-recursive]
    '(menu-item "Delete Marked (not Flagged)" diredp-do-delete-recursive
      :help "Delete marked (not flagged) files, including in marked subdirs"))
(define-key diredp-multiple-recursive-menu [separator-delete] '("--")) ; ----------------

(define-key diredp-multiple-recursive-menu [diredp-do-hardlink-recursive]
  '(menu-item "Hardlink to..." diredp-do-hardlink-recursive
    :help "Make hard links for marked files, including those in marked subdirs"))
(define-key diredp-multiple-recursive-menu [diredp-do-symlink-recursive]
  '(menu-item "Symlink to (Absolute)..." diredp-do-symlink-recursive
              :help "Make absolute symbolic links for marked files, including those in marked subdirs"))
(define-key diredp-multiple-recursive-menu [diredp-do-relsymlink-recursive]
  '(menu-item "Symlink to (Relative)..." diredp-do-relsymlink-recursive
              :help "Make relative symbolic links for marked files, including those in marked subdirs"))
(define-key diredp-multiple-recursive-menu [diredp-do-copy-recursive]
    '(menu-item "Copy to..." diredp-do-copy-recursive
      :help "Copy marked files, including in marked subdirs, to a given directory"))
(define-key diredp-multiple-recursive-menu [diredp-do-move-recursive]
    '(menu-item "Move to..." diredp-do-move-recursive
      :help "Move marked files, including in marked subdirs, to a given directory"))
(define-key diredp-multiple-recursive-menu [separator-copy-move] '("--")) ; -------------

(define-key diredp-multiple-recursive-menu [diredp-capitalize-recursive]
  '(menu-item "Capitalize" diredp-capitalize-recursive
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Capitalize the names of all marked files, including in marked subdirs"))
(define-key diredp-multiple-recursive-menu [diredp-downcase-recursive]
  '(menu-item "Downcase" diredp-downcase-recursive
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename marked files, including in marked subdirs, to lowercase names"))
(define-key diredp-multiple-recursive-menu [diredp-upcase-recursive]
  '(menu-item "Upcase" diredp-upcase-recursive
    :enable (or (not (fboundp 'msdos-long-file-names))  (msdos-long-file-names))
    :help "Rename marked files, including in marked subdirs, to uppercase names"))
(define-key diredp-multiple-recursive-menu [separator-lettercase] '("--")) ; ------------

(define-key diredp-multiple-recursive-menu [diredp-list-marked-recursive]
    '(menu-item "List Marked Files" diredp-list-marked-recursive
      :help "List the files marked here and in marked subdirs, recursively"))
(define-key diredp-multiple-recursive-menu [diredp-copy-filename-as-kill-recursive]
    '(menu-item "Copy File Names (to Paste)" diredp-copy-filename-as-kill-recursive
      :help "Copy names of files marked here and in marked subdirs, to `kill-ring'"))
(define-key diredp-multiple-recursive-menu [diredp-insert-subdirs-recursive]
  '(menu-item "Insert Subdirs" diredp-insert-subdirs-recursive
    :help "Insert the marked subdirectories, gathered recursively"))
(define-key diredp-multiple-recursive-menu [separator-dirs] '("--")) ; ------------------

(define-key diredp-multiple-recursive-menu [diredp-marked-recursive-other-window]
    '(menu-item "Dired (Marked) in Other Window" diredp-marked-recursive-other-window
      :help "Open Dired (in other window) on marked files, including those in marked subdirs"))
(define-key diredp-multiple-recursive-menu [diredp-marked-recursive]
    '(menu-item "Dired (Marked)" diredp-marked-recursive
      :help "Open Dired on marked files, including those in marked subdirs"))
;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(define-key diredp-multiple-recursive-menu [diredp-multiple-w32-browser-recursive]
    '(menu-item "Open Associated Windows Apps" diredp-multiple-w32-browser-recursive
      :help "Run Windows apps for with marked files, including those in marked subdirs")))
(define-key diredp-multiple-recursive-menu [diredp-do-find-marked-files-recursive]
    '(menu-item "Open" diredp-do-find-marked-files-recursive
      :help "Find marked files simultaneously, including those in marked subdirs"))


;; `Multiple' > `Marked Here and Below' > `Apply (Map)' menu.
;;
(defvar diredp-apply-recursive-menu (make-sparse-keymap "Apply (Map)"))
(defalias 'diredp-apply-recursive-menu diredp-apply-recursive-menu)
(define-key diredp-multiple-recursive-menu [apply]
  (cons "Apply (Map)" diredp-apply-recursive-menu))

(define-key diredp-apply-recursive-menu [diredp-do-eval-in-marked-recursive]
  '(menu-item "Eval Sexp In..." diredp-do-eval-in-marked-recursive
              :help "Evaluate a Lisp sexp in marked, including in marked subdirs"))
(define-key diredp-apply-recursive-menu [diredp-do-command-in-marked-recursive]
  '(menu-item "Invoke Command/Macro In..." diredp-do-command-in-marked-recursive
              :help "Invoke command or keyboard macro in each marked file, including in marked subdirs"))
;;; (define-key diredp-apply-recursive-menu [diredp-do-invoke-in-marked-recursive]
;;;   '(menu-item "Invoke Function In..." diredp-do-invoke-in-marked-recursive
;;;               :help "Invoke a Lisp function in each marked file, including in marked subdirs"))
(define-key diredp-apply-recursive-menu [diredp-do-apply-to-marked-recursive]
  '(menu-item "Apply Function To..." diredp-do-apply-to-marked-recursive
              :help "Apply a Lisp function to marked file names, including in marked subdirs"))


;; `Multiple' > `Marked Here and Below' > `Images' menu.
;;
(defvar diredp-images-recursive-menu (make-sparse-keymap "Images"))
(defalias 'diredp-images-recursive-menu diredp-images-recursive-menu)

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-images-recursive-menu 'diredp-images-recursive-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-images-recursive-menu 'diredp-images-recursive-menu "2017-04-09")

(when (boundp 'diredp-menu-bar-images-recursive-menu)
  (defalias 'diredp-menu-bar-images-recursive-menu diredp-menu-bar-images-recursive-menu))
(diredp-make-obsolete 'diredp-menu-bar-images-recursive-menu 'diredp-images-recursive-menu "2017-04-09")

(define-key diredp-multiple-recursive-menu [images]
  '(menu-item "Images" diredp-images-recursive-menu
    :enable (fboundp 'image-dired-delete-tag)))
(define-key diredp-images-recursive-menu [diredp-image-dired-delete-tag-recursive]
  '(menu-item "Delete Image Tag..." diredp-image-dired-delete-tag-recursive
    :help "Remove an `image-dired' tag from marked files, including those in marked subdirs"))
(define-key diredp-images-recursive-menu [diredp-image-dired-tag-files-recursive]
  '(menu-item "Add Image Tags..." diredp-image-dired-tag-files-recursive
    :help "Add `image-dired' tags to marked files, including those in marked subdirs"))
(define-key diredp-images-recursive-menu [diredp-image-dired-comment-files-recursive]
  '(menu-item "Add Image Comment..." diredp-image-dired-comment-files-recursive
    :help "Add image comment to marked files, including those in marked subdirs"))
(define-key diredp-images-recursive-menu [diredp-image-dired-display-thumbs-recursive]
  '(menu-item "Display Image Thumbnails" diredp-image-dired-display-thumbs-recursive
    :help "Show thumbnails for marked image files, including those in marked subdirs"))


;; `Multiple' > `Marked Here and Below' > `Encryption' menu.
;;
(when (fboundp 'epa-dired-do-encrypt)   ; Emacs 23+
  (defvar diredp-menu-bar-encryption-recursive-menu (make-sparse-keymap "Encryption"))
  (define-key diredp-multiple-recursive-menu [encryption]
    (cons "Encryption" diredp-menu-bar-encryption-recursive-menu))
  (define-key diredp-menu-bar-encryption-recursive-menu [diredp-do-decrypt-recursive]
    '(menu-item "Decrypt..." diredp-do-decrypt-recursive
      :help "Decrypt marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-encryption-recursive-menu [diredp-do-verify-recursive]
    '(menu-item "Verify..." diredp-do-verify-recursive
      :help "Verify marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-encryption-recursive-menu [diredp-do-sign-recursive]
    '(menu-item "Sign..." diredp-do-sign-recursive
      :help "Sign marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-encryption-recursive-menu [diredp-do-encrypt-recursive]
    '(menu-item "Encrypt..." diredp-do-encrypt-recursive
      :help "Encrypt marked files, including those in marked subdirs")))


;; `Multiple' > `Marked Here and Below' > `Search' menu.
;;
(defvar diredp-menu-bar-search-recursive-menu (make-sparse-keymap "Search"))
(define-key diredp-multiple-recursive-menu [search]
  (cons "Search" diredp-menu-bar-search-recursive-menu))
(when (fboundp 'dired-do-isearch-regexp) ; Emacs 23+
  (define-key diredp-menu-bar-search-recursive-menu [diredp-do-isearch-regexp-recursive]
    '(menu-item "Isearch Regexp Files..." diredp-do-isearch-regexp-recursive
      :help "Incrementally regexp search marked files, including those in marked subdirs"))
  (define-key diredp-menu-bar-search-recursive-menu [diredp-do-isearch-recursive]
    '(menu-item "Isearch Files..." diredp-do-isearch-recursive
      :help "Incrementally search marked files, including those in marked subdirs")))
(define-key diredp-menu-bar-search-recursive-menu [diredp-do-query-replace-regexp-recursive]
  '(menu-item "Query Replace..." diredp-do-query-replace-regexp-recursive
    :help "Replace regexp in marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-search-recursive-menu [diredp-do-search-recursive]
  '(menu-item "Search Files..." diredp-do-search-recursive
    :help "Regexp search marked files, including those in marked subdirs"))
(define-key diredp-menu-bar-search-recursive-menu [diredp-do-grep-recursive]
  '(menu-item "Grep..." diredp-do-grep-recursive
    :help "Run `grep' on the marked files, including those in marked subdirs"))


;; `Multiple' > `Marked Here and Below' > `Bookmark' menu.
;;
(defvar diredp-menu-bar-bookmarks-recursive-menu (make-sparse-keymap "Bookmark"))
(define-key diredp-multiple-recursive-menu [bookmarks]
  (cons "Bookmark" diredp-menu-bar-bookmarks-recursive-menu))
(define-key diredp-menu-bar-bookmarks-recursive-menu
    [diredp-do-bookmark-in-bookmark-file-recursive]
    '(menu-item "Bookmark in Bookmark File..." diredp-do-bookmark-in-bookmark-file-recursive
      :help "Bookmark marked files, including those in marked subdirs, in a bookmark file"))
(define-key diredp-menu-bar-bookmarks-recursive-menu
    [diredp-set-bookmark-file-bookmark-for-marked-recursive]
  '(menu-item "Create Bookmark-File Bookmark..."
    diredp-set-bookmark-file-bookmark-for-marked-recursive
    :help "Create a bookmark-file bookmark for marked files, including in marked subdirs"))
(define-key diredp-menu-bar-bookmarks-recursive-menu [diredp-do-bookmark-dirs-recursive]
    '(menu-item "Bookmark Dirs..." diredp-do-bookmark-dirs-recursive
      :help "Bookmark this Dired buffer and marked subdirectory Dired buffers, recursively."))
(define-key diredp-menu-bar-bookmarks-recursive-menu [diredp-do-bookmark-recursive]
    '(menu-item "Bookmark..." diredp-do-bookmark-recursive
      :help "Bookmark the marked files, including those in marked subdirs"))



;; `Regexp' menu.
;;
;; REPLACE ORIGINAL `Regexp' menu in `dired.el'.
;;
(defvar diredp-menu-bar-regexp-menu (make-sparse-keymap "Regexp"))
(define-key dired-mode-map [menu-bar regexp] (cons "Regexp" diredp-menu-bar-regexp-menu))

(define-key diredp-menu-bar-regexp-menu [hardlink]
  '(menu-item "Hardlink to..." dired-do-hardlink-regexp ; In `dired-aux.el'.
    :help "Make hard links for files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [symlink]
  '(menu-item "Symlink to (Absolute)..." dired-do-symlink-regexp ; In `dired-aux.el'.
              :help "Make absolute symbolic links for files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [relsymlink]
  '(menu-item "Symlink to (Relative)..." dired-do-relsymlink-regexp ; In `dired-x.el'.
              :help "Make relative symbolic links for files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [copy]
  '(menu-item "Copy to..." dired-do-copy-regexp ; In `dired-aux.el'.
              :help "Copy marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [rename]
  '(menu-item "Move/Rename to..." dired-do-rename-regexp ; In `dired-aux.el'.
              :help "Move/rename marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [unmark-regexp-default-dir]
  '(menu-item "Unmark (Default Dir)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "UNmark files (regexp): ") ?\040 nil))
              :help "Unmark files matching regexp with default directory"
              :keys "C-u C-u \\[dired-mark-files-regexp]"))
(define-key diredp-menu-bar-regexp-menu [unmark-regexp-absolute]
  '(menu-item "Unmark (Absolute)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "UNmark files (regexp): ") ?\040 t))
              :help "Unmark files matching regexp against absolute file name"
              :keys "M-0 \\[dired-mark-files-regexp]"))
(define-key diredp-menu-bar-regexp-menu [unmark-regexp-relative]
  '(menu-item "Unmark (Relative)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "UNmark files (regexp): ") ?\040 'no-dir))
              :help "Unmark files matching regexp against relative file name"
              :keys "C-u \\[dired-mark-files-regexp]"))
(define-key diredp-menu-bar-regexp-menu [mark-regexp-default-dir]
  '(menu-item "Mark (Default Dir)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "Mark files (regexp): ") dired-marker-char nil))
              :help "Mark files matching regexp with default directory"
              :keys "M-9 \\[dired-mark-files-regexp]"))
(define-key diredp-menu-bar-regexp-menu [mark-regexp-absolute]
  '(menu-item "Mark (Absolute)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "Mark files (regexp): ") dired-marker-char t))
              :help "Mark files matching regexp against absolute file name"
              :keys "M-- \\[dired-mark-files-regexp]"))
(define-key diredp-menu-bar-regexp-menu [mark-regexp-relative]
  '(menu-item "Mark (Relative)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "Mark files (regexp): ") dired-marker-char 'no-dir))
              :help "Mark files matching regexp against relative file name"
              :keys "\\[dired-mark-files-regexp]"))
(define-key diredp-menu-bar-regexp-menu [mark-containing]
  '(menu-item "Mark Containing..." dired-mark-files-containing-regexp
              :help "Mark files whose contents matches regexp"))
(define-key diredp-menu-bar-regexp-menu [image-dired-mark-tagged-files]
  '(menu-item "Mark Image Files Tagged..." image-dired-mark-tagged-files
              :enable (fboundp 'image-dired-mark-tagged-files)
              :help "Mark image files whose image tags match regexp"))
(define-key diredp-menu-bar-regexp-menu [flag]
  '(menu-item "Flag..." dired-flag-files-regexp
              :help "Flag files matching regexp for deletion"))


;; `Regexp' > `Here and Below' menu.
;;
(defvar diredp-regexp-recursive-menu (make-sparse-keymap "Here and Below"))
(define-key diredp-menu-bar-regexp-menu [mark-recursive]
  (cons "Here and Below" diredp-regexp-recursive-menu))
(define-key diredp-regexp-recursive-menu [diredp-mark-files-regexp-recursive]
    '(menu-item "Mark Named (Absolute)..." diredp-mark-files-regexp-recursive
      :help "Mark all file names matching a regexp, including those in marked subdirs"))
(define-key diredp-regexp-recursive-menu [diredp-mark-files-containing-regexp-recursive]
    '(menu-item "Mark Containing..." diredp-mark-files-containing-regexp-recursive
      :help "Mark all files with content matching a regexp, including in marked subdirs"))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-regexp-recursive-menu 'diredp-regexp-recursive-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-regexp-recursive-menu 'diredp-regexp-recursive-menu "2017-04-09")

(when (boundp 'diredp-menu-bar-regexp-recursive-menu)
  (defalias 'diredp-menu-bar-regexp-recursive-menu diredp-menu-bar-regexp-recursive-menu))
(diredp-make-obsolete 'diredp-menu-bar-regexp-recursive-menu 'diredp-regexp-recursive-menu "2017-04-09")


;; "Marks" menu.
;;
;; REPLACE ORIGINAL `Marks' menu in `dired.el'.
;;
(defvar diredp-menu-bar-marks-menu (make-sparse-keymap "Marks"))
(define-key dired-mode-map [menu-bar mark] (cons "Marks" diredp-menu-bar-marks-menu))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-mark-menu 'diredp-menu-bar-marks-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-mark-menu 'diredp-menu-bar-marks-menu "2017-04-09")

(define-key diredp-menu-bar-marks-menu [prev]
  '(menu-item "Previous Marked" dired-prev-marked-file :help "Move to previous marked file"))
(define-key diredp-menu-bar-marks-menu [next]
  '(menu-item "Next Marked" dired-next-marked-file :help "Move to next marked file"))
(define-key diredp-menu-bar-marks-menu [restore-markings]
  '(menu-item "Restore All Markings" diredp-restore-markings :help "Restore saved markings"))
(define-key diredp-menu-bar-marks-menu [save-markings]
  '(menu-item "Save All Markings" diredp-save-markings :help "Save all markings (not just `*') here"))
(define-key diredp-menu-bar-marks-menu [marks]
  '(menu-item "Change Mark..." dired-change-marks
    :help "Replace a given mark character with another"))
(define-key diredp-menu-bar-marks-menu [toggle-marks]
  (if (> emacs-major-version 21)
      '(menu-item "Toggle Marked/Unmarked" dired-toggle-marks
        :help "Mark unmarked files, unmark marked ones")
    '(menu-item "Toggle Marked/Unmarked" dired-toggle-marks
      :help "Mark unmarked files, unmark marked ones")))


;; `Marks' > `Tagged' menu.
;;
(when (require 'bookmark+ nil t)
  (defvar diredp-marks-tags-menu (make-sparse-keymap "Tagged (Autofiles)")
    "`Tags' submenu for Dired menu-bar `Marks' menu.")
  (define-key diredp-menu-bar-marks-menu [mark-tags] (cons "Tagged" diredp-marks-tags-menu))

  (define-key diredp-marks-tags-menu [diredp-unmark-files-tagged-none]
    '(menu-item "Unmark Not Tagged with Any..." diredp-unmark-files-tagged-none
      :help "Unmark files that are not tagged with *any* of the tags you enter"))
  (define-key diredp-marks-tags-menu [diredp-unmark-files-tagged-not-all]
    '(menu-item "Unmark Not Tagged with All..." diredp-unmark-files-tagged-not-all
      :help "Unmark files that are not tagged with *all* tags"))
  (define-key diredp-marks-tags-menu [diredp-unmark-files-tagged-some]
    '(menu-item "Unmark Tagged with Some..." diredp-unmark-files-tagged-some
      :help "Unmark files that are tagged with at least one of the tags you enter"))
  (define-key diredp-marks-tags-menu [diredp-unmark-files-tagged-all]
    '(menu-item "Unmark Tagged with All..." diredp-unmark-files-tagged-all
      :help "Unmark files that are tagged with *each* tag you enter"))
  (define-key diredp-marks-tags-menu [diredp-unmark-files-tagged-regexp]
    '(menu-item "Unmark Tagged Matching Regexp..." diredp-unmark-files-tagged-regexp
      :help "Unmark files that have at least one tag that matches a regexp"))
  (define-key diredp-marks-tags-menu [separator-marks-tags] '("--")) ; -------------------------

  (define-key diredp-marks-tags-menu [diredp-mark-files-tagged-none]
    '(menu-item "Mark Not Tagged with Any..." diredp-mark-files-tagged-none
      :help "Mark files that are not tagged with *any* of the tags you enter"))
  (define-key diredp-marks-tags-menu [diredp-mark-files-tagged-not-all]
    '(menu-item "Mark Not Tagged with All..." diredp-mark-files-tagged-not-all
      :help "Mark files that are not tagged with *all* tags"))
  (define-key diredp-marks-tags-menu [diredp-mark-files-tagged-some]
    '(menu-item "Mark Tagged with Some..." diredp-mark-files-tagged-some
      :help "Mark files that are tagged with at least one of the tags you enter"))
  (define-key diredp-marks-tags-menu [diredp-mark-files-tagged-all]
    '(menu-item "Mark Tagged with All..." diredp-mark-files-tagged-all
      :help "Mark files that are tagged with *each* tag you enter"))
  (define-key diredp-marks-tags-menu [diredp-mark-files-tagged-regexp]
    '(menu-item "Mark Tagged Matching Regexp..." diredp-mark-files-tagged-regexp
      :help "Mark files that have at least one tag that matches a regexp")))


;; `Marks' > `Omit' menu.
;;
(defvar diredp-marks-omit-menu (make-sparse-keymap "Omit")
  "`Omit' submenu for Dired menu-bar `Marks' menu.")
(define-key diredp-menu-bar-marks-menu [marks-omit] (cons "Omit" diredp-marks-omit-menu))

(define-key diredp-marks-omit-menu [marks-omit-unmarked]
  '(menu-item "Omit Unmarked" diredp-omit-unmarked :help "Hide lines of unmarked files"))
(define-key diredp-marks-omit-menu [marks-omit-marked]
  '(menu-item "Omit Marked" diredp-omit-marked :help "Hide lines of marked files"))


;; `Marks' > `Flag' menu.
;;
(defvar diredp-marks-flag-menu (make-sparse-keymap "Flag")
  "`Flag' submenu for Dired menu-bar `Marks' menu.")
(define-key diredp-menu-bar-marks-menu [mark-flag] (cons "Flag" diredp-marks-flag-menu))

(define-key diredp-marks-flag-menu [marks-flag-extension]
  '(menu-item "Flag Extension..." dired-flag-extension ; In `dired-x.el'
              :help "Flag all files that have a certain extension, for deletion"))
(define-key diredp-marks-flag-menu [marks-flag-garbage-files]
  '(menu-item "Flag Garbage Files" dired-flag-garbage-files
    :help "Flag unneeded files for deletion"))
(define-key diredp-marks-flag-menu [marks-flag-backup-files]
  '(menu-item "Flag Backup Files" dired-flag-backup-files
    :help "Flag all backup files for deletion"))
(define-key diredp-marks-flag-menu [marks-flag-auto-save-files]
  '(menu-item "Flag Auto-save Files" dired-flag-auto-save-files
    :help "Flag auto-save files for deletion"))
(define-key diredp-marks-flag-menu [marks-flag-region]
  '(menu-item "Flag Region" diredp-flag-region-files-for-deletion
    :visible (diredp-nonempty-region-p)
    :help "Flag all files in the region (selection) for deletion"))
(when (< emacs-major-version 21)
  (put 'diredp-flag-region-files-for-deletion 'menu-enable '(diredp-nonempty-region-p)))
(define-key diredp-marks-flag-menu [marks-flag-deletion]
  '(menu-item "Flag This" dired-flag-file-deletion
    :visible (not (diredp-nonempty-region-p))
    :help "Flag current line's file for deletion"))


;; `Marks' > `Unmark' menu.
;;
(defvar diredp-marks-unmark-menu (make-sparse-keymap "Unmark")
  "`Unmark' submenu for Dired menu-bar `Marks' menu.")
(define-key diredp-menu-bar-marks-menu [mark-mark] (cons "Unmark" diredp-marks-unmark-menu))

(define-key diredp-marks-unmark-menu [unmark-regexp-default-dir]
  '(menu-item "Unmark Name Matching Regexp (Default Dir)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "UNmark files (regexp): ") ?\040 nil))
              :help "Unmark files matching regexp with default directory"
              :keys "C-u C-u \\[dired-mark-files-regexp]"))
(define-key diredp-marks-unmark-menu [unmark-regexp-absolute]
  '(menu-item "Unmark Name Matching Regexp (Absolute)..."
              (lambda () (interactive)
                (dired-mark-files-regexp
                 (diredp-read-regexp "UNmark files (regexp): ")
                 ?\040
                 t))
              :help "Unmark files matching regexp against absolute file name"
              :keys "M-0 \\[dired-mark-files-regexp]"))
(define-key diredp-marks-unmark-menu [unmark-regexp-relative]
  '(menu-item "Unmark Name Matching Regexp (Relative)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "UNmark files (regexp): ") ?\040 'no-dir))
              :help "Unmark files matching regexp against relative file name"
              :keys "C-u \\[dired-mark-files-regexp]"))
(define-key diredp-marks-unmark-menu [unmark-autofiles]
  '(menu-item "Unmark Autofiles" diredp-unmark-autofiles
    :help "Unmark all autofiles (bookmarks with same name as file)"
    :enable (featurep 'bookmark+)))
(define-key diredp-marks-unmark-menu [unmark-all]
  '(menu-item "Unmark All" dired-unmark-all-marks :help "Remove all marks from all files"))
(define-key diredp-marks-unmark-menu [unmark-with]
  '(menu-item "Unmark Marked-With..." dired-unmark-all-files
    :help "Remove a specific mark (or all marks) from every file"))
(define-key diredp-marks-unmark-menu [unmark-region]
  '(menu-item "Unmark Region" diredp-unmark-region-files
    :visible (diredp-nonempty-region-p)
    :help "Unmark all files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-unmark-region-files 'menu-enable '(diredp-nonempty-region-p)))
(define-key diredp-marks-unmark-menu [unmark-this]
  '(menu-item "Unmark This" dired-unmark
    :visible (not (diredp-nonempty-region-p))
    :help "Unmark or unflag current line's file"))


;; `Marks' > `Mark' menu.
;;
(defvar diredp-marks-mark-menu (make-sparse-keymap "Mark")
  "`Mark' submenu for Dired menu-bar `Marks' menu.")
(define-key diredp-menu-bar-marks-menu [marks-mark] (cons "Mark" diredp-marks-mark-menu))

(define-key diredp-marks-mark-menu [marks-mark-regexp-default-dir]
  '(menu-item "Mark Name Matching Regexp (Default Dir)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "Mark files (regexp): ") dired-marker-char nil))
              :help "Mark files matching regexp with default directory"
              :keys "M-9 \\[dired-mark-files-regexp]"))
(define-key diredp-marks-mark-menu [marks-mark-regexp-absolute]
  '(menu-item "Mark Name Matching Regexp (Absolute)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "Mark files (regexp): ") dired-marker-char t))
              :help "Mark files matching regexp against absolute file name"
              :keys "M-- \\[dired-mark-files-regexp]"))
(define-key diredp-marks-mark-menu [marks-mark-regexp-relative]
  '(menu-item "Mark Name Matching Regexp (Relative)..."
              (lambda () (interactive)
                (dired-mark-files-regexp (diredp-read-regexp "Mark files (regexp): ") dired-marker-char 'no-dir))
              :help "Mark files matching regexp against relative file name"
              :keys "\\[dired-mark-files-regexp]"))
(define-key diredp-marks-mark-menu [marks-mark-containing]
  '(menu-item "Mark Content Matching Regexp..." dired-mark-files-containing-regexp
    :help "Mark files whose contents matches regexp"))
(define-key diredp-marks-mark-menu [marks-mark-sexp]
  '(menu-item "Mark If..." dired-mark-sexp ; In `dired-x.el'.
              :help "Mark files that satisfy condition specified by a sexp"))
(define-key diredp-marks-mark-menu [marks-image-dired-mark-tagged-files]
  '(menu-item "Mark Image Files Tagged..." image-dired-mark-tagged-files
    :enable (fboundp 'image-dired-mark-tagged-files) ; In `image-dired.el'.
    :help "Mark image files whose image tags match regexp"))
(when (fboundp 'dired-mark-omitted)     ; In `dired-x.el', Emacs 22+.
  (define-key diredp-marks-mark-menu [marks-mark-omitted]
    '(menu-item "Mark Omitted..." dired-mark-omitted
      :help "Mark all omitted files and subdirectories")))
(define-key diredp-marks-mark-menu [marks-mark-extension]
  '(menu-item "Mark Extension..." diredp-mark/unmark-extension
    :help "Mark all files with specified extension"))
(define-key diredp-marks-mark-menu [marks-mark-autofiles]
  '(menu-item "Mark Autofiles" diredp-mark-autofiles
    :help "Mark all autofiles (bookmarks with same name as file)"
    :enable (featurep 'bookmark+)))
(define-key diredp-marks-mark-menu [marks-mark-symlinks]
  '(menu-item "Mark Symlinks" dired-mark-symlinks
    :visible (fboundp 'make-symbolic-link) :help "Mark all symbolic links"))
(define-key diredp-marks-mark-menu [marks-mark-directories]
  '(menu-item "Mark Directories" dired-mark-directories
    :help "Mark all directories except `.' and `..'"))
(define-key diredp-marks-mark-menu [marks-mark-directory]
  '(menu-item "Mark Old Backups" dired-clean-directory
    :help "Flag old numbered backups for deletion"))
(define-key diredp-marks-mark-menu [marks-mark-executables]
  '(menu-item "Mark Executables" dired-mark-executables :help "Mark all executable files"))
(define-key diredp-marks-mark-menu [diredp-mark-with-char]
  '(menu-item "Mark This with Char..." diredp-mark-with-char
              :visible (not (diredp-nonempty-region-p))
              :help "Mark this line with a character you type"))
(define-key diredp-marks-mark-menu [diredp-mark-region-files-with-char]
  '(menu-item "Mark Region with Char..." diredp-mark-region-files-with-char
              :visible (diredp-nonempty-region-p)
              :help "Mark lines in region with a character you type"))
(define-key diredp-marks-mark-menu [marks-mark-region]
  '(menu-item "Mark Region" diredp-mark-region-files
    :visible (diredp-nonempty-region-p)
    :help "Mark the lines in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-mark-region-files 'menu-enable '(diredp-nonempty-region-p)))
(define-key diredp-marks-mark-menu [marks-mark-this]
  '(menu-item "Mark This" dired-mark
    :visible (not (diredp-nonempty-region-p))
    :help "Mark current line"))


;; `Marks' > `Here and Below' menu.
;;
(defvar diredp-marks-recursive-menu (make-sparse-keymap "Here and Below"))
(define-key diredp-menu-bar-marks-menu [mark-recursive]
  (cons "Here and Below" diredp-marks-recursive-menu))

(define-key diredp-marks-recursive-menu [diredp-flag-auto-save-files-recursive]
    '(menu-item "Flag Auto-Save Files..." diredp-flag-auto-save-files-recursive
      :help "Flag all auto-save files for deletion, including those in marked subdirs"))
(when (fboundp 'diredp-unmark-all-marks-recursive) ; Emacs 22+
  (define-key diredp-marks-recursive-menu [diredp-change-marks-recursive]
    '(menu-item "Change Mark..." diredp-change-marks-recursive
      :help "Change all OLD marks to NEW marks, including those in marked subdirs"))
  (define-key diredp-marks-recursive-menu [diredp-unmark-all-files-recursive]
    '(menu-item "Unmark Marked-With..." diredp-unmark-all-files-recursive
      :help "Remove a given mark everywhere, including in marked subdirs"))
  (define-key diredp-marks-recursive-menu [diredp-unmark-all-marks-recursive]
    '(menu-item "Unmark All..." diredp-unmark-all-marks-recursive
      :help "Remove ALL marks everywhere, including in marked subdirs"))
  (define-key diredp-marks-recursive-menu [separator-1] '("--"))) ; ------------
(define-key diredp-marks-recursive-menu [diredp-mark-sexp-recursive]
  '(menu-item "If..." diredp-mark-sexp-recursive
    :help "Mark files satisfying specified condition, including those in marked subdirs"))
(define-key diredp-marks-recursive-menu [diredp-mark-files-containing-regexp-recursive]
  '(menu-item "Containing Regexp..." diredp-mark-files-containing-regexp-recursive
    :help "Mark all files with content matching a regexp, including in marked subdirs"))
(define-key diredp-marks-recursive-menu [diredp-mark-files-regexp-recursive]
  '(menu-item "Named Regexp (Absolute)..." diredp-mark-files-regexp-recursive
    :help "Mark all file names matching a regexp, including those in marked subdirs"))
(define-key diredp-marks-recursive-menu [diredp-mark-extension-recursive]
  '(menu-item "Extension..." diredp-mark-extension-recursive
    :help "Mark all files with a given extension, including those in marked subdirs"))
(define-key diredp-marks-recursive-menu [diredp-mark-autofiles-recursive]
  '(menu-item "Autofiles" diredp-mark-autofiles-recursive
    :help "Mark all files with a given extension, including those in marked subdirs"
    :enable (featurep 'bookmark+)))
(define-key diredp-marks-recursive-menu [diredp-mark-symlinks-recursive]
  '(menu-item "Symbolic Links" diredp-mark-symlinks-recursive
    :help "Mark all symbolic links, including those in marked subdirs"))
(define-key diredp-marks-recursive-menu [diredp-mark-directories-recursive]
  '(menu-item "Directories" diredp-mark-directories-recursive
    :help "Mark all directories, including those in marked subdirs"))
(define-key diredp-marks-recursive-menu [diredp-mark-executables-recursive]
  '(menu-item "Executables" diredp-mark-executables-recursive
    :help "Mark all executable files, including those in marked subdirs"))


;; "Dir" menu.
;;
;; REPLACE ORIGINAL `Subdir' menu in `dired.el'.
;;
(defvar diredp-menu-bar-dir-menu (make-sparse-keymap "Dir"))
(define-key dired-mode-map [menu-bar subdir] (cons "Dir" diredp-menu-bar-dir-menu))

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'diredp-menu-bar-subdir-menu 'diredp-dir-menu))
(diredp-make-obsolete-variable 'diredp-menu-bar-subdir-menu 'diredp-dir-menu "2017-04-09")

(when (boundp 'diredp-menu-bar-subdir-menu)
  (defalias 'diredp-menu-bar-subdir-menu diredp-menu-bar-subdir-menu))
(diredp-make-obsolete 'diredp-menu-bar-subdir-menu 'diredp-dir-menu "2017-04-09")


;; `Dir' > `Hide/Show' menu.
;;
(defvar diredp-hide/show-menu (make-sparse-keymap "Hide/Show")
  "`Hide/Show' submenu for Dired menu-bar `Dir' menu.")
(define-key diredp-menu-bar-dir-menu [hide-show] (cons "Hide/Show" diredp-hide/show-menu))

(when (fboundp 'dired-omit-mode)
  (define-key diredp-hide/show-menu [dired-omit-mode]
    '(menu-item "Hide/Show Uninteresting (Omit Mode)" dired-omit-mode
      :help "Toggle omission of uninteresting files (Omit mode)")))
(when (fboundp 'dired-hide-details-mode) ; Emacs 24.4+
  (define-key diredp-hide/show-menu [hide-details]
    '(menu-item "Hide/Show Details" dired-hide-details-mode
      :help "Hide or show less important fields of directory listing")))
(define-key diredp-hide/show-menu [hide-all]
  '(menu-item "Hide/Show All Subdirs" dired-hide-all
    :help "Hide all subdirectories, leave only header lines"))
(define-key diredp-hide/show-menu [hide-subdir]
  '(menu-item "Hide/Show Subdir" diredp-hide-subdir-nomove
    :help "Hide or unhide current directory listing"))


;; `Dir' > `Bookmark' menu.
;;
(defvar diredp-bookmark-menu (make-sparse-keymap "Bookmark")
  "`Bookmark' submenu for Dired menu-bar `Dir' menu.")
(define-key diredp-menu-bar-dir-menu [bookmark] (cons "Bookmark" diredp-bookmark-menu))

(define-key diredp-bookmark-menu [diredp-highlight-autofiles-mode]
  '(menu-item "Toggle Autofile Highlighting" diredp-highlight-autofiles-mode
    :help "Toggle whether to highlight autofile bookmarks"
    :visible (and (featurep 'bookmark+)  (featurep 'highlight))))
(define-key diredp-bookmark-menu [diredp-do-bookmark-dirs-recursive]
    '(menu-item "Bookmark Dirs Here and Below..." diredp-do-bookmark-dirs-recursive
      :help "Bookmark this Dired buffer and marked subdirectory Dired buffers, recursively."))
(define-key diredp-bookmark-menu [bookmark-dired]
  '(menu-item "Bookmark Dired Buffer..." bookmark-set :help "Bookmark this Dired buffer"))


;; `Dir' > `Navigate' menu.
;;
(defvar diredp-navigate-menu (make-sparse-keymap "Navigate")
  "`Navigate' submenu for Dired menu-bar `Dir' menu.")
(define-key diredp-menu-bar-dir-menu [navigate] (cons "Navigate" diredp-navigate-menu))

(define-key diredp-navigate-menu [insert]
  '(menu-item "Move To This Subdir" dired-maybe-insert-subdir
    :help "Move to subdirectory line or listing"))
(define-key diredp-navigate-menu [tree-down]
  '(menu-item "Tree Down" dired-tree-down :help "Go to first subdirectory header down the tree"))
(define-key diredp-navigate-menu [tree-up]
  '(menu-item "Tree Up" dired-tree-up :help "Go to first subdirectory header up the tree"))
(define-key diredp-navigate-menu [up]
  '(menu-item "Up Directory" diredp-up-directory :help "Dired the parent directory"))
(define-key diredp-navigate-menu [prev-subdir]
  '(menu-item "Prev Subdir" diredp-prev-subdir :help "Go to previous subdirectory header line"))
(define-key diredp-navigate-menu [next-subdir]
  '(menu-item "Next Subdir" diredp-next-subdir :help "Go to next subdirectory header line"))
(define-key diredp-navigate-menu [prev-dirline]
  '(menu-item "Prev Dirline" diredp-prev-dirline :help "Move to previous directory-file line"))
(define-key diredp-navigate-menu [next-dirline]
  '(menu-item "Next Dirline" diredp-next-dirline :help "Move to next directory-file line"))

(define-key diredp-menu-bar-dir-menu [separator-subdir] '("--")) ; --------------------------

(define-key diredp-menu-bar-dir-menu [image-dired-dired-toggle-marked-thumbs]
  '(menu-item "Toggle Image Thumbnails" image-dired-dired-toggle-marked-thumbs
    :enable (fboundp 'image-dired-dired-toggle-marked-thumbs)
    :help "Add or remove image thumbnails in front of marked file names"))
(when (fboundp 'dired-isearch-filenames) ; Emacs 23+
  (define-key diredp-menu-bar-dir-menu [isearch-filenames-regexp]
    '(menu-item "Isearch Regexp in File Names..." dired-isearch-filenames-regexp
      :help "Incrementally search for regexp in file names only"))
  (define-key diredp-menu-bar-dir-menu [isearch-filenames]
    '(menu-item "Isearch in File Names..." dired-isearch-filenames
      :help "Incrementally search for literal text in file names only.")))
(when (or (> emacs-major-version 21)  (fboundp 'wdired-change-to-wdired-mode))
  (define-key diredp-menu-bar-dir-menu [wdired-mode]
    '(menu-item "Edit File Names (WDired)" wdired-change-to-wdired-mode
      :help "Put a Dired buffer in a mode in which filenames are editable"
      :keys "C-x C-q" :filter (lambda (x) (and (derived-mode-p 'dired-mode)  x)))))
(define-key diredp-menu-bar-dir-menu [diredp-yank-files]
  '(menu-item "Paste Files from Copied Absolute Names" diredp-yank-files
    :help "Paste files here whose absolute names you copied"
    :enable (catch 'dir-menu--yank-files
              (let ((files  (car kill-ring-yank-pointer)))
                (and (stringp files)
                     (dolist (file  (split-string files))
                       (unless (file-name-absolute-p file) (throw 'dir-menu--yank-files nil)))))
              t)))
(when (fboundp 'dired-compare-directories) ; Emacs 22+
  (define-key diredp-menu-bar-dir-menu [compare-directories]
    '(menu-item "Compare Directories..." dired-compare-directories
      :help "Mark files with different attributes in two Dired buffers")))

(define-key diredp-menu-bar-dir-menu [separator-dired-misc] '("--")) ; --------------------

(define-key diredp-menu-bar-dir-menu [diredp-sort-arbitrary-command]
  '(menu-item "Sort Arbitrary File List..." diredp-sort-arbitrary-command
    :enable (consp dired-directory)
    :help "Sort Dired buffer of arbitrary files"))
(define-key diredp-menu-bar-dir-menu [diredp-dired-recent-dirs]
  '(menu-item "Dired Recent Directories..." diredp-dired-recent-dirs
    :visible (boundp 'recentf-list) :enable  (and (boundp 'recentf-list)  (consp recentf-list))
    :help "Open a Dired buffer for recently visited directories"))
(define-key diredp-menu-bar-dir-menu [diredp-dired-recent-files]
  '(menu-item "Dired Recent Files..." diredp-dired-recent-files
    :visible (boundp 'recentf-list) :enable  (and (boundp 'recentf-list)  (consp recentf-list))
    :help "Open a Dired buffer for recently visited files and directories"))
(define-key diredp-menu-bar-dir-menu [diredp-dired-inserted-subdirs]
  '(menu-item "Dired Each Inserted Subdir..." diredp-dired-inserted-subdirs
    :enable (cdr dired-subdir-alist)    ; First elt is current dir.  Must have at least one more.
    :help "Open Dired separately for each of the inserted subdirectories"))
(define-key diredp-menu-bar-dir-menu [diredp-add-to-this-dired-buffer]
  '(menu-item "Add Entries Here..." diredp-add-to-this-dired-buffer
    :help "Add individual file and directory names to the listing"))
(define-key diredp-menu-bar-dir-menu [diredp-dired-union]
  '(menu-item "Dired Union..." diredp-dired-union
    :help "Open Dired for the union of some existing Dired buffers"))
(define-key diredp-menu-bar-dir-menu [diredp-fileset-other-window]
  '(menu-item "Dired Fileset..." diredp-fileset-other-window
    :enable (> emacs-major-version 21) :help "Open Dired on an Emacs fileset"))
(define-key diredp-menu-bar-dir-menu [diredp-dired-for-files]
  '(menu-item "Dired Files Located Anywhere" diredp-dired-for-files
    :help "Open Dired on specific files whose names you provide"))

(define-key diredp-menu-bar-dir-menu [separator-dired-on-set] '("--")) ; --------------------

(define-key diredp-menu-bar-dir-menu [diredp-define-snapshot-dired-commands]
  '(menu-item "Define Dired Commands for Marked Files" diredp-define-snapshot-dired-commands
    :help "Define commands to dired the marked files"))
(define-key diredp-menu-bar-dir-menu [diredp-marked-other-window]
  '(menu-item "Dired Marked Files in Other Window" diredp-marked-other-window
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files only, in other window"))
(define-key diredp-menu-bar-dir-menu [diredp-marked]
  '(menu-item "Dired Marked Files" diredp-marked
    :enable (save-excursion (goto-char (point-min))
                            (and (re-search-forward (dired-marker-regexp) nil t)
                                 (re-search-forward (dired-marker-regexp) nil t)))
    :help "Open Dired on marked files only"))
(define-key diredp-menu-bar-dir-menu [dired]
  '(menu-item "Dired (Filter via Wildcards)..." dired
    :help "Explore a directory (you can provide wildcards)"))
(define-key diredp-menu-bar-dir-menu [diredp-change-ls-switches]
  '(menu-item "Change `ls' switches..." diredp-change-ls-switches
    :help "Change the `ls' switches for this Dired buffer"))

(define-key diredp-menu-bar-dir-menu [separator-dired] '("--")) ; ---------------------

(define-key diredp-menu-bar-dir-menu [diredp-create-file-here]
  '(menu-item "New (Empty) File Here..." diredp-create-file-here
              :help "Create a new empty file in this (sub)directory"))
(define-key diredp-menu-bar-dir-menu [create-directory] ; Moved from "Immediate".
  '(menu-item "New Directory..." dired-create-directory :help "Create a directory"))

(define-key diredp-menu-bar-dir-menu [separator-create-new] '("--")) ; ---------------------

(define-key diredp-menu-bar-dir-menu [insert]
  '(menu-item "Insert/Move-To This Subdir" dired-maybe-insert-subdir
    :help "Move to subdirectory line or listing"))
(define-key diredp-menu-bar-dir-menu [dired-undo]
  '(menu-item "Undo" dired-undo :help "Undo changes: marks, killed lines, and subdir listings"))
(define-key diredp-menu-bar-dir-menu [revert]
  '(menu-item "Refresh (Sync \& Show All)" revert-buffer :help "Update directory contents"))


;;; Mouse-3 menu binding.
(define-key dired-mode-map [down-mouse-3] 'diredp-mouse-3-menu)
(define-key dired-mode-map [mouse-3]      'ignore)


;;; Non-menu Dired bindings.

;; Move `dired-omit-mode' to `C-x M-o', so prefix key `M-o' is free for face/font-lock stuff.
(define-key dired-mode-map "\C-x\M-o" (if (fboundp 'dired-omit-mode) 'dired-omit-mode 'dired-omit-toggle))
(when (memq (lookup-key dired-mode-map "\M-o") '(dired-omit-mode dired-omit-toggle))
  (define-key dired-mode-map "\M-o" nil))

;; These are global, not just Dired mode.  They are on prefix key `C-x D'.
(unless (lookup-key ctl-x-map "D")
  (define-key ctl-x-map   "D" nil)      ; For Emacs 20
  (define-key ctl-x-map   "DA" 'diredp-add-to-dired-buffer)                ; `C-x D A'
  (define-key ctl-x-map   "DF" 'diredp-dired-for-files)                    ; `C-x D F'
  (define-key ctl-x-map   "DR" 'diredp-dired-recent-files)                 ; `C-x D R'
  (define-key ctl-x-map   "Dr" 'diredp-dired-recent-dirs)                  ; `C-x D r'
  (define-key ctl-x-map   "DS" 'diredp-fileset)                            ; `C-x D S'
  (define-key ctl-x-map   "DU" 'diredp-dired-union))                       ; `C-x D U'

(unless (lookup-key ctl-x-4-map "D")
  (define-key ctl-x-4-map "D" nil)      ; For Emacs 20
  (define-key ctl-x-4-map "DA" 'diredp-add-to-dired-buffer-other-window)   ; `C-x 4 D A'
  (define-key ctl-x-4-map "DF" 'diredp-dired-for-files-other-window)       ; `C-x 4 D F'
  (define-key ctl-x-4-map "DR" 'diredp-dired-recent-files-other-window)    ; `C-x 4 D R'
  (define-key ctl-x-4-map "Dr" 'diredp-dired-recent-dirs-other-window)     ; `C-x 4 D r'
  (define-key ctl-x-4-map "DS" 'diredp-fileset-other-window)               ; `C-x 4 D S'
  (define-key ctl-x-4-map "DU" 'diredp-dired-union-other-window))          ; `C-x 4 D U'

;; Navigation
(substitute-key-definition 'dired-up-directory 'diredp-up-directory dired-mode-map)
(substitute-key-definition 'dired-next-line 'diredp-next-line dired-mode-map)
(substitute-key-definition 'dired-previous-line 'diredp-previous-line dired-mode-map)
(substitute-key-definition 'dired-next-dirline 'diredp-next-dirline dired-mode-map)
(substitute-key-definition 'dired-prev-dirline 'diredp-prev-dirline dired-mode-map)
(substitute-key-definition 'dired-next-subdir 'diredp-next-subdir dired-mode-map)
(substitute-key-definition 'dired-prev-subdir 'diredp-prev-subdir dired-mode-map)


(define-key dired-mode-map [S-down-mouse-1] 'ignore) ; (normally `mouse-set-font')
;; `diredp-mouse-mark-region-files' provides Windows-Explorer behavior
;; for selecting (marking) files.
(define-key dired-mode-map [S-mouse-1] 'diredp-mouse-mark-region-files)     ; `S-mouse-1'
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file-other-window)   ; `mouse-2'
;; But be aware that `dired-sort-menu.el' binds `S-mouse-2' to `dired-sort-menu-popup'.
(define-key dired-mode-map [S-down-mouse-2] 'dired-mouse-find-file)         ; `S-mouse-2'
(define-key dired-mode-map [S-mouse-2] 'ignore)
(define-key dired-mode-map [M-mouse-2] 'diredp-mouse-find-file-other-frame) ; `M-mouse-2'

;; On Windows, bind more.
(eval-after-load "w32-browser"
  '(progn
    (define-key dired-mode-map [(control return)] 'dired-w32explore)                ; `C-RET'
    (define-key dired-mode-map [(meta return)] 'dired-w32-browser)                  ; `M-RET'
    (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)                  ; `mouse-2'
    (define-key dired-mode-map (kbd "<C-M-return>") 'dired-multiple-w32-browser)))  ; `C-M-RET'

(when (fboundp 'diredp-w32-drives)
  (when (< emacs-major-version 21) (define-key dired-mode-map ":"    nil)) ; For Emacs 20
  (define-key dired-mode-map ":/"    'diredp-w32-drives))                            ; `:/'

;; Other keyboard keys
;;
(define-key dired-mode-map "@"           nil) ; For Emacs 20
(define-key dired-mode-map "@@"          'diredp-do-apply-to-marked)                ; `@ @'
;;; (define-key dired-mode-map (kbd "@ M-@") 'diredp-do-invoke-in-marked)               ; `@ M-@'
(define-key dired-mode-map (kbd "@ M-x") 'diredp-do-command-in-marked)              ; `@ M-x'
(define-key dired-mode-map (kbd "@ M-:") 'diredp-do-eval-in-marked)                 ; `@ M-:'
(define-key dired-mode-map "$"       'diredp-hide-subdir-nomove)                    ; `$'
(define-key dired-mode-map "\M-$"    'dired-hide-subdir)                            ; `M-$'
(define-key dired-mode-map "="       'diredp-ediff)                                 ; `='
;; This replaces the `dired-x.el' binding of `dired-mark-extension'.
(define-key dired-mode-map "*."      'diredp-mark/unmark-extension)                 ; `* .'
(define-key dired-mode-map "*B"      'diredp-mark-autofiles)                        ; `* B'
(define-key dired-mode-map "*R"      'diredp-restore-markings)                      ; `* R'
(define-key dired-mode-map "*S"      'diredp-save-markings)                         ; `* S'
(define-key dired-mode-map [(control meta ?*)] 'diredp-marked-other-window)         ; `C-M-*'
(define-key dired-mode-map "\M-a"    'dired-do-search)                              ; `M-a'
(define-key dired-mode-map "\M-b"    'diredp-do-bookmark)                           ; `M-b'
(define-key dired-mode-map "\C-\M-b" 'diredp-set-bookmark-file-bookmark-for-marked) ; `C-M-b'
(when diredp-bind-problematic-terminal-keys
  (define-key dired-mode-map [(control meta shift ?b)]                              ; `C-M-B' (aka `C-M-S-b')
    'diredp-do-bookmark-in-bookmark-file))
(define-key dired-mode-map "e"       'diredp-visit-this-file)                       ; `e' (was `dired-find-file')
(define-key dired-mode-map [C-down]  'diredp-visit-next-file)                 ; `C-down' (was `forward-paragraph')
(define-key dired-mode-map [C-up]    'diredp-visit-previous-file)             ; `C-up' (was `backward-paragraph')
(define-key dired-mode-map "\C-\M-g" 'diredp-do-grep)                               ; `C-M-g'
(when (fboundp 'mkhtml-dired-files)     ; In `mkhtml.el'.
  (define-key dired-mode-map "\M-h"  'mkhtml-dired-files))                          ; `M-h'
(define-key dired-mode-map "\C-\M-i" 'diredp-dired-inserted-subdirs)                ; `C-M-i'
(define-key dired-mode-map "\M-q"    (if (< emacs-major-version 21)
                                         'dired-do-query-replace
                                       'dired-do-query-replace-regexp))             ; `M-q'
(when diredp-bind-problematic-terminal-keys
  (define-key dired-mode-map [(control meta shift ?r)]                              ; `C-M-R' (aka `C-M-S-r')
    'diredp-toggle-find-file-reuse-dir))
(define-key dired-mode-map "U"       'dired-unmark-all-marks)                       ; `U'
(substitute-key-definition 'describe-mode 'diredp-describe-mode                     ; `h', `C-h m'
                           dired-mode-map (current-global-map))
(substitute-key-definition 'dired-mark 'diredp-mark dired-mode-map)                 ; `m', `* m'

;; Tags - same keys as in `*Bookmark List*'.
;;
;; NOTE: If this changes then need to update `dired-sort-menu+.el' to reflect the changes.
;;
(define-key dired-mode-map "T"       nil) ; For Emacs 20
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


;; Vanilla Emacs binds `c' to `dired-do-compress-to'.  Use `M-z' instead'.
;; (`dired-sort-menu.el' binds `c' to `dired-sort-menu-toggle-ignore-case'.)
;;
(when (fboundp 'dired-do-compress-to) ; Emacs 25+
  (define-key dired-mode-map (kbd "M-z") 'dired-do-compress-to))


;; Commands for operating on the current line's file.  When possible,
;; these are lower-case versions of the upper-case commands for operating on
;; the marked files.  (Most of the other corresponding lower-case letters are already
;; defined and cannot be used here.)

;; $$$$$$ (define-key dired-mode-map [(control meta ?+)] 'diredp-tag-this-file)
;; $$$$$$ (define-key dired-mode-map [(control meta ?-)] 'diredp-untag-this-file)
(define-key dired-mode-map "\r"      'dired-find-file)                      ; `RET'
(when (fboundp 'diredp-describe-file)
  (define-key dired-mode-map (kbd "C-h RET")        'diredp-describe-file)  ; `C-h RET'
  (define-key dired-mode-map (kbd "C-h C-<return>") 'diredp-describe-file)) ; `C-h C-RET'
(define-key dired-mode-map "%c"      'diredp-capitalize)                    ; `% c'
(define-key dired-mode-map "b"       'diredp-byte-compile-this-file)        ; `b'
(define-key dired-mode-map [(control shift ?b)] 'diredp-bookmark-this-file) ; `C-B'
(define-key dired-mode-map "\M-c"    'diredp-capitalize-this-file)          ; `M-c'
(when (and (fboundp 'diredp-chgrp-this-file)  diredp-bind-problematic-terminal-keys)
  (define-key dired-mode-map [(control meta shift ?g)] 'diredp-chgrp-this-file)) ; `C-M-G' (aka `C-M-S-g')
(define-key dired-mode-map "\M-i"    'diredp-insert-subdirs)                ; `M-i'
(define-key dired-mode-map "\M-l"    'diredp-downcase-this-file)            ; `M-l'
(define-key dired-mode-map "\C-\M-l" 'diredp-list-marked)                   ; `C-M-l'
(define-key dired-mode-map [(control meta shift ?l)] 'diredp-sort-arbitrary-command) ; `C-M-L' (aka `C-M-S-l')
(when diredp-bind-problematic-terminal-keys
  (define-key dired-mode-map [(meta shift ?m)] 'diredp-chmod-this-file))    ; `M-M' (aka `M-S-m')
(define-key dired-mode-map "\C-o"    'diredp-find-file-other-frame)         ; `C-o'
(when (and (fboundp 'diredp-chown-this-file)  diredp-bind-problematic-terminal-keys)
  (define-key dired-mode-map [(meta shift ?o)] 'diredp-chown-this-file))    ; `M-O' (aka `M-S-o')
(define-key dired-mode-map "\C-\M-o" 'dired-display-file)                   ; `C-M-o' (not `C-o')
(define-key dired-mode-map "\M-p"    'diredp-print-this-file)               ; `M-p'
(define-key dired-mode-map "r"       'diredp-rename-this-file)              ; `r'
(when (fboundp 'image-dired-dired-display-image)
  (define-key dired-mode-map "\C-tI"   'diredp-image-show-this-file))       ; `C-t I'
(when diredp-bind-problematic-terminal-keys
  (define-key dired-mode-map [(meta shift ?t)] 'diredp-touch-this-file)     ; `M-T' (aka `M-S-t')
  (define-key dired-mode-map [(control meta shift ?t)] 'dired-do-touch))    ; `C-M-T' (aka `C-M-S-t')
(define-key dired-mode-map "\M-u"    'diredp-upcase-this-file)              ; `M-u'
(define-key dired-mode-map "y"       'diredp-relsymlink-this-file)          ; `y'
(define-key dired-mode-map "\C-w"    'diredp-move-files-named-in-kill-ring) ; `C-w'
(define-key dired-mode-map "\C-y"    'diredp-yank-files)                    ; `C-y'
(define-key dired-mode-map "z"       'diredp-compress-this-file)            ; `z'
(when (fboundp 'dired-show-file-type)
  (define-key dired-mode-map "_"      'dired-show-file-type))               ; `_' (underscore)
(substitute-key-definition 'kill-line 'diredp-delete-this-file              ; `C-k', `delete', `deleteline'
                           dired-mode-map (current-global-map))


;; Commands that handle marked below, recursively.
;; Use `M-+' as a prefix key for all such commands.

(define-prefix-command 'diredp-recursive-map)
(define-key dired-mode-map "\M-+"  diredp-recursive-map) ; `M-+'

(when (fboundp 'char-displayable-p)     ; Emacs 22+
  (define-key diredp-recursive-map "\M-\C-?"   'diredp-unmark-all-files-recursive))     ; `M-DEL'
(define-key diredp-recursive-map "@@"          'diredp-do-apply-to-marked-recursive)    ; `@ @'
;;; (define-key diredp-recursive-map (kbd "@ M-@") 'diredp-do-invoke-in-marked-recursive)   ; `@ M-@'
(define-key diredp-recursive-map (kbd "@ M-x") 'diredp-do-command-in-marked-recursive)  ; `@ M-x'
(define-key diredp-recursive-map (kbd "@ M-:") 'diredp-do-eval-in-marked-recursive)     ; `@ M-:'
(define-key diredp-recursive-map "#"           'diredp-flag-auto-save-files-recursive)  ; `#'
(define-key diredp-recursive-map "*@"          'diredp-mark-symlinks-recursive)         ; `* @'
(define-key diredp-recursive-map "**"          'diredp-mark-executables-recursive)      ; `* *'
(define-key diredp-recursive-map "*/"          'diredp-mark-directories-recursive)      ; `* /'
(define-key diredp-recursive-map "*."          'diredp-mark-extension-recursive)        ; `* .'
(define-key diredp-recursive-map "*("          'diredp-mark-sexp-recursive)             ; `* ('
(define-key diredp-recursive-map "*B"          'diredp-mark-autofiles-recursive)        ; `* B'
(when (fboundp 'char-displayable-p)     ; Emacs 22+
  (define-key diredp-recursive-map "*c"        'diredp-change-marks-recursive))         ; `* c'
(define-key diredp-recursive-map "*%"          'diredp-mark-files-regexp-recursive)     ; `* %'
(when (> emacs-major-version 22)
  (define-key diredp-recursive-map ":d"        'diredp-do-decrypt-recursive)            ; `: d'
  (define-key diredp-recursive-map ":e"        'diredp-do-encrypt-recursive)            ; `: e'
  (define-key diredp-recursive-map ":s"        'diredp-do-sign-recursive)               ; `: s'
  (define-key diredp-recursive-map ":v"        'diredp-do-verify-recursive))            ; `: v'
(define-key diredp-recursive-map "%c"          'diredp-capitalize-recursive)            ; `% c'
(define-key diredp-recursive-map "%g"          'diredp-mark-files-containing-regexp-recursive) ; `% g'
(define-key diredp-recursive-map "%l"          'diredp-downcase-recursive)              ; `% l'
(define-key diredp-recursive-map "%m"          'diredp-mark-files-regexp-recursive)     ; `% m'
(define-key diredp-recursive-map "%u"          'diredp-upcase-recursive)                ; `% u'
(when (fboundp 'dired-do-async-shell-command) ; Emacs 23+
  (define-key diredp-recursive-map "&"         'diredp-do-async-shell-command-recursive)) ; `&'
(define-key diredp-recursive-map "!"           'diredp-do-shell-command-recursive)      ; `!'
(define-key diredp-recursive-map (kbd "C-M-*") 'diredp-marked-recursive-other-window)   ; `C-M-*'
(define-key diredp-recursive-map "A"           'diredp-do-search-recursive)             ; `A'
(define-key diredp-recursive-map "\M-b"        'diredp-do-bookmark-recursive)           ; `M-b'
(when diredp-bind-problematic-terminal-keys
  (define-key diredp-recursive-map [(meta shift ?b)]                                    ; `M-B' (aka `M-S-b')
    'diredp-do-bookmark-dirs-recursive))
(define-key diredp-recursive-map (kbd "C-M-b")                                          ; `C-M-b'
  'diredp-set-bookmark-file-bookmark-for-marked-recursive)
(when diredp-bind-problematic-terminal-keys
  (define-key diredp-recursive-map [(control meta shift ?b)]                            ; `C-M-B' (aka `C-M-S-b')
    'diredp-do-bookmark-in-bookmark-file-recursive))
(define-key diredp-recursive-map "C"           'diredp-do-copy-recursive)               ; `C'
(define-key diredp-recursive-map "D"           'diredp-do-delete-recursive)             ; `D'
(define-key diredp-recursive-map "F"           'diredp-do-find-marked-files-recursive)  ; `F'
(when (fboundp 'diredp-do-chgrp-recursive)
  (define-key diredp-recursive-map "G"         'diredp-do-chgrp-recursive))             ; `G'
(define-key diredp-recursive-map "\C-\M-g"     'diredp-do-grep-recursive)               ; `C-M-g'
(define-key diredp-recursive-map "H"           'diredp-do-hardlink-recursive)           ; `H'
(define-key diredp-recursive-map "\M-i"        'diredp-insert-subdirs-recursive)        ; `M-i'
(define-key diredp-recursive-map "\C-\M-l"     'diredp-list-marked-recursive)           ; `C-M-l'
(define-key diredp-recursive-map "M"           'diredp-do-chmod-recursive)              ; `M'
(when (fboundp 'diredp-do-chown-recursive)
  (define-key diredp-recursive-map "O"         'diredp-do-chown-recursive))             ; `O'
(define-key diredp-recursive-map "P"           'diredp-do-print-recursive)              ; `P'
(define-key diredp-recursive-map "Q"         'diredp-do-query-replace-regexp-recursive) ; `Q'
(define-key diredp-recursive-map "R"           'diredp-do-move-recursive)               ; `R'
(define-key diredp-recursive-map "S"           'diredp-do-symlink-recursive)            ; `S'
(define-key diredp-recursive-map (kbd "M-s a C-s")                                      ; `M-s a C-s'
  'diredp-do-isearch-recursive)
(define-key diredp-recursive-map (kbd "M-s a C-M-s")                                    ; `M-s a C-M-s'
  'diredp-do-isearch-regexp-recursive)
(when diredp-bind-problematic-terminal-keys
  (define-key diredp-recursive-map [(control meta shift ?t)]
    'diredp-do-touch-recursive))                                                        ; `C-M-T' (aka `C-M-S-t')
(define-key diredp-recursive-map "\C-tc"   'diredp-image-dired-comment-files-recursive) ; `C-t c'
(define-key diredp-recursive-map "\C-td"  'diredp-image-dired-display-thumbs-recursive) ; `C-t d'
(define-key diredp-recursive-map "\C-tr"      'diredp-image-dired-delete-tag-recursive) ; `C-t r'
(define-key diredp-recursive-map "\C-tt"       'diredp-image-dired-tag-files-recursive) ; `C-t t'
(when (fboundp 'char-displayable-p)     ; Emacs 22+
  (define-key diredp-recursive-map "U"         'diredp-unmark-all-marks-recursive))     ; `U'
(define-key diredp-recursive-map "\M-("        'diredp-mark-sexp-recursive)             ; `M-('
(define-key diredp-recursive-map "\M-w"        'diredp-copy-filename-as-kill-recursive) ; `M-w'
(define-key diredp-recursive-map "Y"           'diredp-do-relsymlink-recursive)         ; `Y'

(eval-after-load "w32-browser"
  '(define-key diredp-recursive-map (kbd "<C-M-return>") 'diredp-multiple-w32-browser-recursive)) ; `C-M-RET'

;; Undefine some bindings that would try to modify a Dired buffer.  Their key sequences will
;; then appear to the user as available for local (Dired) definition.
(when (fboundp 'undefine-killer-commands) (undefine-killer-commands dired-mode-map))

;;;;;;;;;;;;

(setq diredp-loaded-p  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired+.el ends here

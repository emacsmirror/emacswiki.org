;;; dired+.el --- Extensions to Dired.
;;
;; Filename: dired+.el
;; Description: Extensions to Dired.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2011, Drew Adams, all rights reserved.
;; Created: Fri Mar 19 15:58:58 1999
;; Version: 21.2
;; Last-Updated: Fri Mar 25 15:32:06 2011 (-0700)
;;           By: dradams
;;     Update #: 3082
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dired+.el
;; Keywords: unix, mouse, directories, diredp, dired
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `custom', `dired', `dired+', `dired-aux', `dired-x',
;;   `easymenu', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind',
;;   `fit-frame', `info', `info+', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `strings', `thingatpt', `thingatpt+',
;;   `w32-browser', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to Dired
;;
;;  This file extends functionalities provided by standard GNU Emacs
;;  files `dired.el', `dired-aux.el', and `dired-x.el'.
;;
;;  Key bindings changed.  Menus redefined.  `diredp-mouse-3-menu'
;;  popup menu added.  New commands.  Some commands enhanced.
;;
;;  Additional suggested key bindings:
;;
;;    (define-key ctl-x-map   "d" 'diredp-dired-files)
;;    (define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)
;;
;;  Note: If you want a maximum or minimum fontification for Dired
;;  mode, then customize option `font-lock-maximize-decoration'.  If
;;  you want a different fontification level for Dired than for other
;;  modes, you can do this too by customizing
;;  `font-lock-maximize-decoration'.
;;
;;  Note:
;;
;;    Most of the commands (such as `C' and `M-g') that operate on
;;    marked files have the added feature here that multiple `C-u' use
;;    not the files that are marked or the next or previous N files,
;;    but *all* of the files in the Dired buffer.  Just what "all"
;;    files means changes with the number of `C-u', as follows:
;;
;;    `C-u C-u'         - Use all files present, but no directories.
;;    `C-u C-u C-u'     - Use all files and dirs except `.' and `..'.
;;    `C-u C-u C-u C-u' - use all files and dirs, `.' and `..'.
;;
;;    (More than four `C-u' act the same as two.)
;;
;;    This feature can be particularly useful when you have a Dired
;;    buffer with files from multiple directories.
;;
;;    Note that this behavior is described only in the doc string of
;;    function `dired-get-marked-files'.  It is *not* described in the
;;    doc strings of the various commands, because that would require
;;    redefining each command separately here.  Instead, we redefine
;;    only macro `dired-map-over-marks' and function
;;    `dired-get-filename' in order to achieve this effect.
;;
;;
;;  All new functions, variables, and faces defined here have the
;;  prefix `diredp-' (for Dired Plus) in their names.
;;
;;
;;  Options defined here:
;;
;;    `diff-switches', `diredp-w32-local-drives'.
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
;;    `diredp-no-priv', `diredp-number', `diredp-other-priv',
;;    `diredp-rare-priv', `diredp-read-priv', `diredp-symlink',
;;    `diredp-write-priv'.
;;
;;  Commands defined here:
;;
;;    `diredp-bookmark', `diredp-byte-compile-this-file',
;;    `diredp-capitalize', `diredp-capitalize-this-file',
;;    `diredp-chgrp-this-file', `diredp-chmod-this-file',
;;    `diredp-chown-this-file', `diredp-compress-this-file',
;;    `diredp-copy-this-file', `diredp-delete-this-file',
;;    `diredp-dired-files', `diredp-dired-files-other-window',
;;    `diredp-dired-union', `diredp-dired-union-other-window',
;;    `diredp-do-bookmark', `diredp-do-bookmark-in-bookmark-file',
;;    `diredp-do-grep', `diredp-downcase-this-file', `diredp-ediff',
;;    `diredp-fileset', `diredp-find-a-file',
;;    `diredp-find-a-file-other-frame',
;;    `diredp-find-a-file-other-window',
;;    `diredp-find-file-other-frame',
;;    `diredp-find-file-reuse-dir-buffer',
;;    `diredp-flag-region-files-for-deletion',
;;    `diredp-grep-this-file', `diredp-hardlink-this-file',
;;    `diredp-load-this-file', `diredp-marked',
;;    `diredp-marked-other-window', `diredp-mark-region-files',
;;    `diredp-mark/unmark-extension', `diredp-mouse-3-menu',
;;    `diredp-mouse-backup-diff', `diredp-mouse-diff',
;;    `diredp-mouse-do-bookmark', `diredp-mouse-do-byte-compile',
;;    `diredp-mouse-do-chgrp', `diredp-mouse-do-chmod',
;;    `diredp-mouse-do-chown', `diredp-mouse-do-compress',
;;    `diredp-mouse-do-copy', `diredp-mouse-do-delete',
;;    `diredp-mouse-do-grep', `diredp-mouse-do-hardlink',
;;    `diredp-mouse-do-load', `diredp-mouse-do-print',
;;    `diredp-mouse-do-rename', `diredp-mouse-do-shell-command',
;;    `diredp-mouse-do-symlink', `diredp-mouse-downcase',
;;    `diredp-mouse-ediff', `diredp-mouse-find-file',
;;    `diredp-mouse-find-file-other-frame',
;;    `diredp-mouse-find-file-reuse-dir-buffer',
;;    `diredp-mouse-flag-file-deletion', `diredp-mouse-mark',
;;    `diredp-mouse-mark-region-files', `diredp-mouse-mark/unmark',
;;    `diredp-mouse-unmark', `diredp-mouse-upcase',
;;    `diredp-mouse-view-file', `diredp-omit-marked',
;;    `diredp-omit-unmarked', `diredp-print-this-file',
;;    `diredp-relsymlink-this-file', `diredp-rename-this-file',
;;    `diredp-set-bookmark-file-bookmark-for-marked',
;;    `diredp-shell-command-this-file', `diredp-symlink-this-file',
;;    `diredp-toggle-find-file-reuse-dir',
;;    `diredp-toggle-marks-in-region', `diredp-unmark-region-files',
;;    `diredp-upcase-this-file', `diredp-w32-drives',
;;    `diredp-w32-drives-mode', `toggle-dired-find-file-reuse-dir'.
;;
;;  Non-interactive functions defined here:
;;
;;    `diredp-all-files', `diredp-do-grep-1',
;;    `diredp-fewer-than-2-files-p', `diredp-find-a-file-read-args',
;;    `diredp-dired-files-interactive-spec', `diredp-dired-union-1',
;;    `diredp-dired-union-interactive-spec',
;;    `diredp-make-find-file-keys-reuse-dirs',
;;    `diredp-make-find-file-keys-not-reuse-dirs',
;;    `direp-read-bookmark-file-args', `diredp-this-file-marked-p',
;;    `diredp-this-file-unmarked-p', `diredp-this-subdir'.
;;
;;  Variables defined here:
;;
;;    `diredp-file-line-overlay', `diredp-font-lock-keywords-1',
;;    `diredp-loaded-p', `diredp-menu-bar-immediate-menu',
;;    `diredp-menu-bar-mark-menu', `diredp-menu-bar-operate-menu',
;;    `diredp-menu-bar-regexp-menu', `diredp-menu-bar-subdir-menu',
;;    `diredp-w32-drives-mode-map'.
;;
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
;;  `dired-map-over-marks'    - Treat multiple `C-u' specially.
;;  `dired-readin-insert'     - Use t as WILDCARD arg to
;;                              `dired-insert-directory'.  (Emacs 23+,
;;                              and only for MS Windows)
;;  `dired-revert'            - Reset `mode-line-process' to nil.
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
;;; Change log:
;;
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
;;            diredp-do-bookmark-in-bookmark-file (C-M-S-b), direp-read-bookmark-file-args.
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
;;     Added: direp(-do)-bookmark.  Added to Multiple menu, and bound to M-b.
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
;;            toggle-dired-find-file-reuse-dir, dired+-subst-find-*.
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

(and (< emacs-major-version 21)
     (eval-when-compile (require 'cl))) ;; pop (plus, for Emacs <20: when, unless)

(require 'dired) ;; dired-revert
(require 'dired-aux) ;; dired-bunch-files, dired-do-chxxx, dired-do-create-files,
                     ;; dired-mark-read-string, dired-read-shell-command,
                     ;; dired-run-shell-command, dired-shell-stuff-it
(require 'ediff-util) ;; ediff-read-file-name

(require 'dired-x nil t) ;; (no error if not found) dired-do-relsymlink
(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(when (eq system-type 'windows-nt) ;; (no error if not found):
  (require 'w32-browser nil t)) ;; dired-w32explore, dired-w32-browser, dired-mouse-w32-browser,
                                ;; dired-multiple-w32-browser
(when (< emacs-major-version 21)
  (require 'mkhtml nil t)) ;; (no error if not found): mkhtml-dired-files

;; Don't require Icicles, else get recursive requires.
;; (require 'icicles nil t) ;; (no error if not found): icicle-read-string-completing

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired+)
(require 'dired+)                       ; Ensure loaded before compile this.

;; Quiet byte-compiler.
(defvar dired-switches-alist)
(defvar dired-subdir-switches)
(defvar dired-use-ls-dired) ; Emacs 22+
(defvar grep-use-null-device)

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
(defcustom diredp-w32-local-drives '(("C:" "Local disk"))
  "*Local MS Windows drives that you want to use for `diredp-w32-drives'.
Each entry is a list (DRIVE DESCRIPTION), where DRIVE is the drive
name and DESCRIPTION describes DRIVE."
  :type '(alist
          :key-type   (string        :tag "Drive name")
          :value-type (group (string :tag "Drive description")))
  :group 'Dired-Plus)

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
          multi-C-u case-fold-search found results)
      (when (and (consp ,arg) (> (prefix-numeric-value ,arg) 4))
        (setq arg        (case (prefix-numeric-value ,arg)
                           (16   'all-files-no-dirs) ; `C-u C-u'
                           (64   'all-files-no-dots) ; `C-u C-u C-u'
                           (256  'all-files) ; `C-u C-u C-u C-u'
                           (t    'all-files-no-dirs))
              multi-C-u  t))
      (if (and ,arg (not multi-C-u))
          (if (integerp ,arg)
              (progn;; no save-excursion, want to move point.
                (dired-repeat-over-lines
                 ,arg
                 (function (lambda ()
                   (if ,show-progress (sit-for 0))
                   (setq results  (cons ,body results)))))
                (if (< ,arg 0)
                    (nreverse results)
                  results))
            ;; non-nil, non-integer ARG means use current file:
            (list ,body))
        (let ((regexp  (dired-marker-regexp))
              next-position)
          (save-excursion
            (goto-char (point-min))
            ;; remember position of next marked file before BODY
            ;; can insert lines before the just found file,
            ;; confusing us by finding the same marked file again
            ;; and again and...
            (setq next-position  (and (if multi-C-u
                                          (diredp-get-file-or-dir-name arg)
                                        (re-search-forward regexp nil t))
                                      (point-marker))
                  found          (not (null next-position)))
            (while next-position
              (goto-char next-position)
              (if ,show-progress (sit-for 0))
              (setq results  (cons ,body results))
              ;; move after last match
              (goto-char next-position)
              (forward-line 1)
              (set-marker next-position nil)
              (setq next-position  (and (if multi-C-u
                                            (diredp-get-file-or-dir-name arg)
                                          (re-search-forward regexp nil t))
                                        (point-marker)))))
          (when (and ,distinguish-one-marked (= (length results) 1))
            (setq results  (cons t results)))
          (if found results (list ,body)))))
    ;; save-excursion loses, again
    (dired-move-to-filename)))

;; Just a helper function for `dired-map-over-marks'.
(defun diredp-get-file-or-dir-name (arg)
  "Return name of next file or directory or nil if none.
Argument ARG:
 `all-files-no-dirs' or nil means skip directories.
 `all-files-no-dots' means skip `.' and `..'."
  (let ((fname  nil))
    (while (and (not fname) (not (eobp)))
      (setq fname  (dired-get-filename t t))
      (when (and fname (or (not arg) (eq arg 'all-files-no-dirs))
                 (file-directory-p fname))
        (setq fname  nil))
      (when (and fname (eq arg 'all-files-no-dots)
                 (or (member fname '("." "..")) (string-match "/\.\.?$" fname)))
        (setq fname  nil))
      (forward-line 1))
    (forward-line -1)
    fname))

 
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
  (let* ((all-of-them
	  (save-excursion
	    (dired-map-over-marks (dired-get-filename localp) arg nil distinguish-one-marked)))
	 result)
    (if (not filter)
	(if (and distinguish-one-marked (eq (car all-of-them) t))
	    all-of-them
	  (nreverse all-of-them))
      (dolist (file all-of-them)
	(if (funcall filter file)
	    (push file result)))
      result)))



;;; Stuff from `dired-aux.el'.

(defun dired-map-over-marks-check (fun arg op-symbol &optional show-progress)
  "Map FUN over marked files and display failures.
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
           (dired-log-summary
            (format "Failed to %s %d of %d file%s"
                    (downcase string) count total (dired-plural-s total))
            failures)))))

(when (boundp 'dired-subdir-switches)   ; Emacs 22+
  (defun dired-do-redisplay (&optional arg test-for-subdir)
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
    (if (and test-for-subdir (dired-get-subdir))
        (let* ((dir       (dired-get-subdir))
               (switches  (cdr (assoc-string dir dired-switches-alist))))
          (dired-insert-subdir dir (when arg
                                     (read-string "Switches for listing: "
                                                  (or switches
                                                      dired-subdir-switches
                                                      dired-actual-switches)))))
      (message "Redisplaying...")
      ;; message much faster than making dired-map-over-marks show progress
      (dired-uncache
       (if (consp dired-directory) (car dired-directory) dired-directory))
      (dired-map-over-marks (let ((fname  (dired-get-filename)))
                              (message "Redisplaying... %s" fname)
                              (dired-update-file-line fname))
                            arg)
      (dired-move-to-filename)
      (message "Redisplaying...done"))))

(unless (boundp 'dired-subdir-switches) ; Emacs 20, 21
  (defun dired-do-redisplay (&optional arg test-for-subdir)
    "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing."
    ;; Moves point if the next ARG files are redisplayed.
    (interactive "P\np")
    (if (and test-for-subdir (dired-get-subdir))
        (dired-insert-subdir
         (dired-get-subdir)
         (if arg (read-string "Switches for listing: " dired-actual-switches)))
      (message "Redisplaying...")
      ;; message much faster than making dired-map-over-marks show progress
      (dired-uncache
       (if (consp dired-directory) (car dired-directory) dired-directory))
      (dired-map-over-marks (let ((fname  (dired-get-filename)))
                              (message "Redisplaying... %s" fname)
                              (dired-update-file-line fname))
                            arg)
      (dired-move-to-filename)
      (message "Redisplaying...done"))))

;; From `dired.el'

(when (and (> emacs-major-version 22) (featurep 'ls-lisp+))

  ;; Use t as WILDCARD arg to `dired-insert-directory'.
  ;;
  (defun dired-readin-insert ()
    ;; Insert listing for the specified dir (and maybe file list)
    ;; already in dired-directory, assuming a clean buffer.
    (let (dir file-list)
      (if (consp dired-directory)
          (setq dir        (car dired-directory)
                file-list  (cdr dired-directory))
        (setq dir        dired-directory
              file-list  ()))
      (setq dir  (expand-file-name dir))
      (if (and (equal "" (file-name-nondirectory dir))
               (not file-list))
          ;; If we are reading a whole single directory...
          (dired-insert-directory dir dired-actual-switches nil nil t)
        (if (not (file-readable-p
                  (directory-file-name (file-name-directory dir))))
            (error "Directory %s inaccessible or nonexistent" dir)
          ;; Else treat it as a wildcard spec.
          (dired-insert-directory dir dired-actual-switches file-list t t)))))

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
    (let ((opoint (point))
          (process-environment (copy-sequence process-environment))
          end)
      (if (or dired-use-ls-dired (file-remote-p dir))
          (setq switches  (concat "--dired " switches)))
      ;; We used to specify the C locale here, to force English month names;
      ;; but this should not be necessary any more,
      ;; with the new value of `directory-listing-before-filename-regexp'.
      (if file-list
          (dolist (f file-list)
            (let ((beg (point)))
              ;; Compute wildcard arg this file.
              (insert-directory f switches (string-match "[[?*]" f) nil)
              ;; Re-align fields, if necessary.
              (dired-align-file beg (point))))
        (insert-directory dir switches wildcard (not wildcard)))
      ;; Quote certain characters, unless ls quoted them for us.
      (if (not (string-match "b" dired-actual-switches))
          (save-excursion
            (setq end  (point-marker))
            (goto-char opoint)
            (while (search-forward "\\" end t)
              (replace-match (apply #'propertize
                                    "\\\\"
                                    (text-properties-at (match-beginning 0)))
                             nil t))
            (goto-char opoint)
            (while (search-forward "\^m" end t)
              (replace-match (apply #'propertize
                                    "\\015"
                                    (text-properties-at (match-beginning 0)))
                             nil t))
            (set-marker end nil)))
      (dired-insert-set-properties opoint (point))
      ;; If we used --dired and it worked, the lines are already indented.
      ;; Otherwise, indent them.
      (unless (save-excursion
                (goto-char opoint)
                (looking-at "  "))
        (let ((indent-tabs-mode nil))
          (indent-rigidly opoint (point) 2)))
      ;; Insert text at the beginning to standardize things.
      (save-excursion
        (goto-char opoint)
        (if (and (or hdr wildcard)
                 (not (and (looking-at "^  \\(.*\\):$")
                           (file-name-absolute-p (match-string 1)))))
            ;; Note that dired-build-subdir-alist will replace the name
            ;; by its expansion, so it does not matter whether what we insert
            ;; here is fully expanded, but it should be absolute.
            (insert "  " (directory-file-name (file-name-directory dir)) ":\n"))
        (when wildcard
          ;; Insert "wildcard" line where "total" line would be for a full dir.
          (insert "  wildcard " (file-name-nondirectory dir) "\n"))))))


;;; Stuff from `image-dired.el'.

(when (fboundp 'image-dired-get-thumbnail-image) ; Emacs 22+
  (defun image-dired-dired-insert-marked-thumbs ()
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
         (setq overlay  (car (delq nil (mapcar (lambda (o) (and (overlay-get o 'put-image) o))
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


;; "Single" menu.
;;
;; REPLACE ORIGINAL "Immediate" menu in `dired.el'.
;;
(defvar diredp-menu-bar-immediate-menu (make-sparse-keymap "Single"))
(define-key dired-mode-map [menu-bar immediate]
  (cons "Single" diredp-menu-bar-immediate-menu))
(when (fboundp 'image-dired-dired-display-external) ; Emacs 22+
  (define-key diredp-menu-bar-immediate-menu [image-dired-dired-display-external]
    '(menu-item "Display Image Externally" image-dired-dired-display-external
      :help "Display image in external viewer"))
  (define-key diredp-menu-bar-immediate-menu [image-dired-dired-display-image]
    '(menu-item "Display Image" image-dired-dired-display-image
      :help "Display image in a separate window"))
  (define-key diredp-menu-bar-immediate-menu [separator-image] '("--")))
(define-key diredp-menu-bar-immediate-menu [chown]
  '(menu-item "Change Owner..." diredp-chown-this-file
    :visible (not (memq system-type '(ms-dos windows-nt)))
    :help "Change the owner of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [chgrp]
  '(menu-item "Change Group..." diredp-chgrp-this-file
    :visible (not (memq system-type '(ms-dos windows-nt)))
    :help "Change the group of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [chmod]
  '(menu-item "Change Mode..." diredp-chmod-this-file
    :help "Change mode (attributes) of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [separator-ch] '("--"))
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
(define-key diredp-menu-bar-immediate-menu [separator-misc] '("--"))
(define-key diredp-menu-bar-immediate-menu [hardlink]
  '(menu-item "Hardlink to..." diredp-hardlink-this-file
    :help "Make hard links for current or marked files"))
(if (not (fboundp 'dired-do-relsymlink))
    (define-key diredp-menu-bar-immediate-menu [symlink]
      '(menu-item "Symlink to..." diredp-symlink-this-file
        :visible (fboundp 'make-symbolic-link)
        :help "Make symbolic link for file at cursor"))
  (define-key diredp-menu-bar-immediate-menu [symlink]
    '(menu-item "Symlink to (Absolute)..." diredp-symlink-this-file
      :help "Make absolute symbolic link for file at cursor"))
  (define-key diredp-menu-bar-immediate-menu [relsymlink]
    '(menu-item "Symlink to (Relative)..." diredp-relsymlink-this-file ; In `dired-x.el'.
      :help "Make relative symbolic link for file at cursor")))
(define-key diredp-menu-bar-immediate-menu [separator-link] '("--"))
(define-key diredp-menu-bar-immediate-menu [delete]
  '(menu-item "Delete" diredp-delete-this-file :help "Delete file at cursor"))
(define-key diredp-menu-bar-immediate-menu [capitalize]
  '(menu-item "Capitalize" diredp-capitalize-this-file
    :help "Capitalize (initial caps) name of file at cursor"))
(define-key diredp-menu-bar-immediate-menu [downcase]
  '(menu-item "Downcase" diredp-downcase-this-file
    ;; When running on plain MS-DOS, there's only one letter-case for file names.
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename file at cursor to a lower-case name"))
(define-key diredp-menu-bar-immediate-menu [upcase]
  '(menu-item "Upcase" diredp-upcase-this-file
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename file at cursor to an upper-case name"))
(define-key diredp-menu-bar-immediate-menu [rename]
  '(menu-item "Rename to..." diredp-rename-this-file :help "Rename file at cursor"))
(define-key diredp-menu-bar-immediate-menu [copy]
  '(menu-item "Copy to..." diredp-copy-this-file :help "Copy file at cursor"))
(define-key diredp-menu-bar-immediate-menu [separator-chg] '("--"))
(define-key diredp-menu-bar-immediate-menu [backup-diff]
  '(menu-item "Diff with Backup" dired-backup-diff
    :help "Diff file at cursor with its latest backup"))
(define-key diredp-menu-bar-immediate-menu [diff]
  '(menu-item "Diff..." dired-diff
    :help "Compare file at cursor with another file using `diff'"))
(define-key diredp-menu-bar-immediate-menu [ediff]
  '(menu-item "Compare..." diredp-ediff
    :help "Compare file at cursor with another file"))
(define-key diredp-menu-bar-immediate-menu [separator-diff] '("--"))
(define-key diredp-menu-bar-immediate-menu [insert-subdir]
  '(menu-item "Insert This Subdir" dired-maybe-insert-subdir
    :enable (atom (diredp-this-subdir)) :help "Insert a listing of this subdirectory"))
(define-key diredp-menu-bar-immediate-menu [view]
  '(menu-item "View (Read Only)" dired-view-file
    :help "Examine file at cursor in read-only mode"))
(define-key diredp-menu-bar-immediate-menu [display]
  '(menu-item "Display in Other Window" dired-display-file
    :help "Display file at cursor in a different window"))
(define-key diredp-menu-bar-immediate-menu [find-file-other-frame]
  '(menu-item "Open in Other Frame" diredp-find-file-other-frame
    :help "Edit file at cursor in a different frame"))
(define-key diredp-menu-bar-immediate-menu [find-file-other-window]
  '(menu-item "Open in Other Window" dired-find-file-other-window
    :help "Edit file at cursor in a different window"))
(define-key diredp-menu-bar-immediate-menu [find-file]
  '(menu-item "Open" dired-find-file :help "Edit file at cursor"))


;; "Multiple" menu.
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
    '(menu-item "Sign" epa-dired-do-sign
      :help "Create digital signature of file at cursor"))
  (define-key diredp-menu-bar-operate-menu [epa-dired-do-encrypt]
    '(menu-item "Encrypt" epa-dired-do-encrypt
      :help "Encrypt file at cursor"))
  (define-key diredp-menu-bar-operate-menu [separator-encryption] '("--")))
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
  (define-key diredp-menu-bar-operate-menu [separator-image] '("--")))
(define-key diredp-menu-bar-operate-menu [chown]
  '(menu-item "Change Owner..." dired-do-chown
    :visible (not (memq system-type '(ms-dos windows-nt)))
    :help "Change the owner of marked files"))
(define-key diredp-menu-bar-operate-menu [chgrp]
  '(menu-item "Change Group..." dired-do-chgrp
    :visible (not (memq system-type '(ms-dos windows-nt)))
    :help "Change the owner of marked files"))
(define-key diredp-menu-bar-operate-menu [chmod]
  '(menu-item "Change Mode..." dired-do-chmod :help "Change mode (attributes) of marked files"))
(when (> emacs-major-version 21)
  (define-key diredp-menu-bar-operate-menu [touch]
    '(menu-item "Change Timestamp..." dired-do-touch :help "Change timestamp of marked files")))
(define-key diredp-menu-bar-operate-menu [separator-ch] '("--"))
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
(define-key diredp-menu-bar-operate-menu [separator-search] '("--"))
(define-key diredp-menu-bar-operate-menu [load]
  '(menu-item "Load" dired-do-load :help "Load marked Emacs Lisp files"))
(define-key diredp-menu-bar-operate-menu [compile]
  '(menu-item "Byte Compile" dired-do-byte-compile
    :help "Byte-compile marked Emacs Lisp files"))
(define-key diredp-menu-bar-operate-menu [command]
  '(menu-item "Shell Command..." dired-do-shell-command
    :help "Run a shell command on each of marked files"))
(define-key diredp-menu-bar-operate-menu [compress]
  '(menu-item "Compress/Uncompress" dired-do-compress
    :help "Compress/uncompress marked files"))
(define-key diredp-menu-bar-operate-menu [print]
  '(menu-item "Print..." dired-do-print :help "Print marked files, supplying print command"))
(define-key diredp-menu-bar-operate-menu [diredp-do-bookmark-in-bookmark-file]
  '(menu-item "Bookmark in Bookmark File..." diredp-do-bookmark-in-bookmark-file
    :help "Bookmark the marked files in BOOKMARK-FILE and save BOOKMARK-FILE"))
(define-key diredp-menu-bar-operate-menu [diredp-set-bookmark-file-bookmark-for-marked]
  '(menu-item "Bookmark Project..." diredp-set-bookmark-file-bookmark-for-marked
    :help "Bookmark the marked files and create a bookmark-file bookmark for them"))
(define-key diredp-menu-bar-operate-menu [diredp-do-bookmark]
  '(menu-item "Bookmark..." diredp-do-bookmark :help "Bookmark the marked or next N files"))
(when (fboundp 'mkhtml-dired-files)
  (define-key diredp-menu-bar-operate-menu [mkhtml-dired-files]
    '(menu-item "Create HTML" mkhtml-dired-files
      :help "Create HTML files corresponding to marked files")))
(define-key diredp-menu-bar-operate-menu [separator-link] '("--"))
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
(define-key diredp-menu-bar-operate-menu [separator-move] '("--"))
(define-key diredp-menu-bar-operate-menu [delete-flagged]
  '(menu-item "Delete Flagged" dired-do-flagged-delete
    :help "Delete all files flagged for deletion (D)"))
(define-key diredp-menu-bar-operate-menu [delete]
  '(menu-item "Delete Marked (not Flagged)" dired-do-delete
    :help "Delete current file or all marked files (not flagged files)"))
(define-key diredp-menu-bar-operate-menu [capitalize]
  '(menu-item "Capitalize" diredp-capitalize
    :help "Capitalize (initial caps) the names of all marked files"))
(define-key diredp-menu-bar-operate-menu [downcase]
  '(menu-item "Downcase" dired-downcase
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename marked files to lowercase names"))
(define-key diredp-menu-bar-operate-menu [upcase]
  '(menu-item "Upcase" dired-upcase
    :enable (or (not (fboundp 'msdos-long-file-names)) (msdos-long-file-names))
    :help "Rename marked files to uppercase names"))
(define-key diredp-menu-bar-operate-menu [rename]
  '(menu-item "Rename to..." dired-do-rename :help "Rename current file or move marked files"))
(define-key diredp-menu-bar-operate-menu [copy]
  '(menu-item "Copy to..." dired-do-copy :help "Copy current file or all marked files"))
(define-key diredp-menu-bar-operate-menu [separator-misc] '("--"))
(when (fboundp 'dired-copy-filename-as-kill)
  (define-key diredp-menu-bar-operate-menu [kill-ring]
    '(menu-item "Copy File Names (to Paste)" dired-copy-filename-as-kill
      :help "Copy names of marked files onto kill ring, for pasting")))
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
(when (fboundp 'dired-do-find-marked-files)
  (define-key diredp-menu-bar-operate-menu [find-files]
    '(menu-item "Open" dired-do-find-marked-files ; In `dired-x.el'.
      :help "Open each marked file for editing")))


;; "Regexp" menu.
;;
;; REPLACE ORIGINAL "Regexp" menu in `dired.el'.
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
  '(menu-item "Rename to..." dired-do-rename-regexp
                         :help "Rename marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [copy]
  '(menu-item "Copy to..." dired-do-copy-regexp :help "Copy marked files matching regexp"))
(define-key diredp-menu-bar-regexp-menu [flag]
  '(menu-item "Flag..." dired-flag-files-regexp
    :help "Flag files matching regexp for deletion"))
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
    :enable mark-active :help "Flag all files in the region (selection) for deletion"))
(when (< emacs-major-version 21)
  (put 'diredp-flag-region-files-for-deletion 'menu-enable 'mark-active))
(define-key diredp-menu-bar-mark-menu [deletion]
  '(menu-item "Flag" dired-flag-file-deletion :help "Flag current line's file for deletion"))
(define-key diredp-menu-bar-mark-menu [separator-flag] '("--"))
(define-key diredp-menu-bar-mark-menu [prev]
  '(menu-item "Previous Marked" dired-prev-marked-file :help "Move to previous marked file"))
(define-key diredp-menu-bar-mark-menu [next]
  '(menu-item "Next Marked" dired-next-marked-file :help "Move to next marked file"))
(define-key diredp-menu-bar-mark-menu [marks]
  '(menu-item "Change Marks..." dired-change-marks
    :help "Replace marker with another character"))
(define-key diredp-menu-bar-mark-menu [revert]
  '(menu-item "Refresh (Show All)" revert-buffer :help "Update directory contents"))
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
(define-key diredp-menu-bar-mark-menu [separator-mark] '("--"))
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
    :enable mark-active :help "Mark all of the files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-mark-region-files 'menu-enable 'mark-active))
(define-key diredp-menu-bar-mark-menu [mark]
  '(menu-item "Mark" dired-mark :help "Mark current line's file for future operations"))
(define-key diredp-menu-bar-mark-menu [separator-unmark] '("--"))
(define-key diredp-menu-bar-mark-menu [unmark-all]
  '(menu-item "Unmark All" dired-unmark-all-marks :help "Remove all marks from all files"))
(define-key diredp-menu-bar-mark-menu [unmark-with]
  '(menu-item "Unmark Marked-With..." dired-unmark-all-files
    :help "Remove a specific mark (or all marks) from every file"))
(define-key diredp-menu-bar-mark-menu [unmark-region]
  '(menu-item "Unmark Region" diredp-unmark-region-files
    :enable mark-active :help "Unmark all files in the region (selection)"))
(when (< emacs-major-version 21)
  (put 'diredp-unmark-region-files 'menu-enable 'mark-active))
(define-key diredp-menu-bar-mark-menu [unmark]
  '(menu-item "Unmark" dired-unmark :help "Unmark or unflag current line's file"))


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
  '(menu-item "Tree Down" dired-tree-down
    :help "Go to first subdirectory header down the tree"))
(define-key diredp-menu-bar-subdir-menu [tree-up]
  '(menu-item "Tree Up" dired-tree-up
    :help "Go to first subdirectory header up the tree"))
(define-key diredp-menu-bar-subdir-menu [prev-subdir]
  '(menu-item "Prev Subdir" dired-prev-subdir
    :help "Go to previous subdirectory header line"))
(define-key diredp-menu-bar-subdir-menu [next-subdir]
  '(menu-item "Next Subdir" dired-next-subdir :help "Go to next subdirectory header line"))
(define-key diredp-menu-bar-subdir-menu [prev-dirline]
  '(menu-item "Prev Dirline" dired-prev-dirline :help "Move to previous directory-file line"))
(define-key diredp-menu-bar-subdir-menu [next-dirline]
  '(menu-item "Next Dirline" dired-next-dirline :help "Move to next directory-file line"))
(define-key diredp-menu-bar-subdir-menu [insert]
  '(menu-item "This Subdir" dired-maybe-insert-subdir
    :help "Move to subdirectory line or listing"))
(define-key diredp-menu-bar-subdir-menu [separator-subdir] '("--"))
(define-key diredp-menu-bar-mark-menu [revert]
  '(menu-item "Refresh" revert-buffer :help "Update directory contents"))
(when (fboundp 'dired-isearch-filenames) ; Emacs 23+
  (define-key diredp-menu-bar-subdir-menu [isearch-filenames-regexp]
    '(menu-item "Isearch Regexp in File Names..." dired-isearch-filenames-regexp
      :help "Incrementally search for regexp in file names only"))
  (define-key diredp-menu-bar-subdir-menu [isearch-filenames]
    '(menu-item "Isearch in File Names..." dired-isearch-filenames
      :help "Incrementally search for literal text in file names only.")))
(when (or (> emacs-major-version 21) (fboundp 'wdired-change-to-wdired-mode))
  (define-key diredp-menu-bar-subdir-menu [wdired-mode]
    '(menu-item "Edit File Names (Wdired)" wdired-change-to-wdired-mode
      :help "Put a dired buffer in a mode in which filenames are editable"
      :keys "C-x C-q" :filter (lambda (x) (if (eq major-mode 'dired-mode) x)))))
(when (fboundp 'dired-compare-directories) ; Emacs 22+
  (define-key diredp-menu-bar-subdir-menu [compare-directories]
    '(menu-item "Compare Directories..." dired-compare-directories
      :help "Mark files with different attributes in two Dired buffers")))
(define-key diredp-menu-bar-subdir-menu [up]
  '(menu-item "Up Directory" dired-up-directory :help "Dired the parent directory"))
(define-key diredp-menu-bar-subdir-menu [create-directory] ; Moved from "Immediate".
  '(menu-item "New Directory..." dired-create-directory :help "Create a directory"))
(define-key diredp-menu-bar-subdir-menu [separator-dired-on-set] '("--"))
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


;;; Mouse-3 menu binding.
(define-key dired-mode-map [mouse-3] 'diredp-mouse-3-menu)
;;;;;;;;(define-key dired-mode-map [mouse-3] 'ignore)


;;; Suggested bindings.
;;; (define-key ctl-x-map   "d" 'diredp-dired-files)
;;; (define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)


;;; Non-menu Dired bindings.

;; `diredp-mouse-mark-region-files' provides Windows-Explorer behavior
;; for selecting (marking) files.
(define-key dired-mode-map [S-down-mouse-1] 'ignore) ; (normally `mouse-set-font')
(define-key dired-mode-map [S-mouse-1] 'diredp-mouse-mark-region-files)
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file-other-window)
(define-key dired-mode-map [S-down-mouse-2] 'diredp-mouse-find-file)
(define-key dired-mode-map [S-mouse-2] 'ignore)
(define-key dired-mode-map [M-mouse-2] 'diredp-mouse-find-file-other-frame)
(define-key dired-mode-map "\C-\M-o" 'dired-display-file) ; Was `C-o'.
(define-key dired-mode-map [(control meta ?*)] 'diredp-marked-other-window)
(define-key dired-mode-map "\C-o"    'diredp-find-file-other-frame)
(define-key dired-mode-map "\M-b"    'diredp-do-bookmark)
(define-key dired-mode-map "\C-\M-b" 'diredp-set-bookmark-file-bookmark-for-marked)
(define-key dired-mode-map [(control meta shift ?b)] 'diredp-do-bookmark-in-bookmark-file)
(define-key dired-mode-map "\M-g" 'diredp-do-grep)
(define-key dired-mode-map "U" 'dired-unmark-all-marks)
(define-key dired-mode-map "=" 'diredp-ediff)
(substitute-key-definition 'next-line 'dired-next-line
                           dired-mode-map (current-global-map))
(substitute-key-definition 'previous-line 'dired-previous-line
                           dired-mode-map (current-global-map))
;; Commands for operating on the current line's file.  When possible,
;; these are lower-case versions of the upper-case commands for operating on
;; the marked files.  (Most of the other corresponding lower-case letters are already
;; defined and cannot be used here.)
(define-key dired-mode-map "b" 'diredp-byte-compile-this-file)
(define-key dired-mode-map "r" 'diredp-rename-this-file)
(define-key dired-mode-map "y" 'diredp-relsymlink-this-file)
(define-key dired-mode-map "z" 'diredp-compress-this-file)
(define-key dired-mode-map "\r" 'dired-find-file)
(when (fboundp 'mkhtml-dired-files)
  (define-key dired-mode-map [?\M-h] 'mkhtml-dired-files))
(define-key dired-mode-map [?\M-u] 'diredp-upcase-this-file)
(define-key dired-mode-map [?\M-l] 'diredp-downcase-this-file)
(define-key dired-mode-map [?\M-c] 'diredp-capitalize-this-file)
(define-key dired-mode-map [?\M-m] 'diredp-chmod-this-file)
(define-key dired-mode-map [?\M-p] 'diredp-print-this-file)
(substitute-key-definition 'kill-line 'diredp-delete-this-file
                           dired-mode-map (current-global-map))
;; This replaces the `dired-x.el' binding of `dired-mark-extension'.
(define-key dired-mode-map "*." 'diredp-mark/unmark-extension)

;; On Windows, bind some more keys.
(when (featurep 'w32-browser)
  (define-key dired-mode-map [(control return)] 'dired-w32-browser)
  (define-key dired-mode-map [(meta return)] 'dired-w32explore)
  (define-key diredp-menu-bar-immediate-menu [dired-w32-browser]
    '(menu-item "Open Associated Windows App" dired-w32-browser
      :help "Open file using the Windows app associated with its file type"))
  (define-key diredp-menu-bar-immediate-menu [dired-w32explore]
    '(menu-item "Open in Windows Explorer" dired-w32explore
      :help "Open file in Windows Explorer"))
  (define-key dired-mode-map [mouse-2] 'dired-mouse-w32-browser)
  (define-key diredp-menu-bar-operate-menu [dired-multiple-w32-browser]
    '(menu-item "Open Associated Windows Apps" dired-multiple-w32-browser
      :help "Open files using the Windows apps associated with their file types")))

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
  :link '(emacs-commentary-link :tag "Commentary" "dired+")
  )

 
;;; Face Definitions

;;; Miscellaneous faces.
(defface diredp-display-msg 
    '((((background dark)) (:foreground "Yellow"))
      (t (:foreground "Blue")))
  "*Face used for message display."
  :group 'Dired-Plus)
(defvar diredp-display-msg 'diredp-display-msg)

;;; Faces used to fontify buffer when using second level of fontifying.
(defface diredp-dir-heading
    '((((background dark))
       (:foreground "Yellow" :background "#00003F3F3434")) ; ~ dark green
      (t (:foreground "Blue" :background "Pink")))
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
    '((((background dark))
       (:foreground "Blue" :background "#7575D4D41D1D")) ; ~ olive green
      (t (:foreground "Yellow" :background "Blueviolet")))
  "*Face used for flags and marks (except D) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark 'diredp-flag-mark)

(defface diredp-flag-mark-line
    '((((background dark)) (:background "#787831311414")) ; ~ dark red brown
      (t (:background "Skyblue")))
  "*Face used for flagged and marked lines in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-flag-mark-line 'diredp-flag-mark-line)

(defface diredp-file-suffix
    '((((background dark)) (:foreground "#7474FFFF7474")) ; ~ light green
      (t (:foreground "DarkMagenta")))
  "*Face used for file suffixes in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-suffix 'diredp-file-suffix)

(defface diredp-number
    '((((background dark)) (:foreground "#FFFFFFFF7474")) ; ~ light yellow
      (t (:foreground "DarkBlue")))
  "*Face used for numerical fields in dired buffers.
In particular, inode number, number of hard links, and file size."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-number 'diredp-number)

(defface diredp-symlink
    '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
      (t (:foreground "DarkOrange")))
  "*Face used for symbolic links in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-symlink 'diredp-symlink)

(defface diredp-date-time
    '((((background dark)) (:foreground "#74749A9AF7F7")) ; ~ med blue
      (t (:foreground "DarkGoldenrod4")))
  "*Face used for date and time in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-date-time 'diredp-date-time)

(defface diredp-file-name
    '((((background dark)) (:foreground "Yellow"))
      (t (:foreground "Blue")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-file-name 'diredp-file-name)

(defface diredp-ignored-file-name
    '(;; (((background dark)) (:foreground "#FFFF921F921F")) ; ~ salmon
      ;; (((background dark)) (:foreground "#A71F5F645F64")) ; ~ dark salmon
      (((background dark)) (:foreground "#C29D6F156F15")) ; ~ salmon
      (t (:foreground "#00006DE06DE0")))                  ; ~ dark cyan
  "*Face used for ignored file names  in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-ignored-file-name 'diredp-ignored-file-name)

(defface diredp-compressed-file-suffix
    '((((background dark)) (:foreground "Blue"))
      (t (:foreground "Yellow")))
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
      (t (:background "LightSteelBlue")))
  "*Face used for execute privilege indicator (x) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-exec-priv 'diredp-exec-priv)

(defface diredp-other-priv
    '((((background dark)) (:background "#111117175555")) ; ~ dark blue
      (t (:background "PaleGoldenrod")))
  "*Face used for l,s,S,t,T privilege indicators in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-other-priv 'diredp-other-priv)

(defface diredp-write-priv
    '((((background dark)) (:background "#25258F8F2929")) ; ~ dark green
      (t (:background "Orchid")))
  "*Face used for write privilege indicator (w) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-write-priv 'diredp-write-priv)

(defface diredp-read-priv
    '((((background dark)) (:background "#999932325555")) ; ~ burgundy / dark magenta
      (t (:background "MediumAquamarine")))
  "*Face used for read privilege indicator (w) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-read-priv 'diredp-read-priv)

(defface diredp-no-priv
    '((((background dark)) (:background "#2C2C2C2C2C2C")) ; ~ dark gray
      (t (:background "LightGray")))
  "*Face used for no privilege indicator (-) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-no-priv 'diredp-no-priv)

(defface diredp-rare-priv
    '((((background dark)) (:foreground "Green" :background "#FFFF00008080")) ; ~ hot pink
      (t (:foreground "Magenta" :background "SpringGreen")))
  "*Face used for rare privilege indicators (b,c,s,m,p,S) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-rare-priv 'diredp-rare-priv)

(defface diredp-link-priv
    '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
      (t (:foreground "DarkOrange")))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'Dired-Plus :group 'font-lock-highlighting-faces)
(defvar diredp-link-priv 'diredp-link-priv)

;; Fix Emacs 20 recognition of fields up through file name when size is expressed using `k' etc.
(when (and (< emacs-major-version 21) (not (boundp 'diredp-loaded-p))
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
   ;; 1) Date/time and 2) filename w/o suffix:
   (list dired-move-to-filename-regexp
         (if (or (not (fboundp 'version<)) (version< emacs-version "23.2"))
             (list 1 'diredp-date-time t t)
           (list 2 'diredp-date-time t t)) ; Date/time
         (list "\\(.+\\)$" nil nil (list 0 diredp-file-name 'keep t))) ; Filename

   ;; Files to ignore
   (list (concat "^  \\(.*\\("
                 (concat (mapconcat 'regexp-quote
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
          '(lambda ()
            (set (make-local-variable 'font-lock-defaults)
             (cons '(dired-font-lock-keywords diredp-font-lock-keywords-1) ; Two levels.
              (cdr font-lock-defaults)))))
 
;;; Function Definitions

;;;###autoload
(defun diredp-dired-files (arg &optional switches)
  "Like `dired', but non-positive prefix arg prompts for files to list.
This is the same as `dired' unless you use a non-positive prefix arg.
In that case, you are prompted for names of files and directories to
list, and then you are prompted for the name of the Dired buffer that
lists them.  Use `C-g' when you are done entering file names to list.

In all cases, when inputting a file or directory name you can use
shell wildcards."
  (interactive (diredp-dired-files-interactive-spec ""))
  (when (consp arg)
    (let ((buf  (dired-find-buffer-nocreate (car arg)))) ; Respect file list.
      (when buf (kill-buffer buf))))
  (switch-to-buffer (dired-noselect arg switches)))

;;;###autoload
(defun diredp-dired-files-other-window (arg &optional switches)
  "Same as `diredp-dired-files' except uses another window."
  (interactive (diredp-dired-files-interactive-spec "in other window "))
  (when (consp arg)
    (let ((buf  (dired-find-buffer-nocreate (car arg)))) ; Respect file list.
      (when buf (kill-buffer buf))))
  (dired-other-window arg switches))

(defun diredp-dired-files-interactive-spec (str)
  "`interactive' spec for `diredp-dired-files' commands.
STR is a string appended to the prompt.
With non-negative prefix arg, read switches.
With non-positive prefix arg, read files and dirs to list and then the
 Dired buffer name.  User uses `C-g' when done reading files and dirs."
  (list
   (if (> (prefix-numeric-value current-prefix-arg) 0)
       ;; If a dialog box is about to be used, call `read-directory-name' so the dialog code
       ;; knows we want directories.  Some dialog boxes can only select directories or files
       ;; when popped up, not both.
       (if (and (fboundp 'read-directory-name) (next-read-file-uses-dialog-p))
           (read-directory-name (format "Dired %s(directory): " str) nil default-directory nil)
         (read-file-name (format "Dired %s(directory): " str) nil default-directory nil))
     (let ((insert-default-directory  nil)
           (files                     ())
           file)
       (while (condition-case nil ; Use lax completion, to allow wildcards.
                  (setq file  (read-file-name "File or dir (C-g when done): "))
                (quit nil))
         (push file files))
       (cons (read-string "Dired buffer name: " nil nil default-directory) files)))
   (and current-prefix-arg (natnump (prefix-numeric-value current-prefix-arg))
        (read-string "Dired listing switches: " dired-listing-switches))))

;;;###autoload
(defun diredp-dired-union (dirbufs &optional switches)
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
(defun diredp-dired-union-other-window (dirbufs &optional switches)
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
               (dolist (sub   (cdr (reverse dired-subdir-alist)))
                 (push (list (car sub)) subdirs))
               (dolist (mkd   (dired-remember-marks (point-min) (point-max))) ; This unhides.
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
          (dolist (dir  hidden-dirs)
            (when (dired-goto-subdir dir) (dired-hide-subdir 1))))))))

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
     (while (and dirbufs
                 (condition-case nil
                     (setq buf  (completing-read
                                 "Dired buffer to include (C-g when done): "
                                 dirbufs nil t nil buffer-name-history
                                 (and dirbufs (car (assoc (buffer-name) dirbufs)))))
                   (quit nil)))
       (push buf bufs)
       (setq dirbufs  (delete (cons buf (with-current-buffer buf default-directory)) dirbufs)))
     (setq bufs  (nreverse bufs))
     (cons (read-string "Dired-union buffer name: ") bufs))
   (and current-prefix-arg (read-string "Dired listing switches: " dired-listing-switches))))

;;;###autoload
(defun diredp-fileset (flset-name)
  "Open Dired on the files in fileset FLSET-NAME."
  (interactive
   (list (let ((fd  (or (and (require 'filesets nil t) filesets-data)
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
  
         

;;; `diredp-marked(-other-window)' tries to treat SWITCHES, but SWITCHES seems to be ignored
;;; by `dired' when the DIRNAME arg is a cons, at least on MS Windows.  I filed Emacs bug #952
;;; on 2008-09-10, but this doesn't work in Emacs 20, 21, 22, or 23, so I don't know if it will
;;; ever be fixed.  If it is declared a non-bug and it doesn't work on any platforms, then I'll
;;; remove SWITCHES here, alas.

;;;###autoload
(defun diredp-marked (dirname &optional n switches)
  "Open Dired on only the marked files or the next N files.
With a non-zero numeric prefix arg N, use the next abs(N) files.
A plain (`C-u'), zero, or negative prefix arg prompts for listing
switches as in command `dired'.

Note that the marked files can include files in inserted
subdirectories, so the Dired buffer that is opened can contain files
from multiple directories in the same tree."
  (interactive
   (progn
     (unless (eq major-mode 'dired-mode) (error "Run this command only in Dired mode"))
     (let ((num  (and current-prefix-arg (atom current-prefix-arg)
                      (not (zerop (prefix-numeric-value current-prefix-arg)))
                      (abs (prefix-numeric-value current-prefix-arg)))))
       (list (cons (generate-new-buffer-name (buffer-name)) (dired-get-marked-files t num))
             num
             (and current-prefix-arg    ; Switches
                  (or (consp current-prefix-arg)
                      (< (prefix-numeric-value current-prefix-arg) 0))
                  (read-string "Dired listing switches: " dired-listing-switches))))))
  (unless (or n (save-excursion (goto-char (point-min))
                                (and (re-search-forward (dired-marker-regexp) nil t)
                                     (re-search-forward (dired-marker-regexp) nil t))))
    (error "No marked files"))
  (dired dirname switches))

;;;###autoload
(defun diredp-marked-other-window (dirname &optional n switches)
  "Same as `diredp-marked', but uses a different window."
  (interactive
   (progn
     (unless (eq major-mode 'dired-mode) (error "Run this command only in Dired mode"))
     (let ((num  (and current-prefix-arg (atom current-prefix-arg)
                      (not (zerop (prefix-numeric-value current-prefix-arg)))
                      (abs (prefix-numeric-value current-prefix-arg)))))
       (list (cons (generate-new-buffer-name (buffer-name)) (dired-get-marked-files t num))
             num
             (and current-prefix-arg    ; Switches
                  (or (consp current-prefix-arg)
                      (< (prefix-numeric-value current-prefix-arg) 0))
                  (read-string "Dired listing switches: " dired-listing-switches))))))
  (unless (or n (save-excursion (goto-char (point-min))
                                (and (re-search-forward (dired-marker-regexp) nil t)
                                     (re-search-forward (dired-marker-regexp) nil t))))
    (error "No marked files"))
  (dired-other-window dirname switches))


;; Similar to `dired-mark-extension' in `dired-x.el'.
;; The difference is that this uses prefix arg to unmark, not to determine the mark character.
;;;###autoload
(defun diredp-mark/unmark-extension (extension &optional unmark-p)
  "Mark all files with a certain EXTENSION for use in later commands.
A `.' is not automatically prepended to the string entered.
Non-nil prefix argument UNMARK-P means unmark instead of mark."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                                    "ing extension: "))
         current-prefix-arg))
  (or (listp extension) (setq extension  (list extension)))
  (dired-mark-files-regexp (concat ".";; don't match names with nothing but an extension
                                   "\\("
                                   (mapconcat 'regexp-quote extension "\\|")
                                   "\\)$")
   (and current-prefix-arg ?\040)))

;;;###autoload
(defun diredp-set-bookmark-file-bookmark-for-marked (bookmark-file prefix ; Bound to `C-M-b'
                                                     &optional arg)
  "Bookmark the marked files and create a bookmark-file bookmark for them.
Jumping to the bookmark-file bookmark loads the set of file bookmarks.

Each bookmark name is PREFIX followed by the relative file name.
Interactively, you are prompted for PREFIX.
The bookmarked position is the beginning of the file.

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
  (interactive (direp-read-bookmark-file-args))
  (diredp-do-bookmark-in-bookmark-file bookmark-file prefix arg 'CREATE-BOOKMARK-FILE-BOOKMARK))

;;;###autoload
(defun diredp-do-bookmark-in-bookmark-file (bookmark-file prefix ; Bound to `C-M-S-b' (`C-M-B')
                                            &optional arg  bfile-bookmarkp)
  "Bookmark the marked files in BOOKMARK-FILE and save BOOKMARK-FILE.
You are prompted for BOOKMARK-FILE.  The default is `.emacs.bmk' in
the current directory, but you can enter any file name, anywhere.

The marked files are bookmarked in file BOOKMARK-FILE, but this
command does not make BOOKMARK-FILE the current bookmark file.  To
make it current, use `\\[bmkp-switch-bookmark-file]' (`bmkp-switch-bookmark-file').

Each bookmark name is PREFIX followed by the relative file name.
Interactively, you are prompted for PREFIX.
The bookmarked position is the beginning of the file.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

See also command `diredp-set-bookmark-file-bookmark-for-marked'.

Non-interactively, non-nil BFILE-BOOKMARKP means create a
bookmark-file bookmark for BOOKMARK-FILE."
  (interactive (direp-read-bookmark-file-args))
  (let ((bfile-exists-p  (file-readable-p bookmark-file)))
    (unless bfile-exists-p (bmkp-empty-file bookmark-file))
    (unless bmkp-current-bookmark-file (setq bmkp-current-bookmark-file  bookmark-default-file))
    (let ((old-bmkp-current-bookmark-file  bmkp-current-bookmark-file))
      (unwind-protect
           (progn (bmkp-switch-bookmark-file bookmark-file) ; Changes `*-current-bookmark-file'.
                  (dired-map-over-marks-check
                   #'(lambda () (diredp-bookmark prefix)) arg 'bookmark
                   (diredp-fewer-than-2-files-p arg))
                  (bookmark-save)
                  (unless bfile-exists-p (revert-buffer)))
        (unless (bmkp-same-file-p old-bmkp-current-bookmark-file  bmkp-current-bookmark-file)
          (bmkp-switch-bookmark-file old-bmkp-current-bookmark-file 'NO-MSG))))
    (when bfile-bookmarkp (bmkp-set-bookmark-file-bookmark bookmark-file))))

(defun direp-read-bookmark-file-args ()
  "Read args for `diredp-do-bookmark-in-bookmark-file' and similar."
  (unless (require 'bookmark+ nil t) (error "This command requires library `bookmark+.el'"))
  (unless (eq major-mode 'dired-mode)
    (error "You must be in a Dired buffer to use this command"))
  (list (let* ((insert-default-directory  t)
               (bmk-file
                (expand-file-name
                 (read-file-name "Use bookmark file: " nil
                                 (if (> emacs-major-version 22)
                                     (list ".emacs.bmk" bookmark-default-file)
                                   ".emacs.bmk")))))
          bmk-file)
        (read-string "Prefix for bookmark names: " nil nil
                     (expand-file-name (if (consp dired-directory)
                                           (car dired-directory)
                                         dired-directory)))
        current-prefix-arg))

;;;###autoload
(defun diredp-do-bookmark (prefix &optional arg) ; Bound to `M-b'
  "Bookmark the marked (or the next prefix argument) files.
Each bookmark name is PREFIX followed by the relative file name.
Interactively, you are prompted for the PREFIX.
The bookmarked position is the beginning of the file.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive
   (progn (unless (eq major-mode 'dired-mode)
            (error "You must be in a Dired buffer to use this command"))
          (list (read-string "Prefix for bookmark name: " nil nil
                             (expand-file-name (if (consp dired-directory)
                                                   (car dired-directory)
                                                 dired-directory)))
                current-prefix-arg)))
  (dired-map-over-marks-check #'(lambda () (diredp-bookmark prefix)) arg 'bookmark
                              (diredp-fewer-than-2-files-p arg)))

;;;###autoload
(defun diredp-mouse-do-bookmark (event &optional arg) ; Not bound
  "In Dired, bookmark this file."
  (interactive "e\P")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t)
        (prefix            (read-string "Prefix for bookmark name: " nil nil
                                        (expand-file-name (if (consp dired-directory)
                                                              (car dired-directory)
                                                            dired-directory)))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'(lambda () (diredp-bookmark prefix)) arg 'bookmark t))
  (dired-previous-line 1))

(defun diredp-bookmark (prefix)
  "Bookmark the file or directory named on the current line.
The bookmark name is PREFIX followed by the relative file name.
Return nil for success, file name of unsuccessful operation otherwise."
  (bookmark-maybe-load-default-file)
  (let ((file  (dired-get-file-for-visit))
        failure)
    (condition-case err
        (if (fboundp 'bmkp-file-target-set)
            (bmkp-file-target-set file)
          (let ((bookmark-make-record-function
                 (cond ((and (require 'image nil t) (require 'image-mode nil t)
                             (condition-case nil (image-type file) (error nil)))
                        'image-bookmark-make-record)
                       (t
                        (lambda ()
                          `((filename . ,file)
                            (position . 0)))))))
            (bookmark-store (concat prefix (file-name-nondirectory file))
                            (cdr (bookmark-make-record)) nil)))
      (error (setq failure  (error-message-string err))))
    (if (not failure)
	nil                             ; Return nil for success.
      (if (fboundp 'bmkp-file-target-set)
          (dired-log failure)
        (dired-log "Failed to create bookmark for `%s':\n%s\n" file failure))
      (dired-make-relative file))))     ; Return file name for failure.


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
  (setq dir  (file-name-as-directory dir))
  (let ((alist  dired-buffers) result elt buf pattern)
    (while alist
      (setq elt  (car alist)
            buf  (cdr elt))
      (if (buffer-name buf)
          (if (dired-in-this-tree dir (car elt))
              (with-current-buffer buf
                (and (assoc dir dired-subdir-alist)
                     (or (null file)
                         (let ((wildcards
                                ;; Allow for consp `dired-directory' too.
                                (file-name-nondirectory (if (consp dired-directory)
                                                            (car dired-directory)
                                                          dired-directory))))
                           (or (= 0 (length wildcards))
                               (string-match (dired-glob-regexp wildcards) file))))
                     (setq result  (cons buf result)))))
        ;; else buffer is killed - clean up:
        (setq dired-buffers  (delq elt dired-buffers)))
      (setq alist  (cdr alist)))
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
  (defun diredp-find-a-file (filename &optional wildcards)
    "`find-file', but use file on current line as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file filename wildcards))
  
  (defun diredp-find-a-file-other-frame (filename &optional wildcards)
    "`find-file-other-frame', but use file under cursor as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file-other-frame filename wildcards))
  
  (defun diredp-find-a-file-other-window (filename &optional wildcards)
    "`find-file-other-window', but use file under cursor as default (`M-n')."
    (interactive (diredp-find-a-file-read-args "Find file: " nil))
    (find-file-other-window filename wildcards))
  
  (defun diredp-find-a-file-read-args (prompt mustmatch)
    (list (let ((find-file-default  (abbreviate-file-name (dired-get-file-for-visit))))
            (minibuffer-with-setup-hook
             (lambda () (setq minibuffer-default  find-file-default))
             (read-file-name prompt nil default-directory mustmatch)))
          t)))

;; Define these for Emacs 20 and 21.
(unless (fboundp 'dired-get-file-for-visit) ; Defined in Emacs 22.
  (defun dired-get-file-for-visit ()
    "Get the current line's file name, with an error if file does not exist."
    (interactive)
    ;; We pass t for second arg so that we don't get error for `.' and `..'.
    (let ((raw  (dired-get-filename nil t))
          file-name)
      (if (null raw)
          (error "No file on this line"))
      (setq file-name  (file-name-sans-versions raw t))
      (if (file-exists-p file-name)
          file-name
        (if (file-symlink-p file-name)
            (error "File is a symlink to a nonexistent target")
          (error "File no longer exists; type `g' to update Dired buffer")))))

  (defun dired-find-alternate-file ()
    "In Dired, visit this file or directory instead of the dired buffer."
    (interactive)
    (set-buffer-modified-p nil)
    (find-alternate-file (dired-get-file-for-visit))))

;;;###autoload
(defun diredp-find-file-reuse-dir-buffer ()
  "Like `dired-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' if the target is not a directory."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((file  (dired-get-file-for-visit)))
    (if (file-directory-p file) (find-alternate-file file) (find-file file))))

;;;###autoload
(defun diredp-mouse-find-file-reuse-dir-buffer (event)
  "Like `diredp-mouse-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' if the target is not a directory."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion (goto-char (posn-point (event-end event)))
                      (setq file  (dired-get-file-for-visit))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (find-alternate-file (file-name-sans-versions file t))
      (find-file (file-name-sans-versions file t)))))

;;;###autoload
(defalias 'diredp-toggle-find-file-reuse-dir 'toggle-dired-find-file-reuse-dir)
;;;###autoload
(defun toggle-dired-find-file-reuse-dir (force-p)
  "Toggle whether Dired `find-file' commands reuse directories.
A prefix arg specifies directly whether or not to reuse.
 If its numeric value is non-negative then reuse; else do not reuse.

To set the behavior as a preference (default behavior), put this in
your ~/.emacs, where VALUE is 1 to reuse or -1 to not reuse:

 (toggle-dired-find-file-reuse-dir VALUE)"
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
(defun diredp-omit-marked ()
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
(defun diredp-omit-unmarked ()
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
(defun diredp-ediff (file2)
  "Compare file at cursor with file FILE2 using `ediff'.
FILE2 defaults to the file at the cursor as well.  If you enter just a
directory name for FILE2, then the file at the cursor is compared with
a file of the same name in that directory.  FILE2 is the second file
given to `ediff'; the file at the cursor is the first."
  (interactive
   (progn
     (require 'ediff)
     (list (ediff-read-file-name        ; In `ediff.el'.
            (format "Compare %s with" (dired-get-filename t))
            (dired-current-directory) (dired-get-filename)))))
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
(defun dired-do-compress (&optional arg)
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
(defun dired-do-byte-compile (&optional arg)
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
(defun dired-do-load (&optional arg)
  "Load the marked (or next prefix argument) Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive "P")
  (dired-map-over-marks-check #'dired-load arg 'load (diredp-fewer-than-2-files-p arg)))

(when (< emacs-major-version 22)
  (defun diredp-do-grep (command-args)
    "Run `grep' on marked (or next prefix arg) files.
A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer."
    (interactive (progn (unless grep-command (grep-compute-defaults))
                        (list (diredp-do-grep-1))))
    (grep command-args)))

(unless (< emacs-major-version 22)
  (defun diredp-do-grep (command-args)
    "Run `grep' on marked (or next prefix arg) files.
A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer."
    (interactive (progn (unless (and grep-command (or (not grep-use-null-device)
                                                      (eq grep-use-null-device t)))
                          (grep-compute-defaults))
                        (list (diredp-do-grep-1))))
    (grep command-args)))

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
                                (or files (dired-get-marked-files nil current-prefix-arg))
                                " "))
             (- (length up-to-files) 2)))
     nil nil 'grep-history default)))

;;;###autoload
(define-derived-mode diredp-w32-drives-mode fundamental-mode "Drives"
  "Open Dired for an MS Windows drive (local or remote)."
  (setq buffer-read-only  t))

;; The next two commands were originally taken from Emacs Wiki, page WThirtyTwoBrowseNetDrives:
;; http://www.emacswiki.org/emacs/WThirtyTwoBrowseNetDrives.  They are referred to there as
;; commands `show-net-connections' and `netdir'.  I am hoping that the contributor (anonymous)
;; does not mind my adapting them and including them in Dired+.

;;;###autoload
(defun diredp-w32-list-mapped-drives ()
  "List network connection information for shared MS Windows resources.
This just invokes the Windows `NET USE' command."
  (interactive) 
  (unless (eq system-type 'windows-nt)
    (error "This command is only for use on MS Windows"))
  (shell-command "net use"))

;;;###autoload
(defun diredp-w32-drives (&optional other-window-p)
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
  (unless (eq system-type 'windows-nt)
    (error "This command is only for use on MS Windows"))
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
    (mapcar (lambda (x)
              (lexical-let ((x  x))
                (widget-create 'push-button
                               :notify (lambda (widget &rest ignore)
                                         (dired (car x)))
                               (concat (car x) "  " (cadr x))))
              (widget-insert "\n"))
            (sort drive (lambda (a b) (string-lessp (car a) (car b)))))
    (goto-char (point-min))
    (diredp-w32-drives-mode)))

;; No longer used.  It was used in `dired-do-grep(-1)' before the new `dired-get-marked-files'.
(defun diredp-all-files ()
  "List of all files shown in current Dired buffer.
Directories are not included."
  (let ((pos    (make-marker))
        (files  ())
        file)
    (save-excursion
      (goto-char (point-min))  (beginning-of-line)
      (while (not (eobp))
        (beginning-of-line)
        (while (and (not (eobp)) (dired-between-files)) (forward-line 1))
        (save-excursion (forward-line 1) (move-marker pos (1+ (point))))
        (setq file  (dired-get-filename nil t)) ; Non-nil second arg means "also . and ..".
        (when file                      ; Remove directory portion if in same directory.
          (setq file  (dired-get-filename (dired-in-this-tree file default-directory) t)))
        (unless (or (not file) (file-directory-p file)) (push file files))
        (goto-char pos))
      (move-marker pos nil))
    (setq files  (sort files
                       (if (and (featurep 'ls-lisp)
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
           (or (and (not switches) (dired-goto-subdir dirname))
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
(defun dired-do-find-marked-files (&optional arg)
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
             ;; Vertically split off a window of desired size.
             ;; Upper window will have SIZE lines.
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
;; Reset `mode-line-process' to nil.
;;
(when (< emacs-major-version 21)
  (or (fboundp 'old-dired-revert) (fset 'old-dired-revert (symbol-function 'dired-revert)))
  (defun dired-revert (&optional arg noconfirm)
    (setq mode-line-process  nil)        ; Set by, e.g., `find-dired'.
    (old-dired-revert arg noconfirm)))


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
              (add-text-properties
               (save-excursion (beginning-of-line) (point))
               (save-excursion (end-of-line) (point))
               '(mouse-face highlight
                 help-echo "mouse-2: visit this file in other window"))
              (put-text-property
               (point) (save-excursion (dired-move-to-end-of-filename) (point))
               'dired-filename t))
          (error nil))
        (forward-line 1)))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; If at root on a Windows drive, go up to a list of available drives.
;;
;;;###autoload
(defun dired-up-directory (&optional other-window)
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
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn (if other-window
                   (dired-other-window up)
                 (dired up))
               (dired-goto-file dir))
        (and (eq system-type 'windows-nt) (diredp-w32-drives)))))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; 1. Fixes Emacs bug #7126: Didn't work with arbitrary file list (cons arg to `dired').
;; 2. Remove `/' from directory name before comparing with BASE.
;;
;;;###autoload
(defun dired-goto-file (file)
  "Go to line describing file FILE in this dired buffer."
  ;; Return value of point on success, else nil.
  ;; FILE must be an absolute file name.
  ;; Loses if FILE contains control chars like "\007" for which ls
  ;; either inserts "?" or "\\007" into the buffer, so we won't find
  ;; it in the buffer.
  (interactive
   (prog1                               ; let push-mark display its message
       (list (expand-file-name
              (read-file-name "Goto file: " (dired-current-directory))))
     (push-mark)))
  (setq file  (directory-file-name file)) ; does no harm if no directory
  (let ((found  nil)
        case-fold-search dir)
    (setq dir  (or (file-name-directory file) (error "File name `%s' is not absolute" file)))
    (save-excursion
      (goto-char (point-min))
      (let ((search-string  (replace-regexp-in-string "\^m" "\\^m" file nil t))
            (here           nil))
        (setq search-string  (replace-regexp-in-string "\\\\" "\\\\" search-string nil t))
        ;; Use `here' to ensure we don't keep searching for a directory entry.
        (while (and (not (eobp)) (not found) (not (equal here (point))))
          (setq here  (point))
          (if (search-forward (concat " " search-string) nil 'NO-ERROR)
              ;; Must move to filename since an (actually correct) match could have been
              ;; elsewhere on the line (e.g. "-" would match somewhere in permission bits).
              (setq found  (dired-move-to-filename))
            ;; If this isn't the right line, move forward to avoid trying this line again.
            (forward-line 1)))))
    (unless found
      (save-excursion
        ;; The hair here is to get the result of dired-goto-subdir
        ;; without really calling it if we don't have any subdirs.
        (if (if (string= dir (expand-file-name default-directory))
                (goto-char (point-min))
              (and (cdr dired-subdir-alist) (dired-goto-subdir dir)))
            (let ((base      (file-name-nondirectory file))
                  search-string
                  (boundary  (dired-subdir-max)))
              (setq search-string  (replace-regexp-in-string "\^m" "\\^m" base nil t)
                    search-string  (replace-regexp-in-string "\\\\" "\\\\" search-string nil t))
              (while (and (not found)
                          ;; filenames are preceded by SPC, this makes
                          ;; the search faster (e.g. for the filename "-"!).
                          (search-forward (concat " " search-string)
                                          boundary 'move))
                ;; Remove / from filename, then compare with BASE.
                ;; Match could have BASE just as initial substring or
                ;; or in permission bits or date or not be a proper filename at all.
                (if (and (dired-get-filename 'no-dir t)
                         (equal base (directory-file-name (dired-get-filename 'no-dir t))))
                    ;; Must move to filename since an (actually
                    ;; correct) match could have been elsewhere on the
                    ;; line (e.g. "-" would match somewhere in the
                    ;; permission bits).
                    (setq found  (dired-move-to-filename))
                  ;; If this isn't the right line, move forward to avoid
                  ;; trying this line again.
                  (forward-line 1)))))))
    (and found (goto-char found))))     ; Return value of point (i.e., FOUND).


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
Optional arg NO-ERROR-IF-NOT-FILEP means treat `.' and `..' as
regular filenames and return nil if no filename on this line.
Otherwise, an error occurs in these cases."
  (let (case-fold-search file p1 p2 already-absolute)
    (save-excursion
      (if (setq p1  (dired-move-to-filename (not no-error-if-not-filep)))
          (setq p2  (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line, but no-error-if-not-filep is t:
    (when (setq file (and p1 p2 (buffer-substring p1 p2)))
      ;; Get rid of the mouse-face property that file names have.
      (set-text-properties 0 (length file) nil file)
      ;; Unquote names quoted by ls or by dired-insert-directory.
      ;; Using read to unquote is much faster than substituting
      ;; \007 (4 chars) -> ^G  (1 char) etc. in a lisp loop.
      (setq file  (read (concat "\""
                                ;; Some ls -b don't escape quotes, argh!
                                ;; This is not needed for GNU ls, though.
                                (or (dired-string-replace-match
                                     "\\([^\\]\\|\\`\\)\"" file "\\1\\\\\"" nil t)
                                    file)
                                "\"")))
      ;; The above `read' will return a unibyte string if FILE
      ;; contains eight-bit-control/graphic characters.
      (when (and (fboundp 'string-to-multibyte) ; Emacs 22
                 enable-multibyte-characters
                 (not (multibyte-string-p file)))
        (setq file  (string-to-multibyte file))))
    (and file (file-name-absolute-p file)
         ;; A relative file name can start with ~.
         ;; Don't treat it as absolute in this context.
         (not (eq (aref file 0) ?~))
         (setq already-absolute  t))
    (cond ((null file) nil)
          ((eq localp 'verbatim) file)
          ((and (not no-error-if-not-filep) (member file '("." ".." "./" "../")))
           (error "Cannot operate on `.' or `..'"))
          ((and (eq localp 'no-dir) already-absolute)
           (file-name-nondirectory file))
          (already-absolute
           (let ((handler  (find-file-name-handler file nil)))
             ;; check for safe-magic property so that we won't
             ;; put /: for names that don't really need them.
             ;; For instance, .gz files when auto-compression-mode is on.
             (if (and handler (not (get handler 'safe-magic)))
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


;; REPLACE ORIGINAL in `dired.el':
;;
;; Display a message to warn that flagged, not marked, files will be deleted.
;;
;;;###autoload
(defun dired-do-flagged-delete (&optional no-msg)
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
    (if (save-excursion (goto-char (point-min))
                        (re-search-forward regexp nil t))
        (dired-internal-do-deletions
         ;; This can't move point since last arg is nil.
         (dired-map-over-marks (cons (dired-get-filename) (point)) nil)
         nil)
      (unless no-msg (message "(No deletions requested.)")))))


;; REPLACE ORIGINAL in `dired.el':
;;
;; Display a message to warn that marked, not flagged, files will be deleted.
;;
;;;###autoload
(defun dired-do-delete (&optional arg)
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
  (dired-internal-do-deletions
   ;; This may move point if ARG is an integer.
   (dired-map-over-marks (cons (dired-get-filename) (point)) arg)
   arg))

;;;###autoload
(defun diredp-capitalize (&optional arg)
  "Rename all marked (or next ARG) files by capitilizing them.
This gives the file name(s) a first character in upper case and the
rest lower case."
  (interactive "P")
  (dired-rename-non-directory #'capitalize "Rename by capitalizing:" arg))


;;; Versions of `dired-do-*' commands for just this line's file.
;;;###autoload
(defun diredp-delete-this-file ()
  "In dired, delete the file on the cursor line, upon confirmation."
  (interactive) (dired-do-delete 1))

;;;###autoload
(defun diredp-capitalize-this-file ()
  "In dired, rename the file on the cursor line by capitilizing it.
This gives the file name a first character in upper case and the rest
lower case."
  (interactive) (diredp-capitalize 1))

;;;###autoload
(defun diredp-downcase-this-file ()
  "In dired, rename the file on the cursor line to lower case."
  (interactive) (dired-downcase 1))

;;;###autoload
(defun diredp-upcase-this-file ()
  "In dired, rename the file on the cursor line to upper case."
  (interactive) (dired-upcase 1))

;;;###autoload
(defun diredp-rename-this-file ()
  "In dired, rename the file on the cursor line."
  (interactive) (dired-do-rename 1))

;;;###autoload
(defun diredp-copy-this-file ()
  "In dired, copy the file on the cursor line."
  (interactive) (dired-do-copy 1))

;;;###autoload
(defun diredp-relsymlink-this-file ()
  "In dired, make a relative symbolic link to file on cursor line."
  (interactive) (and (fboundp 'dired-do-relsymlink) (dired-do-relsymlink 1)))

;;;###autoload
(defun diredp-symlink-this-file ()
  "In dired, make a symbolic link to the file on the cursor line."
  (interactive) (dired-do-symlink 1))

;;;###autoload
(defun diredp-hardlink-this-file ()
  "In dired, add a name (hard link) to the file on the cursor line."
  (interactive) (dired-do-hardlink 1))

;;;###autoload
(defun diredp-print-this-file ()
  "In dired, print the file on the cursor line."
  (interactive) (dired-do-print 1))

;;;###autoload
(defun diredp-grep-this-file ()
  "In dired, grep the file on the cursor line."
  (interactive)
  (unless (and grep-command (or (< emacs-major-version 22) (not grep-use-null-device)
                                (eq grep-use-null-device t)))
    (grep-compute-defaults))
  (grep (diredp-do-grep-1 (list (dired-get-filename t)))))

;;;###autoload
(defun diredp-compress-this-file ()
  "In dired, compress or uncompress the file on the cursor line."
  (interactive) (dired-do-compress 1))

;;;###autoload
(defun diredp-shell-command-this-file (command)
  "In dired, run a shell COMMAND on the file on the cursor line."
  (interactive
   (list (dired-read-shell-command (concat "! on " "%s: ") 1
                                   (list (dired-get-filename t)))))
  (dired-do-shell-command command 1))

;;;###autoload
(defun diredp-byte-compile-this-file ()
  "In dired, byte compile the (Lisp source) file on the cursor line."
  (interactive) (dired-do-byte-compile 1))

;;;###autoload
(defun diredp-load-this-file ()
  "In dired, load the file on the cursor line."
  (interactive) (dired-do-load 1))

;;;###autoload
(defun diredp-chmod-this-file ()
  "In dired, change the mode of the file on the cursor line."
  (interactive) (dired-do-chmod 1))

;;;###autoload
(defun diredp-chgrp-this-file ()
  "In dired, change the group of the file on the cursor line."
  (interactive) (dired-do-chgrp 1))

;;;###autoload
(defun diredp-chown-this-file ()
  "In dired, change the owner of the file on the cursor line."
  (interactive) (dired-do-chown 1))


;; REPLACE ORIGINAL in `dired-x.el'.
;;
;; 1. Variable (symbol) `s' -> `blks'.
;; 2. Fixes to remove leading space from `uid' and allow `.' in `gid'.
;; 3. Cleaned up doc string and code a bit.
;;
;;;###autoload
(defun dired-mark-sexp (predicate &optional unmark-p)
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
            (setq inode  (string-to-number (buffer-substring (match-beginning 1)
                                                             (match-end 1)))
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
          (setq size  (string-to-number (buffer-substring (save-excursion
                                                            (backward-word 1)
                                                            (setq pos  (point)))
                                                          (point))))
          (goto-char pos)
          (backward-word 1)
          ;; if no gid is displayed, gid will be set to uid
          ;; but user will then not reference it anyway in PREDICATE.
          (setq gid   (buffer-substring (save-excursion (forward-word 1) (point)) (point))
                time  (buffer-substring (match-beginning 1) (1- (dired-move-to-filename)))
                name  (buffer-substring (point) (or (dired-move-to-end-of-filename t)
                                                    (point)))
                sym   (if (looking-at " -> ")
                          (buffer-substring (progn (forward-char 4) (point))
                                            (progn (end-of-line) (point)))
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
(defun diredp-mark-region-files (&optional unmark-p)
  "Mark all of the files in the current region (if it is active).
With non-nil prefix arg, unmark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (beginning-of-line) (point))
          end  (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (diredp-this-file-unmarked-p))
                     "region file"))))

;;;###autoload
(defun diredp-unmark-region-files (&optional mark-p)
  "Unmark all of the files in the current region (if it is active).
With non-nil prefix arg, mark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (beginning-of-line) (point))
          end  (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char  (if mark-p dired-marker-char ?\040)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (diredp-this-file-marked-p))
                     "region file"))))

;;;###autoload
(defun diredp-flag-region-files-for-deletion ()
  "Flag all of the files in the current region (if it is active) for deletion."
  (interactive)
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg  (save-excursion (goto-char beg) (beginning-of-line) (point))
          end  (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char  dired-del-marker))
      (dired-mark-if (and (<= (point) end) (>= (point) beg) (diredp-this-file-unmarked-p ?\D))
                     "region file"))))

;;;###autoload
(defun diredp-toggle-marks-in-region (start end)
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
(defun diredp-mouse-3-menu (event)
  "Dired pop-up `mouse-3' menu, for files in selection or current line."
  (interactive "e")
  (if (not (and (fboundp 'mouse3-dired-use-menu) mark-active))
      (let (choice)                     ; No `mouse3.el' or no region.
        (if mark-active
            (setq choice  (x-popup-menu
                           event
                           (list
                            "Files in Region"
                            (list
                             ""
                             '("Mark" . diredp-mark-region-files)
                             '("Unmark" . diredp-unmark-region-files)
                             '("Toggle Marked/Unmarked" . diredp-toggle-marks-in-region)
                             '("Flag for Deletion" .
                               diredp-flag-region-files-for-deletion)))))
          (let* ((mouse-pos                  (event-start event))
                 (inhibit-field-text-motion  t) ; Just in case.
                 bol eol
                 (file/dir-name
                  (save-excursion
                    (set-buffer (window-buffer (posn-window mouse-pos)))
                    (save-excursion
                      (goto-char (posn-point mouse-pos))
                      (save-excursion
                        (setq bol  (progn (beginning-of-line) (point))
                              eol  (progn (end-of-line) (point))))
                      (if diredp-file-line-overlay ; Don't re-create if exists.
                          (move-overlay diredp-file-line-overlay bol eol
                                        (current-buffer))
                        (setq diredp-file-line-overlay  (make-overlay bol eol))
                        (overlay-put diredp-file-line-overlay 'face 'region))
                      (and (not (eobp)) (dired-get-filename nil t))))))
            (sit-for 0)
            (setq choice
                  (x-popup-menu
                   (and file/dir-name event)
                   (list
                    "This File"
                    (if file/dir-name
                        (list
                         file/dir-name

                         ;; Stuff from `Mark' menu.
                         (if (dired-file-marker file/dir-name)
                             '("Unmark" . diredp-mouse-unmark) ; It's now marked.
                           '("Mark" . diredp-mouse-mark)) ;  It's now unmarked.
                         '("Flag for Deletion" . diredp-mouse-flag-file-deletion)
                         '("--")        ; Separator.

                         ;; Stuff from `Single' / `Multiple' menus.
                         '("Open" . diredp-mouse-find-file)
                         '("Open in Other Window" .
                           dired-mouse-find-file-other-window)
                         '("Open in Other Frame" .
                           diredp-mouse-find-file-other-frame)
                         (and (featurep 'w32-browser)
                              '("Open Associated Windows App" . dired-mouse-w32-browser))
                         (and (featurep 'w32-browser)
                              '("Open in Windows Explorer" . dired-mouse-w32explore))
                         '("View (Read Only)" . diredp-mouse-view-file)
                         '("--")        ; Separator.

                         '("Compare..." . diredp-mouse-ediff)
                         '("Diff..." . diredp-mouse-diff)
                         '("Diff with Backup" . diredp-mouse-backup-diff)
                         '("--")        ; Separator.

                         '("Bookmark..." . diredp-mouse-do-bookmark)
                         '("Copy to..." . diredp-mouse-do-copy)
                         '("Rename to..." . diredp-mouse-do-rename)
                         '("Upcase" . diredp-mouse-upcase)
                         '("Downcase" . diredp-mouse-downcase)
                         '("Delete" . diredp-mouse-do-delete)
                         '("Shell Command..." . diredp-mouse-do-shell-command)
                         (and (fboundp 'dired-do-relsymlink)
                              '("Symlink to (Relative)..."
                                . dired-do-relsymlink))
                         '("Symlink to..." . diredp-mouse-do-symlink)
                         '("Hardlink to..." . diredp-mouse-do-hardlink)
                         '("Print" . diredp-mouse-do-print)
                         '("Grep" . diredp-mouse-do-grep)
                         '("Compress/Uncompress" . diredp-mouse-do-compress)
                         '("Byte Compile" . diredp-mouse-do-byte-compile)
                         '("Load" . diredp-mouse-do-load)
                         '("Change Mode..." . diredp-mouse-do-chmod)
                         '("Change Group..." . diredp-mouse-do-chgrp)
                         '("Change Owner..." . diredp-mouse-do-chown)
                         )
                      '("" (""))))))    ; No menu: not on a file line.
            (when diredp-file-line-overlay
              (delete-overlay diredp-file-line-overlay))))
        (and choice (call-interactively choice)))
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
  (defun dired-find-file ()
    "In dired, visit the file or directory named on this line."
    (interactive)
    (let* ((dgf-result  (or (dired-get-filename nil t) (error "No file on this line")))
           (file-name   (file-name-sans-versions dgf-result t)))
      (if (file-exists-p file-name)
          (find-file file-name)
        (if (file-symlink-p file-name)
            (error "File is a symlink to a nonexistent target")
          (error "File no longer exists; type `g' to update Dired buffer"))))))

;;;###autoload
(defun diredp-find-file-other-frame ()
  "In dired, visit this file or directory in another frame."
  (interactive)
  (find-file-other-frame (file-name-sans-versions (dired-get-filename nil t) t)))

;;;###autoload
(defun diredp-mouse-find-file-other-frame (event)
  "In dired, visit file or directory clicked on in another frame."
  (interactive "e")
  (let ((pop-up-frames  t))
    (dired-mouse-find-file-other-window event)))


;; REPLACE ORIGINAL in `dired.el'.
;;
;; Allow `.' and `..', by using non-nil second arg to `dired-get-filename'.
;;
;;;###autoload
(defun dired-mouse-find-file-other-window (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file  (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (find-file-other-window (file-name-sans-versions file t))))

;;;###autoload
(defun diredp-mouse-find-file (event)
  "Replace dired in its window by this file or directory."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file  (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

;;;###autoload
(defun diredp-mouse-view-file (event)
  "Examine this file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file  (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist) (dired-goto-subdir file))
            (dired file))
      (view-file file))))               ; In `view.el'.

;;;###autoload
(defun diredp-mouse-ediff (event)
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
(defun diredp-mouse-diff (event &optional switches)
  "Compare this file (pointed by mouse) with file FILE2 using `diff'.
FILE2 defaults to the file at the mark.  This file is the first file
given to `diff'.  With prefix arg, prompt for second arg SWITCHES,
which are options for `diff'."
  (interactive "e")
  (let ((default    (if (mark t)
                        (save-excursion (goto-char (mark t))
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
(defun diredp-mouse-backup-diff (event)
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
                                          (mapconcat 'identity diff-switches " "))))))
        (mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (diff-backup (dired-get-filename) switches)))

;;;###autoload
(defun diredp-mouse-mark (event)
  "In dired, mark this file.
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
(defun diredp-mouse-unmark (event)
  "In dired, unmark this file.
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
(defun diredp-mouse-mark/unmark (event)
  "Mark/unmark file or directory at mouse EVENT."
  (interactive "e")
  (let* ((mouse-pos                  (event-start event))
         (inhibit-field-text-motion  t) ; Just in case.
         bol eol
         (file/dir-name
          (save-excursion
            (set-buffer (window-buffer (posn-window mouse-pos)))
            (save-excursion
              (goto-char (posn-point mouse-pos))
              (save-excursion
                (setq bol  (progn (beginning-of-line) (point))
                      eol  (progn (end-of-line) (point))))
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
(defun diredp-mouse-mark-region-files (event)
  "Mark files between point and the mouse."
  (interactive "e")
  (call-interactively 'mouse-save-then-kill)
  (diredp-mark-region-files))

;; This can be bound to [S-mouse-1] to give behavior similar to Windows Explorer.
;; If you don't bind `diredp-mouse-mark/unmark' to, for instance, `C-mouse-1', then
;; Consider binding this to [S-mouse-1].
;;;###autoload
(defun diredp-mouse-mark/unmark-mark-region-files (event)
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
(defun diredp-mouse-flag-file-deletion (event)
  "In dired, flag this file for deletion.
If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (let ((dired-marker-char  dired-del-marker)) (dired-mark 1))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-copy (event)
  "In dired, copy this file.
This normally preserves the last-modified date when copying."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'copy #'dired-copy-file
                         (if dired-copy-preserve-time "Copy [-p]" "Copy")
                         1 dired-keep-marker-copy))

;;;###autoload
(defun diredp-mouse-do-rename (event)
  "In dired, rename this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'move #'dired-rename-file "Move" 1 dired-keep-marker-rename "Rename"))

;;;###autoload
(defun diredp-mouse-upcase (event)
  "In dired, rename this file to upper case."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory #'upcase "Rename to uppercase:" nil))

;;;###autoload
(defun diredp-mouse-downcase (event)
  "In dired, rename this file to lower case."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-rename-non-directory #'downcase "Rename to lowercase:" nil))

;;;###autoload
(defun diredp-mouse-do-delete (event)
  "In dired, delete this file, upon confirmation."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-internal-do-deletions (dired-map-over-marks (cons (dired-get-filename)
                                                           (point)) 1)
                               1)
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-shell-command (event)
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
  (let ((mouse-pos  (event-start event))
        (command    (dired-read-shell-command "! on %s: " nil (dired-get-marked-files t nil))))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-bunch-files (- 10000 (length command))
                       #'(lambda (&rest files)
                           (dired-run-shell-command (dired-shell-stuff-it command files t 1)))
                       nil
                       (dired-get-marked-files t 1))))

;;;###autoload
(defun diredp-mouse-do-symlink (event)
  "Make symbolic link to this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'symlink #'make-symbolic-link "Symlink" 1 dired-keep-marker-symlink))

;;;###autoload
(defun diredp-mouse-do-hardlink (event)
  "Make hard link (alias) to this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-create-files 'hardlink #'add-name-to-file "Hardlink" 1 dired-keep-marker-hardlink))

;;;###autoload
(defun diredp-mouse-do-print (event)
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
(defun diredp-mouse-do-grep (event)
  "Run grep against this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (unless grep-command (grep-compute-defaults))
  (grep (diredp-do-grep-1 (list (dired-get-filename t)))))

;;;###autoload
(defun diredp-mouse-do-compress (event)
  "Compress or uncompress this file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-compress 1 'compress t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-byte-compile (event)
  "Byte compile this file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-byte-compile 1 'byte-compile t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-load (event)
  "Load this Emacs Lisp file."
  (interactive "e")
  (let ((mouse-pos         (event-start event))
        (dired-no-confirm  t))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos))
    (dired-map-over-marks-check #'dired-load 1 'load t))
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chmod (event)
  "Change the mode of this file.
This calls chmod, so symbolic modes like `g+w' are allowed."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Mode" "chmod" 'chmod 1)
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chgrp (event)
  "Change the group of this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Group" "chgrp" 'chgrp 1)
  (dired-previous-line 1))

;;;###autoload
(defun diredp-mouse-do-chown (event)
  "Change the owner of this file."
  (interactive "e")
  (let ((mouse-pos  (event-start event)))
    (select-window (posn-window mouse-pos))
    (goto-char (posn-point mouse-pos)))
  (dired-do-chxxx "Owner" dired-chown-program 'chown 1)
  (dired-previous-line 1))

;;;;;;;;;;;;

(setq diredp-loaded-p  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired+.el ends here

;;; buff-menu+.el --- Extensions to `buff-menu.el'.    -*- coding:utf-8 -*-
;;
;; Filename: buff-menu+.el
;; Description: Extensions to `buff-menu.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2014, Drew Adams, all rights reserved.
;; Created: Mon Sep 11 10:29:56 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Dec 26 08:34:09 2013 (-0800)
;;           By: dradams
;;     Update #: 2842
;; URL: http://www.emacswiki.org/buff-menu+.el
;; Doc URL: http://www.emacswiki.org/BufferMenuPlus
;; Keywords: mouse, local, convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.1
;;
;; Features that might be required by this library:
;;
;;   `avoid', `fit-frame', `frame-fns', `misc-cmds', `misc-fns',
;;   `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `buff-menu.el', including: new bindings, faces,
;;    and menus; selective column display; and directional column
;;    sorting.
;;
;;    NOTE: Emacs Dev rewrote `buff-menu.el' for Emacs 24.2, so that
;;          it uses `tabulated-list-mode'.  I have not yet updated
;;          `buff-menu+.el' to accommodate this vanilla rewrite, and I
;;          do not know when I might get around to doing that.
;;
;;          If you want to use `buff-menu+.el' with Emacs 24.2 or
;;          later, then you can download the Emacs 23 or Emacs 24.1
;;          version of `buff-menu.el' and put that in your `load-path'
;;          in such a way that it shadows the Emacs 24.2+ version.
;;          You can get the Emacs 23.4 version here, for instance
;;          (combine the URL into a single line):
;;
;;            http://bzr.savannah.gnu.org/lh/emacs/emacs-23/download/
;;             head:/buffmenu.el-20091113204419-o5vbwnq5f7feedwu-197/buff-menu.el
;;
;;          Sorry for the inconvenience.
;;
;;    Note: By default, the buffer menu is shown in a different
;;          window.  If you prefer to show it in the current window,
;;          then just do this:
;;
;;          (add-to-list 'same-window-buffer-names "*Buffer List*")
;;
;;  Faces defined here:
;;
;;    `buffer-menu-headings', `buffer-menu-current-buffer',
;;    `buffer-menu-directory-buffer', `buffer-menu-flagged-buffer',
;;    `buffer-menu-marked-buffer', `buffer-menu-star-buffer',
;;    `buffer-menu-view-mark', `buffer-menu-delete-mark',
;;    `buffer-menu-save-mark', `buffer-menu-modified-mark',
;;    `buffer-menu-read-only-mark', `buffer-menu-buffer-name',
;;    `buffer-menu-mode', `buffer-menu-size', `buffer-menu-time',
;;    `buffer-menu-file-name'.
;;
;;  User options defined here (Emacs 22+):
;;
;;    `Buffer-menu-file-flag', `Buffer-menu-mode-flag',
;;    `Buffer-menu-time-flag', `Buffer-menu-time-format'.
;;
;;  Commands defined here:
;;
;;    `Buffer-menu-decrease-max-buffer+size' (Emacs 22+),
;;    `Buffer-menu-delete-flagged',
;;    `Buffer-menu-increase-max-buffer+size' (Emacs 22+),
;;    `Buffer-menu-mouse-3-menu', `Buffer-menu-mouse-delete',
;;    `Buffer-menu-mouse-execute', `Buffer-menu-mouse-modified',
;;    `Buffer-menu-mouse-other-window', `Buffer-menu-mouse-save',
;;    `Buffer-menu-mouse-unmark', `Buffer-menu-toggle-file-column'
;;    (Emacs 22+), `Buffer-menu-toggle-mode-column' (Emacs 22+),
;;    `Buffer-menu-toggle-time-column' (Emacs 22+),
;;    `Buffer-menu-toggle-time-format' (Emacs 22+).
;;
;;  Internal variables defined here:
;;
;;    `buffer-menu-buffer-name',
;;    `Buffer-menu-buffer+size-computed-width',
;;    `buffer-menu-current-buffer', `buffer-menu-directory-buffer',
;;    `buffer-menu-flagged-buffer', `buffer-menu-marked-buffer',
;;    `buffer-menu-star-buffer', `buffer-menu-delete-mark',
;;    `buffer-menu-file-name', `buffer-menu-font-lock-keywords',
;;    `buffer-menu-headings', `buffer-menu-mode',
;;    `buffer-menu-modified-mark', `buffer-menu-read-only-mark',
;;    `buffer-menu-save-mark', `buffer-menu-size', `buffer-menu-time',
;;    `buffer-menu-view-mark'.
;;
;;  Other functions defined here:
;;
;;    `Buffer-menu-fontify-and-adjust-frame',
;;    `buffer-menu-nb-marked-in-mode-name',
;;    `buffer-menu-set-default-value'.
;;
;;
;;  ***** NOTE: The following user option (variable) defined in
;;              `buff-menu.el' has been REDEFINED HERE:
;;
;;  `Buffer-menu-sort-column' - A user option now. Numeric, default=1.
;;
;;
;;  ***** NOTE: The following hook defined in `buff-menu.el'
;;              has been REDEFINED HERE:
;;
;;  `Buffer-menu-mode-hook' (aka `buffer-menu-mode-hook') -
;;     Fontify buffer and fits its frame.
;;     Add number of marked and flagged lines to mode in mode line.
;;
;;
;;  ***** NOTE: The following functions defined in `buff-menu.el'
;;              have been REDEFINED HERE:
;;
;;  `buffer-menu' -
;;     1. Different help message.
;;     2. Prefix ARG =< 0 now means list (all) buffers alphabetically.
;;        (It used to mean the same as ARG > 0.)
;;        Prefix ARG >= 0 means list just file buffers.
;;     3. Use pop-to-buffer instead of switch-to-buffer.
;;  `Buffer-menu-beginning' - Protected with `boundp' for Emacs 20.
;;  `Buffer-menu-buffer+size' - Use computed width for Buffer + Size.
;;  `Buffer-menu-execute' - Deletes windows (frame) when kills buffer.
;;  `Buffer-menu-make-sort-button' -
;;     1. If same column as last sort, flip direction of sort.
;;     2. Column header face indicates sort direction.
;;     3. CRM is indicated by COLUMN = 1, not by nil COLUMN.
;;  `Buffer-menu-mode' -
;;     1. Doc string reflects new bindings.
;;     2. mouse-face on whole line, not just buffer name.
;;  `Buffer-menu-select' - When Buffer Menu is `window-dedicated-p',
;;                         uses `pop-to-buffer' to display.
;;  `Buffer-menu-sort' -
;;     1. Allow negative COLUMN. Allow COLUMN = 1 or -1.
;;     2. When COLUMN = `Buffer-menu-sort-column', then flip that.
;;     3. Added message at end indicating the kind of sort.
;;  `list-buffers-noselect' - Use longest buffer name+size to indent.
;;                          - Change sort direction if same column.
;;                          - Add sort buttons for CRM and Time also.
;;                          - Sort test is different: no sort for CRM.
;;                          - Go to bob if `desired-point' undefined.
;;
;;  In your init file (`~/.emacs') file, do this:
;;
;;    (require 'buff-menu+)
;;
;;  NOTE:
;;
;;  1. This file MUST be saved with encoding UTF-8 or equivalent,
;;     because it contains an em-dash character.
;;
;;  2. If you byte-compile this using a version of Emacs prior to 23,
;;     and you use the byte-compiled file with Emacs 23 or later, then
;;     some keys, such as `q', will not be defined in the buffer list.
;;     (So byte-compile it using Emacs 23 or later.)
;;
;;  3. Starting with Emacs 24.3, Emacs development changed
;;     `buff-menu.el' so that it is based on `tabulated-list' mode.
;;     Unfortunately, that breaks the `buff-menu+.el' enhancements.  I
;;     have not had the time to update `buff-menu+.el' for
;;     compatibility with Emacs 24.3 and later.  If you want to use
;;     `buff-menu+.el' with Emacs 24.3 or later, you can download the
;;     Emacs 23 version of `buff-menu.el' and put that in your
;;     `load-path'.  You will lose no features if you do that: Emacs
;;     24.3 and later add no enhancements to `buff-menu.el' - they
;;     just base it on `tabulated-list.el'.  You can download Emacs 23
;;     `buff-menu.el' here: http://ftp.gnu.org/gnu/emacs/ or here:
;;     http://www.gnu.org/prep/ftp.html.  That version will work fine
;;     with Emacs 24.3 and later and with `buff-menu+.el'.  I might
;;     eventually get around to updating `buff-menu+.el' to
;;     accommodate the `buff-menu.el' change, but it is not my first
;;     priority.  Sorry for this annoyance.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2013/04/20 dadams
;;     list-buffers-noselect and globally:
;;       Set Buffer-menu-buffer+size-width to 26 if nil or unbound.  Emacs 24.3+ defaults it to nil.
;; 2012/08/28 dadams
;;     Buffer-menu-select: Updated for Emacs 24.3+ (but I don't yet support > 24.2).
;;     Handle Emacs 23 capitalization of buffer-menu-mode-hook.
;; 2012/06/21 dadams
;;     Added: buffer-menu-nb-marked-in-mode-name.
;; 2012/01/15 dadams
;;     Rename commands to capitalized Buffer-menu from buffer-menu.
;;     Bind Buffer-menu-toggle-(file|mode|time)-column to M-f, M-m, M-t.
;;     Bind Buffer-menu-toggle-time-format to C-M-t.
;;     Bind Buffer-menu-delete-flagged to C-M-x for all Emacs versions.
;;     Buffer-menu-mode: Use \\[...] for those commands.
;; 2011/12/19 dadams
;;     Buffer-menu-mode, Buffer-menu-mouse-3-menu: Use line-end-position, not end-of-line + point.
;; 2011/10/27 dadams
;;     Handle Dired buffers like file buffers, e.g., wrt arg FILES-ONLY.
;; 2011/06/29 dadams
;;     Buffer-menu-buffer+size: Corrected SPC syntax (for Emacs 20) from ?\s  to ?\ .
;; 2011/05/02 dadams
;;     buffer-menu-font-lock-keywords, Buffer-menu(-mouse)-execute, list-buffers-noselect,
;;       Buffer-menu-mouse-unmark:
;;         Accommodate frame-bufs.el.
;; 2011/04/28 dadams
;;     Buffer-menu-fontify-and-adjust-frame:
;;       Fix bug introduced 2011-04-16: Do frame operations inside save-window-excursion.
;; 2011/04/22 dadams
;;     list-buffers-noselect: Removed (if buffer-list... around the dolist.
;; 2011/04/18 dadams
;;     Buffer-menu-sort, Buffer-menu-revert-function, buffer-menu:
;;       Use Buffer-menu-buffer-column, not hard-coded 4.  Thx to Alp Aker.
;; 2011/04/16 dadams
;;     Buffer-menu-fontify-and-adjust-frame:
;;       Use with-current-buffer, not save(-window)-excursion.  Thx to Alp Aker.
;; 2011/02/20 dadams
;;     list-buffers-noselect: Use only Emacs 22+ call to format-mode-line.
;;                            Use property font-lock-face, not face.
;;     Buffer-menu-sort: Typo: setq buffer-read-only was missing value.
;; 2011/02/03 dadams
;;     All deffaces: Provided default values for dark-background screens too.
;; 2011/01/03 dadams
;;     Removed autoload cookies from non def* sexps.
;; 2010/07/15 dadams
;;     Buffer-menu-fontify-and-adjust-frame:
;;       Added call to font-lock-refresh-defaults.  Thx to Bastian Beischer.
;; 2009/07/27 dadams
;;     Buffer-menu-revert-function: Updated wrt Emacs 23: use window-buffer.
;;     Buffer-menu-mode (Emacs 23): Use revert-buffer in doc string, not Buffer-menu-revert.
;;     Buffer-menu-(sort|delete-flagged): Buffer-menu-revert -> Buffer-menu-revert-function.
;;     Do defvars to silence byte-compiler for all Emacs versions.
;; 2009/05/26 dadams
;;     Buffer-menu-mode: Added separate definition for Emacs 23, because derived mode (keys etc.).
;; 2009/04/16 dadams
;;     buffer-menu-font-lock-keywords: Fix regexp for truncated star buffers (...[).
;; 2009/02/22 dadams
;;     Added comment to Commentary about how to show the buffer menu in the same window.
;; 2008/02/09 dadams
;;     Added: Buffer-menu-delete-flagged. Bound to C-M-x and added to Buffer-menu-mouse-3-menu.
;; 2008/02/05 dadams
;;     Added: buffer-menu-toggle-time-format, Buffer-menu-time-format.
;;     buffer-menu-font-lock-keywords, list-buffers-noselect: Treat short and long time formats.
;;     list-buffers-noselect: Extend column buttons from one column to the next (beyond text).
;;     Removed optional arg for column toggles.
;;     Renamed buffer-menu-set-flag to buffer-menu-set-default-value.
;;     Buffer-menu-buffer+size: Add name-props and size-props only if non-nil.
;; 2008/02/04 dadams
;;     buffer-menu-font-lock-keywords: Highlight Dired buffer name even if no Mode.
;;                                     Allow multiple digits in <N>.
;;     list-buffers-noselect:
;;       Compute longest buffer name + size combination, and use when indenting file name.
;;     Added: Buffer-menu-buffer+size, buffer-menu-(de|in)crease-max-buffer+size (and bound),
;;            Buffer-menu-buffer+size-computed-width.
;;     buffer-menu: Updated doc string.
;;     Thx to Tom Wurgler for suggestions.
;; 2008/02/03 dadams
;;     buffer-menu-font-lock-keywords: Highlight Dired buffer name, if file flag but no mode flag.
;; 2008/02/01 dadams
;;     Allow <0-9> after star buffer names.  Thx to Tom Wurgler.
;; 2008/01/31 dadams
;;     Added options to hide columns.  Thx to Tom Wurgler for the suggestion.
;;       Buffer-menu-*-flag, and changed buffer-menu-font-lock-keywords accordingly.
;;       list-buffers-noselect: Update for Buffer-menu-*-flag.
;;       Added: *-set-flag, *-toggle-*-column, buffer-menu-font-lock-keywords (function).
;;     Group Buffer-Menu-Plus: Remove prefix.  Different parent group for Emacs 22.
;; 2008/01/05 dadams
;;     list-buffers-noselect: Eval underline char at compile time - see comment.
;;     Buffer-menu-sort-column: Made it a user option.
;;     Removed: Buffer-menu-sort-button-map, Buffer-menu-sort-by-column.
;;     Don't define for Emacs before version 22: Buffer-menu-make-sort-button, Buffer-menu-sort*.
;; 2007/09/23 dadams
;;     Removed second arg to undefine-killer-commands.
;;     Brought up to date with Emacs 22.
;; 2007/02/28 dadams
;;     buffer-menu-font-lock-keywords: regexp tweaks.
;; 2007/02/26 dadams
;;     Added: buffer-menu-(star|directory|marked|flagged)-buffer.
;;     buffer-menu-font-lock-keywords: Treat new faces.  Thx to Tom Wurgler <twurgl@goodyear.com>.
;; 2007/02/16 dadams
;;     buffer-menu: Fixed use of negative arg when using header line.
;;     list-buffers-noselect:
;;       Don't sort if sort column is 1 or -1.  Instead, just don't reverse list if -1.
;;       Go to point-min if desired-point is not defined.
;;     Buffer-menu-make-sort-button: Different help echoes for CRM and Buffer.
;;     Buffer-menu-sort, Buffer-menu-make-sort-button:
;;       Add to help message for CRM sort: mention Buffer-menu-use-frame-buffer-list.
;;     Buffer-menu-mode: Mention sorting columns in doc string.
;;     Add defvar for Buffer-menu-use-frame-buffer-list to quiet byte compiler.
;;     Changed all Emacs 22 tests to test emacs-major-version. 
;; 2007/02/05 dadams
;;     list-buffers-noselect: Use ?\u2014 for the EM dash.  Thx to Henry Atting.
;; 2007/02/02 dadams
;;     Added coding declaration to file header.  Thx to Henry Atting.
;;     Changed temporary emacs version tests to > emacs 21.
;; 2006/04/10 dadams
;;      list-buffers-noselect: Updated Emacs 22 version wrt latest CVS version.
;; 2006/01/07 dadams
;;      Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Added defgroup 'Buffer-Menu-Plus.  Added :link.
;; 2005/11/04 dadams
;;     Added: Buffer-menu-sort-button-map, Buffer-menu-sort-by-column, if not available in Emacs.
;; 2005/11/01 dadams
;;     Buffer-menu-make-sort-button: Updated to reflect latest CVS version:
;;       Added text property: column.  Use Buffer-menu-sort-button-map.
;;       Mention mouse-1 in :help.
;; 2005/07/08 dadams
;;     Buffer-menu-fontify-and-adjust-frame: Wrapped in save-*excursion's.
;; 2005/07/04 dadams
;;     Buffer-menu-fontify-and-adjust-frame: Fixed typo: boundp -> fboundp.
;; 2005/06/22 dadams
;;     Use defface for faces now.
;;     Renamed faces: *-face to *.
;;     No longer require def-face-const.el.
;; 2005/06/21 dadams
;;     list-buffers-noselect: Emacs 22 renamed Buffer-menu-buffer-face to Buffer-menu-buffer.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/12/05 dadams
;;     Buffer-menu-execute, Buffer-menu-mouse-execute: minor corrections.
;; 2004/11/30 dadams
;;     Added Time column (with sorting).
;;       Added buffer-menu-time-face.
;;       Buffer-menu-sort, Buffer-menu-make-sort-button, list-buffers-noselect,
;;     list-buffers-noselect: Major changes: Time column, updated to latest CVS (new 2nd arg) etc.
;;     buffer-menu-font-lock-keywords: Rewrote for time etc.
;;     Sort CRM column also now.
;;     Buffer-menu-fontify-and-fit-frame renamed to Buffer-menu-fontify-and-adjust-frame.
;;       Added raise-frame (but Emacs bug, so raise doesn't work on Windows).
;;       turn-on-font-lock, instead of font-lock-fontify-buffer.
;;     Removed defvar ;;;###autoload's.
;; 2004/11/23 dadams
;;     buffer-menu-mode-hook: call font-lock-fontify-buffer.
;;     buffer-menu: Do not call font-lock-fontify-buffer.
;;     Added Buffer-menu-fontify-and-fit-frame. Hook fits frame too.
;;     Require fit-frame.el.
;;     Removed Buffer-menu-revert.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/11/19 dadams
;;     Protected tests of Buffer-menu-use-header-line with boundp for older versions.
;; 2004/11/13 dadams
;;     buffer-menu-font-lock-keywords: overwrite for *-mode-face, *-size-face, *-file-name-face.
;; 2004/10/17 dadams
;;     Updated to include Daniel Pfeiffer's fix to buff-menu.el of bug I reported on losing
;;       marks when you sort columns:
;;         1) Added Buffer-menu-revert-function, 2) Added (and modified) Buffer-menu-beginning,
;;         3) Use Buffer-menu-beginning in Buffer-menu-execute and Buffer-menu-select,
;;         4) Buffer-menu-sort: incorporated Daniel's mark-saving code.
;;     Note: when the new version comes out (from CVS),  I will 1) update list-buffers-noselect
;;       to new version that uses 4-arg version of format-mode-line and 2) remove new definition
;;       of Buffer-menu-revert-function added here now.
;; 2004/10/16 dadams
;;     Added directional column sorting, with highlighting:
;;       Added: Buffer-menu-make-sort-button, Buffer-menu-sort.
;;       list-buffers-noselect: Add sort button for CRM. Sort directionally.
;;     Only require cl.el when compile.
;;     Buffer-menu-revert: Fontify for Emacs 21 also (needed after revert).
;; 2004/10/15 dadams
;;     Buffer-menu-mode: Don't skip first two lines if Buffer-menu-use-header-line.
;; 2004/10/13 dadams
;;     Updated for Emacs 21:
;;       buffer-menu-font-lock-keywords, Buffer-menu-mode, Buffer-menu-execute, Buffer-menu-select
;;       Added list-buffers-noselect for Emacs 21 (bug fix).
;;       require cl.el only when compile on Emacs 20.
;;     Added Buffer-menu-revert: Fontifies.
;; 2004/07/21 dadams
;;     Buffer-menu-mode: Don't set Buffer-menu-buffer-column unless < Emacs 20.
;; 2001/01/02 dadams
;;     Protect undefine-killer-commands via fboundp.
;; 1999/08/26 dadams
;;     1. Added: buffer-menu-*-face's, buffer-menu-font-lock-keywords.
;;     2. Add buffer-menu-font-lock-keywords to buffer-menu-mode-hook.
;; 1997/03/21 dadams
;;     Buffer-menu-execute, Buffer-menu-mouse-execute:
;;       Only use kill-buffer-and-its-windows if fboundp.
;; 1996/07/01 dadams
;;     buffer-menu: Prefix arg =< 0 sorts alphabetically now.
;; 1996/07/01 dadams
;;     Added redefinition of Buffer-menu-select.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/01/25 dadams
;;     1. kill-buffer -> kill-buffer-and-its-windows.
;;     2. Buffer-menu-mode: Put mouse-face on whole buffer line.
;; 1996/01/12 dadams
;;     Redefined buffer-menu.
;; 1996/01/09 dadams
;;     kill-buffer -> kill-buffer-delete-frames
;; 1995/12/28 dadams
;;     Buffer-menu-mouse-3-menu: Corrected by adding temp local var.
;; 1995/12/14 dadams
;;     1. Highlight buffer line when mouse-3 menu displayed.
;;        Added Buffer-menu-overlay.
;;     2. mouse-3 menu is reduced to non-buffer-specifics when not on a buffer line.
;; 1995/12/13 dadams
;;     Added Buffer-menu-mouse-3-menu.  Use it instead of Buffer-menu-mouse-3-map.
;; 1995/12/13 dadams
;;     1) Put back Buffer-menu-select, in place of Buffer-menu-mouse-other-window.
;;     2) Added menu on mouse-3: Added: Buffer-menu-mouse-3-map,
;;        Buffer-menu-mouse-execute, Buffer-menu-mouse-modified,
;;        Buffer-menu-mouse-delete, Buffer-menu-mouse-save,
;;        Buffer-menu-mouse-unmark.
;; 1995/09/11 dadams
;;     Buffer-menu-mode: Added bindings list to doc string.
;; 1995/09/11 dadams
;;     Redefined Buffer-menu-execute: deletes frame w/ kill.
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

 ;; Cannot do (require 'buff-menu), because `buff-menu.el' does no `provide'.
 ;; Don't want to do a (load-library "buff-menu") either, because it wouldn't
 ;; allow doing (eval-after-load "buff-menu" '(progn (require 'buff-menu+)))

(eval-when-compile (require 'cl)) ;; case, (plus, for Emacs 20: push, pop, dolist,
                                  ;;        and, for Emacs <20: cadr, when, unless)

(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(require 'misc-cmds nil t) ;; (no error if not found): kill-buffer-and-its-windows
(require 'fit-frame nil t) ;; (no error if not found): fit-frame


;; To quiet the byte compiler:
(defvar Buffer-menu-buffer+size-width)
(defvar Buffer-menu-file-flag)
(defvar Buffer-menu-files-only)
(defvar Buffer-menu-mode-flag)
(defvar Buffer-menu-mode-width)
(defvar Buffer-menu-sort-column)
(defvar Buffer-menu-sort-button-map)
(defvar Buffer-menu-time-flag)
(defvar Buffer-menu-time-format)
(defvar Buffer-menu-use-frame-buffer-list)
(defvar Buffer-menu-use-header-line)
(defvar frame-bufs-mode)                ; Defined in `frame-bufs.el'
(defvar frame-bufs-full-list)           ; Defined in `frame-bufs.el'
(defvar header-line-format)
(defvar Info-current-file)
(defvar Info-current-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;


(unless (> emacs-major-version 21)
  (defgroup Buffer-Menu-Plus nil
    "Enhancements to the buffer menu."
    :link `(url-link :tag "Send Bug Report"
            ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
buff-menu+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
    :link '(url-link :tag "Other Libraries by Drew"
            "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
    :link '(url-link :tag "Download"
            "http://www.emacswiki.org/cgi-bin/wiki/buff-menu+.el")
    :link '(url-link :tag "Description"
            "http://www.emacswiki.org/cgi-bin/wiki/BufferMenu#BufferMenuPlus")
    :link '(emacs-commentary-link :tag "Commentary" "buff-menu+")
    :group 'tools :group 'convenience))

(when (> emacs-major-version 21)
  (defgroup Buffer-Menu-Plus nil
    "Enhancements to the buffer menu."
    :link `(url-link :tag "Send Bug Report"
            ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
buff-menu+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
    :link '(url-link :tag "Other Libraries by Drew"
            "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
    :link '(url-link :tag "Download"
            "http://www.emacswiki.org/cgi-bin/wiki/buff-menu+.el")
    :link '(url-link :tag "Description"
            "http://www.emacswiki.org/cgi-bin/wiki/BufferMenu#BufferMenuPlus")
    :link '(emacs-commentary-link :tag "Commentary" "buff-menu+")
    :group 'Buffer-menu :group 'tools :group 'convenience)

  (defvar Buffer-menu-buffer+size-computed-width 0
    "Max width of all buffer names, plus 4 for initial `CRM '.")


  (define-key Buffer-menu-mode-map "-" 'Buffer-menu-decrease-max-buffer+size)

  (defun Buffer-menu-decrease-max-buffer+size () ; Bound to `-'.
    "Decrease option `Buffer-menu-buffer+size-width' by one."
    (interactive)
    (let ((orig  Buffer-menu-buffer+size-width))
      (condition-case nil
          (progn
            (setq Buffer-menu-buffer+size-width  (1- Buffer-menu-buffer+size-width))
            (buffer-menu)
            (message "New max width: %s" Buffer-menu-buffer+size-width))
        (error (progn (setq Buffer-menu-buffer+size-width  orig)
                      (buffer-menu)
                      (error "Cannot decrease further"))))))


  (define-key Buffer-menu-mode-map "+" 'Buffer-menu-increase-max-buffer+size)

  (defun Buffer-menu-increase-max-buffer+size () ; Bound to `+'.
    "Increase option `Buffer-menu-buffer+size-width' by one."
    (interactive)
    (when (> (1+ Buffer-menu-buffer+size-width) 150) (error "Cannot increase further"))
    (setq Buffer-menu-buffer+size-width  (1+ Buffer-menu-buffer+size-width))
    (buffer-menu)
    (message "New max width: %s" Buffer-menu-buffer+size-width))


  (define-key Buffer-menu-mode-map "\C-\M-t" 'Buffer-menu-toggle-time-format)

  (defun Buffer-menu-toggle-time-format () ; Bound to `C-M-t'.
    "Toggle `Buffer-menu-time-format' and redisplay Buffer Menu."
    (interactive)
    (buffer-menu-set-default-value 'Buffer-menu-time-format
                                   (if (eq 'short Buffer-menu-time-format) 'long 'short))
    (buffer-menu))


  (define-key Buffer-menu-mode-map "\M-t" 'Buffer-menu-toggle-time-column)

  (defun Buffer-menu-toggle-time-column () ; Bound to `M-t'.
    "Toggle `Buffer-menu-time-flag' and redisplay Buffer Menu."
    (interactive)
    (buffer-menu-set-default-value 'Buffer-menu-time-flag (not Buffer-menu-time-flag))
    (buffer-menu))


  (define-key Buffer-menu-mode-map "\M-m" 'Buffer-menu-toggle-mode-column)

  (defun Buffer-menu-toggle-mode-column () ; Bound to `M-m'.
    "Toggle `Buffer-menu-mode-flag' and redisplay Buffer Menu."
    (interactive)
    (buffer-menu-set-default-value 'Buffer-menu-mode-flag (not Buffer-menu-mode-flag))
    (buffer-menu))


  (define-key Buffer-menu-mode-map "\M-f" 'Buffer-menu-toggle-file-column)

  (defun Buffer-menu-toggle-file-column () ; Bound to `M-f'.
    "Toggle `Buffer-menu-file-flag' and redisplay Buffer Menu."
    (interactive)
    (buffer-menu-set-default-value 'Buffer-menu-file-flag (not Buffer-menu-file-flag))
    (buffer-menu))

  (defun buffer-menu-set-default-value (symb val)
    "Set default value of SYMB to VAL.
Update `buffer-menu-font-lock-keywords' accordingly."
    (set-default symb val)
    (setq buffer-menu-font-lock-keywords  (buffer-menu-font-lock-keywords)))

  (defun buffer-menu-font-lock-keywords ()
    "Returns the list of font lock keywords for the buffer menu."
    (let* ((frame-bufs-p       (and (boundp 'frame-bufs-mode) frame-bufs-mode))
           (bit-column-header  (if frame-bufs-p "CRM[ F].*" "CRM.*"))
           (bits               (if frame-bufs-p "....." "...."))
           (shortbits          (if frame-bufs-p "...." "...")))
      (list
       (list (concat "^\\(" bit-column-header "\\)") 1 'buffer-menu-headings) ; CRM or CRMF
       (list (concat "^" bits
                     "\\(.*[^ \t\n]\\)[ \t]+[0-9]+[ \t]+\\(.* \\(AM\\|PM\\)\\)?\\([^/\n]+\\)")
             (list 1 'buffer-menu-buffer-name)) ; Default buffer name
       (cond (Buffer-menu-mode-flag     ; Directory buffer name
              (list (concat "^" bits "\\(.*[^ \t\n]\\)[ \t]+[0-9]+[ \t]+[a-zA-Z :0-9]*[ \t]+Dired")
                    1 'buffer-menu-directory-buffer t t))
             (Buffer-menu-file-flag
              (list (concat "^" bits "\\(.*[^ \t\n]\\)[ \t]+[0-9]+[ \t]+[^/\n]+[ \t\n]\
\\(\\([~]\\|\\([a-zA-Z]:\\)\\)?/.*/\\)$")
                    1 'buffer-menu-directory-buffer t t))
             ;; We can't show that it's a directory, because we have no way of knowing that
             (t (list "")))
       (list (concat "^" bits
                     "\\(\\*.*[^ \t\n][[*]\\(<[0-9]+>\\)?\\)[ \t]+") ; Star buffer (e.g. *scratch*)
             1 'buffer-menu-star-buffer t t)
       ;; Time & Mode
       (cond ((and Buffer-menu-time-flag Buffer-menu-mode-flag (eq 'short Buffer-menu-time-format))
              (list "^.*[ \t][0-9]+[ \t]+\\([0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\)?\\([^\n]+\\)"
                    (list 1 'buffer-menu-time t t) (list 2 'buffer-menu-mode t t)))
             ((and Buffer-menu-time-flag Buffer-menu-mode-flag)
              (list "^.*[ \t][0-9]+[ \t]+\\(.* \\(AM\\|PM\\)\\)?\\([^\n]+\\)"
                    (list 1 'buffer-menu-time t t) (list 3 'buffer-menu-mode t t)))
             (Buffer-menu-time-flag
              (if (eq 'short Buffer-menu-time-format)
                  (list "^.*[ \t][0-9]+[ \t]+\\([0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\)?"
                        (list 1 'buffer-menu-time t t))
                (list "^.*[ \t][0-9]+[ \t]+\\(.* \\(AM\\|PM\\)\\)?" (list 1 'buffer-menu-time t t))))
             (Buffer-menu-mode-flag
              (list "^.*[ \t][0-9]+[ \t]+\\([^/\n]+\\)" 1 'buffer-menu-mode t t))
             (t ""))
       (list "^.*[ \t]\\([0-9]+\\)\\(  \\|[\n]\\)" 1 'buffer-menu-size t t) ; Size
       (if Buffer-menu-file-flag
           (list "^.*[ \t][0-9]+[ \t]+[^/\n]+[ \t\n]\
\\(\\(\\([~]\\|\\([a-zA-Z]:\\)\\)*/.*\\)\\|([^ \t]+).*\\)$" ; File name of Info file + node
                 1 'buffer-menu-file-name t t)
         "")
       (list "^\\([.]\\)" 1 'buffer-menu-current-buffer t t) ; Current buffer mark (.)
       (list "^\\(>\\)" 1 'buffer-menu-view-mark t t) ; To view mark (>)
       (list (concat "^>" shortbits
                     "\\(.*[^ \t\n]\\)[ \t]+[0-9]+[ \t]+\\(.* \\(AM\\|PM\\)\\)?\\([^/\n]+\\)")
             (list 1 'buffer-menu-marked-buffer 'prepend t)) ; Buffer name when marked (>)
       (list (concat "^D" shortbits
                     "\\(.*[^ \t\n]\\)[ \t]+[0-9]+[ \t]+\\(.* \\(AM\\|PM\\)\\)?\\([^/\n]+\\)")
             (list 1 'buffer-menu-flagged-buffer t t)) ; Buffer name when flagged (D)
       (list "^\\(D\\)" 1 'buffer-menu-delete-mark t t) ; Deletion flag (D)
       (list "^..\\(S\\)" 1 'buffer-menu-save-mark t t) ; Save flag (S)
       (list "^..\\([*]\\)" 1 'buffer-menu-modified-mark t t) ; Buffer-modified-p (*)
       (list "^.\\(%\\)" 1 'buffer-menu-read-only-mark t t) ; Read-only-p (%)
       )))

  (defcustom Buffer-menu-time-format 'short
    "*Format for Time column of buffer menu."
    :type '(choice
            (const :tag "Short: hh:mm:ss"          short)
            (const :tag "Long: day hh:mm:ss AM/PM" long))
    :group 'Buffer-Menu-Plus)

  (defcustom Buffer-menu-time-flag t
    "*Non-nil means Buffer Menu displays the last time the buffer was displayed."
    :type 'boolean :group 'Buffer-Menu-Plus
    :initialize 'custom-initialize-default
    :set 'buffer-menu-set-default-value)

  (defcustom Buffer-menu-mode-flag t
    "*Non-nil means Buffer Menu displays the buffer's mode."
    :type 'boolean :group 'Buffer-Menu-Plus
    :initialize 'custom-initialize-default
    :set 'buffer-menu-set-default-value)

  (defcustom Buffer-menu-file-flag t
    "*Non-nil means Buffer Menu displays the buffer's file."
    :type 'boolean :group 'Buffer-Menu-Plus
    :initialize 'custom-initialize-default
    :set 'buffer-menu-set-default-value)  


  ;; REPLACE ORIGINAL in `buff-menu.el'.
  ;;
  ;; A user option now.  It must be numeric.  Initial value is 1, not nil.
  ;;
  (defcustom Buffer-menu-sort-column 1
    "*Sorted by (1) visit, (2) buffer, (3) size, (4) time, (5) mode, (6) file.
Click a column heading to sort by that field and update this option."
    :type '(choice
            (const :tag "Sort by time of last visit" 1)
            (const :tag "Sort by buffer name"        2)
            (const :tag "Sort by buffer size"        3)
            (const :tag "Sort by time of last use"   4)
            (const :tag "Sort by file name"          5))
    :group 'Buffer-Menu-Plus)


  ;; This is needed because `buff-menu.el' is preloaded and set to nil.
  (setq Buffer-menu-sort-column  (or Buffer-menu-sort-column 1))


  ;; REPLACE ORIGINAL in `buff-menu.el'.
  ;;
  ;; Use `Buffer-menu-buffer+size-computed-width', not `Buffer-menu-buffer+size-width'.
  ;;
  (defun Buffer-menu-buffer+size (name size &optional name-props size-props)
    (if (> (+ (length name) (length size) 1) Buffer-menu-buffer+size-computed-width)
        (setq name  (if (string-match "<[0-9]+>$" name)
                        (concat (substring name 0 (- Buffer-menu-buffer+size-computed-width
                                                     (max (length size) 3)
                                                     (match-end 0)
                                                     (- (match-beginning 0))
                                                     2))
                                "["     ; Cut-off character.
                                (match-string 0 name))
                      (concat (substring name 0 (- Buffer-menu-buffer+size-computed-width
                                                   (max (length size) 3)
                                                   2))
                              "[")))    ; Cut-off character.
      (setq name  (copy-sequence name))) ; Don't put properties on (buffer-name).
    (when name-props (add-text-properties 0 (length name) name-props name))
    (when size-props (add-text-properties 0 (length size) size-props size))
    (concat name (make-string (- Buffer-menu-buffer+size-computed-width
                                 (length name)
                                 (length size))
                              ?\  )     ; ?\  instead of ?\s, so can be byte-compiled in Emacs 20.
            size))
  )


;; Emacs 24.3+ sets the default value to nil.
(unless (and (boundp 'Buffer-menu-buffer+size-width)  Buffer-menu-buffer+size-width)
  (setq Buffer-menu-buffer+size-width  26))




;;; Faces used to fontify buffer.

(defface buffer-menu-headings
    '((((background dark))
       (:foreground "#00005A5AFFFF" :background "#FFFF9B9BFFFF")) ; ~ blue, pink
      (t (:foreground "Orange" :background "DarkGreen")))
  "*Face used for headings in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-headings 'buffer-menu-headings)

(defface buffer-menu-current-buffer
    '((((background dark)) (:foreground "Cyan" :background "#808000002B2B")) ; dark red brown
      (t (:foreground "Red" :background "Aquamarine")))
  "*Face used for current buffer mark in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-current-buffer 'buffer-menu-current-buffer)

(defface buffer-menu-view-mark
    '((((background dark)) (:foreground "Cyan" :background "#808000002B2B")) ; dark red brown
      (t (:foreground "Red" :background "Aquamarine")))
  "*Face used for buffers to view mark (>) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-view-mark 'buffer-menu-view-mark)

(defface buffer-menu-star-buffer
    '((((background dark)) (:foreground "#FFFFFFFF7474")) ; ~ light yellow
      (t (:foreground "DarkBlue")))
  "*Face used for buffers named \"*...*\"in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus :group 'font-lock-highlighting-faces)
(defvar buffer-menu-star-buffer 'buffer-menu-star-buffer)

(defface buffer-menu-directory-buffer
    '((((background dark)) (:foreground "Yellow" :background "#525227271919")) ; ~ dark brown
      (t (:foreground "Blue" :background "LightBlue")))
  "*Face used for directory buffers in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus :group 'font-lock-highlighting-faces)
(defvar buffer-menu-directory-buffer 'buffer-menu-directory-buffer)

(defface buffer-menu-marked-buffer
    '((t (:underline t)))
  "*Face used for buffers marked with `>' in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus :group 'font-lock-highlighting-faces)
(defvar buffer-menu-marked-buffer 'buffer-menu-marked-buffer)

(defface buffer-menu-flagged-buffer
    '((((background dark)) (:foreground "Cyan"))
      (t (:foreground "Red")))
  "*Face used for buffers marked with `D' in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus :group 'font-lock-highlighting-faces)
(defvar buffer-menu-flagged-buffer 'buffer-menu-flagged-buffer)

(defface buffer-menu-delete-mark
    '((((background dark)) (:foreground "#808000002B2B" :background "Cyan"))
      (t (:foreground "Aquamarine" :background "Red")))
  "*Face used for buffers to delete mark (D) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-delete-mark 'buffer-menu-delete-mark)

(defface buffer-menu-save-mark
    '((((background dark)) (:foreground "#00005A5AFFFF" :background "Yellow")) ; ~blue
      (t (:foreground "Orange" :background "Blue")))
  "*Face used for buffers to save mark (S) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-save-mark 'buffer-menu-save-mark)

(defface buffer-menu-modified-mark
    '((((background dark)) (:foreground "#00007373FFFF")) ; ~ blue
      (t (:foreground "DarkOrange")))
  "*Face used for modified buffers mark (*) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-modified-mark 'buffer-menu-modified-mark)

(defface buffer-menu-read-only-mark
    '((((background dark)) (:foreground "Blue"))
      (t (:foreground "Yellow")))
  "*Face used for read-only buffers mark (%) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-read-only-mark 'buffer-menu-read-only-mark)

(defface buffer-menu-buffer-name
    '((((background dark)) (:foreground "Yellow"))
      (t (:foreground "Blue")))
  "*Face used for buffer names in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-buffer-name 'buffer-menu-buffer-name)
;; Redefine standard face `Buffer-menu-buffer' as `buffer-menu-buffer-name'.
(put 'Buffer-menu-buffer 'face-alias 'buffer-menu-buffer-name)

(defface buffer-menu-mode
    '((((background dark)) (:foreground "#FFFF9B9BFFFF")) ; ~ pink
      (t (:foreground "DarkGreen")))
  "*Face used for buffer modes in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-mode 'buffer-menu-mode)

(defface buffer-menu-size
    '((((background dark)) (:foreground "#7474FFFFFFFF")) ; ~ light cyan
      (t (:foreground "DarkRed")))
  "*Face used for buffer sizes in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-size 'buffer-menu-size)

(defface buffer-menu-time
    '((((background dark)) (:foreground "#74749A9AF7F7")) ; ~ light blue
      (t (:foreground "DarkGoldenrod4")))
  "*Face used for buffer time in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-time 'buffer-menu-time)

(defface buffer-menu-file-name
    '((((background dark)) (:foreground "#7474FFFF7474")) ; ~ light green
      (t (:foreground "DarkMagenta")))
  "*Face used for file names in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-file-name 'buffer-menu-file-name)

(if (> emacs-major-version 21)
    (defvar buffer-menu-font-lock-keywords (buffer-menu-font-lock-keywords) ; Emacs 22
      "*Expressions to highlight in Buffer Menu mode.")
  (defvar buffer-menu-font-lock-keywords ; Emacs 20, 21
    (list
     (list "^\\( M.*\\)" 1 'buffer-menu-headings)
     (list "^....\\(.*[^ \t\n]\\)[ \t]+[0-9]+[ \t]+" ; Default buffer name
           1 'buffer-menu-buffer-name)
     (list "^....\\(.*[^ \t\n]\\)[ \t]+[0-9]+[ \t]+[a-zA-Z :0-9]*[ \t]+Dired"
           1 'buffer-menu-directory-buffer t t) ; Directory buffer name
     (list "^....\\(\\*.*[^ \t\n]\\*\\(<[0-9]+>\\)?\\)[ \t]+" ; Star buffer name
           1 'buffer-menu-star-buffer t t)
     (list "^.*[ \t][0-9]+[ \t]+\\([^/\n]+\\)" 1 'buffer-menu-mode t t) ; Mode
     (list "^.*[ \t]\\([0-9]+\\)[ \t]+[^/\n]+" 1 'buffer-menu-size t t) ; Size
     (list "^.*[ \t][0-9]+[ \t]+[^/\n]+[ \t\n]\\(\\([~]\\|\\([a-zA-Z]:\\)\\)*/.*\\)$"
           1 'buffer-menu-file-name t t) ; File name
     (list "^\\([.]\\)" 1 'buffer-menu-current-buffer t t) ; Current buffer mark (.)
     (list "^\\(>\\)" 1 'buffer-menu-view-mark t t) ; To view mark (>)
     (list "^>...\\(.*[^ \t\n]\\)[ \t\]+[0-9]" ; Buffer name when marked (>)
           1 'buffer-menu-marked-buffer 'prepend t)
     (list "^D...\\(.*[^ \t\n]\\)[ \t\]+[0-9]" ; Buffer name when flagged (D)
           1 'buffer-menu-flagged-buffer t t)
     (list "^\\(D\\)" 1 'buffer-menu-delete-mark t t) ; Deletion flag (D)
     (list "^.\\(S\\)" 1 'buffer-menu-save-mark t t) ; Save flag (S)
     (list "^.\\([*]\\)" 1 'buffer-menu-modified-mark t t) ; Buffer-modified-p (*)
     (list "^..\\(%\\)" 1 'buffer-menu-read-only-mark t t) ; Read-only-p (%)
     ) "*Expressions to highlight in Buffer Menu mode."))


;; Undefine some bindings that would try to modify a buffer-menu buffer. Their key sequences
;; will then appear to the user as available for local (Buffer Menu) definition.
(when (fboundp 'undefine-killer-commands) (undefine-killer-commands Buffer-menu-mode-map))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; Protect `Buffer-menu-files-only' with boundp (for Emacs 20).
;;
(defun Buffer-menu-revert-function (ignore1 ignore2)
  (unless (eq buffer-undo-list t) (setq buffer-undo-list  ()))
  ;; We can not use save-excursion here.  The buffer gets erased.
  (let ((opoint            (point))
        (eobp              (eobp))
        (ocol              (current-column))
        (oline             (progn (move-to-column Buffer-menu-buffer-column)
                                  (get-text-property (point) 'buffer)))
        (prop              (point-min))
        ;; Do not make undo records for the reversion.
        (buffer-undo-list  t))
    ;; We can be called by Auto Revert Mode with the "*Buffer Menu*"
    ;; temporarily the current buffer.  Make sure that the
    ;; interactively current buffer is correctly identified with a `.'
    ;; by `list-buffers-noselect'.
    (with-current-buffer (window-buffer)
      (list-buffers-noselect (and (boundp 'Buffer-menu-files-only) Buffer-menu-files-only)))
    (if oline
        (while (setq prop  (next-single-property-change prop 'buffer))
          (when (eq (get-text-property prop 'buffer) oline)
            (goto-char prop)
            (move-to-column ocol)))
      (goto-char (if eobp (point-max) opoint)))))

(defun Buffer-menu-fontify-and-adjust-frame ()
  "Use for `Buffer-menu-mode-hook'.  Fontify, fit and raise frame."
  (with-current-buffer (get-buffer-create "*Buffer List*")
    (when (< emacs-major-version 21) (make-local-variable 'font-lock-defaults))
    (setq font-lock-defaults  '(buffer-menu-font-lock-keywords t))
    (turn-on-font-lock)
    (when (get-buffer-window (current-buffer) 'visible)
      (save-window-excursion
        (select-window (get-buffer-window (current-buffer) 'visible))
        (when (and (fboundp 'fit-frame) (one-window-p t)) (fit-frame))
        (raise-frame)))
    ;; Refresh `font-lock-keywords' from `font-lock-defaults'
    (when (fboundp 'font-lock-refresh-defaults) (font-lock-refresh-defaults))))

;; Fontify buffer, then fit and raise its frame.  (In Emacs 23 they capitalized the name.)
(if (> emacs-major-version 22)
    (add-hook 'Buffer-menu-mode-hook 'Buffer-menu-fontify-and-adjust-frame)
  (add-hook 'buffer-menu-mode-hook 'Buffer-menu-fontify-and-adjust-frame))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; Treat Emacs 20 too.
;;
(defun Buffer-menu-beginning ()
  (goto-char (point-min))
  (unless (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)
    (forward-line)))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;;   1. Different help message.
;;   2. Prefix ARG =< 0 now means list all buffers alphabetically.
;;      (It used to mean the same as ARG > 0.)
;;      Prefix ARG >= 0 means list just file buffers.
;;   3. Use `pop-to-buffer' instead of `switch-to-buffer'.
;;      This means that the buffer menu is shown in a different window.
;;      If you prefer to show it in the current window, then just do this:
;;          (add-to-list 'same-window-buffer-names "*Buffer List*")
;;
;;;###autoload
(defun buffer-menu (&optional arg)
  "Show a menu to let you save, delete or select buffers.
By default (no or null prefix arg), the buffers are listed in order of
last access (visit).  With a non-nil prefix ARG:
  ARG >= 0  means only buffers visiting files are listed.
  ARG <= 0  means the buffers are listed alphabetically.
 (ARG = 0   means only buffers visiting files, listed alphabetically.)

Type `?' in buffer \"*Buffer List*\" for more information.
Type `q' there to quit the buffer menu."
  (interactive "P")
  (let ((num-arg  (prefix-numeric-value arg)))
    (if (and arg (< num-arg 0)) (list-buffers) (list-buffers arg))
    (let ((newpoint  (save-excursion (set-buffer "*Buffer List*") (point))))
      (pop-to-buffer "*Buffer List*")
      (when (and arg (not (> num-arg 0))) ; Sort lines after header.
        (let ((buffer-read-only  nil))
          (goto-char (point-min))
          (unless Buffer-menu-use-header-line (forward-line 2)) ; Header.
          (forward-char Buffer-menu-buffer-column)
          (sort-columns nil (point) (save-excursion (goto-char (point-max))
                                                    (when (bolp) (backward-char 1))
                                                    (point)))))
      (goto-char newpoint)))
  (message "Help: ?;   Menu: mouse-3;   Show: v;   Mark: u,m,s,d;   \
Save/Delete: x;   Misc: g,~,%%,t"))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; 1. Doc string reflects new bindings.
;; 2. mouse-face on whole line, not just buffer name.
;; 3. Compatible with Emacs prior to Emacs 22 also.
;;
(when (< emacs-major-version 23)
  (defun Buffer-menu-mode ()
    "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
In Buffer menu mode, chars do not insert themselves, but are commands.
\\<Buffer-menu-mode-map>
\(\"Current line\" here is the line of the text cursor or the mouse.)


Display Options
---------------
Click `mouse-3' for a context-sensitive menu of buffer operations.

These features are available for Emacs 22 and later:

* You can click a column heading to sort by that column.  Clicking
  again reverses the sort direction.  The current sort column is
  indicated by an underlined or overlined column heading.  Sorting by
  column `CRM' depends on the value of option
  `Buffer-menu-use-frame-buffer-list'.

* You can resize the Buffer and Size columns using `+' and `-'.

* You can toggle showing columns Time, Mode, and File using
  `\\[Buffer-menu-toggle-time-column]',
  `\\[Buffer-menu-toggle-mode-column]', and
  `\\[Buffer-menu-toggle-file-column]'.
  You can toggle the Time format using
  `\\[Buffer-menu-toggle-time-format]]'.
  (These re only for Emacs 21 and later.)

Column `CRM':
 `C' shows `>' if you have marked the buffer to be displayed,
           `D' if you have marked it for deletion, and
           `.' for the buffer from which you came (current).
 `R' shows `%' if the buffer is read-only.
 `M' shows `*' if the buffer is modified, and
           `S' if you have marked it for saving.

The other columns are the Buffer name, its Size in characters, the
last Time the buffer was displayed, its major Mode, and the visited
File name (if any).

Displaying Buffers
------------------
\\[Buffer-menu-mouse-select], \\[Buffer-menu-select], \\[Buffer-menu-this-window] -- \
Select current line's buffer.
\\[Buffer-menu-mark]\t-- Mark current line's buffer `>' to be displayed (via \
`\\[Buffer-menu-select]').
\\[Buffer-menu-select]\t-- Show buffers marked `>'.  Select current line's \
buffer.
\\[Buffer-menu-1-window]\t-- Select current line's buffer (only) in a \
full-frame window.
\\[Buffer-menu-2-window]\t-- Select current line's buffer in one window.
\t   Display previous buffer in a second window.
\\[Buffer-menu-switch-other-window]\t-- Display current line's buffer in \
another window.  No select.
\\[Buffer-menu-view]\t-- select current line's buffer, but in view-mode.
\\[Buffer-menu-view-other-window]\t-- select that buffer in
  another window, in view-mode.
\\[Buffer-menu-toggle-files-only]\t-- toggle whether to display only file buffers.

Marking/Unmarking Buffers to be Saved/Deleted
---------------------------------------------
\\[Buffer-menu-save]\t-- Mark current line's buffer `S' to be saved.    \
Cursor down.
\\[Buffer-menu-delete]\t-- Mark current line's buffer `D' to be deleted.  \
Cursor down.
\\[Buffer-menu-delete-backwards]\t-- Mark current line's buffer `D' to be \
deleted.  Cursor up.
\\[Buffer-menu-unmark]\t-- Unmark current line.  Cursor down. (Prefix arg: \
Cursor up.)
\\[Buffer-menu-backup-unmark]\t-- Cursor up, then unmark line.

Saving/Deleting Buffers
-----------------------
\\[Buffer-menu-execute]\t-- Save / Delete marked buffers (marks `S', `D').
\\[Buffer-menu-delete-flagged]\t-- Delete all buffers marked `D', even if modified.

Miscellaneous
-------------
\\[Buffer-menu-revert]\t-- Update the list of buffers.
\\[Buffer-menu-not-modified]\t-- Clear modified-flag on current line's buffer.
\\[Buffer-menu-toggle-read-only]\t-- Toggle read-only status of current \
line's buffer.
\\[Buffer-menu-visit-tags-table]\t-- `visit-tags-table' using current line's \
buffer.


Bindings in Buffer Menu mode:
----------------------------

\\{Buffer-menu-mode-map}"
    (kill-all-local-variables)
    (use-local-map Buffer-menu-mode-map)
    (setq major-mode  'Buffer-menu-mode
          mode-name   "Buffer Menu")
    (save-excursion
      (goto-char (point-min))
      (when (< emacs-major-version 20)  ; Hardcoded to 4, starting in Emacs 20
        (search-forward "Buffer")
        (backward-word 1)
        (setq Buffer-menu-buffer-column  (current-column)))
      (when (or (not (boundp 'Buffer-menu-use-header-line)) (not Buffer-menu-use-header-line))
        (forward-line 2))               ; First two lines are title, unless use header line.
      (while (not (eobp))
        (put-text-property (point) (line-end-position) 'mouse-face 'highlight)
        (forward-line 1)))
    (set (make-local-variable 'revert-buffer-function) 'Buffer-menu-revert-function)
    (when (> emacs-major-version 21)
      (set (make-local-variable 'buffer-stale-function) #'(lambda (&optional noconfirm) 'fast)))
    (setq truncate-lines    t
          buffer-read-only  t)
    (if (> emacs-major-version 21)
        (if (> emacs-major-version 22)  ; Capitalized in Emacs 23.
            (run-mode-hooks 'Buffer-menu-mode-hook)
          (run-mode-hooks 'buffer-menu-mode-hook))
      (run-hooks 'buffer-menu-mode-hook))))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; 1. Doc string reflects new bindings.
;; 2. mouse-face on whole line, not just buffer name.
;;
;; NOTE: If you byte-compile this file with Emacs 20 and then use the byte-compiled result,
;; `buff-menu+.elc', in Emacs 23, then keys such as `q' and `g' will not be defined.  It's best
;; to byte-compile `buff-menu+.el' with the same Emacs version where you use `buff-menu+.elc'.
;;
(when (> emacs-major-version 22)
  ;; Buffer Menu mode is suitable only for specially formatted data.
  (put 'Buffer-menu-mode 'mode-class 'special)

  (define-derived-mode Buffer-menu-mode special-mode "Buffer Menu"
    "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
In Buffer menu mode, chars do not insert themselves, but are commands.
\\<Buffer-menu-mode-map>
\(\"Current line\" here is the line of the text cursor or the mouse.)


Display Options
---------------
Click `mouse-3' for a context-sensitive menu of buffer operations.

These features are available for Emacs 22 and later:

* You can click a column heading to sort by that column.  Clicking
  again reverses the sort direction.  The current sort column is
  indicated by an underlined or overlined column heading.  Sorting by
  column `CRM' depends on the value of option
  `Buffer-menu-use-frame-buffer-list'.

* You can resize the Buffer and Size columns using `+' and `-'.

* You can toggle showing columns Time, Mode, and File using `\\[Buffer-menu-toggle-time-column]',
  `\\[Buffer-menu-toggle-mode-column]', and `\\[Buffer-menu-toggle-file-column]'.  You can toggle \
the Time format using `\\[Buffer-menu-toggle-time-format]'.

Column `CRM':
 `C' shows `>' if you have marked the buffer to be displayed,
           `D' if you have marked it for deletion, and
           `.' for the buffer from which you came (current).
 `R' shows `%' if the buffer is read-only.
 `M' shows `*' if the buffer is modified, and
           `S' if you have marked it for saving.

The other columns are the Buffer name, its Size in characters, the
last Time the buffer was displayed, its major Mode, and the visited
File name (if any).

Displaying Buffers
------------------
\\[Buffer-menu-mouse-select], \\[Buffer-menu-select], \\[Buffer-menu-this-window], \
\\[Buffer-menu-other-window] -- Select current line's buffer.
\\[Buffer-menu-mark]\t-- Mark current line's buffer `>' to be displayed (via \
`\\[Buffer-menu-select]').
\\[Buffer-menu-select]\t-- Show buffers marked `>'.  Select current line's \
buffer.
\\[Buffer-menu-1-window]\t-- Select current line's buffer (only) in a \
full-frame window.
\\[Buffer-menu-2-window]\t-- Select current line's buffer in one window.
\t   Display previous buffer in a second window.
\\[Buffer-menu-switch-other-window]\t-- Display current line's buffer in \
another window.  No select.
\\[Buffer-menu-view]\t-- select current line's buffer, but in view-mode.
\\[Buffer-menu-view-other-window]\t-- select that buffer in
  another window, in view-mode.
\\[Buffer-menu-toggle-files-only]\t-- toggle whether to display only file & Dired buffers.

Marking/Unmarking Buffers to be Saved/Deleted
---------------------------------------------
\\[Buffer-menu-save]\t-- Mark current line's buffer `S' to be saved.    \
Cursor down.
\\[Buffer-menu-delete]\t-- Mark current line's buffer `D' to be deleted.  \
Cursor down.
\\[Buffer-menu-delete-backwards]\t-- Mark current line's buffer `D' to be \
deleted.  Cursor up.
\\[Buffer-menu-unmark]\t-- Unmark current line.  Cursor down. (Prefix arg: \
Cursor up.)
\\[Buffer-menu-backup-unmark]\t-- Cursor up, then unmark line.

Saving/Deleting Buffers
-----------------------
\\[Buffer-menu-execute]\t-- Save / Delete marked buffers (marks `S', `D').
\\[Buffer-menu-delete-flagged]\t-- Delete all buffers marked `D', even if modified.

Miscellaneous
-------------
\\[revert-buffer]\t-- Update the list of buffers.
\\[Buffer-menu-not-modified]\t-- Clear modified-flag on current line's buffer.
\\[Buffer-menu-toggle-read-only]\t-- Toggle read-only status of current \
line's buffer.
\\[Buffer-menu-visit-tags-table]\t-- `visit-tags-table' using current line's \
buffer.
\\[Buffer-menu-isearch-buffers] -- Do incremental search in the marked buffers.
\\[Buffer-menu-isearch-buffers-regexp] -- Isearch for regexp in the marked buffers.
\\[Buffer-menu-bury] -- bury the buffer listed on this line.


Bindings in Buffer Menu mode:
----------------------------

\\{Buffer-menu-mode-map}"
    (save-excursion
      (let ((inhibit-read-only  t))
        (goto-char (point-min))
        (when (or (not (boundp 'Buffer-menu-use-header-line)) (not Buffer-menu-use-header-line))
          (forward-line 2))             ; First two lines are title, unless use header line.
        (while (not (eobp))
          (put-text-property (point) (line-end-position) 'mouse-face 'highlight)
          (forward-line 1))))
    (set (make-local-variable 'revert-buffer-function) 'Buffer-menu-revert-function)
    (set (make-local-variable 'buffer-stale-function) #'(lambda (&optional noconfirm) 'fast))
    (setq truncate-lines    t
          buffer-read-only  t)))


(define-key Buffer-menu-mode-map "\C-\M-x" 'Buffer-menu-delete-flagged)

;;;###autoload
(defun Buffer-menu-delete-flagged ()    ; Bound to `C-M-x'.
  "Delete all buffers marked `D', even if they have been modified.
If there are any file buffers that have been modified since the last
save, then you must confirm the deletion of all at once.

You can mark a buffer for deletion (`D') using command `\\<Buffer-menu-mode-map>\
\\[Buffer-menu-delete]'."
  (interactive)
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buff-menu-buffer  (current-buffer))
          (buffer-read-only  nil)
          (bufs              ())
          (mod-bufs          ())
          (kill-fn           (if (fboundp 'kill-buffer-and-its-windows)
                                 #'kill-buffer-and-its-windows
                               #'kill-buffer))
          buf)
      (while (re-search-forward "^D" nil t)
        (forward-char -1)
        (setq buf  (Buffer-menu-buffer nil))
        (unless (or (eq buf nil) (eq buf buff-menu-buffer))
          (push buf bufs)
          (when (and (buffer-file-name buf) (buffer-modified-p buf)) (push buf mod-bufs)))
        (forward-line 1))
      (unless bufs (error "No buffers flagged for deletion"))
      (when (and mod-bufs (let ((visible-bell  t)) (ding) t)
                 (let ((last-nonmenu-event  nil)
                       (use-dialog-box      t))
                   (not (yes-or-no-p
                         (concat "Modified buffers.  Delete all anyway? (`"
                                 (mapconcat (lambda (b) (buffer-name b)) mod-bufs "', `")
                                 "')")))))
        (error "OK, no buffers deleted"))
      (dolist (buf bufs)
        (save-excursion
          (set-buffer buf)
          (set-buffer-modified-p nil)
          (funcall kill-fn buf)))
      (Buffer-menu-revert-function nil nil))))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; 1. Deletes frame when kills buffer.
;; 2. Compatible with Emacs prior to Emacs 22 also.
;; 3. Accommodate `frame-bufs.el'.
;;
;;;###autoload
(defun Buffer-menu-execute ()
  "Save or delete buffers marked `S' (\"save\") or `D' (\"delete\").
Buffers can be so marked using commands `\\<Buffer-menu-mode-map>\
\\[Buffer-menu-save]' and `\\[Buffer-menu-delete]', respectively."
  (interactive)  
  (when (and (boundp 'frame-bufs-mode) frame-bufs-mode) (frame-bufs-buffer-menu-execute))
  (save-excursion
    (Buffer-menu-beginning)
    (while (if (> emacs-major-version 21)
               (re-search-forward "^..S" nil t)
             (re-search-forward "^.S" nil t))
      (let ((modp  nil))
        (save-excursion
          (set-buffer (Buffer-menu-buffer t))
          (save-buffer)
          (setq modp  (buffer-modified-p)))
        (let ((buffer-read-only  nil))
          (delete-char -1)
          (insert (if modp ?* ? ))))))
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buff-menu-buffer  (current-buffer))
          (buffer-read-only  nil))
      (while (re-search-forward "^D" nil t)
        (forward-char -1)
        (let ((buf  (Buffer-menu-buffer nil)))
          (or (eq buf nil) (eq buf buff-menu-buffer)
              (save-excursion (if (fboundp 'kill-buffer-and-its-windows)
                                  (kill-buffer-and-its-windows buf)
                                (kill-buffer buf))))
          (if (and buf (buffer-name buf))
              (progn (delete-char 1) (insert ? ))
            (delete-region (point) (progn (forward-line 1) (point)))
            (unless (bobp) (forward-char -1))))))))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; When Buffer Menu is `window-dedicated-p', uses `pop-to-buffer' to display.
;;
(when (or (> emacs-major-version 24)    ; Emacs 24.3+.
          (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
  (defun Buffer-menu-select ()
    "Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with command `\\<Buffer-menu-mode-map>\\[Buffer-menu-mark]'.
Delete and replace any previously existing windows in the selected
frame.  But if the Buffer Menu window is dedicated, do not delete it."
    (interactive)
    (let* ((line-buffer  (Buffer-menu-buffer t))
           (menu-buffer  (current-buffer))
           (others       (delq line-buffer (Buffer-menu-marked-buffers t))))
      (cond ((window-dedicated-p (selected-window)) ; Keep Buffer Menu if dedicated window.
             (pop-to-buffer line-buffer)
             (unless (eq menu-buffer line-buffer) (bury-buffer menu-buffer))
             (dolist (buf others) (pop-to-buffer buf)))
            (t
             (delete-other-windows)
             (switch-to-buffer line-buffer)
             (unless (eq menu-buffer line-buffer) (bury-buffer menu-buffer))
             (let ((height       (/ (1- (frame-height)) (1+ (length others)))))
               (dolist (buf others)
                 (split-window nil height)
                 (other-window 1)
                 (switch-to-buffer buf)))
             (other-window 1))))))      ; Back to beginning.


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; When Buffer Menu is `window-dedicated-p', uses `pop-to-buffer' to display.
;;
(unless (or (> emacs-major-version 24)  ; Emacs 20-23.
            (and (= emacs-major-version 24)  (> emacs-minor-version 2)))
  (defun Buffer-menu-select ()
    "Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with command `\\<Buffer-menu-mode-map>\\[Buffer-menu-mark]'.
Delete and replace any previously existing windows in the selected
frame.  But if the Buffer Menu window is dedicated, do not delete it."
    (interactive)
    (let ((buff    (Buffer-menu-buffer t))
          (menu    (current-buffer))
          (others  ())
          tem)
      (Buffer-menu-beginning)
      (while (re-search-forward "^>" nil t)
        (setq tem  (Buffer-menu-buffer t))
        (let ((buffer-read-only  nil)) (delete-char -1) (insert ?\ ))
        (or (eq tem buff) (memq tem others) (setq others  (cons tem others))))
      (setq others  (nreverse others))
      (cond ((window-dedicated-p (selected-window)) ; Can't split dedicated win.
             (pop-to-buffer buff)
             (unless (eq menu buff) (bury-buffer menu))
             (while others
               (pop-to-buffer (car others))
               (pop others)))
            (t
             (setq tem  (/ (1- (frame-height)) (1+ (length others))))
             (delete-other-windows)
             (switch-to-buffer buff)
             (unless (eq menu buff) (bury-buffer menu))
             (if (equal (length others) 0)
                 (progn
;;;              ;; Restore previous window configuration before displaying
;;;              ;; selected buffers.
;;;              (if Buffer-menu-window-config
;;;                  (progn (set-window-configuration
;;;                            Buffer-menu-window-config)
;;;                         (setq Buffer-menu-window-config  nil)))
                   (switch-to-buffer buff))
               (while others
                 (split-window nil tem)
                 (other-window 1)
                 (switch-to-buffer (car others))
                 (setq others  (cdr others)))
               (other-window 1)))))))   ; Back to the beginning.


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; Allow negative COLUMN.  Allow COLUMN = 1 or -1.
;; When COLUMN = `Buffer-menu-sort-column', then flip `Buffer-menu-sort-column'.
;; Message at end.
;;
(when (> emacs-major-version 21)
  (defun Buffer-menu-sort (column)
    "Sort the buffer menu by COLUMN.
Consecutive executions of the same COLUMN reverse the sort order."
    (interactive "P")
    (when column
      (setq column  (prefix-numeric-value column))
      (when (= column 0) (setq column  1))
      (when (> column 6) (setq column  6))
      (when (< column -6) (setq column  -6)))
    (if (equal Buffer-menu-sort-column column)
        (setq Buffer-menu-sort-column  (- column))
      (setq Buffer-menu-sort-column  column))
    (let (buffer-read-only lll buf m1 m2)
      (save-excursion
        (Buffer-menu-beginning)
        (while (not (eobp))
          (when (buffer-live-p
                 (setq buf  (get-text-property (+ (point) Buffer-menu-buffer-column) 'buffer)))
            (setq m1  (char-after)
                  m1  (and (memq m1 '(?> ?D)) m1)
                  m2  (char-after (+ (point) 2))
                  m2  (and (eq m2 ?S) m2))
            (when (or m1 m2) (push (list buf m1 m2) lll)))
          (forward-line)))
      (Buffer-menu-revert-function nil nil)
      (setq buffer-read-only  t)
      (save-excursion
        (Buffer-menu-beginning)
        (while (not (eobp))
          (when (setq buf  (assq (get-text-property (+ (point) Buffer-menu-buffer-column) 'buffer)
                                 lll))
            (setq m1  (cadr buf)
                  m2  (cadr (cdr buf)))
            (when m1 (delete-char 1) (insert m1) (backward-char 1))
            (when m2 (forward-char 2) (delete-char 1) (insert m2)))
          (forward-line))))
    (message "Buffers are now sorted %s%s."
             (case (abs column)
               (1 "by time of last visit - see `Buffer-menu-use-frame-buffer-list'")
               (2 "by buffer name")
               (3 "by size")
               (4 "by time of last display")
               (5 "by major-mode name")
               (otherwise "by associated file (including path)"))
             (if (natnump Buffer-menu-sort-column) ", ascending" ", descending"))))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; If same column as last sort, then flip direction of sort.
;; CRM is indicated by COLUMN = 1, not by nil COLUMN.
;; Apply different face to sort column heading, depending on direction.
;;
(when (> emacs-major-version 21)
  (defun Buffer-menu-make-sort-button (name button-column)
    (let ((the-sort-column-p  nil))
      (when (equal button-column (abs Buffer-menu-sort-column))
        (setq the-sort-column-p  t
              button-column      (- button-column)))
      (propertize name 'column button-column
                  'help-echo (case (abs button-column)
                               (1 (if Buffer-menu-use-header-line
                                      "mouse-1, mouse-2: sort by time of last visit - \
see `Buffer-menu-use-frame-buffer-list'"
                                    "mouse-2, RET: sort by time of last visit - \
see `Buffer-menu-use-frame-buffer-list'"))
                               (2 (if Buffer-menu-use-header-line
                                      "mouse-1, mouse-2: sort by buffer name"
                                    "mouse-2, RET: sort by buffer name"))
                               (4 "mouse-1, mouse-2: sort by time of last display/access")
                               (t (if Buffer-menu-use-header-line
                                      (concat "mouse-1, mouse-2: sort by " (downcase name))
                                    (concat "mouse-2, RET: sort by " (downcase name)))))
                  'mouse-face 'highlight
                  (when the-sort-column-p 'face) (when the-sort-column-p
                                                   (if (natnump Buffer-menu-sort-column)
                                                       '(:underline t)
                                                     '(:overline t)))
                  'keymap Buffer-menu-sort-button-map))))


;; REPLACE ORIGINAL in `buff-menu.el'.
;;
;; 1. Compute longest buffer name + size combination, and use when indenting file name.
;; 2. Add sort buttons for CRM and Time also.
;; 3. The test for column 1 (CRM) to determine whether to sort is =1, not null.
;; 4. Sort direction depends on sign of `Buffer-menu-sort-column'.
;; 5. Go to beginning of buffer if `desired-point' is not defined.
;; 6. Accommodate `frame-bufs.el'.
;; 7. Treat Dired buffers like file buffers, e.g., wrt FILES-ONLY.
;;
(when (> emacs-major-version 21)
  (defun list-buffers-noselect (&optional files-only buffer-list)
    "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

If BUFFER-LIST is non-nil, it should be a list of buffers;
it means list those buffers and no others.

For more information, see the function `buffer-menu'."
    ;; Compute longest buffer name + size combination.
    ;; $$$$$$ Could be costly if lots of buffers - maybe have an option to be able to not do it?
    (let ((len  0)
          buf+size)
      ;; Emacs 24.3+ sets the default value of `Buffer-menu-buffer+size-width' to nil.
      (unless Buffer-menu-buffer+size-width  (setq Buffer-menu-buffer+size-width  26))
      (setq Buffer-menu-buffer+size-computed-width  Buffer-menu-buffer+size-width)
      (dolist (buffer (buffer-list))
        (setq buf+size  (concat (buffer-name buffer) (number-to-string (buffer-size buffer))))
        (when (and (not (string= (substring buf+size 0 1) " ")) ; Don't count internal buffers.
                   (> (length buf+size) len))
          (setq len  (length buf+size))))
      (when (< (+ len 1) Buffer-menu-buffer+size-width)
        (setq Buffer-menu-buffer+size-computed-width  (+ len 1))))
    (let* ((old-buffer       (current-buffer))
           (standard-output  standard-output)
           (mode-end         (if Buffer-menu-mode-flag
                                 (make-string (- Buffer-menu-mode-width 4) ?\ )
                               ""))
           (frame-bufs-p     (and (boundp 'frame-bufs-mode) frame-bufs-mode))
           (header
            (concat (Buffer-menu-make-sort-button
                     (concat "CRM" (if frame-bufs-p (if frame-bufs-full-list "F" " ") "")) 1)
                    " "
                    (Buffer-menu-buffer+size (Buffer-menu-make-sort-button "Buffer" 2)
                                             (Buffer-menu-make-sort-button "Size" 3))
                    "  "
                    (and Buffer-menu-time-flag
                         (if (eq 'short Buffer-menu-time-format)
                             (Buffer-menu-make-sort-button "Time   " 4)
                           (Buffer-menu-make-sort-button "Time          " 4)))
                    (and Buffer-menu-time-flag "   ")
                    (and Buffer-menu-mode-flag
                         (Buffer-menu-make-sort-button (concat "Mode" mode-end) 5))
                    (if Buffer-menu-mode-flag
                        (if Buffer-menu-time-flag " " "  ")
                      (and (not Buffer-menu-time-flag) " "))
                    (and Buffer-menu-file-flag (Buffer-menu-make-sort-button
                                                "File           " 6))
                    "\n"))
           list desired-point name buffer-time mode file)
      (when (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)
        (let ((pos  0))
          ;; Turn whitespace chars in the header into stretch specs so
          ;; they work regardless of the header-line face.
          (while (string-match "[ \t\n]+" header pos)
            (setq pos  (match-end 0))
            (put-text-property (match-beginning 0) pos 'display
                               ;; Assume fixed-size chars in the buffer.
                               (list 'space :align-to pos)
                               header)
            (put-text-property (match-beginning 0) (1- pos) 'mouse-face 'highlight header)))
        ;; REMOVED:
        ;; Try to better align the one-char headers.
        ;; (put-text-property 0 (if frame-bufs-p 4 3) 'face 'fixed-pitch header)
        ;; Add a "dummy" leading space to align the beginning of the header
        ;; line with the beginning of the text (rather than with the left
        ;; scrollbar or the left fringe). --Stef
        (setq header  (concat (propertize " " 'display '(space :align-to 0)) header)))
      (with-current-buffer (get-buffer-create "*Buffer List*")
        (setq buffer-read-only  nil)
        (erase-buffer)
        (setq standard-output  (current-buffer))
        (unless (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)
          ;; Use U+2014 (EM DASH) to underline if possible, else use U+002D (HYPHEN-MINUS).
          ;; Must eval this at compile time.
          ;; Tried a runtime check of (char-displayable-p ?\u2014), but if this is compiled in
          ;; an Emacs version before 22 then get an error that `char-displayable-p' is called
          ;; with two args (since ?\u is interpreted as ?u).
          (let ((underline  (eval-when-compile (if (> emacs-major-version 21) ?\u2014 ?-))))
            (insert header (apply 'string
                                  (mapcar (lambda (ch) (if (memq ch '(?\n ?\  )) ch underline))
                                          header)))))
;;;;           (insert header (propertize "---" 'face 'fixed-pitch) " ")
;;;;           (insert (Buffer-menu-buffer+size "------" "----"))
;;;;           (insert "  ----" mode-end "----\n")
;;;;           (put-text-property 1 (point) 'intangible t))
        ;; Collect info for every buffer we're interested in.
        (dolist (buffer  (or buffer-list
                             (and frame-bufs-p  (frame-bufs-buffer-list (selected-frame)
                                                                        frame-bufs-full-list))
                             (buffer-list (and Buffer-menu-use-frame-buffer-list
                                               (selected-frame)))))
          (with-current-buffer buffer
            (let ((name  (buffer-name))
                  (file  (or buffer-file-name
                             (and (eq major-mode 'dired-mode) default-directory))))
              (unless (and (not buffer-list)
                           (or
                            ;; Don't mention internal buffers.
                            (and (string= (substring name 0 1) " ") (null file))
                            ;; Maybe don't mention buffers without files.
                            (and files-only (not file))
                            (string= name "*Buffer List*")))
                ;; Otherwise output info.
                (let (;; Need to record two values for time: numerical time value, for
                      ;; sorting, and string time value, for display.
                      (buffer-time (and Buffer-menu-time-flag
                                        (cons (or (float-time buffer-display-time) 0)
                                              (if buffer-display-time
                                                  (format-time-string
                                                   (if (eq 'short Buffer-menu-time-format)
                                                       "%02H:%02M:%02S"
                                                     "%_3a %_2l:%02M:%02S %_2p")
                                                   buffer-display-time)
                                                (if (eq 'short Buffer-menu-time-format)
                                                    "        "
                                                  "               ")))))
                      (mode (concat (format-mode-line mode-name nil nil buffer)
                                    (and mode-line-process
                                         (format-mode-line
                                          mode-line-process nil nil buffer))))
                      (bits (concat (if (eq buffer old-buffer) "." " ")
                                    ;; Make output buffer appear read-only, even though it is not
                                    ;; (yet).  Actually make it read-only later.
                                    (if (or (eq buffer standard-output) buffer-read-only) 
                                        "%" 
                                      " ")
                                    (if (buffer-modified-p) "*" " ") ; Modified mark.
                                    (if frame-bufs-p ; Frame-bufs info.
                                        (frame-bufs-bit-info buffer)
                                      "")
                                    " "))) ; Space separator.
                  (unless file
                    ;; No visited file.  Check local value of `list-buffers-directory' and,
                    ;; for Info buffers, `Info-current-file'.
                    (cond ((and (boundp 'list-buffers-directory) list-buffers-directory)
                           (setq file  list-buffers-directory))
                          ((eq major-mode 'Info-mode)
                           (setq file  Info-current-file)
                           (cond ((equal file "dir")
                                  (setq file  "*Info Directory*"))
                                 ((eq file 'apropos)
                                  (setq file  "*Info Apropos*"))
                                 ((eq file 'history)
                                  (setq file  "*Info History*"))
                                 ((eq file 'toc)
                                  (setq file  "*Info TOC*"))
                                 ((not (stringp file)) ; avoid errors
                                  (setq file  nil))
                                 (t
                                  (setq file  (concat "(" (file-name-nondirectory file) ")"
                                                      Info-current-node)))))))
                  (push (list buffer bits name (buffer-size) buffer-time mode file) list))))))
        ;; Preserve original list order (by reversing).
        ;; Flip it if Buffer-menu-sort-column = -1.
        (unless (eq -1 Buffer-menu-sort-column) (setq list  (nreverse list)))
        ;; Place the buffers's info in the output buffer, sorted if necessary.
        (dolist (buffer (if (eq 1 (abs Buffer-menu-sort-column))
                            list
                          (let* ((descending-p             (natnump Buffer-menu-sort-column))
                                 (Buffer-menu-sort-column  (abs Buffer-menu-sort-column)))
                            (sort list
                                  (cond ((eq Buffer-menu-sort-column 3) ; Size
                                         (if descending-p
                                             (lambda (a b) (< (nth 3 a) (nth 3 b)))
                                           (lambda (a b) (< (nth 3 b) (nth 3 a)))))
                                        ((eq Buffer-menu-sort-column 4) ; Time (value)
                                         (if descending-p
                                             (lambda (a b) (< (car (nth 4 a)) (car (nth 4 b))))
                                           (lambda (a b) (< (car (nth 4 b)) (car (nth 4 a))))))
                                        (t
                                         (if descending-p
                                             (lambda (a b)
                                               (string< (nth Buffer-menu-sort-column a)
                                                        (nth Buffer-menu-sort-column b)))
                                           (lambda (a b)
                                             (string< (nth Buffer-menu-sort-column b)
                                                      (nth Buffer-menu-sort-column a))))))))))
          (when (eq (car buffer) old-buffer) (setq desired-point  (point)))
          (insert (cadr buffer)
                  ;; Put the buffer name into a text property
                  ;; so we don't have to extract it from the text.
                  ;; This way we avoid problems with unusual buffer names.
                  (Buffer-menu-buffer+size (nth 2 buffer)
                                           (int-to-string (nth 3 buffer))
                                           `(buffer-name ,(nth 2 buffer) buffer ,(car buffer)
                                             font-lock-face
                                             ,(if (facep 'Buffer-menu-buffer-face)
                                                  'Buffer-menu-buffer-face ; < Emacs 22
                                                  'Buffer-menu-buffer) ; Emacs 22
                                             mouse-face highlight
                                             help-echo "mouse-2: select this buffer")))
          (when Buffer-menu-time-flag (insert "  " (cdr (nth 4 buffer)))) ; Time
          (when Buffer-menu-mode-flag
            (insert "  "
                    (if (> (length (nth 5 buffer)) Buffer-menu-mode-width) ; Mode
                        (substring (nth 5 buffer) 0 Buffer-menu-mode-width)
                      (nth 5 buffer))))
          (when (and Buffer-menu-file-flag (nth 6 buffer)) ; File
            (indent-to (+ Buffer-menu-buffer-column Buffer-menu-buffer+size-computed-width
                          (if Buffer-menu-mode-flag (1+ Buffer-menu-mode-width) 0)
                          (if Buffer-menu-time-flag
                              (if (eq 'short Buffer-menu-time-format) 9 16)
                            0)
                          3)
                       1)
            (princ (abbreviate-file-name (nth 6 buffer))))
          (princ "\n"))
        (Buffer-menu-mode)
        (when (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)
          (setq header-line-format  header))
        ;; DESIRED-POINT doesn't have to be set; it is not set when the
        ;; current buffer is not displayed for some reason.
        (goto-char (or desired-point (point-min)))
        (setq Buffer-menu-files-only  files-only)
        (set-buffer-modified-p nil)
        (current-buffer)))))

(define-key Buffer-menu-mode-map [down-mouse-3] 'Buffer-menu-mouse-3-menu)
(define-key Buffer-menu-mode-map [mouse-3] 'ignore)

;; Another way, but it shows the menu even if not on a buffer line,
;; and it doesn't show it if on the line but not on the buffer name itself.
;;(defvar Buffer-menu-mouse-3-map (make-sparse-keymap "Buffers"))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-execute]
;;  '("Execute: Save/Delete Marked Buffers" . Buffer-menu-mouse-execute))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-modified]
;;  '("Mark as Modified/Unmodified (*)" . Buffer-menu-mouse-modified))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-delete]
;;  '("Mark to Delete Buffer (D)" . Buffer-menu-mouse-delete))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-save]
;;  '("Mark to Save Buffer (S)" . Buffer-menu-mouse-save))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-unmark]
;;  '("Unmark Buffer" . Buffer-menu-mouse-unmark))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-select]
;;  '("Select Buffer" . Buffer-menu-mouse-select))

(defvar Buffer-menu-overlay nil
  "Overlay to highlight line of buffer during popup of `mouse-3' menu.")

;;;###autoload
(defun Buffer-menu-mouse-3-menu (event)
  "Pop up menu for Mouse-3 for buffer listed in buffer menu."
  (interactive "e")
  (let* ((mouse-pos    (event-start event))
         bol eol temp
         (buffer-name  (save-excursion
                         (set-buffer (window-buffer (posn-window mouse-pos)))
                         (save-excursion
                           (goto-char (posn-point mouse-pos))
                           (setq bol  (line-beginning-position)
                                 eol  (line-end-position))
                           (if Buffer-menu-overlay ; Don't recreate if exists.
                               (move-overlay Buffer-menu-overlay bol eol (current-buffer))
                             (setq Buffer-menu-overlay  (make-overlay bol eol))
                             (overlay-put Buffer-menu-overlay 'face 'region))
                           (setq temp  (and (not (eobp)) (Buffer-menu-buffer nil)))
                           ;; Nil if mouse is not on a buffer name.
                           (and temp (buffer-name temp)))))) ; temp no longer used.
    (sit-for 0)
    (let ((selection
           (x-popup-menu
            event
            (list
             "Menu"
             (if buffer-name
                 (list
                  buffer-name
                  '("Select Buffer" . Buffer-menu-mouse-select)
                  '("Unmark Buffer" . Buffer-menu-mouse-unmark)
                  '("Mark to Save Buffer (S)" . Buffer-menu-mouse-save)
                  '("Mark to Delete Buffer (D)" . Buffer-menu-mouse-delete)
                  '("Mark as Modified/Unmodified (*)" . Buffer-menu-mouse-modified)
                  '("--")               ; Separator: next not buffer-specific.
                  '("Execute: Save/Delete Marked Buffers" . Buffer-menu-mouse-execute)
                  '("Delete All `D', Even If Modified" . Buffer-menu-delete-flagged))
               (list ""
                     '("Execute: Save/Delete Marked Buffers" . Buffer-menu-mouse-execute)
                     '("Delete All `D', Even If Modified" . Buffer-menu-delete-flagged)))))))
      (when Buffer-menu-overlay (delete-overlay Buffer-menu-overlay))
      (and selection (call-interactively selection)))))

;; Don't need this if use dedicated frame for buffer menu.
;;;###autoload
(defun Buffer-menu-mouse-other-window (event)
  "Select, in another window, the buffer on whose line you click."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer  (Buffer-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun Buffer-menu-mouse-unmark (event)
  "Cancel all requested operations on buffer."
  (interactive "e")
  (let ((frame-bufs-p  (and (boundp 'frame-bufs) frame-bufs-mode))
        buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer  (Buffer-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (beginning-of-line)
    (if (looking-at " [-M]")            ;header lines
        (ding)
      (let* ((mod               (buffer-modified-p buffer))
             (readonly          (save-excursion (set-buffer buffer) buffer-read-only))
             (buffer-read-only  nil)
             (local-info        (if frame-bufs-p
                                    (frame-bufs-bit-info buffer)
                                  "")))
        (delete-char (if frame-bufs-p 4 3))
        (insert (concat (if readonly (if mod " *%" "  %") (if mod " * " "   ")) local-info))))
    (beginning-of-line)))

;;;###autoload
(defun Buffer-menu-mouse-save (event)
  "Mark buffer to be saved.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (forward-char 1)
  (if (looking-at " [-M]")              ;header lines
      (ding)
    (let ((buffer-read-only  nil)) (delete-char 1) (insert ?S)))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-delete (event)
  "Mark buffer to be deleted.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (if (looking-at " [-M]")              ;header lines
      (ding)
    (let ((buffer-read-only  nil))  (delete-char 1) (insert ?D)))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-modified (event)
  "Mark buffer as unmodified (no changes to save) if modified, and vice versa."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (forward-char 1)
  (let ((buffer-read-only  nil)
        modified-p)
    (save-excursion
      (set-buffer (Buffer-menu-buffer t))
      (set-buffer-modified-p (not (buffer-modified-p))))
    (cond ((= ?\* (char-after (point)))
           (delete-char 1)
           (insert ?\ ))
          (t
           (delete-char 1)
           (insert ?\*))))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-execute (event)
  "Save and/or delete buffers marked `S' or `D', respectively.
Buffers can be marked via commands `\\<Buffer-menu-mode-map>\
\\[Buffer-menu-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'
\(or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-save]' and \
`\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-delete]')."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (when (and (boundp 'frame-bufs-mode) frame-bufs-mode) (frame-bufs-buffer-menu-execute))
  (save-excursion
    (Buffer-menu-beginning)
    (while (if (> emacs-major-version 21)
               (re-search-forward "^..S" nil t)
             (re-search-forward "^.S" nil t))
      (let ((modp  nil))
        (save-excursion
          (set-buffer (Buffer-menu-buffer t))
          (save-buffer)
          (setq modp  (buffer-modified-p)))
        (let ((buffer-read-only  nil))  (delete-char -1) (insert (if modp ?* ? ))))))
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buff-menu-buffer  (current-buffer))
          (buffer-read-only  nil))
      (while (re-search-forward "^D" nil t)
        (forward-char -1)
        (let ((buf  (Buffer-menu-buffer nil)))
          (or (eq buf nil) (eq buf buff-menu-buffer)
              (save-excursion (if (fboundp 'kill-buffer-and-its-windows)
                                  (kill-buffer-and-its-windows buf)
                                (kill-buffer buf))))
          (if (and buf (buffer-name buf))
              (progn (delete-char 1) (insert ? ))
            (delete-region (point) (progn (forward-line 1) (point)))
            (unless (bobp) (forward-char -1))))))))

(when (> emacs-major-version 21)
  (defun buffer-menu-nb-marked-in-mode-name ()
    "Add number of marked and flagged lines to mode name in the mode line.
\(Flagged means flagged for deletion.)
If the current line is marked/flagged and there are others
marked/flagged after it then show `N/M', where N is the number
marked/flagged through the current line and M is the total number
marked/flagged."
    (setq mode-name
          `(,mode-name
            (:eval (let* ((marked-regexp   "^>")
                          (nb-marked       (count-matches marked-regexp
                                                          (point-min) (point-max))))
                     (if (not (> nb-marked 0))
                         ""
                       (propertize
                        (format " %s%d>"
                                (save-excursion
                                  (forward-line 0)
                                  (if (looking-at (concat marked-regexp ".*"))
                                      (format "%d/" (1+ (count-matches marked-regexp
                                                                       (point-min) (point))))
                                    ""))
                                nb-marked)
                        'face 'buffer-menu-mode-line-marked))))
            (:eval (let* ((flagged-regexp  "^D")
                          (nb-flagged      (count-matches flagged-regexp
                                                          (point-min) (point-max))))
                     (if (not (> nb-flagged 0))
                         ""
                       (propertize
                        (format " %s%dD"
                                (save-excursion
                                  (forward-line 0)
                                  (if (looking-at (concat flagged-regexp ".*"))
                                      (format "%d/" (1+ (count-matches flagged-regexp
                                                                       (point-min) (point))))
                                    ""))
                                nb-flagged)
                        'face 'buffer-menu-mode-line-flagged)))))))

  (defface buffer-menu-mode-line-marked
      ;; '((t (:inherit buffer-menu-view-mark)))
      '((t (:foreground "Blue")))
    "*Face for marked number in mode line `mode-name' for Dired buffers."
    :group 'Buffer-Menu-Plus :group 'font-lock-highlighting-faces)

  (defface buffer-menu-mode-line-flagged
      ;;  '((t (:inherit buffer-menu-delete-mark)))
      '((t (:foreground "Red")))
    "*Face for flagged number in mode line `mode-name' for Dired buffers."
    :group 'Buffer-Menu-Plus :group 'font-lock-highlighting-faces)

  (if (> emacs-major-version 22)
      (add-hook 'Buffer-menu-mode-hook 'buffer-menu-nb-marked-in-mode-name)
    (add-hook 'buffer-menu-mode-hook 'buffer-menu-nb-marked-in-mode-name)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'buff-menu+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buff-menu+.el ends here

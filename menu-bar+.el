;;; menu-bar+.el --- Extensions to `menu-bar.el'.
;;
;; Filename: menu-bar+.el
;; Description: Extensions to `menu-bar.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2011, Drew Adams, all rights reserved.
;; Created: Thu Aug 17 10:05:46 1995
;; Version: 21.1
;; Last-Updated: Tue Jan  4 11:24:45 2011 (-0800)
;;           By: dradams
;;     Update #: 3368
;; URL: http://www.emacswiki.org/cgi-bin/wiki/menu-bar+.el
;; Keywords: internal, local, convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `fit-frame', `frame-fns',
;;   `help+20', `info', `info+', `menu-bar', `misc-cmds', `misc-fns',
;;   `second-sel', `strings', `thingatpt', `thingatpt+', `unaccent',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget'.
;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `menu-bar.el'.  Redefines the default menu bar.
;;
;;  Main differences:
;;
;;    1. Menus "Search", "Frames" and "Do Re Mi" were added.
;;    2. Menus "File", "Edit", & "Help" were changed.
;;    3. Menu order was changed.
;;    3. Buffer-local menus are separated from global menus via "||".
;;
;;  Functions defined here:
;;
;;    `describe-menubar', `fill-paragraph-ala-mode',
;;    `menu-bar-create-directory', `menu-bar-next-tag-other-window',
;;    `menu-bar-word-search-backward', `menu-bar-word-search-forward',
;;    `nonincremental-repeat-search-backward',
;;    `nonincremental-repeat-search-forward'
;;    `nonincremental-repeat-word-search-backward',
;;    `nonincremental-repeat-word-search-forward'.
;;
;;  Macros defined here:
;;
;;    `menu-bar-make-toggle-any-version'.
;;
;;  Menu variables defined here:
;;
;;    `menu-bar-apropos-menu', `menu-bar-describe-menu',
;;    `menu-bar-divider-menu', `menu-bar-edit-fill-menu',
;;    `menu-bar-edit-region-menu', `menu-bar-edit-sort-menu',
;;    `menu-bar-emacs-lisp-manual-menu', `menu-bar-emacs-manual-menu',
;;    `menu-bar-frames-menu', `menu-bar-print-menu',
;;    `menu-bar-search-replace-menu', `menu-bar-search-tags-menu',
;;    `menu-bar-whereami-menu'.
;;
;;
;;  ***** NOTE: The following functions defined in `menu-bar.el' have
;;              been REDEFINED HERE:
;;
;;  `kill-this-buffer' -        Deletes buffer's windows as well, if
;;                              `sub-kill-buffer-and-its-windows'.
;;  `menu-bar-options-save' - Added options are saved (>= Emacs 21).
;;
;;  `menu-bar-select-buffer' - Uses -other-frame.
;;
;;  `menu-bar-select-frame' - Use Emacs 22 version for Emacs 20.
;;
;;
;;  ***** NOTE: The following variables defined in `menu-bar.el' have
;;              been REDEFINED HERE:
;;
;;  `menu-bar-edit-menu', `menu-bar-file(s)-menu',
;;  `menu-bar-manuals-menu', `menu-bar-options-menu',
;;  `menu-bar-search-menu'.
;;
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `menu-bar.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "menu-bar" '(require 'menu-bar+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2011/01/04 dadams
;;     defsubst -> defun.
;;     Removed autoload cookies from defvar.  Added for commands.
;; 2010/06/04 dadams
;;     Frames menu: Handle fit-frame.el and frame-cmds.el separately.  Added Toggle Max stuff.
;; 2010/05/28 dadams
;;     Added items new-file and new-directory.  Added function menu-bar-create-directory.
;; 2010/05/25 dadams
;;     Added to Frames menu: max-frame, maximize-frame-(horizontally|vertically).
;; 2010/01/12 dadams
;;     describe-menubar: save-excursion + set-buffer -> with-current-buffer.
;; 2009/11/07 dadams
;;     Applied doremi cmd renamings (added +).
;;     Added to Do Re Mi menu: *-all-frames-fg+, *(-face)-(bg|fg)-color-name+, *-all-faces-bg+.
;; 2009/06/25 dadams
;;     Use renaming: yank-secondary-or-swap-w-region to secondary-dwim.
;;     Added: secondary-swap-region (Swap Region and Secondary) to Edit menu.
;;     Renamed secondary selection items in Edit menu.
;; 2009/06/18 dadams
;;     Added doremi-buffer-font-size to Do Re Mi menu.
;; 2009/05/17 dadams
;;     Updated to reflect thumb-frm.el name changes.
;; 2008/05/23 dadams
;;     Soft-require second-sel.el.
;; 2008/05/06 dadams
;;     Renamed yank-secondary-or-convert-primary to yank-secondary-or-swap-w-region.
;;     Added: secondary-to-primary.
;; 2008/05/04 dadams
;;     Added primary-to-secondary to Edit menu.
;;     Changed :enable condition for yank-secondary - use x-get-selection.
;; 2008/02/01 dadams
;;     Update Search menu.
;;       Added: menu-bar-last-search-type, nonincremental-repeat-*, menu-bar-word-search-*.
;;       Define nonincremental-repeat-word-search* only for Emacs 22.
;;       Added menu-bar-i-search-menu submenu for Emacs 22.
;; 2007/12/14 dadams
;;     Require help+20.el for Emacs 20.  Require (new) help+.el for Emacs 22.
;;     Reorganize Help submenus: Apropos, Learn More > (Emacs|Emacs Lisp).
;; 2007/12/11 dadams
;;     menu-bar-final-items: Treat Emacs 21 like 20 - the item is File but it's called files.
;; 2007/12/09 dadams
;;     Added to Help > Describe: describe-(option(-of-type)|command).
;; 2007/12/02 dadams
;;     Added to Help > Describe (and reordered):
;;       describe-(face|keymap|file|input-method|coding-system(-briefly)|current-display-table), 
;;     Soft require help-fns+.el.
;; 2007/11/01 dadams
;;     Do Re Mi menu:
;;       Added Window Size.
;;       Move Frame (Vertically|Horizontally)->Move Frame.  Frame (Height|Width)->Frame Size.
;; 2007/10/26 dadams
;;     Added doremi-undo-* to Do Re Mi menu.
;; 2007/08/12 dadams
;;     Removed soft require of highlight.el. Moved code to highlight.el.
;; 2007/08/11 dadams
;;     Removed soft require of replace+.el. Moved code to replace+.el.
;; 2007/06/08 dadams
;;     Renamed: *-tag-other-frame to *-tag-other-window.
;; 2007/06/02 dadams
;;     Renamed: highlight-region(-regexp-region) to hlt-highlight-region(-regexp-region),
;;              unhighlight-region to hlt-unhighlight-region.
;; 2007/05/22 dadams
;;     Removed menu-item-any-version.  Use menu-item everywhere (OK for Emacs 20 also now).
;; 2006/10/13 dadams
;;     menu-bar-final-items: Put pop-up-tool-bar at end, if defined.
;; 2006/05/19 dadams
;;     menu-bar-options-save: Updated to latest Emacs 22 definition. Added Emacs 21 definition.
;; 2005/11/08 dadams
;;     Added to menu-bar-edit-menu: undo, cut, copy, paste, select paste, clear,
;;       separator-edit-delete-lines.
;;     Added to menu-bar-search-tags-menu: set-tags-name, apropos-tags, separator-tags-misc,
;;       separator-tags-regexp, next-tag-other-frame, 
;;     Added: yank-menu, menu-bar-next-tag-other-frame, menu-bar-select-frame.
;; 2005/10/23 dadams
;;     Removed references to menu-bar-files-menu - test version, not boundp menu-bar-file-menu.
;;     Still keep "files" in menu-bar-final-items for version < 21; else wrong order.
;; 2005/08/02 dadams
;;     Added to Do Re Mi menu: doremi-all-faces-fg, doremi-all-frames-bg.
;; 2005/06/14 dadams
;;     For Emacs 22: menu-bar-files-menu -> menu-bar-file-menu.
;;     No longer redefine File(s) menu from scratch, removing default bindings.
;;     Open File and Open Directory: Don't use other frame, except in Emacs < 22.
;;     Don't bother to rename File menu items (suggested renamings to emacs-devel@gnu.org.
;;     menu-bar-edit-menu: defvar -> setq.
;;     menu-bar-final-items: Use default order.
;; 2005/05/28 dadams
;;     Protected menu-bar-last-search-type with boundp (thanks to Tim Johnson for the report).
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/05/10 dadams
;;     Added: set[-all]-frame-alist-parameter[s]-from-frame.
;; 2005/01/25 dadams
;;     Added: menu-bar-make-toggle-any-version, menu-bar-options-save.
;;     Added to Options menu: doremi-push-frame-config-for-cmds-flag, inhibit-fit-frame-flag,
;;           autofit-frames-flag, thumbify-instead-of-iconify-flag, replace-w-completion-flag.
;; 2005/01/20 dadams
;;     Removed: exit-with-confirmation.
;; 2005/01/09 dadams
;;     Renamed: doremi-bg-rgb to doremi-bg, doremi-face-bg-rgb to doremi-face-bg,
;;              doremi-face-fg-rgb to doremi-face-fg.
;; 2005/01/02 dadams
;;     Added doremi-marks, doremi-global-marks.
;; 2004/12/28 dadams
;;     Added doremi-face-fg-rgb, doremi-face-bg-rgb, doremi-*-separator.
;; 2004/12/11 dadams
;;     Added doremi-thumbnail-frames.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;;     Require info+.el for all versions of Emacs.
;; 2004/11/16 dadams
;;     Removed requires of files that redefine std cmds, since std versions available.
;; 2004/10/12 dadams
;;     Added require of replace+.el for Emacs 21 also.
;; 2004/10/01 dadams
;;     Updated for Emacs 21 also.
;;     Added macro menu-item-any-version.
;; 2004/09/26 dadams
;;     Use new Do Re Mi names and files.
;; 2004/09/20 dadams
;;     Use adjust-bg-rgb instead of adjust-bg-color.
;; 2004/09/11 dadams
;;     Reflected move of commands from doremi.el to doremi-frm.el
;; 2004/09/10 dadams
;;     Replaced dlgopen.el with w32browser-dlgopen.el.
;; 2004/09/07 dadams
;;     Added doremi menu.
;; 2004/03/19 dadams
;;     Added to menu-bar-frames-menu: tile-frames-[horizontally|vertically].
;; 2000/09/27 dadams
;;     1. Added to Files menu: execute-extended-command, repeat-complex-command.
;;     2. Removed help-frame condition on show-*Help*-buffer.
;; 1999/10/07 dadams
;;     Added show-calendar and separator to Tools menu.
;; 1999/10/01 dadams
;;     Added: menu-bar-divider-menu.  Use it for [menu-bar divider].
;; 1999/09/02 dadams
;;     kill-this-buffer: use sub-kill-buffer-and-its-windows.
;; 1999/08/25 dadams
;;     1. Added Frames menu.  Changed Help to ? menu.
;;     2. Commented out menu-bar-print-menu.
;; 1999/04/08 dadams
;;     Added to help menu: help-for-help.
;; 1999/04/07 dadams
;;     1. Bound apropos stuff regardless of (fboundp 'apropos).
;;     2. Corrected help menu order.
;; 1999/04/07 dadams
;;     1. Added to help menu: help-on-click, save-*Help*-buffer.
;;     2. apropos-symbol->apropos; super-apropos-symbol->apropos-documentation.
;; 1999/04/06 dadams
;;     Added *highlight*-region fns to Edit->Region submenu.
;; 1999/04/02 dadams
;;     Only add "Show *Help* Buffer" if help-frame.
;; 1999/03/26 dadams
;;     Added vc-ediff to ediff menu (when fboundp).
;; 1999/03/23 dadams
;;     Added: ediff-revision, vc-diff.
;; 1999/03/17 dadams
;;     1. Moved Replace menu to be a Search submenu.
;;     2. Reordered Edit menu.
;;     3. Removed default Help items (duplicate).
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/24 dadams
;;     Added edit-options to Edit menu.
;; 1996/04/22 dadams
;;     1. menu-bar-edit-menu:  Added: flush-lines, keep-lines.
;;     2. menu-bar-edit-region-menu:  Added: (un)tabify-region, center-region,
;;        indent-rigidly-region, abbrevs-region, macro-region.
;; 1996/04/04 dadams
;;     1. Added fill-paragraph-ala-mode.
;;     2. Edit menu:
;;        a. Added yank-secondary and select-all to Edit menu.
;;        d. Added Edit submenus Fill, Region, Sort, Highlight.
;; 1996/03/18 dadams
;;     Added vc-diff to menu-bar-ediff-menu.
;; 1996/03/12 dadams
;;     Added diff and reordered ediff menu.
;; 1996/03/08 dadams
;;     Added redefinition of kill-this-buffer.
;; 1996/02/08 dadams
;;     Added: save-*Help*-buffer, describe-syntax, locate-library,
;;            finder-by-keyword, view-emacs-lisp-news.
;; 1996/01/26 dadams
;;     no-op -> %$>disabled@!^ (Shouldn't be bound command, else binding is shown.)
;; 1996/01/25 dadams
;;     menu-bar-help-menu: Added Emacs FAQ.
;; 1996/01/17 dadams
;;     apropos -> apropos-symbol, super-apropos -> super-apropos-symbol.
;; 1995/09/11 dadams
;;     Bookmarks added to Search menu.
;; 1995/08/29 dadams
;;     1) Added to Search menu: grep, occur.
;;     2) Put tags searches on submenu of Search.
;; 1995/08/23 dadams
;;     Changed menu-bar-final-items order.
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

(require 'menu-bar)

(and (< emacs-major-version 21);; dolist (plus, for Emacs <20: when, unless)
     (eval-when-compile (require 'cl)))
(when (eq system-type 'windows-nt)
  (require 'w32browser-dlgopen nil t)) ;; (no error if not found): dlgopen-open-files
                                       ;; `w32browser-dlgopen.el' is based on `dlgopen.el'
                                       ;; by Binu Jose Philip

(when (< emacs-major-version 21)
  (require 'help+20 nil t) ;; (no error if not found): describe-keymap, help-on-click/key
  (require 'unaccent nil t)) ;; (no error if not found): unaccent-region

(when (> emacs-major-version 21)
  (require 'help+ nil t) ;; (no error if not found): help-on-click/key
  (require 'help-fns+ nil t)) ;; (no error if not found): describe-keymap

(require 'info+ nil t) ;; (no error if not found): menu-bar-read-lispref, info-emacs-manual,
(require 'misc-cmds nil t) ;; (no error if not found): kill-buffer-and-its-windows
(require 'second-sel nil t) ;; (no error if not found):
                            ;; primary-to-secondary, secondary-to-primary, yank-secondary
(require 'apropos+ nil t) ;; (no error if not found): apropos-user-options

;; To quiet the Emacs 20 byte compiler
(defvar menu-bar-goto-menu)
(defvar menu-bar-last-search-type)

;;;;;;;;;;;;;;;;;;;;



;; REPLACES ORIGINAL in `menu-bar.el':
;; Use Emacs 22 definition.
;; Emacs 20 version fails when last-command-event is the name of the frame.
;;
(when (< emacs-major-version 21)
  (defun menu-bar-select-frame ()
    (interactive)
    (let (frame)
      (dolist (f (frame-list))
        (when (equal last-command-event (frame-parameter f 'name))
          (setq frame f)))
      ;; FRAME can be nil when user specifies the selected frame.
      (setq frame (or frame (selected-frame)))
      (make-frame-visible frame)
      (raise-frame frame)
      (select-frame frame))))


;; REPLACES ORIGINAL in `menu-bar.el':
;; Uses -other-frame.
;;;###autoload
(defun menu-bar-select-buffer ()
  "Switch to `last-command-event' buffer in other frame."
  (interactive) (switch-to-buffer-other-frame last-command-event)) ;`files+.el'


;; REPLACES ORIGINAL MENU-BAR.

;;; Main MENU-BAR entries.
;; Divider before standard menus.
(defvar menu-bar-divider-menu (make-sparse-keymap "Divider"))
(define-key global-map [menu-bar divider] (cons "||" menu-bar-divider-menu))
(define-key menu-bar-divider-menu [menu-bar-divider-hint]
  '("<-- Current mode menus to left.   ||   Common menus to right -->"
    . describe-menubar))

;;;###autoload
(defun describe-menubar ()
  "Explain the menu bar, in general terms."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (substitute-command-keys
            "To the right of the menu bar divider (\"||\") are the general menus
that always appear in every buffer.  To the left of this symbol, there
may also be additional menus that are specific to the buffer's mode
\(use `\\[describe-mode]' for information on a buffer's mode).

The general menus are as follows:

    Buffers  File  Tools  Edit  Frames  Do Re Mi  Help

Use the \"Frames\" menu to resize, tile, and hide/show frames.
Use the \"Do Re Mi\" menu to incrementally change things.
The \"Help\" menu extends the \"Help\" menu described in the Emacs manual (`\\[info]').

For information on a menu item, use the \"This\" item in the \"Describe\"
submenu of the \"Help\" menu."))
    (print-help-return-message)
    (with-current-buffer standard-output
      (help-mode)
      (buffer-string))))                ; Return the text we displayed.


;; REPLACES ORIGINAL defined in `menu-bar.el'.
(setq menu-bar-edit-menu (make-sparse-keymap "Edit"))
(define-key global-map [menu-bar edit] (cons "Edit" menu-bar-edit-menu))


;; REPLACES ORIGINAL menuus defined in `menu-bar.el'.
;; These are all moved to new top-level Search menu.
(if (< emacs-major-version 21)
    (global-unset-key [menu-bar search])
  (global-unset-key [menu-bar edit search])
  (global-unset-key [menu-bar edit separator-search])
  (global-unset-key [menu-bar edit replace])
  (global-unset-key [menu-bar edit goto])
  (global-unset-key [menu-bar edit bookmark])
  (global-unset-key [menu-bar edit separator-bookmark]))

(defconst menu-bar-search-menu (make-sparse-keymap "Search"))
(define-key global-map [menu-bar search]  (cons "Search" menu-bar-search-menu))

(when (or (featurep 'doremi-frm) (featurep 'doremi-cmd))
  (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
  (define-key global-map [menu-bar doremi] (cons "Do Re Mi" menu-bar-doremi-menu)))

(when (or (featurep 'frame-cmds) (featurep 'fit-frame))
  (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
  (define-key global-map [menu-bar frames] (cons "Frames" menu-bar-frames-menu)))

;; Main menu-bar order.
(setq menu-bar-final-items
      (append (if (< emacs-major-version 22)
                  '(divider files edit buffer tools search mule)
                '(divider file edit options buffer tools search))
              (and (boundp 'menu-bar-frames-menu) '(frames))
              (and (boundp 'menu-bar-doremi-menu) '(doremi))
              '(help-menu)
              (and (fboundp 'show-tool-bar-for-one-command) '(pop-up-tool-bar))))

;;; FRAMES menu.
(when (and (featurep 'fit-frame) (not (featurep 'frame-cmds)) (eq window-system 'w32))
  (define-key menu-bar-frames-menu [maximize-frame]
    '(menu-item "Maximize Frame" maximize-frame :help "Maximize the selected frame")))
(when (featurep 'fit-frame)
  (define-key menu-bar-frames-menu [fit-frame]
    '(menu-item "Fit This Frame" fit-frame :help "Resize frame to fit its selected window")))
  
(when (featurep 'frame-cmds)
  (define-key menu-bar-frames-menu [set-all-params-from-frame]
    '(menu-item "Set All Frame Parameters from Frame" set-all-frame-alist-parameters-from-frame
      :help "Set frame parameters of a frame to their current values in frame"))
  (define-key menu-bar-frames-menu [set-params-from-frame]
    '(menu-item "Set Frame Parameter from Frame..." set-frame-alist-parameter-from-frame
      :help "Set parameter of a frame alist to its current value in frame"))
  (define-key menu-bar-frames-menu [separator-frame-1] '("--"))
  (define-key menu-bar-frames-menu [tile-frames-vertically]
    '(menu-item "Tile Frames Vertically..." tile-frames-vertically
      :help "Tile all visible frames vertically"))
  (define-key menu-bar-frames-menu [tile-frames-horizontally]
    '(menu-item "Tile Frames Horizontally..." tile-frames-horizontally
      :help "Tile all visible frames horizontally"))
  (define-key menu-bar-frames-menu [separator-frame-2] '("--"))
  (define-key menu-bar-frames-menu [toggle-max-frame-vertically]
    '(menu-item "Toggle Max Frame Vertically" toggle-max-frame-vertically
      :help "Maximize or restore the selected frame vertically"
      :enable (frame-parameter nil 'restore-height)))
  (define-key menu-bar-frames-menu [toggle-max-frame-horizontally]
    '(menu-item "Toggle Max Frame Horizontally" toggle-max-frame-horizontally
      :help "Maximize or restore the selected frame horizontally"
      :enable (frame-parameter nil 'restore-width)))
  (define-key menu-bar-frames-menu [toggle-max-frame]
    '(menu-item "Toggle Max Frame" toggle-max-frame
      :help "Maximize or restore the selected frame (in both directions)"
      :enable (or (frame-parameter nil 'restore-width) (frame-parameter nil 'restore-height))))
  (define-key menu-bar-frames-menu [maximize-frame-vertically]
    '(menu-item "Maximize Frame Vertically" maximize-frame-vertically
      :help "Maximize the selected frame vertically"))
  (define-key menu-bar-frames-menu [maximize-frame-horizontally]
    '(menu-item "Maximize Frame Horizontally" maximize-frame-horizontally
      :help "Maximize the selected frame horizontally"))
  (define-key menu-bar-frames-menu [maximize-frame]
    '(menu-item "Maximize Frame" maximize-frame
      :help "Maximize the selected frame (in both directions)"))
  (define-key menu-bar-frames-menu [separator-frame-3] '("--"))
  (define-key menu-bar-frames-menu [iconify-everything]
    '(menu-item "Iconify All Frames" iconify-everything
      :help "Iconify all frames of session at once"))
  (define-key menu-bar-frames-menu [show-hide]
    '(menu-item "Hide Frames / Show Buffers" show-hide
      :help "Show, if only one frame visible; else hide.")))

;;; DO RE MI menu.
(when (featurep 'doremi-cmd)
  (define-key menu-bar-doremi-menu [doremi-global-marks+]
    '(menu-item "Global Marks" doremi-global-marks+
      :help "Successively cycle among global marks: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-marks+]
    '(menu-item "Marks in Buffer" doremi-marks+
      :help "Successively cycle among marks in this buffer: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-bookmarks+]
    '(menu-item "Bookmarks" doremi-bookmarks+
      :help "Successively cycle among bookmarks: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-buffers+]
    '(menu-item "Buffers" doremi-buffers+
      :help "Successively cycle among buffers: `up'/`down'")))
(when (featurep 'thumb-frm)
  (define-key menu-bar-doremi-menu [thumfr-doremi-thumbnail-frames+]
    '(menu-item "Fisheye Frame" thumfr-doremi-thumbnail-frames+
      :help "Cycle among frames using fisheye: `up'/`down'")))
(when (and (boundp 'menu-bar-doremi-menu) (featurep 'frame-cmds))
  (define-key menu-bar-doremi-menu [save-frame-config]
    '(menu-item "Save Frame Configuration" save-frame-config
      :help "Save current frame configuration (M-x jump-to-frame-config-register restores)")))
(when (featurep 'doremi-frm)
  (define-key menu-bar-doremi-menu [doremi-frame-configs+]
    '(menu-item "Frame Configurations" doremi-frame-configs+
      :help "Cycle among frame configurations recorded: `up'/`down'"))

  (define-key menu-bar-doremi-menu [doremi-fonts-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-font+]
    '(menu-item "Font" doremi-font+
      :help "Successively cycle among fonts, choosing by name: `up'/`down'"))
  (when (fboundp 'text-scale-increase)    ; Emacs 23+.
    (define-key menu-bar-doremi-menu [doremi-buffer-font-size+]
      '(menu-item "Buffer Text Size (Zoom)" doremi-buffer-font-size+
        :help "Change text size for buffer incrementally: `up'/`down'")))
  (define-key menu-bar-doremi-menu [doremi-frame-font-size+]
    '(menu-item "Frame Font Size (Zoom)" doremi-frame-font-size+
      :help "Change font size for frame incrementally: `up'/`down'"))

  (define-key menu-bar-doremi-menu [doremi-all-frames-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-all-frames-fg+]
    '(menu-item "All Frame Foregrounds..." doremi-all-frames-fg+
      :help "Change foreground of all frames incrementally: `up'/`down' (no undo)"))
  (define-key menu-bar-doremi-menu [doremi-all-frames-bg+]
    '(menu-item "All Frame Backgrounds..." doremi-all-frames-bg+
      :help "Change background of all frames incrementally: `up'/`down' (no undo)"))

  (define-key menu-bar-doremi-menu [doremi-frame-colors-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-undo-last-frame-color-change]
    '(menu-item "Undo Frame Color Change" doremi-undo-last-frame-color-change
      :enable doremi-last-frame-color
      :help "Undo the last frame color change by `doremi-fg+' or `doremi-bg+'"))
  (define-key menu-bar-doremi-menu [doremi-fg-color-name+]
    '(menu-item "Frame Foreground Name..." doremi-fg-color-name+
      :help "Change frame foreground color incrementally: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-fg+]
    '(menu-item "Frame Foreground..." doremi-fg+
      :help "Change frame foreground color incrementally: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-bg-color-name+]
    '(menu-item "Frame Background Name..." doremi-bg-color-name+
      :help "Change frame background color incrementally: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-bg+]
    '(menu-item "Frame Background..." doremi-bg+
      :help "Change frame background color incrementally: `up'/`down'"))

  (define-key menu-bar-doremi-menu [doremi-all-faces-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-all-faces-fg+]
    '(menu-item "All Faces - Foreground..." doremi-all-faces-fg+
      :help "Change foreground color of all faces incrementally: `up'/`down' (no undo)"))
  (define-key menu-bar-doremi-menu [doremi-all-faces-bg+]
    '(menu-item "All Faces - Background..." doremi-all-faces-bg+
      :help "Change background color of all faces incrementally: `up'/`down' (no undo)"))
  (define-key menu-bar-doremi-menu [doremi-faces-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-undo-last-face-change]
    '(menu-item "Undo Face Color Change" doremi-undo-last-face-change
      :enable (facep 'doremi-last-face) ; Actually, it's always non-nil, so always enabled.
      :help "Undo the last face color change by Do Re Mi"))
  (define-key menu-bar-doremi-menu [doremi-face-fg-color-name+]
    '(menu-item "Face Foreground Name..." doremi-face-fg-color-name+
      :help "Change foreground color name of a face incrementally: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-face-fg+]
    '(menu-item "Face Foreground..." doremi-face-fg+
      :help "Change foreground color of a face incrementally: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-face-bg-color-name+]
    '(menu-item "Face Background Name..." doremi-face-bg-color-name+
      :help "Change background color name of a face incrementally: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-face-bg+]
    '(menu-item "Face Background..." doremi-face-bg+
      :help "Change background color of a face incrementally: `up'/`down'"))
  )
(when (featurep 'doremi-cmd)
  (define-key menu-bar-doremi-menu [doremi-color-themes+]
    '(menu-item "Color Themes" doremi-color-themes+
      :help "Successively cycle among color themes: `up'/`down'")))

(when (featurep 'doremi-frm)
  (define-key menu-bar-doremi-menu [doremi-frame-params-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-frame-vertically+]
    '(menu-item "Move Frame" doremi-frame-vertically+
      :help "Move frame incrementally: `up'/`down'/`left'/`right'"))
  (define-key menu-bar-doremi-menu [doremi-frame-height+]
    '(menu-item "Frame Size" doremi-frame-height+
      :help "Resize frame incrementally: `up'/`down'/`left'/`right'")))
(when (featurep 'doremi-cmd)
  (define-key menu-bar-doremi-menu [doremi-window-height+]
    '(menu-item "Window Size" doremi-window-height+
      :help "Resize window incrementally: `up'/`down'/`left'/`right'"
      :enable (not (one-window-p)))))


;;; FILES menu.
;;
(when (< emacs-major-version 21)
  ;; Use `dlgopen-open-files' if available; else use `find-file-other-frame'.
  (define-key menu-bar-file-menu [open-file]
    (if (and (fboundp 'dlgopen-open-files) (eq system-type 'windows-nt))
        '(menu-item "Open File..." dlgopen-open-files
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Read a file into an Emacs buffer")
      '(menu-item "Open File..." find-file-other-frame
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Read a file into an Emacs buffer")))
  ;; Use other frame.
  (define-key menu-bar-file-menu [dired]
    '(menu-item "Open Directory..." dired-other-frame
      :help "Read a directory; operate on its files (Dired)"
      :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))

(define-key menu-bar-file-menu [new-file] ; Add for Emacs < 22.  Rename item otherwise.
  '(menu-item "New File..." find-file
    :enable (or (not (fboundp 'menu-bar-non-minibuffer-window-p))
             (menu-bar-non-minibuffer-window-p))
    :help "Create and edit a new file"))

;;;###autoload
(defun menu-bar-create-directory (directory)
  "Create a subdirectory of `default-directory' called DIRECTORY."
  (interactive (list (read-file-name "Create directory: ")))
  (let ((dir  (directory-file-name (expand-file-name directory))))
    (make-directory dir)
    (message "Created `%s'" dir)))

(define-key-after menu-bar-file-menu [new-directory]
  '(menu-item "New Directory..." menu-bar-create-directory
    :enable (or (not (fboundp 'menu-bar-non-minibuffer-window-p))
             (menu-bar-non-minibuffer-window-p))
    :help "Create a directory")
  'new-file)

(define-key-after menu-bar-file-menu [separator-new] '("--") 'new-directory)

(define-key-after menu-bar-file-menu [exec-cmd]
  '(menu-item "Execute Command" execute-extended-command
    :help "Prompts for a command to execute") 'separator-exit)
(define-key-after menu-bar-file-menu [repeat-cmd]
  '(menu-item "Repeat Earlier Command" repeat-complex-command
    :help "Edit and re-evaluate last complex command") 'exec-cmd)

(define-key-after menu-bar-file-menu [separator-execute] '("--") 'repeat-cmd)
(define-key-after menu-bar-file-menu [exit-emacs]
  '(menu-item "Exit Emacs" save-buffers-kill-emacs
    :help "Save unsaved buffers, then exit") 'separator-execute)


;; REPLACES ORIGINAL in `menu-bar.el':
;; Deletes buffer's windows as well.
;;;###autoload
(defun kill-this-buffer ()
"Delete the current buffer and delete all of its windows."
  (interactive)
  (if (and (boundp 'sub-kill-buffer-and-its-windows) ; In `setup-keys.el'.
           sub-kill-buffer-and-its-windows
           (fboundp 'kill-buffer-and-its-windows))
      (kill-buffer-and-its-windows (current-buffer)) ;`misc-cmds.el'
    (kill-buffer (current-buffer))))    ; <-- original defn.


;; EDIFF submenu of TOOLS
(when (fboundp 'vc-ediff)
  (define-key menu-bar-tools-menu [compare]
    '(menu-item "Compare" menu-bar-ediff-menu ; Remove "(Ediff)".
      :help "Display differences between files/directories")))
(define-key menu-bar-ediff-menu [ediff-revision] ; Defined in `vc+.el'.
  '(menu-item "File with Revision..." vc-ediff :help "Compare file versions using `ediff'"))
(define-key-after menu-bar-ediff-menu [vc-diff] ; Defined in `vc+.el'.
  '(menu-item "File with Revision using Diff" vc-diff
    :help "Display diffs between file versions using `diff'") 'ediff-revision)
(define-key-after menu-bar-ediff-menu [diff]
  '(menu-item "Two Files using Diff..." diff ; `diff+.el'
    :help "Display diffs between two files using `diff'") 'ediff-files)

(define-key menu-bar-edit-menu [undo]
  '(menu-item "Undo" undo :help "Undo last operation"
    :enable (and (not buffer-read-only)
             (not (eq t buffer-undo-list))
             (if (eq last-command 'undo) pending-undo-list (consp buffer-undo-list)))))
;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-cut] '("--") 'undo)
(define-key-after menu-bar-edit-menu [cut]
  '(menu-item "Cut" kill-region
    :help "Cut (kill) text in region between mark and current position"
    :enable (and mark-active (not buffer-read-only))) 'separator-edit-cut)
(define-key-after menu-bar-edit-menu [copy]
  '(menu-item "Copy" menu-bar-kill-ring-save
    :help "Copy text in region between mark and current position"
    :enable mark-active :keys "\\[kill-ring-save]") 'cut)

;; Use `x-get-selection', not `x-selection-exists-p', because of Emacs bugs on Windows etc.
;; See thread "x-selection-exists-p  vs  x-get-selection", emacs-devel@gnu.org, 2008-05-04.
(define-key-after menu-bar-edit-menu [paste]
  '(menu-item "Paste" yank
    :help "Paste (yank) text most recently cut/copied"
    :enable
    (and
     (or
      (and (fboundp 'x-selection-exists-p) (x-selection-exists-p))
      (and x-select-enable-clipboard (x-selection-exists-p 'CLIPBOARD)))
     (not buffer-read-only))) 'copy)
(when (fboundp 'yank-secondary)
  (define-key-after menu-bar-edit-menu [yank-secondary] ; In `second-sel.el'
    '(menu-item "Paste Secondary" yank-secondary
      :help "Paste (yank) secondary selection."
      :enable
      (and (fboundp 'x-get-selection) (x-get-selection 'SECONDARY) (not buffer-read-only))
      :keys "\\[secondary-dwim]")
    'paste)
  (define-key-after menu-bar-edit-menu [primary-to-secondary] ; In `second-sel.el'
    '(menu-item "Move Secondary to Region" primary-to-secondary
      :help "Make the region in the current buffer into the secondary selection."
      :enable mark-active :keys "C-1 \\[secondary-dwim]")
    'yank-secondary)
  (define-key-after menu-bar-edit-menu [secondary-swap-region] ; In `second-sel.el'
    '(menu-item "Swap Region and Secondary" secondary-swap-region
      :help "Make region into secondary selection, and vice versa."
      :enable (and (fboundp 'x-get-selection) (x-get-selection 'SECONDARY))
      :keys "C-- \\[secondary-dwim]")
    'primary-to-secondary)
  (define-key-after menu-bar-edit-menu [secondary-to-primary] ; In `second-sel.el'
    '(menu-item "Select Secondary as Region" secondary-to-primary
      :help "Go to the secondary selection and select it as the active region."
      :enable (and (fboundp 'x-get-selection) (x-get-selection 'SECONDARY))
      :keys "C-0 \\[secondary-dwim]")
    'secondary-swap-region))

(defvar yank-menu (cons "Select Yank" nil))
(fset 'yank-menu (cons 'keymap yank-menu))

(define-key-after menu-bar-edit-menu [select-paste]
  '(menu-item "Select and Paste" yank-menu :help "Paste (yank) text cut or copied earlier"
    :enable (and (cdr yank-menu) (not buffer-read-only)))
  (if (fboundp 'secondary-to-primary) 'secondary-to-primary 'paste))
(define-key-after menu-bar-edit-menu [clear]
  '(menu-item "Clear" delete-region
    :help "Delete the text in region between mark and current position"
    :enable (and mark-active (not buffer-read-only) (not (mouse-region-match))))
  'select-paste)
(define-key-after menu-bar-edit-menu [mark-whole-buffer]
  '(menu-item "Select All" mark-whole-buffer
    :help "Select everything in buffer (for a subsequent cut/copy)") 'clear)

;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-delete-lines] '("--") 'mark-whole-buffer)

(define-key-after menu-bar-edit-menu [flush-lines] ; In `replace+.el' for Emacs 20.
  '(menu-item "Delete Matching Lines..." flush-lines
    :help "Delete all lines after cursor that match a regular expression")
  'separator-edit-delete-lines)
(define-key-after menu-bar-edit-menu [keep-lines] ; In `replace+.el' for Emacs 20.
  '(menu-item "Delete Non-Matching Lines..." keep-lines
    :help "Delete all lines after cursor that do not match a regular expression")
  'flush-lines)
;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-select-all] '("--") 'keep-lines)

(defvar menu-bar-edit-fill-menu (make-sparse-keymap "Fill"))
(define-key-after menu-bar-edit-menu [props]
  '(menu-item "Text Properties" facemenu-menu :help "Change properties of text in region")
  'separator-edit-select-all)
(define-key-after menu-bar-edit-menu [fill]
  (cons "Fill" menu-bar-edit-fill-menu) 'props)
(defvar menu-bar-edit-region-menu (make-sparse-keymap "Edit Region"))
(defalias 'menu-bar-edit-region-menu (symbol-value 'menu-bar-edit-region-menu))
(define-key-after menu-bar-edit-menu [region]
  (cons "Edit Region" menu-bar-edit-region-menu) 'fill)
(defvar menu-bar-edit-sort-menu (make-sparse-keymap "Sort Region"))
(defalias 'menu-bar-edit-sort-menu (symbol-value 'menu-bar-edit-sort-menu))
(define-key-after menu-bar-edit-menu [sort]
  (cons "Sort Region" menu-bar-edit-sort-menu) 'region)

;; EDIT FILL submenu.
(define-key menu-bar-edit-fill-menu [fill-nonuniform-para]
  '(menu-item "Fill Non-Uniform ¶s" fill-nonuniform-paragraphs
    :help "Fill paragraphs in selection, allowing varying indentation"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-fill-menu [fill-indiv-para]
  '(menu-item "Fill Uniform ¶s" fill-individual-paragraphs
    :help "Fill paragraphs of uniform indentation within selection"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-fill-menu [fill-region]
  '(menu-item "Fill ¶s" fill-region
    :help "Fill text in region to fit between left and right margin"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-fill-menu [fill-para]
  '(menu-item "Fill ¶" fill-paragraph-ala-mode
    :help "Fill the paragraph, doing what `M-q' does (if bound)"))

(defun fill-paragraph-ala-mode (&optional arg)
  "Do whatever `M-q' does, if it is bound.  Else, `fill-paragraph'.
Normally, this fills a paragraph according to the current major mode.
For example, in C Mode, `M-q' is normally bound to `c-fill-paragraph',
and in Lisp Mode, `M-q' is normally bound to `lisp-fill-paragraph'.
ARG means justify as well as fill."
  (let ((map (current-local-map)))
    (or (and map (funcall (lookup-key map "\M-q") arg))
        (funcall (lookup-key (current-global-map) "\M-q") arg)
        (fill-paragraph arg))))

;; EDIT REGION submenu.
(when (fboundp 'unaccent-region)
  (define-key menu-bar-edit-region-menu [unaccent-region]
    '(menu-item "Unaccent" unaccent-region ; Defined in `unaccent'.
      :help "Replace accented chars in selection by unaccented chars"
      :enable (and mark-active (not buffer-read-only)))))
(define-key menu-bar-edit-region-menu [capitalize-region]
  '(menu-item "Capitalize" capitalize-region
    :help "Capitalize (initial caps) words in the selection"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-region-menu [downcase-region]
  '(menu-item "Downcase" downcase-region :help "Make words in the selection lower-case"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-region-menu [upcase-region]
  '(menu-item "Upcase" upcase-region :help "Make words in the selection upper-case"
    :enable (and mark-active (not buffer-read-only))))
;;--------------------
(define-key menu-bar-edit-region-menu [separator-chars] '("--"))
(define-key menu-bar-edit-region-menu [untabifyn]
  '(menu-item "Untabify" untabify
    :help "Convert all tabs in region (selection) to multiple spaces"
    :enable mark-active))
(define-key menu-bar-edit-region-menu [tabify-region]
  '(menu-item "Tabify" tabify :help "Convert multiple spaces in region to tabs when possible"
    :enable mark-active))
(define-key menu-bar-edit-region-menu [comment-region]
  '(menu-item "(Un)Comment" comment-region
    :help "Comment or uncomment each line in the selection"
    :enable (and mark-active comment-start (not buffer-read-only))))
(define-key menu-bar-edit-region-menu [center-region]
  '(menu-item "Center" center-region
    :help "Center each nonblank line that starts in the selection"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-region-menu [indent-rigidly-region]
  '(menu-item "Rigid Indent" indent-rigidly
    :help "Indent each line that starts in the selection"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-region-menu [indent-region]
  '(menu-item "Column/Mode Indent" indent-region
    :help "Indent each nonblank line in the selection"
    :enable (and mark-active (not buffer-read-only))))

;;--------------------
(define-key menu-bar-edit-region-menu [separator-indent] '("--"))
(define-key menu-bar-edit-region-menu [abbrevs-region]
  '(menu-item "Expand Abbrevs" expand-region-abbrevs
    :help "Expand each abbrev in the selection (with confirmation)"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-region-menu [macro-region]
  '(menu-item "Exec Keyboard Macro" apply-macro-to-region-lines ; In `macros+.el'.
    :help "Run keyboard macro at start of each line in selection"
    :enable (and last-kbd-macro mark-active (not buffer-read-only))))

;; EDIT SORT submenu.
(define-key menu-bar-edit-sort-menu [sort-regexp-fields]
  '(menu-item "Regexp Fields..." sort-regexp-fields :help "Sort the selection lexicographically"
    :enable (and last-kbd-macro mark-active (not buffer-read-only))))
(define-key menu-bar-edit-sort-menu [sort-pages]
  '(menu-item "Pages" sort-pages :help "Sort pages in the selection alphabetically"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-sort-menu [sort-paragraphs]
  '(menu-item "Paragraphs" sort-paragraphs
    :help "Sort paragraphs in the selection alphabetically"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-sort-menu [sort-numeric-fields]
  '(menu-item "Numeric Field" sort-numeric-fields
    :help "Sort lines in selection numerically by the Nth field"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-sort-menu [sort-fields]
  '(menu-item "Field" sort-fields
    :help "Sort lines in selection lexicographically by the Nth field"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-sort-menu [sort-columns]
  '(menu-item "Columns" sort-columns
    :help "Sort lines in selection alphabetically, by a certain range of columns"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-sort-menu [sort-lines]
  '(menu-item "Lines" sort-lines :help "Sort lines in selection alphabetically"
    :enable (and mark-active (not buffer-read-only))))
(define-key menu-bar-edit-sort-menu [reverse-region]
  '(menu-item "Reverse" reverse-region :help "Reverse the order of the selected lines"
    :enable (and mark-active (not buffer-read-only))))

;;; SEARCH menu.
(when (< emacs-major-version 22)
  (defun nonincremental-repeat-word-search-forward ()
    "Search forward for the previous search string."
    (interactive)
    (word-search-forward (car search-ring)))

  (defun nonincremental-repeat-word-search-backward ()
    "Search backward for the previous search string."
    (interactive)
    (word-search-backward (car search-ring)))

  (define-key menu-bar-search-menu [reminder6] '(" " . %$>disabled@!^))
  (define-key menu-bar-search-menu [reminder5]
    (cons (substitute-command-keys
           "  Incr. Regexp Search: \\[isearch-forward-regexp], \
\\[isearch-backward-regexp]") '%$>disabled@!^))
  (define-key menu-bar-search-menu [reminder4]
    (cons (substitute-command-keys
           " Word Search: \\[isearch-forward] RET C-w, \\[isearch-backward] \
RET C-w") '%$>disabled@!^))
  (define-key menu-bar-search-menu [reminder3]
    (cons (substitute-command-keys
           "Incr. Search: \\[isearch-forward], \\[isearch-backward]  \
\(\\[isearch-forward] C-h: Help)") '%$>disabled@!^))
  (define-key menu-bar-search-menu [reminder2]
    '("           ** Reminder **" . %$>disabled@!^))
  (define-key menu-bar-search-menu [reminder1] '(" " . %$>disabled@!^))
  (put '%$>disabled@!^ 'menu-enable '(not t))
  (when (boundp 'menu-bar-i-search-menu)
    (define-key menu-bar-search-menu [i-search]
      '(menu-item "Incremental Search" menu-bar-i-search-menu
        :help "Incremental Search finds partial matches while you type the search \
string.\nIt is most convenient from the keyboard.  Try it!")))
  ;;--------------------
  (define-key menu-bar-search-menu [separator-search-multiple] '("--")))

(unless (< emacs-major-version 21)
  (define-key menu-bar-search-menu [goto] (cons "Go To" menu-bar-goto-menu)))
(define-key menu-bar-search-menu [bookmark]
  '(menu-item "Bookmarks" menu-bar-bookmark-map
    :help "Record buffer positions (\"bookmarks\"), and jump between them"))
(defvar menu-bar-search-tags-menu (make-sparse-keymap "Tags"))
(defalias 'menu-bar-search-tags-menu (symbol-value 'menu-bar-search-tags-menu))
(define-key menu-bar-search-menu [tags] (cons "Tags" menu-bar-search-tags-menu))
(define-key menu-bar-search-menu [occur]
  '(menu-item "Occurrences..." occur
    :help "Show lines in buffer that contain a match for regular expression"))
(define-key menu-bar-search-menu [grep]
  '(menu-item "Grep..." grep
    :help "Run `grep' and collect output for navigating to match lines"))
(defvar menu-bar-search-replace-menu (make-sparse-keymap "Replace"))
(defalias 'menu-bar-search-replace-menu (symbol-value 'menu-bar-search-replace-menu))
(define-key menu-bar-search-menu [replace] (cons "Replace" menu-bar-search-replace-menu))

(unless (< emacs-major-version 22)
  (defvar menu-bar-i-search-menu
    (make-sparse-keymap "Incremental Search"))
  (define-key menu-bar-i-search-menu [isearch-backward-regexp]
    '(menu-item "        Backward..." isearch-backward-regexp
      :help "Search backwards for a regular expression as you type it"))
  (define-key menu-bar-i-search-menu [isearch-forward-regexp]
    '(menu-item "Regexp Forward..." isearch-forward-regexp
      :help "Search forward for a regular expression as you type it"))
  (define-key menu-bar-i-search-menu [isearch-backward]
    '(menu-item "        Backward..." isearch-backward
      :help "Search backwards for a string as you type it"))
  (define-key menu-bar-i-search-menu [isearch-forward]
    '(menu-item "String Forward..." isearch-forward
      :help "Search forward for a string as you type it"))  
  (define-key menu-bar-search-menu [i-search]
    (list 'menu-item "Incremental Search" menu-bar-i-search-menu))
  ;;--------------------
  (define-key menu-bar-search-menu [separator-search-replace] '("--")))

(when (< emacs-major-version 22)
  ;;--------------------
  (define-key menu-bar-search-menu [separator-search-word] '("--"))
  (define-key menu-bar-search-menu [repeat-word-search-back]
    '(menu-item "             Again" nonincremental-repeat-word-search-backward
      :help "Search backward again for the same word"))
  (define-key menu-bar-search-menu [word-search-back]
    '(menu-item "     Backward..." word-search-backward
      :help "Search backward, ignoring differences in puncuation"))
  (define-key menu-bar-search-menu [repeat-word-search-fwd]
    '(menu-item "             Again" nonincremental-repeat-word-search-forward
      :help "Search forward again for the same word"))
  (define-key menu-bar-search-menu [word-search-fwd]
    '(menu-item "Word Forward..." word-search-forward
      :help "Search forward, ignoring differences in puncuation"))
  ;;--------------------
  (define-key menu-bar-search-menu [separator-search-re] '("--"))
  (define-key menu-bar-search-menu [repeat-regexp-back]
    '(menu-item "               Again" nonincremental-repeat-re-search-backward
      :help "Search forward again for the same regular expression"))
  (define-key menu-bar-search-menu [re-search-backward]
    '(menu-item "       Backward..." nonincremental-re-search-backward
      :help "Search backward for a regular expression"))
  (define-key menu-bar-search-menu [repeat-regexp-fwd]
    '(menu-item "               Again" nonincremental-repeat-re-search-forward
      :help "Search forward again for the same regular expression"))
  (define-key menu-bar-search-menu [re-search-forward]
    '(menu-item "Regexp Forward..." nonincremental-re-search-forward
      :help "Search forward for a regular expression"))
  ;;--------------------
  (define-key menu-bar-search-menu [separator-search] '("--"))
  (define-key menu-bar-search-menu [repeat-search-back]
    '(menu-item "               Again" nonincremental-repeat-search-backward
      :help "Repeat last search backward"
      :enable (or (not (boundp 'menu-bar-last-search-type))
               (and (eq menu-bar-last-search-type 'string) search-ring)
               (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))))
  (define-key menu-bar-search-menu [search-backward]
    '(menu-item "String Backward..." nonincremental-search-backward
      :help "Search backward for a string"))
  (define-key menu-bar-search-menu [repeat-search-fwd]
    '(menu-item "               Again" nonincremental-repeat-search-forward
      :help "Repeat last search forward"
      :enable (or (not (boundp 'menu-bar-last-search-type))
               (and (eq menu-bar-last-search-type 'string) search-ring)
               (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))))
  (define-key menu-bar-search-menu [search-forward]
    '(menu-item "String Forward..." nonincremental-search-forward
      :help "Search forward for a string")))

(unless (< emacs-major-version 22)
  (defun nonincremental-repeat-search-forward ()
    "Search forward for the previous search string or regexp."
    (interactive)
    (cond ((and (eq menu-bar-last-search-type 'string) search-ring)
           (search-forward (car search-ring)))
          ((and (eq menu-bar-last-search-type 'regexp) regexp-search-ring)
           (re-search-forward (car regexp-search-ring)))
          ((and (eq menu-bar-last-search-type 'word) search-ring)
           (word-search-forward (car search-ring)))
          (t (error "No previous search"))))
  
  (defun nonincremental-repeat-search-backward ()
    "Search backward for the previous search string or regexp."
    (interactive)
    (cond ((and (eq menu-bar-last-search-type 'string) search-ring)
           (search-backward (car search-ring)))
          ((and (eq menu-bar-last-search-type 'regexp) regexp-search-ring)
           (re-search-backward (car regexp-search-ring)))
          ((and (eq menu-bar-last-search-type 'word) search-ring)
           (word-search-backward (car search-ring)))
          (t (error "No previous search"))))

  (defun menu-bar-word-search-forward (word)
    "Search forward, ignoring differences in punctuation."
    (interactive "sSearch for word: ")
    (setq menu-bar-last-search-type 'word)
    (if (equal word "")
        (word-search-forward (car search-ring))
      (isearch-update-ring word nil)
      (word-search-forward word)))
  
  (defun menu-bar-word-search-backward (word)
    "Search backward, ignoring differences in punctuation."
    (interactive "sSearch for word: ")
    (setq menu-bar-last-search-type 'word)
    (if (equal word "")
        (word-search-backward (car search-ring))
      (isearch-update-ring word nil)
      (word-search-backward word)))

  (define-key menu-bar-search-menu [repeat-search-back]
    '(menu-item "       Backwards" nonincremental-repeat-search-backward
      :enable (or (and (memq menu-bar-last-search-type '(string word)) search-ring)
               (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
      :help "Repeat last search backwards"))
  (define-key menu-bar-search-menu [repeat-search-fwd]
    '(menu-item "Repeat Forward" nonincremental-repeat-search-forward
      :enable (or (and (memq menu-bar-last-search-type '(string word)) search-ring)
               (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
      :help "Repeat last search forward"))
  ;;--------------------
  (define-key menu-bar-search-menu [separator-repeat-search] '(menu-item "--"))
  (define-key menu-bar-search-menu [menu-bar-word-search-backward]
    '(menu-item "     Backward..." menu-bar-word-search-backward
      :help "Search backward, ignoring differences in punctuation"))
  (define-key menu-bar-search-menu [menu-bar-word-search-forward]
    '(menu-item "Word Forward..." menu-bar-word-search-forward
      :help "Search forward, ignoring differences in punctuation"))
  (define-key menu-bar-search-menu [re-search-backward]
    '(menu-item "       Backward..." nonincremental-re-search-backward
      :help "Search backward for a regular expression"))
  (define-key menu-bar-search-menu [re-search-forward]
    '(menu-item "Regexp Forward..." nonincremental-re-search-forward
      :help "Search forward for a regular expression"))
  (define-key menu-bar-search-menu [search-backward]
    '(menu-item "       Backward..." nonincremental-search-backward
      :help "Search backward for a string"))
  (define-key menu-bar-search-menu [search-forward]
    '(menu-item "String Forward..." nonincremental-search-forward
      :help "Search forward for a string")))


;;; SEARCH TAGS submenu.
(define-key menu-bar-search-tags-menu [set-tags-name]
  '(menu-item "Set Tags File Name..." visit-tags-table
    :help "Tell Tags commands which tag table file to use"))
(define-key menu-bar-search-tags-menu [apropos-tags]
  '(menu-item "Tags Apropos..." tags-apropos :help "Find tags matching a regexp"))
;----------------------
(define-key menu-bar-search-tags-menu [separator-tags-misc] '("--"))
(define-key menu-bar-search-tags-menu [tags-continue]
  '(menu-item "Continue Tags Search/Replace" tags-loop-continue
    :help "Continue last tags search or replace operation"))
(define-key menu-bar-search-tags-menu [tags-search]
  '(menu-item "Search Tagged Files..." tags-search
    :help "Search for a regexp in all tagged files"))
(define-key menu-bar-search-tags-menu [find-tag-regexp]
  '(menu-item "Find Tag Regexp..." find-tag-regexp
    :help "Find tag that matches a regular expression"))
;----------------------
(define-key menu-bar-search-tags-menu [separator-tags-regexp] '("--"))

;;;###autoload
(defun menu-bar-next-tag-other-window ()
  "Find the next definition of the tag already specified."
  (interactive)
  (find-tag-other-window nil t))

(define-key menu-bar-search-tags-menu [next-tag-other-window]
  '(menu-item "Find Next Tag" menu-bar-next-tag-other-window
    :help "Find next tag name"
    :enable (and (boundp 'tags-location-ring) (not (ring-empty-p tags-location-ring)))))
(define-key menu-bar-search-tags-menu [find-tag-other-window]
  '(menu-item "Find Tag..." find-tag-other-window
    :help "Find tag whose name matches input string"))

;; REPLACE submenu
(define-key menu-bar-search-replace-menu [replace-regexp]
  '(menu-item "       Regexp..." replace-regexp
    :help "Replace things after cursor that match regexp"
    :enable (not buffer-read-only)))
(define-key menu-bar-search-replace-menu [replace-string]
  '(menu-item "Global String..." replace-string :help "Replace string, with no confirmation"
    :enable (not buffer-read-only)))
;;--------------------
(define-key menu-bar-search-replace-menu [separator-search-replace-global] '("--"))
(define-key menu-bar-search-replace-menu [tags-query-replace]
  '(menu-item (substitute-command-keys "            Tags... (again: \\[tags-loop-continue])")
    tags-query-replace
    :help "Replace a regexp in tagged files, with confirmation"))
(define-key menu-bar-search-replace-menu [map-query-replace-regexp]
  '(menu-item "            Map..." map-query-replace-regexp
    :help "Replace regexp matches with various strings, in rotation."
    :enable (not buffer-read-only)))
(define-key menu-bar-search-replace-menu [query-replace-regexp]
  '(menu-item "      Regexp..." query-replace-regexp
    :help "Replace regular expression interactively, ask about each occurrence"
    :enable (not buffer-read-only)))

(define-key menu-bar-search-replace-menu [query-replace]
;;   (if (fboundp 'query-replace-w-options) ; Bind it in `replace+.el' now, not here.
;;       '(menu-item "Query String" query-replace-w-options
;;         :help "Replace string interactively, ask about each occurrence"
;;         :enable (not buffer-read-only))
  '(menu-item "Query String" query-replace
    :help "Replace string interactively, ask about each occurrence"
    :enable (not buffer-read-only)))


;;; HELP menu.

;;; General help
(define-key menu-bar-help-menu [separator-genl-help] '("--"))
(when (fboundp 'save-*Help*-buffer)
  (define-key menu-bar-help-menu [save-*Help*-buffer]
    '(menu-item "Save *Help* Buffer" save-*Help*-buffer ; In `help+20.el'.
      :help "Rename *Help* buffer as new buffer *Help*<N>, N=2,3....")))
(when (fboundp 'show-*Help*-buffer)
  (define-key menu-bar-help-menu [show-*Help*-buffer]
    '(menu-item "Show *Help* Buffer" show-*Help*-buffer ; In `frame-cmds.el'
      :help "Raise a frame showing buffer *Help*"
      :enable (and (get-buffer "*Help*") (fboundp 'frames-on) ; In `frame-cmds.el'
               (frames-on (get-buffer "*Help*"))))))
(define-key menu-bar-help-menu [help-for-help]
  '(menu-item "Help on Help..." help-for-help :help "Emacs main help command"))

;;; Remove some default bindings
(define-key menu-bar-help-menu [finder-by-keyword] nil)
(define-key menu-bar-help-menu [emacs-tutorial] nil)
(when (>= emacs-major-version 21)
  (define-key menu-bar-help-menu [emacs-tutorial-language-specific] nil)
  (define-key menu-bar-help-menu [emacs-problems] nil)
  (define-key menu-bar-help-menu [sep1] nil)
  (define-key menu-bar-help-menu [emacs-manual] nil))
(define-key menu-bar-help-menu [sep2] nil)
(define-key menu-bar-help-menu [emacs-faq] nil)
(define-key menu-bar-help-menu [emacs-news] nil)


;;; Whoops!? submenu
(defvar menu-bar-whereami-menu (make-sparse-keymap "Whoops!?"))
(define-key menu-bar-help-menu [whereami]
  (cons "Whoops!?" menu-bar-whereami-menu))
(define-key menu-bar-whereami-menu [view-lossage]
  '(menu-item "What did I do !?" view-lossage :help "Display last 100 input keystrokes"))
(define-key menu-bar-whereami-menu [top-level]
  '(menu-item "Back to Top Level" top-level :help "Exit all recursive editing levels"))
(define-key menu-bar-whereami-menu [keyboard-quit]
  '(menu-item "Cancel Current Action" keyboard-quit :help "Quit any operation in progress"))

;;; Apropos submenu
(defvar menu-bar-apropos-menu (make-sparse-keymap "Apropos"))
(define-key-after menu-bar-help-menu [apropos] (cons "Apropos" menu-bar-apropos-menu)
                  'separator-genl-help)
;; Wipe out standard (Emacs 22+) Apropos menu and define new Apropos menu
;; The names are different so we don't need to specify the order (otherwise existing order rules)
(define-key menu-bar-apropos-menu [apropos-tags]
  '(menu-item "Tags..." tags-apropos :help "Find tags matching a regexp"))
(define-key menu-bar-apropos-menu [apropos-documentation] nil)
(define-key menu-bar-apropos-menu [apropos-doc]
  '(menu-item "Symbol Descriptions (Doc)..." apropos-documentation
    :help "Find functions and variables whose doc string matches a regexp"))
(define-key menu-bar-apropos-menu [apropos] nil)
(define-key menu-bar-apropos-menu [apropos-symbol]
  '(menu-item "Symbols..." apropos :help "Find symbols whose name matches a regexp"))
(define-key menu-bar-apropos-menu [apropos-value] nil)
(define-key menu-bar-apropos-menu [apropos-var-value]
  '(menu-item "Variable Values..." apropos-value
    :help "Find variables whose values match a regexp"))
(define-key menu-bar-apropos-menu [apropos-variables] nil)
(define-key menu-bar-apropos-menu [apropos-variable]
  '(menu-item "All Variables..." apropos-variable
    :help "Find variables whose name matches a regexp"))
(when (fboundp 'apropos-user-options)
  (define-key menu-bar-apropos-menu [apropos-user-options]
    '(menu-item "User Options..." apropos-user-options
      :help "Find user options (variables you can change) whose name matches a regexp")))
(define-key menu-bar-apropos-menu [apropos-commands] nil)
(define-key menu-bar-apropos-menu [apropos-command]
  '(menu-item "Commands..." apropos-command :help "Find commands whose name matches a regexp"))
(define-key menu-bar-apropos-menu [sep1] nil)
(define-key menu-bar-apropos-menu [emacs-command-node] nil)
(define-key menu-bar-apropos-menu [emacs-key-command-node] nil)
(define-key menu-bar-apropos-menu [elisp-index-search] nil)
(define-key menu-bar-apropos-menu [emacs-index-search] nil)
(define-key menu-bar-apropos-menu [emacs-glossary] nil)

;;; Describe submenu
(define-key-after menu-bar-help-menu [describe] (cons "Describe" menu-bar-describe-menu)
                  'apropos)
(if (not (fboundp 'describe-command))
    (define-key menu-bar-describe-menu [describe-function] ; `Function...'
      '(menu-item "Function..." describe-function :help "Describe a command or other function"))
  (define-key menu-bar-describe-menu [describe-command] ; `Command...'
  '(menu-item "Command..." describe-command :help "Describe an Emacs command"))
  (define-key-after menu-bar-describe-menu [describe-function] ; `Function...'
    '(menu-item "Function..." describe-function :help "Describe a command or other function")
    'describe-command))
(if (not (fboundp 'describe-option))
    (define-key-after menu-bar-describe-menu [describe-variable] ; `Variable...'
      '(menu-item "Variable..." describe-variable
        :help "Describe an Emacs user option or other variable")
      'describe-function)
  (define-key-after menu-bar-describe-menu [describe-option] ; `Option...'
    '(menu-item "Option..." describe-option :help "Describe an Emacs user option")
    'describe-function)
  (define-key-after menu-bar-describe-menu [describe-option-of-type] ; `Option of Type...'
    '(menu-item "Option of Type..." describe-option-of-type
      :help "Describe a user option of a particular type")
    'describe-option)
  (define-key-after menu-bar-describe-menu [describe-variable] ; `Variable...'
      '(menu-item "Variable..." describe-variable
        :help "Describe an Emacs user option or other variable")
      'describe-option-of-type))
(define-key-after menu-bar-describe-menu [describe-face] ; `Face...'
  '(menu-item "Face..." describe-face :help "Describe a face")
  'describe-variable)
(define-key menu-bar-describe-menu [describe-key-1] nil) ; Remove this for Emacs 21
(define-key-after menu-bar-describe-menu [describe-key] ; `Key...'
  '(menu-item "Key..." describe-key
    :help "Describe a command bound to a key, mouse action, or menu item")
  'describe-face)
(when (fboundp 'describe-keymap)
  (define-key-after menu-bar-describe-menu [describe-keymap] ; `Keymap...'
    '(menu-item "Keymap..." describe-keymap :help "Describe a keymap")
    'describe-key))
(when (fboundp 'describe-file) ; Defined in `help-fns+.el' and `help+20.el'.
  (define-key-after menu-bar-describe-menu [describe-file] ; `File...'
    '(menu-item "File..." describe-file
      :help "Describe a file")
    (if (fboundp 'describe-keymap) 'describe-keymap 'describe-face)))
(define-key-after menu-bar-describe-menu [describe-input-method] ; `Input Method...'
  '(menu-item "Input Method..." describe-input-method
    :help "Describe keyboard layout for an input method")
  (if (fboundp 'describe-file)
      'describe-file
    (if (fboundp 'describe-keymap) 'describe-keymap 'describe-face)))
(define-key-after menu-bar-describe-menu [describe-coding-system] ; `Coding System...'
  '(menu-item "Coding System..." describe-coding-system :help "Describe a coding system")
  'describe-input-method)
(when (fboundp 'help-on-click/key)      ; `This...' - defined in `help+.el' and `help+20.el'.
  (define-key-after menu-bar-describe-menu [help-on-click/key]
    '(menu-item "This..." help-on-click/key       
      :help "Describe a key/menu sequence or object clicked with the mouse")
    'describe-coding-system))

(define-key-after menu-bar-describe-menu [separator-current]
  '("--") (if (fboundp 'help-on-click/key) 'help-on-click/key 'describe-coding-system))
(define-key-after menu-bar-describe-menu [describe-mode] ; `Current Modes'
  '(menu-item "Current Modes" describe-mode
    :help "Describe this buffer's major and minor modes")
  'separator-current)
(define-key-after menu-bar-describe-menu [list-keybindings] ; `Current Key Bindings'
  '(menu-item "Current Key Bindings" describe-bindings
    :help "List all current keybindings, with brief descriptions") 'describe-mode)
(define-key-after menu-bar-describe-menu [describe-syntax] ; `Current Syntax'
  '(menu-item "Current Syntax" describe-syntax
    :help "Describe the syntax specifications in the current syntax table")
  'list-keybindings)
(when (fboundp 'describe-current-display-table)
  (define-key-after menu-bar-describe-menu [describe-current-display-table]
    '(menu-item "Current Display Table" describe-current-display-table ; `Current Display Table'
      :help "Describe the display table in use in the selected window and buffer")
  'describe-syntax))
(when (fboundp 'describe-current-coding-system-briefly) ; `Current Coding Systems'
  (define-key-after menu-bar-describe-menu [describe-coding-system-briefly]
    '(menu-item "Current Coding Systems" describe-current-coding-system-briefly
      :help "Describe the current coding systems")
    (if (fboundp 'describe-current-display-table)
        'describe-current-display-table
      'list-keybindings)))
(when (fboundp 'describe-menubar)
  (define-key-after menu-bar-describe-menu [describe-menubar] ; `Menu Bar'
    '(menu-item "Menu Bar" describe-menubar :help "Explain the menu-bar, in general terms")
    (if (fboundp 'describe-current-coding-system-briefly)
        'describe-coding-system-briefly
      (if (fboundp 'describe-current-display-table)
          'describe-current-display-table
        'list-keybindings))))

;;; Manuals submenu.

;; REPLACES ORIGINAL defined in `menu-bar.el'.
;; Remove some default bindings.
;; Name changes.
(defconst menu-bar-manuals-menu (make-sparse-keymap "Learn More"))
(define-key-after menu-bar-help-menu [manuals]
  (cons "Learn More" menu-bar-manuals-menu) 'describe)
(when (>= emacs-major-version 21)
  (define-key menu-bar-manuals-menu [order-emacs-manuals]
    '(menu-item "Ordering Manuals" view-order-manuals
      :help "How to order manuals from the Free Software Foundation")))
(define-key-after menu-bar-help-menu [separator-manuals] '("--") 'manuals)
(define-key menu-bar-manuals-menu [man]
  '(menu-item "Unix Man Page..." manual-entry
    :help "Unix man-page documentation for external commands and libraries"))
(define-key menu-bar-manuals-menu [info]
  '(menu-item "All Manuals (`Info')" Info-directory :help "Read any of the installed manuals"))
(define-key menu-bar-manuals-menu [last-info]
  '(menu-item "Last Accessed Manual (`Info')" info
    :help "Open Info, the doc browser, at the last doc place visited"))
(define-key menu-bar-manuals-menu [emacs-faq] nil)
(define-key menu-bar-manuals-menu [emacs-news] nil)
(define-key menu-bar-manuals-menu [key] nil)
(define-key menu-bar-manuals-menu [command] nil)


;;; Emacs Lisp submenu of Manuals submenu.
(defvar menu-bar-emacs-lisp-manual-menu (make-sparse-keymap "Emacs Lisp"))
(define-key menu-bar-manuals-menu [emacs-lisp-manual]
  (cons "Emacs Lisp" menu-bar-emacs-lisp-manual-menu))

;; Is there a direct way to get to Lisp NEWS in Emacs 21?
(when (< emacs-major-version 21)
  (define-key menu-bar-emacs-lisp-manual-menu [emacs-Lisp-News]
    '(menu-item "What's New (Change History)" view-emacs-lisp-news
      :help "Display information on recent changes to Emacs Lisp")))
(define-key menu-bar-emacs-lisp-manual-menu [finder-by-keyword]
  '(menu-item "Locate Libraries by Keyword" finder-by-keyword
    :help "Find Emacs Lisp packages matching a keyword"))
(define-key menu-bar-emacs-lisp-manual-menu [locate-library] ; Defined in `subr.el', `help+20.el'
  '(menu-item "Locate Library..." locate-library
    :help "Show the full path name of an Emacs library"))
(define-key menu-bar-emacs-lisp-manual-menu [emacs-lisp-manual-separator] '("--"))
(when (> emacs-major-version 21)
  (define-key menu-bar-emacs-lisp-manual-menu [elisp-index-search]
    '(menu-item "    Index..." elisp-index-search
      :help "Look up a topic in the Emacs Lisp manual index")))
(when (fboundp 'menu-bar-read-lispref)  ; Defined in `info+.el'.
  (define-key menu-bar-emacs-lisp-manual-menu [menu-bar-read-lispref]
    '(menu-item "Manual" menu-bar-read-lispref
      :help "Read the Emacs Lisp reference manual"))
  (define-key menu-bar-emacs-lisp-manual-menu [info-elintro]
    '(menu-item "Intro to Elisp" menu-bar-read-lispintro
      :help "Read an introduction to Emacs Lisp programming")))


;;; Emacs submenu of Manuals submenu.
(defvar menu-bar-emacs-manual-menu (make-sparse-keymap "Emacs"))
(define-key menu-bar-manuals-menu [emacs-manual] (cons "Emacs" menu-bar-emacs-manual-menu))
(when (>= emacs-major-version 21)
  (define-key menu-bar-emacs-manual-menu [emacs-problems]
    '(menu-item "Known Problems" view-emacs-problems
      :help "Known problems of this Emacs version")))
(define-key menu-bar-emacs-manual-menu [emacs-news]
  '(menu-item "What's New (Change History)" view-emacs-news
    :help "New features of this Emacs version"))
(define-key menu-bar-emacs-manual-menu [emacs-faq]
  '(menu-item "FAQ" view-emacs-FAQ
    :help "Read frequently asked questions about Emacs (with answers)"))
(if (> emacs-major-version 21)
    (define-key menu-bar-emacs-manual-menu [emacs-tutorial-language-specific]
      '(menu-item "Tutorial (Choose Language)..." help-with-tutorial-spec-language
        :help "Learn Emacs with a hands-on tutorial"))
  (define-key menu-bar-emacs-manual-menu [emacs-tutorial]
    '(menu-item "Tutorial" help-with-tutorial :help "Learn Emacs with a hands-on tutorial")))
(define-key menu-bar-emacs-manual-menu [emacs-manual-separator] '("--"))
(when (> emacs-major-version 21)
  (define-key menu-bar-emacs-manual-menu [emacs-glossary]
    '(menu-item "    Glossary" search-emacs-glossary
      :help "Show the Emacs manual Glossary"))
  (define-key menu-bar-emacs-manual-menu [emacs-index-search]
    '(menu-item "    Index..." emacs-index-search
      :help "Look up a topic in the Emacs manual index")))
(define-key menu-bar-emacs-manual-menu [key]
  '(menu-item "    Key Description..." Info-goto-emacs-key-command-node
    :help "Show Emacs manual section that describes a key sequence"))
(define-key menu-bar-emacs-manual-menu [command]
  '(menu-item "    Command Description..." Info-goto-emacs-command-node
    :help "Show Emacs manual section that describes a command"))
(when (fboundp 'info-emacs-manual)
  (define-key menu-bar-emacs-manual-menu [info-emacs-manual]
    '(menu-item "Manual" info-emacs-manual :help "Read the Emacs manual")))


;;; OPTIONS menu.

(defmacro menu-bar-make-toggle-any-version (name variable doc message help &rest body)
  "Return a valid `menu-bar-make-toggle' call in Emacs 20 or later.
NAME is the name of the toggle command to define.
VARIABLE is the variable to set.
DOC is the menu-item name.
MESSAGE is the toggle message, minus status.
HELP is :help string.
BODY is the function body to use.  If present, it is responsible for
setting the variable and displaying a status message (not MESSAGE)."
  (if (< emacs-major-version 21)
      `(menu-bar-make-toggle ,name ,variable ,doc ,message ,@body)
    `(menu-bar-make-toggle ,name ,variable ,doc ,message ,help ,@body)))

(when (or (boundp 'doremi-push-frame-config-for-cmds-flag)
          (boundp 'fit-frame-inhibit-fitting-flag)
          (boundp 'autofit-frames-flag) (boundp 'thumfr-thumbify-dont-iconify-flag))
  (define-key menu-bar-options-menu [frames-separator] '("--")))
(when (boundp 'doremi-push-frame-config-for-cmds-flag)
  (define-key menu-bar-options-menu [doremi-push-frame-config]
    (menu-bar-make-toggle-any-version menu-bar-doremi-push-frame-config
                          doremi-push-frame-config-for-cmds-flag
                          "Save Frame Configs (DoReMi)"
                          "Saving frame configurations is %s for DoReMi commands"
                          "Saving of frame configurations by DoReMi commands")))
(when (boundp 'fit-frame-inhibit-fitting-flag)
  (define-key menu-bar-options-menu [inhibit-fit-frame]
    (menu-bar-make-toggle-any-version menu-bar-inhibit-fit-frame fit-frame-inhibit-fitting-flag
                          "Inhibit Frame Fitting"
                          "Inhibit frame fitting is %s (overrides automatic frame fitting)"
                          "Inhibit frame fitting")))
(when (boundp 'autofit-frames-flag)
  (define-key menu-bar-options-menu [autofit-frames]
    (menu-bar-make-toggle-any-version menu-bar-autofit-frames autofit-frames-flag
                          "Fit Frames Automatically"
                          "Automatic fitting of one-window frames is %s"
                          "Automatic fitting of one-window frames")))
(when (boundp 'thumfr-thumbify-dont-iconify-flag)
  (define-key menu-bar-options-menu [thumfr-thumbify-frames]
    (menu-bar-make-toggle-any-version menu-bar-thumbify-frames
                                      thumfr-thumbify-dont-iconify-flag
                          "Thumbify, Don't Iconify, Frames"
                          "Thumbifying instead of iconifying frames is %s"
                          "Thumbifying instead of iconifying frames")))

(define-key menu-bar-options-menu [all-options-separator] '("--"))
(define-key menu-bar-options-menu [edit-options]
  '(menu-item "Show, Edit All Options" edit-options
    :help "Edit a list of Emacs user option (variable) values"))

;; (when (boundp 'replace-w-completion-flag)
;;   (define-key-after menu-bar-options-menu [replace-w-completion-flag]
;;     (menu-bar-make-toggle-any-version menu-bar-toggle-replace-w-completion
;;                                       replace-w-completion-flag
;;                                       "Completion for Query Replace"
;;                                       "Using completion with query replace is %s"
;;                                       "Using completion with query replace")
;;     'case-fold-search))

;; Can't seem to byte-compile this - try to debug later.
;; (when (and (< emacs-major-version 21) (featurep 'icomplete)) ; Not needed/available for 21?
;;   (define-key-after menu-bar-options-menu [icomplete-mode]
;;     (menu-bar-make-toggle toggle-icomplete-mode icomplete-mode
;;                           "Command Completion Clues" "Completion Clues %s")
;;     'all-options-separator))



;; REPLACES ORIGINAL in `menu-bar.el'.
;; Updated for added items.
;; For this to work, however, compilation needs to be with Emacs >= 21,
;; to use the right definition of `menu-bar-make-toggle'.
;;
(cond ((= emacs-major-version 21)
       (defun menu-bar-options-save ()
         "Save current values of Options menu items using Custom."
         (interactive)
         (dolist (elt '(debug-on-quit debug-on-error auto-compression-mode
                        case-fold-search truncate-lines show-paren-mode
                        transient-mark-mode global-font-lock-mode
                        current-language-environment default-input-method
                        ;; D. ADAMS: Added these options.
                        doremi-push-frame-config-for-cmds-flag
                        fit-frame-inhibit-fitting-flag autofit-frames-flag
                        thumfr-thumbify-dont-iconify-flag replace-w-completion-flag))
           (if (default-value elt)
               (customize-save-variable elt (default-value elt))))
         (if (memq 'turn-on-auto-fill text-mode-hook)
             (customize-save-variable 'text-mode-hook
                                      (default-value 'text-mode-hook)))
         (if (featurep 'saveplace)
             (customize-save-variable 'save-place (default-value 'save-place)))
         (if (featurep 'uniquify)
             (customize-save-variable 'uniquify-buffer-name-style
                                      (default-value 'uniquify-buffer-name-style)))))
      ((> emacs-major-version 21)
       (defun menu-bar-options-save ()
         "Save current values of Options menu items using Custom."
         (interactive)
         (let ((need-save nil))
           ;; These are set with menu-bar-make-mm-toggle, which does not
           ;; put on a customized-value property.
           (dolist (elt '(line-number-mode column-number-mode size-indication-mode
                          cua-mode show-paren-mode transient-mark-mode
                          blink-cursor-mode display-time-mode display-battery-mode))
             (and (customize-mark-to-save elt)
                  (setq need-save t)))
           ;; These are set with `customize-set-variable'.
           (dolist (elt '(scroll-bar-mode debug-on-quit debug-on-error tooltip-mode
                          menu-bar-mode tool-bar-mode save-place uniquify-buffer-name-style
                          fringe-mode indicate-empty-lines indicate-buffer-boundaries
                          case-fold-search current-language-environment default-input-method
                          ;; D. ADAMS: Added these options.
                          doremi-push-frame-config-for-cmds-flag fit-frame-inhibit-fitting-flag
                          autofit-frames-flag thumfr-thumbify-dont-iconify-flag
                          replace-w-completion-flag
                          ;; Saving `text-mode-hook' is somewhat questionable,
                          ;; as we might get more than we bargain for, if
                          ;; other code may has added hooks as well.
                          ;; Nonetheless, not saving it would like be confuse
                          ;; more often.
                          ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
                          text-mode-hook))
             (and (get elt 'customized-value)
                  (customize-mark-to-save elt)
                  (setq need-save t)))
           ;; Save if we changed anything.
           (when need-save
             (custom-save-all))))))



;;;@@@Emacs19 (autoload 'show-calendar "calendar+"
;;;@@@Emacs19   "Show *Calendar* buffer, generating it if not already present." t)
;;;@@@Emacs19 (autoload 'calendar "calendar+"
;;;@@@Emacs19   "Display a 3-month calendar in another window." t)
;;;@@@Emacs19  ;; Autoloaded from `calendar+.el': calendar, show-calendar

;;; ---------------------------

;;;@@@Emacs19 (defvar menu-bar-help-menu (make-sparse-keymap "Help")) ; Wipe out original.
;;;@@@Emacs19 (define-key global-map [menu-bar help-menu] (cons "?" menu-bar-help-menu))


;;; TOOLS menu.
;;;@@@Emacs19 ;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar tools separator-print])
;;;@@@Emacs19 (global-unset-key [menu-bar tools ps-print-region])
;;;@@@Emacs19 (global-unset-key [menu-bar tools ps-print-buffer])
;;;@@@Emacs19 (global-unset-key [menu-bar tools print-region])
;;;@@@Emacs19 (global-unset-key [menu-bar tools print-buffer])
;;;@@@Emacs19
;;;@@@Emacs19 (defvar menu-bar-print-menu (make-sparse-keymap "Print"))
;;;@@@Emacs19 (define-key menu-bar-tools-menu [print] (cons "Print" menu-bar-print-menu))
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [separator-print] '("--") 'print)
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [separator-cal] '("--") 'rmail)
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [show-calendar]
;;;@@@Emacs19                 '("Show Calendar" . show-calendar) 'separator-cal)
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [calendar]
;;;@@@Emacs19                 '("Calendar and Reminders" . calendar) 'calendar)

;;;@@@Emacs19 ;; PRINT submenu of TOOLS
;;;@@@Emacs19 ;; The `*-declp-*' commands are defined in `misc-cmds.el'.
;;;@@@Emacs19 (define-key menu-bar-print-menu [ps-print-region]
;;;@@@Emacs19   '("Region as Postscript" . ps-print-region-with-faces))
;;;@@@Emacs19 ;(put 'declp-region-w-switches 'menu-enable 'mark-active)
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [declp-region-w-switches]
;;;@@@Emacs19 ;  '("Region with Switches..." . declp-region-w-switches))
;;;@@@Emacs19 ;(put 'pr-declp-region 'menu-enable 'mark-active)
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-paged-region]
;;;@@@Emacs19 ;  '("Paged Region..." . pr-declp-region))
;;;@@@Emacs19 ;(put 'declp-region 'menu-enable 'mark-active)
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-region]
;;;@@@Emacs19 ;  '("Region..." . declp-region))
;;;@@@Emacs19 ;;--------------------
;;;@@@Emacs19 (define-key menu-bar-print-menu [separator-print-buffer] '("--"))
;;;@@@Emacs19 (define-key menu-bar-print-menu [ps-print-buffer]
;;;@@@Emacs19   '("Buffer as Postscript" . ps-print-buffer-with-faces))
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [declp-buffer-w-switches]
;;;@@@Emacs19 ;  '("Buffer with Switches..." . declp-buffer-w-switches))
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-paged-buffer]
;;;@@@Emacs19 ;  '("Paged Buffer..." . pr-declp-buffer))
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-buffer]
;;;@@@Emacs19 ;  '("Buffer..." . declp-buffer))

;;;@@@Emacs19 ;;; VERSION CONTROL submenu of TOOLS
;;;@@@Emacs19 (when (boundp 'vc-menu-map)
;;;@@@Emacs19 ;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar tools vc separator1])
;;;@@@Emacs19 (global-unset-key [menu-bar tools vc separator2])
;;;@@@Emacs19 (define-key vc-menu-map [vc-status-here] '("Files Here" . vc-status-here)))
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-directory] '("Files Below" . vc-directory)
;;;@@@Emacs19                   'vc-status-here)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-dir-separator1] '("--") 'vc-directory)
;;;@@@Emacs19 (define-key-after vc-menu-map [ediff-revision]
;;;@@@Emacs19                   '("Compare with Version..." . vc-ediff) 'vc-dir-separator1)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-diff]
;;;@@@Emacs19              '("Compare Last Version using Diff" . vc-diff) 'ediff-revision)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-version-other-window]
;;;@@@Emacs19   '("Show Other Version..." . vc-version-other-window) 'vc-diff)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-dir-separator2] '("--")
;;;@@@Emacs19                   'vc-version-other-window)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-rename-file]
;;;@@@Emacs19                   '("Rename File..." . vc-rename-file)
;;;@@@Emacs19   'vc-register)
;;;@@@Emacs19 (define-key vc-menu-map [vc-check-out] '("Check In/Out" . vc-toggle-read-only))
;;;@@@Emacs19
;;;@@@Emacs19 (put 'vc-diff 'menu-enable
;;;@@@Emacs19      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-ediff 'menu-enable
;;;@@@Emacs19                '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-version-other-window 'menu-enable
;;;@@@Emacs19                  '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-toggle-read-only 'menu-enable '(or vc-mode vc-dired-mode))
;;;@@@Emacs19 (put 'vc-insert-headers 'menu-enable '(or vc-mode vc-dired-mode))
;;;@@@Emacs19 ;;; vc-dired-mode is OK (e.g. Unregistered).
;;;@@@Emacs19 (put 'vc-register 'menu-enable '(not vc-mode))
;;;@@@Emacs19 (put 'vc-rename-file 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-revert-buffer 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-cancel-version 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-print-log 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-update-change-log 'menu-enable
;;;@@@Emacs19  '(or (eq (vc-buffer-backend) 'RCS) vc-dired-mode (eq 'dired-mode major-mode)))


;;; EDIT menu
;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar edit separator-edit])

;;;@@@Emacs19 (define-key menu-bar-edit-menu [undo] '("Undo" . advertised-undo))
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [separator-edit-undo] '("--") 'undo)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [cut]
;;;@@@Emacs19                   '("Cut" . kill-region) 'separator-edit-undo)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [copy]
;;;@@@Emacs19                   '("Copy" . menu-bar-kill-ring-save) 'cut)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [paste] '("Paste" . yank) 'copy)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [select-paste]
;;;@@@Emacs19                   '("Select and Paste" . yank-menu)
;;;@@@Emacs19   'paste)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [clear]
;;;@@@Emacs19                   '("Clear" . delete-region) 'select-paste)


;;;@@@Emacs19 (defalias 'menu-bar-edit-fill-menu (symbol-value 'menu-bar-edit-fill-menu))

;;;@@@Emacs19 (when (fboundp 'start-process)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [spell]
;;;@@@Emacs19                   '("Spell" . ispell-menu-map) 'sort))


;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar help emacs-news])
;;;@@@Emacs19 (global-unset-key [menu-bar help emacs-faq])

;;;@@@Emacs19 (define-key menu-bar-help-menu [help-on-click]
;;;@@@Emacs19   '("What's This?..." . help-on-click/key))       ; Defined in `help+.el'.
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [help-for-help]
;;;@@@Emacs19   '("Help on Help..." . help-for-help) 'help-on-click)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-menubar]
;;;@@@Emacs19   '("Describe Menu Bar" . describe-menubar) 'help-for-help)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-help-click] '("--")
;;;@@@Emacs19   'view-lossage)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-mode]
;;;@@@Emacs19   '("Describe Mode" . describe-mode) 'separator-help-click)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-syntax]
;;;@@@Emacs19   '("Describe Mode Syntax" . describe-syntax) 'describe-mode)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-variable]
;;;@@@Emacs19   '("Describe Variable..." . describe-variable) 'describe-syntax)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [edit-options]
;;;@@@Emacs19   '("Show/Set Variables" . edit-options) 'describe-variable)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-function]
;;;@@@Emacs19   '("Describe Function..." . describe-function) 'edit-options)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [where-is]
;;;@@@Emacs19   '("Where is Command..." . where-is) 'describe-function)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [Info-goto-emacs-command-node]
;;;@@@Emacs19   '("`Info' on Command..." . Info-goto-emacs-command-node) 'where-is)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-key]
;;;@@@Emacs19   '("Describe Key/Menu..." . describe-key) 'Info-goto-emacs-command-node)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [Info-goto-emacs-key-command-node]
;;;@@@Emacs19   '("`Info' on Key/Menu..." . Info-goto-emacs-key-command-node) 'describe-key)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [list-keybindings]
;;;@@@Emacs19   '("Show Key/Menu Bindings" . describe-bindings)
;;;@@@Emacs19   'Info-goto-emacs-key-command-node)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-manuals] '("--")
;;;@@@Emacs19   'apropos-doc%%%%%%%%%%%%%)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [info] '("`Info' (online manuals)" . info)
;;;@@@Emacs19   'separator-manuals)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [man]
;;;@@@Emacs19   '("Unix Manual..." . manual-entry) 'info)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-emacs] '("--") 'man)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-tutorial]
;;;@@@Emacs19   '("Emacs Tutorial" . help-with-tutorial) 'separator-emacs)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-faq]
;;;@@@Emacs19   '("Emacs FAQ" . view-emacs-FAQ) 'emacs-tutorial)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-news]
;;;@@@Emacs19   '("Emacs Changes" . view-emacs-news) 'emacs-faq)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-lispdoc] '("--")
;;;@@@Emacs19   'emacs-news)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [finder-by-keyword]
;;;@@@Emacs19   '("Lisp Libraries by Keyword" . finder-by-keyword) ; Defined in `finder.el'.
;;;@@@Emacs19   'separator-lispdoc)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [locate-library] ; Defined in `help.el'.
;;;@@@Emacs19   '("Locate Lisp Library..." . locate-library) 'finder-by-keyword)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-Lisp-News]
;;;@@@Emacs19   '("Emacs Lisp Changes" . view-emacs-lisp-news) ; Defined in `help.el'.
;;;@@@Emacs19   'locate-library)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-about] '("--")
;;;@@@Emacs19   'emacs-Lisp-News)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-version]
;;;@@@Emacs19   '("Show Version" . emacs-version) 'separator-about)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [report-emacs-bug]
;;;@@@Emacs19   '("Send Bug Report..." . report-emacs-bug) 'emacs-version)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-help-buffer] '("--")
;;;@@@Emacs19   'report-emacs-bug)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [save-*Help*-buffer]
;;;@@@Emacs19   '("Save *Help* Buffer" . save-*Help*-buffer) 'separator-help-buffer)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [show-*Help*-buffer]
;;;@@@Emacs19   '("Show *Help* Buffer" . show-*Help*-buffer) 'save-*Help*-buffer)

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'menu-bar+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; menu-bar+.el ends here


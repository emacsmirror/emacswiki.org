;;; facemenu+.el --- Extensions to `facemenu.el'.
;;
;; Filename: facemenu+.el
;; Description: Extensions to `facemenu.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2009, Drew Adams, all rights reserved.
;; Created: Sat Jun 25 14:42:07 2005
;; Version:
;; Last-Updated: Mon Nov 16 15:51:47 2009 (-0800)
;;           By: dradams
;;     Update #: 1527
;; URL: http://www.emacswiki.org/cgi-bin/wiki/facemenu+.el
;; Keywords: faces, extensions, convenience, menus, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `doremi', `doremi-frm', `easymenu', `eyedropper',
;;   `facemenu', `faces', `faces+', `frame-cmds', `frame-fns',
;;   `hexrgb', `misc-fns', `mwheel', `ring', `ring+', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `facemenu.el'.
;;
;;  This library enhances the "Text Properties" menu.  It adds menu
;;  items to the menu, and provides two different versions of the
;;  menu: one for the menu-bar Edit menu (`facemenu-menu') and one for
;;  the mouse popup menu (`facemenu-mouse-menu').  In standard library
;;  `facemenu.el', these two menus are the same.
;;
;;  Items are added to each of these menus to examine, copy, and
;;  change foreground and background colors in various ways.
;;
;;  In the `C-mouse-2' popup version of the menu
;;  (`facemenu-mouse-menu'), menu items use the character under the
;;  mouse pointer, instead of the character after the text cursor
;;  (point).  For example, in the mouse menu, "Describe Properties"
;;  describes the text properties under the mouse pointer.  This makes
;;  the mouse menu generally more convenient than the menubar menu -
;;  just point and click.
;;
;;  Menu items "Do Re Mi - *" make use of commands `doremi-face-fg+'
;;  `doremi-face-bg+', and `doremi-undo-last-face-change', which are
;;  defined in library `doremi-frm.el'.  They let you change the face
;;  color incrementally, using the arrow keys or the mouse wheel, and
;;  undo such changes.
;;
;;  Menu items "Palette - *" make use of library `palette.el' (which
;;  is available only for Emacs 22 and later).  They let you use a
;;  color palette to change the color.  You can restore the face color
;;  before as it was before palette editing, by using command
;;  `facemenup-face-bg-restore' or `facemenup-face-fg-restore'.
;;
;;  If option `facemenup-palette-update-while-editing-flag' is nil,
;;  then quitting the palette using `q' cancels any color change you
;;  made there, and exiting the palette using `x' effects the color
;;  change.  If the option is non-nil (the default), then each time
;;  you change the palette color the face color changes as well.  This
;;  lets you see the effect immediately, without exiting the palette,
;;  but you cannot then use `q' in the palette to cancel the edit.
;;  But you can still restore the color as it was before the edit.
;;
;;  Both Do Re Mi and the color palette let you change colors by
;;  changing color components, whether RGB (red, green, blue) or HSV
;;  (hue, saturation, value).
;;
;;  In addition, standard commands `facemenu-set-face' (`M-o o') and
;;  `list-faces-display' have been enhanced to let you pick a face to
;;  apply from the *Faces* display.  The face sample text is an action
;;  button that does this.  The face name is a link to the face
;;  description, which also has a link to customize the face.  If the
;;  region is active when you call either of these functions, then
;;  clicking a face's sample text applies the face to the region;
;;  otherwise, it applies the face to newly entered text.  To use this
;;  feature with `facemenu-set-face', use a numeric prefix argument,
;;  such as `C-8'.
;;
;;  The reason for this enhancement to `facemenu-set-face' and
;;  `list-faces-display' is to let you see what face you are choosing:
;;  its appearance, not just its name.  If you use Icicles, then you
;;  do not need this enhancement, because face names are displayed in
;;  *Completions* with their own faces.
;;
;;  Similarly, standard command `list-colors-display' has been
;;  enhanced to open the color palette on a color when you click it.
;;  Editing the color does not change the original color's definition.
;;  The palette is useful in this context mainly to show you the given
;;  color in context.
;;
;;  Standard command `facemenu-add-face' has also been enhanced here,
;;  so that it prevents the highlighting that you add from being
;;  erased by font lock.  To take advantage of this, you must use
;;  Emacs version 22 or later, and you must also load library
;;  `font-lock+.el'.
;;
;;  If you load library `highlight.el' before you load `facemenu+.el',
;;  then the commands in that library are also added to the Text
;;  Properties menu, as a Highlight submenu.
;;
;;  Commands defined here:
;;
;;    `facemenu-mouse-menu', `facemenup-change-bg-of-face-at-mouse+',
;;    `facemenup-change-bg-of-face-at-point+',
;;    `facemenup-change-fg-of-face-at-mouse+',
;;    `facemenup-change-fg-of-face-at-point+',
;;    `facemenup-customize-face-at-mouse',
;;    `facemenup-customize-face-at-point',
;;    `facemenup-describe-text-properties-at-mouse',
;;    `facemenup-face-bg-restore', `facemenup-face-fg-restore',
;;    `facemenup-palette-face-bg-at-mouse',
;;    `facemenup-palette-face-bg-at-point',
;;    `facemenup-palette-face-fg-at-mouse',
;;    `facemenup-palette-face-fg-at-point',
;;    `facemenup-paste-to-face-bg-at-mouse',
;;    `facemenup-paste-to-face-bg-at-point',
;;    `facemenup-paste-to-face-fg-at-mouse',
;;    `facemenup-paste-to-face-fg-at-point',
;;    `facemenu-rgb-format-for-display',
;;    `facemenup-set-face-attribute',
;;    `facemenup-set-face-attribute-at-mouse',
;;    `facemenup-set-face-attribute-at-point',
;;    `facemenup-set-face-bg-RGB-at-mouse',
;;    `facemenup-set-face-bg-RGB-at-point',
;;    `facemenup-set-face-bg-RGB-hex-at-mouse',
;;    `facemenup-set-face-bg-RGB-hex-at-point',
;;    `facemenup-set-face-fg-RGB-at-mouse',
;;    `facemenup-set-face-fg-RGB-at-point',
;;    `facemenup-set-face-fg-RGB-hex-at-mouse',
;;    `facemenup-set-face-fg-RGB-hex-at-point',
;;    `palette-for-background-at-point',
;;    `palette-for-foreground-at-point'.
;;
;;  User options defined here:
;;
;;    `facemenup-palette-update-while-editing-flag'.
;;
;;  Non-interactive functions defined here:
;;
;;    `facemenup-face-bg', `facemenup-face-fg',
;;    `facemenup-set-face-attribute-at--1',
;;    `facemenup-set-face-from-list'.
;;
;;  Internal variables defined here:
;;
;;    `facemenu-mouse-menu', `facemenup-err-mouse',
;;    `facemenup-err-point', `facemenup-highlight-menu',
;;    `facemenup-last-face-bg', `facemenup-last-face-changed',
;;    `facemenup-last-face-fg'.
;;
;;  Button types defined here:
;;
;;    `help-facemenu-edit-color', `help-facemenu-set-face'.
;;
;;
;;  ***** NOTE: The following functions defined in `facemenu.el'
;;              have been REDEFINED HERE:
;;
;;    `facemenu-add-face' (Emacs 22+), `facemenu-read-color',
;;    `facemenu-set-face' (Emacs 22+), `list-colors-print' (Emacs
;;    22+).
;;
;;
;;  ***** NOTE: The following function defined in `faces.el'
;;              has been REDEFINED HERE (Emacs 22+):
;;
;;    `list-faces-display'.
;;
;;  Add this to your init file (~/.emacs):
;;
;;    (require 'facemenu+)
;;
;;
;;  Suggestions, if you use Emacs 22 or later:
;;
;;  1. Load library `font-lock+.el' also, to prevent font lock from
;;     erasing the highlighting you add using `facemenu+.el'.
;;
;;  2. (This is unrelated to `facemenu+.el'.)  Customize option
;;     `facemenu-listed-faces' to t, so that any faces you use are
;;     automatically added to the face menu.  That way, to use one
;;     again, you need not choose `Other...' in the menu each time.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/11/16 dadams
;;     Added: facemenup-palette-update-while-editing-flag.  Thx to Ahei.
;;     facemenup-palette-face-(bg|fg)-at-(mouse|point):
;;       Respect facemenup-palette-update-while-editing-flag.
;;       Use facemenup-face-(bg|fg), not face-(background|foreground) - bug fix.
;; 2009/11/15 dadams
;;     facemenup-paste-to-face-(bg|fg)-at-point:
;;       Test eyedrop-last-picked-color, not eyedrop-picked-background, for error.
;; 2009/11/07 dadams
;;     Renamed: *-change-(bg|fg)-*-at-(point|mouse) to *-change-(bg|fg)-*-at-(point|mouse)+.
;;     Applied doremi cmd renamings (added +).
;; 2009/08/04 dadams
;;     Added: facemenu-rgb-format-for-display.
;;     list-faces-display: Use read-regexp in interactive spec for Emacs 23.
;;     list-colors-print: Use facemenu-rgb-format-for-display, not hardcoded #RRGGBB.
;; 2009/06/25 dadams
;;     facemenu-read-color: nil REQUIRE-MATCH for completing-read.
;; 2008/04/05 dadams
;;     Use easy-menu-add-item instead of easy-menu-do-add-item (no longer exists in Emacs 23).
;; 2008/01/17 dadams
;;     Removed soft require of icicles.el.
;; 2007/12/27 dadams
;;     Added Help menu item.
;;     Move Display Fonts from end of list to after Display Colors.
;; 2007/11/27 dadams
;;     list-faces-display: If available, use icicle-read-string-completing.
;; 2007/09/04 dadams
;;     Added: redefinition of facemenu-read-color.
;; 2007/06/05 dadams
;;     Added Highlight submenu.  Added: facemenup-highlight-menu.
;; 2007/05/18 dadams
;;     Require cl.el only at compile time (for Emacs < 21).
;; 2007/03/25 dadams
;;     Added: facemenu-add-face.
;; 2007/03/20 dadams
;;     facemenu-set-face: Redefined to let numeric prefix arg use face list.
;;     Mention in header that you need not use the M-o o enhancement if you have Icicles.
;; 2006/08/06 dadams
;;     Added redefinition of list-colors-print.
;;     Added: help-facemenu-edit-color.
;; 2006/08/05 dadams
;;     Added: facemenup-set-face-from-list, help-facemenu-set-face (button) (Emacs 22 only).
;;     Added redefinitions of: facemenu-set-face, list-faces-display (Emacs 22 only).
;;     Require help-mode.el (Emacs 22 only).
;;     Changed text of Eyedropper menu items when palette is available.
;; 2006/06/25 dadams
;;     Moved here from faces+.el: set-face-(back|fore)ground-RGB-*.  No longer require faces+.el.
;;       Renamed with prefix facemenup- and changed background/foreground to bg/fg.
;;     Renamed: set-attribute-of-face* to facemenup-set-face-attribute*,
;;              change-(back|fore)ground-of-face-at* to facemenup-change-(bg|fg)-at*,
;;              customize-face-at* to facemenup-customize-face-at*.
;;     Added: facemenup-face-(bg|fg)(-restore), facemenup-last-face-*, *-paste-to-face*.
;;     facemenup-palette-face-(bg|fg)-at-*, facemenup-set-face-(bg|fg)*: Save last bg|fg.
;;     facemenup-set-face-(bg|fg)*: Use eyedrop-face-at-point.
;;     Changed require of doremi-frm.el to soft require.
;; 2006/06/24 dadams
;;     Added: facemenup-palette-face-(bg|fg)-at-*.  Added to menus.
;;     change-*-of-face-at-*, set-attribute-of-face-at-*, customize-face-at-*:
;;       Use eyedrop-face-at-point.
;;     Updated Commentary.
;; 2006/06/23 dadams
;;     Require eyedropper.el or palette.el, depending on the Emacs version.
;;     pick-(fore|back)ground-at-* -> eyedrop-pick-(fore|back)ground-at-*
;;     change-(fore|back)ground-of-face-at-*:
;;       Use plain C-u, not <0, to pick up picked color.
;;       Call doremi-face-(bg|fg) with pickup-p arg.
;;       Bug fix: Added missing interactive prefix arg.
;;     Renamed: *-at-mouse-pointer to *-at-mouse.
;; 2005/07/02 dadams
;;     Renamed: "Change Face Attribute" to "Set Face Attribute" and
;;              `change-attribute-*' to `set-attribute-*').
;;     Added: "Set Face *".
;;     set-attribute-of-face-at--1: read-from-minibuffer -> read-minibuffer.
;;     set-attribute-of-face: Use set-attribute-of-face-at--1 and intern face.
;;     Reordered menus: grouped foreground and background stuff.
;; 2005/07/01 dadams
;;     change-(fore|back)ground-of-face-at-*: Added increment arg - pass it along.
;;     Added: menu items Pick Up Foreground, Pick Up Background.
;; 2005/06/28 dadams
;;     Added: customize-face-at-point, customize-face-at-mouse-pointer.
;;     Renamed: Change Face (Foreground|Background) -> same + "Incrementally".
;;     change-*-of-face-at-*: Moved *Face Sample* stuff to doremi-frm.el.
;;     Added require of easymenu.el.
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

(require 'facemenu)
(require 'doremi-frm nil t) ;; (no error if not found)
                            ;; doremi-face-fg+, doremi-face-bg+, doremi-undo-last-face-change
(if (fboundp 'defvaralias) ;; Emacs 22
    (require 'palette) ;; eyedrop-pick-*-at-*, eyedrop-face-at-point
  (require 'eyedropper)) ;; eyedrop-pick-*-at-*, eyedrop-face-at-point
(require 'hexrgb nil t) ;; (no error if not found) hexrgb-read-color, hexrgb-hex-to-int
;; (require 'icicles nil t) ;; (no error if not found):
                            ;; icicle-read-color, icicle-read-string-completing
(require 'easymenu) ;; easy-menu-add-item, easy-menu-create-menu,
(when (> emacs-major-version 21) (require 'help-mode)) ;; help-xref (button type)
(when (< emacs-major-version 21) (require 'faces+ nil t)) ;; read-face-name
(eval-when-compile (when (< emacs-major-version 21) (require 'cl))) ;; copy-tree

;; Quiet the byte-compiler.
(defvar palette-action) ;; Defined in `palette.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom facemenup-palette-update-while-editing-flag t
  "Non-nil means update the face whose color you're editing in the palette.
The face is updated automatically each time you change the palette's
current color, which is typically when you hit `RET' or click
`mouse-2'.  If nil, then the face is updated only when you exit the
palette using `x'."
  :type 'boolean :group 'facemenu)

(defconst facemenup-err-mouse "No defined face under mouse pointer. Face to change"
  "Error message for no defined face under mouse pointer.")

(defconst facemenup-err-point "No defined face at cursor (point). Face to change"
  "Error message for no defined face under mouse pointer.")

(defvar facemenup-last-face-changed nil "Last face changed using the face menu.")

(defvar facemenup-last-face-bg nil
  "Background of last face changed using face menu, before the change.")

(defvar facemenup-last-face-fg nil
  "Foreground of last face changed using face menu, before the change.")

;; `font-menus.el' puts Display Fonts last in menu. Move it after Display Colors.
(when (featurep 'font-menus)
  (define-key-after facemenu-menu [display-fonts] '("Display Fonts" . display-fonts) 'dc))

(defconst facemenup-highlight-menu
    (easy-menu-create-menu
    "Highlight"
    (append
     '(["Highlight Region/Buffer" hlt-highlight-region t]
       ["Highlight Regexp in Region/Buffer" hlt-highlight-regexp-region t]
       ["Highlight Regexp to End" hlt-highlight-regexp-to-end t]
       ["Unhighlight Region/Buffer" hlt-unhighlight-region t]
       ["Unhighlight Region/Buffer for Face" hlt-unhighlight-region-for-face t]
       ["Highlighter Pen" hlt-highlighter-mouse t]
       ["Eraser" hlt-eraser-mouse t])
     (and (fboundp 'hlt-show-only)
          '("--"
            ["Hide Only Faces..." hlt-hide-only t]
            ["Show Only Faces..." hlt-show-only t]
            ["Show Faces..." hlt-show t]
            ["Hide Faces..." hlt-hide t]
            "--"))
     '(["Choose Highlighting Face" hlt-choose-default-face t]
       ["Replace Highlighting Face in Region/Buffer" hlt-replace-highlight-face t]
       ["Toggle Using Overlays for Highlighting" hlt-toggle-use-overlays-flag t])
     (and (fboundp 'hlt-toggle-act-on-any-face-flag)
          '(["Toggle Highlighting Arbritrary Faces" hlt-toggle-act-on-any-face-flag t]))
     (and (fboundp 'hlt-next-highlight)
          '(["Go To Next Highlight" hlt-next-highlight t]
            ["Go To Previous Highlight" hlt-previous-highlight t]))))
  "Highlight submenu of Text Properties menu.")

;; Use a different menu for the mouse popup menu from the Edit > Text Properties menu.
(defvar facemenu-mouse-menu (copy-tree facemenu-menu)
  "Facemenu top-level popup mouse menu keymap.")
(defalias 'facemenu-mouse-menu facemenu-mouse-menu)
(define-key global-map [C-down-mouse-2] 'facemenu-mouse-menu)

;; Replace "Describe Properties" (Emacs 22) or "List Properties" (Emacs 20) menu item in
;; mouse menu.  Use `facemenup-describe-text-properties-at-mouse' instead of
;; `describe-text-properties' (Emacs 22) or `list-text-properties-at' (Emacs 20).
(define-key facemenu-mouse-menu [dp]
  (cons (purecopy "Describe Properties") 'facemenup-describe-text-properties-at-mouse))

;; For Emacs 20, rename "List Properties" to "Describe Properties" in menu-bar menu.
(unless (fboundp 'describe-text-properties)
  (define-key facemenu-menu [dp]
    (cons (purecopy "Describe Properties") 'list-text-properties-at)))

;; Add a separator before "Describe Properties": it and the items that
;; follow it do not apply to the region like the items before them do.
(easy-menu-add-item facemenu-mouse-menu () ["---" nil] 'dp)
(easy-menu-add-item facemenu-menu () ["---" nil] 'dp)

;; Add new menu items to mouse menu.
(when (fboundp 'hlt-highlight)
  (easy-menu-add-item facemenu-mouse-menu () facemenup-highlight-menu "Special Properties"))

(when (fboundp 'palette)                ; Defined in `palette.el'.
  (easy-menu-add-item facemenu-mouse-menu () "--")
  (easy-menu-add-item facemenu-mouse-menu () ["Color Palette" palette t])
  (easy-menu-add-item facemenu-mouse-menu () ["Palette - Edit Face Foreground"
                                              facemenup-palette-face-fg-at-mouse t])
  (easy-menu-add-item facemenu-mouse-menu () ["Palette - Edit Face Background"
                                              facemenup-palette-face-bg-at-mouse t]))
(easy-menu-add-item facemenu-mouse-menu () "--")
(cond ((fboundp 'palette)
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Copy Foreground (C-u: Palette)"
                            eyedrop-pick-foreground-at-mouse t])
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Copy Background (C-u: Palette)"
                            eyedrop-pick-background-at-mouse t])
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Paste to Face Foreground"
                            facemenup-paste-to-face-fg-at-mouse t])
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Paste to Face Background"
                            facemenup-paste-to-face-bg-at-mouse t]))
      (t
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Copy Foreground (C-u: Palette)"
                            eyedrop-pick-foreground-at-mouse t])
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Copy Background (C-u: Palette)"
                            eyedrop-pick-background-at-mouse t])
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Paste to Face Foreground"
                            facemenup-paste-to-face-fg-at-mouse t])
       (easy-menu-add-item facemenu-mouse-menu ()
                           ["Eyedropper - Paste to Face Background"
                            facemenup-paste-to-face-bg-at-mouse t])))
(when (fboundp 'doremi-face-fg+)        ; Defined in `doremi-frm.el'.
  (easy-menu-add-item facemenu-mouse-menu () "--")
  (easy-menu-add-item facemenu-mouse-menu () ["Do Re Mi - Edit Face Foreground"
                                              facemenup-change-fg-of-face-at-mouse+ t])
  (easy-menu-add-item facemenu-mouse-menu () ["Do Re Mi - Edit Face Background"
                                              facemenup-change-bg-of-face-at-mouse+ t])
  (easy-menu-add-item facemenu-mouse-menu () ["Do Re Mi - Undo Last Edit"
                                              doremi-undo-last-face-change t]))
(easy-menu-add-item facemenu-mouse-menu () "--")
(easy-menu-add-item facemenu-mouse-menu () ["Set Face Foreground RGB"
                                            facemenup-set-face-fg-RGB-at-mouse t])
(easy-menu-add-item facemenu-mouse-menu () ["Set Face Background RGB"
                                            facemenup-set-face-bg-RGB-at-mouse t])
(easy-menu-add-item facemenu-mouse-menu () ["Set Face Foreground RGB (Hex)"
                                            facemenup-set-face-fg-RGB-hex-at-mouse t])
(easy-menu-add-item facemenu-mouse-menu () ["Set Face Background RGB (Hex)"
                                            facemenup-set-face-bg-RGB-hex-at-mouse t])
(easy-menu-add-item facemenu-mouse-menu () ["Restore Last Foreground (Except Do Re Mi)"
                                            facemenup-face-fg-restore t])
(easy-menu-add-item facemenu-mouse-menu () ["Restore Last Background (Except Do Re Mi)"
                                            facemenup-face-bg-restore t])
(easy-menu-add-item facemenu-mouse-menu () "--")
(when (fboundp 'set-face-attribute)     ; Emacs 22
  (easy-menu-add-item facemenu-mouse-menu () ["Set Face Attribute"
                                              facemenup-set-face-attribute-at-mouse t]))
(easy-menu-add-item facemenu-mouse-menu ()
                    ["Customize Face" facemenup-customize-face-at-mouse t])

(defun facemenup-help (event)
  "Open the Emacs manual to help about formatted text."
  (interactive "e") (info "(emacs)Editing Format Info"))

(easy-menu-add-item facemenu-mouse-menu () "--")
(easy-menu-add-item facemenu-mouse-menu () ["Help" facemenup-help t])


;; Add new menu items to menu-bar menu, preceded by a separator.
(when (fboundp 'hlt-highlight)
  (easy-menu-add-item facemenu-menu () facemenup-highlight-menu "Special Properties"))

(when (fboundp 'palette)                ; Defined in `palette.el'.
  (easy-menu-add-item facemenu-menu () "--")
  (easy-menu-add-item facemenu-menu () ["Color Palette" palette t])
  (easy-menu-add-item facemenu-menu () ["Palette - Edit Face Foreground"
                                        facemenup-palette-face-fg-at-point t])
  (easy-menu-add-item facemenu-menu () ["Palette - Edit Face Background"
                                        facemenup-palette-face-bg-at-point t]))
(when (fboundp 'doremi-face-fg+)        ; Defined in `doremi-frm.el'.
  (easy-menu-add-item facemenu-menu () "--")
  (easy-menu-add-item facemenu-menu () ["Do Re Mi - Edit Face Foreground"
                                        facemenup-change-fg-of-face-at-point+ t])
  (easy-menu-add-item facemenu-menu () ["Do Re Mi - Edit Face Background"
                                        facemenup-change-bg-of-face-at-point+ t])
  (easy-menu-add-item facemenu-menu () ["Do Re Mi - Undo Last Edit"
                                        doremi-undo-last-face-change t]))
(easy-menu-add-item facemenu-menu () "--")
(easy-menu-add-item facemenu-menu () ["Eyedropper Copy Foreground Color"
                                      eyedrop-pick-foreground-at-point t])
(easy-menu-add-item facemenu-menu () ["Eyedropper Copy Background Color"
                                      eyedrop-pick-background-at-point t])
(easy-menu-add-item facemenu-menu () ["Eyedropper Paste to Face Foreground"
                                      facemenup-paste-to-face-fg-at-point t])
(easy-menu-add-item facemenu-menu () ["Eyedropper Paste to Face Background"
                                      facemenup-paste-to-face-bg-at-point t])
(easy-menu-add-item facemenu-menu () ["Set Face Foreground RGB"
                                      facemenup-set-face-fg-RGB-at-point t])
(easy-menu-add-item facemenu-menu () ["Set Face Background RGB"
                                      facemenup-set-face-bg-RGB-at-point t])
(easy-menu-add-item facemenu-menu () ["Set Face Foreground RGB (Hex)"
                                      facemenup-set-face-fg-RGB-hex-at-point t])
(easy-menu-add-item facemenu-menu () ["Set Face Background RGB (Hex)"
                                      facemenup-set-face-bg-RGB-hex-at-point t])
(easy-menu-add-item facemenu-menu () ["Restore Last Foreground (Except Do Re Mi)"
                                      facemenup-face-fg-restore t])
(easy-menu-add-item facemenu-menu () ["Restore Last Background (Except Do Re Mi)"
                                      facemenup-face-bg-restore t])
(easy-menu-add-item facemenu-menu () "--")
(when (fboundp 'set-face-attribute)     ; Emacs 22
  (easy-menu-add-item facemenu-menu () ["Set Face Attribute"
                                        facemenup-set-face-attribute-at-point t]))
(easy-menu-add-item facemenu-menu ()
                    ["Customize Face" facemenup-customize-face-at-point t])

(easy-menu-add-item facemenu-menu () "--")
(easy-menu-add-item facemenu-menu () ["Help" facemenup-help t])

;; Update the menu-bar menu.
(facemenu-update)

;;; ----------------------------------

(defun facemenup-describe-text-properties-at-mouse (event)
  "Describe text properties of character under the mouse pointer."
  (interactive "e")
  ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
  (while (input-pending-p) (discard-input))
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event)))
    (if (fboundp 'describe-text-properties)
        (describe-text-properties (point)) ; Emacs 22
      (list-text-properties-at (point))))) ; Emacs 20

;; Note: The informative messages in these commands do not appear in
;; the minibuffer if `tooltip-mode' is disabled (-1). Emacs Bug
;; reported 2006-06-25.

(defun facemenup-face-bg-restore ()
  "Restore background of last face changed by face menu to last color.
This is not an undo: It always restores the previous color as
the background of the last face changed.
This does not work for face changes made by Do Re Mi."
  (interactive)
  (unless (stringp facemenup-last-face-bg) (error "No previous face background"))
  (unless (facep facemenup-last-face-changed) (error "No face change to restore"))
  (set-face-background facemenup-last-face-changed facemenup-last-face-bg)
  (when (interactive-p)
    (message "Background of `%s' restored to `%s'"
             facemenup-last-face-changed facemenup-last-face-bg)))

(defun facemenup-face-fg-restore ()
  "Restore foreground of last face changed by face menu to last color.
This is not an undo: It always restores the previous color as
the foreground of the last face changed.
This does not work for face changes made by Do Re Mi."
  (interactive)
  (unless (stringp facemenup-last-face-fg) (error "No previous face foreground"))
  (unless (facep facemenup-last-face-changed) (error "No face change to restore"))
  (set-face-foreground facemenup-last-face-changed facemenup-last-face-fg)
  (when (interactive-p)
    (message "Foreground of `%s' restored to `%s'"
             facemenup-last-face-changed facemenup-last-face-fg)))

(when (fboundp 'palette)
  (defun facemenup-palette-face-bg-at-mouse (event)
    "Use palette to edit background of face under the mouse pointer.
To change this face: edit the color in the palette, then exit the
palette using \\<palette-mode-map>`\\[palette-exit]'.

To quit the palette without effecting any change on the face, use `\\[palette-quit]'.
To restore the face color after you have changed it, use
`\\[facemenup-face-bg-restore]'.

If option `facemenup-palette-update-while-editing-flag' is non-nil,
then the face is automatically updated each time you change the
palette's current color, which is typically when you hit `RET' or
click `mouse-2'.  In this case, quitting the palette with `\\[palette-quit]' will not
undo any face changes you made.  Use `\\[facemenup-face-bg-restore]' to restore."
    (interactive "e")
    (let ((face  (save-excursion (set-buffer (window-buffer (posn-window (event-end event))))
                                 (goto-char (posn-point (event-end event)))
                                 (eyedrop-face-at-point))))
      (unless face (setq face  (read-face-name facemenup-err-mouse)))
      (add-hook 'palette-exit-hook `(lambda () (set-face-background ',face palette-current-color)))
      (let ((bg  (facemenup-face-bg face)))
        (setq facemenup-last-face-bg       bg
              facemenup-last-face-changed  face)
        (when facemenup-palette-update-while-editing-flag
          (setq palette-action  `(lambda () (set-face-background ',face palette-current-color))))
        (palette bg)))))

(when (fboundp 'palette)
  (defun facemenup-palette-face-fg-at-mouse (event)
    "Use palette to edit foreground of face under the mouse pointer.
To change this face: edit the color in the palette, then exit the
palette using `\\[palette-exit]'.

To quit the palette without effecting any change on the face, use `\\[palette-quit]'.
To restore the face color after you have changed it, use
`\\[facemenup-face-fg-restore]'.

If option `facemenup-palette-update-while-editing-flag' is non-nil,
then the face is automatically updated each time you change the
palette's current color, which is typically when you hit `RET' or
click `mouse-2'.  In this case, quitting the palette with `\\[palette-quit]' will not
undo any face changes you made.  Use `\\[facemenup-face-fg-restore]' to restore."
    (interactive "e")
    (let ((face  (save-excursion (set-buffer (window-buffer (posn-window (event-end event))))
                                 (goto-char (posn-point (event-end event)))
                                 (eyedrop-face-at-point))))
      (unless face (setq face  (read-face-name facemenup-err-mouse)))
      (add-hook 'palette-exit-hook `(lambda () (set-face-foreground ',face palette-current-color)))
      (let ((fg  (facemenup-face-fg face)))
        (setq facemenup-last-face-fg       fg
              facemenup-last-face-changed  face)
        (when facemenup-palette-update-while-editing-flag
          (setq palette-action  `(lambda () (set-face-foreground ',face palette-current-color))))
        (condition-case nil
            (palette fg)
          (quit (set-face-foreground face fg)))))))

(when (fboundp 'palette)
  (defun facemenup-palette-face-bg-at-point ()
    "Use palette to edit background of face at the cursor (point).
To change this face: edit the color in the palette, then exit the
palette using `\\[palette-exit]'.

To quit the palette without effecting any change on the face, use `\\[palette-quit]'.
To restore the face color after you have changed it, use
`\\[facemenup-face-bg-restore]'.

If option `facemenup-palette-update-while-editing-flag' is non-nil,
then the face is automatically updated each time you change the
palette's current color, which is typically when you hit `RET' or
click `mouse-2'.  In this case, quitting the palette with `\\[palette-quit]' will not
undo any face changes you made.  Use `\\[facemenup-face-bg-restore]' to restore."
    (interactive)
    (let ((face  (eyedrop-face-at-point)))
      (unless face (read-face-name facemenup-err-point))
      (add-hook 'palette-exit-hook
                `(lambda () (set-face-background ',face palette-current-color)))
      (let ((bg  (facemenup-face-bg face)))
        (setq facemenup-last-face-bg       bg
              facemenup-last-face-changed  face)
        (when facemenup-palette-update-while-editing-flag
          (setq palette-action  `(lambda () (set-face-background ',face palette-current-color))))
        (condition-case nil
            (palette bg)
          (quit (set-face-background face bg)))))))

(when (fboundp 'palette)
  (defun facemenup-palette-face-fg-at-point ()
    "Use palette to edit foreground of face at the cursor (point).
To change this face: edit the color in the palette, then exit the
palette using `\\[palette-exit]'.

To quit the palette without effecting any change on the face, use `\\[palette-quit]'.
To restore the face color after you have changed it, use
`\\[facemenup-face-fg-restore]'.

If option `facemenup-palette-update-while-editing-flag' is non-nil,
then the face is automatically updated each time you change the
palette's current color, which is typically when you hit `RET' or
click `mouse-2'.  In this case, quitting the palette with `\\[palette-quit]' will not
undo any face changes you made.  Use `\\[facemenup-face-fg-restore]' to restore."
    (interactive)
    (let ((face  (eyedrop-face-at-point)))
      (unless face (read-face-name facemenup-err-point))
      (add-hook 'palette-exit-hook
                `(lambda () (set-face-foreground ',face palette-current-color)))
      (let ((fg  (facemenup-face-fg face)))
        (setq facemenup-last-face-fg       fg
              facemenup-last-face-changed  face)
        (when facemenup-palette-update-while-editing-flag
          (setq palette-action  `(lambda () (set-face-foreground ',face palette-current-color))))
        (condition-case nil
            (palette fg)
          (quit (set-face-foreground face fg)))))))

(when (fboundp 'doremi-face-bg+)
  (defun facemenup-change-bg-of-face-at-mouse+ (event increment)
    "Use Do Re Mi to edit background of face under the mouse pointer.
This uses command `doremi-face-bg+'; see that for more usage info.
Prefix argument is the INCREMENT of change.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument, then the face background is
first set to the value of `eyedrop-picked-background'.  This happens
only if library `eyedropper.el' or `palette.el' is loaded.  This lets
you pick up a background color from somewhere, using \"Pick Up
Background Color\" (`eyedrop-pick-background-at-mouse'), and then use
that as the initial value for
`facemenup-change-bg-of-face-at-mouse+'."
    (interactive "e\np")
    (let ((echo-keystrokes  0)
          (face             (save-excursion
                              (set-buffer (window-buffer (posn-window (event-end event))))
                              (goto-char (posn-point (event-end event)))
                              (eyedrop-face-at-point))))
      (unless face (setq face  (read-face-name facemenup-err-mouse)))
      ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
      (while (input-pending-p) (discard-input))
      (doremi-face-bg+ face
                       (read-char-exclusive (format "Change background of `%s'. \
Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: " face))
                       increment (consp current-prefix-arg)))))

(when (fboundp 'doremi-face-fg+)
  (defun facemenup-change-fg-of-face-at-mouse+ (event increment)
    "Use Do Re Mi to edit foreground of face under the mouse pointer.
This uses command `doremi-face-fg+'; see that for more usage info.
Prefix argument is the INCREMENT of change.

If `eyedrop-picked-foreground' is non-nil and you use plain `C-u'
instead of a numeric prefix argument, then the face foreground is
first set to the value of `eyedrop-picked-foreground'.  This
happens only if library `eyedropper.el' or `palette.el' is
loaded.  This lets you pick up a foreground color from somewhere,
using \"Pick Up Foreground Color\"
\(`eyedrop-pick-foreground-at-mouse'), and then use that as the
initial value for `facemenup-change-fg-of-face-at-mouse+'."
    (interactive "e\np")
    (let ((echo-keystrokes  0)
          (face             (save-excursion
                              (set-buffer (window-buffer (posn-window (event-end event))))
                              (goto-char (posn-point (event-end event)))
                              (eyedrop-face-at-point))))
      (unless face (setq face  (read-face-name facemenup-err-mouse)))
      ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
      (while (input-pending-p) (discard-input))
      (doremi-face-fg+ face
                       (read-char-exclusive (format "Change foreground of `%s'. \
Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: " face))
                       increment (consp current-prefix-arg)))))

(when (fboundp 'doremi-face-bg+)
  (defun facemenup-change-bg-of-face-at-point+ (increment)
    "Use Do Re Mi to edit background of face at the cursor (point).
This uses command `doremi-face-bg+'; see that for more usage info.
Prefix argument is the INCREMENT of change.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument, then the face background is
first set to the value of `eyedrop-picked-background'.  This
happens only if library `eyedropper.el' or `palette.el' is
loaded.  This lets you pick up a background color from somewhere,
using \"Pick Up Background Color\"
\(`eyedrop-pick-background-at-point'), and then use that as the
initial value for `facemenup-change-bg-of-face-at-point+'."
    (interactive "p")
    (let ((echo-keystrokes  0)
          (face             (eyedrop-face-at-point)))
      (unless face (read-face-name facemenup-err-point))
      (doremi-face-bg+ face (read-char-exclusive (format "Change background of `%s'. \
Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: " face))
                       increment (consp current-prefix-arg)))))

(when (fboundp 'doremi-face-fg+)
  (defun facemenup-change-fg-of-face-at-point+ (increment)
    "Use Do Re Mi to edit foreground of face at the cursor (point).
This uses command `doremi-face-fg+'; see that for more usage info.
Prefix argument is the INCREMENT of change.

If `eyedrop-picked-foreground' is non-nil and you use plain `C-u'
instead of a numeric prefix argument, then the face foreground is
first set to the value of `eyedrop-picked-foreground'.  This
happens only if library `eyedropper.el' or `palette.el' is
loaded.  This lets you pick up a foreground color from somewhere,
using \"Pick Up Foreground Color\"
\(`eyedrop-pick-foreground-at-point'), and then use that as the
initial value for `facemenup-change-fg-of-face-at-point+'."
    (interactive "p")
    (let ((echo-keystrokes  0)
          (face             (eyedrop-face-at-point)))
      (unless face (read-face-name facemenup-err-point))
      (doremi-face-fg+ face (read-char-exclusive (format "Change foreground of `%s'. \
Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: " face))
                       increment (consp current-prefix-arg)))))

(defun facemenup-set-face-bg-RGB-at-mouse (event)
  "Set RGB of background of face at character under the mouse pointer.
RGB is specified in decimal."
  (interactive "e")
  (let ((echo-keystrokes  0)
        (face             (save-excursion
                            (set-buffer (window-buffer (posn-window (event-end event))))
                            (goto-char (posn-point (event-end event)))
                            (eyedrop-face-at-point)))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
    (while (input-pending-p) (discard-input))
    (while (or (not (wholenump red)) (>= red 256))
      (setq red  (read-minibuffer "Red value (decimal): ")))
    (while (or (not (wholenump green)) (>= green 256))
      (setq green  (read-minibuffer "Green value (decimal): ")))
    (while (or (not (wholenump blue)) (>= blue 256))
      (setq blue  (read-minibuffer "Blue value (decimal): ")))
    (setq red    (format "%02x" red)
          green  (format "%02x" green)
          blue   (format "%02x" blue))
    (let ((bg  (facemenup-face-bg face)))
      (setq facemenup-last-face-bg       bg
            facemenup-last-face-changed  face))
    (set-face-background face (format "#%s%s%s" red green blue))))

(defun facemenup-set-face-fg-RGB-at-mouse (event)
  "Set RGB of foreground of face at character under the mouse pointer.
RGB is specified in decimal."
  (interactive "e")
  (let ((echo-keystrokes  0)
        (face             (save-excursion
                            (set-buffer (window-buffer (posn-window (event-end event))))
                            (goto-char (posn-point (event-end event)))
                            (eyedrop-face-at-point)))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
    (while (input-pending-p) (discard-input))
    (while (or (not (wholenump red)) (>= red 256))
      (setq red  (read-minibuffer "Red value (decimal): ")))
    (while (or (not (wholenump green)) (>= green 256))
      (setq green  (read-minibuffer "Green value (decimal): ")))
    (while (or (not (wholenump blue)) (>= blue 256))
      (setq blue  (read-minibuffer "Blue value (decimal): ")))
    (setq red    (format "%02x" red)
          green  (format "%02x" green)
          blue   (format "%02x" blue))
    (let ((fg  (facemenup-face-fg face)))
      (setq facemenup-last-face-fg       fg
            facemenup-last-face-changed  face))
    (set-face-foreground face (format "#%s%s%s" red green blue))))

(defun facemenup-set-face-bg-RGB-at-point ()
  "Set RGB of background of face at character following cursor (point).
RGB is specified in decimal, from 0 to 255."
  (interactive)
  (let ((face  (eyedrop-face-at-point))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    (while (or (not (wholenump red)) (>= red 256))
      (setq red  (read-minibuffer "Red value (decimal, 0-255): ")))
    (while (or (not (wholenump green)) (>= green 256))
      (setq green  (read-minibuffer "Green value (decimal, 0-255): ")))
    (while (or (not (wholenump blue)) (>= blue 256))
      (setq blue  (read-minibuffer "Blue value (decimal, 0-255): ")))
    (setq red    (format "%02x" red)
          green  (format "%02x" green)
          blue   (format "%02x" blue))
    (let ((bg  (facemenup-face-bg face)))
      (setq facemenup-last-face-bg       bg
            facemenup-last-face-changed  face))
    (set-face-background face (format "#%s%s%s" red green blue))))

(defun facemenup-set-face-fg-RGB-at-point ()
  "Set RGB of foreground of face at character following cursor (point).
RGB is specified in decimal, from 0 to 255."
  (interactive)
  (let ((face  (eyedrop-face-at-point))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    (while (or (not (wholenump red)) (>= red 256))
      (setq red  (read-minibuffer "Red value (decimal, 0-255): ")))
    (while (or (not (wholenump green)) (>= green 256))
      (setq green  (read-minibuffer "Green value (decimal, 0-255): ")))
    (while (or (not (wholenump blue)) (>= blue 256))
      (setq blue  (read-minibuffer "Blue value (decimal, 0-255): ")))
    (setq red    (format "%02x" red)
          green  (format "%02x" green)
          blue   (format "%02x" blue))
    (let ((fg  (facemenup-face-fg face)))
      (setq facemenup-last-face-fg       fg
            facemenup-last-face-changed  face))
    (set-face-foreground face (format "#%s%s%s" red green blue))))

(defun facemenup-set-face-bg-RGB-hex-at-mouse (event)
  "Set RGB of background of face at character under the mouse pointer.
RGB is specified in hexadecimal, from 0 to FFFF."
  (interactive "e")
  (let ((echo-keystrokes  0)
        (face             (save-excursion
                            (set-buffer (window-buffer (posn-window (event-end event))))
                            (goto-char (posn-point (event-end event)))
                            (eyedrop-face-at-point)))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
    (while (input-pending-p) (discard-input))
    (while (null (condition-case nil
                     (prog1
                         (setq red  (hexrgb-hex-to-int
                                     (read-from-minibuffer "Red value (hex, 0-FFFF): ")))
                       (when (or (< red 0) (> red 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq green  (hexrgb-hex-to-int
                                       (read-from-minibuffer "Green value (hex, 0-FFFF): ")))
                       (when (or (< green 0) (> green 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq blue  (hexrgb-hex-to-int
                                      (read-from-minibuffer "Blue value (hex, 0-FFFF): ")))
                       (when (or (< blue 0) (> blue 65535)) (error)))
                   (error nil))))
    (let ((bg  (facemenup-face-bg face)))
      (setq facemenup-last-face-bg       bg
            facemenup-last-face-changed  face))
    (set-face-background face (format "#%04x%04x%04x" red green blue))))

(defun facemenup-set-face-fg-RGB-hex-at-mouse (event)
  "Set RGB of foreground of face at character under the mouse pointer.
RGB is specified in hexadecimal, from 0 to FFFF."
  (interactive "e")
  (let ((echo-keystrokes  0)
        (face             (save-excursion
                            (set-buffer (window-buffer (posn-window (event-end event))))
                            (goto-char (posn-point (event-end event)))
                            (eyedrop-face-at-point)))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
    (while (input-pending-p) (discard-input))
    (while (null (condition-case nil
                     (prog1
                         (setq red  (hexrgb-hex-to-int
                                     (read-from-minibuffer "Red value (hex, 0-FFFF): ")))
                       (when (or (< red 0) (> red 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq green  (hexrgb-hex-to-int
                                       (read-from-minibuffer "Green value (hex, 0-FFFF): ")))
                       (when (or (< green 0) (> green 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq blue  (hexrgb-hex-to-int
                                      (read-from-minibuffer "Blue value (hex, 0-FFFF): ")))
                       (when (or (< blue 0) (> blue 65535)) (error)))
                   (error nil))))
    (let ((fg  (facemenup-face-fg face)))
      (setq facemenup-last-face-fg       fg
            facemenup-last-face-changed  face))
    (set-face-foreground face (format "#%04x%04x%04x" red green blue))))

(defun facemenup-set-face-bg-RGB-hex-at-point ()
  "Set RGB of background of face at character following cursor (point).
RGB is specified in hexadecimal, from 0 to FFFF."
  (interactive)
  (let ((face  (eyedrop-face-at-point))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    (while (null (condition-case nil
                     (prog1
                         (setq red  (hexrgb-hex-to-int
                                     (read-from-minibuffer "Red value (hex, 0-FFFF): ")))
                       (when (or (< red 0) (> red 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq green  (hexrgb-hex-to-int
                                       (read-from-minibuffer "Green value (hex, 0-FFFF): ")))
                       (when (or (< green 0) (> green 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq blue  (hexrgb-hex-to-int
                                      (read-from-minibuffer "Blue value (hex, 0-FFFF): ")))
                       (when (or (< blue 0) (> blue 65535)) (error)))
                   (error nil))))
    (let ((bg  (facemenup-face-bg face)))
      (setq facemenup-last-face-bg       bg
            facemenup-last-face-changed  face))
    (set-face-background face (format "#%04x%04x%04x" red green blue))))

(defun facemenup-set-face-fg-RGB-hex-at-point ()
  "Set RGB of foreground of face at character following cursor (point).
RGB is specified in hexadecimal, from 0 to FFFF."
  (interactive)
  (let ((face  (eyedrop-face-at-point))
        red green blue)
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    (while (null (condition-case nil
                     (prog1
                         (setq red  (hexrgb-hex-to-int
                                     (read-from-minibuffer "Red value (hex, 0-FFFF): ")))
                       (when (or (< red 0) (> red 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq green  (hexrgb-hex-to-int
                                       (read-from-minibuffer "Green value (hex, 0-FFFF): ")))
                       (when (or (< green 0) (> green 65535)) (error)))
                   (error nil))))
    (while (null (condition-case nil
                     (prog1
                         (setq blue  (hexrgb-hex-to-int
                                      (read-from-minibuffer "Blue value (hex, 0-FFFF): ")))
                       (when (or (< blue 0) (> blue 65535)) (error)))
                   (error nil))))
    (let ((fg  (facemenup-face-fg face)))
      (setq facemenup-last-face-fg       fg
            facemenup-last-face-changed  face))
    (set-face-foreground face (format "#%04x%04x%04x" red green blue))))

(defun facemenup-paste-to-face-bg-at-mouse (event)
  "Paste last color copied to background of face under mouse.
The last color copied is in `eyedrop-last-picked-color'."
  (interactive "e")
  (unless eyedrop-last-picked-color (error "Cannot paste. No color copied"))
  (let ((face  (save-excursion (set-buffer (window-buffer (posn-window (event-end event))))
                               (goto-char (posn-point (event-end event)))
                               (eyedrop-face-at-point))))
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    (let ((bg  (facemenup-face-bg face)))
      (setq facemenup-last-face-bg       bg
            facemenup-last-face-changed  face))
    (set-face-background face eyedrop-last-picked-color)))

(defun facemenup-paste-to-face-fg-at-mouse (event)
  "Paste last color copied to foreground of face under mouse.
The last color copied is in `eyedrop-last-picked-color'."
  (interactive "e")
  (unless eyedrop-last-picked-color (error "Cannot paste. No color copied"))
  (let ((face  (save-excursion (set-buffer (window-buffer (posn-window (event-end event))))
                               (goto-char (posn-point (event-end event)))
                               (eyedrop-face-at-point))))
    (unless face (setq face  (read-face-name facemenup-err-mouse)))
    (let ((fg  (facemenup-face-fg face)))
      (setq facemenup-last-face-fg       fg
            facemenup-last-face-changed  face))
    (set-face-foreground face eyedrop-last-picked-color)))

(defun facemenup-paste-to-face-bg-at-point ()
  "Paste last color copied to background of face at cursor (point).
The last color copied is in `eyedrop-last-picked-color'."
  (interactive)
  (unless eyedrop-last-picked-color (error "Cannot paste. No color copied"))
  (let ((face  (eyedrop-face-at-point)))
    (unless face (read-face-name facemenup-err-point))
    (let ((bg  (facemenup-face-bg face)))
      (setq facemenup-last-face-bg       bg
            facemenup-last-face-changed  face))
    (set-face-background face eyedrop-last-picked-color)))

(defun facemenup-paste-to-face-fg-at-point ()
  "Paste last color copied to foreground of face at cursor (point).
The last color copied is in `eyedrop-last-picked-color'."
  (interactive)
  (unless eyedrop-last-picked-color (error "Cannot paste. No color copied"))
  (let ((face  (eyedrop-face-at-point)))
    (unless face (read-face-name facemenup-err-point))
    (let ((fg  (facemenup-face-fg face)))
      (setq facemenup-last-face-fg       fg
            facemenup-last-face-changed  face))
    (set-face-foreground face eyedrop-last-picked-color)))

(when (fboundp 'set-face-attribute)     ; Emacs 22
  (defun facemenup-set-face-attribute-at-mouse (event)
    "Set attribute of face used at character under the mouse pointer.
You are prompted for the face attribute to change and its new value."
    (interactive "e")
    (let* ((face  (save-excursion
                    (set-buffer (window-buffer (posn-window (event-end event))))
                    (goto-char (posn-point (event-end event)))
                    (eyedrop-face-at-point))))
      (unless face (setq face  (read-face-name facemenup-err-mouse)))
      ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
      (while (input-pending-p) (discard-input))
      (facemenup-set-face-attribute-at--1 face)))

  (defun facemenup-set-face-attribute-at-point ()
    "Set attribute of face used at character following cursor (point).
You are prompted for the face attribute to change and its new value."
    (interactive)
    (let ((face  (eyedrop-face-at-point)))
      (unless face (read-face-name facemenup-err-point))
      (facemenup-set-face-attribute-at--1 face)))

  ;; Helper function
  (defun facemenup-set-face-attribute-at--1 (face)
    (let* ((attr   (intern (completing-read "Face attribute to change:"
                                            [:family :width :height :weight :slant :foreground
                                                     :background :inverse-video :stipple
                                                     :underline :overline :strike-through
                                                     :inherit :box :font :bold :italic]
                                            nil t nil nil ":foreground")))
           (value  (read-minibuffer (format "New value for attribute `%s': " attr))))
      (set-face-attribute face nil attr value)
      (put face 'customized-face (list (list 't (list attr value))))
      (message (substitute-command-keys
                "Use `\\[customize-face]' to revisit changes."))))

  (defun facemenup-set-face-attribute ()
    "Set attribute of face.
You are prompted for the face, attribute to change, and its new value."
    (interactive)
    (let ((face  (intern (symbol-name (read-face-name "Modify face: ")))))
      (facemenup-set-face-attribute-at--1 face))))

(defun facemenup-customize-face-at-mouse (event)
  "Customize the face used at character under the mouse pointer."
  (interactive "e")
  (let ((face  (save-excursion (set-buffer (window-buffer (posn-window (event-end event))))
                               (goto-char (posn-point (event-end event)))
                               (eyedrop-face-at-point))))
    (unless face (read-face-name facemenup-err-mouse))
    (customize-face face)))

(defun facemenup-customize-face-at-point ()
  "Customize the face used at character following cursor (point)."
    (interactive)
    (let ((face  (eyedrop-face-at-point)))
      (unless face (read-face-name facemenup-err-point))
      (customize-face face)))

(defun facemenup-face-bg (face)
  "`face-background', but get frame background if face has none.
For Emacs 22+, this is `face-background' inheriting from `default'."
  (condition-case nil
      (face-background face nil 'default) ; Emacs 22
    (error (or (face-background face) (cdr (assq 'background-color (frame-parameters)))))))

(defun facemenup-face-fg (face)
  "`face-foreground', but get frame foreground if face has none.
For Emacs 22+, this is `face-foreground' inheriting from `default'."
  (condition-case nil
      (face-foreground face nil 'default) ; Emacs 22+.  Raises error for previous versions.
    (error (or (face-foreground face) (cdr (assq 'foreground-color (frame-parameters)))))))


;; REPLACES ORIGINAL in `facemenu.el':
;; Use `icicle-read-color' if defined.  If not, use `hexrgb-read-color' if defined.
;;
;;;###autoload
(defun facemenu-read-color (&optional prompt)
  "Read a color using the minibuffer."
  (setq prompt  (or prompt "Color: "))
  (let* ((completion-ignore-case  t)
         (col                     (cond ((and (boundp 'icicle-mode) icicle-mode)
                                         (icicle-read-color 0 prompt))
                                        ((fboundp 'hexrgb-read-color)
                                         (hexrgb-read-color nil t prompt))
                                        (t
                                         (completing-read
                                          prompt
                                          (or facemenu-color-alist
                                              (if (fboundp 'defined-colors) ; Emacs 22+
                                                  (defined-colors)
                                                (and window-system
                                                     (mapcar 'list (x-defined-colors))))))))))
    (if (equal "" col) nil col)))

(when (>= emacs-major-version 22)

  ;; REPLACES ORIGINAL in `facemenu.el':
  ;; Interactively, numeric prefix arg means show face list for user to choose face by
  ;; clicking a face sample.
  ;;
  (defun facemenu-set-face (face &optional start end)
    "Apply FACE to the region or the next character typed.
With no prefix argument or a numeric prefix argument, if the region is
 active and non-empty, then apply FACE to the region.
Otherwise, apply FACE to the next character inserted.

FACE is determined as follows:
 With no prefix argument or a plain prefix argument (`C-u'), read the
   name of FACE, using completion.
 With a numeric prefix argument:
   Display the face list.  If the user clicks a face sample, then use
   that FACE.

 Call `facemenu-add-new-face', then `facemenu-add-face'.
 Add FACE to the menu of faces, if allowed by `facemenu-listed-faces'."
    (interactive
     (list (progn (barf-if-buffer-read-only)
                  (if (and current-prefix-arg (atom current-prefix-arg))
                      (let ((deactivate-mark  nil)) (list-faces-display))
                    (read-face-name "Use face")))
           (and mark-active (atom current-prefix-arg) (region-beginning))
           (and mark-active (atom current-prefix-arg) (region-end))))
    (unless (and (interactive-p) current-prefix-arg (atom current-prefix-arg))
      (facemenu-add-new-face face)
      (facemenu-add-face face start end)))


  ;; REPLACES ORIGINAL in `faces.el':
  ;; When you click the face's sample text, the face is applied to the previous buffer.
  ;;
  (defun list-faces-display (&optional regexp)
    "List all faces, using the same sample text in each.
The sample text is a string that comes from the variable
`list-faces-sample-text'.

You can click a face name to see a description of the face and
possibly customize it.

You can click a face's sample text to apply that face to the
last-accessed buffer.  If the region in that buffer is active and
non-empty, then the face is applied to it.  Otherwise, the face
is applied to any new text that is entered.

With a prefix argument, you are prompted for a regexp, and only
faces with matching names are displayed."
    (interactive (list (and current-prefix-arg
                            (if (fboundp 'icicle-read-string-completing)
                                (icicle-read-string-completing
                                 "List faces matching regexp: " nil
                                 (lambda (c) (string-match "regexp" (symbol-name c))))
                              (if (fboundp 'read-regexp) ; Emacs 23.
                                  (read-regexp "List faces matching regexp: ")
                                (read-string "List faces matching regexp: "))))))
    (let ((all-faces        (zerop (length regexp)))
          (frame            (selected-frame))
          (max-length       0)
          (deactivate-mark  nil)
          faces line-format disp-frame window face-name)
      ;; We filter and take the max length in one pass
      (setq faces  (delq nil (mapcar (lambda (f) (let ((s  (symbol-name f)))
                                              (when (or all-faces (string-match regexp s))
                                                (setq max-length  (max (length s) max-length))
                                                f)))
                                     (sort (face-list) #'string-lessp))))
      (unless faces (error "No faces matching \"%s\"" regexp))
      (setq max-length   (1+ max-length)
            line-format  (format "%%-%ds" max-length))
      (with-output-to-temp-buffer "*Faces*"
        (save-excursion
          (set-buffer standard-output)
          (setq truncate-lines  t)
          (insert (substitute-command-keys
                   (concat "Use "
                           (if (display-mouse-p) "\\[help-follow-mouse] or ")
                           "\\[help-follow]:\n"
                           " * on a face's sample text to set the region to that face, or\n"
                           " * on a face name to see a description of the face and possibly"
                           " customize it.\n\n"
                           "Face                                      Sample\n\n")))
          (setq help-xref-stack  ())
          (dolist (face  faces)
            (setq face-name  (symbol-name face))
            (insert (format line-format face-name))
            ;; Hyperlink to a help buffer for the face.
            (save-excursion
              (save-match-data
                (search-backward face-name)
                (setq help-xref-stack-item  `(list-faces-display ,regexp))
                (help-xref-button 0 'help-face face)))               
            (let ((beg       (point))
                  (line-beg  (line-beginning-position)))
              (insert list-faces-sample-text)
              ;; Button to apply the face to the active region.
              (save-excursion
                (save-match-data
                  (search-backward list-faces-sample-text)
                  (help-xref-button 0 'help-facemenu-set-face
                                    (list face (other-buffer (current-buffer) t)))))
              (insert "\n")
              (put-text-property beg (1- (point)) 'face face)
              ;; Make all face commands default to the proper face
              ;; anywhere in the line.
              (put-text-property line-beg (1- (point)) 'read-face-name face)
              ;; If the sample text has multiple lines, line up all of them.
              (goto-char beg)
              (forward-line 1)
              (while (not (eobp))
                (insert-char ?  max-length) ; ?\s won't byte-compile in Emacs 20.
                (forward-line 1))))
          (goto-char (point-min)))
        (print-help-return-message))
      ;; If the *Faces* buffer appears in a different frame,
      ;; copy all the face definitions from FRAME,
      ;; so that the display will reflect the frame that was selected.
      (setq window      (get-buffer-window (get-buffer "*Faces*") t)
            disp-frame  (if window (window-frame window) (car (frame-list))))
      (or (eq frame disp-frame)
          (let ((faces  (face-list)))
            (while faces
              (copy-face (car faces) (car faces) frame disp-frame)
              (setq faces  (cdr faces)))))))

  (define-button-type 'help-facemenu-set-face
      :supertype 'help-xref
      'help-function 'facemenup-set-face-from-list
      'help-echo (purecopy "mouse-2, RET: Set region to face"))

  (defun facemenup-set-face-from-list (face+buffer)
    "Like `facemenu-set-face', but acts in another buffer.
Argument FACE+BUFFER is a list (FACE BUFFER), where FACE is the
face to apply and BUFFER is the target buffer.
Also, close the *Faces* display."
    (let ((face    (car face+buffer))
          (buffer  (cadr face+buffer)))
      (save-excursion (set-buffer buffer)
                      (facemenu-add-new-face face)
                      (facemenu-add-face face (and mark-active (region-beginning))
                                         (and mark-active (region-end)))
                      (setq mark-active  nil)))
    (let ((win  (get-buffer-window "*Faces*"))) (when win (delete-window win))))
  

  ;; REPLACES ORIGINAL in `facemenu.el':
  ;; Add hyperlink to open palette on the color.
  ;; Use RGB format that reflects the display's color support.
  ;; 
  (defun list-colors-print (list)
    (let ((rgb-format  (facemenu-rgb-format-for-display))
          (col         (car list))
          rgb-width)
      (when (consp col) (setq col  (car col)))
      (setq rgb-width  (1+ (length (format rgb-format 1 1 1))))
      (dolist (color list)
        (if (consp color)
            (when (cdr color)
              (setq color  (sort color (lambda (a b) (string< (downcase a) (downcase b))))))
          (setq color  (list color)))
        (put-text-property (prog1 (point) (insert (car color)) (indent-to 22))
                           (point) 'face (list ':background (car color)))
        (put-text-property (prog1 (point)
                             (insert " " (if (cdr color)
                                             (mapconcat 'identity (cdr color) ", ")
                                           (car color))))
                           (point) 'face (list ':foreground (car color)))
        (indent-to (max (- (window-width) rgb-width) 44))
        (insert (apply 'format rgb-format (mapcar (lambda (c) (lsh c -8))
                                                  (color-values (car color)))))
        ;; Hyperlink to open palette on the color.
        (save-excursion
          (save-match-data
            (forward-line 0)
            (re-search-forward ".*")
            (setq help-xref-stack-item  `(list-colors-display ,list))
            (help-xref-button 0 'help-facemenu-edit-color (if (consp color) (car color) color))))
        (insert "\n")))
    (goto-char (point-min)))
  
  (defun facemenu-rgb-format-for-display ()
    (let ((ncolors  (display-color-cells (selected-frame)))
          (exp      0))
      (while (> (lsh ncolors (- exp)) 1)
        (setq exp  (1+ exp)))
      (setq exp  (/ exp 12))
      (format "#%%0%dx%%0%dx%%0%dx" exp exp exp)))

  (define-button-type 'help-facemenu-edit-color
      :supertype 'help-xref
      'help-function 'palette
      'help-echo (purecopy "mouse-2, RET: Open palette on color"))



  ;; REPLACES ORIGINAL in `facemenu.el':
  ;; Also put text property `font-lock-ignore' on the highlighted text.
  ;;
  (defun facemenu-add-face (face &optional start end)
    "Add FACE to text between START and END.
If START is nil or START to END is empty, add FACE to next typed character
instead.  For each section of that region that has a different face property,
FACE will be consed onto it, and other faces that are completely hidden by
that will be removed from the list.
If `facemenu-add-face-function' and maybe `facemenu-end-add-face' are non-nil,
they are used to set the face information.

As a special case, if FACE is `default', then the region is left with NO face
text property.  Otherwise, selecting the default face would not have any
effect.  See `facemenu-remove-face-function'."
    (interactive "*xFace: \nr")
    (if (and (eq face 'default)
             (not (eq facemenu-remove-face-function t)))
        (if facemenu-remove-face-function
            (funcall facemenu-remove-face-function start end)
          (if (and start (< start end))
              (remove-text-properties start end '(face default))
            (setq self-insert-face          'default
                  self-insert-face-command  this-command)))
      (if facemenu-add-face-function
          (save-excursion
            (if end (goto-char end))
            (save-excursion
              (if start (goto-char start))
              (insert-before-markers
               (funcall facemenu-add-face-function face end)))
            (if facemenu-end-add-face
                (insert (if (stringp facemenu-end-add-face)
                            facemenu-end-add-face
                          (funcall facemenu-end-add-face face)))))
        (if (and start (< start end))
            (let ((part-start  start)
                  part-end)
              (while (not (= part-start end))
                (setq part-end  (next-single-property-change part-start 'face nil end))
                (let ((prev  (get-text-property part-start 'face)))
                  (put-text-property part-start part-end 'face
                                     (if (null prev)
                                         face
                                       (facemenu-active-faces
                                        (cons face
                                              (if (listp prev)
                                                  prev
                                                (list prev)))
                                        ;; Specify selected frame because nil means to use the
                                        ;; new-frame default settings, and those are usually nil.
                                        (selected-frame))))
                  (put-text-property part-start part-end 'font-lock-ignore t))
                (setq part-start  part-end)))
          (setq self-insert-face  (if (eq last-command self-insert-face-command)
                                      (cons face (if (listp self-insert-face)
                                                     self-insert-face
                                                   (list self-insert-face)))
                                    face)
                self-insert-face-command this-command))))
    (unless (facemenu-enable-faces-p)
      (message "Font-lock mode will override any faces you set in this buffer"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'facemenu+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; facemenu+.el ends here

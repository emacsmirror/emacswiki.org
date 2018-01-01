;;; palette.el --- Color palette useful with RGB, HSV, and color names
;;
;; Filename: palette.el
;; Description: Color palette useful with RGB, HSV, and color names
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2006-2018, Drew Adams, all rights reserved.
;; Created: Sat May 20 07:56:06 2006
;; Version: 0
;; Package-Requires: ((hexrgb "0"))
;; Last-Updated: Mon Jan  1 15:19:41 2018 (-0800)
;;           By: dradams
;;     Update #: 923
;; URL: https://www.emacswiki.org/emacs/download/palette.el
;; Doc URL: https://emacswiki.org/emacs/ColorPalette
;; Keywords: color, rgb, hsv, hexadecimal, face, frame
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `col-highlight', `crosshairs', `frame-fns', `hexrgb',
;;   `hl-line', `hl-line+', `misc-cmds', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+', `vline'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  To use this library:
;;
;;    Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;      (require 'palette) ; Load this library.
;;      M-x palette
;;
;;    You will also need my library `hexrgb.el'; it is loaded
;;    automatically by `palette.el'.  Get it here:
;;    https://www.emacswiki.org/emacs/download/hexrgb.el.
;;
;;  After loading, use command `palette' to display a color palette in
;;  Color Palette mode (`palette-mode').  This has three sub-palettes
;;  (from left to right):
;;
;;   - a hue x saturation palette - buffer Palette (Hue x Saturation).
;;     Hue is horizontal; saturation is vertical.  Hue is the tint of
;;     a color, independent of its brightness and grayness.
;;     Saturation is the purity of a color (opposite of grayness).
;;
;;   - a color-swatch palette - buffer Current/Original
;;
;;   - a value (brightness) palette - buffer Brightness
;;
;;  The color-swatch palette shows the current color and the original
;;  color or the last color saved.  Saving is not persistent.
;;
;;  In the color palette:
;;
;;   - `.' shows info about the current color
;;   - `mouse-1' or `?' shows info about a color at pointer or cursor
;;   - `mouse-2' or `RET' anywhere picks a color as the current color
;;   - `C-?' shows colors similar to the color at pointer or cursor
;;   - Cursor motion is along the grid of colors, with wrapping.
;;     Shifted cursor motion updates the current color as you move.
;;   - `n', `C-s' saves the current color
;;   - `o', `C-o' restores the old (saved) color
;;   - `l', `u' swaps the current color and the last color (undo)
;;   - `c', `M-c' picks a color by name or RGB hex string
;;   - `M-h' picks a color by HSV components (decimal)
;;   - `M-r' picks a color by RGB components (decimal)
;;   - `~~' picks the complement of the current color
;;   - `~h', `~s', `~v' pick the hue, saturation, and value complement
;;   - `r', `g', `b', `h', `s', `v' decreases the red, green, blue,
;;     hue, saturation, value  component of the current color,
;;     respectively; `R', `G', `B', `H', `S', `V' increases the
;;     component
;;   - `C-M-h', `C-M-s', `C-M-v' set the hue, saturation, and value
;;   - `q' quits the palette
;;   - `C-l' refreshes the palette: use if you have a display problem
;;   - `C-h m' provides info on Color Palette mode
;;
;;  Some things to keep in mind when using the Color Palette:
;;
;;  * "Hue" means tint; "saturation" means color purity or intenseness
;;    (opposite of grayness); and "value" means brightness.
;;    Saturation=0 is grayscale; saturation=1 is pure color (no gray).
;;    Value=0 is black (no light); value=1 is as bright as possible
;;    (100% color).
;;
;;  * Hue 0.0 and hue 1.0 are the same: pure red.  The hue x
;;    saturation palette shows this discontinuity.  Move the cursor
;;    horizontally near the right side of this palette and you will
;;    see the hue value jump between 0.0 and 1.0 at a certain point.
;;
;;  * The value (brightness) of the current color is indicated by a
;;    horizontal bar in the Brightness palette (far right).
;;
;;  * Whenever you input a color name, you can use completion against
;;    the list of all recognized colors.  If you also use my library
;;    Icicles, then you can match any part(s) of the color name.
;;
;;  * Color names supported by your Emacs release and platform are
;;    those returned by function `x-color-names'.  This often includes
;;    names that are essentially the same, as duplicates,
;;    e.g. "LightBlue" and "light blue".  By default, the Color
;;    Palette canonicalizes these names by lowercasing them and
;;    removing whitespace.  Then it removes duplicates.  This behavior
;;    is governed by option `hexrgb-canonicalize-defined-colors-flag'.
;;    Customize that option to nil if you need the original, names.
;;
;;  * You can at any time use an RGB hexadecimal color string in place
;;    of a recognized color name.  An RGB string has the form
;;    #XXXXXXXXXXXX, where each X is a hex digit (the # is optional
;;    for input).  The number of Xs must be 12 or less and a multiple
;;    of 3, with the same number of Xs for each of red, green, and
;;    blue.  Examples: #FF0099 (red: FF, green: 00, blue: 99),
;;    #0C1FA329E (red: 0C1, green: FA3, blue: 29E).
;;
;;  * For output, that is, messages and some return values, an RGB
;;    value respects user option `palette-hex-rgb-digits', which
;;    determines the number of hex digits (1 to 4) per RGB component.
;;
;;  * Once you find a color you like, you can use its RGB string
;;    anywhere in Emacs as the color definition of a face or a frame.
;;    Its RGB string is the value of `palette-current-color'.
;;
;;  * The palette should appear in its own, small frame - on my
;;    screen, the frame is about 9cm x 13cm (inside dimensions).  If
;;    the palette appears much larger than that, or if it looks weird,
;;    then your font is probably too large.  In that case, customize
;;    option `palette-font' - see it for more information.  Here is a
;;    screenshot of how the palette should appear:
;;    https://www.emacswiki.org/emacs/ColorPalette.el.
;;
;;  * By default, information about the color at any location is only
;;    available upon demand, by clicking `mouse-1' or `mouse-2', or
;;    hitting `?' or `RET'.  If you prefer additional feedback, set
;;    option `palette-verbose-flag' to non-nil to display color
;;    information each time you move the cursor, pick a color, or
;;    modify a color swatch.  This can slow things down a bit, because
;;    it means additional computation of color components.
;;
;;  * The cursor is positioned in each of the windows so that it
;;    corresponds as well as possible to the other windows.  However,
;;    this correspondance is by no means exact, because of the nature
;;    of conversion betwen RGB and HSV color spaces.  In particular,
;;    the color in the main Palette buffer (`Hue x Saturation') does
;;    not necessarily reflect the current color accurately.  If you
;;    want information about the current color, then use `.', not `?',
;;    or use `?' from the color-swatch window.
;;
;;  * The commands that increase and decrease individual RGB
;;    components (r, g, b, R, G, B) are sometimes unintuitive.  If you
;;    set `palette-verbose-flag' to non-nil and then watch the RGB
;;    feedback in the echo area, these commands will make more sense.
;;    Because the palette displays colors as hue x saturation, RGB
;;    components are converted to the closest HSV components in the
;;    palette.  Increasing an RGB component does not automatically
;;    decrease the other RGB components, so, for instance, increasing
;;    red will not necessarily move directly toward the red area of
;;    the palette.  Just as for HSV component changes (cursor
;;    movements), RGB component changes cycle when you reach one end.
;;    For intance, when you decrease red past 0 it wraps around to 1.
;;
;;  * Non-nil `palette-update-cursor-color-flag' updates the frame
;;    foreground and cursor color dynamically, so that the position of
;;    the current color stands out well against the palette.  For
;;    example, if the current color is red then the foreground color
;;    becomes cyan.  The default value is nil.  When nil, you cannot
;;    see the black cursor against a black background.  When non-nil,
;;    there are two annoyances: 1) updating the cursor color causes
;;    redisplay of the frame, which is slow; 2) If you ask for
;;    information about a color that is very different from the
;;    current color, then it still might be difficult to see the
;;    cursor because of its color.  In that case, you can hit `RET' to
;;    make it the current color so its position stands out better.
;;    (Hit `l' to undo).
;;
;;  * You can at any time toggle options `palette-verbose-flag' and
;;    `palette-update-cursor-color-flag' with keys `f' (for
;;    "feedback") and `e' (for "enhanced cursor color").
;;
;;  * By default, feedback about a color includes its RGB hex string,
;;    RGB decimal components, and HSV decimal components.  If your
;;    minibuffer is too short for all of that info, or if you are
;;    interested in only some of it, then you can change the value of
;;    user option `palette-message-info' accordingly.  In addition,
;;    you can use commands `palette-hex-info', `palette-hsv-info',
;;    `palette-rgb-info' at any time to obtain only color information
;;    of one type.
;;
;;  * I am interested in suggestions for improving the interactive
;;    response.  You will find that the color palette is usable, but
;;    some palette operations can be slow.  This is due to using Emacs
;;    faces to display the colors: 10000 faces are used just for the
;;    100x100 color hue x saturation palette.  Emacs faces are, so
;;    far, not designed to be used this way (many at a time).  An
;;    alternative design would be to use an image instead of
;;    characters with faces.  I am not interested in such a design,
;;    however, at least for now.  I want to push the face envelope.
;;
;;  * If you call `palette' from Emacs-Lisp code, you can use various
;;    hook functions to do something with the latest color value.  See
;;    `palette-change-color-hook', `palette-exit-hook',
;;    and`palette-save-color-hook'.  This gives you a way to react to
;;    user palette actions.
;;
;;  ** Eye Dropper and `eyedropper.el' **
;;
;;  You can at any time, from any Emacs window, pick up the foreground
;;  or background color at the current cursor position (point),
;;  setting `palette-picked-background' or`palette-picked-foreground',
;;  as well as `palette-current-color', to it.  Use commands
;;  `eyedropper-foreground' and `eyedropper-background' to do this.
;;  You can then set any Emacs face or frame color using the value of
;;  `palette-current-color'.  With a prefix argument (`C-u'), these
;;  commands also display the color palette.
;;
;;  Library `palette.el' is a superset of the functionality provided
;;  by library `eyedropper.el'.  If you use Emacs 22 or later, then
;;  you can use `palette.el' instead of `eyedropper.el'; `palette.el'
;;  will satisfy all of the requirements by any other libraries that
;;  require `eyedropper.el'.  It does this via (provide 'eyedropper)
;;  and by providing aliases for all of the `eyedropper.el' functions
;;  and variables.  If for some reason you do load both libraries,
;;  then load `palette.el' after `eyedropper.el'.
;;
;;  ** Use with Crosshairs **
;;
;;  I recommend that you also use library `crosshairs.el', so that you
;;  can turn on `crosshairs-mode' (using the suggested key binding of
;;  `C-+').  That makes the location of the cursor much more visible,
;;  which really helps since it is necessarily very small for the
;;  palette.
;;
;;  ** Use with Icicles **
;;
;;  If you use this library with Icicles (`icicles.el' and associated
;;  files), which I recommend, then `c' is bound in the palette to an
;;  Icicles multi-command that lets you choose colors by name.  After
;;  you hit `c', you can hit `TAB' or `S-TAB' to use Icicles
;;  completion.  During completion, you can use `C-next', `C-prior',
;;  `C-down', `C-up', and `C-RET' to change the current color to
;;  different colors, by name, successively.  This lets you browse
;;  colors by name, seeing what they look like immediately.
;;
;;  ** Do Re Mi **
;;
;;  See also my library `doremi-frm.el' to incrementally adjust face
;;  and frame properties, including colors, using the arrow keys or a
;;  mouse wheel.  The color changes are applied instantly to the
;;  face(s) or frames, so you see the result as you make the changes.
;;
;;
;;  User options defined here:
;;
;;    `palette-change-color-hook', `palette-exit-hook',
;;    `palette-font', `palette-hex-rgb-digits',
;;    `palette-message-info', `palette-save-color-hook',
;;    `palette-update-cursor-color-flag', `palette-verbose-flag'.
;;
;;  Commands defined here:
;;
;;    `background-color', `colors', `complement',
;;    `eyedrop-background-at-mouse', `eyedrop-background-at-point',
;;    `eyedrop-foreground-at-mouse', `eyedrop-foreground-at-point',
;;    `eyedrop-pick-background-at-mouse',
;;    `eyedrop-pick-background-at-point',
;;    `eyedrop-pick-foreground-at-mouse',
;;    `eyedrop-pick-foreground-at-point', `eyedropper-background',
;;    `eyedropper-foreground', `foreground-color', `hsv', `palette',
;;    `palette-background-at-mouse', `palette-background-at-point',
;;    `palette-brightness-scale', `palette-current-color',
;;    `palette-current-rgb-to-kill-ring', `palette-decrease-blue',
;;    `palette-decrease-green', `palette-decrease-hue',
;;    `palette-decrease-red', `palette-decrease-saturation',
;;    `palette-decrease-value', `palette-down', `palette-down+pick',
;;    `palette-exit', `palette-foreground-at-mouse',
;;    `palette-foreground-at-point', `palette-help',
;;    `palette-hex-info', `palette-hsv-info', `palette-increase-blue',
;;    `palette-increase-green', `palette-increase-hue',
;;    `palette-increase-red', `palette-increase-saturation',
;;    `palette-increase-value', `palette-left', `palette-left+pick',
;;    `palette-list-colors-nearest', `palette-menu',
;;    `palette-pick-background-at-mouse',
;;    `palette-pick-background-at-point', `palette-pick-color-by-hsv',
;;    `palette-pick-color-by-name', `palette-pick-color-by-rgb',
;;    `palette-pick-color-complement',
;;    `palette-pick-color-hue-complement',
;;    `palette-pick-color-saturation-complement',
;;    `palette-pick-color-value-complement',
;;    `palette-pick-foreground-at-mouse',
;;    `palette-pick-foreground-at-point', `palette-quit',
;;    `palette-read-color', `palette-refresh',
;;    `palette-restore-old-color', `palette-rgb-info',
;;    `palette-right', `palette-right+pick', `palette-save-new-color',
;;    `palette-set-hsv', `palette-set-hue', `palette-set-saturation',
;;    `palette-set-value', `palette-swap-last-color',
;;    `palette-swatch', `palette-toggle-cursor-color',
;;    `palette-toggle-verbose', `palette-up', `palette-up+pick',
;;    `palette-where-is-color', `pick-background-color',
;;    `pick-foreground-color', `rgb', `toggle-palette-cursor-color',
;;    `toggle-palette-verbose'.
;;
;;  Non-interactive functions defined here:
;;
;;    `eyedrop-color-message', `eyedrop-face-at-point',
;;    `palette-barf-if-outside-palette', `palette-color-message',
;;    `palette-complement-or-alternative', `palette-face-at-point',
;;    `palette-pick-by-name-action', `palette-set-current-color',
;;    `palette-update-blink-cursor-mode'.
;;
;;  Internal variables defined here:
;;
;;    `eyedrop-last-picked-color', `eyedrop-picked-background',
;;    `eyedrop-picked-foreground', `palette-action',
;;    `palette-current-color', `palette-last-color',
;;    `palette-last-picked-color', `palette-menu',
;;    `palette-menu-complement', `palette-menu-set',
;;    `palette-menu-increase-decrease', `palette-mode-map',
;;    `palette-old-color', `palette-picked-background',
;;    `palette-picked-foreground', `palette-saved-blink-cursor-mode'.
;;
;;  Do NOT try to use this library without a window manager.
;;  That is, do not try to use this with `emacs -nw'.
;;
;;  Compatibility: You really need Emacs 22 for this, but reduced
;;  functionality is available for Emacs 20 and 21.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/12/24 dadams
;;     Added palette-popup-map as obsolete alias for palette-menu.
;;     Soft-require crosshairs.el.
;; 2016/12/23 dadams
;;     Added:
;;       palette-menu-complement, palette-menu-increase-decrease, palette-menu-set,
;;       palette-pick-color-hue-complement, palette-pick-color-saturation-complement,
;;       palette-pick-color-vaule-complement, palette-set-hue, palette-set-saturation,
;;       palette-set-saturation, palette-set-hsv (alias).
;;     Renamed: palette-popup-menu (cmd) and palette-popup-map (keymap) to palette-menu.
;;     palette-mode-map:
;;       Added submenus palette-menu-complement, palette-menu-increase-decrease,
;;       palette-menu-set.  Moved complement and inc/dec to those submenus.
;;     Key bindings: palette-pick-color(-*)-complement, palette-set-(hue|saturation|value).
;;     palette-toggle-cursor-color: Update frame parameters when change to enhanced also.
;; 2015/05/09 dadams
;;     palette-(foreground|background)-at-point: Add let clause for unspecified-(fg|bg).
;;     palette-list-colors-nearest: Require misc-cmds.el and raise error if not Emacs 24+.
;; 2014/10/19 dadams
;;     palette-where-is-color: Hack to handle hue=0.0.
;;     palette-where-is-color, palette-brightness-scale:
;;       Fixed typos: condition-case error handlers.
;; 2014/09/02 dadams
;;     Added: palette-list-colors-nearest.
;;     Soft-require misc-cmds.el.
;;     palette-mode-map: Bind palette-list-colors-nearest. to C-?.
;;     palette-popup-map: Added palette-list-colors-nearest.
;;     palette-mode: Mention C-? binding in doc string.
;; 2014/08/17 dadams
;;     Added: palette-read-color.  Use it everywhere instead of hexrgb-read-color.
;; 2012/03/18 dadams
;;     palette-current-rgb-to-kill-ring: Added optional MSG-P arg and message.
;; 2012/03/17 dadams
;;     Added option palette-hex-rgb-digits, palette-current-rgb-to-kill-ring.
;;     Bound palette-current-rgb-to-kill-ring to M-w in palette keymap.
;;     palette-hex-info, palette-current-color, palette-color-message:
;;       Use palette-hex-rgb-digits.
;; 2012/01/05 dadams
;;     Cleaned up some doc strings.  Added optional arg MSG-P, instead of using interactive-p.
;; 2011/11/26 dadams
;;     palette-pick-color-by-name, palette-where-is-color, palette, palette-(hex|rgb|hsv)-info:
;;       Use new arg order for hexrgb-read-color.
;; 2011/07/24 dadams
;;     Fixed calls to make-string of SPC chars.
;;     palette-pick-color-by-name-multi, palette-pick-by-name-action:
;;       Moved to icicles-cmd2.el and renamed with prefix icicle-.
;;       Removed key bindings for them.
;;     Removed soft require of icicles for icicle-define-command.
;;     define-key-after -> define-key (no AFTER arg anyway).
;;     Removed commentary note about byte-compilation.
;;     palette(-swatch|-brightness-scale): Fixed make-string escape char: \s SPC, not \s- SPC.
;; 2011/01/04 dadams
;;     Added autoload cookies (for defgroup, defcustom, and commands).
;; 2009/11/18 dadams
;;     Added: palette-saved-blink-cursor-mode, palette-update-blink-cursor-mode.
;;     palette-mode: add-hook palette-update-blink-cursor-mode.
;;     palette: Save blink-cursor-mode to palette-saved-blink-cursor-mode.
;;     palette-(exit|quit): Turn off blink-cursor-mode per palette-saved-blink-cursor-mode, and
;;                          remove-hook palette-update-blink-cursor-mode.
;; 2009/11/17 dadams
;;     palette-quit: Added optional arg.  If nil, reset palette-action, palette-exit-hook.
;;     palette-exit: Call palette-quit with arg.  Reset palette-action.
;;     palette: Call palette-quit with arg.
;; 2009/11/16 dadams
;;     Added: palette-action.
;;     palette-pick-background-at-(mouse|point): Call palette-action.  Thx to Ahei.
;; 2009/11/15 dadams
;;     palette-*-at-point: Fix for consp face value.  Fix for absent fg/bg value.
;; 2009/11/07 dadams
;;     palette-pick-color-by-name, palette-pick-color-by-name-multi, palette,
;;       palette-pick-by-name-action: Use function, not var, hexrgb-defined-colors.
;; 2009/08/04 dadams
;;     palette-mode-map: Added: palette-current-color (. and menu item).
;;     palette-background-at-point: Added Note to doc string.
;; 2009/05/24 dadams
;;     palette-brightness-scale, palette-swatch: Emacs 23 workaround:
;;       Bind split-window-preferred-function around display of buffer.
;;     palette, palette-brightness-scale, palette-swatch:
;;       Try to prevent using a tab bar if `tabbar-mode' is on generally.
;;     palette-where-is-color, palette-brightness-scale:
;;       Use forward-line instead of next-line and previous-line.
;; 2008/10/17 dadams
;;     Use (eval-after-load 'icicles, not (when (fboundp 'icicle-define-command.
;;     palette-font: Protect calls to x-list-fonts with window-system check.
;; 2007/10/11 dadams
;;     palette-(back|fore)ground-at-(mouse|point):
;;       Added optional MSG-P arg (instead of interactive-p).
;; 2007/09/07 dadams
;;     Added: palette-set-current-color, palette-(change|save)-color-hook.
;;     palette-save-new-color: Run palette-save-color-hook.  Return color.
;;     Replace (setq palette-current-color...) by (palette-set-current-color...) everywhere.
;; 2007/09/02 dadams
;;     Added: complement, palette-pick-color-complement.  Bound to `~'.
;; 2006/12/27 dadams
;;     palette: Bind 1on1-change-cursor-on-input-method-flag to nil in palette frame.
;; 2006/08/06 dadams
;;     palette: Print original color (name) in feedback message, always.
;; 2006/08/04 dadams
;;     Bug fix: defalias of foreground-color to palette-foreground-at-point (typo).
;; 2006/07/28 dadams
;;     palette-face-at-point: Use car, not caar, for (*-color . "...") test.
;; 2006/06/25 dadams
;;     Added: palette-last-picked-color.
;;     palette-pick-(back|fore)ground-at-*: Save color in palette-last-picked-color.
;; 2006/06/24 dadams
;;     Added: palette-face-at-point, palette-exit(-hook).  Bound palette-exit.
;;     palette-(back|fore)ground-at-point:
;;       Use palette-face-at-point, except in palette-mode.
;;       Use get-char-property, not get-text-property.
;;     palette: Return palette-current-color.
;; 2006/06/23 dadams
;;     Added palette-picked-*.
;;     Added eyedrop-* aliases.
;;     Added (provide 'eyedropper).
;;     palette-pick-(back|fore)ground-at-*: Set palette-picked-(back|fore)ground.
;; 2006/06/12 dadams
;;     Renamed: palette-pick-color-at-* to palette-pick-background-at-*,
;;              palette-(fg|bg)* to palette-(fore|back)ground*.
;;     Added: palette-(pick-)foreground-at-*, (pick-)(fore|back)ground-color.
;;     palette-pick-(bg|fg)-at-*:
;;       Added show-p arg.  Don't display sample unless palette window is displayed.
;; 2006/06/06 dadams
;;     Added: palette-pick-color-by-name-multi, palette-pick-by-name-action.
;;     Require icicles.el during byte-compilation.
;;     Use hexrgb-defined-colors(-alist) instead of calls to x-defined-colors.
;; 2006/06/04 dadams
;;     palette-mode: Bind truncate-lines to t.
;;     palette, palette-pick-color-by-name, : Use two non-nil args to hexrgb-read-color.
;;                                            Do not use facemenu-color-alist.
;;     palette-where-is-color: Use non-nil first arg to hexrgb-read-color.
;;     palette-brightness-scale: Cleanup.
;;     Renamed: palette-update-cursor-color to palette-update-cursor-color-flag.
;;     Added: palette-verbose-flag, palette-toggle-*, toggle-palette-*.
;;     Use palette-verbose-flag in palette-where-is-color, palette-(right|down), palette
;; 2006/06/03 dadams
;;     Added: palette-(hex|hsv|rgb|message)-info, palette-(in|de)crease-(hue|saturation|value),
;;            palette-popup-(menu|map), palette-help, palette-font.
;;     Added mouse-3 menu.
;; 2006/06/02 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

(eval-when-compile (require 'cl)) ;; case

(require 'hexrgb) ;; hexrgb-approx-equal, hexrgb-blue, hexrgb-color-name-to-hex,
                  ;; hexrgb-complement, hexrgb-defined-colors,
                  ;; hexrgb-defined-colors-alist, hexrgb-green, hexrgb-hex-to-rgb,
                  ;; hexrgb-hex-to-hsv, hexrgb-hsv-to-hex, hexrgb-hue,
                  ;; hexrgb-read-color, hexrgb-red, hexrgb-rgb-hex-to-rgb-hex,
                  ;; hexrgb-rgb-to-hex, hexrgb-rgb-to-hsv, hexrgb-saturation, hexrgb-value

(require 'misc-cmds nil t) ;; (no error if not found) list-colors-nearest
(require 'crosshairs nil t) ;; (no error if not found) Recommended, to show cursor better.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defgroup Color-Palette nil
  "A color palette: 1) hue x saturation palette and 2) brightness scale."
  :prefix "palette-" :group 'doremi :group 'frames :group 'faces
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
palette.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/palette.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/ColorPalette")
  :link '(emacs-commentary-link :tag "Commentary" "palette"))

;;;###autoload
(defcustom palette-update-cursor-color-flag nil
  "*Non-nil means dynamically update the cursor to make it stand out.
This can cause redisplay of the palette frame, which means a slowdown."
  :type 'boolean :group 'Color-Palette :group 'doremi)

;;;###autoload
(defcustom palette-verbose-flag nil
  "*Non-nil means display color info often.
Otherwise, display it only on demand.
Non-nil slows things down to recalculate color components often."
  :type 'boolean :group 'Color-Palette :group 'doremi)

;;;###autoload
(defcustom palette-hex-rgb-digits 4
  "*Number of hex RGB digits to use in output (messages, return values).
This is the number for each component (red, green, blue).
Must be a number from 1 to 4.  This value is only for output.
\(Internally, 4 is used for calculation.)"
  :type '(choice (const 1) (const 2) (const 3) (const 4)) :group 'Color-Palette)

;;;###autoload
(defcustom palette-message-info 'all
  "*Type of information to print in a palette message.
Possible values are:
 all - RGB hex, RGB decimal, and HSV decimal information
 hex - RGB hex information
 hsv - HSV decimal information
 rgb - RGB decimal information
 hex+hsv - RGB hex and HSV decimal information
 hex+rgb - RGB hex and RGB decimal information
 rgb+hsv - RGB decimal and HSV decimal information

RGB hex here means either the color name or an RGB hex color string.
The latter uses `palette-hex-rgb-digits' hex digits for each
component."
   :type '(choice
           (const :tag "RGB hex, RGB decimal, and HSV decimal information" all)
           (const :tag "RGB hex information"                               hex)
           (const :tag "HSV decimal information"                           hsv)
           (const :tag "RGB decimal information"                           rgb)
           (const :tag "RGB hex and HSV decimal information"               hex+hsv)
           (const :tag "RGB hex and RGB decimal information"               hex+rgb)
           (const :tag "RGB decimal and HSV decimal information"           rgb+hsv))
   :group 'Color-Palette :group 'doremi)

;;;###autoload
(defcustom palette-font
  (and window-system
       (or (car (x-list-fonts "-*-Courier-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))
           (car (x-list-fonts "-*-fixed-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))
           (car (x-list-fonts "-*-Terminal-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))
           (car (x-list-fonts "-*-*-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))))
  "*Font to use for the color palette.
The only characters in the font that are used are the space character
and `e'.  The only things that matter about the font are these:

 - The smaller the font, the smaller the color palette.  A size of 5
   is good, producing a frame that is about 8cm x 13 cm inside.

 - The squarer the font, the better, so that the palette is not
   unnecessarily elongated.  Short, fat fonts are good.

 - A fixed font is good, so that the brightness-indicator line, which
   is a string of `e' characters, appears the same width as the
   Brightness palette.

NOTE: Do not try to use library `palette.el' without a window manager.
      That is, do not try to use it with `emacs -nw'."
  :type 'string :group 'Color-Palette :group 'doremi)

;;;###autoload
(defcustom palette-change-color-hook nil
  "*Functions to run at the end of `palette-set-current-color'.
Typically, applications bind this hook to a function that does
something with the new value of `palette-current-color' after a color
change."
  :type 'hook :group 'Color-Palette)

;;;###autoload
(defcustom palette-exit-hook nil
  "*Functions to run at the end of `palette-exit'.
This is reset to nil after running.
These hook functions are run after `palette-quit' is run.

Typically, applications bind this hook to a function that does
something with `palette-current-color'.  When the user exits the
palette (using `\\<palette-mode-map>\\[palette-exit]), the value of `palette-current-color' \
can be used
by this hook."
  :type 'hook :group 'Color-Palette)

;;;###autoload
(defcustom palette-save-color-hook nil
  "*Functions to run at the end of `palette-save-new-color'."
  :type 'hook :group 'Color-Palette)

(defvar palette-action nil
  "Function called on the current palette color, whenever it changes.")

(defvar palette-current-color "#000000000000"
  "Current (new) color, as a hex RGB color string.
Use `\\<palette-mode-map>\\[palette-save-new-color]' to save it.")

(defvar palette-last-color "#000000000000"
  "Current color before last command, as a hex RGB color string.
Use `\\<palette-mode-map>\\[palette-swap-last-color]' to make this the current color.")

(defvar palette-old-color "#000000000000"
  "Saved color, as a hex RGB color string.
Use `\\<palette-mode-map>\\[palette-restore-old-color]' to make this the current color.")

(defvaralias 'eyedrop-picked-background 'palette-picked-background)
(defvar palette-picked-background nil
  "Color last picked from a face or frame background.
A hex RGB color string.
You can use `palette-pick-background-at-point' or
`palette-pick-background-at-mouse' to pick the color.")

(defvaralias 'eyedrop-picked-foreground 'palette-picked-foreground)
(defvar palette-picked-foreground nil
  "Color last picked from a face or frame foreground.
A hex RGB color string.
You can use `palette-pick-foreground-at-point' or
`palette-pick-foreground-at-mouse' to pick the color.")

(defvaralias 'eyedrop-last-picked-color 'palette-last-picked-color)
(defvar palette-last-picked-color nil
  "Color last picked from a face or frame foreground or background.
A hex RGB color string.")

(defvar palette-saved-blink-cursor-mode nil
  "Value of option `blink-cursor-mode' before `palette' is displayed.
Option `blink-cursor-mode' is restored (turned on or off) to reflect
this saved value when you exit or quit the palette.
The saved value is updated when `palette' is called and whenever the
user updates `blink-cursor-mode'.")

(defvar palette-menu nil "Keymap for `palette-mode' popup menu.")
(define-obsolete-variable-alias 'palette-popup-map 'palette-menu "2016-Dec-23")
(defvar palette-menu-increase-decrease nil
  "Keymap for `Increase/Decrease' submap of `palette-mode' popup menu.")
(defvar palette-menu-complement nil
  "Keymap for `Complement' submap of `palette-mode' popup menu.")
(defvar palette-menu-set nil "Keymap for `Set' submap of `palette-mode' popup menu.")

(defvar palette-mode-map nil "Keymap for `palette-mode'.")
(unless palette-mode-map
  (let ((map           (make-sparse-keymap "Color Palette"))
        (menu          (make-sparse-keymap "Color Palette Menu"))
        (menu-inc/dec  (make-sparse-keymap "Increase/Decrease Component"))
        (menu-compl    (make-sparse-keymap "Complement"))
        (menu-set      (make-sparse-keymap "Set Component")))

    (define-key map [down-mouse-1] 'ignore)
    (define-key map [drag-mouse-1] 'ignore)
    (define-key map [mouse-1]      'palette-background-at-mouse)
    (define-key map [down-mouse-2] 'ignore)
    (define-key map [drag-mouse-2] 'ignore)
    (define-key map [mouse-2]      'palette-pick-background-at-mouse)
    (define-key map [down-mouse-3] 'palette-menu)
    (define-key map [mouse-3]      'ignore)
    (define-key map "?"       'palette-background-at-point)
    (when (fboundp 'list-colors-nearest) ; Library `misc-cmds.el', Emacs 24+.
      (define-key map [(control ?\?)] 'palette-list-colors-nearest))
    (define-key map "."       'palette-current-color)
    (define-key map "~~"      'palette-pick-color-complement)
    (define-key map "~h"      'palette-pick-color-hue-complement)
    (define-key map "~s"      'palette-pick-color-saturation-complement)
    (define-key map "~v"      'palette-pick-color-value-complement)
    (define-key map "B"       'palette-increase-blue) ; B, b = blue
    (define-key map "b"       'palette-decrease-blue)
    (define-key map "c"       'palette-pick-color-by-name)
    (define-key map "e"       'palette-toggle-cursor-color) ; e = enhanced cursor color
    (define-key map "f"       'palette-toggle-verbose) ; f = frequent feedback
    (define-key map "G"       'palette-increase-green) ;G, g = green
    (define-key map "g"       'palette-decrease-green)
    (define-key map "H"       'palette-increase-hue) ; H, h = hue
    (define-key map "h"       'palette-decrease-hue)
    (define-key map "l"       'palette-swap-last-color)   ; l = last
    (define-key map "n"       'palette-save-new-color)    ; n = new
    (define-key map "o"       'palette-restore-old-color) ; o = old
    (define-key map "q"       'palette-quit)              ; q = quit
    (define-key map "R"       'palette-increase-red)      ; R, r = red
    (define-key map "r"       'palette-decrease-red)
    (define-key map "S"       'palette-increase-saturation) ; S, s = saturation
    (define-key map "s"       'palette-decrease-saturation)
    (define-key map "u"       'palette-swap-last-color) ; u = undo
    (define-key map "V"       'palette-increase-value)  ; V ,v = value
    (define-key map "v"       'palette-decrease-value)
    (define-key map "w"       'palette-where-is-color) ; w = where is it?
    (define-key map "x"       'palette-exit)           ; x = exit
    (define-key map "\C-hm"   'palette-help)
    (define-key map "\C-l"    'palette-refresh)
    (define-key map "\r"      'palette-pick-background-at-point)
    (define-key map "\C-o"    'palette-restore-old-color)  ; o = old
    (define-key map "\C-s"    'palette-save-new-color)     ; s = save
    (define-key map "\M-c"    'palette-pick-color-by-name) ; c = color
    (define-key map "\M-h"    'palette-pick-color-by-hsv)  ; h = HSV
    (define-key map "\M-r"    'palette-pick-color-by-rgb)  ; r = RGB
    (define-key map "\M-w"    'palette-current-rgb-to-kill-ring)
    (define-key map "\C-\M-h" 'palette-set-hue)
    (define-key map "\C-\M-s" 'palette-set-saturation)
    (define-key map "\C-\M-v" 'palette-set-value)
    (define-key map [(shift control f)] 'palette-right+pick)
    (define-key map [(shift right)]     'palette-right+pick)
    (define-key map [(shift control b)] 'palette-left+pick)
    (define-key map [(shift left)]      'palette-left+pick)
    (define-key map [(shift control n)] 'palette-down+pick)
    (define-key map [(shift down)]      'palette-down+pick)
    (define-key map [(shift control p)] 'palette-up+pick)
    (define-key map [(shift up)]        'palette-up+pick)

    (define-key menu [refresh]
      '(menu-item "Refresh" palette-refresh
        :help "Refresh the color palette"))
    (define-key menu [exit]
      '(menu-item "Exit (Update Action)" palette-exit
        :help "Exit the color palette with exit action, if defined."))
    (define-key menu [quit]
      '(menu-item "Quit (Cancel)" palette-quit
        :help "Quit the color palette without any exit action."))
    (define-key menu [separator-5] '(menu-item "--"))

    (define-key menu [current-rgb-to-kill-ring]
      '(menu-item "Copy Current Color as Kill" palette-current-rgb-to-kill-ring
        :help "Copy color as RGB hex string, respecting `palette-hex-rgb-digits'"))
    (define-key menu [current-color]
      '(menu-item "Current Color Info" palette-current-color
        :help "Return the current color and show info about it"))
    (define-key menu [bg-at-point]
      '(menu-item "Color at Cursor" palette-background-at-point
        :help "Return the background color under the text cursor"))
    (when (fboundp 'list-colors-nearest) ; Library `misc-cmds.el', Emacs 24+.
      (define-key menu [list-colors-nearest]
        '(menu-item "List Nearest Colors" palette-list-colors-nearest
          :help "List the colors nearest the color under the text cursor")))
    (define-key menu [separator-4] '(menu-item "--"))

    (define-key menu [pick-color-by-name]
      `(menu-item "Choose Color By Name" palette-pick-color-by-name
                  :help "Set the current color to a color you name"))
    (define-key menu [pick-color-by-hsv]
      '(menu-item "Choose Color By HSV" palette-pick-color-by-hsv
        :help "Set the current color by providing hue, saturation, and value"))
    (define-key menu [pick-color-by-rgb]
      '(menu-item "Choose Color By RGB" palette-pick-color-by-rgb
        :help "Set the current color by providing red, green, and blue components"))
    (define-key menu [separator-3] '(menu-item "--"))

    (define-key menu [swap-last-color]
      '(menu-item "Swap Last Color (Undo)" palette-swap-last-color
        :help "Swap the last color and the current color"))
    (define-key menu [save-new-color]
      '(menu-item "Save Current Color" palette-save-new-color
        :help "Save the current color as the old (original) color"))
    (define-key menu [restore-old-color]
      '(menu-item "Restore Old Color" palette-restore-old-color
        :help "Restore the old (original) color as the current color"))
    (define-key menu [toggle-verbose]
      '(menu-item "Toggle Frequent Feedback" palette-toggle-verbose
        :help "Toggle using frequent color info feedback (`palette-toggle-verbose-flag')"))
    (define-key menu [toggle-cursor-color]
      '(menu-item "Toggle Enhanced Cursor Color" palette-toggle-cursor-color
        :help "Toggle updating the cursor color so the cursor stands out \
\(`palette-update-cursor-color-flag')"))
    (define-key menu [separator-2] '(menu-item "--"))

    (define-key menu-set [set-value]
      '(menu-item "Set Value" palette-set-value
        :help "Set the value (brightness) of the current color."))
    (define-key menu-set [set-saturation]
      '(menu-item "Set Saturation" palette-set-saturation
        :help "Set the saturation of the current color."))
    (define-key menu-set [set-hue]
      '(menu-item "Set Hue" palette-set-hue :help "Set the hue of the current color."))

    (define-key menu-inc/dec [decrease-red]
      '(menu-item "  Decrease Red" palette-decrease-red
        :help "Decrease the red component of the current color by ARG/100"))
    (define-key menu-inc/dec [increase-red]
      '(menu-item "Increase Red" palette-increase-red
        :help "Increase the red component of the current color by ARG/100"))
    (define-key menu-inc/dec [decrease-green]
      '(menu-item "  Decrease Green" palette-decrease-green
        :help "Decrease the green component of the current color by ARG/100"))
    (define-key menu-inc/dec [increase-green]
      '(menu-item "Increase Green" palette-increase-green
        :help "Increase the green component of the current color by ARG/100"))
    (define-key menu-inc/dec [decrease-blue]
      '(menu-item "  Decrease Blue" palette-decrease-blue
        :help "Decrease the blue component of the current color by ARG/100"))
    (define-key menu-inc/dec [increase-blue]
      '(menu-item "Increase Blue" palette-increase-blue
        :help "Increase the blue component of the current color by ARG/100"))
    (define-key menu-inc/dec [separator-1] '(menu-item "--"))

    (define-key menu-inc/dec [decrease-hue]
      '(menu-item "  Decrease Hue" palette-decrease-hue
        :help "Decrease the hue component of the current color by ARG/100"))
    (define-key menu-inc/dec [increase-hue]
      '(menu-item "Increase Hue" palette-increase-hue
        :help "Increase the hue component of the current color by ARG/100"))
    (define-key menu-inc/dec [decrease-saturation]
      '(menu-item "  Decrease Saturation" palette-decrease-saturation
        :help "Decrease the saturation component of the current color by ARG/100"))
    (define-key menu-inc/dec [increase-saturation]
      '(menu-item "Increase Saturation" palette-increase-saturation
        :help "Increase the saturation component of the current color by ARG/100"))
    (define-key menu-inc/dec [decrease-value]
      '(menu-item "  Decrease Value" palette-decrease-value
        :help "Decrease the value component of the current color by ARG/100"))
    (define-key menu-inc/dec [increase-value]
      '(menu-item "Increase Value" palette-increase-value
        :help "Increase the value component of the current color by ARG/100"))

    (define-key menu-compl [complement-value]
      '(menu-item "Complement Value" palette-pick-color-value-complement
        :help "Set the current color to its value (brightness) complement."))
    (define-key menu-compl [complement-saturation]
      '(menu-item "Complement Saturation" palette-pick-color-saturation-complement
        :help "Set the current color to its saturation complement."))
    (define-key menu-compl [complement-hue]
      '(menu-item "Complement Hue" palette-pick-color-hue-complement
        :help "Set the current color to its hue complement."))
    (define-key menu-compl [complement]
      '(menu-item "Complement Color" palette-pick-color-complement
        :help "Set the current color to its complement."))

    (define-key menu [set-menu]
      `(menu-item "Set Component" ,menu-set :help "Set a single color component"))
    (define-key menu [inc/dec-menu]
      `(menu-item "Increase/Decrease Component" ,menu-inc/dec
                  :help "Increase or Decrease a single color component"))
    (define-key menu [complement-menu]
      `(menu-item "Complement" ,menu-compl :help "Complement a color or color component"))
    (setq palette-mode-map  map
          palette-menu      menu)))

(substitute-key-definition 'forward-char 'palette-right palette-mode-map global-map)
(substitute-key-definition 'backward-char 'palette-left palette-mode-map global-map)
(substitute-key-definition 'next-line 'palette-down palette-mode-map global-map)
(substitute-key-definition 'previous-line 'palette-up palette-mode-map global-map)

(defun palette-read-color (&optional prompt convert-to-RGB-p allow-empty-name-p msgp)
  "Read a color name or RGB hexadecimal triplet.
Optional argument PROMPT is a non-default prompt to use.

Interactively, or if CONVERT-TO-RGB-P is non-nil, return the RGB hex
string for the chosen color.  If nil, return the color name.

Optional arg ALLOW-EMPTY-NAME-P controls what happens if you enter an
empty color name (that is, you just hit `RET').  If non-nil, then
`icicle-read-color' returns an empty color name, \"\".  If nil, then
it raises an error.  Calling programs must test for \"\" if
ALLOW-EMPTY-NAME-P is non-nil.  They can then perform an appropriate
action in case of empty input.

Interactively, or with non-nil MSGP, show chosen color in echo area.

NOTE:
 If library Icicles is loaded then this is the same as command
 `icicle-read-color' - see its doc string for more information.
 Otherwise, this is the same as `hexrgb-read-color' in library
 `hexrgb.el' - see its doc for more information."
  (interactive "i\np\ni\np")    ; Always convert to RGB interactively.
  (funcall (if (fboundp 'icicle-read-color) 'icicle-read-color 'hexrgb-read-color)
           prompt convert-to-RGB-p allow-empty-name-p msgp))

(if (< emacs-major-version 22)
    ;; Emacs 20 and 21: Cannot have a nil parent mode, so use fundamental-mode.
    (define-derived-mode palette-mode fundamental-mode "Color Palette"
      "Major mode for using the color palette.
Turning on this mode runs the normal hook `palette-mode-hook'.
Use command `palette' to display a color palette in Color Palette
mode (`palette-mode').  This has three sub-palettes (from left to
right):

 - a hue x saturation palette - buffer Palette (Hue x Saturation)
   Hue is horizontal; saturation is vertical.  Hue is the tint of a
   color, independent of its brightness and grayness.  Saturation is
   the purity of a color (opposite of grayness).

 - a color-swatch palette - buffer Current/Original

 - a value (brightness) palette - buffer Brightness

The color-swatch palette shows the current color and the original
color or the last color saved.  Saving is not persistent.

In the color palette:

 - `mouse-1' or `?' anywhere shows info about a color
 - `mouse-2' or `RET' anywhere picks a color as the current color
 - `C-?' shows colors similar to the color at pointer or cursor
 - Cursor motion is along the grid of colors, with wrapping.
   Shifted cursor motion updates the current color as you move.
 - `n', `C-s' saves the current color
 - `o', `C-o' restores the old (saved) color
 - `l', `u' swaps the current color and the last color (last, undo)
 - `c', `M-c' picks a color by name or RGB hex string
 - `M-h' picks a color by HSV components (decimal)
 - `M-r' picks a color by RGB components (decimal)
 - `~' picks the complement of the current color
 - `r', `g', `b', `h', `s', `v' decreases the red, green, blue, hue,
   saturation, value  component of the current color, respectively;
   `R', `G', `B', `H', `S', `V' increases the component
 - `M-w' copies the current color (RGB hex) to the kill ring
 - `q' quits the palette
 - `C-l' refreshes the palette: use if you have a display problem
 - `C-h m' provides info on Color Palette mode

Some things to keep in mind when using the Color Palette:

 * Whenever you input a color name, you can use completion against the
   list of all recognized colors.  If you also use my library Icicles,
   then you can match any part(s) of the color name.

 * You can at any time use an RGB hexadecimal color string in place of
   a recognized color name.  An RGB string has the form #XXXXXXXXXXXX,
   where each X is a hex digit (the # is optional for input).  The
   number of Xs must be 12 or less and a multiple of 3, with the same
   number of Xs for each of red, green, and blue.  Examples:
   #FF0099 (red: FF, green: 00, blue: 99), #0C1FA329E (red: 0C1,
   green: FA3, blue: 29E).

 * For output, that is, messages and some return values, an RGB value
   respects user option `palette-hex-rgb-digits', which determines the
   number of hex digits (1 to 4) per RGB component.

 * Once you find a color you like, you can use its RGB string anywhere
   in Emacs as the color definition of a face or a frame.  Its RGB
   string is the value of `palette-current-color'.

 * Hue 0.0 and hue 1.0 are the same: pure red.  The hue x saturation
   palette shows this discontinuity.  Move the cursor horizontally
   near the right side of this palette and you will see the hue value
   jump between 0.0 and 1.0 at a certain point.

 * By default, information about the color at any location is only
   available upon demand, by clicking `mouse-1' or `mouse-2', or
   hitting `?' or `RET'.  If you prefer additional feedback, set
   option `palette-verbose-flag' to non-nil to display color
   information each time you move the cursor, pick a color, or modify
   a color swatch.  This can slow things down a bit, because it means
   additional computation of color components.

 * Non-nil `palette-update-cursor-color-flag' updates the frame
   foreground and cursor color dynamically, so that the position of
   the current color stands out well against the palette.  For
   example, if the current color is red then the foreground color
   becomes cyan.  The default value is nil.  When nil, you cannot see
   the black cursor against a black background.  When non-nil, there
   are two annoyances: 1) updating the cursor color causes redisplay
   of the frame, which is slow; 2) If you ask for information about a
   color that is very different from the current color, then it still
   might be difficult to see the cursor because of its color.  In that
   case, you can hit `RET' to make it the current color so its
   position stands out better.
   (Hit `l' to undo).

 * You can at any time toggle options `palette-verbose-flag' and
   `palette-update-cursor-color-flag' with keys `f' (for \"feedback\")
   and `e' (for \"enhanced cursor color\").

 * By default, feedback about a color includes its RGB hex string, RGB
   decimal components, and HSV decimal components.  If your minibuffer
   is too short for all of that info, or if you are interested in only
   some of it, then you can change the value of user option
   `palette-message-info' accordingly.  In addition, you can use
   commands `palette-hex-info', `palette-hsv-info', `palette-rgb-info'
   at any time to obtain only color information of one type.

 * If you enter an empty name (that is, just hit `RET') when you are
   prompted for a color name, then a name is picked randomly."
      (setq mode-line-format  nil)
      (set (make-local-variable 'auto-hscroll-mode) nil)
      (set (make-local-variable 'auto-window-vscroll) nil)
      (set (make-local-variable 'transient-mark-mode) nil)
      (set (make-local-variable 'truncate-lines) t)
      (setq show-trailing-whitespace        nil
            cursor-in-non-selected-windows  t)
      (when (fboundp 'blink-cursor-mode)
        (add-hook 'blink-cursor-mode-hook 'palette-update-blink-cursor-mode)
        (blink-cursor-mode 1)))

  ;; Emacs 22.
  (define-derived-mode palette-mode nil "Color Palette"
    "Major mode for using the color palette.
Turning on this mode runs the normal hook `palette-mode-hook'.
Use command `palette' to display a color palette in Color Palette
mode (`palette-mode').  This has three sub-palettes (from left to
right):

 - a hue x saturation palette - buffer Palette (Hue x Saturation)
   Hue is horizontal; saturation is vertical.  Hue is the tint of a
   color, independent of its brightness and grayness.  Saturation is
   the purity of a color (opposite of grayness).

 - a color-swatch palette - buffer Current/Original

 - a value (brightness) palette - buffer Brightness

The color-swatch palette shows the current color and the original
color or the last color saved.  Saving is not persistent.

In the color palette:

 - `mouse-1' or `?' anywhere shows info about a color
 - `mouse-2' or `RET' anywhere picks a color as the current color
 - `C-?' shows colors similar to the color at pointer or cursor
 - Cursor motion is along the grid of colors, with wrapping.
   Shifted cursor motion updates the current color as you move.
 - `n', `C-s' saves the current color
 - `o', `C-o' restores the old (saved) color
 - `l', `u' swaps the current color and the last color (last, undo)
 - `c', `M-c' picks a color by name or RGB hex string
 - `M-h' picks a color by HSV components (decimal)
 - `M-r' picks a color by RGB components (decimal)
 - `~' picks the complement of the current color
 - `r', `g', `b', `h', `s', `v' decreases the red, green, blue, hue,
   saturation, value  component of the current color, respectively;
   `R', `G', `B', `H', `S', `V' increases the component
 - `M-w' copies the current color (RGB hex) to the kill ring
 - `q' quits the palette
 - `C-l' refreshes the palette: use if you have a display problem
 - `C-h m' provides info on Color Palette mode

Some things to keep in mind when using the Color Palette:

 * Whenever you input a color name, you can use completion against the
   list of all recognized colors.  If you also use my library Icicles,
   then you can match any part(s) of the color name.

 * You can at any time use an RGB hexadecimal color string in place of
   a recognized color name.  An RGB string has the form #XXXXXXXXXXXX,
   where each X is a hex digit (the # is optional for input).  The
   number of Xs must be 12 or less and a multiple of 3, with the same
   number of Xs for each of red, green, and blue.  Examples:
   #FF0099 (red: FF, green: 00, blue: 99), #0C1FA329E (red: 0C1,
   green: FA3, blue: 29E).

 * For output, that is, messages and some return values, an RGB value
   respects user option `palette-hex-rgb-digits', which determines the
   number of hex digits (1 to 4) per RGB component.

 * Once you find a color you like, you can use its RGB string anywhere
   in Emacs as the color definition of a face or a frame.  Its RGB
   string is the value of `palette-current-color'.

 * Hue 0.0 and hue 1.0 are the same: pure red.  The hue x saturation
   palette shows this discontinuity.  Move the cursor horizontally
   near the right side of this palette and you will see the hue value
   jump between 0.0 and 1.0 at a certain point.

 * By default, information about the color at any location is only
   available upon demand, by clicking `mouse-1' or `mouse-2', or
   hitting `?' or `RET'.  If you prefer additional feedback, set
   option `palette-verbose-flag' to non-nil to display color
   information each time you move the cursor, pick a color, or modify
   a color swatch.  This can slow things down a bit, because it means
   additional computation of color components.

 * Non-nil `palette-update-cursor-color-flag' updates the frame
   foreground and cursor color dynamically, so that the position of
   the current color stands out well against the palette.  For
   example, if the current color is red then the foreground color
   becomes cyan.  The default value is nil.  When nil, you cannot see
   the black cursor against a black background.  When non-nil, there
   are two annoyances: 1) updating the cursor color causes redisplay
   of the frame, which is slow; 2) If you ask for information about a
   color that is very different from the current color, then it still
   might be difficult to see the cursor because of its color.  In that
   case, you can hit `RET' to make it the current color so its
   position stands out better.
   (Hit `l' to undo).

 * You can at any time toggle options `palette-verbose-flag' and
   `palette-update-cursor-color-flag' with keys `f' (for \"feedback\")
   and `e' (for \"enhanced cursor color\").

 * By default, feedback about a color includes its RGB hex string, RGB
   decimal components, and HSV decimal components.  If your minibuffer
   is too short for all of that info, or if you are interested in only
   some of it, then you can change the value of user option
   `palette-message-info' accordingly.  In addition, you can use
   commands `palette-hex-info', `palette-hsv-info', `palette-rgb-info'
   at any time to obtain only color information of one type.

 * If you enter an empty name (that is, just hit `RET') when you are
   prompted for a color name, then a name is picked randomly."
    (setq mode-line-format  nil)
    (set (make-local-variable 'auto-hscroll-mode) nil)
    (set (make-local-variable 'auto-window-vscroll) nil)
    (set (make-local-variable 'transient-mark-mode) nil)
    (set (make-local-variable 'truncate-lines) t)
    (setq show-trailing-whitespace        nil
          cursor-in-non-selected-windows  t)
    (when (fboundp 'blink-cursor-mode)
      (add-hook 'blink-cursor-mode-hook 'palette-update-blink-cursor-mode)
      (blink-cursor-mode 1))))

(defun palette-update-blink-cursor-mode ()
  "Update `palette-saved-blink-cursor-mode' from option `blink-cursor-mode'.
No update is made if we are in the palette."
  (unless (eq major-mode 'palette-mode)
    (setq palette-saved-blink-cursor-mode  blink-cursor-mode)))

;;;###autoload
(defun palette-menu (event)       ; Bound to `mouse-3'.
  "Display a popup menu of palette commands.
EVENT is a mouse event."
  (interactive "e")
  (popup-menu palette-menu))

;;;###autoload
(defun palette-help ()                  ; Bound to `C-h m'.
  "Describe Color Palette mode."
  (interactive)
  (let ((pop-up-frames  t)) (describe-mode (get-buffer "Palette (Hue x Saturation)"))))

;;;###autoload
(defun palette-hex-info (color)
  "Print the hexadecimal RGB string for COLOR.
With prefix arg, prompts for color name.
Otherwise, uses the color at the cursor."
  (interactive
   (list (if current-prefix-arg (palette-read-color nil t) (palette-background-at-point))))
  (message "RGB hex: %s" (hexrgb-rgb-hex-to-rgb-hex color palette-hex-rgb-digits)))

;;;###autoload
(defun palette-hsv-info (color)
  "Print the HSV components of COLOR.
With prefix arg, prompts for color name.
Otherwise, uses the color at the cursor."
  (interactive
   (list (if current-prefix-arg (palette-read-color nil t) (palette-background-at-point))))
  (message "HSV: %s" (hexrgb-hex-to-hsv color)))

;;;###autoload
(defun palette-rgb-info (color)
  "Print the RGB components of COLOR.
With prefix arg, prompts for color name.
Otherwise, uses the color at the cursor."
  (interactive
   (list (if current-prefix-arg (palette-read-color nil t) (palette-background-at-point))))
  (message "RGB: %s" (hexrgb-hex-to-rgb color)))

;;;###autoload
(defun palette-current-color (&optional msg-p)
  "Return the current palette color, `palette-current-color'.
The value returned respects option `palette-hex-rgb-digits'.
For the full value instead, use variable, not function,
`palette-current-color'.

Interactively, display a message with information about the color.
Non-interactively, non-nil optional arg MSG-P means show an
informative message."
  (interactive "p")
  (let ((color  (hexrgb-rgb-hex-to-rgb-hex palette-current-color palette-hex-rgb-digits)))
    (when msg-p (palette-color-message color t))
    color))

;;;###autoload
(defun palette-current-rgb-to-kill-ring (&optional msg-p) ; Bound to `M-w'.
  "Copy the RGB hex string for the current color to the kill ring.
Respect option `palette-hex-rgb-digits'."
  (interactive "p")
  (kill-new (palette-current-color))
  (when msg-p (message "Copied: %s" (palette-current-color))))

;;;###autoload
(defalias 'eyedrop-background-at-mouse 'palette-background-at-mouse)
;;;###autoload
(defun palette-background-at-mouse (event &optional msg-p) ; Bound to `mouse-1'.
  "Return the background color under the mouse pointer.
Display it in a message, respecting option `palette-hex-rgb-digits'.
Return the full value, however, ignoring `palette-hex-rgb-digits'.

EVENT is a mouse event.
Non-interactively, non-nil optional arg MSG-P means display an
informative message."
  (interactive "e\np")
  ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
  (while (input-pending-p) (discard-input))
  (set-buffer (window-buffer (posn-window (event-end event))))
  (mouse-set-point event)
  (let ((bg  (palette-background-at-point)))
    (when msg-p (if bg (palette-color-message bg t) (message "No background color here")))
    bg))

;;;###autoload
(defalias 'eyedrop-foreground-at-mouse 'palette-foreground-at-mouse)
;;;###autoload
(defun palette-foreground-at-mouse (event &optional msg-p)
  "Return the foreground color under the mouse pointer.
Display it in a message, respecting option `palette-hex-rgb-digits'.
Return the full value, however, ignoring `palette-hex-rgb-digits'.

EVENT is a mouse event.
Non-interactively, non-nil optional arg MSG-P means display an
informative message."
  (interactive "e\np")
  ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
  (while (input-pending-p) (discard-input))
  (set-buffer (window-buffer (posn-window (event-end event))))
  (mouse-set-point event)
  (let ((fg  (palette-foreground-at-point)))
    (when msg-p (if fg (palette-color-message fg t) (message "No foreground color here")))
    fg))

(defalias 'eyedrop-face-at-point 'palette-face-at-point)
(defun palette-face-at-point ()
  "Return the face under the text cursor.
If there is more than one face, return the first one.
Return nil if there is no face at point."
  (let* ((faceprop  (or (get-char-property (point) 'read-face-name)
                        (get-char-property (point) 'face)
                        'default))
         (face      (cond ((symbolp faceprop) faceprop)
                          ;; List of faces (don't treat an attribute spec).
                          ;; Just use the first face.
                          ((and (consp faceprop) (not (keywordp (car faceprop)))
                                (not (memq (car faceprop)
                                           '(foreground-color background-color))))
                           (car faceprop))
                          (t nil))))    ; Invalid face value.
    (if (facep face) face nil)))

;;;###autoload
(defalias 'background-color 'palette-background-at-point)
;;;###autoload
(defalias 'eyedrop-background-at-point 'palette-background-at-point)
;;;###autoload
(defun palette-background-at-point (&optional msg-p) ; Bound to `?'.
  "Return the background color under the text cursor.
Display it in a message, respecting option `palette-hex-rgb-digits'.
Return the full value, however, ignoring `palette-hex-rgb-digits'.

There need be no defined face at the cursor position (point).

Non-interactively, non-nil optional arg MSG-P means display an
informative message.

NOTE: The cursor is positioned in each of the windows so that it
      corresponds as well as possible to the other windows.  However,
      this correspondance is by no means exact.  In particular, the
      color in the main Palette buffer (`Hue x Saturation') does not
      necessarily reflect the current color accurately.

      If you want information about the current color, then use \
\\<palette-mode-map>`\\[palette-current-color]'
      instead.  (Or use `\\[palette-background-at-point]' from the
      color-swatch buffer (`Current/Original')."
  (interactive "p")
  ;; Outside the palette, we need to check both for a named face (via `palette-face-at-point')
  ;; and face properties that are not associated with named faces.
  ;; Inside the palette, there is no need to check for a named face.
  (let* ((face  (or (and (not (eq major-mode 'palette-mode)) (palette-face-at-point))
                    (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face)
                    'default))
         (bg    (cond ((and face (symbolp face))
                       (condition-case nil
                           (face-background face nil 'default) ; Emacs 22.
                         (error (or (face-background face) ; Emacs 20
                                    (cdr (assq 'background-color (frame-parameters)))))))
                      ((consp face)
                       (cond ((eq 'background-color (car face)) (cdr face))
                             ((and (consp (cdr face)) (memq 'background-color face))
                              (cdr (memq 'background-color face)))
                             ((and (consp (cdr face)) (memq ':background face))
                              (cadr (memq ':background face)))
                             (t (cdr (assq 'background-color (frame-parameters)))))) ; No bg.
                      (t nil)))         ; Invalid face value.
         (bg    (and (not (member bg '("unspecified-fg" "unspecified-bg")))  bg)))
    (when msg-p (if bg (palette-color-message bg t) (message "No background color here")))
    bg))

;;;###autoload
(defun palette-list-colors-nearest (color) ; Bound to `C-?'.
  "List the colors that are nearest COLOR, the current palette color.
With a prefix arg, you are prompted for COLOR."
  (interactive
   (list (if current-prefix-arg (palette-read-color nil t) (palette-background-at-point))))
  (unless (and (require 'misc-cmds nil t)
               (fboundp 'list-colors-nearest))
    (error "This command requires library `misc-cmds.el' and Emacs 24 or later"))
  (let ((pop-up-frames  t)) (list-colors-nearest color)))

;;;###autoload
(defalias 'foreground-color 'palette-foreground-at-point)
;;;###autoload
(defalias 'eyedrop-foreground-at-point 'palette-foreground-at-point)
;;;###autoload
(defun palette-foreground-at-point (&optional msg-p)
  "Return the foreground color under the text cursor.
Display it in a message, respecting option `palette-hex-rgb-digits'.
Return the full value, however, ignoring `palette-hex-rgb-digits'.

There need be no defined face at the cursor position (point).

Non-interactively, non-nil optional arg MSG-P means display an
informative message."
  (interactive "p")
  ;; Outside the palette, we need to check both for a named face (via `palette-face-at-point')
  ;; and face properties that are not associated with named faces.
  ;; Inside the palette, there is no need to check for a named face.
  (let* ((face  (or (and (not (eq major-mode 'palette-mode))
                         (palette-face-at-point))
                    (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face)
                    'default))
         (fg    (cond ((and face (symbolp face))
                       (condition-case nil
                           (face-foreground face nil 'default) ; Emacs 22.
                         (error (or (face-foreground face) ; Emacs 20
                                    (cdr (assq 'foreground-color (frame-parameters)))))))
                      ((consp face)
                       (cond ((eq 'foreground-color (car face)) (cdr face))
                             ((and (consp (cdr face)) (memq 'foreground-color face))
                              (cdr (memq 'foreground-color face)))
                             ((and (consp (cdr face)) (memq ':foreground face))
                              (cadr (memq ':foreground face)))
                             (t (cdr (assq 'foreground-color (frame-parameters)))))) ; No fg.
                      (t nil)))         ; Invalid face value.
         (fg    (and (not (member fg '("unspecified-fg" "unspecified-bg")))  fg)))
    (when msg-p (if fg (palette-color-message fg t) (message "No foreground color here")))
    fg))

;;;###autoload
(defalias 'eyedrop-pick-background-at-mouse 'palette-pick-background-at-mouse)
;;;###autoload
(defun palette-pick-background-at-mouse (event &optional show-p msg-p)
  "Set the current color to the background color under the mouse pointer.
The background color is saved in `palette-picked-background' and
`palette-last-picked-color'.  The new current color is returned.

EVENT is a mouse event.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there.
Non-interactively, non-nil optional arg MSG-P means show an
informative message."
  (interactive "e\nP\p")
  (setq palette-last-color  palette-current-color)
  (let ((win  (posn-window (event-end event)))
        (bg   (palette-background-at-mouse event)))
    (palette-set-current-color bg)
    (setq palette-picked-background  bg
          palette-last-picked-color  bg)
    (unless (stringp bg) (error "No background color here to pick"))
    (when msg-p (palette-color-message bg))
    (cond ((get-buffer-window "Palette (Hue x Saturation)" 'visible)
           (palette-brightness-scale)
           (palette-swatch))
          (show-p (palette bg)))
    (select-window win)
    ;; If we are in the palette and `palette-action' is defined, then call it.
    (when (and palette-action (eq major-mode 'palette-mode)) (funcall palette-action))
    bg))

(defun palette-set-current-color (new-value)
  "Set `palette-current-color' and run `palette-change-color-hook'.
NEW-VALUE is the new value.  It is returned.
Use this everywhere instead of (setq palette-current-color new-value)."
  (prog1 (setq palette-current-color  new-value)
    (run-hooks 'palette-change-color-hook)))

;;;###autoload
(defalias 'eyedrop-pick-foreground-at-mouse 'palette-pick-foreground-at-mouse)
;;;###autoload
(defun palette-pick-foreground-at-mouse (event &optional show-p msg-p) ; Bound to `mouse-2'.
  "Set the current color to the foreground color under the mouse pointer.
The foreground color is saved in `palette-picked-foreground' and
`palette-last-picked-color'.  The new current color is returned.

EVENT is a mouse event.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there.
Non-interactively, non-nil optional arg MSG-P means show an
informative message."
  (interactive "e\nP\p")
  (setq palette-last-color  palette-current-color)
  (let ((win  (posn-window (event-end event)))
        (fg   (palette-foreground-at-mouse event)))
    (palette-set-current-color fg)
    (setq palette-picked-foreground  fg
          palette-last-picked-color  fg)
    (unless (stringp fg) (error "No foreground color here to pick"))
    (when msg-p (palette-color-message fg))
    (cond ((get-buffer-window "Palette (Hue x Saturation)" 'visible)
           (palette-brightness-scale)
           (palette-swatch))
          (show-p (palette fg)))
    (select-window win)
    fg))

;;;###autoload
(defalias 'eyedropper-background 'palette-pick-background-at-point)
;;;###autoload
(defalias 'pick-background-color 'palette-pick-background-at-point)
;;;###autoload
(defalias 'eyedrop-pick-background-at-point 'palette-pick-background-at-point)
;;;###autoload
(defun palette-pick-background-at-point (&optional show-p msg-p) ; Bound to `RET'.
  "Set the current color to the background color under the text cursor.
The background color is saved in `palette-picked-background' and
`palette-last-picked-color'.  The (new) current color is returned.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there.
Non-interactively, non-nil optional arg MSG-P means show an
informative message."
  (interactive "P\np")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (palette-background-at-point))
    (setq palette-picked-background  palette-current-color
          palette-last-picked-color  palette-current-color)
    (unless (stringp palette-current-color) (error "No background color here to pick"))
    (when msg-p (palette-color-message palette-current-color))
    (cond ((get-buffer-window "Palette (Hue x Saturation)" 'visible)
           (palette-brightness-scale)
           (palette-swatch))
          (show-p (palette palette-current-color))))
  ;; If we are in the palette and `palette-action' is defined, then call it.
  (when (and palette-action (eq major-mode 'palette-mode)) (funcall palette-action))
  palette-current-color)

;;;###autoload
(defalias 'eyedropper-foreground 'palette-pick-foreground-at-point)
;;;###autoload
(defalias 'pick-foreground-color 'palette-pick-foreground-at-point)
;;;###autoload
(defalias 'eyedrop-pick-foreground-at-point 'palette-pick-foreground-at-point)
;;;###autoload
(defun palette-pick-foreground-at-point (&optional show-p msg-p)
  "Set the current color to the foreground color under the text cursor.
The foreground color is saved in `palette-picked-foreground' and
`palette-last-picked-color'.  The (new) current color is returned.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there.
Non-interactively, non-nil optional arg MSG-P means show an
informative message."
  (interactive "P\np")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (palette-foreground-at-point))
    (setq palette-picked-foreground  palette-current-color
          palette-last-picked-color  palette-current-color)
    (unless (stringp palette-current-color) (error "No foreground color here to pick"))
    (when msg-p (palette-color-message palette-current-color))
    (cond ((get-buffer-window "Palette (Hue x Saturation)" 'visible)
           (palette-brightness-scale)
           (palette-swatch))
          (show-p (palette palette-current-color))))
  palette-current-color)

;;;###autoload
(defun palette-pick-color-by-name (color) ; Bound to `c', `M-c'.
  "Set the current palette color to a COLOR you name.
Instead of a color name, you can use an RGB string #XXXXXXXXXXXX,
where each X is a hex digit.  The number of Xs must be a multiple of
3, with the same number of Xs for each of red, green, and blue.
If you enter an empty color name, then a color is picked randomly.
The new current color is returned."
  (interactive (list (palette-read-color nil nil t)))
  (when (string= "" color)              ; User doesn't care - why not use a random color?
    (let* ((colors  (hexrgb-defined-colors))
           (rand    (random (length colors))))
      (setq color  (elt colors rand))))
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (setq color  (hexrgb-color-name-to-hex color)) ; Needed if not interactive.
    (palette-set-current-color color)
    (palette-where-is-color color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

;;;###autoload
(defalias 'rgb 'palette-pick-color-by-rgb)
;;;###autoload
(defun palette-pick-color-by-rgb (red green blue) ; Bound to `M-r'.
  "Set the current color by providing RED, GREEN, and BLUE components.
Each component is from 0.0 to 1.0 inclusive."
  (interactive "nColor by RGB (0 to 1) - Red: \nnGreen: \nnBlue: ")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-rgb-to-hex red green blue))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

;;;###autoload
(defalias 'hsv 'palette-pick-color-by-hsv)
;;;###autoload
(defalias 'palette-set-hsv 'palette-pick-color-by-hsv)
;;;###autoload
(defun palette-pick-color-by-hsv (hue saturation value) ; Bound to `M-h'.
  "Set the current color by providing HUE, SATURATION, and VALUE.
Each component is from 0.0 to 1.0 inclusive."
  (interactive "nColor by HSV (0 to 1) - Hue: \nnSaturation: \nnValue: ")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-hsv-to-hex hue saturation value))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

;;;###autoload
(defun palette-set-hue (hue)            ; Bound to `C-M-h'.
  "Set the current color by providing the HUE (0.0 to 1.0)."
  (interactive (list (read-number "New hue (0.0 to 1.0): "
                                  (hexrgb-hue palette-current-color))))
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((sat  (hexrgb-saturation palette-current-color))
          (val  (hexrgb-value palette-current-color)))
      (palette-set-current-color (hexrgb-hsv-to-hex hue sat val))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-set-saturation (saturation) ; Bound to `C-M-s'.
  "Set the current color by providing the SATURATION (0.0 to 1.0)."
  (interactive (list (read-number "New saturation (0.0 to 1.0): "
                                  (hexrgb-saturation palette-current-color))))
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((hue  (hexrgb-hue palette-current-color))
          (val  (hexrgb-value palette-current-color)))
      (palette-set-current-color (hexrgb-hsv-to-hex hue saturation val))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-set-value (value)        ; Bound to `C-M-v'.
  "Set the current color by providing the VALUE (0.0 to 1.0)."
  (interactive (list (read-number "New value (0.0 to 1.0): "
                                  (hexrgb-value palette-current-color))))
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((hue  (hexrgb-hue palette-current-color))
          (sat  (hexrgb-saturation palette-current-color)))
      (palette-set-current-color (hexrgb-hsv-to-hex hue sat value))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defalias 'complement 'palette-pick-color-complement)
;;;###autoload
(defun palette-pick-color-complement () ; Bound to `~~'.
  "Set the current palette color to its complement."
  (interactive)
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-complement palette-current-color))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

;;;###autoload
(defun palette-pick-color-hue-complement () ; Bound to `~h'.
  "Set the current palette color to its hue complement.
Saturation and value are not changed."
  (interactive)
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-hue-complement palette-current-color))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

;;;###autoload
(defun palette-pick-color-saturation-complement () ; Bound to `~s'.
  "Set the current palette color to its saturation complement.
Hue and value are not changed."
  (interactive)
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-saturation-complement
                                palette-current-color))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

;;;###autoload
(defun palette-pick-color-value-complement () ; Bound to `~v'.
  "Set the current palette color to its value complement.
Hue and saturation are not changed."
  (interactive)
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-value-complement palette-current-color))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

;;;###autoload
(defun palette-save-new-color ()        ; Bound to `n', `C-s'.
  "Save the current color as the old (original) color.
The old color becomes the last color, so it is available by \
`\\<palette-mode-map>\\[palette-swap-last-color]'.
This command runs `palette-save-color-hook' at the end.
The saved color is returned."
  (interactive)
  (save-selected-window
    (setq palette-last-color  palette-old-color
          palette-old-color   palette-current-color)
    (palette-swatch t))
  (run-hooks 'palette-save-color-hook)
  palette-old-color)

;;;###autoload
(defun palette-swap-last-color ()       ; Bound to `l', `u'.
  "Swap the last color and the current color."
  (interactive)
  (save-selected-window
    (setq palette-last-color  (prog1 palette-current-color
                                (palette-set-current-color palette-last-color)))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch)))

;;;###autoload
(defun palette-restore-old-color ()     ; Bound to `o', `C-o'.
  "Restore the old (original) color as the current color."
  (interactive)
  (save-selected-window
    (setq palette-last-color  palette-current-color)
    (palette-set-current-color palette-old-color)
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch)))

;;;###autoload
(defun palette-refresh ()               ; Bound to `C-l'.
  "Refresh the color palette."
  (interactive)
  (save-selected-window
    (let ((win  (get-buffer-window "Current/Original" 'visible)))
      (when win (select-window win) (goto-char (point-min)) (recenter)))))

;;;###autoload
(defun palette-exit ()                  ; Bound to `x'.
  "Exit the color palette with exit action, if defined.
Call `palette-quit', then run `palette-exit-hook', then reset
`palette-action' and `palette-exit-hook'.
Turn off `blink-cursor-mode', if it was off before showing palette.
Return `palette-current-color'."
  (interactive)
  (unwind-protect
       (progn (palette-quit 'dont-reset)
              (run-hooks 'palette-exit-hook))
    (setq palette-action     nil        ; Reset.
          palette-exit-hook  nil)
    (when (and (fboundp 'blink-cursor-mode) (not palette-saved-blink-cursor-mode))
      (blink-cursor-mode -1))
    (remove-hook 'blink-cursor-mode-hook 'palette-update-blink-cursor-mode)
    palette-current-color))             ; Return latest value.

;;;###autoload
(defun palette-quit (&optional dont-reset) ; Bound to `q'.
  "Quit the color palette without any exit action.
Unlike palette-exit', this does not run `palette-exit-hook'.
Unless DONT-RESET is non-nil, reset `palette-action' and
 `palette-exit-hook'.
Turn off `blink-cursor-mode', if it was off before showing palette.
Return `palette-current-color'."
  (interactive)
  (unwind-protect
       (progn
         (let ((win  (get-buffer-window "Palette (Hue x Saturation)" 'visible)))
           (when win (select-window win) (delete-frame)))
         (when (get-buffer "Palette (Hue x Saturation)")
           (kill-buffer "Palette (Hue x Saturation)"))
         (when (get-buffer "Brightness") (kill-buffer "Brightness"))
         (when (get-buffer "Current/Original") (kill-buffer "Current/Original")))
    (unless dont-reset
      (setq palette-action     nil      ; Reset.
            palette-exit-hook  nil))
    (when (and (fboundp 'blink-cursor-mode) (not palette-saved-blink-cursor-mode))
      (blink-cursor-mode -1))
    (remove-hook 'blink-cursor-mode-hook 'palette-update-blink-cursor-mode)
    palette-current-color))             ; Return latest value.

;;;###autoload
(defun palette-where-is-color (color &optional cursor-color) ; Bound to `w'.
  "Move to the palette location of COLOR.
This does not change the current color.
Non-nil optional arg CURSOR-COLOR means update the cursor color, if
option `palette-update-cursor-color-flag' is non-nil."
  (interactive (list (palette-read-color nil t)))
  (setq color  (hexrgb-color-name-to-hex color)) ; Needed if not interactive.
  (let ((target-hue              (hexrgb-hue color))
        (target-sat              (hexrgb-saturation color))
        (hue-sat-win             (get-buffer-window "Palette (Hue x Saturation)" 'visible))
        bg hue sat)
    (when (hexrgb-approx-equal target-hue 0.0) (setq target-hue  1.0))
    (unless hue-sat-win (error "No Palette displayed - use command `palette'"))
    (select-window hue-sat-win)
    (if (< target-sat 0.049)
        (goto-char (- (point-max) 50)) ; Grayscale color (saturation=0).
      (while (and (not (eobp))
                  (setq bg  (palette-background-at-point))
                  (setq sat  (hexrgb-saturation bg))
                  (< target-sat sat))
        (condition-case nil (forward-line 1) (end-of-buffer nil)))
      (while (and (not (bobp))
                  (setq bg  (palette-background-at-point))
                  (setq sat  (hexrgb-saturation bg))
                  (> target-sat sat))
        (condition-case nil (forward-line -1) (beginning-of-buffer nil)))
      (while (and (not (eolp))
                  (setq bg  (palette-background-at-point))
                  (setq hue  (hexrgb-hue bg))
                  (progn (when (hexrgb-approx-equal hue 0.0) (setq hue  1.0)) t)
                  (< target-hue hue))
        (forward-char))
      (while (and (not (bolp))
                  (setq bg  (palette-background-at-point))
                  (setq hue  (hexrgb-hue bg))
                  (progn (when (hexrgb-approx-equal hue 0.0) (setq hue  1.0)) t)
                  (> target-hue hue))
        (backward-char))
      (when palette-update-cursor-color-flag
        (let ((col  (or cursor-color (palette-complement-or-alternative color))))
          (modify-frame-parameters (selected-frame) `(,(cons 'foreground-color col)
                                                       ,(cons 'cursor-color col)
                                                       ,(cons 'mouse-color col)))))
      (when palette-verbose-flag (palette-color-message color)))))

;;;###autoload
(defun palette-right (&optional arg)    ; Bound to `C-f'.
  "Move right ARG places, wrapping around from the left.
ARG < 0 means move left, wrapping around from the right."
  (interactive "p")
  (let* ((fwd-p  (wholenump arg))
         ;;(redisplay-dont-pause) ;; I don't really see any difference.
         (start  (point))
         (max    (save-excursion
                   (if (not fwd-p) (beginning-of-line) (end-of-line) (forward-char -1))
                   (- (point) start))))
    (setq arg  (prefix-numeric-value arg))
    (forward-char (if fwd-p (min max arg) (max max arg)))
    (when (if fwd-p (< max arg) (< arg max))
      (if fwd-p (beginning-of-line) (end-of-line) (forward-char -1))
      (forward-char (if fwd-p (- arg (1+ max)) (1+ (- arg max)))))
    (unless (or (input-pending-p) (not palette-verbose-flag))
      (let ((bg  (palette-background-at-point)))
        (if bg (palette-color-message bg t) (message "No background color here"))
        bg))))

;;;###autoload
(defun palette-left (&optional arg)    ; Bound to `C-b'.
  "Move left ARG chars, wrapping around from the right.
ARG < 0 means move right, wrapping around from the left."
  (interactive "p")
  (palette-right (- (prefix-numeric-value arg))))

;; This assumes that each line ends with a newline.
;;;###autoload
(defun palette-down (&optional arg)     ; Bound to `C-n'.
  "Move down ARG places, wrapping around from the top.
ARG < 0 means move up, wrapping around from the bottom."
  (interactive "p")
  (let* ((fwd-p   (wholenump arg))
         ;;(redisplay-dont-pause) ;; I don't really see any difference.
         (column  (current-column))
         (start   (line-beginning-position))
         (max     (save-excursion (if (not fwd-p)
                                      (goto-char (point-min))
                                    (goto-char (point-max))
                                    (forward-line -1))
                                  (beginning-of-line)
                                  (count-lines (point) start))))
    (unless fwd-p (setq max  (- max)))
    (setq arg  (prefix-numeric-value arg))
    (forward-line (if fwd-p (min max arg) (max max arg)))
    (when (if fwd-p (< max arg) (< arg max))
      (if fwd-p (goto-char (point-min)) (goto-char (point-max)) (forward-line -1))
      (forward-line (if fwd-p (- arg (1+ max)) (1+ (- arg max)))))
    (move-to-column column))
  (unless (or (input-pending-p) (not palette-verbose-flag))
    (let ((bg  (palette-background-at-point)))
      (if bg (palette-color-message bg t) (message "No background color here"))
      bg)))

;;;###autoload
(defun palette-up (&optional arg)     ; Bound to `C-p'.
  "Move up ARG chars, wrapping around from the bottom.
ARG < 0 means move down, wrapping around from the top."
  (interactive "p")
  (palette-down (- (prefix-numeric-value arg))))

;;;###autoload
(defun palette-right+pick (&optional arg) ; Bound to `S-right', `C-S-f' (`C-F').
  "`palette-right' followed by `palette-pick-background-at-point'.
The prefix ARG is passed is passed to `palette-right'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-right arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

;;;###autoload
(defun palette-left+pick (&optional arg) ; Bound to `S-left', `C-S-b' (`C-B').
  "`palette-left' followed by `palette-pick-background-at-point'.
The prefix ARG is passed is passed to `palette-left'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-left arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

;;;###autoload
(defun palette-down+pick (&optional arg) ; Bound to `S-down', `C-S-n' (`C-N').
  "`palette-down' followed by `palette-pick-background-at-point'.
The prefix ARG is passed is passed to `palette-down'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-down arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

;;;###autoload
(defun palette-up+pick (&optional arg) ; Bound to `S-up', `C-S-p' (`C-P').
  "`palette-up' followed by `palette-pick-background-at-point'.
The prefix ARG is passed is passed to `palette-up'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-up arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

;;;###autoload
(defun palette-increase-hue (&optional arg) ; Bound to `H'.
  "Increase the hue component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((hue  (+ (/ (float arg) 100.0) (hexrgb-hue palette-current-color)))
          (sat  (hexrgb-saturation palette-current-color))
          (val  (hexrgb-value palette-current-color)))
      (palette-set-current-color (hexrgb-hsv-to-hex hue sat val))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-decrease-hue (&optional arg) ; Bound to `h'.
  "Decrease the hue component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-hue (- arg)))

;;;###autoload
(defun palette-increase-saturation (&optional arg) ; Bound to `S'.
  "Increase the saturation component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((hue  (hexrgb-hue palette-current-color))
          (sat  (+ (/ (float arg) 100.0) (hexrgb-saturation palette-current-color)))
          (val  (hexrgb-value palette-current-color)))
      (palette-set-current-color (hexrgb-hsv-to-hex hue sat val))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-decrease-saturation (&optional arg) ; Bound to `s'.
  "Decrease the saturation component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-saturation (- arg)))

;;;###autoload
(defun palette-increase-value (&optional arg) ; Bound to `V'.
  "Increase the value component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((hue  (hexrgb-hue palette-current-color))
          (sat  (hexrgb-saturation palette-current-color))
          (val  (+ (/ (float arg) 100.0) (hexrgb-value palette-current-color))))
      (palette-set-current-color (hexrgb-hsv-to-hex hue sat val))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-decrease-value (&optional arg) ; Bound to `v'.
  "Decrease the value component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-value (- arg)))

;;;###autoload
(defun palette-increase-red (&optional arg) ; Bound to `R'.
  "Increase the red component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((red    (+ (/ (float arg) 100.0) (hexrgb-red palette-current-color)))
          (green  (hexrgb-green palette-current-color))
          (blue  (hexrgb-blue palette-current-color)))
      (palette-set-current-color (hexrgb-rgb-to-hex red green blue))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-decrease-red (&optional arg) ; Bound to `r'.
  "Decrease the red component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-red (- arg)))

;;;###autoload
(defun palette-increase-green (&optional arg) ; Bound to `G'.
  "Increase the green component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((red    (hexrgb-red palette-current-color))
          (green  (+ (/ (float arg) 100.0) (hexrgb-green palette-current-color)))
          (blue  (hexrgb-blue palette-current-color)))
      (palette-set-current-color (hexrgb-rgb-to-hex red green blue))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-decrease-green (&optional arg) ; Bound to `g'.
  "Decrease the green component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-green (- arg)))

;;;###autoload
(defun palette-increase-blue (&optional arg) ; Bound to `B'.
  "Increase the blue component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (let ((red    (hexrgb-red palette-current-color))
          (green  (hexrgb-green palette-current-color))
          (blue  (+ (/ (float arg) 100.0) (hexrgb-blue palette-current-color))))
      (palette-set-current-color (hexrgb-rgb-to-hex red green blue))
      (palette-where-is-color palette-current-color)
      (palette-brightness-scale)
      (palette-swatch)))
  palette-current-color)

;;;###autoload
(defun palette-decrease-blue (&optional arg) ; Bound to `b'.
  "Decrease the blue component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-blue (- arg)))

;;;###autoload
(defalias 'toggle-palette-verbose 'palette-toggle-verbose)
;;;###autoload
(defun palette-toggle-verbose ()        ; Bound to `f'.
  "Toggle using frequent color info feedback.
This toggles option `palette-toggle-verbose-flag'."
  (interactive)
  (setq palette-verbose-flag  (not palette-verbose-flag))
  (message "Verbose color feedback is now %s" (if palette-verbose-flag "ON" "OFF")))

;;;###autoload
(defalias 'toggle-palette-cursor-color 'palette-toggle-cursor-color)
;;;###autoload
(defun palette-toggle-cursor-color ()   ; Bound to `e'.
  "Toggle updating the cursor color so the cursor stands out.
This toggles option `palette-update-cursor-color-flag'."
  (interactive)
  (setq palette-update-cursor-color-flag  (not palette-update-cursor-color-flag))
  (if palette-update-cursor-color-flag
      (let ((col  (palette-complement-or-alternative palette-current-color)))
        (modify-frame-parameters (selected-frame) `(,(cons 'foreground-color col)
                                                     ,(cons 'cursor-color col)
                                                     ,(cons 'mouse-color col))))
    (modify-frame-parameters
     (selected-frame)
     '((foreground-color . "Black") (cursor-color . "Black") (mouse-color . "Black"))))
  (message "Cursor highlighting is now %s" (if palette-update-cursor-color-flag "ON" "OFF")))

;;;###autoload
(defalias 'colors 'palette)
;;;###autoload
(defun palette (&optional color)
  "Display a color-palette frame in Color Palette mode.
This includes these areas:
 - a hue x saturation palette
 - a swatch of the current color
 - a swatch of the original (old) color
 - a brightness scale
COLOR is the color used for both swatches.
If you enter an empty color name, then a color is picked randomly.
See `palette-mode' for more information."
  (interactive (list (palette-read-color nil nil t)))
  (message "Loading palette...")
  (when (string= "" color)              ; User doesn't care - why not use a random color?
    (let* ((colors  (hexrgb-defined-colors))
           (rand    (random (length colors))))
      (setq color  (elt colors rand))))
  (palette-set-current-color (hexrgb-color-name-to-hex color))
  (setq palette-old-color  palette-current-color)
  (when (and (boundp 'blink-cursor-mode) (not (eq major-mode 'palette-mode)))
    (setq palette-saved-blink-cursor-mode  blink-cursor-mode))
  (unless palette-font (error "You must define `palette-font'.  `C-h v' for more information"))
  (palette-quit 'dont-reset)
  (let* ((pop-up-frames                   t)
         (window-min-width                5)
         (fit-frame-inhibit-fitting-flag  t) ; Defined in `fit-frame.el'.
         (temp-buffer-setup-hook          nil)
         (temp-buffer-show-functions      nil)
         (width                           100)
         (height                          100)
         (stringlen                       (* width height)))
    (set-buffer (get-buffer-create "Palette (Hue x Saturation)"))
    ;; Create the palette frame.  Prevent `1on1-change-cursor-on-input-method-flag' (from
    ;; `oneonone.el') from changing the cursor color.
    (make-variable-frame-local '1on1-change-cursor-on-input-method-flag)
    (modify-frame-parameters
     (make-frame
      `((menu-bar-lines . 0) (tool-bar-lines . 0) (left-fringe . 0) (right-fringe . 0)
        (fringe . 0) (height . 100) (width . 115) (minibuffer) (vertical-scroll-bars)
        (cursor-type . box) (background-color . "Black") (mouse-color . "Black")
        (cursor-color . "Black") ,(cons 'font palette-font)))
     '((1on1-change-cursor-on-input-method-flag)))
    (with-output-to-temp-buffer "Palette (Hue x Saturation)"
      (let* ((cells  (make-string stringlen ?\   ))
             (hue    0.999999)
             (sat    1.0)
             (index  0)
             (col    "#000000000000")
             (hhh    0)
             (sss    0))
        (while (< index stringlen)
          (setq sss  0)
          (while (< sss height)
            (setq hhh  0
                  hue  1.0)
            (while (< hhh width)
              (put-text-property index (1+ index)
                                 'face (cons 'background-color
                                             (setq col  (hexrgb-hsv-to-hex hue sat 1.0)))
                                 cells)
              (put-text-property index (1+ index) 'pointer 'hand cells)
              (setq hue   (* (- hue 0.01) 0.999)
                    hhh   (1+ hhh)
                    index  (1+ index)))
            (setq sat  (* sat 0.97)
                  sss  (1+ sss))))
        (set-buffer "Palette (Hue x Saturation)")
        (setq sss    0
              index  0)
        (while (< sss height)
          (insert (substring cells index (+ index width)) ?\n)
          (setq sss    (1+ sss)
                index  (+ index width)))))
    (select-window (get-buffer-window "Palette (Hue x Saturation)" 'visible))
    ;; The next 2 lines prevent using a tab bar if `tabbar-mode' is on.
    (set-window-dedicated-p (selected-window) t)
    (setq header-line-format  nil
          window-size-fixed   t)
    (palette-mode)
    (setq buffer-read-only  t)
    (split-window (selected-window) width t)
    (palette-swatch)
    (palette-swatch t)
    (split-window (selected-window) 10 t)
    (palette-brightness-scale)
    (select-window (get-buffer-window "Palette (Hue x Saturation)" 'visible)))
  (redisplay t)                         ; Get rid of any header line from `tabbar-mode' etc.
  (palette-color-message color)         ; Orig. name.
  palette-current-color)

;;;###autoload
(defun palette-brightness-scale (&optional color msg-p)
  "Display a brightness (value) scale for COLOR.
If a color palette is already displayed, then just update it.
Non-interactively, non-nil optional arg MSG-P means show an
informative message."
  (interactive (list (palette-read-color) t))
  (setq color  (or color palette-current-color))
  (setq color  (hexrgb-color-name-to-hex color)) ; Needed if not interactive.
  (let* ((width          5)
         (height         100)
         (hue-sat-win    (get-buffer-window "Palette (Hue x Saturation)" 'visible))
         (pop-up-frames  (not hue-sat-win))
         (stringlen      (* width height))
         (target-val     (hexrgb-value color))
         (val            1.0))
    (if (and msg-p hue-sat-win)
        (palette-pick-color-by-name color)
      (let ((split-window-preferred-function  (lambda (w) (eq w (get-lru-window)))))
        (with-output-to-temp-buffer "Brightness"
          (let* ((cells  (make-string stringlen ?\   ))
                 (hue    (hexrgb-hue color))
                 (sat    (hexrgb-saturation color))
                 (index  0)
                 (col    "#FFFFFFFFFFFF")
                 (hhh    0)
                 (sss    0))
            (while (< index stringlen)
              (setq sss  0)
              (while (< sss height)
                (setq hhh  0
                      col  (hexrgb-hsv-to-hex hue sat val))
                (while (< hhh width)
                  (put-text-property index (1+ index) 'face (cons 'background-color col)
                                     cells)
                  (put-text-property index (1+ index) 'pointer 'hand cells)
                  (setq hhh    (1+ hhh)
                        index  (1+ index)))
                (setq val  (* val 0.97)
                      sss  (1+ sss))))
            (set-buffer "Brightness")
            (setq sss    0
                  index  0)
            (while (< sss height)
              (insert (substring cells index (+ index width)) ?\n)
              (setq sss    (1+ sss)
                    index  (+ index width))))))
      (select-window (get-buffer-window "Brightness" 'visible))
      ;; The next 2 lines prevent using a tab bar if `tabbar-mode' is on.
      (set-window-dedicated-p (selected-window) t)
      (setq header-line-format  nil)
      (while (and (not (eobp))
                  (setq val  (hexrgb-value (palette-background-at-point)))
                  (< target-val val))
        (condition-case nil (forward-line 1) (end-of-buffer nil)))
      (while (and (not (bobp))
                  (setq val  (hexrgb-value (palette-background-at-point)))
                  (> target-val val))
        (condition-case nil (forward-line -1) (beginning-of-buffer nil)))
      (save-excursion  ; Place horizontal line over the current value.
        (let ((buffer-read-only  nil)
              (cells             (make-string 5 ?e))
              (bg                (get-text-property (point) 'face)))
          (delete-char 5)
          (put-text-property 0 5 'face bg cells)
          (insert cells)))
      (palette-mode)
      (setq buffer-read-only  t)
      (let ((complement-color  (palette-complement-or-alternative color)))
        (cond (hue-sat-win
               (select-window hue-sat-win)
               (palette-where-is-color color complement-color))
              (t
               (modify-frame-parameters
                (selected-frame)
                `((menu-bar-lines . 0) (tool-bar-lines . 0) (cursor-type . box)
                  (left-fringe . 0) (right-fringe . 0) (fringe . 0) (minibuffer) (height . 101)
                  (vertical-scroll-bars) (background-color . "White") ,(cons 'width (1+ width))
                  ,(cons 'foreground-color complement-color)
                  ,(cons 'mouse-color complement-color)
                  ,(cons 'font palette-font) ,(cons 'cursor-color complement-color)))
               ;; Get rid of any header line from `tabbar-mode' etc.
               (when msg-p (redisplay t))))))))

;;;###autoload
(defun palette-swatch (&optional oldp color msg-p)
  "Display a color swatch.
OLDP non-nil means update the original (old) color;
     nil means update the current (new) color.
If a color palette is already displayed, then just update it.
Interactively, you are prompted for the COLOR to display.
Non-interactively, non-nil optional arg MSG-P means show an
informative message."
  (interactive (list nil (palette-read-color) t))
  (let* ((width          10)
         (height         50)
         (hue-sat-win    (get-buffer-window "Palette (Hue x Saturation)" 'visible))
         (swatch-name    "Current/Original")
         (pop-up-frames  (not hue-sat-win))
         (stringlen      (* width height)))
    (if (and msg-p hue-sat-win)
        (palette-pick-color-by-name color)
      (setq color  (or color (hexrgb-color-name-to-hex ; Needed if not interactive.
                              (if oldp palette-old-color palette-current-color))))
      (let* ((cells  (make-string stringlen ?\   ))
             (hue    (hexrgb-hue color))
             (sat    (hexrgb-saturation color))
             (val    1.0)
             (index  0)
             (col    "#FFFFFFFFFFFF")
             (hhh    0)
             (sss    0))
        (while (< index stringlen)
          (setq sss  0)
          (while (< sss height)
            (setq hhh  0)
            (while (< hhh width)
              (put-text-property index (1+ index) 'face (cons 'background-color color)
                                 cells)
              (put-text-property index (1+ index) 'pointer 'hand cells)
              (setq hhh    (1+ hhh)
                    index  (1+ index)))
            (setq sss  (1+ sss))))
        (set-buffer (get-buffer-create swatch-name))
        (if oldp
            (goto-char (+ stringlen height 1))
          (unless (= (point-min) (point-max))
            (delete-region (point-min) (+ stringlen height 1))
            (goto-char (point-min))))
        (setq sss    0
              index  0)
        (while (< sss height)
          (insert (substring cells index (+ index width)) ?\n)
          (setq sss    (1+ sss)
                index  (+ index width))))
      (let ((split-window-preferred-function  (lambda (w) (eq w (get-lru-window)))))
        (display-buffer swatch-name))
      (select-window (get-buffer-window swatch-name 'visible))
      ;; The next 2 lines prevent using a tab bar if `tabbar-mode' is on.
      (set-window-dedicated-p (selected-window) t)
      (setq header-line-format  nil)
      (palette-mode)
      (goto-char (point-min))
      (unless hue-sat-win
        (let ((complement-color  (palette-complement-or-alternative color)))
          (modify-frame-parameters
           (selected-frame)
           `((menu-bar-lines . 0) (tool-bar-lines . 0) (left-fringe . 0) (right-fringe . 0)
             (fringe . 0) (minibuffer) (vertical-scroll-bars) (background-color . "White")
             (cursor-type . box) ,(cons 'foreground-color complement-color)
             ,(cons 'mouse-color complement-color) ,(cons 'height (1+ height))
             ,(cons 'width (1+ width)) ,(cons 'cursor-color complement-color)
             ,(cons 'font palette-font)))
          ;; Get rid of any header line from `tabbar-mode' etc.
          (when msg-p (redisplay t)))))))

(defun palette-complement-or-alternative (color &optional alternative)
  "Return complement of COLOR or ALTERNATIVE (default Red).
Return ALTERNATIVE if value component of COLOR is approximately its
own complement or its saturation is less than 0.2.  The default
ALTERNATIVE color is Red or Cyan, depending on the current hue."
  (let ((hue  (hexrgb-hue color)))
    (setq alternative  (or alternative (if (or (hexrgb-approx-equal hue 1.0 0.2)
                                               (hexrgb-approx-equal hue 0.0 0.0 0.1))
                                           "Cyan"
                                         "Red")))
    (let ((complement  (hexrgb-complement color)))
      (if (or (hexrgb-approx-equal (hexrgb-value complement) (hexrgb-value color) 0.4)
              (< (hexrgb-saturation color) 0.2))
          alternative
        complement))))

(defalias 'eyedrop-color-message 'palette-color-message)
(defun palette-color-message (color &optional hint-p)
  "Print information about COLOR, according to `palette-message-info'.
If an RGB hex pattern is shown then it respects option
`palette-hex-rgb-digits'.

Return COLOR as a color name or a full RGB hex pattern (ignoring
`palette-hex-rgb-digits').

Non-nil HINT-P means provide a hint about how to pick the color."
  (let* ((rgb  (hexrgb-hex-to-rgb color))
         (hex  (if (eq ?# (aref color 0))
                   (hexrgb-rgb-hex-to-rgb-hex color palette-hex-rgb-digits)
                 color))
         (hsv  (apply #'hexrgb-rgb-to-hsv rgb))
         (msg  (case palette-message-info
                 (all      (format "Color: %s, RGB: %s, HSV: %s" hex rgb hsv))
                 (hex      (format "Color: %s" hex))
                 (hsv      (format "HSV: %s" hsv))
                 (rgb      (format "RGB: %s" rgb))
                 (hex+hsv  (format "Color: %s, HSV: %s" hex hsv))
                 (hex+rgb  (format "Color: %s, RGB: %s" hex rgb))
                 (rgb+hsv  (format "RGB: %s, HSV: %s" rgb hsv)))))
    (if hint-p (message "%s  (pick: mouse-2, RET)" msg) (message "%s" msg)))
  color)                                ; Return it.

;; Not needed, so far.
(defun palette-barf-if-outside-palette ()
  "Raise an error if `this-command' is called outside the palette."
  (unless (eq major-mode 'palette-mode)
    (error "Command `%s' must be called from the minibuffer" this-command)))

;;;;;;;;;;;;;;;;

(provide 'eyedropper) ;; Satisfy (require 'eyedropper).
(provide 'palette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; palette.el ends here

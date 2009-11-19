;;; palette.el --- Color palette useful with RGB, HSV, and color names
;;
;; Filename: palette.el
;; Description: Color palette useful with RGB, HSV, and color names
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2009, Drew Adams, all rights reserved.
;; Created: Sat May 20 07:56:06 2006
;; Version: 22.0
;; Last-Updated: Wed Nov 18 17:01:08 2009 (-0800)
;;           By: dradams
;;     Update #: 512 4
;; URL: http://www.emacswiki.org/cgi-bin/wiki/palette.el
;; Keywords: color, rgb, hsv, hexadecimal, face, frame
;; Compatibility: GNU Emacs: 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `hexrgb'.
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
;;    http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el.
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
;;   - Cursor motion is along the grid of colors, with wrapping.
;;     Shifted cursor motion updates the current color as you move.
;;   - `n', `C-s' saves the current color
;;   - `o', `C-o' restores the old (saved) color
;;   - `l', `u' swaps the current color and the last color (undo)
;;   - `c', `M-c' picks a color by name or RGB hex string
;;   - `M-h' picks a color by HSV components (decimal)
;;   - `M-r' picks a color by RGB components (decimal)
;;   - `~' picks the complement of the current color
;;   - `r', `g', `b', `h', `s', `v' decreases the red, green, blue,
;;     hue, saturation, value  component of the current color,
;;     respectively; `R', `G', `B', `H', `S', `V' increases the
;;     component
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
;;    for input).  The number of Xs must be a multiple of 3, with the
;;    same number of Xs for each of red, green, and blue.  Examples:
;;    #FF0099 (red: FF, green: 00, blue: 99), #0C1FA329E (red: 0C1,
;;    green: FA3, blue: 29E).
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
;;    http://www.emacswiki.org/cgi-bin/wiki/ColorPalette.el.
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
;;    `palette-font', `palette-message-info',
;;    `palette-save-color-hook', `palette-update-cursor-color-flag',
;;    `palette-verbose-flag'.
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
;;    `palette-decrease-blue', `palette-decrease-green',
;;    `palette-decrease-hue', `palette-decrease-red',
;;    `palette-decrease-saturation', `palette-decrease-value',
;;    `palette-down', `palette-down+pick', `palette-exit',
;;    `palette-foreground-at-mouse', `palette-foreground-at-point',
;;    `palette-help', `palette-hex-info', `palette-hsv-info',
;;    `palette-increase-blue', `palette-increase-green',
;;    `palette-increase-hue', `palette-increase-red',
;;    `palette-increase-saturation', `palette-increase-value',
;;    `palette-left', `palette-left+pick',
;;    `palette-pick-background-at-mouse',
;;    `palette-pick-background-at-point', `palette-pick-color-by-hsv',
;;    `palette-pick-color-by-name',
;;    `palette-pick-color-by-name-multi', `palette-pick-color-by-rgb',
;;    `palette-pick-color-complement',
;;    `palette-pick-foreground-at-mouse',
;;    `palette-pick-foreground-at-point', `palette-popup-menu',
;;    `palette-quit', `palette-refresh', `palette-restore-old-color',
;;    `palette-rgb-info', `palette-right', `palette-right+pick',
;;    `palette-save-new-color', `palette-swap-last-color',
;;    `palette-swatch', `palette-toggle-cursor-color',
;;    `palette-toggle-verbose', `palette-up', `palette-up+pick',
;;    `palette-where-is-color', `pick-background-color',
;;    `pick-foreground-color', `rgb', `toggle-palette-cursor-color',
;;    `toggle-palette-verbose', .
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
;;    `palette-last-picked-color', `palette-mode-map',
;;    `palette-old-color', `palette-picked-background',
;;    `palette-picked-foreground', `palette-popup-map',
;;    `palette-saved-blink-cursor-mode'.
;;
;;  Do NOT try to use this library without a window manager.
;;  That is, do not try to use this with `emacs -nw'.
;;
;;  Compatibility: You really need Emacs 22 for this, but reduced
;;  functionality is available for Emacs 20 and 21.
;;
;;  Byte-compilation:
;;
;;  If you byte-compile `palette.el' without Icicles (`icicles.el')
;;  loaded, then you will likely get warnings such as the following.
;;  These warnings are all benign.
;;
;;    palette.el:861:1:Warning: `(completion-ignore-case t)' is a
;;      malformed function
;;    palette.el:863:26:Warning: reference to free variable
;;      `palette-pick-color-by-name-multi'
;;    palette.el:870:26:Warning: reference to free variable
;;      `palette-pick-by-name-action'
;;
;;    In end of data:
;;    palette.el:1475:1:Warning: the function `palette-mode' is not
;;      known to be defined.
;;
;;  You can byte-compile `palette.el' with Icicles loaded, and then
;;  use `palette.elc' in Emacs with or without Icicles.  If Icicles is
;;  loaded at runtime, then `c' is bound to an Icicles multi-command;
;;  otherwise, it is bound to a simple command.
;;
;;  To be able to use this library with Icicles, you must compile it
;;  with Icicles loaded.  Otherwise, you will not be able to load the
;;  byte-compiled file into an Emacs session that has loaded Icicles.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
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

(eval-when-compile (require 'icicles nil t)) ;; icicle-define-command

(require 'hexrgb) ;; hexrgb-approx-equal, hexrgb-blue, hexrgb-color-name-to-hex,
                  ;; hexrgb-complement, hexrgb-defined-colors,
                  ;; hexrgb-defined-colors-alist, hexrgb-green, hexrgb-hex-to-rgb,
                  ;; hexrgb-hex-to-hsv, hexrgb-hsv-to-hex, hexrgb-hue, hexrgb-read-color,
                  ;; hexrgb-red, hexrgb-rgb-to-hex, hexrgb-rgb-to-hsv, hexrgb-saturation,
                  ;; hexrgb-value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup Color-Palette nil
  "A color palette: 1) hue x saturation palette and 2) brightness scale."
  :prefix "palette-" :group 'doremi :group 'frames :group 'faces
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
palette.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/palette.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/ColorPalette")
  :link '(emacs-commentary-link :tag "Commentary" "palette"))

(defcustom palette-update-cursor-color-flag nil
  "Non-nil means dynamically update the cursor to make it stand out.
This can cause redisplay of the palette frame, which means a slowdown."
  :type 'boolean :group 'Color-Palette :group 'doremi)

(defcustom palette-verbose-flag nil
  "Non-nil: display color info often; nil: display it only on demand.
Non-nil slows things down to recalculate color components often."
  :type 'boolean :group 'Color-Palette :group 'doremi)

(defcustom palette-message-info 'all
  "Type of information to print in a palette message.
Possible values are:
 all - RGB hex, RGB decimal, and HSV decimal information
 hex - RGB hex information
 hsv - HSV decimal information
 rgb - RGB decimal information
 hex+hsv - RGB hex and HSV decimal information
 hex+rgb - RGB hex and RGB decimal information
 rgb+hsv - RGB decimal and HSV decimal information"
   :type '(choice
           (const :tag "RGB hex, RGB decimal, and HSV decimal information" all)
           (const :tag "RGB hex information"                               hex)
           (const :tag "HSV decimal information"                           hsv)
           (const :tag "RGB decimal information"                           rgb)
           (const :tag "RGB hex and HSV decimal information"               hex+hsv)
           (const :tag "RGB hex and RGB decimal information"               hex+rgb)
           (const :tag "RGB decimal and HSV decimal information"           rgb+hsv))
   :group 'Color-Palette :group 'doremi)

(defcustom palette-font
  (and window-system
       (or (car (x-list-fonts "-*-Courier-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))
           (car (x-list-fonts "-*-fixed-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))
           (car (x-list-fonts "-*-Terminal-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))
           (car (x-list-fonts "-*-*-*-*-*-*-5-*-*-*-*-*-iso8859-1" nil nil 1))))
  "Font to use for the color palette.
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

(defcustom palette-change-color-hook nil
  "*Functions to run at the end of `palette-set-current-color'.
Typically, applications bind this hook to a function that does
something with the new value of `palette-current-color' after a color
change."
  :type 'hook :group 'Color-Palette)

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

(defcustom palette-save-color-hook nil
  "*Functions to run at the end of `palette-save-new-color'."
  :type 'hook :group 'Color-Palette)

(defvar palette-action nil
  "Function called on the current palette color, whenever it changes.")

(defvar palette-current-color "#000000000000"
  "Current (new) color.  Use `\\<palette-mode-map>\\[palette-save-new-color]' \
to save it.")

(defvar palette-last-color "#000000000000"
  "Current color before last command.  Use `\\<palette-mode-map>\\[palette-swap-last-color]' \
to make this the current color.")

(defvar palette-old-color "#000000000000"
  "Saved color.  Use `\\<palette-mode-map>\\[palette-restore-old-color]' \
to make this the current color.")

(defvaralias 'eyedrop-picked-background 'palette-picked-background)
(defvar palette-picked-background nil
  "Color last picked from a face or frame background.
You can use `palette-pick-background-at-point' or
`palette-pick-background-at-mouse' to pick the color.")

(defvaralias 'eyedrop-picked-foreground 'palette-picked-foreground)
(defvar palette-picked-foreground nil
  "Color last picked from a face or frame foreground.
You can use `palette-pick-foreground-at-point' or
`palette-pick-foreground-at-mouse' to pick the color.")

(defvaralias 'eyedrop-last-picked-color 'palette-last-picked-color)
(defvar palette-last-picked-color nil
  "Color last picked from a face or frame foreground or background.")

(defvar palette-saved-blink-cursor-mode nil
  "Value of `blink-cursor-mode' before `palette' is displayed.
`blink-cursor-mode' is restored (turned on or off) to reflect this
saved value when you exit or quit the palette.
The saved value is updated when `palette' is called and whenever the
user updates `blink-cursor-mode'.")

(defvar palette-popup-map nil "Keymap for `palette-mode' popup menu.")
(defvar palette-mode-map nil "Keymap for `palette-mode'.")
(unless palette-mode-map
  (let ((map        (make-sparse-keymap "Color Palette"))
        (popup-map  (make-sparse-keymap "Color Palette Menu")))
    (define-key map [down-mouse-1] 'ignore)
    (define-key map [drag-mouse-1] 'ignore)
    (define-key map [mouse-1]      'palette-background-at-mouse)
    (define-key map [down-mouse-2] 'ignore)
    (define-key map [drag-mouse-2] 'ignore)
    (define-key map [mouse-2]      'palette-pick-background-at-mouse)
    (define-key map [mouse-3]      'ignore)
    (define-key map [down-mouse-3] 'palette-popup-menu)
    (define-key map "?"    'palette-background-at-point)
    (define-key map "."    'palette-current-color)
    (define-key map "~"    'palette-pick-color-complement)
    (define-key map "B"    'palette-increase-blue) ; B, b = blue
    (define-key map "b"    'palette-decrease-blue)
    (if (featurep 'icicles)             ; c = color
        (define-key map "c"  'palette-pick-color-by-name-multi) ; Icicles multi-command
      (define-key map "c"   'palette-pick-color-by-name))
    (define-key map "e"    'palette-toggle-cursor-color) ; e = enhanced cursor color
    (define-key map "f"    'palette-toggle-verbose) ; f = frequent feedback
    (define-key map "G"    'palette-increase-green) ;G, g = green
    (define-key map "g"    'palette-decrease-green)
    (define-key map "H"    'palette-increase-hue) ; H, h = hue
    (define-key map "h"    'palette-decrease-hue)
    (define-key map "l"    'palette-swap-last-color)     ; l = last
    (define-key map "n"    'palette-save-new-color)      ; n = new
    (define-key map "o"    'palette-restore-old-color)   ; o = old
    (define-key map "q"    'palette-quit)                ; q = quit
    (define-key map "R"    'palette-increase-red)        ; R, r = red
    (define-key map "r"    'palette-decrease-red)
    (define-key map "S"    'palette-increase-saturation) ; S, s = saturation
    (define-key map "s"    'palette-decrease-saturation)
    (define-key map "u"    'palette-swap-last-color) ; u = undo
    (define-key map "V"    'palette-increase-value)  ; V ,v = value
    (define-key map "v"    'palette-decrease-value)
    (define-key map "w"    'palette-where-is-color) ; w = where is it?
    (define-key map "x"    'palette-exit)           ; x = exit
    (define-key map "\C-hm" 'palette-help)
    (define-key map "\C-l" 'palette-refresh)
    (define-key map "\r"   'palette-pick-background-at-point)
    (define-key map "\C-o" 'palette-restore-old-color) ; o = old
    (define-key map "\C-s" 'palette-save-new-color)    ; s = save
    (define-key map "\M-c" 'palette-pick-color-by-name) ; c = color
    (define-key map "\M-h" 'palette-pick-color-by-hsv)  ; h = HSV
    (define-key map "\M-r" 'palette-pick-color-by-rgb)  ; r = RGB
    (define-key map [(shift control f)] 'palette-right+pick)
    (define-key map [(shift right)]     'palette-right+pick)
    (define-key map [(shift control b)] 'palette-left+pick)
    (define-key map [(shift left)]      'palette-left+pick)
    (define-key map [(shift control n)] 'palette-down+pick)
    (define-key map [(shift down)]      'palette-down+pick)
    (define-key map [(shift control p)] 'palette-up+pick)
    (define-key map [(shift up)]        'palette-up+pick)
    (define-key map [down-mouse-3]      'palette-popup-menu)

    (define-key-after popup-map [current-color]
      '(menu-item "Current Color Info" palette-current-color
        :help "Return the current color and show info about it"))
    (define-key-after popup-map [bg-at-point]
      '(menu-item "Info at Cursor" palette-background-at-point
        :help "Return the background color under the text cursor"))
    (define-key-after popup-map [separator-1] '(menu-item "--"))
    (define-key-after popup-map [pick-color-by-name]
      `(menu-item "Choose Color By Name"
                  ,(if (featurep 'icicles) 'palette-pick-color-by-name-multi 
                       'palette-pick-color-by-name)
                  :help "Set the current color to a color you name"))
    (define-key-after popup-map [pick-color-by-hsv]
      '(menu-item "Choose Color By HSV" palette-pick-color-by-hsv
        :help "Set the current color by providing hue, saturation, and value"))
    (define-key-after popup-map [pick-color-by-rgb]
      '(menu-item "Choose Color By RGB" palette-pick-color-by-rgb
        :help "Set the current color by providing red, green, and blue components"))
    (define-key-after popup-map [separator-2] '(menu-item "--"))
    (define-key-after popup-map [swap-last-color]
      '(menu-item "Swap Last Color (Undo)" palette-swap-last-color
        :help "Swap the last color and the current color"))
    (define-key-after popup-map [save-new-color]
      '(menu-item "Save Current Color" palette-save-new-color
        :help "Save the current color as the old (original) color"))
    (define-key-after popup-map [restore-old-color]
      '(menu-item "Restore Old Color" palette-restore-old-color
        :help "Restore the old (original) color as the current color"))
    (define-key-after popup-map [separator-3] '(menu-item "--"))
    (define-key-after popup-map [complement]
      '(menu-item "Complement" palette-pick-color-complement
        :help "Set the current color to its complement."))
    (define-key-after popup-map [increase-red]
      '(menu-item "Increase Red" palette-increase-red
        :help "Increase the red component of the current color by ARG/100"))
    (define-key-after popup-map [decrease-red]
      '(menu-item "  Decrease Red" palette-decrease-red
        :help "Decrease the red component of the current color by ARG/100"))
    (define-key-after popup-map [increase-green]
      '(menu-item "Increase Green" palette-increase-green
        :help "Increase the green component of the current color by ARG/100"))
    (define-key-after popup-map [decrease-green]
      '(menu-item "  Decrease Green" palette-decrease-green
        :help "Decrease the green component of the current color by ARG/100"))
    (define-key-after popup-map [increase-blue]
      '(menu-item "Increase Blue" palette-increase-blue
        :help "Increase the blue component of the current color by ARG/100"))
    (define-key-after popup-map [decrease-blue]
      '(menu-item "  Decrease Blue" palette-decrease-blue
        :help "Decrease the blue component of the current color by ARG/100"))
    (define-key-after popup-map [increase-hue]
      '(menu-item "Increase Hue" palette-increase-hue
        :help "Increase the hue component of the current color by ARG/100"))
    (define-key-after popup-map [decrease-hue]
      '(menu-item "  Decrease Hue" palette-decrease-hue
        :help "Decrease the hue component of the current color by ARG/100"))
    (define-key-after popup-map [increase-saturation]
      '(menu-item "Increase Saturation" palette-increase-saturation
        :help "Increase the saturation component of the current color by ARG/100"))
    (define-key-after popup-map [decrease-saturation]
      '(menu-item "  Decrease Saturation" palette-decrease-saturation
        :help "Decrease the saturation component of the current color by ARG/100"))
    (define-key-after popup-map [increase-value]
      '(menu-item "Increase Value" palette-increase-value
        :help "Increase the value component of the current color by ARG/100"))
    (define-key-after popup-map [decrease-value]
      '(menu-item "  Decrease Value" palette-decrease-value
        :help "Decrease the value component of the current color by ARG/100"))
    (define-key-after popup-map [separator-4] '(menu-item "--"))
    (define-key-after popup-map [toggle-verbose]
      '(menu-item "Toggle Frequent Feedback" palette-toggle-verbose
        :help "Toggle using frequent color info feedback (`palette-toggle-verbose-flag')"))
    (define-key-after popup-map [toggle-cursor-color]
      '(menu-item "Toggle Enhanced Cursor Color" palette-toggle-cursor-color
        :help "Toggle updating the cursor color so the cursor stands out \
\(`palette-update-cursor-color-flag')"))
    (define-key-after popup-map [refresh]
      '(menu-item "Refresh" palette-refresh
        :help "Refresh the color palette"))
    (define-key-after popup-map [exit]
      '(menu-item "Exit (Update Action)" palette-exit
        :help "Exit the color palette with exit action, if defined."))
    (define-key-after popup-map [quit]
      '(menu-item "Quit (Cancel)" palette-quit
        :help "Quit the color palette without any exit action."))

    (setq palette-mode-map   map
          palette-popup-map  popup-map)))

(substitute-key-definition 'forward-char 'palette-right palette-mode-map global-map)
(substitute-key-definition 'backward-char 'palette-left palette-mode-map global-map)
(substitute-key-definition 'next-line 'palette-down palette-mode-map global-map)
(substitute-key-definition 'previous-line 'palette-up palette-mode-map global-map)

(if (< emacs-major-version 22)
    ;; Emacs 20 and 21: Cannot have a nil parent mode, so use fundamental-mode.
    (define-derived-mode palette-mode fundamental-mode "Color Palette"
      "Major mode for using the color palette.
Turning on this mode runs the normal hook `palette-mode-hook'.
Use command `palette' to display a color palette in Color Palette
mode (`palette-mode').  This has three sub-palettes (from left to
right):

 - a hue x saturation palette - buffer Palette (Hue x Saturation)
   Hue is horizontal; saturation is vertical.  Hue is the tint of
   a color, independent of its brightness and grayness.
   Saturation is the purity of a color (opposite of grayness).

 - a color-swatch palette - buffer Current/Original

 - a value (brightness) palette - buffer Brightness

The color-swatch palette shows the current color and the original
color or the last color saved.  Saving is not persistent.

In the color palette:

 - `mouse-1' or `?' anywhere shows info about a color
 - `mouse-2' or `RET' anywhere picks a color as the current color
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
 - `q' quits the palette
 - `C-l' refreshes the palette: use if you have a display problem
 - `C-h m' provides info on Color Palette mode

Some things to keep in mind when using the Color Palette:

 * Whenever you input a color name, you can use completion
   against the list of all recognized colors.  If you also use my
   library Icicles, then you can match any part(s) of the color
   name.

 * You can at any time use an RGB hexadecimal color string in
   place of a recognized color name.  An RGB string has the form
   #XXXXXXXXXXXX, where each X is a hex digit (the # is optional
   for input).  The number of Xs must be a multiple of 3, with
   the same number of Xs for each of red, green, and blue.
   Examples: #FF0099 (red: FF, green: 00, blue: 99),
   #0C1FA329E (red: 0C1, green: FA3, blue: 29E).

 * Once you find a color you like, you can use its RGB string
   anywhere in Emacs as the color definition of a face or a
   frame.  Its RGB string is the value of
   `palette-current-color'.

 * Hue 0.0 and hue 1.0 are the same: pure red.  The hue x
   saturation palette shows this discontinuity.  Move the cursor
   horizontally near the right side of this palette and you will
   see the hue value jump between 0.0 and 1.0 at a certain point.

 * By default, information about the color at any location is
   only available upon demand, by clicking `mouse-1' or
   `mouse-2', or hitting `?' or `RET'.  If you prefer additional
   feedback, set option `palette-verbose-flag' to non-nil to
   display color information each time you move the cursor, pick
   a color, or modify a color swatch.  This can slow things down
   a bit, because it means additional computation of color
   components.

 * Non-nil `palette-update-cursor-color-flag' updates the frame
   foreground and cursor color dynamically, so that the position
   of the current color stands out well against the palette.  For
   example, if the current color is red then the foreground color
   becomes cyan.  The default value is nil.  When nil, you cannot
   see the black cursor against a black background.  When
   non-nil, there are two annoyances: 1) updating the cursor
   color causes redisplay of the frame, which is slow; 2) If you
   ask for information about a color that is very different from
   the current color, then it still might be difficult to see the
   cursor because of its color.  In that case, you can hit `RET'
   to make it the current color so its position stands out
   better.
   (Hit `l' to undo).

 * You can at any time toggle options `palette-verbose-flag' and
   `palette-update-cursor-color-flag' with keys `f' (for
   \"feedback\") and `e' (for \"enhanced cursor color\").

 * By default, feedback about a color includes its RGB hex
   string, RGB decimal components, and HSV decimal components.
   If your minibuffer is too short for all of that info, or if
   you are interested in only some of it, then you can change the
   value of user option `palette-message-info' accordingly.  In
   addition, you can use commands `palette-hex-info',
   `palette-hsv-info', `palette-rgb-info' at any time to obtain
   only color information of one type.

 * If you enter an empty name (that is, just hit `RET') when you
   are prompted for a color name, then a name is picked
   randomly."
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
   Hue is horizontal; saturation is vertical.  Hue is the tint of
   a color, independent of its brightness and grayness.
   Saturation is the purity of a color (opposite of grayness).

 - a color-swatch palette - buffer Current/Original

 - a value (brightness) palette - buffer Brightness

The color-swatch palette shows the current color and the original
color or the last color saved.  Saving is not persistent.

In the color palette:

 - `mouse-1' or `?' anywhere shows info about a color
 - `mouse-2' or `RET' anywhere picks a color as the current color
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
 - `q' quits the palette
 - `C-l' refreshes the palette: use if you have a display problem
 - `C-h m' provides info on Color Palette mode

Some things to keep in mind when using the Color Palette:

 * Whenever you input a color name, you can use completion
   against the list of all recognized colors.  If you also use my
   library Icicles, then you can match any part(s) of the color
   name.

 * You can at any time use an RGB hexadecimal color string in
   place of a recognized color name.  An RGB string has the form
   #XXXXXXXXXXXX, where each X is a hex digit (the # is optional
   for input).  The number of Xs must be a multiple of 3, with
   the same number of Xs for each of red, green, and blue.
   Examples: #FF0099 (red: FF, green: 00, blue: 99),
   #0C1FA329E (red: 0C1, green: FA3, blue: 29E).

 * Once you find a color you like, you can use its RGB string
   anywhere in Emacs as the color definition of a face or a
   frame.  Its RGB string is the value of
   `palette-current-color'.

 * Hue 0.0 and hue 1.0 are the same: pure red.  The hue x
   saturation palette shows this discontinuity.  Move the cursor
   horizontally near the right side of this palette and you will
   see the hue value jump between 0.0 and 1.0 at a certain point.

 * By default, information about the color at any location is
   only available upon demand, by clicking `mouse-1' or
   `mouse-2', or hitting `?' or `RET'.  If you prefer additional
   feedback, set option `palette-verbose-flag' to non-nil to
   display color information each time you move the cursor, pick
   a color, or modify a color swatch.  This can slow things down
   a bit, because it means additional computation of color
   components.

 * Non-nil `palette-update-cursor-color-flag' updates the frame
   foreground and cursor color dynamically, so that the position
   of the current color stands out well against the palette.  For
   example, if the current color is red then the foreground color
   becomes cyan.  The default value is nil.  When nil, you cannot
   see the black cursor against a black background.  When
   non-nil, there are two annoyances: 1) updating the cursor
   color causes redisplay of the frame, which is slow; 2) If you
   ask for information about a color that is very different from
   the current color, then it still might be difficult to see the
   cursor because of its color.  In that case, you can hit `RET'
   to make it the current color so its position stands out
   better.
   (Hit `l' to undo).

 * You can at any time toggle options `palette-verbose-flag' and
   `palette-update-cursor-color-flag' with keys `f' (for
   \"feedback\") and `e' (for \"enhanced cursor color\").

 * By default, feedback about a color includes its RGB hex
   string, RGB decimal components, and HSV decimal components.
   If your minibuffer is too short for all of that info, or if
   you are interested in only some of it, then you can change the
   value of user option `palette-message-info' accordingly.  In
   addition, you can use commands `palette-hex-info',
   `palette-hsv-info', `palette-rgb-info' at any time to obtain
   only color information of one type.

 * If you enter an empty name (that is, just hit `RET') when you
   are prompted for a color name, then a name is picked
   randomly."
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
  "Update `palette-saved-blink-cursor-mode' from `blink-cursor-mode'.
No update is made if we are in the palette."
  (unless (eq major-mode 'palette-mode)
    (setq palette-saved-blink-cursor-mode  blink-cursor-mode)))

(defun palette-popup-menu (event)       ; Bound to `mouse-3'.
  "Display a popup menu of palette commands."
  (interactive "e")
  (popup-menu palette-popup-map))

(defun palette-help ()                  ; Bound to `C-h m'.
  "Describe Color Palette mode."
  (interactive)
  (let ((pop-up-frames  t)) (describe-mode (get-buffer "Palette (Hue x Saturation)"))))

(defun palette-hex-info (color)
  "Print the hexadecimal RGB string for COLOR.
With prefix arg, prompts for color name.
Otherwise, uses the color at the cursor."
  (interactive
   (list (if current-prefix-arg (hexrgb-read-color) (palette-background-at-point))))
  (message "RGB hex: %s" color))

(defun palette-hsv-info (color)
  "Print the HSV components of COLOR.
With prefix arg, prompts for color name.
Otherwise, uses the color at the cursor."
  (interactive
   (list (if current-prefix-arg (hexrgb-read-color) (palette-background-at-point))))
  (message "HSV: %s" (hexrgb-hex-to-hsv color)))

(defun palette-rgb-info (color)
  "Print the RGB components of COLOR.
With prefix arg, prompts for color name.
Otherwise, uses the color at the cursor."
  (interactive
   (list (if current-prefix-arg (hexrgb-read-color) (palette-background-at-point))))
  (message "RGB: %s" (hexrgb-hex-to-rgb color)))

(defun palette-current-color (&optional msg-p)
  "Return the current palette color, `palette-current-color'.
Interactively, display a message with information about the color."
  (interactive "p")
  (when msg-p (palette-color-message palette-current-color t))
  palette-current-color)

(defalias 'eyedrop-background-at-mouse 'palette-background-at-mouse)
(defun palette-background-at-mouse (event &optional msg-p) ; Bound to `mouse-1'.
  "Return the background color under the mouse pointer.
Non-nil optional arg MSG-P means display an informative message."
  (interactive "e\np")
  ;; Emacs bug on Windows: Get extra, pending <C-drag-mouse-2> event, so discard it.
  (while (input-pending-p) (discard-input))
  (set-buffer (window-buffer (posn-window (event-end event))))
  (mouse-set-point event)
  (let ((bg  (palette-background-at-point)))
    (when msg-p (if bg (palette-color-message bg t) (message "No background color here")))
    bg))

(defalias 'eyedrop-foreground-at-mouse 'palette-foreground-at-mouse)
(defun palette-foreground-at-mouse (event &optional msg-p)
  "Return the foreground color under the mouse pointer.
Non-nil optional arg MSG-P means display an informative message."
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

(defalias 'background-color 'palette-background-at-point)
(defalias 'eyedrop-background-at-point 'palette-background-at-point)
(defun palette-background-at-point (&optional msg-p) ; Bound to `?'.
  "Return the background color under the text cursor.
There need be no defined face at the cursor position (point).
Non-nil optional arg MSG-P means display an informative message.

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
                      (t nil))))        ; Invalid face value.
    (when msg-p (if bg (palette-color-message bg t) (message "No background color here")))
    bg))

(defalias 'foreground-color 'palette-foreground-at-point)
(defalias 'eyedrop-foreground-at-point 'palette-foreground-at-point)
(defun palette-foreground-at-point (&optional msg-p)
  "Return the foreground color under the text cursor.
There need be no defined face at the cursor position (point).
Non-nil optional arg MSG-P means display an informative message."
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
                      (t nil))))        ; Invalid face value.
    (when msg-p (if fg (palette-color-message fg t) (message "No foreground color here")))
    fg))

(defalias 'eyedrop-pick-background-at-mouse 'palette-pick-background-at-mouse)
(defun palette-pick-background-at-mouse (event &optional show-p)
  "Set the current color to the background color under the mouse pointer.
The background color is saved in `palette-picked-background' and
`palette-last-picked-color'.  The new current color is returned.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there."
  (interactive "e\nP")
  (setq palette-last-color  palette-current-color)
  (let ((win  (posn-window (event-end event)))
        (bg   (palette-background-at-mouse event)))
    (palette-set-current-color bg)
    (setq palette-picked-background  bg
          palette-last-picked-color  bg)
    (unless (stringp bg) (error "No background color here to pick"))
    (when (interactive-p) (palette-color-message bg))
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

(defalias 'eyedrop-pick-foreground-at-mouse 'palette-pick-foreground-at-mouse)
(defun palette-pick-foreground-at-mouse (event &optional show-p) ; Bound to `mouse-2'.
  "Set the current color to the foreground color under the mouse pointer.
The foreground color is saved in `palette-picked-foreground' and
`palette-last-picked-color'.  The new current color is returned.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there."
  (interactive "e\nP")
  (setq palette-last-color  palette-current-color)
  (let ((win  (posn-window (event-end event)))
        (fg   (palette-foreground-at-mouse event)))
    (palette-set-current-color fg)
    (setq palette-picked-foreground  fg
          palette-last-picked-color  fg)
    (unless (stringp fg) (error "No foreground color here to pick"))
    (when (interactive-p) (palette-color-message fg))
    (cond ((get-buffer-window "Palette (Hue x Saturation)" 'visible)
           (palette-brightness-scale)
           (palette-swatch))
          (show-p (palette fg)))
    (select-window win)
    fg))

(defalias 'eyedropper-background 'palette-pick-background-at-point)
(defalias 'pick-background-color 'palette-pick-background-at-point)
(defalias 'eyedrop-pick-background-at-point 'palette-pick-background-at-point)
(defun palette-pick-background-at-point (&optional show-p) ; Bound to `RET'.
  "Set the current color to the background color under the text cursor.
The background color is saved in `palette-picked-background' and
`palette-last-picked-color'.  The (new) current color is returned.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there."
  (interactive "P")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (palette-background-at-point))
    (setq palette-picked-background  palette-current-color
          palette-last-picked-color  palette-current-color)
    (unless (stringp palette-current-color) (error "No background color here to pick"))
    (when (interactive-p) (palette-color-message palette-current-color))
    (cond ((get-buffer-window "Palette (Hue x Saturation)" 'visible)
           (palette-brightness-scale)
           (palette-swatch))
          (show-p (palette palette-current-color))))
  ;; If we are in the palette and `palette-action' is defined, then call it.
  (when (and palette-action (eq major-mode 'palette-mode)) (funcall palette-action))
  palette-current-color)

(defalias 'eyedropper-foreground 'palette-pick-foreground-at-point)
(defalias 'pick-foreground-color 'palette-pick-foreground-at-point)
(defalias 'eyedrop-pick-foreground-at-point 'palette-pick-foreground-at-point)
(defun palette-pick-foreground-at-point (&optional show-p)
  "Set the current color to the foreground color under the text cursor.
The foreground color is saved in `palette-picked-foreground' and
`palette-last-picked-color'.  The (new) current color is returned.
Non-nil optional arg SHOW-P (prefix arg) means display the palette.
If called from the color palette, update the current color there."
  (interactive "P")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (palette-foreground-at-point))
    (setq palette-picked-foreground  palette-current-color
          palette-last-picked-color  palette-current-color)
    (unless (stringp palette-current-color) (error "No foreground color here to pick"))
    (when (interactive-p) (palette-color-message palette-current-color))
    (cond ((get-buffer-window "Palette (Hue x Saturation)" 'visible)
           (palette-brightness-scale)
           (palette-swatch))
          (show-p (palette palette-current-color))))
  palette-current-color)

(defun palette-pick-color-by-name (color) ; Bound to `c', `M-c'.
  "Set the current color to a color you name.
Instead of a color name, you can use an RGB string #XXXXXXXXXXXX,
where each X is a hex digit.  The number of Xs must be a multiple of
3, with the same number of Xs for each of red, green, and blue.
If you enter an empty color name, then a color is picked randomly.
The new current color is returned."
  (interactive (list (hexrgb-read-color t t)))
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

(eval-after-load 'icicles
  '(icicle-define-command palette-pick-color-by-name-multi ; Bound to `c'.
    "Set the current color to a color you name.
Instead of a color name, you can use an RGB string #XXXXXXXXXXXX,
where each X is a hex digit.  The number of Xs must be a multiple of
3, with the same number of Xs for each of red, green, and blue.
If you enter an empty color name, then a color is picked randomly.
The new current color is returned."     ; Doc string
    palette-pick-by-name-action         ; Action function
    "Color (name or #R+G+B+): "         ; `completing-read' arguments
    (hexrgb-defined-colors-alist) nil nil nil nil nil nil
    ((completion-ignore-case t))))

(defun palette-pick-by-name-action (color)
  "Helper function for `palette-pick-color-by-name'.
This is the action function, when `palette.el' is used with Icicles."
  (if (string= "" color)
      (let* ((colors  (hexrgb-defined-colors))
             (rand    (random (length colors)))) ; Random color.
        (setq color  (elt colors rand)))
    (let ((hex-string  (hexrgb-rgb-hex-string-p color t)))
      (when (and hex-string (not (eq 0 hex-string))) (setq color  (concat "#" color))) ; Add #.
      (if (not (or hex-string (if (fboundp 'test-completion) ; Not defined in Emacs 20.
                                  (test-completion color (hexrgb-defined-colors-alist))
                                (try-completion color (hexrgb-defined-colors-alist)))))
          (error "No such color: %S" color)
        (setq color  (hexrgb-color-name-to-hex color))))
    (setq palette-last-color  palette-current-color)
    (save-selected-window
      (setq color  (hexrgb-color-name-to-hex color)) ; Needed if not interactive.
      (palette-set-current-color color)
      (palette-where-is-color color)
      (palette-brightness-scale)
      (palette-swatch))
    palette-current-color))

(defalias 'rgb 'palette-pick-color-by-rgb)
(defun palette-pick-color-by-rgb (red green blue) ; Bound to `M-r'.
  "Set the current color by providing red, green, and blue components.
Each component is from 0.0 to 1.0 inclusive."
  (interactive "nColor by RGB (0 to 1) - Red: \nnGreen: \nnBlue: ")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-rgb-to-hex red green blue))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

(defalias 'hsv 'palette-pick-color-by-hsv)
(defun palette-pick-color-by-hsv (hue saturation value) ; Bound to `M-h'.
  "Set the current color by providing hue, saturation, and value.
Each component is from 0.0 to 1.0 inclusive."
  (interactive "nColor by HSV (0 to 1) - Hue: \nnSaturation: \nnValue: ")
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-hsv-to-hex hue saturation value))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

(defalias 'complement 'palette-pick-color-complement)
(defun palette-pick-color-complement () ; Bound to `~'.
  "Set the current palette color to its complement."
  (interactive)
  (setq palette-last-color  palette-current-color)
  (save-selected-window
    (palette-set-current-color (hexrgb-complement palette-current-color))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch))
  palette-current-color)

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

(defun palette-swap-last-color ()       ; Bound to `l', `u'.
  "Swap the last color and the current color."
  (interactive)
  (save-selected-window
    (setq palette-last-color  (prog1 palette-current-color
                                (palette-set-current-color palette-last-color)))
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch)))

(defun palette-restore-old-color ()     ; Bound to `o', `C-o'.
  "Restore the old (original) color as the current color."
  (interactive)
  (save-selected-window
    (setq palette-last-color  palette-current-color)
    (palette-set-current-color palette-old-color)
    (palette-where-is-color palette-current-color)
    (palette-brightness-scale)
    (palette-swatch)))

(defun palette-refresh ()               ; Bound to `C-l'.
  "Refresh the color palette."
  (interactive)
  (save-selected-window
    (let ((win  (get-buffer-window "Current/Original" 'visible)))
      (when win (select-window win) (goto-char (point-min)) (recenter)))))

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

(defun palette-where-is-color (color &optional cursor-color) ; Bound to `w'.
  "Move to the palette location of COLOR.
This does not change the current color."
  (interactive (list (hexrgb-read-color t)))
  (setq color  (hexrgb-color-name-to-hex color)) ; Needed if not interactive.
  (let ((target-hue              (hexrgb-hue color))
        (target-sat              (hexrgb-saturation color))
        (hue-sat-win             (get-buffer-window "Palette (Hue x Saturation)" 'visible))
        bg hue sat)
    (unless hue-sat-win (error "No Palette displayed - use command `palette'"))
    (select-window hue-sat-win)
    (if (< target-sat 0.049)
        (goto-char (- (point-max) 50)) ; Grayscale color (saturation=0).
      (while (and (not (eobp)) (setq bg  (palette-background-at-point))
                  (setq sat  (hexrgb-saturation bg)) (< target-sat sat))
        (condition-case nil (forward-line 1) (goto-char (point-max))))
      (while (and (not (bobp)) (setq bg  (palette-background-at-point))
                  (setq sat  (hexrgb-saturation bg)) (> target-sat sat))
        (condition-case nil (forward-line -1) (goto-char (point-min))))
      (while (and (not (eolp)) (setq bg  (palette-background-at-point))
                  (setq hue  (hexrgb-hue bg)) (< target-hue hue))
        (forward-char))
      (while (and (not (bolp)) (setq bg  (palette-background-at-point))
                  (setq hue  (hexrgb-hue bg)) (> target-hue hue))
        (backward-char))
      (when palette-update-cursor-color-flag
        (let ((col  (or cursor-color (palette-complement-or-alternative color))))
          (modify-frame-parameters (selected-frame) `(,(cons 'foreground-color col)
                                                       ,(cons 'cursor-color col)
                                                       ,(cons 'mouse-color col)))))
      (when palette-verbose-flag (palette-color-message color)))))

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

(defun palette-left (&optional arg)    ; Bound to `C-b'.
  "Move left ARG chars, wrapping around from the right.
ARG < 0 means move right, wrapping around from the left."
  (interactive "p")
  (palette-right (- (prefix-numeric-value arg))))

;; This assumes that each line ends with a newline.
(defun palette-down (&optional arg)     ; Bound to `C-n'.
  "Move down ARG places, wrapping around from the top.
ARG < 0 means move up, wrapping around from the bottom."
  (interactive "p")
  (let* ((fwd-p   (wholenump arg))
         ;;(redisplay-dont-pause) ;; I don't really see any difference.
         (column  (current-column))
         (start   (progn (beginning-of-line) (point)))
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

(defun palette-up (&optional arg)     ; Bound to `C-p'.
  "Move up ARG chars, wrapping around from the bottom.
ARG < 0 means move down, wrapping around from the top."
  (interactive "p")
  (palette-down (- (prefix-numeric-value arg))))

(defun palette-right+pick (&optional arg) ; Bound to `S-right', `C-S-f' (`C-F').
  "`palette-right' followed by `palette-pick-background-at-point'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-right arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

(defun palette-left+pick (&optional arg) ; Bound to `S-left', `C-S-b' (`C-B').
  "`palette-left' followed by `palette-pick-background-at-point'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-left arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

(defun palette-down+pick (&optional arg) ; Bound to `S-down', `C-S-n' (`C-N').
  "`palette-down' followed by `palette-pick-background-at-point'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-down arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

(defun palette-up+pick (&optional arg) ; Bound to `S-up', `C-S-p' (`C-P').
  "`palette-up' followed by `palette-pick-background-at-point'."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-up arg)
  (unless (input-pending-p) (palette-pick-background-at-point)))

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

(defun palette-decrease-hue (&optional arg) ; Bound to `h'.
  "Decrease the hue component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-hue (- arg)))

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

(defun palette-decrease-saturation (&optional arg) ; Bound to `s'.
  "Decrease the saturation component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-saturation (- arg)))

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

(defun palette-decrease-value (&optional arg) ; Bound to `v'.
  "Decrease the value component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-value (- arg)))

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

(defun palette-decrease-red (&optional arg) ; Bound to `r'.
  "Decrease the red component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-red (- arg)))

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

(defun palette-decrease-green (&optional arg) ; Bound to `g'.
  "Decrease the green component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-green (- arg)))

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

(defun palette-decrease-blue (&optional arg) ; Bound to `b'.
  "Decrease the blue component of the current color by ARG/100."
  (interactive "p")
  (setq palette-last-color  palette-current-color)
  (palette-increase-blue (- arg)))

(defun toggle-palette-verbose 'palette-toggle-verbose)
(defun palette-toggle-verbose ()        ; Bound to `f'.
  "Toggle using frequent color info feedback.
This toggles option `palette-toggle-verbose-flag'."
  (interactive)
  (setq palette-verbose-flag  (not palette-verbose-flag))
  (message "Verbose color feedback is now %s" (if palette-verbose-flag "ON" "OFF")))

(defun toggle-palette-cursor-color 'palette-toggle-cursor-color)
(defun palette-toggle-cursor-color ()   ; Bound to `e'.
  "Toggle updating the cursor color so the cursor stands out.
This toggles option `palette-update-cursor-color-flag'."
  (interactive)
  (setq palette-update-cursor-color-flag  (not palette-update-cursor-color-flag))
  (unless palette-update-cursor-color-flag
    (modify-frame-parameters
     (selected-frame)
     '((foreground-color . "Black") (cursor-color . "Black") (mouse-color . "Black"))))
  (message "Cursor highlighting is now %s" (if palette-update-cursor-color-flag "ON" "OFF")))

(defalias 'colors 'palette)
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
  (interactive (list (hexrgb-read-color nil t)))
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
      (let* ((cells  (make-string stringlen ?\s- ))
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

(defun palette-brightness-scale (&optional color)
  "Display a brightness (value) scale for COLOR.
If a color palette is already displayed, then just update it."
  (interactive (list (hexrgb-read-color)))
  (setq color  (or color palette-current-color))
  (setq color  (hexrgb-color-name-to-hex color)) ; Needed if not interactive.
  (let* ((width          5)
         (height         100)
         (hue-sat-win    (get-buffer-window "Palette (Hue x Saturation)" 'visible))
         (pop-up-frames  (not hue-sat-win))
         (stringlen      (* width height))
         (target-val     (hexrgb-value color))
         (val            1.0))
    (if (and (interactive-p) hue-sat-win)
        (palette-pick-color-by-name color)
      (let ((split-window-preferred-function  (lambda (w) (eq w (get-lru-window)))))
        (with-output-to-temp-buffer "Brightness"
          (let* ((cells  (make-string stringlen ?\s- ))
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
        (condition-case nil (forward-line 1) (goto-char (point-max))))
      (while (and (not (bobp))
                  (setq val  (hexrgb-value (palette-background-at-point)))
                  (> target-val val))
        (condition-case nil (forward-line -1) (goto-char (point-min))))
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
               (when (interactive-p) (redisplay t))))))))

(defun palette-swatch (&optional oldp color)
  "Display a color swatch for COLOR.
OLDP non-nil means update the original (old) color;
     nil means update the current (new) color.
If a color palette is already displayed, then just update it."
  (interactive (list nil (hexrgb-read-color)))
  (let* ((width          10)
         (height         50)
         (hue-sat-win    (get-buffer-window "Palette (Hue x Saturation)" 'visible))
         (swatch-name    "Current/Original")
         (pop-up-frames  (not hue-sat-win))
         (stringlen      (* width height)))
    (if (and (interactive-p) hue-sat-win)
        (palette-pick-color-by-name color)
      (setq color  (or color (hexrgb-color-name-to-hex ; Needed if not interactive.
                              (if oldp palette-old-color palette-current-color))))
      (let* ((cells  (make-string stringlen ?\s- ))
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
          (when (interactive-p) (redisplay t)))))))

(defun palette-complement-or-alternative (color &optional alternative)
  "Complement of COLOR, or ALTERNATIVE if COLOR is its own complement.
The default ALTERNATIVE color is Red."
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
  "Print information about COLOR.
Non-nil HINT-P means provide a hint about how to pick the color."
  (let* ((rgb  (hexrgb-hex-to-rgb color))
         (hsv  (apply #'hexrgb-rgb-to-hsv rgb))
         (msg  (case palette-message-info
                 (all (format "Color: %s, RGB: %s, HSV: %s" color rgb hsv))
                 (hex (format "RGB hex: %s" color))
                 (hsv (format "HSV: %s" hsv))
                 (rgb (format "RGB: %s" rgb))
                 (hex+hsv (format "Color: %s, HSV: %s" color hsv))
                 (hex+rgb (format "Color: %s, RGB: %s" color rgb))
                 (rgb+hsv (format "RGB: %s, HSV: %s" rgb hsv)))))
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

;;; mon-css-color.el --- Highlight and edit CSS colors
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;; Copyright © 2008, 2009 Lennart Borgman
;; Copyright © 2008  Niels Giesen
;;; ================================================================

;; AUTHOR: Niels Giesen
;; MAINTAINER: MON KEY
;; FILENAME: mon-css-color.el
;; CREATED: 2009-05-09
;; VERSION: MON-0.04
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: processes, CSS, hypermedia, extensions, tools

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-css-color.el is a modified version of Niels Giesen's css-color.el
;; Likewise, mon-css-color.el is a fork of the css-color.el version "0.03" of
;; 2008 provided by the nxhtml suite as an `utility' file.
;;
;; mon-css-color.el differs in the following ways from the version provided with
;; the nxhtml package.
;;
;; - css-color-mode related functions lives in a separate namespace e.g.:
;;   `css-color:*' versus nxhtml's `css-color-*';
;; - It does not require users download the entire nXhtml package.
;; - It is doesn't shadow other css related library user level bindings/hooks 
;; - It provides additional documentation and docstring xrefs;
;; - It can be used with mon-doc-help-css.el package;
;; - Its function parameter names are more verbose (but functionally equivalent);
;; - It wraps all calls to `looking-at'/`looking-back' in `save-match-data' forms;
;; - It doesn't attempt to be `mumamo' aware/compatible;  
;;
;; A full comparison of mon-css-color.el with the nxhtml version of css-color.el
;; can be made by acquiring the from one of the following following urls:
;;
;; :SEE (URL `http://bazaar.launchpad.net/%7Enxhtml/nxhtml/main/annotate/head%3A/util/css-color.el')
;; :SEE (URL `http://bazaar.launchpad.net/~nxhtml/nxhtml/main/files/head%3A/')
;; :SEE (URL `http://www.emacswiki.org/emacs/NxhtmlMode')
;; 
;; USAGE:
;; Make sure mon-css-color.el is in your loadpath and put the following two
;; lines in your .emacs init file:
;;
;; (autoload  css-color-mode "mon-css-color" "" t)
;; (add-hook 'css-mode-hook  css-color-turn-on-in-buffer)
;;
;; <Timestamp: #{2010-03-31T12:47:59-04:00Z}#{10133} - by MON KEY>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Following are Niels Gielsen's commentary from css-color.el v0.3 2008
;;
;; Edit css-colors in hex, rgb or hsl notation in-place, with
;; immediate feedback by font-locking. Cycle between color-spaces.
;;
;; Css-color.el propertizes colours in a CSS stylesheet found by
;; font-locking code with a keymap. From that keymap, you can easily
;; adjust values such as red green and blue, hue, saturation and
;; value, or switch between different color (space) notations.
;;
;; It supports all 'css-colors', so hex, rgb(), hsl() and even HTML
;; color names (although I wouldn't use them myself, it is nice to be
;; able to quickly convert those), can be used and switched between.
;;
;; The rgb() notation can be expressed either in percentages or in
;; values between 0-255.
;;
;; You can cycle between the different formats (with SPACE), so that
;; it is possible to edit the color in hsl mode (which is more
;; intuitive than hsv, although hsv has its merits too), and switch
;; back to rgb or hex if so desired.
;;
;; With point on a color, the keys - and = to are bound to the down
;; and up functions for channels (or 'fields'). Toggling percentage
;; in rgb() is done with the % key (not sure if that is wise
;; though). The TAB key is bound to go to the next channel, cycling
;; when at the end. color.el propertizes the longhand hexcolours
;; found by the
;;
;; Caveats:
;;
;; Notation cycling can often introduce small errors inherent to
;; switching color spaces. Currently there is no check nor a warning
;; for that.
;;
;; ToDo:
;;
;; Try and fix those conversion inaccuracies. This cannot be done
;; completely I guess. But maybe we can check whether this has
;; occured, and then warn.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;; FUNCTIONS:►►►
;; `css-color:adjust-hsv-hue-at-posn', `css-color:adjust-hsv-sat-at-posn',
;; `css-color:adjust-hsv-val-at-posn', `css-color:adjust-hex-at-posn',
;; `css-color:text-property-color-start', `css-color:text-property-color-region',
;; `css-color:cycle-type', `css-color:rgb-down',
;; `css-color:text-property-color-end', `css-color:font-lock-hook-fun',
;; `css-color:foreground-color', `css-color:get-color-at-point',
;; `css-color:hex-to-hsv', `css-color:hex-to-rgb',
;; `css-color:hexify-anystring', `css-color:hexval-beginning',
;; `css-color:hsl-to-hex', `css-color:hsl-to-rgb',
;; `css-color:hsl-to-rgb-fractions', `css-color:hsv-to-hex',
;; `css-color:hsv-to-hsl', `css-color:hsv-to-prop-hexstring',
;; `css-color:hsv-to-rgb', `css-color:hsv-hue-down', `css-color:hue-to-rgb',
;; `css-color:hsv-hue-up', `css-color:incr-hsv-hue', `css-color:incr-hsv-sat',
;; `css-color:incr-hsv-val', `css-color:what-channel', `css-color:next-type',
;; `css-color:normalize-hue', `css-color:num-down', `css-color:num-up',
;; `css-color:pal-lumsig', `css-color:parse-hsl', `css-color:repl-color-at-posn',
;; `css-color:rgb-to-hex', `css-color:rgb-to-hsl', `css-color:rgb-to-hsv',
;; `css-color:run-tests', `css-color:hsv-saturation-down',
;; `css-color:hsv-saturation-up', `css-color:string-hex-to-hsl',
;; `css-color:string-hsl-to-hex', `css-color:string-hsl-to-rgb',
;; `css-color:string-name-to-hex', `css-color:string-rgb-to-hex',
;; `css-color:string-rgb-to-name', `css-color:examine-color',
;; `css-color:toggle-percentage', `css-color:rgb-up',
;; `css-color:hsv-value-down', `css-color:hsv-value-up',
;; `css-color:what-channel', `css-color:within-bounds',
;; `css-color-turn-on-in-buffer', `css-color-global-mode',
;; `css-color:html-color-by-name', `css-color:html-color-both-cases',
;; FUNCTIONS:◄◄◄
;;
;; CONSTANTS:
;; `css-color:version', `*css-color:hex-chars*', `*css-color:html-colors*',
;; `*css-color:type-circle*', `*regexp-css-color-color*',
;; `*regexp-css-color-hex*', `*regexp-css-color-hsl*', `*regexp-css-color-rgb*',
;;
;; VARIABLES:
;; `*css-color:bg-history*', `*css-color:fg-history*',
;; `*css-color:generic-map*', `*css-color:keywords*', `*css-color:map*',
;; `*regexp-css-color-html*', `*css-color-string-frob*',
;;
;; NOTES:
;;
;; RELATED-MATERIAL:
;; W3C SVG color keywords: 
;; :SEE (URL `http://www.w3.org/TR/SVG/types.html#ColorKeywords')
;;
;; W3C Candidate Recommendation 2009-09-08
;; Cascading Style Sheets Level 2 Revision 1 (CSS 2.1) Specification
;; :SEE (URL `http://www.w3.org/TR/CSS2/colors.html')
;;
;; Drew Adams' hexrgb.el
;; :SEE (URL `http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el')
;;
;; TODO:
;;
;; URL: http://www.emacswiki.org/emacs/mon-css-color.el
;; FILE-PUBLISHED: <Timestamp: #{2010-03-31T12:47:59-04:00Z}#{10133} - by MON KEY>
;;
;; FILE-CREATED: 2009-05-09
;;
;; =================================================================
;; Following GFDL permissions apply to documented features having either the
;; `css-color:' or `*regexp-css-color-' symbol prefixes:
;;
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; Code:

(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
(defconst css-color:version "MON-0.04"
  "<Timestamp: #{2010-06-04T17:23:15-04:00Z}#{10225} - by MON KEY>
:SEE-ALSO `css-color-mode'.\n►►►")

;;;###autoload
(defgroup css-color ()
  "Customization group for features provided by the  `css-color' package.\n
:SEE-ALSO `css-color-mode', `css-color-global-mode',
`css-color-turn-on-in-buffer', `css-color:version'.\n►►►"
  :group 'css)

;;; ==============================
(defun css-color-turn-on-in-buffer ()
  "Turn on `css-color-mode' in `css-mode'.\n
:SEE-ALSO `css-color-global-mode'.\n►►►"
  (when (derived-mode-p 'css-mode)
    (css-color-mode 1)))

;;;###autoload
(define-globalized-minor-mode css-color-global-mode css-color-mode
  css-color-turn-on-in-buffer
  :group 'css-color)

;;; ==============================
(defconst *css-color:hex-chars* (concat (number-sequence 48 57) 
                                        (number-sequence 97 102) 
                                        (number-sequence 65 70))
  "Composing chars in hexadecimal notation, save for the hash (#) sign.\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*css-color:hex-chars*) (unintern "*css-color:hex-chars*" obarray) )

;;; ==============================
;;; :NOTE :FILE faces.el `read-color' has this regexp:
;;;  "^#?\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)+$"
;;; :NOTE Also, there is the regexp character class `[:xdigit:]'.
(defconst *regexp-css-color-hex* "#\\([a-fA-F[:digit:]]\\{6\\}\\|[a-fA-F[:digit:]]\\{3\\}\\)"
  "A regexp to match css-color hexadecimal strings.\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`read-color', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*regexp-css-color-hex*) (unintern "*regexp-css-color-hex*" obarray) )

;;; ==============================
(defconst *regexp-css-color-hsl* 
  (concat "hsla?(\\([[:digit:]]\\{1,3\\}\\)"
          ",[[:space:]]*"
          "\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*\\)\\)"
          "%,[[:space:]]*"
          "\\([[:digit:]]\\{1,3\\}\\)"
          "\\(?:\.?[[:digit:]]*\\)%)")
  "A regexp to match css-colors as hue saturation value strings.\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*regexp-css-color-hsl*) (unintern "*regexp-css-color-hsl*" obarray) )

;;; ==============================
(defconst *regexp-css-color-rgb* 
  (concat "rgba?(\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*%\\)?\\)"
          ",[[:space:]]*"
          "\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*%\\)?\\)"
          ",[[:space:]]*"
          "\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*%\\)?\\)"
          "\\(:?,[[:space:]]*\\(0\.[0-9]+\\|1\\)\\)?)")
  "A regexp to match css-colors as rgb value strings.\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*regexp-css-color-rgb*) (unintern "*regexp-css-color-rgb*" obarray) )

;;; ==============================
(defconst *css-color:html-colors*
  '(("AliceBlue" "#F0F8FF")
    ("AntiqueWhite" "#FAEBD7")
    ("Aqua" "#00FFFF")
    ("Aquamarine" "#7FFFD4")
    ("Azure" "#F0FFFF")
    ("Beige" "#F5F5DC")
    ("Bisque" "#FFE4C4")
    ("Black" "#000000")
    ("BlanchedAlmond" "#FFEBCD")
    ("Blue" "#0000FF")
    ("BlueViolet" "#8A2BE2")
    ("Brown" "#A52A2A")
    ("BurlyWood" "#DEB887")
    ("CadetBlue" "#5F9EA0")
    ("Chartreuse" "#7FFF00")
    ("Chocolate" "#D2691E")
    ("Coral" "#FF7F50")
    ("CornflowerBlue" "#6495ED")
    ("Cornsilk" "#FFF8DC")
    ("Crimson" "#DC143C")
    ("Cyan" "#00FFFF")
    ("DarkBlue" "#00008B")
    ("DarkCyan" "#008B8B")
    ("DarkGoldenRod" "#B8860B")
    ("DarkGray" "#A9A9A9")
    ("DarkGrey" "#A9A9A9")
    ("DarkGreen" "#006400")
    ("DarkKhaki" "#BDB76B")
    ("DarkMagenta" "#8B008B")
    ("DarkOliveGreen" "#556B2F")
    ("Darkorange" "#FF8C00")
    ("DarkOrchid" "#9932CC")
    ("DarkRed" "#8B0000")
    ("DarkSalmon" "#E9967A")
    ("DarkSeaGreen" "#8FBC8F")
    ("DarkSlateBlue" "#483D8B")
    ("DarkSlateGray" "#2F4F4F")
    ("DarkSlateGrey" "#2F4F4F")
    ("DarkTurquoise" "#00CED1")
    ("DarkViolet" "#9400D3")
    ("DeepPink" "#FF1493")
    ("DeepSkyBlue" "#00BFFF")
    ("DimGray" "#696969")
    ("DimGrey" "#696969")
    ("DodgerBlue" "#1E90FF")
    ("FireBrick" "#B22222")
    ("FloralWhite" "#FFFAF0")
    ("ForestGreen" "#228B22")
    ("Fuchsia" "#FF00FF")
    ("Gainsboro" "#DCDCDC")
    ("GhostWhite" "#F8F8FF")
    ("Gold" "#FFD700")
    ("GoldenRod" "#DAA520")
    ("Gray" "#808080")
    ("Grey" "#808080")
    ("Green" "#008000")
    ("GreenYellow" "#ADFF2F")
    ("HoneyDew" "#F0FFF0")
    ("HotPink" "#FF69B4")
    ("IndianRed" "#CD5C5C")
    ("Indigo" "#4B0082")
    ("Ivory" "#FFFFF0")
    ("Khaki" "#F0E68C")
    ("Lavender" "#E6E6FA")
    ("LavenderBlush" "#FFF0F5")
    ("LawnGreen" "#7CFC00")
    ("LemonChiffon" "#FFFACD")
    ("LightBlue" "#ADD8E6")
    ("LightCoral" "#F08080")
    ("LightCyan" "#E0FFFF")
    ("LightGoldenRodYellow" "#FAFAD2")
    ("LightGray" "#D3D3D3")
    ("LightGrey" "#D3D3D3")
    ("LightGreen" "#90EE90")
    ("LightPink" "#FFB6C1")
    ("LightSalmon" "#FFA07A")
    ("LightSeaGreen" "#20B2AA")
    ("LightSkyBlue" "#87CEFA")
    ("LightSlateGray" "#778899")
    ("LightSlateGrey" "#778899")
    ("LightSteelBlue" "#B0C4DE")
    ("LightYellow" "#FFFFE0")
    ("Lime" "#00FF00")
    ("LimeGreen" "#32CD32")
    ("Linen" "#FAF0E6")
    ("Magenta" "#FF00FF")
    ("Maroon" "#800000")
    ("MediumAquaMarine" "#66CDAA")
    ("MediumBlue" "#0000CD")
    ("MediumOrchid" "#BA55D3")
    ("MediumPurple" "#9370D8")
    ("MediumSeaGreen" "#3CB371")
    ("MediumSlateBlue" "#7B68EE")
    ("MediumSpringGreen" "#00FA9A")
    ("MediumTurquoise" "#48D1CC")
    ("MediumVioletRed" "#C71585")
    ("MidnightBlue" "#191970")
    ("MintCream" "#F5FFFA")
    ("MistyRose" "#FFE4E1")
    ("Moccasin" "#FFE4B5")
    ("NavajoWhite" "#FFDEAD")
    ("Navy" "#000080")
    ("OldLace" "#FDF5E6")
    ("Olive" "#808000")
    ("OliveDrab" "#6B8E23")
    ("Orange" "#FFA500")
    ("OrangeRed" "#FF4500")
    ("Orchid" "#DA70D6")
    ("PaleGoldenRod" "#EEE8AA")
    ("PaleGreen" "#98FB98")
    ("PaleTurquoise" "#AFEEEE")
    ("PaleVioletRed" "#D87093")
    ("PapayaWhip" "#FFEFD5")
    ("PeachPuff" "#FFDAB9")
    ("Peru" "#CD853F")
    ("Pink" "#FFC0CB")
    ("Plum" "#DDA0DD")
    ("PowderBlue" "#B0E0E6") ;; (assoc-ignore-case "PowderBlue" color-name-rgb-alist)
    ("Purple" "#800080")
    ("Red" "#FF0000")
    ("RosyBrown" "#BC8F8F")
    ("RoyalBlue" "#4169E1")
    ("SaddleBrown" "#8B4513")
    ("Salmon" "#FA8072")
    ("SandyBrown" "#F4A460")
    ("SeaGreen" "#2E8B57")
    ("SeaShell" "#FFF5EE")
    ("Sienna" "#A0522D")
    ("Silver" "#C0C0C0")
    ("SkyBlue" "#87CEEB")
    ("SlateBlue" "#6A5ACD")
    ("SlateGray" "#708090")
    ("SlateGrey" "#708090")
    ("Snow" "#FFFAFA")
    ("SpringGreen" "#00FF7F")
    ("SteelBlue" "#4682B4")
    ("Tan" "#D2B48C")
    ("Teal" "#008080")
    ("Thistle" "#D8BFD8")
    ("Tomato" "#FF6347")
    ("Turquoise" "#40E0D0")
    ("Violet" "#EE82EE")
    ("Wheat" "#F5DEB3")
    ("White" "#FFFFFF")
    ("WhiteSmoke" "#F5F5F5")
    ("Yellow" "#FFFF00")
    ("YellowGreen" "#9ACD32"))
  "List of color names mapped to their hexadecimal values.\n
:EXAMPLE\n\n\(assoc-ignore-case \"yellowgreen\" *css-color:html-colors*\)\n
:NOTE The 16-bit RGB values X color name conversions can be found in
`color-name-rgb-alist' and accessed with:\n
\(assoc-ignore-case \"PowderBlue\" color-name-rgb-alist\)\n
These are the W3C SVG color keywords found here:
:SEE (URL `http://www.w3.org/TR/SVG/types.html#ColorKeywords ')\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*css-color:html-colors*) (unintern "*css-color:html-colors*" obarray) )


;;; ==============================
;;; :CHANGESET 1839
;;; :CREATED <Timestamp: #{2010-06-09T20:11:33-04:00Z}#{10233} - by MON KEY>
(defun css-color:html-color-both-cases ()
  "Return both cases of HTML colors in car of `*css-color:html-colors*'.\n
:EXAMPLE\n\n\(css-color:html-color-both-cases\)\n
\(mapcar #'car *css-color:html-colors*\)\n
:SEE-ALSO `css-color:html-color-by-name', `*css-color:html-colors*',
`css-color:examine-color', `*regexp-css-color-html*', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (let (html-color-up-n-down)
   (mapc #'(lambda (hcund) 
             (let ((up-n-down (car hcund)))
               (push up-n-down html-color-up-n-down)
               (push (downcase up-n-down) html-color-up-n-down)))
         *css-color:html-colors*)
   html-color-up-n-down))

;;; ==============================
;;; :NOTE `*regexp-css-color-html*' has false matches for colors with [.+:-] in
;;; prefix/suffix causing font-lock to highlight color names in CSS symbols 
;;; ids, selectors, classe etc. e.g. following are getting lit up:
;;; ``.txtbold-black-justify'' ``.ligneg_light_brown''
;;;
(defvar *regexp-css-color-html*
  ;;  (concat "\\([^.+:_-]" (regexp-opt (css-color:html-color-both-cases)) "[^.+:_-]\\)")
  (concat "\\<\\(" (funcall 'regexp-opt (css-color:html-color-both-cases)) "\\)\\>")
  "*Regexp generated from values of `*css-color:html-colors*'.\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*regexp-css-color-html*) (unintern "*regexp-css-color-html*" obarray) )

;;; ==============================
;;; (defconst *regexp-css-color-color*
;;;   (concat "\\(?1:"
;;;   (mapconcat
;;;    'identity
;;;    (list *regexp-css-color-hex*
;;; 	 *regexp-css-color-hsl*
;;; 	 *regexp-css-color-rgb*) "\\|")
;;;   "\\)"))
;;; ==============================
(defconst *regexp-css-color-color* 
  (concat  "\\(?:#"
           "\\(?:[a-fA-F[:digit:]]\\{6\\}\\|[a-fA-F[:digit:]]\\{3\\}\\)"
           "\\|hsl("
           "\\(?:[[:digit:]]\\{1,3\\}\\)"
           ",[[:space:]]*"
           "\\(?:[[:digit:]]\\{1,3\\}\\)"
           "%,[[:space:]]*"
           "\\(?:[[:digit:]]\\{1,3\\}\\)%)"
           "\\|rgba?("
           "\\(?:[[:digit:]]\\{1,3\\}%?\\)"
           ",[[:space:]]*"
           "\\(?:[[:digit:]]\\{1,3\\}%?\\)"
           ",[[:space:]]*"
           "\\(?:[[:digit:]]\\{1,3\\}%?\\)"
           "\\(?:,[[:space:]]*\\(?:0.[0-9]+\\|1\\)\\)?)"
           "\\)")
  "Regular expression containing only shy groups matching any type of CSS color.\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*regexp-css-color-color*) (unintern "*regexp-css-color-color*" obarray) )

;;; ==============================
;;; :PREFIX "cck-"
(defvar *css-color:keywords*
  `((,*regexp-css-color-hex*
     (0 (progn
          (when (= 7 (- (match-end 0) (match-beginning 0)))
            (put-text-property (match-beginning 0) (match-end 0) 'keymap *css-color:map*))
          (put-text-property (match-beginning 0) (match-end 0) 'color-css-type 'hex)
          (put-text-property (match-beginning 0) (match-end 0) 'rear-nonsticky t) 
          (put-text-property (match-beginning 0) (match-end 0) 
                             'face (list :background (match-string-no-properties 0)
                                         :foreground (css-color:foreground-color (match-string-no-properties 0)))))))
    (,*regexp-css-color-html*
     (0 (let ((cck-htm-clr (css-color:string-name-to-hex (match-string-no-properties 0))))
          (put-text-property (match-beginning 0) (match-end 0) 'keymap *css-color:generic-map*)
          (put-text-property (match-beginning 0) (match-end 0) 'color-css-type 'name)
          (put-text-property (match-beginning 0) (match-end 0) 'rear-nonsticky t)
          (put-text-property (match-beginning 0) (match-end 0) 
                             'face (list :background cck-htm-clr
                                         :foreground (css-color:foreground-color cck-htm-clr))))))
    (,*regexp-css-color-hsl*
     (0 (let ((cck-hsl-clr (concat "#" (apply #'css-color:hsl-to-hex
                                        (mapcar #'string-to-number
                                                (list (match-string-no-properties 1)
                                                      (match-string-no-properties 2)
                                                      (match-string-no-properties 3)))))))
          (put-text-property (match-beginning 0) (match-end 0) 'keymap *css-color:generic-map*)
          (put-text-property (match-beginning 0) (match-end 0) 'color-css-type 'hsl)
          (put-text-property (match-beginning 0) (match-end 0) 'rear-nonsticky t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face   (list :background cck-hsl-clr
                                           :foreground (css-color:foreground-color cck-hsl-clr))))))
    (,*regexp-css-color-rgb*
     (0 (let ((cck-rgb-clr (css-color:string-rgb-to-hex (match-string-no-properties 0))))
          (put-text-property (match-beginning 0) (match-end 0) 'keymap *css-color:generic-map*)
          (put-text-property (match-beginning 0) (match-end 0) 'color-css-type 'rgb)
          (put-text-property (match-beginning 0) (match-end 0) 'rear-nonsticky t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face (list :background cck-rgb-clr
                                         :foreground (css-color:foreground-color cck-rgb-clr)))))))
  "*A list of regexps for font-locking css-color's in `css-color-mode'.\n
:SEE-ALSO `*regexp-css-color-hex*', `*regexp-css-color-hsl*',
`*regexp-css-color-rgb*', `*regexp-css-color-html*', `*regexp-css-color-color*',
`*css-color:keywords*', `*css-color:html-colors*', `*regexp-rgb-hex*',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*css-color:keywords*) (unintern "*css-color:keywords*" obarray) )

;;; ==============================
;;;###autoload
(define-minor-mode css-color-mode
  "Show hex color literals with the given color as background.\n
In this mode hexadecimal colour specifications like #3253ff are
displayed with the specified colour as background.\n
Adds kiybindings for special CSS colour editing commands when
point is at a hexadecimal colour:\n\n
\\{*css-color:map*}\n\n
:SEE-ALSO `css-color-turn-on-in-buffer', `css-color-global-mode',
`*css-color:map*', `*css-color:generic-map*', `css-color:font-lock-hook-fun',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  :initial-value nil
  :group 'css-color
  (unless font-lock-defaults
     (error (concat ":MACRO `define-minor-mode' "
                    "-- can not do minor-mode `css-color-mode' with this major-mode")))
  (if css-color-mode
      (progn
	(unless font-lock-mode (font-lock-mode 1))
        (css-color:font-lock-hook-fun)
        (add-hook 'font-lock-mode-hook 'css-color:font-lock-hook-fun nil t))
    (remove-hook  'font-lock-mode-hook 'css-color:font-lock-hook-fun t)
    (font-lock-remove-keywords nil *css-color:keywords*))
  ;;
  ;; <Timestamp: Tuesday May 05, 2009 @ 04:16.44 PM - by MON>
  ;; :COMMENTED Was causing problems | :UNCOMMENTED  (font-lock-fontify-buffer) below.
  ;; (save-restriction
  ;;   (widen)
  ;;   (mumamo-mark-for-refontification (point-min) (point-max))))
  ;; ==============================
  ;; :NOTE Lennart changed his version of css-color.el 
  ;; :SEE :CHANGESET 679 2010-05-27 03:15:26
  ;; He commented the `save-restriction' form as per our comments above and
  ;; changed following :FROM (font-lock-fontify-buffer) -> (jit-lock-refontify)
  ;; So, do the same.
  ;;
  ;; (font-lock-fontify-buffer))
  ;;
  (jit-lock-refontify))  
  
;;; ==============================
;;; :NOTE It isn't clear to me that `css-color-mode' needs to be permanent-local
;;; _unless_ one is using `mumamo' features. 
;;; So, until I find a reason otherwise I'm leaving the following commented.
;;;
;;; :COMMENTED <Timestamp: #{2010-05-27T19:48:53-04:00Z}#{10214} - by MON KEY>
;;; (put 'css-color-mode 'permanent-local t)

;;; ==============================
(defun css-color:font-lock-hook-fun ()
  "Add css-color pattern to font-lock's.\n
:SEE-ALSO `css-color-mode', `css-color-turn-on-in-buffer',
`css-color-global-mode', `*css-color:map*', `*css-color:generic-map*',
`css-color:font-lock-hook-fun', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (if font-lock-mode
      (font-lock-add-keywords nil *css-color:keywords* t)
    (css-color-mode -1)))

;;; ==============================
(defvar *css-color:map*
  (let ((css-c-map (make-sparse-keymap)))
    (define-key css-c-map "=" 'css-color:rgb-up)
    (define-key css-c-map "-" 'css-color:rgb-down)
    (define-key css-c-map "h" 'css-color:hsv-hue-up)
    (define-key css-c-map "H" 'css-color:hsv-hue-down)
    (define-key css-c-map "s" 'css-color:hsv-saturation-up)
    (define-key css-c-map "S" 'css-color:hsv-saturation-down)
    (define-key css-c-map "v" 'css-color:hsv-value-up)
    (define-key css-c-map "V" 'css-color:hsv-value-down)
    (define-key css-c-map (kbd "TAB") 'css-color:next-channel)
    (define-key css-c-map (kbd "<SPC>") 'css-color:cycle-type)
    css-c-map)
  "Mode map for minor-mode `css-color-mode'.\n
\\{*css-color:map*}\n
:SEE-ALSO `css-color-turn-on-in-buffer', `css-color-global-mode',
`*css-color:map*', `*css-color:generic-map*', `css-color:font-lock-hook-fun',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*css-color:map*) (unintern "*css-color:map*" obarray) )

;;; ==============================
(defvar *css-color:generic-map* 
  (let ((css-g-map (make-sparse-keymap)))
    (define-key css-g-map "=" 'css-color:num-up)
    (define-key css-g-map "-" 'css-color:num-down)
    (define-key css-g-map (kbd "SPC") 'css-color:cycle-type)
    (define-key css-g-map "%" 'css-color:toggle-percentage)
    (define-key css-g-map (kbd "TAB") 'css-color:next-channel)
    css-g-map)
  "Mode map for simple numbers in minor-mode `css-color-mode'.\n
\\{*css-color:generic-map*}\n
:SEE-ALSO `css-color-turn-on-in-buffer', `css-color-global-mode',
`*css-color:map*', `*css-color:generic-map*', `css-color:font-lock-hook-fun',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*css-color:generic-map*) (unintern "*css-color:generic-map*" obarray) )

;;; ==============================
(defun css-color:pal-lumsig (r->pal g->pal b->pal)
  "Return PAL luminance signal, but in range 0-255.\n
Arguments are as follows:
\n R->PAL = rgb Red;\n G->PAL = rgb Green;\n B->PAL = RGB Blue.\n
:SEE-ALSO `color-distance', `color-values', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (+ (* 0.3 r->pal) (* 0.59 g->pal) (* 0.11 b->pal)))

;;; ==============================
(defun css-color:foreground-color (hex-color)
  "Return suitable foreground color for displaying HEX-COLOR.\n
Foreground color value computed according to `css-color:pal-lumsig'.\nn
When HEX-COLOR has a pal luminance above 128 return \"black\" (hex #000000)
otherwise return \"gray100\" (hex #ffffff).\n
:EXAMPLE\n\n\(css-color:foreground-color  \"#f08080\"\)\n
\(x-color-values \(css-color:foreground-color  \"#f08080\"\)\)\n
\(css-color:foreground-color \"#696969\"\)\n
\(x-color-values \(css-color:foreground-color \"#696969\"\)\)\n
:SEE-ALSO `css-color:hex-to-rgb', `color-values', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (r g b) (css-color:hex-to-rgb hex-color)
    (if (< (css-color:pal-lumsig r g b) 128)
	"#fff"
      "#000")))

;;; ==============================
(defun css-color:normalize-hue (normalize-hue)
  "Normalize value of NORMALIZE-HUE -- a css-color.\n
:SEE-ALSO `css-color:hsl-to-rgb', `css-color:num-up', `css-color:num-down',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
 (mod (+ (mod normalize-hue 360) 360) 360))

;;; ==============================
(defun css-color:within-bounds (num-bnd min-rng max-rng)
  "Test if NUM-BND is within range MIN-RNG and MAX-RNG.\n
:SEE-ALSO `css-color:adjust-hex-at-posn', `css-color:hsl-to-rgb',
`css-color:incr-hsv-sat', `css-color:incr-hsv-val', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (min (max min-rng num-bnd) max-rng))

;;; ==============================
(defun css-color:hex-to-rgb (hex-str->rgb)
  "Convert string HEX-STR->RGB from hex value to rgb value.\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (cond ((not (string-match "^#?[a-fA-F[:digit:]]*$" hex-str->rgb))
         (error (concat ":FUNCTION `css-color:hex-to-rgb' "
                        "-- no valid hexadecimal: %s")
                hex-str->rgb))
        ((= 0 (length hex-str->rgb))
         nil)
        ((= (aref hex-str->rgb 0) 35)
         (css-color:hex-to-rgb (substring hex-str->rgb 1)))
        ((= (mod (length hex-str->rgb) 2) 1)
         (css-color:hex-to-rgb 
          (mapconcat #'(lambda (c)
                         (make-string 2 c))
                     (string-to-list hex-str->rgb) "")))
        (t (cons (string-to-number (substring hex-str->rgb 0 2) 16)
                 (css-color:hex-to-rgb (substring hex-str->rgb 2))))))

;;; ==============================
(defun css-color:hex-to-hsv (hex->hsv)
  "Return css-color hex->hsv value as hsv.\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (r g b) (css-color:hex-to-rgb hex->hsv)
    (css-color:rgb-to-hsv r g b)))

;;; ==============================
(defun css-color:rgb-to-hex (r g b)
  "Return R G B as #rrggbb in hexadecimal, propertized keymap `*css-color:map*'.\n
Arguments are as follows:\n R = Red;\n G = Green;\n B = Blue.\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
   (format "%02x%02x%02x" r g b))

;;; ==============================
;;; :PREFIX "ccrthsv-"
(defun css-color:rgb-to-hsv (r g b)
  "Return list of (hue saturation value).\n
Arguments are as follows:\n R = Red;\n G = Green;\n B = Blue.\n
Measure saturation and value on a scale from 0 - 100.\n
GIMP-style, that is.\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (let* ((r (float r))
	 (g (float g))
	 (b (float b))
	 (ccrthsv-max (max r g b))
	 (ccrthsv-min (min r g b)))
    (values (round (cond ((and (= r g) (= g b)) 0)
                         ((and (= r ccrthsv-max)
                               (>= g b))
                          (* 60 (/ (- g b) (- ccrthsv-max ccrthsv-min))))
                         ((and (= r ccrthsv-max)
                               (< g b))
                          (+ 360 (* 60 (/ (- g b) (- ccrthsv-max ccrthsv-min)))))
                         ((= ccrthsv-max g)
                          (+ 120 (* 60 (/ (- b r) (- ccrthsv-max ccrthsv-min)))))
                         ((= ccrthsv-max b)
                          (+ 240 (* 60 (/ (- r g) (- ccrthsv-max ccrthsv-min)))))))
            (round (* 100 (if (= ccrthsv-max 0) 
                              0 
                            (- 1 (/ ccrthsv-min ccrthsv-max))))) 
            (round (/ ccrthsv-max 2.55)))))

;;; ==============================
;;; :PREFIX "ccrthsl-"
(defun css-color:rgb-to-hsl (r g b)
  "Return R G B (in range 0-255) converted to HSL \(0-360 for hue, rest in %\).\n
Arguments are as follows:\n R = Red;\n G = Green;\n B = Blue.\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (let* ((r (/ r 255.0))
	 (g (/ g 255.0))
	 (b (/ b 255.0))
	 (h 0)
	 (s 0)
	 (l 0)
	 (v (max r g b))
	 (m (min r g b))
	 (l (/ (+ m v) 2.0))
	 (ccrthsl-vm 0)
	 (ccrthsl-r2 0)
	 (ccrthsl-g2 0)
	 (ccrthsl-b2 0))
    (multiple-value-bind (h s v)
	(if (<= l 0)
	    (values h s l)
	  (setq ccrthsl-vm (- v m)
		s ccrthsl-vm)
	  (if (>= 0 s)
	      (values h s l)
	    (setq s (/ s (if (<= l 0.5)
			     (+ v m)
			   (- 2.0 v m))))
	    (if (not (= 0 ccrthsl-vm))
                (setq ccrthsl-r2 (/ (- v r) ccrthsl-vm)
		      ccrthsl-g2 (/ (- v g) ccrthsl-vm)
		      ccrthsl-b2 (/ (- v b) ccrthsl-vm)))
	    (cond ((= r v) (setq h (if (= g m)
                                       (+ 5.0 ccrthsl-b2)
                                     (- 1.0 ccrthsl-g2))))
		  ((= g v) (setq h (if (= b m)
                                       (+ 1.0 ccrthsl-r2)
                                     (- 3.0 ccrthsl-b2))))
		  (t (setq h (if (= r m)
                                 (+ 3.0 ccrthsl-g2)
                               (- 5.0 ccrthsl-r2)))))
	    (values (/ h 6.0) s l)))
      (list (round (* 360 h)) (* 100 s) (* 100 l)))))

;;; ==============================
(defun css-color:hsv-to-hsl (h s v)
  "Convert hsv values H S V to hsl.\n
Arguments are as follows:\n
 H is the hsv HUE;\n S is the hsv Saturation;\n V is the hsv Value;\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (r g b) (css-color:hsv-to-rgb h s v)
    (css-color:rgb-to-hsl r g b)))

;;; ==============================
(defun css-color:hsv-to-hex (h s v)
  "Convert the hsv args H S V to hex values.\n
Arguments are as follows:\n
 H is the hsv HUE;\n S is the hsv Saturation;\n V is the hsv Value;\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
   (apply #'css-color:rgb-to-hex (css-color:hsv-to-rgb h s v)))

;;; ==============================
;;; :PREFIX "cchtr-"
(defun css-color:hsv-to-rgb (h s v)
  "Convert a point in the H S V color space to list of normalized rgb values.\n
H is the HUE an angle in the range of 0 degrees inclusive to 360 exclusive.
The remainder of division by 360 is used for out-of-range values.\n
S the saturation is in the range of 0 to 100.\n
V the value is in the range of 0 to 100.\n
Returns a list of values in the range of 0 to 255.\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  ;; Coerce to float and get hue into range.
  (setq h (mod h 360.0)
        s (/ (float s) 100)
        v (/ (float v) 100))
  (let* ((cchtr-hi (floor h 60.0))
         (cchtr-f (- (/ h 60.0) cchtr-hi))
         (cchtr-p (* v (- 1.0 s)))
         (cchtr-q (* v (- 1.0 (* cchtr-f s))))
         ;; cannot use variable t, obviously.
         (cchtr-u (* v (- 1.0 (* (- 1.0 cchtr-f) s))))
         r g b)
    (case cchtr-hi
      (0 (setq r v g cchtr-u b cchtr-p))
      (1 (setq r cchtr-q g v b cchtr-p))
      (2 (setq r cchtr-p g v b cchtr-u))
      (3 (setq r cchtr-p g cchtr-q b v))
      (4 (setq r cchtr-u g cchtr-p b v))
      (5 (setq r v g cchtr-p b cchtr-q)))
    (mapcar #'(lambda (cchtr-L-1) (round (* 255 cchtr-L-1))) (list r g b))))

;;; ==============================
(defun css-color:hsv-to-prop-hexstring (color-data)
  "Propertize hsv COLOR-DATA converted to string with `css-color:hsv-to-hex'.\n
Put the keymap property as `*css-color:map*'.\n
Put the css-color property as COLOR-DATA.\n
:SEE-ALSO `css-color:incr-hsv-hue', `css-color:incr-hsv-sat',
`css-color:incr-hsv-val', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (propertize (apply #'css-color:hsv-to-hex color-data)
              'keymap *css-color:map* 'color-css-type color-data)) ;; 'color color-data))

;;; ==============================
;;; :PREFIX "cchtrf-"
;;; :MODIFICATIONS <Timestamp: #{2010-03-31T12:52:02-04:00Z}#{10133} - by MON KEY>
;;; :ADDED let binding for local vars `cchtrf-m1` `cchtrf-m2`.
(defun css-color:hsl-to-rgb-fractions (h s l)
  "Convert H S L to fractional rgb values.\n
H is the hsl HUE;\n S is the hsl Saturation;\n L is the hsl lightness;\n
:SEE-ALSO `css-color:hue-to-rgb',`css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (let (cchtrf-m1 cchtrf-m2)
    (if (<= l 0.5)
        (setq cchtrf-m2 (* l (+ s 1)))
        (setq cchtrf-m2 (- (+ l s) (* l s))))
    (setq cchtrf-m1 (- (* l 2) cchtrf-m2))
    (values (css-color:hue-to-rgb cchtrf-m1 cchtrf-m2 (+ h (/ 1 3.0)))
            (css-color:hue-to-rgb cchtrf-m1 cchtrf-m2 h)
            (css-color:hue-to-rgb cchtrf-m1 cchtrf-m2 (- h (/ 1 3.0))))))

;;; ==============================
(defun css-color:hsl-to-rgb (h s l)
  "Convert H S L to rgb color value.\n
Arguments are as follows:\n
H is the hsl HUE;\n S is the hsl Saturation;\n L is the hsl lightness;\n
:SEE-ALSO `css-color:within-bounds', `css-color:hex-to-rgb',
`css-color:hex-to-hsv', `css-color:hsv-to-hsl', `css-color:hsv-to-hex',
`css-color:hsv-to-rgb', `css-color:rgb-to-hsv', `css-color:rgb-to-hex',
`css-color:rgb-to-hsl', `css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (r g b)
      (css-color:hsl-to-rgb-fractions
       (/ h 360.0)
       (/ s 100.0)
       (/ l 100.0))
    (values (css-color:within-bounds (* 256 r) 0 255)
	    (css-color:within-bounds (* 256 g) 0 255)
	    (css-color:within-bounds (* 256 b) 0 255))))

;;; ==============================
(defun css-color:hsl-to-hex (h s l)
  "Convert H S L to hex color value.\n
H is the hsl HUE;\n S is the hsl Saturation;\n L is the hsl lightness;\n
:SEE-ALSO `css-color:hex-to-rgb', `css-color:hex-to-hsv',
`css-color:hsv-to-hsl', `css-color:hsv-to-hex', `css-color:hsv-to-rgb',
`css-color:rgb-to-hsv', `css-color:rgb-to-hex', `css-color:rgb-to-hsl',
`css-color:hsl-to-rgb', `css-color:hsl-to-hex', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (apply #'css-color:rgb-to-hex (css-color:hsl-to-rgb h s l)))

;;; ==============================
(defun css-color:hue-to-rgb (x y h)
  "Convert X Y H to css-color rgb value.\n
:SEE-ALSO `css-color:hsl-to-rgb-fractions', `css-color:hex-to-rgb',
`css-color:hex-to-hsv', `css-color:hsv-to-hsl', `css-color:hsv-to-hex',
`css-color:hsv-to-rgb', `css-color:rgb-to-hsv', `css-color:rgb-to-hex',
`css-color:rgb-to-hsl', `css-color:hsl-to-rgb', `css-color:hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (when (< h 0) (incf h))
  (when (> h 1) (decf h))
  (cond ((< h (/ 1 6.0)) (+ x (* (- y x) h 6)))
        ((< h 0.5) y)
        ((< h (/ 2.0 3.0)) (+ x (* (- y x) (- (/ 2.0 3.0) h) 6)))
        (t x)))

;;; ==============================
;;; :PREFIX "ccph-"
;;; :ADDED `save-match-data'
;;; :MODIFICATIONS <Timestamp: #{2010-06-07T12:38:23-04:00Z}#{10231} - by MON KEY>
(defun css-color:parse-hsl (hsl-string)
  "Parse `*regexp-css-color-hsl*' matches for HSL-STRING.\n
Return matches for hue saturation luminance as list of numbers.\n
:SEE-ALSO `css-color:string-hsl-to-rgb', `css-color:string-hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (save-match-data
    (string-match *regexp-css-color-hsl* hsl-string)
    (let ((ccph-rtn-hsl-str (mapcar #'string-to-number
                               (list (match-string 1 hsl-string)
                                     (match-string 2 hsl-string)
                                     (match-string 3 hsl-string)))))
      ccph-rtn-hsl-str)))

;;; ==============================
(defun css-color:incr-hsv-hue (hsv-color hsv-hue-incr) 
  "Increment the hue of HSV-COLOR by HSV-HUE-INCR.\n
:SEE-ALSO `css-color:parse-hsl', `css-color:hsv-to-prop-hexstring',
`css-color:incr-hsv-sat', `css-color:incr-hsv-val', `css-color:hsv-hue-up',
`css-color:hsv-value-up', `css-color:hsv-value-down',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (h s v) hsv-color
    (css-color:hsv-to-prop-hexstring (list (+ hsv-hue-incr h) s v))))

;;; ==============================
(defun css-color:incr-hsv-sat (hsv-color hsv-sat-incr)
  "Increment the satruation of HSV-COLOR by HSV-SAT-INCR.\n
:SEE-ALSO `css-color:hsv-to-prop-hexstring', `css-color:within-bounds',
`css-color:incr-hsv-hue', `css-color:incr-hsv-val', `css-color:hsv-saturation-up',
`css-color:hsv-value-up', `css-color:hsv-value-down',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (h s v) hsv-color
    (css-color:hsv-to-prop-hexstring
     (list h (css-color:within-bounds (+ hsv-sat-incr s) 0 100) v))))

;;; ==============================
(defun css-color:incr-hsv-val (hsv-color hsv-val-incr)
  "Increment the satruation of HSV-COLOR by HSV-VAL-INCR.\n
:SEE-ALSO `css-color:hsv-to-prop-hexstring', `css-color:within-bounds',
`css-color:incr-hsv-hue', `css-color:incr-hsv-sat', `css-color:hsv-value-up',
`css-color:hsv-value-up', `css-color:hsv-value-down',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (h s v) hsv-color
    (css-color:hsv-to-prop-hexstring
     (list h s (css-color:within-bounds (+ hsv-val-incr v) 0 100)))))

;;; ==============================
(defun css-color:hexval-beginning ()
  "Go to beginning of a css-color with a hex value.\n
:SEE-ALSO `*css-color:hex-chars*', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (skip-chars-backward *css-color:hex-chars*)
  (when (eq (char-after) 35)
    (forward-char 1)))

;;; ==============================
;;; :PREFIX "ccrcap-"
(defun css-color:repl-color-at-posn (css-color:fun incr-replace)
  "Replace css-color at position with return value of `css-color:fun' incremented
by INCR-REPLACE.\n
:SEE-ALSO `css-color:hexval-beginning', `css-color:get-color-at-point',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (let ((ccrcap-psn (point)))
    (css-color:hexval-beginning)
    (insert (funcall css-color:fun (css-color:get-color-at-point) incr-replace))
    (delete-region (point) (+ (point) 6))
    (goto-char ccrcap-psn)))

;;; ==============================
;;; :PREFIX "ccgcap-"
(defun css-color:get-color-at-point ()
  "Get the css-color at point.\n
:SEE-ALSO `css-color:hexval-beginning', `css-color:hex-to-hsv',
`css-color:repl-color-at-posn', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (save-excursion
    (css-color:hexval-beginning)
    (let ((ccgcap-saved-color (get-text-property (point) 'color-css-type))) ;; 'color)))
      (or ccgcap-saved-color
          ;;(css-color:hex-to-hsv (buffer-substring-no-properties (point) (+ (point) 6)) )))))
	  (css-color:hex-to-hsv (mon-buffer-sub-no-prop (point) (+ (point) 6))  )))))

;;; ==============================
(defun css-color:adjust-hsv-hue-at-posn (hsv-hue-adj-incr)
  "Adjust the hsv hue at position by HSV-HUE-ADJ-INCR.\n
:SEE-ALSO `css-color:repl-color-at-posn', `css-color:incr-hsv-hue',
`css-color:adjust-hsv-sat-at-posn', `css-color:adjust-hsv-hue-at-posn',
`css-color:adjust-hsv-val-at-posn', `css-color:hsv-value-up',
`css-color:hsv-value-down', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:repl-color-at-posn 'css-color:incr-hsv-hue hsv-hue-adj-incr))

;;; ==============================
(defun css-color:adjust-hsv-sat-at-posn (hsv-sat-adj-incr)
  "Adjust the hsv saturation at position by HSV-SAT-ADJ-INCR.\n
:SEE-ALSO `css-color:repl-color-at-posn', `css-color:incr-hsv-sat',
`css-color:adjust-hsv-sat-at-posn', `css-color:adjust-hsv-hue-at-posn',
`css-color:adjust-hsv-val-at-posn', `css-color:hsv-value-up',
`css-color:hsv-value-down', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:repl-color-at-posn 'css-color:incr-hsv-sat hsv-sat-adj-incr))

;;; ==============================
(defun css-color:adjust-hsv-val-at-posn (hsv-val-adj-incr)
 "Adjust the hsv saturation at position by HSV-VAL-ADJ-INCR.\n
:SEE-ALSO `css-color:repl-color-at-posn', `css-color:incr-hsv-val',
`css-color:repl-color-at-posn', `css-color:adjust-hsv-sat-at-posn',
`css-color:adjust-hsv-hue-at-posn', `css-color:adjust-hsv-val-at-posn'
`css-color:hsv-value-up', `css-color:hsv-value-down', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:repl-color-at-posn 'css-color:incr-hsv-val hsv-val-adj-incr))

;;; ==============================
;;; :PREFIX "ccwc-"
(defun css-color:what-channel ()
  "Move backward over the `*css-color:hex-chars*'.\n
:SEE-ALSO `css-color:adjust-hex-at-posn', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (let ((ccwc-psn (point)))
    (prog1
	(/ (skip-chars-backward *css-color:hex-chars*) -2)
      (goto-char ccwc-psn))))

;;; ==============================
;;; :PREFIX "ccahap-"
(defun css-color:adjust-hex-at-posn (incr-hex)
  "Increment the hex css-color at point by INCR-HEX.\n
:SEE-ALSO `css-color:what-channel', `css-color:hexval-beginning',
`css-color:within-bounds', `*css-color:map*', `css-color:rgb-up',
`css-color:rgb-down', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive "p")
  (let ((ccahap-psn (point))
	(ccahap-chnl (css-color:what-channel)))
    (css-color:hexval-beginning)
    (let ((ccahap-rgb (css-color:hex-to-rgb
                ;;(buffer-substring-no-properties (point) (+ 6 (point))) )))
                (mon-buffer-sub-no-prop (point) (+ 6 (point))) )))
      (setf (nth ccahap-chnl ccahap-rgb)
	    (css-color:within-bounds (+ incr-hex (nth ccahap-chnl ccahap-rgb)) 0 255))
      (delete-region (point) (+ 6 (point)))
      (insert (propertize ;(with-no-warnings (format "%02x%02x%02x" ccahap-rgb))
               (ignore-errors ;; (format "%02x%02x%02x" ccahap-rgb))
                 (apply 'format "%02x%02x%02x" ccahap-rgb))
               'keymap *css-color:map* 'css-color nil 'rear-nonsticky t)))
    (goto-char ccahap-psn)))

;;; ==============================
(defun css-color:rgb-up (incr-rgb-val)
  "Increment the rgb hex css-color by INCR-RGB-VAL.\n
:SEE-ALSO `css-color:rgb-down', `css-color:adjust-hex-at-posn',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hex-at-posn incr-rgb-val))

;;; ==============================
(defun css-color:rgb-down (decr-rgb-val)
  "Decrement the rgb hex css-color by DECR-RGB-VAL.\n
:SEE-ALSO `css-color:rgb-up', `css-color:adjust-hex-at-posn',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hex-at-posn (- decr-rgb-val)))

;;; ==============================
(defun css-color:hsv-hue-up (incr-hsv-hue)
  "Increment the hsv hue css-color by INCR-HSV-HUE.\n
:SEE-ALSO `css-color:hsv-hue-down', `css-color:adjust-hue-at-p',
`css-color:incr-hsv-hue', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hsv-hue-at-posn incr-hsv-hue))

;;; ==============================
(defun css-color:hsv-hue-down (decr-hsv-hue)
  "Decrement the hsv hue css-color by DECR-HSV-HUE.\n
:SEE-ALSO `css-color:hsv-hue-up', `css-color:adjust-hue-at-p',
`css-color:incr-hsv-hue', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hsv-hue-at-posn (- decr-hsv-hue))) 

;;; ==============================
(defun css-color:hsv-saturation-up (incr-hsv-sat)
  "Increment the hsv saturation css-color by INCR-HSV-SAT.\n
:SEE-ALSO `css-color:adjust-hsv-sat-at-posn', `css-color:incr-hsv-sat',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hsv-sat-at-posn incr-hsv-sat))

;;; ==============================
(defun css-color:hsv-saturation-down (decr-hsv-sat)
  "Decrement the hsv saturation css-color by DECR-HSV-SAT.\n
:SEE-ALSO `css-color:adjust-hsv-sat-at-posn', `css-color:incr-hsv-sat',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hsv-sat-at-posn (- decr-hsv-sat)))

;;; ==============================
(defun css-color:hsv-value-up (incr-hsv-val)
  "Increment the hsv value css-color by INCR-HSV-VAL.\n
:SEE-ALSO `css-color:adjust-hsv-val-at-posn', `css-color:incr-hsv-val',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hsv-val-at-posn incr-hsv-val))

;;; ==============================
(defun css-color:hsv-value-down (decr-hsv-val)
  "Decrement the hsv value css-color by DECR-HSV-VAL.\n
:SEE-ALSO `css-color:adjust-hsv-val-at-posn', `css-color:incr-hsv-val',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "p")
  (css-color:adjust-hsv-val-at-posn (- decr-hsv-val)))

;;; ==============================
;;; :PREFIX "ccnu-"
(defun css-color:num-up (incr-arg)
  "Increase css-color value by INCR-ARG.\n
:SEE-ALSO `*css-color:generic-map*', `css-color:normalize-hue',
`css-color:rgb-up', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive "p")
  (save-excursion
    (let ((ccnu-digits (concat `(,@(number-sequence 49 57) 48))))
      (skip-chars-backward ccnu-digits)
      (save-match-data
        (when (looking-at "[[:digit:]]+")
          (replace-match
           (propertize
            (let ((ccnu-num (+ (string-to-number (match-string 0)) incr-arg)))
              (save-match-data
                (cond ((looking-at "[[:digit:]]+%")
                       (setq ccnu-num (min ccnu-num 100)))
                      ((looking-back "hsla?(")
                       (setq ccnu-num (css-color:normalize-hue ccnu-num)))
                      ((memq 'color-css-type (text-properties-at (point)))
                       (setq ccnu-num (min ccnu-num 255)))))
              (number-to-string ccnu-num))
            'keymap *css-color:generic-map*)))))))

;;; ==============================
;;; :PREFIX "ccnd-"
(defun css-color:num-down (decr-arg)
  "Decrease css-color value by DECR-ARG.\n
:SEE-ALSO `css-color:normalize-hue', `*css-color:generic-map*',
`css-color:rgb-down', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive "p")
  (save-excursion
    (let ((ccnd-digits (concat `(,@(number-sequence 49 57) 48))))
      (skip-chars-backward ccnd-digits)
      (save-match-data
        (when  (looking-at "[[:digit:]]+")
          (replace-match
           (propertize
            (let ((ccnd-num (- (string-to-number (match-string 0)) decr-arg)))
              (save-match-data
                (cond ((looking-back "hsla?(")
                       (setq ccnd-num (css-color:normalize-hue ccnd-num)))
                      (t (setq ccnd-num (max 0 ccnd-num)))))
              (number-to-string ccnd-num))
            'keymap *css-color:generic-map*)))))))

;;; ==============================
(defun css-color:text-property-color-start ()
  "Skip to beginning of color.\n
Return cons `point' and the color-type text-property.\n
:SEE-ALSO `css-color:text-property-color-end',
`css-color:text-property-color-region', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (while (memq 'color-css-type (text-properties-at (point)))
    (backward-char 1))
  (forward-char 1)
  (cons (point) (plist-get (text-properties-at (point)) 'color-css-type)))

;;; ==============================
(defun css-color:text-property-color-end ()
  "Skip to end of color.\n
Return cons `point' and the color-type text-property.\n
:SEE-ALSO `css-color:text-property-color-start',
`css-color:text-property-color-region', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (while (plist-get (text-properties-at (point)) 'color-css-type)
    (forward-char 1))
  (cons (point) (plist-get (text-properties-at (1- (point))) 'color-css-type)))

;;; ==============================
;;; :PREFIX "cctpcr-"
(defun css-color:text-property-color-region ()
  "Helper function to get the range of color in region as a list.\n
When evaluated with `region-active-p' `desctructoring-bind's return values of
`css-color:text-property-color-end' `css-color:text-property-color-start'.\n
:SEE-ALSO `css-color:cycle-type', `css-color:next-channel',
`css-color:toggle-percentage', `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (destructuring-bind ((cctpcr-beg . cycle-type) (cctpcr-end . cycle-type))
      (list (css-color:text-property-color-start)
            (css-color:text-property-color-end))
    ;; (list beg end cycle-type  (buffer-substring-no-properties cctpcr-beg cctpcr-end))
    (list cctpcr-beg cctpcr-end cycle-type (mon-buffer-sub-no-prop  cctpcr-beg cctpcr-end) )))

;;; ==============================
;;; :CHANGESET 1828
;;; :CREATED <Timestamp: #{2010-06-04T18:57:36-04:00Z}#{10225} - by MON KEY>
(defvar *css-color:string-frob* "css-color:string-%s-to-%s"
  "*A format string evaluated by `css-color:cycle-type' inside an `intern-soft'.\n
Used to conditionally generate a function name b/c we don't know yet know what
`css-color:next-type' will return.\n
Function names so generated include:\n
 `css-color:string-hex-to-hsl', `css-color:string-hsl-to-hex',
 `css-color:string-hsl-to-rgb', `css-color:string-name-to-hex'
 `css-color:string-rgb-to-hex', `css-color:string-rgb-to-name'\n
:SEE-ALSO `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►")
;;
;;;(progn (makunbound '*css-color:string-frob*) (unintern "*css-color:string-frob*" obarray) )

;;; ==============================
(defconst *css-color:type-circle* '#1=(hex hsl rgb name . #1#)
   "Circular list value of css-color types.\n
 :SEE-ALSO `css-color:next-type', `css-color:cycle-type'.\n►►►")
;;
;;;(progn (makunbound '*css-color:type-circle*) (unintern "*css-color:type-circle*" obarray) )

;;; ==============================
(defun css-color:next-type (color-type-sym)
  "Get the COLOR-TYPE-SYM from the circular list `*css-color:type-circle*'.\n
:SEE-ALSO `css-color:cycle-type', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (cadr (member color-type-sym *css-color:type-circle*)))

;;; ==============================
;;; :PREFIX "ccct-"
(defun css-color:cycle-type ()
  "Cycle the the text-properties of the css-color according to its type.\n
:SEE-ALSO `css-color:next-type', `*css-color:type-circle*',
`*css-color:string-frob*', `css-color:text-property-color-region',
`*css-color:map*',`*css-color:generic-map*', `css-color:string-hex-to-hsl',
`css-color:string-hsl-to-hex', `css-color:string-hsl-to-rgb',
`css-color:string-name-to-hex', `css-color:string-rgb-to-hex',
`css-color:string-rgb-to-name', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive)
  (destructuring-bind (ccct-beg ccct-end ccct-cycle-type ccct-color)
      (css-color:text-property-color-region)
    (if (or (= 0 (length ccct-color)) (null ccct-cycle-type))
        (error (concat ":FUNCTION `css-color:cycle-type' "
                       "-- region does not contain a color to cycle")))
    (delete-region ccct-beg ccct-end)
    (insert (propertize
             (funcall (intern-soft
                       (format *css-color:string-frob* ccct-cycle-type
                               (css-color:next-type ccct-cycle-type))) ccct-color)
             'keymap (if (eq (css-color:next-type ccct-cycle-type) 'hex)
                         *css-color:map*
                       *css-color:generic-map*)
             'rear-nonsticky t))
    (goto-char ccct-beg)))

;;; ==============================
(defun css-color:string-hex-to-hsl (hex-color-str->hsl)
  "Convert HEX-COLOR-STR->HSL to hsl value.\n
:SEE-ALSO `css-color:rgb-to-hsl', `css-color:string-hex-to-hsl',
`css-color:cycle-type', `*css-color:string-frob*', `css-color:next-type'
`css-color:string-hex-to-hsl', `css-color:string-hsl-to-hex',
`css-color:string-hsl-to-rgb', `css-color:string-name-to-hex'
`css-color:string-rgb-to-hex', `css-color:string-rgb-to-name',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (h s l)
      (apply #'css-color:rgb-to-hsl (css-color:hex-to-rgb hex-color-str->hsl))
    (format "hsl(%d,%d%%,%d%%)" h s l)))

;;; ==============================
(defun css-color:string-hsl-to-rgb (hsl-color-str->rgb)
  "Convert HSL-COLOR-STR->RGB to rgb value.\n
:SEE-ALSO `css-color:parse-hsl', `css-color:hsl-to-rgb',
`css-color:cycle-type', `*css-color:string-frob*', `css-color:next-type',
`css-color:string-hex-to-hsl', `css-color:string-hsl-to-hex',
`css-color:string-hsl-to-rgb', `css-color:string-name-to-hex',
`css-color:string-rgb-to-hex', `css-color:string-rgb-to-name',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (multiple-value-bind (h s l)
      (css-color:parse-hsl hsl-color-str->rgb)
    (apply #'format "rgb(%d,%d,%d)" 
           (mapcar #'round (css-color:hsl-to-rgb h s l)))))

;;; ==============================
;;; :PREFIX "ccsrtn-"
(defun css-color:string-rgb-to-name (rgb-color-str->name)
  "Convert RGB-COLOR-STR->NAME to css-color name from `*css-color:html-colors*'.\n
:SEE-ALSO `css-color:string-rgb-to-hex', `css-color:string-hex-to-hsl',
`css-color:cycle-type', `*css-color:string-frob*', `css-color:next-type',
`css-color:string-hex-to-hsl', `css-color:string-hsl-to-hex',
`css-color:string-hsl-to-rgb', `css-color:string-name-to-hex',
`css-color:string-rgb-to-hex', `css-color:string-rgb-to-name',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'\n►►►"
  (let ((ccsrtn-c2hex (css-color:string-rgb-to-hex rgb-color-str->name)))
    (or (car (rassoc (list (upcase ccsrtn-c2hex)) *css-color:html-colors*))
        ccsrtn-c2hex)))

;;; ==============================
;;; :WAS (defun css-color:string-name-to-hex (str)
;;;   (let ((str (downcase str)))
;;;     (cadr (assoc-if
;;;            (lambda (a)
;;;              (string= (downcase a) str))
;;;            +*css-color:html-colors*+))))
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-31T13:00:31-04:00Z}#{10133} - by MON KEY>
(defun css-color:string-name-to-hex (color-name-str->hex)
  "Convert the COLOR-NAME-STR->HEX value.\n
:EXAMPLE\n\n\(css-color:string-name-to-hex \"MediumTurquoise\"\)\n
:SEE-ALSO `*css-color:html-colors*',
`css-color:cycle-type', `*css-color:string-frob*', `css-color:next-type',
`css-color:string-hex-to-hsl', `css-color:string-hsl-to-hex',
`css-color:string-hsl-to-rgb', `css-color:string-name-to-hex',
`css-color:string-rgb-to-hex', `css-color:string-rgb-to-name',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (let ((ccsnth-to-hex-myb 
         (assoc-string color-name-str->hex *css-color:html-colors* t)))
    (when ccsnth-to-hex-myb (cadr ccsnth-to-hex-myb))))
;;
;;; :TEST-ME (css-color:string-name-to-hex "MediumTurquoise")

;;; ==============================
;;; :PREFIX "ccsrthx-"
(defun css-color:string-rgb-to-hex (rgb-color-str->hex)
  "Convert the RGB-COLOR-STR->HEX value.\n
:EXAMPLE\n\n\(css-color:string-rgb-to-hex \"rgb\(173,47,93\)\"\)\n
:SEE-ALSO `*regexp-css-color-rgb*', `css-color:rgb-to-hex',
`css-color:cycle-type', `*css-color:string-frob*', `css-color:next-type',
`css-color:string-hex-to-hsl', `css-color:string-hsl-to-hex',
`css-color:string-hsl-to-rgb', `css-color:string-name-to-hex',
`css-color:string-rgb-to-hex', `css-color:string-rgb-to-name',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (save-match-data
    (string-match *regexp-css-color-rgb* rgb-color-str->hex)
    (concat "#"
            (apply #'css-color:rgb-to-hex
                   (mapcar #'(lambda (ccsrthx-L-1) 
                               (if (= (aref ccsrthx-L-1 (1- (length ccsrthx-L-1))) ?\%)
                                   (round (* (string-to-number ccsrthx-L-1) 2.55))
                                 (string-to-number ccsrthx-L-1)))
                    (list (match-string-no-properties 1 rgb-color-str->hex)
                          (match-string-no-properties 2 rgb-color-str->hex)
                          (match-string-no-properties 3 rgb-color-str->hex)))))))
;;
;;; :TEST-ME (css-color:string-rgb-to-hex "rgb(173,47,93)")

;;; ==============================
(defun css-color:string-hsl-to-hex (hsl-color-str->hex)
  "Convert the HSL-COLOR-STR->HEX value.\n
:SEE-ALSO `css-color:hsl-to-hex', `css-color:parse-hsl'
`css-color:cycle-type', `*css-color:string-frob*',
`css-color:next-type', `css-color:string-hex-to-hsl',
`css-color:string-hsl-to-hex', `css-color:string-hsl-to-rgb',
`css-color:string-name-to-hex', `css-color:string-rgb-to-hex',
`css-color:string-rgb-to-name', `mon-help-css-color',
`mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (concat "#" (apply #'css-color:hsl-to-hex (css-color:parse-hsl hsl-color-str->hex))))

;;; ==============================
(defun css-color:next-channel ()
  "Return the next css-color's channel.\n
:SEE-ALSO `css-color:text-property-color-region',
`css-color:text-property-color-end', `css-color:text-property-color-region',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive)
  (multiple-value-bind (beg end cycle-type color)
      (save-excursion (css-color:text-property-color-region))
    (case cycle-type
      ((hsl rgb)
       (if (not (search-forward-regexp ",\\|(" end t))
	   (goto-char (+ beg 4))))
      (hex (cond ((> (point) (- end 3)) (goto-char (+ 1 beg)))
                 ((= (char-after) 35)  (forward-char 1))
                 ((eq (logand (- (point) beg)) 1) (forward-char 1)) 
                 (t (forward-char 2)))))))

;;; ==============================
(defun css-color:hexify-anystring (hexify-str)
  "Try to HEXIFY-STR no matter what its color type.\n
:SEE-ALSO `css-color:string-rgb-to-hex', `css-color:string-hsl-to-hex',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (cond ((string-match "^hsl" hexify-str)
	 (css-color:string-hsl-to-hex hexify-str))
	((string-match "^rgb" hexify-str)
	 (css-color:string-rgb-to-hex hexify-str))
	(t hexify-str)))

;;; ==============================
;;; :PREFIX "cctp-"
(defun css-color:toggle-percentage ()
  "Toggle `css-color-mode' display of colors values in percentages.\n
:SEE-ALSO `*css-color:generic-map*', `css-color:text-property-color-region',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive)
  (let ((cctp-psn (point))) ;; Used?
    (if (eq (nth 2 (save-excursion (css-color:text-property-color-region))) 'rgb)
	(let ((chars (concat `(37 ,@(number-sequence 49 57) 48 46))))
	  (skip-chars-backward chars)
          (save-match-data
            (when (looking-at "[[:digit:]]+\\(?:\.?[[:digit:]]*%\\)?%?")
              (let ((cctp-str (match-string 0)))
                (replace-match (propertize
                                (if (= (aref cctp-str (1- (length cctp-str))) ?\%)
                                    (number-to-string (round (* (string-to-number cctp-str) 2.55)))
                                  (format "%d%%" (/ (string-to-number cctp-str) 2.55)))
                                'keymap *css-color:generic-map* 'rear-nonsticky t))))))
      (message (concat ":FUNCTION `css-color:toggle-percentage' "
                       "-- color-type at point not formatted as rgb(NN,NN,NN) type <SPC>")))))

;;; ==============================
(defvar *css-color:fg-history* nil
  "List used for completion history with HTML color lookups.\n
:SEE-ALSO `css-color:html-color-by-name', `css-color:examine-color'.\n►►►")
;;
;; (setq *css-color:fg-history* nil)

;;; ==============================
;;; :PREFIX "cchtcbn-"
;;; :CHANGESET 1833
;;; :CREATED <Timestamp: #{2010-06-08T17:31:52-04:00Z}#{10232} - by MON KEY>
(defun css-color:html-color-by-name (&optional this-css-color insrtp intrp prompt)
  "Complete an html css color from `*css-color:html-colors*'.\n
Optional arg THIS-CSS-COLOR is a symbol or string name to `*css-color:html-colors*'
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
Optional arg PROMPT is a string for `completing-read'.
Default prompt is: \"CSS color name: \"\n
:EXAMPLE\n\n(css-color:html-color-by-name 'aliceblue)\n
\(css-color:html-color-by-name \"aliceblue\"\)\n
\(css-color:html-color-by-name \"AliceBlue\"\)\n
\(css-color:html-color-by-name\)\n
\(css-color:html-color-by-name\)\n
\(with-temp-buffer \(css-color:html-color-by-name nil nil t) \(buffer-string\)\)\n
:SEE-ALSO `css-color:examine-color', `css-color:html-color-both-cases',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive "i\ni\np")
  (let ((cchtcbn-cmp (if this-css-color
                         (assoc-string this-css-color *css-color:html-colors* t)
                       (assoc-string (completing-read (or prompt "CSS color name: " )
                                                      (css-color:html-color-both-cases)
                                                      nil nil nil '(*css-color:fg-history* . 1)
                                                      (car *css-color:fg-history*))
                                     *css-color:html-colors* t))))
    ;;(when (or intrp prompt) (push (car cchtcbn-cmp) *css-color:fg-history*))
    (if (and cchtcbn-cmp (or insrtp intrp))
        (save-excursion
          (insert (format ":HTML-COLOR-NAME %s\n:RGB-HEX-VALUE   %s" 
                          (car cchtcbn-cmp) (cadr cchtcbn-cmp))))
      cchtcbn-cmp)))
;;
;;; :TEST-ME (css-color:html-color-by-name "aliceblue")
;;; :TEST-ME (with-temp-buffer (apply 'css-color:html-color-by-name "aliceblue" '(nil t)) (buffer-string))

;;; ==============================
;;; :PREFIX "ccec-"
;;; :CHANGESET 1839
;;; :CREATED <Timestamp: #{2010-06-09T21:05:21-04:00Z}#{10233} - by MON KEY>
(defun css-color:examine-color (fg-color bg-color)
  "Test css-colors interactively.\n
The css-colors are displayed in the echo area.\n
One can specify the colors of any viable CSS color.\n
:EXAMPLE\n\n\(css-color:examine-color \"Red\" \"#000\"\)\n
\(css-color:examine-color \"#0C0\" \"#b0ff00\"\)\n
\(css-color:examine-color \"hsla\(100, 50%, 25%\)\" \"rgb\(255,100,120\)\"\)\n
\(call-interactively 'css-color:examine-color\)\n 
:SEE-ALSO `css-color:html-color-by-name', `css-color:hexify-anystring',
`*css-color:bg-history*', `*css-color:fg-history*' `list-colors-display',
`mon-help-css-color', `mon-help-color-functions', `mon-help-color-chart'.\n►►►"
  (interactive (list 
                (car (css-color:html-color-by-name nil nil nil "Foreground color: "))
                (car (css-color:html-color-by-name nil nil nil "Background color: "))))
  (let* ((ccec-str (concat " Foreground: " fg-color ", Background: " bg-color " ")))
    (put-text-property 0 (length ccec-str)
                       'face `(:foreground ,(css-color:hexify-anystring fg-color)
                               :background ,(css-color:hexify-anystring bg-color)) ccec-str)
    (progn
      (minibuffer-message (concat ":FUNCTION `css-color:examine-color' "
                       " -- here are the colors:\n %s") ccec-str)
      (sit-for 2))))

;;; ==============================             
(defun css-color:run-tests ()
  "Test function for :FILE mon-css-color.el\n
Test following features:\n
 `css-color:string-hex-to-hsl' `css-color:string-rgb-to-hex'
 `css-color:string-hsl-to-rgb' `css-color:string-hsl-to-hex'\n
:EXAMPLE\n\n(css-color:run-tests)\n
:SEE-ALSO `mon-help-css-color', `mon-help-color-functions',
`mon-help-color-chart'.\n►►►"
  (interactive)
  (unless
      (progn
	(assert
	 (string= (css-color:string-hex-to-hsl "#ffff00") "hsl(60,100%,50%)"))
	(assert
	 (string= (css-color:string-rgb-to-hex "rgb(255, 50%,   0)")"#ff7f00"))
	(assert
	 (string= (css-color:string-hsl-to-rgb "hsl(60, 100%, 50%)") "rgb(255,255,0)"))
	(assert
	 (string= (css-color:string-hsl-to-hex "hsl(60, 100%, 50%)") "#ffff00")))
    (message (concat ":FUNCTION `css-color:run-tests' "
                     "-- all tests passed"))))

;;; ==============================
;; W3C SVG color keywords
;; :SEE (URL `http://www.w3.org/TR/SVG/types.html#ColorKeywords')

;;; ("aliceblue" "#f0f8ff" (240 248 255)
;;; ("antiquewhite" "#faebd7" (250 235 215)
;;; ("aqua" "#00ffff" (0 255 255)
;;; ("aquamarine" "#7fffd4" (127 255 212)
;;; ("azure" "#f0ffff" (240 255 255)
;;; ("beige" "#f5f5dc" (245 245 220)
;;; ("bisque" "#ffe4c4" (255 228 196)
;;; ("black" "#000000" (0 0 0)
;;; ("blanchedalmond" "#ffebcd" (255 235 205)
;;; ("blue" "#0000ff" (0 0 255)
;;; ("blueviolet" "#8a2be2" (138 43 226)
;;; ("brown" "#a52a2a" (165 42 42)
;;; ("burlywood" "#deb887" (222 184 135)
;;; ("cadetblue" "#5f9ea0" (95 158 160)
;;; ("chartreuse" "#7fff00" (127 255 0)
;;; ("chocolate" "#d2691e" (210 105 30)
;;; ("coral" "#ff7f50" (255 127 80)
;;; ("cornflowerblue" "#6495ed" (100 149 237)
;;; ("cornsilk" "#fff8dc" (255 248 220)
;;; ("crimson" "#dc143c" (220 20 60)
;;; ("cyan" "#00ffff" (0 255 255)
;;; ("darkblue" "#00008b" (0 0 139)
;;; ("darkcyan" "#008b8b" (0 139 139)
;;; ("darkgoldenrod" "#b8860b" (184 134 11)
;;; ("darkgray" "#a9a9a9" (169 169 169)
;;; ("darkgreen" "#006400" (0 100 0)
;;; ("darkgrey" "#a9a9a9" (169 169 169)
;;; ("darkkhaki" "#bdb76b" (189 183 107)
;;; ("darkmagenta" "#8b008b" (139 0 139)
;;; ("darkolivegreen" "#556b2f" (85 107 47)
;;; ("darkorange" "#ff8c00" (255 140 0)
;;; ("darkorchid" "#9932cc" (153 50 204)
;;; ("darkred" "#8b0000" (139 0 0)
;;; ("darksalmon" "#e9967a" (233 150 122)
;;; ("darkseagreen" "#8fbc8f" (143 188 143)
;;; ("darkslateblue" "#483d8b" (72 61 139)
;;; ("darkslategray" "#2f4f4f" (47 79 79)
;;; ("darkslategrey" "#2f4f4f" (47 79 79)
;;; ("darkturquoise" "#00ced1" (0 206 209)
;;; ("darkviolet" "#9400d3" (148 0 211)
;;; ("deeppink" "#ff1493" (255 20 147)
;;; ("deepskyblue" "#00bfff" (0 191 255)
;;; ("dimgray" "#696969" (105 105 105)
;;; ("dimgrey" "#696969" (105 105 105)
;;; ("dodgerblue" "#1e90ff" (30 144 255)
;;; ("firebrick" "#b22222" (178 34 34)
;;; ("floralwhite" "#fffaf0" (255 250 240)
;;; ("forestgreen" "#228b22" (34 139 34)
;;; ("fuchsia" "#ff00ff" (255 0 255)
;;; ("gainsboro" "#dcdcdc" (220 220 220)
;;; ("ghostwhite" "#f8f8ff" (248 248 255)
;;; ("gold" "#ffd700" (255 215 0)
;;; ("goldenrod" "#daa520" (218 165 32)
;;; ("gray" "#808080" (128 128 128)
;;; ("green" "#008000" (0 128 0)
;;; ("greenyellow" "#adff2f" (173 255 47)
;;; ("grey" "#808080" (128 128 128)
;;; ("honeydew" "#f0fff0" (240 255 240)
;;; ("hotpink" "#ff69b4" (255 105 180)
;;; ("indianred" "#cd5c5c" (205 92 92)
;;; ("indigo" "#4b0082" (75 0 130)
;;; ("ivory" "#fffff0" (255 255 240)
;;; ("khaki" "#f0e68c" (240 230 140)
;;; ("lavender" "#e6e6fa" (230 230 250)
;;; ("lavenderblush" "#fff0f5" (255 240 245)
;;; ("lawngreen" "#7cfc00" (124 252 0)
;;; ("lemonchiffon" "#fffacd" (255 250 205)
;;; ("lightblue" "#add8e6" (173 216 230)
;;; ("lightcoral" "#f08080" (240 128 128)
;;; ("lightcyan" "#e0ffff" (224 255 255)
;;; ("lightgoldenrodyellow" "#fafad2" (250 250 210)
;;; ("lightgray" "#d3d3d3" (211 211 211)
;;; ("lightgreen" "#90ee90" (144 238 144)
;;; ("lightgrey" "#d3d3d3" (211 211 211)
;;; ("lightpink" "#ffb6c1" (255 182 193)
;;; ("lightsalmon" "#ffa07a" (255 160 122)
;;; ("lightseagreen" "#20b2aa" (32 178 170)
;;; ("lightskyblue" "#87cefa" (135 206 250)
;;; ("lightslategray" "#778899" (119 136 153)
;;; ("lightslategrey" "#778899" (119 136 153)
;;; ("lightsteelblue" "#b0c4de" (176 196 222)
;;; ("lightyellow" "#ffffe0" (255 255 224)
;;; ("lime" "#00ff00" (0 255 0)
;;; ("limegreen" "#32cd32" (50 205 50)
;;; ("linen" "#faf0e6" (250 240 230)
;;; ("magenta" "#ff00ff" (255 0 255)
;;; ("maroon" "#800000" (128 0 0)
;;; ("mediumaquamarine" "#66cdaa" (102 205 170)
;;; ("mediumblue" "#0000cd" (0 0 205)
;;; ("mediumorchid" "#ba55d3" (186 85 211)
;;; ("mediumpurple" "#9370db" (147 112 219)
;;; ("mediumseagreen" "#3cb371" (60 179 113)
;;; ("mediumslateblue" "#7b68ee" (123 104 238)
;;; ("mediumspringgreen" "#00fa9a" (0 250 154)
;;; ("mediumturquoise" "#48d1cc" (72 209 204)
;;; ("mediumvioletred" "#c71585" (199 21 133)
;;; ("midnightblue" "#191970" (25 25 112)
;;; ("mintcream" "#f5fffa" (245 255 250)
;;; ("mistyrose" "#ffe4e1" (255 228 225)
;;; ("moccasin" "#ffe4b5" (255 228 181)
;;; ("navajowhite" "#ffdead" (255 222 173)
;;; ("navy" "#000080" (0 0 128)
;;; ("oldlace" "#fdf5e6" (253 245 230)
;;; ("olive" "#808000" (128 128 0)
;;; ("olivedrab" "#6b8e23" (107 142 35)
;;; ("orange" "#ffa500" (255 165 0)
;;; ("orangered" "#ff4500" (255 69 0)
;;; ("orchid" "#da70d6" (218 112 214)
;;; ("palegoldenrod" "#eee8aa" (238 232 170)
;;; ("palegreen" "#98fb98" (152 251 152)
;;; ("paleturquoise" "#afeeee" (175 238 238)
;;; ("palevioletred" "#db7093" (219 112 147)
;;; ("papayawhip" "#ffefd5" (255 239 213)
;;; ("peachpuff" "#ffdab9" (255 218 185)
;;; ("peru" "#cd853f" (205 133 63)
;;; ("pink" "#ffc0cb" (255 192 203)
;;; ("plum" "#dda0dd" (221 160 221)
;;; ("powderblue" "#b0e0e6" (176 224 230)
;;; ("purple" "#800080" (128 0 128)
;;; ("red" "#ff0000" (255 0 0)
;;; ("rosybrown" "#bc8f8f" (188 143 143)
;;; ("royalblue" "#4169e1" (65 105 225)
;;; ("saddlebrown" "#8b4513" (139 69 19)
;;; ("salmon" "#fa8072" (250 128 114)
;;; ("sandybrown" "#f4a460" (244 164 96)
;;; ("seagreen" "#2e8b57" (46 139 87)
;;; ("seashell" "#fff5ee" (255 245 238)
;;; ("sienna" "#a0522d" (160 82 45)
;;; ("silver" "#c0c0c0" (192 192 192)
;;; ("skyblue" "#87ceeb" (135 206 235)
;;; ("slateblue" "#6a5acd" (106 90 205)
;;; ("slategray" "#708090" (112 128 144)
;;; ("slategrey" "#708090" (112 128 144)
;;; ("snow" "#fffafa" (255 250 250)
;;; ("springgreen" "#00ff7f" (0 255 127)
;;; ("steelblue" "#4682b4" (70 130 180)
;;; ("tan" "#d2b48c" (210 180 140)
;;; ("teal" "#008080" (0 128 128)
;;; ("thistle" "#d8bfd8" (216 191 216)
;;; ("tomato" "#ff6347" (255 99 71)
;;; ("turquoise" "#40e0d0" (64 224 208)
;;; ("violet" "#ee82ee" (238 130 238)
;;; ("wheat" "#f5deb3" (245 222 179)
;;; ("white" "#ffffff" (255 255 255)
;;; ("whitesmoke" "#f5f5f5" (245 245 245)
;;; ("yellow" "#ffff00" (255 255 0)
;;; ("yellowgreen" "#9acd32" (154 205 50)
;;; ==============================

;;; ==============================
(provide 'css-color)
(provide 'mon-css-color)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ================================================================
;;; mon-css-color.el ends here
;;; EOF

;;; mon-css-complete.el --- complete css attributes and properties

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-31T17:56:42-04:00Z}#{10133} - by MON>
;;; :NOTE this is a highly modified version of Niels Giesen's css-complete.el
;;; :SEE (URL `http://github.com/pft/elisp-assorted/blob/master/css-complete.el')
;;;
;;; Mon modifications, changes, and additions include:
;;; - All constants renamed to variables using unless/bound-and-true-p/setq
;;;   idiom for instead.
;;;
;;; - Lowers the reliance on the Common Lisp `values' and `destructuring-bind'
;;;   forms to equivalent dumbed Emacs Lisp using primitives, subrs, and backquote
;;;   forms instead.
;;;
;;; - Adds five new macros and three new aliases which abstract away some
;;;   redundancies; adds three new functions, and four new variables.
;;;
;;; - Adds docstrings where lacking and cross-package xrefs with
;;;   mon-doc-help-css.el
;;;
;;;
;;; :ADDED   :VARIABLE `*css-complete-pseudo-ids*'
;;; :ADDED   :VARIABLE `*css-complete-property-ids*'
;;; :ADDED   :VARIABLE `*css-complete-at-ids*' 
;;; :ADDED   :VARIABLE `*css-complete-look-back-regexp*'
;;; :RENAMED :VARIABLE `css-tag-ids'   -> `*css-complete-tag-ids*'
;;; :RENAMED :VARIABLE `css-media-ids' -> `*css-complete-tag-ids*'
;;; :RENAMED :VERIABLE `css-props-and-vals' -> `*css-complete-collect-props-and-vals*'
;;; :RENAMED :VARIABLE `*css-popup-pos-x-offset*' -> `*css-complete-popup-pos-x-offset*'
;;; :CHANGED :FUNCTION `css-delims-maybe-part-val-at-point'  
;;; :CHANGED :VARIABLE `css-tag-ids' --  reflowed list formatting.
;;; :CHANGED :VARIABLE `css-media-ids' -- incorporated media-ids defined in Emacs 23.2 css-mode.el 
;;; :CHANGED :VARIABLE `css-props-and-vals' reflowed list formatting.
;;; :RENAMED :MACRO `collect' -> `css-complete-collect'
;;; :CHANGED :MACRO `css-complete-collect' -- Now uses `mapc' instead of `loop'
;;; :ADDED   :FUNCTION `css-delims-maybe-part-generic'
;;; :ADDED   :MACRO     `css-delete-partial'
;;; :CHANGED :FUNNCTION `css-delete-partial-prop', `css-delete-partial-tag',
;;;                     `css-delete-partial-pseud', `css-delete-partial-at',
;;;                     `css-delete-partial-value', `css-maybe-part-val-at-point',
;;;                     `css-maybe-part-prop-at-point', `css-maybe-part-pseudo-at-point',
;;;                     `css-maybe-part-tag-at-point', `css-maybe-part-at-at-point',
;;;          -- Now implemented with macro `css-delete-partial' instead of dbind.
;;; :CHANGED :FUNCTION `css-delims-maybe-part-at-at-point', `css-delims-maybe-part-pseudo-at-point',
;;;                    `css-delims-maybe-part-prop-at-point', 
;;;          -- Now aliases of `css-delims-maybe-part-generic'
;;; :ADDED :MACRO `css-complete-menu-generate'
;;; :CHANGED :FUNCTION `css-media-completion-menu', `css-at-completion-menu',
;;;                    `css-value-completion-menu', `css-prop-completion-menu',
;;;                   `css-pseudo-completion-menu', `css-tag-completion-menu'
;;;         -- Now evalauate macro `css-complete-menu-generate'.
;;; :ADDED   :MACRO    `css-delims-maybe-generate'
;;; :CHANGED :FUNCTION `css-delims-maybe-part-generic', `css-delims-maybe-part-val-at-point',
;;;                    `css-delims-maybe-part-tag-at-point', 
;;;           -- Now evaluate macro `css-delims-maybe-generator'
;;; :ADDED :MACRO `css-popup-generate'
;;; :CHANGED :FUNCTIONS `css-value-popup-completions', `css-at-popup-completions',
;;;                     `css-at-popup-completions', `css-media-popup-completions',
;;;           -- Now evaluate macro `css-popup-generate'
;;; :ADDED :MACRO `css-at-p-generate'
;;; :CHANGED :FUCNTION `css-after-at-p', `css-at-pseudo-id-p', `css-at-value-p', `css-at-prop-p',
;;;                    `css-at-tag-id-p', `css-at-at-id-p', `css-complete-after-at',
;;;           -- Now evaluate macro `css-at-p-generate'
;;; :ADDED   :FUNCTION `css-media-for-point'
;;; :ADDED   :FUNCTION `mon-css-complete-loadtime'
;;;           -- Now pass char-classes instead of big string
;;; :CHANGED :FUNCTION `css-at-prop-p', `css-at-value-p', `css-prop-for-point',
;;;                    `css-at-tag-id-p', `css-at-at-id-p', `css-after-at-p'
;;;           -- Now access look back regexp by aref idx into constant `*css-complete-look-back-regexp*'
;;; :RENAMED parameters STR -> CSS-CMP-STR
;;; :CHANGED :FUNCTION `css-possible-pseudo-completions', `css-possible-prop-completions',
;;;                    `css-possible-tag-completions', `css-possible-value-completions',
;;;                    `css-possible-at-completions'
;;;            -- Now use `string-match-p' instead of `string-match'
;;; :CHANGED :FUNCTION `css-complete-after-at' 
;;;           -- Now let-binds `save-match-data' and only returns if the search
;;;              was non-nil. Also, no longer invokes read to get at the return
;;;              vale for `case' now evaluated inside a `cond' form instead.
;;;
;;; :NOTE How much (if any) of the features below can be replaced/refactored with:
;;;       `completion-in-region', `test-completion', `lazy-completion-table',
;;;       `completion-table-in-turn', `completion-table-dynamic',
;;;       `completion-table-with-predicate', `completion-table-with-terminator',
;;;       `completion-table-with-context', `complete-with-action',
;;;       `completion-boundaries', `completion--some',
;;;
;;; ==============================

;;; ==============================
;;; css-complete.el --- complete css attributes and properties

;; Copyright (C) 2009  niels giesen

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Keywords: css, web, html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; COMMENTARY:

;; This is not finished, but I do use it in daily life. You might want to
;; disable the electric- commands. For lack of time & a true parser, completion
;; does not always work correctly, e.g. when a comment is present before point
;; and within the same css selector section.


;;; USAGE:

;; Press C-RET to complete attribute or property at point.


;; TODO: 
;;
;; Add real parser. 
;;
;; Define / generate rules/lists for things such as border-style,
;; number, percentage, color, in other words: resolve choice symbols
;; in the `*css-complete-props-and-vals*' list

;;; Code:

(require 'css-mode)

;;; ==============================
;;; :NOTE We don't defvaralias this because we may want to change adapt it later.
;;; :CHANGESET 2025
;;; :CREATED <Timestamp: #{2010-08-02T13:10:40-04:00Z}#{10311} - by MON>
;;; :WAS (defconst *css-complete-at-ids* nil
(defvar *css-complete-at-ids* nil
  "CSS identifiers that appear in the form @foo.\n
:EXAMPLE\n\n\(member \"page\" *css-complete-at-ids*\)\n
:CALLED-BY `css-ats-for-point'\n
:SEE-ALSO `*css-complete-props-and-vals*', `*css-complete-at-ids*',
`*css-complete-media-ids*', `*css-complete-property-ids*',
`*css-complete-pseudo-ids*', `*css-complete-tag-ids*',
`*css-complete-look-back-regexp*', `*css-complete-popup-pos-x-offset*',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►")
;; 
(unless (bound-and-true-p *css-complete-at-ids*)
  (if (and (intern-soft "css-at-ids")
           (bound-and-true-p css-at-ids)
           (symbol-file 'css-at-ids 'defvar)
           (equal (file-name-nondirectory 
                   (file-name-sans-extension 
                    (or (symbol-file 'css-at-ids 'defvar) "")))
                  "css-mode"))
      (setq *css-complete-at-ids* css-at-ids)
    (setq *css-complete-at-ids* '("charset" "font-face" "import" "media" "page"))))

;; :WAS `css-tag-ids'
(defvar *css-complete-tag-ids* nil
  "A list of CSS tag ids.\n
:EXAMPLE\n\n\(member \"script\" *css-complete-tag-ids*\)\n
:CALLED-BY `css-tags-for-point'\n
:SEE-ALSO `*css-complete-props-and-vals*', `*css-complete-at-ids*',
`*css-complete-media-ids*', `*css-complete-property-ids*',
`*css-complete-pseudo-ids*', `*css-complete-tag-ids*',
`*css-complete-look-back-regexp*', `*css-complete-popup-pos-x-offset*',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►")
;;
(unless (bound-and-true-p *css-complete-tag-ids*)
  (setq *css-complete-tag-ids*
        '("html"
          ;;
          "del" "ins" "abbr" "acronym" "fieldset" "blockquote" "q" "code" "samp"
          "cite" "kbd" "var" "dfn" "address" "em" "strong" "pre"
          ;;
          "hr" "sup" "sub" "font" "basefont" "br" "big" "small" "strike" "u" "i"
          "b" "s" "tt" "center" "bdo"
          ;;
          "script" "noscript" "object" "applet"
          "iframe" "p" "div" "span" "h6" "h5" "h4" "h3" "h2" "h1"
          "isindex" "label" "button" "option" "select" "input" "textarea" "form"
          "dt" "dd" "li" "dir" "menu" "ol" "dl" "ul"
          "a"
          "img" "map"
          "table" "tr" "th" "td" "caption" "col" "colgroup" "thead" "tbody" "tfoot"
          ;;
          "base" "style" "link" "head" "body" "frame" "frameset" "noframes"
          "isindex" "nextid" "meta" "title")))

;;; ==============================
;; :NOTE As of Emacs 23.2.1 The constant `css-media-ids' defined in :FILE css-complete.el 
;; differs from the one in lisp/textmodes/css-mode.el
;;
;; (defconst css-media-ids ;; :FILE lisp/textmodes/css-mode.el 
;;   '("all" "aural" "bitmap" "continuous" "grid" "paged" "static" "tactile"
;;     "visual") ;; :MISSING "embossed" "handheld" "print" "projection" "screen" "tty" "tv"
;;   "Identifiers for types of media.")
;;
;; (defconst css-media-ids ;; :FILE css-complete.el
;; :WAS '("all" "aural" "braille" "embossed" "handheld" "print" "projection" "screen"
;;    "tty" "tv")  
;; Following form returns the set union of the two:
;; (delete-dups
;;  '(;; css-mode.el
;;    "all" "aural" "bitmap" "continuous" "grid" "paged" "static" "tactile" "visual" 
;;    ;; css-complete.el
;;    "all" "aural" "braille" "embossed" "handheld" "print" "projection" "screen" "tty" "tv")) 


;; :WAS `css-media-ids'
;;      '("all" "aural" "braille" "embossed" "handheld" "print" 
;;        "projection" "screen"   "tty"      "tv")  
(defvar *css-complete-media-ids* nil
  "A list of CSS media ids.\n
:NOTE When IS-MON-SYSTEM-P returns non-nil value of `css-media-ids' is rebound
to value of `*css-complete-media-ids*' at loadtime with `mon-css-complete-loadtime'.\n
:SEE (URL `http://www.w3.org/TR/CSS2/media.html')\n
:SEE-ALSO `*css-complete-props-and-vals*', `*css-complete-at-ids*',
`*css-complete-media-ids*', `*css-complete-property-ids*',
`*css-complete-pseudo-ids*', `*css-complete-tag-ids*',
`*css-complete-look-back-regexp*', `*css-complete-popup-pos-x-offset*',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►")
;;
(unless (bound-and-true-p *css-complete-media-ids*)
  (setq *css-complete-media-ids*
        '("all" "aural" "bitmap" "continuous" "grid" 
          "paged" "static" "tactile" "visual" "braille" 
          "embossed" "handheld" "print" "projection" 
          "screen" "tty" "tv")))

;;; ==============================
;;; :CHANGESET 2025
;;; :CREATED <Timestamp: #{2010-08-03T21:40:11-04:00Z}#{10312} - by MON>
(defvar *css-complete-property-ids* nil
  "A list of CSS property ids.\n
:NOTE Value is according to return value `css-property-ids'.
:SEE :FILE css-mode.el\n
:CALLED-BY `css-props-for-point'\n
:SEE-ALSO `*css-complete-props-and-vals*', `*css-complete-at-ids*',
`*css-complete-media-ids*', `*css-complete-property-ids*',
`*css-complete-pseudo-ids*', `*css-complete-tag-ids*',
`*css-complete-look-back-regexp*', `*css-complete-popup-pos-x-offset*',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►")
;;
(unless (bound-and-true-p *css-complete-property-ids*)
  (setq *css-complete-property-ids* css-property-ids))

;;; ==============================
;;; :CHANGESET 2025
;;; :CREATED <Timestamp: #{2010-08-03T21:47:56-04:00Z}#{10312} - by MON>
(defvar *css-complete-pseudo-ids* nil
  "A list of CSS pseudo ids.\n
:NOTE Value is according to return value `css-pseudo-ids'.
:SEE :FILE css-mode.el\n
:CALLED-BY `css-pseudos-for-point'\n
:SEE-ALSO `*css-complete-props-and-vals*', `*css-complete-at-ids*',
`*css-complete-media-ids*', `*css-complete-property-ids*',
`*css-complete-pseudo-ids*', `*css-complete-tag-ids*',
`*css-complete-look-back-regexp*', `*css-complete-popup-pos-x-offset*',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►")
;;
(unless (bound-and-true-p *css-complete-pseudo-ids*)
  (setq *css-complete-pseudo-ids* css-pseudo-ids))

;;; ==============================
;; :WAS `css-props-and-vals'
(defvar *css-complete-props-and-vals* nil  
  "List of lists mapping CSS properties to their possible values.\n
Each list has the form:\n
 (<PROPERTY> <PROPERTY-VALUE>*)\n
Unless indicated below <PROPERTY-VALUE> occur as elisp strings in a <PROPERTY> list
these strings are what W3C-TR calls \"keyword values\".\n
Following <PROPERTY-VALUE> elements occur as symbols within a <PROPERTY> list.\n
W3C-TR's exposition of CSS2 property \"syntax\" enumerates a number of `basic
data types' a property-value may take, these include:\n
 percentage length integer number string uri counter shape
 frequency generic-voice specific-voice relative-size absolute-size
 generic-family family-name margin-width angle identifier time\n
Likewise, certain property-value types may have a range of values as a property
bearing the same name, these include:\n
 color border-style border-width\n
:EXAMPLE\n\n\(assoc-string \"z-index\" *css-complete-props-and-vals*\)\n
:SEE (URL `http://www.w3.org/TR/CSS2/syndata.html#values')
:SEE (URL `http://www.w3.org/TR/CSS2/propidx.html')
:SEE-ALSO `*css-complete-props-and-vals*', `*css-complete-at-ids*',
`*css-complete-media-ids*', `*css-complete-property-ids*',
`*css-complete-pseudo-ids*', `*css-complete-tag-ids*',
`*css-complete-look-back-regexp*', `*css-complete-popup-pos-x-offset*',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►")
;;
(unless (bound-and-true-p *css-complete-props-and-vals*)
  (setq *css-complete-props-and-vals*
        '(("z-index" integer "auto")
          ("word-spacing" length "normal")
          ("width" "auto" percentage length)
          ("widows" integer)
          ("white-space" "pre-line" "pre-wrap" "nowrap" "pre" "normal")
          ("volume" "x-loud" "loud" "medium" "soft" "x-soft" "silent" percentage number)
          ("voice-family" generic-voice specific-voice generic-voice specific-voice)
          ("visibility" "collapse" "hidden" "visible")
          ("vertical-align" length percentage "text-bottom" "bottom" "middle"
           "text-top" "top" "super" "sub" "baseline")
          ("unicode-bidi" "bidi-override" "embed" "normal")
          ("top" "auto" percentage length)
          ("text-transform" "none" "lowercase" "uppercase" "capitalize")
          ("text-indent" percentage length)
          ("text-decoration" "blink" "line-through" "overline" "underline" "none")
          ("text-align" "justify" "center" "right" "left")
          ("table-layout" "fixed" "auto")
          ("stress" number) ;; :NOTE Implemented by Emacspeak
          ("speech-rate" "slower" "faster" "x-fast" "fast" "medium" "slow" "x-slow" number)
          ("speak" "spell-out" "none" "normal")
          ("speak-punctuation" "none" "code")
          ("speak-numeral" "continuous" "digits")
          ("speak-header" "always" "once")
          ("right" "auto" percentage length)
          ("richness" number) ;; :NOTE Implemented by Emacspeak
          ("quotes" "none" string string)
          ("position" "fixed" "absolute" "relative" "static")
          ("play-during" "none" "auto" "repeat" "mix" uri)
          ("pitch" "x-high" "high" "medium" "low" "x-low" frequency) ;; frequency -> {Hz|Khz} ;; :NOTE Implemented by Emacspeak
          ("pitch-range" number) ;; :NOTE Implemented by Emacspeak
          ("pause" percentage time)
          ("pause-before" percentage time)
          ("pause-after" percentage time)
          ("page-break-inside" "auto" "avoid")
          ("page-break-before" "right" "left" "avoid" "always" "auto")
          ("page-break-after" "right" "left" "avoid" "always" "auto")
          ("padding" padding-width)
          ("padding-left" padding-width)
          ("padding-bottom" padding-width)
          ("padding-right" padding-width)
          ("padding-top" padding-width)
          ("overflow" "auto" "scroll" "hidden" "visible")
          ("outline" border-width border-style "invert" color)
          ("outline-width" border-width)
          ("outline-style" border-style)
          ("outline-color" "invert" color)
          ("orphans" integer)
          ("min-width" percentage length)
          ("min-height" percentage length)
          ("max-width" "none" percentage length)
          ("max-height" "none" percentage length)
          ("margin" margin-width)
          ("margin-bottom" margin-width)
          ("margin-top" margin-width)
          ("margin-left" margin-width)
          ("margin-right" margin-width)
          ("list-style" "none" uri "outside" "inside" "upper-alpha" "lower-alpha"
           "georgian" "armenian" "upper-latin" "lower-latin" "lower-greek"
           "upper-roman" "lower-roman" "decimal-leading-zero" "decimal" "square"
           "circle" "disc")
          ;; 
          ("list-style-type" "none" "upper-alpha" "lower-alpha" "georgian" "armenian"
           "upper-latin" "lower-latin" "lower-greek" "upper-roman" "lower-roman"
           "decimal-leading-zero" "decimal" "square" "circle" "disc")
          ("list-style-position" "outside" "inside")
          ("list-style-image" "none" uri)
          ("line-height" percentage length number "normal")
          ("letter-spacing" length "normal")
          ("left" "auto" percentage length)
          ("height" "auto" percentage length)
          ("font" "status-bar" "small-caption" "message-box" "menu" "icon" "caption" 
           generic-family family-name percentage length relative-size absolute-size 
           "900" "800" "700" "600" "500" "400" "300" "200" "100" "lighter" "bolder"
           "bold" "normal" "small-caps" "oblique" "italic")
          ("font-weight" "900" "800" "700" "600" "500" "400" "300" "200" "100"
           "lighter" "bolder" "bold" "normal")
          ("font-variant" "small-caps" "normal")
          ("font-style" "oblique" "italic" "normal")
          ("font-size" percentage length relative-size absolute-size)
          ("font-family" generic-family family-name generic-family family-name) 
          ;;  generic-family -> (serif sans-serif cursive fantasy monospace)
          ;; :SEE (URL `http://www.w3.org/TR/CSS2/changes.html#q383')
          ("float" "none" "right" "left")
          ("empty-cells" "hide" "show")
          ("elevation" "lower" "higher" "above" "level" "below" angle)
          ("display" "none" "table-caption" "table-cell" "table-column"
           "table-column-group" "table-row" "table-footer-group" "table-header-group"
           "table-row-group" "inline-table" "table" "inline-block" "run-in" "list-item"
           "block" "inline") 
          ("direction" "rtl" "ltr")
          ("cursor" "progress" "help" "wait" "text" "w-resize" "s-resize" "sw-resize"
           "se-resize" "n-resize" "nw-resize" "ne-resize" "e-resize" "move" "pointer"
           "default" "crosshair" "auto" uri)
          ("cue" "none" uri)
          ("cue-before" "none" uri)
          ("cue-after" "none" uri)
          ("counter-reset" "none" integer identifier)
          ("counter-increment" "none" integer identifier)
          ("content" "no-close-quote" "no-open-quote" "close-quote" "open-quote"
           identifier "attr" counter uri string "none" "normal")
          ("color" color)
          ("clip" "auto" shape) ;; :NOTE <SHAPE> can only have the form {clip rect(<top>, <right>, <bottom>, <left>;}
          ("clear" "both" "right" "left" "none")
          ("caption-side" "bottom" "top")
          ("bottom" "auto" percentage length)
          ("border" "transparent" color border-style border-width)
          ("border-width" border-width)
          ("border-left-width" border-width)
          ("border-bottom-width" border-width)
          ("border-right-width" border-width)
          ("border-top-width" border-width)
          ("border-left-style" border-style)
          ("border-bottom-style" border-style)
          ("border-right-style" border-style) 
          ("border-top-style" border-style)
          ("border-left-color" "transparent" color)
          ("border-bottom-color" "transparent" color)
          ("border-right-color" "transparent" color)
          ("border-top-color" "transparent" color)
          ("border-left" border-style border-width)
          ("border-bottom" border-style border-width)
          ("border-right" border-style border-width)
          ("border-top" border-style border-width)
          ("border-style" border-style)
          ("border-spacing" length length)
          ("border-color" "transparent" color)
          ("border-collapse" "separate" "collapse")
          ("background" "bottom" "center" "top" "right" "left" length percentage
           "fixed" "scroll" "no-repeat" "repeat-y" "repeat-x" "repeat" "none" uri
           "transparent" color)
          ("background-repeat" "no-repeat" "repeat-y" "repeat-x" "repeat")
          ("background-position" "bottom" "center" "top" "right" "center" "left"
           "bottom" "center" "top" length percentage "right" "center" "left" length
           percentage)
          ("background-image" "none" uri)
          ("background-color" "transparent" color)
          ("background-attachment" "fixed" "scroll")
          ("azimuth" "rightwards" "leftwards" "behind" "right-side" "far-right"
           "right" "center-right" "center" "center-left" "left" "far-left" "left-side"
           angle))))

;;; ==============================
;;; :CHANGESET 2025
;;; :CREATED <Timestamp: #{2010-08-02T19:27:39-04:00Z}#{10311} - by MON>
(defvar *css-complete-look-back-regexp* nil
  "A 5 element array holding regexps.\n
Elts of array map as follows:\n
:IDX :FUNCTION               :REGEXP
  0  `css-at-prop-p'         \"[;{][[:space:]]*[a-z-]*\"
  0  `css-at-value-p'        \"[;{][[:space:]]*[a-z-]*\"
  1  `css-prop-for-point'    \"[;[:space:]]\\\\([a-z-]+\\\\):\" 
  2  `css-at-tag-id-p'       \"\\\\(^\\\\|,\\\\)[[:space:]]*[a-z]*[0-9]?\"
  3  `css-at-at-id-p'        \":[a-z-]*\"
  4  `css-after-at-p'        \"@\\\\(?:charset\\\\|font-face\\\\|import\\\\|media\\\\|page\\\\)[[:space:]]+\"
  5  `css-complete-after-at' \"@\\\\(charset\\\\|font-face\\\\|import\\\\|media\\\\|page\\\\)[[:space:]]+\"\n
:EXAMPLE\n\n\(aref *css-complete-look-back-regexp* 2\)\n
:SEE-ALSO `*css-complete-props-and-vals*', `*css-complete-at-ids*',
`*css-complete-media-ids*', `*css-complete-property-ids*',
`*css-complete-pseudo-ids*', `*css-complete-tag-ids*',
`*css-complete-look-back-regexp*', `*css-complete-popup-pos-x-offset*',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►")
;;
(unless (bound-and-true-p *css-complete-look-back-regexp*)
  (setq *css-complete-look-back-regexp*
        (vector 
         ;; 0 `css-at-prop-p',  `css-at-value-p'
         "[;{][[:space:]]*[a-z-]*"             
         ;; 1 `css-prop-for-point'
         "[;[:space:]]\\([a-z-]+\\):"          
         ;; 2 `css-at-tag-id-p'
         "\\(^\\|,\\)[[:space:]]*[a-z]*[0-9]?"
         ;; 3 `css-at-at-id-p'
         ":[a-z-]*"
         ;; 4 `css-after-at-p'
         "@\\(?:charset\\|font-face\\|import\\|media\\|page\\)[[:space:]]+" 
         ;; 5 `css-complete-after-at'
         "@\\(charset\\|font-face\\|import\\|media\\|page\\)[[:space:]]+")))
        
;;; ==============================
;; :WAS `css-popup-pos-x-offset'
(defvar *css-complete-popup-pos-x-offset* nil
  "*A position offset for `x-popup-menu'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►")
;;
(unless (bound-and-true-p *css-complete-popup-pos-x-offset*)
  (setq *css-complete-popup-pos-x-offset*
        (case window-system
          (w32 110)
          (x 40))))

;; :DEBUGGING
 ;; (dolist (untrn 
 ;;          '(*css-complete-props-and-vals* *css-complete-at-ids* 
 ;;            *css-complete-media-ids* *css-complete-property-ids* 
 ;;            *css-complete-pseudo-ids* *css-complete-tag-ids*
 ;;            *css-complete-look-back-regexp* 
 ;;            *css-complete-popup-pos-x-offset*))
 ;;          (makunbound untrn)
 ;;          (unintern untrn))


;;; ==============================
;;; :NOTE This now uses `mapc' instead of `loop'.
;;; :CHANGESET 2041
;;; :CREATED <Timestamp: #{2010-08-05T17:29:15-04:00Z}#{10314} - by MON KEY>
(defmacro css-complete-collect (css-test-fun css-collect-list)
  "Return items in CSS-COLLECT-LIST for which CSS-TEST-FUN returns non-nil.\n
:EXAMPLE\n\n\(progn
  \(pp-macroexpand-expression
   '\(css-complete-collect stringp
      \(cdr \(assoc \"vertical-align\" *css-complete-props-and-vals*\)\)\)\)
  \(with-current-buffer \(get-buffer\"*Pp Macroexpand Output*\"\)
    \(save-excursion 
      \(end-of-defun\)
      \(insert \";; :NOTE Macro expansion ends here\\n\)\"\)
      \(goto-char \(point-min\)\)
      \(insert \"\(save-excursion \(end-of-line 4\)\\n\"
              \";; :NOTE Above form is for example purposes only.\\n\"
              \";;       The actual macro expansion is as follows:\\n\" \)
      ;\(goto-char \(buffer-end 0\)\) \(insert-parentheses 3\)
      \(goto-char \(buffer-end 0\)\) \(indent-pp-sexp\)
      \(goto-char \(buffer-end 1\)\)
      \(insert \"\\n     some-css-thing:  with-val;\\n     vertical-align:\"\)\)\)\)\n
:CALLED-BY `css-vals-for-prop'
:CALLED-BY `css-possible-value-completions'
:CALLED-BY `css-possible-prop-completions'
:CALLED-BY `css-possible-pseudo-completions'
:CALLED-BY `css-possible-tag-completions'
:CALLED-BY `css-possible-at-completions'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS
  ;;  (declare (indent 1) (debug t))
  ;; `(loop for css-gthr-v in ,css-collect-list
  ;;        when (funcall ,css-test-fun css-gthr-v)
  ;;        collect css-gthr-v))
  (declare (indent 1) (debug t))
  (let ((rtn-css-clct 
         (make-symbol 
          (format "rtn-css-clct%d"
                  (prog1 
                      *gensym-counter* 
                    (setq *gensym-counter* (1+ *gensym-counter*)))))))
    `(let (,rtn-css-clct)
       (mapc #'(lambda (css-v)
                 (when (funcall #',css-test-fun css-v)
                   (push css-v ,rtn-css-clct)))
             ,css-collect-list)
       (unless (null ,rtn-css-clct)
         (setq ,rtn-css-clct (nreverse ,rtn-css-clct))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-02T21:27:58-04:00Z}#{10311} - by MON>
(defmacro css-delete-partial (w-region-fncn w-applied-fncn)
  "Apply W-APPLIED-FNCN to results returned by W-REGION-FNCN.\n
W-REGION-FNCN is a function returning a two elt list of the form:
 ( <REGION-END> <REGION-BEG> )\n
W-APPLIED-FNCN is a function to apply to the returned list. 
Its first to args should be a region beggining and end.\n
:EXAMPLE\n\n\(pp-macroexpand-expression
 '\(css-delete-partial css-delims-maybe-part-val-at-point delete-region\)\)\n
\(save-excursion 
  \(end-of-line 3\)
  \(ignore-errors
  \(css-delete-partial css-delims-maybe-part-val-at-point kill-region\)\)
  \(list :KILLED \(concat \(vconcat \(car kill-ring\)\)\)\)\)\n
18--s-11a-aiss-\n
:SEE-ALSO `css-delete-partial-prop', `css-delete-partial-tag',
`css-delete-partial-pseud', `css-delete-partial-at',
`css-delete-partial-value', `css-maybe-part-val-at-point',
`css-maybe-part-prop-at-point', `css-maybe-part-pseudo-at-point',
`css-maybe-part-tag-at-point', `css-maybe-part-at-at-point'.\n►►►"
  (declare (indent 1) (debug t))
  (let ((css-rgn (make-symbol "css-rgn")))
    `(let ((,css-rgn (nreverse (funcall ',w-region-fncn))))
       (apply ',w-applied-fncn ,css-rgn))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-03T22:12:50-04:00Z}#{10312} - by MON>
(defmacro css-complete-menu-generate (menu-str-1 menu-str-2 psbl-fncn)
  "Return a two elt list of with menu titles MENU-STR-1 MENU-STR-2
The cdr of second list elt are conses generated with psbl-fncn.\n
Return value has the form:
 \(\"MENU-STR-1\" \(\"MENU-STR-2\" \(\"CONS\" .\"PAIRS\"\)* \)\)\n
:EXAMPLE\n\n\(pp-macroexpand-expression
   '\(css-complete-menu-generate \"Medium: \" \"Choose\" css-media-for-point\)\)\n
\(css-complete-menu-generate \"Medium: \" \"Choose\" css-media-for-point\)\n
:SEE-ALSO `css-media-completion-menu', `css-at-completion-menu',
`css-value-completion-menu', `css-prop-completion-menu',
`css-pseudo-completion-menu', `css-tag-completion-menu',
`mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (declare (indent 1) (debug t))
  `(list ,menu-str-1 
         (cons ,menu-str-2
               (mapcar #'(lambda (ccmg-c) 
                           (cons ccmg-c ccmg-c))
                       (funcall ',psbl-fncn)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-04T16:49:06-04:00Z}#{10313} - by MON>
(defmacro css-delims-maybe-generate (skip-fncn skip-arg)
  "Return a two elt list of buffer positions from point 
to point returned after skipping chars with SKIP-FNCN by SKIP-ARG.\n
SKIP-FNCN a function to skip over a range with.\n
SKIP-ARG a string to skip over.\n
:EXAMPLE\n\n\(pp-macroexpand-expression 
 '\(css-delims-maybe-generate skip-chars-backward \"[:lower:]-\"\)\)\n
\(save-excursion
  \(end-of-line 3\)
  \(css-delims-maybe-generate skip-chars-backward \"[:lower:]-\"\)\)\n
abcd-aasos\n
:SEE-ALSO `css-delims-maybe-part-generic', `css-delims-maybe-part-val-at-point',
`css-delims-maybe-part-tag-at-point', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (declare (indent 1) (debug t))
  `(save-excursion
     `(,(point)
       ,(progn 
          (funcall ',skip-fncn ,skip-arg)
          (point)))))


;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-04T17:05:16-04:00Z}#{10313} - by MON>
(defmacro css-popup-generate (completion-menu-fncn)
  "Generate an `x-popup-menu' with COMPLETION-MENU-FNCN.\n
Return menu at postion retrurned by `css-pos-for-x-popup-menu'.\n
COMPLETION-MENU-FNCN is a function whith returns a completion menu.\n
:EXAMPLE\n\n\(pp-macroexpand-expression 
 '\(css-popup-generate css-value-completion-menu\)\)\n
\(save-excursion \(end-of-line 3\)
 \(css-popup-generate css-value-completion-menu\)\)\n
 position:\n
:SEE-ALSO `css-value-popup-completions', `css-at-popup-completions',
`css-at-popup-completions', `css-media-popup-completions',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►"
  (declare (indent 1) (debug t))
  `(x-popup-menu (funcall 'css-pos-for-x-popup-menu)
                 (,completion-menu-fncn)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-04T19:24:54-04:00Z}#{10313} - by MON>
(defmacro css-at-p-generate (look-back-aref)
  "Return non-nil when looking-back at LOOK-BACK-AREF in
`*css-complete-look-back-regexp*'.\n
:EXAMPLE\n\n\(pp-macroexpand-expression '\(css-at-p-generate 4\)\)\n
\(save-excursion \(end-of-line 3\) \(css-at-p-generate 4\)\)\n
@charset \n
:SEE-ALSO `css-at-value-p', `css-after-at-p', `css-at-tag-id-p',
`css-at-at-id-p', `css-at-prop-p', `css-complete-after-at',
`mon-help-css-complete', `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-check', `mon-help-css-color'.\n►►►"
  (declare (indent 1) (debug t))
  (let ((lb-msnp (make-symbol "lb-msnp"))
        (this-md (make-symbol "this-md")))
    `(let* ((,this-md (make-list 1 nil))
            (,lb-msnp
             (save-match-data
               (when ;; :NOTE Should this pass `looking-back' a LIMIT arg?
                   (looking-back 
                    (aref *css-complete-look-back-regexp* ,look-back-aref))
                 (match-data t ,this-md t)))))
       ,lb-msnp)))

;; :NOTE Can/should this use `assoc-string' instead?
(defun css-vals-for-prop (css-comp-prop)
  "Return assocations for key CSS-COMP-PROP in `*css-complete-props-and-vals*'.\n
Return value is a list of elts which satisfy the predicate `stringp'.\n
:EXAMPLE\n\n\(css-vals-for-prop \"font\"\)\n
\(assoc-string \"font\" *css-complete-props-and-vals*\)\n
\(not \(equal \(cdr \(assoc-string \"font\" *css-complete-props-and-vals*\)\) 
            \(css-vals-for-prop \"font\"\)\)\)\n
:SEE-ALSO `css-complete-collect', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (css-complete-collect stringp 
    (cdr (assoc css-comp-prop *css-complete-props-and-vals*))))

(defun css-prop-for-point ()
  "Return string matching a CSS property before point.\n
Search backward for the regexp at idx 1 of the constant
`*css-complete-look-back-regexp*'.  Does not move point.\n
Return match-string 1 when string before point matches the formats:\n
;bubba-a:\n; bubba-b:\nbubba:\n;\nbubba:\n
:EXAMPLE\n\n(save-excursion (end-of-line 4) (css-prop-for-point))\n
     some-css-thing:  with-val;\n     vertical-align:\n
:CALLED-BY `css-vals-for-point'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (save-match-data
    (save-excursion
      (search-backward-regexp (aref *css-complete-look-back-regexp* 1) nil t)
      (match-string-no-properties 1))))

(defun css-vals-for-point ()
  "Return CSS values for point when `css-prop-for-point' returns non-nil.\n
:EXAMPLE\n\n\(save-excursion \(end-of-line 4\) \(css-vals-for-point\)\)\n
     some-css-thing:  with-val;\n     vertical-align:\n
:CALLED-BY `css-possible-value-completions'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-vals-for-prop (css-prop-for-point)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-02T18:52:17-04:00Z}#{10311} - by MON KEY>
(defun css-delims-maybe-part-generic ()
  "Check position of chars in range \"[:lower:]-\" with `skip-chars-backward'.\n
Return value has the form:\n
 \(<CURRENT-POINT> <RANGE-SKIPPED>\)\n
Does not move point.\n
:NOTE It isn't clear that skipping over _only_ the lowercase case is TRT.\n
:ALIASED-BY `css-delims-maybe-part-prop-at-point'
:ALIASED-BY `css-delims-maybe-part-pseudo-at-point'
:ALIASED-BY `css-delims-maybe-part-at-at-point'\n
:SEE-ALSO `css-delims-maybe-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (save-excursion
  ;;     `(,(point)
  ;;       ,(progn 
  ;;          (skip-chars-backward "[:lower:]-")
  ;;          (point))))
  ;;
  (css-delims-maybe-generate skip-chars-backward "[:lower:]-"))
;;
(defalias 'css-delims-maybe-part-prop-at-point 'css-delims-maybe-part-generic
  (concat 
   "Check position of chars in range \"[:lower:]-\" with `skip-chars-backward'.\n
Return value has the form:\n
 \(<CURRENT-POINT> <RANGE-SKIPPED>\)\n
Does not move point.\n\n"
   ":CALLED-BY `css-maybe-part-prop-at-point'
:CALLED-BY `css-delete-partial-prop'
:CALLED-BY `css-complete-prop'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"))
;;
(defalias 'css-delims-maybe-part-pseudo-at-point 'css-delims-maybe-part-generic
  (concat    
   "Check position of chars in range \"[:lower:]-\" with `skip-chars-backward'.\n
Return value has the form:\n
 \(<CURRENT-POINT> <RANGE-SKIPPED>\)\n
Does not move point.\n\n"
  ":CALLED-BY `css-maybe-part-pseudo-at-point'
:CALLED-BY `css-delete-partial-pseudo'
:CALLED-BY `css-complete-pseudo'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"))
;;
(defalias 'css-delims-maybe-part-at-at-point 'css-delims-maybe-part-generic
  (concat    
   "Check position of chars in range \"[:lower:]-\" with `skip-chars-backward'.\n
Return value has the form:\n
 \(<CURRENT-POINT> <RANGE-SKIPPED>\)\n
Does not move point.\n\n"
  "
:CALLED-BY `css-maybe-part-at-at-point'
:CALLED-BY `css-delete-partial-at'
:CALLED-BY `css-complete-at'
:CALLED-BY `css-complete-media'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"))

;;; ==============================
;;; :CHANGESET 2025
;;; :CREATED <Timestamp: #{2010-08-02T18:57:45-04:00Z}#{10311} - by MON>
(defun css-delims-maybe-part-val-at-point ()
  "Check position of chars in range \"[:lower:][:digit:]-\" with `skip-chars-backward'.\n
Return value has the form:\n
 \(<CURRENT-POINT> <RANGE-SKIPPED>\)\n
:NOTE It isn't clear that skipping over _only_ the lowercase cares is TRT.\n
:CALLED-BY `css-maybe-part-val-at-point'
:CALLED-BY `css-delete-partial-value'
:CALLED-BY `css-complete-value'\n
:SEE-ALSO `css-delims-maybe-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (save-excursion
  ;;   `(,(point)
  ;;     ,(progn 
  ;;        (skip-chars-backward "[:lower:][:digit:]-")
  ;;        (point))))
  ;;
  (css-delims-maybe-generate skip-chars-backward "[:lower:][:digit:]-"))

(defun css-maybe-part-val-at-point ()
  "Get value `css-delims-maybe-part-val-at-point'.\n 
:SEE-ALSO `css-delete-partial', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-val-at-point)
  ;;   (buffer-substring-no-properties beg end)))
  (css-delete-partial 
      css-delims-maybe-part-val-at-point buffer-substring-no-properties))

(defun css-possible-value-completions (css-cmp-str)
  "Return completions for CSS-CMP-STR with `css-vals-for-point'.\n
Return when CSS-CMP-STR satisfies the predicate `string-match-p' for some elt of
the list returned from `css-vals-for-point'.\n
:EXAMPLE\n\n\(save-excursion 
  \(end-of-line 4\)
  \(let \(\(ccs \(car \(css-vals-for-point\)\)\)\)
    \(css-possible-value-completions ccs\)\)\)\n
    height: 300px;\\n
    vertical-align: text-
                         ^^^^^^ <- completes \"bottom\"\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-complete-collect 
      (lambda (css-cmprd-str)
        ;;:WAS (string-match (concat "^" css-cmp-str) css-cmprd-str))
        (string-match-p (concat "^" css-cmp-str) css-cmprd-str))
    (css-vals-for-point)))

(defun css-possible-value-completions-at-point ()
  "Return `css-possible-value-completions' if `css-maybe-part-val-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-possible-value-completions (css-maybe-part-val-at-point)))

(defun css-delete-partial-value ()
  "If non-nil, `delete-region' returned by `css-delims-maybe-part-val-at-point'.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-val-at-point) 
  ;;   (delete-region beg end)))
  (css-delete-partial css-delims-maybe-part-val-at-point delete-region))

(defun css-value-popup-completions ()
  "Return `x-popup-menu' with CSS completions from `css-value-completion-menu'
at position `css-pos-for-x-popup-menu'.\n
:SEE-ALSO `css-popup-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS (x-popup-menu (css-pos-for-x-popup-menu) (css-value-completion-menu)))
  (css-popup-generate css-value-completion-menu))

(defun css-value-completion-menu ()
  "Return CSS completion menu with `css-possible-value-completions-at-point'.\n
:SEE-ALSO `css-complete-menu-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (list "Complete: "
  ;;       (cons 
  ;;        "Values"
  ;;        (mapcar #'(lambda (c) (cons c c))
  ;;       	 (css-possible-value-completions-at-point)))))
  (css-complete-menu-generate 
   "Complete: " "Values" css-possible-value-completions-at-point))

(defun css-pos-for-x-popup-menu ()
 "Return a display position for `x-popup-menu'.\n
Return value from calculated `posn-at-point' according to the offsets specified
by `*css-complete-popup-pos-x-offset*'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (destructuring-bind (win area (x . y) &rest rest) (posn-at-point)
    (values (list (+ *css-complete-popup-pos-x-offset* x) y) win)))

(defun css-complete-value ()
  "Find `css-possible-value-completions-at-point', generate completion menu with
`css-value-popup-completions' and insert the completion value in the region
identified with `css-delims-maybe-part-val-at-point'.\n
`css-complete-value' -> `css-value-popup-completions' <- `css-value-completion-menu'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (let* ((css-comp-pssbl (css-possible-value-completions-at-point))
	 (css-comp-newval (if (null (cdr css-comp-pssbl))
		     (car css-comp-pssbl)
		   (css-value-popup-completions))))
    (when css-comp-newval
      (apply 'delete-region (css-delims-maybe-part-val-at-point))
      (insert css-comp-newval ";"))))

;;; :TODO Real check.
(defun css-at-value-p ()
  "Return non-nil when looking back at a CSS value.\n
Return list value per `css-possible-value-completions-at-point' when not
looking-back at a match for the regexp at idx 0 of constant
`*css-complete-look-back-regexp*' and point is inside a CSS block.\n
:SEE-ALSO `css-at-p-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (and  (> (car (syntax-ppss)) 0)
        ;; :WAS (not (looking-back (aref *css-complete-look-back-regexp* 0)))
        (not (css-at-p-generate 0))
        (css-possible-value-completions-at-point)))

;;; :TODO Real check.
(defun css-at-prop-p ()
  "Return non-nil when looking back at a CSS property.\n
Return list value per `css-possible-prop-completions-at-point' when looking-back 
matches the regexp at idx 0 of `*css-complete-look-back-regexp*'.\n 
:SEE-ALSO `css-at-p-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS (and (looking-back (aref *css-complete-look-back-regexp* 0))
  (and (css-at-p-generate 0) (css-possible-prop-completions-at-point)))
       
(defun css-maybe-part-prop-at-point ()
  "When `css-delims-maybe-part-prop-at-point' is non-nil retrun a string.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-prop-at-point)
  ;;   (buffer-substring-no-properties beg end)))
  (css-delete-partial 
      css-delims-maybe-part-prop-at-point buffer-substring-no-properties))
    

(defun css-possible-prop-completions (css-cmp-str)
  "Return completions for CSS-CMP-STR with `css-props-for-point'.\n
:SEE-ALSO `css-complete-collect', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (css-complete-collect 
      (lambda (css-cmprd-str)
        ;; :WAS (string-match  (concat "^" css-cmp-str) css-cmprd-str)) 
        (string-match-p (concat "^" css-cmp-str) css-cmprd-str))
    (css-props-for-point)))

(defun css-possible-prop-completions-at-point ()
  "Return `css-possible-prop-completions' if `css-maybe-part-prop-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-possible-prop-completions (css-maybe-part-prop-at-point)))

(defun css-props-for-point ()
  "Return value of `*css-complete-property-ids*'.\n
:EXAMPLE\n\n\(car \(member \"list-style-type\" \(css-props-for-point\)\)\)\n
:CALLED-BY `css-possible-prop-completions'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS  css-property-ids)
  *css-complete-property-ids*)

(defun css-delete-partial-prop ()
  "If non-nil, `delete-region' returned by `css-delims-maybe-part-prop-at-point'.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color',.\n►►►"
  ;; :WAS (destructuring-bind (end beg) (css-delims-maybe-part-prop-at-point)
  ;;   (delete-region beg end)))
  (css-delete-partial css-delims-maybe-part-prop-at-point delete-region))
  

(defun css-prop-popup-completions ()
  "Return `x-popup-menu' with CSS completions from `css-prop-completion-menu' at
position `css-pos-for-x-popup-menu'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (x-popup-menu (css-pos-for-x-popup-menu) (css-prop-completion-menu)))

(defun css-prop-completion-menu ()
  "Return CSS completion menu with `css-possible-prop-completions-at-point'.\n
:SEE-ALSO `css-complete-menu-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (list "Complete: "
  ;;       (cons 
  ;;        "Properties"
  ;;        (mapcar #'(lambda (c)
  ;;       	   (cons c c))
  ;;       	 (css-possible-prop-completions-at-point)))))
  (css-complete-menu-generate 
   "Complete: " "Properties" css-possible-prop-completions-at-point))

(defun css-complete-prop ()
  "Find `css-possible-prop-completions-at-point', generate completion menu with
`css-prop-popup-completions' and insert the completion value in the region
identified with `css-delims-maybe-part-tag-prop-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-indent-line)
  (let* ((css-comp-pssbl (css-possible-prop-completions-at-point))
	 (css-comp-newval (if (null (cdr css-comp-pssbl))
                              (car css-comp-pssbl)
                            (css-prop-popup-completions))))
    (when css-comp-newval
      (apply 'delete-region (css-delims-maybe-part-prop-at-point))
      (insert css-comp-newval ": "))))

(defun css-at-pseudo-id-p ()
  "Return non-nil when looking-back at the regexp \":[a-z-]*\".
:SEE-ALSO `css-at-p-generate', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS (looking-back ":[a-z-]*"))
  (css-at-p-generate 3))

(defun css-maybe-part-pseudo-at-point ()
  "When `css-delims-maybe-part-val-at-point' is non-nil return a string.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-pseudo-at-point)
  ;;   (buffer-substring-no-properties beg end)))
  (css-delete-partial 
      css-delims-maybe-part-pseudo-at-point buffer-substring-no-properties))

(defun css-possible-pseudo-completions (css-cmp-str)
  "Return completions for CSS-CMP-STR with `css-pseudos-for-point'.\n
:SEE-ALSO `css-complete-collect',`mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (css-complete-collect 
      (lambda (css-cmprd-str)
        ;; :WAS (string-match  (concat "^" css-cmp-str) css-cmprd-str)) 
        (string-match-p  (concat "^" css-cmp-str) css-cmprd-str))
    (css-pseudos-for-point)))

(defun css-possible-pseudo-completions-at-point ()
  "Return `css-possible-pseudo-completions' if `css-maybe-part-pseudo-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-possible-pseudo-completions (css-maybe-part-pseudo-at-point)))

(defun css-pseudos-for-point ()
  "Return value of `*css-complete-pseudo-ids*'.\n
:EXAMPLE\n\n\(car \(member \"first-child\" \(css-pseudos-for-point\)\)\)\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS css-pseudo-ids)
  *css-complete-pseudo-ids*)

(defun css-delete-partial-pseudo ()
  "If non-nil, `delete-region' returned by `css-delims-maybe-part-pseudov-at-point'.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-pseudo-at-point)
  ;;   (delete-region beg end)))
  (css-delete-partial css-delims-maybe-part-pseudo-at-point delete-region))

(defun css-pseudo-popup-completions ()
  "Return `x-popup-menu' with CSS completions from `css-pseudo-completion-menu'
at position `css-pos-for-x-popup-menu'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (x-popup-menu (css-pos-for-x-popup-menu) (css-pseudo-completion-menu)))

(defun css-pseudo-completion-menu ()
  "Return CSS completion menu with `css-possible-pseudo-completions-at-point'.\n
:SEE-ALSO `css-complete-menu-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (list "Complete: "
  ;;       (cons 
  ;;        "Pseudoerties"
  ;;        (mapcar #'(lambda (c)
  ;;       	   (cons c c))
  ;;       	 (css-possible-pseudo-completions-at-point)))))
  (css-complete-menu-generate 
   "Complete: " "Pseudo properties" css-possible-pseudo-completions-at-point))

(defun css-complete-pseudo ()
  "Find `css-possible-pseudo-completions-at-point', generate completion menu
with `css-pseudo-popup-completions' and insert the completion value in the
region identified with `css-delims-maybe-part-pseudo-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-indent-line)
  (let* ((css-comp-pssbl (css-possible-pseudo-completions-at-point))
	 (css-comp-newval (if (null (cdr css-comp-pssbl))
		     (car css-comp-pssbl)
		   (css-pseudo-popup-completions))))
    (when css-comp-newval
      (apply 'delete-region (css-delims-maybe-part-pseudo-at-point))
      (insert css-comp-newval))))

(defun css-at-tag-id-p ()
  "Return non-nil when looking-back at a CSS tag id.\n
Predicate to test if matches for the regexp:
 \"\\\\\(^\\\\|,\\\\\)[[:space:]]*[a-z]*[0-9]?\"\n
:EXAMPLE\n\n\(save-excursion \(end-of-line 3\) \(css-at-tag-id-p\)\)\n
 sometag1\n
:SEE-ALSO `css-at-p-generate', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS (looking-back (aref *css-complete-look-back-regexp* 2)))
  (css-at-p-generate 2))

(defun css-delims-maybe-part-tag-at-point ()
  "Find leftmost buffer position with `css-mode-syntax-table' word syntax from point.\n
Return value has the form:\n
 \(<CURRENT-POINT> <PREVIOUS-WORD-SYNTAX-FROM-POINT>\)\n
Does not move point.\n
:EXAMPLE\n\n\(eq \(apply '- \(with-syntax-table css-mode-syntax-table
                \(save-excursion \(end-of-line 3\) \(backward-char 1\)
                                \(css-delims-maybe-part-tag-at-point\)\)\)\) 6\)\n
	color: #000066;\n
:SEE-ALSO `css-delims-maybe-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS 
  ;; (save-excursion
  ;;   `(,(point)
  ;;     ,(progn 
  ;;        (skip-syntax-backward "w") ;; Should this take a LIM arg?
  ;;        (point))))
  (css-delims-maybe-generate skip-syntax-backward "w"))

(defun css-maybe-part-tag-at-point ()
  "When `css-delims-maybe-part-val-at-point' is non-nil return a string.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-tag-at-point)
  ;;   (buffer-substring-no-properties beg end)))
  (css-delete-partial
      css-delims-maybe-part-tag-at-point buffer-substring-no-properties))

(defun css-tags-for-point ()
  "Return value of `*css-complete-tag-ids*'.\n
:EXAMPLE\n\n\(car \(member \"meta\" \(css-tags-for-point\)\)\)\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  *css-complete-tag-ids*)

(defun css-possible-tag-completions (css-cmp-str)
  "Return completions for CSS-CMP-STR with `css-tags-for-point'.\n
:SEE-ALSO `css-complete-collect',`mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (css-complete-collect 
      (lambda (css-cmprd-str)
        ;; :WAS (string-match  (concat "^" css-cmp-str) css-cmprd-str))
        (string-match-p  (concat "^" css-cmp-str) css-cmprd-str))
    (css-tags-for-point)))

(defun css-possible-tag-completions-at-point ()
  "Return `css-possible-tag-completions' with `css-maybe-part-tag-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-possible-tag-completions (css-maybe-part-tag-at-point)))

(defun css-delete-partial-tag ()
  "If non-nil, `delete-region' returned by `css-delims-maybe-part-tag-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-tag-at-point)
  ;;   (delete-region beg end)))
  (css-delete-partial css-delims-maybe-part-tag-at-point delete-region))

(defun css-tag-popup-completions ()
  "Return `x-popup-menu' with CSS completions from `css-tag-completion-menu' at
position `css-pos-for-x-popup-menu'.\n
:SEE-ALSO `css-popup-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;;   (completing-read "Tag: " (css-possible-tag-completions-at-point) nil t 
  ;; 		   (css-maybe-part-tag-at-point))
  (css-popup-generate css-tag-completion-menu))

(defun css-tag-completion-menu ()
  "Return CSS completion menu with `css-possible-tag-completions-at-point'.\n
:SEE-ALSO `css-complete-menu-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (list "Complete tag: "
  ;;       (cons "Tags" (mapcar #'(lambda (c)
  ;;                                (cons c c))
  ;;                            (css-possible-tag-completions-at-point)))))
  (css-complete-menu-generate 
   "Complete tag: " "Tags" css-possible-tag-completions-at-point))

(defun css-complete-tag ()
  "Find `css-possible-tag-completions-at-point', generate completion menu with
`css-tag-popup-completions' and insert the completion value in the region
identified with `css-delims-maybe-part-tag-at-point'.\n
`css-complete-tag' -> `css-tag-popup-completions' <- `css-tag-completion-menu'\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-indent-line)
  (let* ((css-comp-pssbl (css-possible-tag-completions-at-point))
	 (css-comp-newval (if (null (cdr css-comp-pssbl))
		     (car css-comp-pssbl)
		   (css-tag-popup-completions))))
    (when css-comp-newval
      (apply 'delete-region (css-delims-maybe-part-tag-at-point))
      (insert css-comp-newval))))

(defun css-at-at-id-p ()
  "Return non-nil when looking-back at a CSS at id.\n :SEE-ALSO
`css-at-p-generate', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS (looking-back (aref *css-complete-look-back-regexp* 3)))
  (css-at-p-generate 3))

(defun css-maybe-part-at-at-point ()
  "If non-nil, return string from `css-delims-maybe-part-val-at-point'.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  ;; :WAS 
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-at-at-point)
  ;;   (buffer-substring-no-properties beg end)))
  (css-delete-partial
      css-delims-maybe-part-at-at-point buffer-substring-no-properties))

(defun css-ats-for-point ()
  "Return value of `*css-complete-at-ids*'.\n
:EXAMPLE\n\n\(car \(member \"charset\" (css-ats-for-point\)\)\)\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  *css-complete-at-ids*)

(defun css-possible-at-completions (css-cmp-str)
  "Return completions for CSS-CMP-STR with `css-ats-for-point'.\n
:SEE-ALSO `css-complete-collect', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (css-complete-collect 
      (lambda (css-cmprd-str)
        ;; :WAS (string-match (concat "^" css-cmp-str) css-cmprd-str))
        (string-match-p (concat "^" css-cmp-str) css-cmprd-str))
    (css-ats-for-point)))

(defun css-possible-at-completions-at-point ()
  "Return `css-possible-at-completions' if `css-maybe-part-at-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-possible-at-completions (css-maybe-part-at-at-point)))

(defun css-delete-partial-at ()
  "If non-nil, `delete-region' returned by `css-delims-maybe-part-at-at-point'.\n
:SEE-ALSO `css-delete-partial', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; (destructuring-bind (end beg) (css-delims-maybe-part-at-at-point)
  ;;   (delete-region beg end)))
  (css-delete-partial css-delims-maybe-part-at-at-point delete-region))

(defun css-at-popup-completions ()
  "Return `x-popup-menu' with CSS completions from `css-at-completion-menu'.\n
:SEE-ALSO `css-popup-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (css-popup-generate css-at-completion-menu))

(defun css-at-completion-menu ()
  "Return CSS completion menu with `css-possible-at-completions-at-point'.\n
:SEE-ALSO `css-complete-menu-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (list "Complete at: "
  ;;       (cons 
  ;;        "Ats"
  ;;        (mapcar #'(lambda (c)
  ;;       	   (cons c c))
  ;;       	 (css-possible-at-completions-at-point)))))
  (css-complete-menu-generate 
   "Complete at: " "Ats" css-possible-at-completions-at-point))

(defun css-complete-at ()
  "Find `css-possible-at-completions-at-point', generate completion menu with
`css-at-popup-completions' and insert the completion value in the region
identified with `css-delims-maybe-part-at-at-point'.\n
`css-complete-at' -> `css-at-popup-completions' <- `css-at-completion-menu'\n
:SEE-ALSO `css-possible-at-completions-at-point',
`css-delims-maybe-part-at-at-point', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (css-indent-line)
  (let* ((css-comp-pssbl (css-possible-at-completions-at-point))
	 (css-comp-newval (if (null (cdr css-comp-pssbl))
		     (car css-comp-pssbl)
		   (css-at-popup-completions))))
    (when css-comp-newval
      (apply 'delete-region (css-delims-maybe-part-at-at-point))
      (insert css-comp-newval))))

;;; :NOTE `css-at-string-p' is a duplicated function. Not sure which is correct.

;;; (defun css-at-string-p ()
;;;   (member 'font-lock-string-face (text-properties-at (point))))

(defun css-at-string-p ()
  "Return the 3rd elt of `syntax-ppss' \(0 indexed\).\n
A `css-complete' predicate.\n
:SEE-ALSO `css-at-comment-p', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
   (nth 3 (syntax-ppss)))

(defun css-at-filename-p ()
  "Return non-nil when inside a string as per `css-at-string-p'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-at-string-p))

(defun css-complete-filename ()
  "Complete filename around point with `comint-dynamic-complete-filename'.\n
:SEE-ALSO `css-at-filename-p', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  (call-interactively 'comint-dynamic-complete-filename))

(defun css-at-comment-p ()
  "Return the 4th elt of `syntax-ppss' \(0 indexed\).\n
A `css-complete' predicate.\n
:SEE-ALSO `css-at-string-p', `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (nth 4 (syntax-ppss)))

(defun css-after-at-p ()
  "Retrun non-nil if `looking-back' at an CSS @VALUE.\n
A `css-complete' predicate to trigger `css-complete-after-at' completion.\n
:EXAMPLE\n\n\(save-excursion \(end-of-line 2\) \(css-after-at-p\)\)
@charset \n
:SEE-ALSO `css-at-p-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS (looking-back "@\\(?:charset\\|font-face\\|import\\|media\\|page\\)[[:space:]]+"))
  (css-at-p-generate 4))


;;; ==============================
;;; :CHANGESET 2041
;;; :CREATED <Timestamp: #{2010-08-04T18:58:57-04:00Z}#{10313} - by MON KEY>
(defun css-complete-after-at ()
  "If were looking-back on a attribute perform an appropreate action.\n
:EXAMPLE\n\n\(save-excursion \(end-of-line 3\) \(css-complete-after-at\)\)\n
@media \n
:SEE-ALSO `css-at-p-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :NOTE Why did this invoke `read'?
  ;; :WAS (looking-back (aref *css-complete-look-back-regexp* 5))
  (let* ((myb-mtch (css-at-p-generate 5))
         (lb-msnp1 (when myb-mtch (save-match-data
                                    (set-match-data myb-mtch)
                                    (match-string-no-properties 1)))))
    (when lb-msnp1
      (cond ((member lb-msnp1 '("charset" "import" "page"))
             (insert "\"\""))
            ((equal lb-msnp1 "font-face")
             (call-interactively 'css-electric-left-brace))
            ((equal lb-msnp1 "media")
             (css-complete-media))))))

;;; ==============================
;;; :CHANGESET 2025
;;; :CREATED <Timestamp: #{2010-08-03T21:32:51-04:00Z}#{10312} - by MON>
(defun css-media-for-point ()
  "Return value of `*css-complete-media-ids*'.\n
:EXAMPLE\n\n\(car \(member \"aural\" \(css-media-for-point\)\)\)\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  *css-complete-media-ids*)

(defun css-media-completion-menu ()
  "Return CSS completion menu with `*css-complete-media-ids*'.\n
:EXAMPLE\n\n\(css-media-completion-menu\)\n
:SEE-ALSO `css-complete-menu-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS
  ;; (list "Medium: "
  ;;       (cons 
  ;;        "Choose"
  ;;        (mapcar #'(lambda (medium)
  ;;                    (cons medium medium)) 
  ;;                ;; :WAS *css-complete-media-ids*))))
  ;;                (css-media-for-point)))))
  (css-complete-menu-generate "Medium: " "Choose" css-media-for-point))

(defun css-complete-media ()
  "Find `*css-complete-media-ids*' completions `css-media-popup-completions'
insert the completion value in the region identified with
`css-delims-maybe-part-at-at-point'.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (css-indent-line)
  (let* ((css-comp-pssbl *css-complete-media-ids*)
	 (css-comp-newval (if (null (cdr css-comp-pssbl))
		     (car css-comp-pssbl)
                     (css-media-popup-completions))))
    (when css-comp-newval
      (apply 'delete-region (css-delims-maybe-part-at-at-point))
      (insert css-comp-newval))))

(defun css-media-popup-completions ()
  "Popup `css-media-completion-menu' at position `css-pos-for-x-popup-menu'.\n
:EXAMPLE\n\n\(css-media-popup-completions\)\n
:SEE-ALSO `css-popup-generate', `mon-help-css-complete',
`mon-help-css-properties', `mon-help-css-mode', `mon-help-css-check',
`mon-help-css-color'.\n►►►"
  ;; :WAS (x-popup-menu (css-pos-for-x-popup-menu) (css-media-completion-menu)))  
  (css-popup-generate css-media-completion-menu))

(defun css-complete ()
  "Complete CSS values according to `css-at-*' predicate satisfaction.\n
When predicate returns non-nil return per an assocatiated `css-complete-*' fncn.\n
CSS at predicates map to following CSS completion functions:\n
 `css-at-at-id-p'     <- `css-complete-at'
 `css-at-pseudo-id-p' <- `css-complete-pseudo'
 `css-at-value-p'     <- `css-complete-value'
 `css-after-at-p'     <- `css-complete-after-at'
 `css-at-prop-p'      <- `css-complete-prop'
 `css-at-tag-id-p'    <- `css-complete-tag'
 `css-at-filename-p'  <- `css-complete-filename'\n
When `css-at-string-p' or `css-at-comment-p' return non-nil or no completions
are found message user as accordingly.\n
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (interactive)
  (css-indent-line)
  (cond ((css-at-string-p)
         (message (concat ":FUNCTION `css-complete' " 
                          "-- can not satisfy predicate `css-at-string-p' "
                          "no completion inside string")))
        ((css-at-comment-p)
         (message (concat ":FUNCTION `css-complete' "
                          "-- can not satisfy predicate `css-at-comment-p' "
                          "no completion inside comments")))
        ((css-at-at-id-p)     (css-complete-at))
        ((css-at-pseudo-id-p) (css-complete-pseudo))
        ((css-at-value-p)     (css-complete-value))
        ((css-after-at-p)     (css-complete-after-at))
        ((css-at-prop-p)      (css-complete-prop))
        ((css-at-tag-id-p)    (css-complete-tag))
        ((css-at-filename-p)  (css-complete-filename))
        (t (message ":FUNCTION `css-complete' -- no completions"))))

(defun css-electric-left-brace ()
  "
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (interactive)
  (css-indent-line)
  (let ((char ?\{))
    (insert char "\n\n" (matching-paren char)))
  (forward-line -1)
  (fill-paragraph t)
  (css-indent-line))

(defun css-electric-left-bracket ()
  "
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (interactive)
  (let ((char ?\[))
    (insert char 
	    (matching-paren char)))
  (forward-char -1)
  (fill-paragraph t))

(defun css-electric-left-paren ()
  "
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (interactive)
  (let ((char ?\())
    (insert char (matching-paren char))) 
  (forward-char -1)
  (fill-paragraph t))

(defun css-electric-quotes ()
  "
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (interactive)
  (let ((char ?\"))
    (insert char  char))
  (forward-char -1))

(defun css-electric-semicolon ()
  "
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (interactive)
  (let ((char ?\;))
    (insert char))
  (fill-paragraph t))

(define-key css-mode-map [C-return] 'css-complete)
(define-key css-mode-map "{"        'css-electric-left-brace)
(define-key css-mode-map "["        'css-electric-left-bracket)
(define-key css-mode-map "\""       'css-electric-quotes)
(define-key css-mode-map "("        'css-electric-left-paren)
(define-key css-mode-map ";"        'css-electric-semicolon)


;;; ==============================
;;; :CHANGESET 2025
;;; :CREATED <Timestamp: #{2010-08-02T13:54:52-04:00Z}#{10311} - by MON>
(defun mon-css-complete-loadtime (w-msg-user)
  "Rebind constant `css-media-ids' to `*css-complete-media-ids*' at loadtime.\n
When `IS-MON-SYSTEM-P' is non-nil evaluated by `mon-after-mon-utils-loadtime'.\n
:SEE :FILE lisp/textmodes/css-mode.el
:SEE-ALSO `mon-help-css-complete', `mon-help-css-properties',
`mon-help-css-mode', `mon-help-css-check', `mon-help-css-color'.\n►►►"
  (when (and (intern-soft "IS-MON-SYSTEM-P")
             (bound-and-true-p IS-MON-SYSTEM-P)
             (intern-soft "css-media-ids")
             (bound-and-true-p css-media-ids)
             (symbol-file 'css-media-ids 'defvar)
             (equal (file-name-nondirectory 
                     (file-name-sans-extension 
                      (or (symbol-file 'css-media-ids 'defvar) "")))
                    "css-mode"))
    (setq w-msg-user t)
    (setq css-media-ids *css-complete-media-ids*))
  (when w-msg-user (message 
                    (concat ":FUNCTION `mon-css-complete-loadtime' " 
                            "-- constant `css-media-ids' rebound to value of "
                            "`*css-complete-media-ids*' at loadtime"))))

;; (provide 'css-complete)
;;; css-complete.el ends here

(provide 'mon-css-complete)

;;; ==============================
;;; mon-css-complete.el ends here
;;; EOF

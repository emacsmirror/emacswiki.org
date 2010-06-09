;;; mon-doc-help-css.el --- extends mon-doc-help-utils with CSS related docs
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-doc-help-css.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-06-04T15:42:39-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: docs, matching, lisp, hypermedia

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-doc-help-css.el provides extenions to mon-doc-help-utils.el for CSS
;; related functions and packages.
;;
;; FUNCTIONS:►►►
;; `mon-help-css-mode', `mon-help-css-complete', `mon-help-css-check',
;; `mon-help-css-properties',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; ALIASED/ADVISED/SUBST'D:
;; `mon-help-csstidy'      -> `mon-help-css-check'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-css.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-06-08T18:57:54-04:00Z}#{10232} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-doc-help-css. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-06-04T15:42:39-04:00Z}#{10225} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))


;; (eval-when-compile 
;;   (when (or (and (intern-soft "IS-MON-SYSTEM-P")
;;                  (bound-and-true-p IS-MON-SYSTEM-P))
;;             ;; :NOTE we've renamed most of the variable symbol provied by css-check.el
;;             ;; (featurep 'css-check) 
;;             (featurep 'mon-css-check))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-01T21:28:02-04:00Z}#{10222} - by MON>
(defun mon-help-css-check (&optional insertp intrp)
  "A list of flags for csstidy command.\n
;; :CSS-CHECK-FUNCTIONS
`css-check'
`css-check-toggle-follow'
`css-check-next-line'
`css-check-prev-line'
`css-check-goto-line-at-p'
`css-check-apply-line-at-p'
`css-check-undo-application-at-p'
`css-check-applied-face'
`css-check-unapplied-face'
`*css-file*'
`*css-check-map*'
`*css-check-follow-mode*'
`*css-check-csstidy-path*'\n
;; :CSSTIDY-OPTIONS
--allow_html_in_templates=[false|true]             ; :NOTE Default value first.\n
--compress_colors=[true|false]\n
--compress_font-weight=[true|false]\n
--discard_invalid_properties=[false|true]\n
--lowercase_s=[false|true]\n
--preserve_css=[false|true]\n
--remove_bslash=[true|false]\n
--remove_last_;=[false|true]\n
--silent=[false|true]\n
--sort_properties=[false|true]\n
--sort_selectors=[false|true]\n
--timestamp=[false|true]\n
--merge_selectors=[2|1|0]\n
--case_properties=[0|1|2]\n
--optimise_shorthands=[1|2|0]\n
--template=[default|filename|low|high|highest]\n
;; :CSSTIDY-USAGE\n
:EXAMPLE\n
 $> csstidy <INPUT-FILENAME> <OPTION(S)> <OUTPUT-FILENAME>\n
 $> csstidy mycssfile.css --remove_last_;=true myoutputfile.css\n
To optimise a CSS file and send the output to screen do:\n
 $> csstidy mycssfile.css\n
To save csstidy output to a file do:\n
 $> csstidy mycssfile.css myoutputfile.css\n
When using the filename \"-\", CSSTidy reads from stdin.
To change settings, add this flag after the input file:\n
 \"--thesettingyouwanttochange=true or false\"\n
Where \"true\" is first in the syntax description, this indicates that \"true\"
is the default value (the same applies to false).\n
;; :CSSTIDY-HACKS THAT WORK
Following lists all CSS hacks which will not (by default) be modified by
CSSTidy.\n
When \"preserve CSS\" is non-nil, more hacks will be saved.
In general one should avoide CSS hacks, however where they are needed/desired
you should these hacks:\n
o p\\roperty:value; -- \(Simplified Box Model Hack\)
                      :NOTE Only when option --remove_bslash=true\n
  #test-span {
  c\olor:green;
  }\n
:SEE (URL `http://centricle.com/ref/css/filters/tests/sbmh/')\n
o div#test -- ID Selector w/ Element\n
  span#test-span {
  color:green;
  }\n
:SEE (URL `http://centricle.com/ref/css/filters/tests/id/')
:SEE (URL `http://www.w3.org/TR/REC-CSS2/selector.html#id-selectors')\n
o head:first-child+body div -- Owen Hack\n
  head:first-child+body #test-span {
  color:green;
  }\n
:SEE (URL `http://www.albin.net/CSS/OwenHack.html')
:SEE (URL `http://john.albin.net/css/owen-hack')\n
o body>div -- Child Selector\n
  div>#test-span {
  color:green;
  }\n
:SEE (URL `http://w3development.de/css/hide_css_from_browsers/child/')\n
o html[xmlns] div -- Attribute Selector\n
  html[xmlns] #test-span {
  color:green;
  }\n
:SEE (URL `http://w3development.de/css/hide_css_from_browsers/attribute/')\n
o @import \"null?\\\"\\={\"; @import \"styles.css\"; -- High Pass Filter
                                            \(and all other @import rules\)\n
  <link rel=\"stylesheet\" type=\"text/css\" href=\"high_pass.css\" />\n
:SEE (URL `http://tantek.com/CSS/Examples/highpass.html')\n
o @media all{/* rules */} -- All Rules Hack\n
  @media all {
  #test-span {color:green}
  }\n
o html div -- Star HTML Bug\n
  * html #test-span {
  color:green;
  }\n
o i{content:\"\\\"/*\"} div{property:value}  -- Inline High Pass Filter\n
  i{content:\"\\\"/*\"}
  #test-span {color:green}\n
  :SEE (URL `http://www.tantek.com/CSS/Examples/inlinehpf.html')\n
o html*#test -- *7 Hack\n
  html*#test-span {
  color:green;
  }\n
o _property:value -- Underscore Hack\n
 #test-span {
 _color: green;
 }\n
:SEE (URL `http://wellstyled.com/css-underscore-hack.html')
:SEE (URL `http://www.w3.org/TR/CSS21/')
:SEE (URL `http://www.w3.org/Style/CSS/current-work')\n
:NOTE When building csstidy from source on GNU/Linux systems the symbol
`in_char_arr' has no callers and causes Scons to fail to build.  The symbol
appears in the two files: misc.cpp and misc.hpp   When the code blocks for
`in_char_arr' are commented out Scons will compile the the csstidy executable.
:SEE (URL `http://csstidy.sourceforge.net/usage.php')\n
:SEE-ALSO `mon-help-css-properties', `mon-help-ebay-template-mode',
`mon-help-css-mode', `mon-help-css-complete', `mon-help-css-check'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-csstidy :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
(defalias 'mon-help-csstidy 'mon-help-css-check)
;;
;;; :TEST-ME (mon-help-csstidy)
;;; :TEST-ME (mon-help-csstidy t)
;;; :TEST-ME (describe-function 'mon-help-csstidy)
;;; :TEST-ME (apply 'mon-help-csstidy '(t))
;;
;; )) ;; :CLOSE `eval-when-compile'

;;; ==============================
;;; :CHANGESET 1828
;;; :CREATED <Timestamp: #{2010-06-04T15:47:25-04:00Z}#{10225} - by MON KEY>
(defun mon-help-css-complete (&optional insertp intrp)
  "A list of functions variables from the css-complete package.\n
;; :CSS-COMPLETE-VARIABLES
`css-popup-pos-x-offset' ;<VARIABLE>
`css-tag-ids'            ;<CONSTANT>
`css-media-ids'          ;<CONSTANT>
`css-props-and-vals`    ;<CONSTANT>\n
;; :CSS-COMPLETE-FUNCTIONS
`css-complete-collect' ;<MACRO>
`css-vals-for-prop'
`css-prop-for-point'
`css-vals-for-point'
`css-delims-maybe-part-val-at-point'
`css-maybe-part-val-at-point'
`css-possible-value-completions'
`css-possible-value-completions-at-point'
`css-delete-partial-value'
`css-value-popup-completions'
`css-value-completion-menu'
`css-pos-for-x-popup-menu'
`css-complete-value'
`css-at-value-p'
`css-at-prop-p'
`css-delims-maybe-part-prop-at-point'
`css-maybe-part-prop-at-point'
`css-possible-prop-completions'
`css-possible-prop-completions-at-point'
`css-props-for-point'
`css-delete-partial-prop'
`css-prop-popup-completions'
`css-prop-completion-menu'
`css-complete-prop'
`css-at-pseudo-id-p'
`css-delims-maybe-part-pseudo-at-point'
`css-maybe-part-pseudo-at-point'
`css-possible-pseudo-completions'
`css-possible-pseudo-completions-at-point'
`css-pseudos-for-point'
`css-delete-partial-pseudo'
`css-pseudo-popup-completions'
`css-pseudo-completion-menu'
`css-complete-pseudo'
`css-at-tag-id-p'
`css-delims-maybe-part-tag-at-point'
`css-maybe-part-tag-at-point'
`css-tags-for-point'
`css-possible-tag-completions'
`css-possible-tag-completions-at-point'
`css-delete-partial-tag'
`css-tag-popup-completions'
`css-tag-completion-menu'
`css-complete-tag'
`css-at-at-id-p'
`css-delims-maybe-part-at-at-point'
`css-maybe-part-at-at-point'
`css-ats-for-point'
`css-possible-at-completions'
`css-possible-at-completions-at-point'
`css-delete-partial-at'
`css-at-popup-completions'
`css-at-completion-menu'
`css-complete-at'
`css-at-string-p'
`css-at-filename-p'
`css-complete-filename'
`css-at-comment-p'
`css-after-at-p'
`css-complete-after-at'
`css-media-completion-menu'
`css-complete-media'
`css-media-popup-completions'
`css-complete'
`css-electric-left-brace'
`css-electric-left-bracket'
`css-electric-left-paren'
`css-electric-quotes'
`css-electric-semicolon'
:SEE :FILE site-lisp/mon-css-complete.el
:SEE-ALSO `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-complete', `mon-help-css-check'.\n►►►"
(interactive "i\nP")
(if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-css-complete :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-css-complete)
;;; :TEST-ME (mon-help-css-complete t)
;;; :TEST-ME (documentation 'mon-help-css-complete)
;;; :TEST-ME (apply 'mon-help-css-complete '(t))

;;; ==============================
;;; :CHANGESET 1828
;;; :CREATED <Timestamp: #{2010-06-04T15:49:33-04:00Z}#{10225} - by MON KEY>
(defun mon-help-css-mode (&optional insertp intrp)
  "List of functions and vars for `css-mode'.\n
;; :CSS-MODE-FUNCTIONS
`css-mode'
`css-fill-paragraph'
`css-backward-sexp'
`css-forward-sexp'
`css-indent-calculate-virtual'
`css-indent-calculate'
`css-indent-line'\n
;; :CSS-MODE-EXTRACT-FUNCTIONS
`css-extract-keyword-list'
`css-extract-parse-val-grammar'
`css-extract-props-and-vals'\n
;; :CSS-MODE-VARIABLES 
`css-mode-map'
`css-indent-offset'         
`css-electric-keys'
`css-mode-abbrev-table'
`css-mode-hook'
`css-font-lock-defaults'
`css-font-lock-keywords'
`css-mode-syntax-table'
`css-navigation-syntax-table'   ;<CONSTANT>\n
;; :CSS-MODE-VARIABLES-REGEXPS
`css-escapes-re'	        ;<CONSTANT>
`css-nmchar-re'	                ;<CONSTANT>
`css-nmstart-re'	        ;<CONSTANT>
`css-ident-re'	                ;<CONSTANT>
`css-name-re'                   ;<CONSTANT>\n
;; :CSS-MODE-VARIABLES-IDS
`css-pseudo-ids'                ;<CONSTANT>
`css-at-ids'                    ;<CONSTANT>
`css-descriptor-ids'            ;<CONSTANT>
`css-media-ids'      	        ;<CONSTANT>
`css-property-ids'              ;<CONSTANT>\n
:SEE :FILE lisp/textmodes/css-mode.el\n
:SEE-ALSO `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-complete', `mon-help-css-check'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-css-mode :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-css-mode)
;;; :TEST-ME (mon-help-css-mode t)
;;; :TEST-ME (documentation 'mon-help-css-mode)
;;; :TEST-ME (apply 'mon-help-css-mode '(t))

;;; ==============================
;;; :CHANGESET 1828
;;; :CREATED <Timestamp: #{2010-06-04T15:57:44-04:00Z}#{10225} - by MON KEY>
(defun mon-help-css-color (&optional insertp intrp)
  "A List of functions and vars for working with css-color-mode\n
:FILE mon-css-color.el
;; :CSS-COLOR-MODE-ADJUST-TYPE
`css-color:rgb-down'
`css-color:hsv-hue-down'
`css-color:hsv-hue-up'
`css-color:incr-hsv-hue'
`css-color:incr-hsv-sat'
`css-color:incr-hsv-val'
`css-color:num-down'
`css-color:num-up'
`css-color:hsv-saturation-down'
`css-color:hsv-saturation-up'
`css-color:rgb-up'
`css-color:hsv-value-down'
`css-color:hsv-value-up'\n
;; CSS-COLOR-MODE-TOGGLE-TYPE
`css-color:toggle-percentage'
`css-color:cycle-type'
`css-color:next-type'\n
;; :CSS-COLOR-MODE-CONVERT-TYPE
`css-color:hex-to-hsv'
`css-color:hex-to-rgb'
`css-color:hsl-to-hex'
`css-color:hsl-to-rgb'
`css-color:hsl-to-rgb-fractions'
`css-color:hsv-to-hex'
`css-color:hsv-to-hsl'
`css-color:hsv-to-rgb'
`css-color:hue-to-rgb'
`css-color:rgb-to-hex'
`css-color:rgb-to-hsl'
`css-color:rgb-to-hsv'\n
;; :CSS-COLOR-MODE-CONVERT-STRING
`css-color:string-hex-to-hsl'
`css-color:string-hsl-to-hex'
`css-color:string-hsl-to-rgb'
`css-color:string-name-to-hex'
`css-color:string-rgb-to-hex'
`css-color:string-rgb-to-name'
`css-color:hexify-anystring'\n
;; CSS-COLOR-MODE-PARSE
`css-color:adjust-hsv-hue-at-posn'
`css-color:adjust-hsv-sat-at-posn'
`css-color:adjust-hsv-val-at-posn'
`css-color:adjust-hex-at-posn'
`css-color:repl-color-at-posn'
`css-color:what-channel'
`css-color:hexval-beginning'
`css-color:foreground-color'
`css-color:get-color-at-point'
`css-color:normalize-hue'
`css-color:pal-lumsig'
`css-color:parse-hsl'
`css-color:within-bounds'\n
;; CSS-COLOR-MODE-TEXT-PROPERTY
`css-color:hsv-to-prop-hexstring'
`css-color:text-property-color-start'
`css-color:text-property-color-region'
`css-color:text-property-color-end'\n
;; CSS-COLOR-MODE-FUNCTIONS
`css-color:html-color-by-name'
`css-color:examine-color'
`css-color-global-mode'
`css-color-turn-on-in-buffer'
`css-color:font-lock-hook-fun'
`css-color:run-tests'\n
;; :CSS-COLOR-MODE-VARIABLES
`*css-color:map*'            ;<VARIABLE> 
`*css-color:generic-map*'    ;<VARIABLE> 
`*css-color:keywords*'	     ;<VARIABLE> 
`*css-color:hex-chars*'      ;<CONSTANT> 
`*css-color:html-colors*'    ;<CONSTANT> 
`css-color:version'	     ;<CONSTANT>\n
;; :CSS-COLOR-MODE-REGEXPS
`*regexp-css-color-html*'    ;<VARIABLE>
`*regexp-css-color-color*'   ;<CONSTANT>
`*regexp-css-color-hex*'     ;<CONSTANT>
`*regexp-css-color-hsl*'     ;<CONSTANT>
`*regexp-css-color-rgb*'     ;<CONSTANT>\n
:SEE :FILE mon-css-color.el
:SEE-ALSO `mon-help-css-properties', `mon-help-css-mode',
`mon-help-css-complete', `mon-help-css-check'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-css-color :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-css-color)
;;; :TEST-ME (mon-help-css-color t)
;;; :TEST-ME (documentation 'mon-help-css-color)
;;; :TEST-ME (apply 'mon-help-css-color '(t))


;;; ==============================
;;; :CHANGESET 1833
;;; :CREATED <Timestamp: #{2010-06-08T18:20:49-04:00Z}#{10232} - by MON KEY>
(defun mon-help-css-properties (&optional insertp intrp)
  "A list of CSS properties.\n

Each CSS property definition begins with a summary of key information that
resembles the following:

:NAME           A property name 
:VALUE          Legal values and syntax
:INIT-VAL       Default initial value for property 
:APPLIES-TO     Elements this property applies to
:INHERITED-P    Whether the property is inherited
:PERCENTAGES    How and if a percentage values is interpreted
:MEDIA-GROUP    Applicative media group(s) of the property

:NOTE The \"Computed value:\" field is not included. W3C-TR provides this field
as indication for the method of deriving the computed value for a property.

;; :CSS-PROPERTIES-INDEX
----
:NAME        azimuth 
VALUE        <ANGLE> | [[ left-side | far-left | left | center-left | center |
             center-right | right | far-right | right-side ] || behind ]
             | leftwards | rightwards | inherit
:INIT-VAL    center
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP aural

----
:NAME        background
:VALUE       [ `background-color` || `background-image` || `background-repeat` ||
               `background-attachment` || `background-position`] | inherit
:INIT-VAL    element-background<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES element-background-position
:MEDIA-GROUP visual

---
:NAME        background-attachment
:VALUE       scroll | fixed | inherit
:INIT-VAL    scroll 
:APPLIES-TO  all 
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---          
:NAME        background-color
:VALUE       <COLOR> | transparent | inherit
:INIT-VAL    transparent
:APPLIES-TO  all 
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---          
:NAME        background-image
:VALUE       <URI> | none | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        background-position
:VALUE       [ [ <PERCENTAGE> | <LENGTH> | left | center | right ] 
             [ <PERCENTAGE> | <LENGTH> | top | center | bottom ]? ] |
             [ [ left | center | right ] || [ top | center | bottom ] ] | inherit
:INIT-VAL    0% 0%
:APPLIES-TO   refer-element-background<-this-element
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        background-repeat
:VALUE       repeat | repeat-x | repeat-y | no-repeat | inherit
:INIT-VAL    repeat
:APPLIES-TO  all
:INHERITED-P no 
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border
:VALUE       [ `border-width` || `border-style` || `border-top-color` ] | inherit
:INIT-VAL    element-border<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-color
:VALUE       [ <COLOR> | transparent ]{1,4} | inherit
:INIT-VAL    element-border-color<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-width
:VALUE       <BORDER-WIDTH>{1,4} | inherit
:INIT-VAL    element-border-width<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-collapse
:VALUE       collapse | separate | inherit
:INIT-VAL    separate
:APPLIES-TO  elment-display<-table element-display<-inline-table
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-spacing
:VALUE       <LENGTH> <LENGTH>? | inherit
:INIT-VAL    0
:APPLIES-TO  elment-display<-table element-display<-inline-table
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-style
:VALUE       <BORDER-STYLE>{1,4} | inherit
:INIT-VAL    element-border-style<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-top
:VALUE       [ `border-width` || `border-style` || `border-top-color` ] | inherit
:INIT-VAL    element-border-top<-this-property-value
             ;; Or, `color` property val or computed value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-top-color
:VALUE       <COLOR> | transparent | inherit
:INIT-VAL    element-border-top-color<-this-color
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        border-top-width
:VALUE       `border-width` | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-top-style
:VALUE       `border-style` | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A  
:MEDIA-GROUP visual

---
:NAME        border-right
:VALUE       [ `border-width` || `border-style` || `border-top-color` ] | inherit
:INIT-VAL    element-border-right<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-right-color
:VALUE       <COLOR> | transparent | inherit
:INIT-VAL    element-border-right-color<-this-color 
          ;; Or, `color` property val or computed value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        border-right-style
:VALUE       `border-style` | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A  
:MEDIA-GROUP visual

---
:NAME        border-right-width
:VALUE       `border-width` | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-bottom
:VALUE       [ `border-width` || `border-style` || `border-top-color` ] | inherit
:INIT-VAL    element-border-bottom<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-bottom-color
:VALUE       <COLOR> | transparent | inherit
:INIT-VAL    element-border-bottom-color<-this-color 
          ;; Or, `color` property val or computed value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        border-bottom-width
:VALUE             <BORDER-WIDTH> | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-bottom-style
:VALUE             <BORDER-STYLE> | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A  
:MEDIA-GROUP visual

---
:NAME        border-left
:VALUE       [ `border-width` || `border-style` || `border-top-color` ] | inherit
:INIT-VAL    element-border-left<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        border-left-color
:VALUE       <COLOR> | transparent | inherit
:INIT-VAL    element-border-left-color<-this-color 
          ;; Or, `color` property val or computed value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        border-left-style
:VALUE       `border-style` | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A  
:MEDIA-GROUP visual

---
:NAME        border-left-width
:VALUE       `border-width` | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        bottom
:VALUE       <LENGTH> | <PERCENTAGE> | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  element<-element-position
:INHERITED-P no
:PERCENTAGES refer-containing-block-height
:MEDIA-GROUP visual

---
:NAME        caption-side
:VALUE       top | bottom | inherit
:INIT-VAL    top
:APPLIES-TO  element-display<-table-caption
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        clear
:VALUE       none | left | right | both | inherit
:INIT-VAL    none
:APPLIES-TO  element-block-level
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        clip
:VALUE       <SHAPE> | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  element<-element-position<-absolute
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual
;; :NOTE <SHAPE> --> rect(`top` `right` `bottom` `left`)

---
:NAME        color
:VALUE       <COLOR> | inherit
:INIT-VAL    user-agent-dependent
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        content
:VALUE       normal | none | 
             [ <STRING> | <URI> | <COUNTER> | attr(<IDENTIFIER>) |
              open-quote | close-quote | no-open-quote | no-close-quote ]+
             | inherit
:INIT-VAL    normal
:APPLIES-TO  element-pseduo-:before and element-pseudo-:after
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP all

---
:NAME        counter-increment
:VALUE       [ <IDENTIFIER> <INTEGER>? ]+ | none | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP all

---
:NAME        counter-reset
:VALUE       [ <IDENTIFIER> <INTEGER>? ]+ | none | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP all

---
:NAME        cue-after
:VALUE       <URI> | none | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        cue-before
:VALUE       <URI> | none | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        cue
:VALUE       [ `cue-before` || `cue-after` ] | inherit
:INIT-VAL    element-cue<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        cursor
:VALUE       [ [<URI> ,]* [ auto | crosshair | default | pointer | move |
               e-resize | ne-resize | nw-resize | n-resize | se-resize |
               sw-resize | s-resize | w-resize | text | wait | help |
               progress ] ] | inherit
:INIT-VAL    auto
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual interactive

---
:NAME        direction
:VALUE       ltr | rtl | inherit
:INIT-VAL    ltr
:APPLIES-TO  all  ;; :SEE W3C-TR for discussion. for exceptions.
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        display
:VALUE       inline | block | list-item | run-in | inline-block | table |
             inline-table | table-row-group | table-header-group |
             table-footer-group | table-row | table-column-group |
             table-column | table-cell | table-caption | none | inherit
:INIT-VAL    inline
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP all

---
:NAME        elevation
:VALUE       <ANGLE> | below | level | above | higher | lower | inherit
:INIT-VAL    level
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        empty-cells
:VALUE       show | hide | inherit
:INIT-VAL    show
:APPLIES-TO   element-display<-table-cell
:INHERITED-P yes
:MEDIA-GROUP visual

---
:NAME        float
:VALUE       left | right | none | inherit
:INIT-VAL    none
:APPLIES-TO  all  ;; :NOTE (but, see 9.7)
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        font-family
:VALUE       [[ <FAMILY-NAME> | <GENERIC-FAMILY> ]
              [ <FAMILY-NAME>| <GENERIC-FAMILY>]* ] | inherit
:INIT-VAL    user-agent-dependent
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual
:NOTE  <GENERIC-FAMILY> -> (serif sans-serif cursive fantasy monospace)
:SEE  W3C-TR \"C.6.3 Section 15.3 Font family: the 'font-family' property\"
 ,----
 | [2009-08-31] The list of keywords in \"\(e.g., 'initial', 'inherit',
 | 'default', 'serif', 'sans-serif', 'monospace', 'fantasy', and 'cursive'\)\"
 | isn't an example, but is in fact the complete and normative list.
 `---- \(URL `http://www.w3.org/TR/CSS2/changes.html#q383'\)

---
:NAME        font-size
:VALUE       <ABSOLUTE-SIZE> | <RELATIVE-SIZE> | <LENGTH> | <PERCENTAGE> | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES refer-element-font-size<-this-element-parent
:MEDIA-GROUP visual

---
:NAME        font-style
:VALUE       normal | italic | oblique | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        font-variant
:VALUE       normal | small-caps | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        font-weight
:VALUE       normal | bold | bolder | lighter |
             100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900 | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        font
:VALUE       [ [ `font-style` || `font-variant` || `font-weight` ]? 
                 `font-size` [ / `line-height` ]? `font-family` ] |
             caption | icon | menu | message-box | small-caption |
             status-bar | inherit
:INIT-VAL    element-font<-this-propery-vale
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES: element-font<-this-property-value
:MEDIA-GROUP visual

---
:NAME        font-size
:VALUE        <absolute-size>  | <relative-size>  | <length>  | <percentage>  | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES element-font-size<-this-property-value
:MEDIA-GROUP visual

---
:NAME        height
:VALUE       <LENGTH> | <PERCENTAGE> | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  :ALL-EXCEPT element-display<-inline-non-replaced
                         element-display<-table-column
                         element-display<-column-group
:INHERITED-P no
:PERCENTAGES  N/A ;; :SEE W3C-TR for discussion
:MEDIA-GROUP visual

---
:NAME        left
:VALUE       <LENGTH> | <PERCENTAGE> | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  element<-element-position
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        letter-spacing
:VALUE       normal | <LENGTH> | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        line-height
:VALUE       normal | <NUMBER> | <LENGTH> | <PERCENTAGE> | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES refer-element-font-size<-this-element
:MEDIA-GROUP visual

---
:NAME        list-style-image
:VALUE       <URI> | none | inherit
:INIT-VAL    none
:APPLIES-TO  element-display<-list-item
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        list-style-position
:VALUE       inside | outside | inherit
:INIT-VAL    element-display<-list-item
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        list-style-type
:VALUE       disc | circle | square | decimal | decimal-leading-zero |
             lower-roman | upper-roman | lower-greek | lower-latin |
             upper-latin | armenian | georgian | lower-alpha | upper-alpha |
             none | inherit
:INIT-VAL    disc
:APPLIES-TO  element-display<-list-item
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        list-style
:VALUE      [ `list-style-type` || `list-style-position` || `list-style-image` ]
            | inherit
:INIT-VAL    element-list-style<-this-property-value
:APPLIES-TO  element-display<-list-item
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        margin
:VALUE       <MARGIN-WIDTH>{1,4} | inherit
:INIT-VAL    element-margin<-this-property-value
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:PERCENTAGES no, refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        margin-right
:VALUE       <MARGIN-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width 
:MEDIA-GROUP visual

---
:NAME        margin-left
:VALUE       <MARGIN-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width 
:MEDIA-GROUP visual

---
:NAME        margin-top
:VALUE       <MARGIN-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        margin-bottom
:VALUE       <MARGIN-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        max-height
:VALUE       <LENGTH> | <PERCENTAGE> | none | inherit
:INIT-VAL    none
:APPLIES-TO  :ALL-EXCEPT element-display<-inline-non-replaced
                         element-display<-table-column
                         element-display<-table-column-group
:INHERITED-P no
:PERCENTAGES N/A ;; :SEE W3C-TR for discussion
:MEDIA-GROUP visual

---
:NAME        max-width
:VALUE       <LENGTH> | <PERCENTAGE> | none | inherit
:INIT-VAL    none
:APPLIES-TO  :ALL-EXCEPT element-display<-inline-non-replaced
                         element-display<-table-row
                         element-display<-table-row-group
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        min-height
:VALUE       <LENGTH> | <PERCENTAGE> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-inline-non-replaced
                         element-display<-table-column
                         element-display<-table-column-group
:INHERITED-P no
:PERCENTAGES N/A ;; :SEE W3C-TR for discussion
:MEDIA-GROUP visual

---
:NAME        min-width
:VALUE       <LENGTH> | <PERCENTAGE> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-inline-non-replaced
                        element-display<-table-row
                        element-display<-table-row-group
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        orphans
:VALUE       <INTEGER> | inherit
:INIT-VAL    2
:APPLIES-TO  element-block-level
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual paged

---
:NAME        outline-color
:VALUE       <COLOR> | invert | inherit
:INIT-VAL    invert
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual interactive

---
:NAME        outline-style
:VALUE       `border-style` | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual interactive

---
:NAME        outline-width
:value `border-width` | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual interactive

---
:NAME        outline
:VALUE       [ `outline-color` || `outline-style` || `outline-width` ] | inherit
:INIT-VAL    element-outline<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual interactive

---
:NAME        overflow
:VALUE       visible | hidden | scroll | auto | inherit
:INIT-VAL    visible
:APPLIES-TO  element-display<-block-non-replaced 
             element-display<-table-cell 
             element-display<-inline-block
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        padding
:VALUE       <PADDING-WIDTH>{1,4} | inherit
:INIT-VAL    element-padding<-this-property-value
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        padding-top
:VALUE       <PADDING-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        padding-right
:VALUE       <PADDING-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        padding-bottom
:VALUE       <PADDING-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        padding-left
:VALUE       <PADDING-WIDTH> | inherit
:INIT-VAL    0
:APPLIES-TO  :ALL-EXCEPT element-display<-table-row-group
                         element-display<-table-header-group
                         element-display<-table-footer-group
                         element-display<-table-row
                         element-display<-table-column-group
                         element-display<-table-column
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        page-break-after
:VALUE       auto | always | avoid | left | right | inherit
:INIT-VAL    auto
:APPLIES-TO  element-block-level ;; :SEE W3C-TR for discussion. for exceptions.
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual paged

---
:NAME        page-break-before
:VALUE       auto | always | avoid | left | right | inherit
:INIT-VAL    auto
:APPLIES-TO  element-block-level ;; :SEE W3C-TR for discussion. for exceptions.
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual paged

---
:NAME        page-break-inside
:VALUE       avoid | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  element-block-level ;; :SEE W3C-TR for discussion. for exceptions.
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual paged

---
:NAME        pause-after
:VALUE       <TIME> | <PERCENTAGE> | inherit
:INIT-VAL    0
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES  N/A ;; :SEE W3C-TR for discussion
:MEDIA-GROUP aural

---
:NAME        pause-before
:VALUE       <TIME> | <PERCENTAGE> | inherit
:INIT-VAL    0
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A ;; :SEE W3C-TR for discussion
:MEDIA-GROUP aural

---
:NAME        pause
:VALUE       [ [<TIME> | <PERCENTAGE>]{1,2} ] | inherit
:INIT-VAL    element-pause<-this-property-value
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A ;; :SEE W3C-TR for discussion `pause-before` and `pause-after`
:MEDIA-GROUP aural

---
:NAME        pitch-range
:VALUE       <NUMBER> | inherit
:INIT-VAL    50
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        pitch
:VALUE       <FREQUENCY> | x-low | low | medium | high | x-high | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        play-during
:VALUE       <URI> [ mix || repeat ]? | auto | none | inherit
:INIT-VAL    auto
:APPLIES-TO  all
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP aural

---
:NAME        position
:VALUE       static | relative | absolute | fixed | inherit
:INIT-VAL    static
:APPLIES-TO  all
:INHERITED-P no
:MEDIA-GROUP visual

---
:NAME        quotes
:VALUE       [<STRING> <STRING>]+ | none | inherit
:INIT-VAL    user-agent-dependent
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        richness
:VALUE       <NUMBER> | inherit
:INIT-VAL    50
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        right
:VALUE       <LENGTH> | <PERCENTAGE> | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  element<-element-position
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        speak-header
:VALUE       once | always | inherit
:INIT-VAL    once
:APPLIES-TO  element-w-table-header-information
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP aural

---
:NAME        speak-numeral
:VALUE       digits | continuous | inherit
:INIT-VAL    continuous
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP aural

---
:NAME        speak-punctuation
:VALUE       code | none | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        speak
:VALUE       normal | none | spell-out | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP aural

---
:NAME        speech-rate
:VALUE       <NUMBER> | x-slow | slow | medium | fast | x-fast | faster | slower
             | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP aural

---
:NAME        stress
:VALUE       <NUMBER> | inherit
:INIT-VAL    50
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP aural

---
:NAME        table-layout
:VALUE       auto | fixed | inherit
:INIT-VAL    auto
:APPLIES-TO  elmenent-display<-table
            element-display<-inline-table
:INHERITED-P no
:PERCENTAGES N/A 
:MEDIA-GROUP visual

---
:NAME        text-align
:VALUE       left | right | center | justify | inherit
:INIT-VAL    elment-direction<-rtl ;; :NOTE Unless element-direction<-ltr
:APPLIES-TO  element-display<-block
             element-display<-table-cell
             element-display<-inline
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        text-decoration
:VALUE       none | [ underline || overline || line-through || blink ] | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P no ;; :SEE W3C-TR for discussion.
:MEDIA-GROUP visual

---
:NAME        text-indent
:VALUE       <LENGTH> | <PERCENTAGE> | inherit
:INIT-VAL    0
:APPLIES-TO  element-display<-block
            element-display<-table-cell
            element-display<-inline
:INHERITED-P yes
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        text-transform
:VALUE       capitalize | uppercase | lowercase | none | inherit
:INIT-VAL    none
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        top
:VALUE       <LENGTH> | <PERCENTAGE> | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  element<-element-position
:INHERITED-P no
:PERCENTAGES refer-containing-block-height
:MEDIA-GROUP visual

---
:NAME        unicode-bidi
:VALUE       normal | embed | bidi-override | inherit
:INIT-VAL    normal
:APPLIES-TO  all ;; :SEE W3C-TR for exceptions.
:INHERITED-P no
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        vertical-align
:VALUE       baseline | sub | super | top | text-top | middle | bottom |
             text-bottom | <PERCENTAGE> | <LENGTH> | inherit
:INIT-VAL    baseline
:APPLIES-TO  element-display<-inline
            element-display<-table-cell
:INHERITED-P no
:PERCENTAGES refer-element-line-height<-this-element
:MEDIA-GROUP visual

---
:NAME        visibility
:VALUE       visible | hidden | collapse | inherit
:INIT-VAL    visible
:APPLIES-TO  all
:INHERITED-P yes
:MEDIA-GROUP visual

---
:NAME        voice-family
:VALUE       [[<SPECIFIC-VOICE> | <GENERIC-VOICE> ],]* 
              [<SPECIFIC-VOICE> | <GENERIC-VOICE> ] | inherit
:INIT-VAL    user-agent-dependent
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP aural

---
:NAME        volume
:VALUE       <NUMBER> | <PERCENTAGE> | 
             silent | x-soft | soft | medium | loud | x-loud | inherit
:INIT-VAL    medium
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES refer-inherited-value 
:MEDIA-GROUP aural

 ---
:NAME        white-space
:VALUE       normal | pre | nowrap | pre-wrap | pre-line | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

---
:NAME        widows
:VALUE       <INTEGER> | inherit
:INIT-VAL    2
:APPLIES-TO  element-block-level
:INHERITED-P yes
:PERCENTAGES N/A 
:MEDIA-GROUP visual paged

---
:NAME        width
:VALUE       <LENGTH> | <PERCENTAGE> | auto | inherit
:INIT-VAL    auto
:APPLIES-TO  :ALL-EXCEPT element-display<-inline-non-replaced
                         element-display<-table-row
                         element-display<-table-row-group
:INHERITED-P no
:PERCENTAGES refer-containing-block-width
:MEDIA-GROUP visual

---
:NAME        word-spacing
:VALUE       normal | <LENGTH> | inherit
:INIT-VAL    normal
:APPLIES-TO  all
:INHERITED-P yes
:PERCENTAGES N/A
:MEDIA-GROUP visual

;; :CSS-BASIC-TYPES\n
 <GENERIC-FAMILY> -> (serif sans-serif cursive fantasy monospace)

 <LENGTH> -> (relative | absolute)
         | relative -> {em ex px}
                    | em -> font-size
                    | ex -> x-height
                    | px -> pixel
         | absolute -> {in cm mm pt pc}
                    | in -> inch
                    | cm -> centimeter
                    | pt -> point -> 1/72 inch
                    | pc -> pica  -> 12 points

<PERCENTAGE> N%

<URI> :NOTE The notation used to designate URIs :SEE RFC3986 in property values is \"url\(\)\"

Following are all subtypes of <NUMBER>:

<FREQUENCY> -> {Hz Khz}
             | Hz -> Hertz
             | Khz -> kilohertz

<TIME> -> {ms|s} 
        | ms -> milliseconds 
        | s  -> seconds

<ANGLE> -> {deg grad rad}
         | deg  -> degrees
         | grad -> grads
         | rad  -> radians

<SHAPE> Has _one_ possible form:
        {clip rect(<top>, <right>, <bottom>, <left>;}

<ABSOLUTE-SIZE>  -> { xx-small | x-small | small | medium | large | x-large | xx-large }
<RELATIVE-SIZE>  -> { larger smaller}
<GENERIC-FAMILY> -> {serif sans-serif cursive fantasy monospace}

<COLOR> -> { named-color #rgb #rrggbb }
        | named-color -> maroon
        | #rgb        -> #00F
        | #rrggbb     -> #5F9EA0


;; :CSS-DESCRIPTOR-INDEX
 __________________________________________________________________________
|                |                                            |            |
|      :NAME     |                   :VALUES                  | :INIT-VAL  |
|----------------¦--------------------------------------------¦------------|
| ascent         |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| baseline       |<NUMBER>                                    |0           |
|----------------¦--------------------------------------------¦------------|
| bbox           |<NUMBER>, <NUMBER>, <NUMBER>, <NUMBER>      |undefined   |
|----------------¦--------------------------------------------¦------------|
| cap-height     |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| centerline     |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| definition-src |<URI>                                       |undefined   |
|----------------¦--------------------------------------------¦------------|
| descent        |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| font-family    |[ <FAMILY-NAME> | <GENERIC-FAMILY> ]        |depends on  |
|                | [, [ <FAMILY-NAME> | <GENERIC-FAMILY> ]]*  |user agent  |
|----------------¦--------------------------------------------¦------------|
| font-size      |all | <LENGTH> [, <LENGTH>]*                |all         |
|----------------¦--------------------------------------------¦------------|
|                |all | [ normal | ultra-condensed |          |            |
|                |extra-condensed | condensed | semi-condensed|            |
|                || semi-expanded | expanded | extra-expanded |            |
| font-stretch   || ultra-expanded ] [, [ normal |            |normal      |
|                |ultra-condensed | extra-condensed |         |            |
|                |condensed | semi-condensed | semi-expanded ||            |
|                |expanded | extra-expanded | ultra-expanded] |            |
|                |]*                                          |            |
|----------------¦--------------------------------------------¦------------|
| font-style     |all | [ normal | italic | oblique ] [,      |all         |
|                |[normal | italic | oblique] ]*              |            |
|----------------¦--------------------------------------------¦------------|
| font-variant   |[normal | small-caps] [,[normal |           |normal      |
|                |small-caps]]*                               |            |
|----------------¦--------------------------------------------¦------------|
|                |all | [normal | bold | 100 | 200 | 300 | 400|            |
| font-weight    || 500 | 600 | 700 | 800 | 900] [, [normal | |all         |
|                |bold | 100 | 200 | 300 | 400 | 500 | 600 |  |            |
|                |700 | 800 | 900]]*                          |            |
|----------------¦--------------------------------------------¦------------|
| mathline       |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| panose-1       |[<INTEGER>]{10}                             |0 0 0 0 0 0 |
|                |                                            |0 0 0 0     |
|----------------¦--------------------------------------------¦------------|
| slope          |<NUMBER>                                    |0           |
|----------------¦--------------------------------------------¦------------|
|                |[ <URI> [format(<STRING> [, <STRING>]*)] |  |            |
| src            |<FONT-FACE-NAME> ] [, <URI> [format(<STRING>|undefined   |
|                |[, <STRING>]*)] | <FONT-FACE-NAME> ]*       |            |
|----------------¦--------------------------------------------¦------------|
| stemh          |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| stemv          |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| topline        |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| unicode-range  |<URANGE> [, <URANGE>]*                      |U+0-7FFFFFFF|
|----------------¦--------------------------------------------¦------------|
| units-per-em   |<NUMBER>                                    |undefined   |
|----------------¦--------------------------------------------¦------------|
| widths         |[<URANGE> ]? [<NUMBER> ]+ [,[<URANGE> ]?    |undefined   |
|                |<NUMBER> ]+]                                |            |
|----------------¦--------------------------------------------¦------------|
| x-height       |<NUMBER>                                    |undefined   |
|________________|____________________________________________|__________75^


The :CSS-PROPERTIES-INDEX list above was adapted from the non-normative index
entitled \"Appendix F. Full property table\". The \"Computed value:\" field
which W3C-TR provides this field as indication for the method of deriving the
computed value for a property has not been included.  Otherwise, the list
reflects (but modifies presentation of) the convetions defined in the the
\"Conventions\" subsection \"CSS property definitions\" of the W3C Technical
Report dated 2009-09-08.
:SEE \(URL `http://www.w3.org/TR/CSS2/about.html'\)\n

The :CSS-DESCRIPTOR-INDEX table above was sourced from W3C-TR recomendation
REC-CSS2-20080411 -- \"Appendix G. Descriptor index\".
:SEE \(URL `http://www.w3.org/TR/2008/REC-CSS2-20080411/css2.txt'\)\n
:SEE-ALSO `mon-help-ebay-template-mode', `mon-help-css-mode',
`mon-help-css-complete', `mon-help-css-check'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-css-properties :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-css-properties)
;;; :TEST-ME (mon-help-css-properties t)
;;; :TEST-ME (documentation 'mon-help-css-properties)
;;; :TEST-ME (apply mon-help-css-properties '(t))

;;; ==============================
(provide 'mon-doc-help-css)
;;; ==============================

;;; ====================================================================
;;; mon-doc-help-css.el ends here
;;; EOF

;;; xsl-mode.el
;; XSL Mode

;; Version 0.9, for XEmacs 21.4+
;; By Chuck Adams <cja987@yahoo.com>
;; Based on xslide.el, by Tony Graham <tkg@menteith.com>

;; This mode only works under XEmacs, as it uses a number of
;; XEmacs-specific functions.  XEmacs version 21.4 and up is
;; recommended, as previous versions had a buggy implementation of
;; buffer-syntactic-context, which this relies heavily on.

;; Requires psgml, which you can get from the xemacs package
;; repository.  All other packages should be included with xemacs.

;; XXX FIXME: general documentation here
;; XXX FIXME: sample init.el here

;; xsl-mode.el is free software
;; As this was derived from a GPL'd work, the contents of this file
;; are licensed for distribution under the terms of the GNU General
;; Public License (version 2 or later).  The text of this license may be
;; found in accompanying file named COPYING.

;; This version is not intended for public release.  If you receive
;; it, the GPL allows you to redistribute it, but please don't without
;; asking for a more recent version.  Thank you.


;; BUGS
;; 
;;   * Numerous FIXME items throughout source, none show-stopping

;; TODO
;;
;;   * Split out XSL-specific stuff and make this a custom XML mode
;;   * Support some actual XSL functionality, e.g. xpath helpers
;;   * Fake SGML DTD (with mostly ANY declarations) for psgml's sake
;;   * Make xsl font-lock cooperate more with psgml font lock
;;   * Search path for xml doc and xsl sheet



;;;; Requirements

(require 'font-lock)
(require 'advice)
(require 'psgml)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
(defcustom xsl-initial-stylesheet-file
  (locate-library "xsl-mode-initial.xsl" t)
  "*File containing initial stylesheet inserted into empty XSL buffers"
  :type '(choice (file :must-match t) (const :tag "No initial stylesheet" nil))
  :group 'xsl)

(defcustom xsl-initial-stylesheet-initial-point 0
  "*Initial position of point in initial stylesheet"
  :type '(integer)
  :group 'xsl)

;; It doesn't appear to work to derive from a derived mode, so much of
;; this was copied verbatim from xml-mode's derivation declaration
(define-derived-mode xsl-mode sgml-mode "XSL"
  ;; xml-mode stuff below
  (setq sgml-xml-p t
	sgml-omittag nil
	sgml-shorttag nil
	sgml-namecase-general nil
	sgml-minimize-attributes nil
	sgml-always-quote-attributes t
	sgml-indent-data t
	sgml-validate-command "nsgmls -wxml -s %s %s")

  (make-local-variable 'sgml-declaration)
  (setq sgml-declaration sgml-xml-declaration)
  
  ;; end of xml mode, back to xsl
  (xsl-build-elements-abbrev-table)
  (add-hook 'pre-abbrev-expand-hook 'xsl-pre-abbrev-expand-hook)

  ;; insert initial stylesheet into blank buffers
  (when (and xsl-initial-stylesheet-file
	     (eq (point-min) (point-max)))
    (insert-file-contents xsl-initial-stylesheet-file)
    (goto-char xsl-initial-stylesheet-initial-point)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Variables

;; XXX TODO generalize these into all the attributes of an xml schema,
;; then generate one.  Reverse it and presto, a parser.

;; XXX FIXME remove compile time dependencies


(eval-and-compile 

(defvar xsl-xslt-ns-prefix "xsl"
  "*Prefix for the XSL namespace")
  
(defvar xsl-fo-ns-prefix "fo"
  "*Prefix for the Formatting Object namespace")

(defvar xsl-element-symbol-alist
  (list
   '("apply-imports"
     "empty"
     ()
     "xai")
   '("apply-templates"
     "block"
     ("select" "mode")
     "xat")
   '("attribute"
     "block"
     ("name" "namespace")
     "xa")
   '("attribute-set"
     "block"
     ("name" "use-attribute-sets")
     "xas")
   '("call-template"
     "block"
     ("name")
     "xct")
   '("choose"
     "block"
     ()
     "xc")
   '("comment"
     "block"
     ()
     "xcm")
   '("copy"
     "block"
     ("use-attribute-sets")
     "xcp")
   '("copy-of"
     "block"
     ("select")
     "xco")
   '("decimal-format"
     "block"
     ("name" "decimal-separator" "grouping-separator" "infinity"
      "minus-sign" "NaN" "percent" "per-mille" "zero-digit"
      "digit" "pattern-separator")
     "xdf")
   '("element"
     "block"
     ("name" "namespace" "use-attribute-sets")
     "xe")
   '("fallback"
     "block"
     ()
     "xfb")
   '("for-each"
     "block"
     ("select")
     "xfe")
   '("if"
     "block"
     ("test")
     "xif")
   '("import"
     "empty"
     ("href")
     "xim")
   '("include"
     "empty"
     ("href")
     "xinc")
   '("key"
     "block"
     ("name" "match" "use")
     "xk")
   '("message"
     "block"
     ("terminate")
     "xme")
   '("namespace-alias"
     "block"
     ("stylesheet-prefix" "result-prefix")
     "xna")
   '("number"
     "empty"
     ("level" "count" "from" "value" "format" "lang" "letter-value"
      "grouping-separator" "grouping-size")
     "xn")
   '("otherwise"
     "block"
     ()
     "xo")
   '("output"
     "empty"
     ("method" "version" "encoding" "omit-xml-declaration"
      "standalone" "doctype-public" "doctype-system"
      "cdata-section-elements" "indent" "media-type")
     "xout")
   '("param"
     "block"
     ("name" "select")
     "xpa")
   '("preserve-space"
     "empty"
     ("elements")
     "xps")
   '("processing-instruction"
     "block"
     ("name")
     "xpi")
   '("sort"
     "empty"
     ("select" "lang" "data-type" "order" "case-order")
     "xso")
   '("strip-space"
     "empty"
     ("elements")
     "xss")
   (list "stylesheet"
     "block"
     (list
      '("id" nil)
      '("extension-element-prefixes" nil)
      '("exclude-result-prefixes" nil)
      '("version" nil)
      '("xmlns" nil)
      '("xmlns:xsl" t)
      '("xmlns:fo" nil))
     "xs")
   '("template"
     "block"
     ("match" "mode" "priority" "name")
     "xt")
   '("text"
     "inline"
     ("disable-output-escaping")
     "xtxt")
   (list "transform"
     "block"
     (list
      '("id" nil)
      '("extension-element-prefixes" nil)
      '("exclude-result-prefixes" nil)
      '("version" nil)
      '("xmlns" nil)
      '("xmlns:xsl" t)
      '("xmlns:fo" nil))
     "xtran")
   '("value-of"
     "empty"
     ("select" "disable-output-escaping")
     "xvo")
   '("variable"
     "block"
     ("name" "select")
     "xva")
   '("when"
     "block"
     ("test")
     "xw")
   '("with-param"
     "block"
     ("name" "select")
     "xwp")))

(defvar xsl-attributes-alist
  (list
   '("NaN" "nan" ())
   '("cdata-section-elements" "cds" ())
   '("count" "cnt" ())
   '("data-type" "dt" ())
   '("decimal-separator" "ds" ())
   '("digit" "dig" ())
   '("disable-output-escaping" "doe" ())
   '("doctype-public" "dtp" ())
   '("doctype-system" "dts" ())
   '("elements" "ele" ())
   '("encoding" "enc" ())
   '("exclude-result-prefixes" "erp" ())
   '("extension-element-prefixes" "eep" ())
   '("format" "fmt" ())
   '("from" "fr" ())
   '("grouping-separator" "gsep" ())
   '("grouping-size" "gsiz" ())
   '("href" "href" ())
   '("id" "id" ())
   '("indent" "ind" ())
   '("infinity" "inf" ())
   '("lang" "l" ())
   '("letter-value" "lv" ())
   '("level" "lvl" ())
   '("match" "m" ())
   '("media-type" "mt" ())
   '("method" "meth" ())
   '("minus-sign" "ms" ())
   '("mode" "mo" ())
   '("n-digits-per-group" "ndpg" ())
   '("name" "n" ())
   '("namespace" "ns" ())
   '("omit-xml-declaration" "oxml" ())
   '("order" "o" ())
   '("pattern-separator" "ps" ())
   '("per-mille" "pm" ())
   '("percent" "perc" ())
   '("priority" "p" ())
   '("result-prefix" "rp" ())
   '("select" "s" ())
   '("standalone" "stand" ())
   '("stylesheet-prefix" "spr" ())
   '("terminate" "ter" ())
   '("test" "t" ())
   '("use" "use" ())
   '("use-attribute-sets" "ua" ())
   '("value" "v" ())
   '("version" "ver" ())
   '("xmlns" "xn" ())
   '("xmlns:fo" "xnf" ())
   '("xmlns:xsl" "xnx" ("http://www.w3.org/1999/XSL/Transform"))
   '("zero-digit" "zd" ())))

(defvar xsl-fo-symbol-alist
  (list
   '("basic-link" "inline" () "fbl")
   '("bidi-override" "inline" () "fbo")
   '("block" "block" () "fb")
   '("block-container" "block" () "fbc")
   '("character" "empty" () "fc")
   '("color-profile" "empty" () "fcp")
   '("conditional-page-master-reference" "empty" () "fcpmr")
   '("declarations" "block" () "fd")
   '("external-graphic" "empty" () "feg")
   '("float" "block" () "ff")
   '("flow" "block" () "ff")
   '("footnote" "block" () "ff")
   '("footnote-body" "block" () "ffb")
   '("initial-property-set" "empty" () "fips")
   '("inline" "inline" () "fi")
   '("inline-container" "inline" () "fic")
   '("instream-foreign-object" "block" () "fifo")
   '("layout-master-set" "block" () "flms")
   '("leader" "inline" () "fl")
   '("list-block" "block" () "flb")
   '("list-item" "block" () "fli")
   '("list-item-body" "block" () "flib")
   '("list-item-label" "block" () "flil")
   '("marker" "inline" () "fm")
   '("multi-case" "inline" () "fmc")
   '("multi-properties" "block" () "fmp")
   '("multi-property-set" "empty" () "fmps")
   '("multi-switch" "block" () "fms")
   '("multi-toggle" "inline" () "fmt")
   '("page-number" "empty" () "fpn")
   '("page-number-citation" "empty" () "fpnc")
   '("page-sequence" "block" () "fps")
   '("page-sequence-master" "block" () "fpsm")
   '("region-after" "empty" () "fra")
   '("region-before" "empty" () "frb")
   '("region-body" "empty" () "frb")
   '("region-end" "empty" () "fre")
   '("region-start" "empty" () "frs")
   '("repeatable-page-master-alternatives" "block" () "frpma")
   '("repeatable-page-master-reference" "empty" () "frpmr")
   '("retrieve-marker" "empty" () "frm")
   '("root" "block" () "fr")
   '("simple-page-master" "block" () "fspm")
   '("single-page-master-reference" "empty" () "fspm")
   '("static-content" "block" () "fsc")
   '("table" "block" () "ft")
   '("table-and-caption" "block" () "ftac")
   '("table-body" "block" () "ftb")
   '("table-caption" "block" () "ftc")
   '("table-cell" "block" () "ftc")
   '("table-column" "empty" () "ftc")
   '("table-footer" "block" () "ftf")
   '("table-header" "block" () "fth")
   '("table-row" "block" () "ftr")
   '("title" "inline" () "ft")
   '("wrapper" "inline" () "fw")))

(defvar xsl-fo-attribute-symbol-alist
  (list
   '("absolute-position" "ap")
   '("active-state" "as")
   '("alignment-adjust" "aa")
   '("alignment-baseline" "ab")
   '("auto-restore" "ar")
   '("azimuth" "a")
   '("background" "b")
   '("background-attachment" "ba")
   '("background-color" "bc")
   '("background-image" "bi")
   '("background-position" "bp")
   '("background-position-horizontal" "bph")
   '("background-position-vertical" "bpv")
   '("background-repeat" "br")
   '("baseline-shift" "bs")
   '("blank-or-not-blank" "bon")
   '("block-progression-dimension" "bpd")
   '("block-progression-dimension.maximum" "bpdmax")
   '("block-progression-dimension.minimum" "bpdmin")
   '("block-progression-dimension.optimum" "bpdopt")
   '("border" "b")
   '("border-after-color" "bac")
   '("border-after-precedence" "bap")
   '("border-after-style" "bas")
   '("border-after-width" "baw")
   '("border-after-width.conditionality" "bawc")
   '("border-after-width.length" "bawl")
   '("border-before-color" "bbc")
   '("border-before-precedence" "bbp")
   '("border-before-style" "bbs")
   '("border-before-width" "bbw")
   '("border-before-width.conditionality" "bbwc")
   '("border-before-width.length" "bbwc")
   '("border-bottom" "bb")
   '("border-bottom-color" "bbc")
   '("border-bottom-style" "bbs")
   '("border-bottom-width" "bbw")
   '("border-bottom-width.conditionality" "bbwc")
   '("border-bottom-width.length" "bbwl")
   '("border-collapse" "bc")
   '("border-color" "bc")
   '("border-end-color" "bec")
   '("border-end-precedence" "bep")
   '("border-end-style" "bes")
   '("border-end-width" "bew")
   '("border-end-width.conditionality" "bewc")
   '("border-end-width.length" "bewl")
   '("border-left" "bl")
   '("border-left-color" "blc")
   '("border-left-style" "bls")
   '("border-left-width" "blw")
   '("border-left-width.conditionality" "blwc")
   '("border-left-width.length" "blwl")
   '("border-right" "br")
   '("border-right-color" "brc")
   '("border-right-style" "brs")
   '("border-right-width" "brw")
   '("border-right-width.conditionality" "brwc")
   '("border-right-width.length" "brwl")
   '("border-separation.block-progression-direction" "bsbpd")
   '("border-separation.inline-progression-direction" "bsipd")
   '("border-spacing" "bs")
   '("border-start-color" "bsc")
   '("border-start-precedence" "bsp")
   '("border-start-style" "bss")
   '("border-start-width" "bsw")
   '("border-start-width.conditionality" "bswc")
   '("border-start-width.length" "bswl")
   '("border-style" "bs")
   '("border-top" "bt")
   '("border-top-color" "btc")
   '("border-top-style" "bts")
   '("border-top-width" "btw")
   '("border-top-width.conditionality" "btwc")
   '("border-top-width.length" "btwl")
   '("border-width" "bw")
   '("bottom" "b")
   '("break-after" "ba")
   '("break-before" "bb")
   '("caption-side" "cs")
   '("case-name" "cn")
   '("case-title" "ct")
   '("character" "ch")
   '("clear" "cl")
   '("clip" "cli")
   '("color" "c")
   '("color-profile-name" "cpn")
   '("column-count" "cc")
   '("column-gap" "cg")
   '("column-number" "cn")
   '("column-width" "cw")
   '("content-height" "ch")
   '("content-type" "ct")
   '("content-width" "cw")
   '("country" "c")
   '("cue" "c")
   '("cue-after" "ca")
   '("cue-before" "cb")
   '("destination-placement-offset" "dpo")
   '("direction" "d")
   '("display-align" "da")
   '("dominant-baseline" "db")
   '("elevation" "e")
   '("empty-cells" "ec")
   '("end-indent" "ei")
   '("ends-row" "er")
   '("extent" "e")
   '("external-destination" "ed")
   '("float" "f")
   '("flow-name" "fn")
   '("font" "f")
   '("font-family" "ff")
   '("font-selection-strategy" "fss")
   '("font-size" "fs")
   '("font-size-adjust" "fsa")
   '("font-stretch" "fs")
   '("font-style" "fs")
   '("font-variant" "fv")
   '("font-weight" "fw")
   '("force-page-count" "fpc")
   '("format" "f")
   '("glyph-orientation-horizontal" "goh")
   '("glyph-orientation-vertical" "gov")
   '("grouping-separator" "gs")
   '("grouping-size" "gs")
   '("height" "h")
   '("hyphenate" "hy")
   '("hyphenation-character" "hc")
   '("hyphenation-keep" "hk")
   '("hyphenation-ladder-count" "hlc")
   '("hyphenation-push-character-count" "hpc")
   '("hyphenation-remain-character-count" "hrcc")
   '("id" "i")
   '("indicate-destination" "id")
   '("initial-page-number" "ipn")
   '("inline-progression-dimension" "ipd")
   '("inline-progression-dimension.maximum" "ipdmax")
   '("inline-progression-dimension.minimum" "ipdmin")
   '("inline-progression-dimension.optimum" "ipdopt")
   '("internal-destination" "id")
   '("intrusion-displace" "id")
   '("keep-together.within-column" "ktc")
   '("keep-together.within-line" "ktl")
   '("keep-together.within-page" "ktp")
   '("keep-with-next.within-column" "kwnc")
   '("keep-with-next.within-line" "kwnl")
   '("keep-with-next.within-page" "kwnp")
   '("keep-with-previous.within-column" "kwpc")
   '("keep-with-previous.within-line" "kwpl")
   '("keep-with-previous.within-page" "kwpp")
   '("language" "la")
   '("last-line-end-indent" "lle")
   '("leader-alignment" "la")
   '("leader-length" "ll")
   '("leader-length.maximum" "llmax")
   '("leader-length.minimum" "llmin")
   '("leader-length.optimum" "llopt")
   '("leader-pattern" "lp")
   '("leader-pattern-width" "lpw")
   '("left" "le")
   '("letter-spacing" "ls")
   '("letter-value" "lv")
   '("line-height" "lh")
   '("line-height-shift-adjustment" "lhs")
   '("line-stacking-strategy" "lss")
   '("linefeed-treatment" "lt")
   '("margin" "m")
   '("margin-bottom" "mb")
   '("margin-left" "ml")
   '("margin-right" "mr")
   '("margin-top" "mt")
   '("marker-class-name" "mcn")
   '("master-name" "mn")
   '("master-reference" "mr")
   '("max-height" "mh")
   '("max-width" "mw")
   '("maximum-repeats" "mr")
   '("media-usage" "mu")
   '("min-height" "mh")
   '("min-width" "mw")
   '("number-columns-repeated" "ncr")
   '("number-columns-spanned" "ncs")
   '("number-rows-spanned" "nrs")
   '("odd-or-even" "ooe")
   '("orphans" "or")
   '("overflow" "ov")
   '("padding" "pd")
   '("padding-after" "pa")
   '("padding-before" "pb")
   '("padding-bottom" "pb")
   '("padding-end" "pe")
   '("padding-left" "pl")
   '("padding-right" "pr")
   '("padding-start" "ps")
   '("padding-top" "pt")
   '("page-break-after" "pba")
   '("page-break-before" "pbb")
   '("page-break-inside" "pbi")
   '("page-height" "ph")
   '("page-position" "pp")
   '("page-width" "pw")
   '("pause" "p")
   '("pause-after" "pa")
   '("pause-before" "pb")
   '("pitch" "p")
   '("pitch-range" "pr")
   '("play-during" "pd")
   '("position" "p")
   '("precedence" "p")
   '("provisional-distance-between-starts" "pdbs")
   '("provisional-label-separation" "pls")
   '("ref-id" "rid")
   '("reference-orientation" "ro")
   '("region-name" "rn")
   '("relative-align" "ra")
   '("relative-position" "rp")
   '("rendering-intent" "ri")
   '("retrieve-boundary" "rb")
   '("retrieve-class-name" "rcn")
   '("retrieve-position" "rp")
   '("richness" "rich")
   '("right" "rig")
   '("role" "ro")
   '("rule-style" "rs")
   '("rule-thickness" "rt")
   '("scaling" "sc")
   '("scaling-method" "sm")
   '("score-spaces" "ss")
   '("script" "scr")
   '("show-destination" "sde")
   '("size" "si")
   '("source-document" "sdo")
   '("space-after" "sa")
   '("space-before" "sb")
   '("space-end" "se")
   '("space-start" "ss")
   '("span" "spn")
   '("speak" "spe")
   '("speak-header" "sh")
   '("speak-numeral" "sn")
   '("speak-punctuation" "sp")
   '("speech-rate" "sr")
   '("src" "src")
   '("start-indent" "si")
   '("starting-state" "ss")
   '("starts-row" "sr")
   '("stress" "str")
   '("suppress-at-line-break" "salb")
   '("switch-to" "st")
   '("table-layout" "tl")
   '("table-omit-footer-at-break" "tofb")
   '("table-omit-header-at-break" "tohb")
   '("target-presentation-context" "tpc")
   '("target-processing-context" "tpc")
   '("target-stylesheet" "ts")
   '("text-align" "ta")
   '("text-align-last" "tal")
   '("text-altitude" "ta")
   '("text-decoration" "td")
   '("text-depth" "td")
   '("text-indent" "ti")
   '("text-shadow" "ts")
   '("text-transform" "tt")
   '("top" "top")
   '("treat-as-word-space" "taw")
   '("unicode-bidi" "ub")
   '("vertical-align" "va")
   '("visibility" "vi")
   '("voice-family" "vf")
   '("volume" "vo")
   '("white-space" "ws")
   '("white-space-collapse" "wsc")
   '("white-space-treatment" "wst")
   '("widows" "wdw")
   '("width" "w")
   '("word-spacing" "ws")
   '("wrap-option" "wo")
   '("writing-mode" "wm")
   '("xml:lang" "xl")
   '("z-index" "zi")))

) ; end eval-and-compile

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font lock

;; Define mode-specific faces


(eval-and-compile 

(defface xsl-xslt-main-face
  '((((background light))
     (:foreground "SlateBlue4"))
    (((background dark))
     (:foreground "Wheat")))
  "Used for local name portion of XSLT elements and attributes"
  :group 'xsl-faces)
(defvar xsl-xslt-main-face 'xsl-xslt-main-face
  "Used for local name portion of XSLT elements and attributes")

(defface xsl-xslt-alternate-face
  '((((background light))
     (:foreground "SlateBlue2"))
    (((background dark))
     (:foreground "LightGray")))
  "Used for prefix and colon portion of XSLT elements and attributes"
  :group 'xsl-faces)

(defvar xsl-xslt-alternate-face 'xsl-xslt-alternate-face
  "Used for prefix and colon portion of XSLT elements and attributes")

(defface xsl-fo-main-face
  '((((background light))
     (:foreground "darkorchid4"))
    (((background dark))
     (:foreground "PaleGreen")))
  "Used for local name portion of formatting object elements and attributes"
  :group 'xsl-faces)
(defvar xsl-fo-main-face 'xsl-fo-main-face
  "Used for local name portion of formatting object elements and attributes")

(defface xsl-fo-alternate-face
  '((((background light))
     (:foreground "darkorchid3"))
    (((background dark))
     (:foreground "Yellow")))
  "Used for prefix and colon portion of formatting object elements and attributes"
  :group 'xsl-faces)
(defvar xsl-fo-alternate-face 'xsl-fo-alternate-face
  "Used for prefix and colon portion of formatting object elements and attributes")

(defface xsl-other-element-face
  '((((background light))
     (:foreground "Blue"))
    (((background dark))
     (:foreground "Coral")))
  "Used for literal result element tags"
  :group 'xsl-faces)

(defvar xsl-other-element-face 'xsl-other-element-face
  "Used for literal result element tags")

) ;; end eval-and-compile
;;;;; Constants

(eval-and-compile
  (defvar xsl-font-lock-keywords
    (list
     ;;
     ;; Reserved XML Processing instruction lookalikes
     ;;
     '(
       "\\(<\\?\\)\\(xml\\)\\(\\s-+version\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+encoding\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+standalone\\s-*=\\s-*\\('\\(yes\\|no\\)'\\|\"\\(yes\\|no\\)\"\\)\\)?\\s-*\\(\\?>\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil)
       (3 font-lock-type-face nil t)
       (5 font-lock-type-face nil t)
       (7 font-lock-type-face nil t)
       (11 font-lock-keyword-face))
     ;;
     ;; Non-reserved XML Processing instruction
     ;; Any XML PI that doesn't start with "<?xml"
     ;;
     '("\\(<\\?\\)\\([^ \t?>]+\\)[ \t]*\\([^?>]\\|\\?[^>]\\|>[^\n\r]\\)*\\(\\?>\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face)
       (4 font-lock-keyword-face))
     ;;
     ;; Marked section start
     ;;
     '("\\(<!\\[\\)[^[]*\\(\\[\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-keyword-face))
     ;;
     ;; XSL formatting objects
     ;;
     (list
      (concat "\\(</?\\)\\(" xsl-fo-ns-prefix ":\\)\\("
	      (regexp-opt
	       (mapcar 'car xsl-fo-symbol-alist))
	      "\\)\\(\\s-+\\([^/>]\\|/[^>]\\)+\\)*\\(/?>\\|$\\)")
      '(1 xsl-fo-main-face)
      '(2 xsl-fo-alternate-face)
      '(3 xsl-fo-main-face))
     (list
      (concat "</?" xsl-fo-ns-prefix ":\\([^/>]\\|/[^>]\\)*\\(/?>\\)")
      '(2 xsl-fo-main-face))
     ;;
     ;; XSL elements
     ;;
     (list
      (concat "\\(</?\\)\\(" xsl-xslt-ns-prefix ":\\)\\("
	      (regexp-opt
	       (mapcar 'car xsl-element-symbol-alist))
	      "\\)\\(\\s-+[^= 	]+[ 	]*=[ 	]*\\('[^']*'\\|\"[^\"]*\"\\)\\)*\\s-*\\(/?>\\)")
      '(1 xsl-xslt-main-face)
      '(2 xsl-xslt-alternate-face)
      '(3 xsl-xslt-main-face))
     (list
      (concat "</?" xsl-xslt-ns-prefix ":\\S-+\\(\\s-+[^=> 	]+[ 	]*=[ 	]*\\('[^']*'\\|\"[^\"]*\"\\)\\)*\\s-*\\(/?>\\)")
      '(3 xsl-xslt-main-face))
     ;;
     ;; XSL attributes
     ;;
     (let* ((xsl-attributes-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-attributes-alist)
	      t))
	    (xsl-attributes-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-attributes-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-attributes-alist-regexp
	 "[ \t]*=[ \t]*\"\\)"
	 "\\([^\"<]*\\)"
	 "\\(\"\\)")
	(list 1 xsl-xslt-alternate-face)
	(list (+ 2 xsl-attributes-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-attributes-alist-regexp-depth)
	      xsl-xslt-alternate-face)))
     ;; do again with single-quote delimiters
     (let* ((xsl-attributes-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-attributes-alist)
	      t))
	    (xsl-attributes-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-attributes-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-attributes-alist-regexp
	 "[ \t]*=[ \t]*'\\)"
	 "\\([^'<]*\\)"
	 "\\('\\)")
	(list 1 xsl-xslt-alternate-face)
	(list (+ 2 xsl-attributes-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-attributes-alist-regexp-depth)
	      xsl-xslt-alternate-face)))
     ;;
     ;; XSL formatting object properties
     ;;
     (let* ((xsl-fo-attribute-symbol-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-fo-attribute-symbol-alist)
	      t))
	    (xsl-fo-attribute-symbol-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-fo-attribute-symbol-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-fo-attribute-symbol-alist-regexp
	 "[ \t]*=[ \t]*\"\\)"
	 "\\([^\"<]*\\)"
	 "\\(\"\\)")
	(list 1 xsl-fo-alternate-face 'append)
	(list (+ 2 xsl-fo-attribute-symbol-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-fo-attribute-symbol-alist-regexp-depth)
	      xsl-fo-alternate-face)))
     ;; do again with single-quote delimiters
     (let* ((xsl-fo-attribute-symbol-alist-regexp
	     (regexp-opt
	      (mapcar 'car xsl-fo-attribute-symbol-alist)
	      t))
	    (xsl-fo-attribute-symbol-alist-regexp-depth
	     (regexp-opt-depth
	      (regexp-opt
	       (mapcar 'car xsl-fo-attribute-symbol-alist)
	       t))))
       (list
	(concat
	 "\\b\\("
	 xsl-fo-attribute-symbol-alist-regexp
	 "[ \t]*=[ \t]*'\\)"
	 "\\([^'<]*\\)"
	 "\\('\\)")
	(list 1 xsl-fo-alternate-face 'append)
	(list (+ 2 xsl-fo-attribute-symbol-alist-regexp-depth)
	      font-lock-variable-name-face)
	(list (+ 3 xsl-fo-attribute-symbol-alist-regexp-depth)
	      xsl-fo-alternate-face)))
     ;;
     ;; Mark the start and end of literals, but don't do anything to their
     ;; contents
     ;;
     '("\\('\\)[^']*\\('\\)"
       (1 font-lock-string-face)
       (2 font-lock-string-face))
     '("\\(\"\\)[^\"]*\\(\"\\)"
       (1 font-lock-string-face)
       (2 font-lock-string-face))
     ;;
     ;; { } in attribute values
     ;;
;;     '("\\('\\|\"\\)\\([^{\\1]\\|{{\\)*\\({[^\\1}]*}\\)\\([^{\\1]\\|{{\\)*\\(\\1\\)"
     '("'\\([^{'<]\\|{{\\)*\\({[^'}<]*}\\)\\([^{'<]\\|{{\\)*'"
       (2 font-lock-variable-name-face t))
     '("\"\\([^{\"<]\\|{{\\)*\\({[^\"}<]*}\\)\\([^{\"<]\\|{{\\)*\""
       (2 font-lock-variable-name-face t))
     ;;
     ;; Text inside <xsl:text>
     (list
      (concat "<" xsl-xslt-ns-prefix ":text>"
	      "\\([^<]*\\)"
	      "</" xsl-xslt-ns-prefix ":text>")
      '(1 font-lock-string-face append))
     ;;
     ;; "Other" tags
     ;;
     (list
      (concat "\\(</?\\([^xf/\?!]\\|x[^s]\\|xs[^l]\\|xsl[^:]\\|f[^o]\\|fo[^:]\\)\\([^</>]\\|/[^>]\\)*/?>\\)")
      '(1 xsl-other-element-face t))
     ;;
     ;; Content of tags
     ;;
     (list
      (concat ">\\([^<]+\\)<")
      '(1 font-lock-string-face keep))
     ;;
     ;; Entity references
     ;;
     '("\\([%&][^; \t]+;\\)"
       (1 font-lock-reference-face t))
     ;;
     ;; Put comment patterns last so they mask anything
     ;; that might be inside the comment
     ;;
     '("\\(<!--[^-]*\\(-[^-]+\\)*-->\\)"
       (1 font-lock-comment-face t))
     )
    "Additional expressions to highlight in XSL mode.")
) ;; end eval-and-compile

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; processing
(defcustom xsl-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[xsl-process] is run."
  :type '(choice (const :tag "Yes" t) (const :tag "No" nil))
  :group 'xsl-process)

(defcustom xsl-process-command
  (list
   ;; XT Windows executable
   "xt %i %s %o"
   ;; XT Java
   "java com.jclark.xsl.Driver %i %s %o"
   ;; Instant Saxon
   "saxon -o %o %i %s"
   ;; Instant Saxon using xml-stylesheet PI
   "saxon -o %o %i"
   ;; Saxon
   "java com.icl.saxon.StyleSheet -o %o %i %s"
   ;; Saxon using xml-stylesheet PI
   "java com.icl.saxon.StyleSheet -o %o %i")
  "*The shell command to process an XSL document.

This is a `format' control string that by default should contain three
`%s' conversion specifications: the first will be replaced by the
value of xsl-process-input-file \(or the empty string, if nil\); the
second will be replaced by xsl-process-stylesheet-file \(or the empty
string, if nil\); the third will be replaced by
xsl-process-output-file \(or the empty string, if nil\).

If `xsl-process-files' is non-nil, the format string should contain
one `%s' conversion specification for each element of its result.

If xsl-process-command is a list, then every element should be a
string.  The strings will be tried in order and %-sequences in the
string will be replaced according to the list below, if the string contains
%-sequences with no replacement value the next string will be tried.

%b means the visited file of the current buffer
%i means the value of xsl-process-input-file
%s means the value of xsl-process-stylesheet-file
%o means the value of xsl-process-output-file
"
  :type '(repeat :tag "Commands" string)
  :group 'xsl-process)

(defvar xsl-process-files nil
  "If non-nil, a function of no arguments that returns a list of file names.
These file names will serve as the arguments to the `xsl-process-command'
format control string instead of the defaults.")

(defcustom xsl-process-error-regexps
  '(("file:\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)
    ("file:/\\(\\([A-Za-z]:\\)?[^:]+\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?" 1 3 5)
    ("^Error at [^ ]+ on line \\([0-9]+\\) of file:\\([^:]+\\):$" 2 1)
    ("^Error at [^ ]+ on line \\([0-9]+\\) of file:/\\(\\([a-z]:\\)?[^:]+\\):$" 2 1))
  "Alist of regexps to recognize error messages from `xsl-process'.
See `compilation-error-regexp-alist'."
  :type '(repeat :tag "Regexps"
		 (list (regexp :tag "Regexp")
		       (integer :tag "File index")
		       (integer :tag "Line index")
		       (choice :tag "Column index"
			       (integer)
			       (const :tag "None" nil))))
  :group 'xsl-process)

(defvar xsl-xml-source nil
  "*If non-nil, this is the name of the XML source file.")
(put 'xsl-xml-source 'xsl-type 'string)

(defvar xsl-xsl-result nil
  "*If non-nil, this is the name of the XSL result file.")
(put 'xsl-xsl-result 'xsl-type 'string)

(defvar xsl-process-command-history nil
  "The minibuffer history list for `xsl-process''s COMMAND argument.")
(make-variable-buffer-local 'xsl-process-command-history)

(defvar xsl-process-input-file nil
  "Filename of input file for `xsl-process' command")
(make-variable-buffer-local 'xsl-process-input-file)

(defvar xsl-process-input-history nil
  "The minibuffer history list for `xsl-get-process-parameters''s INPUT argument.")

(defvar xsl-process-stylesheet-file nil
  "Filename of stylesheet file for `xsl-process' command")
(make-variable-buffer-local 'xsl-process-stylesheet-file)

(defvar xsl-process-stylesheet-history nil
  "The minibuffer history list for `xsl-get-process-parameters''s STYLESHEET argument.")

(defvar xsl-process-output-file nil
  "Filename of output file for `xsl-process' command")
(make-variable-buffer-local 'xsl-process-output-file)

(defvar xsl-process-output-history nil
  "The minibuffer history list for `xsl-get-process-parameters''s OUTPUT argument.")

(defvar xsl-process-command-entered nil
  "Whether a process command has been entered for this buffer")
(make-variable-buffer-local 'xsl-process-command-entered)

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun xsl-subst-expand-char (c parts)
  (cdr-safe (assq (downcase c) parts)))

(defun xsl-subst-expand (s parts)
  (loop for i from 0 to (1- (length s))
    as c = (aref s i)
    concat (if (eq c ?%)
	       (or (xsl-subst-expand-char (aref s (incf i)) parts)
		   (return nil)) 
	     (char-to-string (aref s i)))))

(defun xsl-populate-process-command-history ()
  (cond
   ((consp xsl-process-command)
    (let ((process-subst
	   (list
	    (cons ?b (and (buffer-file-name)
			  (file-name-nondirectory (buffer-file-name))))
	    (cons ?i xsl-process-input-file)
	    (cons ?s xsl-process-stylesheet-file)
	    (cons ?o xsl-process-output-file))))
      (setq xsl-process-command-history
	    (append
	     (mapcar (lambda (x)
		       (xsl-subst-expand x process-subst))
		     xsl-process-command)
	     xsl-process-command-history))))
   (t
    (apply 'format xsl-process-command
	   (if xsl-process-files
	       (funcall xsl-process-files)
	     (list (or xsl-xml-source "")
		   (let ((name (buffer-file-name)))
		     (if name
			 (file-name-nondirectory name)
		       ""))
		   (or xsl-xsl-result "")))))))

(defun xsl-read-file-name (prompt default history)
  "Read filename from minibuffer with default and command history."
    (read-file-name prompt nil default t default history))

(defun xsl-get-process-parameters ()
  "Get and set the parameters for the `xsl-process' command"
  (interactive)

  (setq xsl-process-input-file
	(xsl-read-file-name "Input file: "
			    (concat (file-name-sans-extension
				     (file-name-nondirectory
				      (buffer-file-name)))
				    ".xml")
			    'xsl-process-input-history)
	xsl-process-stylesheet-file
	(xsl-read-file-name "Stylesheet file: "
			    (file-name-nondirectory
			     (buffer-file-name))
			    'xsl-process-stylesheet-history)
	xsl-process-output-file
	(xsl-read-file-name "Output file: "
			    (concat (file-name-sans-extension
				     (file-name-nondirectory
				      xsl-process-input-file))
				    ".html")
			    'xsl-process-output-history))
  
  (setq xsl-process-command-entered t))

(defun xsl-process (command)
  "Process an XSL stylesheet.

Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *XSL process*.  You can then use the
command \\[next-error] to find the next error message and move to the
line in the XSL document that caused it.

The first time that the program is run and whenever you provide a
prefix argument, e.g. \\[universal-argument] \\[xsl-process], prompts
for input filename, stylesheet file, and output filename.  Those
values are used with the templates in `xsl-process-command' to
populate this command's command history with the command lines to run
several XSLT processors using those values.  Use M-p and M-n to step
through the predefined commands, edit a command if necessary, or enter
a new command line.  The next time that this command is run, the
previously executed command is used as the default."
  (interactive
   (list (progn
	   (if (or
		current-prefix-arg
		(null xsl-process-command-entered)
		(null xsl-process-command-history))
	       (progn
		 (xsl-get-process-parameters)
		 (xsl-populate-process-command-history)))
	   (read-from-minibuffer "Process command: "
				     (car xsl-process-command-history)
				     nil nil
				     'xsl-process-command-history))))
  (xsl-do-compile command))

(defun xsl-auto-compile ()
  "Process the current stylesheet based on current settings.
Prompts for settings if none have been specified for this buffer"
  (interactive)
  (let ((cmd (car xsl-process-command-history)))
    (if cmd (xsl-do-compile cmd)
      (call-interactively 'xsl-process))))

(defun xsl-do-compile (command)
  (when xsl-offer-save (save-some-buffers nil nil))
  (compile-internal command "No more errors" "XSL process"
		    nil
		    xsl-process-error-regexps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Define core `xsl' group.
(defgroup xsl nil
  "Major mode for editing XSL."
  :prefix "xsl-"
  :group 'languages)

(defgroup xsl-faces nil
  "Font faces used in XSL mode."
  :group 'xsl
  :group 'faces)

(defgroup xsl-process nil
  "Running XSL processors from XSL mode."
  :group 'xsl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

(define-key xsl-mode-map [return] 'newline-and-indent)
(define-key xsl-mode-map [space]        'xsl-electric-space)
(define-key xsl-mode-map "="   	  'xsl-electric-equals)
(define-key xsl-mode-map "/"   	  'xsl-electric-slash) 
(define-key xsl-mode-map [(control c) (control x) (control p)] 'xsl-process)
(define-key xsl-mode-map [(control c) (control x) (control c)] 'xsl-auto-compile)

(defun xsl-insert-end-tag ()
  "Performs sgml-insert-end-tag with some extra electric reformatting"
  (interactive)
  (cond
   ((save-excursion
      (beginning-of-line) (looking-at "^\\s-*$")) nil)
   ((looking-at "\\s-*$") (newline-and-indent)))
  (sgml-insert-end-tag)
  (sgml-indent-line))


(defun xsl-electric-slash ()
  (interactive)
  (insert "/")
  (unless (memq (buffer-syntactic-context) '(string comment block-comment))
    (when (save-excursion (backward-char 2) (looking-at "</"))
      (delete-backward-char 2)
      (xsl-insert-end-tag))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

;; This is a redone syntax table, since I neglected to inherit it from
;; the parent.  I'll probably keep it this way, it's better than
;; psgml's, and portable no matter what mode you choose to derive
;; from.

;; Some punctuation that isn't
(modify-syntax-entry ?: "_" xsl-mode-syntax-table)
(modify-syntax-entry ?_ "_" xsl-mode-syntax-table)
(modify-syntax-entry ?. "_" xsl-mode-syntax-table)
(modify-syntax-entry ?# "_" xsl-mode-syntax-table)

;; string delimiters
(modify-syntax-entry ?\" "\"" xsl-mode-syntax-table)
(modify-syntax-entry ?\' "\"" xsl-mode-syntax-table)

;; comment
(modify-syntax-entry ?- "_ 1234" xsl-mode-syntax-table)

;; The occurrence indicators and connectors are punctuation to us.
(modify-syntax-entry ?| "." xsl-mode-syntax-table)
(modify-syntax-entry ?, "." xsl-mode-syntax-table)
(modify-syntax-entry ?& "." xsl-mode-syntax-table)
(modify-syntax-entry ?? "." xsl-mode-syntax-table)
(modify-syntax-entry ?+ "." xsl-mode-syntax-table)
(modify-syntax-entry ?* "." xsl-mode-syntax-table)
(modify-syntax-entry ?/ "." xsl-mode-syntax-table)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart abbrev expansion

(defvar xsl-priv-abbrev-hint (cons nil nil))
(make-variable-buffer-local 'xsl-priv-abbrev-hint)
 
(defsubst xsl-priv-get-abbrev-hint ()
  "Internal use -- user code that touches this will be shot."
  (cdr xsl-priv-abbrev-hint))


(defun xsl-electric-space ()
  (interactive)
  (expand-abbrev)

  (let ((expanded (abbrev-expansion last-abbrev-text local-abbrev-table))
	(hint (xsl-priv-get-abbrev-hint))
	(context (xsl-syntactic-context)))

    (if (memq context `(comment block-comment ,nil))
	(insert " ")
      
      ;; else
      (case hint
	(attribute (insert "=\""))
	(element (insert " ")
		  (xsl-display-attributes-help expanded))

	(otherwise (insert " ")
		   (when (memq context '(element-name element-attr))
		     (xsl-display-attributes-help
		      (xsl-get-current-element-name))))))))

(defun xsl-electric-equals ()
    (interactive)
    (expand-abbrev)
    (insert "=")
    (when (eq (xsl-priv-get-abbrev-hint) 'attribute) (insert "\"")))

(defun xsl-find-opening-bracket ()
  "places point at the opening brace of the current element, skipping string literals"
  (search-backward "<" nil t)
  (if (eq (buffer-syntactic-context) 'string)
      (xsl-find-opening-bracket)))

(defun xsl-find-closing-bracket ()
  "places point at the closing brace of the current element, skipping string literals"
  (search-forward ">" nil t)
  (if (eq (buffer-syntactic-context) 'string)
      (xsl-find-closing-bracket)))

(defun xsl-find-closing-bracket-backward ()
  "places point at the closing brace of the previous element, skipping string literals"
  (search-backward ">" nil t)
  (if (eq (buffer-syntactic-context) 'string)
      (xsl-find-closing-bracket-backward)))

(defun xsl-in-element-p ()
  "returns t if point is inside an element, opening or closing, nil otherwise"
  ;; searching backward should always find an opening element
  ;; after a closing one.  cdata sections don't count as elements
  ;; XXX FIXME: recognize cdata
  
  (let ((opoint (save-excursion (xsl-find-opening-bracket) (point)))
	(cpoint (save-excursion (xsl-find-closing-bracket-backward) (point))))

    (< cpoint opoint)))

(defvar xsl-mode-elements-abbrev-table nil
  "abbrevs for all element names in the xsl and fo namespaces")

(defun xsl-build-elements-abbrev-table ()
  (setq xsl-mode-elements-abbrev-table (make-abbrev-table))

  (mapcar #'(lambda (x)
	      (define-abbrev xsl-mode-elements-abbrev-table
		(nth 3 x)
		(concat xsl-xslt-ns-prefix
			(when xsl-xslt-ns-prefix ":")
			(car x))))
	  xsl-element-symbol-alist)
  
  (mapcar #'(lambda (x)
	      (define-abbrev xsl-mode-elements-abbrev-table
		(nth 3 x)
		(concat xsl-fo-ns-prefix
			(when xsl-fo-ns-prefix ":")
			(car x))))
	  xsl-fo-symbol-alist))

(defun xsl-priv-attributes-from-datum (datum)
"Pulls element attribute names out of an element from xsl-element-symbol-alist
normalizes from two data formats, either a flat list or an alist."
  (and datum 
       (let ((attribs (nth 2 datum)))
	 (cond ((eq datum nil) nil)
	       ((listp (car attribs)) (mapcar #'(lambda (x) (car x)) attribs))
	       (t attribs)))))

(defun xsl-syntactic-context (&optional supercede)
  "Returns context as a superset of buffer-syntactic-context
	'string        - in a string
	'comment       - in a comment
	'block-comment - in a block-comment
	'element-name  - in the name portion of an element
	'element-attr  - in the attributes portion of an element
	'cdata         - in a CDATA section (not implemented)
	nil            - undetermined (probably inside a container element)

PI's and meta elements should probably get their own context sometime,
since they're not required to act like normal elements."

  (or (buffer-syntactic-context)
      ;; XXX FIXME put cdata check right here
      (and (xsl-in-element-p)
	    (if (let* ((opoint (save-excursion
				 (xsl-find-opening-bracket) (point)))
		      (spoint (save-excursion
				(when (re-search-backward "\\s-" opoint t)
				    (point)))))
		  spoint)
		'element-attr
	      'element-name))))

(defvar xsl-contextual-abbrevs-cache
  (make-hash-table :test 'equal
		   :weakness 'key))

(defvar xsl-contextual-abbrevs-cache-size 50
  "*Causes this cache to keep roughly this many most recently used items")

(defvar xsl-contextual-abbrevs-cache-ring
  (make-ring xsl-contextual-abbrevs-cache-size)
  "MRU list for this cache")
  
(defun xsl-get-contextual-abbrev-table (key &optional nomake)
  "Returns the abbrev table corresponding to the context named by KEY.
If the optional arg NOMAKE is non-nil, check the cache (xsl-contextual-abbrevs-hashtable) only, do not attempt to build a table and cache it."
  (let ((entry (gethash key xsl-contextual-abbrevs-cache)))
    (unless (or entry nomake)
      (setq entry (xsl-make-contextual-abbrev-table key))
      (puthash key entry xsl-contextual-abbrevs-cache)
      (ring-insert xsl-contextual-abbrevs-cache-ring key))
    entry))

(defun xsl-make-contextual-abbrev-table (key)
  "make an abbrev table corresponding to syntactic context KEY.
Returns nil if no such key exists or there are no attributes defined for it.
At the moment, what this does is select the abbrevs out of xsl-attributes-alist that are members of the third element of the KEY's entry in xsl-element-symbol-alist and build an abbrev table only those elements." 

  (let ((attributes (xsl-priv-attributes-from-datum (assoc key xsl-element-symbol-alist)))
	(table (make-abbrev-table)))

	(mapcar #'(lambda (x) (xsl-add-attribute-abbrev table x)) attributes)
	(and attributes table)))

(defun xsl-add-attribute-abbrev (table abbrev)
  (define-abbrev table
    (or (nth 1 (assoc abbrev xsl-attributes-alist)) abbrev)
    abbrev))

(defun xsl-get-current-element-name ()
  "Returns the current element name (left of point), stripping xsl namespace"
  (save-excursion
    (re-search-backward "<\\(xsl:\\)?\\(.*?\\)\\s-" nil t)
    (match-string 2)))

(defun xsl-determine-abbrev-table ()
  "Determines the abbrev table to use based on context.
If entering an element name, it expands only element names.
In attributes, only valid attributes are expanded (not implemented yet)
"
  (case (xsl-syntactic-context)

    (element-name (setcar xsl-priv-abbrev-hint 'element)
		  xsl-mode-elements-abbrev-table)
    
    (element-attr (setcar xsl-priv-abbrev-hint 'attribute)
		  (xsl-get-contextual-abbrev-table
		   (xsl-get-current-element-name)))

    (otherwise nil)))

(defun xsl-pre-abbrev-expand-hook ()
  (setcdr xsl-priv-abbrev-hint nil)
  (setq local-abbrev-table (xsl-determine-abbrev-table)))

(defvar xsl-attributes-help-string-cache
  (make-hash-table :test 'equal
		   :weakness 'key))

(defvar xsl-attributes-help-string-cache-size 50
  "*Causes this cache to keep roughly this many most recently used items")

(defvar xsl-attributes-help-string-cache-ring
  (make-ring xsl-attributes-help-string-cache-size)
  "MRU list for this cache")
  
(defun xsl-attributes-help-string (elem &optional nomake)
  (let ((entry (gethash elem xsl-attributes-help-string-cache)))
    (unless (or entry nomake)
      (setq entry (xsl-make-attributes-help-string elem))
      (puthash elem entry xsl-attributes-help-string-cache)
      (ring-insert xsl-attributes-help-string-cache-ring elem))
    entry))

(defun xsl-make-attributes-help-string (elem)
  "Returns a short string of the available abbreviations for attributes of ELEM"
    (let ((table (xsl-get-contextual-abbrev-table elem))
	  (accum nil))
      (when (vectorp table)
	(mapatoms #'(lambda (x)
		      (setq accum
			    (cons (xsl-priv-format-one-abbrev x table) accum)))
		  table)
	(mapconcat #'identity accum " "))))

(defun xsl-priv-format-one-abbrev (asym table)
  "internal function.  NO TOUCHIE!"
  (let* ((name (symbol-name asym))
	(expansion (abbrev-expansion name table)))
    (format "%s=%s" name expansion)))

(defun xsl-display-attributes-help (name)
  "Display a short informative message about attributes allowed by the element named by SYMBOL"
  (save-match-data
    (let* ((canon (and (string-match "^\\(xsl:\\|fo:\\)?\\(.*\\)" name)
		       (match-string 2 name)))
	   (message (xsl-attributes-help-string canon)))
      (and message (display-message 'message message)))))

(defun xsl-priv-post-abbrev-expand-function (result)
  "Stores a hint for to act on whenever an abbrev was just expanded.
This hint is used by various electric characters in the mode to insert
the proper text, as well as by the auto-help system to prevent
unnecessary parsing for determining context information"
  (when result
    (sgml-indent-line)
    (setcdr xsl-priv-abbrev-hint (car xsl-priv-abbrev-hint))))

(defadvice expand-abbrev (around post-abbrev-advice activate)
  (let ((result ad-do-it))

  (run-hook-with-args 'post-abbrev-expand-hook result)
  (setq ad-return-value result)))

(make-local-hook 'post-abbrev-expand-hook)
(add-hook 'post-abbrev-expand-hook 'xsl-priv-post-abbrev-expand-function)


(provide 'xsl-mode)
;;; xsl-mode.el ends here

;;; unicode-fonts.el --- Configure Unicode fonts
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/unicode-fonts
;; URL: http://raw.github.com/rolandwalker/unicode-fonts/master/unicode-fonts.el
;; Version: 0.3.6
;; Last-Updated: 22 May 2013
;; EmacsWiki: UnicodeFonts
;; Keywords: i18n, faces, frames, wp, interface
;; Package-Requires: ((font-utils "0.6.8") (ucs-utils "0.7.2") (persistent-soft "0.8.6") (pcache "0.2.3"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart:
;;
;;     Configure an extended Latin font for your default face, such
;;     as Monaco, Consolas, or DejaVu Sans Mono.
;;
;;     Install these fonts
;;
;;        http://users.teilar.gr/~g1951d/Symbola706.zip
;;        http://www.quivira-font.com/files/Quivira.ttf
;;        http://sourceforge.net/projects/dejavu/files/dejavu/2.33/dejavu-fonts-ttf-2.33.tar.bz2
;;
;;     Remove Unifont from your system.
;;
;;     (require 'unicode-fonts)
;;
;;     (unicode-fonts-setup)
;;
;; Testing:
;;
;;     C-h h                                         ; M-x view-hello-file
;;     M-x list-charset-chars RET unicode-bmp RET    ; search for 210x
;;     M-x list-charset-chars RET unicode-smp RET    ; if your backend supports astral chars
;;     M-x unicode-fonts-debug-insert-block RET Mathematical_Operators RET
;;
;; Explanation:
;;
;; Emacs maintains font mappings on a per-glyph basis, meaning
;; that multiple fonts are used at the same time (transparently) to
;; display any character for which you have a font.  Furthermore,
;; Emacs does this out of the box.
;;
;; However, font mappings via fontsets are a bit difficult to
;; configure.  In addition, the default setup does not always pick
;; the most legible fonts.  As the manual warns, the choice of font
;; actually displayed for a non-ASCII character is "somewhat random".
;;
;; The Unicode standard provides a way to organize font mappings: it
;; divides character ranges into logical groups called "blocks".  This
;; library configures Emacs in a Unicode-friendly way by providing
;; mappings from
;;
;;     each Unicode block  ---to--->   a font with good coverage
;;
;; and makes the settings available via the customization interface.
;;
;; To use unicode-fonts, place the unicode-fonts.el file somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'unicode-fonts)
;;     (unicode-fonts-setup)
;;
;; See important notes about startup speed below.
;;
;; To gain any benefit from the library, you must have fonts with good
;; Unicode support installed on your system.  If you are running a
;; recent version of OS X or Microsoft Windows, you already own some
;; good multi-lingual fonts, though you would do very well to download
;; and install the four items below:
;;
;; From http://dejavu-fonts.org/wiki/Download
;;
;;     DejaVu Sans, DejaVu Sans Mono
;;
;; From http://www.quivira-font.com/downloads.php
;;
;;     Quivira
;;
;; From http://users.teilar.gr/~g1951d/
;;
;;     Symbola
;;
;; Many non-free fonts are referenced by the default settings.
;; However, free alternatives are also given wherever possible, and
;; patches are of course accepted to improve every case.
;;
;; On the assumption that an extended Latin font such as Monaco,
;; Consolas, or DejaVu Sans Mono is already being used for the default
;; face, no separate mappings are provided for the following Unicode
;; blocks:
;;
;;     Latin Extended Additional
;;     Latin Extended-A
;;     Latin-1 Supplement
;;     Spacing Modifier Letters
;;
;; It is also recommended to remove GNU Unifont from your system.
;; Unifont is very useful for debugging, but not useful for reading.
;;
;; The default options favor correctness and completeness over speed,
;; and can add many seconds to startup time in GUI mode.  Note that
;; when possible a font cache is kept between sessions, so try
;; starting Emacs a second time to see the true startup cost.  To
;; further increase startup speed, enter the customization interface
;; and
;;
;;     1. Remove fonts from `unicode-fonts-block-font-mapping'
;;        which are not present on your system.
;;
;;     2. Disable blocks in `unicode-fonts-block-font-mapping'
;;        which you are not interested in displaying.
;;
;; If you are using a language written in Chinese or Arabic script,
;; try customizing `unicode-fonts-skip-font-groups' to control which
;; script you see, and send a friendly bug report.
;;
;; See Also
;;
;;     M-x customize-group RET unicode-fonts RET
;;     M-x customize-variable RET unicode-fonts-block-font-mapping RET
;;
;; Notes
;;
;; Free fonts recognized by this package may be downloaded
;; from the following locations:
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=DoulosSIL_download
;;
;;         Doulos SIL                    ; Extended European and diacritics
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=Gentium_download
;;
;;         Gentium Plus                  ; Greek
;;
;;     From http://users.teilar.gr/~g1951d/
;;
;;         Aegean, Aegyptus, Akkadian    ; Ancient languages
;;         Analecta                      ; Ancient languages, Deseret
;;         Musica                        ; Musical Symbols
;;
;;     From http://www.wazu.jp/gallery/views/View_MPH2BDamase.html
;;
;;         MPH 2B Damase                 ; Arabic, Armenian, Buginese, Cherokee, Georgian,
;;                                       ; Glagolitic, Hanunoo, Kharoshthi, Limbu, Osmanya,
;;                                       ; Shavian, Syloti Nagri, Tai Le, Thaana
;;
;;     From http://wenq.org/enindex.cgi?Home
;;
;;         WenQuanYi Zen Hei             ; CJK (Simplified Chinese)
;;
;;     From http://babelstone.co.uk/Fonts/
;;
;;         BabelStone Han                ; CJK (Simplified Chinese)
;;         BabelStone Phags-pa Book      ; Phags-pa
;;
;;     From http://kldp.net/projects/unfonts/
;;
;;         Un Batang                     ; CJK (Hangul)
;;
;;     From http://sourceforge.jp/projects/hanazono-font/releases/
;;
;;         Hana Min A, Hana Min B        ; CJK (Japanese)
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=SILYi_home
;;
;;         Nuosu SIL                     ; CJK (Yi)
;;
;;     From http://www.daicing.com/manchu/index.php?page=fonts-downloads
;;
;;         Daicing Xiaokai               ; Mongolian
;;
;;     From http://www.library.gov.bt/IT/fonts.html
;;
;;         Jomolhari                     ; Tibetan
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=
;;
;;         Padauk                        ; Myanmar
;;
;;     From http://sarovar.org/projects/samyak/
;;
;;         Samyak                        ; Devanagari, Gujarati, Malayalam, Oriya, Tamil
;;
;;     From http://guca.sourceforge.net/typography/fonts/anmoluni/
;;
;;         AnmolUni                      ; Gurmukhi
;;
;;     From http://brahmi.sourceforge.net/downloads2.html
;;
;;         Kedage                        ; Kannada
;;
;;     From http://www.omicronlab.com/bangla-fonts.html
;;
;;         Mukti Narrow                  ; Bengali
;;
;;     From http://www.kamban.com.au/downloads.html
;;
;;         Akshar Unicode                ; Sinhala
;;
;;     From http://tabish.freeshell.org/eeyek/download.html
;;
;;         Eeyek Unicode                 ; Meetei Mayek
;;
;;     From http://scripts.sil.org/CMS/scripts/page.php?&item_id=Mondulkiri
;;
;;         Khmer Mondulkiri              ; Khmer
;;
;;     From http://www.laoscript.net/downloads/
;;
;;         Saysettha MX                  ; Lao
;;
;;     From http://www.geocities.jp/simsheart_alif/taithamunicode.html
;;
;;         Lanna Alif                    ; Tai Tham
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=DaiBannaSIL
;;
;;         Dai Banna SIL                 ; New Tai Lue
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?item_id=TaiHeritage
;;
;;         Tai Heritage Pro              ; Tai Viet
;;
;;     From http://sabilulungan.org/aksara/
;;
;;         Sundanese Unicode             ; Sundanese
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=ArabicFonts
;;
;;         Scheherazade                  ; Arabic
;;
;;     From http://www.farsiweb.ir/wiki/Persian_fonts
;;
;;         Koodak                        ; Arabic (Farsi)
;;
;;     From http://openfontlibrary.org/font/ahuramazda/
;;
;;         Ahuramzda                     ; Avestan
;;
;;     From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=AbyssinicaSIL
;;
;;         Abyssinica SIL                ; Ethiopic
;;
;;     From http://www.bethmardutho.org/index.php/resources/fonts.html
;;
;;         Estrangelo Nisibin            ; Syriac
;;
;;     From http://www.evertype.com/fonts/nko/
;;
;;         Conakry                       ; N'ko
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Requires font-utils.el, ucs-utils.el
;;
;; Bugs
;;
;;     The default choice of font for each code block balances coverage
;;     versus appearance.  This is necessarily subjective.
;;
;;     Checking for font availability is slow.  This library can
;;     add anywhere between 0.1 - 10 secs to startup time.  It is
;;     slowest under X11.  Some per-architecture limitations are
;;     documented in font-utils.el
;;
;;     Calling `set-fontset-font' can easily crash Emacs.  There is a
;;     workaround, but it may not be sufficient on all platforms.
;;     Tested on Cocoa Emacs, Native Mac Emacs, X11/XQuartz,
;;     MS Windows XP.
;;
;;     Glyph-by-glyph fallthrough happens differently depending on the
;;     font backend.  On Cocoa Emacs, glyph-by-glyph fallthrough does not
;;     occur, and manual per-glyph overrides are required to maximize
;;     coverage.  Fallthrough works on MS Windows, but not perfectly.
;;     X11/FreeType behaves most predictably.
;;
;;     The following ranges cannot be overridden within the
;;     "fontset-default" fontset:
;;
;;         Latin Extended Additional
;;         Latin Extended-B
;;         Spacing Modifier Letters
;;
;;     `unicode-fonts-overrides-mapping' shows some order-dependence,
;;     which must indicate a bug in this code.
;;
;;     A number of the entries in `unicode-fonts-overrides-mapping'
;;     are workarounds for the font Monaco, and therefore specific
;;     to OS X.
;;
;;     Widths of alternate fonts do not act as expected on MS Windows.
;;     For example, DejaVu Sans Mono box-drawing characters may use
;;     a different width than the default font.
;;
;; TODO
;;
;;     @@@@@@@ test again on windows with new font-utils
;;
;;     (set-language-environment "UTF-8") ?
;;
;;     add coverage comments to all mapping entries
;;
;;     Remove very old Microsoft entries (eg Monotype.com which was
;;     renamed Andale)
;;
;;     Recognize the default font and make smarter choices when it is
;;     one of the provided mappings.  (On Cocoa, the default font is
;;     returned when font-info fails, which is not a good thing
;;     overall.)
;;
;;     For every font, list font version and unicode blocks which are
;;     complete.
;;
;;     Note all decorative fonts
;;
;;     Get Windows 8 font listing
;;
;;     Adobe international fonts which are supplied with Reader
;;
;;     Apple fonts which could not be mapped
;;         Wawati TC
;;         Weibei TC
;;         Weibei SC
;;         Wawati SC
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;; No rights are claimed over data created by the Unicode
;; Consortium, which are included here under the terms of
;; the Unicode Terms of Use.
;;
;;; Code:
;;

;;; requirements

;; for callf, callf2, member*, incf, remove-if, remove-if-not
(require 'cl)

(autoload 'font-utils-exists-p                 "font-utils"  "Test whether FONT-NAME (a string or font object) exists.")
(autoload 'font-utils-read-name                "font-utils"  "Read a font name using `completing-read'.")
(autoload 'font-utils-lenient-name-equal       "font-utils"  "Leniently match two strings, FONT-NAME-A and FONT-NAME-B.")
(autoload 'font-utils-first-existing-font      "font-utils"  "Return the (normalized) first existing font name from FONT-NAMES.")
(autoload 'font-utils-name-from-xlfd           "font-utils"  "Return the font-family name from XLFD, a string.")
(autoload 'font-utils-is-qualified-variant     "font-utils"  "Test whether FONT-NAME-1 and FONT-NAME-2 are qualified variants of the same font.")

(autoload 'ucs-utils-char                      "ucs-utils"   "Return the character corresponding to NAME, a UCS name.")
(autoload 'ucs-utils-pretty-name               "ucs-utils"   "Return a prettified UCS name for CHAR.")

;;; constants

(defconst unicode-fonts-planes
  '(("unicode-bmp"         #x0000    #xFFFF)            ; plane  0
    ("unicode-smp"         #x10000   #x1FFFF)           ; plane  1
    ("unicode-sip"         #x20000   #x2FFFF)           ; plane  2
    ("unicode-unassigned"  #x30000   #xDFFFF)           ; planes 3-13
    ("unicode-ssp"         #xE0000   #xEFFFF)           ; plane  14
    ("unicode-pua-a"       #xF0000   #xFFFFF)           ; plane  15
    ("unicode-pua-b"       #x100000  #x10FFFF))         ; plane  16
  "Alist of Unicode 6.1 planes.")

(defconst unicode-fonts-blocks
  '(("Aegean Numbers"                                  #x10100  #x1013F)
    ("Alchemical Symbols"                              #x1F700  #x1F77F)
    ("Alphabetic Presentation Forms"                   #xFB00   #xFB4F)
    ("Ancient Greek Musical Notation"                  #x1D200  #x1D24F)
    ("Ancient Greek Numbers"                           #x10140  #x1018F)
    ("Ancient Symbols"                                 #x10190  #x101CF)
    ("Arabic Extended-A"                               #x08A0   #x08FF)
    ("Arabic Mathematical Alphabetic Symbols"          #x1EE00  #x1EEFF)
    ("Arabic Presentation Forms-A"                     #xFB50   #xFDFF)
    ("Arabic Presentation Forms-B"                     #xFE70   #xFEFF)
    ("Arabic Supplement"                               #x0750   #x077F)
    ("Arabic"                                          #x0600   #x06FF)
    ("Armenian"                                        #x0530   #x058F)
    ("Arrows"                                          #x2190   #x21FF)
    ("Avestan"                                         #x10B00  #x10B3F)
    ("Balinese"                                        #x1B00   #x1B7F)
    ("Bamum Supplement"                                #x16800  #x16A3F)
    ("Bamum"                                           #xA6A0   #xA6FF)
    ("Basic Latin"                                     #x0000   #x007F)
    ("Batak"                                           #x1BC0   #x1BFF)
    ("Bengali"                                         #x0980   #x09FF)
    ("Block Elements"                                  #x2580   #x259F)
    ("Bopomofo Extended"                               #x31A0   #x31BF)
    ("Bopomofo"                                        #x3100   #x312F)
    ("Box Drawing"                                     #x2500   #x257F)
    ("Brahmi"                                          #x11000  #x1107F)
    ("Braille Patterns"                                #x2800   #x28FF)
    ("Buginese"                                        #x1A00   #x1A1F)
    ("Buhid"                                           #x1740   #x175F)
    ("Byzantine Musical Symbols"                       #x1D000  #x1D0FF)
    ("CJK Compatibility Forms"                         #xFE30   #xFE4F)
    ("CJK Compatibility Ideographs Supplement"         #x2F800  #x2FA1F)
    ("CJK Compatibility Ideographs"                    #xF900   #xFAFF)
    ("CJK Compatibility"                               #x3300   #x33FF)
    ("CJK Radicals Supplement"                         #x2E80   #x2EFF)
    ("CJK Strokes"                                     #x31C0   #x31EF)
    ("CJK Symbols and Punctuation"                     #x3000   #x303F)
    ("CJK Unified Ideographs Extension A"              #x3400   #x4DBF)
    ("CJK Unified Ideographs Extension B"              #x20000  #x2A6DF)
    ("CJK Unified Ideographs Extension C"              #x2A700  #x2B73F)
    ("CJK Unified Ideographs Extension D"              #x2B740  #x2B81F)
    ("CJK Unified Ideographs"                          #x4E00   #x9FFF)
    ("Carian"                                          #x102A0  #x102DF)
    ("Chakma"                                          #x11100  #x1114F)
    ("Cham"                                            #xAA00   #xAA5F)
    ("Cherokee"                                        #x13A0   #x13FF)
    ("Combining Diacritical Marks Supplement"          #x1DC0   #x1DFF)
    ("Combining Diacritical Marks for Symbols"         #x20D0   #x20FF)
    ("Combining Diacritical Marks"                     #x0300   #x036F)
    ("Combining Half Marks"                            #xFE20   #xFE2F)
    ("Common Indic Number Forms"                       #xA830   #xA83F)
    ("Control Pictures"                                #x2400   #x243F)
    ("Coptic"                                          #x2C80   #x2CFF)
    ("Counting Rod Numerals"                           #x1D360  #x1D37F)
    ("Cuneiform Numbers and Punctuation"               #x12400  #x1247F)
    ("Cuneiform"                                       #x12000  #x123FF)
    ("Currency Symbols"                                #x20A0   #x20CF)
    ("Cypriot Syllabary"                               #x10800  #x1083F)
    ("Cyrillic Extended-A"                             #x2DE0   #x2DFF)
    ("Cyrillic Extended-B"                             #xA640   #xA69F)
    ("Cyrillic Supplement"                             #x0500   #x052F)
    ("Cyrillic"                                        #x0400   #x04FF)
    ("Deseret"                                         #x10400  #x1044F)
    ("Devanagari Extended"                             #xA8E0   #xA8FF)
    ("Devanagari"                                      #x0900   #x097F)
    ("Dingbats"                                        #x2700   #x27BF)
    ("Domino Tiles"                                    #x1F030  #x1F09F)
    ("Egyptian Hieroglyphs"                            #x13000  #x1342F)
    ("Emoticons"                                       #x1F600  #x1F64F)
    ("Enclosed Alphanumeric Supplement"                #x1F100  #x1F1FF)
    ("Enclosed Alphanumerics"                          #x2460   #x24FF)
    ("Enclosed CJK Letters and Months"                 #x3200   #x32FF)
    ("Enclosed Ideographic Supplement"                 #x1F200  #x1F2FF)
    ("Ethiopic Extended"                               #x2D80   #x2DDF)
    ("Ethiopic Extended-A"                             #xAB00   #xAB2F)
    ("Ethiopic Supplement"                             #x1380   #x139F)
    ("Ethiopic"                                        #x1200   #x137F)
    ("General Punctuation"                             #x2000   #x206F)
    ("Geometric Shapes"                                #x25A0   #x25FF)
    ("Georgian Supplement"                             #x2D00   #x2D2F)
    ("Georgian"                                        #x10A0   #x10FF)
    ("Glagolitic"                                      #x2C00   #x2C5F)
    ("Gothic"                                          #x10330  #x1034F)
    ("Greek Extended"                                  #x1F00   #x1FFF)
    ("Greek and Coptic"                                #x0370   #x03FF)
    ("Gujarati"                                        #x0A80   #x0AFF)
    ("Gurmukhi"                                        #x0A00   #x0A7F)
    ("Halfwidth and Fullwidth Forms"                   #xFF00   #xFFEF)
    ("Hangul Compatibility Jamo"                       #x3130   #x318F)
    ("Hangul Jamo Extended-A"                          #xA960   #xA97F)
    ("Hangul Jamo Extended-B"                          #xD7B0   #xD7FF)
    ("Hangul Jamo"                                     #x1100   #x11FF)
    ("Hangul Syllables"                                #xAC00   #xD7AF)
    ("Hanunoo"                                         #x1720   #x173F)
    ("Hebrew"                                          #x0590   #x05FF)
    ;; ("High Private Use Surrogates"                  #xDB80   #xDBFF) ; no displayable characters
    ;; ("High Surrogates"                              #xD800   #xDB7F) ; no displayable characters
    ("Hiragana"                                        #x3040   #x309F)
    ("IPA Extensions"                                  #x0250   #x02AF)
    ("Ideographic Description Characters"              #x2FF0   #x2FFF)
    ("Imperial Aramaic"                                #x10840  #x1085F)
    ("Inscriptional Pahlavi"                           #x10B60  #x10B7F)
    ("Inscriptional Parthian"                          #x10B40  #x10B5F)
    ("Javanese"                                        #xA980   #xA9DF)
    ("Kaithi"                                          #x11080  #x110CF)
    ("Kana Supplement"                                 #x1B000  #x1B0FF)
    ("Kanbun"                                          #x3190   #x319F)
    ("Kangxi Radicals"                                 #x2F00   #x2FDF)
    ("Kannada"                                         #x0C80   #x0CFF)
    ("Katakana Phonetic Extensions"                    #x31F0   #x31FF)
    ("Katakana"                                        #x30A0   #x30FF)
    ("Kayah Li"                                        #xA900   #xA92F)
    ("Kharoshthi"                                      #x10A00  #x10A5F)
    ("Khmer Symbols"                                   #x19E0   #x19FF)
    ("Khmer"                                           #x1780   #x17FF)
    ("Lao"                                             #x0E80   #x0EFF)
    ("Latin Extended Additional"                       #x1E00   #x1EFF)
    ("Latin Extended-A"                                #x0100   #x017F)
    ("Latin Extended-B"                                #x0180   #x024F)
    ("Latin Extended-C"                                #x2C60   #x2C7F)
    ("Latin Extended-D"                                #xA720   #xA7FF)
    ("Latin-1 Supplement"                              #x0080   #x00FF)
    ("Lepcha"                                          #x1C00   #x1C4F)
    ("Letterlike Symbols"                              #x2100   #x214F)
    ("Limbu"                                           #x1900   #x194F)
    ("Linear B Ideograms"                              #x10080  #x100FF)
    ("Linear B Syllabary"                              #x10000  #x1007F)
    ("Lisu"                                            #xA4D0   #xA4FF)
    ;; ("Low Surrogates"                               #xDC00   #xDFFF) ; no displayable characters
    ("Lycian"                                          #x10280  #x1029F)
    ("Lydian"                                          #x10920  #x1093F)
    ("Mahjong Tiles"                                   #x1F000  #x1F02F)
    ("Malayalam"                                       #x0D00   #x0D7F)
    ("Mandaic"                                         #x0840   #x085F)
    ("Mathematical Alphanumeric Symbols"               #x1D400  #x1D7FF)
    ("Mathematical Operators"                          #x2200   #x22FF)
    ("Meetei Mayek Extensions"                         #xAAE0   #xAAFF)
    ("Meetei Mayek"                                    #xABC0   #xABFF)
    ("Meroitic Cursive"                                #x109A0  #x109FF)
    ("Meroitic Hieroglyphs"                            #x10980  #x1099F)
    ("Miao"                                            #x16F00  #x16F9F)
    ("Miscellaneous Mathematical Symbols-A"            #x27C0   #x27EF)
    ("Miscellaneous Mathematical Symbols-B"            #x2980   #x29FF)
    ("Miscellaneous Symbols and Pictographs"           #x1F300  #x1F5FF)
    ("Miscellaneous Symbols and Arrows"                #x2B00   #x2BFF)
    ("Miscellaneous Symbols"                           #x2600   #x26FF)
    ("Miscellaneous Technical"                         #x2300   #x23FF)
    ("Modifier Tone Letters"                           #xA700   #xA71F)
    ("Mongolian"                                       #x1800   #x18AF)
    ("Musical Symbols"                                 #x1D100  #x1D1FF)
    ("Myanmar Extended-A"                              #xAA60   #xAA7F)
    ("Myanmar"                                         #x1000   #x109F)
    ("NKo"                                             #x07C0   #x07FF)
    ("New Tai Lue"                                     #x1980   #x19DF)
    ("Number Forms"                                    #x2150   #x218F)
    ("Ogham"                                           #x1680   #x169F)
    ("Ol Chiki"                                        #x1C50   #x1C7F)
    ("Old Italic"                                      #x10300  #x1032F)
    ("Old Persian"                                     #x103A0  #x103DF)
    ("Old South Arabian"                               #x10A60  #x10A7F)
    ("Old Turkic"                                      #x10C00  #x10C4F)
    ("Optical Character Recognition"                   #x2440   #x245F)
    ("Oriya"                                           #x0B00   #x0B7F)
    ("Osmanya"                                         #x10480  #x104AF)
    ("Phags-pa"                                        #xA840   #xA87F)
    ("Phaistos Disc"                                   #x101D0  #x101FF)
    ("Phoenician"                                      #x10900  #x1091F)
    ("Phonetic Extensions Supplement"                  #x1D80   #x1DBF)
    ("Phonetic Extensions"                             #x1D00   #x1D7F)
    ("Playing Cards"                                   #x1F0A0  #x1F0FF)
    ("Private Use Area"                                #xE000   #xF8FF)
    ("Rejang"                                          #xA930   #xA95F)
    ("Rumi Numeral Symbols"                            #x10E60  #x10E7F)
    ("Runic"                                           #x16A0   #x16FF)
    ("Samaritan"                                       #x0800   #x083F)
    ("Saurashtra"                                      #xA880   #xA8DF)
    ("Sharada"                                         #x11180  #x111DF)
    ("Shavian"                                         #x10450  #x1047F)
    ("Sinhala"                                         #x0D80   #x0DFF)
    ("Small Form Variants"                             #xFE50   #xFE6F)
    ("Sora Sompeng"                                    #x110D0  #x110FF)
    ("Spacing Modifier Letters"                        #x02B0   #x02FF)
    ("Specials"                                        #xFFF0   #xFFFF)
    ("Sundanese Supplement"                            #x1CC0   #x1CCF)
    ("Sundanese"                                       #x1B80   #x1BBF)
    ("Superscripts and Subscripts"                     #x2070   #x209F)
    ("Supplemental Arrows-A"                           #x27F0   #x27FF)
    ("Supplemental Arrows-B"                           #x2900   #x297F)
    ("Supplemental Mathematical Operators"             #x2A00   #x2AFF)
    ("Supplemental Punctuation"                        #x2E00   #x2E7F)
    ("Supplementary Private Use Area-A"                #xF0000  #xFFFFF)
    ("Supplementary Private Use Area-B"                #x100000 #x10FFFF)
    ("Syloti Nagri"                                    #xA800   #xA82F)
    ("Syriac"                                          #x0700   #x074F)
    ("Tagalog"                                         #x1700   #x171F)
    ("Tagbanwa"                                        #x1760   #x177F)
    ("Tags"                                            #xE0000  #xE007F)
    ("Tai Le"                                          #x1950   #x197F)
    ("Tai Tham"                                        #x1A20   #x1AAF)
    ("Tai Viet"                                        #xAA80   #xAADF)
    ("Tai Xuan Jing Symbols"                           #x1D300  #x1D35F)
    ("Takri"                                           #x11680  #x116CF)
    ("Tamil"                                           #x0B80   #x0BFF)
    ("Telugu"                                          #x0C00   #x0C7F)
    ("Thaana"                                          #x0780   #x07BF)
    ("Thai"                                            #x0E00   #x0E7F)
    ("Tibetan"                                         #x0F00   #x0FFF)
    ("Tifinagh"                                        #x2D30   #x2D7F)
    ("Transport and Map Symbols"                       #x1F680  #x1F6FF)
    ("Ugaritic"                                        #x10380  #x1039F)
    ("Unified Canadian Aboriginal Syllabics Extended"  #x18B0   #x18FF)
    ("Unified Canadian Aboriginal Syllabics"           #x1400   #x167F)
    ("Vai"                                             #xA500   #xA63F)
    ("Variation Selectors Supplement"                  #xE0100  #xE01EF)
    ("Variation Selectors"                             #xFE00   #xFE0F)
    ("Vedic Extensions"                                #x1CD0   #x1CFF)
    ("Vertical Forms"                                  #xFE10   #xFE1F)
    ("Yi Radicals"                                     #xA490   #xA4CF)
    ("Yi Syllables"                                    #xA000   #xA48F)
    ("Yijing Hexagram Symbols"                         #x4DC0   #x4DFF))
  "Alist of Unicode 6.1 blocks.")

(defvar unicode-fonts-known-font-characteristics
  '(("Abadi MT Condensed"             :licenses (microsoft))
    ("Abyssinica SIL"                 :licenses (free))
    ("Adobe Arabic"                   :licenses (adobe) :arabic 'standard)
    ("Adobe Hebrew"                   :licenses (adobe))
    ("Adobe Minion Web"               :licenses (microsoft))
    ("Aegean"                         :licenses (free))
    ("Aegyptus"                       :licenses (free))
    ("Agency FB"                      :licenses (microsoft))
    ("Aharoni"                        :licenses (microsoft))
    ("Ahuramzda"                      :licenses (free))
    ("Akaash"                         :licenses (free))
    ("Akkadian"                       :licenses (free))
    ("Aksara Bali"                    :licenses (free))
    ("Akshar Unicode"                 :licenses (free))
    ("Al Bayan"                       :licenses (apple) :arabic 'standard)
    ("Aleem Urdu Unicode"             :licenses (free) :arabic 'urdu)
    ("Algerian"                       :licenses (microsoft))
    ("Almanac MT"                     :licenses (microsoft))
    ("American Typewriter"            :licenses (apple))
    ("American Uncial"                :licenses (microsoft))
    ("Analecta"                       :licenses (free))
    ("Andale Mono"                    :spacing 'mono :licenses (apple microsoft))
    ("Andalus"                        :licenses (microsoft))
    ("Andy"                           :licenses (microsoft))
    ("Angsana New"                    :licenses (microsoft))
    ("AngsanaUPC"                     :licenses (microsoft))
    ("AnmolUni"                       :licenses (free))
    ("Aparajita"                      :licenses (microsoft))
    ("Apple Braille"                  :licenses (apple))
    ("Apple Casual"                   :licenses (apple))
    ("Apple Chancery"                 :licenses (apple))
    ("Apple Color Emoji"              :licenses (apple))
    ("Apple Gothic"                   :licenses (apple))
    ("Apple LiGothic"                 :licenses (apple) :chinese 'traditional)
    ("Apple LiSung"                   :licenses (apple) :chinese 'traditional)
    ("Apple Myungjo"                  :licenses (apple) :glyph-quality 'low)
    ("Apple SD Gothic Neo"            :licenses (apple))
    ("Apple Symbols"                  :licenses (apple))
    ("Arabic Transparent"             :licenses (microsoft) :arabic 'standard)
    ("Arabic Typesetting"             :licenses (microsoft) :arabic 'standard)
    ("Arial Black"                    :licenses (apple microsoft))
    ("Arial Hebrew"                   :licenses (apple))
    ("Arial Narrow Special"           :licenses (microsoft))
    ("Arial Narrow"                   :licenses (apple microsoft))
    ("Arial Rounded MT Bold"          :licenses (apple microsoft))
    ("Arial Special"                  :licenses (microsoft))
    ("Arial Unicode MS"               :licenses (apple microsoft) :arabic 'standard)
    ("Arial"                          :licenses (apple microsoft))
    ("Augsburger Initials"            :licenses (microsoft))
    ("Avenir Next Condensed"          :licenses (apple))
    ("Avenir Next"                    :licenses (apple))
    ("Avenir"                         :licenses (apple))
    ("Ayuthaya"                       :licenses (apple))
    ("BabelStone Han"                 :chinese 'simplified :licenses (free))
    ("BabelStone Phags-pa Book"       :licenses (free))
    ("Baghdad"                        :licenses (apple) :arabic 'standard)
    ("Bangla MN"                      :licenses (apple))
    ("Bangla Sangam MN"               :licenses (apple))
    ("Baoli SC"                       :licenses (apple) :chinese 'simplified)
    ("Baskerville Old Face"           :licenses (microsoft))
    ("Baskerville"                    :licenses (apple))
    ("Batak-Unicode"                  :licenses (free))
    ("Batang"                         :licenses (microsoft))
    ("Bauhaus 93"                     :licenses (microsoft))
    ("Beesknees ITC"                  :licenses (microsoft))
    ("Bell MT"                        :licenses (microsoft))
    ("Berlin Sans FB"                 :licenses (microsoft))
    ("Bernard MT Condensed"           :licenses (microsoft))
    ("Biau Kai"                       :licenses (apple) :chinese 'traditional)
    ("Bickley Script"                 :licenses (microsoft))
    ("Big Caslon"                     :licenses (apple))
    ("Blackadder ITC"                 :licenses (microsoft))
    ("Bodoni MT Condensed"            :licenses (microsoft))
    ("Bodoni MT"                      :licenses (microsoft))
    ("Bon Apetit MT"                  :licenses (microsoft))
    ("Book Antiqua"                   :licenses (microsoft))
    ("Bookman Old Style"              :licenses (microsoft))
    ("Bookshelf Symbol 7"             :licenses (microsoft))
    ("Bradley Hand ITC"               :licenses (microsoft))
    ("Braggadocio"                    :licenses (microsoft))
    ("Brahmi"                         :licenses (free))
    ("BriemScript"                    :licenses (microsoft))
    ("Britannic Bold"                 :licenses (microsoft))
    ("Broadway"                       :licenses (microsoft))
    ("Browallia New"                  :licenses (microsoft))
    ("BrowalliaUPC"                   :licenses (microsoft))
    ("Brush Script MT"                :licenses (microsoft))
    ("Brush Script Std"               :licenses (apple))
    ("Calibri"                        :licenses (microsoft) :cleartype 't)
    ("Californian FB"                 :licenses (microsoft))
    ("Calisto MT"                     :licenses (microsoft))
    ("Cambria Math"                   :licenses (microsoft) :cleartype 't :buggy-before-vista t)
    ("Cambria"                        :licenses (microsoft))
    ("Candara"                        :licenses (microsoft))
    ("Cariadings"                     :licenses (microsoft))
    ("Castellar"                      :licenses (microsoft))
    ("Centaur"                        :licenses (microsoft))
    ("Century Gothic"                 :licenses (microsoft))
    ("Century Schoolbook"             :licenses (microsoft))
    ("Century"                        :licenses (microsoft))
    ("Chalkboard SE"                  :licenses (apple))
    ("Chalkboard"                     :licenses (apple))
    ("Chalkduster"                    :licenses (apple))
    ("Charcoal CY"                    :licenses (apple))
    ("Chiller"                        :licenses (microsoft))
    ("Cochin"                         :licenses (apple))
    ("Code2000"                       :licenses (unclear))
    ("Code2001"                       :licenses (free))
    ("Code2002"                       :licenses (free))
    ("Colonna MT"                     :licenses (microsoft))
    ("Comic Sans MS"                  :licenses (apple microsoft))
    ("Conakry"                        :licenses (free))
    ("Consolas"                       :spacing 'mono :licenses (microsoft) :cleartype 't)
    ("Constantia"                     :licenses (microsoft))
    ("Contemporary Brush"             :licenses (microsoft))
    ("Cooper Black"                   :licenses (microsoft))
    ("Cooper Std"                     :licenses (apple))
    ("Copperplate Gothic"             :licenses (microsoft))
    ("Copperplate"                    :licenses (apple))
    ("Corbel"                         :licenses (microsoft))
    ("Cordia New"                     :licenses (microsoft))
    ("CordiaUPC"                      :licenses (microsoft))
    ("Corsiva Hebrew"                 :licenses (apple))
    ("Courier New"                    :spacing 'mono :licenses (apple microsoft))
    ("Courier"                        :licenses (apple))
    ("Curlz MT"                       :licenses (microsoft))
    ("DFKai-SB"                       :chinese 'traditional :licenses (microsoft))
    ("Dai Banna SIL Book"             :licenses (free))
    ("Dai Banna SIL Light"            :licenses (free))
    ("Daicing Xiaokai"                :licenses (free))
    ("Damascus"                       :licenses (apple))
    ("DaunPenh"                       :licenses (microsoft))
    ("David"                          :licenses (microsoft))
    ("DecoType Naskh"                 :licenses (apple) :arabic 'naskh)
    ("DejaVu Sans Mono"               :spacing 'mono :licenses (free) :arabic 'standard)
    ("DejaVu Sans"                    :licenses (free) :arabic 'standard)
    ("DejaVu Serif"                   :licenses (free) :arabic 'standard)
    ("Desdemona"                      :licenses (microsoft))
    ("Devanagari MT"                  :licenses (apple))
    ("Devanagari Sangam MN"           :licenses (apple))
    ("Didot"                          :licenses (apple))
    ("DilleniaUPC"                    :licenses (microsoft))
    ("Directions MT"                  :licenses (microsoft))
    ("Diwani Letter"                  :licenses (microsoft) :decorative t :arabic 'standard)
    ("DokChampa"                      :licenses (microsoft))
    ("Dotum"                          :licenses (microsoft))
    ("Doulos SIL"                     :licenses (free))
    ("Ebrima"                         :licenses (microsoft))
    ("Eckmann"                        :licenses (microsoft))
    ("Edda"                           :licenses (microsoft))
    ("Edwardian Script ITC"           :licenses (microsoft))
    ("Eeyek Unicode"                  :licenses (free))
    ("Elephant"                       :licenses (microsoft))
    ("Elham"                          :licenses (free) :arabic 'farsi)
    ("Engravers MT"                   :licenses (microsoft))
    ("Engravers"                      :licenses (microsoft))
    ("Enviro"                         :licenses (microsoft))
    ("Eras ITC"                       :licenses (microsoft))
    ("Estrangelo Edessa"              :licenses (microsoft free))
    ("Estrangelo Nisibin"             :licenses (free))
    ("Ethiopia Jiret"                 :licenses (free))
    ("Ethiopia WashRa SemiBold"       :licenses (free))
    ("Ethopic Yebse"                  :licenses (free))
    ("EucrosiaUPC"                    :licenses (microsoft))
    ("Euphemia UCAS"                  :licenses (apple))
    ("Euphemia"                       :licenses (microsoft))
    ("Eurostile"                      :licenses (microsoft))
    ("FangSong"                       :spacing 'mono :chinese 'simplified :licenses (microsoft))
    ("Farsi Simple Bold"              :licenses (microsoft) :arabic 'farsi)
    ("Felix Titling"                  :licenses (microsoft))
    ("Fine Hand"                      :licenses (microsoft))
    ("Fixed Miriam Transparent"       :licenses (microsoft))
    ("Flexure"                        :licenses (microsoft))
    ("Footlight MT Light"             :licenses (microsoft))
    ("Footlight MT"                   :licenses (microsoft))
    ("Forte"                          :licenses (microsoft))
    ("FrankRuehl"                     :licenses (microsoft))
    ("Franklin Gothic"                :licenses (microsoft))
    ("FreesiaUPC"                     :licenses (microsoft))
    ("Freestyle Script"               :licenses (microsoft))
    ("French Script MT"               :licenses (microsoft))
    ("Futura"                         :licenses (microsoft apple))
    ("GB18030 Bitmap"                 :licenses (apple) :chinese 'simplified :glyph-quality 'low)
    ("Gabriola"                       :licenses (microsoft))
    ("Garamond MT"                    :licenses (microsoft))
    ("Garamond"                       :licenses (microsoft))
    ("Gautami"                        :licenses (microsoft))
    ("Geeza Pro"                      :licenses (apple) :arabic 'standard)
    ("Geneva CY"                      :licenses (apple))
    ("Geneva"                         :licenses (apple))
    ("Gentium Plus Compact"           :licenses (free))
    ("Gentium Plus"                   :licenses (free))
    ("Georgia Ref"                    :licenses (microsoft))
    ("Georgia"                        :licenses (apple microsoft))
    ("Gigi"                           :licenses (microsoft))
    ("Gill Sans MT Condensed"         :licenses (microsoft))
    ("Gill Sans MT"                   :licenses (microsoft))
    ("Gill Sans"                      :licenses (apple))
    ("Gisha"                          :licenses (microsoft))
    ("Gloucester MT Extra Condensed"  :licenses (microsoft))
    ("Gloucester"                     :licenses (microsoft))
    ("Goudy Old Style"                :licenses (microsoft))
    ("Goudy Stout"                    :licenses (microsoft))
    ("Gradl"                          :licenses (microsoft))
    ("Gujarati MT"                    :licenses (apple))
    ("Gujarati Sangam MN"             :licenses (apple))
    ("Gulim"                          :licenses (microsoft))
    ("GungSeo"                        :licenses (apple))
    ("Gungsuh"                        :licenses (microsoft))
    ("Gurmukhi MN"                    :licenses (apple))
    ("Gurmukhi Sangam MN"             :licenses (apple))
    ("Hacen Sudan"                    :licenses (free))
    ("Haettenschweiler"               :licenses (microsoft))
    ("HanaMinA"                       :licenses (free) :chinese 'traditional)
    ("HanaMinB"                       :licenses (free))
    ("Harlow Solid"                   :licenses (microsoft))
    ("Harrington"                     :licenses (microsoft))
    ("HeadLineA"                      :licenses (apple) 'decorative t)
    ("Hei"                            :licenses (apple) :chinese 'simplified)
    ("Heiti SC"                       :licenses (apple) :chinese 'simplified)
    ("Heiti TC"                       :licenses (apple) :chinese 'traditional)
    ("Helvetica CY"                   :licenses (apple))
    ("Helvetica Neue"                 :licenses (apple))
    ("Helvetica"                      :licenses (apple))
    ("Herculanum"                     :licenses (apple))
    ("High Tower Text"                :licenses (microsoft))
    ("Hiragino Kaku Gothic Pro"       :licenses (apple))
    ("Hiragino Kaku Gothic ProN"      :licenses (apple))
    ("Hiragino Kaku Gothic Std"       :licenses (apple))
    ("Hiragino Kaku Gothic StdN"      :licenses (apple))
    ("Hiragino Maru Gothic Pro"       :licenses (apple))
    ("Hiragino Maru Gothic ProN"      :licenses (apple))
    ("Hiragino Mincho Pro"            :licenses (apple))
    ("Hiragino Mincho ProN"           :licenses (apple))
    ("Hiragino Sans GB"               :licenses (apple))
    ("Hoefler Text"                   :licenses (apple))
    ("Holidays MT"                    :licenses (microsoft))
    ("Homa"                           :licenses (free) :arabic 'farsi)
    ("Impact"                         :licenses (apple microsoft))
    ("Imprint MT Shadow"              :licenses (microsoft))
    ("InaiMathi"                      :licenses (apple))
    ("Informal Roman"                 :licenses (microsoft))
    ("IrisUPC"                        :licenses (microsoft))
    ("Iskoola Pota"                   :licenses (microsoft))
    ("Issa & Giliana Classic"         :licenses (free))
    ("JasmineUPC"                     :licenses (microsoft))
    ("Jokerman"                       :licenses (microsoft))
    ("Jomolhari"                      :licenses (free))
    ("Juice ITC"                      :licenses (microsoft))
    ("Kai"                            :licenses (apple) :chinese 'simplified)
    ("KaiTi"                          :licenses (microsoft))
    ("Kailasa"                        :licenses (apple))
    ("Kaiti SC"                       :licenses (apple) :chinese 'simplified)
    ("Kalinga"                        :licenses (microsoft))
    ("Kannada MN"                     :licenses (apple))
    ("Kannada Sangam MN"              :licenses (apple))
    ("Kartika"                        :licenses (microsoft))
    ("Kedage"                         :licenses (free))
    ("Kefa"                           :licenses (apple))
    ("Keyboard"                       :licenses (apple))
    ("Keystrokes MT"                  :licenses (microsoft))
    ("Khmer Busra"                    :licenses (free))
    ("Khmer MN"                       :licenses (apple))
    ("Khmer Mondulkiri"               :licenses (free))
    ("Khmer Sangam MN"                :licenses (apple))
    ("Khmer UI"                       :licenses (microsoft))
    ("Kino MT"                        :licenses (microsoft))
    ("KodchiangUPC"                   :licenses (microsoft))
    ("Kokila"                         :licenses (microsoft))
    ("Kokonor"                        :licenses (apple))
    ("Koodak"                         :licenses (free) :arabic 'farsi)
    ("Kristen ITC"                    :licenses (microsoft))
    ("Krungthep"                      :licenses (apple))
    ("Kufi Extended Outline"          :licenses (microsoft) :decorative t :arabic 'standard)
    ("Kufi Outline Shaded"            :licenses (microsoft) :decorative t :arabic 'standard)
    ("Kufi Standard GK"               :licenses (apple) :arabic 'standard)
    ("Kunstler Script"                :licenses (microsoft))
    ("LCD"                            :licenses (microsoft))
    ("Lanna Alif"                     :licenses (free))
    ("Lantinghei SC"                  :licenses (apple) :chinese 'simplified)
    ("Lantinghei TC"                  :spacing 'mono :licenses (apple) :chinese 'traditional)
    ("Lao MN"                         :licenses (apple))
    ("Lao Sangam MN"                  :licenses (apple))
    ("Lao UI"                         :licenses (microsoft))
    ("Latha"                          :licenses (microsoft))
    ("Leelawadee"                     :licenses (microsoft))
    ("Levenim MT"                     :licenses (microsoft))
    ("LiHei Pro"                      :licenses (apple) :chinese 'traditional)
    ("LiSong Pro"                     :licenses (apple) :chinese 'traditional)
    ("Libian SC"                      :licenses (apple) :chinese 'simplified)
    ("LilyUPC"                        :licenses (microsoft))
    ("Lucida Blackletter"             :licenses (microsoft))
    ("Lucida Bright Math"             :licenses (microsoft))
    ("Lucida Bright"                  :licenses (microsoft))
    ("Lucida Calligraphy"             :licenses (microsoft) :decorative t)
    ("Lucida Console"                 :spacing 'mono :licenses (microsoft))
    ("Lucida Fax"                     :licenses (microsoft))
    ("Lucida Grande"                  :licenses (apple))
    ("Lucida Handwriting"             :licenses (microsoft) :decorative t)
    ("Lucida Sans Typewriter"         :licenses (microsoft))
    ("Lucida Sans Unicode"            :licenses (microsoft))
    ("Lucida Sans"                    :licenses (microsoft))
    ("MPH 2B Damase"                  :licenses (free) :arabic 'standard)
    ("MS Gothic"                      :spacing 'mono :licenses (microsoft))
    ("MS LineDraw"                    :licenses (microsoft))
    ("MS Mincho"                      :spacing 'mono :licenses (microsoft))
    ("MS Outlook"                     :licenses (microsoft))
    ("MS PGothic"                     :licenses (microsoft))
    ("MS PMincho"                     :licenses (microsoft))
    ("MS Reference Sans Serif"        :licenses (microsoft))
    ("MS Reference Specialty"         :licenses (microsoft))
    ("MS Reference"                   :licenses (microsoft))
    ("MS UI Gothic"                   :licenses (microsoft))
    ("MT Extra"                       :licenses (microsoft))
    ("MV Boli"                        :licenses (microsoft))
    ("Maduram"                        :licenses (free))
    ("Magneto"                        :licenses (microsoft))
    ("Maiandra GD"                    :licenses (microsoft))
    ("Malayalam MN"                   :licenses (apple))
    ("Malayalam Sangam MN"            :licenses (apple))
    ("Malgun Gothic"                  :licenses (microsoft))
    ("Mangal"                         :licenses (microsoft))
    ("Map Symbols"                    :licenses (microsoft))
    ("Marion"                         :licenses (apple))
    ("Marker Felt"                    :licenses (apple) :decorative t)
    ("Marlett"                        :licenses (microsoft))
    ("Matisse ITC"                    :licenses (microsoft))
    ("Matura MT Script Capitals"      :licenses (microsoft))
    ("McZee"                          :licenses (microsoft))
    ("Mead"                           :licenses (microsoft))
    ("Meetei Mayek"                   :licenses (free))
    ("Meiryo UI"                      :licenses (microsoft))
    ("Meiryo"                         :licenses (microsoft))
    ("Menlo"                          :licenses (apple))
    ("Mercurius Script MT"            :licenses (microsoft))
    ("Microsoft Himalaya"             :licenses (microsoft))
    ("Microsoft JhengHei"             :chinese 'traditional :licenses (microsoft))
    ("Microsoft New Tai Lue"          :licenses (microsoft))
    ("Microsoft PhagsPa"              :licenses (microsoft))
    ("Microsoft Sans Serif"           :licenses (apple microsoft) :arabic 'standard)
    ("Microsoft Tai Le"               :licenses (microsoft))
    ("Microsoft Uighur"               :licenses (microsoft) :arabic 'uighur)
    ("Microsoft YaHei"                :chinese 'simplified :licenses (microsoft))
    ("Microsoft Yi Baiti"             :licenses (microsoft))
    ("MingLiU"                        :spacing 'mono :chinese 'traditional :licenses (microsoft))
    ("Minion Web"                     :licenses (microsoft))
    ("Miriam Fixed"                   :spacing 'mono :licenses (microsoft))
    ("Miriam"                         :licenses (microsoft))
    ("Mistral"                        :licenses (microsoft))
    ("Modern No. 20"                  :licenses (microsoft))
    ("Monaco"                         :spacing 'mono :licenses (apple))
    ("Mongolian Baiti"                :licenses (microsoft))
    ("Monlam Uni Sans Serif"          :licenses (free))
    ("Monotype Corsiva"               :licenses (microsoft))
    ("Monotype Sorts"                 :licenses (microsoft))
    ("MoolBoran"                      :licenses (microsoft))
    ("Mshtakan"                       :licenses (apple))
    ("Mukti Narrow"                   :licenses (free))
    ("Musica"                         :licenses (free))
    ("Myanmar MN"                     :licenses (apple))
    ("Myanmar Sangam MN"              :licenses (apple))
    ("NSimSun"                        :licenses (microsoft))
    ("Nadeem"                         :licenses (apple) :arabic 'standard)
    ("Nanum Brush Script"             :licenses (apple))
    ("Nanum Gothic"                   :licenses (apple))
    ("Nanum Myeongjo"                 :licenses (apple))
    ("Nanum Pen Script"               :licenses (apple))
    ("Narkisim"                       :licenses (microsoft))
    ("Nazli"                          :licenses (free) :arabic 'farsi)
    ("New Caledonia"                  :licenses (microsoft))
    ("New Peninim MT"                 :licenses (apple))
    ("News Gothic MT"                 :licenses (microsoft))
    ("Niagara Engraved"               :licenses (microsoft))
    ("Niagara Solid"                  :licenses (microsoft))
    ("Niagara"                        :licenses (microsoft))
    ("Noteworthy"                     :licenses (apple))
    ("Nuosu SIL"                      :licenses (free))
    ("Nyala"                          :licenses (microsoft))
    ("OCR A Extended"                 :licenses (microsoft))
    ("OCR-B-Digits"                   :licenses (microsoft))
    ("OCRB"                           :licenses (microsoft))
    ("Old Antic Bold"                 :licenses (microsoft) :decorative t :arabic 'standard)
    ("Old English Text MT"            :licenses (microsoft))
    ("Onyx"                           :licenses (microsoft))
    ("Optima"                         :licenses (apple))
    ("Oriya MN"                       :licenses (apple))
    ("Oriya Sangam MN"                :licenses (apple))
    ("Osaka"                          :spacing 'mono :licenses (apple))
    ("PC Myungjo"                     :spacing 'mono :licenses (apple))
    ("PMingLiU"                       :licenses (microsoft))
    ("PT Sans"                        :licenses (apple))
    ("Padauk"                         :licenses (free))
    ("Palace Script MT"               :licenses (microsoft))
    ("Palatino Linotype"              :licenses (microsoft))
    ("Palatino"                       :licenses (apple))
    ("Papyrus"                        :licenses (apple microsoft))
    ("Parade"                         :licenses (microsoft))
    ("Parchment"                      :licenses (microsoft))
    ("Parties MT"                     :licenses (microsoft))
    ("Peignot Medium"                 :licenses (microsoft))
    ("Pepita MT"                      :licenses (microsoft))
    ("Perpetua Titling MT"            :licenses (microsoft))
    ("Perpetua"                       :licenses (microsoft))
    ("PilGi"                          :licenses (apple) :glyph-quality low)
    ("Placard Condensed"              :licenses (microsoft))
    ("Plantagenet Cherokee"           :licenses (apple microsoft))
    ("Playbill"                       :licenses (microsoft))
    ("Poor Richard"                   :licenses (microsoft))
    ("Pristina"                       :licenses (microsoft))
    ("Qataban"                        :licenses (free))
    ("Quivira"                        :licenses (free))
    ("Raanana"                        :licenses (apple))
    ("Raavi"                          :licenses (microsoft))
    ("Rage"                           :licenses (microsoft))
    ("Ransom"                         :licenses (microsoft))
    ("Ravie"                          :licenses (microsoft))
    ("RefSpecialty"                   :licenses (microsoft))
    ("Rockwell Condensed"             :licenses (microsoft))
    ("Rockwell"                       :licenses (microsoft))
    ("Rod"                            :licenses (microsoft))
    ("Roya"                           :licenses (free) :arabic 'farsi)
    ("Runic MT Condensed"             :licenses (microsoft))
    ("ST Fangsong"                    :licenses (apple) :chinese 'simplified)
    ("ST Heiti"                       :licenses (apple) :chinese 'simplified)
    ("ST Kaiti"                       :licenses (apple) :chinese 'simplified)
    ("ST Song"                        :licenses (apple) :chinese 'simplified)
    ("STIX General"                   :licenses (apple free))
    ("STIX Integrals D"               :licenses (apple free))
    ("STIX Integrals Sm"              :licenses (apple free))
    ("STIX Integrals Up D"            :licenses (apple free))
    ("STIX Integrals Up Sm"           :licenses (apple free))
    ("STIX Integrals Up"              :licenses (apple free))
    ("STIX NonUnicode"                :licenses (apple free))
    ("STIX Size Five Sym"             :licenses (apple free))
    ("STIX Size Four Sym"             :licenses (apple free))
    ("STIX Size One Sym"              :licenses (apple free))
    ("STIX Size Three Sym"            :licenses (apple free))
    ("STIX Size Two Sym"              :licenses (apple free))
    ("STIX Variants"                  :licenses (apple free))
    ("Sakkal Majalla"                 :licenses (microsoft))
    ("Samyak Devanagari"              :licenses (free))
    ("Samyak Gujarati"                :licenses (free))
    ("Samyak Malayalam"               :licenses (free))
    ("Samyak Oriya"                   :licenses (free))
    ("Samyak Tamil"                   :licenses (free))
    ("Samyak"                         :licenses (free))
    ("Sathu"                          :licenses (apple))
    ("Saysettha MX"                   :licenses (free))
    ("Scheherazade"                   :licenses (free))
    ("Script MT"                      :licenses (microsoft))
    ("Segoe Chess"                    :licenses (microsoft))
    ("Segoe Print"                    :licenses (microsoft))
    ("Segoe Script"                   :licenses (microsoft))
    ("Segoe UI"                       :licenses (microsoft))
    ("Shonar Bangla"                  :licenses (microsoft))
    ("Showcard Gothic"                :licenses (microsoft))
    ("Shruti"                         :licenses (microsoft))
    ("Siddhanta"                      :licenses (free))
    ("Signs MT"                       :licenses (microsoft))
    ("Silom"                          :licenses (apple))
    ("SimHei"                         :spacing 'mono :chinese 'simplified :licenses (microsoft))
    ("SimSun"                         :spacing 'mono :chinese 'simplified :licenses (microsoft apple))
    ("Simplified Arabic Fixed"        :spacing 'mono :licenses (microsoft) :arabic 'standard)
    ("Simplified Arabic"              'mono :licenses (microsoft) :arabic 'standard)
    ("Sinhala MN"                     :licenses (apple))
    ("Sinhala Sangam MN"              :licenses (apple))
    ("Skia"                           :licenses (apple))
    ("Snap ITC"                       :licenses (microsoft))
    ("Songti SC"                      :licenses (apple))
    ("Sourashtra"                     :licenses (free))
    ("Sports MT"                      :licenses (microsoft))
    ("Stencil"                        :licenses (microsoft))
    ("Stop"                           :licenses (microsoft))
    ("Sundanese Unicode"              :licenses (free))
    ("Sylfaen"                        :licenses (microsoft))
    ("Symbol"                         :licenses (apple microsoft))
    ("Symbola"                        :licenses (free))
    ("Tahoma"                         :licenses (apple microsoft) :arabic 'standard)
    ("Tai Heritage Pro"               :licenses (free))
    ("Tamil MN"                       :licenses (apple))
    ("Tamil Sangam MN"                :licenses (apple))
    ("Telugu MN"                      :licenses (apple))
    ("Telugu Sangam MN"               :licenses (apple))
    ("Tempo Grunge"                   :licenses (microsoft))
    ("Tempus Sans ITC"                :licenses (microsoft))
    ("Terafik"                        :licenses (free) :arabic 'farsi)
    ("Thonburi"                       :licenses (apple))
    ("Times New Roman Special"        :licenses (microsoft))
    ("Times New Roman"                :licenses (apple microsoft))
    ("Times"                          :licenses (apple))
    ("Titr"                           :licenses (free) :arabic 'farsi)
    ("Traditional Arabic"             :licenses (microsoft) :arabic 'standard)
    ("Transport MT"                   :licenses (microsoft))
    ("Trebuchet MS"                   :licenses (apple microsoft))
    ("Tuladha Jejeg"                  :licenses (free))
    ("Tunga"                          :licenses (microsoft))
    ("Tw Cen MT Condensed"            :licenses (microsoft))
    ("Tw Cen MT"                      :licenses (microsoft))
    ("UnBatang"                       :licenses (free))
    ("Utsaah"                         :licenses (microsoft))
    ("Vacation MT"                    :licenses (microsoft))
    ("Vani"                           :licenses (microsoft))
    ("Verdana Ref"                    :licenses (microsoft))
    ("Verdana"                        :licenses (apple microsoft))
    ("Vijaya"                         :licenses (microsoft))
    ("Viner Hand ITC"                 :licenses (microsoft))
    ("Vivaldi"                        :licenses (microsoft))
    ("Vixar ASCI"                     :licenses (microsoft))
    ("Vladimir Script"                :licenses (microsoft))
    ("Vrinda"                         :licenses (microsoft))
    ("Wawati SC"                      :chinese 'simplified :licenses (apple))
    ("Wawati TC"                      :chinese 'traditional :licenses (apple))
    ("Webdings"                       :licenses (apple microsoft))
    ("Weibei SC"                      :chinese 'simplified :licenses (apple))
    ("Weibei TC"                      :chinese 'traditional :licenses (apple))
    ("WenQuanYi Zen Hei Mono"         :spacing 'mono :licenses (free) :chinese 'simplified)
    ("WenQuanYi Zen Hei"              :licenses (free) :chinese 'simplified)
    ("Westminster"                    :licenses (microsoft))
    ("Wide Latin"                     :licenses (microsoft))
    ("Wingdings 2"                    :licenses (apple microsoft))
    ("Wingdings 3"                    :licenses (apple microsoft))
    ("Wingdings"                      :licenses (apple microsoft))
    ("Xingkai SC"                     :chinese 'simplified :licenses (apple) :decorative t)
    ("Yuanti SC"                      :chinese 'simplified :licenses (apple))
    ("Yuppy SC"                       :chinese 'simplified :licenses (apple))
    ("Yuppy TC"                       :chinese 'traditional :licenses (apple))
    ("Zapf Dingbats"                  :licenses (apple))
    ("Zapfino"                        :licenses (apple))
    ("unifont"                        :licenses (free) :glyph-quality 'low)
    ))

;;; customizable variables

;;;###autoload
(defgroup unicode-fonts nil
  "Configure Unicode fonts."
  :version "0.3.6"
  :link '(emacs-commentary-link :tag "Commentary" "unicode-fonts")
  :link '(url-link :tag "Github" "http://github.com/rolandwalker/unicode-fonts")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/UnicodeFonts")
  :prefix "unicode-fonts-"
  :group 'i18n
  :group 'faces)

;;;###autoload
(defgroup unicode-fonts-tweaks nil
  "Tweaks for `unicode-fonts', especially regarding font availability."
  :group 'unicode-fonts)

(defcustom unicode-fonts-use-prepend (not (or (eq window-system 'ns)
                                              (eq window-system 'mac)))
  "Whether the 'prepend argument to `set-fontset-font' works.

Whether this argument works is depending on your operating system
and the font backend used by your Emacs build.

This defaults to nil when using the Cocoa or native Mac font
backends on OS X, t otherwise."
  :type 'boolean
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-existence-checks 'all
  "How unicode-fonts will dynamically check fonts at startup.

This option dramatically affects startup time, but is not
recommended to change from the default.

\"Check All Fonts at Startup\" is the slowest, but provides
full sanity-checking and the maximum number of gplyhs made
displayable.

\"Only First Existing Font\" is five to ten times faster than
checking all fonts.  The drawback is that fewer fallbacks will
be provided, meaning that fewer glyphs may be displayable.

\"Load All Fonts Without Checking\" is fast and provides the
maximum number of fallbacks, but Emacs could behave unpredictably
when it is instructed to display using a nonexistent font."
  :type '(choice
          (const :tag "Check All Fonts at Startup"               all)
          (const :tag "Only First Existing Font for Each Block"  first)
          (const :tag "Load All Fonts Without Checking"          none))
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-restrict-to-fonts nil
  "Limit fonts (and font-existence checks) to only those listed here.

This is a way to speed startup by informing Emacs ahead of
time that only certain fonts are present.

Each font name is a string, typically in Fontconfig font-name
format.

Leave the list empty for no restriction."
  :type '(repeat string)
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-skip-fonts nil
  "Skip over the fonts listed here.  Do not apply them as defaults.

This can be used to speed startup time, and also to enforce
choices of style.

Note, however, that this package merely provides clues to Emacs
about which fonts are good.  Even if this package skips over a
font, Emacs may still choose that font if you don't provide
a better clue.

Each font name is a string, typically in Fontconfig font-name
format.

Leave the list empty for no per-font exclusions."
  :type '(repeat string)
  :group 'unicode-fonts-tweaks)

(defcustom unicode-fonts-skip-font-groups (if (and (eq window-system 'w32)
                                                   (< window-system-version 6))
                                              '(buggy-before-vista decorative low-quality-glyphs)
                                            '(decorative low-quality-glyphs))
  "Skip over groups of fonts listed here.

This can be used to speed startup time, and also to enforce
choices of style.

Note well: each set is defined from a list of properties kept
within this library.  This listing is perforce incomplete,
therefore this setting cannot be expected to work very well
with regard to enforcing style.

It may help you get started.

Leave the list empty for no per-group exclusions."
  :type '(set  (const :tag "Simplified Chinese Script"           'chinese-simplified)
               (const :tag "Traditional Chinese Script"          'chinese-traditional)
               (const :tag "Standard Arabic Script"              'arabic-standard)
               (const :tag "Farsi Arabic Script"                 'arabic-farsi)
               (const :tag "Naskh Arabic Script"                 'arabic-naskh)
               (const :tag "Urdu Arabic Script"                  'arabic-urdu)
               (const :tag "Available only from Microsoft"       'microsoft-only)
               (const :tag "Available from Microsoft and others" 'microsoft)
               (const :tag "Non-ClearType"                       'non-cleartype)
               (const :tag "Available only from Apple"           'apple-only)
               (const :tag "Available from Apple and others"     'apple)
               (const :tag "Free"                                'free)
               (const :tag "Non-free"                            'non-free)
               (const :tag "Decorative"                          'decorative)
               (const :tag "Low Quality Glyphs"                  'low-quality-qlyphs)
               (const :tag "Buggy Display Before Vista"          'buggy-before-vista))
  :group 'unicode-fonts-tweaks)

;;;###autoload
(defgroup unicode-fonts-debug nil
  "Settings for debugging `unicode-fonts'."
  :group 'unicode-fonts)

(defcustom unicode-fonts-debug-availability nil
  "Debug font availability to the messages buffer."
  :type 'boolean
  :group 'unicode-fonts-debug)

;;; toplevel customize group

(defcustom unicode-fonts-less-feedback nil
  "Give less echo area feedback.

Leaving this off allows you to see the impact of this
library on startup time."
  :type 'boolean
  :group 'unicode-fonts)

(defcustom unicode-fonts-fallback-font-list '("Symbola" "Quivira")
  "Candidates for a general fallback font.

The fonts from this list will be used, in order, for characters
which have no explicit mapping.

Each font name is a string, typically in Fontconfig font-name
format.

Set to nil to disable."
  :type '(repeat string)
  :group 'unicode-fonts)

(defcustom unicode-fonts-fontset-names '("fontset-default" "fontset-startup" "fontset-standard")
  "Fontsets in which to install mappings via `set-fontset-font'."
  :type '(repeat string)
  :group 'unicode-fonts)

(defcustom unicode-fonts-block-font-mapping
  '(("Aegean Numbers"                                   (
                                                         "Aegean"
                                                         "Quivira"
                                                         ))
    ("Alchemical Symbols"                               (
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Alphabetic Presentation Forms"                    (
                                                         "DejaVu Sans:width=condensed"
                                                         "Arial Unicode MS"
                                                         "Quivira"
                                                         ))
    ("Ancient Greek Musical Notation"                   (
                                                         "Musica"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Ancient Greek Numbers"                            (
                                                         "Apple Symbols"
                                                         "Aegean"
                                                         "Quivira"
                                                         ))
    ("Ancient Symbols"                                  (
                                                         "Aegean"
                                                         "Quivira"
                                                         ))
    ;; ("Arabic Extended-A"                             (""))                           ; todo
    ;; ("Arabic Mathematical Alphabetic Symbols"        (""))                           ; todo
    ("Arabic Presentation Forms-A"                      (                               ; todo insufficient free coverage
                                                         "Geeza Pro"                    ; 595/611
                                                         "Arial Unicode MS"             ; 593/611
                                                         "Microsoft Sans Serif"
                                                         "Tahoma"
                                                         "Kufi Standard GK"
                                                         "Andalus"
                                                         "Arabic Typesetting"
                                                         "Adobe Arabic"                 ; 171/611
                                                         "DecoType Naskh"               ; 57/611
                                                         "Al Bayan"                     ; 62/611
                                                         "DejaVu Sans Mono"             ; 72/611
                                                         "DejaVu Sans:width=condensed"  ; 98/611
                                                         "MPH 2B Damase"                ; 24/611
                                                         "Code2000"                     ; 155/611
                                                         ))
    ("Arabic Presentation Forms-B"                      (
                                                         "DejaVu Sans Mono"             ; 140/140
                                                         "Geeza Pro"                    ; 140/140
                                                         "Adobe Arabic"                 ; 125/140
                                                         "Arial Unicode MS"
                                                         "Microsoft Sans Serif"
                                                         "Kufi Standard GK"
                                                         "DejaVu Sans:width=condensed"  ; 140/140
                                                         "DecoType Naskh"               ; 89/140
                                                         ))
    ("Arabic Supplement"                                (
                                                         "Courier New"
                                                         "Simplified Arabic Fixed"
                                                         "Simplified Arabic"
                                                         "Geeza Pro"
                                                         "Damascus"
                                                         "Andalus"
                                                         "Arabic Typesetting"
                                                         "Traditional Arabic"
                                                         "Adobe Arabic"                 ; 30/48
                                                         "Scheherazade"
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "MPH 2B Damase"
                                                         ))
    ("Arabic"                                           (
                                                         "Courier New"
                                                         "Simplified Arabic Fixed"
                                                         "Simplified Arabic"
                                                         "Adobe Arabic"                 ; 209/252
                                                         "Geeza Pro"
                                                         "Baghdad"                      ; 81/252
                                                         "Damascus"
                                                         "Al Bayan"                     ; 79/252
                                                         "Andalus"
                                                         "Arabic Typesetting"
                                                         "Traditional Arabic"
                                                         "Scheherazade"
                                                         "DejaVu Sans Mono"
                                                         "DejaVu Sans:width=condensed"
                                                         "Arial Unicode MS"
                                                         "Nadeem"
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "MPH 2B Damase"
                                                         "Kufi Standard GK"
                                                         "DecoType Naskh"
                                                         "Koodak"
                                                         ))
    ("Armenian"                                         (
                                                         "Mshtakan"
                                                         "Sylfaen"
                                                         "DejaVu Sans:width=condensed"
                                                         "Quivira"
                                                         "MPH 2B Damase"
                                                         "Arial Unicode MS"
                                                         ))
    ("Arrows"                                           (
                                                         "DejaVu Sans Mono"
                                                         "Apple Symbols"
                                                         "Cambria Math"                 ; 112/112
                                                         "DejaVu Sans:width=condensed"
                                                         "Arial Unicode MS"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Avestan"                                          (
                                                         "Ahuramzda"
                                                         ))
    ("Balinese"                                         (
                                                         "Aksara Bali"
                                                         ))
    ;; ("Bamum Supplement"                              (""))                           ; todo
    ;; ("Bamum"                                         (""))                           ; todo
    ;; ("Basic Latin"                                   (""))                           ; covered by the default font
    ;; ("Batak"                                         (""))                           ; todo
    ("Bengali"                                          (
                                                         "Bangla Sangam MN"
                                                         "Vrinda"
                                                         "Mukti Narrow"
                                                         "Akaash"
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         ))
    ("Block Elements"                                   (
                                                         "DejaVu Sans Mono"
                                                         "DejaVu Sans:width=condensed"
                                                         "Apple Symbols"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Bopomofo Extended"                                (
                                                         "MingLiU"
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "DFKai-SB"
                                                         "Microsoft JhengHei"
                                                         "BabelStone Han"               ; 27/27
                                                         ))
    ("Bopomofo"                                         (                               ; prefer traditional
                                                         "Lantinghei TC"
                                                         "MingLiU"
                                                         "SimHei"
                                                         "LiSong Pro"                   ; 37/41
                                                         "FangSong"
                                                         "SimSun"
                                                         "DFKai-SB"
                                                         "WenQuanYi Zen Hei Mono"       ; 41/41
                                                         "Microsoft JhengHei"
                                                         "Microsoft YaHei"
                                                         "Lantinghei SC"
                                                         "Arial Unicode MS"
                                                         "BabelStone Han"               ; 41/41
                                                         ))
    ("Box Drawing"                                      (
                                                         "DejaVu Sans Mono"
                                                         "DejaVu Sans"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ;; ("Brahmi"                                        (""))
    ("Braille Patterns"                                 (
                                                         "Quivira"
                                                         "DejaVu Sans:width=condensed"
                                                         "Apple Symbols"
                                                         "Symbola"
                                                         ))
    ("Buginese"                                         (
                                                         "MPH 2B Damase"
                                                         ))
    ("Buhid"                                            (
                                                         "Quivira"
                                                         ))
    ("Byzantine Musical Symbols"                        (
                                                         "Musica"
                                                         "Symbola"
                                                         ))
    ("CJK Compatibility Forms"                          (
                                                         "WenQuanYi Zen Hei Mono"       ; 32/32
                                                         "Lantinghei SC"
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "LiSong Pro"                   ; 26/32
                                                         "Baoli SC"                     ; 19/32
                                                         "Microsoft YaHei"
                                                         "Lantinghei TC"
                                                         "BabelStone Han"               ; 32/32
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Symbola"
                                                         "Xingkai SC"                   ; 19/32
                                                         "DFKai-SB"
                                                         ))
    ("CJK Compatibility Ideographs Supplement"          (
                                                         "WenQuanYi Zen Hei Mono"       ; 542/542
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "MingLiU"
                                                         "HanaMinA"                     ; 542/542
                                                         "Hiragino Kaku Gothic Pro"
                                                         "Hiragino Maru Gothic Pro"
                                                         "Hiragino Mincho Pro"
                                                         "Microsoft JhengHei"
                                                         "LiSong Pro"                   ; 11/542
                                                         ))
    ("CJK Compatibility Ideographs"                     (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "WenQuanYi Zen Hei Mono"       ; 455/472
                                                         "BabelStone Han"               ; 472/472
                                                         "UnBatang"                     ; 268/472
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Arial Unicode MS"
                                                         "Lantinghei SC"
                                                         "HanaMinA"                     ; 472/472
                                                         ))
    ("CJK Compatibility"                                (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Lantinghei SC"
                                                         "Lantinghei TC"
                                                         "Arial Unicode MS"
                                                         "WenQuanYi Zen Hei Mono"       ; 154/256
                                                         "HanaMinA"                     ; 149/256
                                                         "BabelStone Han"               ; 73/256
                                                         ))
    ("CJK Radicals Supplement"                          (
                                                         "WenQuanYi Zen Hei Mono"       ; 115/115
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "HanaMinA"                     ; 115/115
                                                         "BabelStone Han"               ; 115/115
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "DFKai-SB"
                                                         "Apple Symbols"
                                                         ))
    ("CJK Strokes"                                      (
                                                         "WenQuanYi Zen Hei Mono"       ; 36/36
                                                         "HanaMinA"                     ; 36/36
                                                         "BabelStone Han"               ; 26/26
                                                         ))
    ("CJK Symbols and Punctuation"                      (
                                                         "Lantinghei SC"
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "HanaMinA"                     ; 45/64
                                                         "WenQuanYi Zen Hei Mono"       ; 38/64
                                                         "LiSong Pro"                   ; 33/64
                                                         "ST Fangsong"                  ; 35/64
                                                         "Microsoft YaHei"
                                                         "Lantinghei TC"
                                                         "MingLiU"
                                                         "Arial Unicode MS"
                                                         "PC Myungjo"
                                                         "BabelStone Han"               ; 54/64
                                                         "Osaka:spacing=m"
                                                         ))
    ("CJK Unified Ideographs"                           (
                                                         "WenQuanYi Zen Hei Mono"       ; 20932/20941
                                                         "Lantinghei SC"
                                                         "Songti SC"                    ; 20910/20941
                                                         "SimHei"
                                                         "FangSong"
                                                         "ST Fangsong"                  ; 20910/20941
                                                         "SimSun"
                                                         "LiSong Pro"                   ; 17595/20941
                                                         "Baoli SC"                     ; 7103/20941
                                                         "HanaMinA"                     ; 20941/20941
                                                         "BabelStone Han"               ; 19051/20941
                                                         "Apple LiGothic"               ; 13060/20941
                                                         "Lantinghei TC"
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "DFKai-SB"
                                                         "Arial Unicode MS"
                                                         "Xingkai SC"                   ; 7103/20941
                                                         "GB18030 Bitmap"               ; 20902/20941
                                                         "UnBatang"                     ; 4260/20941
                                                         ))
    ("CJK Unified Ideographs Extension A"               (
                                                         "SimHei"
                                                         "FangSong"
                                                         "ST Fangsong"                  ; 6582/6646
                                                         "SimSun"
                                                         "Songti SC"                    ; 6582/6646
                                                         "Microsoft YaHei"
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Code2000"
                                                         "DFKai-SB"
                                                         "BabelStone Han"               ; 691/6646
                                                         "GB18030 Bitmap"               ; 6578/6646
                                                         ))
    ("CJK Unified Ideographs Extension B"               (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "LiSong Pro"                   ; 1640/42711
                                                         "Microsoft YaHei"
                                                         "HanaMinB"                     ; 42711/42711
                                                         "Code2002"                     ; 20158/24711
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "BabelStone Han"               ; 625/24711
                                                         "DFKai-SB"
                                                         ))
    ("CJK Unified Ideographs Extension C"               (
                                                         "HanaMinB"                     ; 4149/4149
                                                         "BabelStone Han"               ; 399/4149
                                                         ))
    ("CJK Unified Ideographs Extension D"               (
                                                         "HanaMinB"                     ; 222/222
                                                         "BabelStone Han"               ; 222/222
                                                         ))
    ("Carian"                                           (
                                                         "Aegean"
                                                         "Quivira"
                                                         ))
    ;; ("Chakma"                                        (""))                           ; todo
    ("Cham"                                             (                               ; todo quality free alternative
                                                         "Code2000"
                                                         ))
    ("Cherokee"                                         (
                                                         "Plantagenet Cherokee"
                                                         "MPH 2B Damase"
                                                         "Quivira"
                                                         ))
    ("Combining Diacritical Marks Supplement"           (
                                                         "Consolas"                     ; 13/128
                                                         "Courier New"                  ;  4/128
                                                         "Doulos SIL"                   ; 14/128
                                                         "DejaVu Sans:width=condensed"  ;  6/128
                                                         "Segoe UI"                     ;  6/128
                                                         "Tahoma"                       ; 13/128
                                                         "Code2000"                     ; 13/128
                                                         ))
    ("Combining Diacritical Marks for Symbols"          (
                                                         "Cambria Math"                 ; 22/33
                                                         "Symbola"
                                                         ))
    ("Combining Diacritical Marks"                      (
                                                         "Monaco"
                                                         "Consolas"
                                                         "Cambria Math"                 ; 110/112
                                                         "Courier New"
                                                         "DejaVu Sans:width=condensed"
                                                         "DejaVu Sans Mono"
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "Arial"
                                                         "Quivira"
                                                         ))
    ("Combining Half Marks"                             (
                                                         "Monaco"
                                                         "Consolas"
                                                         "Symbola"
                                                         ))
    ("Common Indic Number Forms"                        (
                                                         "Siddhanta"
                                                         ))
    ("Control Pictures"                                 (
                                                         "Apple Symbols"
                                                         "Arial Unicode MS"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Counting Rod Numerals"                            (
                                                         "WenQuanYi Zen Hei Mono"       ; 18/18
                                                         "Apple Symbols"
                                                         "Symbola"
                                                         "Quivira"
                                                         "Code2001"                     ; 18/18
                                                         ))
    ("Cuneiform Numbers and Punctuation"                (
                                                         "Akkadian"
                                                         ))
    ("Cuneiform"                                        (
                                                         "Akkadian"
                                                         ))
    ("Currency Symbols"                                 (
                                                         "Monaco"
                                                         "Consolas"                     ; 22/
                                                         "DejaVu Sans Mono"
                                                         "DejaVu Sans:width=condensed"
                                                         "Apple Symbols"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Cypriot Syllabary"                                (
                                                         "Aegean"
                                                         "Code2001"                     ; 55/55
                                                         ))
    ("Cyrillic Extended-A"                              (
                                                         "Quivira"
                                                         ))
    ("Cyrillic Extended-B"                              (
                                                         "Quivira"
                                                         ))
    ("Cyrillic Supplement"                              (
                                                         "Consolas"                     ; 20/40
                                                         "Courier New"
                                                         "Calibri"
                                                         "DejaVu Sans:width=condensed"
                                                         "Doulos SIL"                   ; 34/40
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Cyrillic"                                         (
                                                         "Consolas"                     ; 255/256
                                                         "Monaco"                       ; 191/256
                                                         "DejaVu Sans Mono"             ; 180/256
                                                         "DejaVu Sans:width=condensed"  ; 256/256
                                                         "Courier New"                  ; 118/256
                                                         "Calibri"                      ; 255/256
                                                         "Microsoft Sans Serif"         ; 246/256
                                                         "Code2000"                     ; 256/256
                                                         "Arial Unicode MS"             ; 226/256
                                                         "Doulos SIL"                   ; 220/256
                                                         "Symbola"                      ; 256/256
                                                         "Charcoal CY"                  ; 94/256
                                                         "Geneva CY"                    ; 94/256
                                                         ))
    ("Deseret"                                          (
                                                         "Apple Symbols"
                                                         "Analecta"
                                                         "Code2001"                     ; 80/80
                                                         ))
    ("Devanagari Extended"                              (
                                                         "Siddhanta"                    ; 28/28
                                                         ))
    ("Devanagari"                                       (
                                                         "Devanagari Sangam MN"
                                                         "Devanagari MT"
                                                         "Mangal"
                                                         "Samyak Devanagari"
                                                         "Samyak"
                                                         "Siddhanta"                    ; 127/127
                                                         "Arial Unicode MS"
                                                         ))
    ("Dingbats"                                         (
                                                         ;; "Apple Color Emoji"
                                                         "DejaVu Sans Mono"             ; 144/191
                                                         "Zapf Dingbats"                ; 174/191
                                                         "DejaVu Sans:width=condensed"  ; 174/191
                                                         "Arial Unicode MS"             ; 160/191
                                                         "Code2000"                     ; 174/191
                                                         "Symbola"                      ; 191/191
                                                         "Quivira"                      ; 160/191
                                                         ))
    ("Domino Tiles"                                     (
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Egyptian Hieroglyphs"                             (
                                                         "Aegyptus"
                                                         ))
    ("Emoticons"                                        (
                                                         ;; "Apple Color Emoji"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Enclosed Alphanumeric Supplement"                 (
                                                         "Quivira"
                                                         "BabelStone Han"               ; 171/171
                                                         ))
    ("Enclosed Alphanumerics"                           (
                                                         ;; "Aqua Kana"
                                                         "Arial Unicode MS"
                                                         "Quivira"
                                                         "BabelStone Han"               ; 160/160
                                                         ))
    ("Enclosed CJK Letters and Months"                  (
                                                         "WenQuanYi Zen Hei Mono"       ; 202/254
                                                         "SimHei"
                                                         "FangSong"
                                                         "MingLiU"
                                                         ;; "Aqua Kana"
                                                         "Arial Unicode MS"
                                                         "HanaMinA"                     ; 249/254
                                                         "BabelStone Han"               ; 191/254
                                                         "Quivira"
                                                         "UnBatang"                     ; 58/254
                                                         ))
    ("Enclosed Ideographic Supplement"                  (
                                                         "HanaMinA"                     ; 57/57
                                                         "BabelStone Han"               ; 57/57
                                                         ))
    ("Ethiopic Extended"                                (
                                                         "Kefa"
                                                         "Abyssinica SIL"
                                                         "Code2000"
                                                         ))
    ("Ethiopic Extended-A"                              (
                                                         "Kefa"
                                                         "Abyssinica SIL"
                                                         "Code2000"
                                                         ))
    ("Ethiopic Supplement"                              (
                                                         "Kefa"
                                                         "Abyssinica SIL"
                                                         "Code2000"
                                                         ))
    ("Ethiopic"                                         (
                                                         "Kefa"
                                                         "Nyala"
                                                         "Abyssinica SIL"
                                                         "Ethiopia Jiret"
                                                         "Ethiopia WashRa SemiBold"
                                                         "Ethopic Yebse"
                                                         "Code2000"
                                                         ))
    ("General Punctuation"                              (
                                                         "Monaco"
                                                         "Apple Symbols"
                                                         "Cambria Math"
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Geometric Shapes"                                 (
                                                         "DejaVu Sans Mono"
                                                         "DejaVu Sans:width=condensed"
                                                         "Arial Unicode MS"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Georgian Supplement"                              (
                                                         "DejaVu Serif:width=condensed" ; 83/88
                                                         "MPH 2B Damase"                ; 38/40
                                                         "Quivira"                      ; 40/40
                                                         ))
    ("Georgian"                                         (
                                                         "DejaVu Sans Mono"             ; 45/88
                                                         "DejaVu Sans:width=condensed"  ; 83/88
                                                         "Arial Unicode MS"             ; 78/88
                                                         "Code2000"                     ; 83/88
                                                         "Quivira"                      ; 88/88
                                                         "Sylfaen"                      ; 40/88
                                                         "MPH 2B Damase"                ; 39/88
                                                         ))
    ("Glagolitic"                                       (
                                                         "MPH 2B Damase"
                                                         "Quivira"
                                                         ))
    ("Gothic"                                           (
                                                         "Analecta"
                                                         "Quivira"
                                                         "Code2001"                     ; 27/27
                                                         ))
    ("Greek Extended"                                   (
                                                         "Consolas"                     ; 232/233
                                                         "DejaVu Sans Mono"
                                                         "Courier New"
                                                         "DejaVu Sans:width=condensed"
                                                         "Microsoft Sans Serif"
                                                         "Gentium Plus Compact"
                                                         "Gentium Plus"
                                                         "Arial Unicode MS"
                                                         "Arial"
                                                         "Tahoma"
                                                         "Doulos SIL"
                                                         "Quivira"
                                                         ))
    ("Greek and Coptic"                                 (
                                                         "Consolas"                     ; 127/134
                                                         "DejaVu Sans Mono"             ; 110/134
                                                         "DejaVu Sans:width=condensed"  ; 134/134
                                                         "Calibri"                      ; 127/134
                                                         "Microsoft Sans Serif"         ; 112/134
                                                         "Gentium Plus Compact"
                                                         "Gentium Plus"
                                                         "Lucida Console"               ; 73/134
                                                         "Arial Unicode MS"             ; 105/134
                                                         "Symbola"                      ; 134/134
                                                         "Quivira"                      ; 134/134
                                                         ))
    ("Gujarati"                                         (
                                                         "Gujarati Sangam MN"
                                                         "Gujarati MT"
                                                         "Shruti"
                                                         "Samyak Gujarati"
                                                         "Samyak"
                                                         "Arial Unicode MS"
                                                         ))
    ("Gurmukhi"                                         (
                                                         "Gurmukhi Sangam MN"
                                                         "Gurmukhi MN"
                                                         "Raavi"
                                                         "Arial Unicode MS"
                                                         "AnmolUni"
                                                         ))
    ("Halfwidth and Fullwidth Forms"                    (
                                                         "Meiryo"                       ; 166/225
                                                         "Arial Unicode MS"             ; 223/225
                                                         "Microsoft JhengHei"           ; 225/225
                                                         "BabelStone Han"               ; 173/225
                                                         "Apple Symbols"                ;  55/225
                                                         "Code2000"                     ; 186/225
                                                         ))
    ("Hangul Compatibility Jamo"                        (
                                                         "PC Myungjo"                   ; 94/94
                                                         "Malgun Gothic"
                                                         "Gulim"
                                                         "Dotum"
                                                         "Batang"
                                                         "Gungsuh"
                                                         "Apple Myungjo"                ; 94/94
                                                         "UnBatang"                     ; 94/94
                                                         "WenQuanYi Zen Hei Mono"       ; 94/94
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         "HeadLineA"                    ; 94/94
                                                         ))
    ("Hangul Jamo Extended-A"                           (
                                                         "UnBatang"
                                                         ))
    ;; ("Hangul Jamo Extended-B"                        (""))                           ; todo
    ("Hangul Jamo"                                      (
                                                         "UnBatang"                     ; 186/186
                                                         "WenQuanYi Zen Hei Mono"       ; 146/186
                                                         "PC Myungjo"                   ; 67/186
                                                         "Malgun Gothic"
                                                         "Gulim"
                                                         "Dotum"
                                                         "Batang"
                                                         "Gungsuh"
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         ))
    ("Hangul Syllables"                                 (
                                                         "Apple Gothic"                 ; 11172/11172
                                                         "Malgun Gothic"
                                                         "Gulim"
                                                         "Dotum"
                                                         "Batang"
                                                         "Gungsuh"
                                                         "UnBatang"                     ; 11172/11172
                                                         "WenQuanYi Zen Hei Mono"       ; 11172/11172
                                                         "Arial Unicode MS"
                                                         "Code2000"
                                                         ))
    ("Hanunoo"                                          (
                                                         "MPH 2B Damase"
                                                         "Quivira"
                                                         ))
    ("Hebrew"                                           (
                                                         "Miriam Fixed"
                                                         "Arial Hebrew"
                                                         "Raanana"
                                                         "New Peninim MT"
                                                         "Aharoni"
                                                         "David"
                                                         "FrankRuehl"
                                                         "Gisha"
                                                         "Levenim MT"
                                                         "Narkisim"
                                                         "Rod"
                                                         "Courier New"
                                                         "Adobe Hebrew"                 ; 54/87
                                                         "Microsoft Sans Serif"
                                                         "Tahoma"
                                                         "Lucida Sans Unicode"
                                                         "Arial Unicode MS"
                                                         "Arial"
                                                         "Quivira"
                                                         ))
    ;; ("High Private Use Surrogates"                   (""))                           ; no displayable characters
    ;; ("High Surrogates"                               (""))                           ; no displayable characters
    ("Hiragana"                                         (
                                                         "Osaka:spacing=m"
                                                         "MS Gothic"
                                                         "MS Mincho"
                                                         "MingLiU"
                                                         "Hiragino Kaku Gothic Pro"
                                                         ;; "Aqua Kana"
                                                         "Meiryo"
                                                         "Arial Unicode MS"
                                                         "HanaMinA"                     ; 93/93
                                                         "BabelStone Han"               ; 93/93
                                                         ))
    ("IPA Extensions"                                   (
                                                         "Monaco"
                                                         "Consolas"                     ; 96/96
                                                         "DejaVu Sans Mono"             ; 96/96
                                                         "Courier New"
                                                         "Arial Unicode MS"
                                                         "Arial"
                                                         "Tahoma"
                                                         "Microsoft Sans Serif"
                                                         "Symbola"                      ; 96/96
                                                         "Quivira"                      ; 96/96
                                                         ))
    ("Ideographic Description Characters"               (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "BabelStone Han"               ; 12/12
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "Apple Myungjo"
                                                         "HanaMinA"                     ; 12/12
                                                         "Quivira"
                                                         "DFKai-SB"
                                                         ))
    ("Imperial Aramaic"                                 (
                                                         "Quivira"
                                                         ))
    ("Inscriptional Pahlavi"                            (
                                                         "WenQuanYi Zen Hei"
                                                         ))
    ("Inscriptional Parthian"                           (
                                                         "WenQuanYi Zen Hei"
                                                         ))
    ("Javanese"                                         (
                                                         "Tuladha Jejeg"
                                                         ))
    ;; ("Kaithi"                                        (""))                           ; todo
    ("Kana Supplement"                                  (
                                                         "HanaMinA"                     ; 2/2
                                                         "BabelStone Han"               ; 2/2
                                                         ))
    ("Kanbun"                                           (
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Meiryo"
                                                         ;; "Aqua Kana"
                                                         "Arial Unicode MS"
                                                         "WenQuanYi Zen Hei Mono"       ; 14/16
                                                         "HanaMinA"                     ; 16/16
                                                         "BabelStone Han"               ; 16/16
                                                         "MingLiU"
                                                         ))
    ("Kangxi Radicals"                                  (
                                                         "WenQuanYi Zen Hei Mono"       ; 214/214
                                                         "SimHei"
                                                         "FangSong"
                                                         "SimSun"
                                                         "Microsoft YaHei"
                                                         "BabelStone Han"               ; 214/214
                                                         "HanaMinA"                     ; 214/214
                                                         "MingLiU"
                                                         "Microsoft JhengHei"
                                                         "DFKai-SB"
                                                         "Apple Myungjo"
                                                         "Apple Symbols"

                                                         ;; "Aqua Kana"
                                                         ))
    ("Kannada"                                          (
                                                         "Kannada Sangam MN"
                                                         "Tunga"
                                                         "Akshar Unicode"
                                                         "Arial Unicode MS"
                                                         "Kedage"
                                                         "Code2000"
                                                         ))
    ("Katakana Phonetic Extensions"                     (
                                                         "MS Gothic"
                                                         "MingLiU"
                                                         ;; "Aqua Kana"
                                                         "Meiryo"
                                                         "HanaMinA"                     ; 16/16
                                                         "BabelStone Han"               ; 16/16
                                                         ))
    ("Katakana"                                         (
                                                         "Osaka:spacing=m"
                                                         ;; "Aqua Kana"
                                                         "MS Gothic"
                                                         "MingLiU"
                                                         "Meiryo"
                                                         "HanaMinA"                     ; 96/96
                                                         "Arial Unicode MS"
                                                         "BabelStone Han"               ; 96/96
                                                         ))
    ("Kayah Li"                                         (                               ; todo quality free alternative
                                                         "Code2000"
                                                         ))
    ("Kharoshthi"                                       (
                                                         "MPH 2B Damase"
                                                         ))
    ("Khmer Symbols"                                    (
                                                         "Khmer Sangam MN"
                                                         "Khmer Mondulkiri"
                                                         "Khmer Busra"
                                                         "Code2000"
                                                         ))
    ("Khmer"                                            (
                                                         "Khmer Sangam MN"
                                                         "DaunPenh"
                                                         "Code2000"
                                                         "MoolBoran"
                                                         "Khmer Mondulkiri"
                                                         "Khmer Busra"
                                                         ))
    ("Lao"                                              (
                                                         "DejaVu Sans Mono"             ; 45/65
                                                         "Lao Sangam MN"
                                                         "DokChampa"
                                                         "Arial Unicode MS"             ; 65/65
                                                         "Saysettha MX"                 ; 65/65
                                                         "DejaVu Sans:width=condensed"  ; 65/65
                                                         ))
    ;; ("Latin Extended Additional"                     (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "Quivira"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                  ))
    ;; ("Latin Extended-A"                              (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"
    ;;                                                   "Consolas"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                   "Quivira"
    ;;                                                  ))
    ;; ("Latin Extended-B"                              (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"                       ; fairly incomplete
    ;;                                                   "Consolas"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "Quivira"
    ;;                                                  ))
    ("Latin Extended-C"                                 (
                                                         "DejaVu Sans:width=condensed"
                                                         "Cambria Math"
                                                         "Quivira"
                                                         ))
    ("Latin Extended-D"                                 (
                                                         "Quivira"
                                                         "Code2000"
                                                         ))
    ;; ("Latin-1 Supplement"                            (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"
    ;;                                                   "Consolas"
    ;;                                                   "DejaVu Sans Mono"
    ;;                                                   "Courier New"
    ;;                                                   "DejaVu Sans:width=condensed"
    ;;                                                   "Quivira"
    ;;                                                  ))
    ;; ("Lepcha"                                        (""))                           ; todo
    ("Letterlike Symbols"                               (
                                                         "Apple Symbols"                ; 77/80
                                                         "Cambria Math"                 ; 67/80
                                                         "DejaVu Sans:width=condensed"  ; 75/80
                                                         "Arial Unicode MS"             ; 57/80
                                                         "Code2000"                     ; 80/80
                                                         "Symbola"                      ; 80/80
                                                         "Quivira"                      ; 80/80
                                                         ))
    ("Limbu"                                            (
                                                         "MPH 2B Damase"
                                                         ))
    ("Linear B Ideograms"                               (
                                                         "Aegean"
                                                         "Code2001"                     ; 123/123
                                                         ))
    ("Linear B Syllabary"                               (
                                                         "Aegean"
                                                         "Code2001"                     ; 88/88
                                                         ))
    ("Lisu"                                             (
                                                         "Quivira"
                                                         ))
    ;; ("Low Surrogates"                                (""))                           ; no displayable characters
    ("Lycian"                                           (
                                                         "Aegean"
                                                         "Quivira"
                                                         ))
    ("Lydian"                                           (
                                                         "Aegean"
                                                         "Quivira"
                                                         ))
    ("Mahjong Tiles"                                    (
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Malayalam"                                        (
                                                         "Malayalam Sangam MN"
                                                         "Kartika"
                                                         "Samyak Malayalam"
                                                         "Samyak"
                                                         "Akshar Unicode"
                                                         "Arial Unicode MS"
                                                         ))
    ;; ("Mandaic"                                       (""))                           ; todo
    ("Mathematical Alphanumeric Symbols"                (
                                                         "Cambria Math"                 ; 994/996
                                                         "Code2001"                     ; 994/996
                                                         "Symbola"                      ; 996/996
                                                         "Quivira"                      ; 996/996
                                                         ))
    ("Mathematical Operators"                           (
                                                         "DejaVu Sans Mono"             ; 159/256
                                                         "Apple Symbols"                ; 256/256
                                                         "Cambria Math"                 ; 256/256
                                                         "DejaVu Sans:width=condensed"  ; 256/256
                                                         "Arial Unicode MS"             ; 242/256
                                                         "Code2000"                     ; 256/256
                                                         "Symbola"                      ; 256/256
                                                         "Quivira"                      ; 256/256
                                                         ))
    ;; ("Meetei Mayek Extensions"                       ("")                            ; todo
    ("Meetei Mayek"                                     (
                                                         "Eeyek Unicode"                ; fails x11
                                                         "Meetei Mayek"
                                                         ))
    ("Meroitic Cursive"                                 (
                                                         "Aegyptus"
                                                         ))
    ("Meroitic Hieroglyphs"                             (
                                                         "Aegyptus"
                                                         ))
    ;; ("Miao"                                          (""))                           ; todo
    ("Miscellaneous Mathematical Symbols-A"             (
                                                         "Apple Symbols"
                                                         "Symbola"                      ; 48/48
                                                         "Quivira"                      ; 48/48
                                                         "Cambria Math"                 ; 28/48
                                                         ))
    ("Miscellaneous Mathematical Symbols-B"             (
                                                         "Apple Symbols"
                                                         "Cambria Math"                 ; 128/128
                                                         "Code2000"                     ; 128/128
                                                         "Symbola"                      ; 128/128
                                                         "Quivira"                      ; 128/128
                                                         ))
    ("Miscellaneous Symbols and Pictographs"            (
                                                         ;; "Apple Color Emoji"         ; 533/533
                                                         "Symbola"                      ; 533/533
                                                         "Quivira"                      ; 157/533
                                                         ))
    ("Miscellaneous Symbols and Arrows"                 (
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Miscellaneous Symbols"                            (
                                                         "Apple Symbols"
                                                         "Arial Unicode MS"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Miscellaneous Technical"                          (
                                                         "Apple Symbols"
                                                         "Cambria Math"                 ; 208/244
                                                         "Symbola"                      ; 244/244
                                                         "Quivira"                      ; 244/244
                                                         ))
    ("Modifier Tone Letters"                            (
                                                         "Apple Myungjo"
                                                         "Apple Symbols"
                                                         "Doulos SIL"                   ; 32/32
                                                         "Code2000"                     ; 32/32
                                                         "Quivira"                      ; 32/32
                                                         ))
    ("Mongolian"                                        (
                                                         "ST Fangsong"
                                                         "ST Heiti"
                                                         "ST Kaiti"
                                                         "ST Song"
                                                         "Mongolian Baiti"
                                                         "Daicing Xiaokai"
                                                         "Code2000"
                                                         ))
    ("Musical Symbols"                                  (
                                                         "Musica"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Myanmar Extended-A"                               (
                                                         "Myanmar Sangam MN"
                                                         "Padauk"
                                                         ))
    ("Myanmar"                                          (
                                                         "Myanmar Sangam MN"
                                                         "Myanmar MN"
                                                         "Padauk"
                                                         "Code2000"
                                                         ))
    ("NKo"                                              (
                                                         "Conakry"                      ; 59/59
                                                         "DejaVu Sans:width=condensed"  ; 54/59
                                                         "Code2000"                     ; 59/59
                                                         ))
    ("New Tai Lue"                                      (
                                                         "Dai Banna SIL Book"
                                                         "Dai Banna SIL Book:style=Regular"
                                                         ))
    ("Number Forms"                                     (
                                                         "DejaVu Sans:width=condensed"  ; 55/58
                                                         "Arial Unicode MS"             ; 48/58
                                                         "Symbola"                      ; 58/58
                                                         "Quivira"                      ; 58/58
                                                         "Code2000"                     ; 54/58
                                                         ))
    ("Ogham"                                            (
                                                         "DejaVu Sans:width=condensed"
                                                         "Quivira"
                                                         ))
    ("Ol Chiki"                                         (                               ; todo quality free alternative
                                                         "Code2000"
                                                         ))
    ("Old Italic"                                       (
                                                         "Aegean"
                                                         "Quivira"
                                                         "Code2001"                     ; 35/35
                                                         ))
    ("Old Persian"                                      (
                                                         "Aegean"
                                                         "Code2001"                     ; 50/50
                                                         ))
    ("Old South Arabian"                                (
                                                         "Qataban"
                                                         "Quivira"
                                                         ))
    ("Old Turkic"                                       (
                                                         "Quivira"
                                                         ))
    ("Optical Character Recognition"                    (
                                                         "Apple Symbols"
                                                         "Arial Unicode MS"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Oriya"                                            (
                                                         "Oriya Sangam MN"
                                                         "Kalinga"
                                                         "Samyak Oriya"
                                                         "Samyak"
                                                         "Arial Unicode MS"
                                                         ))
    ("Osmanya"                                          (
                                                         "MPH 2B Damase"
                                                         "Code2001"                     ; 40/40
                                                         ))
    ("Phags-pa"                                         (
                                                         "Microsoft PhagsPa"
                                                         "BabelStone Phags-pa Book"     ; 56/56
                                                         "BabelStone Phags-pa Book:style=Regular"
                                                         "Code2000"                     ; 56/56
                                                         ))
    ("Phaistos Disc"                                    (
                                                         "Aegean"
                                                         "Code2001"                     ; 46/46
                                                         ))
    ("Phoenician"                                       (
                                                         "Aegean"
                                                         "Quivira"
                                                         "Code2001"                     ; 27/29
                                                         ))
    ("Phonetic Extensions Supplement"                   (
                                                         "Consolas"                     ; 64/64
                                                         "Calibri"                      ; 64/64
                                                         "Courier New"                  ; 64/64 ; todo a better OSX choice
                                                         "Quivira"                      ; 64/64
                                                         "DejaVu Sans Mono"             ; 37/64
                                                         "DejaVu Sans:width=condensed"  ; 38/64
                                                         "Code2000"                     ; 64/64
                                                         ))
    ("Phonetic Extensions"                              (
                                                         "Monaco"
                                                         "Consolas"                     ; 128/128
                                                         "Calibri"                      ; 128/128
                                                         "Quivira"                      ; 128/128
                                                         "Courier New"                  ; 128/128
                                                         "DejaVu Sans:width=condensed"
                                                         ))
    ("Playing Cards"                                    (
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ;; ("Private Use Area"                              (""))
    ("Rejang"                                           (                               ; todo quality free alternative
                                                         "Code2000"
                                                         ))
    ;; ("Rumi Numeral Symbols"                          (""))
    ("Runic"                                            (
                                                         "Quivira"
                                                         ))
    ("Samaritan"                                        (
                                                         "Quivira"
                                                         ))
    ("Saurashtra"                                       (
                                                         "Code2000"
                                                         "Sourashtra"
                                                         ))
    ;; ("Sharada"                                       (""))                           ; todo
    ("Shavian"                                          (
                                                         "Apple Symbols"
                                                         "MPH 2B Damase"
                                                         "Code2001"                     ; 48/48
                                                         ))
    ("Sinhala"                                          (
                                                         "Sinhala Sangam MN"
                                                         "Iskoola Pota"
                                                         "Akshar Unicode"
                                                         ))
    ("Small Form Variants"                              (
                                                         "Apple Symbols"
                                                         "Arial Unicode MS"
                                                         "WenQuanYi Zen Hei Mono"       ; 25/26
                                                         "Code2000"
                                                         ))
    ;; ("Sora Sompeng"                                  (""))                           ; todo
    ;; ("Spacing Modifier Letters"                      (                               ; hopefully well-covered by the default font
    ;;                                                   "Monaco"                       ; 79/80
    ;;                                                   "Consolas"                     ; 80/80
    ;;                                                   "DejaVu Sans Mono"             ; 48/80
    ;;                                                   'Cambria Math"                 ; 80/80
    ;;                                                   "Arial Unicode MS"             ; 57/80
    ;;                                                   "Code2000"                     ; 80/80
    ;;                                                   "DejaVu Sans:width=condensed"  ; 63/80
    ;;                                                   "Quivira"                      ; 80/80
    ;;                                                   "Symbola"                      ; 80/80
    ;;                                                   ))
    ("Specials"                                         (
                                                         "Apple Symbols"
                                                         "Arial Unicode MS"
                                                         "DejaVu Sans Mono"
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"
                                                         "Quivira"
                                                         "BabelStone Han"               ; 5/5
                                                         ))
    ;; ("Sundanese Supplement"                          ())                             ; todo
    ("Sundanese"                                        (
                                                         "Sundanese Unicode"
                                                         "Hacen Sudan"
                                                         ))
    ("Superscripts and Subscripts"                      (
                                                         "Cambria Math"                 ; 32/42
                                                         "Consolas"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Supplemental Arrows-A"                            (
                                                         "Apple Symbols"
                                                         "Cambria Math"                 ; 16/16
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Supplemental Arrows-B"                            (
                                                         "Apple Symbols"
                                                         "Cambria Math"                 ; 128/128
                                                         "Symbola"
                                                         "Quivira"
                                                         ))
    ("Supplemental Mathematical Operators"              (
                                                         "Apple Symbols"
                                                         "Cambria Math"                 ; 256/256
                                                         "Symbola"                      ; 256/256
                                                         "Quivira"                      ; 256/256
                                                         ))
    ("Supplemental Punctuation"                         (
                                                         "Symbola"
                                                         "Quivira"
                                                         "Code2000"
                                                         ))
    ;; ("Supplementary Private Use Area-A"              (""))
    ;; ("Supplementary Private Use Area-B"              (""))
    ("Syloti Nagri"                                     (
                                                         "MPH 2B Damase"
                                                         ))
    ("Syriac"                                           (
                                                         "Estrangelo Edessa"
                                                         "Estrangelo Nisibin"
                                                         "Code2000"
                                                        ))
    ("Tagalog"                                          (
                                                         "Quivira"
                                                         ))
    ("Tagbanwa"                                         (
                                                         "Quivira"
                                                         ))
    ("Tags"                                             (
                                                         "BabelStone Han"               ; 97/97
                                                         ))
    ("Tai Le"                                           (
                                                         "Microsoft Tai Le"
                                                         "MPH 2B Damase"
                                                         ))

    ("Tai Tham"                                         (
                                                         "Lanna Alif"
                                                         ))
    ("Tai Viet"                                         (
                                                         "Tai Heritage Pro"
                                                         ))
    ("Tai Xuan Jing Symbols"                            (
                                                         "Apple Symbols"
                                                         "WenQuanYi Zen Hei Mono"       ; 87/87
                                                         "BabelStone Han"               ; 87/87
                                                         "DejaVu Sans:width=condensed"
                                                         "Symbola"
                                                         "Quivira"
                                                         "Code2001"                     ; 87/87
                                                         ))
    ;; ("Takri"                                         (""))                           ; todo
    ("Tamil"                                            (
                                                         "Tamil Sangam MN"
                                                         "InaiMathi"
                                                         "Latha"
                                                         "Maduram"
                                                         "Akshar Unicode"
                                                         "Samyak Tamil"
                                                         "Samyak"
                                                         "Arial Unicode MS"
                                                         ))
    ("Telugu"                                           (
                                                         "Telugu Sangam MN"
                                                         "Gautami"
                                                         "Akshar Unicode"
                                                         "Code2000"
                                                         "Arial Unicode MS"
                                                         ))
    ("Thaana"                                           (
                                                         "MV Boli"
                                                         "MPH 2B Damase"
                                                         ))
    ("Thai"                                             (
                                                         "Ayuthaya"
                                                         "Silom"
                                                         "Krungthep"
                                                         "Sathu"
                                                         "Thonburi"
                                                         "DokChampa"
                                                         "Angsana New"
                                                         "Tahoma"
                                                         "Arial Unicode MS"
                                                         "Quivira"
                                                         ))
    ("Tibetan"                                          (
                                                         "Kailasa"
                                                         "Kokonor"
                                                         "Microsoft Himalaya"
                                                         "Jomolhari"
                                                         "Monlam Uni Sans Serif"
                                                         "Arial Unicode MS"
                                                         ))
    ("Tifinagh"                                         (
                                                         "DejaVu Sans:width=condensed"
                                                         "Quivira"
                                                         ))
    ("Transport and Map Symbols"                        (
                                                         ;; "Apple Color Emoji"
                                                         "Symbola"
                                                         ))
    ("Ugaritic"                                         (
                                                         "Aegean"
                                                         "Code2001"                     ; 31/31
                                                         ))
    ("Unified Canadian Aboriginal Syllabics Extended"   (
                                                         "Euphemia UCAS"
                                                         "Euphemia"
                                                         "Quivira"
                                                         ))
    ("Unified Canadian Aboriginal Syllabics"            (
                                                         "Euphemia UCAS"
                                                         "Euphemia"
                                                         "Quivira"
                                                         ))
    ("Vai"                                              (
                                                         "Quivira"
                                                         ))
    ("Variation Selectors Supplement"                   (
                                                         "BabelStone Han"               ; 240/240
                                                         ))
    ("Variation Selectors"                              (
                                                         "BabelStone Han"               ; 16/16
                                                         ))
    ("Vedic Extensions"                                 (
                                                         "Siddhanta"
                                                         ))
    ("Vertical Forms"                                   (
                                                         "Symbola"
                                                         ))
    ("Yi Radicals"                                      (
                                                         "ST Fangsong"                  ; 32/32
                                                         "PC Myungjo"
                                                         "Microsoft Yi Baiti"
                                                         "Nuosu SIL"
                                                         ))
    ("Yi Syllables"                                     (
                                                         "ST Fangsong"                  ; 1024/1024
                                                         "Apple Myungjo"
                                                         "Microsoft Yi Baiti"
                                                         "Nuosu SIL"
                                                         ))
    ("Yijing Hexagram Symbols"                          (
                                                         "Apple Symbols"
                                                         "DejaVu Sans:width=condensed"
                                                         "WenQuanYi Zen Hei Mono"       ; 64/64
                                                         "BabelStone Han"               ; 64/64
                                                         "Symbola"
                                                         "Quivira"
                                                         )))
  "Preferred fonts for each Unicode block.

These mappings are only installed in Emacs if a preferred font
for the block is available on your system.  When multiple fonts
are given, each is tried in order."
  :type '(alist :key-type string :value-type (group (repeat :tag "Fonts" (string :tag ""))))
  :options (mapcar 'car unicode-fonts-blocks)
  :group 'unicode-fonts)

(defcustom unicode-fonts-ignore-overrides nil
  "Ignore settings in `unicode-fonts-overrides-mapping'."
  :type 'boolean
  :group 'unicode-fonts)

(defcustom unicode-fonts-overrides-mapping
  '(
    ;; Control Pictures block
    ("Symbol for Escape"                              "Symbol for Escape"                           ("Keyboard"                          ))  ; OSX shift key

    ;; Arrows block
    ("Rightwards Arrow with Hook"                     "Rightwards Arrow with Hook"                  ("Keyboard"                          ))
    ("Rightwards Arrow to Bar"                        "Rightwards Arrow to Bar"                     ("Lucida Grande"                     ))  ; Tab key
    ("Upwards White Arrow"                            "Upwards White Arrow"                         ("Keyboard" "Lucida Grande"          ))  ; OSX shift key
    ("Upwards White Arrow from Bar"                   "Upwards White Arrow from Bar"                ("Keyboard" "Lucida Grande"          ))

    ;; Miscellaneous Technical block
    ("Up Arrowhead"                                   "Up Arrowhead"                                ("Keyboard" "Lucida Grande"          ))  ; OSX control key
    ("Projective"                                     "Projective"                                  ("Lucida Grande"                     ))  ; OSX key?
    ("Up Arrowhead Between Two Horizontal Bars"       "Up Arrowhead Between Two Horizontal Bars"    ("Keyboard" "Lucida Grande"          ))
    ("Place of Interest Sign"                         "Place of Interest Sign"                      ("Keyboard" "Lucida Grande"          ))  ; OSX command key
    ("Option Key"                                     "Option Key"                                  ("Keyboard" "Lucida Grande"          ))
    ("Erase to the Right"                             "Erase to the Right"                          ("Keyboard" "Lucida Grande"          ))
    ("X in a Rectangle Box"                           "X in a Rectangle Box"                        ("Keyboard" "Lucida Grande"          ))
    ("Erase To the Left"                              "Erase To the Left"                           ("Keyboard" "Lucida Grande"          ))  ; Backspace
    ("APL Functional Symbol Quad Backslash"           "APL Functional Symbol Quad Backslash"        ("Lucida Grande"                     ))  ; OSX key?
    ("Alternative Key Symbol"                         "Alternative Key Symbol"                      ("Keyboard" "Lucida Grande"          ))  ; OSX alt key
    ("Broken Circle with Northwest Arrow"             "Broken Circle with Northwest Arrow"          ("Keyboard" "Lucida Grande"          ))  ; OSX power key
    ("Eject Symbol"                                   "Eject Symbol"                                ("Keyboard" "Lucida Grande"          ))

    ;; General Punctuation block
    ("En Quad"                                        "Zero Width Joiner"                           ("DejaVu Sans" "Symbola" "Arial Unicode MS"))   ; space variations are proportional
    ("Bullet"                                         "Bullet"                                      ("DejaVu Sans:width=condensed"       ))

    ;; Geometric Shapes block
    ("White Bullet"                                   "White Bullet"                                ("DejaVu Sans:width=condensed"       ))

    ;; Mathematical Operators block
    ("Circled Times"                                  "Circled Times"                               ("Arial Unicode MS"                  ))

    ;; Currency Symbols block
    ("Livre Tournois Sign"                            #x20CF                                        ("Symbola"                           ))
    ("Drachma Sign"                                   "Drachma Sign"                                ("DejaVu Sans Mono"                  ))
    ("German Penny Sign"                              "German Penny Sign"                           ("DejaVu Sans Mono"                  ))
    ("New Sheqel Sign"                                "New Sheqel Sign"                             ("DejaVu Sans Mono"                  ))

    ;; Dingbats block
    ("White Heavy Check Mark"                         "White Heavy Check Mark"                          ("Symbola"                      ))
    ("Raised Fist"                                    "Raised Hand"                                     ("Symbola"                      ))
    ("Sparkles"                                       "Sparkles"                                        ("Symbola"                      ))
    ("Cross Mark"                                     "Cross Mark"                                      ("Symbola"                      ))
    ("Negative Squared Cross Mark"                    "Negative Squared Cross Mark"                     ("Symbola"                      ))
    ("Black Question Mark Ornament"                   "White Exclamation Mark Ornament"                 ("Symbola"                      ))
    ("Heavy Exclamation Mark Symbol"                  "Heavy Exclamation Mark Symbol"                   ("Symbola"                      ))
    ("Heavy Low Single Comma Quotation Mark Ornament" "Heavy Low Double Comma Quotation Mark Ornament"  ("Symbola"                      ))
    ("Dingbat Negative Circled Digit One"             "Dingbat Negative Circled Sans-Serif Number Ten"  ("Zapf Dingbats" "DejaVu Sans:width=condensed" "Symbola"))
    ("Heavy Plus Sign"                                "Heavy Division Sign"                             ("Symbola"                      ))
    ("Curly Loop"                                     "Curly Loop"                                      ("Symbola"                      ))
    ("Double Curly Loop"                              "Double Curly Loop"                               ("Symbola"                      ))

    ;; Phonetic Extensions block
    ("Latin Small Letter Turned A"                    "Latin Small Letter Turned A"                 ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter C with Curl"                 "Latin Small Letter C with Curl"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Closed Reversed Open E"      "Latin Small Letter Closed Reversed Open E"   ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Gamma"                       "Latin Small Letter Gamma"                    ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Rams Horn"                   "Latin Small Letter Rams Horn"                ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter L with Belt"                 "Latin Small Letter L with Belt"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Closed Omega"                "Latin Small Letter Closed Omega"             ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Esh with Curl"               "Latin Small Letter Esh with Curl"            ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter V with Hook"                 "Latin Small Letter V with Hook"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Z with Retroflex Hook"       "Latin Small Letter Z with Retroflex Hook"    ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Z with Curl"                 "Latin Small Letter Z with Curl"              ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Ezh with Curl"               "Latin Small Letter Ezh with Curl"            ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Closed Open E"               "Latin Small Letter Closed Open E"            ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Letter Small Capital G with Hook"         "Latin Letter Small Capital G with Hook"      ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter J with Crossed-Tail"         "Latin Small Letter J with Crossed-Tail"      ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Dezh Digraph"                "Latin Small Letter Dezh Digraph"             ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Dz Digraph with Curl"        "Latin Small Letter Dz Digraph with Curl"     ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Small Letter Tc Digraph with Curl"        "Latin Small Letter Tc Digraph with Curl"     ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Letter Voiced Laryngeal Spirant"          "Latin Letter Ain"                            ("Consolas" "Quivira"                     ))
    ("Modifier Letter Small A"                        "Modifier Letter Small Turned A"              ("Consolas" "DejaVu Sans Mono" "Quivira"  ))
    ("Modifier Letter Small Open E"                   "Modifier Letter Small Turned Open E"         ("Consolas" "DejaVu Sans Mono" "Quivira"  ))
    ("Modifier Letter Small Ain"                      "Modifier Letter Small Chi"                   ("Consolas" "Quivira"                     ))
    ("Greek Subscript Small Letter Beta"              "Greek Subscript Small Letter Chi"            ("Consolas" "Quivira"                     ))
    ("Latin Small Letter Insular G"                   "Latin Small Letter Insular G"                ("Consolas" "Quivira"                     ))

    ;; Superscripts and Subscripts block
    ("Latin Subscript Small Letter A"                 "Latin Subscript Small Letter Schwa"          ("Consolas" "DejaVu Sans Mono" "Symbola"  ))
    ("Latin Subscript Small Letter H"                 "Latin Subscript Small Letter T"              (           "DejaVu Sans Mono" "Symbola"  ))

    ;; Spacing Modifier Letters block
    ("Modifier Letter Small Gamma"                    "Modifier Letter Small Gamma"                 ("Consolas" "DejaVu Sans Mono" "Symbola"  ))

    ;; Latin Extended-B block
    ("Latin Capital Letter G with Hook"               "Latin Small Letter Hv"                       ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Oi"                        "Latin Capital Letter P with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter V with Hook"               "Latin Small Letter Y with Hook"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Tone Five"                 "Latin Letter Wynn"                           ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Yogh"                      "Latin Small Letter Yogh"                     ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter L with Curl"                 "Latin Small Letter T with Curl"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter B with Hook"               "Latin Capital Letter B with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter C with Hook"               "Latin Capital Letter C with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter C with Hook"                 "Latin Small Letter C with Hook"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter D with Hook"               "Latin Capital Letter D with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Turned Delta"                "Latin Small Letter Turned Delta"             ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter K with Hook"               "Latin Capital Letter K with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Lambda with Stroke"          "Latin Small Letter Lambda with Stroke"       ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Letter Yr"                                "Latin Letter Yr"                             ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Letter Reversed Esh Loop"                 "Latin Letter Reversed Esh Loop"              ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter T with Hook"               "Latin Capital Letter T with Hook"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Ezh with Tail"               "Latin Small Letter Ezh with Tail"            ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Hwair"                     "Latin Capital Letter Hwair"                  ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter S with Swash Tail"           "Latin Small Letter S with Swash Tail"        ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Small Letter Z with Swash Tail"           "Latin Small Letter Z with Swash Tail"        ("Consolas" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"  ))
    ("Latin Capital Letter Wynn"                      "Latin Capital Letter Wynn"                   ("Consolas" "DejaVu Sans:width=condensed"                     ))
    ("Latin Small Letter Ou"                          "Latin Small Letter Ou"                       ("Consolas" "DejaVu Sans:width=condensed"                     ))
    ("Latin Small Letter Glottal Stop"                "Latin Small Letter Y with Stroke"            ("Consolas" "DejaVu Sans:width=condensed"                     ))

    ;; Latin Extended Additional block
    ("Latin Small Letter Long S with Diagonal Stroke" "Latin Small Letter Delta"                    ("DejaVu Sans:width=condensed" "Quivira"))
    ("Latin Capital Letter Middle-Welsh Ll"           "Latin Small Letter Y with Loop"              ("Quivira"                              ))

    ;; Enclosed CJK Letters and Months block
    ("Circled Hangul Kiyeok"                          "Korean Standard Symbol"                      ("PC Myungjo" "PilGi" "Malgun Gothic"))         ; Korean symbols

    ;; Halfwidth and Fullwidth Forms block
    ("Halfwidth Ideographic Full Stop"                "Halfwidth Katakana Semi-Voiced Sound Mark"   ("Osaka:spacing=m" "Meiryo" "HanaMinA"))        ; Japanese letters

    ;; Greek and Coptic block
    ("Coptic Capital Letter Shei"                     "Coptic Small Letter Dei"                     ("Microsoft Sans Serif" "DejaVu Sans:width=condensed"))

    ;; Alphabetic Presentation Forms block
    ("Hebrew Letter Yod with Hiriq"                   "Hebrew Ligature Alef Lamed"                  ("Miriam Fixed" "Arial Hebrew" "Adobe Hebrew" "Arial Unicode MS" "Quivira"))
    ("Armenian Small Ligature Men Now"                "Armenian Small Ligature Men Xeh"             ("Mshtakan" "Sylfaen" "DejaVu Sans:width=condensed" "Quivira" "Arial Unicode MS" ))

    ;; Arabic letters
    ("Arabic Letter Hah with Small Arabic Letter Tah Below" "Arabic Letter Kaf with Two Dots Above" ("Geeza Pro"                         ))

    ;; Cyrillic Supplement block
    ("Cyrillic Capital Letter Lha"                    "Cyrillic Small Letter Pe with Descender"      ("DejaVu Sans:width=condensed" "Doulos SIL" "Symbola" "Quivira"))
    ("Cyrillic Capital Letter Shha with Descender"    "Cyrillic Small Letter Shha with Descender"    ("Doulos SIL" "Symbola" "Quivira"    ))
    (#x528                                            #x52F                                          ("Symbola" "Quivira"                 ))

    ;; Cyrillic block
    ("Cyrillic Capital Letter Omega"                     "Cyrillic Small Letter Omega"                          ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Iotified E"                "Cyrillic Small Letter Psi"                            ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Izhitsa"                   "Cyrillic Small Letter Er With Tick"                   ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ghe with Middle Hook"      "Cyrillic Small Letter Ghe with Middle Hook"           ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ka With Vertical Stroke"   "Cyrillic Small Letter Bashkir Ka"                     ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Pe With Middle Hook"       "Cyrillic Small Letter Abkhasian Ha"                   ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Ligature Te Tse"                  "Cyrillic Small Letter Che With Vertical Stroke"       ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Abkhasian Che"             "Cyrillic Small Letter Abkhasian Che With Descender"   ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ka with Hook"              "Cyrillic Small Letter Palochka"                       ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Abkhasian Dze"             "Cyrillic Small Letter Abkhasian Dze"                  ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ghe with Descender"        "Cyrillic Small Letter Ghe with Descender"             ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))
    ("Cyrillic Capital Letter Ghe With Stroke And Hook"  "Cyrillic Small Letter Ha With Stroke"                 ("Consolas" "DejaVu Sans:width=condensed" "Symbola"))

    ;; Number forms block (making the vulgar fractions monospace if possible)
    ("Vulgar Fraction One Third"    "Vulgar Fraction Seven Eighths"  ("Consolas" "DejaVu Sans Mono"      ))

    ;; Letterlike Symbols block
    ("Account Of"                   "Addressed To The Subject"       ("Apple Symbols" "Symbola" "Quivira"))
    ("Cada Una"                     "Cada Una"                       ("Apple Symbols" "Symbola" "Quivira"))
    ("Prescription Take"            "Telephone Sign"                 ("Apple Symbols" "Symbola" "Quivira"))
    ("Versicle"                     "Versicle"                       ("Apple Symbols" "Symbola" "Quivira"))
    ("Turned Capital F"             "Turned Capital F"               ("Apple Symbols" "Symbola" "Quivira"))
    ("Facsimile Sign"               "Facsimile Sign"                 ("Apple Symbols" "Symbola" "Quivira"))
    ("Double-Struck Small Pi"       "Double-Struck Small Pi"         ("Symbola" "Quivira"                ))
    ("Per Sign"                     "Per Sign"                       ("Symbola" "Quivira"                ))
    ("Symbol For Samaritan Source"  "Symbol For Samaritan Source"    ("Symbola" "Quivira"                ))

    ;; Greek and Coptic block
    ("Greek Capital Letter Heta"                "Greek Small Letter Archaic Sampi"       ("DejaVu Sans:width=condensed" "Symbola" "Quivira"))
    ("Greek Capital Letter Pamphylian Digamma"  "Greek Small Letter Pamphylian Digamma"  ("DejaVu Sans:width=condensed" "Symbola" "Quivira"))
    ("Greek Capital Kai Symbol"                 "Greek Capital Kai Symbol"               ("DejaVu Sans:width=condensed" "Symbola" "Quivira")))

  "Overrides for `unicode-fonts-block-font-mapping' over arbitrary ranges.

Ranges are specified using the full UCS name or UCS number of
the start and end characters.  To override just one character,
give the same value for both endpoints.

These mappings are only installed in Emacs if a preferred font
for the range is available on your system.  When multiple fonts
are given, each is tried in order.

If the font backend provided by your operating system handles
glyph-by-glyph fallthrough well, you may not need many of
these mappings."
  :type '(alist :key-type (choice (string :tag "Start Character Name")
                                  (integer :tag "Start Character Number"))
                :value-type (group (choice (string :tag "End Character Name")
                                           (integer :tag "End Character Number"))
                                   (repeat :tag "Preferred Fonts" (string :tag ""))))
  :group 'unicode-fonts)

;;; variables

(defvar unicode-fonts-setup-done              nil "Fontsets for which unicode-font setup is complete.")
(defvar unicode-fonts-skipped-fonts-computed  nil "The computed extension of `unicode-fonts-skip-fonts'.")

;; note: variable outside unicode-fonts- namespace
(defvar unicode-block-history                 nil "History of Unicode blocks entered in the minibuffer.")

;;; utility functions

;;;###autoload
(defun unicode-fonts-first-existing-font (font-names)
  "Return the (normalized) first existing font name from FONT-NAMES.

FONT-NAMES is a list, with each element typically in Fontconfig
font-name format.

The font existence-check is lazy; fonts after the first hit are
not checked."
  (font-utils-first-existing-font (remove-if #'(lambda (x)
                                                    (member* x unicode-fonts-skipped-fonts-computed :test 'font-utils-lenient-name-equal))
                                                font-names)))

;;;###autoload
(defun unicode-fonts-font-exists-p (font-name &optional point-size strict)
  "Run `font-utils-exists-p' with a limited scope.

The scope is defined by `unicode-fonts-restrict-to-fonts'.

FONT-NAME, POINT-SIZE, and STRICT are as documented at
`font-utils-exists-p'."
  (font-utils-exists-p font-name point-size strict unicode-fonts-restrict-to-fonts))

(defsubst unicode-fonts--create-char-range (range)
  "Create a numeric character range from RANGE.

RANGE is a list of two UCS character representations, either in
the form of integer code points or Unicode character names.

The return value is a list of two ascending integers, or nil on
error."
  (let ((return-range (copy-tree range)))
    (when (stringp (car return-range))
      (setf (car return-range) (ucs-utils-char (car return-range) nil)))
    (when (stringp (cadr return-range))
      (setf (cadr return-range) (ucs-utils-char (cadr return-range) nil)))
    (if (and (integerp (car return-range)) (integerp (cadr return-range)))
        (progn
          (when (> (car return-range) (cadr return-range))
            (setq return-range (nreverse return-range)))
          return-range)
      nil)))

(defun unicode-fonts-compute-skipped-fonts ()
  "Compute list of fonts to skip from consideration."
  (setq unicode-fonts-skipped-fonts-computed unicode-fonts-skip-fonts)
  (dolist (cell unicode-fonts-known-font-characteristics)
    (let ((name (car cell))
          (props (cdr cell)))
      (setq name (replace-regexp-in-string ":.*\\'" "" name))
      (when (and (memq 'chinese-traditional unicode-fonts-skip-font-groups)
                 (eq 'traditional (plist-get props :chinese)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'chinese-simplified unicode-fonts-skip-font-groups)
                 (eq 'simplified (plist-get props :chinese)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-standard unicode-fonts-skip-font-groups)
                 (eq 'standard (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-farsi unicode-fonts-skip-font-groups)
                 (eq 'farsi (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'arabic-naskh unicode-fonts-skip-font-groups)
                 (eq 'naskh (plist-get props :arabic)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'microsoft-only unicode-fonts-skip-font-groups)
                 (equal '(microsoft) (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'apple-only unicode-fonts-skip-font-groups)
                 (equal '(apple) (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'microsoft unicode-fonts-skip-font-groups)
                 (memq 'microsoft (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'apple unicode-fonts-skip-font-groups)
                 (memq 'apple (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'free unicode-fonts-skip-font-groups)
                 (memq 'free (plist-get props :licenses)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'non-free unicode-fonts-skip-font-groups)
                 (not (memq 'free (plist-get props :licenses))))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'low-quality-glyphs unicode-fonts-skip-font-groups)
                 (eq 'low (plist-get props :glyph-quality)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'decorative unicode-fonts-skip-font-groups)
                 (eq t (plist-get props :decorative)))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'non-cleartype unicode-fonts-skip-font-groups)
                 (not (eq t (plist-get props :cleartype))))
        (push name unicode-fonts-skipped-fonts-computed))
      (when (and (memq 'buggy-before-vista unicode-fonts-skip-font-groups)
                 (eq t (plist-get props :buggy-before-vista)))
        (push name unicode-fonts-skipped-fonts-computed))
      (delete-dups unicode-fonts-skipped-fonts-computed))))

;;;###autoload
(defun unicode-fonts-read-block-name (&optional ido)
  "Read a Unicode block name using `completing-read'.

Spaces are replaced with underscores in completion values, but
are removed from the return value.

Use `ido-completing-read' if IDO is set."
  (save-match-data
    (let ((prompt "Block: ")
          (reader (if ido 'ido-completing-read 'completing-read))
          (block-names (mapcar #'(lambda (x)
                                   (replace-regexp-in-string " " "_" x))
                               (mapcar 'car unicode-fonts-blocks))))
      (replace-regexp-in-string "_" " "
         (funcall reader prompt block-names nil nil nil unicode-block-history)))))

;;; debugging functions

(defun unicode-fonts-debug-info-at-point ()
  "Display debug info about the character at point."
  (when (char-after)
    (let ((font (font-at (point)))
          (font-name nil)
          (font-size nil)
          (block-name nil)
          (plane-name nil)
          (char-name (ucs-utils-pretty-name (char-after))))
      (when font
        (setq font-name (or (font-get font :name)
                            (font-get font :family)))
        (when (and font-name
                   (symbolp font-name))
          (setq font-name (symbol-name font-name)))
        (setq font-size (font-get font :size))
        (when (numberp font-size)
          (setq font-size (number-to-string font-size)))
        (when (and (stringp font-name)
                   (stringp font-size)
                   (> (length font-name) 0)
                   (> (length font-size) 0))
          (callf concat font-name "-" font-size))
        (unless (and (stringp font-name)
                     (> (length font-name) 0))
          (setq font-name (font-utils-name-from-xlfd (font-xlfd-name font)))))
      (setq block-name
           (catch 'bn
             (dolist (cell unicode-fonts-blocks)
               (let* ((block-name (car cell))
                      (char-range (cdr cell)))
                 (when (and (>= (char-after) (car char-range))
                            (<= (char-after) (cadr char-range)))
                   (throw 'bn block-name))))))
      (setq plane-name
           (catch 'pn
             (dolist (cell unicode-fonts-planes)
               (let* ((plane-name (car cell))
                      (char-range (cdr cell)))
                 (when (and (>= (char-after) (car char-range))
                            (<= (char-after) (cadr char-range)))
                   (throw 'pn plane-name))))))
      (message "font: %s / block: %s / plane: %s / char: %s" font-name block-name plane-name char-name))))

(defun unicode-fonts-debug-change-font-for-block (&optional block-name font-name)
  "Calling this command can crash Emacs.

Temporarily change the font used for BLOCK-NAME to FONT-NAME.

To permanently change the font for BLOCK-NAME, use the
customization interface."
  (callf or block-name (unicode-fonts-read-block-name 'ido))
  (callf or font-name (font-utils-read-name 'ido))
  (assert (assoc-string block-name unicode-fonts-blocks 'case-fold) nil "No such block")
  (assert (unicode-fonts-font-exists-p font-name) nil "Font does not is exist or is not understood: %s" font-name)
  (when (y-or-n-p (propertize "Really risk crashing Emacs?" 'face 'highlight))
    (message "")
    (let ((char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold))))
      (dolist (fontset-name (remove-if-not #'(lambda (fs) (ignore-errors (fontset-info fs))) unicode-fonts-fontset-names))
        (set-fontset-font fontset-name
                          (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                          (cons font-name "iso10646-1"))))))

(defun unicode-fonts-debug-change-all-fonts (&optional font-name)
  "Calling this command can crash Emacs.

Temporarily change the font used for all blocks to FONT-NAME."
  (callf or font-name (font-utils-read-name 'ido))
  (assert (unicode-fonts-font-exists-p font-name) nil "Font does not is exist or is not understood: %s" font-name)
  (when (y-or-n-p (propertize "Really risk crashing Emacs?" 'face 'highlight))
    (dolist (fontset-name (remove-if-not #'(lambda (fs) (ignore-errors (fontset-info fs))) unicode-fonts-fontset-names))
      (dolist (cell unicode-fonts-block-font-mapping)
        (let* ((block-name (car cell))
               (char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold))))
          (when char-range
            (set-fontset-font fontset-name
                              (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                              (cons font-name "iso10646-1"))))))))

(defun unicode-fonts-debug-interactively (&optional arg)
  "Always show the font at point.

This is a buffer-local setting.  Turn it off by quitting the
buffer or calling this function with negative ARG."
  (if (and (numberp arg)
           (< arg 0))
      (remove-hook 'post-command-hook 'unicode-fonts-debug-info-at-point t)
    ;; else
    (add-hook 'post-command-hook 'unicode-fonts-debug-info-at-point t t)))

(defun unicode-fonts-debug-insert-block (&optional block-name)
  "Insert all the characters from BLOCK-NAME for debugging purposes.

See also: `list-charset-chars'."
  (callf or block-name (unicode-fonts-read-block-name 'ido))
  (set-buffer-multibyte t)
  (cond
    ((eq block-name 'all)
     (dolist (name (reverse (mapcar 'car unicode-fonts-blocks)))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-cjk)
     (dolist (name (reverse '(
                     "Bopomofo Extended"
                     "Bopomofo"
                     "CJK Compatibility Forms"
                     "CJK Compatibility Ideographs Supplement"
                     "CJK Compatibility Ideographs"
                     "CJK Compatibility"
                     "CJK Radicals Supplement"
                     "CJK Strokes"
                     "CJK Symbols and Punctuation"
                     "CJK Unified Ideographs Extension A"
                     "CJK Unified Ideographs Extension B"
                     "CJK Unified Ideographs Extension C"
                     "CJK Unified Ideographs Extension D"
                     "CJK Unified Ideographs"
                     "Enclosed CJK Letters and Months"
                     "Enclosed Ideographic Supplement"
                     "Ideographic Description Characters"
                     "Hangul Compatibility Jamo"
                     "Hangul Jamo Extended-A"
                     "Hangul Jamo Extended-B"
                     "Hangul Jamo"
                     "Hangul Syllables"
                     "Kana Supplement"
                     "Kanbun"
                     "Kangxi Radicals"
                     "Katakana Phonetic Extensions"
                     "Katakana"
                     "Hiragana"
                     "Modifier Tone Letters"
                     "Yi Radicals"
                     "Yi Syllables")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-greek)
     (dolist (name (reverse '(
                     "Ancient Greek Musical Notation"
                     "Ancient Greek Numbers"
                     "Greek Extended"
                     "Greek and Coptic"
                     "Linear B Ideograms"
                     "Linear B Syllabary")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-math)
     (dolist (name (reverse '(
                     "Mathematical Alphanumeric Symbols"
                     "Mathematical Operators"
                     "Miscellaneous Mathematical Symbols-A"
                     "Miscellaneous Mathematical Symbols-B"
                     "Miscellaneous Technical"
                     "Superscripts and Subscripts"
                     "Supplemental Arrows-A"
                     "Supplemental Arrows-B"
                     "Supplemental Mathematical Operators"
                     "Number Forms"
                     "Letterlike Symbols")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-arabic)
     (dolist (name (reverse '(
                     "Arabic Extended-A"
                     "Arabic Mathematical Alphabetic Symbols"
                     "Arabic Presentation Forms-A"
                     "Arabic Presentation Forms-B"
                     "Arabic Supplement"
                     "Arabic")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-cyrillic)
     (dolist (name (reverse '(
                     "Cyrillic Extended-A"
                     "Cyrillic Extended-B"
                     "Cyrillic Supplement"
                     "Cyrillic")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-ethiopic)
     (dolist (name (reverse '(
                     "Ethiopic Extended"
                     "Ethiopic Extended-A"
                     "Ethiopic Supplement"
                     "Ethiopic")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-arrows)
     (dolist (name (reverse '(
                     "Arrows"
                     "Miscellaneous Symbols and Arrows"
                     "Supplemental Arrows-A"
                     "Supplemental Arrows-B"
                     "Transport and Map Symbols")))
       (unicode-fonts-debug-insert-block name)))
    ((eq block-name 'all-symbols)
     (dolist (name (reverse '(
                     "Aegean Numbers"
                     "Alchemical Symbols"
                     "Alphabetic Presentation Forms"
                     "Ancient Greek Musical Notation"
                     "Ancient Greek Numbers"
                     "Ancient Symbols"
                     "Arabic Mathematical Alphabetic Symbols"
                     "Arrows"
                     "Block Elements"
                     "Box Drawing"
                     "Byzantine Musical Symbols"
                     "CJK Symbols and Punctuation"
                     "Combining Diacritical Marks for Symbols"
                     "Common Indic Number Forms"
                     "Control Pictures"
                     "Counting Rod Numerals"
                     "Cuneiform Numbers and Punctuation"
                     "Currency Symbols"
                     "Dingbats"
                     "Domino Tiles"
                     "Emoticons"
                     "Enclosed Alphanumeric Supplement"
                     "Enclosed Alphanumerics"
                     "Enclosed CJK Letters and Months"
                     "Enclosed Ideographic Supplement"
                     "General Punctuation"
                     "Geometric Shapes"
                     "Halfwidth and Fullwidth Forms"
                     "IPA Extensions"
                     "Khmer Symbols"
                     "Latin-1 Supplement"
                     "Letterlike Symbols"
                     "Mahjong Tiles"
                     "Mathematical Alphanumeric Symbols"
                     "Mathematical Operators"
                     "Miscellaneous Mathematical Symbols-A"
                     "Miscellaneous Mathematical Symbols-B"
                     "Miscellaneous Symbols and Arrows"
                     "Miscellaneous Symbols and Pictographs"
                     "Miscellaneous Symbols"
                     "Miscellaneous Technical"
                     "Musical Symbols"
                     "Number Forms"
                     "Optical Character Recognition"
                     "Playing Cards"
                     "Rumi Numeral Symbols"
                     "Small Form Variants"
                     "Spacing Modifier Letters"
                     "Specials"
                     "Superscripts and Subscripts"
                     "Supplemental Arrows-A"
                     "Supplemental Arrows-B"
                     "Supplemental Mathematical Operators"
                     "Supplemental Punctuation"
                     "Tai Xuan Jing Symbols"
                     "Transport and Map Symbols"
                     "Yijing Hexagram Symbols")))
       (unicode-fonts-debug-insert-block name)))
    (t
     (save-match-data
       (let ((char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold)))
             (counter 0)
             (posn nil))
         (assert (assoc-string block-name unicode-fonts-blocks 'case-fold) nil "No such block")
         (unless (looking-at-p "^")
           (insert "\n"))
         (setq posn (point))
         (insert (replace-regexp-in-string " " "_" block-name) "\n-----\n")
         (loop for i from (car char-range) to (cadr char-range)
               do (progn
                    (insert (ucs-utils-char i)
                            "  "
                            (concat "#x" (upcase (format "%02x" i)))
                            "  "
                            "\"" (ucs-utils-pretty-name i) "\""
                            "\n")
                    (incf counter)
                    (when (eq 0 (% counter 16))
                      (insert "\n"))))
         (push-mark (point) t t)
         (goto-char posn))))))

(defun unicode-fonts-debug-check-duplicate-fonts (font-name font-list)
  "Test whether FONT-NAME occurs more than once in FONT-LIST.

Returns a list of duplicates when there is more than one
occurrence, otherwise nil."
  (let ((matches (copy-list (member* font-name font-list :test 'font-utils-lenient-name-equal)))
        (hits nil)
        (dupes nil))
    (setq matches (sort matches #'(lambda (a b)
                                    (equal a font-name))))
    (push (pop matches) dupes)
    (while (setq hits (copy-list (member* font-name matches :test 'font-utils-lenient-name-equal)))
      (let ((hit (pop hits)))
        (unless (font-utils-is-qualified-variant font-name hit)
          (push hit dupes)))
      (setq matches hits))
    (when (> (length dupes) 1)
      dupes)))

(defun unicode-fonts-debug-validate-data (&optional insert)
  "Validate `unicode-fonts-block-font-mapping' and other data.

With optional INSERT, insert debug information into the current
buffer instead of sending it to the *Messages* log."
  (let ((message-function 'message)
        (known-fonts (mapcar 'car unicode-fonts-known-font-characteristics))
        (reporter nil)
        (dupes nil)
        (counter 0)
        (all-override-ranges nil))
    (when insert
      (require 'express)
      (setq message-function 'express-message-insert))

    ;; known fonts
    (setq reporter (make-progress-reporter "Checking fonts for duplicates ... " 0 (length known-fonts)))
    (setq counter 0)
    (dolist (font known-fonts)
      (progress-reporter-update reporter (incf counter))
      (when (setq dupes (unicode-fonts-debug-check-duplicate-fonts font known-fonts))
        (funcall message-function "\n-----\nFont %s\n-----" font)
        (funcall message-function "ERROR: font occurs at least twice in known fonts: %s" dupes)))
    (progress-reporter-done reporter)

    ;; mappings
    (setq reporter (make-progress-reporter "Checking Unicode block mappings ... " 0 (length unicode-fonts-block-font-mapping)))
    (setq counter 0)
    (dolist (cell unicode-fonts-block-font-mapping)
      (progress-reporter-update reporter (incf counter))
      (let* ((block-name (car cell))
             (char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold)))
             (all-fonts-with-qualifiers (cadr cell))
             (all-fonts (mapcar #'(lambda (x) (replace-regexp-in-string ":.*\\'" "" x)) all-fonts-with-qualifiers))
             (existing-fonts (remove-if-not 'unicode-fonts-font-exists-p all-fonts))
             (existing-unskipped-fonts (remove-if #'(lambda (x)
                                                      (member* x unicode-fonts-skipped-fonts-computed :test 'font-utils-lenient-name-equal)) existing-fonts))
             (best-font (pop existing-unskipped-fonts))
             (licenses nil))
        (funcall message-function "\n-----\nBlock %s\n-----" block-name)
        (dolist (qualified-font all-fonts-with-qualifiers)
          (let ((font (replace-regexp-in-string ":.*\\'" "" qualified-font)))
            (when (setq dupes (unicode-fonts-debug-check-duplicate-fonts qualified-font all-fonts-with-qualifiers))
              (funcall message-function "ERROR: font occurs at least twice in block: %s" dupes))
            (let ((plist (cdr (assoc-string font unicode-fonts-known-font-characteristics))))
              (if plist
                  (setq licenses (append licenses (plist-get plist :licenses)))
                (funcall message-function "ERROR: Font %s is not listed" font)))))
        (unless (memq 'microsoft licenses)
          (funcall message-function "No Microsoft font for block %s" block-name))
        (unless (memq 'free licenses)
          (funcall message-function "No Free font for block %s" block-name))
        (unless (memq 'apple licenses)
          (funcall message-function "No Apple font for block %s" block-name))
        (unless existing-fonts
          (funcall message-function "No displayable font on this system for %s" block-name))
        (unless best-font
          (funcall message-function "No acceptable font on this system for %s" block-name))))
    (progress-reporter-done reporter)

    ;; overrides
    (setq reporter (make-progress-reporter "Checking overrides ... " 0 (length unicode-fonts-overrides-mapping)))
    (setq counter 0)
    (dolist (cell unicode-fonts-overrides-mapping)
      (progress-reporter-update reporter (incf counter))
      (let* ((char-range (unicode-fonts--create-char-range (list (car cell) (cadr cell))))
             (all-fonts (mapcar #'(lambda (x) (replace-regexp-in-string ":.*\\'" "" x)) (car (last cell))))
             (existing-fonts (remove-if-not 'unicode-fonts-font-exists-p all-fonts))
             (existing-unskipped-fonts (remove-if #'(lambda (x)
                                                      (member* x unicode-fonts-skipped-fonts-computed :test 'font-utils-lenient-name-equal)) existing-fonts))
             (best-font (pop existing-unskipped-fonts))
             (licenses nil))
        (funcall message-function "\n-----\nOverride %s\n-----" (list (car cell) (cadr cell)))
        (if (not char-range)
            (funcall message-function "ERROR: invalid character range")
          ;; else
          (when (not (eq (ucs-utils-char (car cell) nil) (car char-range)))
            (funcall message-function "Warning: character range out of order"))
          (dolist (old-range all-override-ranges)
            (when (and (>= (car char-range) (car old-range))
                       (<= (car char-range) (cadr old-range)))
              (funcall message-function "ERROR: first element overlaps with another range"))
            (when (and (>= (cadr char-range) (car old-range))
                       (<= (cadr char-range) (cadr old-range)))
              (funcall message-function "ERROR: last element overlaps with another range")))
          (push char-range all-override-ranges)
          (dolist (font all-fonts)
            (when (setq dupes (unicode-fonts-debug-check-duplicate-fonts font all-fonts))
              (funcall message-function "ERROR: font occurs at least twice in override: %s" dupes))
            (let ((plist (cdr (assoc-string font unicode-fonts-known-font-characteristics))))
              (if plist
                  (setq licenses (append licenses (plist-get plist :licenses)))
                (funcall message-function "ERROR: Font %s is not listed" font))))
          ;; todo here check licenses for coverage as above
          )))
    (progress-reporter-done reporter)))

;;; driver for setup

(defun unicode-fonts--setup-1 (fontset-name)
  "Driver for `unicode-fonts-setup'.

FONTSET-NAME is a fontset to modify using `set-fontset-font'."
  (when (display-multi-font-p)

    (let ((reporter nil)
          (counter 0))

      ;; debug font availability
      (when unicode-fonts-debug-availability
        (let ((all-fonts nil)
              (current-msg (current-message))
              (message-log-max most-positive-fixnum))
          (dolist (cell unicode-fonts-block-font-mapping)
            (callf append all-fonts (cadr cell)))
          (delete-dups all-fonts)
          (setq reporter (make-progress-reporter "Unicode Fonts - Debugging Fonts ... " 0 (length all-fonts)))
          (setq counter 0)
          (dolist (font all-fonts)
            (progress-reporter-update reporter (incf counter))
            (unless (unicode-fonts-font-exists-p font)
              (message "Unicode-fonts: font not found: %s" font)))
          (message current-msg))
        (progress-reporter-done reporter))

      ;; first, install fallback mapping
      (let* ((fonts (remove-if #'(lambda (x)
                                   (member* x unicode-fonts-skipped-fonts-computed
                                            :test 'font-utils-lenient-name-equal))
                               unicode-fonts-fallback-font-list))
             (best-font nil))
        (cond
          ((eq unicode-fonts-existence-checks 'none)
           t)
          ((eq unicode-fonts-existence-checks 'first)
           (setq fonts (list (unicode-fonts-first-existing-font fonts))))
          (t    ; 'all
           (setq fonts (remove-if-not 'unicode-fonts-font-exists-p fonts))))
        (setq best-font (pop fonts))
        (when best-font
          (set-fontset-font fontset-name nil best-font))
        (dolist (lesser-font fonts)
          (set-fontset-font fontset-name nil lesser-font nil 'append)))

      ;; next, install mappings by unicode block
      ;; this is slow the first time through, because of unicode-fonts-font-exists-p
      (unless unicode-fonts-less-feedback
        (setq reporter (make-progress-reporter (format "Unicode Fonts - Mapping Unicode Blocks in %s ... " fontset-name) 0 (length unicode-fonts-block-font-mapping)))
        (setq counter 0))
      (dolist (cell unicode-fonts-block-font-mapping)
        (unless unicode-fonts-less-feedback
          (progress-reporter-update reporter (incf counter)))
        (let* ((block-name (car cell))
               (char-range (cdr (assoc-string block-name unicode-fonts-blocks 'case-fold)))
               (fonts (remove-if #'(lambda (x)
                                     (member* x unicode-fonts-skipped-fonts-computed
                                              :test 'font-utils-lenient-name-equal))
                                 (cadr cell)))
               (best-font nil))
        (when char-range
          (cond
            ((eq unicode-fonts-existence-checks 'none)
             t)
            ((eq unicode-fonts-existence-checks 'first)
             (setq fonts (list (unicode-fonts-first-existing-font fonts))))
            (t    ; 'all
             (setq fonts (remove-if-not 'unicode-fonts-font-exists-p fonts))))
          (setq best-font (pop fonts))
          (when best-font
            (set-fontset-font fontset-name
                              (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                              (cons best-font "iso10646-1")))
          (dolist (lesser-font fonts)
            (set-fontset-font fontset-name
                              (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                              (cons lesser-font "iso10646-1") nil 'append)))))
      (unless unicode-fonts-less-feedback
        (progress-reporter-done reporter))

      ;; finally, install "overrides", which are mappings over an arbitrary range
      (unless unicode-fonts-ignore-overrides
        (unless unicode-fonts-less-feedback
          (setq reporter (make-progress-reporter (format "Unicode Fonts - Mapping Overrides in %s ... " fontset-name) 0 (length unicode-fonts-overrides-mapping)))
          (setq counter 0))
        (dolist (cell unicode-fonts-overrides-mapping)
          (unless unicode-fonts-less-feedback
            (progress-reporter-update reporter (incf counter)))
          (let* ((char-range (unicode-fonts--create-char-range (list (car cell) (cadr cell))))
                 (fonts (remove-if #'(lambda (x)
                                       (member* x unicode-fonts-skipped-fonts-computed
                                                :test 'font-utils-lenient-name-equal))
                                   (car (last cell))))
                 (best-font nil))
            (when char-range
              (cond
                ((eq unicode-fonts-existence-checks 'none)
                 t)
                ((eq unicode-fonts-existence-checks 'first)
                 (setq fonts (list (unicode-fonts-first-existing-font fonts))))
                (t    ; 'all
                 (setq fonts (remove-if-not 'unicode-fonts-font-exists-p fonts))))
              (if unicode-fonts-use-prepend
                  (dolist (font (reverse fonts))
                    (set-fontset-font fontset-name
                                      (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                                      (cons font "iso10646-1") nil 'prepend))
                ;; else
                (setq best-font (pop fonts))
                (when best-font
                  (set-fontset-font fontset-name
                                    (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                                    (cons best-font "iso10646-1")))
                  (dolist (font fonts)
                    (set-fontset-font fontset-name
                                      (cons (decode-char 'ucs (car char-range)) (decode-char 'ucs (cadr char-range)))
                                      (cons font "iso10646-1") nil 'append))))))
            (unless unicode-fonts-less-feedback
              (progress-reporter-done reporter))))))

;;; main entry point

;;;###autoload
(defun unicode-fonts-setup (&optional fontset-names)
  "Set up Unicode fonts for FONTSET-NAMES.

FONTSET-NAMES must be a list of strings.  Fontset names
which do not currently exist will be ignored.  The
default value is `unicode-fonts-fontset-names'."
  (interactive)
  (unicode-fonts-compute-skipped-fonts)
  (callf or fontset-names unicode-fonts-fontset-names)
  (dolist (fontset-name (remove-if-not #'(lambda (fs) (ignore-errors (fontset-info fs))) fontset-names))
    ;; Cocoa Emacs often crashes if this is run more than once for a fontset
    (unless (member fontset-name unicode-fonts-setup-done)
      (push fontset-name unicode-fonts-setup-done)
      (if (and (memq window-system '(ns))
               (not after-init-time))
          ;; Cocoa Emacs crashes unless this is deferred.  set-language-environment-hook
          ;; seems more logical than after-init-hook, but s-l-h appears to have already happened.
          (add-hook 'after-init-hook `(lambda () (unicode-fonts--setup-1 ,fontset-name)))
        (unicode-fonts--setup-1 fontset-name)))))

(provide 'unicode-fonts)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: cleartype Consolas Ethiopic Samyak BabelStone Symbola
;; LocalWords: Quivira DejaVu UnicodeFonts utils Unifont charset Cham
;; LocalWords: Akkadian Analecta Musica Doulos WenQuanYi Saysettha
;; LocalWords: Eeyek Akshar Mukti Kedage AnmolUni Padauk Lanna Banna
;; LocalWords: Koodak Ahuramzda Abyssinica Estrangelo Nisibin fontset
;; LocalWords: Wawati Weibei Alchemical Avestan Bamum Batak Bopomofo
;; LocalWords: Brahmi Buginese Buhid Carian Chakma Deseret Gurmukhi
;; LocalWords: Halfwidth Fullwidth Jamo Hanunoo Parthian Kaithi Kana
;; LocalWords: Kanbun Kangxi Katakana Kharoshthi Lepcha Limbu Mandaic
;; LocalWords: Miao Chiki Osmanya Phags Rejang Rumi Saurashtra Tham
;; LocalWords: Sharada Tagbanwa Jing Xuan Takri Thaana Yijing Damase
;; LocalWords: Gentium Batang Hana Nuosu Daicing Xiaokai Jomolhari
;; LocalWords: Alif Andale callf incf Kayah Lisu Sora Sompeng Syloti
;; LocalWords: Nagri Syllabics Arial glyphs Meetei Mayek Naskh Tahoma
;; LocalWords: ClearType Mshtakan Sylfaen Cambria Lucida Grande Yogh
;; LocalWords: Projective Sheqel
;;

;;; unicode-fonts.el ends here

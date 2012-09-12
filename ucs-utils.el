;;; ucs-utils.el --- Utilities for Unicode characters
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/ucs-utils
;; URL: http://raw.github.com/rolandwalker/ucs-utils/master/ucs-utils.el
;; Version: 0.7.0
;; Last-Updated: 6 Sep 2012
;; EmacsWiki: UcsUtils
;; Package-Requires: ((persistent-soft "0.8.0") (pcache "0.2.3"))
;; Keywords: i18n, extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; This library provides some utilities for manipulating Unicode
;; characters.  There are three interactive commands
;;
;;    `ucs-utils-ucs-insert'       ; `ucs-insert' workalike using ido
;;    `ucs-utils-eval'             ; the inverse of `ucs-insert'
;;    `ucs-utils-install-aliases'  ; install shorter aliases
;;
;; The other functions are only useful from other Lisp code:
;;
;;    `ucs-utils-char'
;;    `ucs-utils-first-existing-char'
;;    `ucs-utils-vector'
;;    `ucs-utils-string'
;;    `ucs-utils-intact-string'
;;    `ucs-utils-pretty-name'
;;    `ucs-utils-read-char-by-name'
;;    `ucs-utils-subst-char-in-region'
;;
;; To use ucs-utils, place the ucs-utils.el library somewhere Emacs
;; can find it, and add the following to your ~/.emacs file:
;;
;;    (require 'ucs-utils)
;;
;; and optionally
;;
;;    (ucs-install-aliases)
;;
;; See Also
;;
;;    M-x customize-group RET ucs-utils RET
;;    http://en.wikipedia.org/wiki/Universal_Character_Set
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;    Tested on GNU Emacs versions 23.3 and 24.1
;;
;;    For full Emacs 23.x support, the library ucs-utils-6.0-delta.el
;;    should also be installed.
;;
;;    Requires persistent-soft.el
;;
;;    Uses if present: memoize.el
;;
;; Bugs
;;
;; TODO
;;
;;    Accept synonyms on inputs? at least Tab would be nice.
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
;;; Code:
;;

;;; requires

;; for callf, callf2, assert, flet, loop, let*
(eval-when-compile
  (require 'cl))

(require 'memoize nil t)

(autoload 'pp                        "pp"              "Output the pretty-printed representation of OBJECT, any Lisp object.")
(autoload 'pp-display-expression     "pp"              "Prettify and display EXPRESSION in an appropriate way, depending on length.")

(autoload 'persistent-soft-store     "persistent-soft" "Under SYMBOL, store VALUE in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-fetch     "persistent-soft" "Return the value for SYMBOL in the LOCATION persistent data store." t)
(autoload 'persistent-soft-exists-p  "persistent-soft" "Return t if SYMBOL exists in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-flush     "persistent-soft" "Flush data for the LOCATION data store to disk."                    t)

(declare-function gensym                           "cl-macs.el")
(declare-function memoize                          "memoize.el")
(declare-function memoize-wrap                     "memoize.el")
(declare-function ucs-utils-orig-read-char-by-name "ucs-utils.el")

;;; customizable variables

;;;###autoload
(defgroup ucs-utils nil
  "Utilities for Unicode characters."
  :version "0.7.0"
  :link '(emacs-commentary-link "ucs-utils")
  :prefix "ucs-utils-"
  :group 'i18n
  :group 'extensions)

(defcustom ucs-utils-trade-memory-for-speed t
  "At the cost of memory, speed up checks for UCS characters.

The speedup is greater than an order of magnitude.  The memory
cost is roughly 3 Megabytes."
  :type 'boolean
  :group 'ucs-utils)

(defcustom ucs-utils-use-persistent-storage "ucs-utils"
  "Use persistent disk storage when available.

This speeds some operations between sessions.

Internally, this value is a string which is used for the filename
of the persistent data store."
  :type '(choice
          (const :tag "Yes"  "ucs-utils")
          (const :tag "No"   nil))
  :group 'ucs-utils)

;;; variables

(defvar ucs-utils-names-hash nil "A hashed copy of the `ucs-names' alist.")
(defvar ucs-utils-all-prettified-names nil "List of all UCS names, prettified.")
(defvar persistent-soft-storage-expiration (* 60 60 24 30) "Number of seconds to keep on-disk storage.")

(defvar ucs-utils-names-corrections
  '(
    ;; ambiguities
    ("LATIN CAPITAL LETTER YOGH"                                 . #x021C)
    ("LATIN SMALL LETTER YOGH"                                   . #x021D)
    ("CYRILLIC CAPITAL LETTER E"                                 . #x042D)
    ("CYRILLIC CAPITAL LETTER I"                                 . #x0418)
    ("CYRILLIC SMALL LETTER I"                                   . #x0438)
    ("CYRILLIC SMALL LETTER E"                                   . #x044D)
    ("TIBETAN LETTER -A"                                         . #x0F60)
    ("TIBETAN SUBJOINED LETTER -A"                               . #x0FB0)
    ("GEORGIAN SMALL LETTER AN"                                  . #x2D00)
    ("GEORGIAN SMALL LETTER BAN"                                 . #x2D01)
    ("GEORGIAN SMALL LETTER GAN"                                 . #x2D02)
    ("GEORGIAN SMALL LETTER DON"                                 . #x2D03)
    ("GEORGIAN SMALL LETTER EN"                                  . #x2D04)
    ("GEORGIAN SMALL LETTER VIN"                                 . #x2D05)
    ("GEORGIAN SMALL LETTER ZEN"                                 . #x2D06)
    ("GEORGIAN SMALL LETTER TAN"                                 . #x2D07)
    ("GEORGIAN SMALL LETTER IN"                                  . #x2D08)
    ("GEORGIAN SMALL LETTER KAN"                                 . #x2D09)
    ("GEORGIAN SMALL LETTER LAS"                                 . #x2D0A)
    ("GEORGIAN SMALL LETTER MAN"                                 . #x2D0B)
    ("GEORGIAN SMALL LETTER NAR"                                 . #x2D0C)
    ("GEORGIAN SMALL LETTER ON"                                  . #x2D0D)
    ("GEORGIAN SMALL LETTER PAR"                                 . #x2D0E)
    ("GEORGIAN SMALL LETTER ZHAR"                                . #x2D0F)
    ("GEORGIAN SMALL LETTER RAE"                                 . #x2D10)
    ("GEORGIAN SMALL LETTER SAN"                                 . #x2D11)
    ("GEORGIAN SMALL LETTER TAR"                                 . #x2D12)
    ("GEORGIAN SMALL LETTER UN"                                  . #x2D13)
    ("GEORGIAN SMALL LETTER PHAR"                                . #x2D14)
    ("GEORGIAN SMALL LETTER KHAR"                                . #x2D15)
    ("GEORGIAN SMALL LETTER GHAN"                                . #x2D16)
    ("GEORGIAN SMALL LETTER QAR"                                 . #x2D17)
    ("GEORGIAN SMALL LETTER SHIN"                                . #x2D18)
    ("GEORGIAN SMALL LETTER CHIN"                                . #x2D19)
    ("GEORGIAN SMALL LETTER CAN"                                 . #x2D1A)
    ("GEORGIAN SMALL LETTER JIL"                                 . #x2D1B)
    ("GEORGIAN SMALL LETTER CIL"                                 . #x2D1C)
    ("GEORGIAN SMALL LETTER CHAR"                                . #x2D1D)
    ("GEORGIAN SMALL LETTER XAN"                                 . #x2D1E)
    ("GEORGIAN SMALL LETTER JHAN"                                . #x2D1F)
    ("GEORGIAN SMALL LETTER HAE"                                 . #x2D20)
    ("GEORGIAN SMALL LETTER HE"                                  . #x2D21)
    ("GEORGIAN SMALL LETTER HIE"                                 . #x2D22)
    ("GEORGIAN SMALL LETTER WE"                                  . #x2D23)
    ("GEORGIAN SMALL LETTER HAR"                                 . #x2D24)
    ("GEORGIAN SMALL LETTER HOE"                                 . #x2D25)
    ("HANGUL LETTER KIYEOK"                                      . #x3131)
    ("HANGUL LETTER PIEUP"                                       . #x3142)
    ("HANGUL LETTER CIEUC"                                       . #x3148)
    ("PARENTHESIZED HANGUL KIYEOK"                               . #x3200)
    ("PARENTHESIZED HANGUL PIEUP"                                . #x3205)
    ("PARENTHESIZED HANGUL CIEUC"                                . #x3208)
    ("CIRCLED HANGUL KIYEOK"                                     . #x3260)
    ("CIRCLED HANGUL PIEUP"                                      . #x3265)
    ("CIRCLED HANGUL CIEUC"                                      . #x3268)
    ("HALFWIDTH HANGUL LETTER KIYEOK"                            . #xFFA1)
    ("HALFWIDTH HANGUL LETTER PIEUP"                             . #xFFB2)
    ("HALFWIDTH HANGUL LETTER CIEUC"                             . #xFFB8)
    ("SQUARED MV"                                                . #x1F14B)
    ("BELL"                                                      . #x1F514)
    ;; Unicode 6.0 to 6.1 delta
    ("ARMENIAN DRAM SIGN"                                        . #x058F)
    ("ARABIC SIGN SAMVAT"                                        . #x0604)
    ("ARABIC LETTER BEH WITH SMALL V BELOW"                      . #x08A0)
    ("ARABIC LETTER JEEM WITH TWO DOTS ABOVE"                    . #x08A2)
    ("ARABIC LETTER TAH WITH TWO DOTS ABOVE"                     . #x08A3)
    ("ARABIC LETTER FEH WITH DOT BELOW AND THREE DOTS ABOVE"     . #x08A4)
    ("ARABIC LETTER QAF WITH DOT BELOW"                          . #x08A5)
    ("ARABIC LETTER LAM WITH DOUBLE BAR"                         . #x08A6)
    ("ARABIC LETTER MEEM WITH THREE DOTS ABOVE"                  . #x08A7)
    ("ARABIC LETTER YEH WITH TWO DOTS BELOW AND HAMZA ABOVE"     . #x08A8)
    ("ARABIC LETTER YEH WITH TWO DOTS BELOW AND DOT ABOVE"       . #x08A9)
    ("ARABIC LETTER REH WITH LOOP"                               . #x08AA)
    ("ARABIC LETTER WAW WITH DOT WITHIN"                         . #x08AB)
    ("ARABIC LETTER ROHINGYA YEH"                                . #x08AC)
    ("ARABIC CURLY FATHA"                                        . #x08E4)
    ("ARABIC CURLY DAMMA"                                        . #x08E5)
    ("ARABIC CURLY KASRA"                                        . #x08E6)
    ("ARABIC CURLY FATHATAN"                                     . #x08E7)
    ("ARABIC CURLY DAMMATAN"                                     . #x08E8)
    ("ARABIC CURLY KASRATAN"                                     . #x08E9)
    ("ARABIC TONE ONE DOT ABOVE"                                 . #x08EA)
    ("ARABIC TONE TWO DOTS ABOVE"                                . #x08EB)
    ("ARABIC TONE LOOP ABOVE"                                    . #x08EC)
    ("ARABIC TONE ONE DOT BELOW"                                 . #x08ED)
    ("ARABIC TONE TWO DOTS BELOW"                                . #x08EE)
    ("ARABIC TONE LOOP BELOW"                                    . #x08EF)
    ("ARABIC OPEN FATHATAN"                                      . #x08F0)
    ("ARABIC OPEN DAMMATAN"                                      . #x08F1)
    ("ARABIC OPEN KASRATAN"                                      . #x08F2)
    ("ARABIC SMALL HIGH WAW"                                     . #x08F3)
    ("ARABIC FATHA WITH RING"                                    . #x08F4)
    ("ARABIC FATHA WITH DOT ABOVE"                               . #x08F5)
    ("ARABIC KASRA WITH DOT BELOW"                               . #x08F6)
    ("ARABIC LEFT ARROWHEAD ABOVE"                               . #x08F7)
    ("ARABIC RIGHT ARROWHEAD ABOVE"                              . #x08F8)
    ("ARABIC LEFT ARROWHEAD BELOW"                               . #x08F9)
    ("ARABIC RIGHT ARROWHEAD BELOW"                              . #x08FA)
    ("ARABIC DOUBLE RIGHT ARROWHEAD ABOVE"                       . #x08FB)
    ("ARABIC DOUBLE RIGHT ARROWHEAD ABOVE WITH DOT"              . #x08FC)
    ("ARABIC RIGHT ARROWHEAD ABOVE WITH DOT"                     . #x08FD)
    ("ARABIC DAMMA WITH DOT"                                     . #x08FE)
    ("GUJARATI ABBREVIATION SIGN"                                . #x0AF0)
    ("LAO LETTER KHMU GO"                                        . #x0EDE)
    ("LAO LETTER KHMU NYO"                                       . #x0EDF)
    ("GEORGIAN CAPITAL LETTER YN"                                . #x10C7)
    ("GEORGIAN CAPITAL LETTER AEN"                               . #x10CD)
    ("GEORGIAN LETTER AEN"                                       . #x10FD)
    ("GEORGIAN LETTER HARD SIGN"                                 . #x10FE)
    ("GEORGIAN LETTER LABIAL SIGN"                               . #x10FF)
    ("SUNDANESE SIGN VIRAMA"                                     . #x1BAB)
    ("SUNDANESE CONSONANT SIGN PASANGAN MA"                      . #x1BAC)
    ("SUNDANESE CONSONANT SIGN PASANGAN WA"                      . #x1BAD)
    ("SUNDANESE AVAGRAHA"                                        . #x1BBA)
    ("SUNDANESE LETTER REU"                                      . #x1BBB)
    ("SUNDANESE LETTER LEU"                                      . #x1BBC)
    ("SUNDANESE LETTER BHA"                                      . #x1BBD)
    ("SUNDANESE LETTER FINAL K"                                  . #x1BBE)
    ("SUNDANESE LETTER FINAL M"                                  . #x1BBF)
    ("SUNDANESE PUNCTUATION BINDU SURYA"                         . #x1CC0)
    ("SUNDANESE PUNCTUATION BINDU PANGLONG"                      . #x1CC1)
    ("SUNDANESE PUNCTUATION BINDU PURNAMA"                       . #x1CC2)
    ("SUNDANESE PUNCTUATION BINDU CAKRA"                         . #x1CC3)
    ("SUNDANESE PUNCTUATION BINDU LEU SATANGA"                   . #x1CC4)
    ("SUNDANESE PUNCTUATION BINDU KA SATANGA"                    . #x1CC5)
    ("SUNDANESE PUNCTUATION BINDU DA SATANGA"                    . #x1CC6)
    ("SUNDANESE PUNCTUATION BINDU BA SATANGA"                    . #x1CC7)
    ("VEDIC SIGN ROTATED ARDHAVISARGA"                           . #x1CF3)
    ("VEDIC TONE CANDRA ABOVE"                                   . #x1CF4)
    ("VEDIC SIGN JIHVAMULIYA"                                    . #x1CF5)
    ("VEDIC SIGN UPADHMANIYA"                                    . #x1CF6)
    ("MATHEMATICAL RISING DIAGONAL"                              . #x27CB)
    ("MATHEMATICAL FALLING DIAGONAL"                             . #x27CD)
    ("COPTIC CAPITAL LETTER BOHAIRIC KHEI"                       . #x2CF2)
    ("COPTIC SMALL LETTER BOHAIRIC KHEI"                         . #x2CF3)
    ("GEORGIAN SMALL LETTER YN"                                  . #x2D27)
    ("GEORGIAN SMALL LETTER AEN"                                 . #x2D2D)
    ("TIFINAGH LETTER YE"                                        . #x2D66)
    ("TIFINAGH LETTER YO"                                        . #x2D67)
    ("TURNED COMMA"                                              . #x2E32)
    ("RAISED DOT"                                                . #x2E33)
    ("RAISED COMMA"                                              . #x2E34)
    ("TURNED SEMICOLON"                                          . #x2E35)
    ("DAGGER WITH LEFT GUARD"                                    . #x2E36)
    ("DAGGER WITH RIGHT GUARD"                                   . #x2E37)
    ("TURNED DAGGER"                                             . #x2E38)
    ("TOP HALF SECTION SIGN"                                     . #x2E39)
    ("TWO-EM DASH"                                               . #x2E3A)
    ("THREE-EM DASH"                                             . #x2E3B)
    ("COMBINING CYRILLIC LETTER UKRAINIAN IE"                    . #xA674)
    ("COMBINING CYRILLIC LETTER I"                               . #xA675)
    ("COMBINING CYRILLIC LETTER YI"                              . #xA676)
    ("COMBINING CYRILLIC LETTER U"                               . #xA677)
    ("COMBINING CYRILLIC LETTER HARD SIGN"                       . #xA678)
    ("COMBINING CYRILLIC LETTER YERU"                            . #xA679)
    ("COMBINING CYRILLIC LETTER SOFT SIGN"                       . #xA67A)
    ("COMBINING CYRILLIC LETTER OMEGA"                           . #xA67B)
    ("COMBINING CYRILLIC LETTER IOTIFIED E"                      . #xA69F)
    ("LATIN CAPITAL LETTER C WITH BAR"                           . #xA792)
    ("LATIN SMALL LETTER C WITH BAR"                             . #xA793)
    ("LATIN CAPITAL LETTER H WITH HOOK"                          . #xA7AA)
    ("MODIFIER LETTER CAPITAL H WITH STROKE"                     . #xA7F8)
    ("MODIFIER LETTER SMALL LIGATURE OE"                         . #xA7F9)
    ("MEETEI MAYEK LETTER E"                                     . #xAAE0)
    ("MEETEI MAYEK LETTER O"                                     . #xAAE1)
    ("MEETEI MAYEK LETTER CHA"                                   . #xAAE2)
    ("MEETEI MAYEK LETTER NYA"                                   . #xAAE3)
    ("MEETEI MAYEK LETTER TTA"                                   . #xAAE4)
    ("MEETEI MAYEK LETTER TTHA"                                  . #xAAE5)
    ("MEETEI MAYEK LETTER DDA"                                   . #xAAE6)
    ("MEETEI MAYEK LETTER DDHA"                                  . #xAAE7)
    ("MEETEI MAYEK LETTER NNA"                                   . #xAAE8)
    ("MEETEI MAYEK LETTER SHA"                                   . #xAAE9)
    ("MEETEI MAYEK LETTER SSA"                                   . #xAAEA)
    ("MEETEI MAYEK VOWEL SIGN II"                                . #xAAEB)
    ("MEETEI MAYEK VOWEL SIGN UU"                                . #xAAEC)
    ("MEETEI MAYEK VOWEL SIGN AAI"                               . #xAAED)
    ("MEETEI MAYEK VOWEL SIGN AU"                                . #xAAEE)
    ("MEETEI MAYEK VOWEL SIGN AAU"                               . #xAAEF)
    ("MEETEI MAYEK CHEIKHAN"                                     . #xAAF0)
    ("MEETEI MAYEK AHANG KHUDAM"                                 . #xAAF1)
    ("MEETEI MAYEK ANJI"                                         . #xAAF2)
    ("MEETEI MAYEK SYLLABLE REPETITION MARK"                     . #xAAF3)
    ("MEETEI MAYEK WORD REPETITION MARK"                         . #xAAF4)
    ("MEETEI MAYEK VOWEL SIGN VISARGA"                           . #xAAF5)
    ("MEETEI MAYEK VIRAMA"                                       . #xAAF6)
    ("CJK COMPATIBILITY IDEOGRAPH-FA2E"                          . #xFA2E)
    ("CJK COMPATIBILITY IDEOGRAPH-FA2F"                          . #xFA2F)
    ("MEROITIC HIEROGLYPHIC LETTER A"                            . #x10980)
    ("MEROITIC HIEROGLYPHIC LETTER E"                            . #x10981)
    ("MEROITIC HIEROGLYPHIC LETTER I"                            . #x10982)
    ("MEROITIC HIEROGLYPHIC LETTER O"                            . #x10983)
    ("MEROITIC HIEROGLYPHIC LETTER YA"                           . #x10984)
    ("MEROITIC HIEROGLYPHIC LETTER WA"                           . #x10985)
    ("MEROITIC HIEROGLYPHIC LETTER BA"                           . #x10986)
    ("MEROITIC HIEROGLYPHIC LETTER BA-2"                         . #x10987)
    ("MEROITIC HIEROGLYPHIC LETTER PA"                           . #x10988)
    ("MEROITIC HIEROGLYPHIC LETTER MA"                           . #x10989)
    ("MEROITIC HIEROGLYPHIC LETTER NA"                           . #x1098A)
    ("MEROITIC HIEROGLYPHIC LETTER NA-2"                         . #x1098B)
    ("MEROITIC HIEROGLYPHIC LETTER NE"                           . #x1098C)
    ("MEROITIC HIEROGLYPHIC LETTER NE-2"                         . #x1098D)
    ("MEROITIC HIEROGLYPHIC LETTER RA"                           . #x1098E)
    ("MEROITIC HIEROGLYPHIC LETTER RA-2"                         . #x1098F)
    ("MEROITIC HIEROGLYPHIC LETTER LA"                           . #x10990)
    ("MEROITIC HIEROGLYPHIC LETTER KHA"                          . #x10991)
    ("MEROITIC HIEROGLYPHIC LETTER HHA"                          . #x10992)
    ("MEROITIC HIEROGLYPHIC LETTER SA"                           . #x10993)
    ("MEROITIC HIEROGLYPHIC LETTER SA-2"                         . #x10994)
    ("MEROITIC HIEROGLYPHIC LETTER SE"                           . #x10995)
    ("MEROITIC HIEROGLYPHIC LETTER KA"                           . #x10996)
    ("MEROITIC HIEROGLYPHIC LETTER QA"                           . #x10997)
    ("MEROITIC HIEROGLYPHIC LETTER TA"                           . #x10998)
    ("MEROITIC HIEROGLYPHIC LETTER TA-2"                         . #x10999)
    ("MEROITIC HIEROGLYPHIC LETTER TE"                           . #x1099A)
    ("MEROITIC HIEROGLYPHIC LETTER TE-2"                         . #x1099B)
    ("MEROITIC HIEROGLYPHIC LETTER TO"                           . #x1099C)
    ("MEROITIC HIEROGLYPHIC LETTER DA"                           . #x1099D)
    ("MEROITIC HIEROGLYPHIC SYMBOL VIDJ"                         . #x1099E)
    ("MEROITIC HIEROGLYPHIC SYMBOL VIDJ-2"                       . #x1099F)
    ("MEROITIC CURSIVE LETTER A"                                 . #x109A0)
    ("MEROITIC CURSIVE LETTER E"                                 . #x109A1)
    ("MEROITIC CURSIVE LETTER I"                                 . #x109A2)
    ("MEROITIC CURSIVE LETTER O"                                 . #x109A3)
    ("MEROITIC CURSIVE LETTER YA"                                . #x109A4)
    ("MEROITIC CURSIVE LETTER WA"                                . #x109A5)
    ("MEROITIC CURSIVE LETTER BA"                                . #x109A6)
    ("MEROITIC CURSIVE LETTER PA"                                . #x109A7)
    ("MEROITIC CURSIVE LETTER MA"                                . #x109A8)
    ("MEROITIC CURSIVE LETTER NA"                                . #x109A9)
    ("MEROITIC CURSIVE LETTER NE"                                . #x109AA)
    ("MEROITIC CURSIVE LETTER RA"                                . #x109AB)
    ("MEROITIC CURSIVE LETTER LA"                                . #x109AC)
    ("MEROITIC CURSIVE LETTER KHA"                               . #x109AD)
    ("MEROITIC CURSIVE LETTER HHA"                               . #x109AE)
    ("MEROITIC CURSIVE LETTER SA"                                . #x109AF)
    ("MEROITIC CURSIVE LETTER ARCHAIC SA"                        . #x109B0)
    ("MEROITIC CURSIVE LETTER SE"                                . #x109B1)
    ("MEROITIC CURSIVE LETTER KA"                                . #x109B2)
    ("MEROITIC CURSIVE LETTER QA"                                . #x109B3)
    ("MEROITIC CURSIVE LETTER TA"                                . #x109B4)
    ("MEROITIC CURSIVE LETTER TE"                                . #x109B5)
    ("MEROITIC CURSIVE LETTER TO"                                . #x109B6)
    ("MEROITIC CURSIVE LETTER DA"                                . #x109B7)
    ("MEROITIC CURSIVE LOGOGRAM RMT"                             . #x109BE)
    ("MEROITIC CURSIVE LOGOGRAM IMN"                             . #x109BF)
    ("SORA SOMPENG LETTER SAH"                                   . #x110D0)
    ("SORA SOMPENG LETTER TAH"                                   . #x110D1)
    ("SORA SOMPENG LETTER BAH"                                   . #x110D2)
    ("SORA SOMPENG LETTER CAH"                                   . #x110D3)
    ("SORA SOMPENG LETTER DAH"                                   . #x110D4)
    ("SORA SOMPENG LETTER GAH"                                   . #x110D5)
    ("SORA SOMPENG LETTER MAH"                                   . #x110D6)
    ("SORA SOMPENG LETTER NGAH"                                  . #x110D7)
    ("SORA SOMPENG LETTER LAH"                                   . #x110D8)
    ("SORA SOMPENG LETTER NAH"                                   . #x110D9)
    ("SORA SOMPENG LETTER VAH"                                   . #x110DA)
    ("SORA SOMPENG LETTER PAH"                                   . #x110DB)
    ("SORA SOMPENG LETTER YAH"                                   . #x110DC)
    ("SORA SOMPENG LETTER RAH"                                   . #x110DD)
    ("SORA SOMPENG LETTER HAH"                                   . #x110DE)
    ("SORA SOMPENG LETTER KAH"                                   . #x110DF)
    ("SORA SOMPENG LETTER JAH"                                   . #x110E0)
    ("SORA SOMPENG LETTER NYAH"                                  . #x110E1)
    ("SORA SOMPENG LETTER AH"                                    . #x110E2)
    ("SORA SOMPENG LETTER EEH"                                   . #x110E3)
    ("SORA SOMPENG LETTER IH"                                    . #x110E4)
    ("SORA SOMPENG LETTER UH"                                    . #x110E5)
    ("SORA SOMPENG LETTER OH"                                    . #x110E6)
    ("SORA SOMPENG LETTER EH"                                    . #x110E7)
    ("SORA SOMPENG LETTER MAE"                                   . #x110E8)
    ("SORA SOMPENG DIGIT ZERO"                                   . #x110F0)
    ("SORA SOMPENG DIGIT ONE"                                    . #x110F1)
    ("SORA SOMPENG DIGIT TWO"                                    . #x110F2)
    ("SORA SOMPENG DIGIT THREE"                                  . #x110F3)
    ("SORA SOMPENG DIGIT FOUR"                                   . #x110F4)
    ("SORA SOMPENG DIGIT FIVE"                                   . #x110F5)
    ("SORA SOMPENG DIGIT SIX"                                    . #x110F6)
    ("SORA SOMPENG DIGIT SEVEN"                                  . #x110F7)
    ("SORA SOMPENG DIGIT EIGHT"                                  . #x110F8)
    ("SORA SOMPENG DIGIT NINE"                                   . #x110F9)
    ("CHAKMA SIGN CANDRABINDU"                                   . #x11100)
    ("CHAKMA SIGN ANUSVARA"                                      . #x11101)
    ("CHAKMA SIGN VISARGA"                                       . #x11102)
    ("CHAKMA LETTER AA"                                          . #x11103)
    ("CHAKMA LETTER I"                                           . #x11104)
    ("CHAKMA LETTER U"                                           . #x11105)
    ("CHAKMA LETTER E"                                           . #x11106)
    ("CHAKMA LETTER KAA"                                         . #x11107)
    ("CHAKMA LETTER KHAA"                                        . #x11108)
    ("CHAKMA LETTER GAA"                                         . #x11109)
    ("CHAKMA LETTER GHAA"                                        . #x1110A)
    ("CHAKMA LETTER NGAA"                                        . #x1110B)
    ("CHAKMA LETTER CAA"                                         . #x1110C)
    ("CHAKMA LETTER CHAA"                                        . #x1110D)
    ("CHAKMA LETTER JAA"                                         . #x1110E)
    ("CHAKMA LETTER JHAA"                                        . #x1110F)
    ("CHAKMA LETTER NYAA"                                        . #x11110)
    ("CHAKMA LETTER TTAA"                                        . #x11111)
    ("CHAKMA LETTER TTHAA"                                       . #x11112)
    ("CHAKMA LETTER DDAA"                                        . #x11113)
    ("CHAKMA LETTER DDHAA"                                       . #x11114)
    ("CHAKMA LETTER NNAA"                                        . #x11115)
    ("CHAKMA LETTER TAA"                                         . #x11116)
    ("CHAKMA LETTER THAA"                                        . #x11117)
    ("CHAKMA LETTER DAA"                                         . #x11118)
    ("CHAKMA LETTER DHAA"                                        . #x11119)
    ("CHAKMA LETTER NAA"                                         . #x1111A)
    ("CHAKMA LETTER PAA"                                         . #x1111B)
    ("CHAKMA LETTER PHAA"                                        . #x1111C)
    ("CHAKMA LETTER BAA"                                         . #x1111D)
    ("CHAKMA LETTER BHAA"                                        . #x1111E)
    ("CHAKMA LETTER MAA"                                         . #x1111F)
    ("CHAKMA LETTER YYAA"                                        . #x11120)
    ("CHAKMA LETTER YAA"                                         . #x11121)
    ("CHAKMA LETTER RAA"                                         . #x11122)
    ("CHAKMA LETTER LAA"                                         . #x11123)
    ("CHAKMA LETTER WAA"                                         . #x11124)
    ("CHAKMA LETTER SAA"                                         . #x11125)
    ("CHAKMA LETTER HAA"                                         . #x11126)
    ("CHAKMA VOWEL SIGN A"                                       . #x11127)
    ("CHAKMA VOWEL SIGN I"                                       . #x11128)
    ("CHAKMA VOWEL SIGN II"                                      . #x11129)
    ("CHAKMA VOWEL SIGN U"                                       . #x1112A)
    ("CHAKMA VOWEL SIGN UU"                                      . #x1112B)
    ("CHAKMA VOWEL SIGN E"                                       . #x1112C)
    ("CHAKMA VOWEL SIGN AI"                                      . #x1112D)
    ("CHAKMA VOWEL SIGN O"                                       . #x1112E)
    ("CHAKMA VOWEL SIGN AU"                                      . #x1112F)
    ("CHAKMA VOWEL SIGN OI"                                      . #x11130)
    ("CHAKMA O MARK"                                             . #x11131)
    ("CHAKMA AU MARK"                                            . #x11132)
    ("CHAKMA VIRAMA"                                             . #x11133)
    ("CHAKMA MAAYYAA"                                            . #x11134)
    ("CHAKMA DIGIT ZERO"                                         . #x11136)
    ("CHAKMA DIGIT ONE"                                          . #x11137)
    ("CHAKMA DIGIT TWO"                                          . #x11138)
    ("CHAKMA DIGIT THREE"                                        . #x11139)
    ("CHAKMA DIGIT FOUR"                                         . #x1113A)
    ("CHAKMA DIGIT FIVE"                                         . #x1113B)
    ("CHAKMA DIGIT SIX"                                          . #x1113C)
    ("CHAKMA DIGIT SEVEN"                                        . #x1113D)
    ("CHAKMA DIGIT EIGHT"                                        . #x1113E)
    ("CHAKMA DIGIT NINE"                                         . #x1113F)
    ("CHAKMA SECTION MARK"                                       . #x11140)
    ("CHAKMA DANDA"                                              . #x11141)
    ("CHAKMA DOUBLE DANDA"                                       . #x11142)
    ("CHAKMA QUESTION MARK"                                      . #x11143)
    ("SHARADA SIGN CANDRABINDU"                                  . #x11180)
    ("SHARADA SIGN ANUSVARA"                                     . #x11181)
    ("SHARADA SIGN VISARGA"                                      . #x11182)
    ("SHARADA LETTER A"                                          . #x11183)
    ("SHARADA LETTER AA"                                         . #x11184)
    ("SHARADA LETTER I"                                          . #x11185)
    ("SHARADA LETTER II"                                         . #x11186)
    ("SHARADA LETTER U"                                          . #x11187)
    ("SHARADA LETTER UU"                                         . #x11188)
    ("SHARADA LETTER VOCALIC R"                                  . #x11189)
    ("SHARADA LETTER VOCALIC RR"                                 . #x1118A)
    ("SHARADA LETTER VOCALIC L"                                  . #x1118B)
    ("SHARADA LETTER VOCALIC LL"                                 . #x1118C)
    ("SHARADA LETTER E"                                          . #x1118D)
    ("SHARADA LETTER AI"                                         . #x1118E)
    ("SHARADA LETTER O"                                          . #x1118F)
    ("SHARADA LETTER AU"                                         . #x11190)
    ("SHARADA LETTER KA"                                         . #x11191)
    ("SHARADA LETTER KHA"                                        . #x11192)
    ("SHARADA LETTER GA"                                         . #x11193)
    ("SHARADA LETTER GHA"                                        . #x11194)
    ("SHARADA LETTER NGA"                                        . #x11195)
    ("SHARADA LETTER CA"                                         . #x11196)
    ("SHARADA LETTER CHA"                                        . #x11197)
    ("SHARADA LETTER JA"                                         . #x11198)
    ("SHARADA LETTER JHA"                                        . #x11199)
    ("SHARADA LETTER NYA"                                        . #x1119A)
    ("SHARADA LETTER TTA"                                        . #x1119B)
    ("SHARADA LETTER TTHA"                                       . #x1119C)
    ("SHARADA LETTER DDA"                                        . #x1119D)
    ("SHARADA LETTER DDHA"                                       . #x1119E)
    ("SHARADA LETTER NNA"                                        . #x1119F)
    ("SHARADA LETTER TA"                                         . #x111A0)
    ("SHARADA LETTER THA"                                        . #x111A1)
    ("SHARADA LETTER DA"                                         . #x111A2)
    ("SHARADA LETTER DHA"                                        . #x111A3)
    ("SHARADA LETTER NA"                                         . #x111A4)
    ("SHARADA LETTER PA"                                         . #x111A5)
    ("SHARADA LETTER PHA"                                        . #x111A6)
    ("SHARADA LETTER BA"                                         . #x111A7)
    ("SHARADA LETTER BHA"                                        . #x111A8)
    ("SHARADA LETTER MA"                                         . #x111A9)
    ("SHARADA LETTER YA"                                         . #x111AA)
    ("SHARADA LETTER RA"                                         . #x111AB)
    ("SHARADA LETTER LA"                                         . #x111AC)
    ("SHARADA LETTER LLA"                                        . #x111AD)
    ("SHARADA LETTER VA"                                         . #x111AE)
    ("SHARADA LETTER SHA"                                        . #x111AF)
    ("SHARADA LETTER SSA"                                        . #x111B0)
    ("SHARADA LETTER SA"                                         . #x111B1)
    ("SHARADA LETTER HA"                                         . #x111B2)
    ("SHARADA VOWEL SIGN AA"                                     . #x111B3)
    ("SHARADA VOWEL SIGN I"                                      . #x111B4)
    ("SHARADA VOWEL SIGN II"                                     . #x111B5)
    ("SHARADA VOWEL SIGN U"                                      . #x111B6)
    ("SHARADA VOWEL SIGN UU"                                     . #x111B7)
    ("SHARADA VOWEL SIGN VOCALIC R"                              . #x111B8)
    ("SHARADA VOWEL SIGN VOCALIC RR"                             . #x111B9)
    ("SHARADA VOWEL SIGN VOCALIC L"                              . #x111BA)
    ("SHARADA VOWEL SIGN VOCALIC LL"                             . #x111BB)
    ("SHARADA VOWEL SIGN E"                                      . #x111BC)
    ("SHARADA VOWEL SIGN AI"                                     . #x111BD)
    ("SHARADA VOWEL SIGN O"                                      . #x111BE)
    ("SHARADA VOWEL SIGN AU"                                     . #x111BF)
    ("SHARADA SIGN VIRAMA"                                       . #x111C0)
    ("SHARADA SIGN AVAGRAHA"                                     . #x111C1)
    ("SHARADA SIGN JIHVAMULIYA"                                  . #x111C2)
    ("SHARADA SIGN UPADHMANIYA"                                  . #x111C3)
    ("SHARADA OM"                                                . #x111C4)
    ("SHARADA DANDA"                                             . #x111C5)
    ("SHARADA DOUBLE DANDA"                                      . #x111C6)
    ("SHARADA ABBREVIATION SIGN"                                 . #x111C7)
    ("SHARADA SEPARATOR"                                         . #x111C8)
    ("SHARADA DIGIT ZERO"                                        . #x111D0)
    ("SHARADA DIGIT ONE"                                         . #x111D1)
    ("SHARADA DIGIT TWO"                                         . #x111D2)
    ("SHARADA DIGIT THREE"                                       . #x111D3)
    ("SHARADA DIGIT FOUR"                                        . #x111D4)
    ("SHARADA DIGIT FIVE"                                        . #x111D5)
    ("SHARADA DIGIT SIX"                                         . #x111D6)
    ("SHARADA DIGIT SEVEN"                                       . #x111D7)
    ("SHARADA DIGIT EIGHT"                                       . #x111D8)
    ("SHARADA DIGIT NINE"                                        . #x111D9)
    ("TAKRI LETTER A"                                            . #x11680)
    ("TAKRI LETTER AA"                                           . #x11681)
    ("TAKRI LETTER I"                                            . #x11682)
    ("TAKRI LETTER II"                                           . #x11683)
    ("TAKRI LETTER U"                                            . #x11684)
    ("TAKRI LETTER UU"                                           . #x11685)
    ("TAKRI LETTER E"                                            . #x11686)
    ("TAKRI LETTER AI"                                           . #x11687)
    ("TAKRI LETTER O"                                            . #x11688)
    ("TAKRI LETTER AU"                                           . #x11689)
    ("TAKRI LETTER KA"                                           . #x1168A)
    ("TAKRI LETTER KHA"                                          . #x1168B)
    ("TAKRI LETTER GA"                                           . #x1168C)
    ("TAKRI LETTER GHA"                                          . #x1168D)
    ("TAKRI LETTER NGA"                                          . #x1168E)
    ("TAKRI LETTER CA"                                           . #x1168F)
    ("TAKRI LETTER CHA"                                          . #x11690)
    ("TAKRI LETTER JA"                                           . #x11691)
    ("TAKRI LETTER JHA"                                          . #x11692)
    ("TAKRI LETTER NYA"                                          . #x11693)
    ("TAKRI LETTER TTA"                                          . #x11694)
    ("TAKRI LETTER TTHA"                                         . #x11695)
    ("TAKRI LETTER DDA"                                          . #x11696)
    ("TAKRI LETTER DDHA"                                         . #x11697)
    ("TAKRI LETTER NNA"                                          . #x11698)
    ("TAKRI LETTER TA"                                           . #x11699)
    ("TAKRI LETTER THA"                                          . #x1169A)
    ("TAKRI LETTER DA"                                           . #x1169B)
    ("TAKRI LETTER DHA"                                          . #x1169C)
    ("TAKRI LETTER NA"                                           . #x1169D)
    ("TAKRI LETTER PA"                                           . #x1169E)
    ("TAKRI LETTER PHA"                                          . #x1169F)
    ("TAKRI LETTER BA"                                           . #x116A0)
    ("TAKRI LETTER BHA"                                          . #x116A1)
    ("TAKRI LETTER MA"                                           . #x116A2)
    ("TAKRI LETTER YA"                                           . #x116A3)
    ("TAKRI LETTER RA"                                           . #x116A4)
    ("TAKRI LETTER LA"                                           . #x116A5)
    ("TAKRI LETTER VA"                                           . #x116A6)
    ("TAKRI LETTER SHA"                                          . #x116A7)
    ("TAKRI LETTER SA"                                           . #x116A8)
    ("TAKRI LETTER HA"                                           . #x116A9)
    ("TAKRI LETTER RRA"                                          . #x116AA)
    ("TAKRI SIGN ANUSVARA"                                       . #x116AB)
    ("TAKRI SIGN VISARGA"                                        . #x116AC)
    ("TAKRI VOWEL SIGN AA"                                       . #x116AD)
    ("TAKRI VOWEL SIGN I"                                        . #x116AE)
    ("TAKRI VOWEL SIGN II"                                       . #x116AF)
    ("TAKRI VOWEL SIGN U"                                        . #x116B0)
    ("TAKRI VOWEL SIGN UU"                                       . #x116B1)
    ("TAKRI VOWEL SIGN E"                                        . #x116B2)
    ("TAKRI VOWEL SIGN AI"                                       . #x116B3)
    ("TAKRI VOWEL SIGN O"                                        . #x116B4)
    ("TAKRI VOWEL SIGN AU"                                       . #x116B5)
    ("TAKRI SIGN VIRAMA"                                         . #x116B6)
    ("TAKRI SIGN NUKTA"                                          . #x116B7)
    ("TAKRI DIGIT ZERO"                                          . #x116C0)
    ("TAKRI DIGIT ONE"                                           . #x116C1)
    ("TAKRI DIGIT TWO"                                           . #x116C2)
    ("TAKRI DIGIT THREE"                                         . #x116C3)
    ("TAKRI DIGIT FOUR"                                          . #x116C4)
    ("TAKRI DIGIT FIVE"                                          . #x116C5)
    ("TAKRI DIGIT SIX"                                           . #x116C6)
    ("TAKRI DIGIT SEVEN"                                         . #x116C7)
    ("TAKRI DIGIT EIGHT"                                         . #x116C8)
    ("TAKRI DIGIT NINE"                                          . #x116C9)
    ("MIAO LETTER PA"                                            . #x16F00)
    ("MIAO LETTER BA"                                            . #x16F01)
    ("MIAO LETTER YI PA"                                         . #x16F02)
    ("MIAO LETTER PLA"                                           . #x16F03)
    ("MIAO LETTER MA"                                            . #x16F04)
    ("MIAO LETTER MHA"                                           . #x16F05)
    ("MIAO LETTER ARCHAIC MA"                                    . #x16F06)
    ("MIAO LETTER FA"                                            . #x16F07)
    ("MIAO LETTER VA"                                            . #x16F08)
    ("MIAO LETTER VFA"                                           . #x16F09)
    ("MIAO LETTER TA"                                            . #x16F0A)
    ("MIAO LETTER DA"                                            . #x16F0B)
    ("MIAO LETTER YI TTA"                                        . #x16F0C)
    ("MIAO LETTER YI TA"                                         . #x16F0D)
    ("MIAO LETTER TTA"                                           . #x16F0E)
    ("MIAO LETTER DDA"                                           . #x16F0F)
    ("MIAO LETTER NA"                                            . #x16F10)
    ("MIAO LETTER NHA"                                           . #x16F11)
    ("MIAO LETTER YI NNA"                                        . #x16F12)
    ("MIAO LETTER ARCHAIC NA"                                    . #x16F13)
    ("MIAO LETTER NNA"                                           . #x16F14)
    ("MIAO LETTER NNHA"                                          . #x16F15)
    ("MIAO LETTER LA"                                            . #x16F16)
    ("MIAO LETTER LYA"                                           . #x16F17)
    ("MIAO LETTER LHA"                                           . #x16F18)
    ("MIAO LETTER LHYA"                                          . #x16F19)
    ("MIAO LETTER TLHA"                                          . #x16F1A)
    ("MIAO LETTER DLHA"                                          . #x16F1B)
    ("MIAO LETTER TLHYA"                                         . #x16F1C)
    ("MIAO LETTER DLHYA"                                         . #x16F1D)
    ("MIAO LETTER KA"                                            . #x16F1E)
    ("MIAO LETTER GA"                                            . #x16F1F)
    ("MIAO LETTER YI KA"                                         . #x16F20)
    ("MIAO LETTER QA"                                            . #x16F21)
    ("MIAO LETTER QGA"                                           . #x16F22)
    ("MIAO LETTER NGA"                                           . #x16F23)
    ("MIAO LETTER NGHA"                                          . #x16F24)
    ("MIAO LETTER ARCHAIC NGA"                                   . #x16F25)
    ("MIAO LETTER HA"                                            . #x16F26)
    ("MIAO LETTER XA"                                            . #x16F27)
    ("MIAO LETTER GHA"                                           . #x16F28)
    ("MIAO LETTER GHHA"                                          . #x16F29)
    ("MIAO LETTER TSSA"                                          . #x16F2A)
    ("MIAO LETTER DZZA"                                          . #x16F2B)
    ("MIAO LETTER NYA"                                           . #x16F2C)
    ("MIAO LETTER NYHA"                                          . #x16F2D)
    ("MIAO LETTER TSHA"                                          . #x16F2E)
    ("MIAO LETTER DZHA"                                          . #x16F2F)
    ("MIAO LETTER YI TSHA"                                       . #x16F30)
    ("MIAO LETTER YI DZHA"                                       . #x16F31)
    ("MIAO LETTER REFORMED TSHA"                                 . #x16F32)
    ("MIAO LETTER SHA"                                           . #x16F33)
    ("MIAO LETTER SSA"                                           . #x16F34)
    ("MIAO LETTER ZHA"                                           . #x16F35)
    ("MIAO LETTER ZSHA"                                          . #x16F36)
    ("MIAO LETTER TSA"                                           . #x16F37)
    ("MIAO LETTER DZA"                                           . #x16F38)
    ("MIAO LETTER YI TSA"                                        . #x16F39)
    ("MIAO LETTER SA"                                            . #x16F3A)
    ("MIAO LETTER ZA"                                            . #x16F3B)
    ("MIAO LETTER ZSA"                                           . #x16F3C)
    ("MIAO LETTER ZZA"                                           . #x16F3D)
    ("MIAO LETTER ZZSA"                                          . #x16F3E)
    ("MIAO LETTER ARCHAIC ZZA"                                   . #x16F3F)
    ("MIAO LETTER ZZYA"                                          . #x16F40)
    ("MIAO LETTER ZZSYA"                                         . #x16F41)
    ("MIAO LETTER WA"                                            . #x16F42)
    ("MIAO LETTER AH"                                            . #x16F43)
    ("MIAO LETTER HHA"                                           . #x16F44)
    ("MIAO LETTER NASALIZATION"                                  . #x16F50)
    ("MIAO SIGN ASPIRATION"                                      . #x16F51)
    ("MIAO SIGN REFORMED VOICING"                                . #x16F52)
    ("MIAO SIGN REFORMED ASPIRATION"                             . #x16F53)
    ("MIAO VOWEL SIGN A"                                         . #x16F54)
    ("MIAO VOWEL SIGN AA"                                        . #x16F55)
    ("MIAO VOWEL SIGN AHH"                                       . #x16F56)
    ("MIAO VOWEL SIGN AN"                                        . #x16F57)
    ("MIAO VOWEL SIGN ANG"                                       . #x16F58)
    ("MIAO VOWEL SIGN O"                                         . #x16F59)
    ("MIAO VOWEL SIGN OO"                                        . #x16F5A)
    ("MIAO VOWEL SIGN WO"                                        . #x16F5B)
    ("MIAO VOWEL SIGN W"                                         . #x16F5C)
    ("MIAO VOWEL SIGN E"                                         . #x16F5D)
    ("MIAO VOWEL SIGN EN"                                        . #x16F5E)
    ("MIAO VOWEL SIGN ENG"                                       . #x16F5F)
    ("MIAO VOWEL SIGN OEY"                                       . #x16F60)
    ("MIAO VOWEL SIGN I"                                         . #x16F61)
    ("MIAO VOWEL SIGN IA"                                        . #x16F62)
    ("MIAO VOWEL SIGN IAN"                                       . #x16F63)
    ("MIAO VOWEL SIGN IANG"                                      . #x16F64)
    ("MIAO VOWEL SIGN IO"                                        . #x16F65)
    ("MIAO VOWEL SIGN IE"                                        . #x16F66)
    ("MIAO VOWEL SIGN II"                                        . #x16F67)
    ("MIAO VOWEL SIGN IU"                                        . #x16F68)
    ("MIAO VOWEL SIGN ING"                                       . #x16F69)
    ("MIAO VOWEL SIGN U"                                         . #x16F6A)
    ("MIAO VOWEL SIGN UA"                                        . #x16F6B)
    ("MIAO VOWEL SIGN UAN"                                       . #x16F6C)
    ("MIAO VOWEL SIGN UANG"                                      . #x16F6D)
    ("MIAO VOWEL SIGN UU"                                        . #x16F6E)
    ("MIAO VOWEL SIGN UEI"                                       . #x16F6F)
    ("MIAO VOWEL SIGN UNG"                                       . #x16F70)
    ("MIAO VOWEL SIGN Y"                                         . #x16F71)
    ("MIAO VOWEL SIGN YI"                                        . #x16F72)
    ("MIAO VOWEL SIGN AE"                                        . #x16F73)
    ("MIAO VOWEL SIGN AEE"                                       . #x16F74)
    ("MIAO VOWEL SIGN ERR"                                       . #x16F75)
    ("MIAO VOWEL SIGN ROUNDED ERR"                               . #x16F76)
    ("MIAO VOWEL SIGN ER"                                        . #x16F77)
    ("MIAO VOWEL SIGN ROUNDED ER"                                . #x16F78)
    ("MIAO VOWEL SIGN AI"                                        . #x16F79)
    ("MIAO VOWEL SIGN EI"                                        . #x16F7A)
    ("MIAO VOWEL SIGN AU"                                        . #x16F7B)
    ("MIAO VOWEL SIGN OU"                                        . #x16F7C)
    ("MIAO VOWEL SIGN N"                                         . #x16F7D)
    ("MIAO VOWEL SIGN NG"                                        . #x16F7E)
    ("MIAO TONE RIGHT"                                           . #x16F8F)
    ("MIAO TONE TOP RIGHT"                                       . #x16F90)
    ("MIAO TONE ABOVE"                                           . #x16F91)
    ("MIAO TONE BELOW"                                           . #x16F92)
    ("MIAO LETTER TONE-2"                                        . #x16F93)
    ("MIAO LETTER TONE-3"                                        . #x16F94)
    ("MIAO LETTER TONE-4"                                        . #x16F95)
    ("MIAO LETTER TONE-5"                                        . #x16F96)
    ("MIAO LETTER TONE-6"                                        . #x16F97)
    ("MIAO LETTER TONE-7"                                        . #x16F98)
    ("MIAO LETTER TONE-8"                                        . #x16F99)
    ("MIAO LETTER REFORMED TONE-1"                               . #x16F9A)
    ("MIAO LETTER REFORMED TONE-2"                               . #x16F9B)
    ("MIAO LETTER REFORMED TONE-4"                               . #x16F9C)
    ("MIAO LETTER REFORMED TONE-5"                               . #x16F9D)
    ("MIAO LETTER REFORMED TONE-6"                               . #x16F9E)
    ("MIAO LETTER REFORMED TONE-8"                               . #x16F9F)
    ("ARABIC MATHEMATICAL ALEF"                                  . #x1EE00)
    ("ARABIC MATHEMATICAL BEH"                                   . #x1EE01)
    ("ARABIC MATHEMATICAL JEEM"                                  . #x1EE02)
    ("ARABIC MATHEMATICAL DAL"                                   . #x1EE03)
    ("ARABIC MATHEMATICAL WAW"                                   . #x1EE05)
    ("ARABIC MATHEMATICAL ZAIN"                                  . #x1EE06)
    ("ARABIC MATHEMATICAL HAH"                                   . #x1EE07)
    ("ARABIC MATHEMATICAL TAH"                                   . #x1EE08)
    ("ARABIC MATHEMATICAL YEH"                                   . #x1EE09)
    ("ARABIC MATHEMATICAL KAF"                                   . #x1EE0A)
    ("ARABIC MATHEMATICAL LAM"                                   . #x1EE0B)
    ("ARABIC MATHEMATICAL MEEM"                                  . #x1EE0C)
    ("ARABIC MATHEMATICAL NOON"                                  . #x1EE0D)
    ("ARABIC MATHEMATICAL SEEN"                                  . #x1EE0E)
    ("ARABIC MATHEMATICAL AIN"                                   . #x1EE0F)
    ("ARABIC MATHEMATICAL FEH"                                   . #x1EE10)
    ("ARABIC MATHEMATICAL SAD"                                   . #x1EE11)
    ("ARABIC MATHEMATICAL QAF"                                   . #x1EE12)
    ("ARABIC MATHEMATICAL REH"                                   . #x1EE13)
    ("ARABIC MATHEMATICAL SHEEN"                                 . #x1EE14)
    ("ARABIC MATHEMATICAL TEH"                                   . #x1EE15)
    ("ARABIC MATHEMATICAL THEH"                                  . #x1EE16)
    ("ARABIC MATHEMATICAL KHAH"                                  . #x1EE17)
    ("ARABIC MATHEMATICAL THAL"                                  . #x1EE18)
    ("ARABIC MATHEMATICAL DAD"                                   . #x1EE19)
    ("ARABIC MATHEMATICAL ZAH"                                   . #x1EE1A)
    ("ARABIC MATHEMATICAL GHAIN"                                 . #x1EE1B)
    ("ARABIC MATHEMATICAL DOTLESS BEH"                           . #x1EE1C)
    ("ARABIC MATHEMATICAL DOTLESS NOON"                          . #x1EE1D)
    ("ARABIC MATHEMATICAL DOTLESS FEH"                           . #x1EE1E)
    ("ARABIC MATHEMATICAL DOTLESS QAF"                           . #x1EE1F)
    ("ARABIC MATHEMATICAL INITIAL BEH"                           . #x1EE21)
    ("ARABIC MATHEMATICAL INITIAL JEEM"                          . #x1EE22)
    ("ARABIC MATHEMATICAL INITIAL HEH"                           . #x1EE24)
    ("ARABIC MATHEMATICAL INITIAL HAH"                           . #x1EE27)
    ("ARABIC MATHEMATICAL INITIAL YEH"                           . #x1EE29)
    ("ARABIC MATHEMATICAL INITIAL KAF"                           . #x1EE2A)
    ("ARABIC MATHEMATICAL INITIAL LAM"                           . #x1EE2B)
    ("ARABIC MATHEMATICAL INITIAL MEEM"                          . #x1EE2C)
    ("ARABIC MATHEMATICAL INITIAL NOON"                          . #x1EE2D)
    ("ARABIC MATHEMATICAL INITIAL SEEN"                          . #x1EE2E)
    ("ARABIC MATHEMATICAL INITIAL AIN"                           . #x1EE2F)
    ("ARABIC MATHEMATICAL INITIAL FEH"                           . #x1EE30)
    ("ARABIC MATHEMATICAL INITIAL SAD"                           . #x1EE31)
    ("ARABIC MATHEMATICAL INITIAL QAF"                           . #x1EE32)
    ("ARABIC MATHEMATICAL INITIAL SHEEN"                         . #x1EE34)
    ("ARABIC MATHEMATICAL INITIAL TEH"                           . #x1EE35)
    ("ARABIC MATHEMATICAL INITIAL THEH"                          . #x1EE36)
    ("ARABIC MATHEMATICAL INITIAL KHAH"                          . #x1EE37)
    ("ARABIC MATHEMATICAL INITIAL DAD"                           . #x1EE39)
    ("ARABIC MATHEMATICAL INITIAL GHAIN"                         . #x1EE3B)
    ("ARABIC MATHEMATICAL TAILED JEEM"                           . #x1EE42)
    ("ARABIC MATHEMATICAL TAILED HAH"                            . #x1EE47)
    ("ARABIC MATHEMATICAL TAILED YEH"                            . #x1EE49)
    ("ARABIC MATHEMATICAL TAILED LAM"                            . #x1EE4B)
    ("ARABIC MATHEMATICAL TAILED NOON"                           . #x1EE4D)
    ("ARABIC MATHEMATICAL TAILED SEEN"                           . #x1EE4E)
    ("ARABIC MATHEMATICAL TAILED AIN"                            . #x1EE4F)
    ("ARABIC MATHEMATICAL TAILED SAD"                            . #x1EE51)
    ("ARABIC MATHEMATICAL TAILED QAF"                            . #x1EE52)
    ("ARABIC MATHEMATICAL TAILED SHEEN"                          . #x1EE54)
    ("ARABIC MATHEMATICAL TAILED KHAH"                           . #x1EE57)
    ("ARABIC MATHEMATICAL TAILED DAD"                            . #x1EE59)
    ("ARABIC MATHEMATICAL TAILED GHAIN"                          . #x1EE5B)
    ("ARABIC MATHEMATICAL TAILED DOTLESS NOON"                   . #x1EE5D)
    ("ARABIC MATHEMATICAL TAILED DOTLESS QAF"                    . #x1EE5F)
    ("ARABIC MATHEMATICAL STRETCHED BEH"                         . #x1EE61)
    ("ARABIC MATHEMATICAL STRETCHED JEEM"                        . #x1EE62)
    ("ARABIC MATHEMATICAL STRETCHED HEH"                         . #x1EE64)
    ("ARABIC MATHEMATICAL STRETCHED HAH"                         . #x1EE67)
    ("ARABIC MATHEMATICAL STRETCHED TAH"                         . #x1EE68)
    ("ARABIC MATHEMATICAL STRETCHED YEH"                         . #x1EE69)
    ("ARABIC MATHEMATICAL STRETCHED KAF"                         . #x1EE6A)
    ("ARABIC MATHEMATICAL STRETCHED MEEM"                        . #x1EE6C)
    ("ARABIC MATHEMATICAL STRETCHED NOON"                        . #x1EE6D)
    ("ARABIC MATHEMATICAL STRETCHED SEEN"                        . #x1EE6E)
    ("ARABIC MATHEMATICAL STRETCHED AIN"                         . #x1EE6F)
    ("ARABIC MATHEMATICAL STRETCHED FEH"                         . #x1EE70)
    ("ARABIC MATHEMATICAL STRETCHED SAD"                         . #x1EE71)
    ("ARABIC MATHEMATICAL STRETCHED QAF"                         . #x1EE72)
    ("ARABIC MATHEMATICAL STRETCHED SHEEN"                       . #x1EE74)
    ("ARABIC MATHEMATICAL STRETCHED TEH"                         . #x1EE75)
    ("ARABIC MATHEMATICAL STRETCHED THEH"                        . #x1EE76)
    ("ARABIC MATHEMATICAL STRETCHED KHAH"                        . #x1EE77)
    ("ARABIC MATHEMATICAL STRETCHED DAD"                         . #x1EE79)
    ("ARABIC MATHEMATICAL STRETCHED ZAH"                         . #x1EE7A)
    ("ARABIC MATHEMATICAL STRETCHED GHAIN"                       . #x1EE7B)
    ("ARABIC MATHEMATICAL STRETCHED DOTLESS BEH"                 . #x1EE7C)
    ("ARABIC MATHEMATICAL STRETCHED DOTLESS FEH"                 . #x1EE7E)
    ("ARABIC MATHEMATICAL LOOPED ALEF"                           . #x1EE80)
    ("ARABIC MATHEMATICAL LOOPED BEH"                            . #x1EE81)
    ("ARABIC MATHEMATICAL LOOPED JEEM"                           . #x1EE82)
    ("ARABIC MATHEMATICAL LOOPED DAL"                            . #x1EE83)
    ("ARABIC MATHEMATICAL LOOPED HEH"                            . #x1EE84)
    ("ARABIC MATHEMATICAL LOOPED WAW"                            . #x1EE85)
    ("ARABIC MATHEMATICAL LOOPED ZAIN"                           . #x1EE86)
    ("ARABIC MATHEMATICAL LOOPED HAH"                            . #x1EE87)
    ("ARABIC MATHEMATICAL LOOPED TAH"                            . #x1EE88)
    ("ARABIC MATHEMATICAL LOOPED YEH"                            . #x1EE89)
    ("ARABIC MATHEMATICAL LOOPED LAM"                            . #x1EE8B)
    ("ARABIC MATHEMATICAL LOOPED MEEM"                           . #x1EE8C)
    ("ARABIC MATHEMATICAL LOOPED NOON"                           . #x1EE8D)
    ("ARABIC MATHEMATICAL LOOPED SEEN"                           . #x1EE8E)
    ("ARABIC MATHEMATICAL LOOPED AIN"                            . #x1EE8F)
    ("ARABIC MATHEMATICAL LOOPED FEH"                            . #x1EE90)
    ("ARABIC MATHEMATICAL LOOPED SAD"                            . #x1EE91)
    ("ARABIC MATHEMATICAL LOOPED QAF"                            . #x1EE92)
    ("ARABIC MATHEMATICAL LOOPED REH"                            . #x1EE93)
    ("ARABIC MATHEMATICAL LOOPED SHEEN"                          . #x1EE94)
    ("ARABIC MATHEMATICAL LOOPED TEH"                            . #x1EE95)
    ("ARABIC MATHEMATICAL LOOPED THEH"                           . #x1EE96)
    ("ARABIC MATHEMATICAL LOOPED KHAH"                           . #x1EE97)
    ("ARABIC MATHEMATICAL LOOPED THAL"                           . #x1EE98)
    ("ARABIC MATHEMATICAL LOOPED DAD"                            . #x1EE99)
    ("ARABIC MATHEMATICAL LOOPED ZAH"                            . #x1EE9A)
    ("ARABIC MATHEMATICAL LOOPED GHAIN"                          . #x1EE9B)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK BEH"                     . #x1EEA1)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK JEEM"                    . #x1EEA2)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK DAL"                     . #x1EEA3)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK WAW"                     . #x1EEA5)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK ZAIN"                    . #x1EEA6)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK HAH"                     . #x1EEA7)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK TAH"                     . #x1EEA8)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK YEH"                     . #x1EEA9)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK LAM"                     . #x1EEAB)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK MEEM"                    . #x1EEAC)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK NOON"                    . #x1EEAD)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK SEEN"                    . #x1EEAE)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK AIN"                     . #x1EEAF)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK FEH"                     . #x1EEB0)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK SAD"                     . #x1EEB1)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK QAF"                     . #x1EEB2)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK REH"                     . #x1EEB3)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK SHEEN"                   . #x1EEB4)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK TEH"                     . #x1EEB5)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK THEH"                    . #x1EEB6)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK KHAH"                    . #x1EEB7)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK THAL"                    . #x1EEB8)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK DAD"                     . #x1EEB9)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK ZAH"                     . #x1EEBA)
    ("ARABIC MATHEMATICAL DOUBLE-STRUCK GHAIN"                   . #x1EEBB)
    ("ARABIC MATHEMATICAL OPERATOR MEEM WITH HAH WITH TATWEEL"   . #x1EEF0)
    ("ARABIC MATHEMATICAL OPERATOR HAH WITH DAL"                 . #x1EEF1)
    ("RAISED MC SIGN"                                            . #x1F16A)
    ("RAISED MD SIGN"                                            . #x1F16B)
    ("CIRCLED CROSS POMMEE"                                      . #x1F540)
    ("CROSS POMMEE WITH HALF-CIRCLE BELOW"                       . #x1F541)
    ("CROSS POMMEE"                                              . #x1F542)
    ("NOTCHED LEFT SEMICIRCLE WITH THREE DOTS"                   . #x1F543)
    ("GRINNING FACE"                                             . #x1F600)
    ("EXPRESSIONLESS FACE"                                       . #x1F611)
    ("CONFUSED FACE"                                             . #x1F615)
    ("KISSING FACE"                                              . #x1F617)
    ("KISSING FACE WITH SMILING EYES"                            . #x1F619)
    ("FACE WITH STUCK-OUT TONGUE"                                . #x1F61B)
    ("WORRIED FACE"                                              . #x1F61F)
    ("FROWNING FACE WITH OPEN MOUTH"                             . #x1F626)
    ("ANGUISHED FACE"                                            . #x1F627)
    ("GRIMACING FACE"                                            . #x1F62C)
    ("FACE WITH OPEN MOUTH"                                      . #x1F62E)
    ("HUSHED FACE"                                               . #x1F62F)
    ("SLEEPING FACE"                                             . #x1F634))
  "Corrections for ambiguities or omissions in `ucs-names', resolved in favor of Unicode 6.1.")

;; attempt to load Unicode 6.0 characters for Emacs 23.x
(when (< emacs-major-version 24)
  (require 'ucs-utils-6.0-delta nil t))

;; note: outside the ucs-utils- namespace
(defvar character-name-history nil "History of character names entered in the minibuffer.")

;;; compatibility functions

(unless (fboundp 'memoize)
  ;; by Christopher Wellons <mosquitopsu@gmail.com>
  (defun memoize (func)
    "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
    (typecase func
      (symbol (fset func (memoize-wrap (symbol-function func))) func)
      (function (memoize-wrap func))))
  (defun memoize-wrap (func)
    "Return the memoized version of the given function."
    (let ((table-sym (gensym))
          (val-sym (gensym))
          (args-sym (gensym)))
      (set table-sym (make-hash-table :test 'equal))
      `(lambda (&rest ,args-sym)
         ,(concat (documentation func) "\n(memoized function)")
         (let ((,val-sym (gethash ,args-sym ,table-sym)))
           (if ,val-sym
               ,val-sym
             (puthash ,args-sym (apply ,func ,args-sym) ,table-sym)))))))

;;; utility functions

;; Unfortunately we can't be dash-insensitive b/c UCS names are
;; sensitive to dashes eg TIBETAN LETTER -A vs TIBETAN LETTER A
;; or HANGUL JUNGSEONG O-E vs HANGUL JUNGSEONG OE.
;;
;; Counting deprecated forms, ucs-list is even space-sensitive,
;; though that could be resolved by using only the latest version.
;; Example LATIN SMALL LETTER D Z vs LATIN SMALL LETTER DZ
(defun ucs-utils--lookup (name)
  "Look up the character corresponding to NAME, a UCS name.

NAME is not sensitive to case, allows leading and trailing
double-quotes, and does not distinguish between spaces and
underscores in a UCS name.

Deprecated UCS names are permitted, but conflicts are resolved
in favor of later versions of the Unicode spec.  \"BELL\" is
a famous example of a conflict.

Returns nil if NAME does not exist."
  (when (and ucs-utils-use-persistent-storage
             (or (null (persistent-soft-fetch 'names-hash-emacs-version ucs-utils-use-persistent-storage))
                 (version< (persistent-soft-fetch 'names-hash-emacs-version ucs-utils-use-persistent-storage)
                           emacs-version)))
    (setq ucs-utils-names-hash nil)
    (persistent-soft-store 'ucs-utils-names-hash nil ucs-utils-use-persistent-storage))
  (save-match-data
    (callf upcase name)
    (setq name (replace-regexp-in-string "\\`[ \t\"]+" ""
                  (replace-regexp-in-string "[ \t\"]+\\'" ""
                     (replace-regexp-in-string "[ \t]\\{2,\\}" " "
                        (replace-regexp-in-string "_" " "  name)))))
    (when (and ucs-utils-trade-memory-for-speed
               (not (hash-table-p ucs-utils-names-hash)))
      (unless (hash-table-p (setq ucs-utils-names-hash (persistent-soft-fetch 'ucs-utils-names-hash ucs-utils-use-persistent-storage)))
        (let ((dupes nil)
              (key nil))
          (setq ucs-utils-names-hash (make-hash-table :size (length (ucs-names)) :test 'equal))
          (dolist (cell (ucs-names))
            (setq key (car cell))
            (when (and (gethash key ucs-utils-names-hash)
                       (not (eq (gethash key ucs-utils-names-hash) (cdr cell))))
              (push key dupes))
            (puthash key (cdr cell) ucs-utils-names-hash))
          (delete-dups dupes)
          (dolist (key dupes)
            (remhash key ucs-utils-names-hash))
          (dolist (cell ucs-utils-names-corrections)
            (puthash (car cell) (cdr cell) ucs-utils-names-hash))
          (persistent-soft-store 'names-hash-emacs-version emacs-version ucs-utils-use-persistent-storage)
          (persistent-soft-store 'ucs-utils-names-hash ucs-utils-names-hash ucs-utils-use-persistent-storage)
          (persistent-soft-flush ucs-utils-use-persistent-storage))))
    (cond
      ((hash-table-p ucs-utils-names-hash)
       (gethash name ucs-utils-names-hash))
      ((assoc-string name ucs-utils-names-corrections t)
       (cdr (assoc-string name ucs-utils-names-corrections t)))
      (t
       (cdr (assoc-string name (ucs-names) t))))))

(defun ucs-utils-vector-flatten (vec)
  "Flatten vector or string VEC which may contain other vectors or strings."
  (cond
    ((stringp vec)
     (string-to-vector vec))
    ((null vec)
     (vector nil))
    ((and (vectorp vec)
          (= 0 (length vec)))
     nil)
    ((vectorp vec)
     (vconcat (ucs-utils-vector-flatten (aref vec 0))
              (ucs-utils-vector-flatten (vconcat (cdr (append vec nil))))))
    (t
     (vector vec))))

(defun ucs-utils-prettify-ucs-string (name)
  "Prettify NAME, a string holding the UCS name for a character."
  (callf capitalize name)
  (save-match-data
    (when (string-match "([^()]+?)\\'" name)
      (setq name (replace-match (upcase (match-string-no-properties 0 name)) 'fixed-case 'literal name)))
    (let ((case-fold-search nil))
      (while (string-match " \\(?:And\\|Or\\|Of\\|On\\|The\\|For\\|In\\|With\\|Over\\|Under\\) " name)
        (setq name (replace-match (downcase (match-string-no-properties 0 name)) 'fixed-case 'literal name)))
      (while (string-match "\\<Logical [Aa]nd\\>" name)
        (setq name (replace-match "Logical AND" 'fixed-case 'literal name)))
      (while (string-match "\\<Logical [Oo]r\\>" name)
        (setq name (replace-match "Logical OR" 'fixed-case 'literal name)))
      (while (string-match "\\<N-Ary\\>" name)
        (setq name (replace-match "N-ary" 'fixed-case 'literal name)))
      (while (string-match "\\<Per-Em\\>" name)
        (setq name (replace-match "per-Em" 'fixed-case 'literal name)))
      (while (string-match "\\<Jack-O-Lantern\\>" name)
        (setq name (replace-match "Jack-o-Lantern" 'fixed-case 'literal name)))
      (while (string-match "\\<Fleur-De-Lis\\>" name)
        (setq name (replace-match "Fleur-de-Lis" 'fixed-case 'literal name)))
      (while (string-match "\\<Left-To-Right\\>" name)
        (setq name (replace-match "Left-to-Right" 'fixed-case 'literal name)))
      (while (string-match "\\<Right-To-Left\\>" name)
        (setq name (replace-match "Right-to-Left" 'fixed-case 'literal name)))
      (while (string-match "\\<Minus-Or-Plus\\>" name)
        (setq name (replace-match "Minus-or-Plus" 'fixed-case 'literal name)))
      (while (string-match "\\<Plus-Or-Minus\\>" name)
        (setq name (replace-match "Plus-or-Minus" 'fixed-case 'literal name)))
      (while (string-match "\\<\\(?:Cjk\\|Apl\\|Ocr\\)\\>" name)
        (setq name (replace-match (upcase (match-string-no-properties 0 name)) 'fixed-case 'literal name)))
      (when (string-match "\\`Nko\\>" name)
        (setq name (replace-match "NKo" 'fixed-case 'literal name))))
    name))

;;;###autoload
(defun ucs-utils-pretty-name (char &optional no-hex)
  "Return a prettified UCS name for CHAR.

Based on `get-char-code-property'.  The result has been
title-cased for readability, and will not match into the
`ucs-names' alist until it has been upcased.
`ucs-utils-char' can be used on the title-cased name.

Returns a hexified string if no name is found.  If NO-HEX is
non-nil, then a nil value will be returned when no name is
found."
  (let ((name (get-char-code-property char 'name)))
    (when (equal "<control>" name)
      (setq name (get-char-code-property char 'old-name)))
    (when (eq char ?\s)
      (callf or name "Space"))
    (when (= (length name) 0)
      (setq name (car (rassoc char ucs-utils-names-corrections))))
    (cond
      ((and no-hex
            (= (length name) 0))
       (setq name nil))
      ((= (length name) 0)
       (setq name (concat "#x" (upcase (format "%02x" char)))))
      (t
       (ucs-utils-prettify-ucs-string name)))))

;;;###autoload
(defun ucs-utils-all-prettified-names (&optional progress regenerate)
  "All prettified UCS names, cached in list `ucs-utils-all-prettified-names'.

When optional PROGRESS is given, show progress when generating
cache.

When optional REGENERATE is given, re-generate cache."
  (when (and ucs-utils-use-persistent-storage
             (or (null (persistent-soft-fetch 'prettified-names-emacs-version ucs-utils-use-persistent-storage))
                 (version< (persistent-soft-fetch 'prettified-names-emacs-version ucs-utils-use-persistent-storage)
                           emacs-version)))
    (setq regenerate t))
  (when regenerate
    (setq ucs-utils-all-prettified-names nil)
    (persistent-soft-store 'ucs-utils-all-prettified-names nil ucs-utils-use-persistent-storage))
  (cond
    (ucs-utils-all-prettified-names
     t)
    ((and (not regenerate)
          (persistent-soft-exists-p 'ucs-utils-all-prettified-names ucs-utils-use-persistent-storage)
          (consp (setq ucs-utils-all-prettified-names (persistent-soft-fetch 'ucs-utils-all-prettified-names ucs-utils-use-persistent-storage))))
     t)
    (t
     (let ((reporter (make-progress-reporter "Caching formatted UCS names... " 0 (length (ucs-names))))
           (counter 0)
           (draft-list nil)
           (prev-name nil))
       (dolist (cell (ucs-names))
         (when progress
           (progress-reporter-update reporter (incf counter)))
         (push (replace-regexp-in-string " " "_" (or (ucs-utils-pretty-name (cdr cell) 'no-hex) "")) draft-list))
       (dolist (name (delete "" (sort draft-list 'string<)))
         (unless (equal name prev-name)
           (push name ucs-utils-all-prettified-names))
         (setq prev-name name))
       (callf nreverse ucs-utils-all-prettified-names)
       (persistent-soft-store 'ucs-utils-all-prettified-names ucs-utils-all-prettified-names ucs-utils-use-persistent-storage)
       (persistent-soft-store 'prettified-names-emacs-version emacs-version ucs-utils-use-persistent-storage)
       (persistent-soft-flush ucs-utils-use-persistent-storage)
       (progress-reporter-done reporter))))
  ucs-utils-all-prettified-names)

(defun ucs-utils--subst-char-in-region-1 (start end from-char to-char)
  "Internal driver for `usr-utils-subst-char-in-region'.

Arguments START, END, FROM-CHAR, and TO-CHAR are as documented at
`ucs-utils-subst-char-in-region'."
  ;; done in a specific fashion to maintain markers
  (loop for i from start to (1- end)
        if (eq (char-after i) from-char)
        do (save-excursion
             (goto-char i)
             (insert to-char)
             (delete-char 1))))

;;; external interface (plus `ucs-utils-pretty-name' above)

;;;###autoload
(defun ucs-utils-char (name &optional fallback test)
  "Return the character corresponding to NAME, a UCS name.

NAME is matched leniently by `ucs-utils--lookup'.

Returns FALLBACK if NAME does not exist or is not displayable
according to TEST.  FALLBACK may be either a UCS name or
character, or one of the special symbols described in the next
paragraph.

If FALLBACK is nil or 'drop, returns nil on failure.  If FALLBACK
is 'error, throws an error on failure.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

When NAME is a character, it passes through unchanged, unless
TEST is set, in which case it must pass TEST."
  (let ((char name)
        (orig-fallback fallback))
    (when (and (eq test 'cdp)
               (not (fboundp 'cdp)))
      (setq test 'char-displayable-p))
    (when (stringp char)
      (setq char (ucs-utils--lookup char)))
    (when (stringp fallback)
      (setq fallback (ucs-utils--lookup fallback))
      (assert (integerp fallback) nil "Invalid fallback: %s" orig-fallback))
    (cond
      ((and (integerp char)
            (or (not test) (funcall test char)))
       char)
      ((eq fallback 'error)
       (error "Character invalid or undisplayable: %s" name))
      ((or (eq fallback 'drop)
           (null fallback))
       nil)
      ((vectorp fallback)
       fallback)
      (t
       (assert (integerp fallback) nil "Invalid fallback: %s" orig-fallback)
       fallback))))
(when ucs-utils-trade-memory-for-speed
  (memoize 'ucs-utils-char))

;;;###autoload
(defun ucs-utils-first-existing-char (sequence &optional test)
  "Return the first existing character from SEQUENCE of character names.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol."
 (catch 'char
   (dolist (name sequence)
     (let ((char (ucs-utils-char name nil test)))
     (when char
       (throw 'char char))))))

;;;###autoload
(defun ucs-utils-vector (sequence &optional fallback test no-flatten)
  "Return a vector corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string or character, it will be coerced
to a list of length 1.  Each name in SEQUENCE is matched
leniently by `ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil, characters which do not exist or are
undisplayable will be given as nils in the return value.  When
FALLBACK is 'drop, such characters will be silently dropped from
the return value.  When FALLBACK is 'error, such characters cause
an error to be thrown.

To allow fallbacks of arbitrary length, give FALLBACK as a vector-
of-vectors, with outer length equal to the length of sequence.  The
inner vectors may contain a sequence of characters, a literal
string, or nil.  Eg

   (ucs-utils-vector '(\"Middle Dot\" \"Ampersand\" \"Horizontal Ellipsis\")
                     '[?.           [?a ?n ?d]  [\"...\"]              ])

or

   (ucs-utils-vector \"Horizontal Ellipsis\" '[[\"...\"]])

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

If NO-FLATTEN is non-nil, then a vector-of-vectors may be returned
if multi-character fallbacks were used as in the example above."
  (let ((strip-char :nonexistent)
        (flattener (if no-flatten 'identity 'ucs-utils-vector-flatten)))
    (cond
      ((vectorp sequence)
       (callf append sequence nil))
      ((or (stringp sequence)
           (characterp sequence))
       (callf list sequence)))
    (cond
      ((eq fallback 'drop)
       (setq strip-char nil)
       (setq fallback (make-list (length sequence) nil)))
      ((null fallback)
       (setq fallback (make-list (length sequence) nil)))
      ((eq fallback 'error)
       (setq fallback (make-list (length sequence) 'error)))
      ((vectorp fallback)
       (callf append fallback nil))
      ((or (stringp fallback)
           (characterp fallback))
       (callf list fallback)))
    (assert (and (listp sequence) (listp fallback)) nil "SEQUENCE and FALLBACK should be lists or vectors.")
    (assert (= (length sequence) (length fallback)) nil "SEQUENCE and FALLBACK should be the same length.")
    (funcall flattener (apply 'vector (delq strip-char (loop for elt in sequence
                                                             for back-elt in fallback
                                                             collect (ucs-utils-char elt back-elt test)))))))

;;;###autoload
(defun ucs-utils-string (sequence &optional fallback test)
  "Return a string corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil or 'drop, characters which do not exist or
are undisplayable will be silently dropped from the return value.
When FALLBACK is 'error, such characters cause an error to be
thrown.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol."
  (callf or fallback 'drop)
  (concat (delete nil (ucs-utils-vector sequence fallback test))))

;;;###autoload
(defun ucs-utils-intact-string (sequence fallback &optional test)
  "Return a string corresponding to SEQUENCE of UCS names or characters.

This function differs from `ucs-utils-string' in that FALLBACK is
a non-optional single string, to be used unless every member of
SEQUENCE exists and passes TEST.  FALLBACK may not be nil, 'error,
or 'drop as in `ucs-utils-string'.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol."
  (assert (stringp fallback) nil "FALLBACK must be a string")
  (condition-case nil
      (concat (ucs-utils-vector sequence 'error test))
    (error nil
           fallback)))

;;;###autoload
(defun ucs-utils-subst-char-in-region (start end from-char to-char &optional no-undo)
  "From START to END, replace FROM-CHAR with TO-CHAR each time it occurs.

If optional arg NO-UNDO is non-nil, don't record this change for
undo and don't mark the buffer as really changed.

Characters may be of differing byte-lengths.

The character at the position END is not included, matching the
behavior of `subst-char-in-region'.

This function is slower than `subst-char-in-region'."
  (setq from-char (ucs-utils-char from-char 'error))
  (setq to-char   (ucs-utils-char to-char 'error))
  (if no-undo
      (let ((buffer-undo-list nil)
            (modified (buffer-modified-p)))
        (ucs-utils--subst-char-in-region-1 start end from-char to-char)
        (set-buffer-modified-p modified))
    ;; else
    (ucs-utils--subst-char-in-region-1 start end from-char to-char)))

(fset 'ucs-utils-orig-read-char-by-name (symbol-function 'read-char-by-name))

;;;###autoload
(defun ucs-utils-read-char-by-name (prompt &optional ido)
  "Read a character by its Unicode name or hex number string.

A wrapper for `read-char-by-name', with the option to use
`ido-completing-read'.

PROMPT is displayed, and a string that represents a character by
its name is read.

When IDO is set, several seconds are required on the first
run as all completion candidates are pre-generated."
  (if (not ido)
      (ucs-utils-orig-read-char-by-name prompt)
    ;; else
    (let ((input (ido-completing-read
                  prompt
                  (ucs-utils-all-prettified-names 'progress) nil nil nil character-name-history)))
      (when (string-match-p "\\`[0-9a-fA-F]+\\'" input)
        (callf2 concat "#x" input))
      (if (string-match-p "\\`#" input)
          (read input)
        ;; else
        (ucs-utils--lookup input)))))

;;; interactive commands

;;;###autoload
(defun ucs-utils-eval (&optional pos arg)
  "Display a string UCS name for the character at POS.

POS defaults to the current point.

If `transient-mark-mode' is enabled and there is an active
region, return a list of strings UCS names, one for each
character in the region.  If called from Lisp with an
explicit POS, ignores the region.

If called with universal prefix ARG, display the result
in a separate buffer.  If called with two universal
prefix ARGs, replace the current character or region with
its UCS name translation."
  (interactive)
  (let ((result nil)
        (print-level nil)
        (print-length nil))
    (callf or arg current-prefix-arg)
    (cond
      ((and (not pos)
            (use-region-p))
       (dolist (char (string-to-list (buffer-substring-no-properties (region-beginning) (region-end))))
         (push (ucs-utils-pretty-name char) result))
       (setq result (nreverse result)))
      (t
       (save-excursion
         (goto-char (or pos (point)))
         (setq result (ucs-utils-pretty-name (char-after))))))
    (assert result nil "Failed to find name for character at: %s" pos)
    (cond
      ((equal arg '(4))
       (flet ((frame-width (&rest args) 0))
         (pp-display-expression result "*Pp Eval Output*")))
      ((consp arg)
       (if (and (not pos)
                (use-region-p))
           (progn
             (let ((begin (region-beginning)))
               (delete-region (region-beginning) (region-end))
               (goto-char begin)
               (pp result (current-buffer))))
         ;; else
         (goto-char (or pos (point)))
         (insert (concat "\"" (prin1 result) "\""))
         (delete-char 1)))
      (t
       (pp-display-expression result "*Pp Eval Output*")))
    result))

;;;###autoload
(defun ucs-utils-ucs-insert (character &optional count inherit)
  "Insert CHARACTER in COUNT copies, where CHARACTER is a Unicode code point.

Works like `ucs-insert', with the following differences

   * Uses `ido-completing-read' at the interactive prompt

   * If `transient-mark-mode' is enabled, and the region contains
     a valid UCS character name, that value is used as the
     character name and the region is replaced.

   * A UCS character name string may be passed for CHARACTER.

INHERIT is as documented at `ucs-insert'."
  (interactive
   (list
    (if (and (use-region-p)
             (ucs-utils-char (buffer-substring-no-properties (region-beginning) (region-end)) nil))
        (prog1
          (ucs-utils-char (buffer-substring-no-properties (region-beginning) (region-end)) nil)
          (delete-region (region-beginning) (region-end)))
      ;; else
      (ucs-utils-read-char-by-name "Unicode (name or hex): " 'ido))
    (prefix-numeric-value current-prefix-arg)
    t))
  (ucs-insert (ucs-utils-char character 'error) count inherit))

;;;###autoload
(defun ucs-utils-install-aliases ()
  "Install aliases outside the \"ucs-utils-\" namespace.

The following aliases will be installed

   `ucs-char'                  for   `ucs-utils-char'
   `ucs-first-existing-char'   for   `ucs-utils-first-existing-char'
   `ucs-string'                for   `ucs-utils-string'
   `ucs-intact-string'         for   `ucs-utils-intact-string'
   `ucs-vector'                for   `ucs-utils-vector'
   `ucs-pretty-name'           for   `ucs-utils-pretty-name'
   `ucs-eval'                  for   `ucs-utils-eval'"
  (interactive)
  (defalias 'ucs-char                 'ucs-utils-char)
  (defalias 'ucs-first-existing-char  'ucs-utils-first-existing-char)
  (defalias 'ucs-string               'ucs-utils-string)
  (defalias 'ucs-intact-string        'ucs-utils-intact-string)
  (defalias 'ucs-vector               'ucs-utils-vector)
  (defalias 'ucs-pretty-name          'ucs-utils-pretty-name)
  (defalias 'ucs-eval                 'ucs-utils-eval))

(provide 'ucs-utils)

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
;; LocalWords:  UcsUtils utils eval callf flet YOGH alist ZHAR PHAR
;; LocalWords:  KHAR GHAN JHAN KIYEOK PIEUP CIEUC HALFWIDTH Fleur
;; LocalWords:  JUNGSEONG
;;

;;; ucs-utils.el ends here

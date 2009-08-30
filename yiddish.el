;;; yiddish.el --- Quail package for inputting Yiddish characters  -*-coding: utf-8;-*-

;; Copyright (C) 2008  Niels Giesen <niels.giesen@gmail.com>

;; Author: Niels Giesen <niels.giesen@gmail.com>
;; Keywords: mule, input method, Yiddish
;; Version: 0.2

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

;;; Commentary:

;; (describe-input-method 'yiddish)
;; (describe-input-method 'yiddish-yivo)

;;; ToDo:

;; Ditch stretched forms (maybe ditch any representational character)

;; Add support for automatic final forms before any non-alpha-character.

;; Look into support for automatic shtume alef prepending.

;; Add vu and (initial) yi.

;;; Code:

(require 'quail)

(quail-define-package
 "yiddish" "Yiddish" "ײ" t
 "Yiddish (UTF-8) input method.

Based on the Gnome transliterated keymap for the Israeli input
method, with some notable changes (Israeli users: you will should
yourself in the foot expecting a layout for Ivrit. This layout
could have been useful in Birobidzhan):

The letters ט and ת are reversed, as the letter ט is more common in Yiddish.

The letter \"e\" inserts ע instead of א.
The letter \"o\" inserts א instead of ס.
The letter \"s\" inserts ס instead of ש (type \"w\", \"S\" or \"sh\" for ש).
The letter \"v\" inserts װ instead of single ו.

Additionally, the digraphs ױ, ײ and װ are inserted for \"oi/ui\", \"ii\" and
\"uu\", and the sequence אױ is inserted for ooi.

When followed by a space (signalling the end of a word) the final forms of
the characters מ,נ,פ,כ and צ (ם,ן,ף,ך and ץ) are inserted.

When followed by a space (signaling the end of a word) the final
forms of the characters מ,נ,פ,כ and צ (ם,ן,ף,ך and ץ) are
inserted. Newlines,tab chars and interpunction do NOT insert the
final forms. If anyone knows a simple way for that, please tell
me.

Use the shift key when inserting a vowel of the set [ו, י, ײ, ױ]
to insert it with a prepending alef (א). This marks the beginning
of the word (analogous to capitalised substantives in German). So
you can write Ein, cvei, drei, translating to אײן, צװײ דרײ.

Just to give a feel for how to write, here's a song by Isadore
Lillian in one of its input forms:

Ich her ersht derfun as siz do a sun vus varemt In leicht azoi shein
men hot Oisgetracht gur az bei der nacht 
sheint a lbnh (levone) mit khn (khein)
gehert Oich amol fun shteren On zul 
vus finklen far Uns angenem
men sugt's blit a blum vus shenkt Uns parfium 
nor Ich Einer veis nit fun dem

di sun hot far mir noch kein mul nit gesheint 
di lbnh bahalt sich sis tinkel
Un di shteren ken Ich shveren
ferloshen In sei Is der finkl.
kein blum hot far mir noch kein mul nit geblit 
dus glik dacht sich Ois hot mich feint
Ich sug es mit shmerc mit wetig In herc 
di sun hot far mir noch kein mul nit gesheint.

Stretched forms always appear as the last option and -as they are
representational characters- should NOT be used. Depending on the
font used, their display might very well be the same as the
unstretched forms.

Punctuation marks be input after the character they apply to. Pressing `,
backtick, repeatedly cycles through candidates. Some common combinations
for (punctuated) Yiddish for which no separate character code exists are given
as candidates (notably עֶ ײֵ) with the characters themselves to form a composite
character."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules

 ("#" ["א"])
 ("ts" [?צ ?צּ ?ץ])

 ("w" [?ש ?שּ ?שׁ ?שׂ ?שּׁ ?שּׂ])
 ("v" ?װ)
 ("uu" ?װ)
 ("oi" ?ױ)
 ("Oi" ["אױ"])
 ("I" ["אי" "איּ"])
 ("O" ["או"])
 ("U" ["או"])
 ("ui" ?ױ)
 ("ii" [?ײ ?ײַ "יִיִ" "ײֵ"])
 ("ei" [?ײ ?ײַ "יִיִ" "ײֵ"])
 ("ai" [?ײַ "ײַ"])
 ("Ei" ["אײ" "אײַ" "איִיִ" "אײֵ"])
 ("Ii" ["אײ" "אײַ" "איִיִ" "אײֵ"])
 ("\"" [?״ ?\"])
 ("'" [?׳ ?'])
 ("q" [?ק ?קּ])
 ("r" [?ר ?רּ ?ﬧ])
 ("t" [?ט ?טּ])
 ("sh" ?ש)

 ;; czech keyboard:
 ("š" ?ש)
 ("ž" ["זש"])
 ("č" ["טש"])

 ("S" ?ש)
 ("u" [?ו ?וּ ?וֹ])
 ("N" ?ן)
 ("M" [?ם ?ﬦ])

 ("m" [?מ ?מּ ?ם ?ﬦ])
 ("n" [?נ ?נּ ?ן])
 ("p" [?פ ?פּ ?ף ?ףּ])
 ("f" [?פֿ])
 ("k" [?ק ?קּ ?כ ?כּ ?כֿ ?ך ?ךּ ?ﬤ])
 ("ch" [?כ ?כֿ ?ך ?ﬤ])
 ("c" [?צ ?צּ ?ץ])

 ("m " ["ם "])
 ("n " ["ן "])
 ("p " ["ף " "ףּ "])
 ("f " ["ף "])
 ("k " ["ק" "ך " "ךּ "])
 ("ch " ["ך " "ךּ "])
 ("c " ["ץ "])
 ("ts " ["ץ "])

 ("p" ?פ)
 ("s" [?ס ?סּ])
 ("d" [?ד ?דּ ?ﬢ])
 ("g" [?ג ?גּ])

 ("y" [?ע "עֶ" ?ﬠ])
 ("e" [?ע "עֶ" ?ﬠ])
 ("i" [?י ?יּ "יֽ"])
 ("j" ?י ?יּ "יֽ")
 ("x" ?ח)
 ("l" [?ל ?לּ ?ﬥ])

 ("K" [?ך ךּ])
 ("F" ?ף)
 ("P" [?ף ?ףּ])

 ("z" ?ז ?זּ)
 ("o" [?אָ ?א ?אּ "אֵ" ?ﬡ])
 ("a" ["אַ" ?א ?אּ "אֵ" ?ﬡ])

 ("al" ["אל" ?ﭏ])
 ("b" [?ב ?בּ ?בֿ])

 ("h" ?ה ?הּ ?ﬣ)
 ("H" ?ח)
 ("kh" ?ח)

 ("n" ?נ)

 ("T" [?ת ?תּ ?ﬨ])
 ("C" ?ץ)

 ("`" [?` ?ָ ?ֵ ?ֶ ?ִ ?ַ ?ְ ?ׁ ?ׂ ?ּ])
 ("``" [?ֱ ?ֲ ?ֳ ?ﬞ ?ֹ ?ֻ])
 ("```" [?ֽ ?־ ?ֿ ?׀ ?׃])
 ;; Turn back to nothing on full cycle
 ("````" [""])

 ;; interpunction
 ("+" [?+ ?﬩])
 ("-" [?ֿ ?־ ?-]))


(quail-define-package
 "yiddish-yivo" "Yiddish" "YIVO" t
 "Yiddish (UTF-8) input method based on YIVO transliteration."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("#" ["א"])
 ("a" ["אַ"])
 ("o" ["אָ"])
 ("b" ["ב"])
 ("v" ["װ" "בֿ"])
 ("g" ["ג"])
 ("d" ["ד"])
 ("h" ["ה"])
 ("u" ["ו" "וּ"])
 ("z" ["ז"])
 ("kh" ["כ" "ח" "ך"])
 ("t" ["ט" "תּ"])
 ("y" ["י"])
 ("i" ["י" "יּ"])
 ("k" ["ק" "כּ"])
 ("l" ["ל"])
 ("m" ["מ" "ם"])
 ("n" ["נ" "ן"])
 ("s" ["ס" "שׂ" "ת"])
 ("e" ["ע"])
 ("p" ["פּ"])
 ("f" ["פֿ" "ף"])
 ("ts" ["צ" "ץ"])
 ("r" ["ר"])
 ("sh" ["ש"])
 ("zh" ["זש"])
 ("oi" ["ױ"])
 ("ey" ["ײ"])
 ("ay" ["ײַ"])

 ;; Interpunction
 ("+" [?+ ?﬩])
 ("-" [?־ ?ֿ ?-])

 ;; Hebrew typewriter style punctuation marks:
 ("Q" ?ְ)
 ("W" ?ֱ)
 ("E" ?ֲ)
 ("R" ?ֳ)
 ("T" ?ִ)
 ("Y" ?ֵ)
 ("U" ?ֶ)
 ("I" ?ַ)
 ("O" ?ָ)
 ("P" ?ֹ)
 ("A" ?ֻ)
 ("S" ?ּ)
 ("D" ?ֽ)
 ("F" ?־)
 ("G" ?ֿ)
 ("H" ?׀)
 ("J" ?ׁ)
 ("K" ?ׂ)
 ("L" ?׃)

 ("`" [?` ?ָ ?ֵ ?ֶ ?ִ ?ַ ?ְ ?ׁ ?ׂ ?ּ])
 ("``" [?ֱ ?ֲ ?ֳ ?ﬞ ?ֹ ?ֻ])
 ("```" [?ֽ ?־ ?ֿ ?׀ ?׃])

 ("````" [""])

 ;; interpunction
 ("+" [?+ ?﬩])
 ("-" [?ֿ ?־ ?-]))

(provide 'yiddish)
;;; yiddish.el ends here

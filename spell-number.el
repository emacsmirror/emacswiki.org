;;; spell-number.el --- Spell out an integer or currency in words.  -*- coding: utf-8 -*-

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2006, 2007 Vinicius Jose Latorre

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Time-stamp: <2007/08/31 11:30:03 vinicius>
;; Version: 3.1.1
;; Keywords: spell, local
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is NOT (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Introduction
;; ------------
;;
;; To use spell-number, insert in your ~/.emacs:
;;
;;        (require 'spell-number)
;;
;; `spelln-integer-in-words' spells out an integer in words in the language
;; specified by `spelln-language'.
;;
;; `spelln-currency-in-words' spells out a currency in words in the language
;; specified by `spelln-language' and in the country specified by
;; `spelln-country'.
;;
;; `spelln-language-database' and `spelln-currency-database' contains language
;; information for spelling.
;;
;; `spelln-numeric-string-in-words' and `spelln-currency-string-in-words'
;; accept numeric string as parameter.
;;
;; `spelln-zero-cents' indicates if " and zero cents" should be spelled.
;;
;; `spelln-and-p' indicates if " and " should be spelled.
;;
;; `spelln-comma-p' indicates if ", " should be spelled.
;;
;; `spelln-gender-default' specifies the default gender to be used when there
;; is no neuter gender.
;;
;; `spelln-period-character' specifies the character to separate periods.
;;
;; `spelln-decimal-character' specifies the decimal point character.
;;
;; `spelln-number-customize' customizes spell-number options.
;;
;;
;; Examples
;; --------
;;
;; Numeric examples:
;;
;;   (setq spelln-language 'english-us)
;;   (spelln-integer-in-words 121121)
;;  ==> "one hundred twenty-one thousand, one hundred twenty-one"
;;
;;   (setq spelln-language 'english-us)
;;   (setq spelln-country 'united-states)
;;   (spelln-currency-in-words 1121.21)
;;  ==> "one thousand, one hundred twenty-one dollars and twenty-one cents"
;;
;; You can also use numeric strings:
;;
;;   (setq spelln-language 'english-us)
;;   (setq spelln-period-character ?,)
;;   (spelln-numeric-string-in-words "121,121")
;;  ==> "one hundred twenty-one thousand, one hundred twenty-one"
;;
;;   (setq spelln-language 'english-us)
;;   (setq spelln-country 'united-states)
;;   (setq spelln-period-character ?,)
;;   (setq spelln-decimal-character ?.)
;;   (spelln-currency-string-in-words "1,121.21")
;;  ==> "one thousand, one hundred twenty-one dollars and twenty-one cents"
;;
;;   (setq spelln-language 'japanese)
;;   (setq spelln-period-character ?,)
;;   (spelln-numeric-string-in-words "978,4510,2837,0000,4037")
;;  ==> "kyûhyaku nanajû hakkei yonsen gohyaku jutchô nisen happyaku sanjû
;;       nanaoku yonsen sanjû nana"
;;
;; The maximum numeric string that spell-number gets to spell out in words is:
;; * With 3 digits per period: "999,999,999,999,999,999"
;;   (danish, dutch, english-gb, english-us, esperanto, finnish, french-ch,
;;    french-fr, german, italian, norwegian, portuguese-br and swedish)
;; * With 4 digits per period: "9999,9999,9999,9999,9999"
;;   (japanese)
;; * With 6 digits per period: "999999,999999,999999,999999,999999"
;;   (catalan, english-eur, portuguese-pt and spanish)
;; Below is the answer given by spell-number to numeric strings above the
;; maximum value.
;;
;;   (setq spelln-language 'english-us)
;;   (setq spelln-period-character ?,)
;;   (spelln-numeric-string-in-words "2,000,000,000,000,121,121")
;;  ==> "two ?10^18? , one hundred twenty-one thousand, one hundred twenty-one"
;;
;; From Emacs Lisp Reference Manual:
;;
;;    The range of values for an integer depends on the machine.  The minimum
;;    range is -268435456 to 268435455 (29 bits; i.e., -2**28 to 2**28 - 1),
;;    but some machines may provide a wider range.
;;
;;    The precise range of floating point numbers is machine-specific; it is
;;    the same as the range of the C data type `double' on the machine you are
;;    using.
;;
;;
;; Gender Engine
;; -------------
;;
;; The gender engine is designed for spelling out number and currency.  It's
;; beyond the scope of gender engine to handle general gender in all languages.
;;
;; See the following examples of gender usage:
;;
;; . In english (US): (only `neuter')
;;    (spelln-integer-in-words 101)
;;	 ==> "one hundred one"
;;			  `neuter'
;;    (spelln-currency-in-words 101.01)
;;	 ==> "one hundred one dollars and one cent"
;;			  `neuter'	  `neuter'
;;
;; . In german (DE): (`neuter' is used to express numeral only)
;;    (spelln-integer-in-words 101)
;;	 ==> "einhundertundeins"
;;			   `neuter'
;;    (spelln-currency-in-words 101.01)
;;	 ==> "einhundertundeine Mark und ein Pfennig"
;;			   `feminine'	 `masculine'
;;
;; . In portuguese (BR): (no `neuter')
;;    (spelln-integer-in-words 101)
;;	 ==> "cento e um"
;;		      `masculine'
;;    (spelln-currency-in-words 101.01)
;;	 ==> "cento e um reais e um centavo"
;;		      `masculine' `masculine'
;;
;; . In spanish (ES): (no `neuter', but it's used to express numeral only)
;;    (spelln-integer-in-words 101)
;;	 ==> "ciento uno"
;;		     `neuter'
;;    (spelln-currency-in-words 101.01)
;;	 ==> "ciento una pesetas y un céntimo"
;;		     `feminine'	   `masculine'
;;
;; As you can note from the examples above, there are cases where the `neuter'
;; gender used by spell-number differs from the usual way that `neuter' gender
;; is used in a language.  This is a trick used to spell out numbers correctly.
;;
;;
;; Languages & Countries
;; ---------------------
;;
;; The following languages are supported:
;;
;; catalan, danish, dutch, english-eur, english-gb, english-us, esperanto,
;; finnish, french-fr, french-ch, german, italian, japanese, norwegian,
;; portuguese-br, portuguese-pt, spanish and swedish.
;;
;; The following countries are supported:
;;
;; andorra-french, andorra-spanish, antigua-and-barbuda, argentina, australia,
;; austria, bahamas, barbados, belgium, belize, benin, bolivia, brazil, brunei,
;; burkina-faso, burundi, cameroon, canada, cape-verde central-african-republic,
;; chad, chile, colombia, comoros, congo, costa-rica, cuba, cyprus, denmark,
;; djibouti, dominica, dominican-republic, ecuador, el-salvador,
;; equatorial-guinea, fiji, finland, france, gabon, germany, grenada, guatemala,
;; guinea, guinea-bissau, guyana, haiti, honduras, ireland, italy, ivory-coast,
;; jamaica, japan, kenya, kiribati, liberia, liechtenstein, luxembourg,
;; madagascar, mali, mexico, monaco, mozambique, namibia, nauru, netherlands,
;; new-zealand, nicaragua, niger, norway, panama, paraguay, peru, philippines,
;; portugal, rwanda, sao-tome-and-principe, senegal, singapore, solomon-islands,
;; somalia, south-africa, spain, st-kitts-and-nevis, st-lucia,
;; st-vicent-and-grenadines, sweden, switzerland, taiwan, tanzania, togo,
;; trinidad-and-tobago, tuvalu, uganda, united-kingdom, united-states, uruguay,
;; venezuela and zimbabwe.
;;
;;
;; Number in Several Language
;; --------------------------
;;
;; To see example of numbers (until million) in several languages, see the URL:
;;
;;    http://www.travlang.com/languages/
;;
;; Also, it was used:
;;
;;    Webster's New World College Dictionary
;;    MacMillan, USA, Third Edition
;;    page 876, Table of Monetary Units
;;    page 1565, Numbers
;;
;;
;; American and British Numbers
;; ----------------------------
;;
;; For numbers in english above million, see the URLs:
;;
;;    http://www.m-w.com/mw/table/number.htm
;;    http://db.uwaterloo.ca/~alopez-o/math-faq/mathtext/node25.html
;;    (or http://db.uwaterloo.ca/~alopez-o/math-faq/node54.html)
;;    http://home.earthlink.net/~mrob/pub/math/largenum.html
;;    http://www.unc.edu/~rowlett/units/large.html
;;
;;
;; Japanese Numbers
;; ----------------
;;
;; For large numbers in japanese (above 10,000), see the URL:
;;
;;    http://www.sf.airnet.ne.jp/~ts/japanese/largenumber.html
;;
;; For small numbers in japanese (less than 1,000), see the URL:
;;
;;    http://www.sf.airnet.ne.jp/~ts/japanese/smallnumber.html
;;
;; See also:
;;
;;    http://www.sf.airnet.ne.jp/~ts/japanese/counter.html
;;    http://www.sf.airnet.ne.jp/~ts/japanese/javanumber.html
;;
;;
;; How to Use the Functions From a Shell
;; -------------------------------------
;;
;; Create a script file (let's say currency.sh) containing the following code:
;;
;;    #! /bin/bash
;;
;;    # spell-number.el path
;;    LISPDIR=/usr/local/lib/emacs/site-lisp
;;
;;    cat >$$.data <<EOF
;;    (setq spelln-language 'english-us)
;;    (setq spelln-country 'united-states)
;;    (message (spelln-currency-in-words $*))
;;    EOF
;;
;;    emacs -batch -load $LISPDIR/spell-number.el -load $$.data
;;
;;    rm $$.data
;;
;; So, when you type:
;;
;;    currency.sh 1121.21
;;
;; It's displayed:
;;
;;    one thousand, one hundred twenty-one dollars and twenty-one cents
;;
;;
;; About "and"s and "comma"s
;; -------------------------
;;
;; Some languages (and their variations) have several ways to express a
;; number.  For example, in english we can have:
;;
;; * american english
;;    > 2,765,133,001
;;      1. two billion, seven hundred sixty-five million, one hundred
;;         thirty-three thousand and one
;;      2. two billion, seven hundred sixty-five million, one hundred
;;         thirty-three thousand one
;;      3. two billion seven hundred sixty-five million one hundred
;;         thirty-three thousand and one
;;      4. two billion seven hundred sixty-five million one hundred
;;         thirty-three thousand one
;;    > 960
;;      5. nine hundred and sixty
;;      6. nine hundred sixty
;;
;; * british english
;;    > 2,765,133,001
;;      7. two milliard, seven hundred and sixty five million, one hundred and
;;         thirty three thousand and one
;;      8. two milliard seven hundred and sixty five million one hundred and
;;         thirty three thousand and one
;;    > 960
;;      9. nine hundred and sixty
;;
;; * "european" english (I saw this variation in Spell::Number Perl package)
;;    > 2,765,133,001
;;     10. two thousand, seven hundred sixty five million, one hundred thirty
;;         three thousand one
;;     11. two thousand seven hundred sixty five million one hundred thirty
;;         three thousand one
;;    > 960
;;     12. nine hundred sixty
;;
;; Note that in cases 2, 4 and 6 the "and" is omitted.  And in cases 3, 4, 8
;; and 11 the "comma" is omitted.
;;
;; To accommodate these variations, use the variables:
;;
;; `spelln-and-p'	to indicate if " and " should be spelled.
;;
;; `spelln-comma-p'	to indicate if ", " should be spelled.
;;
;; The default value for both variables above is t, that is, " and " and ", "
;; are spelled.
;;
;;
;; Acknowledgments
;; ---------------
;;
;; Thanks to Kalle Olavi Niemitalo <kon@iki.fi> for finnish and finland
;; corrections.
;;
;; Thanks to Juanma Barranquero <lektu@teleline.es> for spanish corrections, and
;; for zero cents and gender suggestions.
;;
;; Thanks to Florian Weimer <fw@s.netic.de> for german gender corrections.
;;
;; Thanks to Eberhard Burr <Eberhard.Burr@gmx.de> for german corrections.
;;
;; Thanks to Petra Stempfle <?@?> for german contribution.
;;
;; Thanks to Antonio Orlando Faro da Silva <ao@cpqd.com.br> for portuguese (PT)
;; corrections.
;;
;; Thanks to Ailton Bauer Paschoal <?@?> for italian contribution.
;;
;; Thanks to Emmanuel Michon <emmanuel_michon@sdesigns.com>, Christophe Cuq
;; <ccuq@teaser.fr> and John S. Yates Jr <john@everfile.com> for french
;; corrections.
;;
;; Thanks to Luciene Mastrandrea <tucunlu@dglnet.com.br> for french
;; contribution.
;;
;; Thanks to Don Provan <dprovan@ra.lucent.com> for American and British
;; Numbers.
;;
;; Thanks to Franz Zahaurek <fzk@gams.at> for How to Use the Functions From a
;; Shell.
;;
;; Thanks to all who emailed comments and contributions.
;;
;;
;;
;; Fell free to send contributions, suggestions, corrections, new languages,
;; etc. to maintainer.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Options:


(defgroup spelln-number nil
  "Spell out an integer or currency in words"
  :link '(emacs-library-link :tag "Source Lisp File" "spell-number.el")
  :prefix "spelln-"
  :group 'local)


(defcustom spelln-language 'portuguese-br
  "*Specify the language to spell out a number in words.

See also `spelln-language-database'."
  :type '(radio
	  :tag "Language"
	  (const catalan)       (const danish)     (const dutch)
	  (const english-eur)   (const english-gb) (const english-us)
	  (const esperanto)     (const finnish)    (const french-ch)
	  (const french-fr)     (const german)     (const italian)
	  (const japanese)      (const norwegian)  (const portuguese-br)
	  (const portuguese-pt) (const spanish)    (const swedish))
  :group 'spelln-number)


(defcustom spelln-country 'brazil
  "*Specify the country to spell out a currency in words.

See also `spelln-country-database'."
  :type '(radio
	  :tag "Country"
	  (const andorra-french) (const andorra-spanish)
	  (const antigua-and-barbuda)
	  (const argentina)    (const australia)       (const austria)
	  (const bahamas)      (const barbados)        (const belgium)
	  (const belize)       (const benin)           (const bolivia)
	  (const brazil)       (const brunei)
	  (const burkina-faso) (const burundi)         (const cameroon)
	  (const canada)       (const cape-verde)
	  (const central-african-republic)             (const chad)
	  (const chile)        (const colombia)        (const comoros)
	  (const congo)        (const costa-rica)      (const cuba)
	  (const cyprus)       (const denmark)         (const djibouti)
	  (const dominica)     (const dominican-republic)
	  (const ecuador)      (const el-salvador)
	  (const equatorial-guinea)                    (const fiji)
	  (const finland)      (const france)          (const gabon)
	  (const germany)      (const grenada)         (const guatemala)
	  (const guinea)       (const guinea-bissau)   (const guyana)
	  (const haiti)        (const honduras)        (const ireland)
	  (const italy)        (const ivory-coast)     (const jamaica)
	  (const japan)        (const kenya)           (const kiribati)
	  (const liberia)      (const liechtenstein)   (const luxembourg)
	  (const madagascar)   (const mali)            (const mexico)
	  (const monaco)       (const mozambique)      (const namibia)
	  (const nauru)        (const netherlands)     (const new-zealand)
	  (const nicaragua)    (const niger)           (const norway)
	  (const panama)       (const paraguay)        (const peru)
	  (const philippines)  (const portugal)        (const rwanda)
	  (const sao-tome-and-principe)                (const senegal)
	  (const singapore)    (const solomon-islands) (const somalia)
	  (const south-africa) (const spain)
	  (const st-kitts-and-nevis)                   (const st-lucia)
	  (const st-vicent-and-grenadines)             (const sweden)
	  (const switzerland)  (const taiwan)          (const tanzania)
	  (const togo)         (const trinidad-and-tobago)
	  (const tuvalu)       (const uganda)          (const united-kingdom)
	  (const united-states)                        (const uruguay)
	  (const venezuela)    (const zimbabwe))
  :group 'spelln-number)


(defcustom spelln-zero-cents t
  "*Non-nil means that \" and zero cents\" is spelled out.

If cent part is different than zero, it's always spelled out.

It's used in `spelln-currency-in-words' and `spelln-currency-string-in-words'."
  :type 'boolean
  :group 'spelln-number)


(defcustom spelln-and-p t
  "*Non-nil means that and part of `spelln-language-database' is spelled out.

See also `spelln-language-database'.

It's used in `spelln-currency-in-words' and `spelln-currency-string-in-words'."
  :type 'boolean
  :group 'spelln-number)


(defcustom spelln-comma-p t
  "*Non-nil means that comma part of `spelln-language-database' is spelled out.

See also `spelln-language-database'.

It's used in `spelln-currency-in-words' and `spelln-currency-string-in-words'."
  :type 'boolean
  :group 'spelln-number)


(defcustom spelln-gender-default 'masculine
  "*Specify the default gender to be used when there is no neuter gender.

It's used in `spelln-int' and `spelln-str'."
  :type '(radio :tag "Gender"
		(const feminine) (const masculine))
  :group 'spelln-number)


(defcustom spelln-period-character ?.
  "*Specify the character to separate periods.

For example, in the numeric string \"1.000,00\" this variable should be set to
character `.'.

It's used in `spelln-currency-string-in-words' and
`spelln-numeric-string-in-words'."
  :type 'character
  :group 'spelln-number)


(defcustom spelln-decimal-character ?,
  "*Specify the decimal point character.

For example, in the numeric string \"1.000,00\" this variable should be set to
character `,'.

It's used in `spelln-currency-string-in-words'."
  :type 'character
  :group 'spelln-number)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables and Functions:


;; to avoid compilation gripes
(defvar spelln-zero            nil)
(defvar spelln-comma           nil)
(defvar spelln-minus           nil)
(defvar spelln-and             nil)
(defvar spelln-digits-period   3)
(defvar spelln-tens            nil)
(defvar spelln-hundreds        nil)
(defvar spelln-hundreds-tens   nil)
(defvar spelln-singular-period nil)
(defvar spelln-plural-period   nil)
(defvar spelln-extra           nil)
(defvar spelln-currency-extra  nil)
(defvar spelln-gender          1)
(defvar spelln-env-strings     nil)
(defvar spelln-env-separator   0)
(defvar spelln-env-period      0)
(defvar spelln-env-gender      2)


(defun spelln-aref-gender (number)
  (if (vectorp number)
      (or (aref number spelln-env-gender)
	  (aref number spelln-gender))
    number))


(defun spelln-aref-period (vector &optional null-p)
  (if (> spelln-env-period 5)
      (and (not null-p)
	   (format " ?10^%d? " (* spelln-digits-period  spelln-env-period)))
    (aref vector spelln-env-period)))


(defun spelln-aref-gender-period (vector &optional null-p)
  (spelln-aref-gender (spelln-aref-period vector null-p)))


(defun spelln-concat (&rest str-list)
  (setq spelln-env-strings (concat (apply 'concat str-list)
				   spelln-env-strings)))


(defun spelln-concat-power (strnum vector)
  (let ((power  (and (> spelln-env-period 0)
		     vector (spelln-aref-gender-period vector t)))
	(strnum (spelln-aref-gender strnum)))
    (if power
	(spelln-concat (substring strnum 0 (- (length strnum) (aref vector 0)))
		       power)
      (spelln-concat strnum
		     (spelln-aref-gender-period spelln-plural-period)))))


(defun spelln-currency-extra (str)
  (let ((clist spelln-currency-extra)
	extra)
    (save-match-data
      (while clist
	(setq extra (car clist)
	      clist (cdr clist))
	(and (string-match (aref extra 0) str)
	     (setq str (replace-match (aref extra 1) t nil str))))))
  str)


(defun spelln-engine-hundreds (integer &optional period-state)
  ;; initializations & next separator
  (setq integer (mod integer 1000)
	spelln-env-separator
	(cond
	 ((= spelln-env-separator 0)	; null "and"
	  1)
	 ((= spelln-env-separator 1)	; "and"
	  (and spelln-and-p
	       (spelln-concat (if (vectorp spelln-and)
				  (and (> integer 0)
				       (aref spelln-and
					     (if (> spelln-env-period 1)
						 1
					       0)))
				spelln-and)))
	  (if (and (vectorp spelln-and) (<= integer 0))
	      1
	    2))
	 ((= spelln-env-separator 2)	; ","
	  (and spelln-comma-p
	       (spelln-concat spelln-comma))
	  2)
	 ((= spelln-env-separator 3)	; null ","
	  2)))
  ;; hundreds spelling
  (cond
   ((= integer 0)			; period "000"
    (and period-state
	 (spelln-concat (spelln-aref-gender-period (if (= period-state 2)
						       spelln-plural-period
						     spelln-singular-period))))
    (setq spelln-env-separator (if (= spelln-env-separator 1)
				   (if (vectorp spelln-and)
				       1
				     0)	; "000" initial
				 3)))	; "000" intermediate

   ((= integer 1)			; period "001"
    (spelln-concat (spelln-aref-gender-period (if (and period-state
						       (= period-state 2))
						  spelln-plural-period
						spelln-singular-period))))
   (t					; any period  (2..999)
    (let ((tens     (mod integer 100))	; current tens
	  (hundreds (/   integer 100)))	; current hundreds
      (cond
       ;; no extras
       ((null spelln-extra)
	(spelln-concat (spelln-aref-gender (aref spelln-tens tens))
		       (spelln-aref-gender-period spelln-plural-period))
	(and (> hundreds 0)		; hundreds (100..999)
	     (spelln-concat (spelln-aref-gender
			     (aref (if (zerop tens)
				       spelln-hundreds
				     (setq spelln-env-separator 2)
				     spelln-hundreds-tens)
				   hundreds)))))
       ;; extras - only hundreds (100, 200, 300, ... , 900)
       ((zerop tens)
	(spelln-concat-power (aref spelln-hundreds hundreds)
			     (aref spelln-extra 2)))
       ;; extras - for sure tens and/or units (1 .. 99), maybe hundreds
       (t
	(setq spelln-env-separator 2)
					; handle tens and units
	(spelln-concat-power (aref spelln-tens tens)
			     (let ((units (mod tens 10)))
			       (if (zerop units)
				   (aref spelln-extra 1)	       ; tens
				 (aref (aref spelln-extra 0) units)))) ; units
					; handle hundreds
	(and (> hundreds 0)
	     (spelln-concat
	      (spelln-aref-gender (aref spelln-hundreds-tens hundreds)))))))))
  ;; next period
  (setq spelln-env-period (1+ spelln-env-period)))


(defun spelln-engine (integer)
  (spelln-engine-hundreds integer
			  (cond ((= integer 0) nil)
				((= integer 1) 1)
				(t             2)))
  (and (> spelln-digits-period 3)
       (= spelln-env-period 1)
       (setq spelln-env-period 2))
  (and (> (setq integer (/ integer 1000)) 0)
       (let ((spelln-env-period 1))
	 (spelln-engine-hundreds integer))))


(defconst spelln-gender-alist
  '((feminine  . 0)
    (masculine . 1)
    (neuter    . 2))
  "Alist for gender index used by `spelln-int' and `spelln-str'.")


(defun spelln-env-init (gender-sym)
  (setq spelln-env-strings   nil
	spelln-env-period    0
	spelln-env-separator 0
	spelln-env-gender
	(or (cdr (assoc gender-sym spelln-gender-alist))
	    2)))			; neuter


(defun spelln-int (int gender-sym)
  (let ((negative (< int 0))
	(integer (abs int))
	(power (expt 10 spelln-digits-period))
	rest rest-tens rest-units tens units)
    (if (or (null spelln-tens) (zerop integer))
	spelln-zero			; zero
      ;; initializations
      (spelln-env-init gender-sym)
      ;; set default gender index
      (setq spelln-gender (or (cdr (assoc spelln-gender-default
					  spelln-gender-alist))
			      1))	; masculine
      ;; spell out number in words
      (while (> integer 0)
	(setq rest (mod integer power))	; current period
	(spelln-engine rest)
	(setq integer (/ integer power)))
      ;; indicate sign number and trim spaces
      (spelln-sign-and-trim-spaces negative))))


(defun spelln-str (str gender-sym &optional places)
  (let ((len (length str))
	(power10 [1 10 100 1000 10000 100000 1000000])
	(number 0)
	negative start stri integer power)
    (if (or (null spelln-tens) (zerop len))
	nil				; invalid state
      ;; set default gender index
      (setq spelln-gender (or (cdr (assoc spelln-gender-default
					  spelln-gender-alist))
			      1)	; masculine
	    start (cond ((= (aref str 0) ?+) 1)
			((= (aref str 0) ?-) (setq negative t) 1)
			(t                   0)
			))
      ;; skip initial zeros
      (or places
	  (while (and (< start len)
		      (or (= (aref str start) ?0)
			  (= (aref str start) spelln-period-character)))
	    (setq start (1+ start))))
      (if (= start len)
	  (cons 0 spelln-zero)		; zero
	;; initializations
	(spelln-env-init gender-sym)
	;; set index to end of string
	(if (not places)
	    (setq stri  (1- len)
		  power 1)
	  (setq stri start)
	  (while (and (> places 0) (< stri len))
	    ;; skip period separator
	    (while (and (< stri len)
			(= (aref str stri) spelln-period-character))
	      (setq stri (1+ stri)))
	    (setq stri   (1+ stri)
		  places (1- places)))
	  (setq stri  (1- stri)
		power (aref power10 places)))
	;; spell out numeric string in words
	(while (>= stri start)
	  ;; get current period
	  (let ((i 0))
	    (setq integer 0)
	    (while (and (>= stri start) (< i spelln-digits-period))
	      ;; skip period separator
	      (while (and (>= stri start)
			  (= (aref str stri) spelln-period-character))
		(setq stri (1- stri)))
	      (and (>= stri start)
		   (setq integer (+ (* (aref power10 i)
				       (- (aref str stri) ?0))
				    integer)))
	      (setq i    (1+ i)
		    stri (1- stri))))
	  (setq integer (* integer power))
	  ;; spell out current period
	  (spelln-engine integer))
	(cons (cond ((> spelln-env-period 1)  2)  ; above one
		    ((= integer 0)            0)  ; zero
		    ((= integer 1)            1)  ; one
		    (t                        2)) ; above one
	      ;; indicate sign number and trim spaces
	      (spelln-sign-and-trim-spaces negative))))))


(defun spelln-sign-and-trim-spaces (negative)
  (if (or (null spelln-env-strings) (string= spelln-env-strings ""))
      spelln-zero
    ;; indicate sign number
    (and negative
	 (setq spelln-env-strings (concat spelln-minus spelln-env-strings)))
    (let ((from 0)
	  (to   (1- (length spelln-env-strings))))
      ;; trim initial spaces
      (while (= (aref spelln-env-strings from) ?\ )
	(setq from (1+ from)))
      ;; trim trailing spaces
      (while (= (aref spelln-env-strings to) ?\ )
	(setq to (1- to)))
      (substring spelln-env-strings from (1+ to)))))


(defconst spelln-language-database
  '(
    (catalan				; LANGUAGE
     "zero"				; 0 - zero
     ""					; 1 - comma
     "menos"				; 2 - minus
     " i"				; 3 - and
     6					; 4 - digits per period
     [""				; 5 - tens [0-99]
      [" una" " un" nil]    [" dues" " dos" nil] " tres"
      " quatre"             " cinc"              " sis"
      " set"                " vuit"              " nou"
      " deu"
      " onze"               " dotze"             " tretze"
      " catorze"            " quinze"            " setze"
      " disset"             " divuit"            " dinou"
      " vint"
      [" vint-i-una"  " vint-i-un"  nil]
      [" vint-i-dues" " vint-i-dos" nil]         " vint-i-tres"
      " vint-i-quatre"      " vint-i-cinc"       " vint-i-sis"
      " vint-i-set"         " vint-i-vuit"       " vint-i-nou"
      " trenta"
      [" trenta-i-una"  " trenta-i-un"  nil]
      [" trenta-i-dues" " trenta-i-dos" nil]     " trenta-i-tres"
      " trenta-i-cuatro"    " trenta-i-cinco"    " trenta-i-seis"
      " trenta-i-siete"     " trenta-i-ocho"     " trenta-i-nueve"
      " quarenta"
      [" quarenta-i-una"  " quarenta-i-un"  nil]
      [" quarenta-i-dues" " quarenta-i-dos" nil] " quarenta-i-tres"
      " quarenta-i-cuatro"  " quarenta-i-cinco"  " quarenta-i-seis"
      " quarenta-i-siete"   " quarenta-i-ocho"   " quarenta-i-nueve"
      " cinquanta"
      [" cinqanta-i-una"  " cinqanta-i-un"  nil]
      [" cinqanta-i-dues" " cinqanta-i-dos" nil] " cinqanta-i-tres"
      " cinqanta-i-cuatro"  " cinqanta-i-cinco"  " cinqanta-i-seis"
      " cinqanta-i-siete"   " cinqanta-i-ocho"   " cinqanta-i-nueve"
      " seixanta"
      [" seixanta-i-una"  " seixanta-i-un"  nil]
      [" seixanta-i-dues" " seixanta-i-dos" nil] " seixanta-i-tres"
      " seixanta-i-cuatro"  " seixanta-i-cinco"  " seixanta-i-seis"
      " seixanta-i-siete"   " seixanta-i-ocho"   " seixanta-i-nueve"
      " setanta"
      [" setanta-i-una"  " setanta-i-un"  nil]
      [" setanta-i-dues" " setanta-i-dos" nil]   " setanta-i-tres"
      " setanta-i-cuatro"   " setanta-i-cinco"   " setanta-i-seis"
      " setanta-i-siete"    " setanta-i-ocho"    " setanta-i-nueve"
      " vuitanta"
      [" vuitanta-i-una"  " vuitanta-i-un"  nil]
      [" vuitanta-i-dues" " vuitanta-i-dos" nil] " vuitanta-i-tres"
      " vuitanta-i-cuatro"  " vuitanta-i-cinco"  " vuitanta-i-seis"
      " vuitanta-i-siete"   " vuitanta-i-ocho"   " vuitanta-i-nueve"
      " noranta"
      [" noranta-i-una"  " noranta-i-un"  nil]
      [" noranta-i-dues" " noranta-i-dos" nil]   " noranta-i-tres"
      " noranta-i-cuatro"   " noranta-i-cinco"   " noranta-i-seis"
      " noranta-i-siete"    " noranta-i-ocho"    " noranta-i-nueve"]
     [""				; 6 - hundreds [0-9]
      " cent"
      " doscent"
      " trescent"
      " quatrecent"
      " cinccent"
      " siscent"
      " setcent"
      " vuitcent"
      " noucent"]
     [""				; 7 - hundreds-tens [0-9]
      " cent"
      " doscent"
      " trescent"
      " quatrecent"
      " cinccent"
      " siscent"
      " setcent"
      " vuitcent"
      " noucent"]
     [[" una" " un" nil]		; 8 - singular period [0-4]
      " mil"
      " un millón"
      " un billón"
      " un trillón"
      " un quatrillón"]
     [""				; 9 - plural period [0-4]
      " mil"
      " millones"
      " billones"
      " trillones"
      " quatrillones"]
     )
    (danish				; LANGUAGE
     "nul"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " og"				; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " en"                 " to"                 " tre"
      " fire"               " fem"                " seks"
      " syv"                " otte"               " ni"
      " ti"
      " elleve"             " tolv"               " tretten"
      " fjorten"            " femten"             " seksten"
      " sytten"             " atten"              " nitten"
      " tyve"
      " en og tyve"         " to og tyve"         " tre og tyve"
      " fire og tyve"       " fem og tyve"        " seks og tyve"
      " syv og tyve"        " otte og tyve"       " ni og tyve"
      " tredive"
      " en og tredive"      " to og tredive"      " tre og tredive"
      " fire og tredive"    " fem og tredive"     " seks og tredive"
      " syv og tredive"     " otte og tredive"    " ni og tredive"
      " fyrre"
      " en og fyrre"        " to og fyrre"        " tre og fyrre"
      " fire og fyrre"      " fem og fyrre"       " seks og fyrre"
      " syv og fyrre"       " otte og fyrre"      " ni og fyrre"
      " halvtreds"
      " en og halvtreds"    " to og halvtreds"    " tre og halvtreds"
      " fire og halvtreds"  " fem og halvtreds"   " seks og halvtreds"
      " syv og halvtreds"   " otte og halvtreds"  " ni og halvtreds"
      " tres"
      " en og tres"         " to og tres"         " tre og tres"
      " fire og tres"       " fem og tres"        " seks og tres"
      " syv og tres"        " otte og tres"       " ni og tres"
      " halvfjerds"
      " en og halvfjerds"   " to og halvfjerds"   " tre og halvfjerds"
      " fire og halvfjerds" " fem og halvfjerds"  " seks og halvfjerds"
      " syv og halvfjerds"  " otte og halvfjerds" " ni og halvfjerds"
      " firs"
      " en og firs"         " to og firs"         " tre og firs"
      " fire og firs"       " fem og firs"        " seks og firs"
      " syv og firs"        " otte og firs"       " ni og firs"
      " halvfems"
      " en og halvfems"     " to og halvfems"     " tre og halvfems"
      " fire og halvfems"   " fem og halvfems"    " seks og halvfems"
      " syv og halvfems"    " otte og halvfems"   " ni og halvfems"]
     [""				; 6 - hundreds [0-9]
      " et hundred"
      " to hundred"
      " tre hundred"
      " fire hundred"
      " fem hundred"
      " seks hundred"
      " syv hundred"
      " otte hundred"
      " ni hundred"]
     [""				; 7 - hundreds-tens [0-9]
      " et hundred og"
      " to hundred og"
      " tre hundred og"
      " fire hundred og"
      " fem hundred og"
      " seks hundred og"
      " syv hundred og"
      " otte hundred og"
      " ni hundred og"]
     [" en"				; 8 - singular period [0-4]
      " en tusind"
      " en million"
      " en milliard"
      " en billion"
      " en billiard"]
     [""				; 9 - plural period [0-4]
      " tusind"
      " millioner"
      " milliarder"
      " billioner"
      " billiarder"]
     )
    (dutch				; LANGUAGE
     "nul"				; 0 - zero
     ""					; 1 - comma
     "minus "				; 2 - minus
     "en"				; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      ["eene" "een" nil] "twee"           "drie"
      "vier"             "vijf"           "zes"
      "zeven"            "acht"           "negen"
      "tien"
      "elf"              "twaalf"         "dertien"
      "veertien"         "vijftien"       "zestien"
      "zeventien"        "achttien"       "negentien"
      "twintig"
      "eenentwintig"     "tweeentwintig"  "drieentwintig"
      "vierentwintig"    "vijfentwintig"  "zesentwintig"
      "zevenentwintig"   "achtentwintig"  "negenentwintig"
      "dertig"
      "eenendertig"      "tweeendertig"   "drieendertig"
      "vierendertig"     "vijfendertig"   "zesendertig"
      "zevenendertig"    "achtendertig"   "negenendertig"
      "veertig"
      "eenenveertig"     "tweeenveertig"  "drieenveertig"
      "vierenveertig"    "vijfenveertig"  "zesenveertig"
      "zevenenveertig"   "achtenveertig"  "negenenveertig"
      "vijftig"
      "eenenvijftig"     "tweeenvijftig"  "drieenvijftig"
      "vierenvijftig"    "vijfenvijftig"  "zesenvijftig"
      "zevenenvijftig"   "achtenvijftig"  "negenenvijftig"
      "zestig"
      "eenenzestig"      "tweeenzestig"   "drieenzestig"
      "vierenzestig"     "vijfenzestig"   "zesenzestig"
      "zevenenzestig"    "achtenzestig"   "negenenzestig"
      "zeventig"
      "eenenzeventig"    "tweeenzeventig" "drieenzeventig"
      "vierenzeventig"   "vijfenzeventig" "zesenzeventig"
      "zevenenzeventig"  "achtenzeventig" "negenenzeventig"
      "tachtig"
      "eenentachtig"     "tweeentachtig"  "drieentachtig"
      "vierentachtig"    "vijfentachtig"  "zesentachtig"
      "zevenentachtig"   "achtentachtig"  "negenentachtig"
      "negentig"
      "eenennegentig"    "tweeennegentig" "drieennegentig"
      "vierennegentig"   "vijfennegentig" "zesennegentig"
      "zevenennegentig"  "achtennegentig" "negenennegentig"]
     [""				; 6 - hundreds [0-9]
      "eenhonderd"
      "tweehonderd"
      "driehonderd"
      "vierhonderd"
      "vijfhonderd"
      "zeshonderd"
      "zevenhonderd"
      "achthonderd"
      "negenhonderd"]
     [""				; 7 - hundreds-tens [0-9]
      "eenhonderd"
      "tweehonderd"
      "driehonderd"
      "vierhonderd"
      "vijfhonderd"
      "zeshonderd"
      "zevenhonderd"
      "achthonderd"
      "negenhonderd"]
     [["eene" "een" nil]		; 8 - singular period [0-4]
      "eenduizend"
      "een miljoen "
      "een ?? "
      "een ?? "
      "een ?? "]
     [""				; 9 - plural period [0-4]
      "duizend"
      " miljoen "
      " ?? "
      " ?? "
      " ?? "]
     )
    (english-eur			; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " and"				; 3 - and
     6					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " one"           " two"           " three"
      " four"          " five"          " six"
      " seven"         " eight"         " nine"
      " ten"
      " eleven"        " twelve"        " thirteen"
      " fourteen"      " fifteen"       " sixteen"
      " seventeen"     " eighteen"      " nineteen"
      " twenty"
      " twenty one"    " twenty two"    " twenty three"
      " twenty four"   " twenty five"   " twenty six"
      " twenty seven"  " twenty eight"  " twenty nine"
      " thirty"
      " thirty one"    " thirty two"    " thirty three"
      " thirty four"   " thirty five"   " thirty six"
      " thirty seven"  " thirty eight"  " thirty nine"
      " fourty"
      " fourty one"    " fourty two"    " fourty three"
      " fourty four"   " fourty five"   " fourty six"
      " fourty seven"  " fourty eight"  " fourty nine"
      " fifty"
      " fifty one"     " fifty two"     " fifty three"
      " fifty four"    " fifty five"    " fifty six"
      " fifty seven"   " fifty eight"   " fifty nine"
      " sixty"
      " sixty one"     " sixty two"     " sixty three"
      " sixty four"    " sixty five"    " sixty six"
      " sixty seven"   " sixty eight"   " sixty nine"
      " seventy"
      " seventy one"   " seventy two"   " seventy three"
      " seventy four"  " seventy five"  " seventy six"
      " seventy seven" " seventy eight" " seventy nine"
      " eighty"
      " eighty one"    " eighty two"    " eighty three"
      " eighty four"   " eighty five"   " eighty six"
      " eighty seven"  " eighty eight"  " eighty nine"
      " ninety"
      " ninety one"    " ninety two"    " ninety three"
      " ninety four"   " ninety five"   " ninety six"
      " ninety seven"  " ninety eight"  " ninety nine"]
     [""				; 6 - hundreds [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [""				; 7 - hundreds-tens [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [" one"				; 8 - singular period [0-4]
      " one thousand"
      " one million"
      " one billion"
      " one trillion"
      " one quatrillion"]
     [""				; 9 - plural period [0-4]
      " thousand"
      " million"
      " billion"
      " trillion"
      " quatrillion"]
     )
    (english-gb				; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " and"				; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " one"           " two"           " three"
      " four"          " five"          " six"
      " seven"         " eight"         " nine"
      " ten"
      " eleven"        " twelve"        " thirteen"
      " fourteen"      " fifteen"       " sixteen"
      " seventeen"     " eighteen"      " nineteen"
      " twenty"
      " twenty one"    " twenty two"    " twenty three"
      " twenty four"   " twenty five"   " twenty six"
      " twenty seven"  " twenty eight"  " twenty nine"
      " thirty"
      " thirty one"    " thirty two"    " thirty three"
      " thirty four"   " thirty five"   " thirty six"
      " thirty seven"  " thirty eight"  " thirty nine"
      " fourty"
      " fourty one"    " fourty two"    " fourty three"
      " fourty four"   " fourty five"   " fourty six"
      " fourty seven"  " fourty eight"  " fourty nine"
      " fifty"
      " fifty one"     " fifty two"     " fifty three"
      " fifty four"    " fifty five"    " fifty six"
      " fifty seven"   " fifty eight"   " fifty nine"
      " sixty"
      " sixty one"     " sixty two"     " sixty three"
      " sixty four"    " sixty five"    " sixty six"
      " sixty seven"   " sixty eight"   " sixty nine"
      " seventy"
      " seventy one"   " seventy two"   " seventy three"
      " seventy four"  " seventy five"  " seventy six"
      " seventy seven" " seventy eight" " seventy nine"
      " eighty"
      " eighty one"    " eighty two"    " eighty three"
      " eighty four"   " eighty five"   " eighty six"
      " eighty seven"  " eighty eight"  " eighty nine"
      " ninety"
      " ninety one"    " ninety two"    " ninety three"
      " ninety four"   " ninety five"   " ninety six"
      " ninety seven"  " ninety eight"  " ninety nine"]
     [""				; 6 - hundreds [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [""				; 7 - hundreds-tens [0-9]
      " one hundred and"
      " two hundred and"
      " three hundred and"
      " four hundred and"
      " five hundred and"
      " six hundred and"
      " seven hundred and"
      " eight hundred and"
      " nine hundred and"]
     [" one"				; 8 - singular period [0-4]
      " one thousand"
      " one million"
      " one milliard"
      " one billion"
      " one billiard"]
     [""				; 9 - plural period [0-4]
      " thousand"
      " million"
      " milliard"
      " billion"
      " billiard"]
     )
    (english-us				; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " and"				; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " one"           " two"           " three"
      " four"          " five"          " six"
      " seven"         " eight"         " nine"
      " ten"
      " eleven"        " twelve"        " thirteen"
      " fourteen"      " fifteen"       " sixteen"
      " seventeen"     " eighteen"      " nineteen"
      " twenty"
      " twenty-one"    " twenty-two"    " twenty-three"
      " twenty-four"   " twenty-five"   " twenty-six"
      " twenty-seven"  " twenty-eight"  " twenty-nine"
      " thirty"
      " thirty-one"    " thirty-two"    " thirty-three"
      " thirty-four"   " thirty-five"   " thirty-six"
      " thirty-seven"  " thirty-eight"  " thirty-nine"
      " fourty"
      " fourty-one"    " fourty-two"    " fourty-three"
      " fourty-four"   " fourty-five"   " fourty-six"
      " fourty-seven"  " fourty-eight"  " fourty-nine"
      " fifty"
      " fifty-one"     " fifty-two"     " fifty-three"
      " fifty-four"    " fifty-five"    " fifty-six"
      " fifty-seven"   " fifty-eight"   " fifty-nine"
      " sixty"
      " sixty-one"     " sixty-two"     " sixty-three"
      " sixty-four"    " sixty-five"    " sixty-six"
      " sixty-seven"   " sixty-eight"   " sixty-nine"
      " seventy"
      " seventy-one"   " seventy-two"   " seventy-three"
      " seventy-four"  " seventy-five"  " seventy-six"
      " seventy-seven" " seventy-eight" " seventy-nine"
      " eighty"
      " eighty-one"    " eighty-two"    " eighty-three"
      " eighty-four"   " eighty-five"   " eighty-six"
      " eighty-seven"  " eighty-eight"  " eighty-nine"
      " ninety"
      " ninety-one"    " ninety-two"    " ninety-three"
      " ninety-four"   " ninety-five"   " ninety-six"
      " ninety-seven"  " ninety-eight"  " ninety-nine"]
     [""				; 6 - hundreds [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [""				; 7 - hundreds-tens [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [" one"				; 8 - singular period [0-4]
      " one thousand"
      " one million"
      " one billion"
      " one trillion"
      " one quatrillion"]
     [""				; 9 - plural period [0-4]
      " thousand"
      " million"
      " billion"
      " trillion"
      " quatrillion"]
     )
    ;; it's used char `ü' in `naü' (9) because there is no proper character
    ;; representation.
    (esperanto				; LANGUAGE
     "nulo"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     ""					; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " unu"          " du"           " tri"
      " kvar"         " kvin"         " ses"
      " sep"          " ok"           " naü"
      " dek"
      " dek unu"      " dek du"       " dek tri"
      " dek kvar"     " dek kvin"     " dek ses"
      " dek sep"      " dek ok"       " dek naü"
      " dudek"
      " dudek unu"    " dudek du"     " dudek tri"
      " dudek kvar"   " dudek kvin"   " dudek ses"
      " dudek sep"    " dudek ok"     " dudek naü"
      " tridek"
      " tridek unu"   " tridek du"    " tridek tri"
      " tridek kvar"  " tridek kvin"  " tridek ses"
      " tridek sep"   " tridek ok"    " tridek naü"
      " kvardek"
      " kvardek unu"  " kvardek du"   " kvardek tri"
      " kvardek kvar" " kvardek kvin" " kvardek ses"
      " kvardek sep"  " kvardek ok"   " kvardek naü"
      " kvindek"
      " kvindek unu"  " kvindek du"   " kvindek tri"
      " kvindek kvar" " kvindek kvin" " kvindek ses"
      " kvindek sep"  " kvindek ok"   " kvindek naü"
      " sesdek"
      " sesdek unu"   " sesdek du"    " sesdek tri"
      " sesdek kvar"  " sesdek kvin"  " sesdek ses"
      " sesdek sep"   " sesdek ok"    " sesdek naü"
      " sepdek"
      " sepdek unu"   " sepdek du"    " sepdek tri"
      " sepdek kvar"  " sepdek kvin"  " sepdek ses"
      " sepdek sep"   " sepdek ok"    " sepdek naü"
      " okdek"
      " okdek unu"    " okdek du"     " okdek tri"
      " okdek kvar"   " okdek kvin"   " okdek ses"
      " okdek sep"    " okdek ok"     " okdek naü"
      " naüdek"
      " naüdek unu"   " naüdek du"    " naüdek tri"
      " naüdek kvar"  " naüdek kvin"  " naüdek ses"
      " naüdek sep"   " naüdek ok"    " naüdek naü"]
     [""				; 6 - hundreds [0-9]
      " cent"
      " ducent"
      " tricent"
      " kvarcent"
      " kvincent"
      " sescent"
      " sepcent"
      " okcent"
      " naücent"]
     [""				; 7 - hundreds-tens [0-9]
      " cent"
      " ducent"
      " tricent"
      " kvarcent"
      " kvincent"
      " sescent"
      " sepcent"
      " okcent"
      " naücent"]
     [" unu"				; 8 - singular period [0-4]
      " mil"
      " miliono"
      " miliardo"
      " duiliono"
      " duiliardo"]
     [""				; 9 - plural period [0-4]
      " mil"
      " milionoj"
      " miliardoj"
      " duilionoj"
      " duiliardoj"]
     )
    (finnish				; LANGUAGE
     "nolla"				; 0 - zero
     ""					; 1 - comma
     " miinus "				; 2 - minus
     ""					; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      "yksi"                   "kaksi"                  "kolme"
      "neljä"                  "viisi"                  "kuusi"
      "seitsemän"              "kahdeksan"              "yhdeksän"
      "kymmenen"
      "yksitoista"             "kaksitoista"            "kolmetoista"
      "neljätoista"            "viisitoista"            "kuusitoista"
      "seitsemäntoista"        "kahdeksantoista"        "yhdeksäntoista"
      "kaksikymmentä"
      "kaksikymmentäyksi"      "kaksikymmentäkaksi"     "kaksikymmentäkolme"
      "kaksikymmentäneljä"     "kaksikymmentäviisi"     "kaksikymmentäkuusi"
      "kaksikymmentäseitsemän" "kaksikymmentäkahdeksan"
      "kaksikymmentäyhdeksän"
      "kolmekymmentä"
      "kolmekymmentäyksi"      "kolmekymmentäkaksi"     "kolmekymmentäkolme"
      "kolmekymmentäneljä"     "kolmekymmentäviisi"     "kolmekymmentäkuusi"
      "kolmekymmentäseitsemän" "kolmekymmentäkahdeksan"
      "kolmekymmentäyhdeksän"
      "neljäkymmentä"
      "neljäkymmentäyksi"      "neljäkymmentäkaksi"     "neljäkymmentäkolme"
      "neljäkymmentäneljä"     "neljäkymmentäviisi"     "neljäkymmentäkuusi"
      "neljäkymmentäseitsemän" "neljäkymmentäkahdeksan"
      "neljäkymmentäyhdeksän"
      "viisikymmentä"
      "viisikymmentäyksi"      "viisikymmentäkaksi"     "viisikymmentäkolme"
      "viisikymmentäneljä"     "viisikymmentäviisi"     "viisikymmentäkuusi"
      "viisikymmentäseitsemän" "viisikymmentäkahdeksan"
      "viisikymmentäyhdeksän"
      "kuusikymmentä"
      "kuusikymmentäyksi"      "kuusikymmentäkaksi"     "kuusikymmentäkolme"
      "kuusikymmentäneljä"     "kuusikymmentäviisi"     "kuusikymmentäkuusi"
      "kuusikymmentäseitsemän" "kuusikymmentäkahdeksan"
      "kuusikymmentäyhdeksän"
      "seitsemänkymmentä"
      "seitsemänkymmentäyksi"  "seitsemänkymmentäkaksi"
      "seitsemänkymmentäkolme"
      "seitsemänkymmentäneljä" "seitsemänkymmentäviisi"
      "seitsemänkymmentäkuusi"
      "seitsemänkymmentäseitsemän" "seitsemänkymmentäkahdeksan"
      "seitsemänkymmentäyhdeksän"
      "kahdeksankymmentä"
      "kahdeksankymmentäyksi"   "kahdeksankymmentäkaksi"
      "kahdeksankymmentäkolme"
      "kahdeksankymmentäneljä"  "kahdeksankymmentäviisi"
      "kahdeksankymmentäkuusi"
      "kahdeksankymmentäseitsemän" "kahdeksankymmentäkahdeksan"
      "kahdeksankymmentäyhdeksän"
      "yhdeksänkymmentä"
      "yhdeksänkymmentäyksi"  "yhdeksänkymmentäkaksi" "yhdeksänkymmentäkolme"
      "yhdeksänkymmentäneljä" "yhdeksänkymmentäviisi" "yhdeksänkymmentäkuusi"
      "yhdeksänkymmentäseitsemän" "yhdeksänkymmentäkahdeksan"
      "yhdeksänkymmentäyhdeksän"]
     [""				; 6 - hundreds [0-9]
      "sata"
      "kaksisataa"
      "kolmesataa"
      "neljäsataa"
      "viisisataa"
      "kuusisataa"
      "seitsemänsataa"
      "kahdeksansataa"
      "yhdeksänsataa"]
     [""				; 7 - hundreds-tens [0-9]
      "sata"
      "kaksisataa"
      "kolmesataa"
      "neljäsataa"
      "viisisataa"
      "kuusisataa"
      "seitsemänsataa"
      "kahdeksansataa"
      "yhdeksänsataa"]
     ["yksi"				; 8 - singular period [0-4]
      "tuhat"
      "miljoona"
      "??"
      "??"
      "??"]
     [""				; 9 - plural period [0-4]
      "tuhatta"
      "miljoonaa"
      "??"
      "??"
      "??"]
     )
    (french-ch				; LANGUAGE
     "zéro"				; 0 - zero
     ","				; 1 - comma
     "moins"				; 2 - minus
     ""					; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " un"               " deux"           " trois"
      " quatre"           " cinq"           " six"
      " sept"             " huit"           " neuf"
      " dix"
      " onze"             " douze"          " treize"
      " quatorze"         " quinze"         " seize"
      " dix-sept"         " dix-huit"       " dix-neuf"
      " vingt"
      " vingt et un"      " vingt-deux"     " vingt-trois"
      " vingt-quatre"     " vingt-cinq"     " vingt-six"
      " vingt-sept"       " vingt-huit"     " vingt-neuf"
      " trente"
      " trente et un"     " trente-deux"    " trente-trois"
      " trente-quatre"    " trente-cinq"    " trente-six"
      " trente-sept"      " trente-huit"    " trente-neuf"
      " quarante"
      " quarante et un"   " quarante-deux"  " quarante-trois"
      " quarante-quatre"  " quarante-cinq"  " quarante-six"
      " quarante-sept"    " quarante-huit"  " quarante-neuf"
      " cinquente"
      " cinquante et un"  " cinquante-deux" " cinquante-trois"
      " cinquante-quatre" " cinquante-cinq" " cinquante-six"
      " cinquante-sept"   " cinquante-huit" " cinquante-neuf"
      " soixante"
      " soixante et un"   " soixante-deux"  " soixante-trois"
      " soixante-quatre"  " soixante-cinq"  " soixante-six"
      " soixante-sept"    " soixante-huit"  " soixante-neuf"
      " septante"
      " septante et un"   " septante-deux"  " septante-trois"
      " septante-quatre"  " septante-cinq"  " septante-six"
      " septante-sept"    " septante-huit"  " septante-neuf"
      " octante"
      " octante et un"    " octante-deux"   " octante-trois"
      " octante-quatre"   " octante-cinq"   " octante-six"
      " octante-sept"     " octante-huit"   " octante-neuf"
      " nonante"
      " nonante et un"    " nonante-deux"   " nonante-trois"
      " nonante-quatre"   " nonante-cinq"   " nonante-six"
      " nonante-sept"     " nonante-huit"   " nonante-neuf"]
     [""				; 6 - hundreds [0-9]
      " cent"
      " deux cents"
      " trois cents"
      " quatre cents"
      " cinq cents"
      " six cents"
      " sept cents"
      " huit cents"
      " neuf cents"]
     [""				; 7 - hundreds-tens [0-9]
      " cent"
      " deux cent"
      " trois cent"
      " quatre cent"
      " cinq cent"
      " six cent"
      " sept cent"
      " huit cent"
      " neuf cent"]
     [" un"				; 8 - singular period [0-4]
      " mille"
      " un million"
      " un milliard"
      " un billion"
      " un billiard"]
     [""				; 9 - plural period [0-4]
      " mille"
      " millions"
      " milliards"
      " billions"
      " billiards"]
     )
    (french-fr				; LANGUAGE
     "zéro"				; 0 - zero
     ","				; 1 - comma
     "moins"				; 2 - minus
     ""					; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " un"                    " deux"                  " trois"
      " quatre"                " cinq"                  " six"
      " sept"                  " huit"                  " neuf"
      " dix"
      " onze"                  " douze"                 " treize"
      " quatorze"              " quinze"                " seize"
      " dix-sept"              " dix-huit"              " dix-neuf"
      " vingt"
      " vingt et un"           " vingt-deux"            " vingt-trois"
      " vingt-quatre"          " vingt-cinq"            " vingt-six"
      " vingt-sept"            " vingt-huit"            " vingt-neuf"
      " trente"
      " trente et un"          " trente-deux"           " trente-trois"
      " trente-quatre"         " trente-cinq"           " trente-six"
      " trente-sept"           " trente-huit"           " trente-neuf"
      " quarante"
      " quarante et un"        " quarante-deux"         " quarante-trois"
      " quarante-quatre"       " quarante-cinq"         " quarante-six"
      " quarante-sept"         " quarante-huit"         " quarante-neuf"
      " cinquente"
      " cinquante et un"       " cinquante-deux"        " cinquante-trois"
      " cinquante-quatre"      " cinquante-cinq"        " cinquante-six"
      " cinquante-sept"        " cinquante-huit"        " cinquante-neuf"
      " soixante"
      " soixante et un"        " soixante-deux"         " soixante-trois"
      " soixante-quatre"       " soixante-cinq"         " soixante-six"
      " soixante-sept"         " soixante-huit"         " soixante-neuf"
      " soixante-dix"
      " soixante et onze"      " soixante-douze"        " soixante-treize"
      " soixante-quatorze"     " soixante-quinze"       " soixante-seize"
      " soixante-dix-sept"     " soixante-dix-huit"     " soixante-dix-neuf"
      " quatre-vingts"
      " quatre-vingt-un"       " quatre-vingt-deux"     " quatre-vingt-trois"
      " quatre-vingt-quatre"   " quatre-vingt-cinq"     " quatre-vingt-six"
      " quatre-vingt-sept"     " quatre-vingt-huit"     " quatre-vingt-neuf"
      " quatre-vingt-dix"
      " quatre-vingt-onze"     " quatre-vingt-douze"    " quatre-vingt-treize"
      " quatre-vingt-quatorze" " quatre-vingt-quinze"   " quatre-vingt-seize"
      " quatre-vingt-dix-sept" " quatre-vingt-dix-huit"
      " quatre-vingt-dix-neuf"]
     [""				; 6 - hundreds [0-9]
      " cent"
      " deux cents"
      " trois cents"
      " quatre cents"
      " cinq cents"
      " six cents"
      " sept cents"
      " huit cents"
      " neuf cents"]
     [""				; 7 - hundreds-tens [0-9]
      " cent"
      " deux cent"
      " trois cent"
      " quatre cent"
      " cinq cent"
      " six cent"
      " sept cent"
      " huit cent"
      " neuf cent"]
     [" un"				; 8 - singular period [0-4]
      " mille"
      " un million"
      " un milliard"
      " un billion"
      " un billiard"]
     [""				; 9 - plural period [0-4]
      " mille"
      " millions"
      " milliards"
      " billions"
      " billiards"]
     )
    (german				; LANGUAGE
     "null"				; 0 - zero
     ""					; 1 - comma
     "minus "				; 2 - minus
     ["und" "und "]			; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      ["eine" "ein" "eins"] "zwei"            "drei"
      "vier"                "fünf"            "sechs"
      "sieben"              "acht"            "neun"
      "zehn"
      "elf"                 "zwölf"           "dreizehn"
      "vierzehn"            "fünfzehn"        "sechzehn"
      "siebzehn"            "achtzehn"        "neunzehn"
      "zwanzig"
      "einundzwanzig"       "zweiundzwanzig"  "dreiundzwanzig"
      "vierundzwanzig"      "fünfundzwanzig"  "sechsundzwanzig"
      "siebenundzwanzig"    "achtundzwanzig"  "neunundzwanzig"
      "dreissig"
      "einunddreissig"      "zweiunddreissig" "dreiunddreissig"
      "vierunddreissig"     "fünfunddreissig" "sechsunddreissig"
      "siebenunddreissig"   "achtunddreissig" "neununddreissig"
      "vierzig"
      "einundvierzig"       "zweiundvierzig"  "dreiundvierzig"
      "vierundvierzig"      "fünfundvierzig"  "sechsundvierzig"
      "siebenundvierzig"    "achtundvierzig"  "neunundvierzig"
      "fünfzig"
      "einundfünfzig"       "zweiundfünfzig"  "dreiundfünfzig"
      "vierundfünfzig"      "fünfundfünfzig"  "sechsundfünfzig"
      "siebenundfünfzig"    "achtundfünfzig"  "neunundfünfzig"
      "sechzig"
      "einundsechzig"       "zweiundsechzig"  "dreiundsechzig"
      "vierundsechzig"      "fünfundsechzig"  "sechsundsechzig"
      "siebenundsechzig"    "achtundsechzig"  "neunundsechzig"
      "siebzig"
      "einundsiebzig"       "zweiundsiebzig"  "dreiundsiebzig"
      "vierundsiebzig"      "fünfundsiebzig"  "sechsundsiebzig"
      "siebenundsiebzig"    "achtundsiebzig"  "neunundsiebzig"
      "achtzig"
      "einundachtzig"       "zweiundachtzig"  "dreiundachtzig"
      "vierundachtzig"      "fünfundachtzig"  "sechsundachtzig"
      "siebenundachtzig"    "achtundachtzig"  "neunundachtzig"
      "neunzig"
      "einundneunzig"       "zweiundneunzig"  "dreiundneunzig"
      "vierundneunzig"      "fünfundneunzig"  "sechsundneunzig"
      "siebenundneunzig"    "achtundneunzig"  "neunundneunzig"]
     [""				; 6 - hundreds [0-9]
      "einhundert"
      "zweihundert"
      "dreihundert"
      "vierhundert"
      "fünfhundert"
      "sechshundert"
      "siebenhundert"
      "achthundert"
      "neunhundert"]
     [""				; 7 - hundreds-tens [0-9]
      "einhundertund"
      "zweihundertund"
      "dreihundertund"
      "vierhundertund"
      "fünfhundertund"
      "sechshundertund"
      "siebenhundertund"
      "achthundertund"
      "neunhundertund"]
     [["eine" "ein" "eins"]		; 8 - singular period [0-4]
      "eintausend"
      "eine Million "
      "eine Milliarde "
      "eine Billion "
      "eine Billiarde "]
     [""				; 9 - plural period [0-4]
      "tausend"
      " Millionen "
      " Milliarden "
      " Billionen "
      " Billiarden "]
     )
    (italian				; LANGUAGE
     "zero"				; 0 - zero
     ""					; 1 - comma
     "meno "				; 2 - minus
     ""					; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      "uno"              "due"             "tre"
      "quattro"          "cinque"          "sei"
      "sette"            "otto"            "nove"
      "diece"
      "undici"           "dodici"          "tredici"
      "quattordici"      "quindici"        "sedici"
      "diciasette"       "diciotto"        "dicianove"
      "venti"
      "ventuno"          "ventidue"        "ventitre"
      "ventiquattro"     "venticinque"     "ventisei"
      "ventisette"       "ventotto"        "ventinove"
      "trenta"
      "trentuno"         "trentadue"       "trentatre"
      "trentaquattro"    "trentacinque"    "trentasei"
      "trentasette"      "trentotto"       "trentanove"
      "quaranta"
      "quarantuno"       "quarantadue"     "quarantatre"
      "quarantaquattro"  "quarantacinque"  "quarantasei"
      "quarantasette"    "quarantotto"     "quarantanove"
      "cinquanta"
      "cinquantuno"      "cinquantadue"    "cinquantatre"
      "cinquantaquattro" "cinquantacinque" "cinquantasei"
      "cinquantasette"   "cinquantotto"    "cinquantanove"
      "sessanta"
      "sessantuno"       "sessantadue"     "sessantatre"
      "sessantaquattro"  "sessantacinque"  "sessantasei"
      "sessantasette"    "sessantotto"     "sessantanove"
      "settanta"
      "settantuno"       "settantadue"     "settantatre"
      "settantaquattro"  "settantacinque"  "settantasei"
      "settantasette"    "settantotto"     "settantanove"
      "ottanta"
      "ottantuno"        "ottantadue"      "ottantatre"
      "ottantaquattro"   "ottantacinque"   "ottantasei"
      "ottantasette"     "ottantotto"      "ottantanove"
      "novanta"
      "novantuno"        "novantadue"      "novantatre"
      "novantaquattro"   "novantacinque"   "novantasei"
      "novantasette"     "novantotto"      "novantanove"]
     [""				; 6 - hundreds [0-9]
      "cento"
      "duecento"
      "trecento"
      "quattrocento"
      "cinquecento"
      "seicento"
      "settecento"
      "ottocento"
      "novecento"]
     [""				; 7 - hundreds-tens [0-9]
      "cento"
      "duecento"
      "trecento"
      "quattrocento"
      "cinquecento"
      "seicento"
      "settecento"
      "ottocento"
      "novecento"]
     ["uno"				; 8 - singular period [0-4]
      "mille"
      "un milione "
      "un miliardo "
      "un billón "
      "un billardo "]
     [""				; 9 - plural period [0-4]
      "mila"
      " milione "
      " miliardo "
      " billón "
      " billardo "]
     )
    (japanese				; LANGUAGE
     "rei"				; 0 - zero
     ""					; 1 - comma
     "??"				; 2 - minus
     ""					; 3 - and
     4					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " ichi"           " ni"              " san"
      " yon"            " go"              " roku"
      " nana"           " hachi"           " kyû"
      " jû"
      " jû ichi"       " jû ni"          " jû san"
      " jû yon"        " jû go"          " jû roku"
      " jû nana"       " jû hachi"       " jû kyû"
      " nijû"
      " nijû ichi"    " nijû ni"       " nijû san"
      " nijû yon"     " nijû go"       " nijû roku"
      " nijû nana"    " nijû hachi"    " nijû kyû"
      " sanjû"
      " sanjû ichi"   " sanjû ni"      " sanjû san"
      " sanjû yon"    " sanjû go"      " sanjû roku"
      " sanjû nana"   " sanjû hachi"   " sanjû kyû"
      " yonjû"
      " yonjû ichi"   " yonjû ni"      " yonjû san"
      " yonjû yon"    " yonjû go"      " yonjû roku"
      " yonjû nana"   " yonjû hachi"   " yonjû kyû"
      " gojû"
      " gojû ichi"    " gojû ni"       " gojû san"
      " gojû yon"     " gojû go"       " gojû roku"
      " gojû nana"    " gojû hachi"    " gojû kyû"
      " rokujû"
      " rokujû ichi"  " rokujû ni"     " rokujû san"
      " rokujû yon"   " rokujû go"     " rokujû roku"
      " rokujû nana"  " rokujû hachi"  " rokujû kyû"
      " nanajû"
      " nanajû ichi"  " nanajû ni"     " nanajû san"
      " nanajû yon"   " nanajû go"     " nanajû roku"
      " nanajû nana"  " nanajû hachi"  " nanajû kyû"
      " hachijû"
      " hachijû ichi" " hachijû ni"    " hachijû san"
      " hachijû yon"  " hachijû go"    " hachijû roku"
      " hachijû nana" " hachijû hachi" " hachijû kyû"
      " kyûjû"
      " kyûjû ichi"  " kyûjû ni"     " kyûjû san"
      " kyûjû yon"   " kyûjû go"     " kyûjû roku"
      " kyûjû nana"  " kyûjû hachi"  " kyûjû kyû"]
     [""				; 6 - hundreds [0-9]
      " hyaku"
      " nihyaku"
      " sambyaku"
      " yonhyaku"
      " gohyaku"
      " roppyaku"
      " nanahyaku"
      " happyaku"
      " kyûhyaku"]
     [""				; 7 - hundreds-tens [0-9]
      " hyaku"
      " nihyaku"
      " sambyaku"
      " yonhyaku"
      " gohyaku"
      " roppyaku"
      " nanahyaku"
      " happyaku"
      " kyûhyaku"]
     [" ichi"				; 8 - singular period [0-4]
      " sen"
      " ichiman"
      " ichioku"
      " itchô"
      " ikkei"]
     [""				; 9 - plural period [0-4]
      "sen"
      "man"
      "oku"
      "chô"
      "kei"]
     [					; 10 - extras
      ;; 10^3      10^4     10^8 10^12    10^16
      ;; sen       man      oku  chô      kei
      ;; 0 - units
      [	;; 0 - rei
       nil
       ;; 1 - ichi
       [4 "issen"  nil      nil  "itchô"  "ikkei"]
       ;; 2 - ni
       nil
       ;; 3 - san
       [3 "sanzen" "samman" nil  nil      "sankei"]
       ;; 4 - yon
       [3 nil      "yomman" nil  nil      nil]
       ;; 5 - go
       nil
       ;; 6 - roku
       [4 nil      nil      nil  nil      "rokkei"]
       ;; 7 - nana
       nil
       ;; 8 - hachi
       [5 "hassen" nil      nil  "hatchô" "hakkei"]
       ;; 9 - kyû
       nil]
      ;; 1 - tens - jû
      [2 "jussen"  nil      nil  "jutchô" "jukkei"]
      ;; 2 - hundreds - hyaku
      [3 nil       nil      nil  nil      "hyakkei"]
      ;; 3 - thousands - sen
      [3 nil       "semman" nil  nil      nil]]
     )
    (norwegian				; LANGUAGE
     "null"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " og"				; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " en"         " to"         " tre"
      " fire"       " fem"        " seks"
      " syv"        " åtte"       " ni"
      " ti"
      " elleve"     " tolv"       " tretten"
      " fjorten"    " femten"     " seksten"
      " søtten"     " atten"      " nitten"
      " tyve"
      " tjueen"     " tjueto"     " tjuetre"
      " tjuefire"   " tjuefem"    " tjueseks"
      " tjuesyv"    " tjueåtte"   " tjueni"
      " tretti"
      " trettien"   " trettito"   " trettitre"
      " trettifire" " trettifem"  " trettiseks"
      " trettisyv"  " trettiåtte" " trettini"
      " førti"
      " førtien"    " førtito"    " førtitre"
      " førtifire"  " førtifem"   " førtiseks"
      " førtisyv"   " førtiåtte"  " førtini"
      " femti"
      " femtien"    " femtito"    " femtitre"
      " femtifire"  " femtifem"   " femtiseks"
      " femtisyv"   " femtiåtte"  " femtini"
      " seksti"
      " sekstien"   " sekstito"   " sekstitre"
      " sekstifire" " sekstifem"  " sekstiseks"
      " sekstisyv"  " sekstiåtte" " sekstini"
      " sytti"
      " syttien"    " syttito"    " syttitre"
      " syttifire"  " syttifem"   " syttiseks"
      " syttisyv"   " syttiåtte"  " syttini"
      " åtti"
      " åttien"     " åttito"     " åttitre"
      " åttifire"   " åttifem"    " åttiseks"
      " åttisyv"    " åttiåtte"   " åttini"
      " nitti"
      " nittien"    " nittito"    " nittitre"
      " nittifire"  " nittifem"   " nittiseks"
      " nittisyv"   " nittiåtte"  " nittini"]
     [""				; 6 - hundreds [0-9]
      " ett hundre"
      " to hundre"
      " tre hundre"
      " fire hundre"
      " fem hundre"
      " seks hundre"
      " syv hundre"
      " åtte hundre"
      " ni hundre"]
     [""				; 7 - hundreds-tens [0-9]
      " ett hundre og"
      " to hundre og"
      " tre hundre og"
      " fire hundre og"
      " fem hundre og"
      " seks hundre og"
      " syv hundre og"
      " åtte hundre og"
      " ni hundre og"]
     [" ett"				; 8 - singular period [0-4]
      " ett tusen"
      " en million"
      " en milliard"
      " en billion"
      " en billiard"]
     [""				; 9 - plural period [0-4]
      " tusen"
      " millioner"
      " milliarder"
      " billioner"
      " billiarder"]
     )
    (portuguese-br			; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "menos"				; 2 - minus
     " e"				; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      [" uma" " um" nil]    [" duas" " dois" nil]   " três"
      " quatro"             " cinco"                " seis"
      " sete"               " oito"                 " nove"
      " dez"
      " onze"               " doze"                 " treze"
      " quatorze"           " quinze"               " dezesseis"
      " dezessete"          " dezoito"              " dezenove"
      " vinte"
      [" vinte e uma"  " vinte e um"   nil]
      [" vinte e duas" " vinte e dois" nil]         " vinte e três"
      " vinte e quatro"     " vinte e cinco"        " vinte e seis"
      " vinte e sete"       " vinte e oito"         " vinte e nove"
      " trinta"
      [" trinta e uma"  " trinta e um"   nil]
      [" trinta e duas" " trinta e dois" nil]       " trinta e três"
      " trinta e quatro"    " trinta e cinco"       " trinta e seis"
      " trinta e sete"      " trinta e oito"        " trinta e nove"
      " quarenta"
      [" quarenta e uma"  " quarenta e um"   nil]
      [" quarenta e duas" " quarenta e dois" nil]   " quarenta e três"
      " quarenta e quatro"  " quarenta e cinco"     " quarenta e seis"
      " quarenta e sete"    " quarenta e oito"      " quarenta e nove"
      " cinqüenta"
      [" cinqüenta e uma"  " cinqüenta e um"   nil]
      [" cinqüenta e duas" " cinqüenta e dois" nil] " cinqüenta e três"
      " cinqüenta e quatro" " cinqüenta e cinco"    " cinqüenta e seis"
      " cinqüenta e sete"   " cinqüenta e oito"     " cinqüenta e nove"
      " sessenta"
      [" sessenta e uma" " sessenta e um" nil]
      [" sessenta e duas" " sessenta e dois" nil]   " sessenta e três"
      " sessenta e quatro"  " sessenta e cinco"     " sessenta e seis"
      " sessenta e sete"    " sessenta e oito"      " sessenta e nove"
      " setenta"
      [" setenta e uma"  " setenta e um"   nil]
      [" setenta e duas" " setenta e dois" nil]     " setenta e três"
      " setenta e quatro"   " setenta e cinco"      " setenta e seis"
      " setenta e sete"     " setenta e oito"       " setenta e nove"
      " oitenta"
      [" oitenta e uma"  " oitenta e um"   nil]
      [" oitenta e duas" " oitenta e dois" nil]     " oitenta e três"
      " oitenta e quatro"   " oitenta e cinco"      " oitenta e seis"
      " oitenta e sete"     " oitenta e oito"       " oitenta e nove"
      " noventa"
      [" noventa e uma"  " noventa e um"   nil]
      [" noventa e duas" " noventa e dois" nil]     " noventa e três"
      " noventa e quatro"   " noventa e cinco"      " noventa e seis"
      " noventa e sete"     " noventa e oito"       " noventa e nove"]
     [""				; 6 - hundreds [0-9]
      " cem"
      [" duzentas"     " duzentos"     nil]
      [" trezentas"    " trezentos"    nil]
      [" quatrocentas" " quatrocentos" nil]
      [" quinhentas"   " quinhentos"   nil]
      [" seiscentas"   " seiscentos"   nil]
      [" setecentas"   " setecentos"   nil]
      [" oitocentas"   " oitocentos"   nil]
      [" novecentas"   " novecentos"   nil]]
     [""				; 7 - hundreds-tens [0-9]
      " cento e"
      [" duzentas e"     " duzentos e"     nil]
      [" trezentas e"    " trezentos e"    nil]
      [" quatrocentas e" " quatrocentos e" nil]
      [" quinhentas e"   " quinhentos e"   nil]
      [" seiscentas e"   " seiscentos e"   nil]
      [" setecentas e"   " setecentos e"   nil]
      [" oitocentas e"   " oitocentos e"   nil]
      [" novecentas e"   " novecentos e"   nil]]
     [[" uma" " um" nil]		; 8 - singular period [0-4]
      " um mil"
      " um milhão"
      " um bilhão"
      " um trilhão"
      " um quatrilhão"]
     [""				; 9 - plural period [0-4]
      " mil"
      " milhões"
      " bilhões"
      " trilhões"
      " quatrilhões"]
     )
    (portuguese-pt			; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "menos"				; 2 - minus
     " e"				; 3 - and
     6					; 4 - digits per period
     [""				; 5 - tens [0-99]
      [" uma" " um" nil]    [" duas" " dois" nil]   " três"
      " quatro"             " cinco"                " seis"
      " sete"               " oito"                 " nove"
      " dez"
      " onze"               " doze"                 " treze"
      " catorze"            " quinze"               " dezasseis"
      " dezassete"          " dezoito"              " dezanove"
      " vinte"
      [" vinte e uma"  " vinte e um"   nil]
      [" vinte e duas" " vinte e dois" nil]         " vinte e três"
      " vinte e quatro"     " vinte e cinco"        " vinte e seis"
      " vinte e sete"       " vinte e oito"         " vinte e nove"
      " trinta"
      [" trinta e uma"  " trinta e um"   nil]
      [" trinta e duas" " trinta e dois" nil]       " trinta e três"
      " trinta e quatro"    " trinta e cinco"       " trinta e seis"
      " trinta e sete"      " trinta e oito"        " trinta e nove"
      " quarenta"
      [" quarenta e uma"  " quarenta e um"   nil]
      [" quarenta e duas" " quarenta e dois" nil]   " quarenta e três"
      " quarenta e quatro"  " quarenta e cinco"     " quarenta e seis"
      " quarenta e sete"    " quarenta e oito"      " quarenta e nove"
      " cinqüenta"
      [" cinqüenta e uma"  " cinqüenta e um"   nil]
      [" cinqüenta e duas" " cinqüenta e dois" nil] " cinqüenta e três"
      " cinqüenta e quatro" " cinqüenta e cinco"    " cinqüenta e seis"
      " cinqüenta e sete"   " cinqüenta e oito"     " cinqüenta e nove"
      " sessenta"
      [" sessenta e uma" " sessenta e um" nil]
      [" sessenta e duas" " sessenta e dois" nil]   " sessenta e três"
      " sessenta e quatro"  " sessenta e cinco"     " sessenta e seis"
      " sessenta e sete"    " sessenta e oito"      " sessenta e nove"
      " setenta"
      [" setenta e uma"  " setenta e um"   nil]
      [" setenta e duas" " setenta e dois" nil]     " setenta e três"
      " setenta e quatro"   " setenta e cinco"      " setenta e seis"
      " setenta e sete"     " setenta e oito"       " setenta e nove"
      " oitenta"
      [" oitenta e uma"  " oitenta e um"   nil]
      [" oitenta e duas" " oitenta e dois" nil]     " oitenta e três"
      " oitenta e quatro"   " oitenta e cinco"      " oitenta e seis"
      " oitenta e sete"     " oitenta e oito"       " oitenta e nove"
      " noventa"
      [" noventa e uma"  " noventa e um"   nil]
      [" noventa e duas" " noventa e dois" nil]     " noventa e três"
      " noventa e quatro"   " noventa e cinco"      " noventa e seis"
      " noventa e sete"     " noventa e oito"       " noventa e nove"]
     [""				; 6 - hundreds [0-9]
      " cem"
      [" duzentas"     " duzentos"     nil]
      [" trezentas"    " trezentos"    nil]
      [" quatrocentas" " quatrocentos" nil]
      [" quinhentas"   " quinhentos"   nil]
      [" seiscentas"   " seiscentos"   nil]
      [" setecentas"   " setecentos"   nil]
      [" oitocentas"   " oitocentos"   nil]
      [" novecentas"   " novecentos"   nil]]
     [""				; 7 - hundreds-tens [0-9]
      " cento e"
      [" duzentas e"     " duzentos e"     nil]
      [" trezentas e"    " trezentos e"    nil]
      [" quatrocentas e" " quatrocentos e" nil]
      [" quinhentas e"   " quinhentos e"   nil]
      [" seiscentas e"   " seiscentos e"   nil]
      [" setecentas e"   " setecentos e"   nil]
      [" oitocentas e"   " oitocentos e"   nil]
      [" novecentas e"   " novecentos e"   nil]]
     [[" uma" " um" nil]		; 8 - singular period [0-4]
      " um mil"
      " um milhão"
      " um bilhão"
      " um trilhão"
      " um quatrilhão"]
     [""				; 9 - plural period [0-4]
      " mil"
      " milhões"
      " bilhões"
      " trilhões"
      " quatrilhões"]
     )
    (spanish				; LANGUAGE
     "cero"				; 0 - zero
     ""					; 1 - comma
     "menos"				; 2 - minus
     " y"				; 3 - and
     6					; 4 - digits per period
     [""				; 5 - tens [0-99]
      [" una" " un" "uno"]  " dos"               " tres"
      " cuatro"             " cinco"             " seis"
      " siete"              " ocho"              " nueve"
      " diez"
      " once"               " doce"              " trece"
      " catorce"            " quince"            " dieciséis"
      " diecisiete"         " dieciocho"         " diecinueve"
      " veinte"
      [" veintiuna" " veintiun" " veintiuno"]
      " veintidós"          " veintitrés"
      " veinticuatro"       " veinticinco"       " veintiseis"
      " veintisiete"        " veintiocho"        " veintinueve"
      " treinta"
      [" treinta y una" " treinta y un" " treinta y uno"]
      " treinta y dos"      " treinta y tres"
      " treinta y cuatro"   " treinta y cinco"   " treinta y seis"
      " treinta y siete"    " treinta y ocho"    " treinta y nueve"
      " cuarenta"
      [" cuarenta y una" " cuarenta y un" " cuarenta y uno"]
      " cuarenta y dos"     " cuarenta y tres"
      " cuarenta y cuatro"  " cuarenta y cinco"  " cuarenta y seis"
      " cuarenta y siete"   " cuarenta y ocho"   " cuarenta y nueve"
      " cincuenta"
      [" cincuenta y una" " cincuenta y un" " cincuenta y uno"]
      " cincuenta y dos"    " cincuenta y tres"
      " cincuenta y cuatro" " cincuenta y cinco" " cincuenta y seis"
      " cincuenta y siete"  " cincuenta y ocho"  " cincuenta y nueve"
      " sesenta"
      [" sesenta y una" " sesenta y un" " sesenta y uno"]
      " sesenta y dos"      " sesenta y tres"
      " sesenta y cuatro"   " sesenta y cinco"   " sesenta y seis"
      " sesenta y siete"    " sesenta y ocho"    " sesenta y nueve"
      " setenta"
      [" setenta y una" " setenta y un" " setenta y uno"]
      " setenta y dos"      " setenta y tres"
      " setenta y cuatro"   " setenta y cinco"   " setenta y seis"
      " setenta y siete"    " setenta y ocho"    " setenta y nueve"
      " ochenta"
      [" ochenta y una" " ochenta y un" " ochenta y uno"]
      " ochenta y dos"      " ochenta y tres"
      " ochenta y cuatro"   " ochenta y cinco"   " ochenta y seis"
      " ochenta y siete"    " ochenta y ocho"    " ochenta y nueve"
      " noventa"
      [" noventa y una" " noventa y un" " noventa y uno"]
      " noventa y dos"      " noventa y tres"
      " noventa y cuatro"   " noventa y cinco"   " noventa y seis"
      " noventa y siete"    " noventa y ocho"    " noventa y nueve"]
     [""				; 6 - hundreds [0-9]
      " cien"
      [" doscientas"    " doscientos"    nil]
      [" trescientas"   " trescientos"   nil]
      [" cuatrocientas" " cuatrocientos" nil]
      [" quinientas"    " quinientos"    nil]
      [" seiscientas"   " seiscientos"   nil]
      [" setecientas"   " setecientos"   nil]
      [" ochocientas"   " ochocientos"   nil]
      [" novecientas"   " novecientos"   nil]]
     [""				; 7 - hundreds-tens [0-9]
      " ciento"
      [" doscientas"    " doscientos"    nil]
      [" trescientas"   " trescientos"   nil]
      [" cuatrocientas" " cuatrocientos" nil]
      [" quinientas"    " quinientos"    nil]
      [" seiscientas"   " seiscientos"   nil]
      [" setecientas"   " setecientos"   nil]
      [" ochocientas"   " ochocientos"   nil]
      [" novecientas"   " novecientos"   nil]]
     [[" una" " un" " uno"]		; 8 - singular period [0-4]
      " mil"
      " un millón"
      " un billón"
      " un trillón"
      " un cuadrillón"]
     [""				; 9 - plural period [0-4]
      " mil"
      " millones"
      " billones"
      " trillones"
      " cuadrillones"]
     )
    (swedish				; LANGUAGE
     "null"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " och"				; 3 - and
     3					; 4 - digits per period
     [""				; 5 - tens [0-99]
      " ett"        " två"        " tre"
      " fyra"       " fem"        " sex"
      " sju"        " åtta"       " nio"
      " tio"
      " elva"       " tolv"       " tretton"
      " fjorton"    " femton"     " sexton"
      " sjutton"    " arton"      " nitton"
      " tjugo"
      " tjugoett"   " tjugotvå"   " tjugotre"
      " tjugofyra"  " tjugofem"   " tjugosex"
      " tjugosju"   " tjugoåtta"  " tjugonio"
      " trettio"
      " trettiett"  " trettitvå"  " trettitre"
      " trettifyra" " trettifem"  " trettisex"
      " trettisju"  " trettiåtta" " trettinio"
      " fyrtio"
      " fyrtiett"   " fyrtitvå"   " fyrtitre"
      " fyrtifyra"  " fyrtifem"   " fyrtisex"
      " fyrtisju"   " fyrtiåtta"  " fyrtinio"
      " femtio"
      " femtiett"   " femtitvå"   " femtitre"
      " femtifyra"  " femtifem"   " femtisex"
      " femtisju"   " femtiåtta"  " femtinio"
      " sextio"
      " sextiett"   " sextitvå"   " sextitre"
      " sextifyra"  " sextifem"   " sextisex"
      " sextisju"   " sextiåtta"  " sextinio"
      " sjuttio"
      " sjuttiett"  " sjuttitvå"  " sjuttitre"
      " sjuttifyra" " sjuttifem"  " sjuttisex"
      " sjuttisju"  " sjuttiåtta" " sjuttinio"
      " åttio"
      " åttiett"    " åttitvå"    " åttitre"
      " åttifyra"   " åttifem"    " åttisex"
      " åttisju"    " åttiåtta"   " åttinio"
      " nittio"
      " nittiett"   " nittitvå"   " nittitre"
      " nittifyra"  " nittifem"   " nittisex"
      " nittisju"   " nittiåtta"  " nittinio"]
     [""				; 6 - hundreds [0-9]
      " ett hundra"
      " två hundra"
      " tre hundra"
      " fyra hundra"
      " fem hundra"
      " sex hundra"
      " sju hundra"
      " åtta hundra"
      " nio hundra"]
     [""				; 7 - hundreds-tens [0-9]
      " ett hundra och"
      " två hundra och"
      " tre hundra och"
      " fyra hundra och"
      " fem hundra och"
      " sex hundra och"
      " sju hundra och"
      " åtta hundra och"
      " nio hundra och"]
     [" ett"				; 8 - singular period [0-4]
      " ett tusen"
      " en milljon"
      " en milljard"
      " en billjon"
      " en billjard"]
     [""				; 9 - plural period [0-4]
      " tusen"
      " milljoner"
      " milljarder"
      " billjoner"
      " billjarder"]
     )
    )
  "Alist where each element has the following form:

    (LANGUAGE
     ;; 0 - zero
     0
     ;; 1 - comma
     \",\"
     ;; 2 - minus (negative numbers)
     \"minus\"
     ;; 3 - and (for example: one thousand AND one)
     ;;         (special case in some languages, see text below)
     \"\"
     ;; 4 - digits per period
     ;;     (for example: 3 - american english has period each 1,000;
     ;;                   4 - japanese has period each 10,000;
     ;;                   6 - spanish has period each 1,000,000)
     3
     ;; 5 - tens [0-99]
     [\"\"
      1  2  3
      4  5  6
      7  8  9
      10
      11 12 13
      14 15 16
      17 18 19
      20
      ...
      97 98 99]
     ;; 6 - hundreds [0-9] (for example: one hundred)
     [\"\" 100 200 .. 900]
     ;; 7 - hundreds-tens [0-9] (for example: one hundred AND ten)
     ;;                         (special case in some languages)
     [\"\" 100 200 .. 900]
     ;; 8 - singular period (like one, one thousand, one million, etc.)
     [1
      1,000
      1,000,000
      1,000,000,000
      1,000,000,000,000
      1,000,000,000,000,000]
     ;; 9 - plural period (like thousands, millions, etc.)
     [\"\"
      1,000
      1,000,000
      1,000,000,000
      1,000,000,000,000
      1,000,000,000,000,000]
     ;; 10 - extras (optional - special cases in some languages)
     [;; 0 - units
      [nil                              ; unit 0
       [4 \"str\" nil   nil \"str\" \"str\"]  ; unit 1
       nil                              ; unit 2
       [3 \"str\" \"str\" nil nil   \"str\"]  ; unit 3
       [3 nil   \"str\" nil nil   nil  ]  ; unit 4
       nil                              ; unit 5
       [4 nil   nil   nil nil   \"str\"]  ; unit 6
       nil                              ; unit 7
       [5 \"str\" nil   nil nil   nil  ]  ; unit 8
       nil]                             ; unit 9
      ;; 1 - tens
      [2  \"str\" nil   nil \"str\" \"str\"]
      ;; 2 - hundreds
      [3  nil   nil   nil nil   \"str\"]
      ;; 3 - thousands
      [3  nil   \"str\" nil nil   nil]]
     )

The numbers should be spelled out in a string or in a vector like:

   [FEMININE MASCULINE NEUTER]

Where FEMININE, MASCULINE and NEUTER are strings representing number in
feminine, masculine and neuter gender, respectively.  If there is no neuter, it
should be nil.

The string representation is used when all gender (feminine, masculine and
neuter) has the same spelling.

NOTE: The gender engine is designed for spelling out number and currency.
      It's beyond the scope of gender engine to handle general gender in all
      languages.

      There are cases where the NEUTER gender used by `spell-number' differs
      from the usual way that NEUTER gender is used in a language.  This is a
      trick used to spell out numbers correctly.

Some language may have two ways to express \"and\" in `;; 3 - and', one for
numbers until million (like one thousand AND one) and other for numbers above
million (like one million AND one), in this case, instead of a string, use a
vector like:

   [\"one and - thousand\" \"other and - million\"]

For an example, see `german' entry.

The item `;; 10 - extras' is optional and is only used in some languages that
has special rules about spelling a number, like `japanese' (see the entry as an
example).  The idea is that the spelling of some numbers are changed when used
is conjunction with some power of 10.  For example, in `japanese' the number 2
is spelled `ni' and the period 1,000 is spelled `sen', so 2,000 is spelled
`nisen'.  When using the number 3 (`san'), the number 3,000 is not spelled
`sansen', but `sanzen'.  In `japanese', the power of 10 used by `spell-number'
are 10, 10^2, 10^3, 10^4, 10^8, 10^12 and 10^16.

The `;; 10 - extras' item is a vector with the following form:

   [UNITS TENS HUNDREDS THOUSANDS]

Where UNITS is used when a unit is joined with a power of 10 has some spelling
modification.  It's nil, if there is no special case; or is a vector with the
following form:

   [UNIT-0 UNIT-1 ... UNIT-9]

Where UNIT-0, UNIT-1, ... and UNIT-9 has the same values as TENS, HUNDREDS and
THOUSANDS.

TENS is used when the multiples of 10 (10, 20, 30, ... and 90) is joined with
a power of 10 (above 10).

HUNDREDS is used when the multiples of 100 (100, 200, 300, ... and 900) is
joined with a power of 10 (above 10^2).

THOUSANDS is used when the multiples of 1000 (1000, 2000, 3000, ... and 9000)
is joined with a power of 10 (above 10^3).

TENS, HUNDREDS and THOUSANDS can be nil, if there is no special case; or can be
a vector with the following form:

   [N S1 S2 S3 S4 S5]

Where,

S1 is used when joining a unit/power with 10^3.

S2 is used when joining a unit/power with 10^4.

S3 is used when joining a unit/power with 10^8.

S4 is used when joining a unit/power with 10^12.

S5 is used when joining a unit/power with 10^16.

S1, S2, S3, S4 and S5 can be nil if there is no special case; or can be a
string, in this case, N characters will be removed from the tail of unit string
and then will be concated with the corresponding S1, S2, S3, S4 or S5.")


(defconst spelln-currency-database
  '(
    (andorra-french			 ; COUNTRY (AD)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (andorra-spanish			    ; COUNTRY (AD)
     [" peseta" " pesetas" " y " feminine]  ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (antigua-and-barbuda		   ; COUNTRY (AG)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (argentina				    ; COUNTRY (AR)
     [" peso" " pesos" " y " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (australia				   ; COUNTRY (AU)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (austria					; COUNTRY (AT)
     [" Schilling" " Schilling" " und " neuter] ; 0 - currency
     [" Groschen" " Groschen" 100 neuter]	; 1 - fractional
     )
    (bahamas				   ; COUNTRY (BS)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (barbados				   ; COUNTRY (BB)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (belgium				 ; COUNTRY (BE)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (belize				   ; COUNTRY (BZ)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (benin				 ; COUNTRY (BJ)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (bolivia					  ; COUNTRY (BO)
     [" boliviano" " bolivianos" " y " masculine] ; 0 - currency
     [" peso" " pesos" 1000 masculine]		  ; 1 - fractional
     )
    (brazil				    ; COUNTRY (BR)
     [" real" " reais" " e " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (brunei				   ; COUNTRY (BN)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (burkina-faso			 ; COUNTRY (BF)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (burundi				 ; COUNTRY (BI)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (cameroon				 ; COUNTRY (CM)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (canada				   ; COUNTRY (CA)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (cape-verde				    ; COUNTRY (CV)
     [" escudo" " escudos" " e " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (central-african-republic		 ; COUNTRY (CF)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (chad				 ; COUNTRY (TD)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (chile					; COUNTRY (CL)
     [" peso" " pesos" " y " masculine]		; 0 - currency
     [" centésimo" " centésimos" 100 masculine] ; 1 - fractional
     )
    (colombia				    ; COUNTRY (CO)
     [" peso" " pesos" " y " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (comoros				 ; COUNTRY (KM)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (congo				 ; COUNTRY (CG)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (costa-rica				    ; COUNTRY (CR)
     [" colón" " colones" " y " masculine]  ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (cuba				    ; COUNTRY (CU)
     [" peso" " pesos" " y " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (cyprus				 ; COUNTRY (CY)
     [" pound" " pounds" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	 ; 1 - fractional
     )
    (denmark				; COUNTRY (DK)
     [" krone" " kroner" " og " neuter] ; 0 - currency
     [" øre" " ører" 100 neuter]	; 1 - fractional
     )
    (djibouti				 ; COUNTRY (DJ)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (dominica				   ; COUNTRY (DM)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (dominican-republic			    ; COUNTRY (DO)
     [" peso" " pesos" " y " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (ecuador				    ; COUNTRY (EC)
     [" sucre" " sucres" " y " masculine]   ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (el-salvador			    ; COUNTRY (SV)
     [" colón" " colones" " y " masculine]  ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (equatorial-guinea			 ; COUNTRY (GQ)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (fiji				   ; COUNTRY (FJ)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (finland				; COUNTRY (FI)
     [" euro" " euroa" " ja " neuter]	; 0 - currency
     [" senttiä" " senttiä" 100 neuter]	; 1 - fractional
     )
    (france				 ; COUNTRY (FR)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (gabon				 ; COUNTRY (GA)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (germany				   ; COUNTRY (DE)
     [" Mark" " Mark" " und " feminine]	   ; 0 - currency
     [" Pfennig" " Pfennig" 100 masculine] ; 1 - fractional
     )
    (grenada				   ; COUNTRY (GD)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (guatemala				      ; COUNTRY (GT)
     [" quetzal" " quetzals" " y " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine]   ; 1 - fractional
     )
    (guinea				 ; COUNTRY (GN)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (guinea-bissau			    ; COUNTRY (GW)
     [" peso" " pesos" " y " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (guyana				   ; COUNTRY (GY)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (haiti				  ; COUNTRY (HT)
     [" gourde" " gourdes" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter]  ; 1 - fractional
     )
    (honduras				     ; COUNTRY (HN)
     [" lempira" " lempiras" " y " feminine] ; 0 - currency
     [" centavo" " centavos" 100 masculine]  ; 1 - fractional
     )
    (ireland				 ; COUNTRY (IE)
     [" pound" " pounds" " and " neuter] ; 0 - currency
     [" penny" " pence" 100 neuter]	 ; 1 - fractional
     )
    (italy				    ; COUNTRY (IT)
     [" lira" " lire" " e " neuter]	    ; 0 - currency
     [" centesimo" " centesimo" 100 neuter] ; 1 - fractional
     )
    (ivory-coast			 ; COUNTRY (CI) Cote d'Ivoire
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (jamaica				   ; COUNTRY (JM)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (japan				; COUNTRY (JP)
     ["en" "en" "" neuter]		; 0 - currency
     ["" "" 0 neuter]			; 1 - fractional
     ["n$" "n '"]			; 2 - extras
     ["\\([^']\\)$" "\\1 "]
     )
    (kenya				       ; COUNTRY (KE)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	       ; 1 - fractional
     )
    (kiribati				   ; COUNTRY (KI)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (liberia				   ; COUNTRY (LR)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (liechtenstein			 ; COUNTRY (LI)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (luxembourg				 ; COUNTRY (LU)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (madagascar				 ; COUNTRY (MG)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (mali				 ; COUNTRY (ML)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (mexico				    ; COUNTRY (MX)
     [" peso" " pesos" " y " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (monaco				 ; COUNTRY (MC)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (mozambique				      ; COUNTRY (MZ)
     [" metical" " meticals" " e " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine]   ; 1 - fractional
     )
    (namibia				   ; COUNTRY (NA)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (nauru				   ; COUNTRY (??)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (netherlands			      ; COUNTRY (NL)
     [" guilder" " guilder" " en " masculine] ; 0 - currency
     [" cent" " cent" 100 masculine]	      ; 1 - fractional
     )
    (new-zealand			   ; COUNTRY (NZ)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (nicaragua				      ; COUNTRY (NI)
     [" córdoba" " córdobas" " y " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine]   ; 1 - fractional
     )
    (niger				 ; COUNTRY (NE)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (norway				; COUNTRY (NO)
     [" krone" " kroner" " og " neuter] ; 0 - currency
     [" øre" " ører" 100 neuter]	; 1 - fractional
     )
    (panama				 ; COUNTRY (PP)
     [" balboa" " balboas" " y " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	 ; 1 - fractional
     )
    (paraguay				    ; COUNTRY (PY)
     [" guaraní" " guaranís" " y " neuter]  ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (peru				    ; COUNTRY (PE)
     [" sol" " soles" " y " neuter]	    ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (philippines			    ; COUNTRY (PH)
     [" peso" " pesos" " y " masculine]	    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (portugal				    ; COUNTRY (PT)
     [" escudo" " escudos" " e " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (rwanda				 ; COUNTRY (RW)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (sao-tome-and-principe		    ; COUNTRY (ST)
     [" dobra" " dobras" " y " feminine]    ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (senegal				 ; COUNTRY (SN)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (singapore				   ; COUNTRY (SG)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (solomon-islands			   ; COUNTRY (SB)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (somalia				       ; COUNTRY (SO)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	       ; 1 - fractional
     )
    (south-africa			; COUNTRY (ZA)
     [" rand" " rands" " and " neuter]	; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (spain				    ; COUNTRY (ES)
     [" peseta" " pesetas" " y " feminine]  ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (st-kitts-and-nevis			   ; COUNTRY (KN)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (st-lucia				   ; COUNTRY (LC)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (st-vicent-and-grenadines		   ; COUNTRY (VC)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (sweden				 ; COUNTRY (SE)
     [" krona" " kronor" " och " neuter] ; 0 - currency
     [" öre" " ören" 100 neuter]	 ; 1 - cent
     )
    (switzerland			 ; COUNTRY (CH)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (taiwan				   ; COUNTRY (TW)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (tanzania				       ; COUNTRY (TZ)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	       ; 1 - fractional
     )
    (togo				 ; COUNTRY (TG)
     [" franc" " francs" " et " neuter]	 ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (trinidad-and-tobago		   ; COUNTRY (TT)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (tuvalu				   ; COUNTRY (TV)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (uganda				       ; COUNTRY (UG)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	       ; 1 - fractional
     )
    (united-kingdom			 ; COUNTRY (GB)
     [" pound" " pounds" " and " neuter] ; 0 - currency
     [" penny" " pence" 100 neuter]	 ; 1 - fractional
     )
    (united-states			   ; COUNTRY (US)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    (uruguay					; COUNTRY (UY)
     [" peso" " pesos" " y " masculine]		; 0 - currency
     [" centésimo" " centésimos" 100 masculine] ; 1 - fractional
     )
    (venezuela				       ; COUNTRY (VE)
     [" bolívar" " bolívares" " y " masculine] ; 0 - currency
     [" céntimo" " céntimos" 100 masculine]    ; 1 - fractional
     )
    (zimbabwe				   ; COUNTRY (ZW)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	   ; 1 - fractional
     )
    )
  "Alist where each element has the following form:

    (COUNTRY
     ;; 0 - currency
     [\" dollar\" \" dollars\" \" and \" GENDER]
     ;; 1 - fractional
     [\" cent\" \" cents\" 100 GENDER]
     ;; 2 - extras (optional)
     [SEARCH-REGEXP REPLACE-REGEXP]...
     )

Where GENDER indicates the word gender.  Valid values are `feminine',
`masculine' or `neuter'.  Any other value is treated as `neuter'.

The `;; 2 - extras' item is optional and it's used by some languages that has
special rules in spelling numbers with currency, like japanese language (see
`japan' entry).  The rules are applied only in the currency part (item 0).

The rules are specified by vectors with the following form:

   [SEARCH-REGEXP REPLACE-REGEXP]

So, the currency part is searched by SEARCH-REGEXP, if there is a match, the
REPLACE-REGEXP is used to replace the matching.")


(defun spelln-string-match (match str)
  (let (case-fold-search)
    (or (and (string-match match str)
	     (= (match-beginning 0) 0)
	     (= (match-end 0) (length str)))
	(error "Invalid numeric string for spelling out: %S" str))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun spelln-number-customize ()
  "Customize spelln-number options."
  (interactive)
  (customize-group 'spelln-number))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Commands:


;;;###autoload
(defun spelln-integer-in-words (int &optional gender-sym)
  "Return the spelling of integer INT in words.

Optionally, GENDER-SYM specifies the spelling gender.
Valid values for GENDER-SYM are: 'feminine, 'masculine or 'neuter.
Any other value is treated as 'neuter."
  (interactive "nInteger to spell out in words: ")
  (let* ((integer-base (cdr (assq spelln-language spelln-language-database)))
	 (spelln-zero            (nth  0 integer-base))
	 (spelln-comma           (nth  1 integer-base))
	 (spelln-minus           (nth  2 integer-base))
	 (spelln-and             (nth  3 integer-base))
	 (spelln-digits-period   (nth  4 integer-base))
	 (spelln-tens            (nth  5 integer-base))
	 (spelln-hundreds        (nth  6 integer-base))
	 (spelln-hundreds-tens   (nth  7 integer-base))
	 (spelln-singular-period (nth  8 integer-base))
	 (spelln-plural-period   (nth  9 integer-base))
	 (spelln-extra           (nth 10 integer-base))
	 (spell (and integer-base
		     (spelln-int int gender-sym))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))


;;;###autoload
(defun spelln-currency-in-words (value)
  "Return the spelling of number VALUE as a currency in words."
  (interactive "nCurrency to spell out in words: ")
  (let* ((integer-base (cdr (assq spelln-language spelln-language-database)))
	 (spelln-zero            (nth  0 integer-base))
	 (spelln-comma           (nth  1 integer-base))
	 (spelln-minus           (nth  2 integer-base))
	 (spelln-and             (nth  3 integer-base))
	 (spelln-digits-period   (nth  4 integer-base))
	 (spelln-tens            (nth  5 integer-base))
	 (spelln-hundreds        (nth  6 integer-base))
	 (spelln-hundreds-tens   (nth  7 integer-base))
	 (spelln-singular-period (nth  8 integer-base))
	 (spelln-plural-period   (nth  9 integer-base))
	 (spelln-extra           (nth 10 integer-base))
	 (country-base (cdr (assq spelln-country spelln-currency-database)))
	 (spelln-currency        (nth 0 country-base))
	 (spelln-fractional      (nth 1 country-base))
	 (spelln-currency-extra  (nthcdr 2 country-base))
	 (money (truncate value))
	 (frac  (abs (round (* (- value money) (aref spelln-fractional 2)))))
	 (spell (and integer-base country-base
		     (concat
		      (spelln-currency-extra
		       (spelln-int money (aref spelln-currency 3))) ; value
		      (aref spelln-currency			    ; currency
			    (if (or (= money 1) (= money -1))
				0
			      1))
		      (and (or (/= frac 0) ; fractional spelling
			       spelln-zero-cents)
			   (concat (aref spelln-currency 2) ; and
				   (spelln-int frac	    ; value
					       (aref spelln-fractional 3))
				   (aref spelln-fractional ; fractional
					 (if (= frac 1)
					     0
					   1))))))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))


;;;###autoload
(defun spelln-numeric-string-in-words (str &optional gender-sym)
  "Return the spelling of a numeric string STR in words.

STR should match the regexp \"[-+]?[0-9P]+\", where P is the value of variable
`spelln-period-character'.

For: (setq spelln-period-character ?,)
A valid numeric string is \"+1,234,567\" or \"1234567\".

Optionally, GENDER-SYM specifies the spelling gender.
Valid values for GENDER-SYM are: 'feminine, 'masculine or 'neuter.
Any other value is treated as 'neuter."
  (interactive "sNumeric string to spell out in words: ")
  (let* ((integer-base (cdr (assq spelln-language spelln-language-database)))
	 (spelln-zero            (nth  0 integer-base))
	 (spelln-comma           (nth  1 integer-base))
	 (spelln-minus           (nth  2 integer-base))
	 (spelln-and             (nth  3 integer-base))
	 (spelln-digits-period   (nth  4 integer-base))
	 (spelln-tens            (nth  5 integer-base))
	 (spelln-hundreds        (nth  6 integer-base))
	 (spelln-hundreds-tens   (nth  7 integer-base))
	 (spelln-singular-period (nth  8 integer-base))
	 (spelln-plural-period   (nth  9 integer-base))
	 (spelln-extra           (nth 10 integer-base))
	 (spell (and (save-match-data
		       (spelln-string-match
			(concat "[-+]?[0-9"
				(regexp-quote
				 (char-to-string spelln-period-character))
				"]+")
			str))
		     (cdr (spelln-str str gender-sym)))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))


;;;###autoload
(defun spelln-currency-string-in-words (value)
  "Return the spelling of numeric string VALUE as a currency in words.

VALUE should match the regexp \"[-+]?[0-9P]+\\(D[0-9]*\\)?\", where P is the
value of variable `spelln-period-character' and D is the value of variable
`spelln-decimal-character'.

For: (setq spelln-period-character ?,
	   spelln-decimal-character ?.)
A valid numeric string is \"+1,234,567.89\" or \"1234567.89\"."
  (interactive "sCurrency string to spell out in words: ")
  (let* ((integer-base (cdr (assq spelln-language spelln-language-database)))
	 (spelln-zero            (nth  0 integer-base))
	 (spelln-comma           (nth  1 integer-base))
	 (spelln-minus           (nth  2 integer-base))
	 (spelln-and             (nth  3 integer-base))
	 (spelln-digits-period   (nth  4 integer-base))
	 (spelln-tens            (nth  5 integer-base))
	 (spelln-hundreds        (nth  6 integer-base))
	 (spelln-hundreds-tens   (nth  7 integer-base))
	 (spelln-singular-period (nth  8 integer-base))
	 (spelln-plural-period   (nth  9 integer-base))
	 (spelln-extra           (nth 10 integer-base))
	 (country-base (cdr (assq spelln-country spelln-currency-database)))
	 (spelln-currency        (nth 0 country-base))
	 (spelln-fractional      (nth 1 country-base))
	 (spelln-currency-extra  (nthcdr 2 country-base))
	 decimal
	 (spell
	  (and integer-base country-base
	       (save-match-data
		 (and (spelln-string-match
		       (concat "[-+]?[0-9"
			       (regexp-quote
				(char-to-string spelln-period-character))
			       "]+\\("
			       (regexp-quote
				(char-to-string spelln-decimal-character))
			       "[0-9]*\\)?")
		       value)
		      (progn
			(setq decimal (match-beginning 1)) ; decimal point
			t)))
	       (let ((money (spelln-str (if decimal
					    (substring value 0 decimal)
					  value)
					(aref spelln-currency 3)))
		     (frac  (if (<= (aref spelln-fractional 2) 0)
				'(0 . "")
			      (spelln-str (if decimal
					      (substring value (1+ decimal))
					    "0")
					  (aref spelln-fractional 3)
					  (truncate
					   (log10 (aref spelln-fractional 2)))))))
		 (concat (spelln-currency-extra (cdr money)) ; value
			 (aref spelln-currency		     ; currency
			       (if (= (car money) 1)
				   0
				 1))
			 (and (or (/= (car frac) 0) ; fractional spelling
				  spelln-zero-cents)
			      (concat (aref spelln-currency 2) ; and
				      (cdr frac)	       ; value
				      (aref spelln-fractional  ; fractional
					    (if (= (car frac) 1)
						0
					      1)))))))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'spell-number)


;;; spell-number.el ends here

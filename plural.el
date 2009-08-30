;;; plural.el --- Pluralize english nouns.

;; Copyright (C) 2006, 2007  Aaron S. Hawley

;; Author: Aaron S. Hawley
;; Keywords: convenience, spelling
;; Version: $Id: plural.el,v 1.5 2007/09/17 19:43:26 aaronh Exp $
;; URL: http://www.emacswiki.org/cgi-bin/emacs-en/PluralizeEnglish

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA, or visit <http://www.gnu.org/licenses/>.

;;; Commentary:

;; On any word try `M-x plural-make-plural'.

;;; Code:

(require 'thingatpt)

(defun plural-make-plural ()
  (interactive)
  ;FIXME: Can this word boundary logic be simpler?
  (if (and (or (looking-at "\\w") ;; in word
	       (looking-at "\\b\\W")) ;; past end of word
	   (not (looking-at "\\b\\w"))) ;; but not at beginning
      (forward-word -1)) ;; backward word
  (let ((word (word-at-point)))
    (if (null word)
	(message "No word at point")
      (if (not (plural-wordp word))
	  (message "Not a word: %s" word)
	(if (not (plural-nounp word))
	    (message "Not a noun: %s" word)
	  (let ((plural (plural-pluralize word)))
	    (if (not (plural-wordp plural))
		(message "Could not find correct plural: %s" word)
	      (kill-word 1) ;; delete word
	      (insert plural))))))))

(defun plural-wordp (word)
  t) ;; Use `ispell-word'?

(defun plural-nounp (word)
  t) ;; Use an external dictionary?  What about proper nouns?

(defun plural-pluralize (noun)
  ;; iterate over the plural rules associative list
  (let ((plural-replace
	 (assoc-default noun plural-knowledge 
			'pluralize-case-conditional-string-match)))
    (if (null plural-replace) ;; if failed match
	noun ;; should never happen
      (replace-match plural-replace t nil noun)))) ;; return plural

(defun pluralize-case-conditional-string-match (re str)
  (let ((is-lower (equal (downcase re) re)))
    ;; Fold case if string is lowercase.
    (let ((case-fold-search is-lower))
      (string-match re str))))

(defvar plural-knowledge
  '(
    ("ss$" . "sses")
    ("zz$" . "zzes") ;; Example: "buzzes".
    ("sh$" . "shes")
    ("tch$" . "tches")
    ("eaf$" . "eaves")
    ("ief$" . "ieves") ;; Example: "theives".
    ("roof$" . "roofs")
    ("oof$" . "ooves")
    ("ife$" . "ives")
    ("lf$" . "lves")
    ("[aeiou]y$" . "\\&s")
    ("ndum$" . "nda") ;; Example: "addendum".
    ("um$" . "a") ;; Example: "media", "criteria", "symposia",
		  ;; "crania", curriculum", "data".
    ("^die$" . "dice")
    ("dogma$" . "dogmas") ;; exception to -ma rule.
    ("lemma$" . "lemmas") ;; exception to -ma rule.
    ("schema$" . "schemas") ;; exception to -ma rule.
    ("ia$" . "ium") ;; Example: "bacteria".
    ("ma$" . "mata") ;; Example: "stigma".
    ("na$" . "nae") ;; Example: "antenna".
    ("ta$" . "tum") ;; Example: "strata".
    ("Atlas$" . "Atlantes") ;; Case-sensitive
    ("atlas$" . "atlases")
    ("Harry$" . "Harrys") ;; Case-sensitive
    ("aircraft$" . "aircraft")
    ("alga$" . "algae")
    ("alumna$" . "alumnae")
    ("alumnus$" . "alumni")
    ("ameoba$" . "ameobae")
    ("automaton$" . "automata")
    ("bacillus$" . "bacilli")
    ("banjo$" . "banjos")
    ("beau$" . "beaux")
    ("cactus$" . "cacti") ;; Or "cactuses".
    ("cannon$" . "cannon") ;; Or "cannons".
    ("canto$" . "cantos")
    ("cargo$" . "cargos")
    ("cattle$" . "cattle")
    ("child$" . "children")
    ("cod$" . "cod")
    ("corpus$" . "corpora")
    ("dwarf$" . "dwarves")
    ("cs$" . "csen") ;; Example: "emacsen".
    ("foot$" . "feet")
    ("formula$" . "formulae")
    ("graffito$" . "graffiti")
    ("rion$" . "ria") ;; Example: "criteria".
    ("deer$" . "deer")
    ("focus$" . "foci")
    ("genus$" . "genera")
    ("goose$" . "geese")
    ("hedron$" . "hedra") ;; Example: "polyhedron".
    ("hippopotamus$" . "hippopotami")
;;    ("index$" . "indices") ;; "indexes" is also acceptable.
    ("insigne$" . "insignia")
    ("life$" . "lives")
    ("louse$" . "lice")
    ("mackerel$" . "mackerel")
    ("man$" . "men")
    ("matrix$" . "matrices")
    ("moose$" . "moose")
    ("motto$" . "mottos")
    ("mouse$" . "mice")
    ("nucleus$" . "nuclei")
    ("octopus$" . "octopi") ;; Or "octopuses".
    ("offspring" . "offspring")
    ("opus$" . "opera")
    ("\\box$" . "oxen")
    ("panino$" . "panini")
    ("paparazzo$" . "paparazzi")
    ("phalanx$" . "phalanges")
    ("phenomenon$" . "phenomena")
    ("people$" . "people")
    ("perch$" . "perch") ;; Can certain uses of "perch" be plural?
    ("piano$" . "pianos")
    ("police$" . "police")
    ("portico$" . "porticos")
    ("quarto$" . "quartos")
    ("radius$" . "radii")
    ("rhinoceros$" . "rhinoceri") ;; Or "rhinoceroses".
;;    ("series$" . "series") ;; Already has an "s".
    ("sheep$" . "sheep")
;;    ("species$" . "species") ;; Already has an "s".
    ("solo$" . "solos")
    ("syllabus$" . "syllabi")
    ("terminus$" . "termini")
    ("ulus$" . "uli") ;; Example: "stimuli".
    ("trout$" . "trout")
    ("tooth$" . "teeth")
    ("uterus$" . "uteri") ;; Or "uteruses".
    ("virtuoso" . "virtuosi")
    ("viscus$" . "viscera")
;;    ("woman$" . "women") ;; See "man$".
;;    ("e$" . "es") ;; Fall-through to "[^s]$".
    ("is$" . "es") ;; Example: "axes", "crises", "testes".
    ("us$" . "uses") ;; Example: "campuses", "platypuses", "prospectuses".
    ("io$" . "ios")
    ("oo$" . "oos")
    ("o$" . "oes")
    ("y$" . "ies")
    ("[ei]x$" . "ices") ;; Example: "vertices".
    ("x$" . "xes")
    ("[^s]$" . "\\&s")) ;; Add an `s' if not an `s'.
  "Associative list with first element a regular expression
 for the suffix of nouns, and the second element is
 the replacement to make the word plural.

Matches are made in order of appearance.

Sorted by order of plural \"operation\", secondarily by case order,
then by alphabetical order.

Documentation on plural rules at:
 http://en.wikipedia.org/wiki/English_plural")

;; Converted from a tab delimitted list with:
;; (query-replace-regexp "^\\(\\w+\\)	\\(\\w+\\)" "(\"\\1\" \"\\2\")" 
(defvar plural-tests
  '(("Atlas" "Atlantes")
    ("Harry" "Harrys")
    ("addendum" "addenda")
    ("agendum" "agenda")
    ("aircraft" "aircraft")
    ("alga" "algae")
    ("alumna" "alumnae")
    ("alumnus" "alumni")
    ("ameoba" "ameobae")
    ("analysis" "analyses")
    ;; ("appendix" "appendices") ;; "appendexes" is acceptable.
    ("antenna" "antennae")
    ("apex" "apices")
    ("appendix" "appendices")
    ("atlas" "atlases")
    ("automaton" "automata")
    ("ax" "axes")
    ("axis" "axes")
    ("bacillus" "bacilli")
    ("bacteria" "bacterium")
    ("banjo" "banjos")
    ("baby" "babies")
    ("basis" "bases")
    ("bath" "baths")
    ("bay" "bays")
    ("beau" "beaux")
    ("birdie" "birdies")
    ("box" "boxes")
    ;; ("bureau" "bureaux") ;; "bureaus" is acceptable.
    ("bush" "bushes")
    ("buzz" "buzzes")
    ("blues" "blues")
    ("buffalo" "buffaloes")
    ("boy" "boys")
    ("cactus" "cacti")
    ("calf" "calves")
    ("campus" "campuses")
    ("candelabrum" "candelabra")
    ("cannon" "cannon")
    ("canto" "cantos")
    ("cargo" "cargos")
    ("cat" "cats")
    ("cattle" "cattle")
    ("cervix" "cervices")
    ("chair" "chairs")
    ("cherry" "cherries")
    ("child" "children")
    ("choice" "choices")
    ("cod" "cod")
    ("corpus" "corpora")
    ;;    ("cow" "kine") ;; archaic
    ("cranium" "crania")
    ("crisis" "crises")
    ("criterion" "criteria")
    ("curriculum" "curricula")
    ("datum" "data")
    ("day" "days")
    ("diagnosis" "diagnoses")
    ("die" "dice") ;; Not to be confused with past tense verb: "dies".
    ("deer" "deer")
    ("dish" "dishes")
    ("dogma" "dogmas")
    ("dwarf" "dwarves")
    ("echo" "echoes")
    ("emacs" "emacsen")
    ("emphasis" "emphases")
    ;;    ("eye" "eyen") ;; archaic
    ;;    ("fish" "fish") ;; "fishes" is plural of "fish species"
    ("focus" "foci")
    ("foot" "feet")
    ("formula" "formulae")
    ("forum" "fora")
    ("genus" "genera")
    ("girl" "girls")
    ("glass" "glasses")
    ("goose" "geese")
    ("graffito" "graffiti")
    ;;    ("head" "head") ;; "head of cattle"
    ("hero" "heroes")
    ("hippopotamus" "hippopotami")
    ("hoof" "hooves")
    ("house" "houses")
    ("hypothesis" "hypotheses")
    ;;    ("index" "indices") ;; "indexes" is acceptable
    ("insigne" "insignia")
    ("invoice" "invoices")
    ("jeans" "jeans")
    ("judge" "judges")
    ("kangaroo" "kangaroos")
    ("knife" "knives")
    ("lady" "ladies")
    ("lemma" "lemmas")
    ("licorice" "licorices")
    ("louse" "lice")
    ("mackerel" "mackerel")
    ("man" "men")
    ("matrix" "matrices")
    ("medium" "media")
    ("memorandum" "memoranda")
    ("monkey" "monkeys")
    ("moose" "moose")
    ("moth" "moths")
    ("motto" "mottos")
    ("mouse" "mice")
    ("mouth" "mouths")
    ("neurosis" "neuroses")
    ("nucleus" "nuclei")
    ("nuptial" "nuptials")
    ("oasis" "oases")
    ("octopus" "octopi")
    ("offspring" "offspring")
    ("opus" "opera")
    ("osprey" "ospreys")
    ("ox" "oxen")
    ("panino" "panini")
    ("panda" "pandas")
    ("paparazzo" "paparazzi")
    ("parenthesis" "parentheses")
    ("people" "people")
    ("perch" "perch")
    ("penny" "pennies")
    ("pie" "pies")
    ("poppy" "poppies")
    ("phalanx" "phalanges")
    ("phase" "phases")
    ("phenomenon" "phenomena")
    ("phenomenon" "phenomena")
    ("piano" "pianos")
    ("police" "police")
    ("place" "places")
    ("platypus" "platypuses")
    ("polyhedron" "polyhedra")
    ("poppy" "poppies")
    ("portico" "porticos")
    ("potato" "potatoes")
    ("proof" "proofs")
    ("prospectus" "prospectuses")
    ("quarto" "quartos")
    ("radius" "radii")
    ("rhinoceros" "rhinoceri")
    ("rice" "rices")
    ("roof" "roofs")
    ("schema" "schemas") ;; Or "schemata".
    ("series" "series")
    ("sheaf" "sheaves")
    ("sheep" "sheep")
    ("shelf" "shelves")
    ;;    ("shoe" "shoon") ;; archaic
    ("solo" "solos")
    ("species" "species")
    ("spy" "spies")
    ("staff" "staffs")
    ("stigma" "stigmata")
    ("stimulus" "stimuli")
    ("stoma" "stomata")
    ("strata" "stratum")
    ("studio" "studios")
    ("switch" "switches")
    ("syllabus" "syllabi")
    ("symposium" "symposia")
    ("synopsis" "synopses")
    ("terminus" "termini")
    ("testis" "testes")
    ("thesis" "theses")
    ("thief" "thieves")
    ("tiding" "tidings")
    ("tooth" "teeth")
    ("trout" "trout")
    ("turf" "turfs")
    ("uterus" "uteri")
    ("vertex" "vertices")
    ("victual" "victuals")
    ("virtuoso" "virtuosi")
    ("viscus" "viscera")
    ("volcano" "volcanoes")
    ("vortex" "vortices")
    ("witch" "witches")
    ("wife" "wives")
    ("wolf" "wolves")
    ("woman" "women")
    ("zeugma" "zeugmata")
    ("zoo" "zoos"))
  "List with each element containing a pair.
 The first element is the singular noun,
 the second element is the plural form.")

(defun plural-test-plural (word plural)
  "Pluralize WORD and compare with expected PLURAL.
 Returns error message string on failure, nil on success."
  (let ((computed-plural (plural-pluralize word)))
    (if (not (equal computed-plural
		    plural))
	(format "`%s' is not plural of `%s'--expected `%s'"
		computed-plural word plural))))

;; (plural-test-plural "meat" "meat")

(defun plural-run-tests ()
  (let ((test-buffer-name "*plural-tests*")
	(max-lisp-eval-depth (* (length plural-tests) 3)))
    (with-current-buffer (or (get-buffer test-buffer-name)
			   (generate-new-buffer test-buffer-name))
    (delete-region (point-min) (point-max)) ;; empty the buffer
    (let ((failures ;; iterate over the tests
	   (plural-filter 'identity
			  (mapcar (lambda (l)
				    (apply 'plural-test-plural l))
				  plural-tests))))
      (if (null failures)
	  (kill-buffer test-buffer-name)
	(insert (mapconcat 'identity failures "\n")) ;; insert errors
	(newline)
	(insert (plural-test-results (- (length plural-tests)
					(length failures))
				     (length failures))))
      ;; test statistics
      (message "%s" (plural-test-results (- (length plural-tests)
					    (length failures))
					 (length failures)))))))
;; (plural-run-tests)

(defun plural-test-results (n-pass n-fail)
  (let ((total (+ n-pass n-fail)))
    (format "Test results: success: %d/%d (%.1f%%), failures: %d/%d (%.1f%%)"
	    n-pass total (* (/ (float n-pass) total) 100)
	    n-fail total (* (/ (float n-fail) total) 100))))

(defun plural-filter (f l)
  ;; filter with tail-recursion to avoid stack-overflow
  (plural-filter2 f l nil))

;; (plural-filter 'null '(nil t nil)) => (nil nil)

(defun plural-filter2 (f l result)
  (if (null l)
      result
    (plural-filter2 f (cdr l) (if (funcall f (car l))
				  (append result (list (car l)))
				result))))

(provide 'plural)

;;; plural.el ends here

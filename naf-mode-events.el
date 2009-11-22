;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-events.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-events lists terms which flag on events which had bearin on art
;;; Styles or movements. Used for `naf-mode' font-locking.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;; `naf-mode-world-events' `naf-mode-art-events'
;;;
;;; VARIABLES:
;;; `*naf-mode-events-xrefs*'
;;; `*naf-mode-art-events*' `*naf-world-events*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;; `naf-mode-events' -> naf-mode-world-events
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;;
;;; NOTES:
;;; This file uses the provide/require idiom because of the defconstant forms.
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;;
;;; THIRD-PARTY-CODE:
;;;
;;; Strip or reformat with regexps on these commonly employed "TAGS":
;;;
;;; TAGS-APPEARING-IN-COMMENTS:
;;; :CLEANUP :CLOSE :COURTESY :CREATED :DATE :EMACS-WIKI :EVAL-BELOW-TO-TEST
;;; :FIXES :FIXME :HIS :IF-NOT-FEATURE-P :KEYWORD-REGEXPS-IN
;;; :LOAD-SPECIFIC-PROCEDURES :MODIFICATIONS :RENAMED :SEE-BELOW :SUBJECT :TODO
;;; :TEST-ME :UNCOMMENT-BELOW-TO-TEST :VERSION :WAS
;;;
;;; TAGS-APPEARING-IN-DOCSTRINGS:
;;; :ALIASED-BY :CALLED-BY :EXAMPLE :FACE-DEFINED-IN :FACE-DOCUMENTED-IN
;;; :FILE :IDIOM :NOTE :SEE :SEE-ALSO :SOURCE :USED-BY
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-events.el)
;;; FIRST-PUBLISHED: <Timestamp: #{2009-11-21T20:03:03-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:24:57-04:00Z}#{09327} - by MON>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright (C) MON KEY - 2009
;;; ============================
;;; CODE:


;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-14T18:49:27-04:00Z}#{09381} - by MON>
(eval-and-compile
(defvar *naf-mode-events-xrefs* 
  '(*naf-art-events* 
    *naf-art-events-french*
    *naf-art-events-english*
    *naf-art-events-generic* 
    *naf-art-events-generic-english*
    *naf-art-events-generic-french*
    *naf-world-events* 
    *naf-mode-events-xrefs* 
    mon-help-naf-mode-faces)
  "List of symbol names of variables which xref each other in naf-mode-events
package. See FILE: \"./naf-mode-events.el\"."))
;;
;;;test-me; 
;;
;;;(progn (makunbound '*naf-mode-events-xrefs*) (unintern '*naf-mode-events-xrefs*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-17T16:37:52-04:00Z}#{09384} - by MON>
;; (defvar *naf-events-CIAM*
;;  ("CIAM I"    "1928" "La Sarraz, Switzerland")
;;  ("CIAM II"   "1929" "Frankfurt, Germany")
;;  ("CIAM III"  "1930" "Brussels, Belgium")
;;  ("CIAM IV"   "1933" "Athens, Greece")
;;  ("CIAM V"    "1937" "Paris, France")
;;  ("CIAM VI"   "1947" "Bridgwater, England")
;;  ("CIAM VII"  "1949" "Bergamo, Italy")
;;  ("CIAM VIII" "1951" "Hoddesdon, England")
;;  ("CIAM IX"   "1953" "Aix-en-Provence, France")
;;  ("CIAM X"    "1956" "Dubrovnik, Yugoslavia")
;;  ("CIAM XI"   "1959" "Otterlo, the Netherlands"))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-17T16:54:21-04:00Z}#{09384} - by MON>
;; (defvar *naf-world-expositions*
;;   '(("1851" "London, United Kingdom")
;;     ("1855" "Paris, France")
;;     ("1862" "London, United Kingdom")
;;     ("1867" "Paris, France")
;;     ("1873" "Vienna, Austria")
;;     ("1876" "Philadelphia, United States")
;;     ("1878" "Paris, France")
;;     ("1879" "Sydney, Australia")
;;     ("1880" "Melbourne, Australia")
;;     ("1881" "Milan, Italy")
;;     ("1884" "New Orleans, United States")
;;     ("1888" "Barcelona, Spain")
;;     ("1889" "Paris, France")
;;     ("1893" "Chicago, United States")
;;     ("1896" "Nizhny Novgorod, Russia")
;;     ("1896" "Budapest, Hungary")
;;     ("1897" "Brussels, Belgium")
;;     ("1898" "Omaha, United States")
;;     ("1900" "Paris, France")
;;     ("1901" "Buffalo, United States")
;;     ("1901" "Charleston, United States")
;;     ("1904" "St. Louis, United States")
;;     ("1905" "Liège, Belgium")
;;     ("1906" "Milan, Italy")
;;     ("1910" "Brussels, Belgium")
;;     ("1911" "Turin, Italy")
;;     ("1913" "Ghent, Belgium")
;;     ("1914" "Lyon, France")
;;     ("1915" "San Francisco, United States")
;;     ("1915" "San Diego, United States")
;;     ("1929" "Barcelona, Spain") "Exposición Ibero-Americana"
;;     ("1933" "Chicago, United States")
;;     ("1935" "Brussels,, Belgium")
;;     ("1937" "Paris, France")
;;     ("1939" "New York City, United States")
;;     ("1939-1940" "San Francisco, United States")
;;     ("1958" "Brussels, Belgium")
;;     ("1962" "Seattle, United States")
;;     ("1964" "New York, United States")
;;     ("1967" "Montreal, Canada") ;; over 50 million visitors
;;     ("1968" "San Antonio, United States")
;;     ("1970" "Osaka, Japan")
;;     ("1974" "Spokane, United States")
;;     ("1982" "Knoxville, United States")
;;     ("1984" "New Orleans, United States")
;;     ("1985" "Tsukuba, Japan")
;;     ("1986" "Vancouver, Canada")
;;     ("1988" "Brisbane, Australia")
;;     ("1990" "Osaka, Japan")
;;     ("1992" "Seville, Spain")
;;     ("1993" "Daejeon, South Korea")
;;     ("1998" "Lisbon, Portugal")
;;     ("1999" "China")			;Kunming International Garden Festival 
;;     ("2000" "Hanover, Germany")
;;     ("2005" "Aichi, Japan")
;;     ("2006" "Thailand")			;Chiang Mai Royal Flora Ratchaphruek 
;;     ("2008" "Zaragoza, Spain")
;;     ("2010" "Shanghai, China")
;;     ("2012" "Yeosu, South Korea")
;;     ("2015" "Milan, Italy")))
;; "Expo 86" ;; ;;"World Exposition on Transportation and Communication" "1986" "Vancouver, British Columbia"
;; "Expo 58" ;; "1958" "Brussels, Belgium"
;; "Expo 67" ;; "1967" "Montreal, Canada"
;; "Expo 70" ;; "1970" "Osaka, Japan"
;; "Expo 92" ;; "1992" "Seville, Spain" 

;;"Expo 19[0-9]\\{2,2\\}"
;;"Expo [0-9]\\{2,2\\}"
;;"Expo '[0-9]\\{2,2\\}"


;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-16T15:26:37-04:00Z}#{09383} - by MON>
(eval-and-compile
(defvar *naf-art-events-generic*
  '("Triennale"
    "Weissenhof Siedlung"
    "Esposizione dei Prodotti e delle Manufatture nazionale" ;; 1846 - Genoa, Sardinia
    "Esposizione Industriale" ;; 1854 - Genoa, Sardinia
    "Allgemeine deutsche Industrie-Ausstellung" ;; 1854 - Bavaria Munich, Bavaria
    )
  "*Keyword list of generic 'international' terms associated with art events.
Terms in this list should are non-English and non-French.
Used for `naf-mode' font-locking."))
;;
;;; CREATED: <Timestamp: #{2009-09-16T15:47:08-04:00Z}#{09383} - by MON>
(eval-and-compile
(defconst naf-mode-art-events-generic (concat (regexp-opt *naf-art-events-generic* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-art-events-generic* naf-mode-art-events-generic
				   *naf-mode-events-xrefs* naf-mode-event-fface))
;;
;; (progn (makunbound '*naf-art-events-generic*) (unintern '*naf-art-events-generic*)
;;       (makunbound 'naf-mode-art-events-generic) (unintern 'naf-mode-art-events-generic))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-16T15:26:37-04:00Z}#{09383} - by MON>
(eval-and-compile
(defvar *naf-art-events-generic-french*
  '("Congrès Internationaux" 
    "Exposition Universelle"  ;; "1900" "Paris, France"
    "l'Exposition Universelle"
    "Exposition universelle"
    "Exposition de"
    "Exposition des"
    "Exposition Internationale"
    ;;
    "Salon d'"
    "Salon de la"
    "Salon de"
    "Salon des")
  "*Keyword list of terms which flag on art events and occurences.
Used for  `naf-mode' font-locking."))
;;
;;; CREATED: <Timestamp: #{2009-09-16T15:47:08-04:00Z}#{09383} - by MON>
(eval-and-compile
(defconst naf-mode-art-events-generic-french (concat (regexp-opt *naf-art-events-generic-french* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-art-events-generic-french* naf-mode-art-events-generic-french
				   *naf-mode-events-xrefs* naf-mode-event-fface))
;;
;; (progn (makunbound '*naf-art-events-generic-french*) (unintern '*naf-art-events-generic-french*)
;;       (makunbound 'naf-mode-art-events-generic-french) (unintern 'naf-mode-art-events-generic-french))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-17T16:19:46-04:00Z}#{09384} - by MON>
(eval-and-compile
(defvar *naf-art-events-french*
  '("Salon d'Automne" ;; "October 31, 1903" "Petit Palais, Paris, France" ;;"October 18-November 25, 1905 Fauvist exhibit."
    "Salon d'automne"
    "Salon des Artistes Décorateurs"
    "Salon des Artistes Français"
    "Salon des artistes français"
    "Salon des Artistes Indépendants" ;; 1884
    "Salon des artistes indépendants" ;; 1884
    "Salon des réalités nouvelles"
    "Salon des Peintres"
    "Salon des peintres"
    "Exposition des Arts Décoratifs"
    "Exposition des Arts Décoratifs et Industriels"
    "Esposizione Internazionale d'Arte Decorativa Moderna"
    "Exposition Internationale des Arts et Techniques dans la Vie Moderne"
    "Exposition Internationale des Arts Decoratifs et Industriels Modernes"
    ;; (CIRPAC) - (International Committee for the Resolution of Problems in Contemporary Architecture)
    "Comité International pour la Résolution des Problèmes de l’Architecture Contemporaine"
    "Congrès Internationaux D'Architecture Moderne" ;; i.e. (CIAM) - CIAM was CIRPAC
    "International Congress of Modern Architecture" ;; i.e. (CIAM) - CIAM was CIRPAC
    ;;FOLLOWING: sponsored by the Union Centrale des Arts Décoratifs (UCAD)
    "L'Art de la femme"				      ;; "1892"
    "L'Art moderne cadre de la vie contemporaine"     ;; "1930"
    "La Siège français du Moyen Âge à nos jours"      ;; "1947"
    "Les Ateliers du goût"			      ;; "1948"
    )
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-art-events-french 
  (concat (regexp-opt *naf-art-events-french* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-art-events-french* naf-mode-art-events-french 
				      *naf-mode-events-xrefs* naf-mode-event-fface))
;;
;; (progn (makunbound '*naf-art-events-french*) (unintern '*naf-art-events-french*)
;;       (makunbound 'naf-mode-art-events-french) (unintern 'naf-mode-art-events-french))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-16T15:26:37-04:00Z}#{09383} - by MON>
(eval-and-compile
(defvar *naf-art-events-generic-english*
  '("World's Fair"
    "World's Fair of"
    "Universal Exposition"
    "First Exhibition of"
    "Exhibition of"
    ;;"Convention on "
    ;;
    "International Exhibition"
    "International Exposition"
    "National Exhibition"
    "National Exposition"
    "Annual International Exhibition"
    "Annual International Exposition"
    "International Exposition"
    ;;
    "Olympic Games"
    "st Olympic Games"
    "rd Olympic Games"
    "th Olympic Games"
    "st Olympiad"
    "rd Olympiad"
    "th Olympiad"
    )
  "*Keyword list of terms which flag on art events and occurences.
Used for `naf-mode' font-locking."))
;;
;;; CREATED: <Timestamp: #{2009-09-16T15:47:08-04:00Z}#{09383} - by MON>
(eval-and-compile
(defconst naf-mode-art-events-generic-english 
  (concat (regexp-opt *naf-art-events-generic-english* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-art-events-generic-english* naf-mode-art-events-generic-english
				   *naf-mode-events-xrefs* naf-mode-event-fface))
;;
;; (progn (makunbound '*naf-art-events-generic-english*) (unintern '*naf-art-events-generic-english*)
;;       (makunbound 'naf-mode-art-events-generic-english) (unintern 'naf-mode-art-events-generic-english))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-17T16:19:46-04:00Z}#{09384} - by MON>
(eval-and-compile
(defvar *naf-art-events-english*
  '("International Committee for the Resolution of Problems in Contemporary Architecture" ;; (CIRPAC)
    "International Congress of Modern Architecture" ;; i.e. (CIAM) - CIAM was CIRPAC    
    "Convention on International Exhibitions"
    "Exposition of British Society" ;; "1849" "Birmingham, England, United Kingdom" 
    "Panama Pacific Exposition"  
    "Panama-Pacific Exposition" ;; "1915" "San Francisco, California, United States"  
    "Pan-American Exposition"	;; "1901" "Buffalo, New York"
    "Pan American Exposition"
    "Exhibition of the Industry of All Nations" ;; "1853" "New York City, New York"
    "Art Treasures Exhibition" ;; "1857" "Royal Botanical Gardens, Stretford"
    "World's Columbian Exposition"
    "Columbian World's Exposition" ;; "18""Chicago, Illinois"
    "Columbian Worlds Fair"
    "Chicago Worlds Fair"
    "Chicago Columbian Exposition"
    "St. Louis Exposition" ;; "1904" "St. Louis, Missouri, United States"
    ;; "Louisiana Purchase Exposition" ;;coincides with "1904 Summer Olympics"
    ;;"Louisiana Purchase International Exposition and Olympic Games" 
    "Crystal Palace Exhibition"
    "Great Exhibition" 
    ;; "Great Exhibition of the Works of Industry of all Nations" 
    ;; "1851" "Hyde Park, London, England, United Kingdom"
    ;;
    ;; "American Exhibition of the Products, Arts and Manufactures of Foreign Nations"
    ;; "1883" "Boston, Massachusetts, United States"
    )
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-art-events-english 
  (concat (regexp-opt *naf-art-events-english* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-art-events-english* naf-mode-art-events-english 
				   *naf-mode-events-xrefs* naf-mode-event-fface))
;;
;; (progn (makunbound '*naf-art-events-english*) (unintern '*naf-art-events-english*)
;;       (makunbound 'naf-mode-art-events-english) (unintern 'naf-mode-art-events-english))


;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-16T15:44:48-04:00Z}#{09383} - by MON>
(eval-and-compile
(defvar *naf-world-events*
  '( ;; Wars
    "First World War"
    "Second World War"
    "Franco-Prussian War"
    "Revolutionary War"
    "The Great War"
    "the Great War"
    "Civil War"
    "Première Guerre mondiale"
    "Seconde Guerre mondiale"
    "WW I"
    "WW II"
    "WWI"
    "WWII"
    "World War I"
    "World War II")
  "*Keyword list for world events `naf-mode' font-locking."))
;;
;;; MODIFICATIONS: <Timestamp: #{2009-09-16T15:47:51-04:00Z}#{09383} - by MON>
;;; RENAMED: naf-mode-events -> naf-mode-world-events
(eval-and-compile
(defconst naf-mode-world-events (concat (regexp-opt *naf-world-events* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-world-events* naf-mode-world-events
				   *naf-mode-events-xrefs* naf-mode-event-fface))
;;
;; (progn (makunbound '*naf-world-events*) (unintern '*naf-world-events*)
;;       (makunbound 'naf-mode-world-events) (unintern 'naf-mode-world-events))

;;; ==============================
(provide 'naf-mode-events)
;;; ==============================

;;; ================================================================
;;; naf-mode-events.el ends here
;;; EOF

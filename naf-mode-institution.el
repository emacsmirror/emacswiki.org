;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-institution.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-institution keyword font-locking of institution names.
;;; `naf-mode' constants in this file use `naf-mode-institution-face'
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `naf-mode-benezit-museum-short'
;;; `naf-mode-institution-museum-names' 
;;; `naf-mode-academy-names'            
;;; `naf-mode-school-names-intnl'       
;;; `naf-mode-school-names-english'     
;;; `naf-mode-school-names-anchored'    
;;; `naf-mode-institution-names-generic'
;;;
;;; VARIABLES:
;;; `*naf-institution-museum-names*'
;;; `*naf-institution-names-anchored*'
;;; `*naf-institution-names-generic*'
;;; `*naf-academy-name*'
;;; `*naf-school-names-intnl*'
;;; `*naf-school-names-english*'
;;;
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;; Entire file needs re-contextualization, refactoring, granularity!!!
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-institution.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T21:32:51-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED: Autumn 2007/Winter 2008
;;; <Timestamp: #{2009-08-09T12:30:59-04:00Z}#{09327} - by MON KEY>
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
;;; ==============================
;;; Copyright © 2009 MON KEY
;;; ============================
;;; CODE:

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-13T13:40:52-04:00Z}#{09377} - by MON>
(eval-and-compile
(defvar *naf-mode-institution-xrefs* 
  '(naf-mode-benezit-museum-short 
    *naf-institution-museum-names* 
    *naf-institution-names-anchored* 
    *naf-institution-names-generic* 
    *naf-academy-name* 
    *naf-school-names-intnl* 
    *naf-school-names-english* 
    *naf-mode-institution-xrefs*
    mon-help-naf-mode-faces)
  "List of symbol names of variables which xref each other in the
naf-mode-institution package. 
See FILE: \"./naf-mode-institution.el\".\n►►►"))
;;
;;;test-me; *naf-mode-institution-xrefs*
;;;test-me;(remq 'naf-mode-benezit-museum-short *naf-mode-institution-xrefs*)

;;; ==============================
;;; Regexp at end catches Benezit abreviated museum forms.
;;; \\|\\(\\(Mus\\..\\)\\(\\(\\(Nat\\..\\)\\|\\(Mod..\\)\\|\\(d'Art.\\)\\)+\\)+?\\)
;;; \(\(Nat\|d'Art\|Mod\|Mus\|\.\)\)
;;; ==============================
(defconst naf-mode-benezit-museum-short 
  '"\\(Mus\\. \\(Mod\\.\\|Nat\\.\\|d'\\|d'Art\\( Mod\\.\\)?\\)\\)"
  "*Regexp of abreviated museum names for font-locking in `naf-mode' 
FONT-LOCKING-FACE: `naf-mode-institution-fface'.\n
EXAMPLE:
Mus. d'Art Mod.\nMus. Nat.\nMus. d'Art Mod.
Mus. des Beaux-Arts\nMus. d'Orsay\n
See also; `naf-mode-institution-museum-names', `naf-mode-academy-names'
`naf-mode-school-names-intnl', `naf-mode-school-names-english'
`naf-mode-institution-names-generic',`naf-mode-benezit-museum-short'
`naf-mode-inst-names-anchored'.\n►►►")
;;
;;;test-me;naf-mode-benezit-museum-short
;;
;;;(progn (makunbound 'naf-mode-benezit-museum-short) 
;;;       (unintern 'naf-mode-benezit-museum-short))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-13T12:12:30-04:00Z}#{09377} - by MON>
(eval-and-compile
(defvar *naf-institution-museum-names*
  (list "Bibliothèque nationale" ;; FRENCH MUSEUMS INSTNS:
        "Bibliothèque Nationale"
        "Musée National d'Art Moderne"
        "Musée d'Art Moderne"
	"Musée d'art Moderne"
        "Musée de l'Affiche"
        "Musée de l'affiche"
        "Musée de la Publicité"
        "Musée de la mode et du textile"
	"Musée de Picardie" 
        "Musée des arts décoratifs"
        "Musée Victoria et Albert"
        ;; MUSEUM ANCHOR:
        "Museum of Modern Art"
        "National Gallery of Art"
        ;; ENGLISH MUSEUMS AND LIBRARIES:
        "NYPL"
        "Library of Congress"
        "Huntington Library"
        "Pierpont Morgan Library"
        "Morgan Library & Museum"
        "Folger Shakespeare Library"
        "Brandywine River Museum"
        "Victoria and Albert Museum"
        "Guggenheim Museum"
        "Bridgeman Art Library"
        "Fogg Museum"
        "Whitney Museum"
        "Prints and Photographs Division"
        "Smithsonian Institution"
        "Cooper-Hewitt Museum"
        "Metropolitan Museum of Art"
        "Tate Museum"
        "Tate Gallery"
        "MOMA" 
        ;; INTERNATIONAL MUSEUMS
	"La Triennale di Milano" ;; Milan, Italy
	"Triennale di Milano" ;; Milan, Italy
        "Stedelijk Museum" ;; Amsterdam
        "Gesellschafts und Wirtschafts Museum")  ;; Vienna
  "*Keyword list of institution for keyword font-locking in `naf-mode'."))
;;
(eval-and-compile
(defconst naf-mode-institution-museum-names 
  (regexp-opt *naf-institution-museum-names* 'paren)))
;;
(eval-and-compile
(mon-help-swap-var-doc-const-val 
    *naf-institution-museum-names* naf-mode-institution-museum-names
    *naf-mode-institution-xrefs* naf-mode-institution-fface))
;;
;;(progn (makunbound '*naf-institution-museum-names*) 
;;       (unintern '*naf-institution-museum-names*)
;;       (makunbound 'naf-mode-institution-museum-names)
;;       (unintern 'naf-mode-institution-museum-names))

;;; ==============================
;;; INSTITUTIONS:
;;; ==============================

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-13T12:12:30-04:00Z}#{09377} - by MON>
(eval-and-compile
(defvar *naf-institution-names-anchored*
  '("Academy of Art" ;; ANCHORED INSTITUTION NAMES
    "Academy of Arts"
    "Academy of the Fine Arts" ;; "of Fine Arts" (difficult)
    "Academy of Fine Arts"
    "Academy of"
    "Académie de"
    "Académie des"
    "Académie des Beaux-Arts"
    "Académie des beaux-arts" ;; BNF's presentation style
    "Art Academy"
    "Art Alliance"
    "Art Club"
    "Art Institute of"
    "Art Institute"
    "Art Museum"
    "Art School"
    "Athenaeum"
    "Central School of"
    "Central School"
    "College of"
    "College"
    "Council of"
    "Ecole de Beaux Arts"
    "Ecole des Beaux Arts"
    "Ecole des Beaux-Arts"
    "École des Beaux-Arts"
    "École des Arts Industriels"
    "Historical Society"
    "Institut de France"
    "Institute of Art"
    "Institute of Design"
    "Institute of Fine Arts"
    "Institute of Graphic Arts"
    "Institute of"
    "L'Académie des Beaux-Arts"
    "L'Académie des"
    "l'Académie royale de"
    "L'Académie royale de"
    "Museum"
    "Muesuem of"
    "Museum of Art"
    "Museum School of Art"
    "Museum School of Fine Arts"
    "Museum School of Industrial Design"
    "Museum School"
    "Musée National d'"
    "Musée National d'Art"
    "Musée National de"
    "Musée National des"
    "Musée National"
    "Musée d'Art"
    "Musée de l'"
    "Musée de la"
    "Musée des Arts"
    "Musée des arts"
    "National Acedemy"
    "National Academy of"
    "National Institute of"
    "National Institute"
    "National Museum of"
    "National Museum"
    "National Society of"
    "National Society"
    "Public Library"	
    "Royal Academy of"
    "Royal Academy"
    "Royal Society for"
    "Royal Society of Arts"
    "School of Applied Art"
    "School of Art and"
    "School of Art"
    "School of Design"
    "School of Design"
    "School of Fine Art"
    "School of Fine Arts"
    "School of Fine and Applied Art"
    "School of Fine and Industrial Art"
    "School of Visual Arts"
    "Society of Arts"
    "Society of"
    "Socieété des"
    "Société Nationale"
    "Technical School"
    "University of"
    "University"
    "Académie de"
    "l'Académie de"
    "l'Académie Royale"
    "l'Académie française"
    "l'Académie des Beaux-Arts"
    "l'Ecole de Beaux-Arts"
    "l'École de l'"
    "l'École de"
    "l'École des Arts Appliqués"
    "l'École des Arts Décoratifs"
    "l'École des Arts et Métiers Graphiques"
    "l'École des Beaux Arts"
    "l'École des Beaux-Arts de"
    "l'École des")
  "*Keyword list of anchored institution names for font-locking in `naf-mode'."))
;;
(eval-and-compile
(defconst naf-mode-inst-names-anchored 
  (regexp-opt *naf-institution-names-anchored* 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val 
      *naf-institution-names-anchored* naf-mode-inst-names-anchored
      *naf-mode-institution-xrefs* naf-mode-institution-fface))
;;
;;;(progn (makunbound '*naf-institution-names-anchored*) (unintern '*naf-institution-names-anchored*))
;;;       (makunbound 'naf-mode-inst-names-anchored) (unintern 'naf-mode-inst-names-anchored)
;;        

;;; Royal Society for the Encouragement of Arts, Manufactures and Commerce
;;; ==============================
;;; RENAME: *naf-institution-names-generic*  -> *naf-institution-names-generic-english* 
;;; MODIFICATIONS: <Timestamp: #{2009-09-13T12:12:30-04:00Z}#{09377} - by MON>
(eval-and-compile
(defvar *naf-institution-names-generic* 
  '("Architectural League"
    "Art Directors Club"
    "Art Workers' Guild"
    "Artist Association"
    "Artist Guild"
    "Artists Association"
    "Artists Guild"
    "Federal Works Progress Administration"
    "Graphic Artist Guild"
    "Painters Club"
    "Palette and Chisel Club"
    "Pen and Pencil Club"
    "Players Club"
    "Salmagundi Club"
    "Salon International"
    "Silvermine Guild"
    "Sketch Club"
    "Society of Illustrators"
    "Society of Industrial Artists"
    "Society of Landscape Painters"
    "Society of Publication Designers"
    "Society of the Fine Arts"
    "Tavern Club"
    "Type Directors Club"
    "Watercolor Club"
    "Watercolor Society")
  "*Keyword list of generic institution names for use in `naf-mode'.
Matches institution names associated with the _word_ 'Art'."))
;;
(eval-and-compile
(defconst naf-mode-institution-names-generic 
  (regexp-opt *naf-institution-names-generic* 'paren)))
;;
(eval-and-compile 
(mon-help-swap-var-doc-const-val 
    *naf-institution-names-generic* naf-mode-institution-names-generic
    *naf-mode-institution-xrefs* naf-mode-institution-fface))
;;
;;(progn (makunbound '*naf-institution-names-generic*)
;;       (unintern '*naf-institution-names-generic*)
;;       (makunbound 'naf-mode-institution-names-generic)
;;       (unintern 'naf-mode-institution-names-generic))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-13T12:12:30-04:00Z}#{09377} - by MON>
(eval-and-compile
(defvar *naf-academy-name* 
  '("l'Académie de la Grande Chaumière"
    "l'Académie Julian"       ;; Julian (Rodolphe) (1839-1907)
    "Académie Delphinale"     ;; established 1772 
    "Academie Colorossi"
    "Académie Colorossi"
    "Colorossi Academy"
    "Académie de Saint-Luc"
    "Académie de la Palete"
    "Académie Julian"
    "Academie Julian"
    "Académie Gérôme"        ;; Jean Léon Gérôme
    "Académie Boulanger"     ;; Gustave Boulanger
    "Académie Bonnat"        ;; Léon Bonnat
    "Academie Royale"
    "Académie Royale"
    ;; Theater School est. 1870 by Stanisław Koźmian
    ;; Close ties w/ Moscow Art Theatre later Lee Strasberg, etc.
    "Szkola Krakowa" 	"Szkoła Krakowa"
    "Szkola Kraków"  	"Szkoła Kraków" 
    "Szkola Krakówa" 	"Szkoła Krakówa"
    ;;Московский Художественный Академический Театр
    "Moscow Art Theatre")
  "*Keyword list of academy names for font-locking in `naf-mode'.
Regexps built from this list match 19th-20th C Parisian and French academies of
Art, Arts related training Institutions, and 'Art Schools'."))
;;
(eval-and-compile 
(defconst naf-mode-academy-names 
  (regexp-opt *naf-academy-name* 'paren)))
;;
(eval-and-compile 
(mon-help-swap-var-doc-const-val  *naf-academy-name*  naf-mode-academy-names 
                                  *naf-mode-institution-xrefs* naf-mode-institution-fface))
   
;;
;;(progn (makunbound 'naf-mode-academy-names)
;;       (unintern 'naf-mode-academy-names)
;;       (makunbound '*naf-academy-name*)
;;       (unintern '*naf-academy-name*))

;;; ==============================
;;; European and French Schools - non anchored:
;;; ==============================

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-13T12:12:30-04:00Z}#{09377} - by MON>
(eval-and-compile
(defvar *naf-school-names-intnl*
  '(;; INTERNATIONAL NAMED SCHOOLS:
    "Bureau International des Expositions"
    "International Exhibitions Bureau"
    "Council of Industrial Design"
    "Société des Artistes Indépendants";; "July 19, 1884" "Paris, France'
    "Société des artistes indépendants";; "July 19, 1884" "Paris, France'
    "Société des artistes français" ;; (SAF) "Decmeber 27, 1881" "Ferry (Jules)" 
    "Société du Musée des Arts Décoratifs"
    "Société des Artistes-Décorateurs"
    "Société des artistes français"
    "Société Nationale des Beaux-Arts"
    "Société des Peintres-Graveurs"
    "Syndicat National des Peintres Illustrateurs"
    "Sociétaire du Salon des Artistes Français"
    "Société d'Encouragement à l'Industrie Nationale"
    "l'Union des Arts Décoratifs"
    "Union des Artistes Modernes" ;; i.e. (UAM) ;; group
    "Union des Arts Décoratifs"
    "Union Centrale des Arts Décoratifs" ;; i.e. (UCAD)
    "Union Centrale des Beaux-Arts Appliqués à l'Industrie"
    "Conservatoire des Arts et Métiers"
    "Centre de Création Industrielle"
    "Centre de Creation Industrielle" ;; CCI est. 1969 as part of (UCAD)
    "Art Institute Orell Füssli" ;; (Switzerland)
    "Real Academia de Bellas Artes de San Fernando"
    "Staatlichen Kunstakademie"
    "Ulm Hochschule fur Gestaltun" ;; i.e. (Ulm College of Design)
    "French Academy in Rome"
    "Circulo de Bellas Artes" ;; i.e. (Fine Arts Society) in Madrid
    ;; (Vienna Workshops, Production Co-operative of Artist Craftsmen in Vienna - Wiener Werkstatte)    
    "Produktive Germeinschaft von Kunsthandwerkern in Wien"
    ;; MVWKH really a school but...
    "Munchner Vereinigte Werkstatten für Kunst in Handwerk")
  "*Keyword list of Schools, Academies, Training Institutions, Syndicates,
Organizations, Councils, Associatieas, etc. These oare either common, generic,
or lack specificity to be included elswhere in the naf-mode-institution package.
Used for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-school-names-intnl 
  (regexp-opt *naf-school-names-intnl* 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-school-names-intnl* naf-mode-school-names-intnl 
                                   *naf-mode-institution-xrefs* naf-mode-institution-fface))
;;
;;(progn (makunbound '*naf-mode-school-names-intnl*)
;;       (unintern '*naf-school-names-intnl*)
;;       (makunbound 'naf-mode-school-names-intnl) 
;;       (unintern 'naf-mode-school-names-intnl))

;;; ==============================
;;; English and American School names, non-anchored.
;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-13T12:12:30-04:00Z}#{09377} - by MON>
(eval-and-compile
(defvar *naf-school-names-english*
  '( ;; ENGLISH NAMED SCHOOLS:
    "Art Students League of" 
    "Art Students League"
    "Art Center School of Design"
    "Art Center College of Design"
    "Black Mountain"
    "Brandywine School"
    "Camden Arts Centre"
    "Central School of Art and Design" ;; London 
    "Cooper Union"                     ;; New York
    "Corcoran School of Art"           ;; Washington, D.C.
    "Cranbrook Academy of Art"
    "Drexel Institute"
    "Fashion Institute of Technology"
    "Grand Central School of Art"
    "Phoenix Art Institute"    ;; New York
    "Silvermine School of Art" ;; Norwalk, Connecticut
    "Illustration Academy"     ;; Kansas City
    "Harrison Commercial Art Institute"
    "Lowell Institute"
    "Linthicum Institute" 
    "Otis Art Institute"
    "Central Academy of Commercial Art" ;; Cincinnati, Ohio
    "Bunzlau State Ceramic School"
    "Fenway School of Illustration"
    "Frank Reilly School of Art"
    "New School for Social Research"
    "Parsons School of Design" ;; New York
    "Pratt School"
    "Pratt Institute"
    "Philadelphia School of Design for Women" ;; i.e. (PSDW)
    "St Martin's School of Art"
    "World's Art Center")
  "*Keyword list art training Schools/Institutions for `naf-mode' font-locking.
List is primarily English and American. 
Anchored names in `naf-mode-inst-names-anchored' catch most of the difference."))
;;
(eval-and-compile
(defconst naf-mode-school-names-english 
  (regexp-opt *naf-school-names-english* 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val
      *naf-school-names-english* naf-mode-school-names-english  
      *naf-mode-institution-xrefs* naf-mode-institution-fface))
;;
;;;(progn (makunbound '*naf-school-names-english*) 
;;;        (unintern '*naf-school-names-english*)
;;;        (makunbound 'naf-mode-school-names-english) 
;;;        (unintern 'naf-mode-school-names-english))  

;;; ==============================
;;; When testing/recompiling:
;; (progn 
;;   (makunbound '*naf-mode-institution-xrefs*)       (unintern '*naf-mode-institution-xrefs*)
;;   (makunbound '*naf-school-names-english*) (unintern '*naf-school-names-english*)
;;   (makunbound 'naf-mode-school-names-english) (unintern 'naf-mode-school-names-english)  
;;   (makunbound '*naf-mode-school-names-intnl*) (unintern '*naf-school-names-intnl*)
;;   (makunbound 'naf-mode-school-names-intnl) (unintern 'naf-mode-school-names-intnl)
;;   (makunbound 'naf-mode-academy-names) (unintern 'naf-mode-academy-names)
;;   (makunbound '*naf-academy-name*) (unintern '*naf-academy-name*)
;;   (makunbound '*naf-institution-names-generic*) (unintern '*naf-institution-names-generic*)
;;   (makunbound 'naf-mode-institution-names-generic) (unintern 'naf-mode-institution-names-generic)
;;   (makunbound '*naf-institution-names-anchored*) (unintern '*naf-institution-names-anchored*)
;;   (makunbound 'naf-mode-inst-names-anchored) (unintern 'naf-mode-inst-names-anchored)
;;   (makunbound '*naf-institution-museum-names*) (unintern '*naf-institution-museum-names*)
;;   (makunbound 'naf-mode-institution-museum-names) (unintern 'naf-mode-institution-museum-names)
;;   (makunbound 'naf-mode-benezit-museum-short) (unintern 'naf-mode-benezit-museum-short))

;;; ==============================
(provide 'naf-mode-institution)
;;; ==============================

;;; ================================================================
;;; naf-mode-institution.el ends here
;;; EOF

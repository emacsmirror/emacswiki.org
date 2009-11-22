;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-db-fields.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-db-fields lists data fields inserted and headwords for `naf-mode'.
;;; These fields are present in one or more NAF types defined by skeletons in
;;; in ./naf-skeletons.el
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;;
;;; `naf-mode-delim',`naf-mode-comment-delim', 
;;; `naf-mode-db-entry', `naf-mode-name-divider',
;;; `naf-mode-field-names', `naf-mode-field-names-bnf', 
;;; `naf-mode-db-field-flags-bnf',
;;;
;;; VARIABLES:
;;;
;;; MACROS:
;;;
;;; SUBST or ALIASES:
;;;
;;; DEPRECATED, RENAMED, OR MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO: naf-mode-field-names needs to be refactored per DB e.g. 
;;; LOC-generic:, ULAN, BNF, OCLC, etc.
;;;
;;; NOTES:
;;; This file uses the provide/require idiom because of the defconstant forms.
;;;
;;; SNIPPETS:
;;; See also; `naf-mode-field-names', `naf-mode-field-names-bnf', `naf-mode-db-entry',
;;; `naf-mode-db-field-flags-bnf',`naf-mode-ulan-rltd-ppl-corp', 
;;; `naf-mode-delim',`naf-mode-comment-delim', `naf-mode-name-divider'.
;;; Used in `naf-mode'.
;;;
;;; THIRD PARTY CODE:
;;;
;;; THIRD-PARTY-SOURCES:
;;; LOC -> Library Of Congress
;;; (URL `http://www.loc.gov/index.html') - LOC homepage
;;; (URL `http://authorities.loc.gov') - Authority Files
;;; (URL `http://catalog.loc.gov/') - Publications Catalog
;;; (URL `http://www.loc.gov/rr/print/catalog.html') - Prints & Photographs
;;; LOC does not charge permission fees for use of such material and generally
;;; does not grant or deny permission to publish or otherwise distribute
;;; material in its collections.
;;;
;;; BNF -> Bibliothèque Nationale de France
;;; (URL `http://catalogue.bnf.fr/jsp/recherche_autorites_bnf.jsp?nouvelleRecherche=O&host=catalogue') 
;;; (URL `http://www.bnf.fr/pages/zNavigat/frame/version_anglaise.htm?ancre=english.htm') - catalog
;;; (URL `http://www.bnf.fr/pages/outils/auteur.htm') - editorial mention and droits d'auteur
;;; (URL `http://www.bnf.fr/') - BNF homepage
;;;
;;; OCLC -> Online Computer Library Center;
;;; (URL `http://firstsearch.oclc.org/') -  OCLC First Search
;;; FirstSearch® Copyright © 1992-2009 OCLC as to electronic presentation and
;;; platform. All Rights Reserved.
;;;
;;; ULAN -> Union List of Artist Names
;;; (URL `http://www.getty.edu/research/conducting_research/vocabularies/ulan/')
;;; Portions of lists contained herein were sourced from publicly accessible 
;;; data made available at getty.edu. The digital version of the ULAN is 
;;; Copyright © J. Paul Getty Trust.  Code presented or contained of following file
;;; does not in any way represent the ULAN, J. Paul Getty Trust, www.getty.edu, nor
;;; their associates or affiliates.
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-db-fields.el')
;;; FILE-PUBLISHED:<Timestamp: #{2009-11-21T20:38:31-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-09T11:59:32-04:00Z}#{09327} - by MON KEY>
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
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:


;;; ==============================
(provide 'naf-mode-db-fields)
;;; ==============================

;;; ==============================
(defconst naf-mode-delim '"^-?+"
  "*Keyword regexp for fontlocking level section divider in \.naf files.\n
EXAMPLE:\n\"---\"\n
Fontlocking of this keyword provided by `naf-mode-delim-face' with var 
`naf-mode-delim-fface'.\n
See also; `naf-mode-field-names', `naf-mode-field-names-bnf', `naf-mode-db-entry',
`naf-mode-db-field-flags-bnf',`naf-mode-ulan-rltd-ppl-corp',`naf-mode-comment-delim',
`naf-mode-name-divider'.\nUsed in `naf-mode'.")

;;;test-me;naf-mode-delim
;;;(progn (unintern 'naf-mode-delim)(makunbound 'naf-mode-delim))
       
;;; ==============================
(defconst naf-mode-comment-delim '"^;;; ==+[[:space:]]?$"
  "*Keyword regexp for fontlocking elsip style 'comment divider' in .naf files.\n
EXAMPLE:\n
\";;; ==============================\"\n
Fontlocking provided by `naf-mode-delim-face' with var `naf-mode-delim-fface'.
See also; `naf-mode-field-names', `naf-mode-field-names-bnf', `naf-mode-db-entry',
`naf-mode-db-field-flags-bnf',`naf-mode-ulan-rltd-ppl-corp', `naf-mode-delim',
`naf-mode-name-divider'.\nUsed in `naf-mode'.")

;;;test-me; naf-mode-comment-delim
;;;(progn (makunbound 'naf-mode-comment-delim) (unintern 'naf-mode-comment-delim))

;;; ==============================
(defconst naf-mode-name-divider '"|"
  "*Keyword for default divider in 'Used-for:' field when creating .naf files.
EXAMPLE:\n
Lastname, Firstname | Firstname Lastname | F. Lastname | Lastname, Variantname\n
Font-locking is provided by `naf-mode-name-divider-fface'
See also; `naf-mode-field-names', `naf-mode-field-names-bnf', `naf-mode-db-entry',
`naf-mode-db-field-flags-bnf',`naf-mode-ulan-rltd-ppl-corp', `naf-mode-delim',
`naf-mode-comment-delim'.\nUsed in `naf-mode'.")

;;;test-me; naf-mode-name-divider
;;;(progn (makunbound 'naf-mode-name-divider ) (unintern 'naf-mode-name-divider ))

;;; ==============================
(let ((naf-db-entry
       (list 
        ";; -\*- mode: NAF; -\*-" ;see if this works. will yield 6(six) slashes when optimized. should work
        "BNF:"
        "DNB:"
        "LOC:"
        "LOC-P&P:"
        "ULAN:"
        "OCLC:"
        "OTHER-DB:"
        "COPAC:"
        "Bios:"
        "Artist-naf:"
        "Artist-doc:"
        "Book-naf:"
        "Book-doc:"
        "People-naf:"
        "People-doc:"
        "Author-naf:"
        "Author-doc:"
        "Brand-naf:"
        "Brand-doc:"
        "non-posting-source:"
        "non-posting-wiki-source:"
        "non-posting-ebay-source:"
        "non-posting-philsp-source:"
        "non-posting-imdb-source:"
        "non-posting-internet-source:"
        "non-posting-benezit-source:"
        "accessed:"
        "references:"
        "source:"
        "Source:"
        "SOURCE:"
        ";;; brand-naf EOF"
        ";;; artist-naf EOF"
        ";;; people-naf EOF"
        ";;; book-naf EOF"
        ";;; author-naf EOF"
        ";;; item-naf EOF"
        )))
;;
(defconst naf-mode-db-entry (concat "^" (regexp-opt naf-db-entry 'paren))
  "*Keyword list of automatically inserted fields created with naf templates.
Templates include: `artist-naf', `brand-naf', `people-naf', `book-naf',
`author-naf', `item-naf'. Fontlocking provided by `naf-mode-db-entry-face' 
with var `naf-mode-db-entry-fface'.
See also; `naf-mode-field-names', `naf-mode-field-names-bnf', `naf-mode-db-entry',
`naf-mode-db-field-flags-bnf',`naf-mode-ulan-rltd-ppl-corp', `naf-mode-delim',
`naf-mode-comment-delim', `naf-mode-name-divider'.\nUsed in `naf-mode'."))

;;;test-me; naf-mode-db-entry
;;;(progn (makunbound 'naf-mode-db-entry)(unintern 'naf-mode-db-entry))

;;; ==============================
(let ((naf-field-names (list
			"Abbreviated Title:"  
			"Accession No:" 
			"Ads-for:"            ;dbc-field
			"Appeared-in:"        ;dbc-field
			"Artists-associated:" ;dbc-field
			"Auction-records:"    ;dbc-field
			"Author(s):" 
			"Authors-associated:"           ;dbc-field
			"Biographies:"                  ;ULAN 
			"Biographical/Historical Note:" ;LOC-field
			"Birth and Death Places:"       ;ULAN *
			"Book-notes:"                   ;dbc-field
			"Born:"                         ;dbc-field, ULAN
			"Brand-name:"                   ;dbc-field
			"CALL NUMBER:" 
			"CONTROL #:" 
			"CREATED/PUBLISHED:" 
			"CREATOR:" 
			"Class Descriptors:" 
			"Content-and-subjects:" 
			"Contents:"     ;dbc-field
			"Continues:" 
			"Corporate Name:" 
			"Current Frequency:" 
			"DIGITAL ID:" 
			"Database:" 
			"Date-founded:" ;dbc-field
			"Description:" 
			"Descriptor:" 
			"Dewey Class No.:" 
			"Died:"         ;dbc-field, ;ULAN
			"Display-name:" 
			"Display-title:" 
			"Document Type:" 
			"education: "         ;ULAN ;use TRL-WSP
			"Edition Information:" 
			"Entry:"
			"Events:"             ;ULAN
			"ebay-item-number:"   ;dbc-field
			"ebay-item-seller:"   ;dbc-field
			"ebay-item-realized:" ;dbc-field
			"ebay-item-ended:"    ;dbc-field
			"FORMAT:" 
			"Found In:" 
			"Founded-by:"   ;dbc-field
			"Frequency:" 
			"Full-title:"   ;dbc-field
			"Gender:"       ;ULAN
			"Genre/Form:" 
			"Genre:"
			"Geographic Area Code:" 
			"HEADING:" 
			"Heading:" 
			"ID:"           ;ULAN
			"ISSN:" 
			"Identifier:" 
			"LC Class Number:"   ;LOC-field
			"LC Classification:" ;LOC-field
			"LC Control No."     ;LOC-field
			"LC Control Number:" ;LOC-field
			"LC Copy:"           ;LOC-field
			"LCCN Permalink:" 
                        "Cancel/Invalid LCCN:" ;LOC-field
			"Language:" 
			"List/Hierarchical Position:" ;ULAN
			"Location-country:" 
			"Location-published:" ;dbc-field
			"MEDIUM:" 
			"Main author:"
			"Main Title:" 
			"Material Type:" 
			"Movie-posters:" 
			"Notes:" 
			"NOTES:" 
			"Note(s):"
			"Note:" 
			"Named Person:" 
			"Names:"         ;ULAN
			"Nationalities:" ;ULAN
			"Number-of-illustrations:" 
			"Number-of-pages:" 
			"Number-of-volumes:" 
			"Other Edition Available:" 
			"Other System No.:" 
			"Other names:"
			"Other Titles:" 
			"Other-roles:"
			"Physical desc\.:"
			"Personal Name:" 
			"Preceding Title:" 
			"Products-associated:" ;dbc-field 
			"Publication :" 
			"Publication:" 
			"Publication Dates:" 
			"Published-by:" 
			"Published/Created:" 
			"Publisher Location:" 
			"Publisher:"    ;dbc-field
			"Quality Code:" 
			"REPOSITORY:" 
			"REPRODUCTION NUMBER:" 
			"RIGHTS INFORMATION:" 
			"Record Type:"  ;ULAN 
			"Related Names:" 
			"Related People and Corporate Bodies:" ;ULAN
			"Related People or Corporate Bodies:"  ;ULAN
			"Relevance:" 
			"Repository:" 
			"Reproduction No./Source:" 
			"Responsibility:" 
			"Roles:"        ;dbc-field, ;ULAN
			"SUBJECTS:" 
			"SUMMARY:" 
			"Scope Note:" 
			"Search Also Under:" 
			"Series Title:" 
			"Slogans:"                  ;dbc-field
			"Sources and Contributors:" ;ULAN
			"Special Note:" 
			"Special-notes:" 
			"Standard No:" 
			"Subject:"      ;ULAN
			"Subjects:" 
			"Succeeding Title:" 
			"Summary:"
			"TITLE:" 
			"Title:"
			"Title details:"
			"Type :" 
			"Type of Material:" 
			"Uniform Title:" 
			"Update:" 
			"Uploaded-by:"  ;dbc-field
			"Used For/See From:" 
			"Used-for:"     ;dbc-field
			"Year:")))
  ;;		
  (defconst naf-mode-field-names (concat "^"  (regexp-opt naf-field-names 'paren))
    "*Keywords for BOL font-locking with `naf-mode-db-field-face'.
Sections of `artist-naf', `author-naf', `book-naf', `brand-naf', and `people-naf'.
`item-naf' fields are not defined here yet.\n
Note: this constant will be refactored to accomodate National db fields of LOC,
ULAN, and OCLC with these inheriting face attributes from `naf-mode-field-face'
and taking the concatenated form naf-mode-field-names-*. 
Global var bound to `naf-mode-field-fface'.\n
See also; `naf-mode-field-names', `naf-mode-field-names-bnf', `naf-mode-db-entry',
`naf-mode-db-field-flags-bnf',`naf-mode-ulan-rltd-ppl-corp'.\nUsed in `naf-mode'."))

;;;test-me; naf-mode-field-names  
;;;(progn (makunbound 'naf-mode-field-names) (unintern 'naf-mode-field-names))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-11T16:38:02-04:00Z}#{09375} - by MON KEY>
;;; ADDED: new BNF fields. 
;;; Where there is potential for overlap followed them w/ WSP e.g. :
;;; "BNF-FIELD : "
;;; ==============================
(let ((naf-field-names-bnf 
       (list
	"Appartient au recueil :"
	"Auteur(s) : "                             ;BNF *TRL-WSP   
	"Autre(s) auteur(s) :"			   ;BNF
	"Autre(s) forme(s) du titre :"		   ;BNF
	"Circuit de distribution :"                ;BNF  *TRL-WSP
	"Classement géographique :"                ;BNF
	;; "Collection :"	  ;I can't bring myself to trust this one :(
	"Comprend : "                              ;BNF  *TRL-WSP
	"Coordonnées géographiques :"		   ;BNF
	"Cote(s) BnF :"				   ;BNF
	"Création :"				   ;BNF
	"Description matérielle :"		   ;BNF
	"Distributeur : "                          ;BNF *TRL-WSP
	"Domaine(s) : "				   ;BNF *TRL-WSP
	"Domaine(s) d'expression artistique :"	   ;BNF
	"Enregistrement :"                         ;BNF
	"Forme(s) rejetée(s) :"			   ;BNF
	"Genre : "                                 ;BNF *TRL-WSP
	"Indice(s) Dewey :"			   ;BNF 
	"Indice de l'Histoire de France :"         ;BNF
	"Interprète(s) :"                          ;BNF
	"Langue(s) :"				   ;BNF
	"Lien au titre d'ensemble :"               ;BNF
	"Lien à la collection :"                   ;BNF
	"Marque : "                                ;BNF *TRL-WSP
	"Mort :"				   ;BNF
	"Naissance :"				   ;BNF
	"Nation(s) : "                             ;BNF *TRL-WSP
	"Nationalité(s) :"                         ;BNF
	"Note(s) : "                               ;BNF *TRL-WSP
	"Notice n° :"				   ;BNF
	"Numérotation :"			   ;BNF
	"Participant(s) : "                        ;BNF *TRL-WSP
	"Producteur(s) :"                          ;BNF 
	"Profession(s) : "			   ;BNF
	"Projection : "                            ;BNF *TRL-WSP
	"Publication : "                           ;BNF *TRL-WSP
	"Périodicité :"				   ;BNF
	"Responsabilité(s) exercée(s) sur les documents :" ;BNF
	"Référence(s) commerciale(s) :"                    ;BNF
	"Réunit : "                                        ;BNF *TRL-WSP
	"Sexe : "					   ;BNF *TRL-WSP
	"Source(s) : "					   ;BNF *TRL-WSP
	"Sujet(s) : "                                      ;BNF
	"Sujet(s) géographique(s) :"                       ;BNF
	"Technique(s) privilégiée(s) :"			   ;BNF
	"Titre clé :"					   ;BNF
	"Titre d'ensemble :"                               ;BNF
	"Titre(s) :"					   ;BNF
	"Titre(s) en liaison :"				   ;BNF
	"Thème(s) : "                                      ;BNF *TRL-WSP
	"Typologie : "                                     ;BNF *TRL-WSP
	"Type de publication :"                            ;BNF 
	"Type de ressource électronique :"		   ;BNF
	"Type de la collectivité officielle :"		   ;BNF
	"Échelle(s) :"                                     ;BNF
	"Éditeur : "                                       ;BNF *TRL-WSP
	"Édition : "                                       ;BNF *TRL-WSP
	"<Employé pour :"
	)))
;;
(defconst naf-mode-field-names-bnf
  (concat "^" (regexp-opt naf-field-names-bnf 'paren) );"\\>") 
  "*Keywords for BNF db-field entries used in .naf files.
Fontlocking with `naf-mode-field-bnf-face' with var `naf-mode-field-bnf-fface'.
See: BIBLIOTHÈQUE NATIONALE DE FRANCE:
\(URL `http://catalogue.bnf.fr/jsp/recherche_autorites_bnf.jsp?nouvelleRecherche=O&host=catalogue') 
\(URL `http://www.bnf.fr/pages/zNavigat/frame/version_anglaise.htm?ancre=english.htm')
See also; `naf-mode-field-names', `naf-mode-db-entry', `naf-mode-db-field-flags-bnf',
`naf-mode-ulan-rltd-ppl-corp', `naf-mode-delim', `naf-mode-comment-delim',
`naf-mode-name-divider'.\nUsed in `naf-mode'."))

;;;test-me; naf-mode-field-names-bnf
;;;(progn (makunbound 'naf-mode-field-names-bnf) (unintern 'naf-mode-field-names-bnf))
  
;;; ==============================
;;; TODO: Build an anchored fontlock for the BNF field 'Type :'.
;;;  ("Type : " . {enregistrement sonore, texte imprimé, monographie, 
;;;                document électronique, document cartographique,
;;;                image animée, image fixe, recueil de pièces
;;;                multi-media,
;;;   }+)
;;; ==============================
(let ((naf-db-field-flags-bnf
       (list
	"forme internationale"
	"Mise à jour : " ;don't concat regex-opt, "Mise à jour :" won't catching ':' is notword const
	"masculin" 
	"féminin" ;"^\\(Sexe : \\(masculin\\|féminin\\)\\)"
	)))
;;
(defconst naf-mode-db-field-flags-bnf (concat "\\<" (regexp-opt naf-db-field-flags-bnf 'paren))
  "*Keyowrds for fontlocking BNF terms which appear in conjunction with 
`naf-mode-field-names-bnf' but which occur in a secondary place of those fields.
See: BIBLIOTHÈQUE NATIONALE DE FRANCE:
\(URL `http://catalogue.bnf.fr/jsp/recherche_autorites_bnf.jsp?nouvelleRecherche=O&host=catalogue') 
\(URL `http://www.bnf.fr/pages/zNavigat/frame/version_anglaise.htm?ancre=english.htm')
See also; `naf-mode-field-names', `naf-mode-field-names-bnf', `naf-mode-db-entry',
`naf-mode-db-field-flags-bnf',`naf-mode-ulan-rltd-ppl-corp', `naf-mode-delim',
`naf-mode-comment-delim', `naf-mode-name-divider'.\nUsed in `naf-mode'."))

;;;test-me;naf-mode-db-field-flags-bnf
;;;(progn (makunbound 'naf-mode-db-field-flags-bnf)(unintern 'naf-mode-db-field-flags-bnf))
                   
;;; ==============================
;;; BNF FIELD TESTS:
;;; (insert naf-mode-field-names-bnf)
;;; ==============================
;;;test-me;Autre(s) auteur(s) : 
;;;test-me;Autre(s) forme(s) du titre : 
;;;test-me;Création : 
;;;test-me;Description matérielle : 
;;;test-me;Domaine(s) : 
;;;test-me;Forme(s) rejetée(s) : 
;;;test-me;Indice(s) Dewey : 
;;;test-me;Langue(s) : 
;;;test-me;Mort :" 
;;;test-me;Naissance :
;;;test-me;Note(s) :"			
;;;test-me;Notice n° :
;;;test-me;Numérotation :
;;;test-me;Profession(s) : 
;;;test-me;Périodicité :
;;;test-me;Responsabilité(s) exercée(s) sur les documents :
;;;test-me;Sexe :
;;;test-me;Source(s) :
;;;test-me;Technique(s) privilégiée(s) :
;;;test-me;Titre clé :
;;;test-me;Titre(s) :" 
;;;test-me;Titre(s) en liaison :
;;;test-me;Appartient au recueil :
;;;test-me;Auteur(s) : 
;;;test-me;Circuit de distribution :
;;;test-me;Classement géographique :
;;;test-me;Comprend : 
;;;test-me;Coordonnées géographiques :
;;;test-me;Cote(s) BnF : 
;;;test-me;Distributeur : 
;;;test-me;Enregistrement :
;;;test-me;Genre : 
;;;test-me;Indice de l'Histoire de France :
;;;test-me;Interprète(s) :
;;;test-me;Lien au titre d'ensemble :
;;;test-me;Lien à la collection :
;;;test-me;Marque : 
;;;test-me;Nation(s) : 
;;;test-me;Participant(s) : 
;;;test-me;Producteur(s) :
;;;test-me;Projection : 
;;;test-me;Référence(s) commerciale(s) :
;;;test-me;Réunit : 
;;;test-me;Sujet(s) : 
;;;test-me;Sujet(s) géographique(s) :
;;;test-me;Titre d'ensemble :
;;;test-me;Thème(s) : 
;;;test-me;Typologie : 
;;;test-me;Type de publication :
;;;test-me;Type de ressource électronique :
;;;test-me;Type de la collectivité officielle :
;;;test-me;Échelle(s) :
;;;test-me;Éditeur : 
;;;test-me;Édition : 
;;;test-me;<Employé pour :

;;; ==============================
;;; NAF-MODE-FIELD-NAMES-TESTS:
;;; (insert naf-mode-field-names)
;;; ==============================
;;;test-me;Abbreviated Title:  
;;;test-me;Accession No: 
;;;test-me;Ads-for: 
;;;test-me;Appeared-in: 
;;;test-me;Artists-associated: 
;;;test-me;Auction-records:
;;;test-me;Author(s): 
;;;test-me;Authors-associated: 
;;;test-me;Biographies: 
;;;test-me;Birth and Death Places: 
;;;test-me;Book-notes: 
;;;test-me;Born: 
;;;test-me;Brand-name: 
;;;test-me;CALL NUMBER: 
;;;test-me;CONTROL #: 
;;;test-me;CREATED/PUBLISHED: 
;;;test-me;CREATOR: 
;;;test-me;Class Descriptors: 
;;;test-me;Content-and-subjects: 
;;;test-me;Contents: 
;;;test-me;Continues: 
;;;test-me;Corporate Name: 
;;;test-me;Current Frequency: 
;;;test-me;DIGITAL ID: 
;;;test-me;Database: 
;;;test-me;Date-founded: 
;;;test-me;Description: 
;;;test-me;Descriptor: 
;;;test-me;Dewey Class No.: 
;;;test-me;Died: 
;;;test-me;Display-name: 
;;;test-me;Display-title: 
;;;test-me;Document Type: 
;;;test-me;Edition Information: 
;;;test-me;Entry: 
;;;test-me;FORMAT: 
;;;test-me;Found In: 
;;;test-me;Founded-by: 
;;;test-me;Frequency: 
;;;test-me;Full-title: 
;;;test-me;Gender: 
;;;test-me;Genre/Form: 
;;;test-me;Geographic Area Code: 
;;;test-me;HEADING: 
;;;test-me;Heading: 
;;;test-me;ID: 
;;;test-me;ISSN: 
;;;test-me;Identifier: 
;;;test-me;LC Class Number: 
;;;test-me;LC Classification: 
;;;test-me;LC Control No. 
;;;test-me;LC Control Number: 
;;;test-me;LC Copy: 
;;;test-me;LCCN Permalink: 
;;;test-me;Language: 
;;;test-me;List/Hierarchical Position: 
;;;test-me;Location-country: 
;;;test-me;Location-published: 
;;;test-me;MEDIUM: 
;;;test-me;Main Title: 
;;;test-me;Material Type: 
;;;test-me;Movie-posters: 
;;;test-me;Notes: 
;;;test-me;NOTES: 
;;;test-me;Naissance : 
;;;test-me;Named Person: 
;;;test-me;Names: 
;;;test-me;Nationalities: 
;;;test-me;Note(s): 
;;;test-me;Note: 
;;;test-me;Number-of-illustrations: 
;;;test-me;Number-of-pages: 
;;;test-me;Number-of-volumes: 
;;;test-me;Other Edition Available: 
;;;test-me;Other System No.: 
;;;test-me;Other Titles: 
;;;test-me;Other-roles: 
;;;test-me;Personal Name: 
;;;test-me;Preceding Title: 
;;;test-me;Products-associated: 
;;;test-me;Publication : 
;;;test-me;Publication: 
;;;test-me;Publication Dates: 
;;;test-me;Published-by: 
;;;test-me;Published/Created: 
;;;test-me;Publisher Location: 
;;;test-me;Publisher: 
;;;test-me;Quality Code: 
;;;test-me;REPOSITORY: 
;;;test-me;REPRODUCTION NUMBER: 
;;;test-me;RIGHTS INFORMATION: 
;;;test-me;Record Type: 
;;;test-me;Related Names: 
;;;test-me;Related People and Corporate Bodies: 
;;;test-me;Related People or Corporate Bodies: 
;;;test-me;Relevance: 
;;;test-me;Repository: 
;;;test-me;Reproduction No./Source: 
;;;test-me;Responsibility: 
;;;test-me;Roles: 
;;;test-me;SUBJECTS: 
;;;test-me;SUMMARY: 
;;;test-me;Scope Note: 
;;;test-me;Search Also Under: 
;;;test-me;Series Title: 
;;;test-me;Slogans: 
;;;test-me;Sources and Contributors: 
;;;test-me;Special Note: 
;;;test-me;Special-notes: 
;;;test-me;Standard No: 
;;;test-me;Subject: 
;;;test-me;Subjects: 
;;;test-me;Succeeding Title: 
;;;test-me;TITLE: 
;;;test-me;Title: 
;;;test-me;Type : 
;;;test-me;Type of Material: 
;;;test-me;Uniform Title: 
;;;test-me;Update: 
;;;test-me;Uploaded-by: 
;;;test-me;Used For/See From: 
;;;test-me;Used-for: 
;;;test-me;Year:
;;; ==============================

;;; ==============================
(require 'naf-mode-db-fields)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;; (provide 'naf-mode-db-fields)
;;; ==============================

;;; ================================================================
;;; naf-mode-db-fields.el ends here
;;; EOF

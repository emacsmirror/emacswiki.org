;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-nation-french.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-nation-french lists of French Nation names - ISO.
;;; 
;;; naf-mode-name -> naf-mode-place-face.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `naf-mode-nation-french'
;;;
;;; VARIABLES:
;;; `*naf-nation-french*', `*naf-mode-nation-french-xrefs*'
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
;;; This and others need have an interlingua xref'd hashtable.
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-nation-french.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T20:32:43-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED: Summer 2008
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:44:32-04:00Z}#{09327} - by MON KEY>
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
;;; ==============================
;;; CODE:

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-23T16:58:18-04:00Z}#{09393} - by MON>
(eval-and-compile
(defvar *naf-mode-nation-french-xrefs* 
  '(*naf-mode-nation-french-xrefs* 
    *naf-nation-french*
    *naf-nation-english*
    mon-help-naf-mode-faces)
  "List of symbol names of variables which xref each other in naf-mode-nation-french
package. See FILE: \"./naf-mode-nation-french.el\"."))
;;
;;;test-me; *naf-mode-nation-french-xrefs*
;;
;;;(progn (makunbound '*naf-mode-nation-french-xrefs*)
;;;       (unintern '*naf-mode-nation-french-xrefs*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-23T16:58:13-04:00Z}#{09393} - by MON>
(eval-and-compile
(defvar *naf-nation-french*
  '("Afrique du Sud"               ;; "Saint Helena" ;; co-refs->English
    "Albanie"                      ;; "Barbuda"      ;; co-refs->English
    "Algérie"                      ;; "Belarus"      ;; co-refs->English
    "Allemagne"                    ;; "Djibouti"     ;; co-refs->English
    "Andorre"                      ;; "Ecuador"      ;; co-refs->English
    "Angleterre"                   ;; "Kiribati"     ;; co-refs->English
    "Antarctique"                  ;; "Korea"        ;; co-refs->English
    "Antilles Néerlandaises"       ;; "Pitcairn"     ;; co-refs->English
    "Arabie Saoudite"              ;; "Qatar"        ;; co-refs->English
    "Arménie"                      ;; "Romania"      ;; co-refs->English
    "Autriche"                     ;; "Saint Lucia"  ;; co-refs->English
    "Azerbaïdjan"                  ;; "Uzbekistan"   ;; co-refs->English
    "Bahreïn"                      ;; "Georgia       ;; conflicts-English-State-name
    "Barbade"
    "Belgique"
    "Bermudes"
    "Bhoutan"
    "Bolivie"
    "Bosnie-Herzégovine"
    "Brunéi Darussalam"
    "Brésil"
    "Bulgarie"
    "Burkina Faso"
    "Bélarus"
    "Bénin"
    "Cambodge"
    "Cameroun"
    "Cap-Vert"
    "Chine"
    "Colombie"
    "Comores"
    "Corée"
    "Croatie"
    "Côte D'Ivoire"
    "Danemark"
    "Dominique"
    "Espagne"
    "Estonie"
    "Fidji"
    "Gambie"
    "Grenade"
    "Groenland"
    "Grèce"
    "Guadeloupe"
    "Guernesey"
    "Guinea-Bissau"
    "Guinée Équatoriale"
    "Guinée-Bissau"
    "Guyane"
    "Géorgie"
    "Haïti"
    "Hollande"
    "Hongrie"
    "Inde"
    "Indonésie"
    "Irlande"
    "Israël"
    "Italie"
    "Jamaïque"
    "Japon"
    "Jordanie"
    "Kirghizistan"
    "Koweït"
    "Lesotho"
    "Lettonie"
    "Libéria"
    "Lituanie"
    "Macao"
    "Macédoine"
    "Madagascar"
    "Malaisie"
    "Maldives"
    "Malte"
    "Maroc"
    "Mauritanie"
    "Mexique"
    "Micronésie"
    "Mongolie"
    "Monténégro"
    "Namibie"
    "Nigéria"
    "Niué"
    "Norfolk, Île"
    "Norvège"
    "Nouvelle-Calédonie"
    "Nouvelle-Zélande"
    "Népal"
    "Ouganda"
    "Ouzbékistan"
    "Palaos"
    "Papouasie-Nouvelle-Guinée"
    "Pays de Galles"
    "Pays-Bas"
    "Pologne"
    "Polynésie"
    "Pérou"
    "Roumanie"
    "Russie"
    "République Dominicaine"
    "République Tchèque"
    "Saint-Barthélemy"
    "Saint-Kitts-et-Nevis"
    "Saint-Marin"
    "Saint-Martin"
    "Saint-Pierre-et-Miquelon"
    "Saint-Siège"
    "Saint-Vincent-et-Les Grenadines"
    "Sainte-Hélène"
    "Sainte-Lucie"  
    "Sao Tomé-et-Principe"
    "Serbie"
    "Singapour"
    "Slovaquie"
    "Slovénie"
    "Somalie"
    "Suède"
    "Syrienne"
    "Sénégal"
    "Tadjikistan"
    "Tanzanie"
    "Taïwan"
    "Tchad"
    "Tchécoslovaquie"
    "Terres Australes Françaises"
    "Thaïlande"
    "Timor-Leste"
    "Trinité-et-Tobago"
    "Tunisie"
    "Turkménistan"
    "Turquie"
    "Vanuatu"
    "Việt Nam"
    "Wallis et Futuna"
    "Yougoslave"
    "Yémen"
    "Zambie"
    "ÉTATS-UNIS D'AMÉRIQUE"
    "Égypte"
    "Émirats Arabes Unis"
    "Équateur"
    "Érythrée"
    "État de la Cité du Vatican"
    "États Unis"
    "États-Unis"
    "États-Unis d'Amérique"
    "États-Unis d'Amerique"
    "États Unis d'Amérique"
    "États Unis d'Amerique"
    "Etats Unis d'Amerique"
    "Etats-Unis d'Amérique"
    "Éthiopie"
    "Île Bouvet"
    "Île Christmas"
    "Île Jan Mayen"
    "Île de Man"
    "Îles Caïmanes"
    "Îles Cook"
    "Îles Falkland"
    "Îles Féroé"
    "Îles Mariannes du Nord"
    "Îles Marshall"
    "Îles Salomon"
    "Îles Sandwich"
    "Îles Åland")
  "*Keyword list of Nation names - French Spelling - ISO and Benezit forms.
Used for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-nation-french
  (concat "\\<" (regexp-opt *naf-nation-french* 'paren) "\\>"))) ;; Consider removing trailing "\\>"
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  *naf-nation-french* naf-mode-nation-french                                    
                                    *naf-mode-nation-french-xrefs* naf-mode-place-fface)) 
;;
;; (progn (makunbound '*naf-nation-french*) (unintern '*naf-nation-french*)
;;       (makunbound 'naf-mode-nation-french) (unintern 'naf-mode-nation-french))


;;; ==============================
(provide 'naf-mode-nation-french)
;;; ==============================

;;; ================================================================
;;; naf-mode-nation-french.el ends here
;;; EOF

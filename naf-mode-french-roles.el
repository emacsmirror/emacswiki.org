;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-french-roles.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-french-roles provides font-lock keywords which indicate
;;; those French words denoting terms which indicate artistic role. 
;;; Keywords are pulled primarily from Benezit.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `naf-mode-french-roles-primary', `naf-mode-french-roles-secondary'
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
;;; TODO:
;;;
;;; NOTES:
;;; This file uses the provide/require idiom because of the defconstant forms.
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-french-roles.el)
;;; FIRST-PUBLISHED: <Timestamp: #{2009-11-21T20:03:03-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:29:33-04:00Z}#{09327} - by MON KEY>
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
(provide 'naf-mode-french-roles)
;;; ==============================

;;; ==============================
(let ((naf-french-roles-primary
     (list
      "Affichiste"
      "Aquafortiste"
      "Aquarelliste"
      "Architecte"
      "Auteur"
      "Auteur d'Assemblages"
      "Auteur Dramatique"
      "Couturier"
      "Caricaturiste"
      "Critique d'Art"
      "Créatur d'Environments"
      "Céramiste"
      "Dessinateur"
      "Dessinatrice"
      "Directrice de Théâtre"
      "Décorateur de Théâtre"
      "Décorateur de Maison"
      "Décorateur"
      "Décoratrice"
      "Ébéniste"
      "Écrivain"
      "Écrivain Dramatique"
      "Fondeur"
      "Fresquiste"
      "Graveur"
      "Graveur au Burin"
      "Graveur d'Ex-Libres"
      "Graveur d'Ornements"
      "Graveur de Reproductions"
      "Graveur de Vignettes"
      "Graveur sur Bois"
      "Graveur à l'Eau-Forte"
      "Graveur à La Manière Noire"
      "Illustrateur"
      "Illustrateur de Livres Pour Enfants"
      "Illustratrice"
      "Lithographe"
      "Miniaturiste"
      "Modeleur"
      "Mosaïste"
      "Médailleur"
      "Orfèvre"
      "Ornemaniste"
      "Pastelliste"
      "Paysagiste"
      "Peintre"
      "Peintre Animalier"
      "Peintre d'Architectures"
      "Peintre d'Assemblages"
      "Peintre de Scenes"
      "Peintre de Batailles"
      "Peintre de Cartons"
      "Peintre de Cartons de Tapisseries"
      "Peintre de Cartons de Vitraux"
      "Peintre de Collages"
      "Peintre de Compositions"
      "Peintre de Compositions Allégoriques"
      "Peintre de Compositions Animées"
      "Peintre de Compositions Murales"
      "Peintre de Compositions à Personnages"
      "Peintre de d'Histoire"
      "Peintre de Decoration Mulares"
      "Peintre de Décorations Murales"
      "Peintre de Décors"
      "Peintre de Décors de Théâtre"
      "Peintre de Figures"
      "Peintre de Fleurs"
      "Peintre de Fleurs et de Fruits"
      "Peintre de Fruits"
      "Peintre de Fruits et de Fleurs"
      "Peintre de Marines"
      "Peintre de Natures Mortes"
      "Peintre de Paysages"
      "Peintre de Paysages Animès"
      "Peintre de Paysages d'Eau"
      "Peintre de Photomontages"
      "Peintre de Portaits"
      "Peintre de Scènes de Genre"
      "Peintre de Scènes Typiques"
      "Peintre de Sujets de Sport"
      "Peintre de Sujets Fantastique"
      "Peintre de Sujets Militaires"
      "Peintre de Sujets Mythologiques"
      "Peintre sur Porcelaine"
      "Peintre Tecnhique Mixte"
      "Peintre Verrier"
      "Peintre à la Gouache"
      "Portraitiste"
      "Sculpteur"
      "Sculpteur d'Ornements"
      "Sculpteur de Bustes"
      "Sculpteur de Compositions Religeuses"
      "Sculpteur de d'Animaux"
      "Sculpteur de Figures"
      "Sculpteur de Monuments"
      "Sculpteur de Paysages Urbains"
      "Sculpteur sur Bois"
      "Sérigraphe"
      "Tapissier"      )))
;;
(defconst naf-mode-french-roles-primary
  (concat "\\(" (regexp-opt naf-french-roles-primary  'paren) "\\(,?\\)\\)")
  "Provides keywords for French words that signify artistic roles. 
Keywords are pulled primarily from Benezit. Terms usually occur in the 'Roles:'
field of one or more of NAF file types. Used for font-lock in `naf-mode'.\n
See also; `naf-mode-french-roles-secondary'."))

;;; ==============================
(let ((naf-french-roles-secondary
     (list
      "affichiste"
      "aquafortiste"
      "aquarelliste"
      "architecte"
      "auteur"
      "auteur d'assemblages"
      "auteur dramatique"
      "caricaturiste"
      "couturier"
      "ciseleur-doreurs"
      "collaboratuer de journaux illustrés"
      "critique d'art"
      "créatur d'environments"
      "céramiste"
      "dessinateur"
      "dessinatrice"
      "directrice de théâtre"
      "décorateur"
      "décoratrice"
      "ébéniste"
      "écrivain"
      "écrivain dramatique"
      "fabriques"
      "fondeur"
      "fresquiste"
      "graveur"
      "graveur à la manière noire"
      "graveur au burin"
      "graveur d'ex-libres"
      "graveur d'ornements"
      "graveur de reproductions"
      "graveur de vignettes"
      "graveur sur bois"
      "graveur à l'eau-forte"
      "humoriste"
      "illustrateur"
      "illustratrice"
      "illustrateur de livres pour enfants"
      "lithographe"
      "marbreurs"
      "miniaturiste"
      "modeleur"
      "mosaïste"
      "médailleur"
      "orfèvre"
      "ornemaniste"
      "pastelliste"
      "paysagiste"
      "peintre"
      "peintre animalier"
      "peintre d'architectures"
      "peintre d'assemblages"
      "peintre de Scenes"
      "peintre de batailles"
      "peintre de cartons"
      "peintre de cartons de tapisseries"
      "peintre de cartons de vitraux"
      "peintre de collages"
      "peintre de compositions"
      "peintre de compositions allégoriques"
      "peintre de compositions animées"
      "peintre de compositions murales"
      "peintre de compositions à personnages"
      "peintre de d'histoire"
      "peintre de decoration mulares"
      "peintre de décorations murales"
      "peintre de décors"
      "peintre de décors de théâtre"
      "peintre de figures"
      "peintre de fleurs"
      "peintre de fleurs et de fruits"
      "peintre de fruits"
      "peintre de fruits et de fleurs"
      "peintre de marines"
      "peintre de natures mortes"
      "peintre de paysages"
      "peintre de paysages animès"
      "peintre de paysages d'eau"
      "peintre de photomontages"
      "peintre de portaits"
      "peintre de scènes de genre"
      "peintre de scènes typiques"
      "peintre de sujets de sport"
      "peintre de sujets fantastique"
      "peintre de sujets militaires"
      "peintre de sujets mythologiques"
      "peintre sur porcelaine"
      "peintre tecnhique mixte"
      "peintre verrier"
      "peintre à la gouache"
      "portraitiste"
      "sculpteur"
      "sculpteur d'ornements"
      "sculpteur de bustes"
      "sculpteur de compositions religeuses"
      "sculpteur de d'animaux"
      "sculpteur de figures"
      "sculpteur de monuments"
      "sculpteur de paysages urbains"
      "sculpteur sur bois"
      "sérigraphe"
      "tapissiers"
      "tapissier")))
;;
(defconst naf-mode-french-roles-secondary
    (concat "\\<" (regexp-opt naf-french-roles-secondary 'paren) "\\>")
    "Keywords for font-locking of French works signifying artistic roles. 
Keywords occur primarily in the Benezit text sections. Used to aid in term
identification in `naf-mode'.\n
See also; `naf-mode-french-roles-primary'."))

;;; ==============================
(require 'naf-mode-french-roles)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;; (provide 'naf-mode-french-roles)
;;; ==============================

;;; ================================================================
;;; naf-mode-french-roles.el ends here
;;; EOF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-publications-periodicals-french.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-publications-periodicals-french provides keyword 
;;; fontlocking of French periodical titles for `naf-mode'.
;;;
;;; FUNCTIONS:
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `naf-mode-publications-periodicals-french'
;;;
;;; VARIABLES:
;;; `*naf-mode-publications-periodicals-french-xrefs*'
;;; `*naf-publications-periodicals-french*'
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
;;; PUBLIC-LINK:
;;; (URL `http://www.emacswiki.org/emacs/naf-mode-publications-periodicals-french.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T20:20:39-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-22T15:53:32-04:00Z}#{09346} - by MON KEY>
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
;;; CREATED: <Timestamp: #{2009-10-02T18:25:52-04:00Z}#{09405} - by MON>
(eval-and-compile
(defvar *naf-mode-publications-periodicals-french-xrefs*
  '(*naf-publications-periodicals-french*
    *naf-publications-periodicals-english*
    *naf-publications-periodicals-english-one-word*
    *naf-naf-publications-periodicals-intnl*
    *naf-mode-publications-periodicals-french-xrefs*
    mon-help-naf-mode-faces)
  "*List of symbol names of variables which xref each other in the
`naf-mode-publications-periodicals-french' package.
See FILE: \"./naf-mode-publications-periodicals-french.el\"."))
;;
;;;test-me; *naf-mode-publications-periodicals-french-xrefs*
;;
;;;(progn (makunbound '*naf-mode-publications-periodicals-french-xrefs*)
;;;       (unintern '*naf-mode-publications-periodicals-french-xrefs*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-02T18:26:35-04:00Z}#{09405} - by MON>
(eval-and-compile
(defvar *naf-publications-periodicals-french*
  '("Adam - La Revue de l'homme"
    "Art Gout Beaute"
    "Art, Goût, Beauté"
    "Art, gout, beaute"
    "Art, goût, beauté"
    "Arts et Metiers Graphiques"
    "Arts et Métiers Graphiques"
    "l'Assiette au Beurre"
    "Assiette au Beurre"
    "Au Quartier Latin"
    "Cahiers d'Art"
    "Cahiers du Jardin des modes"
    "Canard Sauvage"
    "Canard sauvage"
    "Chambard Socialiste"
    "Clarté"
    "Cocorico"
    "Comoedia Illustre"
    "Comoedia Illustré"
    "Comoedia illustré"
    "Costumes et manteaux"
    "Courier Francais"
    "Courier Français"
    "Crapouillot"
    "Cri de Paris"
    "DLM"
    "Derrière Le Miroir"
    "Derrière le Miroir"
    "derrière le miroir"
    "Derriere Le Miroir"
    "Derriere le Miroir"
    "derriere le miroir"
    "Écho de Paris"
    "Echo de Paris"
    "Escarmouche"
    "Fantasio"
    "Femina"
    "Le Figaro"
    "Figaro Illustre"
    "Figaro Illustré"
    "France-Illustration"
    "Frou Frou"
    "Frou-Frou"
    "Gazette du Bon Genre"
    "Gazette du Bon Ton"
    "Gazette du bon ton"
    "Gil Blas"
    "Gil Blas Illustré"
    "Gil Blas Illustre"
    "Hommes Magazine"
    "Jardin des modes"
    "Journal des dames et des modes"
    "L'Art et la mode"
    "L'Illsutration"
    "l'Illsutration"
    "L'Image"
    "L'illustration"
    "l'Écho de Paris"
    "L'Écho de Paris"
    "L'Echo de Paris"
    "L'Estampe originale"
    "l'Estampe originale"
    "La Baionnette"
    "La Baïonnette"
    "La Caricature"
    "La Femme élégante à Paris"
    "La France"
    "La Guirlande"
    "La Plume"
    "La Vie Artistique"
    "La Vie Au Grand Air"
    "La Revue Blanche" 
    "La Revue blanche"
    "La Vie Élégante"
    "La mode pratique"
    "Le Canard sauvage"
    "Le Centaure" 
    "Le Charivari"
    "Le Chat Noir"
    "Le Mot"
    "Le Petit Journal"
    "Le Petit journal"
    "Le Petit Français Illustré"
    "Le Pêle-mêle"
    "Le Rire"
    "Le Rire Rouge" 
    "Le Soleil du Dimanche"
    "Le Sourire"
    "Le Temoin"
    "Le Theatre"
    "Le Théâtre"
    "Le Tour du monde"
    "Le chat noir"
    "Lectures Pour Tous"
    "Les Feuillets d'Art"
    "Les Idées Nouvelles da La Mode"
    "Les albums du Jardin des modes"
    "Les modes parisiennes illustrées"
    "Méditerranée Revue Illustrée"
    "Mediterranee Revue Illustree"
    "Minotaure" 
    "Modes et Travaux"
    "Officiel de la couture et de la mode"
    "Paris Toujours"
    "Paris-Caprice"
    "Paris Comique"
    "Paris chic"
    "Plaisir de France"
    "Programmes Illustres"
    "Programmes Illustrés"
    "Revue Blanche"
    "Revue blanche"
    "Revue Cerpa" 
    "Robes élégantes"
    "Soleil du Dimanche Illustré"
    "Vie Au Grand Air"
    "Vie En Rose"
    "Vie Parisienne"
    "Vie au grand air"
    "Voici la mode"
    "Voici la mode; art, goût, beauté"
    "VU"
    "Vu"
    "Zhar-Ptitsa" 
    "l'Escarmouche"
    "l'Officiel")
  "*Keyword list of French Periodical titles for `naf-mode' font-locking."))
;;
(eval-and-compile
(defconst naf-mode-publications-periodicals-french
  (concat "\\<" (regexp-opt *naf-publications-periodicals-french* 'paren) "\\>")))
;;
(eval-and-compile
  (mon-help-swap-var-doc-const-val
      *naf-publications-periodicals-french* naf-mode-publications-periodicals-french
      *naf-mode-publications-periodicals-french-xrefs* naf-mode-publication-periodical-fface))
;;
;;(progn (makunbound '*naf-publications-periodicals-french*) 
;;       (unintern '*naf-publications-periodicals-french*)
;;       (makunbound 'naf-mode-publications-periodicals-french) 
;;       (unintern 'naf-mode-publications-periodicals-french))

;;; ==============================
(provide 'naf-mode-publications-periodicals-french)
;;; ==============================

;;; ================================================================
;;; naf-mode-publications-periodicals-french.el ends here
;;; EOF

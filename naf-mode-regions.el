;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-regions.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-regions
;;; French region-names and combined list of other region names (non 
;;; French Language). For use with place name flagging with `naf-mode' 
;;; font-locking.
;;;
;;; Both constants locked with `naf-mode-place-face', `naf-mode-place-fface'.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `naf-mode-region-names-other'
;;; `naf-mode-region-names-french'
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
;;; Multiple lists at bottom of file remain to be bound and assigned face/props.
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-regions.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:22:33-05:00Z}#{09477} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T17:11:35-04:00Z}#{09327} - by MON KEY>
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
(provide 'naf-mode-regions)
;;; ==============================

;;; ==============================
(let ((naf-region-names-non-french
      (list
       ;; TYPES OF PLACES:
       "County"
       "county of"
       "Country of"
       "countryside"
       "City of"
       "city of"
       "Town of"
       "town of"
       "State of"
       "province of"
       "province of"
       "region"
       ;; GEOGRAPHIC FEATURE DESCRIPTORS:
       "Gulf of"
       "Bay of"
       "Peninsula"
       "Ocean"
       "Lake"
       "River"
       "Island"
       "Isle"
       "Mountains"
       ;; CARDINAL DIRECTIONS:
       "East"
       "Eastern"
       "North"
       "Northeast"
       "Northeastern"
       "Northern"
       "Northwest"
       "Northwestern"
       "South"
       "Southeast"
       "Southeastern"
       "Southern"
       "Southewestern"
       "Southwest"
       "West"
       "Western"
       "east"
       "eastern"
       "north"
       "northeast"
       "northeastern"
       "northern"
       "northwest"
       "northwestern"
       "Midwest"
       "midwest"
       "south"
       "southeast"
       "southeastern"
       "southern"
       "southwestern"
       "southwest"
       "west"
       "western"
       "Nord-Ouest"
       "Sud-Ouest"
       "Sud-Est"
       "Nord-Est"
       "Ouest"
       "Nord"
       "Sud"
       ;; REGION NAMES:
       "Orient"
       "Oriental"
       "Atlantic"
       "Pacific"
       "Bavaria"
       "Africa"
       "Asia"
       "Arctic"
       "Europe"
       "Persia"
       ;;
       "Appalachia"
       "Cumberland"
       "New England" "La Nouvelle Angleterre"
       "Rocky Mountains" "Les Montagnes Rocheuses"
       "Appalachian Mountains"
       "Hudson River"
       "Potomac River"
       "Great Lakes"
       "Erie Canal"
       "Niagara Falls"
       "Lake Erie"
       "Lake Superior"
       "Lake Huron"
       ;; "Lake Ontario"
       ;;
       "Mediterranean"
       "Nordic"
       "Baltic"
       "Bohemia"
       "Caucasus"
       "Crimea"
       "Siberia"
       "Urals"
       "Transylvania" "Transilvania" "Transylvanie" "Siebenbürgen"
       "Andalusia" "Andalucia" "Andalousie"
       "Castilla" "Castille"
       "Catalonia" "Catalunya" "Cataluña"
      )))
;;
(defconst naf-mode-region-names-other
  (concat (regexp-opt naf-region-names-non-french 'paren) "\\>")
  "Combined list of other non French region names.
Used for place name flagging with `naf-mode' font-locking."))

;;; ==============================
(let  ((naf-region-names-french
        (list
         "Alsace"
         "Alsace-Lorraine"
         "Bas-Rhin"
         "Haut-Rhin"
         ;; 
         "Aquitaine"
         "Dordogne"
         ;; Gironde
         ;; Landes
         "Lot-et-Garonne"
         "Pyrénées-Atlantiques"
         ;; 
         "Auvergne"
         ;; Allier
         ;; Cantal
         "Haute-Loire"
         "Puy-de-Dôme"
         "Basse-Normandie"
         ;; (Lower Normandy)
         ;; Calvados
         ;; Manche
         ;; Orne
         ;;
         ;; Bourgogne (Burgundy)
         "Burgundy" "Bourgogne"
         "Côte-d'Or"
         ;; Nièvre
         "Saône-et-Loire"
         ;; Yonne
         ;;
         ;; Bretagne (Brittany)
         "Côtes-d'Armor"
         "Finistère"
         "Ille-et-Vilaine"
         ;; Morbihan
         ;;
         ;; Centre
         ;; Cher
         "Eure-et-Loir"
         ;; Indre
         "Indre-et-Loire"
         "Loir-et-Cher"
         ;; Loiret
         ;;
         "Champagne-Ardenne"
         "Ardennes"
         ;; Aube
         ;; Marne
         "Haute-Marne"
         ;;
         ;; Corse (Corsica)
         "Corse-du-Sud"
         "Haute-Corse"
         ;;
         "Franche-Comté"
         ;; Doubs
         ;; Jura
         "Haute-Saône"
         "Territoire de Belfort"
         ;;
        "Haute-Normandie"
         ;; (Upper Normandy)
         ;; Eure
         "Seine-Maritime"
         ;;
         "Île-de-France"
         ;; Paris
         "Seine-et-Marne"
         ;; Yvelines
         ;; Essonne
         "Hauts-de-Seine"
         "Seine-Saint-Denis"
         "Val-de-Marne"
         "Val-d'Oise"
         ;;
         "Languedoc-Roussillon"
         ;; Aude
         ;; Gard
         ;; Hérault
         ;; Lozère
         "Pyrénées-Orientales"
         ;;
         ;; Limousin
         ;; Corrèze
         ;; Creuse
         "Haute-Vienne"
         ;;
         ;; Lorraine
         "Meurthe-et-Moselle"
         ;; Meuse
         ;; Moselle
         ;; Vosges
         ;;
         "Midi-Pyrénées"
         ;; Ariège
         ;; Aveyron
         "Haute-Garonne"
         ;; Gers
         ;; Lot
         "Hautes-Pyrénées"
         ;; Tarn
         "Tarn-et-Garonne"
         ;;
         "Nord-Pas de Calais"
         ;; Nord
         "Pas-de-Calais"
         ;;
         "Pays-de-la-Loire"
         ;; (Loire Country)
         "Loire-Atlantique"
         "Maine-et-Loire"
         ;; Mayenne
         ;; Sarthe
         ;; Vendée
         ;;
         "Picardy" "Picardie"
         ;; Picardie (Picardy)
         ;; Aisne
         ;; Oise
         ;; Somme
         ;;
         "Poitou-Charentes"
         ;; Charente
         "Charente-Maritime"
         "Deux-Sèvres"
         ;; Vienne
         ;;
         "Provence-Alpes-Côte d'Azur"
         "Alpes-de-Haute-Provence"
         "Hautes-Alpes"
         "Alpes-Maritimes"
         "Bouches-du-Rhône"
         ;; Vaucluse
         ;;
         "Rhône-Alpes"
         ;; Ardèche
         ;; Drôme
         ;; Isère
         ;; Loire
         ;; Rhône
         "Savoy" "Savoie"
         "Haute-Savoie"
         )))
;; 
(defconst naf-mode-region-names-french
  (concat (regexp-opt naf-region-names-french 'paren))
  "List of French region\(al\) names. 
Use for place name flagging of `naf-mode' font-locking."))

;;;; ==============================
;;; Ain | Aisne | Allier | Alpes-de-Haute-Provence | Hautes-Alpes | Alpes-Maritimes
;;; | Ardèche | Ardennes | Ariège | Aube | Aude | Aveyron | Bouches-du-Rhône |
;;; Calvados | Cantal | Charente | Charente-Maritime | Cher | Corrèze | Corse-du-Sud
;;; | Haute-Corse | Côte-d'Or | Côtes-d'Armor | Creuse | Dordogne | Doubs | Drôme |
;;; Eure | Eure-et-Loir | Finistère | Gard | Haute-Garonne | Gers | Gironde |
;;; Hérault | Ille-et-Vilaine | Indre | Indre-et-Loire | Isère | Jura | Landes |
;;; Loir-et-Cher | Loire | Haute-Loire | Loire-Atlantique | Loiret | Lot |
;;; Lot-et-Garonne | Lozère | Maine-et-Loire | Manche | Marne | Haute-Marne |
;;; Mayenne | Meurthe-et-Moselle | Meuse | Morbihan | Moselle | Nièvre | Nord | Oise
;;; | Orne | Pas-de-Calais | Puy-de-Dôme | Pyrénées-Atlantiques | Hautes-Pyrénées |
;;; Pyrénées-Orientales | Bas-Rhin | Haut-Rhin | Rhône | Haute-Saône |
;;; Saône-et-Loire | Sarthe | Savoie | Haute-Savoie | Paris | Seine-Maritime |
;;; Seine-et-Marne | Yvelines | Deux-Sèvres | Somme | Tarn | Tarn-et-Garonne | Var |
;;; Vaucluse | Vendée | Vienne | Haute-Vienne | Vosges | Yonne | Territoire de
;;; Belfort | Essonne | Hauts-de-Seine | Seine-Saint-Denis | Val-de-Marne |
;;; Val-d'Oise
;;; ==============================
;;; SPANISH REGIONS:
;;; (Andalucia . Andalousie)
;;; (Asturias . Asturies)
;;; (Cantabria . Cantabrie)
;;; (Castilla . Castille)
;;; (Catalunya . Catalogne)
;;; (Extremadura . Estrémadure)
;;; (Galicia . Galice)
;;; (Murcia . Murcie)
;;; (Navarra . Navarre)
;;; ==============================
;;; Romanian Regions
;;; (Moldova .  Moldavie)
;;; ==============================
;;; Portugal Region
;;; (Madeira . Madère)
;;; ==============================
;;; German Regions
;;; Baden Bade
;;; Hessen Hesse
;;; Pfalz Palatinat
;;; Bayern "Bavière"
;;; Brandenburg Brandebourg
;;; Mecklenburg Meklembourg
;;; Pomerania Poméranie
;;; Rheinland Rhénanie
;;; Saarland Sarre
;;; Saxony Saxe
;;; Schlesien Silésie
;;; Schwaben Souabe
;;; Thüringen Thuringe
;;; Westfalen Westphalie
;;; ==============================

;;; ==============================
(require 'naf-mode-regions)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;; (provide 'naf-mode-regions)
;;; ==============================

;;; ================================================================
;;; naf-mode-regions.el ends here
;;; EOF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-intnl-city-names.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-intnl-city-names
;;; Keyword place names of international citys in English, French
;;; and occassionaly German. Used with `naf-mode' for font-locks.
;;; naf-mode-name -> naf-mode-place-face.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS or VARIABLES:
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
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-intnl-city-names.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:32:00-05:00Z}#{09477} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-09T12:37:09-04:00Z}#{09327} - by MON KEY>
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
(provide 'naf-mode-intnl-city-names)
;;; ==============================

;;; ==============================
;;; French and international alternate Spellings.
;;; ==============================
(let ((naf-intnl-auction-city-names
       (list
	"AMSTERDAM"
	"BERLIN"
	"BERNE"
	"BORDEAUX"
	"BOSTON"
	"BRUXELLES"
	"BÂLE"
	"COPENHAGUE"
	"DUBLIN"
	"FRIBOURG"
	"GENÈVE"
	"LOKEREN"
	"LONDRES"
	"LUCERNE"
	"MILAN"
	"MONACO"
	"MONTE-CARLO"
	"MUNICH"
	"NEUCHÂTEL"
	"NEUILLY"
	"NEW YORK"
	"PARIS"
	"ROME"
	"STOCKHOLM"
	"VERSAILLES"
	"VIENNE"
	"ZURICH"
	)))
;;
(defconst naf-mode-intnl-auction-city-names
  (concat "\\<" (regexp-opt naf-intnl-auction-city-names 'paren) "\\>")
  "City names highlighting for Benezit auctions.\n
Used with `naf-mode' for fontlocking."))

;;; ==============================
(let ((naf-intnl-city-names
       (list
	"Aix-la-Chapelle" "Aachen"
	"Bay of Biscay" "Golfe de Gascogne"
	"Brittany" "Bretagne" 
	"Cape Town" 
	"Cartagena" "Carthagène"
	"Cesena" "Césène"
	"Coburg" "Cobourg"
	"Corssica" "Corse"
	"Danzig" "Gdańsk"
	"Darmstadt"
	"Gascony" "Gascogne"
	"Gempenach" "Champagny"
	"Guangzhou" "Canton"
	"Göttingen" "Goettingue"
	"Lombardy" "Lombardia"
	"Mallorca" "Majorque"
	"Mannheim"
	"Modena" "Modène"
	"Nanjing" "Nankin"
	"Normandy" "Normandie"
	"Oviedo" "Oviede"
	"Rheims" "Reims"
	"Salamanca" "Salamanque"
	"Sardinia" "Sardegna"
	"Schönberg" "Schoenberg"
	"The Riviera" "Côte d'Azure" "the Riviera"
	"Tilburg" "Tilbourg"
	"Toledo" "Tolède"
	"Tongeren" "Tongres"
	"Tyrol" "Tirolo"
	"Ulm"
	"Vlissingen" "Flessingue"
	"Wittenberg" "Wittemberg"
	"Zaragoza" "Saragosse"
	"Zeeland" "Zélande"
        "Aix-en-Provence"
        "Aix-les-Bains"
        "Amsterdam"
        "Antwerp" "Anvers"
        "Assisi" "Assise"
        "Athens" "Athènes"
        "Augsburg" "Augsbourg"
        "Avignon"
        "Barcelona" "Barcelone"
        "Basel" "Bâle" "Basle"
	"Bayonne" "Baiona"
        "Beijing" "Pékin" "Peking"
        "Belfast"
        "Belgrade"
        "Bergen"
        "Berlin" "Berlín"
        "Berne"
        "Bilbao"
        "Blenheim"
        "Bois-le-Duc" "Hezogenbusch" "'s-Hertogenbosch"
        "Bologna" "Bologne"
        "Bremen" "Brême"
        "Briston"
        "Brodeaux"
        "Bruges"
        "Brunswick" "Braunschweig"
        "Brussels" "Bruxelles"
        "Bucharest" "Bucarest" "Bucureşti"
        "Budapest"
        "Buenos Aires"
        "Cairo"
        "Calais"
        "Cambridge"
        "Canterbury" "Cantorbèry"
        "Carcassonne"
        "Christiania" "Kristiania"
        "Clermont-Ferrand"
        "Coblenz" "Coblence" "Koblenz"
        "Colchester"
        "Cologne" "Koln" "Cöln" "Köln"
        "Constantinople" "Istanbul" "Stamboul" "İstambul"
        "Copenhagen" "Copenhague" "København"
        "Corfu" "Corfou" "Corcyre"
        "Corinth" "Corinthe"
        "Crécy-en-Ponthieu"
        "Cádiz" "Cadix"
        "Córdoba" "Cordoue"
        "Dijon"
        "Dover" "Douvres"
        "Dresden" "Dresde"
        "Dublin"
        "Dunkirk" "Dunkerque"
        "Dusseldorf" "Düsseldorf"
        "Edinburgh" "Édimbourg"
        "Eisenstadt"
        "Elsinore" "Elseneur"
        "Erlangen"
        "Exeter"
        "Florence" "Firenze"
        "Frankfurt" "Frankfurt am Main" "Francfort-sur-le-Main" "Francfort-sur-l'Oder"
        "Freiburg" "Fribourg-en-Brigovie" "Fribourg"
        "Freising" "Frisingue"
        "Gallipoli"
        "Geneva" "Genève"
        "Genoa" "Gênes"
        "Ghent"
        "Girona" "Gérone"
        "Glasgow"
        "Glastonbury"
        "Gmund" "Gmünd"
        "Gothenburg" "Götenborg" "Göteborg"
        "Goucester" "Glocester"
        "Granada"
        "Grenoble"
        "Groningen" "Groningue"
        "Görlitz"
        "Hamburg" "Hambourg"
        "Hanover" "Hanovre"
        "Hasselt"
        "Helsinki"
        "Hong Kong" "Pinyin" 
        "Jena" "Iéna"
        "Kaliningrad" "Königsberg"
        "Kiev" "Kyiv"
        "Killarney"
        "Kraków" "Cracovie" "Krakow" "Cracow"
        "Kuala Lumpur"
        "Kyoto"
        "Laussanne"
        "Leicester"
        "Leiden" "Leyden" "Leyde"
        "Leipzig"
        "Leuven" "Louvain"
        "Lille"
        "Limoges"
        "Lisbon" "Lisbonne" "Lisboa"
        "Liverpool"
        "Livorno" "Livourne"
        "Liège"
        "Lleida" "Léida"
        "London" "Londres"
        "Lourdes"
        "Lucerne" "Luzern"
        "Lund"
        "Luxembourg" "Luxemburg"
        "Lüneburg" "Lunebourg"
        "Maastricht"
        "Madrid"
        "Mainz" "Mayence"
        "Manchester"
        "Marseiile" "Marseilles"
        "Melbourne"
        "Metz"
        "Mexico City"
        "Milan" "Milano"
        "Minsk"
        "Monaco"
        "Montbéliard"
        "Montreal" "Montréal"
        "Moscow" "Moscou" "Moskva" "Москва"
        "Munich" "Munchen" "Múnich"
        "Munster" "Münster"
        "Naples" "Napoli"
        "Narbonne"
        "Neuchâtel"
        "Newcastle upon Tyne"
        "Newport"
        "Nivelles" "Nivilles"
        "Nuremberg" "Nürnberg"
        "Odessa"
        "Osaka"
        "Oslo"
        "Ostend" "Ostende" "Oostende"
        "Ottawa"
        "Oxford"
        "Oświęcim" "Auschwitz"
        "Padua" "Padoue" "Padova"
        "Palerno" "Palerme"
        "Pamplona" "Pampelune"
        "Paris"
        "Parma" "Parme"
        "Pilsen"
        "Pompeii" "Pompéi"
        "Potsdam"
        "Prague" "Praha"
        "Regensburg" "Ratisbonne" "Ratisbon"
        "Rennes"
        "Reykjavík"
        "Rio de Janeiro"
        "Rome" "Roma" 
        "Rouen"
        "Saarbrücken" "Sarrebruck"
        "Saarlouis" "Sarrelouis"
        ;;        
        "Saint Petersburg" "St. Petersburg" "St Petersburg" "Saint Pétersbourg" 
        "Saint-Pétersbourg" "Petrograd" "Sankt-Peterburg" "Leningrad" "Санкт-Петербург"
        ;;
        "Saint-Quentin" "Saint Quentin"
        "Salzburg" "Salzbourg"
        "San Sebastián" "Saint-Sébastien" "Saint Sébastien"
        "Santiago de Compostela" "Saint-Jacques-de-Compostelle"
        "Saragossa" "Saragosse"
        "Sarajevo"
        "Sarrebourg" "Saarburg"
        "Sarreguemines" "Saargemünd"
        "Sartena" "Sartene"
        "Saverne"
        "Schaffhausen" "Schaffhouse"
        "Schweinfurt"
        "Schwyz" "Schwytz"
        "Seoul" "Seóul"
        "Sevastopol" "Sebastopol"
        "Seville" "Sevilla" "Séville"
        "Shrewsbury"
        "Siena" "Sienne"
        "Smolensk"
        "Solothurn" "Soleure"
        "Sonderborg" "Sonderburg"
        "St. Albans" "St Albans"
        "St. Gallen" "St Gallen" "Saint-Gall"
        "St. Moritz" "St Moritz"
        "Stockholm"
        "Strasbourg"
        "Stuttgart"
        "Syracuse"
        "Sáo Paulo"
        "Sélestat" "Schlettstadt"
        "Tarragona"
        "Tbilisi" "Tbilissi"
        "The Hague" "Den Haag" "der Haag" "La Haye"
        "Thionvile" "Diedenhofen"
        "Tokyo"
        "Tongeren" "Tongern" "Tongres"
        "Toronto"
        "Toulon"
        "Trieste"
        "Trondheim" "Drontheim"
        "Turckheim"
        "Turin" "Torino"
        "Tuscany" "Toscana"
        "Tübingen" "Tubingue"
        "Uppsala"
        "Utrecht"
        "Valencia" "Valence"
        "Valkenburg" "Fauquemont"
        "Valletta" "La Valett"
        "Vatican City" "Cité du Vatican" "Civitas Vaticana"
        "Venice" "Venedig" "Venise"
        "Ventimiglia" "Vintimille"
        "Verdun"
        "Verona" "Vérone"
        "Versailles"
        "Veurne" "Furnes"
        "Vicenza" "Wiesenthein"
        "Vienna" "Vienne" "Wien"
        "Viljandi" "Felloin"
        "Villach"
        "Vilnius"
        "Visé"
        "Vitsyebsk" "Vitebsk" "Witebsk"
        "Vlorë" "Valona"
        "Volgograd" "Stalingrad" "Tsartisyn" "Walgograd"
        "Warsaw" "Varsovie" "Warszawa"
        "Waterford"
        "Wavre"
        "Weimar"
        "Westminster"
        "Winchester"
        "Wissembourg"
        "Würzburg" "Wurzbourg"
        "Yalta"
        "Ypres"
        "Zagreb"
        "Zurich" "Zürich" 
        "Zweibrücken" "Deux-Ponts"
        )))
;;
(defconst naf-mode-intnl-city-names
  (concat "\\<" (regexp-opt naf-intnl-city-names 'paren) "\\>")
  "Keyword place names of international citys in English, French
and occassionaly German when using naf-mode font-locks."  ))

;;; ==============================
(require 'naf-mode-intnl-city-names)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;; (provide 'naf-mode-intnl-city-names)
;;; ==============================

;;; ================================================================
;;; naf-mode-intnl-city-names.el ends here
;;; EOF

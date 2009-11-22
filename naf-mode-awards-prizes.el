;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-awards-prizes.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-awards-prizes provides keyword highlighting of historic Awards and
;;; Prize names in the arts.
;;;
;;; FUNCTIONS:
;;;
;;; CONSTANTS:
;;; `naf-mode-awards-prizes-names'
;;;
;;; VARIABLES:
;;; `*naf-mode-awards-prizes-xrefs*'
;;; `*naf-awards-prizes-names*'
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-awards-prizes.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:59:51-05:00Z}#{09477} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Wednesday April 08, 2009 @ 11:33.29 AM - by MON KEY>
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
;;; CREATED: <Timestamp: #{2009-09-14T18:49:27-04:00Z}#{09381} - by MON>
(eval-and-compile
(defvar *naf-mode-awards-prizes-xrefs*
  '(*naf-awards-prizes-names* mon-help-naf-mode-faces)
  "List of symbol names of variables which xref each other in naf-mode-awards-prizes
package. See FILE: \"./naf-mode-awards-prizes.el\"."))
;;
;;;test-me; *naf-mode-awards-prizes-xrefs*
;;
;;;(progn (makunbound '*naf-mode-awards-prizes-xrefs*) (unintern '*naf-mode-awards-prizes-xrefs*))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-14T18:23:37-04:00Z}#{09381} - by MON>
(eval-and-compile
(defvar *naf-awards-prizes-names*
  '("Chevalier de la Légion d'Honneur"
    "Design Centre Awards Scheme"
    "Grand Prix de Rome"
    "Prix de Rome"
    "chevalier de la Légion d'Honneur"
    "médaille d'argent"
    "médaille d'honneur"
    "médaille d'or"
    "médaille de deuxième classe"
    "médaille de troisième classe"
    "première médaille"
    "une médaille de deuxième classe"
    )
  "*Keyword list of historic Awards and Prize names in the arts.
Used for font-locking in `naf-mode'."))
;;
(eval-and-compile 
(defconst naf-mode-awards-prizes-names
  (concat "\\<" (regexp-opt *naf-awards-prizes-names* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-awards-prizes-names* naf-mode-awards-prizes-names 
				   *naf-mode-awards-prizes-xrefs* naf-mode-awards-prizes-fface))
;;
;; (progn (makunbound '*naf-awards-prizes-names*) (unintern '*naf-awards-prizes-names*)
;;       (makunbound 'naf-mode-awards-prizes-names) (unintern 'naf-mode-awards-prizes-names))


;;; ==============================
;;; Painters recieveing the Prix de Rome 1800-1935:
;;; NOTE: Unverified 
;;; SOURCE: (URL `http://fr.wikipedia.org/wiki/Prix_de_Rome')
;;; accessed: #{2009-09-17T20:16:39-04:00Z}#{09385} - MON
;;; ==============================
;;; nth1 1YEAR nth2 ARTIST-and-born-died  nth3 Notes/trained-under/prize-count/title-or-subject-of-paingitn
;; (defvar *naf-mode-awards-prizes-prix-de-rome*
;;   '(("1800" "Jean-Pierre Granger" "Antiochus renvoie son fils à Scipion")
;;     ("1801" "Jean Auguste Dominique Ingres" "Les Ambassadeurs d'Agamemnon devant Achille")
;;     ("1802" "Alexandre Menjaud"  "Éponine et Sabinus découverts dans une grotte")
;;     ("1803" "Merry-Joseph Blondel (1781-1853)" "Énée emportant son père Anchise Georges Rouget, second grand prix")
;;     ("1804" "Joseph-Denis Odevaere")
;;     ("1805" "Félix Boisselier (1776-1811)" "La Mort de Démosthène")
;;     ("1806" "Félix Boisselier" "Retour de l'enfant Prodigue")
;;     ("1807" "François Joseph Heim (1787-1865)" "Thésée vainqueur du minotaure")
;;     ("1808" "Alexandre-Charles Guillemot" "Erasistrate découvre la cause de la maladie d'Antiochus")
;;     ("1809" "Jérôme-Martin Langlois (1779-1836)" "Priam redemande à Achille le corps de Son fils")
;;     ("1810" "Michel-Martin Drölling (1786-1851)" "La Colère d'Achille")
;;     ("1811" "Alexandre-Denis-Joseph Abel de Pujol" "Lyourgue présente aux Lacédémoniens l'héritier du trône")
;;     ("1812" "Louis-Vincent-Léon Pallière (1787-1820)" "Ulysse et Télémaque massacrent les poursuivants de Pénélope")
;;     ("1813" "François-Edouard Picot")
;;     ("1813" "Henri-Joseph de Forestier (né en 1790 à Saint-Domingue élève de Vincent et David)" "La Mort de Jacob")
;;     ("1814" "Jean-Baptiste Vinchon" "Diagoras porté en triomphe par ses fils")
;;     ("1815" "Jean Alaux dit le Romain (1787-1864)" "Briséis rendue a Achille trouve dans sa tente le corps de Patrocle")
;;     ("1816" "Antoine-Jean-Baptiste Thomas (1791-1833)" "OEnone refuse de secourir Paris au le siège de Troie")
;;     ("1817" "Léon Cogniet (1794-1880)" "Hélène délivrée par Castor et Pollux")
;;     ("1817" "Achille Etna Michallon (1796-1822)" "(Paysage historique nouvelle catégorie attribuée pour la première fois)")
;;     ("1818" "Nicolas-Auguste Hesse" "Philémon et Baucis reçoivent Jupiter et Mercure")
;;     ("1819" "François Dubois" "Thémistocle se réfugie chez Admète Roi des Molosses")
;;     ("1819" "Charles-Philippe Larivière" "second Prix de Rome")
;;     ("1820" "Amable-Paul Coutan")
;;     ("1821" "Joseph-Désiré Court" "Samson et Dalila")
;;     ("1821" "Jean-Charles-Joseph Rémond (1795-1875)" "(Paysage historique) L'Enlèvement de Proserpine")
;;     ("1823" "Auguste-Hyacinthe Debay" "Premier grand Prix de Rome (Histoire).")
;;     ("1824" "Charles-Philippe Larivière")
;;     ("1825" "André Giroux (1801-1879)" "(Paysage historique)")
;;     ("1825" "Sébastien Louis Norblin de la Gourdaine (1796-1884)" "Antigone donnant la sépulture à Polynice")
;;     ("1829" "Jean-Louis Bézard" "Premier grand Prix de Rome en Histoire")
;;     ("1829" "Jean-Baptiste Adolphe Gibert (né en 1802 à Pointe-à-Pitre élève de Guillon-Lethière)" "Premier grand Prix de Rome en Paysage.")
;;     ("1830" "Emile Signol (1804-1892)")
;;     ("1831" "Henry-Frédéric-Schopin (Né à Lubeck en 1804 de parents français élève de Gros mort en 1890)")
;;     ("1832" "Hippolyte Flandrin (1809-1864)" "Thésée reconnu par son père")
;;     ("1833" "Gabriel Prieur (1806-1879)" "(Paysage historique)")
;;     ("1834" "Paul Jourdy (1805-1856)" "sur le thème Homère devenu vieux et aveugle s'en allait de ville en ville chantant ses vers et mendiant et partout il excitait l'enthousiasme et l'admiration des peuples.")
;;     ("1836" "Dominique,-Louis-Ferréol Papety (né en 1815)" "Premier Grand Prix.")
;;     ("1836" "Charles Octave Blanchard (1814-1842)" "Second Premier Grand Prix tous deux élèves de Leon Coigniet.")
;;     ("1836" "Jean Murat" "Second Grand Prix élève de feu Regnault et de Blanchet de l'Institut. Sujet Biblique; Le frappement du Rocher")
;;     ("1837" "Eugène-Ferdinand Buttura (1812-1852)" "(Paysage historique)")
;;     ("1837" "Thomas Couture" "second Prix")
;;     ("1838" "Isidore Pils (1813-1875)" "(Saint Pierre guérissant un boîteux à la porte du Temple)")
;;     ("1839" "Ernest Hébert" "La Coupe de Joseph")
;;     ("1840" "Pierre-Nicolas Brisset")
;;     ("1841" "Hippolyte Félix Lanoüe (1812-1872)" "premier Grand Prix en Paysage.")
;;     ("1842" "Victor Biennourry (1823-1893)")
;;     ("1844" "Félix-Joseph Barrias")
;;     ("1845" "Jean-Achille Benouville" "premier Grand Prix en Paysage")
;;     ("1845" "François-Léon Benouville (1821-1859)" "premier Grand Prix en Histoire")
;;     ("1845" "Alexandre Cabanel" "sur le thème Jésus dans le Prétoire.")
;;     ("1847" "Jules Eugène Lenepveu")
;;     ("1847" "Paul Baudry" "second grand prix")
;;     ("1848" "Joseph Stallaert")
;;     ("1848" "William-Adolphe Bouguereau")
;;     ("1848" "Gustave Boulanger" "second prix")
;;     ("1849" "Gustave Boulanger" "Ulysse reconnu par Euryclée")
;;     ("1849" "Charles-Camille Chazal (1825-1875)" "second grand prix. En Paysage")
;;     ("1849" "Charles-Joseph Lecointe" "premier Grand Prix.")
;;     ("1850" "William-Adolphe Bouguereau"  "Zénobie trouvée par des bergers sur les bords de l'Araxe")
;;     ("1850" "Paul Baudry" "Zénobie trouvée par des bergers sur les bords de l'Araxe")
;;     ("1851" "François-Nicolas Chifflard" "Périclès au lit de mort de son fils")
;;     ("1851" "Félix-Henri Giacomotti" "second grand prix")
;;     ("1851" "Émile Lévy" "second grand prix")
;;     ("1854" "Félix-Henri Giacomotti")
;;     ("1854" "Armand Bernard" "(Paysage historique) Lycidas et Moéris")
;;     ("1855" "Émile Lévy (né en 1826)" "premier Grand Prix.")
;;     ("1856" "Jules-Élie Delaunay" "second grand prix")
;;     ("1856" "Ernest-Eugène Hiolle")
;;     ("1857" "Charles Sellier" "La Résurrection de Lazare")
;;     ("1857" "Léon Bonnat" "second grand prix")
;;     ("1858" "Jean-Jacques Henner" "Adam et Ève découvrant le corps d'Abel")
;;     ("1860" "Ernest Michel" "Sophocle accusé par ses fils")
;;     ("1861" "Léon Perrault")
;;     ("1861" "Jules Joseph Lefebvre (1834-1912)")
;;     ("1864" "Diogène Maillart (né le 28 octobre 1840)" "élève de Léon Cogniet, Cornu et Laemlein. Premier grand Prix")
;;     ("1864" "Alexandre-Louis Leloir (né le 14 mars 1843)" "élève de son père. Premier second grand Prix")
;;     ("1864" " Eugène-Romain Thirion (né le 19 mai 1836)" "élève de Cabanel Deuxième second grand Prix; sur le thème Homère dans l'ile de Scyros.")
;;     ("1865" "Jules Machard" "Orphée aux Enfers")
;;     ("1865" "André Hennebicq")
;;     ("1865" "Gustave Huberti")
;;     ("1866" "Henri Regnault (1843-1871)")
;;     ("1867" "Paul-Joseph Blanc" "Le meurtre de Laïus par OEdipe")
;;     ("1868" "Édouard-Théophile Blanchard (1844-1879)")
;;     ("1869" "Luc-Olivier Merson (1846-1920)" "Le soldat de Marathon (Nota, cinq des dix concurrents étaient des élèves de Cabanel.")
;;     ("1871" "Édouard Toudouze" "Les Adieux d'OEdipe aux corps de sa femme et de ses fils.")
;;     ("1873" "Aimé Morot")
;;     ("1874" "Paul-Albert Besnard")
;;     ("1875" "Léon Comerre (1850-1916)" "L'Annonce aux bergers.")
;;     ("1875" "Jules Bastien-Lepage" "est deuxième")
;;     ("1876" "Joseph Wencker (1848-1919)")
;;     ("1877" "Théobald Chartran (1849-1907)" "Prise de Rome par les Gaulois.")
;;     ("1880" "Henri-Lucien Doucet (1856-1895)" "La Rencontre d'Ulysse et de Télémaque")
;;     ("1881" "Louis-Edouard-Paul Fournier (1857-1913)")
;;     ("1883" "Marcel-André Baschet" "pour OEdipe maudissant son fils Polynice")
;;     ("1883" "Émile Friant" "(second prix)")
;;     ("1884" "Henri Pinta (1856-1944)" "Grand Prix de Rome")
;;     ("1884" "Edouard Cabane" "(second prix)")
;;     ("1889" "Gaston Thys" "Deux Prix cette année là celui de l'année précédente n'ayant pas été décerné.")
;;     ("1889" "Ernest Laurent" "Deux Prix cette année là celui de l'année précédente n'ayant pas été décerné.")
;;     ("1889" "Jean-Celestin Danguy" "Premier second grand Prix")
;;     ("1889" "Charles-Amable Lenoir" "Deuxième second grand Prix")
;;     ("1891" "Hubert-Denis Etcheverry")
;;     ("1891" "Adolphe Déchenaud" "second grand prix")
;;     ("1894" "Auguste Leroux" "(1871-1954)" "Judith et Holopherne")
;;     ("1895" "Gaston Larée" "Premier grand prix")
;;     ("1895" "Albert Laurens" "Premier second grand prix")
;;     ("1895" "Jules Guinier" "Deuxième second grand prix. Thème: Le Christ mort descendu de la croix est pleuré par les saintes femmes.")
;;     ("1896" "Charles-Lucien Moulin (né en 1869)" "élève de Bouguereau, Luc-Olivier Merson, et Gabriel Ferrier. Premier grand prix sur le thème Fier de son talent sur la flûte" "Marsyas osa défier Apollon. Il fut convenu que le vaincu serait à la merci du vainqueur. Marsyas" "vaincu fut attaché à un arbre et impitoyablement écorché par un esclave. Apollon assiste au supplice.")
;;     ("1896" "Léon-Laurent Galand (né en 1872)" "élève de Gustave Moreau et Joseph Blanc Premier second grand prix.")
;;     ("1896" "Henri Guinier" "Deuxième second grand prix à Henri Guinier, élève de l'Académie Julian")
;;     ("1897" "Pas de Premier Grand Prix sur le thème Vulcain" "aidé de la Force et de la Violence" "enchaîne Prométhée sur les cimes du Caucase. Roger premier Second Grand Prix" "Sellier deuxième Second Grand Prix" "mention à Gibert.")
;;     ("1898" "Jean-Amédée Gibert" "La Piscine de Bethsaïda")
;;     ("1898" "William Laparra (1873-1920)" "(ou 1897 ? d'après ministère de la Culture)")
;;     ("1900" "Fernand Sabatté (? cf. 1938)")
;;     ("1902" "Paul Sieffert")
;;     ("1904" "Georges-Paul Leroux (1877-1957)" "La Famille (second grand prix de Peinture)")
;;     ("1905" "Lucien Jonas" "(1880-1947)")
;;     ("1905" "Albert Henry Krehbiel")
;;     ("1906" "François-Maurice Roganeau (1883-1973)" "Premier Grand prix de peinture et second prix de sculpture")
;;     ("1907" "Louis Léon Eugène Billotey (1883-1940)")
;;     ;; ("1907" "Émile Aubry ?"))
;;     ("1908" "Jean Lefeuvre")
;;     ("1909" "Pierre Bodard (1881/1937)" "Premier grand Prix avec Cerès rendant la vie à un enfant")
;;     ("1910" "Jean Dupas")
;;     ("1911" "Jean-Gabriel Domergue")
;;     ("1912" "Gabriel Girodon")
;;     ("1913" "Robert Davaux")
;;     ("1914" "Victor-Julien Giraud" "La Passion de la Vierge; Jean Despujols")
;;     ("1919" "Louis-Pierre Rigal" "Jeunesse et Vieillesse")
;;     ("1921" "Constantin Font (Born: January 11, 1890 in Auch (Gers) Died: 1954 in Paris")
;;     ("1922" "Pierre-Henri Ducos de La Haille")
;;     ("1923" "Pierre Dionisi (1904-1976)")
;;     ("1924" "René Marie Castaing")
;;     ("1924" "Charles Hoffbauer")
;;     ("1925" "Odette Pauvert (1903-1966)" "(premier prix de Rome obtenu par une femme) pour La légende de saint Ronan")
;;     ("1928" "Nicolas Untersteller" "Concert champêtre")
;;     ("1930" "Yves Brayer")
;;     ("1930" "Salvatore DeMaio")))

;;; ==============================
(provide 'naf-mode-awards-prizes)
;;; ==============================

;;; ==============================
;;;(require 'naf-mode-awards-prizes)
;;; ==============================


;;; ================================================================
;;; naf-mode-awards-prizes.el ends here
;;; EOF


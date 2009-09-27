;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-students-of-julian.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-students-of-julian provides variable lists and 
;;; regexp-opt'd constants of artist's whom atteneded Rodolphe Julian's 
;;; Académie Julian. 
;;; Each var list contains the artist names according to their country of birth
;;; or residence. Elts of var lists are of the form:
;;; ("Firstname Lastname" "Lastname (Firstname)")
;;; Each Var list has an associated Constant containing regexp-opt'd expressong of 
;;; the vars car and cadr (per element). These Constants are fontlocked with the
;;; face and face-var: 
;;; `naf-mode-artist-student-of-julian-face', 
;;; `naf-mode-artist-student-of-julian-fface'
;;;
;;; Lists were generated from the wiki entry:
;;; ``Élèves de l'Académie Julian
;;;   Artistes classés selon leur pays d'origine Période 1867-1940''
;;; SOURCE:
;;; (URL `http://fr.wikipedia.org/wiki/Acad%C3%A9mie_Julian#.C3.89l.C3.A8ves_de_l.27Acad.C3.A9mie_Julian')
;;; accessed: #{2009-09-17T21:48:38-04:00Z}#{09385} - MON
;;; 
;;; NOTE: This list has not yet ben verified/corroborated/xrefd with the correspoding ULAN entries.
;;; ==============================
;;; FUNCTIONS:
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `naf-mode-students-of-julian-french'
;;; `naf-mode-students-julian-us'
;;; `naf-mode-students-of-julian-misc'
;;; `naf-mode-students-of-julian-brazil'
;;; `naf-mode-students-of-julian-canada'
;;; `naf-mode-students-of-julian-finland'
;;; `naf-mode-students-of-julian-germany'
;;; `naf-mode-students-of-julian-norway'
;;; `naf-mode-students-of-julian-russia'
;;; `naf-mode-students-of-julian-switzerland'
;;; `naf-mode-students-of-julian-uk'
;;;
;;; VARIABLES:
;;; `*naf-students-of-julian-french*'
;;; `*naf-students-julian-us*'
;;; `*naf-students-of-julian-misc*'
;;; `*naf-students-of-julian-brazil*'
;;; `*naf-students-of-julian-canada*'
;;; `*naf-students-of-julian-finland*'
;;; `*naf-students-of-julian-germany*'
;;; `*naf-students-of-julian-norway*'
;;; `*naf-students-of-julian-russia*'
;;; `*naf-students-of-julian-switzerland*'
;;; `*naf-students-of-julian-uk*'
;;; `*naf-mode-students-of-julian-xrefs*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED: 
;;;
;;; RENAMED
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
;;; (URL `http://www.emacswiki.org/emacs/naf-mode-students-of-julian.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-26T19:01:45-04:00Z}#{09396} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-09-18T21:02:21-04:00Z}#{09386} - by MON>
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
;;; ©opyright (C) - MON KEY - 2009
;;; ==============================
;;; CODE:

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-25T12:03:14-04:00Z}#{09395} - by MON>
(eval-and-compile
(defvar *naf-mode-students-of-julian-xrefs* 
  '(*naf-mode-students-of-julian-xrefs* 
    *naf-students-of-julian-french*
    *naf-students-julian-us*
    *naf-students-of-julian-misc*
    *naf-students-of-julian-brazil*
    *naf-students-of-julian-canada*
    *naf-students-of-julian-finland*
    *naf-students-of-julian-germany*
    *naf-students-of-julian-norway*
    *naf-students-of-julian-russia*
    *naf-students-of-julian-switzerland*
    *naf-students-of-julian-uk*
    mon-help-naf-mode-faces)
  "List of symbol names of variables which xref each other in <PKG>
package. See FILE: \"./naf-mode-students-of-julian.el\"."))
;;
;;;test-me; *naf-mode-students-of-julian-xrefs* 
;;
;;;(progn (makunbound '*naf-mode-students-of-julian-xrefs*) 
;;;       (unintern '*naf-mode-students-of-julian-xrefs*))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-french*
  '(;; FRENCH 
    ("Abel-Truchet Louis"                "Louis (Abel-Truchet)")
    ("Alfassa Mirra"                     "Mirra (Alfassa)")
    ("Amas Ernest"                       "Ernest (Amas)")
    ("André Albert"                      "Albert (André)")
    ("Autenheimer Claude"                "Claude (Autenheimer)")
    ("Bazaine Jean"                      "Jean (Bazaine)")
    ("Bergevin Albert"                   "Albert (Bergevin)")
    ("Bonnard Pierre"                    "Pierre (Bonnard)")
    ("Boucher Jean"                      "Jean (Boucher)")
    ("Bourgeois Louise"                  "Louise (Bourgeois)")
    ("Brun Clément"                      "Clément (Brun)")
    ("Cavaillès Jules"                   "Jules (Cavaillès)")
    ("Chabas Maurice"                    "Maurice (Chabas)")
    ("Chabaud Auguste"                   "Auguste (Chabaud)")
    ("Chastel Roger"                     "Roger (Chastel)")
    ("Compard Émile"                     "Émile (Compard)")
    ("Couronne Pierre"                   "Pierre (Couronne)")
    ("Crotti Jean-Joseph"                "Jean-Joseph (Crotti)")
    ("Daudet Lucien"                     "Lucien (Daudet)")
    ("Denis Maurice"                     "Maurice (Denis)")
    ("Derain André"                      "André (Derain)")
    ("Devillié Charles"                  "Charles (Devillié)")
    ("Dignimont André"                   "André (Dignimont)")
    ("Dubuffet Jean"                     "Jean (Dubuffet)")
    ("Duchamp Marcel"                    "Marcel (Duchamp)")
    ("Dufrénoy Georges"                  "Georges (Dufrénoy)")
    ("Déchenaud Adolphe"                 "Adolphe (Déchenaud)")
    ("Désiré-Lucas Louis-Marie"          "Louis-Marie (Désiré-Lucas)")
    ("Evrard Louis"                      "Louis (Evrard)")
    ("Faivre Abel"                       "Abel (Faivre)")
    ("Fajard Suzanne"                    "Suzanne (Fajard)")
    ("Fié-Fieux Madeleine"               "Madeleine (Fié-Fieux)")
    ("Fontanarosa Lucien"                "Lucien (Fontanarosa)")
    ("Gimel Georges"                     "Georges (Gimel)")
    ("Guinier Henri"                     "Henri (Guinier)")
    ("Hélène-Renée Cabart-Danneville"    "Cabart-Danneville (Hélène-Renée)")
    ("Larcher Albert"                    "Albert (Larcher)")
    ("Laronze Jean"                      "Jean (Laronze)")
    ("Lartigue Jacques Henri"            "Henri (Lartigue Jacques)")
    ("Lestrange Gisèle"                  "Gisèle (Lestrange)")
    ("Lotiron Robert"                    "Robert (Lotiron)")
    ("Léger Fernand"                     "Fernand (Léger)")
    ("Majorelle Jacques"                 "Jacques (Majorelle)")
    ("Marguerite Jeanne Carpentier"      "Carpentier (Marguerite Jeanne)")
    ("Martine Martine"                   "Martine (Martine)")
    ("Martinez Antoine"                  "Antoine (Martinez)")
    ("Matisse Henri"                     "Henri (Matisse)")
    ("Maurin Charles"                    "Charles (Maurin)")
    ("Midy Arthur"                       "Arthur (Midy)")
    ("Moret Henry"                       "Henry (Moret)")
    ("Morisset François Henri"           "Henri (Morisset François)")
    ("Poupelet Jane"                     "Jane (Poupelet)")
    ("Quentin Mélanie"                   "Mélanie (Quentin)")
    ("Ranson Paul Elie"                  "Elie (Ranson Paul)")
    ("Rochegrosse Georges-Antoine"       "Georges-Antoine (Rochegrosse)")
    ("Roussel Ker-Xavier"                "Ker-Xavier (Roussel)")
    ("Schützenberger Paul René"          "René (Schützenberger Paul)")
    ("Sibra Paul"                        "Paul (Sibra)")
    ("Simon Lucien"                      "Lucien (Simon)")
    ("Steinmetz Philippe"                "Philippe (Steinmetz)")
    ("Sue Gabriel"                       "Gabriel (Sue)")
    ("Sérusier Paul"                     "Paul (Sérusier)")
    ("Thivier Émile"                     "Émile (Thivier)")
    ("Valtat Louis"                      "Louis (Valtat)")
    ("Vercoutter Jean"                   "Jean (Vercoutter)")
    ("Villemot Bernard"                  "Bernard (Villemot)")
    ("Villon Jacques"                    "Jacques (Villon)")
    ("Vuillard Édouard"                  "Édouard (Vuillard)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-french 
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-french*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-french*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val 
      *naf-students-of-julian-french* naf-mode-students-of-julian-french
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-french* 
;;;test-me; naf-mode-students-of-julian-french

;;
;;;(progn 
;;; (makunbound '*naf-students-of-julian-french*)
;;; (unintern   '*naf-students-of-julian-french*)
;;; (makunbound 'naf-mode-students-of-julian-french)
;;; (unintern   'naf-mode-students-of-julian-french))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-us*
  '( ;; UNITED STATES
    ("Grant Wood" "Wood (Grant)")
    ("Beatrice Wood" "Wood (Beatrice)")
    ("Fanny Vandegrift" "Vandegrift (Fanny)")
    ("Guy Rose" "Rose (Guy)")
    ("Robert Rauschenberg" "Rauschenberg (Robert)")
    ("John Willard Raught" "Raught (John Willard)")
    ("Joseph Raphael" "Raphael (Joseph)")
    ("Edward Clark Potter" "Potter (Edward Clark)")
    ("Lilla Cabot Perry" "Perry (Lilla Cabot)")
    ("Waldo Peirce" "Peirce (Waldo)")
    ("Jules Pages" "Pages (Jules)")
    ("Richard Miller" "Miller (Richard)")
    ("Willard Leroy Metcalf" "Metcalf (Willard Leroy)")
    ("Arthur Frank Mathews" "Mathews (Arthur Frank)")
    ("Joseph Christian Leyendecker" "Leyendecker (Joseph Christian)")
    ("Francis Xavier Leyendecker" "Leyendecker (Francis Xavier)")
    ("Albert Henry Krehbiel" "Krehbiel (Albert Henry)")
    ("Anna Klumpke" "Klumpke (Anna)")
    ("Charles Hopkinson" "Hopkinson (Charles)")
    ("Childe Hassam" "Hassam (Childe)")
    ("Henri Bernard Goetz" "Goetz (Henri Bernard)")
    ("John Marshall Gamble" "Gamble (John Marshall)")
    ("Arthur Wesley Dow" "Dow (Arthur Wesley)")
    ("Eanger Irving Couse" "Couse (Eanger Irving)")
    ("Robert W. Chambers" "Chambers (Robert W.)")
    ("Gutzon Borglum" "Borglum (Gutzon)")
    ("George Biddle" "Biddle (George)")
    ("Saul Bernstein" "Bernstein (Saul)")
    ("Thomas Hart Benton" "Benton (Thomas Hart)")
    ("Frank Weston Benson" "Benson (Frank Weston)")
    ("Cecilia Beaux" "Beaux (Cecilia)")
    ("Thomas Pollock Anshutz" "Anshutz (Thomas Pollock)")
    ("George Charles Aid" "Aid (George Charles)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-us
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-us*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-us*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-us* naf-mode-students-of-julian-us
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-us* 
;;;test-me; naf-mode-students-of-julian-us
;;
;;;(progn 
;;; (makunbound '*naf-students-of-julian-us*) 
;;; (unintern '*naf-students-of-julian-us*)
;;; (makunbound 'naf-mode-students-of-julian-us)
;;; (unintern 'naf-mode-students-of-julian-us))

;;; =============================
(eval-and-compile
(defvar *naf-students-of-julian-germany*
  '( ;; GERMANY
     ("Ludwig Gustav Scheuermann" "Scheuermann (Ludwig Gustav)")
     ("Hilla de Rebay" "Rebay (Hilla de)")
     ("Leo Putz" "Putz (Leo)")
     ("Emil Nolde" "Nolde (Emil)")
     ("Ludwig Meidner" "Meidner (Ludwig)")
     ("Käthe Kollwitz" "Kollwitz (Käthe)")
     ("Georg Kolbe" "Kolbe (Georg)")
     ("Fritz Erler" "Erler (Fritz)")
     ("Louise Breslau" "Breslau (Louise)")
     ("Ernst Barlach" "Barlach (Ernst)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-germany
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-germany*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-germany*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-germany* naf-mode-students-of-julian-germany
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-germany*
;;;test-me; naf-mode-students-of-julian-germany
;;
;;
;; (progn 
;;  (makunbound '*naf-students-of-julian-germany*) 
;;  (unintern '*naf-students-of-julian-germany*)
;;  (makunbound 'naf-mode-students-of-julian-germany) 
;;  (unintern 'naf-mode-students-of-julian-germany))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-brazil*
  '(;; BRAZIL
    ("Eliseu Visconti" "Visconti (Eliseu)")
    ("Nicolina Vaz de Assis" "Assis (Nicolina Vaz de)")
    ("Ismael Nery" "Nery (Ismael)")
    ("França (Julieta de)" "França (Julieta de)")
    ("Rodolfo Amoedo" "Amoedo (Rodolfo)")
    ("Georgina de Albuquerque" "Albuquerque (Georgina de)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-brazil
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-brazil*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-brazil*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-brazil* naf-mode-students-of-julian-brazil
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-brazil*
;;;test-me; naf-mode-students-of-julian-brazil
;;
;; (progn 
;;  (makunbound '*naf-students-of-julian-brazil*) 
;;  (unintern '*naf-students-of-julian-brazil*)
;;  (makunbound 'naf-mode-students-of-julian-brazil)
;;  (unintern 'naf-mode-students-of-julian-brazil))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-canada*
  '( ;; CANADA
    ;;Suzor-Côté (Marc-Aurèle de Foy)
    ("Narcisse Poirier" "Poirier (Narcisse)")
    ("Sophie Pemberton" "Pemberton (Sophie)")
    ("Ernest Lawson" "Lawson (Ernest)")
    ("Clarence Gagnon" "Gagnon (Clarence)")
    ("Henri Fabien" "Fabien (Henri)")
    ("Rodolphe Duguay" "Duguay (Rodolphe)")
    ("James Wilson Morrice" "Morrice (James Wilson)")
    ("John Charles Pinhey" "Pinhey (John Charles)")
    ("Robert W. Pilot" "Pilot (Robert W.)")
    ("Alexander Young Jackson" "Jackson (Alexander Young)")
    ("Joseph-Charles Franchère" "Franchère (Joseph-Charles)")
    ("Octave Bélanger" "Bélanger (Octave)")
    ("Henri Beau" "Beau (Henri)")
    ("Raoul Barré" "Barré (Raoul)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-canada
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-canada*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-canada*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val
      *naf-students-of-julian-canada* naf-mode-students-of-julian-canada
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-canada*
;;;test-me; naf-mode-students-of-julian-canada
;;
;; (progn 
;;  (makunbound '*naf-students-of-julian-canada*) 
;;  (unintern '*naf-students-of-julian-canada*)
;;  (makunbound 'naf-mode-students-of-julian-canada)
;;  (unintern 'naf-mode-students-of-julian-canada))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-misc*
  '(("Roger Limouse" "Limouse (Roger)")                       ;; ALGERIA                
    ("Maurice Mazo" "Mazo (Maurice)")                         ;; ALGERIA                
    ("Chas Laborde" "Laborde (Chas)")                         ;; ARGENTINA              
    ("Edgar Chahine" "Chahine (Edgar)")                       ;; ARMENIA                
    ("Richard Jeranian" "Jeranian (Richard)")                 ;; ARMENIA                
    ("Fernand Allard-l'Olivier" "Allard-l'Olivier (Fernand)") ;; BELGIUM
    ("Fernand Khnopff"        "Khnopff (Fernand)")            ;; BELGIUM
    ("Georges-Émile Lebacq"   "Lebacq (Georges-Émile)")       ;; BELGIUM
    ("Léon Bakst" "Bakst (Léon)")                             ;; BELARUS                
    ("Francisco Antonio Cano" "Cano (Francisco Antonio)")     ;; COLOMBIA               
    ("Kay Nielsen" "Nielsen (Kay)")                           ;; DENMARK                
    ("Philip Alexius de Laszlo" "Laszlo (Philip Alexius de)") ;; HUNGARY
    ;; Lajos Márk  ;; HUNGARY?
    ("Constance Markievicz" "Markievicz (Constance)")         ;; IRELAND                
    ("Marella Agnelli" "Agnelli (Marella)")                   ;; ITALY                  
    ("Khalil Gibran" "Gibran (Khalil)")                       ;; LEBANON                
    ("Jacques Lipchitz" "Lipchitz (Jacques)")                 ;; LITHUANIA              
    ("Charles Frederick Goldie" "Goldie (Charles Frederick)") ;; NEW ZEALAND            
    ("Bror Julius Olsson Nordfeldt" "Nordfeldt (Bror Julius Olsson)") ;; SWEDEN                 
    ("Carl Wilhelmson" "Wilhelmson (Carl)")                   ;; SWEDEN                 
    ("David Garfinkiel" "Garfinkiel (David)")                 ;; POLAND                 
    ("Jozef Mehoffer" "Mehoffer (Jozef)")                     ;; POLAND                 
    ("Wladyslaw Slewinski" "Slewinski (Wladyslaw)")           ;; POLAND                 
    ("Ioan Andreescu" "Andreescu (Ioan)")                     ;; ROMANIA                
    ("Camil Ressu" "Ressu (Camil)")                           ;; ROMANIA                
    ("František Kupka" "Kupka (František)")                   ;; CZECH REPUBLIC (1896)  
    ("Alfons Mucha" "Mucha (Alfons)")                         ;; CZECH REPUBLIC         
    ("Cevat Dereli" "Dereli (Cevat)")                         ;; TURKEY                 
    ("Joaquim Perez" "Perez (Joaquim)")                       ;; TURKEY                 
    ("Cassandre" "Cassandre")                                 ;; UKRAINE                
    ("Jean Peské" "Peské (Jean)")                             ;; UKRAINE                
    ("David Ossipovitch Widhopff" "Widhopff (David Ossipovitch)") ;; UKRAINE                
    ("Emilio Boggio" "Boggio (Emilio)")                       ;; VENEZUELA
    ("Arturo Michelena" "Michelena (Arturo)"))                ;; VENEZUELA    
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-misc
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-misc*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-misc*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-misc* naf-mode-students-of-julian-misc
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-misc*
;;;test-me; naf-mode-students-of-julian-misc
;;
;; (progn 
;;  (makunbound '*naf-students-of-julian-misc*)
;;  (unintern '*naf-students-of-julian-misc*)
;;  (makunbound 'naf-mode-students-of-julian-misc) 
;;  (unintern 'naf-mode-students-of-julian-misc))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-finland*
  '(;; FINLAND
    ("Akseli Gallen-Kallela" "Gallen-Kallela (Akseli)")
    ("Amélie Helga Lundahl" "Lundahl (Amélie Helga)")
    ("Pekka Halonen" "Halonen (Pekka)")
    ("Magnus Enckell" "Enckell (Magnus)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-finland
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-finland*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-finland*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-finland* naf-mode-students-of-julian-finland
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-finland*
;;;test-me; naf-mode-students-of-julian-finland
;;
;; (progn 
;;  (makunbound '*naf-students-of-julian-finland*) 
;;  (unintern '*naf-students-of-julian-finland*)
;;  (makunbound 'naf-mode-students-of-julian-finland) 
;;  (unintern 'naf-mode-students-of-julian-finland))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-norway*
  '(;; NORWAY
    ("Menga Schjelderup-Ebbe" "Schjelderup-Ebbe (Menga)")
    ("Aagot Vangen" "Vangen (Aagot)")
    ("Gunnar Utsond" "Utsond (Gunnar)")
    ("Wilhelm Rasmussen" "Rasmussen (Wilhelm)")
    ("Valentin Kielland" "Kielland (Valentin)")
    ("Halfdan Hertzberg" "Hertzberg (Halfdan)")
    ("Augusta Harmens" "Harmens (Augusta)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-norway 
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-norway*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-norway*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-norway* naf-mode-students-of-julian-norway 
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-norway*
;;;test-me; naf-mode-students-of-julian-norway
;;
;; (progn 
;; (makunbound '*naf-students-of-julian-norway*)         
;; (unintern '*naf-students-of-julian-norway*)
;; (makunbound 'naf-mode-students-of-julian-norway)
;; (unintern 'naf-mode-students-of-julian-norway))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-uk*
  '(;; UNITED KINGDOM
    ("Stanley Hayter" "Hayter (Stanley)")
    ("Anthony Gross" "Gross (Anthony)")
    ("Eric Forbes Robertson" "Robertson (Eric Forbes)")
    ("Jacob Epstein" "Epstein (Jacob)")
    ("Anne Dunn" "Dunn (Anne)")
    ("Charles Conder" "Conder (Charles)")
    ("Robert Bevan" "Bevan (Robert)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-uk
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-uk*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-uk*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-uk* naf-mode-students-of-julian-uk
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-uk*
;;;test-me; naf-mode-students-of-julian-uk
;;
;; (progn 
;;  (makunbound '*naf-students-of-julian-uk*)
;;  (unintern '*naf-students-of-julian-uk*)
;;  (makunbound 'naf-mode-students-of-julian-uk)
;;  (unintern 'naf-mode-students-of-julian-uk))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-russia*
  '(;; RUSSIA
    ("Boris Anrep"            "Anrep (Boris)")
    ("Marie Bashkirtseff"     "Bashkirtseff (Marie)")
    ("Alexandre Chevtchenko"  "Chevtchenko (Alexandre)")
    ("Gleb Derujinksy"        "Derujinksy (Gleb)")
    ("Serge Férat"            "Férat (Serge)")     
    ("Eugene Lanceray"        "Lanceray (Eugene)")
    ("Max Weber"              "Weber (Max)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-russia
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-russia*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-russia*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-russia* naf-mode-students-of-julian-russia
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-russia*
;;;test-me; naf-mode-students-of-julian-russia
;;
;; (progn 
;;  (makunbound '*naf-students-of-julian-russia*) 
;;  (unintern '*naf-students-of-julian-russia*)
;;  (makunbound 'naf-mode-students-of-julian-russia)
;;  (unintern 'naf-mode-students-of-julian-russia))

;;; ==============================
(eval-and-compile
(defvar *naf-students-of-julian-switzerland*
  '(;; SWITZERLAND
    ("Cuno Amiet"      "Amiet (Cuno)")
    ("Ernest Biéler"   "Biéler (Ernest)")
    ("Edmond Bille"    "Bille (Edmond)")
    ("Hans Erni"       "Erni (Hans)")
    ("Joseph Kaiser"   "Kaiser (Joseph)")
    ("Hermann Obrist"  "Obrist (Hermann)")
    ("Félix Vallotton" "Vallotton (Félix)"))
  "*Keyword list of for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-students-of-julian-switzerland
(regexp-opt (append
             (mapcar (lambda (x) (car x)) *naf-students-of-julian-switzerland*)
             (mapcar (lambda (x) (cadr x)) *naf-students-of-julian-switzerland*)) 'paren)))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val  
      *naf-students-of-julian-switzerland* naf-mode-students-of-julian-switzerland
      *naf-mode-students-of-julian-xrefs* naf-mode-artist-student-of-julian-fface))
;;
;;;test-me; *naf-students-of-julian-switzerland*
;;;test-me; naf-mode-students-of-julian-switzerland 
;;
;;(progn 
;; (makunbound '*naf-students-of-julian-switzerland*) 
;; (unintern '*naf-students-of-julian-switzerland*)
;; (makunbound 'naf-mode-students-of-julian-switzerland) 
;; (unintern 'naf-mode-students-of-julian-switzerland))

;;; ==============================
;;; CLEANUP:
;;;(progn
;;;  ;; CONSTANTS:
;;;  (makunbound 'naf-mode-students-of-julian-french)
;;;  (makunbound 'naf-mode-students-julian-us)
;;;  (makunbound 'naf-mode-students-of-julian-misc)
;;;  (makunbound 'naf-mode-students-of-julian-brazil)
;;;  (makunbound 'naf-mode-students-of-julian-canada)
;;;  (makunbound 'naf-mode-students-of-julian-finland)
;;;  (makunbound 'naf-mode-students-of-julian-germany)
;;;  (makunbound 'naf-mode-students-of-julian-norway)
;;;  (makunbound 'naf-mode-students-of-julian-russia)
;;;  (makunbound 'naf-mode-students-of-julian-switzerland)
;;;  (makunbound 'naf-mode-students-of-julian-uk)
;;;  ;;
;;;  (unintern 'naf-mode-students-of-julian-french)
;;;  (unintern 'naf-mode-students-julian-us)
;;;  (unintern 'naf-mode-students-of-julian-misc)
;;;  (unintern 'naf-mode-students-of-julian-brazil)
;;;  (unintern 'naf-mode-students-of-julian-canada)
;;;  (unintern 'naf-mode-students-of-julian-finland)
;;;  (unintern 'naf-mode-students-of-julian-germany)
;;;  (unintern 'naf-mode-students-of-julian-norway)
;;;  (unintern 'naf-mode-students-of-julian-russia)
;;;  (unintern 'naf-mode-students-of-julian-switzerland)
;;;  (unintern 'naf-mode-students-of-julian-uk)
;;;  ;; VARIABLES:
;;;  (makunbound '*naf-students-of-julian-french*)
;;;  (makunbound '*naf-students-julian-us*)
;;;  (makunbound '*naf-students-of-julian-misc*)
;;;  (makunbound '*naf-students-of-julian-brazil*)
;;;  (makunbound '*naf-students-of-julian-canada*)
;;;  (makunbound '*naf-students-of-julian-finland*)
;;;  (makunbound '*naf-students-of-julian-germany*)
;;;  (makunbound '*naf-students-of-julian-norway*)
;;;  (makunbound '*naf-students-of-julian-russia*)
;;;  (makunbound '*naf-students-of-julian-switzerland*)
;;;  (makunbound '*naf-students-of-julian-uk*)
;;;  (makunbound '*naf-mode-students-of-julian-xrefs*)
;;;  (unintern '*naf-students-of-julian-french*)
;;;  (unintern '*naf-students-julian-us*)
;;;  (unintern '*naf-students-of-julian-misc*)
;;;  (unintern '*naf-students-of-julian-brazil*)
;;;  (unintern '*naf-students-of-julian-canada*)
;;;  (unintern '*naf-students-of-julian-finland*)
;;;  (unintern '*naf-students-of-julian-germany*)
;;;  (unintern '*naf-students-of-julian-norway*)
;;;  (unintern '*naf-students-of-julian-russia*)
;;;  (unintern '*naf-students-of-julian-switzerland*)
;;;  (unintern '*naf-students-of-julian-uk*)
;;;  (unintern '*naf-mode-students-of-julian-xrefs*))

;;; ==============================
(provide 'naf-mode-students-of-julian)
;;; ==============================

;;; ================================================================
;;; naf-mode-students-of-julian.el ends here
;;; EOF

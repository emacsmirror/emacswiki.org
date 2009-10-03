;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-publications-periodicals-intnl.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-publications-periodicals-intnl 
;;; Titles of International Periodicals 
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `naf-mode-publications-periodicals-intnl'
;;;
;;; VARIABLES:
;;; `*naf-naf-publications-periodicals-intnl*'
;;; `*naf-mode-publications-periodicals-intnl-xrefs*'
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
;;; NOT CURRENTLY SEARCHING:
;;; Styl, Blätter für Mode und die angenehmen Dinge des Lebens
;;; Style arts; feminine fashions, accessories, decoration
;;; Perfection; album pratique de la mode.
;;; Mode; Everywoman's Fashion Magazine
;;; Stile
;;; Hor zu
;;; Bild
;;; Quick
;;; Pan
;;; AIZ  ;;; (German)
;;; Modo ;;; (Italy)
;;; ==============================
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK:
;;; (URL `http://www.emacswiki.org/emacs/naf-mode-publications-periodicals-intnl.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T20:23:33-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T17:01:00-04:00Z}#{09327} - by MON KEY>
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
;;; ================================================================
;;; Copyright © 2009 MON KEY
;;; ==============================
;;; CODE:

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-02T18:48:20-04:00Z}#{09405} - by MON>
(eval-and-compile
(defvar *naf-mode-publications-periodicals-intnl-xrefs*
  '(*naf-naf-publications-periodicals-intnl*
    *naf-publications-periodicals-english*
    *naf-publications-periodicals-english-one-word*
    *naf-publications-periodicals-french*
    *naf-mode-publications-periodicals-intnl-xrefs*
    mon-help-naf-mode-faces)
  "*List of symbol names of variables which xref each other in the
`naf-mode-publications-periodicals-intnl' package.
See FILE: \"./naf-mode-publications-periodicals-intnl.el\"."))
;;
;;;test-me; *naf-mode-publications-periodicals-intnl-xrefs*
;;
;;;(progn (makunbound '*naf-mode-publications-periodicals-intnl-xrefs*)
;;;       (unintern '*naf-mode-publications-periodicals-intnl-xrefs*))
;; "International Periodical titles.\n
;; 

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-02T18:35:14-04:00Z}#{09405} - by MON>
(eval-and-compile
(defvar *naf-publications-periodicals-intnl*
  '("Ty I Ja"                      ;;; (You and I - Eastern Block)
    "Umelecky Mesicnik"            ;;; (Artistic Monthly) - (Czech)
    "Sovremennaya Arkhitektura"    ;;; (Russian)
    "Zenit"                        ;;; (Yugoslavian)
    "Tik Tak"                      ;;; (Denmark)
    "De 8 en opbouw"
    ;; SPAIN
    "Vértice"                      ;;; Spain
    "Blanco y Negro"               ;;; Spain
    "Blanco Y Negro"               ;;; Spain
    "Scena Illustrata"             ;;; Spain
    "Nuevo Mundo"                  ;;; Spain
    "Para Ti"                      ;;; Spain
    "Revista Ford"                 ;;; Spain
    "La Moda maschile"             ;;; Spain
    ;; NETHERLANDS
    "Blad Voor Kunst"              ;;; (Netherlands)
    "Wendingen"                    ;;; (Netherlands)
    "Mandril"                      ;;; (Netherlands)
    ;; ITALY
    "La Rivista"                   ;;; (Italy)
    "La Lettura"                   ;;; (Italy)
    "Stile Industria"              ;;; (Italy)
    "Domus"                        ;;; (Italy)
    "Ottagono"                     ;;; (Italy)
    ;; Germano-Austrian
    "Berliner Illustrierte"         ;;; Germano-Austrian
    "Blau-rot"                      ;;; Germano-Austrian
    "Bunte Illustrierte"            ;;; Germano-Austrian
    "Das Neue Blatt"                ;;; Germano-Austrian
    "Das Plakat"                    ;;; Germano-Austrian
    "Der Silberspiegel"             ;;; Germano-Austrian
    "Der Speigel"                   ;;; Germano-Austrian
    "Deutsche Kunst und Dekoration" ;;; Germano-Austrian
    "Die Bottcherstrasse"           ;;; Germano-Austrian
    "Die Dame"                      ;;; Germano-Austrian
    "Die Form"                      ;;; Germano-Austrian
    "Die Kunst"                     ;;; Germano-Austrian
    "Die Neue Linie"                ;;; Germano-Austrian
    "Die Woche"                     ;;; Germano-Austrian
    "Fliegende Blatter"             ;;; Germano-Austrian
    "Fliegende Blätter"             ;;; Germano-Austrian
    "Frankfurter Illustrierte"      ;;; Germano-Austrian
    "Gebrauchsgraphik"              ;;; Germano-Austrian
    "Jugend"                        ;;; Germano-Austrian
    "Kladderadatsch"                ;;; Germano-Austrian
    "Kristall"                      ;;; Germano-Austrian
    "Lustige Blätter"               ;;; Germano-Austrian
    "Munchner Illustrierte"         ;;; Germano-Austrian
    "Neuer Illustrierte"            ;;; Germano-Austrian
    "Simplicissimus"                ;;; Germano-Austrian
    "Styl, Blätter für Mode"        ;;; Germano-Austrian
    "Typographische Monatsblatter"  ;;; Germano-Austrian
    "Ver Sacrum"                    ;;; Germano-Austrian ;; Scesession      
    )
  "*Keyword list International periodical titles for `naf-mode' font-locking."))
;;
(eval-and-compile
(defconst naf-mode-naf-publications-periodicals-intnl
  (concat "\\<" (regexp-opt *naf-publications-periodicals-intnl* 'paren) "\\>")))
;;
(eval-and-compile
  (mon-help-swap-var-doc-const-val
      *naf-publications-periodicals-intnl* naf-mode-publications-periodicals-intnl
      *naf-mode-publications-periodicals-intnl-xrefs*  naf-mode-publication-periodical-fface))
;;
;;(progn (makunbound '*naf-publications-periodicals-intnl*)
;;       (unintern '*naf-publications-periodicals-intnl*)
;;       (makunbound 'naf-mode-naf-publications-periodicals-intnl)
;;       (unintern 'naf-mode-naf-publications-periodicals-intnl))
;;

;;; ==============================
(provide 'naf-mode-publications-periodicals-intnl)
;;; ==============================

;;; ================================================================
;;; naf-mode-publications-periodicals-intnl.el ends here
;;; EOF


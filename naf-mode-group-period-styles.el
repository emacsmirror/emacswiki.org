;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-group-period-styles.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-group-period-styles provides  
;;; list of terms to flag on artistic Groups, Subjects, Periods, Styles 
;;; or movements. Used for keyword font-locking in `naf-mode'.
;;; naf-mode-name uses naf-mode- - face.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `naf-mode-group-period-styles'
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-group-period-styles.el)
;;; FIRST-PUBLISHED <Timestamp: #{2009-11-21T20:12:01-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:30:29-04:00Z}#{09327} - by MON KEY>
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
;;; Copyright (C) 2009 by MON KEY
;;; ==============================
;;; CODE:

;;; ==============================
(provide 'naf-mode-group-period-styles)
;;; ==============================

;;; ==============================
(let ((naf-subject-periods-styles
       (list
	"Abstrait-constructiviste"
	"abstrait-constructiviste"
	"Algonquin Round Table"
	"Bloomsbury Group"
	"Art-Déco"
	"Art Déco"
	"Art déco"
	"art déco"
	"Art Deco"
	"art deco"
	"Art Nouveau"
	"art nouveau"
	"Arte joven"
	"Arte nuova"
	"Arts and Crafts Movement"
	"Arts and Crafts"
	"Ashcan School"
	"Barbizon"
	"Ballets Russes"
	"Ballets Russe"
	"Bauhaus"
	"Blaue Reiter"
	"Constructavism"
	"Constructavism"
	"Crafts Revival"
	"Cubism"
	"Cubisme"
	"Cubiste"
	"Cubists"
	"Dadaist"
	"De Stijl"
	"Deutsche Werkbund"
	"Devistil Group"
	"Fauves"
	"Fauvism"
	"Fauvist"
	"Functionalism"
	"Futurism"
	"Futuriste"
	"Gesamtkunstwerk"  ;;(trans. total art work)
	"Impressionist"
	"Impressionniste"
	"jazz age"
	"Jazz Age"
	"Jugendstil"
	"L'Art Nouveau"
	"Le Mondern Style"
	"Liberty Style"
	"Mannerism"
	"Naturalism"
	"Nabis"
	"Naïf"
	"Neo-Gothic"
	"Nieuwe kunst"
	"Novecento"
	"Novembergruppe" ;; (November Group)
	"Néoclassique"
	"Odeon Style"
	"Orphism"
	"Pointillist"
	"Postcubiste"
	"Postimpressionniste"
	"Praesens"
	"Pré-impressionniste"
	"Réaliste"
	"Secession"
	"Skønvirke"
	"Stile Floreale"
	"Stile Liberty"
	"Style Métro"
	"Suprematism"
	"Surrealism"
	"Surrealist"
	"Surréalisme"
	"Szkola Krakowa"
	"Vienna Secession"
	"Weiner Werkstatte"
	"Wiener Secession"
	"Wiener Werkstätte"
	"cubiste"
	"expressionniste"
	"futuriste"
	"impressionniste"
	"néoclassique"
	"réaliste"
	"surréalisme"
	"suréaliste"
       )))
;;
(defconst naf-mode-group-period-styles
  (concat (regexp-opt naf-subject-periods-styles 'paren))
  "List of terms which flag on artistic groups, periods, styles, or movements.
See also; `naf-mode-group-period-style-fface', `naf-mode-group-period-style-face'.
Used for font-locking in `naf-mode'." ))
;;; ==============================

;;; ==============================
(require 'naf-mode-group-period-styles)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;; (provide 'naf-mode-group-period-styles)
;;; ==============================

;;; ================================================================
;;; naf-mode-group-period-styles.el ends here
;;; EOF

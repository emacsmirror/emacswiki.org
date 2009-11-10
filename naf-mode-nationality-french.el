;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-nationality-french.el
;;; ================================================================
;;; DESCRIPTION:
;;; `naf-mode-nationality-french' provides
;;; French nationalities keyword lists in Benezit and ISO style. 
;;; Note, some Benezit's differ from ISO.
;;; font-locked by `naf-mode-nationality-face'.
;;; Documented with variable `naf-mode-nationality-fface'.
;;; 
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `naf-mode-nationality-french'
;;;
;;; VARIABLES:
;;; `*naf-mode-nationality-french-xrefs*'
;;; `*naf-nationality-french*'
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-nationality-french.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T21:13:49-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-09T12:50:40-04:00Z}#{09327} - by MON KEY>
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
;;; CREATED: <Timestamp: #{2009-10-01T18:46:42-04:00Z}#{09404} - by MON>
(eval-and-compile
(defvar *naf-mode-nationality-french-xrefs*
  '(*naf-nationality-french*
    *naf-nationality-english*
    *naf-mode-nation-english-xrefs*    
    *naf-mode-nation-french-xrefs*    
    *naf-mode-nationality-english-xrefs*    
    *naf-mode-nationality-french-xrefs*
    mon-help-naf-mode-faces)
  "*List of symbol names of variables which xref each other in the
`naf-mode-nationality-french' package.
See FILE: \"./naf-mode-nationality-french.el\"."))
;;
;;;test-me; *naf-mode-nationality-french-xrefs*
;;
;;;(progn (makunbound '*naf-mode-nationality-french-xrefs*)
;;;       (unintern '*naf-mode-nationality-french-xrefs*))


;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-02T21:03:15-04:00Z}#{09406} - by MON>
(eval-and-compile
(defvar *naf-nationality-french*
  '("Africain"
    "Africaine"
    "Algérien"
    "Algérienne"
    "Allemand"
    "Allemande"
    "Américain"
    "Américaine"
    "Anglais"
    "anglais"
    "Anglaise"
    "Argentine"
    "Asiatique"
    "Australien"
    "australien"
    "Australienne"
    "Autrichien"
    "Autrichienne"
    "autrichien"
    "autrichienne"
    "Belge"
    "Belgique"
    "Bolognese"
    "Britannique"
    "Brésilien"
    "Brésilienne"
    "Canadien"
    "Canadienne"
    "Chinois"
    "Chinoise"
    "Danoise"
    "danoise"
    "Deutsche"
    "égyptienne"
    "Égyptienne"
    "Espagnole"
    "Espagnol"
    "Européen"
    "Européenne"
    "Français"
    "français"
    "Française"
    "française"
    "Hollandais"
    "Hongrois"
    "Indien"
    "Indienne"
    "Irlandais"
    "Irlandaise"
    "Italien"
    "Italienne"
    "Japonais"
    "Japonaise"
    "Marocain"
    "Marocaine"
    "Mexicain"
    "Mexicaine"
    "Norvégien"
    "Néerlandais"
    "Néerlandaise"
    "Polonais"
    "Polonaise"
    "Portugais"
    "Portugaise"
    "Russe"
    "Slovene"
    "Suisse"
    "suédoise"
    "Suédoise"
    "Suédois"
    "Sénégalais"
    "Sénégalaise"
    "Tchéchoslovaque"
    "Égyptien"
    "Égyptienne")
  "*Keyword list of French nationalities in Benezit and 'ISO' style.
Note, some Benezit's differ from ISO.
Used primarily for `naf-mode' font-locking."))
;;
(eval-and-compile
(defconst naf-mode-nationality-french
  ;;Consider not using the traling "\\>"
  (concat "\\<" (regexp-opt *naf-nationality-french* 'paren) "\\>"))) 
;;
(eval-and-compile
  (mon-help-swap-var-doc-const-val
      *naf-nationality-french* naf-mode-nationality-french
      *naf-mode-nationality-french-xrefs* naf-mode-nationality-fface))
;;
;;(progn (makunbound '*naf-nationality-french*)
;;       (unintern '*naf-nationality-french*)
;;       (makunbound 'naf-mode-nationality-french) 
;;       (unintern 'naf-mode-nationality-french))

;;; ==============================
(provide 'naf-mode-nationality-french)    
;;; ==============================

;;; ================================================================
;;; naf-mode-nationality-french.el ends here
;;; EOF

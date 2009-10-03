;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-nationality-english.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-nationality-english lists nationality terms in English.
;;; font-locked with `naf-mode-nationality-fface', `naf-mode-nationality-face'
;;; 
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS: 
;;; `naf-mode-nationality-english'
;;;
;;; VARIABLES:
;;; `*naf-nationality-english*'
;;; `*naf-mode-nationality-english-xrefs*'
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-nationality-english.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T21:11:49-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED: Summer 2008
;;; <Timestamp: #{2009-08-09T12:46:52-04:00Z}#{09327} - by MON KEY>
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
;;; CREATED: <Timestamp: #{2009-10-01T18:33:18-04:00Z}#{09404} - by MON>
(eval-and-compile
(defvar *naf-mode-nationality-english-xrefs*
  '(*naf-nationality-english*
    *naf-nationality-french*
    *naf-mode-nation-english-xrefs*    
    *naf-mode-nation-french-xrefs*    
    *naf-mode-nationality-english-xrefs*    
    *naf-mode-nationality-french-xrefs*
    mon-help-naf-mode-faces)
  "*List of symbol names of variables which xref each other in the
`naf-mode-nationality-english' package.
See FILE: \"./naf-mode-nationality-english.el\"."))
;;
;;;test-me; *naf-mode-nationality-english-xrefs*
;;
;;;(progn (makunbound '*naf-mode-nationality-english-xrefs*)
;;;       (unintern '*naf-mode-nationality-english-xrefs*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-01T18:41:44-04:00Z}#{09404} - by MON>
(eval-and-compile
(defvar *naf-nationality-english*
  '("American"
    "Australian"
    "European"
    "Algerian"
    "Asian"
    "African"
    "African-American"
    "Mexican-American"
    "Asian-American"
    "Chinese-American"
    "Japanese-American"
    "Russian-American"
    "Romanian-American"
    "Austro-Hungarian"
    "Apulian"
    "Argentinean"
    "Athenian"
    "Austrian"
    "Bahraini"
    "Bangladeshi"
    "Belgian"
    "Bohemian"
    "Bolognese"
    "Bolivian"
    "Brazilian"
    "British"
    "Bulgarian"
    "Canadian"
    "Chilean"
    "Chinese"
    "Corinthian"        ;; WAS: "Corinthia" typo?
    "Cuban-American"
    "Cuban"
    "Czech"
    "Czechoslovakian"
    "Danish"
    "Dutch"
    "English"
    "Egyptian"
    "Finnish"
    "Flemish"
    "Florentine"
    "French"
    "German"
    "Greek"
    "Guatemalan"
    "Hungarian"
    "Icelandic"
    "Indian"
    "Indonesian"
    "Iraqi"
    "Irish"
    "Israeli"
    "Italian"
    "Japanese"
    "Jewish"
    "Moorish"
    "Korean"
    "Hispanic"
    "Latin American"
    "Hispanic"
    "Lithuanian"
    "Macedonian"
    "Mexican"
    "Moroccan"
    "Mexican-American"
    "Native American"
    "Netherlandish"
    "Nigerian"
    "Norwegian"
    "Pakistani"
    "Persian"
    "Peruvian"
    "Polish"
    "Puerto Rican"
    "Portuguese"
    "Roumanian"
    "Romanian"
    "Russian"
    "South African"
    "Scandinavian"
    "Scottish"
    "Senegalese"
    "Slovene"
    "Soviet"
    "Spanish"
    "Swedish"
    "Swiss"
    "Sri Lankan"
    "Tyrolean"
    "Uruguayan"
    "Venezuelan"
    "Venetian"
    "Vietnamese"
    "Welsh"
    "Yugoslavian")
  "*Keyword list of English nationality terms for `naf-mode' font-locking."))
;;
(eval-and-compile
(defconst naf-mode-nationality-english
  (concat "\\<" (regexp-opt *naf-nationality-english* 'paren))) "\\>")
;;
(eval-and-compile
  (mon-help-swap-var-doc-const-val
      *naf-nationality-english* naf-mode-nationality-english
      *naf-mode-nationality-english-xrefs* naf-mode-nationality-fface)) ;; <INSERT-FACE-NAME-AFTER-XREF>
;;
;;(progn (makunbound '*naf-nationality-english*) (unintern '*naf-nationality-english*)
;;       (makunbound 'naf-mode-nationality-english) (unintern 'naf-mode-nationality-english))

;;; ==============================
(provide 'naf-mode-nationality-english)
;;; ==============================

;;; ================================================================
;;; naf-mode-nationality-english.el ends here
;;; EOF

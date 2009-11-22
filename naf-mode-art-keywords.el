;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-art-keywords.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-art-keywords for font-locking in `naf-mode'.
;;; Keyword terms that identify art related productions.
;;; fontlocked by: `naf-mode-art-keywords-role-face', `naf-mode-art-keywords-role-fface'.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `naf-mode-art-keywords'
;;;
;;; VARIABLES:
;;; `*naf-art-keywords*'
;;; `*naf-mode-art-keywords-xrefs*'
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
;;; THIS FILE's LIST NEEDS TO BE BROKEN UP!
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-art-keywords.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:51:50-05:00Z}#{09477} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:07:07-04:00Z}#{09327} - by MON KEY>
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
;;; CREATED: <Timestamp: #{2009-09-14T18:43:13-04:00Z}#{09381} - by MON>
(eval-and-compile
(defvar *naf-mode-art-keywords-xrefs*
  '(*naf-art-keywords* *naf-mode-art-keywords-xrefs* mon-help-naf-mode-faces)
  "List of symbol names of variables which xref each other in naf-mode-art-keywords
package. See FILE: \"./naf-mode-art-keywords.el\". ►►►"))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-14T18:23:37-04:00Z}#{09381} - by MON>
(eval-and-compile
(defvar *naf-art-keywords*
  (list "Moulin-Rouge" ;; PLACES: - should thse be moved?
	"moulin rouge"
	"Moulin Rouge"
	"Champs-Elysées"
	"Crystal Palace"
	"Folies Bergere"
	;; FRENCH LANG KEYWORDS:
	"revues humoristiques"
	"Couture"
	"couture"
	"haute couture"
	"haute-couture"
	"Haute Couture"
	"advertising"
	"advertisting art"
	"caricature illustration"
	"cartoon illustratrion"
	"cover art"
	"cover illustration"
	"covers for" ;; consider moving to naf-mode-x-of keyword-list
	"display ad"
	"illuminated manuscript"
	"illuminated manuscripts"
	"illustration art"
	"illustrations"
	"magazine ads"
	"magazine advertisements"
	"magazine cover"
	"magazine covers"
	"magazine illustration"
	"retrospective exhibition"
	"newspaper ads"
	"newspaper advertisements"
	"package designs"
	"poster"
	"posters"
	"poster advertisements"
	"produced advertisements for"  ;; consider moving to naf-mode-x-of keyword-list
	"water colors"
	"window displays"
	;; PRINTMAKING AND ARTMAKING KEYWORDS:
	"murals"
	"pen and ink rendering"
	"pen-and-ink"
	"pen-and-inks"
	"Ink drawings"
	"ink wash"
	"halftone engraving"
	"halftones"
	"letterpress"
	"lithograph"
	"lithographs"
	"lithography"
	"copper plate engraving"
	"steel plate engraving"
	"chromolithograph"
	"gouache"
	"etching"
	"etchings"
	"engravings"
	"typography")
  "*Keyword list identify art related and artistic productions
for font-locking in `naf-mode'."))
;;
(eval-and-compile 
(defconst naf-mode-art-keywords (concat (regexp-opt *naf-art-keywords* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val
      *naf-art-keywords* naf-mode-art-keywords *naf-mode-art-keywords-xrefs* naf-mode-art-keywords-role-fface))
;;
;; (progn (makunbound 'naf-mode-art-keywords) (unintern 'naf-mode-art-keywords)
;;       (makunbound '*naf-art-keywords*) (unintern '*naf-art-keywords*))

;;; ==============================
(provide 'naf-mode-art-keywords)
;;; ==============================

;;; ================================================================
;;; naf-mode-art-keywords.el ends here
;;; EOF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-state-names.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-state-names lists States in United States keyword highlighting
;;;  with `naf-mode'.
;;; `naf-mode-state-names' -> `naf-mode-place-face', `naf-mode-place-fface'.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `naf-mode-name-state-names'
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
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-state-names.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:21:21-05:00Z}#{09477} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T17:23:27-04:00Z}#{09327} - by MON KEY>
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
(provide 'naf-mode-state-names)
;;; ==============================

;;; ==============================
;;; New version with the let statement placed appropriately.
;;; ==============================
(defconst naf-mode-state-names
  (let ((naf-state-names
       (list
	"Alberta"
	"Yukon"
	"New Brunswick"
	"Nova Scotia"
	"Newfoundland"
	"Prince Edward Island"
	"Nunavut"
	"Manitoba"
	"Ontario"
	"Quebec"
	"British Columbia"
	"Saskatchewan"
	"Alabama"
	"Alaska"
	"Arizona"
	"Arkansas"
	"California" "Californie"
	"Colorado"
	"Connecticut"
	"Delaware"
	"Florida" "Floride"
	"Georgia" "Géorgie"
	"Hawaii" "Hawaï"
	"Idaho"
	"Illinois"
	"Indiana"
	"Iowa"
	"Kansas"
	"Kentucky"
	"Louisiana" "Louisane"
	"Maine"
	"Maryland"
	"Massachusetts"
	"Michigan"
	"Minnesota"
	"Mississippi"
	"Missouri"
	"Montana"
	"Nebraska"
	"Nevada"
	"New Hampshire"
	"New Jersey"
	"New Mexico" "Nouveau-Mexique"
	"New York"
	"North Carolina" "Caroline du Nord"
	"North Dakota" "Dakota du Nord"
	"Ohio"
	"Oklahoma"
	"Oregon"
	"Pennsylvania" "Pennsylvanie"
	"Rhode Island"
	"South Carolina" "Caroline du Sud"
	"South Dakota" "Dakota du Sud"
	"Tennessee"
	"Texas"
	"Utah"
	"Vermont"
	"Virginia" "Virginie"
	"Washington"
	"West Virginia" "Virginie Occidentale"
	"Wisconsin"
	"Wyoming"
	)))
    (concat "\\<" (regexp-opt naf-state-names 'paren) "\\>"))
  "List of States of United States.
Includes provincial regions of Ye olde Canada. :P
keyword highlighting for `naf-mode' font-lock faces.")

;;; ==============================
;;; OLD VERSION:
;;; ==============================
;; (let ((naf-state-names
;;        (list
;; 	"Alberta"
;; 	"Yukon"
;; 	"New Brunswick"
;; 	"Nova Scotia"
;; 	"Newfoundland"
;; 	"Prince Edward Island"
;; 	"Nunavut"
;; 	"Manitoba"
;; 	"Ontario"
;; 	"Quebec"
;; 	"British Columbia"
;; 	"Saskatchewan"
;; 	"Alabama"
;; 	"Alaska"
;; 	"Arizona"
;; 	"Arkansas"
;; 	"California" "Californie"
;; 	"Colorado"
;; 	"Connecticut"
;; 	"Delaware"
;; 	"Florida" "Floride"
;; 	"Georgia" "Géorgie"
;; 	"Hawaii" "Hawaï"
;; 	"Idaho"
;; 	"Illinois"
;; 	"Indiana"
;; 	"Iowa"
;; 	"Kansas"
;; 	"Kentucky"
;; 	"Louisiana" "Louisane"
;; 	"Maine"
;; 	"Maryland"
;; 	"Massachusetts"
;; 	"Michigan"
;; 	"Minnesota"
;; 	"Mississippi"
;; 	"Missouri"
;; 	"Montana"
;; 	"Nebraska"
;; 	"Nevada"
;; 	"New Hampshire"
;; 	"New Jersey"
;; 	"New Mexico" "Nouveau-Mexique"
;; 	"New York"
;; 	"North Carolina" "Caroline du Nord"
;; 	"North Dakota" "Dakota du Nord"
;; 	"Ohio"
;; 	"Oklahoma"
;; 	"Oregon"
;; 	"Pennsylvania" "Pennsylvanie"
;; 	"Rhode Island"
;; 	"South Carolina" "Caroline du Sud"
;; 	"South Dakota" "Dakota du Sud"
;; 	"Tennessee"
;; 	"Texas"
;; 	"Utah"
;; 	"Vermont"
;; 	"Virginia" "Virginie"
;; 	"Washington"
;; 	"West Virginia" "Virginie Occidentale"
;; 	"Wisconsin"
;; 	"Wyoming"
;; 	)))
;;   (defconst naf-mode-state-names
;;     (concat "\\<"
;; 	    (regexp-opt
;; 	     naf-state-names 'paren) "\\>")
;;     "List of States in United States keyword highlighting for naf-mode font-lock faces"  ))
;;; ==============================

;;; ==============================
(require 'naf-mode-state-names)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;; (provide 'naf-mode-state-names)
;;; ==============================

;;; ================================================================
;;; naf-mode-state-names.el ends here
;;; EOF

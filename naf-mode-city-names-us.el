;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-city-names-us.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-city-names-us
;;; US City Name keyword highlighting for use with `naf-mode'
;;; `naf-mode-city-names-us' uses `naf-mode-place-face'.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS: 
;;; `naf-mode-city-names-us'
;;;
;;; VARIABLES:
;;; `*naf-mode-city-names-us-xrefs*'
;;; `*naf-city-names-us*'
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-city-names-us.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T21:02:07-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: Sunday August 09, 2009 @ 08:57.22 AM - by MON KEY>
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
  (defvar *naf-mode-city-names-us-xrefs*
    '(*naf-city-names-us* *naf-mode-city-names-us-xrefs* mon-help-naf-mode-faces)
    "List of symbol names of variables which xref each other in naf-mode-city-names-us
package. :SEE :FILE \"./naf-mode-city-names-us.el\"."))
;;
;;;test-me; *naf-mode-city-names-us-xrefs*
;;
;;;(progn (makunbound '*naf-mode-city-names-us-xrefs*) (unintern '*naf-mode-city-names-us-xrefs*))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-14T18:23:37-04:00Z}#{09381} - by MON>
(eval-and-compile
(defvar *naf-city-names-us*
  '("Aberdeen"
   "Albuquerque"
   "Albany"
   "Anchorage"
   "Atlanta"
   "Baltimore"
   "Baton Rouge"
   "Beverly Hills"
   "Birmingham"
   "Boise"
   "Boston"
   "Bridgeport"
   "Bronx"
   "Brooklyn"
   "Buffalo"
   "Burlington"
   "Cedar Rapids"
   "Chadds Ford"
   "Charleston"
   "Chesapeake"
   "Cheyenne"
   "Chicago"
   "Cincinnati"
   "Cleveland"
   "Columbia"
   "Columbus"
   "Concord"
   "Dallas"
   "Dayton"
   "Denver"
   "Des Moines"
   "Detroit"
   "Fayetteville"
   "Flagstaff"
   "Fort Worth"
   "Gettysburg"
   "Grand Rapids"
   "Green Bay"
   "Greensboro"
   "Hartford"
   "Hollywood"
   "Honolulu"
   "Houston"
   "Indianapolis"
   "Jacksonville"
   "Kansas City"
   "Knoxville"
   "Long Island"
   "Laramie"
   "Las Vegas"
   "Lexington"
   "Little Rock"
   "Los Angeles"
   "Louisville"
   "Manchester"
   "Memphis"
   "Miami"
   "Milwaukee"
   "Minneapolis"
   "Mobile"
   "Nashville"
   "New Haven"
   "New Orleans" 
   "La Nouvelle-Orléans" ;; French Spelling
   "New York City"
   "Newark"
   "Oklahoma City"
   "Omaha"
   "Parkersburg"
   "Pasadena"
   "Philadelphia" 
   "Philadelphie" ;; French Spelling
   "Phoenix"
   "Pittsburgh"
   "Portland"
   "Providence"
   "Reno"
   "Richmond"
   "Roanoke"
   "Rochester"
   "St. Louis"
   "Saint Louis"
   "St. Paul"
   "Salem"
   "Salt Lake City"
   "Sacramento"
   "San Antonio"
   "San Diego"
   "San Francisco"
   "San Jose"
   "Santa Fe"
   "Savannah"
   "Seattle"
   "Shreveport"
   "Sioux Falls"
   "Spokane"
   "Springfield"
   "Tacoma"
   "Tampa"
   "Taos"
   "Tucson"
   "Tulsa"
   "Virginia Beach"
   "Washington D. C."
   "Washington D.C."
   "Wichita"
   "Woodstock"
   "Wilmington"
   "Worcester")
  "*Keyword list of City Names in the United States for `naf-mode' font-locking.\n
NOTE: while there are obv. many more U.S. Cities these have been disambiguated
against other city names and terms identified in other naf-mode keyword lists
and some care has been taken to cull this list in an attempt to prevent false
positives."))
;;
(eval-and-compile 
(defconst naf-mode-city-names-us (concat (regexp-opt *naf-city-names-us* 'paren))))
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-city-names-us* naf-mode-city-names-us 
				   *naf-mode-city-names-us-xrefs* naf-mode-place-fface))
;;
;; (progn (makunbound '*naf-city-names-us*) (unintern '*naf-city-names-us*)
;;       (makunbound 'naf-mode-city-names-us) (unintern 'naf-mode-city-names-us))

;;; ==============================
(provide 'naf-mode-city-names-us)
;;; ==============================

;;; ================================================================
;;; naf-mode-city-names-us.el ends here
;;; EOF

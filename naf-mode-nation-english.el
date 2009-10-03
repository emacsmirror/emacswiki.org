;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-nation-english.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-nation-english keyword lists naton names in Engrish.
;;; Used for font-lock highlighting in `naf-mode'.
;;; 
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `naf-mode-nation-english'
;;;
;;; VARIABLES:
;;; `*naf-nation-english*'
;;; `*naf-mode-nation-english-xrefs*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;; RENAMED:
;;; MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;; This and others need have an interlingua xref'd hashtable.
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-nation.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T20:46:36-04:00Z}#{09406} - by MON>
;;;
;;; FILE-CREATED: Autumn 2007/Winter 2008
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:41:00-04:00Z}#{09327} - by MON>
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
;;; CREATED: <Timestamp: #{2009-09-23T17:15:26-04:00Z}#{09393} - by MON>
(eval-and-compile
(defvar *naf-mode-nation-english-xrefs* 
  '(*naf-nation-english*
    *naf-nation-french*
    *naf-mode-nation-english-xrefs* 
    mon-help-naf-mode-faces)
  "List of symbol names of variables which xref each other in naf-mode-nation-english
package. See FILE: \"./naf-mode-nation-english.el\"."))
;;
;;;test-me; *naf-mode-nation-english-xrefs* 
;;
;;;(progn (makunbound '*naf-mode-nation-english-xrefs* ) (unintern '*naf-mode-nation-english-xrefs* ))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-23T17:40:56-04:00Z}#{09393} - by MON>
(eval-and-compile
(defvar *naf-nation-english*
  '("Afghanistan"
    "Albania"
    "Algeria"
    "America"
    "Andorra"
    "Angola"
    "Anguilla"
    "Antarctica"
    "Antigua"
    "Argentina"
    "Armenia"
    "Aruba"
    "Australia"
    "Austria"
    "Azerbaijan"
    "Bahamas"
    "Bahrain"
    "Bangladesh"
    "Barbados"
    "Barbuda"           ;; co-refs->French
    "Belarus"           ;; co-refs->French
    "Belgium"
    "Belize"
    "Benin"
    "Bermuda"
    "Bhutan"
    "Bolivia"
    "Bosnia"
    "Botswana"
    "Bouvet Island"
    "Brazil"
    "Brunei Darussalam"
    "Bulgaria"
    "Burkina Faso"
    "Burundi"
    "Cambodia"
    "Cameroon"
    "Canada"
    "Cape Verde"
    "Cayman Islands"
    "Central African Republic"
    "Chad"
    "Chile"
    "China"
    "Christmas Island"
    "Colombia"
    "Comoros"
    "Congo"
    "Cook Islands"
    "Costa Rica"
    "Croatia"
    "Cuba"
    "Cyprus"
    "Czech Republic"
    "Denmark"
    "Djibouti"
    "Dominica"
    "Dominican Republic"
    "East Timor"
    "Ecuador"
    "Egypt"
    "El Salvador"
    "England"
    "Equatorial Guinea"
    "Eritrea"
    "Estonia"
    "Ethiopia"
    "Falkland Islands"
    "Faroe Islands"
    "Fiji"
    "Finland"
    "France"
    "French Guiana"
    "French Polynesia"
    "Gabon"
    "Gambia"
    "Georgia"
    "Germany"
    "Ghana"
    "Gibraltar"
    "Great Britain"
    "Greece"
    "Greenland"
    "Grenada"
    "Guadeloupe"
    "Guam"
    "Guatemala"
    "Guinea"
    "Guinea-Bissau"
    "Guyana"
    "Haiti"
    "Herzegovina"
    "Holland"
    "Honduras"
    "Hong Kong"
    "Hungary"
    "Iceland"
    "India"
    "Indonesia"
    "Iran"
    "Iraq"
    "Ireland"
    "Israel"
    "Italy"
    "Ivory Coast"
    "Jamaica"
    "Japan"
    "Jordan"
    "Kazakhstan"
    "Kenya"
    "Kiribati"       ;; co-refs->French
    "Korea"          ;; co-refs->French
    "Kuwait"
    "Kyrgyzstan"
    "Latvia"
    "Lebanon"
    "Lesotho"
    "Liberia"
    "Libya"
    "Liechtenstein"
    "Lithuania"
    "Luxembourg"
    "Macau"
    "Madagascar"
    "Malawi"
    "Malaysia"
    "Maldives"
    "Mali"
    "Malta"
    "Mariana Islands"
    "Martinique"
    "Mauritania"
    "Mauritius"
    "Mayotte"
    "Mexico"
    "Micronesia"
    "Moldova"
    "Monaco"
    "Mongolia"
    "Montserrat"
    "Morocco"
    "Mozambique"
    "Myanmar"
    "Namibia"
    "Nepal"
    "Netherlands Antilles"
    "Netherlands"
    "New Caledonia"
    "New Zealand"
    "Nicaragua"
    "Niger"
    "Nigeria"
    "Niue"                 ;; This is a prefix or what? 
    "Norfolk Island"
    "Norway"
    "Oman"
    "Pakistan"
    "Palau"
    "Panama"
    "Papua New Guinea"
    "Paraguay"
    "Peru"
    "Philippines"
    "Pitcairn"              ;; co-refs->French
    "Poland"
    "Portugal"
    "Puerto Rico"
    "Qatar"                 ;; co-refs->French
    "Romania"               ;; co-refs->French
    "Russia"
    "Rwanda"
    "Saint Helena"          ;; co-refs->French
    "Saint Lucia"           ;; co-refs->French
    "Samoa"
    "San Marino"
    "Saudi Arabia"
    "Scotland"
    "Senegal"
    "Seychelles"
    "Sierra Leone"
    "Singapore"
    "Slovakia"
    "Slovenia"
    "Solomon Islands"
    "Somalia"
    "South Africa"
    "Spain"
    "Sri Lanka"
    "Sudan"
    "Suriname"
    "Swaziland"
    "Sweden"
    "Switzerland"
    "Syria"
    "Taiwan"
    "Tajikistan"
    "Tanzania"
    "Thailand"
    "Tokelau"
    "Tonga"
    "Trinidad and Tobago"
    "Tunisia"
    "Turkey"
    "Turkmenistan"
    "Tuvalu"
    "Uganda"
    "Ukraine"
    "United Arab Emirates"
    "United Kingdom"
    "United States of America"
    "United States"
    "Uruguay"
    "Uzbekistan"                 ;; co-refs->French
    "Vanuatu"
    "Venezuela"
    "Vietnam"
    "Viet Nam"
    "Virgin Islands"
    "Wallis and Futuna"
    "Western Sarara"
    "Yemen"
    "Yugoslavia"
    "Zambia"
    "Zimbabwe")
  "*Keyword list of nation names in English for `naf-mode' font-locking."))
;;
(eval-and-compile 
(defconst naf-mode-nation-english
  (concat "\\<" (regexp-opt *naf-nation-english* 'paren) "\\>"))) ;; Consider removing trailing "\\>"
;;
(eval-and-compile 
  (mon-help-swap-var-doc-const-val *naf-nation-english*  naf-mode-nation-english
                                   *naf-mode-nation-english-xrefs* naf-mode-place-fface))
;;
;; (progn (makunbound '*naf-nation-english*) (unintern '*naf-nation-english*)
;;       (makunbound 'naf-mode-nation-english) (unintern 'naf-mode-nation-english))

;;; ==============================
(provide 'naf-mode-nation-english)
;;; ==============================

;;; ================================================================
;;; naf-mode-nation-english.el ends here
;;; EOF

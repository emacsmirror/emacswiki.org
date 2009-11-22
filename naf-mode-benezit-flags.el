;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-benezit-flags.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-benezit-flags Benezit related terms used in/flagged in `naf-mode'.
;;; fontlocked by: `naf-mode-benezit-face', `naf-mode-benezit-fface'
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS or VARIABLES:
;;; `naf-mode-benezit-section-flag', `naf-mode-benezit-currency-acronym'
;;; `*naf-mode-benezit-currency-alist*'
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-benezit-flags.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:51:50-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:  <Timestamp: Autumn 2008 - by MON KEY>
;;; MODIFICATIONS: <Timestamp: #{2009-08-09T12:11:28-04:00Z}#{09327} - by MON KEY>
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
(provide 'naf-mode-benezit-flags)
;;; ==============================

;;; ==============================
(let ((naf-benezit-section-flag
       (list
        "Musées:"
        "Musées :"
        "Bibliogr.:"
        "Bibliogr. :"
        "Ventes Publiques :"
        "Ventes Publiques:"
        "MUSÉES:"
        "MUSÉES :"
        "BIBLIOGR.:"
        "BIBLIOGR. :"
        "VENTES PUBLIQUES :"
        "VENTES PUBLIQUES:"
        )))
(defconst naf-mode-benezit-section-flag
  (concat "\\<" (regexp-opt naf-benezit-section-flag 'paren) )
  "Keywords for `naf-mode' font-locking sections within a Benezit section of document.
See also; `naf-mode-benezit-face', `naf-mode-benezit-fface',
`naf-mode-benezit-currency-acronym', `*naf-mode-benezit-currency-alist*'."))

;;; =====================================================================
(defconst naf-mode-benezit-currency-acronym
(concat
 ;;  1
 "\\("
 ;; ;2
 "\\(ARS\\|ATS\\|AUD\\|BEF\\|BRL\\|CAD\\|CHF\\|DEM\\|DKK"
 "\\|EGP\\|ESP\\|FRF\\|GBP\\|GRD\\|HKD\\|HUF\\|IEP\\|ILS\\|ITL"
 "\\|JPY\\|NLG\\|PTE\\|SEK\\|SGD\\|TWD\\|USD\\|UYU\\|ZAR"
 "\\)" ;close2
 "[\\[:space:]]"
 ;; 3
 "\\(" ;capture currency
 ;; 4                ;5                ;6
 "\\([0-9]\\{1,3\\}\\([\\[:blank:],]\\([0-9]\\{1,3\\}\\)\\)\\{2\\}\\)"  "\\|"
 ;;  7                              8
 "\\([0-9]\\{1,3\\}[\\[:blank:],]\\([0-9]\\{1,3\\}\\)\\)"  "\\|"
 ;;  9
 "\\(" "[0-9],[0-9]\\{3\\}" "\\|" "[0-9]\\{1,4\\}" "\\)"
 "\\)" ;close 3
 "\\)" ;close 1
  )
"regexp to fontlock Benezit currency abbreviations followed a currency amount.
Currency abbreviations are _mostly_ ISO country name acronyms.
Regexp capture group 2(two) catches the acronyms.
Regexp capture group 3(three) catches the currency amounts.\n
EXAMPLE:\nFRF 500\nUSD 5 000\nUSD 5,000\nGBP 5,000,000\nJPY 50 000 000\n
Currencies caught by the regexp include:\n
ARS -> Peso argentin\nATS -> Schilling autrichien\nAUD -> Dollar australien
BEF -> Franc belge\nBRL -> Real (Brésil)\nCAD -> Dollar canadien
CHF -> Franc suisse\nDEM -> Deutsche Mark\nDKK -> Couronne danoise
EGP -> Livre égyptienne\nESP -> Peseta (Espagne)\nFRF -> Franc français
GBP -> Livre sterling\nGRD -> Drachme (Grèce)\nHKD -> Dollar de Hong Kong
HUF -> Forint (Hongrie)\nIEP -> Livre irlandaise\nILS -> Shekel (Israël)
ITL -> Lire (Italie)\nJPY -> Yen (Japon)\nNLG -> Florin ou Gulden (Pays-Bas)
PTE -> Escudo (Portugal)\nSEK -> Couronne suédoise\nSGD -> Dollar de Singapour
TWD -> Dollar de Taïwan\nUSD -> Dollar américain\nUYU -> Uruguayen Peso
ZAR -> Rand (Afrique du Sud).\n
See also; `*naf-mode-benezit-currency-alist*', `naf-mode-benezit-section-flag'.
Used in `naf-mode'.")

;;;test-me; naf-mode-benezit-currency-acronym
;;;test-me; (regexp-opt-depth naf-mode-benezit-currency-acronym)
;;;test-me;
;;; (progn (search-forward-regexp naf-mode-benezit-currency-acronym)
;;;        (message (concat
;;;                  "Currency Acronym start :%s\n"
;;;                  "Currency Acronym end :%s\n"
;;;                  "Currency Ammount start :%s\n"
;;;                  "Currency Ammount end :%s\n")
;;;                 (match-beginning 2) (match-end 2)
;;;                 (match-beginning 3) (match-end 3)))

;;;UNCOMMENT-TO-TEST:
;;; USD 50 000 000
;;; USD 5,000
;;; USD 500 100 000
;;; USD 5,000
;;; USD 5,000
;;; USD 5 000
;;; USD 5,000,000
;;; USD 5,000
;;; USD 5000
;;; USD 40 000
;;; USD 40,000
;;; USD 1 000 000

;;;(progn (makunbound 'naf-mode-benezit-currency-acronym)
;;;   (unintern 'naf-mode-benezit-currency-acronym))

;;; ==============================
;;; CREATED: <Timestamp: Friday January 09, 2009 @ 01:58.24 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: #{2009-08-22T20:32:48-04:00Z}#{09347} - by MON KEY>
(defvar *naf-mode-benezit-currency-alist*
  '((ARS   Argentina      "ARS"  "Argentine Peso"           "Peso Argentin")           
    (ATS   Austria        "ATS"  "Austrian Schilling"       "Schilling Autrichien")    
    (AUD   Australia      "AUD"  "Australian Dollar"        "Dollar Australien")       
    (BEF   Belgium        "BEF"  "Belgian Franc"            "Franc Belge")             
    (BRL   Brazil         "BRL"  "Brazilian Real"           "Real Brésil")             
    (CAD   Canada         "CAD"  "Canadian Dollar"          "Dollar Canadien")         
    (CHF   Switzerland    "CHF"  "Swiss Franc"              "Franc Suisse")            
    (DEM   Germany        "DEM"  "German Deutsche Mark"     "Deutsche Mark")           
    (DKK   Denmakr        "DKK"  "Danish Kronen"            "Couronne Danoise")        
    (EGP   Egypt          "EGP"  "Egyptian Pound"           "Livre Égyptienne")        
    (ESP   Spain          "ESP"  "Spanish Peseta"           "Peseta Espagne")          
    (FRF   France         "FRF"  "French Franc"             "Franc Français")          
    (GBP   Britain        "GBP"  "British Pounds Sterling"  "Livre sterling Bretagne") 
    (GRD   Greece         "GRD"  "Greek Drakma"             "Drachme Grèce")           
    (HKD   Hong Kong      "HKD"  "Hong Kong Dollar"         "Dollar de Hong Kong")     
    (HUF   Hungary        "HUF"  "Hungarian Forint"         "Forint Hongrie")          
    (IEP   Irelan         "IEP"  "Irish Livre"              "Livre Irlandaise")        
    (ILS   Israel         "ILS"  "Israeli Shekel"           "Shekel Israël")           
    (ITL   Italy          "ITL"  "Italian Lire"             "Lire Italie")             
    (JPY   Japan          "JPY"  "Japanese Yen"             "Yen Japon")               
    (NLG   Netherlands    "NLG"  "Dutch Guilder"            "Florin/Gulden Pays-Bas")  
    (PTE   Portugal       "PTE"  "Portugese Escudo"         "Escudo Portugal")         
    (SEK   Sweden         "SEK"  "Swedish Kronen"           "Couronne Suédoise")       
    (SGD   Singapor       "SGD"  "Singapore Dollar"         "Dollar de Singapour")     
    (TWD   Taiwan         "TWD"  "Taiwanese Dollar"         "Dollar de Taïwan")        
    (USD   United-States  "USD"  "American Dollar"          "Dollar Américain")        
    (UYU   Uruguay        "UYU"  "Uruguayen Peso"           "Peso Uruguayen")          
    (ZAR   South-Africa   "ZAR"  "South African Rand"       "Rand Afrique du Sud"))     
  "*alist of Benezit currencies acronyms.
Acronyms are mapped to ISO Country names, acronym as string, and
Country name as possesive pronoun and currencies common name in both Engrish
and Flench.\n
EXAMPLE:\n(assoc 'USD *naf-mode-benezit-currency-alist*)\n
See also; `naf-mode-benezit-currency-acronym',`naf-mode-benezit-section-flag'.")

;;;test-me;(assoc 'USD *naf-mode-benezit-currency-alist*)
;;;test-me;*naf-mode-benezit-currency-alist*

;;;(progn (makunbound '*naf-mode-benezit-currency-alist*)
;;;   (unintern '*naf-mode-benezit-currency-alist*))

;;; =====================================================================
;;; These are currently living in naf-mode-intnl-city-names.
;;; Benezit auction cities.  Neeed a function to auto re-cap
;; "NEW YORK"
;; "PARIS"
;; "MONTE-CARLO"
;; "LONDRES"
;; "MONACO"
;; "BOSTON"
;; "STOCKHOLM"
;; "COPENHAGUE"
;; "AMSTERDAM"
;; "BRUXELLES"
;; "MUNICH"
;; "BERLIN"
;; "VIENNE"
;; "NEUCHÂTEL"
;; "BÂLE"
;; "GENÈVE"
;; "BERNE"
;; "VERSAILLES"
;; "NEUILLY"
;; "BORDEAUX"
;; "DUBLIN
;;; ==============================

;;; ==============================
(require 'naf-mode-benezit-flags)
;;; ==============================

;;; ================================================================
;;; naf-mode-benezit-flags.el ends here
;;; EOF

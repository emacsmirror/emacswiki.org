;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-french-months.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-french-months
;;; naf-mode-french-months describe the naf-mode-constant font-locked with
;;; naf-mode- -face Catches French Months - optimized.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS or VARIABLES:
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-french-months.el)
;;; FIRST-PUBLISHED: <Timestamp: #{2009-11-21T20:03:03-05:00Z}#{09477} - by MON>
;;;
;;; FILE-CREATED:
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:28:57-04:00Z}#{09327} - by MON KEY>
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
(provide 'naf-mode-french-months)
;;; ==============================

;;; ==============================
(let ((naf-french-months 
       (list "Août" "Avril" "Mai" "Mars" "Juin" "Juillet" "Janvier" "Février"
       "Septembre" "Octobre" "Novembre" "Décembre" "août" "avril" "mai" "mars"
       "juin" "juillet" "janvier" "février" "septembre" "octobre" "novembre"
       "décembre" )))
;;
(defconst naf-mode-french-months 
  (concat "\\<" 
 (regexp-opt naf-french-months 'paren) "\\>")
  "French months font-lock keywords used with `naf-mode'.\n
See also; `mon-defranc-dates', `naf-mode-french-dates', `naf-month-abbrev-alist'."))

;;; (concat
;;;  "[A-Za-z]\(\(\(oût\|vril\|ai\|ars\)\)\|\(\(anv\|évr\)\(ier\)\)\|\(\(cto\|epte\|ove\|éce\)"
;;;     "\(m?+bre\)\)\|\(\(ui\)\(n\|l+et\)\)\)")

;;; ==============================
(require 'naf-mode-french-months)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;; (provide 'naf-mode-french-months)
;;; ==============================

;;; ================================================================
;;; naf-mode-french-months.el ends here
;;; EOF

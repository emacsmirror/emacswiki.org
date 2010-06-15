;;; naf-mode-xrefs.el --- xrefing variable list of corelated naf-mode symbols
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: naf-mode-xrefs.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-10-01T18:34:04-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: data, abbrev, naf-mode

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; naf-mode-xrefs.el provides a variable holding a list of symbol names of
;; variables which xref each other in the in each naf-mode-*.el package. This
;; list is used to put associative docstrings on various constants, variables,
;; functions, etc. going forward. Consider as the xrefing wheel for `naf-mode'.
;;
;; FUNCTIONS:►►►
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;; `*naf-mode-xref-of-xrefs*'
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `*naf-mode-xref-of-xrefs*' <- `naf-mode.el'
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/naf-mode-xrefs.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-10-02T20:39:41-04:00Z}#{09406} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing naf-mode-xrefs. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-10-01T18:34:04-04:00Z}#{09404} - by MON>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2009-2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-14T18:45:10-04:00Z}#{09381} - by MON KEY>
(defvar *naf-mode-xref-of-xrefs*
  '(*naf-mode-art-keywords-xrefs* 
    *naf-mode-institution-xrefs*
    *naf-mode-city-names-us-xrefs*
    *naf-mode-awards-prizes-xrefs*
    *naf-mode-benezit-flags-xrefs*
    *naf-mode-group-period-styles-xrefs*
    ;;*naf-mode-dates-xrefs*
    *naf-mode-date-xrefs*
    *naf-mode-events-xrefs*
    *naf-mode-students-of-julian-xrefs*
    *naf-mode-nation-french-xrefs*
    *naf-mode-nation-english-xrefs*
    *naf-mode-nationality-french-xrefs*
    *naf-mode-nationality-english-xrefs*
    *naf-mode-publications-periodicals-french-xrefs*
    *naf-mode-publications-periodicals-english-xrefs*
    *naf-mode-publications-periodicals-intnl-xrefs*
    *mon-dir-locals-alist-xrefs*)
  "*List of symbol names of variables which xref each other in the
in each naf-mode-*.el package.")
;;
;;; :TEST-ME *naf-mode-xref-of-xrefs*
;;; :TEST-ME (member '*naf-mode-events-xrefs* *naf-mode-xref-of-xrefs*)
;;; :TEST-ME (symbol-value (car (member '*naf-mode-events-xrefs* *naf-mode-xref-of-xrefs*)))
;;
;;;(progn (makunbound '*naf-mode-xref-of-xrefs*) (unintern '*naf-mode-xref-of-xrefs*) )

;;; ==============================
(provide 'naf-mode-xrefs)
;;; ==============================

;;; ====================================================================
;;; naf-mode-xrefs.el ends here
;;; EOF

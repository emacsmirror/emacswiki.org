;;; naf-mode-db-flags.el --- keyword lists and regexps for font-locking in `naf-mode'
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: naf-mode-db-flags.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-04-11T13:28:57-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: naf-mode, applications, matching, i18n, calendar

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; naf-mode-db-flags provides keyword lists and regexps for font-locking in
;; `naf-mode' of generic db 'constants' e.g. words which appear routinely in
;; national DBs but are not headwords and which do not fall under another
;; broader class.
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
;; `naf-mode-db-numbers-flag'    -> Flagging of national db number fields.
;; `*naf-mode-x-of*'             -> Keywords for name flags and x-of type relationships.
;; `naf-mode-timestamp-flag'
;; `naf-mode-accessed-by-flag'
;; `naf-mode-url-flag'
;; `naf-mode-url-wrapper-flag'
;; `naf-mode-db-field-flags-bnf'
;; `naf-mode-db-field-flags'
;; `naf-mode-alternate-name-flags'
;; `*naf-mode-x-of*'
;;
;; FACES:
;;
;; VARIABLES:
;; `*naf-mode-db-flags-xrefs*'
;; `naf-alternate-name-flags'
;; `*naf-x-of*'
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `*regexp-french-date-prefix*'        -> naf-mode-dates.el
;; `*regexp-french-date-siecle*'        -> naf-mode-dates.el
;; `naf-mode-db-field-flags-ulan-paren' -> naf-mode-ulan-utils.el
;; `naf-active-date-flags'              -> naf-mode-dates.el
;; `naf-active-date-flags-paren'        -> naf-mode-dates.el
;; `naf-active-date-flags-solo'         -> naf-mode-dates.el
;; `naf-mode-active-date'               -> naf-mode-dates.el
;; `naf-mode-active-date-flags-solo'    -> naf-mode-dates.el
;; `naf-mode-benezit-date'              -> naf-mode-dates.el
;; 
;; TODO:
;; Following need to be :RENAMED   -> cannonical name
;; `naf-mode-db-field-flags'       -> :RENAME-ME -> -flag
;; `naf-mode-alternate-name-flags' -> :RENAME-ME -> -flag
;; `*naf-mode-x-of*'               -> :RENAME-ME -> -x-of-flag
;;
;; NOTES:
;; This file uses the provide/require idiom because of the defconstant forms.
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/naf-mode-db-flags.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-11-21T20:42:32-05:00Z}#{09477} - by MON>
;; 
;; EMACSWIKI: { URL of an EmacsWiki describing naf-mode-db-flags. }
;;
;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:13:36-04:00Z}#{09327} - by MON>
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-04-11T13:28:57-04:00Z} - by MON KEY>
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

;;; ==============================
(provide 'naf-mode-db-flags)
;;; ==============================

(eval-when-compile (require 'cl))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-14T12:52:09-04:00Z}#{10241} - by MON KEY>
(defvar *naf-mode-db-flags-xrefs* nil 
  "List of  variables which xref each other in naf-mode-db-flags package.\n
:SEE :FILE naf-mode-events.el")
;;
(unless (bound-and-true-p *naf-mode-db-flags-xrefs*)
  (setq *naf-mode-db-flags-xrefs*
        '(naf-mode-timestamp-flag
          naf-mode-accessed-by-flag
          naf-mode-url-wrapper-flag
          naf-mode-url-flag 
          naf-mode-db-numbers-flag
          naf-mode-db-field-flags-bnf
          naf-mode-db-field-flags
          naf-alternate-name-flags
          naf-mode-alternate-name-flags 
          *naf-x-of*
          *naf-mode-x-of*
          )))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 03:44.51 PM  - by MON KEY>
(defconst naf-mode-timestamp-flag 
  (concat "\\(<Timestamp:\\)\\(.*\\)\\( - by "
          (regexp-opt
           (let ((mon-l (number-sequence 1 9))
                 (bug-l (number-sequence 1 7))
                 rtn-l)
             (setq rtn-l '("Ebay"))
             (dolist (i mon-l)
               (setq rtn-l (cons (cadr (assoc i *MON-NAME*)) rtn-l)))
             (dolist (i bug-l)
               (setq rtn-l (cons (cadr (assoc i *BUG-NAME*)) rtn-l)))
             rtn-l)
           'paren)
          ">\\)")
  "*Regexp matches name portion \" - NAME\" of time stamp.\n
Used for fontlocking of timestamps generated with `mon-stamp'.
:EXAMPLE\n\n<Timestamp: Wednesday July 29, 2009 @ 03:32.43 PM  - by MON KEY>\n
:SEE-ALSO `mon-timestamp', `mon-accessed-time-stamp', `mon-accessed-stamp',
`*mon-timestamp-cond*', `naf-mode-accessed-by-flag'.\n
:USED-IN `naf-mode'.\n►►►")
;;
;;;(progn (makunbound 'naf-mode-timestamp-flag) 
;;;       (unintern 'naf-mode-timestamp-flag) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 03:44.51 PM  - by MON KEY>
(defconst naf-mode-accessed-by-flag
  (concat " - "
          (regexp-opt
           (let ((mon-l (number-sequence 1 9))
                 (bug-l (number-sequence 1 7))
                 rtn-l)
             (dolist (i mon-l)
               (setq rtn-l (cons (cadr (assoc i *MON-NAME*)) rtn-l)))
             (dolist (i bug-l)
               (setq rtn-l (cons (cadr (assoc i *BUG-NAME*)) rtn-l)))
             rtn-l)
           'paren))
  "*Regexp to match name portion \" - NAME\" of accessed stamp in `naf-mode'.\n
Used for fontlocking of timestamps generated with `mon-accessed-stamp'.\n
:EXAMPLE\n\n\(let \(gthr-eg\)
  \(save-excursion
    \(dotimes \(i 2\) 
      \(search-forward-regexp naf-mode-accessed-by-flag nil t\)
      \(push \(match-string-no-properties 0\) gthr-eg\)\)
    \(nreverse gthr-eg\)\)\)\n
accessed: Wednesday July 29, 2009 - MON
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!!!\n
accessed: #{2010-04-01T17:35:01-04:00Z}#{10134} - MON KEY
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!!!!!!!\n
:SEE-ALSO `mon-stamp', `mon-timestamp', `mon-accessed-time-stamp',
`*mon-timestamp-cond*', `naf-mode-timestamp-flag'.\n►►►")
;;
;;,---- :UNCOMMENT-TO-TEST
;;| (let ((nmabf (mapconcat 'identity 
;;|                 '("accessed: Wednesday July 29, 2009 - MON"
;;|                   "accessed: #{2010-04-01T17:27:24-04:00Z}#{10134} - MON KEY") "\n")))
;;|   (with-temp-buffer 
;;|     (save-excursion (insert nmabf))
;;|     (dotimes (i 2) (search-forward-regexp naf-mode-accessed-by-flag))
;;|     (when (eq (line-number-at-pos) 2) "BUBBA!")))
;;`----
;;
;;;(progn (makunbound 'naf-mode-accessed-by-flag) 
;;;       (unintern 'naf-mode-accessed-by-flag) )


;;; ==============================
(defconst naf-mode-url-flag 
  (concat "\\(\\(http://\\)\\|\\(www\\.\\)\\)"
          "\\(\\("
          "\\(lccn.loc.gov/.*\\)\\|\\(catalogue.bnf.fr/\\).*\\(/PUBLIC\\)\\)"
          "\\|.*\\(\\."
          "\\(asp\\|aspx\\|cfm\\|com\\|edu\\|html\\|htm\\|gov"
          "\\|jsp\\|org\\|net\\|php\\|xml\\|shtml\\|xhtml\\)"
          "\\)\\)")
  "*Regexp to match URL strings for `naf-mode' font-locking.\n
:NOTE Specific to a limited set of naf-mode related URLs.\n
:FACE-FONT-LOCKING-WITH `naf-mode-field-url-flag-face'.
:FACE-DOCUMENTED-IN `naf-mode-field-url-flag-face'.
:SEE-ALSO `*regexp-wrap-url-schemes*'.\n►►►")
;;
;;; :TEST-ME (search-forward-regexp naf-mode-url-flag)
;;; http://www.bubba.com/bubba.html
;;; http://www.bubba.com/bubba.htm
;;; http://lccn.loc.gov/06005491
;;; http://catalogue.bnf.fr/ark:/12148/ca126560159/PUBLIC
;;; http://catalogue.bnf.fr/
;;; 
;;;(progn (makunbound 'naf-mode-url-flag) (unintern 'naf-mode-url-flag) )

;;; ==============================
;;; :TODO Build regexp for this: "#{2009-08-15T21:01:56-04:00Z}#{09337}"
;;; In naf mode is not getting flagged correctly as spat out by  `mon-accessed-stamp'.
;;;
;;; (defconst naf-mode-time-stamp-ISO-flag nil "")

;;; ==============================
(defconst naf-mode-url-wrapper-flag "^\\(\\((URL `\\)\\(.*\\)\\(')\\)\\)[[:space:]]?$"
  "*Keyword for fontlocking the begining of URL refs in `naf-mode'.
Capture groups \\2{...}\\4 catch one URL on line BOL->EOL with/out traiiling WSP
e.g. \"^(URL {...}')\" _or_ \"^(URL {...}')    \".
:FACE-FONT-LOCKING-WITH `naf-mode-delimit-url-flag-face'
:FACE-DOCUMENTED-IN `naf-mode-delimit-url-flag-fface'.\n
:SEE-ALSO `naf-mode-url-flag',`*regexp-wrap-url-schemes*'.\n►►►")
;;
;;; :TEST-ME (with-temp-buffer 
;;;            (save-excursion (insert "(URL `http://www.google.com')"))
;;;            (if (search-forward-regexp naf-mode-url-wrapper-flag) "BUBBA!"))
;;; 
;;;(progn (makunbound 'naf-mode-url-wrapper-flag) 
;;;       (unintern 'naf-mode-url-wrapper-flag) )

;;; ==============================
;;; :TODO LOC also has this now: nb2007017414
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 03:54.19 PM  - by MON KEY>
(defconst naf-mode-db-numbers-flag
  (concat
   "\\(\\(\\(FRBNF\\)"                        ; <- grp1, grp2, grp3
   ;;^1^^2^^3^^^^^^^^
   "\\|\\(\\(n\\|n\\.\\|no\\|no\\.\\)[ ?]\\)" ; <- grp4 & grp5
   ;;^^^^4^^5^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   "\\|\\(\\[\\)\\)"                    ; <- grp6
   ;;^^^^6^^^^^^^^
   "\\([0-9]\\{8,10\\}\\(]?\\)"         ; <- grp7 & grp8
   ;;^7^^^^^^^^^^^^^^^^^8^^^^^
   "\\)"                                ; <- grp2 :CLOSE
   "\\)"                                ; <- grp1 :CLOSE
   "\\|\\<[0-9]\\{8,10\\}\\>")
  ;; (concat "\\(\\(\\(FRBNF\\)\\|\\(\\(n\\|n\\.\\|no\\|no\\.\\)[ ?]\\)"
  ;;         "\\|\\(\\[\\)\\)\\([0-9]\\{8,10\\}\\(]?\\)\\)\\)"
  ;;         "\\|\\<[0-9]\\{8,10\\}\\>")
  "*Regexps for fontlocking national database UID's.\n
:EXAMPLEn 80126308\nno. 80126308\nn. 80126308\nno 94031775\nn 2005065776\nunk84240548
\[500006383]\nFRBNF12656015\nFRBNF32759170\n
:SEE-ALSO 
:USED-IN `naf-mode'.\n►►►")
;;
;;;(progn (makunbound 'naf-mode-db-numbers-flag) 
;;;       (unintern 'naf-mode-db-numbers-flag) )

;;; ==============================
;;; '"\\(\\(FRBNF\\)\\|\\(\\(n\\|n\\\\.\\|no\\|no\\\\.\\)\\[ \\?]\\)\\|\\(\\\\[\\)\\)\\[0-9]\\{8,10\\}\\(]\\?\\)")
;;; '"\\(\\(FRBNF\\)\\|\\(\\(n\\|n\\.\\|no\\|no\\.\\)\\[ \\?]\\)\\|\\(\\[\\)\\)\\[0-9]\\{8,10\\}\\(]\\?\\)")
;;; :NOTE The following regexes works in query-replace -don't alter it! Make copies.
;;; \(\(FRBNF\)\|\(\(n\|n\.\|no\|no\.\)[ ?]\)\|\(\[\)\)[0-9]\{8,10\}\(]?\)
;;; \(\(\(FRBNF\)\|\(\(n\|n\.\|no\|no\.\)[ ?]\)\|\(\[\)\)\([0-9]\{8,10\}\(]?\)\)\)
;;; ==============================
;;; LOC
;;; n 80126308     8
;;; no. 80126308   8
;;; n. 80126308    8
;;; no 94031775    8
;;; n 2005065776   10
;;; unk84240548    8
;;; [500006383]    9
;;; FRBNF12656015  8
;;; FRBNF32759170  13
;;;
;;; NAFL8084600  ;;ULAN LOC NAF ref
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-10T14:36:43-04:00Z}#{09374} - by MON KEY>
(let ((naf-db-field-flags-bnf
       '("forme internationale"
         ;; ;Mise à jour :" ;;; not catching ':' is not a word const
         "Mise à jour : "  
         ;; "^\\(Sexe : \\(masculin\\|féminin\\)\\)"
         "masculin" "féminin"        
         )))
;;
(defconst naf-mode-db-field-flags-bnf
    (concat "\\<" (regexp-opt naf-db-field-flags-bnf 'paren))
    "*Regexp for font-locking terms which appear in conjunction with BNF
db-field names but occur secondary place in those fields.\n
EXAMPLE:\n\n\(search-forward-regexp naf-mode-db-field-flags-bnf nil t 3\)\)\n
forme internationale\nMise à jour :\nmasculin\nféminin\n
Fontlocking provided by `naf-mode-db-field-entry-bnf-fface'.\n
:SEE-ALSO `naf-db-field-flags'\n:USED-IN `naf-mode'.\n►►►"))
;;
;;;(progn (makunbound 'naf-mode-db-field-flags-bnf)
;;;       (unintern   'naf-mode-db-field-flags-bnf) )

;;; ==============================
;;; `naf-mode-db-field-flags'
;;; :NOTE This list can probably be anchored . highlighter later on...
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 04:04.11 PM  - by MON KEY>
(let ((naf-db-field-flags
       (list  "male"          ;;ULAN => ^\\(Gender: \\(male\\|female\\)\\)"
              "female")))
;;
(defconst naf-mode-db-field-flags
    (concat "\\<" (regexp-opt naf-db-field-flags 'paren) )
"*Regexp for matching for those terms which appear in conjunction with
db-field names but occur secondary place in those fields.\n
:EXAMPLE\n\nforme internationale\nMise à jour :\nmasculin\nféminin\nmale\nfemale\n
:SEE-ALSO \n:USED-IN `naf-mode'.\n►►►"))
;;
;;;(progn (makunbound 'naf-mode-db-field-flags) 
;;;       (unintern 'naf-mode-db-field-flags) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 04:01.11 PM  - by MON KEY>
;;; `naf-mode-alternate-name-flags'
(defvar naf-alternate-name-flags
  '("Also Known As"         "also known as"                   
    "Collective Name"        "collective name"                 
    "Collective Pen Name"    "collective pen name"             
    "Collective Pseudonym"   "collective pseudonym"            
    "Maiden Name"            "family name"                     
    "Nickname"               "maiden name"                     
    "Nom de guerre"          "married name"                    
    "Nom de plume"           "nickname"                        
    "Pen name"               "nom de guerre"                   
    "Pseudonyme"             "nom de plume"                    
    "Psuedonym"              "pen name"                        
    "Real Name"              "pseudonym"                       
    "Stage Name"             "pseudonyme forme internationale" 
    "Surname"                "pseudonyme"                      
    "True Name"              "real name"                       
    "stage name"
    "surname"                         
    "true name")
  "*Keyword list of terms used to designate an identities alternative identifiers.\n
:REGEXPS-IN `naf-mode-alternate-name-flags'
:SEE-ALSO `naf-mode-alternate-name-flags'.\n
:USED-IN `naf-mode'.\n►►►")
;;
(defconst naf-mode-alternate-name-flags 
  (concat "\\<" (regexp-opt naf-alternate-name-flags 'paren))
  "*Regexp for fontlocking keywords which occur in direct association with
an entities authoritative name form.\n
:KEYWORD-LISTS-IN `naf-alternate-name-flags'
:FACE-FONT-LOCKING-WITH `naf-mode-alternate-name-face'.
:FACE-DOCUMENTED-IN `naf-mode-alternate-name-fface'.
Terms typically indicate variant name forms or otherwise help to identify
alternative, pseudo. or pen names.
:SEE-ALSO 
:USED-IN `naf-mode'.\n►►►")
;;
;;;(progn (makunbound 'naf-mode-alternate-name-flags) 
;;        (unintern 'naf-mode-alternate-name-flags) )

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 04:11.23 PM  - by MON KEY>
(defvar *naf-x-of*
  '("Apprentice of"       "apprentice of"
    "Apprentice was"      "apprentice was"
    "Assisted by"         "assisted by"
    "Associate of"        "associate of"
    "Child of"            "child of"
    "Drawings of"         "drawings of"
    "Élève de"            "élève de"
    "Painting of"         "painting of"
    "Paintings of"        "paintings of"
    "Parent of"           "parent of"
    "Partner of"          "partner of"
    "Portrait de"         "portrait de"
    "Portrait of"         "portrait of"
    "Portraits de"        "portraits de"
    "Portraits of"        "portraits of"
    "Sibling of"          "sibling of"
    "Sketches of"         "sketches of"
    "Student of"          "student of"
    "Teacher of"          "teacher of"
    "Teacher was"         "teacher was"
    "Students of"         "students of" 
    "Student was"         "student was"
    "Studied with"        "studied with"
    "Studied under"       "studied under"
    "Élève de Gérôme"     "élève de Gérôme" ;; "Élève de Julian" | l'Académie Julian" | "Académie Julian"
    "Élève de Colorossi"  "élève de Colorossi" ;; Students of Jean Léon Gérôme | "Académie Gérôme"
    "Élève de Julian"     "élève de Julian" ;; Students of Gustave Boulanger | Académie Boulanger 
    "Élève de Bonnat"     "Élève de Bonnat" ;; Léon Bonnat
    )
  "*Keyword list x-of type relationships, for fontlocking in `naf-mode'.
:REGEXPS-IN `*naf-mode-x-of*'
:FACE-FONT-LOCKING-WITH `naf-mode-alternate-name-face'
:FACE-DOCUMENTED-IN `naf-mode-alternate-name-fface'
:SEE-ALSO `*naf-mode-x-of-ulan-bol*', `*naf-mode-ulan-rltd-ppl-corp*'.\n►►►")
;;
(defconst *naf-mode-x-of*
  (concat "\\<" (regexp-opt *naf-x-of* 'paren))
  "*Regexp matching x-of type relationships, for fontlocking in `naf-mode'.
:KEYWORD-LISTS-IN `*naf-x-of*'
:FACE-FONT-LOCKING-WITH `naf-mode-alternate-name-face'
:FACE-DOCUMENTED-IN `naf-mode-alternate-name-fface'
Unlike those identified by the regexps of `*naf-mode-ulan-rltd-ppl-corp*' and
`*naf-mode-x-of-ulan-bol*'.  This regexp triggers only when the relationship is
delimited by whitespace at BOW.  Whereas the keywords of
`*naf-mode-x-of-ulan-bol*' are intended for ULAN keywords at BOL in a headword
postion. Likewise, those of *naf-mode-ulan-rltd-ppl-corp* are identified as
having the capitalized form ':X-OF'\n
:NOTE These are primarily from ULAN.\n
:SEE-ALSO .\n►►►")
;;
;;;(progn (makunbound '*naf-mode-x-of*) (unintern '*naf-mode-x-of*) )

;;; ==============================
(require 'naf-mode-db-flags)
;;; ==============================

;;; ==============================
;;; This file uses the provide/require idiom because of the defconstant forms.
;;;(provide 'naf-mode-db-flags)
;;; ==============================

;;; ==============================
;;; naf-mode-db-flags.el ends here
;;; EOF

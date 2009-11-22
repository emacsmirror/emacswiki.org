;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-db-flags.el
;;; ================================================================
;;; DESCRIPTION:
;;; naf-mode-db-flags provides keyword flags for font-locking in `naf-mode'.
;;; Flagging and font-locking of generic db 'constants' e.g. words which appear
;;; routinely in national DBs but are not headwords and which do not fall
;;; under another broader class.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS:
;;; `naf-mode-db-numbers-flag'    -> Flagging of national db number fields.
;;; `*naf-mode-x-of*'             -> Keywords for name flags and x-of type relationships.
;;; `naf-mode-timestamp-flag'     -> {DOCUMENT ME}
;;; `naf-mode-accessed-by-flag'   -> {DOCUMENT ME}
;;; `naf-mode-url-flag'           -> {DOCUMENT ME}
;;;
;;; VARIABLES:
;;;
;;; MACROS:
;;;
;;; SUBST or ALIASES:
;;;
;;; DEPRECATED, RENAMED, OR MOVED:
;;;
;;; MOVED:
;;; `naf-mode-db-field-flags-ulan-paren' -> ./naf-mode-ulan-utils.el
;;; REQUIRES:
;;;
;;; TODO:
;;; Following need to be renamed -> cannonical name
;;; `naf-mode-db-field-flags'   -> {DOCUMENT-ME} {RENAME-ME -> -flag}
;;; `naf-mode-alternate-name-flags' -> {DOCUMENT-ME} {RENAME-ME -> -flag}
;;; `naf-mode-benezit-date'             -> {DOCUMENT-ME} {RENAME-ME -> -benezit-date-flag}
;;; `*naf-mode-x-of*'                     ->{DOCUMENT ME} {RENAME-ME -> -x-of-flag}
;;;
;;;
;;; NOTES:
;;; This file uses the provide/require idiom because of the defconstant forms.
;;;
;;; SNIPPETS:
;;; naf-mode-CONSTANT-NAME uses naf-mode-FACENAME.
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-db-flags.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-11-21T20:42:32-05:00Z}#{09477} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Saturday April 11, 2009 @ 01:28.57 PM - by MON KEY>
;;; HEADER-ADDED: <Timestamp: #{2009-08-09T12:13:36-04:00Z}#{09327} - by MON KEY>
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
(provide 'naf-mode-db-flags)
;;; ==============================

;;; ==============================
;;; MODIFICATIONS: <Timestamp: Wednesday July 29, 2009 @ 03:44.51 PM  - by MON KEY>
(defconst naf-mode-timestamp-flag
  (concat "\\(<Timestamp:\\)\\(.*\\)\\( - by "
          (regexp-opt
           (let ((mon-l (number-sequence 1 9))
                 (bug-l (number-sequence 1 7))
                 (rtn-l))
             (setq rtn-l '("Ebay"))
             (dolist (i mon-l)
               (setq rtn-l (cons (cadr (assoc i *MON-NAME*)) rtn-l)))
             (dolist (i bug-l)
               (setq rtn-l (cons (cadr (assoc i *BUG-NAME*)) rtn-l)))
             rtn-l)
           'paren)
  ">\\)")
  "Keyword repgexp catches name portion \" - NAME\" of time stamp.
Used for fontlocking of timestamps generated with `mon-stamp'.
EXAMPLE:\n
<Timestamp: Wednesday July 29, 2009 @ 03:32.43 PM  - by MON KEY>\n
See also; `mon-timestamp', `mon-accessed-time-stamp', `mon-accessed-stamp',
`*mon-timestamp-cond-alist*', `naf-mode-accessed-by-flag'.")

;;;test-me; naf-mode-timestamp-flag
;;;(progn (makunbound 'naf-mode-timestamp-flag)(unintern 'naf-mode-timestamp-flag))

          ;; \\("
          ;; (cadr (assoc 1 *MON-NAME*)) "\\|"
          ;; (cadr (assoc 2 *MON-NAME*)) "\\|"
          ;; (cadr (assoc 3 *MON-NAME*)) "\\|"
          ;; (cadr (assoc 4 *MON-NAME*)) "\\|"
          ;; (cadr (assoc 1 *BUG-NAME*)) "\\|"
          ;; (cadr (assoc 2 *BUG-NAME*)) "\\|"
          ;; (cadr (assoc 3 *BUG-NAME*)) "\\|"
          ;; "\\|Ebay\\|BUG\\|Bug\\|MON\\|MON KEY\\)>\\)
;;; ==============================
;;; MODIFICATIONS: <Timestamp: Wednesday July 29, 2009 @ 03:44.51 PM  - by MON KEY>
(defconst naf-mode-accessed-by-flag
(concat " - "
        (regexp-opt
         (let ((mon-l (number-sequence 1 9))
               (bug-l (number-sequence 1 7))
               (rtn-l))
           (dolist (i mon-l)
             (setq rtn-l (cons (cadr (assoc i *MON-NAME*)) rtn-l)))
           (dolist (i bug-l)
             (setq rtn-l (cons (cadr (assoc i *BUG-NAME*)) rtn-l)))
           rtn-l)
         'paren))
  "Keyword repgexp catches name portion \" - NAME\" of accessed stamp.
Used for fontlocking of timestamps generated with `mon-accessed-stamp'.\n
EXAMPLE:\n
accessed: Wednesday July 29, 2009 - MON
.................................!______\n
See also; `mon-stamp', `mon-timestamp', `mon-accessed-time-stamp',
`*mon-timestamp-cond-alist*', `naf-mode-timestamp-flag'.")

;;;test-me; naf-mode-accessed-by-flag
;;;(progn (makunbound 'naf-mode-accessed-by-flag ) (unintern 'naf-mode-accessed-by-flag  ))

;;; ==============================
(defconst naf-mode-url-flag
  '"\\(\\(http://\\)\\|\\(www\\.\\)\\)\\(\\(\\(lccn.loc.gov/.*\\)\\|\\(catalogue.bnf.fr/\\).*\\(/PUBLIC\\)\\)\\|.*\\(\\.\\(asp\\|aspx\\|cfm\\|com\\|edu\\|htm\\|html\\|gov\\|jsp\\|org\\|net\\|php\\|xml\\|shtml\\|xhtml\\)\\)\\)"
  "DBC NAF field specific URL keyword for `naf-mode' font-locking.
Used with `naf-mode-field-url-flag-fface'.")

;;;test-me; naf-mode-url-flag
;;;(progn (makunbound 'naf-mode-url-flag) (unintern 'naf-mode-url-flag))

;;; ==============================
;;; TODO: make a regexp for this "#{2009-08-15T21:01:56-04:00Z}#{09337}"
;;; CREATED: <Timestamp: #{2009-08-15T21:31:20-04:00Z}#{09337} - by MON KEY>
;;; (defconst time-stamp-ISO-flag

;;; ==============================
(defconst naf-mode-url-wrapper-flag
  '"^\\(\\((URL `\\)\\(.*\\)\\(')\\)\\)[[:space:]]?$"
  "Keyword for fontlocking the begining of URL refs in `naf-mode'.
Capture groups \\2{...}\\4 catch one URL on line BOL->EOL with/out traiiling WSP
e.g. \"^(URL {...}')\" _or_ \"^(URL {...}')    \".
Keyword delimited URLs are font-locked with `naf-mode-delimit-url-flag-face'.
:SEE `naf-mode-delimit-url-flag-fface'.")
;;;
;;;test-me:naf-mode-url-flag - http://lccn.loc.gov/06005491
;;;test-me:naf-mode-url-flag - http://catalogue.bnf.fr/ark:/12148/ca126560159/PUBLIC
;;;test-me:naf-mode-url-flag - http://catalogue.bnf.fr/ark:/12148/cb34359034z/PUBLIC
;;;test-me:naf-mode-url-flag - http://catalogue.bnf.fr/

;;;(progn (makunbound 'naf-mode-url-wrapper-flag) (unintern 'naf-mode-url-wrapper-flag))

;;; ==============================
;;; TODO: LOC also has this now: nb2007017414
;;; MODIFICATIONS: <Timestamp: Wednesday July 29, 2009 @ 03:54.19 PM  - by MON KEY>
(defconst naf-mode-db-numbers-flag
'"\\(\\(\\(FRBNF\\)\\|\\(\\(n\\|n\\.\\|no\\|no\\.\\)[ ?]\\)\\|\\(\\[\\)\\)\\([0-9]\\{8,10\\}\\(]?\\)\\)\\)\\|\\<[0-9]\\{8,10\\}\\>"
"Keyword regexps for fontlocking national database UID's.\n\nEXAMPLES:
n 80126308\nno. 80126308\nn. 80126308\nno 94031775\nn 2005065776\nunk84240548
\[500006383]\nFRBNF12656015\nFRBNF32759170")

;;;test-me; naf-mode-db-numbers-flag
;;;(progn (makunbound 'naf-mode-db-numbers-flag) (unintern 'naf-mode-db-numbers-flag))

;;; ==============================
;;; '"\\(\\(FRBNF\\)\\|\\(\\(n\\|n\\\\.\\|no\\|no\\\\.\\)\\[ \\?]\\)\\|\\(\\\\[\\)\\)\\[0-9]\\{8,10\\}\\(]\\?\\)")
;;; '"\\(\\(FRBNF\\)\\|\\(\\(n\\|n\\.\\|no\\|no\\.\\)\\[ \\?]\\)\\|\\(\\[\\)\\)\\[0-9]\\{8,10\\}\\(]\\?\\)")
;;; the following regexes works in query-replace -don't alter it! make copies
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
;;; CREATED: <Timestamp: #{2009-09-10T14:36:43-04:00Z}#{09374} - by MON KEY>
(let ((naf-db-field-flags-bnf
       (list  "forme internationale"
              "Mise à jour : " ;Mise à jour :" ;;; not catching ':' is notword const
              "masculin"
              "féminin"       ;"^\\(Sexe : \\(masculin\\|féminin\\)\\)"
              )))
;;
(defconst naf-mode-db-field-flags-bnf
    (concat "\\<" (regexp-opt naf-db-field-flags-bnf 'paren))
    "*Regexp for fontlocking terms which appear in conjunction with BNF
db-field names but occur secondary place in those fields.\n\nEXAMPLE:
forme internationale\nMise à jour :\nmasculin\nféminin\n
Fontlocking provided by `naf-mode-db-field-entry-bnf-fface'.
See also; `naf-db-field-flags'\nUsed in `naf-mode'."))


;;; ==============================
;;; `naf-mode-db-field-flags'
;;; This list can probably be anchored . highlighter later on...
;;; MODIFICATIONS: <Timestamp: Wednesday July 29, 2009 @ 04:04.11 PM  - by MON KEY>
(let ((naf-db-field-flags
       (list  "male"          ;;ULAN => ^\\(Gender: \\(male\\|female\\)\\)"
              "female")))
;;
(defconst naf-mode-db-field-flags
    (concat "\\<" (regexp-opt naf-db-field-flags 'paren) )
"Provides keyword flagging for those terms which appear in conjunction with
db-field names but occur secondary place in those fields.\n\nEXAMPLE:
forme internationale\nMise à jour :\nmasculin\nféminin\nmale\nfemale\n
Used in `naf-mode' font-lock faces."))

;;;test-me; naf-mode-db-field-flags
;;;(progn (makunbound 'naf-mode-db-field-flags) (unintern 'naf-mode-db-field-flags))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: Wednesday July 29, 2009 @ 04:01.11 PM  - by MON KEY>
;;; `naf-mode-alternate-name-flags'
(let ((naf-alternate-name-flags
      (list "collective name"
            "collective pseudonym"
            "collective pen name"
            "Collective Name"
            "Collective Pseudonym"
            "Collective Pen Name"
            "pseudonyme forme internationale"
            "nickname"
            "Nickname"
            "maiden name"
            "Maiden Name"
            "married name"
            "surname"
            "Surname"
            "Pen name"
            "pen name"
            "nom de plume"
            "nom de guerre"
            "Nom de plume"
            "Nom de guerre"
            "also known as"
            "Also Known As"
            "pseudonyme"
            "Pseudonyme"
            "pseudonym"
            "Psuedonym"
            "stage name"
            "Stage Name"
            "true name"
            "True Name"
            "real name"
            "Real Name"
            "family name")))
;;
(defconst naf-mode-alternate-name-flags
  (concat "\\<" (regexp-opt naf-alternate-name-flags 'paren))
  "Provides keyword regexps for words which occur in direct association with
an entities authoritative name form. Font-locking of these keyword provided by
`naf-mode-alternate-name-face'. Terms typically indicate variant name forms
or otherwise help to identify alternative, pseudo. or pen names.\n\nEXAMPLE:
collective name\ncollective pseudonym\ncollective pen name\nCollective Name
Collective Pseudonym\nCollective Pen Name\npseudonyme forme internationale
nickname\nNickname\nmaiden name\nMaiden Name\nmarried name\nsurname
Surname\nPen name\npen name\nnom de plume\nnom de guerre\nNom de plume
Nom de guerre\nalso known as\nAlso Known As\npseudonyme\nPseudonyme\npseudonym
Psuedonym\nstage name\nStage Name\ntrue name\nTrue Name\nreal name
Real Name\nfamily name\nUsed in `naf-mode' for face font-locking."))


;;;test-me; naf-mode-alternate-name-flags
;;;(progn (makunbound 'naf-mode-alternate-name-flags) (unintern 'naf-mode-alternate-name-flags))

;;; ==============================
;;; TODO:
;;; '"\\(\\(\\(Né\\)\\|\\(Mort\\)\\) le \\)"
;;; '"\\<Né le\\>\\|\\<Mort le\\>"
;;; NEEDS-WORK-AS-OF:
;;; CREATED: <Timestamp: Monday June 23, 2008 @ 12:35.49 PM - by MON KEY>
(defconst naf-mode-benezit-date  '"\\<Né\\>\\|\\<Née\\>\\|\\<Mort\\>"
  "Catches the beginning of the Benezit lifespan string in .naf files
Font-locked by `naf-mode-date-face'See; `naf-mode-date-fface'.\nUsed in `naf-mode'.")

;;;test-me; naf-mode-benezit-date
;;;(progn (makunbound 'naf-mode-benezit-date) (unintern 'naf-mode-benezit-date))

;;; ==============================
;;; `naf-mode-active-date'
(let ((naf-active-date-flags
       (list "Actif en"
             "Actif à"
             "actif en"
             "actif à"
             "active circa"
             "active Circa"
             "active c."
             "active ca."
             "active ca"
             "active cca."
             "Active c."
             "Active ca."
             "Active ca"
             "Active cca.")))
;;
(defconst naf-mode-active-date
  (concat "\\<" (regexp-opt naf-active-date-flags 'paren) )
  "Catches the active period string in `naf-mode'.
Font-locked by `naf-mode-date-face'. See; `naf-mode-date-fface'.\n
EXAMPLE:
Actif en\nActif à\nactif en\nactif à\nactive circa\nactive Circa
active c.\nactive ca.\nactive ca\nactive cca.\nActive c.\nActive ca.\n
Active ca\nActive cca.\n
See also: `naf-mode-active-date-flags-paren', `naf-mode-active-date-flags-solo'"))

;;;test-me; naf-mode-active-date
;;;(progn (makunbound 'naf-mode-active-date) (unintern 'naf-mode-active-date))

;;; ==============================
;;; `naf-mode-active-date-flags-paren'
(let ((naf-active-date-flags-paren
       (list
        "(active circa"
        "(active Circa"
        "(active c."
        "(active ca."
        "(active ca"
        "(active cca."
        "(Active c."
        "(Active ca."
        "(Active ca"
        "(Active cca."
        "(c"
        "(c."
        "(ca."
        "(ca"
        "(cca."
        "(ca."
        "(ca"
        "(cca.")))
;;
(defconst naf-mode-active-date-flags-paren
  (regexp-opt naf-active-date-flags-paren 'paren)
  "Catches the active period string in `naf-mode'.
Font-locked by with NAF-MODE-DATE-FACE. See; `naf-mode-date-fface'.\n
EXAMPLE:
\(active circa\n\(active Circa\n\(active c.\n\(active ca.\n\(active ca
\(active cca.\n\(Active c.\n\(Active ca.\n\(Active ca\n\(Active cca.
\(c\n\(c.\n\(ca.\n\(ca\n\(cca.\n\(ca.\n\(ca\n\(cca.\n
See also: `naf-mode-active-date', `naf-mode-active-date-flags-solo'."))

;;;test-me;
;;;(progn (makunbound 'naf-mode-active-date-flags-paren) (unintern 'naf-mode-active-date-flags-paren))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: Wednesday July 29, 2009 @ 04:12.28 PM  - by MON KEY>
;;; `naf-mode-active-date-flags-solo'
(let ((naf-active-date-flags-solo
       (list " c. "
             " ca. "
             " cca. "
             " ca. "
             " cca. ")))
;;
(defconst naf-mode-active-date-flags-solo
  (regexp-opt naf-active-date-flags-solo 'paren)
  "Catches the active period string in `naf-mode'.
Font-locked by with NAF-MODE-DATE-FACE. See; `naf-mode-date-fface'.\n
EXAMPLE:\n\" c. \"\n\" ca. \"\n\" cca. \"\n\" ca. \"\n\" cca. \"\n
See also; `naf-mode-active-date-flags-paren',`naf-mode-active-date',"))

;;;test-me; naf-mode-active-date-flags-solo
;;;(progn (makunbound 'naf-mode-active-date-flags-solo) (unintern 'naf-mode-active-date-flags-solo))

(defvar *regexp-french-date-prefix*
	'(("Mort en")
	 ("Mort vers")
	 ("Mort le")
	 ("Mort à")
	 ("Né en")
	 ("Né le")
	 ("Né à")
	 ("Née en")
	 ("Née à")
	 ("Née le"))); Century

;;; ==============================
;;; TODO:
;;; (mon-insert-unicode "COMBINING LATIN SMALL LETTER E" t) ;-> ͤ
(defvar *regexp-french-date-siecle*
  '("xix siècle"
    "XIX siècle"
    "xix siècle"
    "xvii-xix siècles"
    "xvii siècle"
    "xviiͤ siècle"
    "XVIIͤ siècle"
    "xixͤ siècle"
    "XIXͤ siècle"
    "xixͤ siècle"
    "xviiͤ-xixͤ siècles"))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: Wednesday July 29, 2009 @ 04:11.23 PM  - by MON KEY>
(let ((naf-x-of
       (list
        "Apprentice of"   "apprentice of"
        "Apprentice was"  "apprentice was"
        "Assisted by"     "assisted by"
        "Associate of"    "associate of"
        "Child of"        "child of"
        "Drawings of"     "drawings of"
	"Élève de"        "élève de"
        "Painting of"     "painting of"
        "Paintings of"    "paintings of"
        "Parent of"       "parent of"
        "Partner of"      "partner of"
        "Portrait de"     "portrait de"
        "Portrait of"     "portrait of"
        "Portraits de"    "portraits de"
        "Portraits of"    "portraits of"
        "Sibling of"      "sibling of"
        "Sketches of"     "sketches of"
        "Student of"      "student of"
        "Teacher of"      "teacher of"
        "Teacher was"     "teacher was"
	"Students of"  	  "students of" 
        "Student was"     "student was"
	"Studied with"    "studied with"
	"Studied under"    "studied under"
	"Élève de Gérôme"     "élève de Gérôme"     ;; "Élève de Julian" | l'Académie Julian" | "Académie Julian"
	"Élève de Colorossi"  "élève de Colorossi"  ;; Students of Jean Léon Gérôme | "Académie Gérôme"
	"Élève de Julian"     "élève de Julian"     ;; Students of Gustave Boulanger | Académie Boulanger 
	"Élève de Bonnat"     "Élève de Bonnat"     ;; Léon Bonnat
	)))
;;
(defconst *naf-mode-x-of*
  (concat "\\<" (regexp-opt naf-x-of 'paren))
  "*Keywords for Fontlocking with `naf-mode-alternate-name-fface' Regexp catches
x-of type relationships, thease are primarily taken from ULAN.  However, unlike
those identified by the regexps of `*naf-mode-ulan-rltd-ppl-corp*' and
`*naf-mode-x-of-ulan-bol*'.  This regexp triggers only when the relationship is
delimited by whitespace at BOW.  Whereas the keywords of
*naf-mode-x-of-ulan-bol* are intended for ULAN keywords at BOL in a headword
postion. Likewise, those of *naf-mode-ulan-rltd-ppl-corp* are identified as
having the capitalized form ':X-OF'
Used in `naf-mode'."))

;;;test-me; *naf-mode-x-of*
;;;(progn (makunbound '*naf-mode-x-of*) (unintern '*naf-mode-x-of*))

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

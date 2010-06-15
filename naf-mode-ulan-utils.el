;;; naf-mode-ulan-utils.el --- utility fncns for converting ULAN data for naf-mode
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: naf-mode-ulan-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-09-01T11:30:04-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: naf-mode, matching, name authority files

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; naf-mode-ulan-utils provides utility fncns for converting ULAN data for
;; `naf-mode' Name Authority Files.
;;
;; FUNCTIONS:►►►
;; `mon-ulan-tsv-assc-rels-type->list', `mon-invert-ulan-triples',
;; `mon-rotate-ulan-triples',
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
;; `naf-mode-ulan-rltd-ppl-corp', `*naf-mode-db-numbers-flag-ulan-loc-naf*',
;; `*naf-mode-x-of-ulan-bol*', `naf-mode-db-field-flags-ulan-paren',
;; `*ulan-associative-roles*', `*ulan-sample-data*',
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `naf-mode-db-field-flags-ulan-paren' <- naf-mode-db-flags.el
;;
;; TODO:
;;
;; NOTES:
;; :SEE (URL `http://www.culture.gouv.fr/culture/inventai/patrimoine/')
;;
;; :SEE (URL `http://jodi.tamu.edu/Articles/v01/i08/Doerr/#Nr.46')
;;
;; Appendix: "&" combinations from the Merimee thesaurus to the AAT
;; Table 3. All "&" combinations from the Merimee thesaurus to the AAT in 1997
;; :SEE (URL `http://jodi.tamu.edu/Articles/v01/i08/Doerr/#Nr.19')
;; :SEE (URL `http://www.culture.gouv.fr/culture/inventai/patrimoine/')
;;
;; :SEE  (URL `http://www.getty.edu/research/conducting_research/vocabularies/training.html')
;; Union List of Artist Names (ULAN): Editorial Guidelines:
;; :SEE (URL `http://www.getty.edu/research/conducting_research/vocabularies/guidelines/ulan_1_contents_intro.html')
;;
;; The majority of ULAN Fields can be presented as derivations of:
;; CDWA (Categories for the Description of Works of Art)
;; :SEE  (URL `http://www.getty.edu/research/conducting_research/standards/cdwa/28person.html')
;; VERSION: Revised 9 June 2009 by Patricia Harpring Murtha Baca and Patricia Harpring, Editors
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;; Regexps of alists contained herein were sourced from publicly accessible 
;; data made available at getty.edu. The digital version of the ULAN is 
;; Copyright © J. Paul Getty Trust.  Code presented or contained of following file
;; does not in any way represent the ULAN, J. Paul Getty Trust, www.getty.edu, nor
;; their associates or affiliates.
;;
;; URL: http://www.emacswiki.org/emacs/naf-mode-ulan-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-11-21T20:19:51-05:00Z}#{09477} - by MON KEY>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing naf-mode-ulan-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-09-01T11:30:04-04:00Z}#{09362} - by MON>
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
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; :NOTE Consider a defvaralias or moving the variables defined in:
;;; :FILE: naf-mode/mon-regexp-symbols.el
;;; `*regexp-ulan-contribs*' `*regexp-clean-ulan-dispatch-chars*'
;;; `*regexp-clean-ulan-fields*' `*regexp-clean-ulan*'
;;; `*regexp-clean-ulan-diacritics*'

(when (locate-library "naf-mode-ulan-help-docs")
  (require 'naf-mode-ulan-help-docs))

;;; ==============================
;;; :NOTE (length "[500006383]") ;=> 11 
;;        (length "500006383");=> 9
;;; :WAS \\([0-9]\\{8,10\\}\\(]?\\)
;;; :CREATED <Timestamp: #{2009-09-12T12:09:02-04:00Z}#{09376} - by MON KEY>
(defconst *naf-mode-db-numbers-flag-ulan* 
  "\\(\\(\\[\\)\\([0-9]\\{8,10\\}\\)\\(]\\)\\)"
  ;;^1^^2^^^^^^^^3^^^^^^^^^^^^^^^^^^^^4^^^^^^
  "*Regexp for font-locking ULAN record ID numbers in `naf-mode'.\n
ID's have the form '[500006383]'.\n
Occurences are font-locked by `naf-mode-db-field-entry-ulan-face'.\n
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (search-forward-regexp *naf-mode-db-numbers-flag-ulan* nil t)
;;   [500006383]
;;; (progn (makunbound '*naf-mode-db-numbers-flag-ulan*)
;;;        (unintern '*naf-mode-db-numbers-flag-ulan*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T12:08:36-04:00Z}#{09376} - by MON KEY>
(defun mon-get-next-ulan-record (&optional without-brackets insertp intrp)
  "Search forward from point for next occurence of a ULAN record ID.
ID's have the form '[500006383]'. 
When WITHOUT-BRACKETS is non-nil or called-interactively with prefix arg
return ID matched without enclosing brackets '[' ']'. 
When called-interactively momentarily display return value:
When INSERTP is non-nil insert return value. Return value as the form:\n
 #{:ULAN-RECORD-START <POSN>}
 #{:ULAN-RECORD-END <POSN>}
 #{:ULAN-RECORD-NUM <INTEGER>}
 \(<POSN> <POSN> <STRING>)\n
:EXAMPLE\n\n(call-interactively 'mon-get-next-ulan-record)\n\n[500006383]\n\n
:SEE-ALSO `*naf-mode-db-numbers-flag-ulan*'.\n►►►"
  (interactive "P\ni\np")
  (let (st-end st-str record-str str-data str-id-int)
    (search-forward-regexp *naf-mode-db-numbers-flag-ulan* nil t)
    ;; "\\(\\(\\[\\)\\([0-9]\\{8,10\\}\\)\\(]\\)\\)"
    ;;  ^^^1^^2^^^^^^^^3^^^^^^^^^^^^^^^^^^^4^^^^^^^       
  (setq st-str (match-beginning 1))
    (setq st-end (match-end 1))
    (setq record-str (if without-brackets 
			(car (mon-string-read-match-string 3))
		       (car (mon-string-read-match-string))))
    (setq str-id-int (when (stringp record-str)
			   (replace-regexp-in-string "\\[" "" record-str))) ;; [ -> char 91
    (setq str-id-int (when (stringp str-id-int)
			   (replace-regexp-in-string "]" "" str-id-int))) ;; ] -> char 93
    (setq str-data (format (concat "#{:ULAN-RECORD-START %s}\n" 
				   "#{:ULAN-RECORD-END %s}\n"
				   "#{:ULAN-RECORD-NUM %s}\n"
				   "%S") ;; (match-data))
			   st-str st-end str-id-int `(,st-str ,st-end ,record-str)))
    (cond (intrp (momentary-string-display (concat "\n" str-data) (point)))
	  (insertp (prin1 str-data (current-buffer)))
	  (t str-data))))
;;
;;; :TEST-ME (call-interactively 'mon-get-next-ulan-record)
;;
;;;(progn (makunbound 'mon-tmp-ulan-record-srch) 
;;;      (unintern 'mon-tmp-ulan-record-srch) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T11:49:08-04:00Z}#{09376} - by MON KEY>
(defconst *naf-mode-db-numbers-flag-ulan-loc-naf* "\\(\\(NAF[LOR]\\)\\(\[0-9]\\{5,13\\}\\)\\)"
  "*Regexp matching LCNAF references in ULAN field entries.\n
:EXAMPLE\nNAFL2001060907\nNAFR8914343\nNAFR907811\n
These are prefixed with the LCNAF flag as:\n
 'LCNAF Library of Congress Name Authority File' as per:
 ULAN-SOURCE-ID: 2100042617
 BRIEF-CITATION: Library of Congress Name Authority Headings
 FULL CITATION: \"Name Authority Headings.\" Library of Congress Authorities
NOTE: ULAN records also conctain references to LOC naf ids as: 'n 88630604'.
This regexp does not match on these.\n
:SEE (URL `http://authorities.loc.gov/')\n
:SEE-ALSO `*naf-mode-db-numbers-flag-ulan*'.\n►►►")

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 29, 2009 @ 03:58.01 PM  - by MON KEY>
(defconst naf-mode-db-field-flags-ulan-paren
  (concat "\\(\\((\\)\\(display\\|index\\|preferred\\|V\\|,\\| \\)+?\\()\\)\\)"
          "\\|\\((\\)\\(inhabited place\\)\\()\\)")
  "*Regexp for keyword fontlocking ULAN flags occuring after name forms.\n
:EXAMPLE:\n\(preferred, index, display, V\)\n\(inhabited place\)\n\(preferred, index, V\)
\(preferred, index\)\n\(preferred\)\n\(display, V\)\n\(display\)\n\(index\)\n\(V\)
:SEE-ALSO `mon-help-naf-mode-ulan-flags'.\n►►►")
;;
;;; :TEST-ME naf-mode-db-field-flags-ulan-paren
;;
;;;(progn (makunbound 'naf-mode-db-field-flags-ulan-paren)
;;        (unintern 'naf-mode-db-field-flags-ulan-paren) )

;;; ==============================
;;; :NOTE regexp to match ulan flags nested in paren don't modify copy!
;;; \(\((\)\(display\|index\|preferred\|V\|,\| \)+?\()\)\)
;;;
;;; (preferred, index, display, V)
;;; (inhabited place)
;;; (preferred, index, V)
;;; (preferred, index)
;;; (preferred)
;;; (display, V)
;;; (display)
;;; (index)
;;; (V)
;;;
;;; tail alternate  of above regexp catches e.g.
;;; \\((\\)\\(inhabited place\\)\\()\\)
;;; \((\)\(inhabited place\)\()\)
;;; catches
;;; (inhabited place)
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T19:36:56-04:00Z}#{09376} - by MON KEY>
(defvar *ulan-sample-data* nil
  "*Path holding ULAN sample data of relational database tables. 
As made available here:
\(URL `http://www.getty.edu/research/conducting_research/vocabularies/ulan/ulan_rel_sample09.zip')
\(URL `http://www.getty.edu/research/conducting_research/vocabularies/download.html')
:SEE-ALSO .\n►►►")
;;
 (unless (and (bound-and-true-p *ulan-sample-data*)
              (not (intern-soft "IS-MON-SYSTEM-P")))
   ;; <Timestamp: #{2010-05-28T19:29:57-04:00Z}#{10215} - by MON KEY>
   ;; :FIXME This is wrong but file-expand-wildcards on my path is not correct.
   ;;   (let (chk-ulan-path 
   (setq *ulan-sample-data*
         (concat *mon-naf-mode-notes* "/ULAN/ulan_rel_utf8_sample09/")))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T20:12:52-04:00Z}#{09377} - by MON KEY>
(defvar *ulan-associative-roles* nil
  "*A list of ULAN associations generated from contents of file
\"ASSOCIATIVE_RELS_TYPE.out\" in pathname held by `*ulan-sample-data*'.
The alist can be inserted into buffer at point with `mon-ulan-tsv-assc-rels-type->list'
:SEE-ALSO .\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T19:04:24-04:00Z}#{09376} - by MON KEY>
(defun mon-rotate-ulan-triples (triple-list)
  "Rotate alist TRIPLE-LIST with elements with form <ROLE-NUM-NUM> to <NUM-ROL-NUM>.\n
Return a TRIPLE-LIST such that:\n \(\"godchild of\" 1575 1574\)\n 
 Becomes\n
 \(1575 \"godchild of\" 1574)\n
:SEE-ALSO `mon-rotate-flatten-list', `mon-rotate-get-rotations-for',
`mon-rotate-next', `mon-rotate-region', `mon-rotate-string'.\n►►►"
  (let ((y '()))
    (mapc #'(lambda (x) 
              (push `(,(cadr x) ,(car x) ,(caddr x)) y))
          triple-list) ;; test-ulan-triple
    y))
;;
;;; :TEST-ME (mon-rotate-ulan-triples test-ulan-triple)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T19:04:21-04:00Z}#{09376} - by MON KEY>
(defun mon-invert-ulan-triples (triple-list)
  "Invert the alist of ULAN triples TRIPLE-LIST.\n
TRIPLE-LIST has the form <ROLE-NUM-NUM> where:
 role1<->role2  role2<->role1 discard keys.\n
:SEE-ALSO `mon-rotate-ulan-triples'.\n►►►"
  (let ((role-n-n triple-list)
	(n-role-n (mon-rotate-ulan-triples triple-list))
	(rtn))
    (mapc #'(lambda (x)
              (let* ((r1-key (caddr x))
                     (r1-v (car x))
                     (r2 (cadr (assoc r1-key n-role-n))))
                (push `(,r1-v ,r2) rtn)))
	  role-n-n)
    rtn))
;;
;;; :TEST-ME (invert-ulan-triples test-ulan-triple)

;;; ==============================
;;; :NOTE Following map to self:
;;
;; sibling by marriage (in-law) of	1551	1551
;; performs with	1306	1306
;; associate of	1302	1302
;; collaborated with	1303	1303
;; possibly identified with	1005	1005
;; step-sibling of	1557	1557
;; fellow student of	1113	1113
;; related to (familial)	1500	1500
;; possibly related to (familial)	1590	1590
;; worked with	1305	1305
;; spouse of	1541	1541
;; friend of	2550	2550
;; administration overlaps with	1413	1413
;; distinguished from	1007	1007
;; formerly identified with	1006	1006
;; sibling of	1501	1501
;; related to	1000	1000
;; cousin of	1521	1521
;; half-sibling of	1556	1556
;; <person to person - teaching/learning>	1100	1100
;; <person to person/person to firm - patronage>	1200	1200
;; <person to firm/group - professional collaboration>	1312	1312
;; <firm/group to firm/group>	1400	1400
;; <person to person - family relationships>	1499	1499
;; <person to person - professional collaboration>	1280	1280
;; <person to person - personal relationship>	2540	2540
;; <person to institution - professional/administrative>	2570	2570

;;; ==============================
;; (concat "^\\([\\[:blank:]]\\(.*\\)\\([\\[:blank:]]\\)"
;;         "\\([0-9]\\{4,4\\}\\)\\([\\[:blank:]]\\)"
;;         "\\([0-9]\\{4,4\\}\\)\\)"))
;;; :CREATED <Timestamp: #{2009-09-12T19:46:42-04:00Z}#{09376} - by MON KEY>
(defun mon-ulan-tsv-assc-rels-type->list (&optional fname)
  "Regexp subr to convert a ULAN list of ULAN Associative relation type triples.\n
FNAME is a path-filename to the file containing TSV formatted tab-separated-values
of relational table data of ULAN associative relation types i.e. somethign like:
\"./ulan_rel_utf8_sample09/ASSOCIATIVE_RELS_TYPE.out\" as made available here:\n
:SEE \(URL `http://www.getty.edu/research/conducting_research/vocabularies/ulan/ulan_rel_sample09.zip')
:SEE \(URL `http://www.getty.edu/research/conducting_research/vocabularies/download.html')\n
:SEE-ALSO `mon-invert-ulan-triples', `*ulan-associative-roles*', `*ulan-sample-data*'.\n►►►"
  (let ((assctv-rels-type 
	 (cond (fname (if (file-readable-p fname) 
			  fname
			(if (file-readable-p (concat *ulan-sample-data* "ASSOCIATIVE_RELS_TYPE.out"))
			    (concat *ulan-sample-data* "ASSOCIATIVE_RELS_TYPE.out")
			  (error (concat ":FUNCTION `mon-ulan-tsv-assc-rels-type->list' "
                                         "-- arg FNAME unreadable or non-existent")))))
	       (t (if (file-readable-p (concat *ulan-sample-data* "ASSOCIATIVE_RELS_TYPE.out"))
		      (concat *ulan-sample-data* "ASSOCIATIVE_RELS_TYPE.out")
		    (error (concat ":FUNCTION `mon-ulan-tsv-assc-rels-type->list' "                     
                                   "-- arg FNAME unreadable or non-existent"))))))
	get-ulan-list)
    (save-excursion
      (setq get-ulan-list
	    (with-temp-buffer
	      (insert-file-contents fname)
	      (goto-char (point-min))
	      (while (search-forward-regexp 
		      (concat		   
		       "^"
		       "\\([\\[:blank:]]\\(.*\\)" ;; grp-2 - relation type
		       "\\([\\[:blank:]]\\)\\([0-9]\\{4,4\\}\\)" ;; grp-4 - role-key1
		       "\\([\\[:blank:]]\\)\\([0-9]\\{4,4\\}\\)" ;; grp-6 - role-key2
		       "\\)") nil t)
		(replace-match "(\"\\2\" \\4 \\6)"))
	      (goto-char (point-min)) 
	      (insert "(setq *ulan-associative-roles* \n'(")
	      (goto-char (point-max))
	      (insert "))")	    
	      (skip-chars-backward "^)")
	      (buffer-string))))
    ;;  (setq get-ulan-list (concat "(setq *ulan-associative-roles*\n" get-ulan-list ")"))  
    (princ (format "\n\n%s" get-ulan-list) (current-buffer))))
;;
;;;(mon-ulan-tsv-assc-rels-type->list 
;;;  (concat *ulan-sample-data* "ASSOCIATIVE_RELS_TYPE.out"))
;;;
;;;(mon-invert-ulan-triples *ulan-associative-roles*)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-01T14:04:05-04:00Z}#{09362} - by MON>
(let ((naf-ulan-rltd-ppl-corp
       (list
        ":APPRENTICE-OF"
        ":APPRENTICE-WAS"
        ":ASSISTED-BY"
        ":ASSOCIATE-OF"
        ":CHILD-OF"
        ":COLLABORATED-WITH"
        ":FOUNDER-OF"
        ":GRANDCHILD-OF"
        ":GRANDPARENT-OF"
        ":GRANDPARENT-WAS"
        ":INFLUENCE"
        ":MEMBER-OF"
        ":PARENT-OF"
        ":PARTNER-OF"
        ":SIBLING-OF"
        ":SPOUSE-OF"
        ":STUDENT-OF"
        ":STUDENT-WAS"
        ":TEACHER-OF"
        ":TEACHER-WAS"
        ":WORKED-WITH")))
;;
(defconst *naf-mode-ulan-rltd-ppl-corp* (regexp-opt naf-ulan-rltd-ppl-corp 'paren)
  "*Keywords for `naf-mode' font-locking with `naf-mode-ulan-ppl-corp-face'\n
:SEE-ALSO `*naf-mode-x-of-ulan-bol*', `*naf-mode-x-of*'.\n
:USED-IN `naf-mode'.\n►►►")) 
;;
;;; :TEST-ME *naf-mode-ulan-rltd-ppl-corp*
;;
;;;(progn (makunbound '*naf-mode-ulan-rltd-ppl-corp*) 
;;;       (unintern '*naf-mode-ulan-rltd-ppl-corp*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-10T16:09:26-04:00Z}#{09374} - by MON KEY>
(let ((naf-x-of-ulan-bol
       (list 	 
	"Apprentice of"   "apprentice of"   	"Apprentice-of"  "apprentice-of" 
	"Apprentice was"  "apprentice was"  	"Apprentice-was" "apprentice-was"
	"Assisted by"	  "assisted by"	    	"Assisted-by"    "assisted-by"	 
	"Associate of"	  "associate of"    	"Associate-of"   "associate-of"	     
	"Child of"	  "child of"	    	"Child-of"       "child-of"	 
	"Drawings of"	  "drawings of"	    	"Drawings-of"    "drawings-of"	 
	"Painting of"	  "painting of"	    	"Painting-of"    "painting-of"	 
	"Paintings of"	  "paintings of"    	"Paintings-of"   "paintings-of"	     
	"Parent of" 	  "parent of" 	    	"Parent-of"      "parent-of"	 
	"Partner of"	  "partner of"	    	"Partner-of"     "partner-of"	 
	"Portrait de"	  "portrait de"	    	"Portrait-de"    "portrait-de"	 
	"Portrait of"	  "portrait of"	    	"Portrait-of"    "portrait-of"	 
	"Portraits de"	  "portraits de"    	"Portraits-de"   "portraits-de"	     
	"Portraits of"	  "portraits of"    	"Portraits-of"   "portraits-of"	     
	"Sibling of"      "sibling of"	    	"Sibling-of"     "sibling-of"	 
	"Sketches of"	  "sketches of"	    	"Sketches-of"    "sketches-of"	 
	"Student of" 	  "student of"	    	"Student-of"     "student-of"	 
	"Teacher of" 	  "teacher of" 	    	"Teacher-of"     "teacher-of"	 
	"Teacher was"     "teacher was"     	"Teacher-was"    "teacher-was"	 
	"Student was"     "student was"     	"Student-was"    "student-was"       
	)))
;;
(defconst *naf-mode-x-of-ulan-bol*
  (concat "^" (regexp-opt naf-x-of-ulan-bol 'paren) "\\>")
  "*Keywords for fontlocking with `naf-mode-ulan-ppl-corp-face' x-of type
relationships primarily from ULAN but where they occor at BOL in a headword w/
or without '-' and/or trailing whitespace. Other ULAN specific keywords flagged
with `*naf-mode-ulan-rltd-ppl-corp*'.  The remainder are caught with
`*naf-mode-x-of*'.\n
:SEE-ALSO .\n:USED-IN `naf-mode'.\n►►►"))

;;; ==============================
(provide 'naf-mode-ulan-utils)
;;; ==============================

;;; ====================================================================
;;; naf-mode-ulan-utils.el ends here
;;; EOF

;;; ==============================
;;; ULAN style citation accessed stamps at EOL. "accessed 16 October 2007"
;;; accessed DD Mmmm YYYY

;;; ==============================
;;; :LCNAF-LIST
;;; \\(\[0-9]\\{5,13\\}\\)
;;; (prin1 (regexp-opt '("NAFL" "NAFR" "NAFO") 'paren) (current-buffer))
;;; (while (search-forward-regexp  "\\(\\(NAF[LOR]\\)\\(\[0-9]\\{5,13\\}\\)\\)" nil t)
;;;       (replace-match "BUBBA"))
;;
;;; :TEST-ME
;;; NAFL2007044963
;;; NAFL77002695
;;; NAFL5016117
;;; NAFR2001051591
;;; NAFR91031147
;;; NAFR8914343
;;; NAFR905364
;;; NAFR94909

;;; ==============================
;;; :TODO Add to ulan replacement list:
;; "partner was/is (firm to person)"
;; "founded by"
;; "assistant of"
;; "partner in"
;; "employee of"
;; "employee was"
;; "great-grandchild of"
;; "court artist to"
;; "patron was"
;; "related to" 
;; "collaborated with"
;; "sibling of"
;; "nephew of"
;; "uncle of"
;; "partner of"
;; "partner in"
;; "child by marriage (in-law) of"
;; "sibling by marriage (in-law) of"
;; "cousin of"
;; "worked with"
;;;	"member of" 	
;;;     "court artist to" 
;;;     "employee was"
;;;     "patron was"
;;      "collaborated with"
;; "partner was/is (firm to person)"
;; "founded by"
;; "assistant of"
;; "partner in"
;; "employee of"
;; "employee was"
;; "great-grandchild of"
;; "court artist to"
;; "patron was"
;; "related to" 
;; "collaborated with"
;; "sibling of"
;; "nephew of"
;; "uncle of"
;; "partner of"
;; "partner in"
;; "child by marriage (in-law) of"
;; "sibling by marriage (in-law) of"
;; "cousin of"
;; "worked with"
;; "apprentice of"
;; "apprentice was"
;; "assisted by"
;; "associate of"
;; "child of"
;; "founder of"
;; "grandchild of"
;; "grandparent of"
;; "grandparent was"
;; "influence"
;; "member of"
;; "parent of"
;; "partner of"
;; "sibling of"
;; "spouse of"
;; "student of"
;; "student was"
;; "teacher of"
;; "teacher was"
;; "worked with"

;;;test-me; *naf-mode-x-of-ulan-bol*
;;
;;;(progn (makunbound '*naf-mode-x-of-ulan-bol*) (unintern '*naf-mode-x-of-ulan-bol*))

;;; ==============================
;;; ULAN SPECIFIC: Head words.
;;; See: `naf-mode-field-names' in "../naf-mode-db-fields.el"
;;;
;;; "Related People and Corporate Bodies:" ;ULAN
;;; "Related People or Corporate Bodies:" ;ULAN
;;; "Sources and Contributors:" ;ULAN
;;; "Record Type:"   ;ULAN
;;; "Birth and Death Places:" ;ULAN
;;; "List/Hierarchical Position:" ;ULAN
;;; "Nationalities:"  ;ULAN
;;; "Gender:" ;ULAN
;;; "ID:" ;ULAN
;;; "Names:" ;ULAN

;;; ==============================
;;; ULAN SHARED: Head words.
;;; See: `naf-mode-field-names' in "../naf-mode-db-fields.el"
;;;
;;; "Biographies:"            ;ULAN 
;;; "Birth and Death Places:" ;ULAN *
;;; "Born:"  ;dbc-field, ULAN
;;; "Died:" ;dbc-field, ;ULAN
;;; "Roles:" ;dbc-field, ;ULAN
;;; "Notes:"
;;; "Subject:" ;ULAN
;;; ==============================

;;; ==============================
;;; NOTE: currently not flagging these subtypes of Events:
;; Events:
;; + "immigration:"
;; + "residence:"

;;; ==============================
;;;
;;; (while (search-forward-regexp
;;;       ;..1...........2....................3
;;;	 "\\([A-z]\\)\\( 	\\.+ \\)\\([A-z ]\\)")
;;;	(replace-match "\\1 - \\3"))
;;;
;;; Top of the AAT hierarchies
;;; Hierarchy of Associated Concepts Facet - Associated Concepts Facet
;;; Hierarchy of Associated Concepts - Associated Concepts
;;; Hierarchy of Physical Attributes Facet - Physical Attributes Facet
;;; Hierarchy of Attributes and Properties - Attributes and Properties
;;; Hierarchy of Conditions and Effects - Conditions and Effects
;;; Hierarchy of Design Elements - Design Elements
;;; Hierarchy of Color - Color
;;; Hierarchy of Styles and Periods Facet - Styles and Periods Facet
;;; Hierarchy of Styles and Periods - Styles and Periods
;;; Hierarchy of Agents Facet - Agents Facet
;;; Hierarchy of People - People
;;; Hierarchy of Organizations - Organizations
;;; Hierarchy of Living Organisms - Living Organisms
;;; Hierarchy of Activities Facet - Activities Facet
;;; Hierarchy of Disciplines - Disciplines
;;; Hierarchy of Functions - Functions
;;; Hierarchy of Events - Events
;;; Hierarchy of Physical and Mental Activities - Physical and Mental Activities
;;; Hierarchy of Processes and Techniques - Processes and Techniques
;;; Hierarchy of Materials Facet - Materials Facet
;;; Hierarchy of Materials - Materials
;;; Hierarchy of Objects Facet - Objects Facet
;;; Hierarchy of Object Groupings and Systems - Object Groupings and Systems
;;; Hierarchy of Object Genres (Hierarchy Name) 	........ Object Genres (Hierarchy Name)
;;; Hierarchy of Components (Hierarchy Name) 	........ Components (Hierarchy Name)
;;; Hierarchy of Built Environment (Hierarchy Name) 	........ Built Environment (Hierarchy Name)
;;; Hierarchy of Furnishings and Equipment - Furnishings and Equipment
;;; Hierarchy of Visual and Verbal Communication - Visual and Verbal Communication

;;; ==============================
;;; Appendix: "&" combinations from the Merimee thesaurus to the AAT
;;; Table 3. All "&" combinations from the Merimee thesaurus to the AAT in 1997
;;; (URL `http://jodi.tamu.edu/Articles/v01/i08/Doerr/#Nr.19')
;;; (URL `http://www.culture.gouv.fr/culture/inventai/patrimoine/')
;;; ==============================
;; Count	French term	Relation	American term combination
;; 1	ACADEMIE	exact equivalence	academy & buildings
;; 2	AIRE DE CONCASSAGE	exact equivalence	crushing & floors
;; 3	AIRE DE LAVAGE	exact equivalence	washing & floors
;; 4	ARCHEVECHE	exact equivalence	bishop (prelate) & palaces
;; 5	ARCHEVECHE	partial equivalence	archbishop & palaces
;; 6	ARDOISIERE	exact equivalence	slate & quarries
;; 7	BASSIN	exact equivalence	artificial & pools
;; 8	BROSSERIE	exact equivalence	brush & factories
;; 9	BUREAU	exact equivalence	factory & offices
;; 10	CABLERIE	exact equivalence	electric cable & factories
;; 11	CALVAIRE	exact equivalence	calvary crosse & monuments
;; 12	CARTONNERIE	exact equivalence	cardboard & factories
;; 13	CHAIRE A PRECHER EXTERIEURE	exact equivalence	exterior & pulpets
;; 14	CHAMBRE DE COMMERCE	exact equivalence	board of trade & buildings
;; 15	CHAMOISERIE	exact equivalence	chamois & factories
;; 16	CHARPENTE EN BOIS	partial equivalence	wood & roofs
;; 17	CHARPENTE METALLIQUE	partial equivalence	metal & roofs
;; 18	CHARRETERIE	exact equivalence	cart & sheds
;; 19	COLLATERAL	exact equivalence	side & aisles
;; 20	CONCIERGERIE	exact equivalence	porter's & lodges
;; 21	CONSERVATOIRE	exact equivalence	drama & schools
;; 22	COURSIERE	exact equivalence	wall & passages
;; 23	COUTELLERIE	exact equivalence	cutlery & factories
;; 24	CRISTALLERIE	exact equivalence	crystal (leadglass) & factories
;; 25	CROIX DE CIMETIERE	exact equivalence	cemetery & crosses
;; 26	DEPENDANCE	exact equivalence	agriculture & outbuildings
;; 27	ECOLE D'AGRICULTURE	exact equivalence	agricultural & schools
;; 28	ECOLE D'ART	exact equivalence	art & schools
;; 29,30	ECOLE DE DANSE	exact equivalence	ballet & schools OR dance & studios (workspaces)
;; 31	<?dicule religieux chr?tien>	exact equivalence	christian &
;; 32	<?difice du g?nie civil>	exact equivalence	civil engineering & buildings
;; 33	EDIFICE RELIGIEUX CHRETIEN	exact equivalence	christian & religious buildings
;; 34	ELEVATION INTERIEURE	exact equivalence	interior & elevations (building divisions)
;; 35	ENCLOS FUNERAIRE	partial equivalence	churchyard & walls
;; 36,37	ENSEMBLE CASTRAL	exact equivalence	castle & complexes OR chateau & complexes
;; 38	ENSEMBLE D'INDUSTRIE ALIMENTAIRE	exact equivalence	food processing plant & complexes
;; 39	ENSEMBLE D'INDUSTRIE CERAMIQUE	exact equivalence	ceramic & complexes
;; 40	ENSEMBLE D'INDUSTRIE CHIMIQUE	exact equivalence	chemical & complexes
;; 41	ENSEMBLE D'INDUSTRIE DU BOIS	exact equivalence	woodworking & complexes
;; 42	ENSEMBLE D'INDUSTRIE DU PAPIER	exact equivalence	papermill & complexes
;; 43	ENSEMBLE D'INDUSTRIE VERRIERE	exact equivalence	glass & complexes
;; 44	ENSEMBLE DE CONSTRUCTION AERONAUTIQUE	exact equivalence	aircraft & complexes
;; 45	ENSEMBLE DE CONSTRUCTION AUTOMOBILE	exact equivalence	motor vehicle & complexes
;; 46	ENSEMBLE DE CONSTRUCTION MECANIQUE	exact equivalence	assembly plant & complexes
;; 47,48	ENSEMBLE DE CONSTRUCTION NAVALE	exact equivalence	shipyard & complexes OR naval shipyard &complexes
;; 49	ENSEMBLE DE FABRICATION DE MATERIAUX DE CONSTRUCTION	exact equivalence	building material & complexes
;; 50	ENSEMBLE DE FABRICATION DES METAUX	partial equivalence	<metalworking plant>& complexes
;; 51	ENSEMBLE DE PETITE METALLURGIE	partial equivalence	machine shop & complexes
;; 52	ENSEMBLE DU GENIE CIVIL	exact equivalence	civil engineering & complexes
;; 53	ENSEMBLE FORTIFIE	exact equivalence	fortification & complexes
;; 54	ENSEMBLE FUNERAIRE	exact equivalence	funerary buildings & complexes
;; 55	ENSEMBLE METALLURGIQUE	partial equivalence	<metalworking plant>& complexes
;; 56	ENSEMBLE TEXTILE	exact equivalence	textile mill & complexes
;; 57	ESCALIER INDEPENDANT	exact equivalence	freestanding & stairs
;; 58	ETABLISSEMENT CONVENTUEL	partial equivalence	christian & religious communities
;; 59	ETABLISSEMENT DE BAINS	exact equivalence	public baths & baths
;; 60	ETABLISSEMENT NAUTIQUE	exact equivalence	boating & clubhouses
;; 61	ETABLISSEMENT PORTUAIRE	exact equivalence	harbor & buildings
;; 62	FAIENCERIE	exact equivalence	faience & factories
;; 63	FECULERIE	exact equivalence	starch & factories
;; 64	<fondations et sols>	exact equivalence	foundations (structural elements) & pavements (surfaceelements)
;; 65	FOUR A CHANVRE	exact equivalence	hemp & ovens
;; 66	FOURNIL	exact equivalence	bake oven & buildings
;; 67	GANTERIE	exact equivalence	glove & factories
;; 68	GARAGE	exact equivalence	automobile & repairshop
;; 69	GAZOMETRE	exact equivalence	natural gas & storage tanks
;; 70	GLACERIE	exact equivalence	mirror & glass & factories
;; 71	HUILERIE	exact equivalence	vegetable oil & animal oil & factories
;; 72	LAC DE JARDIN	exact equivalence	garden & lakes
;; 73	LAVABO DE CLOITRE	exact equivalence	cloister & lavaboes
;; 74	LOCAL SYNDICAL	partial equivalence	trade union & buildings
;; 75	LOGEMENT DE CONTREMAITRE	exact equivalence	foremen's & houses
;; 76	LOGEMENT PATRONAL	exact equivalence	factory & owner's & houses
;; 77	LOGIS ABBATIAL	exact equivalence	abbots' & houses
;; 78	MAISON AUX DIMES	exact equivalence	tithing & offices
;; 79	MAISON MINIATURE	exact equivalence	miniature & houses
;; 80	MILLIAIRE	exact equivalence	Roman & milestones
;; 81	MONTJOIE	partial equivalence	pilgrimage & markers (monuments)
;; 82	OBSERVATOIRE	exact equivalence	astronomical & observatories
;; 83	OUVRAGE D'ART	partial equivalence	civil engineering & structures (single built works)
;; 84	PARFUMERIE	exact equivalence	perfume & factories
;; 85	PASSAGE COUVERT	exact equivalence	carriage & porches
;; 86	PASSAGE D'ENTREE	exact equivalence	carriage & passages
;; 87	PERCEPTION	partial equivalence	tax collectors' & offices
;; 88	PLATRIERE	exact equivalence	plaster & factories
;; 89	PUITS D'AERAGE	exact equivalence	ventilation & shafts (spaces)
;; 90	RAFFINERIE DE PETROLE	exact equivalence	petroleum & refineries
;; 91	RAFFINERIE DE SUCRE	exact equivalence	sugar & refineries
;; 92	ROBINETTERIE	exact equivalence	plumbing hardware & factories
;; 93	SALLE DU THEATRE	exact equivalence	theater & auditoriums
;; 94	SAVONNERIE	exact equivalence	soap (organic material) & factories
;; 95	SECHOIR A CHATAIGNES	exact equivalence	chestnut & drying sheds
;; 96	SECHOIR A MAIS	exact equivalence	corn & drying sheds ;
;; 97	SUCRERIE	exact equivalence	sugar & factories
;; 98	TEMPLE	partial equivalence	protestant & churches
;; 99	TEMPLE PAIEN	partial equivalence	ancient & temples
;; 100	TENNIS	exact equivalence	tennis & courts (builtworks)
;; 101	TONNELLERIE	exact equivalence	barrel (container) & factories
;; 102	TREFILERIE	exact equivalence	wire & factories
;; 103	USINE A GLACE	exact equivalence	ice & factories
;; 104	USINE D'ACIDE SULFURIQUE	exact equivalence	sulfuric acid & factories
;; 105	USINE D'ARMES	exact equivalence	ammunition & factories
;; 106	USINE D'ARMES	partial equivalence	weapon & factories
;; 107	USINE D'ARTICLES EN MATIERE PLASTIQUE	exact equivalence	plastic & hardware & factories
;; 108	USINE D'EBENISTERIE	exact equivalence	cabinetmaking & factories
;; 109	USINE D'ELEMENTS EN MATIERE PLASTIQUE POUR LE BATIMENT	exact equivalence	plastic & building material & factories
;; 110	USINE D'ELEMENTS PREFABRIQUES	partial equivalence	prefabricated & building material & factories
;; 111	USINE D'EMBALLAGE ET CONDITIONNEMENT	exact equivalence	packaging material & factories
;; 112	USINE D'EMBALLAGES EN MATIERE PLASTIQUE	exact equivalence	plastic & packaging material & factories
;; 113	USINE D'EMBOUTISSAGE	exact equivalence	stamping (forming) & factories
;; 114	USINE D'ENCRES	exact equivalence	ink & factories
;; 115	USINE D'ENGRAIS	exact equivalence	fertilizer & factories
;; 116	USINE D'ESTAMPAGE	exact equivalence	cold & stamping (forming) & factories
;; 117	USINE D'HORLOGERIE	exact equivalence	timepiece & factories
;; 118	USINE D'IMPRESSION SUR ETOFFES	exact equivalence	cloth & printing & textile mills
;; 119	USINE D'INSTRUMENTS DE MESURE	exact equivalence	measuring device & factories
;; 120	USINE D'INSTRUMENTS DE MUSIQUE	exact equivalence	musical instrument & factories
;; 121	USINE D'OUATE	exact equivalence	batting & factories
;; 122	USINE D'OUVRAGES EN AMIANTE	exact equivalence	asbestos & factories
;; 123	USINE DE BIMBELOTERIE	partial equivalence	wood & toy (recreational artifact) & factories
;; 124	USINE DE BOISSELLERIE	partial equivalence	turning & factories
;; 125	USINE DE BONNETERIE	exact equivalence	hosiery & factories
;; 126	USINE DE BOUCHONS	exact equivalence	cork (bark) & factories
;; 127	USINE DE BOUGIES	exact equivalence	candle & factories
;; 128	USINE DE BOUTONS	exact equivalence	button (fastener) & factories
;; 129	USINE DE BOYAUDERIE	exact equivalence	gut & factories
;; 130	USINE DE BRODERIE MECANIQUE	exact equivalence	embroidering & factories
;; 131	USINE DE CAOUTCHOUC	exact equivalence	rubber & factories
;; 132	USINE DE CELLULOSE	exact equivalence	cellulose & factories
;; 133	USINE DE CERAMIQUE	exact equivalence	ceramic & factories
;; 134	USINE DE CHAPELLERIE	exact equivalence	hat & factories
;; 135	USINE DE CHAUSSURES	exact equivalence	shoe (footwear) & factories
;; 136	USINE DE CHAUX	exact equivalence	lime & factories
;; 137	USINE DE COLLES	exact equivalence	glue & factories
;; 138	USINE DE CONSTRUCTION AERONAUTIQUE	exact equivalence	aircraft & assembly plants
;; 139	USINE DE CONSTRUCTION AUTOMOBILE	exact equivalence	automobile & assembly plants
;; 140	USINE DE CONSTRUCTION METALLIQUE	exact equivalence	metal & building material & factories
;; 141	USINE DE CONTRE PLAQUE	exact equivalence	plywood & factories
;; 142	USINE DE COSMETIQUES	exact equivalence	cosmetic & factories
;; 143,144	USINE DE CYCLES	exact equivalence	bicycle & factories OR motorcycle & factories
;; 145	USINE DE DECOLLETAGE	partial equivalence	screw & factories
;; 146	USINE DE DENTELLE MECANIQUE	exact equivalence	lace & factories
;; 147	USINE DE DETERGENTS	exact equivalence	detergent & factories
;; 148	USINE DE FABRICATION DE MATERIAUX DE CONSTRUCTION	exact equivalence	building material & factories
;; 149	USINE DE FABRICATION ET DISTILLATION DES GOUDRONS	exact equivalence	tar & refineries
;; 150	USINE DE FERBLANTERIE	exact equivalence	tinware & factories
;; 151	USINE DE FEUTRE	exact equivalence	felt & factories
;; 152	USINE DE FIBRE DE VERRE	exact equivalence	fiberglass & factories
;; 153	USINE DE FIBRES ARTIFICIELLES ET SYNTHETIQUES	exact equivalence	synthetic fiber & factories
;; 154	USINE DE FLACONNAGE	exact equivalence	bottle & factories
;; 155	USINE DE GRES	exact equivalence	stoneware & factories
;; 156	USINE DE MATERIEL AGRICOLE	exact equivalence	agricultural & equipment & factories
;; 157	USINE DE MATERIEL DE TELECOMMUNICATION	exact equivalence	telecommunication & equipment & factories
;; 158	USINE DE MATERIEL ELECTRIQUE INDUSTRIEL	partial equivalence	power producing equipment & factories
;; 159	USINE DE MATERIEL FERROVIAIRE	exact equivalence	railroad car & assembly plants
;; 160	USINE DE MATERIEL FERROVIAIRE	partial equivalence	locomotive & assembly plants
;; 161	USINE DE MATERIEL INFORMATIQUE	exact equivalence	data processing & equipment & assembly plants
;; 162	USINE DE MATERIEL MEDICOCHIRURGICAL	exact equivalence	medical & equipment & factories
;; 163	USINE DE MATERIEL OPTIQUE	exact equivalence	optical instrument & factories
;; 164	USINE DE MATERIEL PHOTO CINEMATOGRAPHIQUE	exact equivalence	photographic equipment & factories
;; 165	USINE DE MATIERES COLORANTES SYNTHETIQUES	exact equivalence	synthetic dye & factories
;; 166	USINE DE MATIERES PLASTIQUES	exact equivalence	plastic & factories
;; 167	USINE DE MENUISERIE	exact equivalence	woodworking & factories
;; 168	USINE DE MEUBLES	exact equivalence	furniture & factories
;; 169	USINE DE PAPIERS PEINTS	exact equivalence	wallpaper & factories
;; 170	USINE DE PARAPLUIES ET CANNES	exact equivalence	cane & factories
;; 171	USINE DE PARAPLUIES ET CANNES	partial equivalence	umbrella & factories
;; 172	USINE DE PASSEMENTERIE	partial equivalence	trimming & factories
;; 173	USINE DE PEINTURES ET VERNIS	exact equivalence	varnish & factories
;; 174	USINE DE PEINTURES ET VERNIS	partial equivalence	paint & factories
;; 175	USINE DE PORCELAINE	exact equivalence	porcelain & factories
;; 176	USINE DE POTERIE	partial equivalence	pottery & factories
;; 177	USINE DE PRODUIT TEXTILE NON TISSE	partial equivalence	felting & factories
;; 178	USINE DE PRODUITS CHIMIQUES	exact equivalence	chemical & factories
;; 179	USINE DE PRODUITS EXPLOSIFS	exact equivalence	explosive & factories
;; 180	USINE DE PRODUITS PHARMACEUTIQUES	exact equivalence	pharmaceutical & factories
;; 181	USINE DE PRODUITS PHOTOGRAPHIQUES ET CINEMATOGRAPHIQUES	exact equivalence	photographic materials & factories
;; 182	USINE DE QUINCAILLERIE	exact equivalence	metal & hardware & factories
;; 183	USINE DE SERRURERIE	exact equivalence	lock (securing device) & factories
;; 184	USINE DE SOUFRE	exact equivalence	sulfur & factories
;; 185	USINE DE TABAC	exact equivalence	tobacco & factories
;; 186	USINE DE TAILLE DE MATERIAUX DE CONSTRUCTION	partial equivalence	stonecutting & factories
;; 187	USINE DE TAILLE DE PIERRE POUR LA JOAILLERIE ET L'INDUSTRIE	exact equivalence	lapidary & factories
;; 188	USINE DE TRAITEMENT DE SURFACE DES METAUX	exact equivalence	plating & factories
;; 189	USINE DE TRANSFORMATION DU LIEGE	exact equivalence	cork (bark) & factories
;; 190	USINE DE VERRE CREUX	exact equivalence	glass & hollow-ware & factories
;; 191	USINE DE VERRE PLAT	exact equivalence	plate glass & factories
;; 192	USINE DE VERRES OPTIQUES	partial equivalence	optical glass & factories
;; 193	USINE LIEE AU TRAVAIL DU BOIS	exact equivalence	woodworking & factories
;; 194	VERRERIE	exact equivalence	glass & factories
;; 195	VERRIERE EN COUVERTURE	exact equivalence	glass & roofs
;; 196	VESTIAIRE D'USINE	exact equivalence	factory & locker rooms
;; 197	VILLA	exact equivalence	ancient & villas
;; 198	VINAIGRERIE	exact equivalence	vinegar & processing plants
;; 199	VOIRIE	exact equivalence	city & streets


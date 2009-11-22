;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-*- mode: EMACS-LISP; fill-column: 9999; -*-
;;; This is naf-mode-faces.el
;;; =====================================================================
;;; `naf-mode-faces' builds the naf faces for use with `naf-mode'.
;;;
;;; FUNCTIONS:►►►
;;; `mon-face-bold>' `naf-facercise-prop-val', `naf-face-prop-val-p',
;;; `naf-map-face-keys>vals',
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS or VARIABLES:
;;; `naf-mode-date-face', `naf-mode-delim-face', `naf-mode-field-face'
;;; `naf-mode-field-face-db-entry', `naf-mode-field-url-flag-face'
;;; `naf-mode-timestamp-face', `naf-mode-accessed-by-face', `naf-mode-benezit-face'
;;; `naf-mode-place-face', `naf-mode-nationality-face', `naf-mode-db-entry-face'
;;; `naf-mode-publication-periodical-face', `naf-mode-event-face'
;;; `naf-mode-group-period-style-face', `naf-mode-institution-face'
;;; `naf-mode-alternate-name-face', `naf-mode-name-divider-face'
;;; `naf-mode-primary-role-face', `naf-mode-secondary-role-face'
;;; `naf-mode-art-keywords-role-face', `naf-mode-role-face-2', `naf-mode-name-face-base'
;;; `naf-mode-artist-face', `naf-mode-author-face', `naf-mode-brand-face'
;;; `naf-mode-people-face', `naf-mode-event-face'
;;; `naf-mode-ulan-ppl-corp-face' `naf-mode-db-field-entry-ulan-face'
;;; `naf-mod-artist-face' `naf-mod-artist-fface'
;;; `naf-mod-artist-student-of-face' `naf-mod-artist-student-of-fface'
;;; `naf-mod-artist-student-of-julian-face'
;;; `naf-mod-artist-student-of-julian-fface'
;;;
;;; MOVED:
;;; `mon-make-bold-normal' <-
;;;
;;; RENAMED:
;;; `mon-make-bold-normal' -> `mon-face-bold>normal'
;;;
;;; TODO:
;;; Xref the facenames with their Constants.
;;;
;;; NOTES:
;;; regexp on faces:
;;; (defface naf-mode-\(.*\)-fface;\(defface naf-mode-\0-face
;;; 'naf-mode-\(.*\)-fface;'naf-mode-\1-face
;;; (defvar naf-mode-\(.*\)-face ';(defvar naf-mode-\1-fface '
;;;
;;; While  not yet fully implemented in `naf-mode' the purpose of the functions:
;;; `naf-facercise-prop-val',`naf-face-prop-val-p', `naf-map-face-keys>vals'
;;; and the variables: `*naf-face-props*', `*naf-face-prop-vals*',
;;; `*naf-face-prop-keywords*', `*naf-face-vals-alist*' is to allow for a way
;;; of putting font-lock-extra-managed-prop using indirection through 
;;; alist lookups. This affords a degree of arbitrary assignment using plists while 
;;; allowing some consistency and verifiability in lieu of an alist lookup prior to
;;; assignment of the property. A plist is an INDISCRIMINATE TRAMP. He'll take any
;;; key/val pair you give him. However, we don't always know in advance which if any
;;; properties we may be wanting to assign at some future point. However, we do
;;; anticipate that these property assignments will use regexps for their
;;; heuristics. Because the face fontlocking in naf-mode are already heavily regexp
;;; based it makes sense to levereage these regexps as a vehicle for carrying
;;; additional text property ``meta-data''.  Regexps are a GIGOLO - he has finite
;;; state, he knows who his clientele are, and when he's on the job when a client
;;; matches his spec he'll fuck anything that moves. What we have then is a
;;; situation where our gigollo regexps are pumping whatever paying client passes by
;;; and once finished passing them off to their Pimp's - in this cases faces using the
;;; font-locking interface. The pimps gather up their $$$ and then having gotten what
;;; they're after proceed to dump the clientele by passing them off to whichever
;;; trampy plist they deem worthy. Our pimps need a way to separate out the
;;; gigollo's clients before they pass them off to just _any_ trampy
;;; plist. Enter the association list - ``THE MATCHMAKER'' She verifys that our
;;; client will have something in common with the wating tramp.
;;; ================================================================
;;;`*naf-face-props*'____
;;;                      |:keywords--> `*naf-face-prop-keywords*'
;;;                      |:values--> :keywords-->`*naf-face-vals-alist*'
;;;`*naf-face-prop-vals*'___________
;;;                                 |:value--> `*naf-face-vals-alist*'
;;;
;;; *naf-face-prop-keywords* ->(car (assoc x *naf-face-vals-alist*) ->(cdr (assoc x *naf-face-vals-alist*))
;;;
;;; ========KEY========  -> =======VAL=======  -> ====ASSOC-VAL->*naf-face-vals-alist*============
;;; :naf-field-type-key     naf-field-type-val   (dbc-field ulan-field loc-field bnf-field oclc-field ebay-field)
;;; :naf-field-value-key    naf-field-value-val  (bnf-entry ulan-entry loc-entry oclc-entry ebay-entry wiki-entry)
;;; :naf-field-dbc-key      naf-field-dbc-val     nil
;;; :naf-field-ulan-key     naf-field-ulan-val    nil
;;; :naf-field-bnf-key      naf-field-bnf-val     nil
;;; :naf-field-oclc-key     naf-field-oclc-val    nil
;;; :naf-field-loc-key      naf-field-loc-val     nil
;;; :naf-field-ebay-key     naf-field-ebay-val    nil
;;; :naf-field-wiki-key     naf-field-wiki-val   (wiki-ref)
;;; :naf-url-key            naf-url-val          (delim-url dbc-url wiki-url ref-url bnf-url internet-url)
;;; :naf-date-key           naf-date-val         (circa active event lifespan timestamp birth death)
;;; :naf-ref-type-key       naf-ref-type-val     (artist-naf author-naf people-naf)
;;; :naf-ref-benezit-key    naf-ref-benezit-val  (benezit-auction benezit-citation benezit-musuem)
;;;
;;; NOTE: <Timestamp: #{2009-08-22T14:45:24-04:00Z}#{09346} - by MON KEY>
;;; Changed the lookup naming scheme to reflect reality.
;;; The first elt ':naf-*-" is now suffixed with 'key' second is suffixed '-val'
;;; ================================================================
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-mode-faces.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-09-20} - by MON KEY>
;;; 
;;; FILE-CREATED:
;;; <Timestamp: Wednesday April 08, 2009 @ 12:38.03 PM - by MON KEY>
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
;;; (require 'naf-mode-faces)
;;; (provide 'naf-mode-faces)
;;; ==============================

;;; ==============================
;;; CREATED: <Timestamp: Tuesday June 23, 2009 @ 10:51.15 AM - by MON KEY>
(defun mon-face-bold>normal ()
"For all faces map face-atrribute :weight from bold > normal."
 (mapc (lambda (face)
         (when (eq (face-attribute face :weight) 'bold)
           (set-face-attribute face nil :weight 'normal)))
       (face-list)))

;;; ==============================
;;; CREATED: <Timestamp: Thursday April 16, 2009 @ 11:53.36 AM - by MON KEY>
(defun naf-facercise-prop-val (fc-prp fc-prp-val)
  "For assigning additional text-properties to `naf-mode' specific faces.
car of the elts of FC-PRP are property keywords for `font-lock-extra-managed-props'.
The cdr of the elts of FC-PRP are assoc keys of elts in second alist FC-PRP-VAL.
The cdr of the elts of FC-PRP-VAL is a list of 'properties' which
`naf-face-prop-val-p' tests as assignable to the keywords in FC-PRP."
  (let ((fp fc-prp)
        (fpv fc-prp-val)
        (put-back))
    (setq put-back `(,(mapcar 'car fp)))
    (let* ((attach-val (mapcar 'cdr fp))
           (prop-cb (mapcar 'identity fpv))
           (out-vals))
      (while attach-val
        (push `(,(cdr (rassoc (car attach-val) fp)) . (,(car prop-cb))) out-vals)
        (pop attach-val)
        (pop prop-cb))
      (add-to-list 'put-back `(,@(nreverse out-vals)))
      put-back)))
;;
;;; :TEST-ME (prin1 (naf-facercise-prop-val *naf-face-props* *naf-face-prop-vals*) (current-buffer))
;;; :TEST-ME (prin1 (cadr (naf-facercise-prop-val *naf-face-props* *naf-face-prop-vals*)) (current-buffer) )
;;; :TEST-ME (prin1 (cdar (naf-facercise-prop-val *naf-face-props* *naf-face-prop-vals*)) (current-buffer) )
;;; :TEST-ME (setq face-testing (cdar (naf-facercise-prop-val *naf-face-props* *naf-face-prop-vals*)))
;;; :TEST-ME (assoc 'naf-field-value-type-val face-testing)
;;; :TEST-ME (identity (cadr (assoc 'naf-field-value-type-val face-testing)))
;;; :TEST-ME (identity (cadr (assoc 'naf-field-type-val face-testing)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday April 16, 2009 @ 11:48.18 AM - by MON>
;;; :MODIFICATIONS <Timestamp: Monday June 08, 2009 @ 09:04.02 PM - by MON>
(defun naf-face-prop-val-p (property-key prop-val)
  "Testing function used to validate text-property plist keywords.
Return a 3\(three\) elt list  for `naf-mode' faces when binding local-variables
with `font-lock-extra-managed-props'. Checks a keywords value is acceptable
before adding additional face specific \(property . val\) pairs for use by
the `naf-font-lock-keywords' constant.\n
car is non-nil if PROPERTY-KEY is t
cadr is non-nil if PROP-VAL is t
caddr is non-nil if PROPERTY-KEY is t. It is the assoc value from `*naf-face-props*'
This last elt is useful for examining putting other elements of `*naf-face-vals-alist*'
should PROP-VAL return nil.\n\n:EXAMPLE\n
\(naf-face-prop-val-p :naf-field-value-type 'ebay-entry\)
=>\(:naf-field-value-type ebay-entry naf-field-value-type-val\)\)\n
:SEE-ALSO `naf-map-face-keys>vals', `*naf-face-vals-alist*',
`*naf-face-prop-keywords*', `*naf-face-prop-vals*', `*naf-face-keyword>vals*'."
  (let* ((key-look (assoc property-key *naf-face-keyword>vals*))
         (key-> (cdr (assoc property-key *naf-face-props*)))
         (val-look (member prop-val key-look))
         (the-val (car val-look))
         (key-pair))
    (if key-look
        (setq key-pair (cons (car key-look)key-pair))
      (setq key-pair (cons nil key-pair)))
    (if (and (car key-pair) val-look)
        (setq key-pair (cons the-val key-pair))
      (setq key-pair (cons nil key-pair)))
    (setq key-pair (cons key-> key-pair))
    (nreverse key-pair)))
;;
;;; :TEST-ME (naf-face-prop-val-p :naf-field-value-type 'ebay-entry)
;;; :TEST-ME (naf-face-prop-val-p :naf-field-value-mtype 'ebay-entrym)
;;; :TEST-ME (naf-face-prop-val-p :naf-field-value-mtype 'ebay-entry)
;;; :TEST-ME (naf-face-prop-val-p :naf-field-value-type 'ebay-entrym)
;;
;;; :TEST-ME *naf-face-vals-alist*
;;; :TEST-ME *naf-face-keyword>vals*
;;; :TEST-ME (assoc :naf-ref-benezit *naf-face-keyword>vals*)
;;; :TEST-ME (rassq 'naf-field-value-type-val *naf-face-props*)
;;; :TEST-ME (assoc :naf-field-value-type *naf-face-props*)
;;; :TEST-ME (assoc 'naf-field-value-type-val *naf-face-vals-alist*)

;;; ==============================
(defun naf-map-face-keys>vals ()
  "Map keywords stored in `*naf-face-prop-keywords*' to the value cell in
`*naf-face-vals-alist*'. Upon first glance this extra indirection might appear
un-needed. The indirection lets us verify/change the values in
`*naf-face-prop-vals*' independent of they key<->value pairs in
`*naf-face-keyword>vals*'.

`*naf-face-props*'____
                      |:keywords--> `*naf-face-prop-keywords*'
                      |:values--> :keywords-->`*naf-face-vals-alist*'
`*naf-face-prop-vals*'___________
                                 |:values--> `*naf-face-vals-alist*'

`*naf-face-prop-keywords*' :keywords__
                                      \\_`*naf-face-keyword>vals*'
    `*naf-face-prop-vals*' :values____/

:SEE-ALSO `naf-facercise-prop-val', `naf-face-prop-val-p'."
  (let ((three *naf-face-prop-keywords*)
        (newp))
    (while three
      (let* ((oldp (car three))
             (kw oldp)
             (val-lkup (cadr (assoc kw *naf-face-props*)))
             (new-cell))
        (setq new-cell (cons kw (cadr (assoc val-lkup *naf-face-vals-alist*))))
        (setq newp  (cons new-cell newp)))
      (setq three (cdr three)))
    newp))
;;
;;; :TEST-ME (member 'people-naf (cdr (assoc :naf-ref-type (naf-map-face-keys>vals))))
;;; :TEST-ME (member 'people-naf (cdr (assoc :naf-ref-type (naf-map-face-keys>vals))))

;;; ==============================
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 12:49.58 PM - by MON>
;;; Is this right???
;;; I Changed these to global vars:
;;; `*naf-face-props*', `*naf-face-prop-vals*',
;;; `*naf-face-prop-keywords*', `*naf-face-vals-alist*'
;;; ==============================

;;; ==============================
;;; (def|const/var| *naf-face-props*
;;; (setq *naf-face-props* 'nil)
(eval-when-compile
(defvar *naf-face-props* nil
  "*Values cell in alist maps keywords to _their_ in `*naf-face-vals-alist*'.
:SEE-ALSO `naf-facercise-prop-val', `naf-face-prop-val-p',
`naf-map-face-keys>vals', `*naf-face-prop-keywords*', `*naf-face-prop-vals*',
`*naf-face-keyword>vals*'."))
;;
(eval-when-compile
(when (not (bound-and-true-p *naf-face-props*))
  (setq *naf-face-props*
        ;;LEAVE THIS AS A DOTTED PAIR or you break `naf-facercise-prop-val'!!!
        `((:naf-field-type-key    . naf-field-type-val)
          (:naf-field-value-key   . naf-field-value-val)
          (:naf-field-dbc-key     . naf-field-dbc-val)
          (:naf-field-ulan-key    . naf-field-ulan-val)
          (:naf-field-bnf-key     . naf-field-bnf-val)
          (:naf-field-oclc-key    . naf-field-oclc-val)
          (:naf-field-loc-key     . naf-field-loc-val)
          (:naf-field-ebay-key    . naf-field-ebay-val)
          (:naf-field-wiki-key    . naf-field-wiki-val)
          (:naf-url-key           . naf-url-val)
          (:naf-date-key          . naf-date-val)
          (:naf-ref-type-key      . naf-ref-type-val)
          (:naf-ref-benezit-key   . naf-ref-benezit-val)))))
;;
;;; :TEST-ME 
;;;(assoc (nth 1 *naf-face-prop-keywords*)
;;;        (naf-field-value-val (bnf-entry ulan-entry loc-entry oclc-entry ebay-entry wiki-entry))
;;;        *naf-face-props*) ;=>(:naf-field-type-key . naf-field-type-val)
;;;
;;; :TEST-ME (cadr (assoc (nth 0 *naf-face-prop-keywords*) *naf-face-props*))

;;; :TEST-ME  *naf-face-props*
;;;(progn (makunbound '*naf-face-props*) (unintern '*naf-face-props*))

;;; ==============================
;;(def|const/var| naf-face-prop-vals
;(setq *naf-face-prop-vals*)
(eval-when-compile
(defvar *naf-face-prop-vals* nil
  "*List holding the values cells in `*naf-face-vals-alist*'.
:SEE-ALSO `naf-facercise-prop-val', `naf-face-prop-val-p',
`naf-map-face-keys>vals' `*naf-face-props*'
`*naf-face-prop-keywords*', `*naf-face-keyword>vals*'."))
;;
(eval-when-compile
  (when (not (bound-and-true-p *naf-face-prop-vals*))
    (setq *naf-face-prop-vals*
 `(;; :naf-field-key <- naf-field-type-val ->
   (dbc-field ulan-field loc-field bnf-field oclc-field ebay-field)
   ;; :naf-field-value-key <- naf-field-value-val
   (bnf-entry ulan-entry loc-entry oclc-entry ebay-entry wiki-entry)
   ;; :naf-field-dbc-key <- naf-field-dbc-val ->
   ()
   ;; :naf-field-ulan-key <- naf-field-ulan-val ->
   ()
   ;; :naf-field-bnf-key <- naf-field-bnf-val ->
   ()
   ;; :naf-field-oclc-key <- naf-field-oclc-val ->
   ()
   ;; :naf-field-loc-key <- naf-field-loc-val ->
   ()
   ;; :naf-field-ebay-key <- naf-field-ebay-val ->
   ()
   ;; :naf-field-wiki-key <- naf-field-wiki-val ->
   (wiki-ref)
   ;; :naf-url-key <- naf-url-val ->
   (delim-url dbc-url wiki-url ref-url bnf-url internet-url)
   ;; :naf-date-key <- naf-date-val ->
   (circa active event lifespan timestamp birth death)
   ;; :naf-ref-type-key <- naf-ref-type-val ->
   (artist-naf author-naf people-naf)
   ;; :naf-ref-benezit-key <- naf-ref-benezit-val ->
   (benezit-auction benezit-citation benezit-musuem benezit-currency)))))
;;
;;; :TEST-ME  *naf-face-prop-vals*
;;;(progn (makunbound '*naf-face-prop-vals*) (unintern '*naf-face-prop-vals*))

;;; ==============================
;;;(def|const/var| naf-face-prop-keywords
;;(setq *naf-face-prop-keywords* 'nil)
(eval-when-compile
(defvar *naf-face-prop-keywords* nil
  "*list of keywords used in alist lookup of key-value pairs in `*naf-face-vals-alist*'
the values these keys map to are held in `*naf-face-prop-vals*'
:SEE-ALSO `naf-facercise-prop-val', `naf-face-prop-val-p', `naf-map-face-keys>vals'
`*naf-face-props*', `*naf-face-keyword>vals*'."))
;;
(eval-when-compile
(when (not (bound-and-true-p *naf-face-prop-keywords*))
  (setq *naf-face-prop-keywords*
        (cdar (naf-facercise-prop-val *naf-face-props* *naf-face-prop-vals*)))))
;;
;;; :TEST-ME  *naf-face-prop-keywords*
;;;(progn (makunbound '*naf-face-prop-keywords*)
;;;   (unintern '*naf-face-prop-keywords*))

;;; ==============================
;;;(def|const/var| naf-face-vals-alist
;;(setq *naf-face-vals-alist* 'nil)
(eval-when-compile
(defvar *naf-face-vals-alist* nil
  "*alist holding key lookup mappings associating `*naf-face-prop-keywords*'
with `*naf-face-prop-vals*'. This association is made through an indirection with
`naf-facercise-prop-val' allowing for the actual keys in this table to be
changed independently of the keywords.
:SEE-ALSO `naf-face-prop-val-p', `naf-map-face-keys>vals',`*naf-face-props*',
`*naf-face-vals-alist*',`*naf-face-keyword>vals*'."))
;;
(eval-when-compile
(when (not (bound-and-true-p *naf-face-vals-alist*))
  (setq *naf-face-vals-alist*
          (cdar (naf-facercise-prop-val *naf-face-props* *naf-face-prop-vals*))
          )))

;;; :TEST-ME (mapcar '(lambda (x) (progn (newline) (prin1  (car x) (current-buffer)))) *naf-face-vals-alist*)
;;; :TEST-ME 
;;;(mapc '(lambda (x) (progn (newline) (prin1 (assoc x  *naf-face-vals-alist*) (current-buffer))))
;;;      (mapcar '(lambda (x) (progn (newline) (prin1  (car x) (current-buffer)))) *naf-face-vals-alist*))
;;; :TEST-ME 
;;; (let* ((gathr-key (mapcar '(lambda (x) (car x)) *naf-face-vals-alist*))
;;;       (gathr-key-v (mapcar '(lambda (x) (cadr (assoc x *naf-face-vals-alist*))) gathr-key)))
;;;   `(,gathr-key ,gathr-key-v))
;;; :TEST-ME  *naf-face-vals-alist*
;;
;;;(progn (makunbound '*naf-face-vals-alist*) (unintern '*naf-face-vals-alist*))

;;; ==============================
(eval-when-compile
(defvar *naf-face-keyword>vals* nil
  "*alist with 1:1 mappings of `*naf-face-prop-keywords*'->`*naf-face-prop-vals*'
`naf-facercise-prop-val', `naf-face-prop-val-p', `naf-map-face-keys>vals'
`*naf-face-props*', `*naf-face-vals-alist*'."))
;;
(eval-when-compile
(when (not (bound-and-true-p *naf-face-keyword>vals*))
  (setq *naf-face-keyword>vals* (naf-map-face-keys>vals))))
;;
;;; :TEST-ME (member 'people-naf (cdr (assoc :naf-ref-type *naf-face-keyword>vals*)))
;;;(progn (makunbound '*naf-face-keyword>vals*) (unintern '*naf-face-keyword>vals*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-10T17:22:35-04:00Z}#{09374} - by MON>
(defun mon-help-naf-mode-faces (&optional insertp intrp)
  "Mapping of face to the var/constant holding fontlock regexp
trailing * e.g. '* indicates the face/var/constant has had docstring xrefd.\n
==================================================
Following are standard naf-mode template keywords:
==================================================\n
,---- (describe-face 'naf-mode-db-entry-face)
|
| `naf-mode-db-entry-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-db-entry'
|
`----\n
,---- (describe-face 'naf-mode-timestamp-face)
| 
| `naf-mode-timestamp-fface'*        
|  |  :KEYWORD-REGEXPS-IN
|  | `naf-mode-timestamp-flag'
|  | 
|  |--+ (describe-face 'naf-mode-accessed-by-face)
|    `naf-mode-accessed-by-fface'*
|     :KEYWORD-REGEXPS-IN
|    `naf-mode-accessed-by-flag'
|
`----\n
,---- (describe-face 'naf-mode-delim-face)
|
| `naf-mode-delim-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-delim'
| `naf-mode-comment-delim'
|
`----\n
,---- (describe-face 'naf-mode-field-url-flag-face)
|
| `naf-mode-field-url-flag-fface'*
|  | :KEYWORD-REGEXPS-IN
|  | `naf-mode-timestamp-flag'
|  | `naf-mode-url-flag'
|  | 
|  |--+ (describe-face 'naf-mode-delimit-url-flag-face)
|    `naf-mode-delimit-url-flag-fface'*
|     :KEYWORD-REGEXPS-IN
|    `naf-mode-url-wrapper-flag'
|
`----\n
,---- (describe-face 'naf-mode-name-divider-face)
| 
| `naf-mode-name-divider-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-name-divider'
|
`----\n
===========================
:BASE-FACES-WITH-INHERITORS
===========================\n
,---- (describe-face 'naf-mode-field-face)
| 
|  `naf-mode-field-fface'*
|   |  :KEYWORD-REGEXPS-IN
|   | `naf-mode-field-names'.
|   | 
|   |--+ (describe-face 'naf-mode-field-bnf-face)
|   | `naf-mode-field-bnf-fface'*
|   |  :KEYWORD-REGEXPS-IN
|   | `naf-mode-field-names-bnf'
|   | 
|   |--+ (describe-face 'naf-mode-field-ulan-face)
|     `naf-mode-field-ulan-fface'*
|      | :KEYWORD-REGEXPS-IN 
|      | 
|      | 
|      |--+ (describe-face 'naf-mode-ulan-ppl-corp-face)
|        `naf-mode-ulan-ppl-corp-fface'*
|         :KEYWORD-REGEXPS-IN
|        `*naf-mode-ulan-rltd-ppl-corp*'
|        `*naf-mode-x-of-ulan-bol*'
|
`----\n
,---- (describe-face 'naf-mode-db-field-entry-face)
| 
|  `naf-mode-db-field-entry-fface'*    
|   |  :KEYWORD-REGEXPS-IN
|   | `naf-mode-db-numbers-flag'
|   | 
|   |--+ (describe-face 'naf-mode-db-field-entry-ulan-face)
|   | `naf-mode-db-field-entry-ulan-fface'*
|   |  :KEYWORD-REGEXPS-IN
|   | `naf-mode-db-field-flags-ulan-paren' 
|   |
|   ---+ (describe-face 'naf-mode-db-field-entry-bnf-face)
|     `naf-mode-db-field-entry-bnf-fface'*  
|      :KEYWORD-REGEXPS-IN
|     `naf-mode-db-field-flags-bnf'      
|
`----\n
,---- (describe-face 'naf-mode-date-face)
|
|  `naf-mode-date-fface'*
|   |  :KEYWORD-REGEXPS-IN 
|   |  `naf-mode-active-date'
|   |  `naf-mode-active-date-flags-solo'
|   |  `naf-mode-active-date-flags-paren'
|   |  `naf-mode-lifespan'
|   |  `naf-mode-date-string'
|   |  `naf-mode-english-dates'
|   |  `naf-mode-french-dates'
|   |  `naf-mode-benezit-date'
|   |  `naf-mode-circa-dates'
|   |  `naf-mode-year-range'
|   |  `naf-mode-english-days'
|   |  `naf-mode-french-days'
|   |  `naf-mode-simple-date'
|   |    
|   |--+ (describe-face 'naf-mode-date-active-face) 
|     `naf-mode-date-active-fface'*
|      :KEYWORD-REGEXPS-IN
|     `naf-mode-active-date'
|
`----\n
,---- (describe-face 'naf-mode-artist-face)
| 
| `naf-mode-artist-fface'
|  |  :KEYWORD-REGEXPS-IN
|  |
|  |--+ (describe-face 'naf-mode-artist-student-of-face)
|    `naf-mode-artist-student-of-fface'
|     |  :KEYWORD-REGEXPS-IN
|     |
|     |--+`naf-mode-artist-student-of-julian-face'
|          |  :KEYWORD-LISTS-IN
|          |  `*naf-students-julian-us*'
|          |  `*naf-students-of-julian-brazil*'
|          |  `*naf-students-of-julian-canada*'
|          |  `*naf-students-of-julian-finland*'
|          |  `*naf-students-of-julian-french*' 
|          |  `*naf-students-of-julian-germany*'
|          |  `*naf-students-of-julian-misc*'
|          |  `*naf-students-of-julian-norway*'
|          |  `*naf-students-of-julian-russia*'
|          |  `*naf-students-of-julian-switzerland*'
|          |  `*naf-students-of-julian-uk*'
|          |  :KEYWORD-REGEXPS-IN
|          |  `naf-mode-students-julian-us'
|          |  `naf-mode-students-of-julian-brazil'
|          |  `naf-mode-students-of-julian-canada'
|          |  `naf-mode-students-of-julian-finland'
|          |  `naf-mode-students-of-julian-french'
|          |  `naf-mode-students-of-julian-germany'
|          |  `naf-mode-students-of-julian-misc'
|          |  `naf-mode-students-of-julian-norway'
|          |  `naf-mode-students-of-julian-russia'
|          |  `naf-mode-students-of-julian-switzerland'
|          |  `naf-mode-students-of-julian-uk'
|
`----\n
======================
;; :FREERANGE-KEYWORDS
======================\n
,---- (describe-face 'naf-mode-benezit-face)
| 
| `naf-mode-benezit-fface'*          
|  :KEYWORD-REGEXPS-IN
| `naf-mode-benezit-section-flag'
| `naf-mode-benezit-currency-acronym'
|
`----\n
,---- (describe-face 'naf-mode-place-face)
| 
| `naf-mode-place-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-nation-english'
| `naf-mode-nation-french'
| `naf-mode-state-names'
| `naf-mode-city-names-us'
| `naf-mode-intnl-auction-city-names'
| `naf-mode-intnl-city-names'
| `naf-mode-region-names-french'
| `naf-mode-region-names-other'
|
`----\n
,---- (describe-face 'naf-mode-event-face)
| 
| `naf-mode-event-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-world-events'
| `naf-mode-art-events'
|
`----\n
,---- (describe-face 'naf-mode-group-period-style-face)
| 
| `naf-mode-group-period-style-fface'*
| :KEYWORD-REGEXPS-IN
| `naf-mode-group-period-styles'
|
`----\n
,---- (describe-face 'naf-mode-institution-face)
| 
| `naf-mode-institution-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-institution-museum-names'
| `naf-mode-academy-names'
| `naf-mode-school-names-intnl'
| `naf-mode-school-names-english'
| `naf-mode-institution-names-generic'
| `naf-mode-benezit-museum-short'
| `naf-mode-inst-names-anchored'
|
`----\n
,---- (describe-face 'naf-mode-alternate-name-face)
| 
| `naf-mode-alternate-name-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-alternate-name-flags'
| `*naf-mode-x-of*'
|
`----\n
,---- (describe-face 'naf-mode-nationality-face)
| 
| `naf-mode-nationality-fface'*
|  :KEYWORD-REGEXPS-IN 
| `naf-mode-nationality-english'
| `naf-mode-nationality-french'.
|
`----\n
,---- (describe-face 'naf-mode-primary-role-face)
| 
| `naf-mode-primary-role-fface'*
|  :KEYWORD-REGEXPS-IN 
| `naf-mode-english-roles-primary'
| `naf-mode-french-roles-primary'
|
`----\n
,---- (describe-face 'naf-mode-secondary-role-face)
| 
| `naf-mode-secondary-role-fface'*
| :KEYWORD-REGEXPS-IN
| `naf-mode-english-roles-secondary'
| `naf-mode-french-roles-secondary'                                   
|
`----\n
,---- (describe-face 'naf-mode-art-keywords-role-face)
| 
| `naf-mode-art-keywords-role-fface'*
|  :KEYWORD-REGEXPS-IN
| `naf-mode-art-keywords'
|
`----\n
,---- (describe-face 'naf-mode-awards-prizes-face)
| 
| `naf-mode-awards-prizes-fface'*     
|  :KEYWORD-REGEXPS-IN
|  `naf-mode-awards-prizes-names' 
|
`----\n
,---- (describe-face 'naf-mode-publication-periodical-face)
|
| `naf-mode-publication-periodical-fface'*
|  :KEYWORD-REGEXPS-IN 
| `*naf-publications-periodicals-english*'
| `*naf-publications-periodicals-english-one-word*'
| `*naf-publications-periodicals-french*'
| `*naf-naf-publications-periodicals-intnl*'
|
`----\n
:SEE-ALSO `mon-help-faces-themes', `mon-help-basic-faces', `mon-help-font-lock'
`mon-help-text-property-stickyness'.►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-naf-mode-faces :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-naf-mode-faces)
;;; :TEST-ME (mon-help-naf-mode-faces t)
;;; :TEST-ME (describe-function 'mon-help-naf-mode-faces)
;;; :TEST-ME (call-interactively 'mon-help-naf-mode-faces)


;;; ===============================================
;;; :STANDARD-NAF-MODE-TEMPLATE-KEYWORD-FACES
;;; ===============================================

;;; ==============================
(defface naf-mode-db-entry-face
  '((((class color) (background light)) (:foreground "DeepPink"))
    (((class color) (background dark)) (:foreground "Red3"))
    (t (:bold t :italic t)))
  "*Face for font-locking National db authority keyword entries in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-db-entry'.
These are standard `naf-mode' template keywords.
:FACE-DOCUMENTED-IN `naf-mode-db-entry-face'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-db-entry-fface 'naf-mode-db-entry-face
  "*Face for `naf-mode' font-locking of National db authority keyword entries.
:KEYWORD-REGEXPS-IN `naf-mode-db-entry'.
These are standard `naf-mode' template keywords.
:FACE-DEFINED-IN `naf-mode-db-entry-face'
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-db-entry-face)
;;
;;;(progn (makunbound 'naf-mode-db-entry-fface) (unintern 'naf-mode-db-entry-fface))

;;; ==============================
(defface naf-mode-delim-face
  '((((class color) (background light)) (:foreground "burlywood"))
    (((class color) (background dark)) (:foreground "DarkKhaki"))
    (t (:bold t :italic t)))
  "*Face for font-locking of keyword delimiter ranges in .naf files.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-delim', `naf-mode-comment-delim'.
:FACE-DOCUMENTED-IN `naf-mode-delim-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-delim-fface 'naf-mode-delim-face
  "*Face for `naf-mode' font-locking of the keyword delimiter ranges.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-delim' and `naf-mode-comment-delim'.
:FACE-DEFINED-IN `naf-mode-delim-face'.\n
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-delim-face)
;;
;;;(progn (makunbound 'naf-mode-delim-fface) (unintern 'naf-mode-delim-fface))

;;; ==============================
(defface naf-mode-name-divider-face
  '((((class color) (background light)) (:foreground "firebrick4"))
    (((class color) (background dark)) (:foreground "firebrick4"))
    (t (:bold t :italic t)))
  "*Face for font-locking of dividing chars of entity name forms in .naf files.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-name-divider'.
:FACE-DOCUMENTED-IN `naf-mode-name-divider-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-name-divider-fface 'naf-mode-name-divider-face
  "*Face for `naf-mode' font-locking of dividing entity name forms.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-name-divider'.
:FACE-DEFINED-IN `naf-mode-name-divider-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-name-divider-face)
;;
;;;(progn (makunbound 'naf-mode-name-divider-fface) (unintern 'naf-mode-name-divider-fface))

;;; ==============================
(defface naf-mode-field-url-flag-face
  '((((class color) (background light)) (:foreground "orangered1"))
    (((class color) (background dark)) (:foreground "orangered1"))
    (t (:bold t :italic t)))
  "Face font-locking of URL refs appearing in db-URL refs in .naf files.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-url-flag'.
:FACE-DOCUMENTED-IN `naf-mode-field-url-flag-face'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-field-url-flag-fface 'naf-mode-field-url-flag-face
  "*Face for `naf-mode' font-locking of .naf db-field URL refs. 
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-url-flag'.
:FACE-DEFINED-IN `naf-mode-field-url-flag-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-field-url-flag-face)
;;
;;;(progn (makunbound 'naf-mode-field-url-flag-fface)
;;;  (unintern 'naf-mode-field-url-flag-fface))

;;; ==============================
;;; :CREATED <Timestamp: Monday April 13, 2009 @ 05:38.32 PM - MON KEY>
(defface naf-mode-delimit-url-flag-face
    '((t (:inherit naf-mode-field-url-flag-face)))
    "*Face for font-locking URLs delimited by (URL `.*') in .naf files.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-url-wrapper-flag'.
This face :INHERITS-FROM `naf-mode-field-url-flag-fface'.
:FACE-DOCUMENTED-IN `naf-mode-delimit-url-flag-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
    :group 'naf-mode
    :group 'naf-mode-faces)
;;
(defvar naf-mode-delimit-url-flag-fface 'naf-mode-delimit-url-flag-face
  "*Face provides font-locking of `naf-mode' URLs delimited by (URL `.*').
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-url-wrapper-flag'.
This face :INHERITS-FROM `naf-mode-field-url-flag-fface'.
:FACE-DEFINED-IN `naf-mode-delimit-url-flag-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-delimit-url-flag-face)
;;
;;;(progn (makunbound 'naf-mode-delimit-url-flag-fface)
;;;   (unintern 'naf-mode-delimit-url-flag-fface))

;;; ==============================
(defface naf-mode-timestamp-face
  '((((class color) (background light)) (:foreground "orangered1"))
    (((class color) (background dark)) (:foreground "orangered1"))
    (t (:bold t :italic t)))
  "Face for `naf-mode' timestamp fontlocking in .naf files.
Fontlock timestamps generated with FUNCTION(S):
`mon-timestamp', `mon-accessed-stamp', `mon-accessed-time-stamp'. 
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-timestamp-flag'.
:FACE-DOCUMENTED-IN `naf-mode-timestamp-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-timestamp-fface 'naf-mode-timestamp-face
  "*Face for `naf-mode' timestamp fontlocking.
Fontlocks timestamps generated with FUNCION(S):
`mon-timestamp', `mon-accessed-stamp',`mon-accessed-time-stamp'.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-timestamp-flag'.
:FACE-DEFINED-IN `naf-mode-timestamp-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-timestamp-face)
;;
;;;(progn (makunbound 'naf-mode-timestamp-fface)
;;;       (unintern 'naf-mode-timestamp-fface))

;;; ==============================
(defface naf-mode-accessed-by-face
  '((t (:inherit naf-mode-timestamp-face)))
  "*Face for font-locking of user names in timestamps in .naf files.
Fontlocking of timestamps generated with FUNCION(S):
`mon-timestamp', `mon-accessed-stamp',`mon-accessed-time-stamp'.
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-accessed-by-flag'.
This face :INHERITS-FROM `naf-mode-timestamp-face'.
:FACE-DOCUMENTED-IN `naf-mode-accessed-by-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-accessed-by-fface 'naf-mode-accessed-by-face
  "*Face for `naf-mode' font-locking of user names in timestamps.
Fontlocking of timestamps generated with FUNCTION(S):
`mon-timestamp', `mon-accessed-stamp', `mon-accessed-time-stamp'. 
These are standard `naf-mode' template keywords.
:KEYWORD-REGEXPS-IN `naf-mode-accessed-by-flag'.
This face :INHERITS-FROM `naf-mode-timestamp-face'.
:FACE-DEFINED-IN `naf-mode-accessed-by-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-accessed-by-face)
;;
;;(progn (makunbound 'naf-mode-accessed-by-fface)
;;       (unintern 'naf-mode-accessed-by-fface))

;;; ==============================
;;; :FIELD-FACES
;;; ==============================

;;; ==============================
(defface naf-mode-field-face
  '((((class color) (background light)) (:foreground "orange"))
    (((class color) (background dark)) (:foreground "orange"))
    (t (:bold t :italic t)))
  "*Face font-locking of primary National db entries fields in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-field-names'.
:FACE-DOCUMENTED-IN `naf-mode-field-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-field-fface  'naf-mode-field-face
  "*Face for `naf-mode' font-locking of National db entries.
:KEYWORD-REGEXPS-IN `naf-mode-field-names'.
:FACE-DEFINED-IN `naf-mode-field-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-field-face)
;;
;;;(progn (makunbound 'naf-mode-field-fface) (unintern 'naf-mode-field-fface))

;;; ==============================
;; MODIFICATIONS: <Timestamp: Friday April 10, 2009 @ 02:35.06 PM - by MON>
;; RENAMED: `naf-mode-field-face-db-entry' -> `naf-mode-db-field-entry-face'
(defface naf-mode-db-field-entry-face
  '((((class color) (background light)) (:foreground "chocolate2"))
    (((class color) (background dark)) (:foreground "chocolate2"))
    (t (:bold t :italic t)))
  "*Face for font-locking of db-field entries in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-db-numbers-flag'.
:FACE-DOCUMENTED-IN `naf-mode-db-field-entry-fface'.
:SEE-ALSO
`naf-mode-db-field-entry-ulan-fface', `naf-mode-db-field-entry-bnf-fface'.\n
Used in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-db-field-entry-fface  'naf-mode-db-field-entry-face
  "*Face for `naf-mode' font-locking of db-field entries.
:KEYWORD-REGEXPS-IN `naf-mode-db-numbers-flag'.
:FACE-DEFINED-IN `naf-mode-db-field-entry-face'.
:SEE-ALSO 
`naf-mode-db-field-entry-ulan-fface', `naf-mode-db-field-entry-bnf-fface'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-db-field-entry-face)
;;
;;;(progn (makunbound 'naf-mode-db-field-entry-fface) (unintern 'naf-mode-db-field-entry-fface))

;;; ==============================
;;; :NOTE :SEE ./naf-mode/notes/faces-as-displayed.naf
;;; :CREATED <Timestamp: Friday April 10, 2009 @ 04:13.56 PM - MON KEY>
(defface naf-mode-field-bnf-face 
  '((t (:inherit naf-mode-field-face)))
  "Face for font-locking of BNF db-field entries in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-field-names-bnf'.
This face :INHERITS-FROM `naf-mode-field-face'.
:FACE-DOCUMENTED-IN `naf-mode-field-bnf-fface'.
:SEE-ALSO `naf-mode-db-field-entry-bnf-fface'.\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-field-bnf-fface 'naf-mode-field-bnf-face
  "*Face for `naf-mode' font-locking of BNF db-field entries.
:KEYWORD-REGEXPS-IN `naf-mode-field-names-bnf'.
:FACE-DEFINED-IN  `naf-mode-field-bnf-face'.
This face :INHERITS-FROM `naf-mode-field-face'.
:SEE-ALSO `naf-mode-db-field-entry-bnf-fface'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-field-bnf-face)
;;
;;;(progn (makunbound 'naf-mode-field-bnf-fface) 
;;;       (unintern 'naf-mode-field-bnf-fface))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-10T14:42:29-04:00Z}#{09374} - by MON>
(defface naf-mode-db-field-entry-bnf-face
  '((t (:inherit naf-mode-db-field-entry-face)))
    "*Face for font-locking of BNF secondary field entries in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-db-field-flags-bnf'.
Face documentation in var `naf-mode-db-field-entry-bnf-fface'.
This face :INHERITS-FROM `naf-mode-db-field-entry-face'.
:SEE-ALSO `naf-mode-db-field-entry-fface', `naf-mode-field-bnf-fface',
`naf-mode-db-field-entry-ulan-fface'.\nUsed in `naf-mode'.\n►►►"
  	    :group 'naf-mode
  	    :group 'naf-mode-faces)
;;
(defvar naf-mode-db-field-entry-bnf-fface 'naf-mode-db-field-entry-bnf-face
    "*Face for font-locking of BNF secondary field entries in `naf-mode'.
:KEYWORD-REGEXPS-IN `naf-mode-db-field-flags-bnf'.
:FACE-DEFINED-IN `naf-mode-db-field-entry-bnf-face'.
This face :INHERITS-FROM `naf-mode-db-field-entry-face'.
:SEE-ALSO `naf-mode-db-field-entry-fface', `naf-mode-field-bnf-fface',
`naf-mode-db-field-entry-ulan-fface'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-db-field-entry-bnf-face)
;;
;;(progn (makunbound 'naf-mode-db-field-entry-bnf-face)
;;       (unintern 'naf-mode-db-field-entry-bnf-face)
;;       (makunbound 'naf-mode-db-field-entry-bnf-fface)
;;       (unintern 'naf-mode-db-field-entry-bnf-fface))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-10T12:19:36-04:00Z}#{09374} - by MON>
(defface naf-mode-field-ulan-face
  '((t (:inherit naf-mode-field-face)))
    "*BASE face for font-locking of primary ULAN fields in .naf files.
:KEYWORD-REGEXPS-IN.
This face :INHERITS-FROM `naf-mode-field-face'.
:FACE-DOCUMENTED-IN `naf-mode-field-ulan-fface'.
:SEE-ALSO `naf-mode-ulan-ppl-corp-face-fface'.\nUsed in `naf-mode'.\n►►►"
    :group 'naf-mode
    :group 'naf-mode-faces)
;;
(defvar naf-mode-field-ulan-fface 'naf-mode-field-ulan-face
    "*BASE face for font-locking of primary ULAN fields in `naf-mode'.
:KEYWORD-REGEXPS-IN.
This face :INHERITS-FROM `naf-mode-field-face'.
:FACE-DEFINED-IN `naf-mode-field-ulan-face'.
:SEE-ALSO `naf-mode-ulan-ppl-corp-face-fface'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-field-ulan-face)
;;
;;(progn 
;; (makunbound 'naf-mode-field-ulan-face)(unintern 'naf-mode-field-ulan-face)
;;  (makunbound 'naf-mode-field-ulan-fface)(unintern 'naf-mode-field-ulan-fface))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-10T14:23:24-04:00Z}#{09374} - by MON>
(defface naf-mode-db-field-entry-ulan-face
  '((t (:inherit naf-mode-db-field-entry-face)))
  "*Face font-locking of secondary ULAN fields \(inside parens\ ) in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-db-field-flags-ulan-paren'.
This face :INHERITS-FROM `naf-mode-field-entry-face'.
:FACE-DOCUMENTED-IN `naf-mode-db-field-entry-ulan-fface'.
:SEE-ALSO `naf-mode-ulan-ppl-corp-fface'.\nUsed in `naf-mode'.\n►►►"
  	    :group 'naf-mode
  	    :group 'naf-mode-faces)
;;
(defvar naf-mode-db-field-entry-ulan-fface 'naf-mode-db-field-entry-ulan-face
    "*Face for `naf-mode' font-locking secondary ULAN fields \(inside parens\ ).
:KEYWORD-REGEXPS-IN `naf-mode-db-field-flags-ulan-paren'.
This face :INHERITS-FROM `naf-mode-field-entry-face'.
:FACE-DEFINED-IN `naf-mode-db-field-entry-ulan-face'.\n
:SEE-ALSO `naf-mode-ulan-ppl-corp-fface'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-db-field-entry-ulan-face)
;;
;;(progn (makunbound 'naf-mode-db-field-entry-ulan-face)
;;  (makunbound 'naf-mode-db-field-entry-ulan-fface)
;;  (unintern 'naf-mode-db-field-entry-ulan-face)
;;  (unintern 'naf-mode-db-field-entry-ulan-fface))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-01T16:18:38-04:00Z}#{09362} - by MON>
(defface naf-mode-ulan-ppl-corp-face
  '((t (:inherit naf-mode-field-ulan-face))) 
    "*Face for font-locking of ULAN 'type-of' fields in `naf-mode' files.
:KEYWORD-REGEXPS-IN
`*naf-mode-ulan-rltd-ppl-corp*', `*naf-mode-x-of-ulan-bol*'.
This face :INHERITS-FROM `naf-mode-db-field-ulan-face'.
:FACE-DOCUMENTED-IN `naf-mode-ulan-ppl-corp-fface'.\n
:SEE-ALSO `*naf-mode-x-of*',`naf-mode-field-face'.\n►►►"
    :group 'naf-mode
    :group 'naf-mode-faces)
;;
(defvar naf-mode-ulan-ppl-corp-fface 'naf-mode-ulan-ppl-corp-face
    "*Face for font-locking of ULAN 'type-of' fields in .naf files.
:KEYWORD-REGEXPS-IN
`*naf-mode-ulan-rltd-ppl-corp*' `*naf-mode-x-of-ulan-bol*'.
This face :INHERITS-FROM `naf-mode-db-field-ulan-face'
:FACE-DEFINED-IN `naf-mode-ulan-ppl-corp-face'
:SEE-ALSO `*naf-mode-x-of', `naf-mode-field-face'\nUsed in `naf-mode'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-naf-mode-ulan-ppl-corp-face)
;;
;;;(progn (makunbound 'naf-mode-naf-mode-ulan-ppl-corp-face-face)
;;;       (makunbound 'naf-mode-naf-mode-ulan-ppl-corp-face-fface)
;;;       (unintern 'naf-mode-naf-mode-ulan-ppl-corp-face-face)
;;;       (unintern 'naf-mode-naf-mode-ulan-ppl-corp-face-fface))

;;; ==============================
(defface naf-mode-date-face
  '((((class color) (background light)) (:foreground "forest green"))
    (((class color) (background dark)) (:foreground "forest green"))
    (t (:bold t :italic t)))
  "Face for font-locking Date related keyword flags in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-active-date', `naf-mode-active-date-flags-solo'
`naf-mode-active-date-flags-paren', `naf-mode-lifespan'
`naf-mode-date-string', `naf-mode-english-dates', `naf-mode-french-dates'
`naf-mode-benezit-date', `naf-mode-circa-dates', `naf-mode-year-range'
`naf-mode-english-days', `naf-mode-french-days', `naf-mode-simple-date'
:FACE-DOCUMENTED-IN `naf-mode-date-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-date-fface 'naf-mode-date-face
  "*Face for `naf-mode' font-locking of date related keyword flags.
:KEYWORD-REGEXPS-IN
`naf-mode-active-date', `naf-mode-active-date-flags-solo'
`naf-mode-active-date-flags-paren', `naf-mode-lifespan'
`naf-mode-date-string', `naf-mode-english-dates', `naf-mode-french-dates'
`naf-mode-benezit-date', `naf-mode-circa-dates', `naf-mode-year-range'
`naf-mode-english-days', `naf-mode-french-days', `naf-mode-simple-date'
:FACE-DEFINED-IN `naf-mode-date-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-date-face)
;;
;;;(progn (makunbound 'naf-mode-date-fface) (unintern 'naf-mode-date-fface))

;;; ==============================
;;; :CREATED <Timestamp: Monday April 13, 2009 @ 06:44.56 PM - MON KEY>
(defface naf-mode-date-active-face
  '((t (:inherit naf-mode-date-face)))
    "*Face for font-locking of active period \(circas, ca. etc.\) in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-active-date'.
This face :INHERITS-FROM `naf-mode-date-face'.
:FACE-DOCUMENTED-IN `naf-mode-date-active-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-date-active-fface 'naf-mode-date-active-face
  "*Face for `naf-mode' font-locking of active period (circas) keywords.
:KEYWORD-REGEXPS-IN `naf-mode-active-date'.
This face :INHERITS-FROM `naf-mode-date-face'.
:FACE-DEFINED-IN `naf-mode-date-active-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-date-active-face)
;;
;;(progn (makunbound 'naf-mode-date-active-fface) 
;;       (unintern 'naf-mode-date-active-fface))

;;; ==============================
(defface naf-mode-benezit-face
  '((((class color) (background light)) (:foreground "pink2"))
    (((class color) (background dark)) (:foreground "pink2"))
    (t (:bold t :italic t)))
  "*Face name for font-locking of Benezit entries in .naf files.
Typically comprise auction price flags and auction related infos.
:KEYWORD-REGEXPS-IN
`naf-mode-benezit-section-flag', `naf-mode-benezit-currency-acronym'.
:FACE-DOCUMENTED-IN `naf-mode-benezit-face'.
:SEE-ALSO `naf-mode-benezit-currency-acronym'.\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-benezit-fface 'naf-mode-benezit-face
  "*Face name for `naf-mode' font-locking of Benezit entries.
Typically thes are comprised fo auction price flags and auction related infos.
:KEYWORD-REGEXPS-IN
`naf-mode-benezit-section-flag', `naf-mode-benezit-currency-acronym'.
:FACE-DOCUMENTED-IN `naf-mode-benezit-face'.
:SEE-ALSO `naf-mode-benezit-currency-acronym'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-benezit-face)
;;;
;;;(progn (makunbound 'naf-mode-benezit-fface)(unintern 'naf-mode-benezit-fface))

;;; ==============================
(defface naf-mode-place-face
   '((((class color) (background light)) (:foreground "yellow4"))
     (((class color) (background dark)) (:foreground "yellow4"))
     (t (:bold t :italic t)))
   "*Face name for font-locking of Nation/Region/State/Place names in naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-nation-english',`naf-mode-nation-french'
`naf-mode-state-names',`naf-mode-city-names-us'
`naf-mode-intnl-auction-city-names',`naf-mode-intnl-city-names'
`naf-mode-region-names-french',`naf-mode-region-names-other'
:FACE-DOCUMENTED-IN `naf-mode-place-fface'.
:SEE-ALSO `naf-mode-city-names-us'.\nUsed in `naf-mode'.\n►►►"
    :group 'naf-mode
    :group 'naf-mode-faces)
;;
(defvar naf-mode-place-fface 'naf-mode-place-face
  "*Face name for `naf-mode' font-locking of Nation/Region/State/Place names.\n
:KEYWORD-REGEXPS-IN
`naf-mode-nation-english',`naf-mode-nation-french'
`naf-mode-state-names',`naf-mode-city-names-us'
`naf-mode-intnl-auction-city-names',`naf-mode-intnl-city-names'
`naf-mode-region-names-french',`naf-mode-region-names-other'
:FACE-DEFINED-IN `naf-mode-place-face'.
:SEE-ALSO `naf-mode-city-names-us'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-place-face)
;;
;;;(progn (makunbound 'naf-mode-place-fface) (unintern 'naf-mode-place-fface))

;;; ==============================
(defface naf-mode-nationality-face
  '((((class color) (background light)) (:foreground "yellow3"))
    (((class color) (background dark)) (:foreground "yellow3"))
    (t (:bold t :italic t)))
  "*Face name for font-locking of nationality keywords .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-nationality-english', `naf-mode-nationality-french'.
:FACE-DOCUMENTED-IN `naf-mode-nationality-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;;
(defvar naf-mode-nationality-fface  'naf-mode-nationality-face
  "*Face name for `naf-mode' font-locking of nationality keywords.
:KEYWORD-REGEXPS-IN
`naf-mode-nationality-english', `naf-mode-nationality-french'.
:FACE-DEFINED-IN `naf-mode-nationality-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-nationality-face)
;;
;;;(progn (makunbound 'naf-mode-nationality-fface)
;;;       (unintern 'naf-mode-nationality-fface))

;;; ==============================
;; TODO: 
;;; :MODIFICATIONS <Timestamp: #{2009-10-02T19:00:53-04:00Z}#{09405} - by MON>
(defface naf-mode-publication-periodical-face
  '((((class color) (background light)) (:foreground "CadetBlue3"))
    (((class color) (background dark)) (:foreground "CadetBlue3"))
    (t (:bold t :italic t)))
  "*Face for font-locking of periodial publications names in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-publications-periodicals-english'
`naf-mode-publications-periodicals-english-one-word'
`naf-mode-publications-periodicals-french'
`naf-mode-publications-periodicals-intnl'
:KEYWORD-LISTS-IN
`*naf-publications-periodicals-english*'
`*naf-publications-periodicals-english-one-word*'
`*naf-publications-periodicals-french*'
`*naf-naf-publications-periodicals-intnl*'
:FACE-DOCUMENTED-IN `naf-mode-publication-periodical-fface'.
:SEE-ALSO\n`*naf-publications-periodicals-french-xrefs*'
`*naf-mode-publications-periodicals-english-xrefs*'
`*naf-mode-publications-periodicals-intnl-xrefs*'\nUsed in `naf-mode'.\n►►►"
    :group 'naf-mode
    :group 'naf-mode-faces)
;;
(defvar naf-mode-publication-periodical-fface 'naf-mode-publication-periodical-face
  "*Face for font-locking  periodical/publication keyword titles in `naf-mode'.
:KEYWORD-REGEXPS-IN
`naf-mode-publications-periodicals-english'
`naf-mode-publications-periodicals-english-one-word'
`naf-mode-publications-periodicals-french'
`naf-mode-publications-periodicals-intnl'
:KEYWORD-LISTS-IN
`*naf-publications-periodicals-english*'
`*naf-publications-periodicals-english-one-word*'
`*naf-publications-periodicals-french*'
`*naf-naf-publications-periodicals-intnl*'
:FACE-DOCUMENTED-IN `naf-mode-publication-periodical-fface'.
:SEE-ALSO\n`*naf-publications-periodicals-french-xrefs*'
`*naf-mode-publications-periodicals-english-xrefs*'
`*naf-mode-publications-periodicals-intnl-xrefs*'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-publication-periodical-face)
;;
;;; (progn (makunbound 'naf-mode-publication-periodical-fface)
;;;        (unintern 'naf-mode-publication-periodical-fface))

;;; ==============================
(defface naf-mode-event-face
  '((((class color) (background light)) (:foreground "LightSkyBlue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:bold t :italic t)))
  "*Face font-locking events of artistic importance/signifigance in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-world-events', `naf-mode-art-events'.
:FACE-DOCUMENTED-IN `naf-mode-event-fface'.
:SEE-ALSO .\nUsed in `naf-mode'\n►►►."
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-event-fface 'naf-mode-event-face
  "*Face font-locking events of artistic importance/signifigance in `naf-mode'.
:KEYWORD-REGEXPS-IN
`naf-mode-world-events', `naf-mode-art-events'.
:FACE-DEFINED-IN `naf-mode-event-face'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-event-face)
;;
;;;(progn (makunbound 'naf-mode-event-fface) (unintern 'naf-mode-event-fface))

;;; ==============================
(defface naf-mode-group-period-style-face
  '((((class color) (background light)) (:foreground "LightSteelBlue"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t :italic t)))
  "*Face for font-locking Group Period Style or artistic names in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-group-period-styles'.
:FACE-DOCUMENTED-IN `naf-mode-group-period-style-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-group-period-style-fface 'naf-mode-group-period-style-face
  "*Face for `naf-mode' font-locking of Group Period Style or artistic names.
:KEYWORD-REGEXPS-IN `naf-mode-group-period-styles'.
:FACE-DEFINED-IN `naf-mode-group-period-style-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-group-period-style-face)
;;
;;;(progn (makunbound 'naf-mode-group-period-style-fface)
;;;       (unintern 'naf-mode-group-period-style-fface))

;;; ==============================
(defface naf-mode-institution-face
  '((((class color) (background light)) (:foreground "CornflowerBlue"))
    (((class color) (background dark)) (:foreground "CornflowerBlue"))
    (t (:bold t :italic t)))
   "*Face fontlocking of institution name keywords in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-institution-museum-names', `naf-mode-institution-museum-names'
`naf-mode-academy-names', `naf-mode-school-names-intnl'
`naf-mode-school-names-english', `naf-mode-institution-names-generic'
`naf-mode-benezit-museum-short',`naf-mode-inst-names-anchored'
:FACE-DOCUMENTED-IN `naf-mode-institution-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
    :group 'naf-mode
    :group 'naf-mode-faces)
;;
(defvar naf-mode-institution-fface 'naf-mode-institution-face
  "*Face for `naf-mode' font-locking of institution name keywords
:KEYWORD-REGEXPS-IN
`naf-mode-institution-museum-names', `naf-mode-institution-museum-names'
`naf-mode-academy-names', `naf-mode-school-names-intnl'
`naf-mode-school-names-english', `naf-mode-institution-names-generic'
`naf-mode-benezit-museum-short',`naf-mode-inst-names-anchored'
:FACE-DEFINED-IN `naf-mode-institution-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-institution-face)
;;
;;(progn (makunbound 'naf-mode-institution-fface)
;;       (unintern 'naf-mode-institution-fface))

;;; ==============================
(defface naf-mode-alternate-name-face
  '((((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "CadetBlue"))
    (t (:bold t :italic t)))
   "Face for font-locking of alternate name keywords in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-alternate-name-flags', `*naf-mode-x-of*'.
:FACE-DOCUMENTED-IN `naf-mode-alternate-name-fface'.
:SEE-ALSO `*naf-mode-x-of-ulan-bol*', `*naf-mode-ulan-rltd-ppl-corp*'.
Used in `naf-mode'.\n►►►"
    :group 'naf-mode
    :group 'naf-mode-faces)
;;
(defvar naf-mode-alternate-name-fface 'naf-mode-alternate-name-face
  "*Face for font-locking of alternate name keywords in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-alternate-name-flags',`*naf-mode-x-of*'.
:FACE-DEFINED-IN `naf-mode-alternate-name-face'.
:SEE-ALSO `*naf-mode-x-of-ulan-bol*',`*naf-mode-ulan-rltd-ppl-corp*'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-alternate-name-face)
;;
;;(progn (makunbound 'naf-mode-alternate-name-fface) 
;;       (unintern 'naf-mode-alternate-name-fface))

;;; ==============================
(defface naf-mode-primary-role-face
  '((((class color) (background light)) (:foreground "orchid3"))
    (((class color) (background dark)) (:foreground "orchid3"))
    (t (:bold t :italic t)))
  "*Face for fontlocking primary Artist roles in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-english-roles-primary', `naf-mode-french-roles-primary'.
:FACE-DOCUMENTED-IN `naf-mode-primary-role-face'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-primary-role-fface 'naf-mode-primary-role-face
  "*Face for fontlocking primary Artist roles in `naf-mode'.
:KEYWORD-REGEXPS-IN
`naf-mode-english-roles-primary', `naf-mode-french-roles-primary'.
:FACE-DEFINED-IN `naf-mode-primary-role-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-primary-role-face)
;;
;;;(progn (makunbound 'naf-mode-primary-role-fface)
;;;        (unintern 'naf-mode-primary-role-fface))

;;; ==============================
(defface naf-mode-secondary-role-face
  '((((class color) (background light)) (:foreground "plum4"))
    (((class color) (background dark)) (:foreground "plum4"))
    (t (:bold t :italic t)))
  "*Face for font-locking of artist 'secondary role' keywords in .naf files.
:KEYWORD-REGEXPS-IN
`naf-mode-english-roles-secondary', `naf-mode-french-roles-secondary'.
:FACE-DOCUMENTED-IN `naf-mode-secondary-role-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-secondary-role-fface 'naf-mode-secondary-role-face
  "*Face for `naf-mode' font-locking of artist 'secondary role' keywords.
:KEYWORD-REGEXPS-IN
`naf-mode-english-roles-secondary', `naf-mode-french-roles-secondary'.
:FACE-DEFINED-IN `naf-mode-secondary-role-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-secondary-role-face)
;;
;;;(progn (makunbound 'naf-mode-secondary-role-fface)
;;;       (unintern 'naf-mode-secondary-role-fface))

;;; ==============================
(defface naf-mode-art-keywords-role-face
  '((((class color) (background light)) (:foreground "MediumPurple4"))
    (((class color) (background dark)) (:foreground "MediumPurple4"))
    (t (:bold t :italic t)))
  "*Face for font-locking art keywords indicating artistic 'role' in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-art-keywords'.
:FACE-DOCUMENTED-IN `naf-mode-art-keywords-role-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-art-keywords-role-fface 'naf-mode-art-keywords-role-face
  "*Face for `naf-mode' font-locking of `roles' in art related artistic production.
:KEYWORD-REGEXPS-IN `naf-mode-art-keywords'.
:FACE-DEFINED-IN `naf-mode-art-keywords-role-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-art-keywords-role-face)
;;
;;;(progn (makunbound 'naf-mode-art-keywords-role-fface)
;;;       (unintern 'naf-mode-art-keywords-role-fface))

;;; ==============================
(defface naf-mode-awards-prizes-face
  '((((class color) (background light)) (:foreground "slate blue"))
    (((class color) (background dark)) (:foreground "slate blue"))
    (t (:bold t :italic t)))
  "*Face for font-locking of awards and prizes keywords in .naf files.
:KEYWORD-REGEXPS-IN `naf-mode-awards-prizes-names'.
:FACE-DOCUMENTED-IN `naf-mode-awards-prizes-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
  :group 'naf-mode
  :group 'naf-mode-faces)
;;
(defvar naf-mode-awards-prizes-fface 'naf-mode-awards-prizes-face
  "*Face for `naf-mode' font-locking of awards and prizes keywords.
:KEYWORD-REGEXPS-IN `naf-mode-awards-prizes-names'.
:FACE-DEFINED-IN `naf-mode-awards-prizes-face'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-awards-prizes-face)
;;
;;(progn (makunbound 'naf-mode-awards-prizes-face)
;;       (unintern 'naf-mode-awards-prizes-face))

;;; ==============================
;;; :NOTE
;;; This was in my custom file but unused:
;;; (naf-mode-artist-face 
;;;  ((((class color) (background dark)) 
;;;    (:inherit default 
;;;     :foreground "yellow" 
;;;     :box (:line-width 2 :color "grey75" :style released-button)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-25T12:38:52-04:00Z}#{09395} - by MON>
(defface naf-mode-artist-face
  '((((class color) (background light)) 
     (:foreground "OliveDrab4"))
    (((class color) (background dark)) 
     (:foreground "OliveDrab3"))
    (t (:bold t :italic t)))
  "*Face for font-locking of artist names as keywords in .naf files.
:KEYWORD-REGEXPS-IN `'.
:FACE-DOCUMENTED-IN `naf-mode-artist-fface'.
This face is INHERITED-BY:
`naf-mode-artist-student-of-fface',`naf-mode-artist-student-of-julian-fface'.
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
   :group 'naf-mode
   :group 'naf-mode-faces)
;;
(defvar naf-mode-artist-fface 'naf-mode-artist-face
  "*Face for `naf-mode' font-locking of artist names as keywords.
:KEYWORD-REGEXPS-IN `'.
:FACE-DEFINED-IN `naf-mode-awards-artist-fface'.
This face is INHERITED-BY:
`naf-mode-artist-student-of-fface'\n`naf-mode-artist-student-of-julian-fface'.
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-artist-face)
;;
;;;(progn (makunbound 'naf-mode-artist-face)
;;;       (unintern 'naf-mode-artist-face)
;;;       (makunbound 'naf-mode-artist-fface)
;;;       (unintern 'naf-mode-artist-fface))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-25T12:38:45-04:00Z}#{09395} - by MON>
(defface naf-mode-artist-student-of-face
  '((t (:inherit naf-mode-artist-face
        :box (:line-width 1 :color "sienna" :style "released-button"))))  
  "*Face for font-locking of artists who studied under another artist. 
:KEYWORD-REGEXPS-IN `'.
:FACE-DOCUMENTED-IN `naf-mode-artist-student-of-fface'.
This face :INHERITS-FROM `naf-mode-artist-fface'
This face is INHERITED-BY: `naf-mode-artist-student-of-julian-fface'
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
   :group 'naf-mode
   :group 'naf-mode-faces)
;;
(defvar naf-mode-artist-student-of-fface 'naf-mode-artist-student-of-face
  "*Face for `naf-mode' font-locking of artists who studied under another artist.
:KEYWORD-REGEXPS-IN `'.
:FACE-DEFINED-IN: `naf-mode-awards-artist-student-of-face'.
This face :INHERITS-FROM `naf-mode-artist-student-of-fface'
This face is INHERITED-BY: `naf-mode-artist-student-of-julian-fface'
:SEE-ALSO.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-artist-student-of-face)
;;
;;;(progn (makunbound 'naf-mode-artist-student-of-face)
;;;        (unintern 'naf-mode-artist-student-of-face)
;;;        (makunbound 'naf-mode-artist-student-of-fface)
;;;        (unintern 'naf-mode-artist-student-of-fface))


;;;    (:inherit default 
;;;     :foreground "yellow" 
;;;     :box (:line-width 2 :color "grey75" :style released-button)))))
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-25T12:52:57-04:00Z}#{09395} - by MON>
(defface naf-mode-artist-student-of-julian-face
  '((t (:inherit naf-mode-artist-student-of-face
        :box (:line-width 1 :color "dark khaki" :style: "released-button"))))
  "*Face for font-locking names of artists who attended the Académie Julian.
:KEYWORD-REGEXPS-IN `'.
:FACE-DOCUMENTED-IN `naf-mode-artist-student-of-julian-fface'.
This face :INHERITS-FROM `naf-mode-artist-student-of-fface'
:SEE-ALSO .\nUsed in `naf-mode'.\n►►►"
   :group 'naf-mode
   :group 'naf-mode-faces)
;;
(defvar naf-mode-artist-student-of-julian-fface 'naf-mode-artist-student-of-julian-face
  "*Face for `naf-mode' font-locking of artists who attended Academie Julian.
:KEYWORD-REGEXPS-IN
`naf-mode-students-of-julian-french'
`naf-mode-students-julian-us'
`naf-mode-students-of-julian-misc'
`naf-mode-students-of-julian-brazil'
`naf-mode-students-of-julian-canada'
`naf-mode-students-of-julian-finland'
`naf-mode-students-of-julian-germany'
`naf-mode-students-of-julian-norway'
`naf-mode-students-of-julian-russia'
`naf-mode-students-of-julian-switzerland'
`naf-mode-students-of-julian-uk'
:KEYWORD-LISTS-IN
`*naf-students-julian-us*'
`*naf-students-of-julian-misc*'
`*naf-students-of-julian-brazil*'
`*naf-students-of-julian-canada*'
`*naf-students-of-julian-finland*'
`*naf-students-of-julian-germany*'
`*naf-students-of-julian-norway*'
`*naf-students-of-julian-russia*'
`*naf-students-of-julian-switzerland*'
`*naf-students-of-julian-uk*'
:FACE-DEFINED-IN `naf-mode-awards-artist-student-of-julian-face'.
This face :INHERITS-FROM `naf-mode-artist-student-of-fface'
:SEE-ALSO `*naf-mode-students-of-julian-xrefs*'.\n►►►")
;;
;;; :TEST-ME (describe-face 'naf-mode-artist-student-of-julian-face)
;;
;;;(progn (makunbound 'naf-mode-artist-student-of-julian-face)
;;;        (unintern 'naf-mode-artist-student-of-julian-face)
;;;        (makunbound 'naf-mode-artist-student-of-julian-fface)
;;;        (unintern 'naf-mode-artist-student-of-julian-fface))

;;; ==============================

;;; ==============================
;;; :CURRENTLY-INACTIVE-FACES
;;; ==============================
;; (defface naf-mode-role-face-2
;;   '((((class color) (background light)) (:foreground "pink"))
;;     (((class color) (background dark)) (:foreground "pink"))
;;     (t (:bold t :italic t)))
;;   "naf-mode fontlock for National db entries ."
;;    :group 'naf-mode
;;    :group 'naf-mode-faces)

;;  (defvar naf-mode-role-face-2 'naf-mode-role-face-2
;;    "role-face2")
;;; ==============================

;;; ==============================
;;; :NAME-FACES-FOR-NAF-MODE
;;; ==============================
;;; (defface naf-mode-name-face-base
;;;   '((((class color) (background light)) (:foreground "some-color"))
;;;     (((class color) (background dark)) (:foreground "some-color"))
;;;     (t (:bold t :italic t)))
;;;   "naf-mode fontlock for named entities this is the default face
;;; it doesn't specialize on in any particular way."
;;;    :group 'naf-mode
;;;         :group 'naf-mode-faces)
;;;
;;;   (defvar naf-mode-name-fface 'naf-mode-name-face-base
;;; "Face name to use for `naf-mode' basic name font-locking -
;;; a non specialized name face.")
;;; ==============================

;;; ==============================
;;; (defface naf-mode-author-face
;;;   '((((class color) (background light)) (:foreground "some-color"))
;;;     (((class color) (background dark)) (:foreground "some-color"))
;;;     (t (:bold t :italic t)))
;;;   "naf-mode fontlock for ____ ."
;;;    :group 'naf-mode
;;;         :group 'naf-mode-faces)
;;;
;;;   (defvar naf-mode-author-fface 'naf-mode-author-face
;;;     "Face name to use for `naf-mode' Author name font-locking.")
;;; ==============================

;;; ==============================
;;; (defface naf-mode-brand-face
;;;   '((((class color) (background light)) (:foreground "some-color"))
;;;     (((class color) (background dark)) (:foreground "some-color"))
;;;     (t (:bold t :italic t)))
;;;   "naf-mode fontlock for ____ ."
;;;    :group 'naf-mode
;;          :group 'naf-mode-faces)
;;;
;;;   (defvar naf-mode-brand-fface 'naf-mode-brand-face
;;;     "Face name to use for `naf-mode' Brand name font-locking.")
;;; ==============================

;;; ==============================
;;; (defface naf-mode-people-face
;;;   '((((class color) (background light)) (:foreground "some-color"))
;;;     (((class color) (background dark)) (:foreground "some-color"))
;;;     (t (:bold t :italic t)))
;;;   "naf-mode fontlock for ____ ."
;;;    :group 'naf-mode
;;;         :group 'naf-mode-faces)
;;;
;;;   (defvar naf-mode- -fface 'naf-mode- -face
;;;     "Face name to use for `naf-mode' Artist name font-locking.")
;;; ==============================

;;; ==============================
(provide 'naf-mode-faces)
;;; ==============================

;;; ==============================
;;; naf-mode-faces.el ends here
;;; EOF

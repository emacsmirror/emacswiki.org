;;; mon-replacement-utils.el --- common regexp and subsitiution procedures 
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-replacement-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-03-08T13:16:02-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, convenience, data

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-replacement-utils provides a collection of routines and commands and
;; abstracts some commonly encountered procedures for processing regexps with
;; their replacements.
;;
;; :NOTE This file used to be named naf-mode-replacements.el
;; :AS-OF <Timestamp: #{2009-12-19T14:11:23-05:00Z}#{09516} - by MON>
;; It has been renamed to mon-replacement-utils.el
;;
;; FUNCTIONS:►►►
;; `mon-replace-regexp-while', `mon-replace-string-while',
;; `replace-string-pairs-region3', `replace-string-pairs-region-no-props',
;; `mon-get-list-yorp', `mon-get-list-norp',
;; `mon-replace-region-regexp-lists-nonint', `mon-replace-region-regexp-lists',
;; `mon-exchange-slash-and-backslash', `mon-cln-file-name-string',
;; `mon-regexp-filter', `mon-cln-html-chars',
;; `mon-cln-html-tags', `mon-string-canonical', 
;; `mon-toggle-case-regexp-region', 
;; `mon-toggle-case-query-user', `mon-toggle-case-regexp',
;; `mon-downcase-regexp-region', `mon-downcase-regexp',
;; `mon-upcase-regexp', `mon-upcase-regexp-region',
;; `mon-line-number-region-incr', `mon-cln-piped-list',
;; `mon-delete-back-up-list', `naf-backup-the-list', `mon-cln-philsp',
;; `mon-cln-ulan', `mon-cln-imdb', `mon-cln-loc', `mon-cln-wiki',
;; `mon-cln-bib', `mon-cln-BIG-whitespace', `mon-cln-whitespace',
;; `mon-cln-trail-whitespace', `mon-kill-whitespace', `mon-cln-blank-lines',
;; `mon-cln-uniq-lines', `mon-cln-spc-tab-eol', `mon-cln-spc-tab-at-eol-in-region',
;; `mon-cln-control-M', `mon-num-to-month', `mon-num-to-month-whitespace',
;; `mon-month-to-num', `mon-abr-to-month', `mon-trans-cp1252-to-latin1',
;; `mon-ital-date-to-eng', `mon-defranc-dates', `mon-defranc-places',
;; `mon-defranc-benezit', `mon-replace-common-abbrevs', `mon-zippify-region',
;; `bug-cln-gilt-group', `mon-cln-csv-fields', `mon-cln-xml<-parsed',
;; `mon-cln-tgm-xml-LF', `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p',
;; `mon-replace-string-pairs-region-no-insert', `mon-cln-xml<-parsed-strip-nil',
;; `mon-cln-up-colon', `mon-regexp-map-match', `mon-regexp-map-match-in-region',
;; `mon-walk-regexps-in-file', `mon-replace-regexps-in-file-list',
;; `mon-cln-mail-headers', `mon-cln-xml-escapes',
;; `mon-replace-unintern-w-query',
;; `mon-make-iso-latin-1-approximation-loadtime', `mon-cln-freenode-log',
;; FUNCTIONS:◄◄◄
;; 
;; MACROS:
;; 
;; METHODS:
;;
;; CLASSES:
;;
;; VARIABLES:
;; `*iso-latin-1-approximation*', `*mon-regexp-unintern*',
;; `*mon-replacement-utils-xrefs*',
;;
;; GROUPS:
;; `mon-replacement-utils'
;;
;; CONSTANTS:
;;
;; RENAMED: 
;; `naf-delete-back-up-list'             -> `mon-delete-back-up-list'
;; `mon-clnBIG-whitespace'               -> `mon-cln-BIG-whitespace'
;; `mon-re-number-region'                -> `mon-line-number-region-incr'
;; `mon-canonical-string'                -> `mon-string-canonical'
;;
;; MOVED:
;; `mon-query-replace-register1<-reg2'   -> mon-empty-registers.el
;; `mon-insert-regexp-template-yyyy'     -> mon-insertion-utils.el
;; `mon-cln-xml-escapes-TEST'            -> mon-testme-utils.el
;; `mon-up/down-case-regexp-TEST'        -> mon-testme-utils.el
;; `mon-line-pipe-lines'                 -> mon-line-utils.el
;; `mon-line-find-duplicates-cln'        -> mon-line-utils.el
;; `mon-toggle-restore-llm'              -> mon-macs.el
;; `mon-naf-mode-toggle-restore-llm'     -> mon-macs.el
;;
;; ALIASED/ADVISED/SUBST'd:
;; `naf-delete-back-up-list'             -> `mon-delete-back-up-list'
;; `mon-map-regexp-matches'              -> `mon-regexp-map-match'
;; `mon-string-canonical'                -> `mon-canonical-string'
;;
;; REQUIRES:
;; Regexps for functions defined here are set with defvar forms in the file:
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-regexp-symbols.el')
;;
;; References the following: CONSTANTS OR VARIABLES:
;; `*regexp-philsp-months*',`*regexp-philsp-apos*', `*regexp-philsp-location*',
;; `*regexp-philsp-swap-location*' `*regexp-philsp-fix-month-dates*', `*regexp-clean-ulan*',
;; `*regexp-clean-imdb*', `*regexp-clean-loc*', `*regexp-clean-wikipedia*',
;; `*regexp-clean-bib*', `regexp-cleanBIG-whitespace', `*regexp-clean-whitespace*',
;; `*regexp-MM->month*', `*regexp-MM->month-whitespace-aware*', `*regexp-month->MM*',
;; `*regexp-cp1252-to-latin1*', `*regexp-ital-to-eng*', `*regexp-defranc-dates*',
;; `*regexp-defranc-places*', `*regexp-defranc-benezit*', `*regexp-common-abbrevs*', 
;;
;; :FILE boxquote.el - `mon-regexp-map-match-in-region' 
;;
;; :FILE 
;; `mon-replace-regexps-in-file-list'
;; `mon-walk-regexps-in-file'
;;
;; TODO:
;; Instances of longlines-mode checks, e.g.:
;;    (and (buffer-local-value longlines-mode (current-buffer)))
;; should _maybe_ be updated with:
;;    (and (boundp 'longlines-mode) (bound-and-true-p longlines-mode))
;; 
;; Need function to show whitespace, tab, _and_ `longlines-mode's
;; hardlines using `longlines-show-hard-newlines'.
;;
;; NOTES:
;;
;; SNIPPETS:
;; Test if we are in a `naf-mode' buffer:
;; (eq (buffer-local-value 'major-mode (current-buffer)) 'naf-mode)
;;
;; THIRD PARTY CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-replacement-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-09-20} - by MON KEY>
;;
;; EMACSWIKI:
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-03-08T13:16:02-04:00Z} - by MON KEY>
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
;; Copyright © 2009-2011 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

(require 'mon-regexp-symbols)



;;; ==============================
;;; :CHANGESET 2406
;;; :CREATED <Timestamp: #{2011-01-20T18:59:00-05:00Z}#{11034} - by MON KEY>
(defgroup mon-replacement-utils nil
  "Customization group for variables and functions of :FILE mon-replacement-utils.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag "\n:EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-replacement-utils.el')" 
          "http://www.emacswiki.org/emacs/mon-replacement-utils.el")
  :link '(emacs-library-link 
          :tag "\n:FILE mon-replacement-utils.el"
          "mon-replacement-utils.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2406
;;; :CREATED <Timestamp: #{2011-01-20T18:58:56-05:00Z}#{11034} - by MON KEY>
(defcustom *mon-replacement-utils-xrefs* 
  '(mon-is-naf-mode-p mon-is-naf-mode-and-llm-p mon-make-iso-latin-1-approximation
    mon-make-iso-latin-1-approximation-loadtime mon-cln-iso-latin-1 deftransmogrify
    mon-transmogrify mon-walk-regexps-in-file mon-replace-regexps-in-file-list
    mon-replace-strings mon-replace-regexp-while mon-replace-string-while
    mon-regexp-filter mon-string-canonical replace-string-pairs-region3
    replace-string-pairs-region-no-props mon-replace-string-pairs-region-no-insert
    mon-get-list-yorp mon-get-list-norp mon-replace-region-regexp-lists-nonint
    mon-replace-region-regexp-lists mon-regexp-map-match
    mon-regexp-map-match-in-region mon-replace-unintern-w-query mon-cln-mail-headers
    mon-cln-csv-fields mon-cln-freenode-log mon-cln-file-name-string
    mon-cln-html-chars mon-cln-html-tags mon-cln-xml-escapes mon-cln-xml<-parsed
    mon-cln-tgm-xml-LF mon-cln-xml<-parsed-strip-nil mon-cln-up-colon
    mon-downcase-hex-values mon-upcase-commented-lines mon-zippify-region
    mon-toggle-case-query-user mon-toggle-case-regexp-region mon-toggle-case-regexp
    mon-downcase-regexp-region mon-upcase-regexp-region mon-downcase-regexp
    mon-upcase-regexp mon-cln-BIG-whitespace mon-cln-whitespace
    mon-cln-trail-whitespace mon-kill-whitespace mon-cln-blank-lines
    mon-cln-spc-tab-eol mon-cln-spc-tab-at-eol-in-region mon-cln-uniq-lines
    mon-exchange-slash-and-backslash mon-cln-control-M mon-cln-piped-list
    mon-delete-back-up-list naf-backup-the-list mon-cln-philsp mon-cln-ulan
    mon-cln-imdb mon-cln-loc mon-cln-wiki mon-cln-bib mon-num-to-month
    mon-num-to-month-whitespace mon-month-to-num mon-abr-to-month
    mon-trans-cp1252-to-latin1 mon-ital-date-to-eng mon-defranc-dates
    mon-defranc-places mon-cln-benezit mon-cln-benezit-fields
    mon-replace-common-abbrevs bug-cln-gilt-group
    ;; :VARIABLES
    *naf-mode-buffer-local-llm* *iso-latin-1-approximation* *mon-regexp-unintern*
    *mon-replacement-utils-xrefs*)
  "Xrefing list of mon replacement functions, constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-replacement-utils.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-doc-help-CL-xrefs*',
`*mon-dir-utils-xrefs*', `*mon-keybindings-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-bzr-utils-xrefs*' `*mon-buffer-utils-xrefs*',
`*mon-env-proc-utils-xrefs*', `*mon-error-utils-xrefs*',
`*mon-line-utils-xrefs*', `*mon-macs-xrefs*', `*mon-plist-utils-xrefs*',
`*mon-post-load-hooks-xrefs*', `*mon-seq-utils-xrefs*',
`*mon-string-utils-xrefs*', `*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*', `*mon-slime-xrefs*',
`*mon-url-utils-xrefs*', `*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*',
`*mon-ulan-utils-xrefs*', `*google-define-redux-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-replacement-utils
  :group 'mon-xrefs)

 
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-12T14:14:46-05:00Z}#{10105} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-08T12:56:12-04:00Z}#{09372} - by MON KEY>
(defun mon-is-naf-mode-p (&optional check-naf-buffer)  
  "Test if buffer is in `naf-mode'.\n
When optional arg CHECK-NAF-BUFFER is non-nil check that buffer.\n
Signal an error if CHECK-NAF-BUFFER does not exist. Default is `current-buffer'.\n
:EXAMPLE\n\n(mon-is-naf-mode-p)\n
\(with-temp-buffer \(naf-mode\) \(mon-is-naf-mode-p\)\)\n
\(let* \(nmtb-chk
       \(nmtb \(get-buffer-create \"*NAF-MODE-TEST*\"\)\)
       \(do-nmtb #'\(lambda \(no\) 
                    \(with-current-buffer nmtb 
                      \(if no \(fundamental-mode\) \(naf-mode\)\)
                      \(push \(mon-is-naf-mode-p \(get-buffer nmtb\)\) nmtb-chk\)\)\)\)\)
       \(progn 
         \(funcall do-nmtb nil\)
         \(funcall do-nmtb t\)
         \(with-current-buffer nmtb \(kill-buffer\)\)
         \(nreverse nmtb-chk\)\)\)\n
:CALLED-BY `mon-is-naf-mode-and-llm-p', and other functions which invoke
`mon-naf-mode-toggle-restore-llm' to test for active naf-mode before running
additional longlines-mode checks.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-buffer-longlines-mode-p', `mon-toggle-restore-llm'.\n►►►"
  (and (featurep 'naf-mode)
    (mon-buffer-check-major-mode 'naf-mode (or check-naf-buffer (current-buffer)))))
;;
;;; :TEST-ME (mon-is-naf-mode-p)
;;; :TEST-ME (with-temp-buffer (naf-mode) (mon-is-naf-mode-p))n
;;;
;; ,---- :UNCOMMENT-TO-TEST
;; | (let* (nmtb-chk
;; |        (nmtb (get-buffer-create "*NAF-MODE-TEST*"))
;; |        (do-nmtb #'(lambda (no) (with-current-buffer nmtb (if no (fundamental-mode) (naf-mode))
;; |                          (push (mon-is-naf-mode-p (get-buffer nmtb)) nmtb-chk)))))
;; |        (progn 
;; |          (funcall do-nmtb nil)
;; |          (funcall do-nmtb t)
;; |          (with-current-buffer nmtb (kill-buffer))
;; |          (nreverse nmtb-chk)))
;; `----

;;; ==============================
;;; :NOTE <Timestamp: #{2010-02-20T17:49:20-05:00Z}#{10076} - by MON KEY>
;;; Either this doesn't make any sense or we're not setting it (or both!).
;;; The only thing I can think of is this variable might be useful if conditionally
;;; set according to some hook (which doesn't yet exist) where we elect not to
;;; immediately drop into to llm.
;;; :CREATED <Timestamp: #{2009-09-08T13:18:17-04:00Z}#{09372} - by MON KEY>
(defvar *naf-mode-buffer-local-llm* nil
  "Test if `longlines-mode' is active in buffer.\n
Automatically becomes buffer-local whenever `naf-mode' initiated in buffer.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-is-naf-mode-p' `mon-is-naf-mode-and-llm-p'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-08T12:59:41-04:00Z}#{09372} - by MON KEY>
(defun mon-is-naf-mode-and-llm-p (&optional naf-buffer-name)
  "Test if buffer is in `naf-mode' and `longlines-mode' is enabled.\n
When NAF-BUFFER-NAME is non-nil chack it, signal an error if does not exist.
Default is `current-buffer'.\n
:EXAMPLE\n\n(mon-is-naf-mode-and-llm-p)\n
\(with-temp-buffer \(naf-mode\) \(mon-is-naf-mode-and-llm-p\)\)\n
\(let* \(nmtb-chk
       \(nmtb \(get-buffer-create \"*NAF-MODE-TEST*\"\)\)
       \(do-nmtb #'\(lambda \(no\) 
                    \(with-current-buffer nmtb 
                      \(if no \(fundamental-mode\) \(naf-mode\)\)
                      \(push \(mon-is-naf-mode-and-llm-p \(get-buffer nmtb\)\)
                               nmtb-chk\)\)\)\)\)
       \(progn 
         \(funcall do-nmtb nil\)
         \(funcall do-nmtb t\)
         \(with-current-buffer nmtb \(kill-buffer\)\)
         \(nreverse nmtb-chk\)\)\)\n
:CALLED-BY `mon-naf-mode-toggle-restore-llm' and other functions which invoke
to test for active naf-mode before evaluating body.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-toggle-restore-llm', `mon-naf-mode-toggle-restore-llm',
`mon-is-naf-mode-p'.\n►►►"
  ;; (let ((do-nbn 
  ;;        (if naf-buffer-name
  ;;            (if (and naf-buffer-name (get-buffer naf-buffer-name))
  ;;                (get-buffer naf-buffer-name)
  ;;              (error (concat ":FUNCTION `mon-is-naf-mode-and-llm-p' "
  ;;                             "-- arg NAF-BUFFER-NAME not a buffer: %s")
  ;;                     (current-buffer))))))
  ;;   (and (mon-is-naf-mode-p do-nbn) (mon-buffer-longlines-mode-p do-nbn))))
  (and (featurep 'naf-mode)
       (mon-is-naf-mode-p naf-buffer-name)
       (mon-buffer-longlines-mode-p naf-buffer-name)))
;;
;;; :TEST-ME (mon-is-naf-mode-and-llm-p)
;;; :TEST-ME (with-temp-buffer (naf-mode) (mon-is-naf-mode-and-llm-p))
;;;
;; ,---- :UNCOMMENT-TO-TEST
;; | (let* (nmtb-chk
;; |        (nmtb (get-buffer-create "*NAF-MODE-TEST*"))
;; |        (do-nmtb #'(lambda (no) 
;; |                     (with-current-buffer nmtb 
;; |                       (if no (fundamental-mode) (naf-mode))
;; |                       (push (mon-is-naf-mode-and-llm-p (get-buffer nmtb))
;; |                                nmtb-chk)))))
;; |        (progn 
;; |          (funcall do-nmtb nil)
;; |          (funcall do-nmtb t)
;; |          (with-current-buffer nmtb (kill-buffer))
;; |          (nreverse nmtb-chk)))
;; `----

 
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `iso-latin-1-approximation'
(defvar *iso-latin-1-approximation* nil 
  "An array mapping ISO-8859-1 characters to ASCII-characters.\n
:NOTE bound at loadtime with `mon-make-iso-latin-1-approximation-loadtime'.\n
:SEE-ALSO `mon-cln-iso-latin-1', `mon-make-iso-latin-1-approximation',
`mon-trans-cp1252-to-latin1'.\n►►►")
;;
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `make-iso-latin-1-approximation'
(defun mon-make-iso-latin-1-approximation ()
"Helper function for `mon-cln-iso-latin-1'.\n
:SEE-ALSO `*iso-latin-1-approximation*',`mon-make-iso-latin-1-approximation',
`mon-trans-cp1252-to-latin1', `mon-make-iso-latin-1-approximation-loadtime'.\n►►►"
  (setq *iso-latin-1-approximation* (make-vector 256 0))
  (loop for i from 0 to 127 
     do (aset *iso-latin-1-approximation* i i))
  (loop for i from 128 below 160 
     for c from 0 below 32 
     do (aset *iso-latin-1-approximation* i c))
  (loop for i from 160 to 255
     for c across (concat " !cL$Y|S\"Ca<--R\"o~23'uP.,1o>***?"
                          "AAAAAAECEEEEIIIITNOOOOOxOUUUUYPs"
                          "aaaaaaeceeeeiiiitnooooo/ouuuuypy")
     do (aset *iso-latin-1-approximation* i c))       
  *iso-latin-1-approximation*)

;;; ==============================
;;; :CHANGESET 2301
;;; :CREATED <Timestamp: #{2010-11-11T15:21:29-05:00Z}#{10454} - by MON KEY>
(defun mon-make-iso-latin-1-approximation-loadtime ()
  "Loadtime function binds variable `*iso-latin-1-approximation*'.\n
Variable bound with `mon-make-iso-latin-1-approximation'.\n
:SEE-ALSO `mon-cln-iso-latin-1'.\n►►►"
  (unless (and (intern-soft "*iso-latin-1-approximation*" obarray)
               (bound-and-true-p *iso-latin-1-approximation*))
    (mon-make-iso-latin-1-approximation)
    (message (concat ":FUNCTION `mon-make-iso-latin-1-approximation-loadtime' "
                     "-- bound variable `*iso-latin-1-approximation*' at loadtime"))))

;;
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-remove-accents'
(defun mon-cln-iso-latin-1 (string)
  "Replace in string all accented characters with an unaccented version.\n
This is done only for ISO-5581-1 characters. Return the modified string.\n
:SEE-ALSO `*iso-latin-1-approximation*', `mon-make-iso-latin-1-approximation',
`mon-trans-cp1252-to-latin1', `deftransmogrify', `mon-cln-mail-headers'
`mon-cln-csv-fields' `mon-cln-file-name-string' `mon-cln-up-colon'
`mon-cln-whitespace' `mon-cln-uniq-lines' `mon-cln-control-M'
`mon-cln-piped-list' `mon-delete-back-up-list' `mon-cln-iso-latin-1'.\n►►►"
  (unless *iso-latin-1-approximation* 
    (mon-make-iso-latin-1-approximation))
  (let ((result (make-string (length string) 0)))
    (loop for p from 0 below (length string)
          do 
          (aset result p (aref *iso-latin-1-approximation* 
                               (% (aref string p) 256))))
    result))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `deftranslation'
(defmacro deftransmogrify (table string language translated-string)
  "A transmogrifier.\n
:SEE `mon-transmogrify' for implementation details.\n
:SEE-ALSO `mon-cln-iso-latin-1'.\n►►►"
  `(progn
     (unless (and (boundp (quote ,table)) ,table)
       (setq ,table (make-vector 7 0)))
     (put (intern ,string ,table)
          ,language 
          (if (eq ,translated-string :idem) ,string ,translated-string))))
;;
(put 'deftransmogrify 'lisp-indent-function 2)
;;
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `localize'
;;; :HIS pjb-invoices.el.restore :WAS `invoice-strings'
;;; :SEE (URL `http://www.informatimago.com/develop/emacs/index.html')
(defun mon-transmogrify (table language string)
  "Lookup in TABLE the STRING, return the translated version for LANGUAGE.\n
:EXAMPLE\n\n\(defvar *transmog-ex* nil \"Localization data for this module.\"\)\n
\(mapcar \(lambda \(slt\) \(deftransmogrify *transmog-ex* \(nth 0 slt\) \(nth 1 slt\) \(nth 2 slt\)\)\)
	'\(\(\"Phone:\" :en :idem\)
	  \(\"Phone:\" :fr \"Téléphone :\"\)
	  \(\"Phone:\" :es \"Teléfono :\"\)
	  \(\"Téléphone :\" :fr :idem\)
	  \(\"Téléphone :\" :es \"Teléfono :\"\)
	  \(\"Téléphone :\" :en \"Phone:\"\)
	  \(\"Billing address:\" :en :idem\)
	  \(\"Billing address:\" :fr \"Adresse de facturation :\"\)
	  \(\"Billing address:\" :es \"Dirección de factura :\"\)
	  \(\"Dirección de factura :\" :es :idem\)
	  \(\"Dirección de factura :\" :fr \"Adresse de facturation :\"\)
	  \(\"Dirección de factura :\" :en \"Billing address:\"\)\)\)\n
\(mon-transmogrify *transmog-ex* :fr  \"Billing address:\"\)\n ;=>\"Adresse de facturation :\"
\(mon-transmogrify *transmog-ex* :es  \"Billing address:\"\)\n ;=>\"Dirección de factura :\"
\(mon-transmogrify *transmog-ex* :en  \"Dirección de factura :\"\)\n ;=>\"Billing address:\"
\(mon-transmogrify *transmog-ex*  :es  \"Phone:\" \)\n ;=>\"Teléfono :\"
\(mon-transmogrify *transmog-ex*  :fr  \"Phone:\" \)\n ;=>\"Téléphone :\"
\(mon-transmogrify *transmog-ex*  :en \"Téléphone :\"\)\n ;=>\"Phone:\"
Pascal Bourguignon's functions have extensive examples:
:SEE `invoice-strings' in :HIS
:FILE ../site-lisp/pjb/emacs-files/pjb-invoices.el.restore\n
:SEE-ALSO `deftransmogrify'.\n►►►"
  (let ((sym (intern-soft string table)))
    (if sym (let ((result (get sym language))) 
              (if result 
                  result
                  (mon-transmogrify table :en string)))
        string)))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-11T19:30:38-05:00Z}#{10105} - by MON KEY>
(defun mon-walk-regexps-in-file (w-fl w-rep bfr-mrkr)
  "Helper function for `mon-replace-regexps-in-file-list'.\n
W-FL is a file to find.\n
W-REP is a list of regexp/replace pairs.\n
BFR-MRKR is a buffer marker to print match/replace logs to.\n
:SEE-ALSO .\n►►►"
  (let ((the-rg w-rep)
        (map-rg #'(lambda (rg-list bmrk)
                    (let (this-rep-cnt)
                      (mapc #'(lambda (this-rgx)
                                (mon-g2be -1)
                                (setq this-rep-cnt 0)
                                (while (search-forward-regexp (car this-rgx) nil t)
                                  (incf this-rep-cnt)
                                  (replace-match (cadr this-rgx)))
                                (when (> this-rep-cnt 0)
                                  (princ (format "\n\n:%d-TIMES\n:W-REGEXP %S\n:REPLACED %S"
                                                 this-rep-cnt (car this-rgx) (cadr this-rgx)) bmrk)))
                            rg-list)))))
    (find-file w-fl)
    (mon-g2be -1)
    (funcall map-rg the-rg bfr-mrkr)))

;;; ==============================
;;; :PREFIX "mrrifl-"
;;; :CREATED <Timestamp: #{2010-03-11T19:30:29-05:00Z}#{10105} - by MON KEY>
(defun mon-replace-regexps-in-file-list (file-list regexp-list)
  "Replace in FILE-LIST the match/replace pairs in REGEXP-LIST.\n
FILE-LIST is a list of files to perform replacements in. It has the form:\n
 \(\"<FILENAME>\" \"<FILENAME1>\" \"<FILENAME2>\"\)\n
REGEXP-LIST is a list of match replace pairs with the form:\n
 \(\(<REGEXP-STRING>  <REPLACE-STRING>)
  \(<REGEXP-STRING1> <REPLACE-STRING1>)
  \(<REGEXP-STRING2> <REPLACE-STRING2>)\)\n
Display a log of replacementsin the buffer named `*REGEXP-REPLACE-HISTORY*'.\n
Files in FILE-LIST are visited as with `find-file' and are not written to disk
they must be saved manually.\n
:NOTE Don't forget; when replacing variabless with leading and trailing
asterisks \(e.g. `*some-var*'\) asterisks is escaped with two backslashes.
IOW do this:\n
 \"\\\\*some-var\\\\*\"\n
This is an aggressive procedure; be careful looping over large file-sets
with poorly formed regexps -- consider invoking this procedure on backups
first.\n
:SEE-ALSO `mon-walk-regexps-in-file', `mon-get-file-mod-times'.\n►►►"
  (let ((mrrifl-fl file-list)
        (mrrifl-rl regexp-list)
        (mrrifl-rep-hst (get-buffer-create "*REGEXP-REPLACE-HISTORY*"))
        (mrrifl-rrh-mrk (make-marker))
        (mrrifl-fl-mod #'(lambda (flmod) (concat "\n\n" (make-string 68 59) "\n"
                                                 (mon-get-file-mod-times flmod)
                                                 "\n" (make-string 68 59)))))
    (with-current-buffer mrrifl-rep-hst
      (set (make-local-variable 'comment-start) ";;")
      (erase-buffer)  
      (mon-g2be -1)
      (princ (concat ":INVOCATION-TIME " (mon-format-iso-8601-time) "\n\n"
                     ":WITH-REGEXP-LIST\n" (pp-to-string mrrifl-rl) "\n" 
                     ":IN-THESE-FILES\n" (mapconcat #'identity mrrifl-fl "\n") 
                     "\n") (current-buffer))
      (set-marker mrrifl-rrh-mrk (point))
      ;; :WAS (comment-region (buffer-end 0) (marker-position mrrifl-rrh-mrk)))
      (comment-region (mon-g2be -1 t) (marker-position mrrifl-rrh-mrk)))
    (mapc #'(lambda (mrrifl-wlk-in-fl)
              (princ (funcall mrrifl-fl-mod mrrifl-wlk-in-fl) mrrifl-rrh-mrk)
              (mon-walk-regexps-in-file mrrifl-wlk-in-fl mrrifl-rl mrrifl-rrh-mrk))
          mrrifl-fl)
    (display-buffer mrrifl-rep-hst t)))

 
;;; ==============================
;;; :PREFIX "mrs-"
;;; :COURTESY :FILE format.el
;;; :CREATED <Timestamp: #{2009-08-20T16:58:13-04:00Z}#{09344} - by MON KEY>
(defun mon-replace-strings (replacing-alist &optional w-alist-reversed beg end)
  "Do multiple replacements in *<BUFFER>*.\n
REPLACING-ALIST is a list of \(FROM . TO\) pairs, which should be proper arguments to
`search-forward' and `replace-match', respectively.\n
When W-ALIST-REVERSED is non-nil the pairs are \(TO . FROM\), which allows use of the same
list in both directions if it contains only literal strings. 
Optional args BEG and END specify a region of the buffer on which to operate.\n
:SEE-ALSO `mon-replace-regexp-while', `mon-replace-regexps-in-file-list'.\n►►►"
  (save-excursion
    (save-restriction
      (let ((mrs-beg beg)
            (mrs-end end))
        (or mrs-beg (setq mrs-beg (mon-g2be -1 t)))
        (if mrs-end (narrow-to-region (mon-g2be -1 t) mrs-end))
        (while replacing-alist
          (let ((mrs-rep-frm (if w-alist-reversed 
                                 (cdr (car replacing-alist)) 
                               (car (car replacing-alist))))
                (mrs-rep-to   (if w-alist-reversed 
                                  (car (car replacing-alist)) 
                                (cdr (car replacing-alist)))))
            (goto-char mrs-beg)
            (while (search-forward mrs-rep-frm nil t)
              (goto-char (match-beginning 0))
              (insert mrs-rep-to)
              (set-text-properties (- (point) (length mrs-rep-to)) 
                                   (point) (text-properties-at (point)))
              (delete-region (point) (+ (point) (- (match-end 0) (match-beginning 0)))))
            (setq replacing-alist (cdr replacing-alist))))))))
  
;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :WAS `clsc-replace-regexp' -> `mon-replace-regexp-while'
(defun mon-replace-regexp-while (w-regexp to-string)
  "Like `replace-regexp', except be silent about it.\n
:SEE-ALSO `mon-replace-string-while'.\n►►►"
  (while (search-forward-regexp w-regexp nil t)
    (replace-match to-string nil nil)))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :WAS `clsc-replace-string' -> `mon-replace-string-while'
(defun mon-replace-string-while (from-string to-string)
  "Like `replace-string', except be silent about it.\n
:SEE-ALSO `mon-replace-regexp-while'.\n►►►"
  (while (search-forward-regexp from-string nil t)
    (replace-match to-string nil t)))

;;; ==============================
;;; :PREFIX "mrf-"
(defun mon-regexp-filter (w-regexp w-filter-lst)
  "Filter W-FILTER-LST of strings W-REGEXP return filtered list.\n
:EXAMPLE\n\(mon-regexp-filter  \"en\"
 \'\(\"one\" \"two\" \"three\" \"four\" \"five\"
   \"six\" \"seven\" \"eight\" \"nine\" \"ten\"\)\)\n
:SEE-ALSO `filter-buffer-substring'.\n►►►"
      (let (mrf-new)
	(dolist (mrf-str w-filter-lst)
	  (when (string-match-p w-regexp mrf-str)
	    (setq mrf-new (cons mrf-str mrf-new))))
	(nreverse mrf-new)))


(declare-function mon-list-proper-p "mon-utils")
;;; ==============================
;;; :COURTESY Xah Lee :WAS `canonicalString' Loosely modeled on the function here:
;;; :SEE (URL `http://xah-forum.blogspot.com/2009_03_08_archive.html')
;;; :PREFIX "mcs-"
;;; :MODIFICATIONS Abstracted to handle lists vectors, conses, in any combinations.
;;; :CHANGESET 2144 <Timestamp: #{2010-09-22T14:20:50-04:00Z}#{10383} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday April 29, 2009 @ 12:49.37 PM - by MON KEY>
(defun mon-string-canonical (canonize-string canon-table)
  "Return the canonical form of CANONIZE-STRING per a lookup in CANON-TABLE.\n
CANON-TABLE should be a list or vector with each element a tuple.\n
First element of each tuple is a regexp.\n
Second element is a string returned if the regexp satisfies `string-match-p'.\n
Tuples should contain at most two elements but may be formatted as a, vector,
proper list, or dotted pair.\n
Any combination of one or more tuple types is possible.\n
For example the follow are all valid forms:\n
 [[\"a\"   \"α\"] [\"γ\"   \"g\"] [\"b\"   \"β\"] { ... } ]
 [\(\"a\"   \"α\"\) \(\"γ\"   \"g\"\) \(\"b\"   \"β\"\) { ... } ]
 [\(\"a\" . \"α\"\) \(\"γ\"   \"g\"\) \(\"b\" . \"β\"\) { ... } ]
 [[\"a\"   \"α\"] \(\"γ\"   \"g\"\) \(\"b\" . \"β\"\) { ... } \)
 \(\(\"a\"   \"α\"\) \(\"γ\"   \"g\"\) \(\"b\"   \"β\"\) { ... } \)
 \(\(\"a\" . \"α\"\) \(\"γ\" . \"g\"\) \(\"b\" . \"β\"\) { ... } \)
 \([\"a\"   \"α\"] [\"γ\"   \"g\"] [\"b\"   \"β\"] { ... } \)
 \([\"a\"   \"α\"] \(\"γ\" . \"g\"\) \(\"b\"   \"β\"\) { ... } \)\n
:EXAMPLE\n\n\(mon-string-canonical \"b\"  [[\"a\"   \"α\"] [\"γ\"   \"g\"] [\"b\"   \"β\"]]\)\n
\(mon-string-canonical \"β\"  [\(\"a\"   \"α\"\) \(\"γ\"   \"g\"\) \(\"β\"   \"b\"\)]\)\n
\(mon-string-canonical \"g\"  [\(\"a\" . \"α\"\) \(\"g\"   \"γ\"\) \(\"b\" . \"β\"\)]\)\n
\(mon-string-canonical \"a\"  [[\"a\"   \"α\"] \(\"γ\"   \"g\"\) \(\"b\" . \"β\"\)\]\)\n
\(mon-string-canonical \"b\" '\(\(\"a\"   \"α\"\) \(\"γ\"   \"g\"\) \(\"b\"   \"β\"\)\)\)\n
\(mon-string-canonical \"g\" '\(\(\"a\" . \"α\"\) \(\"g\" . \"γ\"\) \(\"b\" . \"β\"\)\)\)\n
\(mon-string-canonical \"β\" '\([\"a\"   \"α\"] [\"γ\"   \"g\"] [\"β\"   \"b\"]\)\)\n
\(mon-string-canonical \"α\" '\(\(\"γ\" . \"g\"\) \(\"b\"   \"β\"\) [\"α\"   \"a\"]\)\)\n
:ALIASED-BY `mon-canonical-string'\n
:SEE-ALSO `mon-regexp-filter', `filter-buffer-substring'.\n►►►"
  (let ((mcs-tbl-type (type-of canon-table))
        (mcs-itm-cnt (length canon-table))
        (mcs-idx-cnt 0)
        mcs-did-mtch
        mcs-elt-at-idx
        mcs-elt-idx-typ
        mcs-rslt)
    (while (and (not mcs-did-mtch) (<= mcs-idx-cnt mcs-itm-cnt))
      (setq mcs-elt-at-idx (case mcs-tbl-type
                             (vector  (aref canon-table mcs-idx-cnt))
                             (cons    (elt canon-table mcs-idx-cnt)))
            mcs-elt-idx-typ (case (type-of mcs-elt-at-idx)
                              (vector `(,(aref mcs-elt-at-idx 0) . ,(aref mcs-elt-at-idx 1)))
                              (cons `(,(car mcs-elt-at-idx) . ;Is it a dotted list?
                                      ,(if (mon-list-proper-p mcs-elt-at-idx)
                                           (cadr mcs-elt-at-idx)
                                         (cdr mcs-elt-at-idx))))))
      (when (string-match-p (car mcs-elt-idx-typ)  canonize-string)        
        (setq mcs-did-mtch t)
        (setq mcs-rslt (cdr mcs-elt-idx-typ)))
      (incf  mcs-idx-cnt)
      (setq  mcs-elt-at-idx)
      (setq  mcs-elt-idx-typ))
    mcs-rslt))
;;
;;; :TEST-ME (mon-string-canonical "b"  [["a"   "α"] ["γ"   "g"] ["b"   "β"]])
;;; :TEST-ME (mon-string-canonical "β"  [("a"   "α") ("γ"   "g") ("β"   "b")])
;;; :TEST-ME (mon-string-canonical "g"  [("a" . "α") ("g"   "γ") ("b" . "β")])
;;; :TEST-ME (mon-string-canonical "a"  [["a"   "α"] ("γ"   "g") ("b" . "β")])
;;; :TEST-ME (mon-string-canonical "b" '(("a"   "α") ("γ"   "g") ("b"   "β")))
;;; :TEST-ME (mon-string-canonical "g" '(("a" . "α") ("g" . "γ") ("b" . "β")))
;;; :TEST-ME (mon-string-canonical "β" '(["a"   "α"] ["γ"   "g"] ["β"   "b"]))
;;; :TEST-ME (mon-string-canonical "α" '(("γ" . "g") ("b"   "β") ["α"   "a"]))

 
;;; ==============================
;;; :COURTESY Xah Lee :SEE (URL `http://www.xahlee.org')
;;; :MODIFICATIONS `search-forward'   -> `search-forward-regexp'
;;;                `buffer-substring' -> `buffer-substring-no-properties'
;;; :PREFIX "rspr3-"
(defun replace-string-pairs-region3 (start end pairs-replace-lst)
  "Replace string in region with each elt's string pairs in PAIRS-REPLACE-LST.\n
The car of PAIRS-REPLACE-LST is the target string cadr is the replacement string.
The cadr can be a subexp to replace with.\n
NOTE: To clean discarding text-properties use:
 `replace-string-pairs-region-no-props'.\n
:EXAMPLE\n\n(replace-string-pairs-region3 start end 
 '((\"alpha\" \"A\") (\"beta\" \"B\")))\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `replace-string-pairs-region', `mon-replace-region-regexp-lists'
`mon-replace-region-regexp-lists-nonint', `mon-replace-regexps-in-file-list'.\n►►►"
  ;; :WAS (let ((rspr3-str (buffer-substring-no-properties start end)))
  (let ((rspr3-str (mon-buffer-sub-no-prop start end)))
    ;;  (setq rspr3-str (buffer-substring start end))
    ;;    (setq rspr3-str (filter-buffer-substring start end nil t)) 
    ;; (setq rspr3-str (buffer-substring-no-properties start end))
    (save-excursion
      (setq rspr3-str
            (with-temp-buffer
              (insert rspr3-str)
              (mapc #'(lambda (rspr3-arg)
                        (mon-g2be -1)
                        (while (search-forward-regexp (car rspr3-arg) nil t) 
                          (replace-match (cadr rspr3-arg) t t) ))
                    pairs-replace-lst)
              (buffer-string))))
    (delete-region start end)
    (insert rspr3-str)))

;;; ==============================
;;; :PREFIX "rsprnp-"
(defun replace-string-pairs-region-no-props (start end region-pairs-replace-lst) 
  "Replace string pairs of REGION-PAIRS-REPLACE-LST in region.\n
Do not retain text properties.\n Search string and replace string are literal.\n
The car of REGION-PAIRS-REPLACE-LST is a target string. cadr is its replacement.
The cadr may also be a subexp to replace with.\n
:EXAMPLE\n\n\(replace-string-pairs-region-no-props start end\n
 '((\"alpha\" \"A\") (\"beta\" \"B\")))\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `replace-string-pairs-region',
`mon-replace-string-pairs-region-no-insert', `mon-replace-region-regexp-lists',
`mon-replace-region-regexp-lists-nonint'.\n►►►"
  (let (rsprnp-str)
    ;; :WAS (setq rsprnp-str (buffer-substring-no-properties start end))
    (setq rsprnp-str (mon-buffer-sub-no-prop start end))
    (save-excursion
      (setq rsprnp-str
            (with-temp-buffer
              (insert rsprnp-str)
              (mapc #'(lambda (rsprnp-arg)
                        (mon-g2be -1) ;; :WAS (goto-char (buffer-end 0))
                        (while (search-forward-regexp (car rsprnp-arg) nil t) 
                          (replace-match (cadr rsprnp-arg) t t) ))
                    region-pairs-replace-lst)
              (buffer-string))))
    (delete-region start end)
    (insert rsprnp-str)))

;;; ==============================
;;; :PREFIX "mrsprni-"
;;; :CREATED <Timestamp: #{2009-09-24T12:55:24-04:00Z}#{09394} - by MON KEY>
(defun mon-replace-string-pairs-region-no-insert (start end str-pairs-replace-lst) 
  "Return replace string pairs in region.\n
Does not retain text properties. Does not insert results.
Search string and replace string are literal.
car of STR-PAIRS-REPLACE-LST is the target string cadr is the replacement string.
cadr can be a subexp to replace with.\n
:EXAMPLE\n\(mon-replace-string-pairs-region-no-insert
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"►\"\) 2\)
 '((\"^alpha\" \"A\") (\"^beta\" \"B\") (\"^delta\" \"D\") (\"^epsilon\" \"E\")\)\)\n
►\nalpha\nbeta\ndelta\nepsilon\n►\n
:SEE-ALSO `replace-string-pairs-region', `mon-replace-region-regexp-lists',
`mon-replace-region-regexp-lists-nonint'.\n►►►"
  (let (mrsprni-str)
    ;; :WAS (setq mrsprni-str (buffer-substring-no-properties start end))
    (setq mrsprni-str (mon-buffer-sub-no-prop start end))
    (save-excursion
      (setq mrsprni-str
            (with-temp-buffer
              (insert mrsprni-str)
              (mapc #'(lambda (mrsprni-L-1)
                        (mon-g2be -1)
                        (while (search-forward-regexp (car mrsprni-L-1) nil t) 
                          (replace-match (cadr mrsprni-L-1) t t)))
                    str-pairs-replace-lst)
              (mon-buffer-sub-no-prop) )))
    mrsprni-str))
;;
;;; :TEST-ME 
;;; (mon-replace-string-pairs-region-no-insert
;;;  (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2)
;;;  '(("^alpha" "A") ("^beta" "B") ("^delta" "D") ("^epsilon" "E")))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | ►
;; | alpha 
;; | beta
;; | delta
;; | epsilon
;; | ►
;; `----

;;; ==============================
;;; :TODO This fncn should be rewritten/replaced
;;; :PREFIX "mgly-"
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-get-list-yorp ()
  "Template for accumulating a list from symbols holding lists.\n
:NOTE Originally a help function to interactively pass symbol bound regexp lists
 as invoked by `mon-replace-region-regexp-lists'.\n
:SEE-ALSO `mon-get-list-norp', `mon-replace-region-regexp-lists-nonint'.\n►►►"
  (interactive)
  ;; :NOTE Why the hell is this calling `eval'? 
  (let* ((mgly-rd-lst 
          (eval (read-from-minibuffer "Give Symbol holding list: " nil nil t))))
    (while (yes-or-no-p "Enter another list: ")
      (let* ((mgly-tmp-lst mgly-rd-lst)
             (mgly-read 
              (eval (read-from-minibuffer "Give Symbol holding list: " nil nil t))))
        (setq mgly-rd-lst (append mgly-tmp-lst (mapc #'car mgly-read)))))
    mgly-rd-lst))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-get-list-norp (get-list-a w-args) ;;&rest w-args)
  "Template form accumulating get-list-a list from symbols holding lists.\n
:NOTE Originally a help function to interactively pass symbol bound regexp
lists at invocation. Body is now incorporated in:
`mon-replace-region-regexp-lists-nonint'.\n
:SEE-ALSO `mon-get-list-yorp', `mon-replace-region-regexp-lists'.\n►►►"
  (let ((head-norp get-list-a)
        (tail-norp w-args))
    (while tail-norp
      (setq head-norp (append head-norp (car tail-norp)))
      (setq tail-norp (cdr tail-norp)))
    head-norp))

;;; ==============================
;;; :PREFIX "mrrrln-"
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.30 PM - by MON KEY>
(defun mon-replace-region-regexp-lists-nonint (start end head-lst &rest norp-rest)
  "Non-interactive version of `mon-replace-region-regexp-lists'.
Used as a helper function to search over symbol bound regexp lists.\n
:EXAMPLE\n\(defun hah \(start end\) \(interactive \"r\"\)
\(mon-replace-region-regexp-lists-nonint test-aaa test-bbb test-ccc test-ddd\)\)\n
:SEE-ALSO `mon-get-list-yorp', `mon-get-list-norp'.\n►►►"
  (let* ((mrrrln-bnds `(,start . ,end))
	 (mrrrln-lst  (mon-get-list-norp head-lst norp-rest))
	 (mrrrln-rep-hd  (mapcar #'car mrrrln-lst))
	 (mrrrln-rep-tl (mapcar #'cadr mrrrln-lst))
         mrrrln-rep-cnt
         mrrrln-rep-rgn)
    (mon-naf-mode-toggle-restore-llm nil
      (setq mrrrln-rep-rgn (mon-buffer-sub-no-prop start end)) 
      (setq mrrrln-rep-rgn 
            (with-temp-buffer
              (insert mrrrln-rep-rgn)
              (mon-g2be -1)
              (setq mrrrln-rep-cnt 0)
              (while mrrrln-rep-hd
                (while (search-forward-regexp (car mrrrln-rep-hd) nil t)
                  (replace-match (car mrrrln-rep-tl))
                  (setq mrrrln-rep-cnt (+ mrrrln-rep-cnt 1)))
                ;;(message "Replaced regexp \'%s\' %d times with \'%s\'\n"
                ;;           (car mrrrln-rep-hd) mrrrln-rep-cnt (car mrrrln-rep-tl)))
                (when (not (search-forward-regexp (car mrrrln-rep-hd) nil t))
                  (setq mrrrln-rep-cnt 0)
                  (setq mrrrln-rep-hd (cdr mrrrln-rep-hd))
                  (setq mrrrln-rep-tl (cdr mrrrln-rep-tl))
                  (mon-g2be -1) ))
              (buffer-string)))
      (delete-region start end)
      (insert mrrrln-rep-rgn))))

;;; ==============================
;;; :PREFIX "mrrrl-"
;;; :MODIFICATIONS <Timestamp: #{2009-08-31T12:12:52-04:00Z}#{09361} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-replace-region-regexp-lists (start end &optional regexp-list with-results intrp)
  "Interactive version of `mon-replace-region-regexp-lists-nonint'.\n
Prompt for args of symbol bound regexp lists. 
Replace elts of REGEXP-LIST a symbol holding a list of regexp/replace pairs.
Forms of symbol are searched across region until elements of supplied lists
are exhausted. When WITH-RSULTS is non-nil spit replacement results for each
elt of REGEXP-LIST to *Messages*.\n
:SEE-ALSO `mon-get-list-yorp', `mon-get-list-norp', `mon-replace-regexps-in-file-list'.\n►►►"
  (interactive "r\n\i\nP\np")
  (let* (;; mrrrl-rep-rgn
	 mrrrl-rep-rgn-tmp
	 mrrrl-rep-cnt
	 (mrrrl-lst (if intrp 
                        ;; Why not (apply 'mon-get-list-yorp ()) or 
                        ;;         (funcall 'mon-get-list-yorp) ??
                        ;; :WAS (call-interactively 'mon-get-list-yorp)
                        (funcall 'mon-get-list-yorp)
                      regexp-list))
	 (mrrrl-rep-rgn (mon-buffer-sub-no-prop start end))
         (mrrrl-w/rslts with-results)
         mrrrl-rep-msg)
         (save-excursion 
           (setq mrrrl-rep-rgn-tmp
                 (with-temp-buffer
                   (let ((mrrrl-rep-hd (mapcar #'car mrrrl-lst))
                         (mrrrl-rep-tl (mapcar #'cadr mrrrl-lst)))
                     (setq mrrrl-rep-cnt 0)
                     (insert mrrrl-rep-rgn)
                     (mon-g2be -1)
                     (while mrrrl-rep-hd
                       (if (search-forward-regexp (car mrrrl-rep-hd) nil t)
                           (progn
                             (replace-match (car mrrrl-rep-tl))
                             (setq mrrrl-rep-cnt (1+ mrrrl-rep-cnt)))
                         (progn 
                           (when mrrrl-w/rslts (setq mrrrl-rep-msg 
                                                     (cons
                                                      (format "Replaced regexp \'%s\' -> \'%s\' %d times.\n"
                                                              (car mrrrl-rep-hd) (car mrrrl-rep-tl) mrrrl-rep-cnt)
                                                      mrrrl-rep-msg)))
                           (setq mrrrl-rep-cnt 0)
                           (setq mrrrl-rep-hd (cdr mrrrl-rep-hd))
                           (setq mrrrl-rep-tl (cdr mrrrl-rep-tl))
                           (mon-g2be -1))))
                     (mon-buffer-sub-no-prop))))
           (delete-region start end)
           (insert mrrrl-rep-rgn-tmp))
    (when mrrrl-w/rslts (setq mrrrl-rep-msg (mapconcat #'identity mrrrl-rep-msg ""))
          (cond (intrp (message "%s" mrrrl-rep-msg))
                ((not intrp) (format "%s" mrrrl-rep-msg))))))

 
;;; ==============================
;;; :PREFIX "mrmm-"
;;; :CREATED <Timestamp: #{2010-02-22T18:50:45-05:00Z}#{10081} - by MON KEY>
(defun mon-regexp-map-match (big-regexp big-grp-start big-grp-end)
  "Return a mapped list of conses for match groups and mapstrings.\n
BIG-REGEXP is a dense regexp to search.
BIG-GRP-START is the match-group to map from.
BIG-GRP-END is the match-group to map from.\n
:EXAMPLE\n\n\(save-excursion
  \(let \(\(rsd *regexp-symbol-defs*\)
        rsd-collect\)
    \(dotimes \(r 3 \(setq rsd-collect \(nreverse rsd-collect\)\)\)
      \(push \(mon-regexp-map-match rsd 0 \(regexp-opt-depth rsd\)\) rsd-collect\)\)\)\)\n\n
\(defun some-function \(&optional optional\)
\(defun some-function-22 \(&optional optional\)
\(defun *some/-symbol:->name<-2* \(somevar\n
:ALIASED-BY `mon-map-regexp-matches'\n
:SEE-ALSO `mon-regexp-map-match-in-region', `mon-walk-regexps-in-file'.\n►►►"
  (progn (search-forward-regexp big-regexp nil t)
         (mapcar #'(lambda (mrmm-L-1)
                   (cons mrmm-L-1 (match-string-no-properties mrmm-L-1)))
               (number-sequence big-grp-start big-grp-end))))
;;
;;
;;; :NOTE Not a realistic test example. But a nice loop so keeping.
;;; (let* ((big-string (documentation 'lisp-interaction-mode))
;;;        (split-times (/ (length big-string) 100))
;;;        split-str)
;;;   (do* ((i 0 (1+ i))
;;;         (j i (* i 100))
;;;         (k 100 (+ j 99)))
;;;        ((>= i (1- split-times))
;;;         (setq split-str 
;;;               (mapconcat #'identity  
;;;                          (mapcar #'(lambda (sp-st) 
;;;                                      (let ((tst-str (regexp-opt (split-string sp-st nil t))))
;;;                                        (if (string= (substring tst-str 0 4) "\\(?:")
;;;                                            (setq tst-str (concat "\\(" (substring tst-str 4)))
;;;                                            tst-str)))
;;;                                  (nreverse split-str)) "")))
;;;     (push (substring big-string j k) split-str)))

;;; ==============================
;;; :PREFIX "mrmmir-"
;;; :CREATED <Timestamp: #{2010-03-01T17:05:04-05:00Z}#{10091} - by MON KEY>
(defun mon-regexp-map-match-in-region (start end w-regexp &optional no-new-buffer)
  "Map the regexp W-REGEXP for each line in region START to END.\n
W-REGEXP is a symbol holding a regexp.\n
Return value is as per `mon-regexp-map-match' e.g. a list of match groups.\n
Return value is displayed in the buffer named \"*MON-REGEXP-MAP-MATCH-RESULTS*\".
When optional arg NO-NEW-BUFFER in non-nil and not called-interactively
insert return value in current-buffer at END or region. Does not move point.\n
:EXAMPLE\n
\(let \(\(mrm-wr-eg ;; regexp from `naf-mode-db-numbers-flag'
       \(concat 
        \"\\\\\(\\\\\(\\\\\(FRBNF\\\\\)\"              ; <- grp1, grp2, grp3
        ;;^1^^2^^3^^^^^^^^
        \"\\\\|\\\\\(\\\\\(n\\\\|n\\\\.\\\\|no\\\\|no\\\\.\\\\\)[ ?]\\\\\)\" ; <- grp4 & grp5
        ;;^^^^4^^5^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        \"\\\\|\\\\\(\\\\\\=[\\\\\)\\\\\)\"               ; <- grp6
        ;;^^^^6^^^^^^^^
        \"\\\\\([0-9]\\\\\\={8,10\\\\}\\\\\(]?\\\\\)\"    ; <- grp7 & grp8
        ;;^7^^^^^^^^^^^^^^^^^8^^^^^
        \"\\\\\)\"                           ; <- grp2 :CLOSE
        \"\\\\\)\"                           ; <- grp1 :CLOSE
        \"\\\\|\\\\\\=<[0-9\]\\\\\\={8,10\\\\}\\\\>\"\)\)
      \(mrm-bnd \(nth 1 \(mapcar 'cadr \(mon-help-delimited-region t\)\)\)\)\)
      \(mon-regexp-map-match-in-region \(car mrm-bnd\) \(cdr mrm-bnd\) mrm-wr-eg\)\)\n
►\n80126308\nno. 80126308\nn. 80126308\nno 94031775\nn 2005065776
unk84240548\n\[500006383]\nFRBNF12656015\nFRBNF32759170\n◄\n
:ALIASED-BY `mon-map-regexp-matches-in-region'\n
:SEE-ALSO `mon-walk-regexps-in-file', `mon-replace-regexps-in-file-list'.\n►►►"
  (interactive (list (if (use-region-p) 
                         (region-beginning)
                       (error (concat ":FUNCTION `mon-regexp-map-match-in-region' "
                                      "-- region not active")))
                     (if (use-region-p) 
                         (region-end)
                       (error (concat ":FUNCTION `mon-regexp-map-match-in-region' " 
                                      "-- region not active")))
                     (let ((shr-mbr '("symbol-holding-regexp" "mini-buffer-read")))
                       (if (equal (completing-read 
                                   "Use symbol-holding-regexp or mini-buffer-read: " shr-mbr nil t)
                                  "symbol-holding-regexp")
                           (symbol-value (car (read-from-string (read-string "Which symbol: "))))
                         (read-regexp "Provide a regexp: ")))
                     nil))
  (eval-when-compile (require 'boxquote)) ;; boxquote-text
  (let* ((mrmmir-rgx w-regexp)
         (mrmmir-opt (regexp-opt-depth mrmmir-rgx))
         mrmmir-gthr
         mrmmir-cnt)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (setq mrmmir-cnt (count-lines start end))
        (goto-char start)
        (dotimes (r mrmmir-cnt)
          (push `(,(car (read-from-string (format ":REGEXP-ITER-%d" r)))
                  ,(mon-regexp-map-match mrmmir-rgx 0 mrmmir-opt)) mrmmir-gthr))))
    (setq mrmmir-gthr (nreverse mrmmir-gthr))
    (with-temp-buffer
      (prin1 mrmmir-gthr (current-buffer))
      (pp-buffer)
      (setq mrmmir-gthr (mon-buffer-sub-no-prop)))
    (if no-new-buffer
        (save-excursion (goto-char end)
                        (newline)
                        (princ mrmmir-gthr (current-buffer)))
      (let ((mrmmir-cur-bfr (buffer-name (current-buffer)))
            (mrmmir-rgn  (mon-buffer-sub-no-prop start end))
            (mrmmir-rslt-bfr (get-buffer-create "*MON-REGEXP-MAP-MATCH-RESULTS*")))
        (with-temp-buffer 
          (boxquote-text mrmmir-rgn)
          (let ((comment-start ";")
                (comment-padding 1))
            (comment-region (mon-g2be -1 t)(mon-g2be 1 t) 2))
          (setq mrmmir-rgn (buffer-substring (mon-g2be -1 t)(mon-g2be 1 t))))
        (with-current-buffer mrmmir-rslt-bfr
          (erase-buffer)
          (princ (format (concat
                          ";; :MAPPING-RESULTS-W-REGEXP\n;;  %s\n;; \n"
                          ";; :MAPPED-RESULTS-N-TIMES %d\n;;\n"
                          ";; :IN-BUFFER\n;;  %s\n;; \n"
                          ";; :WITH-REGION \n;; \n"
                          "%s\n\n%s")
                         w-regexp mrmmir-cnt mrmmir-cur-bfr mrmmir-rgn mrmmir-gthr)
                 (current-buffer))
          (mon-g2be -1)
          (emacs-lisp-mode)
          (display-buffer mrmmir-rslt-bfr t))))))

 
;;; ==============================
;;; :CHANGESET 2214
;;; :CREATED <Timestamp: #{2010-10-26T16:39:05-04:00Z}#{10432} - by MON KEY>
(defvar *mon-regexp-unintern* (concat "\\((unintern\\)" ;grp1
                                      "\\([[:space:]]+\\)" ;grp2
                                      "\\('\\)"  ;grp3
                                      ;; :WAS "\\(\\*+?.+*\\*+?\\)" ;grp-4 
                                      "\\([^'][+*]?[A-z0-9-]+?[/:]?[A-z0-9-]+?[+*]?\\)" ;grp4
                                      "\\()\\)" ;grp-5   
                                      )
  "Regexp for use with `mon-replace-unintern-w-query'.\n
group 1 is \"(unintern\"\n
group 2 is any number of whitespace chars.\n
group 3 is a quote \"'\" (char 39) which precedes an asterisk prefixed variable\n
group 4 matches variables names \(including those wrapped with `*`, `+`, and `--'\)\n
group 5 is the closing paren which must immediately follow the variable.\n
 \"\\\\\(\(unintern\\\\\)\\\\\([[:space:]]+\\\\\)\\\\\('\\\\\)\\\\\([^'][+*]?[A-z0-9-]+?[/:]?[A-z0-9-]+?[+*]?\\\\\)\\\\\(\)\\\\\)\"
  ^^1^^^^^^^^^^^^^^2^^^^^^^^^^^^^^^^^3^^^^^^4^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^5^^^^\n
:EXAMPLE\n\n\(save-excursion \(search-forward-regexp *mon-regexp-unintern*\) 
                \(match-string-no-properties 0\)\)\n
\(unintern   '*some-earmuffed-variable*\)\n
:NOTE The property `:last-mark` is used to store the final position in buffer on
the most recent invocation of `mon-replace-unintern-w-query'.\n
\(symbol-plist '*mon-regexp-unintern*\)\n
\(get '*mon-regexp-unintern* :last-mark\)\n
:SEE-ALSO .\n►►►")
;;
(if (and (intern-soft "*mon-regexp-unintern*" obarray)
         (bound-and-true-p *mon-regexp-unintern*))
    (if (symbol-plist '*mon-regexp-unintern*)
        (put  '*mon-regexp-unintern* :last-mark nil)
      (setplist '*mon-regexp-unintern* '(:last-mark nil)))
  (intern "*mon-regexp-unintern*" obarray)
  (setplist '*mon-regexp-unintern* '(:last-mark nil)))
;;
;;; :TEST-ME (get '*mon-regexp-unintern* :last-mark)
;;; :TEST-ME (symbol-plist '*mon-regexp-unintern*)

;; ,----
;; | :WITH-REGEXP
;; | "\\((unintern\\)\\([[:space:]]+\\)\\('\\)\\([^'][+*]?[A-z0-9-]+?[/:]?[A-z0-9-]+?[+*]?\\)\\()\\)"
;; |
;; | (let ((mtch-cnt 0))
;; |   (while (search-forward-regexp *mon-regexp-unintern* nil t)
;; |     (incf mtch-cnt))
;; |   (= mtch-cnt 14))
;; | 
;; | 
;; | (unintern '--bub:Ba8*)  ;; matches 1
;; | (unintern 'bubba)       ;; matches 2
;; | (unintern '*bubba*)     ;; matches 3
;; | (unintern '+bubba+)     ;; matches 4
;; | (unintern 'bubba+)      ;; matches 5
;; | (unintern 'bubba*)      ;; matches 6
;; | (unintern '+bubba*)     ;; matches 7
;; | (unintern 'bubba0*)     ;; matches 8
;; | (unintern '*bubba*)     ;; matches 9
;; | (unintern '*-BuB-bA8)   ;; matches 10
;; | (unintern '--bub:Ba8*)  ;; matches 11
;; | (unintern '--bb-Ba8-)   ;; matches 12
;; | (unintern '**bubba*)    ;; matches (incorrectly) 13
;; | (unintern '++bubba)     ;; matches (incorrectly) 14
;; | (unintern '--b/ub:Ba8*) ;; misses correctly
;; | (unintern '--b*ub-Ba8*) ;; misses correctly
;; | (unintern '+bub:Ba8**)  ;; misses correctly
;; | (unintern '*b:u/b-Ba8*) ;; misses correctly
;; |
;; `----

;;; ==============================
;;; :PREFIX "mruwq-"
;;; :CHANGESET 2214
;;; :CREATED <Timestamp: #{2010-10-26T16:42:37-04:00Z}#{10432} - by MON KEY>
(defun mon-replace-unintern-w-query  ()
  "Find replace occurences of unintern forms in current buffer.\n
Find occurences of:\n
 \(unintern   'vanilla-var\)\n
 \(unintern   'some:variable8\)\n
 \(unintern   'some/var-iable\)\n
 \(unintern   '*some-earmuffed-variable*\)\n
 \(unintern   '+some-plusd-variable+\)\n
 \(unintern   '--some-dashed--\)\n
Query if they should be replaced with:\n
 \(unintern  \"*some-earmuffed-variable*\" obarray\)\n
Log results of matches found and whether a replacements occured or was declined
to buffer named \"*MON-REGEXP-UNINTERN*\" display buffer on on exit from this
function.\n
:EXAMPLE\n\n(save-excursion (mon-with-inhibit-buffer-read-only
                     (mon-replace-unintern-w-query)))\n
 \(unintern   'vanilla-var\)\n
 \(unintern   'some:variable8\)\n
 \(unintern   'some/var-iable\)\n
 \(unintern   'some*failing0-var\)\n
 \(unintern   '*some-earmuffed-variable*\)\n
 \(unintern   'some:failed/variable\)\n
 \(unintern   '+some-plusd-variable+\)\n
 \(unintern   '--some-dashed--\)\n
:SEE-ALSO `*mon-regexp-unintern*'.\n►►►"
  (interactive)
  (let ((mruwq-bfr (get-buffer-create (upcase (symbol-name '*mon-regexp-unintern*))))
        ;; Initially store a buffer here. Later rebind to match related stuff.
        (mruwq-md  (current-buffer)) 
        (mruwq-div (concat (make-string 68 59) "\n" ))
        (mruwq-mrk (make-marker))
        mruwq-lst-mrk)
    (with-current-buffer mruwq-bfr
      (setq mruwq-lst-mrk (get '*mon-regexp-unintern* :last-mark))
      (if (and mruwq-lst-mrk (markerp mruwq-lst-mrk) (marker-position mruwq-lst-mrk)
               (eq (marker-buffer mruwq-lst-mrk) (current-buffer)))
          (goto-char mruwq-lst-mrk)
        (mon-g2be -1))
      (insert mruwq-div
              ";; :FUNCTION `mon-replace-unintern-w-query'\n"
              ":; :IN-BUFFER " (format "%s %S\n" mruwq-md (buffer-name mruwq-md))
              ";; :AT-TIME " (mon-format-iso-8601-time) "\n"
              mruwq-div)
      ;; Unbind `mruwq-md` it for reuse below.
      (setq mruwq-md))
    (unwind-protect 
        (save-excursion  
          (while (search-forward-regexp *mon-regexp-unintern* nil t)
            (setq mruwq-md (mapcar #'(lambda (mruwq-L-1) 
                                       (cons mruwq-L-1 
                                             (match-string-no-properties mruwq-L-1)))
                                   (number-sequence 1 5)))
            (let ((*standard-output* mruwq-bfr)
                  (mruwq-sub-rpl (match-substitute-replacement 
                                  (concat "(unintern" 
                                          (cdr (assq 2 mruwq-md)) ;; keep the whitespace
                                          "\"" (cdr (assq 4 mruwq-md)) "\" "
                                          "obarray)") t)))
              (princ (concat mruwq-div ";; :WITH-MATCH-GROUPS") mruwq-bfr)
              (print mruwq-md mruwq-bfr)
              (princ "\n;; :WITH-MATCH-STRING" mruwq-bfr)
              (print (match-string-no-properties 0) mruwq-bfr)
              (princ "\n;; :WITH-MATCH-DATA" mruwq-bfr)
              (print (match-data 1) mruwq-bfr)
              (if (yes-or-no-p (concat 
                                ":FUNCTION -- found match:  " (match-string-no-properties 0)
                                "\n          -- replace with: " mruwq-sub-rpl 
                                "\n          -- do replace?:  "))
                  (progn
                    (replace-match mruwq-sub-rpl t)
                    (princ "\n;; :DID-REPLACE t\n\n" mruwq-bfr)
                    (princ mruwq-div mruwq-bfr)
                    (setq mruwq-md))
                (progn 
                  (princ "\n;; :DID-REPLACE nil\n\n" mruwq-bfr)
                  (princ mruwq-div mruwq-bfr)
                  (setq mruwq-md))))))
      (with-current-buffer mruwq-bfr
        (set-marker mruwq-mrk (point))
        (put '*mon-regexp-unintern* :last-mark mruwq-mrk)
        (setq mruwq-md nil
              mruwq-lst-mrk nil)))
    (with-current-buffer mruwq-bfr
      (setq mruwq-lst-mrk (get '*mon-regexp-unintern* :last-mark))
      (when (and mruwq-lst-mrk
                 (markerp mruwq-lst-mrk)
                 (marker-position mruwq-lst-mrk)
                 (eq (marker-buffer mruwq-lst-mrk) (current-buffer)))
        (goto-char mruwq-lst-mrk))
      (display-buffer mruwq-bfr t))))

 
;;; ==============================
;;; :PREFIX "mcmh-"
;;; :CREATED <Timestamp: #{2010-03-23T13:32:14-04:00Z}#{10122} - by MON>
(defun mon-cln-mail-headers (start end)
  "Clean manually yanked email header \(Gmail centric\) in region from START to END.\n
Replace Email header fields at BOL e.g.:\n
 from reply-to to cc date subject mailed-by signed-by\n
With:\n
;;; :FIELD <SOME-FIELD-VALUE>\n
When required add whitespace padding before <SOME-FIELD-VALUE> such that all
field values begin at column 17.\n
Insert value of `*mon-default-comment-divider*' and replace BOL occurences of:\n
 show details Mmm 12 \(18 days ago\)\n hide details Mmm 9 \(1 day ago\)
 show details 3:25 AM \(7 hours ago\)\n hide details 12:25 PM \(1 hour ago\)
 hide details 4:55 AM \(0 minutes ago\)\n show details 1:55 PM \(0 minutes ago\)
 hide details 8:55 PM \(1 minute ago\)\n show details 12:55 AM \(12 minutes ago\)\n
:EXAMPLE\n
\(let* \(\(dlm \(nth 1 \(mapcar #'cadr \(mon-help-delimited-region t\)\)\)\)
       \(eg-str \(buffer-substring-no-properties \(car dlm\) \(cdr dlm\)\)\)\)
  \(with-temp-buffer 
    \(insert eg-str\)
    \(mon-cln-mail-headers \(buffer-end 0\)\(buffer-end 1\)\)
    \(setq eg-str \(buffer-substring-no-properties \(buffer-end 0\)\(buffer-end 1\)\)\)\)
  \(mon-help-overlay-result \(car dlm\) \(cdr dlm\) 78 eg-str\)\)\n
►
from	some-name <some-name@some-domain.com>\nreply-to	reply-val <reply-val@some-domain.com>
to	other-name@other-domain.com\ncc	more-names <more-names@more-domain.com>
date	Mon, Jan 00, 2010 at 12:00 AM\nsubject	Some Engaging Subject Matter
mailed-by	some-mailing-domain.com\nsigned-by	some-signing-domain.com\n
show details Mar 2 \(18 day ago\)\nhide details Mar 9 \(1 day ago\)
show details Mar 22 \(18 day ago\)\nhide details Mar 29 \(1 day ago\)
show details 3:25 AM \(7 hours ago\)\nhide details 3:25 AM \(7 hours ago\)
show details 12:25 AM \(7 hours ago\)\nhide details 3:25 AM \(1 hour ago\)
hide details 12:55 PM \(0 minutes ago\)\nshow details 3:55 PM \(0 minutes ago\)
hide details 12:55 PM \(1 minute ago\)\nshow details 12:55 AM \(1 minute ago\)
◄\n
:SEE RFC-2822 (URL `http://tools.ietf.org/html/rfc2822').
:SEE RFC-5322 (URL `http://tools.ietf.org/html/rfc5322').\n
:SEE-ALSO `mon-cln-csv-fields', `mon-cln-file-name-string', `mon-cln-up-colon',
`mon-cln-whitespace', `mon-cln-uniq-lines', `mon-cln-control-M',
`mon-cln-piped-list', `mon-delete-back-up-list', `mon-cln-iso-latin-1'.\n►►►"
  (interactive "r")
  (let ((mcmh-bol-rplc
         '(;; :CapCased-common
           "Date" "Cc" "Subject" "To" "From" "Bcc"
           "Authentication-Results" 
           "Content-Transfer-Encoding" "Content-Disposition" "Content-Type"
           "DKIM-Signature" "Delivered-To" "DomainKey-Signature" "Errors-To" "List-Archive"
           "List-Help" "List-Id" "List-Post" "List-Subscribe" "List-Unsubscribe"
           "MIME-Version" "Message-ID" "Precedence" "Received-SPF" "Received" "References"
           "Return-Path" "Sender" "X-BeenThere:" "X-Forwarded-For" "X-Forwarded-To"
           "X-Generated-By" "X-Mailman-Version" "X-Trak-Extra-Language"
           ;; :CapCased ommitted
           ;; "Comments" "Keywords"
           ;; :downcased-common
           "bcc"
           "from" "reply-to" "to" "cc" "date" "subject" "mailed-by" "signed-by"
           ;; :downcased-ommitted
           ;; "in-reply-to" "comments" "keywords" "message-id"
           ))
        mcmh-lngst-str
        mcmh-bol-rplc-lngst
        mcmh-bol-hd-shw)
    (setq mcmh-lngst-str (car (mon-string-sort-descending (copy-sequence mcmh-bol-rplc))))
    (setq mcmh-bol-rplc-lngst (length mcmh-lngst-str))
    (setq mcmh-bol-rplc 
          (concat
           "^\\("
           ;;"\\(from\\|reply-to\\|to\\|cc\\|date\\|subject\\|mailed-by\\|signed-by\\|MIME-Version\\)" ;<-grp2
           "\\(" (regexp-opt mcmh-bol-rplc) ":?\\)" ;<-grp2
           "\\([[:blank:]]+\\)"                   ;<-grp3
           "\\(.*\\)"                             ;<-grp4
           "\\)"))
    (setq mcmh-bol-hd-shw
          (concat
           "^\\("
           "\\(hide\\|show\\) details "
           "\\(" ;; Gmail displays by: Month N day|Time hour/minute
           "\\([A-Z][a-z]\\{2,2\\} [0-9]\\{1,2\\} ([0-9]+ days? ago)\\)"
           "\\|"
           "\\([0-9]\\{1,2\\}:[0-9]\\{2,2\\} [AP]M ([0-9]\\{1,2\\} \\(hour\\|minute\\)s? ago)\\)"
           "\\)" ;; :CLOSE time regexp
           "\\)"))
    (unwind-protect 
         (progn
           (narrow-to-region start end)
           (goto-char start)
           (while (search-forward-regexp mcmh-bol-rplc nil t)
             ;; :PREFIX "mcmh-lcl-"
             (let* ((mcmh-lcl-m2 (match-string-no-properties 2))
                    (mcmh-lcl-m4 (match-string-no-properties 4))
                    (mcmh-lcl-m2-len (length mcmh-lcl-m2))
                    (mcmh-lcl-m2-add-pad 
                     (if (or (equal (concat mcmh-lngst-str":") mcmh-lcl-m2)
                             (equal mcmh-lngst-str mcmh-lcl-m2)
                             (eq mcmh-lcl-m2-len mcmh-bol-rplc-lngst))
                         1
                       (- mcmh-bol-rplc-lngst mcmh-lcl-m2-len)))
                    (mcmh-lcl-m2-pad (make-string mcmh-lcl-m2-add-pad 32))
                    (mcmh-pad-frmt (format ";;; :%s %s %s" 
                                           (upcase mcmh-lcl-m2) mcmh-lcl-m2-pad mcmh-lcl-m4)))
               (replace-match mcmh-pad-frmt)))
           (princ (format "\n%s\n" *mon-default-comment-divider*)(current-buffer))
           (while (search-forward-regexp mcmh-bol-hd-shw nil t)
             (replace-match ""))
           (mon-g2be -1)
           (while (unless (eobp) (search-forward-regexp "^$" nil t))
             (delete-blank-lines)))
      (widen))))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let* ((dlm (nth 1 (mapcar #'cadr (mon-help-delimited-region t))))
;;|        (eg-str (buffer-substring-no-properties (car dlm) (cdr dlm))))
;;|   (with-temp-buffer 
;;|     (insert eg-str)
;;|     (mon-cln-mail-headers (buffer-end 0)(buffer-end 1))
;;|     (setq eg-str (buffer-substring-no-properties (buffer-end 0)(buffer-end 1))))
;;|   (mon-help-overlay-result (car dlm) (cdr dlm) 78 eg-str))
;;| 
;;| ►
;;| from	some-name <some-name@some-domain.com>
;;| reply-to	reply-val <reply-val@some-domain.com>
;;| to	other-name@other-domain.com
;;| cc	more-names <more-names@more-domain.com>
;;| date	Mon, Jan 00, 2010 at 12:00 AM
;;| subject	Some Engaging Subject Matter
;;| mailed-by	some-mailing-domain.com
;;| signed-by	some-signing-domain.com
;;| 
;;| show details Mar 2 (18 day ago)
;;| hide details Mar 9 (1 day ago)
;;| show details Mar 22 (18 day ago)
;;| hide details Mar 29 (1 day ago)
;;| show details 3:25 AM (7 hours ago)
;;| hide details 3:25 AM (7 hours ago)
;;| show details 12:25 AM (7 hours ago)
;;| hide details 3:25 AM (1 hour ago)
;;| hide details 12:55 PM (0 minutes ago)
;;| show details 3:55 PM (0 minutes ago)
;;| hide details 12:55 PM (1 minute ago)
;;| show details 12:55 AM (1 minute ago)
;;| ◄
;;`----

;;; ==============================
;;; :TODO Should have a seperate interactive function which acts only on the
;;; region and reads multiple fields-list with `mon-read-multiple' and/or `completing-read-multiple'.
;;; :PREFIX "mccf-"
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 11:46.00 AM - by MON KEY>
(defun mon-cln-csv-fields (field-list &optional delim-slot-w delim-row-w no-list w-line-delim-char)
  "Clean data pre-formatted for generation of .csv files.\n
Regexps perform the final conversion. 
FIELD-LIST is a colon delimited list of strings each of which is a slot/column
key for a given value e.g.:\n
 \(\"Name: \" \"Title: \" \"Institution: \" \"Address: \"
   \"City: \"\"State: \" \"Zipcode: \"\)\n
When non-nil DELIM-SLOT-W specifies delimter seperating values - defalut `,'.\n
When non-nil DELIM-ROW-W specifies delimter seperating value rows - defalut `;'.
:NOTE Don't use `##' as a row or slot delim.\n
When NO-LIST is non-nil return results without parens.\n
W-LINE-DELIM-CHAR is non-nil it is a two elt proper list.
Its first elt car is an integer indicating a length its second a is a
character. It has the form: \(<INT> <CHAR>\) and is used to generate a regexp
which will identify and remove any line delimiting blocks as well. e.g. if a
record is delimited with:\n
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
The argument to W-LINE-DELIM-CHAR would be (40 #x7e) generating the regexp:
 \"^~\\\\\\={40\\\\}$\"\n
The Default is \(30 #x3d\) which generates the regexp \"^\\\\\\={30\\\\}$\" to match following:\n
==============================\n
Assumes a data record structure with fields delimited as:\n
\"KEY: \" \"Value\"
\"KEY: \" \"Value\"
\"KEY: \" \"Value\"\n
For example, following are three line delimited records:\n
==============================
Name: Jane Doe
Title: Head of School
Institution: Academy of The Unknown
Address: 111 Some Street
City: Anytown
State: ZZ
Zipcode: 99999
==============================
Name: John Doe
 {... Similar Junk Elided ...}
Zipcode: 888888
==============================
Name: Hubba Bubba
 {... Similar Junk Elided ...}
Zipcode: 000000
==============================\n
:EXAMPLE\n\n\(mon-cln-csv-fields
 '\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \"
   \"City: \" \"State: \" \"Zipcode: \"\)\)\n
\(mon-cln-csv-fields\n '\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \"
   \"City: \" \"State: \" \"Zipcode: \"\) \"+\" nil\)\n
\(mon-cln-csv-fields\n '\(\"Name: \" \"Title: \" \"Institution: \"\"Address: \"
   \"City: \" \"State: \" \"Zipcode: \"\) nil \"_\" t\)\n
\(mon-cln-csv-fields\n '\(\"Name: \" \"Title: \" \"Institution: \"\"Address: \"
   \"City: \" \"State: \" \"Zipcode: \"\) \"@\" \"_\"\)\n
\(mon-cln-csv-fields\n '\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \"
    \"City: \" \"State: \" \"Zipcode: \"\) \"`\" \"|\" t '\(40 #x7e\)\)\n\n
:SEE-ALSO `mon-cln-mail-headers', `mon-cln-csv-fields',
`mon-cln-file-name-string', `mon-cln-up-colon', `mon-cln-whitespace',
`mon-cln-uniq-lines', `mon-cln-control-M', `mon-cln-piped-list',
`mon-delete-back-up-list', `mon-cln-iso-latin-1'.\n►►►"
  (interactive)
  (save-excursion
    (let* ((mccf-csv-mkr field-list)
	   (mccf-pnt-strt (make-marker)) ;; This marker prob. needs its insertion type set.
	   (mccf-fld-hd (first mccf-csv-mkr))
	   (mccf-fld-tl (car (last mccf-csv-mkr)))
	   (mccf-dsw (cond ( ;; Using `##' to recover newlines in final cleanup loop.
                            (string-equal delim-slot-w "##") 
                            (error 
                             (concat ":FUNCTION `mon-cln-csv-fields' "
                                     "-- ## is special in this fuction don't use as a row delimiter")))
                           (delim-slot-w delim-slot-w)
                           (t ",")))
	   (mccf-drw (cond ( ;; Using `##' to recover newlines in final cleanup loop.
                       (string-equal delim-row-w "##") 
		       (error 
                        (concat ":FUNCTION `mon-cln-csv-fields' "
                                "-- ## is special in this fuction don't use as a row delimiter")))
		      (delim-row-w delim-row-w)
		      (t ";")))
           (mccf-ln-delm (and (or (and w-line-delim-char
                                       (or (and (mon-proper-list-p w-line-delim-char) 
                                                (and (wholenump (car w-line-delim-char))
                                                     (characterp (cadr w-line-delim-char))))
                                           (error (concat ":FUNCTION `mon-cln-csv-fields' "
                                                          "-- arg W-LINE-DELIM-CHAR not useable, got: %S")
                                                  w-line-delim-char)))
                                  (setq w-line-delim-char '(30 #x3d)))
                              (concat "^" 
                                      (cdr w-line-delim-char)
                                      "\\{" 
                                      (number-to-string (car w-line-delim-char))
                                      "\\}$")))
	   (mccf-reg-dsw1  (format "\\(: %s\\)" mccf-dsw))
	   (mccf-reg-dsw2  (format "\"%s \"" mccf-dsw))
           mccf-oo)
      (setq mccf-oo  ;; Why `prin1'? Was there a destination stream at some point?
            (mapconcat #'(lambda (mccf-L-1) (prin1 mccf-L-1 )) mccf-csv-mkr mccf-dsw))
      (setq mccf-oo (replace-regexp-in-string  mccf-reg-dsw1 "\" \"" mccf-oo))
      (setq mccf-oo (replace-regexp-in-string "\\(: \\)" "\"" mccf-oo))
      (setq mccf-oo (replace-regexp-in-string "\\(\" \"\\)" mccf-reg-dsw2 mccf-oo))
      (if no-list
	  (setq mccf-oo (concat "\"" mccf-oo mccf-drw "##")) 
	(setq mccf-oo (concat "(\"" mccf-oo ")" mccf-drw "##")))
      (set-marker mccf-pnt-strt (point))
      (insert mccf-oo)     
      (while mccf-csv-mkr
	(let* ((mccf-srch-hd (car mccf-csv-mkr))
	       (mccf-rgx-hd (concat "^\\(\\(" mccf-srch-hd "\\)\\(.*\\)\\)$")))
	  (while (search-forward-regexp mccf-rgx-hd nil t)
	    (cond ((string= mccf-srch-hd mccf-fld-hd)
		   (if no-list
                       ;; _Do not_ put leadnig `('
		       (replace-match (format "\"\\3\"%s" mccf-dsw))	
                       ;;_Do_ put leadnig `('
                       (replace-match (format "\(\"\\3\"%s" mccf-dsw)))) 
		  ((string= mccf-srch-hd mccf-fld-tl)
		   (if no-list
		       (replace-match (format "\"\\3\"%s##" mccf-drw))
		     (replace-match (format "\"\\3\"\)%s##" mccf-drw))))
		  (t (replace-match (format"\"\\3\"%s" mccf-dsw)))))
	  (setq mccf-csv-mkr (cdr mccf-csv-mkr))
	  (goto-char (marker-position mccf-pnt-strt))))
      (goto-char (marker-position mccf-pnt-strt))
      (while (search-forward-regexp mccf-ln-delm  nil t)
	(replace-match ""))
      (goto-char (marker-position mccf-pnt-strt))
      (while (> (buffer-end 1) (point))
	(end-of-line)
	(when (and (not (eobp)) (eolp) (= (char-after (point)) 10))
	  (delete-char 1)))
      (goto-char (marker-position mccf-pnt-strt))
      ;; (while (> (buffer-end 1) (point))
      (let* ((mccf-drw-l (length mccf-drw))
             (mccf-drw-end (substring mccf-drw (- mccf-drw-l 1) mccf-drw-l))
             (mccf-drw-e-char (string-to-char mccf-drw-end)))
        (while (search-forward-regexp (format "\\(%s##\\)" mccf-drw) nil t)
          (when (and                    ;(not (eobp)) 
                 (= (char-before (point)) 35)
                 (= (char-before (- (point) 1)) 35)
                 (= (char-before (- (point) 2)) mccf-drw-e-char)) 
            (replace-match (format "%s\n" mccf-drw))))
        (mon-g2be -1)
        ;; Make sure we got everything. Junk can end up abutting W-LINE-DELIM-CHAR @ EOL
        (setq mccf-ln-delm (substring mccf-ln-delm 0 (1- (length mccf-ln-delm))))
        (while (search-forward-regexp mccf-ln-delm nil t)
          ;; maybe do "\n" instead
          (replace-match ""))
        (set-marker mccf-pnt-strt nil)))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (mapconcat #'identity
;; |            '("Name: Jane Doe"
;; |              "Title: Head of School"
;; |              "Institution: Academy of The Unknown"
;; |              "Address: 111 Some Street"
;; |              "City: Anytown"
;; |              "State: ZZ"
;; |              "Zipcode: 99999") "\n")
;; | (mon-cln-csv-fields
;; |  '("Name: " "Title: " "Institution: " "Address: " "City: " "State: " "Zipcode: "))
;; | (mon-cln-csv-fields
;; |  '("Name: " "Title: " "Institution: " "Address: " "City: " "State: " "Zipcode: ") "+" nil)  
;; | (mon-cln-csv-fields
;; |  '("Name: " "Title: " "Institution: ""Address: " "City: " "State: " "Zipcode: ") nil "_" t) 
;; | (mon-cln-csv-fields 
;; |  '("Name: " "Title: " "Institution: ""Address: " "City: " "State: " "Zipcode: ") "@" "_")
;; | (mon-cln-csv-fields'
;; |  ("Name: " "Title: " "Institution: " "Address: " "City: " "State: " "Zipcode: ") "`" "|" t)
;; `----

;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T12:46:10-05:00Z}#{11022} - by MON KEY>
;;;###autoload
(defun mon-cln-freenode-log ()
"Clean IRC join/part/quit lines from current-buffer.\n
Does not move point.\n
Lines matching `*regexp-clean-IRC-logs*' which have the following general
patterns are removed from current-buffer:\n
NN:NN:NN --- join: <USER1> (~<USER1>@some.ip.address.abc) joined #<CHANNEL>
NN:NN:NN --- quit: <USER2> (<ACTION-OR-REASON>)
NN:NN:NN --- part: <USER3> left #<CHANNEL>\n
:EXAMPLE\n\n\(mon-cln-freenode-log-TEST\)\n
:SEE-ALSO `mon-cln-freenode-log-TEST', `mon-wget-freenode-lisp-logs',
`*freenode-lisp-logs*', `mon-help-CL-minion'.\n►►►"
  (interactive)
  (save-excursion 
    (while (search-forward-regexp *regexp-clean-irc-logs* nil t)
      (replace-match "")
      (backward-delete-char 1))))


;;; ==============================
;;; :PREFIX "mcfns-"
;;; :CREATED <Timestamp: Saturday May 09, 2009 @ 08:40.30 PM - by MON KEY>
(defun mon-cln-file-name-string (fix-string)
  "Replace chars not allowed in w32 filenams `-'.\n
Cleaned chars include:\n
 `/',  `:',  `*', `?', `\"', `<', `>', `|, `\\'\n
:SEE-ALSO `mon-exchange-slash-and-backslash', `convert-standard-filename'
`mon-cln-mail-headers', `mon-cln-csv-fields',
`mon-cln-file-name-string', `mon-cln-up-colon', `mon-cln-whitespace',
`mon-cln-uniq-lines', `mon-cln-control-M', `mon-cln-piped-list',
`mon-delete-back-up-list', `mon-cln-iso-latin-1'.\n►►►"
  (let* ((mcfns-fix-pfx '("/"  ":"  "*"  "?" "\"" "<" ">" "|" "\\\\" ))
	 (mcfns-fix-str fix-string))
	 ;;"\\/:*?\"<>|"))
	 (while mcfns-fix-pfx
	   (let (mcfns-fixing)
	     (setq mcfns-fixing (car mcfns-fix-pfx))
	     (setq mcfns-fix-str (replace-regexp-in-string mcfns-fixing "-" mcfns-fix-str))
	     (setq mcfns-fix-pfx (cdr mcfns-fix-pfx))))
	 ;;(print  mcfns-fix-str)))
	 mcfns-fix-str))
;;
;;; :TEST-ME (prin1 (mon-cln-file-name-string "\\/:*?\"<>|"))

 
;;; ==============================
;;; :TODO This needs to be rebuilt using a better helper function/regex-list per
;;;       the newer `mon-replace-region-regexp-lists-nonint'.
;;; :CREATED <Timestamp: Tuesday February 17, 2009 @ 03:27.10 PM - by MON KEY>
(defun mon-cln-html-chars (start end)
  "Replace  <  by  &lt;  and other similar HTML chars that needs to be encoded.\n
Replace  & ,  > ,  <  with their respective encoded representation.\n
:SEE-ALSO `mon-cln-html-chars', `mon-cln-html-tags', `mon-cln-xml-escapes'.\n►►►"
  (interactive "r")
  (replace-string-pairs-region3 start end
			       '(("&" "&amp;")
				 ("<" "&lt;")
				 (">" "&Gt;"))))

;;; ==============================
;;; :PREFIX "mcht-"
;;; :COURTESY Noah Friedman :HIS buffer-fns.el :WAS `nuke-html-tags'
;;; :MODIFICATIONS To the regexps for text between tags \">Some</a>\" and
;;; crowded periods at end-of-sentence and between two chars at end-of-sentence
;;; w/out whitespace.
;;; :CREATED <Timestamp: Tuesday February 17, 2009 @ 03:48.22 PM - by MON KEY>
(defun mon-cln-html-tags (beg end)
  "Replace common HTML tags with either newline or nil. Poor man's html formatter.\n
:SEE-ALSO `mon-cln-html-chars', `mon-cln-wiki', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-ulan', `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib', `mon-cln-loc'.
`mon-cln-xml<-parsed', `mon-cln-mail-headers', `mon-cln-csv-fields',
`mon-cln-file-name-string', `mon-cln-up-colon', `mon-cln-whitespace',
`mon-cln-uniq-lines', `mon-cln-control-M', `mon-cln-piped-list',
`mon-delete-back-up-list', `mon-cln-iso-latin-1', `mon-cln-xml-escapes'.\n►►►"
  (interactive "r")
  (let ((mcht-tbl '(("\n"                               . nil) ;; :NOTE is this correct? - MON
		 ("\\(\\(.>+\\)\\([A-Za-z0-9: :]*\\)\\(</a>\\)\\)" . "\\2 \\3")
                 ("\\(\\(</a>\\)\\(\.\\)\\)" . "\\3")
                 ;; ("<p>"                              . "\n\n")
                 ("<p>\\|<P>"                              . "\n\n")
                 ;;("<br>"                             . "\n")
                 ("<br>\\|<BR>"                             . "\n")
                 ("</?h[0-9]>"                       . "\n\n")
                 ("</?blockquote>"                   . "\n\n")
                 ("&nbsp;"                           . " ")
		 ("\\(&[^ <]*;\\)\\|\\(<[^>]*>\\)" . nil)
		 ("\\([a-z]\\{1,1\\}\\)\\([:.:]\\)\\([A-Z]\\{1,1\\}\\)" . "\\1\\2 \\3")))
        mcht-rgx mcht-sub)
    (mon-toggle-restore-llm nil
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (while mcht-tbl
            (setq mcht-rgx (car (car mcht-tbl)))
            (setq mcht-sub (cdr (car mcht-tbl)))
            (setq mcht-tbl (cdr mcht-tbl))
            (mon-g2be -1)
            (cond (mcht-sub (while (search-forward-regexp mcht-rgx nil t)
                         (replace-match mcht-sub)))
                  (t (while (search-forward-regexp mcht-rgx nil t)
                       (delete-region (match-beginning 0) (match-end 0)))))))))))

;;; ==============================
;;; :PREFIX "mcxe-"
;;; :CHANGESET 2108
;;; :CREATED <Timestamp: #{2010-09-03T15:35:19-04:00Z}#{10355} - by MON KEY>
(defun mon-cln-xml-escapes () 
  "Replace all occurences of HTML style numeric entity refs in current XML buffer.\n
The W3C XML \"standards\" support Unicode, but do not support numeric entity
codes like \"&#x03C1;\" for Greek ρ or HTML entity codes like \"&eacute;\" for
accented é.\n
Legacy data dumped/imported to an XML file may contain hardwired HTML numeric
entity code refs. These may have their \"&\" (char 38) converted to \"&amp\" and
are therefor doubly difficult to get interpolated back to the correct character
esp. when the numeric entity is a combining character, e.g.:\n
  ́ ->  &;#769;  -> &amp;#769;
  é ->  3&;#769; -> e&amp;#769;\n
:EXAMPLE\n\n(mon-cln-xml-escapes-TEST\)\n
:SEE (URL `http://tlt.its.psu.edu/suggestions/international/bylanguage/ipavowels.html')
:SEE-ALSO `mon-cln-html-tags', `mon-cln-html-chars', `mon-cln-xml<-parsed',
`mon-cln-xml<-parsed-strip-nil', `mon-url-encode', `mon-url-decode'.\n►►►"
  (let ((case-fold-search nil)
        (mcxe-rep-&AMP '(("e&amp;#769;"   . "é")
                         ("a&amp;#769;"   . "á")
                         ("i&amp;#769;"   . "í")
                         ("o&amp;#769;"   . "ó")
                         ("u&amp;#769;"   . "ú")
                         ("y&amp;#769;"   . "ý")
                         ("w&amp;#769;"   . "ẃ")
                         ("E&amp;#769;"   . "É")
                         ("A&amp;#769;"   . "Á")
                         ("I&amp;#769;"   . "Í")
                         ("O&amp;#769;"   . "Ó")
                         ("U&amp;#769;"   . "Ú")
                         ("Y&amp;#769;"   . "Ý")
                         ("W&amp;#769;"   . "Ẃ"))))
    (dolist (mcxe-ramp mcxe-rep-&AMP)
      (mon-g2be -1)
      (while (search-forward-regexp (car mcxe-ramp) nil t)
        (replace-match (cdr mcxe-ramp))))))

;;; ==============================
;;; :PREFIX "mcxp-"
;;; :CREATED <Timestamp: #{2009-11-17T15:36:11-05:00Z}#{09472} - by MON KEY>
(defun mon-cln-xml<-parsed (fname &optional insrtp intrp)
  "Strip CR\LF TAB+ whitespace string event element created by `xml-parse-file'.\n
FNAME is an XML filename path to parse and clean.\n
When INSERTP is non-nil or called-interactively insert pretty printed lisp
representation of XML file fname at point. Does not move point.\n
:NOTE Unlike `mon-cln-xml<-parsed-strip-nil' will not strip `nil' from parsed xml.\n
:SEE-ALSO `mon-cln-tgm-xml-LF', `mon-cln-xml-escapes'\n►►►"
  ;; :WAS (interactive "fXML file to parse: \ni\np")
  (interactive "f:FUNCTION `mon-cln-xml<-parsed' -- XML file to parse: \ni\np")
  (let (mcxp-get-xml)
    (setq mcxp-get-xml
          (with-temp-buffer
            (prin1 (xml-parse-file fname) (current-buffer))
            (mon-g2be -1)
            (while (search-forward-regexp 
                    "\\( \"\n[[:blank:]]+\\)\"\\(\\(\\()\\)\\|\\( (\\)\\)\\)" nil t)
                   ;;^^1^^^^^^^^^^^^^^^^^^^^^^^^^2^^3^^^^^^^^^^^^4^^^^^^^^^^^^
            (replace-match "\\2"))
            (pp-buffer)
            (mon-buffer-sub-no-prop) ))
    (if (or insrtp intrp)
        (save-excursion 
          (newline) 
          (princ mcxp-get-xml (current-buffer)))
        mcxp-get-xml)))
;;
;;; :TEST-ME (mon-cln-xml<-parsed <FNMAME>)

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 04, 2009 @ 11:55.40 AM - by MON KEY>
(defun mon-cln-tgm-xml-LF ()
  "Clean EOL whitespace in tgm->XML conversions.\n
:SEE-ALSO `mon-cln-xml<-parsed', `mon-cln-xml<-parsed-strip-nil',
`mon-cln-xml-escapes'.\n►►►"
  (interactive)
  (save-excursion
    (mon-g2be -1)
    (while (< (point) (mon-g2be 1 t))
      (progn
        (end-of-line)
        (when (and (equal (char-after (point)) 10)
                   (equal (char-after (1+ (point))) 60)
                   (equal (char-after (+ (point) 2)) 47))
          (delete-char 1)))
      (forward-char))))

;;; ==============================
;;; :PREFIX "mcxpsn-"
;;; :MODIFICATIONS <Timestamp: #{2009-11-17T17:00:10-05:00Z}#{09472} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-31T20:57:30-04:00Z}#{09362} - by MON KEY>
(defun mon-cln-xml<-parsed-strip-nil (fname &optional insrtp intrp)
  "De-string-ification of xml parse results from with `xml-parse-file'.\n
FNAME is a filename path to be parsed and cleaned.\n
When INSRTP is non-nil or called-interactively insert result at point.\n
Does not move point.\n
:NOTE Strips `nil' from parsed xml which may not be what you expect.\n
:SEE-ALSO `*regexp-clean-xml-parse*', `mon-cln-xml<-parsed',
`mon-cln-xml-escapes', `mon-cln-tgm-xml-LF'.\n►►►"
  (interactive "f:FUNCTION `mon-cln-xml<-parsed-strip-nil' -- XML file to parse: \ni\np")
  (let (mcxpsn-get-xml)
    (setq mcxpsn-get-xml
          (with-temp-buffer
            (prin1  (xml-parse-file fname) (current-buffer))
            (mon-g2be -1)
            (mon-replace-region-regexp-lists (mon-g2be -1 t) (mon-g2be 1 t) *regexp-clean-xml-parse*)
            (mon-buffer-sub-no-prop)))
    (if (or insrtp intrp)
        (save-excursion 
          (newline) 
          (princ mcxpsn-get-xml (current-buffer)))
      mcxpsn-get-xml)))
;;
;;; :TEST-ME (call-interactively 'mon-cln-xml<-parsed-strip-nil)

 
;;; ==============================
;;; :PREFIX "mcuc-"
;;; :GLOBAL-KEYBINDING (global-set-key "\C-cu:" 'mon-cln-up-colon)
;;; :MODIFICATIONS <Timestamp: #{2010-03-01T15:29:58-05:00Z}#{10091} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-12-19T02:39:48-05:00Z}#{09516} - by MON>
(defun mon-cln-up-colon (start end &optional insrtp intrp)
  "Return colonized string in region at BOL.\n
When region's string contains a trailing `:' or wspc remove it.\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
When called-interactively and the region is not active test but point is at BOL
and line is not longer 34 chars use `line-beginning-position' and
`line-end-position' instead.\n
:EXAMPLE\n\n(mon-cln-up-colon (line-beginning-position 3) (line-end-position 3))\n
upcase and colonize me \n
\(let \(\(buffer-read-only nil\)\) ; :NOTE point is calculated from end of let form.
  \(goto-char \(line-beginning-position 3\)\)
  \(mon-cln-up-colon nil nil nil t\)\)\n
upcase and colonize me \n
:SEE-ALSO `mon-upcase-commented-lines', `mon-downcase-commented-lines',
`mon-line-strings-bq-qt-sym-bol', `mon-line-strings-indent-to-col',
`mon-line-strings-pipe-bol', `mon-line-strings-pipe-to-col',
`mon-line-strings-qt-region', `mon-cln-mail-headers',
`mon-cln-file-name-string'.\n►►►"
  (interactive "i\ni\ni\np") ;; "r\ni\np")
  (let ((mcuc-bl (make-marker))
        (mcuc-el (make-marker))
        mcuc-rgn-ln-if mcuc-sstr)
    (setq mcuc-rgn-ln-if
          (cond ((and start end) `(,start . ,end))
                ((and intrp (use-region-p))
                 `(,(region-beginning) . ,(region-end)))
                ((and intrp (not (use-region-p)) (bolp) (not (or (eolp) (eobp)))
                      ;; Half of 68 columns seems a reasonable length to clamp at.
                      (> 34 (- (line-end-position) (line-beginning-position))))
                 `(,(line-beginning-position) . ,(line-end-position)))
                (t (error (concat ":FUNCTION `mon-cln-up-colon' "
                                  "-- INTRP but no start end values found")))))
    (set-marker mcuc-bl (car mcuc-rgn-ln-if))
    (set-marker mcuc-el (cdr mcuc-rgn-ln-if))
    (setq mcuc-sstr (upcase (mon-buffer-sub-no-prop mcuc-bl mcuc-el)))
    (setq mcuc-sstr (upcase mcuc-sstr))
    (let ((mcuc-chop-sstr (string-to-list mcuc-sstr))
          (chop-tail #'(lambda (mcuc-L-1) (car (last mcuc-L-1))))
          (chop-head #'(lambda (mcuc-L-2) (car mcuc-L-2))))
      ;; 58 -> `:' 32 -> ` ' 45 -> `-' 59 -> `;'
      (while (memq (funcall chop-head mcuc-chop-sstr) '(59 32 58 45))
        (setq mcuc-chop-sstr (cdr mcuc-chop-sstr)))
      (while (memq (funcall chop-tail mcuc-chop-sstr) '(32 58 45))
        (setq mcuc-chop-sstr (butlast mcuc-chop-sstr)))
      (unless (eq (length mcuc-chop-sstr) (length mcuc-sstr))
        (setq mcuc-sstr (apply 'string mcuc-chop-sstr))))
    (setq mcuc-sstr (concat ":" (subst-char-in-string 32 45 mcuc-sstr t)))
    (unwind-protect
        (if intrp 
            (save-excursion 
              (goto-char mcuc-bl)
              (delete-and-extract-region mcuc-bl mcuc-el)
              (princ mcuc-sstr (current-buffer)))
          mcuc-sstr)
      (and (markerp mcuc-bl) (set-marker mcuc-bl nil)) 
      (and (markerp mcuc-el) (set-marker mcuc-el nil))) ))
;;
;;; :TEST-ME (mon-cln-up-colon (+ (line-beginning-position 2) 4) (line-end-position 2))
;;; mon cln up colon
;;;  
;; ,---- :UNCOMMENT-TO-TEST
;; | (progn  ; :NOTE point is calculated from end of progn form.                                     
;; |   (goto-char (line-beginning-position 3))    
;; |   (apply 'mon-cln-up-colon nil '(nil nil t)))
;; | 
;; | ;;; 
;; `----

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 14, 2009 @ 05:47.46 PM - by MON KEY>
(defun mon-downcase-hex-values ()
  "Downcase all CSS Hex values in buffer.\n
:SEE-ALSO `mon-upcase-commented-lines', `mon-downcase-regexp-region',
`mon-cln-up-colon', `mon-line-strings-bq-qt-sym-bol',
`mon-line-strings-indent-to-col', `mon-line-strings-pipe-bol',
`mon-line-strings-pipe-to-col', `url-hexify-string', `url-unhex-string',
`url-unhex', `hexl-hex-string-to-integer', `*regexp-rgb-hex*',
`*css-color:hex-chars*', `*regexp-rgb-hex*', `*regexp-css-color-hex*'.\n►►►"
  (interactive)
  (save-excursion
    (let ((case-fold-search nil))
      (while (search-forward-regexp "#\\([A-Za-z]+\\)" nil t)
        (downcase-region (match-beginning 1)(match-end 1))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-17T22:09:21-04:00Z}#{09385} - by MON KEY>
(defun mon-upcase-commented-lines () ;(start end)
  "Upcase everything in lines that begin with three semicolons \"^;;; \".\n
Does not move point.\n
:NOTE Does not do error checking - so be smart about it.\n
:SEE-ALSO `mon-downcase-commented-lines', `mon-downcase-regexp-region',
`mon-downcase-hex-values' `mon-cln-up-colon', `mon-line-strings-bq-qt-sym-bol',
`mon-line-strings-indent-to-col', `mon-line-strings-pipe-bol', 
`mon-line-strings-pipe-to-col', `mon-region-indent-refill'.\n►►►"
  (interactive) ;;  (interactive "r")
  (save-excursion
    (while (search-forward-regexp "^;;; \\(.*\\)" nil t)
      (upcase-region (match-beginning 1) (match-end 1)))))

;;; ==============================
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
;;; :NOTE `mon-zippify-region' has _so_ many other applications...
;;;       Simply change the  `upcase-word' in the `while' clause :).
(defun mon-zippify-region (beg end &optional rand-limit)
  "Randomly capitalize certain words in the region from BEG and END.\n
Optional third arg RAND-LIMIT means capitalize roughly one out of every
RAND-LIMIT words.\n
:SEE-ALSO `mon-string-wonkify'.\n►►►"
  (interactive "r\np")
  (or rand-limit (setq rand-limit 8))
  (save-excursion
    (goto-char beg)
    (if (bobp) nil (forward-word -1) (forward-word 1))
    (while (< (point) end)
      (if (zerop (random rand-limit))
          (upcase-word 1)
        (forward-word 1)))))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-15T11:54:54-05:00Z}#{10071} - by MON KEY>
(defun mon-toggle-case-query-user (&optional w-region)
  "Return a list for use as interactive args to case toggling functions.\n
Default is to return a 5 element list suitable for `mon-toggle-case-regexp' e.g..
 \(case-toggle-match up-down &optional replace-n w-results intrp\)\n
When W-REGION is non-nil return with bounds of active region as 7 element list
suitable for use as args to `mon-toggle-case-regexp-region' e.g.:
 \(start end case-toggle-regexp case-up-down &optional limit-to insrtp intrp\)\n
:NOTE when W-REGION is non-nil signals an error when region is inactive.\n
:EXAMPLE\n\n\(mon-toggle-case-query-user)                 ;<- `mon-toggle-case-regexp'\n
\(let \(\(mark-ring nil\)                        ;<- `mon-toggle-case-regexp-region'
      \(current-prefix-arg 2\)\)
  \(save-excursion \(push-mark nil nil t\) \(forward-char 9\)  
                  \(prog1 \(mon-toggle-case-query-user t\) \(pop-mark\)\)\)\)
########\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp-region',
`mon-toggle-case-regexp', `mon-downcase-regexp', `mon-downcase-regexp-region',
`mon-upcase-regexp', `mon-upcase-regexp-region', `mon-downcase-commented-lines',
`mon-downcase-hex-values', `mon-toggle-case-regexp-region',
`mon-up/down-case-regexp-TEST', `mon-rectangle-downcase',
`mon-rectangle-upcase', `mon-upcase-commented-lines'.\n►►►"
  (if w-region
      `(,(if (use-region-p) 
             (region-beginning)
           (error (concat ":FUNCTION `mon-toggle-case-regexp-region' "
                          "-- region not selected")))
        ,(region-end)
        ,(read-regexp "Regexp to search: ")
        ,(car (read-from-string (completing-read 
                                 "Which way to toggle (up or down): "
                                 [up down] nil t)))
        ,(or current-prefix-arg)
        nil t)
    `(,(read-regexp "Toggle case with regexp: ")
      ,(car (read-from-string (completing-read 
                               "Which way to toggle (up or down): "
                               [up down] nil t)))
      ,(or current-prefix-arg 
           (read-number "Provide a positive number to limit case toggling to N times: " -1))
      t t))) 
;;
;;; :TEST-ME (mon-toggle-case-query-user)
;;; :TEST-ME (mon-toggle-case-query-user t)

;;; ==============================
;;; :PREFIX "mtcrr-"
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 05:08.23 PM - by MON KEY>
(defun mon-toggle-case-regexp-region (start end case-toggle-regexp case-up-down
                                            &optional limit-to insrtp intrp)
  "Return case toggled region from START END for CASE-TOGGLE-REGEXP matches.\n
CASE-UP-DOWN is a symbol `up' or `down'. When ``up'' `upcase-region' of regexp
matches, when ``down'' `downcase-region'.\n
LIMIT-TO is the maximum number of matches to replace.\n
When optional arg LIMIT-TO times is non-nil limit the amount of times replace .\n
When called-interactively with positve prefix arg use to LIMIT-TO times.
Else, if LIMIT-TO is ommitted prompt for the number of times to replace.
Default is -1 wich case-toggles all CASE-TOGGLE-REGEXP matches in region.\n
When INSRTP is non-nil or called-interactively insert return value in current buffer 
message results of toggling.\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp',
`mon-downcase-regexp', `mon-downcase-regexp-region', `mon-upcase-regexp',
`mon-upcase-regexp-region', `mon-downcase-commented-lines',
`mon-up/down-case-regexp-TEST', `mon-downcase-hex-values',
`mon-rectangle-downcase', `mon-rectangle-upcase',
`mon-upcase-commented-lines'.\n►►►"
  (interactive (funcall #'mon-toggle-case-query-user t))
  (let (mtcrr-dcrr)
    ;; :WAS (setq mtcrr-dcrr (buffer-substring-no-properties start end)) ;; mdrr-str mdrr-end
    (setq mtcrr-dcrr (mon-buffer-sub-no-prop start end))
    (setq mtcrr-dcrr
          (with-temp-buffer 
            (save-excursion (insert mtcrr-dcrr))
            (let ((mtcrr-mdr (mon-toggle-case-regexp 
                              case-toggle-regexp case-up-down limit-to (or insrtp intrp))))
              (setq mtcrr-mdr (cons (mon-buffer-sub-no-prop) mtcrr-mdr) ))))
    (if (or insrtp intrp)
        (save-excursion 
          (delete-region start end)
          (insert (pop mtcrr-dcrr))
          (message mtcrr-dcrr))
      mtcrr-dcrr)))
;;
;;; :EXAMPLE\n\"^\\(#[A-Z0-9]\\{6,6\\}$\\)\" REPLACE-N => 4
;;; #AEAE4D\n#D29966\n#C3A399\n#D3CD99\n#D0CCCC\n#FFFFCC\n
;; (mon-toggle-case-regexp "^\\(#[A-Z0-9]\\{6,6\\}$\\)\" 'down)

;;; ==============================
;;; :PREFIX "mtcr-"
;;; :MODIFICATIONS <Timestamp: #{2010-02-13T16:44:19-05:00Z}#{10066} - by MON KEY>
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 05:08.23 PM - by MON KEY>
(defun mon-toggle-case-regexp (case-toggle-match up-down &optional replace-n
                                                 w-results intrp)
  "Case-toggle matches for the regexp CASE-TOGGLE-MATCH after point.\n
UP-DOWN is symbol `up' or `down'.\n
When optional arg REPLACE-N times is non-nil limit the amount of times replace .\n
When called-interactively with positve prefix arg use to REPLACE-N times.
Else, if REPLACE-N is ommitted prompt for the number of times to replace.
Default is -1 which means to case-toggle all matches after point.
When W-RESULTS is non-nil or called-interactively message results.\n
:EXAMPLE\n
\(mon-up/down-case-regexp-TEST :test-fncn 'toggle-regexp-down\)\n
\(mon-up/down-case-regexp-TEST :test-fncn 'toggle-regexp-up\)\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp-region',
`mon-toggle-case-regexp', `mon-downcase-regexp', `mon-downcase-regexp-region',
`mon-upcase-regexp', `mon-upcase-regexp-region', `mon-downcase-commented-lines',
`mon-toggle-case-regexp-region', `mon-up/down-case-regexp-TEST',
`mon-downcase-hex-values', `mon-rectangle-downcase', `mon-rectangle-upcase',
`mon-upcase-commented-lines'.\n►►►"
  (interactive (funcall 'mon-toggle-case-query-user))
  (let ((mtcr-m-cnt 0)
        (mtcr-rep-cnt  (cond ((and replace-n (minusp replace-n)) nil)
                       (replace-n)))
        (mtcr-ud (cond ((eq up-down 'up) 'upcase-region)
                  ((eq up-down 'down) 'downcase-region)
                  (t (error (concat ":FUNCTION `mon-toggle-case-regexp' "
                                    "-- arg UP-DOWN either up or down")))))
        mtcr-msg)
    (while (search-forward-regexp case-toggle-match nil t)
      (unless (and mtcr-rep-cnt (= mtcr-m-cnt mtcr-rep-cnt))
	(let* ((mtcr-m-start  (match-beginning 1))
	       (mtcr-m-end    (match-end 1))
               (mtcr-m-str (mon-buffer-sub-no-prop mtcr-m-start mtcr-m-end)))
	  (funcall mtcr-ud mtcr-m-start mtcr-m-end)
	  (incf mtcr-m-cnt) ;; (setq mtcr-m-cnt (+ mtcr-m-cnt 1))
          (if (or w-results intrp)
              (setq mtcr-msg (cons 
                         (format "(:MATCH-NUMBER %d :MATCH-START %s :MATCH-END %s :MATCHED %s)"
                                 mtcr-m-cnt mtcr-m-start mtcr-m-end mtcr-m-str) mtcr-msg))))))
    (when (or w-results intrp)
      (setq mtcr-msg (reverse mtcr-msg))
      (setq mtcr-msg (mapconcat #'identity mtcr-msg "\n"))
      (if intrp (message (concat ":FUNCTION `mon-toggle-case-regexp' "
                                 "-- toggled case in following ranges:\n" mtcr-msg))
        mtcr-msg))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (mon-toggle-case-regexp "^\\(#[A-Z0-9]\\{6,6\\}$\\)" 'down 3 t)
;; | #AEAE4D
;; | #D29966
;; | #C3A399
;; | #D3CD99
;; | #D0CCCC
;; | #FFFFCC
;; `----

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-13T16:44:19-05:00Z}#{10066} - by MON KEY>
(defun mon-downcase-regexp-region (dc-start dc-end toggle-case-regexp
                                            &optional limit-to insrtp intrp)
  "Downcase each match of TOGGLE-CASE-REGEXP in region from DC-START to DC-END.\n
When REPLACE-TIMES is non-nil limit upcasing of matches to N times.\n
When WITH-RESULTS is non-nil or called-interactively return results of matches
that were upcased.\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp-region',
`mon-toggle-case-regexp', `mon-downcase-regexp', `mon-downcase-regexp-region',
`mon-upcase-regexp', `mon-upcase-regexp-region', `mon-downcase-commented-lines',
`mon-toggle-case-regexp-region', `mon-up/down-case-regexp-TEST',
`mon-downcase-hex-values', `mon-rectangle-downcase', `mon-rectangle-upcase',
`mon-upcase-commented-lines'.\n►►►"
  (interactive "i\ni\ni\ni\ni\np")
  (if intrp (call-interactively 'mon-toggle-case-regexp-region)
    (eval `(mon-toggle-case-regexp-region 
            ,dc-start ,dc-end ,toggle-case-regexp 'down ,limit-to))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-13T16:44:19-05:00Z}#{10066} - by MON KEY>
(defun mon-upcase-regexp-region (uc-start uc-end toggle-case-regexp
                                          &optional limit-to insrtp intrp)
  "Upcase each match of TOGGLE-CASE-REGEXP in region from UC-START to UC-END.\n
When REPLACE-TIMES is non-nil limit upcasing of matches to N times.\n
When WITH-RESULTS is non-nil or called-interactively return results of matches
that were upcased.\n
:EXAMPLE\n\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp-region',
`mon-toggle-case-regexp', `mon-downcase-regexp', `mon-downcase-regexp-region',
`mon-upcase-regexp', `mon-upcase-regexp-region', `mon-downcase-commented-lines',
`mon-toggle-case-regexp-region', `mon-up/down-case-regexp-TEST',
`mon-downcase-hex-values', `mon-rectangle-downcase', `mon-rectangle-upcase',
`mon-upcase-commented-lines'.\n►►►"
  (interactive "i\ni\ni\ni\ni\np")
  (if intrp (call-interactively 'mon-toggle-case-regexp-region)
    (eval `(mon-toggle-case-regexp-region 
            ,uc-start ,uc-end ,toggle-case-regexp 'up ,limit-to ,insrtp))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-13T20:51:06-05:00Z}#{10067} - by MON KEY>
(defun mon-downcase-regexp (match-on &optional replace-times with-results intrp)
  "Downcase each match of regpexp MATCH-ON.\n
When REPLACE-TIMES is non-nil limit upcasing of matches to N times.\n
When WITH-RESULTS is non-nil or called-interactively return results of matches
that were upcased.\n
:EXAMPLE\n\n\(mon-up/down-case-regexp-TEST :test-fncn 'downcase-regexp\)\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp',
`mon-downcase-regexp-region', `mon-upcase-regexp', `mon-upcase-regexp-region',
`mon-downcase-commented-lines', `mon-toggle-case-regexp-region',
`mon-up/down-case-regexp-TEST', `mon-downcase-hex-values',
`mon-rectangle-downcase', `mon-rectangle-upcase',
`mon-upcase-commented-lines'.\n►►►"
  (interactive "i\n\i\ni\np")
  (if intrp 
      (call-interactively 'mon-toggle-case-regexp)
    (eval `(mon-toggle-case-regexp 
            ,match-on 'down ,replace-times ,with-results))))
;;
;;; :TEST-ME (mon-downcase-regexp "^\\(#[A-Z0-9]\\{6,6\\}$\\)" nil t)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-13T21:01:22-05:00Z}#{10067} - by MON KEY>
(defun mon-upcase-regexp (match-on &optional replace-times with-results intrp)
  "Upcase each match of regpexp MATCH-ON.\n
When REPLACE-TIMES is non-nil limit upcasing of matches to N times.\n
When WITH-RESULTS is non-nil or called-interactively return results of matches
that were upcased.\n
:EXAMPLE\n\n\(mon-up/down-case-regexp-TEST :test-fncn 'upcase-regexp)\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp-region',
`mon-toggle-case-regexp', `mon-downcase-regexp', `mon-downcase-regexp-region',
`mon-upcase-regexp', `mon-upcase-regexp-region', `mon-up/down-case-regexp-TEST',
`mon-downcase-commented-lines', `mon-toggle-case-regexp-region',
`mon-downcase-hex-values', `mon-rectangle-downcase', `mon-rectangle-upcase',
`mon-upcase-commented-lines'.\n►►►"
  (interactive "i\n\i\ni\np")
  (if intrp 
      (call-interactively 'mon-toggle-case-regexp)
    (eval `(mon-toggle-case-regexp 
            ,match-on 'up ,replace-times ,with-results))))
;;
;;; :TEST-ME (mon-upcase-regexp "^\\(#[a-z0-9]\\{6,6\\}$\\)" nil t)


 
;;; ==============================
;;; :RENAMED `mon-clnBIG-whitespace' -> `mon-cln-BIG-whitespace'
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-cln-BIG-whitespace (start end &optional intrp)
  "Rudely fix whitespace in region.\n
More comprehensive than `mon-cln-whitespace' with treatement of leading and
trailing whitespace but can't be trusted to DTRT.\n
For interactive cleaning of trailing tabs and spaces of entirety of current-buffer:
:SEE `mon-kill-whitespace', `mon-cln-trail-whitespace', `mon-cln-blank-lines'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-imdb', `mon-trans-cp1252-to-latin1',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month',
`url-eat-trailing-space', `url-strip-leading-spaces'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
      (save-excursion
        (let ((whsp-start start)
              (whsp-end end)
              whsp-str)
          (setq whsp-str (mon-buffer-sub-no-prop whsp-start whsp-end))
          (setq whsp-str
                (with-temp-buffer
                  (insert whsp-str)
                  (progn
                    (mon-g2be -1)
                    (while (search-forward-regexp "[ \t]+$" nil t)
                      (delete-region (match-beginning 0) (match-end 0)))
                    (mon-g2be -1)
                    (while (search-forward-regexp "[ \t]+$" nil t)
                      (replace-match "" nil nil))
                    (mon-g2be -1)
                    (while (search-forward-regexp "^\\(\\([[:blank:]]+\\)\\([[:graph:]]\\)\\)"   nil t)
                      (replace-match "\\3" nil nil))
                    (mon-g2be -1)
                    (while (search-forward "\t" nil t)
                      (untabify (1- (point)) (mon-g2be 1 t)))
                    ;; (let ((start (buffer-end 0))
                    ;;       (end (buffer-end 1)))
                    (mon-g2be -1)
                    (mon-replace-region-regexp-lists-nonint (mon-g2be -1 t) (mon-g2be 1 t)
                                                            *regexp-clean-big-whitespace*)
                    (mon-g2be -1)
                    (while (search-forward-regexp "^\\([[:blank:]]+$\\)" nil t)
                      (replace-match "\n\n" nil nil))
                    (mon-buffer-sub-no-prop) )))
          (delete-region whsp-start whsp-end)
          (insert whsp-str)))
      (when intrp (message 
                   (concat ":FUNCTION `mon-cln-BIG-whitespace' "
                           " -- big whitespace cleaned")))))

;;; ==============================
(defun mon-cln-whitespace (start end &optional intrp)
  "Clean whitespace in region.\n
:NOTE A more function comprehensive is `mon-cln-BIG-whitespace' and is preferred. 
It handles leading and trailing wspc, but can't always be trusted to DTRT.\n
:REGEXPS-IN `*regexp-clean-whitespace*' 
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-kill-whitespace', `mon-cln-trail-whitespace',
`mon-cln-blank-lines', `mon-cln-mail-headers', `mon-cln-up-colon',
`mon-cln-imdb', `mon-trans-cp1252-to-latin1', `mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month', `url-eat-trailing-space',
`url-strip-leading-spaces'.\n►►►"
  (interactive "r\np")
  (mon-replace-region-regexp-lists-nonint start end *regexp-clean-whitespace*)
  (when intrp 
    (message 
     (concat ":FUNCTION `mon-cln-whitespace' "
             "-- whitespace has been rudely adjusted in region: %d %d")
     start end)))

;;; ==============================
(defun mon-cln-trail-whitespace ()
    "Indiscriminately clean trailing whitespace in _ENTIRE_ buffer.\n
Delete any trailing whitespace, converting tabs to spaces.\n
Use `mon-kill-whitespace' to kill tabs to 1 \(one\) space.\n
:NOTE Operates on entirety of current-buffer not a region.
For interactive whitespace region adjustment use `mon-cln-BIG-whitespace',
`mon-cln-blank-lines', or `mon-cln-whitespace'.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `url-eat-trailing-space', `url-strip-leading-spaces'.\n►►►"
    (interactive)
    (save-excursion
      (mon-g2be -1)
      (while (search-forward-regexp "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (mon-g2be -1)
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (mon-g2be 1 t) ))))


;;; ==============================
(defun mon-kill-whitespace ()
  "Kill trailing whitespace (tab and space) in *<BUFFER>* not region.\n
Unlike `mon-cln-trail-whitespace', doesn't convert tabs to spaces.\n
For interactive whitespace region adjustment use `mon-cln-BIG-whitespace',
`mon-cln-whitespace', or `mon-cln-blank-lines'.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-uniq-lines', `url-eat-trailing-space', `url-strip-leading-spaces'.\n►►►"
  (interactive)
  (save-excursion
    (mon-g2be -1)
    (while (search-forward-regexp "[ \t]+$" nil t)
      (replace-match "" nil nil))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 04:21.17 PM - by MON KEY>
(defun mon-cln-blank-lines (start end); &optional intrp)
  "Delete blank and empty lines in region from START to END.\n
:SEE-ALSO `mon-cln-uniq-lines', `delete-blank-lines',
`mon-line-find-duplicates-cln', `mon-line-find-duplicates'.\n►►►"
  (interactive "r") ;; \np
  (save-excursion
    (let ((mcbl-cln-strt start)
	  (mcbl-cln-end end))		;(message "%s %s" mcbl-cln-strt mcbl-cln-end))
      (while (> mcbl-cln-end (point))
	(cond ((mon-line-next-bol-is-eol) (delete-blank-lines))
	      ((mon-line-bol-is-eol) (delete-blank-lines))
	      ((mon-spacep-is-bol) (delete-blank-lines)))
	(forward-line))
      (while (< mcbl-cln-strt (point))
	(cond ((mon-line-previous-bol-is-eol)(delete-blank-lines))
	      ((mon-line-bol-is-eol) (delete-blank-lines))
	      ((mon-spacep-is-bol) (delete-blank-lines)))
	(forward-line -1)))))

;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:32.08 PM - by MON KEY>
(defun mon-cln-spc-tab-eol () ;;(&optional intrp)
  "Clean current-line of TAB (char 9) and SPC (char 32) at EOL.\n
:SEE-ALSO `mon-cln-spc-tab-at-eol-in-region' `mon-spacep-at-eol',
`mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive); "p")
  (while (mon-spacep-at-eol)
    (goto-char (point-at-eol))
    (delete-char -1)
    (goto-char (point-at-bol))))

;;; ==============================
;;; :PREFIX "mcstaeoi-"
;;; :CALLED-BY `mon-get-proc-buffers-directories' - be careful about modifying.
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 07:01.07 PM - by MON KEY>
(defun mon-cln-spc-tab-at-eol-in-region (start end)
  "Clean region of TAB (char 9) and SPC (char 32) at EOL.\n
:CALLED-BY `mon-get-proc-buffers-directories'.\n
:SEE-ALSO `mon-cln-spc-tab-eol'`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive "r")
  (save-excursion
    (let ((mcstaeoir-rgn (mon-buffer-sub-no-prop start end))
	  mcstaeoir-rtn)
      (setq mcstaeoir-rtn
	    (with-temp-buffer
	      (insert mcstaeoir-rgn)
	      (mon-g2be -1)
	      (while (mon-spacep-at-eol)
		(mon-cln-spc-tab-eol)
		(goto-char (1+ (point-at-eol))))
              (buffer-substring (mon-g2be -1 t) (mon-g2be 1 t))))
      (delete-region start end)
      (insert mcstaeoir-rtn))))

 
;;; ==============================
;;; :PREFIX "mcul-"
;;; :NOTE Duplicate lines are killed to kill-ring. IOW, clobbers the ring.
;;;       Testing a kill-ring restore inside the unwind-protect.
;;; :MODIFICATIONS <Timestamp: #{2010-01-26T20:57:22-05:00Z}#{10043} - by MON KEY>
(defun mon-cln-uniq-lines (beg end)
  "Return the unique lines in region, ommitting ducplicates.\n
Called programmatically ARGS BEG and END denote the \(region to sort\) and uniquify.
:NOTE Use `mon-line-find-duplicates-cln' where possible esp. in `naf-mode'.\n
This function was originally fashioned after `uniq-remove-dup-lines' and kills
matched lines to the kill ring. Doing so allowed a kludge for working around
longlines-mode issues before `mon-toggle-restore-llm' which appears to have
mitigated those issues.\n
:SEE :FILE uniq.el for additional details.\n
:SEE-ALSO `mon-line-find-duplicates-cln', `mon-cln-blank-lines',
`delete-blank-lines', `mon-cln-mail-headers', `mon-cln-csv-fields',
`mon-cln-file-name-string', `mon-cln-up-colon', `mon-cln-whitespace'.\n►►►"
  (interactive "r")
  (let ((mcul-ring kill-ring))
    (unwind-protect
        (save-excursion
          (save-restriction
            (setq kill-ring nil)  
            (narrow-to-region beg end)
            (mon-g2be -1)
            (while (not (eobp))
              (kill-line 1)
              (yank)
              (let ((next-line (point)))
                (while (search-forward-regexp 
                        (format "^%s" (regexp-quote (car kill-ring))) nil t)
                  (replace-match "")
                  (goto-char next-line))))))
      (setq kill-ring mcul-ring))))


;;; ==============================
;;; :PREFIX "mesab-"
;;; :COURTESY Stefan Reichor :HIS xsteve-functions.el
(defun mon-exchange-slash-and-backslash ()
  "Exchange / with \\ and in the current line.\n
Exchange in region when region-active-p is non-nil.\n
:SEE-ALSO `mon-cln-file-name-string', `convert-standard-filename'.\n►►►"
  (interactive)
  (save-match-data
    (save-excursion
      (let ((mesab-rplc-cnt 0)
            (mesab-eol-pos (if mark-active (region-end) (progn (end-of-line) (point))))
            (mesab-bol-pos (if mark-active (region-beginning) (progn (beginning-of-line) (point)))))
        (goto-char mesab-bol-pos)
        (while (search-forward-regexp "/\\|\\\\" mesab-eol-pos t)
          (setq mesab-rplc-cnt (+ mesab-rplc-cnt 1))
          (cond ((string-equal (match-string 0) "/") (replace-match "\\\\" nil nil))
                ((string-equal (match-string 0) "\\") (replace-match "/" nil nil)))
          (message (format "%d changes made." mesab-rplc-cnt)))))))

;;; ==============================
;;; :PREFIX "mccm-"
;;; :COURTESY Stefan Reichor <stefan@xsteve.at> :HIS xsteve-functions.el
(defun mon-cln-control-M (&optional intrp)
  "Remove ^M at EOL in current-buffer.\n
:SEE-ALSO `untabify', `mon-cln-spc-tab-eol', `mon-cln-mail-headers'
`mon-cln-csv-fields' `mon-cln-file-name-string' `mon-cln-up-colon'
`mon-cln-whitespace' `mon-cln-uniq-lines'.\n►►►"
  (interactive "p")
  (let (mccm-msg)
    (save-match-data
      (save-excursion
        (let ((mccm-rmv-cnt 0))
          (mon-g2be -1)
          (while (search-forward-regexp "\xd$"  (mon-g2be 1 t) t)
            (incf mccm-rmv-cnt)
            (replace-match "" nil nil))
          (setq mccm-msg (format "%d \xd's removed from buffer" mccm-rmv-cnt)))))
    (when intrp (message mccm-msg))))

;;; ==============================
;;; :PREFIX "mcpl-"
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-cln-piped-list (start end &optional intrp)
  "Clean region of piped list formatting i.e. \"Name | Name\".
Piped lists are used in the naf-mode sections:
 Used-for: Appeared-in: Ads-for: Artist-associated: Authors-associated:
 Products-associated: Slogans: Content-and-subjects: etc.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-line-pipe-lines', `naf-backup-the-list', `mon-delete-back-up-list',
`mon-line-strings-pipe-bol', `mon-line-strings-pipe-to-col',
`mon-cln-mail-headers', `mon-cln-csv-fields', `mon-cln-file-name-string',
`mon-cln-up-colon', `mon-cln-whitespace', `mon-cln-uniq-lines',
`mon-cln-control-M'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (let ((mcpl-strt start)
	  (mcpl-end end)
	  mcpl-pipe-rgn)
      (setq mcpl-pipe-rgn (mon-buffer-sub-no-prop mcpl-strt mcpl-end))
      (save-excursion
	(setq mcpl-pipe-rgn
	      (with-temp-buffer
		(insert mcpl-pipe-rgn)
                (mon-g2be -1)
		(while (search-forward-regexp "\\([[:space:]]|[[:blank:]]\\)" nil t)
		  (replace-match "\n"))
                (mon-g2be -1)
		(while (search-forward-regexp "\\([[:space:]]|\\|[[:blank:]]|$\\)" nil t)
		  (replace-match "\n"))
                (mon-g2be -1)
		(while (search-forward-regexp 
                        "^\\(|[[:space:]]\\|[[:space:]]|\\|[[:blank:]]\\|[[:blank:]]|\\)" nil t)
		  (replace-match "\n"))
		;; :WAS (sort-lines nil (buffer-end 0) (buffer-end 1))
		;;      (uniq-region (buffer-end 0) (buffer-end 1))
		;;      (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
                (sort-lines nil (mon-g2be -1 t) (mon-g2be 1 t)) 
                (uniq-region (mon-g2be -1 t) (mon-g2be 1 t))
                (mon-buffer-sub-no-prop)))
	(delete-region mcpl-strt mcpl-end)
	(insert mcpl-pipe-rgn)))
    (when intrp (message 
                 (concat ":FUNCTION `mon-cln-piped-list' "
                         "-- | piped | list is clean.")))))
;;
;;(defalias ' ' )

;;; ==============================
;;; :PREFIX "mdbul-"
;;; :RENAMED `naf-delete-back-up-list' -> `mon-delete-back-up-list'
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-delete-back-up-list (start end &optional delim)
  "Given a text item-per-line list with no trailing whitespace, move backwards from
point to BOL and deletes 1 char. This effecively puts point on the next line up.
With each successive previous line deleting until point is no longer greater than point-min.
:NOTE Be careful, function can wreck data, evaluate using `with-temp-buffer'.\n
:ALIASED-BY `naf-delete-back-up-list'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-line-pipe-lines', `mon-cln-piped-list', `naf-backup-the-list',
`mon-delete-back-up-list', `mon-line-strings-pipe-bol',
`mon-line-strings-pipe-to-col',  `mon-cln-mail-headers', `mon-cln-csv-fields',
`mon-cln-file-name-string', `mon-cln-up-colon', `mon-cln-whitespace',
`mon-cln-uniq-lines', `mon-cln-control-M'.\n►►►"
  (interactive "r\np") 
  (let* ((mdbul-dlm (cond ((eq delim 1) " ")
                          ((not delim) " ")
                          ((or delim) delim))))
    (mon-toggle-restore-llm nil  
      (let ((mdbul-bak-strt start)
            (mdbul-bak-end end)
            mdbul-bak-pipe)
        (setq mdbul-bak-pipe (mon-buffer-sub-no-prop mdbul-bak-strt mdbul-bak-end))
        (save-excursion
          (setq mdbul-bak-pipe
                (with-temp-buffer
                  (insert mdbul-bak-pipe)
                  (progn	    
                    (mon-cln-trail-whitespace)
                    (mon-g2be 1)
                    (while (> (point) (mon-g2be -1 t))
                      (beginning-of-line)
                      (insert mdbul-dlm)
                      (beginning-of-line)
                      (delete-char -1)
                      (if (bolp) () (beginning-of-line) ))
                    (mon-g2be 1)
                    (while (search-forward-regexp "\1" nil t)
                      (replace-match " " nil nil)))
                  (mon-buffer-sub-no-prop) ))
          (delete-region mdbul-bak-strt mdbul-bak-end)
          (insert mdbul-bak-pipe))))))

;;; ==============================
(defun naf-backup-the-list (start end)
  "Dedicated interactive function name for `mon-delete-back-up-list'.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-line-pipe-lines', `mon-cln-piped-list', `mon-line-strings-pipe-bol',
`mon-line-strings-pipe-to-col'.\n►►►"
  (interactive "r")
  (mon-delete-back-up-list start end))

 
;;; ==============================
(defun mon-cln-philsp (start end &optional intrp)
  "Clean \(apos, date order, etc.\) in philsp scrapes.\n
:REGEXPS-IN `*regexp-philsp-months*', `*regexp-philsp-months*', `*regexp-philsp-apos*',
`*regexp-philsp-location*', `*regexp-philsp-swap-location*', `*regexp-philsp-fix-month-dates*'.
:SEE :FILE mon-regexp-symbols.el\n
Following is the relevant URL containing content apropos this procedure:
:SEE \(URL `http://www.philsp.com/homeville/FMI/a7.htm')\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-ulan', `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib', `mon-cln-loc',
`mon-cln-html-tags'.\n►►►"
  (interactive "r\np")
  (mon-naf-mode-toggle-restore-llm nil
      (save-excursion
        (mon-replace-region-regexp-lists-nonint start end
         *regexp-philsp-months*    *regexp-philsp-apos*    *regexp-philsp-location*
         *regexp-philsp-swap-location* *regexp-philsp-fix-month-dates*)
        (insert (mapconcat #'identity 
                           `("" ;; preceding newline
                             "-"                                                
                             "non-posting-philsp-source:"
                             ":SEE (URL `http://www.philsp.com/homeville/FMI/a7.htm')"
                             ,(mon-accessed-stamp)
                             "---") "\n")))
      (when intrp (message 
                   (concat ":FUNCTION `mon-cln-philsp' "
                           "-- philsp scrape cleaned")))))

;;; ==============================
;;; :TODO <Timestamp: Wednesday February 18, 2009 @ 05:13.39 PM - by MON KEY>
;;;       (if (and (bolp) ;;; search for certain strings to move backup a line
;;;       e.g. to make the following display on the correct line:
;;;       List/Hierarchical Position: Person
;;;       Nationalities: French (preferred) ... etc.
;;; :PREFIX "mcu-"
;;; :MODIFICATIONS <Timestamp: #{2009-08-31T22:53:08-04:00Z}#{09362} - by MON KEY>
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-cln-ulan (start end &optional with-results intrp)
  "Clean periods, linebreaks, whitespace, tabs, etc. from ULAN scrapes in *<BUFFER>*.
Return `mon-accessed-stamp' appended at end of region.\n
:REGEXPS-IN `*regexp-clean-ulan*' VAR in mon-regexp-symbols.el's
For additional specs:\n
:SEE \(URL `http://www.getty.edu/research/conducting_research/vocabularies/ulan/').\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `*regexp-ulan-contribs*', `*regexp-clean-ulan-fields*',
`*regexp-clean-ulan-diacritics*', `*regexp-clean-ulan-dispatch-chars*',
`mon-regexp-clean-ulan-dispatch-chars-TEST'.\n►►►"
  (interactive "r\nP\np")
   (let ((mcu-w/rslt with-results)
         mcu-rslt-cnt)
     (mon-toggle-restore-llm nil  
       (save-excursion
         (let ((mcu-ulanstr)
               (mcu-ulan-start start)
               (mcu-ulan-end end))
           (setq mcu-ulanstr (mon-buffer-sub-no-prop mcu-ulan-start mcu-ulan-end))
           (setq mcu-ulanstr
                 (with-temp-buffer
                   (insert mcu-ulanstr)
                   (let ((mcu-strt-mrk (make-marker))
                         (mcu-end-mrk (make-marker))
                         mcu-lcl-strt ;;start; (buffer-end 0))
                         mcu-lcl-end)  ;;end); (buffer-end 1)))
                     (set-marker mcu-strt-mrk (mon-g2be -1 t))
                     (set-marker mcu-end-mrk (mon-g2be   1 t))
                     (setq mcu-lcl-strt mcu-strt-mrk)
                     (setq mcu-lcl-end mcu-end-mrk)
                     (if mcu-w/rslt
                         (setq mcu-rslt-cnt 
                               (cons `(,@(mon-replace-region-regexp-lists 
                                          mcu-lcl-strt mcu-lcl-end *regexp-clean-ulan* t))  mcu-rslt-cnt))
                         (mon-replace-region-regexp-lists mcu-lcl-strt mcu-lcl-end *regexp-clean-ulan*))
                     (progn
                       (mon-g2be 1)
                       (while (> (point) 1)
                         (if (and (eolp) (bolp))
                             (delete-backward-char 1)
                           (beginning-of-line))
                         (goto-char (1- (point))))
                       (mon-g2be 1)
                       (newline) (insert "-") (newline)
                       (mon-accessed-stamp t) (newline)
                       (mon-g2be -1))
                     (progn
                       (set-marker mcu-strt-mrk (mon-g2be -1 t))
                       (set-marker mcu-end-mrk  (mon-g2be  1 t))
                       (setq mcu-lcl-strt mcu-strt-mrk)
                       (setq mcu-lcl-end mcu-end-mrk)
                       (if mcu-w/rslt
                           (setq mcu-rslt-cnt 
                                 (cons `(,@(mon-replace-region-regexp-lists 
                                            mcu-lcl-strt mcu-lcl-end *regexp-clean-ulan-fields* t))  mcu-rslt-cnt))
                           (mon-replace-region-regexp-lists mcu-lcl-strt mcu-lcl-end *regexp-clean-ulan-fields*)))
                     (progn
                       (mon-g2be -1)
                       (set-marker mcu-strt-mrk (mon-g2be -1 t))
                       (set-marker mcu-end-mrk  (mon-g2be  1 t))
                       (setq mcu-lcl-strt mcu-strt-mrk)
                       (setq mcu-lcl-end mcu-end-mrk)
                       (if mcu-w/rslt
                           (setq mcu-rslt-cnt 
                                 (cons `(,@(mon-replace-region-regexp-lists 
                                            mcu-lcl-strt mcu-lcl-end *regexp-clean-ulan-dispatch-chars* t)) mcu-rslt-cnt))
                         (mon-replace-region-regexp-lists mcu-lcl-strt mcu-lcl-end *regexp-clean-ulan-dispatch-chars*)))
                     (mon-buffer-sub-no-prop) )))
           (delete-region mcu-ulan-start mcu-ulan-end)
           (insert mcu-ulanstr))))
     (when mcu-w/rslt
       (setq mcu-rslt-cnt (nreverse mcu-rslt-cnt))
       (setq mcu-rslt-cnt (mapconcat #'identity mcu-rslt-cnt "\n"))
       (cond (intrp (message "%s" mcu-rslt-cnt))
             ((not intrp) (format "%s" mcu-rslt-cnt))))))

;;; ==============================
;;; :NOTE New version to test for longlines.
;;; :CREATED <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-imdb (start end &optional intrp)
  "Clean Internet Movie Database scrapes from IMDB.\n
Insert the `non-posting-imdb-source' at end of cleaned region.
:REGEXPS-IN `*regexp-clean-imdb*'
:SEE :FILE mon-regexp-symbols.el\n
:SEE (URL `http://www.IMDB.com').\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib',`mon-cln-ulan',
`mon-cln-loc', `mon-cln-philsp', `mon-cln-html-tags',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month',
`mon-trans-cp1252-to-latin1'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-clean-imdb*)
    (non-posting-imdb-source))
  (when intrp (message 
               (concat ":FUNCTION `mon-cln-imdb' "
                       "-- IMDB refs are cleaned"))))

;;; ==============================
;;; :NOTE New version to test for longlines.
;;; :CREATED <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-loc (start end &optional intrp)
  "Fix combining character diacritics from LOC Authority display scrapes.\n
:REGEXPS-IN `*regexp-clean-loc*' in mon-regexp-symbols.el\n
:SEE \(URL `http://authorities.loc.gov/cgi-bin/Pwebrecon.cgi?DB=local&PAGE=First')\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb', `mon-cln-bib', `mon-cln-ulan',
`mon-cln-philsp', `mon-cln-html-tags', `mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month', `mon-trans-cp1252-to-latin1'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil   
    (mon-replace-region-regexp-lists-nonint start end *regexp-clean-loc*))
  (when intrp (message 
               (concat ":FUNCTION `mon-cln-loc' "
                       "-- LOC cruft cleaned"))))

;;; ==============================
;;; :TODO Build a subr to gather the sections of WIKI `Contents' table and
;;;       search buffer for occurences at BOL WSP e.g. "^ Some Contents
;;;       Section\n" replace each Section with with "►►►SOME CONTENTS
;;;       SECTION◄◄◄\n"
;;; :NOTE Newer version tests for longlines.
;;; :CREATED <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-wiki (start end &optional intrp)
  "Replace unwanted wikipedia formatting in region containing scraped wiki text.\n
:REGEXPS-IN `*regexp-clean-wikipedia*' 
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `non-posting-wiki-source', `mon-cln-html-tags', `mon-cln-bib',
`mon-cln-loc', `mon-cln-ulan', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-whitespace', `mon-replace-common-abrevs', `mon-abr-to-month',
`mon-num-to-month', `mon-trans-cp1252-to-latin1'.\n►►►"
  (interactive "r\np")
  (mon-naf-mode-toggle-restore-llm nil
    (mon-replace-region-regexp-lists-nonint start end *regexp-clean-wikipedia*))
  (when intrp (message 
               (concat ":FUNCTION `mon-cln-wiki' "
                       "-- wiki-refs are clean"))))

;;; ==============================
(defun mon-cln-bib (start end &optional intrp)
  "Replace unwanted bibliograhic formatting in region.\n
:REGEXPS-IN `*regexp-clean-bib*' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-loc', `mon-cln-imdb',
`mon-cln-ulan', `mon-cln-philsp', `mon-cln-html-tags'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-clean-bib*))
    (when intrp (message (concat ":FUNCTION `mon-cln-bib' "
                                 "-- bib cruft cleaned"))))

 
;;; ==============================
(defun mon-num-to-month (start end &optional intrp)
  "Replace Months with number of Month.\n
Number of form MM includes leading 0.
Only match on month nums 0-9 when zero padded e.g.\n
01 02 03 04 05 06 07 08 09.\n
:REGEXPS-IN `*regexp-MM->month*' 
:SEE :FILE mon-regexp-symbols.el.\n
:SEE-ALSO `*regexp-month->MM*', `*regexp-month->canonical-ws*',
`*regexp-abrv-dotted-month->canonical*', `regexp-simple-abrv-month2canonical',
`*regexp-philsp-fix-month-dates*'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (mon-replace-region-regexp-lists-nonint start end *regexp-MM->month*)
  (when intrp (message 
               (concat ":FUCTION `mon-num-to-month' "
                       "-- month-number to month-name-strings complete"))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-num-to-month-whitespace (start end &optional intrp)
 "Replace Month number with Month Name.\n
Only match on month nums 0-9 when zero padded e.g.:\n
 01 02 03 04 05 06 07 08 09\n
:NOTE This is a more whitespace aware version of `mon-num-to-month'.\n
:REGEXPS-IN `*regexp-MM->month-whitespace-aware*' 
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `*regexp-MM->month*', `*regexp-month->canonical-ws*', 
`*regexp-abrv-dotted-month->canonical*', `regexp-simple-abrv-month2canonical',
`*regexp-philsp-fix-month-dates*'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-MM->month-whitespace-aware*))
  (when intrp (message 
               (concat ":FUNCTION `mon-num-to-month-whitespace' "
                       "-- month-number (whitespace aware) to month-name-strings complete"))))

;;; ==============================
(defun mon-month-to-num (start end &optional intrp)
  "Replace Months with number of Month. Number of form MM includes leading 0.\n
:REGEXPS-IN `*regexp-month->MM*' 
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `*regexp-MM->month*', `*regexp-month->canonical-ws*', 
`*regexp-abrv-dotted-month->canonical*', `regexp-simple-abrv-month2canonical',
`*regexp-philsp-fix-month-dates*'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-month->MM*))
  (when intrp (message 
               (concat ":FUNCTION `mon-month-to-num' "
                       "-- month-names to number-strings complete"))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-abr-to-month (start end &optional intrp)
  "De-abbreviate English months into canonical form.
Match abbreviated months - with/out trailing `.'
Additionally, will match with/out leading/trailing whitespace.\n
:REGEXPS-IN `*regexp-month->canonical-ws*' 
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-num-to-month', `mon-cln-wiki', `mon-cln-imdb',
`mon-trans-cp1252-to-latin1', `mon-replace-common-abrevs'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-month->canonical-ws*))
  (when intrp (message 
               (concat ":FUNCTION `mon-abr-to-month' "
                       "-- replaced buffer's abbreviated months with canonical form"))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-trans-cp1252-to-latin1 (start end &optional intrp)
  "Convert cp1252 encoded chars to latin1-iso-8859-*.\n
:REGEXPS-IN `*regexp-cp1252-to-latin1*' 
:SEE :FILE mon-regexp-symbols.el.\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-make-iso-latin-1-approximation', `mon-cln-iso-latin-1',
`*iso-latin-1-approximation*', `mon-cln-wiki', `mon-cln-imdb',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-cp1252-to-latin1*))
  (when intrp
    (message (concat
              ":FUNCTION `mon-trans-cp1252-to-latin1' "
              "crappy w32 cp1252 converted into a less crappy iso-8891-1"))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-ital-date-to-eng (start end &optional intrp)
  "Convert Italian date strings (months, days) into equivalent Engrish strings.\n
:REGEXPS-IN `*regexp-ital-to-eng*' 
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb', `mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month', `mon-defranc-places'.\n►►►"
  (interactive "r")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-ital-to-eng*))
  (when intrp  (message (concat ":FUNCTION `mon-ital-date-to-eng' "
                         "-- Italian date-strings converted to Engrish"))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-defranc-dates (start end  &optional intrp)
  "Convert French date strings (months, days) into equivalent Engrish strings.
Matches day of the week, months, abbrevd months, and months with/out diacritics.\n
:REGEXPS-IN `*regexp-defranc-dates*' 
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `naf-mode-french-months', `mon-defranc-places', `mon-defranc-benezit',
`non-posting-benezit-source', `mon-ital-date-to-eng'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-defranc-dates*))
  (when intrp  (message  (concat ":FUNCTION `mon-defranc-dates' "
                                 "-- buffer has been de-francified"))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-defranc-places (start end &optional intrp)
  "Convert French place names into equivalent English place names.
Matches on French language place names with/out diacritics. 
Conversions include with/out all uppercase styled names - for Benezit auctions.\n
:REGEXPS-IN `*regexp-defranc-places*'
:SEE :FILE mon-regexp-symbols.el\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-defranc-benezit', `mon-defranc-dates', `mon-ital-date-to-eng',
`non-posting-benezit-source', `naf-mode-french-months'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-defranc-places*))
  (when intrp (message 
               (concat ":FUNCTION `mon-defranc-places' "
                       "-- buffers place names have been de-francified"))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-cln-benezit (start end &optional intrp)
  "Convert French Benezit terms into equivalent English terms.
Trie to conservatively catch on terms with diacrtics.\n
:REGEXPS-IN `*regexp-defranc-benezit*' 
:SEE :FILE mon-regexp-symbols.el\n
:ALIASED-BY `mon-defranc-benezit'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-clean-benezit-fields', `*regexp-clean-benezit-fields*',
`mon-defranc-dates', `mon-defranc-places', `non-posting-benezit-source'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-defranc-benezit*))
  (when intrp (message 
               (concat ":FUNCTION `mon-cln-benezit' "
                       "-- Benezit terms have been de-francified"))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-18T15:15:31-04:00Z}#{09385} - by MON KEY>
(defun mon-cln-benezit-fields (start end &optional intrp)
  "Normalize Benezit fields in region.\n
:REGEXPS-IN `*regexp-clean-benezit-fields*' 
:SEE :FILE mon-regexp-symbols.el\n
:ALIASED-BY `mon-defranc-benezit-fields'
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-defranc-benezit', `mon-defranc-dates', `mon-defranc-places', 
`non-posting-benezit-source'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
   (mon-replace-region-regexp-lists-nonint start end *regexp-clean-benezit-fields*))
  (when intrp (minibuffer-message
               (concat ":FUNCTION `mon-cln-benezit-fields' "
                       "-- Benezit fields have been normalized"))))

;;; ==============================
(defun mon-replace-common-abbrevs (start end &optional intrp)
  "Replace common abbreviations.\n
Useful for those with `.' at end of string.\n
:REGEXPS-IN `*regexp-common-abbrevs*'
:SEE :FILE mon-regexp-symbols.el\n
:ALIASED-BY `mon-cln-common-abbrevs'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb',`mon-abr-to-month', `mon-num-to-month',
`mon-trans-cp1252-to-latin1'.\n►►►"
  (interactive "r\np")
  (mon-toggle-restore-llm nil  
    (mon-replace-region-regexp-lists-nonint start end *regexp-common-abbrevs*))
  (when intrp (message 
               (concat ":FUNCTION `mon-replace-common-abbrevs' "
                       "-- Buffer abbreviations fully expanded "
                       "to wordforms aka readable Engrish"))))

;;; ==============================
;;; :TODO Function should be extend to append the scrape to a user supplied
;;;       filename and clean any redundant or pre-existing URLs, and optionally
;;;       pass the file on to shell process. Also, need to adjust the script to
;;;       account for rename rules on files wget pulls.
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T16:52:26-04:00Z}#{09332} - by MON KEY>
(defun bug-cln-gilt-group (start end)
  "Clean image links from html source at gilt.com.\n
Useful to get a working list to pass to a useable wget file e.g.:\n
 \"wget -np -A.jpg -i wget-file\".\n
:SEE (URL `http://www.gilt.com')\n
:SEE-ALSO `*regexp-clean-gilt-group*'.\n►►►"
  (interactive "r")
  (progn
    (mon-replace-region-regexp-lists-nonint start end *regexp-clean-gilt-group*)
    (keep-lines "^.*/lg\.jpg$" start end)))

;;; ==============================
(provide 'mon-replacement-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; =======================
;;; mon-replacement-utils.el ends here
;;; EOF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; This is mon-replacement-utils.el
;;; ================================================================
;;; DESCRIPTON:
;;; Provides defuns for processing commonly used regexps for search and replace.
;;; These defuns are called interactively and often used outside of `naf-mode'.
;;; This is why most get the `mon-' prefix.
;;;
;;; :NOTE This file used to be named naf-mode-replacements.el
;;; :AS-OF <Timestamp: #{2009-12-19T14:11:23-05:00Z}#{09516} - by MON>
;;; It has been renamed to mon-replacement-utils.el
;;;
;;; FUNCTIONS:►►►
;;; `mon-replace-regexp-while', `mon-replace-string-while'
;;; `replace-string-pairs-region3', `replace-string-pairs-region-no-props',
;;; `mon-get-list-yorp', `mon-get-list-norp',
;;; `mon-replace-region-regexp-lists-nonint', `mon-replace-region-regexp-lists',
;;; `mon-exchange-slash-and-backslash', `mon-cln-file-name-string',
;;; `mon-regexp-filter', `mon-cln-html-chars',
;;; `mon-cln-html-tags', `mon-canonical-string', `mon-downcase-region-regexp',
;;; `mon-re-number-region', `mon-pipe-list', `mon-cln-piped-list',
;;; `mon-delete-back-up-list', `naf-backup-the-list', `mon-cln-philsp',
;;; `mon-cln-ulan', `mon-cln-imdb', `mon-cln-loc', `mon-cln-wiki',
;;; `mon-cln-bib', `mon-clnBIG-whitespace', `mon-cln-whitespace',
;;; `mon-cln-trail-whitespace', `mon-kill-whitespace', `mon-cln-blank-lines',
;;; `mon-cln-uniq-lines', `mon-cln-spc-tab-eol', `mon-cln-spc-tab-at-eol-in-region',
;;; `mon-cln-control-M', `mon-num-to-month', `mon-num-to-month-whitespace',
;;; `mon-month-to-num', `mon-abr-to-month', `mon-trans-cp1252-to-latin1',
;;; `mon-ital-date-to-eng', `mon-defranc-dates', `mon-defranc-places',
;;; `mon-defranc-benezit', `mon-replace-common-abbrevs', `mon-zippify-region',
;;; `bug-cln-gilt-group', `mon-cln-csv-fields', `mon-cln-xml<-parsed',
;;; `mon-cln-tgm-xml-LF', `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'
;;; `mon-replace-string-pairs-region-no-insert', `mon-cln-xml<-parsed-strip-nil'
;;; `mon-cln-up-colon'
;;; FUNCTIONS:◄◄◄
;;; 
;;; MACROS:
;;; `mon-naf-mode-toggle-restore-llm'
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; VARIABLES:
;;; `*iso-latin-1-approximation*'
;;;
;;; CONSTANTS:
;;;
;;; RENAMED: 
;;; `naf-delete-back-up-list' -> `mon-delete-back-up-list'
;;;
;;; MOVED:
;;; `mon-query-replace-register1<-reg2' -> mon-empty-registers.el
;;; `mon-insert-regexp-template-yyyy'   -> mon-insertion-utils.el
;;;
;;; ALIASED/ADVISED/SUBST'd:
;;; `naf-delete-back-up-list' -> `mon-delete-back-up-list'
;;;
;;; REQUIRES:
;;; Regexps for functions defined here are set with defvar forms in the file:
;;; (URL `./naf-mode/mon-regexp-symbols.el')
;;;
;;; References the following: CONSTANTS OR VARIABLES:
;;; `philsp-months', `philsp-months',`philsp-apos', `philsp-location' 
;;; `philsp-swap-location' `philsp-fix-month-dates', `*regexp-clean-ulan*',
;;; `regexp-clean-imdb', `regexp-clean-loc', `regexp-clean-wikipedia',
;;; `*regexp-clean-bib*', `regexp-cleanBIG-whitespace', `regexp-clean-whitespace',
;;; `regexp-MM2month', `regexp-MM2month-whitespace-aware', `regexp-month2MM',
;;; `*regexp-cp1252-to-latin1*', `*regexp-ital-to-eng*', `*regexp-defranc-dates*',
;;; `*regexp-defranc-places*', `*regexp-defranc-benezit*', `*regexp-common-abbrevs*', 
;;; `regexp-MM2month-whitespace-aware'
;;;
;;; 'cl used by `iso-latin-1-replacements', `deftransmogrify', etc. 
;;;
;;; TODO:
;;; Instances of longlines-mode checks, e.g.:
;;;    (and (buffer-local-value longlines-mode (current-buffer)))
;;; should _maybe_ be updated with:
;;;    (and (boundp 'longlines-mode) (bound-and-true-p longlines-mode))
;;; 
;;; Need function to show whitespace, tab, _and_ `longlines-mode's
;;; hardlines using `longlines-show-hard-newlines'.
;;;
;;; NOTES:
;;; ,----
;;; | :FROM ../emacs/etc/TODO
;;; | ** Implement intelligent search/replace, going beyond query-replace
;;; |    :SEE (URL `http://groups.csail.mit.edu/uid/chi04.pdf'). 
;;; | ,----
;;; | | :NOTE The above link is dead but:
;;; | | :SEE (URL `http://web.mit.edu/noto/Public/thesis/alisa_proposal.html')
;;; | | :SEE (URL `http://groups.csail.mit.edu/uid/projects/clustering/alisa_m-thesis.pdf')
;;; | `----
;;; `----
;;;
;;; SNIPPETS:
;;; Test if we are in a `naf-mode' buffer
;;; (eq (buffer-local-value 'major-mode (current-buffer)) 'naf-mode)
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK:
;;; (URL `http://www.emacswiki.org/emacs/mon-replacement-utils.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-20} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Wednesday April 08, 2009 @ 01:16.02 PM - by MON KEY>
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
;;; `iso-latin-1-replacements', `deftransmogrify', etc.
(eval-when-compile (require 'cl))

;;; ==============================
(require 'mon-regexp-symbols)
;;; ==============================

;;; ==============================
;;; :NOTE MON always forget to use these functions, lets get reminded!
;;; :CREATED <Timestamp: Wednesday May 13, 2009 @ 01:33.46 PM - by MON KEY>
(defalias 'mon-replace-char-in-string 'subst-char-in-string)
(defalias 'replace-char-in-string-mon 'subst-char-in-string)
(defalias 'replace-in-string-mon 'subst-char-in-string)


;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-08T12:56:12-04:00Z}#{09372} - by MON KEY>
(defun mon-is-naf-mode-p ()
  "Test if current-buffer is in `naf-mode'.\n
:EXAMPLE\n(mon-is-naf-mode-p)\n
:CALLED-BY `mon-is-naf-mode-and-llm-p', and other functions which invoke
`mon-naf-mode-toggle-restore-llm' to test for active naf-mode before running
additional longlines-mode checks.\n►►►"
  (eq (buffer-local-value 'major-mode (current-buffer)) 'naf-mode))
;;
;;; :TEST-ME (mon-is-naf-mode-p)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-08T12:59:41-04:00Z}#{09372} - by MON KEY>
(defun mon-is-naf-mode-and-llm-p ()
  "Test if current-buffer is in `naf-mode' and `longlines-mode' is enabled.\n
:EXAMPLE\n(mon-is-naf-mode-and-llm-p)\n
CELLED-BY: `mon-naf-mode-toggle-restore-llm' and other functions which invoke
to test for active naf-mode before evaluating body.\n
:SEE-ALSO `mon-is-naf-mode-p'.\n►►►"
  (if (mon-is-naf-mode-p)
      (buffer-local-value longlines-mode (current-buffer))
    nil))
;;
;;; :TEST-ME (mon-is-naf-mode-and-llm-p)

;;; ==============================
;; ;;; :CREATED <Timestamp: #{2009-09-08T13:18:17-04:00Z}#{09372} - by MON KEY>
(defvar *naf-mode-buffer-local-llm* nil
  "Test if `longlines-mode' is active in buffer.
Automatically becomes buffer-local whenever `naf-mode' initiated in buffer.\n
:SEE-ALSO `mon-is-naf-mode-p' `mon-is-naf-mode-and-llm-p'.\n
:USED-IN `naf-mode'.\n►►►")
;;
;;; ==============================
;;; :NOTE It appears the macro for toggling longlines mode was yanked
;;;       incorrectly as: `mon-is-naf-mode-and-llm-p' ;<-wrong! It should have
;;;       been: `mon-naf-mode-toggle-restore-llm' So, MON wound up inadverdently
;;;       _undoing_ all of the macro wraps in this file :TESTING this again in:
;;;       :FILE naf-insertion-utils.el 
;;;       :AS-OF <Timestamp: #{2009-09-26T18:19:57-04:00Z}#{09396} - by MON>
;;; :CREATED <Timestamp: #{2009-09-08T15:52:50-04:00Z}#{09372} - by MON KEY>
(defmacro mon-naf-mode-toggle-restore-llm (&rest body)
  "Wrapper macro to temporarily toggle `longlines-mode' in `naf-mode' buffers.\n
:SEE-ALSO `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'.\n►►►"
  (declare (indent 1) (debug t))
  (let ((llm-toggled (make-symbol "llm-toggled")))
    `(let ((,llm-toggled (if (mon-is-naf-mode-and-llm-p) t nil)))
       (when ,llm-toggled (longlines-mode 0))
       (unwind-protect
	   ,@body
	 (when ,llm-toggled (longlines-mode 1))))))

;;; ==============================
;; :REGEXP-OPERATIONS-ON-REGION-AND-BUFFER

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `iso-latin-1-approximation'
(defvar *iso-latin-1-approximation* nil 
  "An array mapping ISO-8859-1 characters to ASCII-characters.\n
:SEE-ALSO `mon-cln-iso-latin-1', `mon-make-iso-latin-1-approximation'.
`mon-trans-cp1252-to-latin1'.\n►►►")
;;
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `make-iso-latin-1-approximation'
(defun mon-make-iso-latin-1-approximation ()
"Helper function for `mon-cln-iso-latin-1'.\n
:SEE-ALSO `*iso-latin-1-approximation*',`mon-make-iso-latin-1-approximation',
`mon-trans-cp1252-to-latin1'.\n►►►"
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
;;
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-remove-accents'
(defun mon-cln-iso-latin-1 (string)
  "Replace in string all accented characters with an unaccented version.
This is done only for ISO-5581-1 characters. Return the modified string.\n
:SEE-ALSO `*iso-latin-1-approximation*', `mon-make-iso-latin-1-approximation',
`mon-trans-cp1252-to-latin1'.\n►►►"
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
  ":SEE `mon-transmogrify' for implementation details.\n►►►"
  `(progn
     (unless (and (boundp (quote ,table)) ,table)
       (setq ,table (make-vector 7 0)))
     (put (intern ,string ,table)
          ,language 
          (if (eq ,translated-string :idem) ,string ,translated-string))))
;;
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `localize'
;;; :HIS pjb-invoices.el.restore :WAS `invoice-strings'
;;; :SEE (URL `http://www.informatimago.com/develop/emacs/index.html')
(defun mon-transmogrify (table language string)
  "Lookup in TABLE the STRING, return the translated version for LANGUAGE.\n
:EXAMPLE
\(defvar *transmog-ex* nil \"Localization data for this module.\"\)
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
	  \(\"Dirección de factura :\" :en \"Billing address:\"\)\)\)
\(mon-transmogrify *transmog-ex* :fr  \"Billing address:\"\)
=>\"Adresse de facturation :\"
\(mon-transmogrify *transmog-ex* :es  \"Billing address:\"\)
=>\"Dirección de factura :\"
\(mon-transmogrify *transmog-ex* :en  \"Dirección de factura :\"\)
\"Billing address:\"
\(mon-transmogrify *transmog-ex*  :es  \"Phone:\" \)
=>\"Teléfono :\"
\(mon-transmogrify *transmog-ex*  :fr  \"Phone:\" \)
=>\"Téléphone :\"
\(mon-transmogrify *transmog-ex*  :en \"Téléphone :\"\)
=>\"Phone:\"
Pascal Bourguignon's functions have extensive examples:
:SEE `invoice-strings' in :HIS
:FILE ../site-lisp/pjb/emacs-files/pjb-invoices.el.restore\n
:SEE-ALSO `deftransmogrify'.\n►►►"
  (let ((sym (intern-soft string table)))
    (if sym 
        (let ((result (get sym language))) 
          (if result 
              result
              (mon-transmogrify table :en string)))
        string)))

;;; ==============================
;;; :COURTESY :FILE format.el
;;; :CREATED <Timestamp: #{2009-08-20T16:58:13-04:00Z}#{09344} - by MON KEY>
(defun mon-replace-strings (alist &optional reverse beg end)
  "Do multiple replacements on the buffer.
ALIST is a list of (FROM . TO) pairs, which should be proper arguments to
`search-forward' and `replace-match', respectively.
When REVERSE is non-nil the pairs are (TO . FROM), which allows use of the same
list in both directions if it contains only literal strings. 
Optional args BEG and END specify a region of the buffer on which to operate.\n
:SEE-ALSO `mon-replace-regexp-while'.\n►►►"
  (save-excursion
    (save-restriction
      (or beg (setq beg (point-min)))
      (if end (narrow-to-region (point-min) end))
      (while alist
	(let ((from (if reverse (cdr (car alist)) (car (car alist))))
	      (to   (if reverse (car (car alist)) (cdr (car alist)))))
	  (goto-char beg)
	  (while (search-forward from nil t)
	    (goto-char (match-beginning 0))
	    (insert to)
	    (set-text-properties (- (point) (length to)) (point)
				 (text-properties-at (point)))
	    (delete-region (point) (+ (point) (- (match-end 0)
						 (match-beginning 0)))))
	  (setq alist (cdr alist)))))))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :WAS `clsc-replace-regexp' -> `mon-replace-regexp-while'
(defun mon-replace-regexp-while (regexp to-string)
  "Like `replace-regexp', except be silent about it.\n
:SEE-ALSO `mon-replace-string-while'.\n►►►"
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :WAS `clsc-replace-string' -> `mon-replace-string-while'
(defun mon-replace-string-while (from-string to-string)
  "Like `replace-string', except be silent about it.\n
:SEE-ALSO `mon-replace-regexp-while'.\n►►►"
  (while (re-search-forward from-string nil t)
    (replace-match to-string nil t)))

;;; ==============================
(defun mon-regexp-filter (regexp list)
  "Filter LIST of strings with REGEXP Return filtered list.\n
:EXAMPLE\n\(mon-regexp-filter  \"en\"
 \'\(\"one\" \"two\" \"three\" \"four\" \"five\"
   \"six\" \"seven\" \"eight\" \"nine\" \"ten\"\)\) \n►►►"
      (let (new)
	(dolist (string list)
	  (when (string-match regexp string)
	    (setq new (cons string new))))
	(nreverse new)))

;;; ==============================
;;; :COURTESY Xah Lee :WAS `canonicalString'
;;; :SEE (URL `http://xah-forum.blogspot.com/2009_03_08_archive.html')
;;; 2009-03-10 > emacs lisp > pairs
;;;  On Mar 9, 7:14 pm, Richard Riley wrote:
;;;  ``Could someone please recommend the best way to remove the 3 similar lines
;;;  doing string-match on the "account" assign and iterate a variable list to
;;;  which I can "add-to-list" in other .el libraries for example?''
;;; ,----
;;; | (if (message-mail-p)
;;; |        (save-excursion
;;; | 	 (let* ((from (save-restriction
;;; | 			(message-narrow-to-headers)
;;; | 			(message-fetch-field "from")))
;;; | 		(account (cond ((string-match ".*root.*" from)"richardriley")
;;; | 			       ((string-match ".*richardriley.*" from)"richardriley")
;;; | 			       ((string-match ".*rileyrgdev.*" from)"rileyrgdev"))))
;;; | 	   (setq message-sendmail-extra-arguments (list "-a" account))))))
;;; `----
;;; Xah's response: "Perhaps something like the following, The code is tested:"
;;;
;;; :CREATED <Timestamp: Wednesday April 29, 2009 @ 12:49.37 PM - by MON KEY>
;;; ==============================
(defun mon-canonical-string (from pairs)
  "Return the canonical string of FROM, determined by the pairs in PAIRS.\n
The PAIRS should be a nested vector of the form:\n
\"[[\"a\" \"α\"] [\"b\" \"β\"] [\"γ\" \"g\"] ...]\"
Where the first element is a regex string to be matched with FROM.
When a match is found retunn the second element.
Retrun nil when no match is found.\n
:EXAMPLE\n\(mon-canonical-String \"b\" [[\"a\" \"α\"] [\"b\" \"β\"] [\"γ\" \"g\"]])\n
:SEE-ALSO .\n►►►"
  (let (totalItems matchFound i result)
    (setq totalItems (length pairs))
    (setq matchFound nil) ;; <- Instead of `(setq foundMatch nil)' 
    (setq i 0)
    (while (and (not matchFound)
		(not (= i totalItems)))
      (if (string-match (elt (elt pairs i) 0) from)
	  (progn
	    (setq result (elt (elt pairs i) 1))
	    (setq matchFound t)))
      (setq i (1+ i)))
    result))
;;
;;; :TEST-ME (mon-canonical-string "b" [["a" "α"] ["b" "β"] ["γ" "g"]]) ;-> β
;;; :TEST-ME (mon-canonical-string "a" [["a" "α"] ["b" "β"] ["γ" "g"]]) ;-> α
;;; :TEST-ME (mon-canonical-string "γ" [["a" "α"] ["b" "β"] ["γ" "g"]]) ;-> g

;;; ==============================
;;; :COURTESY Xah Lee :SEE (URL `http://www.xahlee.org')
;;; :MODIFICATIONS `search-forward'   -> `search-forward-regexp'
;;;                `buffer-substring' -> `buffer-substring-no-properties'
(defun replace-string-pairs-region3 (start end mylist)
  "Replace string from in region with each elt's string pairs in MYLIST.
car of mylist is the target string cadr is the replacement string.
cadr can be a subexp to replace with. NOTE: To clean discarding text-properties
use `replace-string-pairs-region-no-props'.\n
:EXAMPLE\n(replace-string-pairs-region3 start end 
 '((\"alpha\" \"A\") (\"beta\" \"B\")))\n
:SEE-ALSO `replace-string-pairs-region', `mon-replace-region-regexp-lists'
`mon-replace-region-regexp-lists-nonint'.\n\n:USED-IN `naf-mode'.\n►►►"
  (let (mystr)
    ;;  (setq mystr (buffer-substring start end))
    ;;    (setq mystr (filter-buffer-substring start end nil t)) 
    (setq mystr (buffer-substring-no-properties start end))
    (save-excursion
      (setq mystr
            (with-temp-buffer
            (insert mystr)
            (mapc
             (lambda (arg)
               (goto-char (point-min))
               (while (search-forward-regexp (car arg) nil t) 
                 (replace-match (cadr arg) t t) ))
             mylist)
            (buffer-string))))
    (delete-region start end)
    (insert mystr)))

;;; ==============================
(defun replace-string-pairs-region-no-props (start end mylist) 
  "Replace string pairs in region do not retain text properties.
Search string and replace string are literal.
car of mylist is the target string cadr is the replacement string.
cadr can be a subexp to replace with.\n
:EXAMPLE\n\(replace-string-pairs-region-no-props start end\n
 '((\"alpha\" \"A\") (\"beta\" \"B\")))\n
:SEE-ALSO `replace-string-pairs-region', 
`mon-replace-string-pairs-region-no-insert'
`mon-replace-region-regexp-lists'
`mon-replace-region-regexp-lists-nonint'.\n
:USED-IN `naf-mode'.\n►►►"
  (let (mystr)
    ;; (setq mystr (filter-buffer-substring start end nil t)) 
    (setq mystr (buffer-substring-no-properties start end))
    (save-excursion
      (setq mystr
            (with-temp-buffer
              (insert mystr)
              (mapc
               (lambda (arg)
                 (goto-char (point-min))
                 (while (search-forward-regexp (car arg) nil t) 
                   (replace-match (cadr arg) t t) ))
               mylist)
              (buffer-string))))
    (delete-region start end)
    (insert mystr)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-24T12:55:24-04:00Z}#{09394} - by MON KEY>
(defun mon-replace-string-pairs-region-no-insert (start end mylist) 
  "Return replace string pairs in region.
Does not retain text properties. Does not insert results.
Search string and replace string are literal.
car of mylist is the target string cadr is the replacement string.
cadr can be a subexp to replace with.\n
:EXAMPLE\n\(mon-replace-string-pairs-region-no-insert
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"►\"\) 2\)
 '((\"^alpha\" \"A\") (\"^beta\" \"B\") (\"^delta\" \"D\") (\"^epsilon\" \"E\")\)\)\n
►\nalpha\nbeta\ndelta\nepsilon\n►\n
:SEE-ALSO `replace-string-pairs-region', `mon-replace-region-regexp-lists'
`mon-replace-region-regexp-lists-nonint'.\n►►►"
  (let (mystr)
    (setq mystr (buffer-substring-no-properties start end))
    (save-excursion
      (setq mystr
            (with-temp-buffer
              (insert mystr)
              (mapc
               (lambda (arg)
                 (goto-char (point-min))
                 (while (search-forward-regexp (car arg) nil t) 
                   (replace-match (cadr arg) t t) ))
               mylist)
              (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))))
    mystr))
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
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-get-list-yorp ()
  "Template for accumulating a list from symbols holding lists.\n
:NOTE Originally a help function to interactively pass symbol bound regexp lists
      as invoked by `mon-replace-region-regexp-lists'.\n
:SEE-ALSO `mon-get-list-norp', `mon-replace-region-regexp-lists-nonint'.\n►►►"
  (interactive)
  (let* ((read-a-list (eval (read-from-minibuffer "Give Symbol holding list:" nil nil t))))
    (while (yes-or-no-p "Enter another list")
      (let* ((temp-list read-a-list)
             (gimme (eval (read-from-minibuffer "Give Symbol holding list:" nil nil t))))
        (setq read-a-list (append temp-list (mapc 'car gimme)))))
    read-a-list))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-get-list-norp (a &rest args)
  "Template form accumulating a list from symbols holding lists.\n
:NOTE Originally a help function to interactively pass symbol bound regexp
lists at invocation. Body is now incorporated in:
:SEE `mon-replace-region-regexp-lists-nonint'.\n
:SEE-ALSO `mon-get-list-yorp', `mon-replace-region-regexp-lists'.\n►►►"
  (let* ((head-norp a)
	 (tail-norp args))
    (while tail-norp
      (setq head-norp (append head-norp (car tail-norp)))
      (setq tail-norp (cdr tail-norp)))
    head-norp))

;;; ==============================
;;; :TODO Rebuild this to actually take a `start' and `end' arg. 
;;; :WORKING-AS-OF (but WRONG!!)
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.30 PM - by MON KEY>
(defun mon-replace-region-regexp-lists-nonint (hd &rest rst)
  "Non-interactive version of `mon-replace-region-regexp-lists'.
Used as a helper function to search over symbol bound regexp lists.\n
:EXAMPLE\n\(defun hah \(start end\) \(interactive \"r\"\)
\(mon-replace-region-regexp-lists-nonint test-aaa test-bbb test-ccc test-ddd\)\)\n
:SEE-ALSO `mon-get-list-yorp', `mon-get-list-norp'.\n►►►"
  (let* ((rep-region)
	 (rep-region-temp)
	 (rep-count)
	 (my-list
	  (let* ((head-norp hd)
		 (tail-norp rst))
	    (while tail-norp
	      (setq head-norp (append head-norp (car tail-norp)))
	      (setq tail-norp (cdr tail-norp)))
	    head-norp))
	 (rep-tip (mapcar 'car my-list))
	 (rep-tail (mapcar 'cadr my-list))
	 (rep-region (buffer-substring-no-properties start end))
	 (rep-region-temp
          (with-temp-buffer
            (insert rep-region)
	    (goto-char (point-min))
		(setq rep-count 0)
		(while rep-tip
		  (while (re-search-forward (car rep-tip) nil t)
		    (replace-match (car rep-tail))
		    (setq rep-count (+ rep-count 1)))
		    ;;(message "Replaced regexp \'%s\' %d times with \'%s\'\n"(car rep-tip) rep-count (car rep-tail)))
		    (when (not (re-search-forward (car rep-tip) nil t))
		      (setq rep-count 0)
		      (setq rep-tip (cdr rep-tip))
		      (setq rep-tail (cdr rep-tail))
		      (goto-char (point-min))))
	    (buffer-string))))
	  (delete-region start end)
	  (insert rep-region-temp)))
;;
;;;(dolist (i (list 'my-list 'my-read-list 'rep-region-temp 'rep-region 'rep-count))
;;;  (progn (makunbound i) (unintern i)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-31T12:12:52-04:00Z}#{09361} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-replace-region-regexp-lists (start end &optional regexp-list with-results intrp)
  "Interactive version of `mon-replace-region-regexp-lists-nonint'.
Prompt for args of symbol bound regexp lists. 
Replace elts of REGEXP-LIST a symbol holding a list of regexp/replace pairs.
Forms of symbol are searched across region until elements of supplied lists
are exhausted. When WITH-RSULTS is non-nil spit replacement results for each
elt of REGEXP-LIST to *Messages*.\n
:SEE-ALSO `mon-get-list-yorp', `mon-get-list-norp'.\n►►►"
  (interactive "r\n\i\nP\np")
  (let* ((rep-region)
	 (rep-region-temp)
	 (rep-count)
	 (my-list (if intrp (call-interactively 'mon-get-list-yorp)
                    regexp-list))
	 (rep-region (buffer-substring-no-properties start end))
         (w/rslts with-results)
         (rep-repl-msg))
         (save-excursion 
           (setq rep-region-temp
          (with-temp-buffer
            (let ((rep-tip (mapcar 'car my-list))
                  (rep-tail (mapcar 'cadr my-list)))
            (setq rep-count 0)
            (insert rep-region)
	    (goto-char (point-min))
            (while rep-tip
              (if (re-search-forward (car rep-tip) nil t)
                  (progn
                    (replace-match (car rep-tail))
                    (setq rep-count (1+ rep-count)))
                (progn 
                  (when w/rslts (setq rep-repl-msg 
                                     (cons
                                      (format "Replaced regexp \'%s\' -> \'%s\' %d times.\n"
                                              (car rep-tip) (car rep-tail) rep-count)
                                      rep-repl-msg)))
                    (setq rep-count 0)
                    (setq rep-tip (cdr rep-tip))
                    (setq rep-tail (cdr rep-tail))
                    (goto-char (point-min)))))
	    (buffer-substring-no-properties (point-min) (point-max)))))
      (delete-region start end)  (insert rep-region-temp))
    (when w/rslts (setq rep-repl-msg (mapconcat 'identity rep-repl-msg ""))
          (cond (intrp (message "%s" rep-repl-msg))
                ((not intrp) (format "%s" rep-repl-msg))))))

;;; ==============================
;;; :CREATED <Timestamp: Monday May 25, 2009 @ 11:46.00 AM - by MON KEY>
(defun mon-cln-csv-fields (field-list &optional delim-slot-w delim-row-w no-list)
  "Clean data pre-formatted for generation of .csv files.
Regexps perform the final conversion. FIELD-LIST is a colon delimited list of
strings each of which is a slot/column key for a given value e.g.:\n
\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \" \"City: \"\"State: \" \"Zipcode: \"\)\n
When non-nil DELIM-SLOT-W specifies delimter seperating values - defalut `,'.\n
When non-nil DELIM-ROW-W specifies delimter seperating value rows - defalut `;'.
Note: don't use `##' as a row or slot delim.\n
When NO-LIST is non-nil return results without parens.\n
Assumes a data structure with fields delimited as:
==============================
\"KEY: \" \"Value\"
\"KEY: \" \"Value\"
\"KEY: \" \"Value\"
==============================\n
:EXAMPLE-DATA-STRUCTURE
==============================\n
Name: Jane Doe
Title: Head of School
Institution: Academy of The Unknown
Address: 111 Some Street
City: Anytown
State: ZZ
Zipcode: 99999\n
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
    \"City: \" \"State: \" \"Zipcode: \"\) \"`\" \"|\" t\) \n►►►"
  (interactive)
  (save-excursion
    (let* ((csv-maker field-list)
	   (pnt-strt (make-marker))
	   (f (first csv-maker))
	   (l (car (last csv-maker)))
	   (dsw (cond (;; Using `##' to recover newlines in final cleanup loop.
                       (string-equal delim-slot-w "##") 
		       (error "## is special in this fuction don't use as a row delimiter"))
		      (delim-slot-w delim-slot-w)
		      (t ",")))
	   (drw (cond (;; Using `##' to recover newlines in final cleanup loop.
                       (string-equal delim-row-w "##") 
		       (error "## is special in this fuction don't use as a row delimiter"))
		      (delim-row-w delim-row-w)
		      (t ";")))
	   (reg-dsw  (format "\\(: %s\\)" dsw))
	   (reg-dsw2  (format "\"%s \"" dsw))  	  
	   (oo))
      (setq oo (mapconcat '(lambda (x) (prin1 x )) csv-maker dsw))
      (setq oo (replace-regexp-in-string  reg-dsw "\" \"" oo))
      (setq oo (replace-regexp-in-string "\\(: \\)" "\"" oo))
      (setq oo (replace-regexp-in-string "\\(\" \"\\)" reg-dsw2 oo))
      (if no-list
	  (setq oo (concat "\"" oo drw "##")) 
	(setq oo (concat "(\"" oo ")" drw "##")))
      (set-marker pnt-strt (point))
      (insert oo)     
      (while csv-maker
	(let* ((srch-hd (car csv-maker))
	       (hd (concat "^\\(\\(" srch-hd "\\)\\(.*\\)\\)$")))
	  (while (search-forward-regexp hd nil t)
	    (cond ((string= srch-hd f)
		   (if no-list
                       ;; _Do not_ put leadnig `('
		       (replace-match (format "\"\\3\"%s" dsw))	
                       ;;_Do_ put leadnig `('
                       (replace-match (format "\(\"\\3\"%s" dsw)))) 
		  ((string= srch-hd l)
		   (if no-list
		       (replace-match (format "\"\\3\"%s##" drw))
		     (replace-match (format "\"\\3\"\)%s##" drw))))
		  (t (replace-match (format"\"\\3\"%s" dsw)))))
	  (setq csv-maker (cdr csv-maker))
	  (goto-char (marker-position pnt-strt))))
      (goto-char (marker-position pnt-strt))
      (while (search-forward-regexp "==============================" nil t)
	(replace-match ""))
      (goto-char (marker-position pnt-strt))
      (while (> (point-max) (point))
	(end-of-line)
	(when (and (not (eobp)) (eolp) (= (char-after (point)) 10))
	  (delete-char 1)))
      (goto-char (marker-position pnt-strt))
      ;; (while (> (point-max) (point))
      (let* ((drw-l (length drw))
	     (drw-end (substring drw (- drw-l 1) drw-l))
	     (drw-e-char (string-to-char drw-end)))
	(while (search-forward-regexp (format "\\(%s##\\)" drw) nil t)
	  (when (and			;(not (eobp)) 
		 (= (char-before (point)) 35)
		 (= (char-before (- (point) 1)) 35)
		 (= (char-before (- (point) 2)) drw-e-char)) 
	    (replace-match (format "%s\n" drw))))))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
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
;;; :CREATED <Timestamp: Saturday May 09, 2009 @ 08:40.30 PM - by MON KEY>
(defun mon-cln-file-name-string (fix-string)
  "Replace chars not allowed in w32 filenams `-'.
Chars Cleaned include:\n
`/',  `:',  `*', `?', `\"', `<', `>', `|, `\\'\n
:SEE-ALSO `mon-exchange-slash-and-backslash'.\n►►►"
  (let* ((ff-prefix '("/"  ":"  "*"  "?" "\"" "<" ">" "|" "\\\\" ))
	 (to-fix fix-string))
	 ;;"\\/:*?\"<>|"))
	 (while ff-prefix
	   (let (fixing)
	     (setq fixing (car ff-prefix))
	     (setq to-fix (replace-regexp-in-string fixing "-" to-fix))
	     (setq ff-prefix (cdr ff-prefix))))
	 ;;(print  to-fix)))
	 to-fix))
;;
;;; :TEST-ME (prin1 (mon-cln-file-name-string "\\/:*?\"<>|"))

;;; ==============================
;;; :TODO This needs to be rebuilt using a better helper function/regex-list per
;;;       the newer `mon-replace-region-regexp-lists-nonint'.
;;; :CREATED <Timestamp: Tuesday February 17, 2009 @ 03:27.10 PM - by MON KEY>
(defun mon-cln-html-chars (start end)
  "Replace  <  by  &lt;  and other similar HTML chars that needs to be encoded.
Replace  & ,  > ,  <  with their respective encoded representation.\n
:SEE-ALSO `mon-cln-html-chars', `mon-nuke-html-tags'.\n►►►"
  (interactive "r")
  (replace-string-pairs-region3 start end
			       '(("&" "&amp;")
				 ("<" "&lt;")
				 (">" "&Gt;"))))

;;; ==============================
;;; :COURTESY Noah Friedman :HIS buffer-fns.el :WAS `nuke-html-tags'
;;; :MODIFICATIONS To the regexps for text between tags \">Some</a>\" and
;;; crowded periods at end-of-sentence and between two chars at end-of-sentence
;;; w/out whitespace.
;;; :CREATED <Timestamp: Tuesday February 17, 2009 @ 03:48.22 PM - by MON KEY>
;;; ==============================
(defun mon-cln-html-tags (beg end)
  "Replace common HTML tags with either newline or nil. Poor man's html formatter.\n
:SEE-ALSO `mon-cln-html-chars', `mon-cln-wiki', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-ulan', `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib', `mon-cln-loc'.\n►►►"
  (interactive "r")
  (let ((table '(("\n"                               . nil) ;; MON note: is this correct?
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
        re sub)
    (let* ((test-llm (mon-is-naf-mode-and-llm-p))
           (is-on test-llm)
           (llm-off))
      (progn
        (when is-on (longlines-mode 0) (setq llm-off 't))
	(save-excursion
	  (save-restriction
	    (narrow-to-region beg end)
	    (while table
	      (setq re (car (car table)))
	      (setq sub (cdr (car table)))
	      (setq table (cdr table))
	      (goto-char (point-min))
	      (cond (sub
		     (while (re-search-forward re nil t)
		       (replace-match sub)))
		    (t (while (re-search-forward re nil t)
                         (delete-region (match-beginning 0) (match-end 0))))))))
        (when llm-off (longlines-mode 1) (setq llm-off 'nil))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-17T15:36:11-05:00Z}#{09472} - by MON KEY>
(defun mon-cln-xml<-parsed (fname &optional insertp intrp)
  "Strip non-sensical strings created by xml-parse-file when trailing CR\LF TAB+
follow an element(s). FNAME is an XML filename path to parse and clean.
When INSERTP is non-nil or called-interactively insert pretty printed lisp
representation of XML file fname at point. Does not move point.
:NOTE Unlike `mon-cln-xml<-parsed-strip-nil' will not strip `nil' from parsed xml.\n
:SEE-ALSO `mon-cln-tgm-xml-LF'\n►►►"
  (interactive "fXML file to parse: \ni\np")
  (let (get-xml)
    (setq get-xml
          (with-temp-buffer
            (prin1 (xml-parse-file fname) (current-buffer))
            (goto-char (point-min))
            (while (search-forward-regexp 
                    "\\( \"\n[\[:blank:]]+\\)\"\\(\\(\\()\\)\\|\\( (\\)\\)\\)" nil t)
                   ;;^^1^^^^^^^^^^^^^^^^^^^^^^^^^2^^3^^^^^^^^^^^^4^^^^^^^^^^^^
            (replace-match "\\2"))
            (pp-buffer)
            (buffer-substring-no-properties (point-min) (point-max))))
    (if (or insertp intrp)
        (save-excursion 
          (newline) 
          (princ get-xml (current-buffer)))
        get-xml)))
;;
;;; :TEST-ME (mon-cln-xml<-parsed <FNMAME>)

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 04, 2009 @ 11:55.40 AM - by MON KEY>
(defun mon-cln-tgm-xml-LF ()
  "Clean EOL whitespace in tgm->XML conversions.\n
:SEE-ALSO `mon-cln-xml<-parsed', `mon-cln-xml<-parsed-strip-nil'.\n►►►"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (progn
        (end-of-line)
        (when
            (and (equal (char-after (point)) 10)
                 (equal (char-after (1+ (point))) 60)
                 (equal (char-after (+ (point) 2)) 47))
          (delete-char 1)))
      (forward-char))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-11-17T17:00:10-05:00Z}#{09472} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-31T20:57:30-04:00Z}#{09362} - by MON KEY>
(defun mon-cln-xml<-parsed-strip-nil (fname &optional insertp intrp)
  "De-string-ification of xml parse results from with `xml-parse-file'.
FNAME is a filename path to be parsed and cleaned. 
When INSERTP is non-nil or called-interactively insert result at point.
Does not move point.
:NOTE Strips `nil' from parsed xml which may not be what you expect.\n
:SEE-ALSO `*regexp-clean-xml-parse*', `mon-cln-xml<-parsed',
`mon-cln-tgm-xml-LF'.\n►►►"
  (interactive "fXML file to parse: \ni\np")
  (let (get-xml)
    (setq get-xml
          (with-temp-buffer
            (prin1  (xml-parse-file fname)
                    (current-buffer))
            (goto-char (point-min))
            (mon-replace-region-regexp-lists (point-min) (point-max) *regexp-clean-xml-parse*)
            (buffer-substring-no-properties (point-min) (point-max))))
    (if (or insertp intrp)
    (save-excursion (newline) (princ get-xml (current-buffer)))
    get-xml)))
;;
;;; :TEST-ME (call-interactively 'mon-cln-xml<-parsed-strip-nil)
        
;;; ==============================
;;; :GLOBAL-KEYBINDING (global-set-key "\C-cu:" 'mon-cln-up-colon)
;;; :CREATED <Timestamp: #{2009-12-19T02:39:48-05:00Z}#{09516} - by MON>
(defun mon-cln-up-colon (start end &optional intrp)
  "Insert colonized string in region at BOL.
When regions string contains a trailing `:' remove it before inserting.\n
:SEE-ALSO `mon-upcase-commented-lines', `mon-downcase-commented-lines'
`mon-line-strings-bq-qt-sym-bol', `mon-line-strings-indent-to-col',
`mon-line-strings-pipe-bol', `mon-line-strings-pipe-to-col',
`mon-line-strings-qt-region'.\n►►►"
  (interactive "i\ni\np")
  (let ((bl (make-marker))
        (el (make-marker))
        (sstr))
    (set-marker bl (if start start (region-beginning)))
    (set-marker el (if end end (region-end)))
    ;;(save-excursion
      (goto-char bl) (princ ":" (current-buffer))
      (setq sstr (delete-and-extract-region bl el))
      (subst-char-in-string 32 45 sstr t)
      (setq sstr (upcase sstr))
      ;; (setq sstr (replace-regexp-in-string "\\(.*\\):" "\\1" sstr ))
      (setq sstr (if (string-match ":" sstr (1- (length sstr)))
                     (substring sstr 0 (1- (length sstr)))
                     sstr))
      (goto-char bl)
      (princ sstr (current-buffer))))

;;; ==============================
;;; :COURTESY Stefan Reichor :HIS xsteve-functions.el
(defun mon-exchange-slash-and-backslash ()
  "Exchange / with \\ and in the current line.
Exchange in region when region-active-p is non-nil.\n
:SEE-ALSO `mon-cln-file-name-string'.\n►►►"
  (interactive)
  (save-match-data
    (save-excursion
      (let ((replace-count 0)
            (eol-pos (if mark-active (region-end) (progn (end-of-line) (point))))
            (bol-pos (if mark-active (region-beginning) (progn (beginning-of-line) (point)))))
        (goto-char bol-pos)
        (while (re-search-forward "/\\|\\\\" eol-pos t)
          (setq replace-count (+ replace-count 1))
          (cond ((string-equal (match-string 0) "/") (replace-match "\\\\" nil nil))
                ((string-equal (match-string 0) "\\") (replace-match "/" nil nil)))
          (message (format "%d changes made." replace-count)))))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 14, 2009 @ 05:47.46 PM - by MON KEY>
(defun mon-downcase-hex-values ()
  "Downcase all CSS Hex values in buffer.\n
:SEE-ALSO `mon-upcase-commented-lines', `mon-downcase-region-regexp'
`mon-cln-up-colon', `mon-line-strings-bq-qt-sym-bol', `
`mon-line-strings-indent-to-col', `mon-line-strings-pipe-bol', 
`mon-line-strings-pipe-to-col'.\n►►►"
  (interactive)
  (save-excursion
    (while (re-search-forward "#\\([A-Z]+\\)")
      (downcase-region (match-beginning 1)(match-end 1)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-17T22:09:21-04:00Z}#{09385} - by MON KEY>
(defun mon-upcase-commented-lines () ;(start end)
  "Upcase everthing in lines that begins with three semicolons \"^;;; \".
Does not move point.\n:NOTE Does not do error checking - so be smart about it.\n
:SEE-ALSO `mon-downcase-commented-lines', `mon-downcase-region-regexp'
`mon-downcase-hex-values' `mon-cln-up-colon', `mon-line-strings-bq-qt-sym-bol',
`mon-line-strings-indent-to-col', `mon-line-strings-pipe-bol', 
`mon-line-strings-pipe-to-col'.\n►►►"
  (interactive) ;;  (interactive "r")
  (save-excursion
    (while (search-forward-regexp "^;;; \\(.*\\)" nil t)
      (upcase-region (match-beginning 1) (match-end 1)))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 05:08.23 PM - by MON KEY>
(defun mon-downcase-region-regexp (downcase-rgxp &optional intrp)
  "Downcase the pattern after point with DOWNCASE-RGXP REPLACE-N times.\n
With following regexp return 4 downcased replacements, replacing only 4 of the 6
upcased hex numbers:\n
:EXAMPLE\n\"^\\(#[A-Z0-9]\\{6,6\\}$\\)\" REPLACE-N => 4
#AEAE4D\n#D29966\n#C3A399\n#D3CD99\n#D0CCCC\n#FFFFCC\n
:SEE-ALSO `mon-downcase-commented-lines', `mon-downcase-region-regexp'
`mon-downcase-hex-values', `mon-cln-up-colon', `mon-line-strings-bq-qt-sym-bol',
`mon-line-strings-indent-to-col', `mon-line-strings-pipe-bol', 
`mon-line-strings-pipe-to-col'.\n►►►"
(interactive "i\np")
  ;; (save-excursion
    (let ((m-count 0)
          (dc-regexp (if intrp (read-regexp "regexp to search") downcase-rgxp))
          (msg))
      (while 
          ;; (or (< m-count replace-n) 
	  (search-forward-regexp dc-regexp nil t)
	(let* ((m-start (match-beginning 1))
	       (m-end  (match-end 1))
	       (m-string (buffer-substring-no-properties m-start m-end)))
	  (downcase-region m-start m-end)
	  (setq m-count (+ m-count 1))
          (setq msg (cons 
                     (format "match-number %d | match start %s | matchend %s  matched: %s"
                             m-count m-start m-end m-string) msg))))
      (when intrp 
        (setq msg (reverse msg))
        (setq msg (mapconcat 'identity msg "\n"))
        (message (concat "Downcased following regions:\n" msg)))))
   
;;; ==============================
;;; :TODO Needs to take a step argument to adjust count-rep's increment on each pass.
;;; :CREATED <Timestamp: Saturday February 28, 2009 @ 02:25.53 PM - by MON KEY>
(defun mon-re-number-region (start end &optional intrp)
  "Sequentially renumber numbers (0-999 inclusive) in a region.
Prompt for starting number - default is 0.
Useful for re-numbering out of sequence numbers in filenames.\n
:SEE-ALSO `mon-insert-string-incr',`mon-insert-numbers-padded',
`mon-number-lines-region', `mon-rectangle-sum-column'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let* ((test-llm (mon-is-naf-mode-and-llm-p))
         (is-on test-llm)
         (llm-off))
      (when is-on (longlines-mode 0) (setq llm-off 't))
      (save-excursion
        (let ((count-rep (read-number "Start from number:" 0))
              (regn-nums)
              (regn-start start)
              (regn-end end))
          (setq regn-nums (buffer-substring-no-properties regn-start regn-end))
          (setq regn-nums
                (with-temp-buffer
                  (insert regn-nums)
                  (goto-char (point-min))
                  (while (re-search-forward "[0-9]\\{1,3\\}" nil t )
                    (replace-match
                     (number-to-string
                      (prog1
                          (identity count-rep)
                        (setq count-rep (1+ count-rep))))))
                  (buffer-substring-no-properties (point-min) (point-max))))
          (delete-region regn-start regn-end)
          (insert regn-nums)))
      (when llm-off (longlines-mode 1) (setq llm-off 'nil))) 
      (when intrp  (message "renumbered region.")))
;;
;;(defalias ' ' )

;;; ==============================
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
;;; :NOTE `mon-zippify-region' has _so_ many other applications...
;;;       Simply change the  `upcase-word' in the `while' clause :).
(defun mon-zippify-region (beg end &optional rand-limit)
  "Randomly capitalize certain words in the region from BEG and END.
Optional third arg RAND-LIMIT means capitalize roughly one out of every
RAND-LIMIT words.\n►►►"
  (interactive "rp")
  (or rand-limit (setq rand-limit 8))
  (save-excursion
    (goto-char beg)
    (if (bobp) nil (forward-word -1) (forward-word 1))
    (while (< (point) end)
      (if (zerop (random rand-limit))
          (upcase-word 1)
        (forward-word 1)))))

;;; ==============================
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-clnBIG-whitespace (start end &optional intrp)
  "Rudely fixes whitespace in region. More comprehensive than `mon-cln-whitespace'
with treatement of leading and trailing whitespace but can't be trusted to DTRT.
For interactive cleaning of trailing tabs and spaces in *buffer*:
:SEE `mon-kill-whitespace', `mon-cln-trail-whitespace', `mon-cln-blank-lines'\n
:SEE-ALSO `mon-cln-imdb', `mon-trans_cp1252_to_latin1',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
   (save-excursion
     (let ((whsp-str)
           (whsp-start start)
           (whsp-end end))
       (setq whsp-str (buffer-substring-no-properties whsp-start whsp-end))
       (setq whsp-str
             (with-temp-buffer
               (insert whsp-str)
               (progn
                 (goto-char (point-min))
                 (while (re-search-forward "[ \t]+$" nil t)
                   (delete-region (match-beginning 0) (match-end 0)))
                 (goto-char (point-min))
                 (while (re-search-forward "[ \t]+$" nil t)
                   (replace-match "" nil nil))
                 (goto-char (point-min))
                 (while (re-search-forward "^\\(\\([[:blank:]]+\\)\\([[:graph:]]\\)\\)"   nil t)
                   (replace-match "\\3" nil nil))
                 (goto-char (point-min))
                 (while (search-forward "\t" nil t)
                   (untabify (1- (point)) (point-max)))
                 (let ((start (point-min))
                       (end (point-max)))
                   (goto-char (point-min))
                   (mon-replace-region-regexp-lists-nonint
                    regexp-clean-big-whitespace))
                 (goto-char (point-min))
                 (while (re-search-forward "^\\([[:blank:]]+$\\)" nil t)
                   (replace-match "\n\n" nil nil))
                 (buffer-substring-no-properties (point-min) (point-max)))))
       (delete-region whsp-start whsp-end)
       (insert whsp-str)))
   (when llm-off (longlines-mode 1) (setq llm-off nil))
   (when intrp (message "Wiki-refs are cleaned."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-cln-whitespace (start end &optional intrp)
  "Clean whitespace in region.\n
:NOTE A more function comprehensive is `mon-clnBIG-whitespace' and is preferred. 
      It handles leading _and_ trailing whitespace _but_ can't always
      be trusted to DTRT.\n
:REGEXPS-IN-VARIABLE `regexp-clean-whitespace' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-kill-whitespace', `mon-cln-trail-whitespace',
`mon-cln-blank-lines', `mon-cln-imdb', `mon-trans_cp1252_to_latin1',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (mon-replace-region-regexp-lists-nonint  regexp-clean-whitespace)
  (when intrp (message "The whitespace has been rudely adjusted.")))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-cln-trail-whitespace ()
    "Indiscriminately clean trailing whitespace in _ENTIRE_ buffer.
Delete any trailing whitespace, converting tabs to spaces.
Use `mon-kill-whitespace' to kill tabs to 1 (one) space.
Operates on entire *buffer* not region. For interactive whitespace
region adjustment use `mon-clnBIG-whitespace', `mon-cln-blank-lines',
or `mon-cln-whitespace'.\n:USED-IN `naf-mode'.\n►►►"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max)))))

;;; ==============================
(defun mon-kill-whitespace ()
  "Kill trailing whitespace (tab and space) in *buffer* not region.
Unlike `mon-cln-trail-whitespace', doesn't convert tabs to spaces.\n
For interactive whitespace region adjustment use `mon-clnBIG-whitespace',
`mon-cln-whitespace', or `mon-cln-blank-lines'.\n
:SEE-ALSO `mon-cln-uniq-lines'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))
;;
;;(defalias ' ' )

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 04:21.17 PM - by MON KEY>
(defun mon-cln-blank-lines (start end &optional intrp)
  "Delete blank and empty lines in region from START to END.\n
:SEE-ALSO `mon-cln-uniq-lines', `delete-blank-lines'.\n►►►"
  (interactive "r\np")
  (save-excursion
    (let ((cln-start start)
	  (cln-end end))		;(message "%s %s" cln-start cln-end))
      (while (> cln-end (point))
	(cond ((mon-line-next-bol-is-eol) (delete-blank-lines))
	      ((mon-line-bol-is-eol) (delete-blank-lines))
	      ((mon-spacep-is-bol) (delete-blank-lines)))
	(forward-line))
      (while (< cln-start (point))
	(cond ((mon-line-previous-bol-is-eol)(delete-blank-lines))
	      ((mon-line-bol-is-eol) (delete-blank-lines))
	      ((mon-spacep-is-bol) (delete-blank-lines)))
	(forward-line -1)))))

;;; ==============================
(defun mon-cln-uniq-lines (beg end &optional intrp)
  "Return the unique lines in region, ommitting ducplicates.
Called programmatically ARGS BEG and END denote the \(region to sort\) and uniquify.
:SEE-ALSO `mon-cln-blank-lines', `delete-blank-lines'. 
:See `uniq-remove-dup-lines' in :FILE uniq.el for additional spec.\n►►►"
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while (re-search-forward (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:32.08 PM - by MON KEY>
(defun mon-cln-spc-tab-eol (&optional intrp)
  "Clean current-line of TAB (char 9) and SPC (char 32) at EOL.\n
:SEE-ALSO `mon-cln-spc-tab-at-eol-in-region' `mon-spacep-at-eol',
`mon-spacep-is-bol', `mon-spacep-not-bol', `mon-spacep',
`mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive "p")
  (while (mon-spacep-at-eol)
    (goto-char (point-at-eol))
    (delete-char -1)
    (goto-char (point-at-bol))))

;;; ==============================
;;; :CALLED-BY `mon-get-proc-buffers-directories' - be careful about modifying.
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 07:01.07 PM - by MON KEY>
(defun mon-cln-spc-tab-at-eol-in-region (start end)
  "Clean region of TAB (char 9) and SPC (char 32) at EOL.\n
:CALLED-BY `mon-get-proc-buffers-directories'.\n
:SEE-ALSO `mon-cln-spc-tab-eol'`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive "r")
  (save-excursion
    (let ((this-region (buffer-substring-no-properties start end))
	  (rtrn))
      (setq rtrn
	    (with-temp-buffer
	      (insert this-region)
	      (goto-char (point-min))
	      (while (mon-spacep-at-eol)
		(mon-cln-spc-tab-eol)
		(goto-char (1+ (point-at-eol))))
	      (buffer-substring (point-min) (point-max))))
      (delete-region start end)
      (insert rtrn))))

;;; ==============================
;;; :COURTESY Stefan Reichor <stefan@xsteve.at> :HIS xsteve-functions.el
(defun mon-cln-control-M (&optional intrp)
  "Remove ^M at EOL in current-buffer.\n►►►"
  (interactive "p")
(let (msg)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "
$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (setq msg (format "%d \C-m removed from buffer." remove-count)))))
  ;;(string-to-char "
")
  ;;(format "\C-m")
  (when intrp (message msg))))

;;; ==============================
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-cln-piped-list (start end &optional intrp)
  "Clean region of piped list formatting i.e. \"Name | Name\".
Piped lists are used in the naf-mode sections:
 Used-for: Appeared-in: Ads-for: Artist-associated: Authors-associated:
 Products-associated: Slogans: Content-and-subjects: etc.\n
:SEE-ALSO `mon-pipe-list', `naf-backup-the-list',`mon-delete-back-up-list'
`mon-line-strings-pipe-bol', `mon-line-strings-pipe-to-col'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let* ((test-llm (mon-is-naf-mode-and-llm-p));;(buffer-local-value longlines-mode (current-buffer)))
         (is-on test-llm)
         (llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
    (let ((pipe-start start)
	  (pipe-end end)
	  (regn-pipe))
      (setq regn-pipe (buffer-substring-no-properties pipe-start pipe-end))
      (save-excursion
	(setq regn-pipe
	      (with-temp-buffer
		(insert regn-pipe)
		(goto-char (point-min))
		(while (re-search-forward "\\([[:space:]]|[[:blank:]]\\)" nil t)
		  (replace-match "\n"))
		(goto-char (point-min))
		(while (re-search-forward "\\([[:space:]]|\\|[[:blank:]]|$\\)" nil t)
		  (replace-match "\n"))
		(goto-char (point-min))
		(while (re-search-forward 
                        "^\\(|[[:space:]]\\|[[:space:]]|\\|[[:blank:]]\\|[[:blank:]]|\\)" nil t)
		  (replace-match "\n"))
		(sort-lines nil (point-min) (point-max))
		(uniq-region (point-min) (point-max))
		(buffer-substring-no-properties (point-min) (point-max))))
	(delete-region pipe-start pipe-end)
	(insert regn-pipe)))
    (when llm-off (longlines-mode 1) (setq llm-off nil))
    (when intrp  (message "Piped | list is clean."))))
;;
;;(defalias ' ' )

;;; ==============================
;;; :RENAMED `naf-delete-back-up-list' -> `mon-delete-back-up-list'
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-delete-back-up-list (start end &optional delim)
  "Given a text item-per-line list with no trailing whitespace, moves backwards from
point to BOL and deletes 1 char. This effecively puts point on the next line up. 
With each successive previous line deleting until point is no longer greater than point-min.
:NOTE Be careful, function can wreck data, evaluate using `with-temp-buffer'.\n
:SEE-ALSO `mon-pipe-list', `mon-cln-piped-list',`naf-backup-the-list',
`mon-delete-back-up-list', `mon-line-strings-pipe-bol',
`mon-line-strings-pipe-to-col'.
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np") 
  (let* ((is-on (mon-is-naf-mode-and-llm-p))
	 (llm-off)
	 (the-delim (cond ((eq delim 1) " ")
                           ((not delim) " ")
                           ((or delim) delim))))
    (when is-on (longlines-mode 0) (setq llm-off t))
    (let ((bak-start start)
	  (bak-end end)
	  (bak-pipe))
      (setq bak-pipe (buffer-substring-no-properties bak-start bak-end))
       (save-excursion
         (setq bak-pipe
               (with-temp-buffer
                 (insert bak-pipe)
                 (progn	    
                   (mon-cln-trail-whitespace)
                   (goto-char (point-max))
                   (while
                       (> (point)(point-min))
                     (beginning-of-line)
                     (insert the-delim)
                     (beginning-of-line)
                     (delete-backward-char 1)
                     (if (bolp)
                         () (beginning-of-line) ))
                   (goto-char (point-max))
                   (while (re-search-forward " " nil t)
                     (replace-match " " nil nil)))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (delete-region bak-start bak-end)
         (insert bak-pipe)))
     (when llm-off (longlines-mode 1) (setq llm-off nil))
     ))
;;
(defalias 'naf-delete-back-up-list 'mon-delete-back-up-list)

;;; ==============================
;;; :CREATED <Timestamp: Wednesday February 11, 2009 @ 04:34.31 PM - by MON KEY>
(defun mon-pipe-list (start end  &optional intrp)
  "Insert \" | \" between each item on an item per line region.
NOTE: Item on last line in region should be an empty line.
Useful for building piped lists in sections of `naf-mode' .naf files including:
  Used-for: Appeared-in: Ads-for: Artist-associated: Authors-associated:
  Products-associated: Slogans: Content-and-subjects: etc.\n
:SEE-ALSO `mon-cln-piped-list',`mon-line-strings-pipe-bol',
`mon-line-strings-pipe-to-col', `naf-backup-the-list',`mon-delete-back-up-list'.
►►►"
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-max))
      (progn
	(unless (and (point-max) (mon-spacep))
	  (newline))
	(while (mon-spacep)
	  (let* ((forward-count (skip-chars-forward "[:space:]"))
		 (backward-count (skip-chars-backward "[:space:]"))
		 (empty (and (eolp) (bolp))))
	    (when empty
	      (delete-backward-char 1))
	    (when (< backward-count 0)
	      (let* ((count-back (abs backward-count)))
		(delete-char count-back)))
            ;; Test for abutting characters.
	    (if (and (not (mon-spacep)) (not (mon-spacep nil t)))
		(progn
		  (insert " | ")
		  (beginning-of-line)))))
	(when (and
	       (mon-spacep)
	       (bolp)
	       (not (mon-spacep nil t)))
	  (progn
	    (backward-char 1)
	    (if (eolp)
		(delete-char 1)
	      (while (mon-spacep nil t)
		(delete-char 1)))
            ;; test for abutting characters.
	    (when (and (not (mon-spacep)) (not (mon-spacep nil t)))
	      (progn
		(insert " | ")
		(beginning-of-line))))))
      (goto-char (point-max))
      ;; catches traling " | " on tail of returned piped-list.
      (progn		   
	(re-search-backward "[[:space:]]|[[:space:]]$" nil t)
	(replace-match ""))))
  (when intrp (message "Finished Piping that list.")))
;;
;;(defalias ' ' )

;;; ==============================
(defun naf-backup-the-list (start end)
  "Dedicated interactive function name for `mon-delete-back-up-list'.\n
:SEE-ALSO `mon-pipe-list', `mon-cln-piped-list', `mon-line-strings-pipe-bol',
`mon-line-strings-pipe-to-col'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r")
  (mon-delete-back-up-list start end))

;;; ==============================
(defun mon-cln-philsp (start end &optional intrp)
  "Clean \(apos, date order, etc.\) in copy/paste scrapes from philsp.
:REGEXPS-IN-VARIABLE `philsp-months', `philsp-months', `philsp-apos',
`philsp-location',`philsp-swap-location', `philsp-fix-month-dates'.
:VARIABLE-IN-FILE mon-regexp-symbols.el\n
Following is the relevant URL containing content apropos this procedure:
:SEE \(URL `http://www.philsp.com/homeville/FMI/a7.htm')\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-ulan', `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib', `mon-cln-loc',
`mon-cln-html-tags'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
	(save-excursion
	  (mon-replace-region-regexp-lists-nonint
	   philsp-months    philsp-apos    philsp-location
	   philsp-swap-location philsp-fix-month-dates)
	  (newline)
	  (insert "-")   (newline)
	  (insert "non-posting-philsp-source:") (newline)
	  (insert  "(URL `http://www.philsp.com/homeville/FMI/a7.htm')") (newline)
	  (mon-accessed-stamp t) (newline)
	  (insert "---"))
	(when llm-off (longlines-mode 1))
      (when intrp (message "philsp-cleaned."))))
;;
;;(defalias ' ' )

;;; ==============================
;;; :TODO <Timestamp: Wednesday February 18, 2009 @ 05:13.39 PM - by MON KEY>
;;;       (if (and (bolp) ;;; search for certain strings to move backup a line
;;;       e.g. to make the following display on the correct line:
;;;       List/Hierarchical Position: Person
;;;       Nationalities: French (preferred) ... etc.
;;; :MODIFICATIONS <Timestamp: #{2009-08-31T22:53:08-04:00Z}#{09362} - by MON KEY>
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-cln-ulan (start end &optional with-results intrp)
  "Clean periods, linebreaks, whitespace, tabs, etc. from ULAN scrapes in buffer.
Operates on region. Returns `mon-accessed-stamp' appends an at end of region.\n
:REGEXPS-IN-VARIABLE `*regexp-clean-ulan*' VAR in mon-regexp-symbols.el's
:SEE-ALSO `*regexp-ulan-contribs*', `*regexp-clean-ulan-fields*',
`*regexp-clean-ulan-diacritics*', `*regexp-clean-ulan-dispatch-chars*'.\n
For additional specs:\n
:SEE \(URL `http://www.getty.edu/research/conducting_research/vocabularies/ulan/').\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\nP\np")
   (let (;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
	 (is-on (mon-is-naf-mode-and-llm-p))
	 (llm-off)
         (w/rslt with-results)
         (rslt-cnt))
     (when is-on (longlines-mode 0) (setq llm-off t))
	(save-excursion
          (let ((ulanstr)
                (ulan-start start)
                (ulan-end end))
            (setq ulanstr (buffer-substring-no-properties ulan-start ulan-end))
            (setq ulanstr
	      (with-temp-buffer
		(insert ulanstr)
		(let ((start); (point-min))
		      (end); (point-max))
                      (start-mrk (make-marker))
                      (end-mrk (make-marker)))
                  (set-marker start-mrk (point-min))
                  (set-marker end-mrk (point-max))
                  (setq lcl-start start-mrk)
                  (setq lcl-end end-mrk)
                  (if w/rslt
                      (setq rslt-cnt 
                            (cons `(,@(mon-replace-region-regexp-lists 
                                       lcl-start lcl-end *regexp-clean-ulan* t))  rslt-cnt))
                    (mon-replace-region-regexp-lists lcl-start lcl-end *regexp-clean-ulan*))
		  (progn
		    (goto-char (point-max))
		    (while (> (point) 1)
		      (if (and (eolp) (bolp))
			  (delete-backward-char 1)
			(beginning-of-line))
		      (goto-char (1- (point))))
		    (goto-char (point-max))
		    (newline) (insert "-") (newline)
		    (mon-accessed-stamp t) (newline)
                    (goto-char (point-min)))
                  (progn
                  (set-marker start-mrk (point-min))   (set-marker end-mrk (point-max))
                  (setq lcl-start start-mrk)           (setq lcl-end end-mrk)
                  (if w/rslt
                      (setq rslt-cnt 
                            (cons `(,@(mon-replace-region-regexp-lists 
                                       lcl-start lcl-end *regexp-clean-ulan-fields* t))  rslt-cnt))
                    (mon-replace-region-regexp-lists lcl-start lcl-end *regexp-clean-ulan-fields*)))
                  (progn
                    (goto-char (point-min))
                    (set-marker start-mrk (point-min))  (set-marker end-mrk (point-max))
                    (setq lcl-start start-mrk)          (setq lcl-end end-mrk)
                  (if w/rslt
                      (setq rslt-cnt 
                            (cons `(,@(mon-replace-region-regexp-lists 
                                       lcl-start lcl-end *regexp-clean-ulan-dispatch-chars* t)) rslt-cnt))
                    (mon-replace-region-regexp-lists lcl-start lcl-end *regexp-clean-ulan-dispatch-chars*)))
		(buffer-substring-no-properties (point-min) (point-max)))))
	       (delete-region ulan-start ulan-end)
	       (insert ulanstr)))
         (when llm-off (longlines-mode 1) (setq llm-off nil))
	  (when w/rslt
	    (setq rslt-cnt (nreverse rslt-cnt))
	    (setq rslt-cnt (mapconcat 'identity rslt-cnt "\n"))
	    (cond ((intrp (message "%s" rslt-cnt)))
		  ((not intrp) (format "%s" rslt-cnt))))))
;;
;;;(progn (makunbound 'mon-cln-ulan) (unintern 'mon-cln-ulan))

;;; ==============================
;;; :NOTE New version to test for longlines.
;;; :CREATED <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-imdb (start end &optional intrp)
  "Clean Internet Movie Database scrapes from IMDB.
Insert the `non-posting-imdb-source' at end of cleaned region.
:REGEXPS-IN-VARIABLE `regexp-clean-imdb'
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE (URL `http://www.IMDB.com').\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib',`mon-cln-ulan',
`mon-cln-loc', `mon-cln-philsp', `mon-cln-html-tags',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month',
`mon-trans_cp1252_to_latin1'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off 't))
     (mon-replace-region-regexp-lists-nonint  regexp-clean-imdb)
     (non-posting-imdb-source)
     (when llm-off (longlines-mode 1)); (setq llm-off 'nil))
     (when intrp (message "IMDB refs are cleaned."))))
;;
;;(defalias ' ' )

;;; ==============================
;;; :NOTE New version to test for longlines.
;;; :CREATED <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-loc (start end &optional intrp)
  "Fix combining character diacritics from LOC Authority display scrapes.
:SEE \(URL `http://authorities.loc.gov/cgi-bin/Pwebrecon.cgi?DB=local&PAGE=First')\n
:REGEXPS-IN-VARIABLE `regexp-clean-loc' in mon-regexp-symbols.el\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb', `mon-cln-bib', `mon-cln-ulan',
`mon-cln-philsp', `mon-cln-html-tags', `mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month', `mon-trans_cp1252_to_latin1'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
 (let ((is-on (mon-is-naf-mode-and-llm-p))
        (llm-off))
   (when is-on (longlines-mode 0) (setq llm-off 't))
   (mon-replace-region-regexp-lists-nonint regexp-clean-loc)
   (when llm-off (longlines-mode 1)); (setq llm-off 'nil))
   (when intrp (message "LOC cruft cleaned."))))
;;
;;(defalias ' ' )

;;; ==============================
;;; :TODO Build a subr to gather the sections of WIKI `Contents' table and
;;;       search buffer for occurences at BOL WSP e.g. "^ Some Contents
;;;       Section\n" replace each Section with with "►►►SOME CONTENTS
;;;       SECTION◄◄◄\n"
;;; :NOTE Newer version tests for longlines.
;;; :CREATED <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-wiki (start end &optional intrp)
  "Replace unwanted wikipedia formatting in region containing scraped wiki text.\n
:REGEXPS-IN-VARIABLE `regexp-clean-wikipedia' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `non-posting-wiki-source', `mon-cln-html-tags', `mon-cln-bib',
`mon-cln-loc', `mon-cln-ulan', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-whitespace', `mon-replace-common-abrevs', `mon-abr-to-month',
`mon-num-to-month', `mon-trans_cp1252_to_latin1'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  ;;; (let ((is-on (mon-is-naf-mode-and-llm-p))
  ;;;       (llm-off))
  ;;;   (when is-on (longlines-mode 0) (setq llm-off t))
  (mon-naf-mode-toggle-restore-llm
      (mon-replace-region-regexp-lists-nonint regexp-clean-wikipedia))
  ;; (when llm-off (longlines-mode 1) (setq llm-off nil))
    (when intrp (message "Wiki-refs are cleaned.")))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-cln-bib (start end &optional intrp)
  "Replace unwanted bibliograhic formatting in region.\n
:REGEXPS-IN-VARIABLE `*regexp-clean-bib*' 
VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-loc', `mon-cln-imdb'
`mon-cln-ulan', `mon-cln-philsp', `mon-cln-html-tags'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
    (mon-replace-region-regexp-lists-nonint *regexp-clean-bib*)
    (when llm-off(longlines-mode 1) (setq llm-off 'nil))
    (when intrp (message "Bib cruft cleaned."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-num-to-month (start end &optional intrp)
  "Replace Month's with number of Month. 
Number of form MM includes leading 0.
Only matches on month nums 0-9 when zero padded e.g.\n
01 02 03 04 05 06 07 08 09.\n
:REGEXPS-IN-VARIABLE `regexp-MM2month' 
:VARIABLE in :FILE mon-regexp-symbols.el.\n
Other date related variables include: 
:SEE-ALSO `regexp-month2MM', `regexp-month2canonical-ws',
`regexp-abrv-dotted-month2canonical', `regexp-simple-abrv-month2canonical',
`philsp-fix-month-dates'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (mon-replace-region-regexp-lists-nonint regexp-MM2month)
  (when intrp (message "Month Number to Month Names Strings Complete.")))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-num-to-month-whitespace (start end &optional intrp)
 "Replace Month number with Month Name. 
This is a more whitespace aware version of `mon-num-to-month'.
Only matches on month nums 0-9 when zero padded e.g.:\n
01 02 03 04 05 06 07 08 09.\n
:REGEXPS-IN-VARIABLE `regexp-MM2month-whitespace-aware' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
Other date related variables include:
:SEE-ALSO `regexp-MM2month', `regexp-month2canonical-ws', 
`regexp-abrv-dotted-month2canonical',`regexp-simple-abrv-month2canonical',
`philsp-fix-month-dates'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
   (mon-replace-region-regexp-lists-nonint regexp-MM2month-whitespace-aware)
   (when llm-off (longlines-mode 1))
   (when intrp 
     (message "Month Number (whitespace aware) to Month Names Strings Complete."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-month-to-num (start end &optional intrp)
  "Replace Month's with number of Month. Number of form MM includes leading 0.\n
:REGEXPS-IN-VARIABLE `regexp-month2MM' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
Other date related variables include: 
:SEE-ALSO `regexp-MM2month', `regexp-month2canonical-ws', 
`regexp-abrv-dotted-month2canonical', `regexp-simple-abrv-month2canonical',
`philsp-fix-month-dates'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
  (mon-replace-region-regexp-lists-nonint regexp-month2MM)
  (when llm-off (longlines-mode 1))
  (when intrp (message "Month Names to Number Strings Complete."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-abr-to-month (start end &optional intrp)
  "De-abbreviate English months into canonical form.
Matches on abbreviated months - with/out trailing `.'
Additionally, will match with/out leading/trailing whitespace.\n
:REGEXPS-IN-VARIABLE `regexp-month2MM' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-num-to-month', `mon-cln-wiki', `mon-cln-imdb',
`mon-trans_cp1252_to_latin1', `mon-replace-common-abrevs'.\n
:USED-IN `naf-mode'.\n►►►"
 (interactive "r\np")
 (let ((is-on (mon-is-naf-mode-and-llm-p))
       (llm-off))
   (when is-on (longlines-mode 0) (setq llm-off t))
  (mon-replace-region-regexp-lists-nonint regexp-month2canonical-ws)
  (when llm-off (longlines-mode 1))
  (when intrp (message "Replaced buffer's abbreviated months with canonical form."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-trans-cp1252-to-latin1 (start end &optional intrp)
  "Convert cp1252 encoded chars to latin1-iso-8859-*.\n
:REGEXPS-IN-VARIABLE `*regexp-cp1252-to-latin1*' 
:VARIABLE in :FILE mon-regexp-symbols.el.\n
:SEE-ALSO `mon-make-iso-latin-1-approximation', `mon-cln-iso-latin-1',
`*iso-latin-1-approximation*', `mon-cln-wiki',`mon-cln-imdb',
`mon-replace-common-abrevs', `mon-abr-to-month',`mon-num-to-month'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
   (mon-replace-region-regexp-lists-nonint *regexp-cp1252-to-latin1*)
   (when llm-off (longlines-mode 1))
   (when intrp
     (message (concat
	       "Crappy w32 cp1252 converted into a less crappy iso-8891-1. \n"
	       "Eventually you will find yourself converting this to utf-8 - *sigh*...")))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-ital-date-to-eng (start end &optional intrp)
  "Convert Italian date strings (months, days) into equivalent Engrish strings.\n
:REGEXPS-IN-VARIABLE `*regexp-ital-to-eng*' 
:VARIABLE in :FILE :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb', `mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month',`mon-defranc-places'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
   (mon-replace-region-regexp-lists-nonint *regexp-ital-to-eng*)
   (when llm-off (longlines-mode 1))
   (when intrp  (message "'Italian Date string converted to Engrish."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-defranc-dates (start end  &optional intrp)
  "Convert French date strings (months, days) into equivalent Engrish strings.
Matches day of the week, months, abbrevd months, and months with/out diacritics.\n
:REGEXPS-IN-VARIABLE `*regexp-defranc-dates*' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `naf-mode-french-months', `mon-defranc-places', `mon-defranc-benezit'
`non-posting-benezit-source',`mon-ital-date-to-eng'.\n\nUsed-in `naf-mode'.\n►►►."
 (interactive "r\np")
 (let ((is-on (mon-is-naf-mode-and-llm-p))
       (llm-off))
   (when is-on (longlines-mode 0) (setq llm-off t))
   (mon-replace-region-regexp-lists-nonint *regexp-defranc-dates*)
   (when llm-off (longlines-mode 1))
   (when intrp  (message "'Buffer has been de-franced."))))
;;
;;;(progn (makunbound 'mon-defranc-dates) (unintern 'mon-defranc-dates))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-defranc-places (start end &optional intrp)
  "Convert French place names into equivalent English place names.
Matches on French language place names with/out diacritics. 
Conversions include with/out all uppercase styled names - for Benezit auctions.\n
:REGEXPS-IN-VARIABLE `*regexp-defranc-places*'
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-defranc-benezit', `mon-defranc-dates', `mon-ital-date-to-eng',
`non-posting-benezit-source', `naf-mode-french-months'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
    (mon-replace-region-regexp-lists-nonint *regexp-defranc-places*)
  (when llm-off (longlines-mode 1))
  (when intrp (message "Buffers place names have been de-francified."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-defranc-benezit (start end &optional intrp)
  "Convert French Benezit terms into equivalent English terms.
Tries to conservatively catch on terms with diacrtics.\n
:REGEXPS-IN-VARIABLE `*regexp-defranc-benezit*' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-clean-benezit-fields', `*regexp-clean-benezit-fields*'
`mon-defranc-dates',`mon-defranc-places', `non-posting-benezit-source'.\n
:USED-IN `naf-mode'.\n►►►"
 (interactive "r\np")
 (let ((is-on (mon-is-naf-mode-and-llm-p))
       (llm-off))
   (when is-on (longlines-mode 0) (setq llm-off t))
  (mon-replace-region-regexp-lists-nonint *regexp-defranc-benezit*)
  (when llm-off (longlines-mode 1))
  (when intrp (message "Benezit terms have been de-francified."))))
;;
;;; Alias :CREATED <Timestamp: #{2009-09-18T15:21:40-04:00Z}#{09385} - by MON KEY>
(defalias 'mon-cln-benezit 'mon-defranc-benezit)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-18T15:15:31-04:00Z}#{09385} - by MON KEY>
(defun mon-cln-benezit-fields (start end &optional intrp)
  "Normalize Benezit fields in region.\n
:REGEXPS-IN-VARIABLE `*regexp-clean-benezit-fields*' 
:VARIABLE in :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-defranc-benezit', `mon-defranc-dates',`mon-defranc-places', 
`non-posting-benezit-source'.\n\n:USED-IN `naf-mode'.\n►►►"
 (interactive "r\np")
 (let ((is-on (mon-is-naf-mode-and-llm-p))
       (llm-off))
   (when is-on (longlines-mode 0) (setq llm-off t))
   (mon-replace-region-regexp-lists-nonint *regexp-clean-benezit-fields*)
   (when llm-off (longlines-mode 1))
   (when intrp (message "Benezit fields have been normalized."))))
;;
(defalias 'mon-defranc-benezit-fields 'mon-cln-benezit-fields)

;;; ==============================
(defun mon-replace-common-abbrevs (start end &optional intrp)
  "Replace common abbreviations.
Useful for those with `.' at end of string.\n
:REGEXPS-IN-VARIABLE `*regexp-common-abbrevs*'
:VARIABLE :FILE mon-regexp-symbols.el\n
:SEE-ALSO `mon-cln-wiki', `mon-cln-imdb',`mon-abr-to-month', `mon-num-to-month'
`mon-trans_cp1252_to_latin1'.\n\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
    (mon-replace-region-regexp-lists-nonint *regexp-common-abbrevs*)
    (when llm-off (longlines-mode 1))
    (when intrp (message "Buffer's pesky abbreviations now fully expanded words aka readable Engrish."))))
;;
;;; :CREATED <Timestamp: #{2009-09-18T15:31:19-04:00Z}#{09385} - by MON KEY>
(defalias 'mon-cln-common-abbrevs 'mon-replace-common-abbrevs)

;;; ==============================
;;; :TODO Function should be extend to append the scrape to a user supplied
;;;       filename and clean any redundant or pre-existing URLs, and optionally
;;;       pass the file on to shell process. Also, need to adjust the script to
;;;       account for rename rules on files wget pulls.
;;; :MODIFICATIONS <Timestamp: #{2009-08-11T16:52:26-04:00Z}#{09332} - by MON KEY>
(defun bug-cln-gilt-group (start end)
  "Clean image links from html source at (URL `http://www.gilt.com').
Useful to get a working list to pass to a useable wget file e.g.:\n
\"wget -np -A.jpg -i wget-file\".\n
:SEE-ALSO `*regexp-clean-gilt-group*'.\n►►►"
  (interactive "r")
  (progn
    (mon-replace-region-regexp-lists-nonint *regexp-clean-gilt-group*)
    (keep-lines "^.*/lg\.jpg$" start end)))

;;; ==============================
(provide 'mon-replacement-utils)
;;; ==============================

;;; =======================
;;; mon-replacement-utils.el ends here
;;; =======================
;;; EOF

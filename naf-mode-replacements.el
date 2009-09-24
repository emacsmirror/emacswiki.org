;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; This is naf-mode-replacements.el
;;; ================================================================
;;; DESCRIPTON:
;;; Provides defuns for processing commonly used regexps for search and replace.
;;; These defuns are called interactively and often used outside of `naf-mode'.
;;; That is why most get the `mon-' prefix.
;;;
;;; FUNCTIONS:►►►
;;; `mon-replace-regexp-while', `mon-replace-string-while'
;;; `replace-string-pairs-region3', `replace-string-pairs-region-no-props',
;;; `mon-get-list-yorp', `mon-get-list-norp',
;;; `mon-replace-region-regexp-lists-nonint', `mon-replace-region-regexp-lists',
;;; `mon-exchange-slash-and-backslash', `mon-cln-file-name-string',
;;; `mon-regexp-filter', `mon-replace-html-chars',
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
;;; `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'
;;; FUNCTIONS:◄◄◄
;;; 
;;; VARIABLES:
;;; `*iso-latin-1-approximation*'
;;;
;;; MACROS:
;;; `mon-naf-mode-toggle-restore-llm'
;;;
;;; RENAMED: 
;;; `naf-delete-back-up-list' -> `mon-delete-back-up-list'
;;;
;;; MOVED:
;;; `mon-query-replace-register1<-reg2' -> ./mon-empty-registers.el
;;; `mon-insert-regexp-template-yyyy' -> ./mon-insertion-utils.el
;;;
;;; ALIASES:
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
;;; (URL `http://www.emacswiki.org/emacs/naf-mode-replacements.el')
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
;;; Copyright (C) 2009 MON KEY 
;;; ==========================
;;; CODE:

;;; ==============================
;;; iso-latin-1-replacements, deftransmogrify, etc.
(eval-when-compile (require 'cl))

;;; ==============================
(require 'mon-regexp-symbols)
;;; ==============================

;;; ==============================
;;; MON always forget to use these functions, lets get reminded!
;;; CREATED: <Timestamp: Wednesday May 13, 2009 @ 01:33.46 PM - by MON KEY>
(defalias 'mon-replace-char-in-string 'subst-char-in-string)
(defalias 'replace-char-in-string-mon 'subst-char-in-string)
(defalias 'replace-in-string-mon 'subst-char-in-string)

;;; ==============================
;;; NOTE: using a macro instead.
;; ;;; CREATED: <Timestamp: #{2009-09-08T13:18:17-04:00Z}#{09372} - by MON KEY>
;; (defvar *naf-mode-buffer-local-llm* nil
;;   "Used in `naf-mode' buffers to test if `longlines-mode' is active
;; Automatically becomes buffer-local whenever naf-mode initiated in buffer.
;; See also; `mon-is-naf-mode-p' `mon-is-naf-mode-and-llm-p'.")

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-08T12:56:12-04:00Z}#{09372} - by MON KEY>
(defun mon-is-naf-mode-p ()
  "Test if current-buffer is in `naf-mode'.\n
EXAMPLE:\n(mon-is-naf-mode-p)\n
CALLED-BY: `mon-is-naf-mode-and-llm-p', and other functions which invoke
`mon-naf-mode-toggle-restore-llm' to test for active naf-mode before running
additional longlines-mode checks."
  (eq (buffer-local-value 'major-mode (current-buffer)) 'naf-mode))

;;;test-me;(mon-is-naf-mode-p)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-08T12:59:41-04:00Z}#{09372} - by MON KEY>
(defun mon-is-naf-mode-and-llm-p ()
  "Test if current-buffer is in `naf-mode' and `longlines-mode' is enabled.
EXAMPLE:\n(mon-is-naf-mode-and-llm-p)\n
CELLED-BY: `mon-naf-mode-toggle-restore-llm' and other functions which invoke
to test for active naf-mode before running evaluating body.
See also; `mon-is-naf-mode-p'."
  (if (mon-is-naf-mode-p)
      (buffer-local-value longlines-mode (current-buffer))
    nil))

;;;test-me;(mon-is-naf-mode-and-llm-p)

;;; ==============================
;;; NO-GOOD: What are we doing wrong here???
;;; CREATED: <Timestamp: #{2009-09-08T15:52:50-04:00Z}#{09372} - by MON KEY>
;; (defmacro mon-naf-mode-toggle-restore-llm (&rest body)
;;   "Wrapper macro to temporarily toggle longlines-mode in `naf-mode' buffers.
;; See also; `mon-is-naf-mode-and-llm-p', `mon-is-naf-mode-p'."
;; ;; (declare (indent 1) (debug t))
;;   (let ((llm-toggled (make-symbol "llm-toggled")))
;;     `(let ((,llm-toggled (if (mon-is-naf-mode-and-llm-p) t nil)))
;;        (when ,llm-toggled (longlines-mode 0))
;;        (unwind-protect
;; 	   ,@body
;; 	 (when ,llm-toggled (longlines-mode 1))))))
;; 1))))))

;;; ==============================
;;; Regexp operations on Region and Buffer.
;;; ==============================

;;; ==============================
;;; COURTESY: Pascal J. Bourguignon HIS: pjb-strings.el WAS: `iso-latin-1-approximation'
(defvar *iso-latin-1-approximation* nil 
  "An array mapping ISO-8859-1 characters to ASCII-characters.
See also; `mon-cln-iso-latin-1', `mon-make-iso-latin-1-approximation'.")
;;
;;; COURTESY: Pascal J. Bourguignon HIS: pjb-strings.el WAS: `make-iso-latin-1-approximation'
(defun mon-make-iso-latin-1-approximation ()
"Helper function for `mon-cln-iso-latin-1'.
See also; `*iso-latin-1-approximation*', `mon-make-iso-latin-1-approximation'."
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
;;; COURTESY: Pascal J. Bourguignon HIS: pjb-strings.el WAS: `string-remove-accents'
(defun mon-cln-iso-latin-1 (string)
  "Replace in string all accented characters with an unaccented version.
This is done only for ISO-5581-1 characters. Returns the modified string.
See also; '*iso-latin-1-approximation*', `mon-make-iso-latin-1-approximation'."
  (unless *iso-latin-1-approximation* 
    (mon-make-iso-latin-1-approximation))
  (let ((result (make-string (length string) 0)))
    (loop for p from 0 below (length string)
       do 
       (aset result p (aref *iso-latin-1-approximation* 
                            (% (aref string p) 256))))
    result))

;;; ==============================
;;; COURTESY: Pascal J. Bourguignon HIS: pjb-strings.el WAS: `deftranslation'
(defmacro deftransmogrify (table string language translated-string)
"See `mon-transmogrify' for implementation details."
  `(progn
     (unless (and (boundp (quote ,table)) ,table)
       (setq ,table (make-vector 7 0)))
     (put (intern ,string ,table)
          ,language 
          (if (eq ,translated-string :idem) ,string ,translated-string))))
;;
;;; COURTESY: Pascal J. Bourguignon HIS: pjb-strings.el WAS: `localize'
;;; HIS: pjb-invoices.el.restore WAS: `invoice-strings'
;;; SEE: (URL `http://www.informatimago.com/develop/emacs/index.html')
(defun mon-transmogrify (table language string)
  "Lookup in TABLE the STRING, return the translated version for LANGUAGE.
EXAMPLE:
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
;;For full example See: `invoice-strings' 
;; in: `../site-lisp/pjb/emacs-files/pjb-invoices.el.restore'"
  (let ((sym (intern-soft string table)))
    (if sym 
        (let ((result (get sym language))) 
          (if result 
              result
              (mon-transmogrify table :en string)))
        string)))

;;; ==============================
;;; COURTESY: format.el
;;; CREATED: <Timestamp: #{2009-08-20T16:58:13-04:00Z}#{09344} - by MON KEY>
(defun mon-replace-strings (alist &optional reverse beg end)
  "Do multiple replacements on the buffer.
ALIST is a list of (FROM . TO) pairs, which should be proper arguments to
`search-forward' and `replace-match', respectively.
Optional second arg REVERSE, if non-nil, means the pairs are (TO . FROM),
so that you can use the same list in both directions if it contains only
literal strings.
Optional args BEG and END specify a region of the buffer on which to operate."
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
;;; COURTESY: Nelson H. F. Beebe HIS: clsc.el VERSION: 1.53 of 2001-05-27
;;; WAS: `clsc-replace-regexp' -> `mon-replace-regexp-while'
(defun mon-replace-regexp-while (regexp to-string)
 "Like `replace-regexp', except be silent about it.\n
See also; `mon-replace-string-while'."
 (while (re-search-forward regexp nil t)
   (replace-match to-string nil nil)))

;;; ==============================
;;; COURTESY: Nelson H. F. Beebe HIS: clsc.el VERSION: 1.53 of 2001-05-27
;;; WAS:`clsc-replace-string' -> `mon-replace-string-while'
(defun mon-replace-string-while (from-string to-string)
  "Like `replace-string', except be silent about it.\n
See also; `mon-replace-regexp-while'."
  (while (re-search-forward from-string nil t)
    (replace-match to-string nil t)))

;;; ==============================
;;; CREATED: <Timestamp: Saturday July 04, 2009 @ 11:55.40 AM - by MON KEY>
(defun mon-cln-tgm-xml-LF ()
  "Clean EOL whitespace in tgm->XML conversions."
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
;;; CREATED: <Timestamp: Monday May 25, 2009 @ 11:46.00 AM - by MON KEY>
(defun mon-cln-csv-fields (field-list &optional delim-slot-w delim-row-w no-list)
  "Clean data pre-formatted for generation of .csv files.
Regexps perform the final conversion. FIELD-LIST is a colon delimited list of
strings each of which is a slot/column key for a given value e.g.:\n
\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \" \"City: \"\"State: \" \"Zipcode: \"\)\n
When DELIM-SLOT-W is non-nil it should specify a delimter seperating
values defalut is `,'.\n
When DELIM-ROW-W is non-nil it should specify a delimter seperating
value rows defalut is `;'.\n
Note: don't use `##' as a row or slot delim.\n
When NO-LIST is non-nil return results without parens.
Assumes a data structure with fields delimited as:\n
==============================
\"KEY: \" \"Value\"
\"KEY: \" \"Value\"
\"KEY: \" \"Value\"
==============================\n
EXAMPLE DATA STRUCTURE:
==============================
Name: Jane Doe
Title: Head of School
Institution: Academy of The Unknown
Address: 111 Some Street
City: Anytown
State: ZZ
Zipcode: 99999
==============================\n
EXAMPLE:
\(mon-cln-csv-fields
'\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \" \"City: \" \"State: \" \"Zipcode: \"\)\)\n
\(mon-cln-csv-fields
'\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \" \"City: \" \"State: \" \"Zipcode: \"\) \"+\" nil\)\n
\(mon-cln-csv-fields
'\(\"Name: \" \"Title: \" \"Institution: \"\"Address: \" \"City: \" \"State: \" \"Zipcode: \"\) nil \"_\" t\)\n
\(mon-cln-csv-fields
'\(\"Name: \" \"Title: \" \"Institution: \"\"Address: \" \"City: \" \"State: \" \"Zipcode: \"\) \"@\" \"_\"\)\n
\(mon-cln-csv-fields
'\(\"Name: \" \"Title: \" \"Institution: \" \"Address: \" \"City: \" \"State: \" \"Zipcode: \"\) \"`\" \"|\" t\)"
  (interactive)
  (save-excursion
    (let* ((csv-maker field-list)
	   (pnt-strt (make-marker))
	   (f (first csv-maker))
	   (l (car (last csv-maker)))
	   (dsw (cond ((string-equal delim-slot-w "##") ;using `##' to recover newlines in final cleanup loop 
		       (error "## is special in this fuction don't use as a row delimiter"))
		      (delim-slot-w delim-slot-w)
		      (t ",")))
	   (drw (cond ((string-equal delim-row-w "##") ;using `##' to recover newlines in final cleanup loop 
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
		       (replace-match (format "\"\\3\"%s" dsw))	;_do not_ put leadnig `('
		     (replace-match (format "\(\"\\3\"%s" dsw)))) ;_do_ put leadnig `('
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
      ;;(while (> (point-max) (point))
      (let* ((drw-l (length drw))
	     (drw-end (substring drw (- drw-l 1) drw-l))
	     (drw-e-char (string-to-char drw-end)))
	(while (search-forward-regexp (format "\\(%s##\\)" drw) nil t)
	  (when (and			;(not (eobp)) 
		 (= (char-before (point)) 35)
		 (= (char-before (- (point) 1)) 35)
		 (= (char-before (- (point) 2)) drw-e-char)) 
	    (replace-match (format "%s\n" drw))))))))

;;;test-me;(mon-cln-csv-fields'("Name: " "Title: " "Institution: " "Address: " "City: " "State: " "Zipcode: "))	      
;;;test-me;(mon-cln-csv-fields'("Name: " "Title: " "Institution: " "Address: " "City: " "State: " "Zipcode: ") "+" nil)  
;;;test-me;(mon-cln-csv-fields'("Name: " "Title: " "Institution: ""Address: " "City: " "State: " "Zipcode: ") nil "_" t) 
;;;test-me;(mon-cln-csv-fields'("Name: " "Title: " "Institution: ""Address: " "City: " "State: " "Zipcode: ") "@" "_")   
;;;test-me;(mon-cln-csv-fields'("Name: " "Title: " "Institution: " "Address: " "City: " "State: " "Zipcode: ") "`" "|" t)

;;; ==============================
;;; COURTESY: (URL `http://www.xahlee.org')
;;; MODIFICATIONS: `search-forward' -> `search-forward-regexp'
(defun replace-string-pairs-region3 (start end mylist)
  "Replace string pairs in region. -used in `naf-mode'.
The search string and replace string literal.  E.g.:
 (replace-string-pairs-region start end 
 '((\"alpha\" \"A\") (\"beta\" \"B\")))
 *(Use `replace-string-pairs-region-no-props' to clean discarding text props.)*"
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
  "Replace string pairs in region. Search string and replace string are literal.
Does not retain text properties.
Uses `filter-buffer-substring' start end nil t"
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
;;; CREATED: <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-get-list-yorp ()
"Template for accumulating a list from symbols holding lists. Originally a help
function to interactively pass symbol bound regexp lists as invoked by
`mon-replace-region-regexp-lists'.\n
See also; `mon-get-list-norp', `mon-replace-region-regexp-lists-nonint'."
  (interactive)
  (let* ((read-a-list (eval (read-from-minibuffer "Give Symbol holding list:" nil nil t))))
   (while (yes-or-no-p "Enter another list")
     (let* ((temp-list read-a-list)
	    (gimme (eval (read-from-minibuffer "Give Symbol holding list:" nil nil t))))
	    (setq read-a-list (append temp-list (mapc 'car gimme)))))
   read-a-list))

;;; ==============================
;;; CREATED: <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
(defun mon-get-list-norp (a &rest args)
  "Template form accumulating a list from symbols holding lists. Originally a help
function to interactively pass symbol bound regexp lists when invoked. Body is
now incorporated in `mon-replace-region-regexp-lists-nonint'.
See also; `mon-get-list-yorp', `mon-replace-region-regexp-lists'."
  (let* ((head-norp a)
	 (tail-norp args))
    (while tail-norp
      (setq head-norp (append head-norp (car tail-norp)))
      (setq tail-norp (cdr tail-norp)))
    head-norp))

;;; ==============================
;;; TODO: Rebuild this to actually take a `start' and `end' arg. 
;;; WORKING-AS-OF: (but WRONG!!)
;;; CREATED: <Timestamp: Wednesday February 04, 2009 @ 07:04.30 PM - by MON KEY>
(defun mon-replace-region-regexp-lists-nonint (hd &rest rst)
  "Non-interactive version of `mon-replace-region-regexp-lists'.
Used as a helper function to search over symbol bound regexp lists as follows:
\(defun hah \(start end\) \(interactive \"r\"\)
\(mon-replace-region-regexp-lists-nonint test-aaa test-bbb test-ccc test-ddd\)\)
See also; `mon-get-list-yorp', `mon-get-list-norp'."
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

;;;   (makunbound 'my-list)(makunbound 'my-read-list)
;;;   (makunbound 'rep-region-temp)(makunbound 'rep-region)
;;;   (makunbound 'rep-count))

;;; ==============================
;;; CREATED: <Timestamp: Wednesday February 04, 2009 @ 07:04.37 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: #{2009-08-31T12:12:52-04:00Z}#{09361} - by MON KEY>
(defun mon-replace-region-regexp-lists (start end &optional regexp-list with-results intrp)
  "Interactive verstion of `mon-replace-region-regexp-lists-nonint'.
Prompt for args of symbol bound regexp lists. Regexp replace the supplied alist
forms of symbol are searched across region until elements of supplied alists
are exhausted. See also; `mon-get-list-yorp', `mon-get-list-norp'."
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
;;; Are we using this anymore, or was this the debug version?
;;; COMMENTED-OUT: <Timestamp: Monday February 09, 2009 @ 09:24.23 PM - by MON KEY>
;;;
;; (defun mon-replace-with-list-given (list-given)
;;   "Helper function for `mon-rplc-reg-with-list'."
;;   (let*((rep-list list-given)
;;         (rep-tip (mapcar 'car list-given))
;;         (rep-tail (mapcar 'cadr list-given)))
;;             (progn
;; 	      (while rep-tip
;;               (while (re-search-forward (car rep-tip) nil t)
;;                 (replace-match (car rep-tail))
;; 		(when (re-search-forward (car rep-tip) nil t)
;; 		  (goto-char (point-min))))
;;                 (prin1 (format "Looked at regexp %s\n" (car rep-tip)))
;; 		(setq rep-tip (cdr rep-tip))
;; 		(prin1 (format "Replaced them with regexp %s\n" (car rep-tail)))
;;                 (setq rep-tail (cdr rep-tail))))))
;;; ==============================

;;; ==============================
;;; COURTESY: Stefan Reichor stefan@xsteve.at HIS: xsteve-functions.el
(defun mon-exchange-slash-and-backslash ()
  "Exchanges / with \\ and in the current line or in the region when a
2region-mark is active.\n
See also; `mon-cln-file-name-string'."
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
;;; CREATED: <Timestamp: Saturday May 09, 2009 @ 08:40.30 PM - by MON KEY>
(defun mon-cln-file-name-string (fix-string)
  "Replaces chars not allowed in w32 filenams `-'.
Cleaned chars include: `/',  `:',  `*', `?', `\"', `<', `>', `|, `\\'
See also; `mon-exchange-slash-and-backslash'."
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

;;;test-me;(prin1 (mon-cln-file-name-string "\\/:*?\"<>|"))

;;; ==============================
(defun mon-regexp-filter (regexp list)
  "Filter LIST of strings with REGEXP.
 \(mon-regexp-filter \"en\" \'\(\"one\" \"two\" \"three\" \"four\" \"five\"
                     \"six\" \"seven\" \"eight\" \"nine\" \"ten\"\)\)
    => \(\"seven\" \"ten\"\)"
      (let (new)
	(dolist (string list)
	  (when (string-match regexp string)
	    (setq new (cons string new))))
	(nreverse new)))

;;; ==============================
;;; TODO: This needs to be rebuilt using a better helper function/regex-list per
;;; the newer `mon-replace-region-regexp-lists-nonint'.
;;; CREATED: <Timestamp: Tuesday February 17, 2009 @ 03:27.10 PM - by MON KEY>
;;; ==============================
(defun mon-replace-html-chars (start end)
  "Replace  <  by  &lt;  and other similar HTML chars that needs to be encoded.
Select a region `mon-replace-html-chars' and have all  & ,  > ,  <  replaced
by their encoded entity.
See also; `mon-replace-html-chars', `mon-nuke-html-tags'."
  (interactive "r")
  (replace-string-pairs-region3 start end
			       '(("&" "&amp;")
				 ("<" "&lt;")
				 (">" "&Gt;"))))

;;; ==============================
;;; COURTESY: Noah Friedman HIS: buffer-fns.el WAS: `nuke-html-tags'
;;; MODIFICATIONS: to the regexps for text between tags \">Some</a>\" and
;;; crowded periods at end-of-sentence and between two chars at end-of-sentence
;;; w/out whitespace.
;;; CREATED: <Timestamp: Tuesday February 17, 2009 @ 03:48.22 PM - by MON KEY>
;;; ==============================
(defun mon-cln-html-tags (beg end)
  "Replace common HTML tags with either newline or nil. Poor man's html formatter.
See also; `mon-replace-html-chars', `mon-cln-wiki', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-ulan', `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib', `mon-cln-loc'."
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
		    (t
		     (while (re-search-forward re nil t)
		       (delete-region (match-beginning 0) (match-end 0))))))))
        	(when llm-off (longlines-mode 1) (setq llm-off 'nil)))
        )))

;;; ==============================
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 12:49.37 PM - by MON KEY>
;;; COURTESY: Xah Lee 
;;; (URL `http://xah-forum.blogspot.com/2009_03_08_archive.html')
;;; WAS: `canonicalString'
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
;;; Xah's response: perhaps something like the following, The code is tested:
;;; ==============================
(defun mon-canonical-string (from pairs)
  "Returns the canonical string of FROM, determined by the pairs in PAIRS.\n
The PAIRS should be a nested vector of the form:\n
\"[[\"a\" \"α\"] [\"b\" \"β\"] [\"γ\" \"g\"] ...]\"
where the first element is a regex string to be matched with FROM.
If match, then the second element is returned.
If no match is found, nil is returned.\n
EXAMPLE:
\(mon-canonical-String \"b\" [[\"a\" \"α\"] [\"b\" \"β\"] [\"γ\" \"g\"]]) returns \"β\"."
  (let (totalItems matchFound i result)
    (setq totalItems (length pairs))
    (setq matchFound nil) ;;; <-- INSTEAD OF: `(setq foundMatch nil)' 
;;; ^^^^^^^^^^^^^^^^^^^^^
    (setq i 0)
    (while (and (not matchFound)
		(not (= i totalItems)))
      (if (string-match (elt (elt pairs i) 0) from)
	  (progn
	    (setq result (elt (elt pairs i) 1))
	    (setq matchFound t)))
      (setq i (1+ i)))
    result))

;;;test-me;(mon-canonical-string "b" [["a" "α"] ["b" "β"] ["γ" "g"]]) ;-> β
;;;test-me;(mon-canonical-string "a" [["a" "α"] ["b" "β"] ["γ" "g"]]) ;-> α
;;;test-me;(mon-canonical-string "γ" [["a" "α"] ["b" "β"] ["γ" "g"]]) ;-> g

;;; ==============================
;;; CREATED: <Timestamp: Thursday May 14, 2009 @ 05:47.46 PM - by MON KEY>
(defun mon-downcase-hex-values ()
  "Downcase all CSS Hex values in buffer.
See also; `mon-downcase-region-regexp'."
  (interactive)
  (save-excursion
    (while (re-search-forward "#\\([A-Z]+\\)")
      (downcase-region (match-beginning 1)(match-end 1)))))


;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-17T22:09:21-04:00Z}#{09385} - by MON KEY>
(defun mon-upcase-commented-lines () ;(start end)
  "Upcase everthing on a lines beginning with three semicolons \"^;;; \".
Does not move point. Does not do error checking - so be smart about it."
  (interactive) ;;  (interactive "r")
  (save-excursion
    (while (search-forward-regexp "^;;; \\(.*\\)" nil t)
      (upcase-region (match-beginning 1) (match-end 1)))))

;;; ==============================
;;; CREATED: <Timestamp: Thursday April 30, 2009 @ 05:08.23 PM - by MON KEY>
(defun mon-downcase-region-regexp (downcase-rgxp &optional intrp)
  "Downcase the pattern after point with DOWNCASE-RGXP REPLACE-N times.
EXAMPLE: 
To return 4 downcased replacements, replace only 4 of the 6 upcased hex numbers
with following RGXP => \"^\\(#[A-Z0-9]\\{6,6\\}$\\)\" REPLACE-N => 4
#AEAE4D\n#D29966\n#C3A399\n#D3CD99\n#D0CCCC\n#FFFFCC\n
See also; `mon-downcase-hex-values'."
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
;;; CREATED: <Timestamp: Saturday February 28, 2009 @ 02:25.53 PM - by MON KEY>
;;; TODO: Needs to take a step argument to adjust count-rep's increment on each pass.
(defun mon-re-number-region (start end &optional intrp)
  "Sequentially renumber numbers (0-999 inclusive) in a region. Prompts for
starting number - defaults to 0. Useful for re-numbering out of sequence
numbers in filenames.\n
See also; `mon-insert-string-incr',`mon-insert-numbers-padded',
`mon-number-lines-region', `mon-rectangle-sum-column'.\n\nUsed in `naf-mode'."
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
;;; CREATED: <Timestamp: Wednesday February 11, 2009 @ 04:34.31 PM - by MON KEY>
(defun mon-pipe-list (start end  &optional intrp)
  "Insert \" | \" between each item on an item per line region.
NOTE: item on last line in region should be an empty line.
Useful for building piped lists in sections of `naf-mode' .naf files including:
Used-for:\nAppeared-in:\nAds-for:\nArtist-associated:\nAuthors-associated:\n
Products-associated:\nSlogans:\nContent-and-subjects:\n
See also; `mon-cln-piped-list', `naf-backup-the-list',`mon-delete-back-up-list'."
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
            ;; test for abutting characters
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
            ;; test for abutting characters
	    (when (and (not (mon-spacep)) (not (mon-spacep nil t)))
	      (progn
		(insert " | ")
		(beginning-of-line))))))
      (goto-char (point-max))
      ;; catches traling " | " on tail of returned piped-list
      (progn		   
	(re-search-backward "[[:space:]]|[[:space:]]$" nil t)
	(replace-match ""))))
  (when intrp (message "Finished Piping that list.")))
;;
;;(defalias ' ' )

;;; ==============================
;;; CREATED: <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-cln-piped-list (start end &optional intrp)
  "Clean region of piped list formatting i.e. \"Name | Name\".
Piped lists are used in NAF sections: Used-for:, Appeared-in:, Ads-for:,
Artist-associated:, Authors-associated:, Products-associated:, Slogans:,
Content-and-subjects:, etc.\n
See also; `mon-pipe-list', `naf-backup-the-list',`mon-delete-back-up-list'.\n
Used in `naf-mode'."
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
;;; CREATED: <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
;;; RENAMED: `naf-delete-back-up-list' -> `mon-delete-back-up-list'
(defun mon-delete-back-up-list (start end &optional delim)
  "Given a text item-per-line list with no trailing whitespace, moves backwards from
point to BOL and deletes 1 char. This effecively puts point on the next line up. 
With each successive previous line deleting until point is no longer greater than point-min.
Be careful this function can wreck your data - so evaluate `with-temp-buffer'.\n
See also; `mon-pipe-list', `mon-cln-piped-list',`naf-backup-the-list',
`mon-delete-back-up-list'.\nUsed in `naf-mode'."
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
(defun naf-backup-the-list (start end)
  "Dedicated interactive function name for `mon-delete-back-up-list'.
See also; `mon-pipe-list', `mon-cln-piped-list'.\nUsed in `naf-mode'."
  (interactive "r")
  (mon-delete-back-up-list start end))

;;; ==============================
(defun mon-cln-philsp (start end &optional intrp)
  "Clean \(apos, date order, etc.\) in copy/paste scrapes from philsp.\n
Symbols evaluated are VARs defined in mon-regexp-symbols.el:
`philsp-months'`philsp-months' `philsp-apos' `philsp-location'
`philsp-swap-location' `philsp-fix-month-dates'.\n
The relevant urls for mon-cln-philsp operations are:
\(URL `http://www.philsp.com/homeville/FMI/a7.htm')\n
Used in `naf-mode'. See also; `mon-cln-wiki', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-ulan', `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib', `mon-cln-loc',
`mon-cln-html-tags'."
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
;;; TODO: <Timestamp: Wednesday February 18, 2009 @ 05:13.39 PM - by MON KEY>
;;; (if (and (bolp) ;;; search for certain strings to move backup a line
;;; e.g. to make the following display on correct line:
;;; List/Hierarchical Position: Person
;;; Nationalities: French (preferred) ... etc.
;;; MODIFICATIONS: <Timestamp: #{2009-08-31T22:53:08-04:00Z}#{09362} - by MON KEY>
;;; CREATED: <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-cln-ulan (start end &optional with-results intrp)
  "Clean periods, linebreaks, whitespace, tabs, etc. from ULAN scrapes in buffer.
Operates on region. When finished appends an `mon-accessed-stamp' at end of region.
Regexps held by `*regexp-clean-ulan*' VAR in mon-regexp-symbols.el's
See also; `*regexp-ulan-contribs*', `*regexp-clean-ulan-fields*',
`*regexp-clean-ulan-diacritics*', `*regexp-clean-ulan-dispatch-chars*'.
Used in `naf-mode'.\nFor additional specs see:\n
\(URL `http://www.getty.edu/research/conducting_research/vocabularies/ulan/')."
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

;;;(progn (makunbound 'mon-cln-ulan) (unintern 'mon-cln-ulan))

;;; ==============================
;;; NOTE: New version to test for longlines.
;;; CREATED: <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-imdb (start end &optional intrp)
  "Clean Internet Movie Database scrapes from (URL `http://www.IMDB.com').
Regexps held by `regexp-clean-imdb' VAR in mon-regexp-symbols.el
Inserts the `non-posting-imdb-source' at end of cleaned region.\n
Used in `naf-mode'. See also; `mon-cln-wiki', `mon-cln-imdb',`mon-cln-bib',
`mon-cln-ulan', `mon-cln-loc', `mon-cln-philsp', `mon-cln-html-tags',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month',
`mon-trans_cp1252_to_latin1'."
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
;;; NOTE: New version to test for longlines
;;; CREATED: <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-loc (start end &optional intrp)
  "Fix combining character diacritics from LOC Authority display scrapes.
SEE: \(URL `http://authorities.loc.gov/cgi-bin/Pwebrecon.cgi?DB=local&PAGE=First')\n
Regexps held by `regexp-clean-loc' in mon-regexp-symbols.el\n
See also; `mon-cln-wiki', `mon-cln-imdb', `mon-cln-bib', `mon-cln-ulan',
`mon-cln-philsp', `mon-cln-html-tags', `mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month', `mon-trans_cp1252_to_latin1'.
Used in `naf-mode'."
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
;;; TODO: Build a subr to gather the sections of WIKI `Contents' table and search 
;;; buffer for occurences at BOL WSP e.g. "^ Some Contents Section\n" replace each
;;; Section with with "►►►SOME CONTENTS SECTION◄◄◄\n"
;;; NOTE: Newer version tests for longlines.
;;; CREATED: <Timestamp: Saturday March 21, 2009 @ 02:57.37 PM - by MON KEY>
(defun mon-cln-wiki (start end &optional intrp)
  "Replace unwanted wikipedia formatting in region containing scraped wiki text.\n
Regexps held by `regexp-clean-wikipedia' VAR in mon-regexp-symbols.el\n
Used in `naf-mode'. See also; `non-posting-wiki-source', `mon-cln-html-tags',
`mon-cln-bib', `mon-cln-loc', `mon-cln-ulan', `mon-cln-philsp', `mon-cln-imdb',
`mon-cln-whitespace', `mon-replace-common-abrevs', `mon-abr-to-month',
`mon-num-to-month', `mon-trans_cp1252_to_latin1'."
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
    (mon-replace-region-regexp-lists-nonint regexp-clean-wikipedia)
    (when llm-off (longlines-mode 1) (setq llm-off nil))
    (when intrp (message "Wiki-refs are cleaned."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-cln-bib (start end &optional intrp)
  "Replace unwanted bibliograhic formatting in region.\n
Regexps held by `*regexp-clean-bib*' VAR in mon-regexp-symbols.el\n
Used in `naf-mode'. See also; `mon-cln-wiki', `mon-cln-loc', `mon-cln-imdb'
`mon-cln-ulan', `mon-cln-philsp', `mon-cln-html-tags'."
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
;;; CREATED: <Timestamp: #{2009-08-31T20:57:30-04:00Z}#{09362} - by MON KEY>
(defun mon-cln-xml<-parsed (fname &optional insertp intrp)
  "Destrinigification of xml parse results geneerated with `xml-parse-file'.
FNAME is a filename path to be parsed and cleaned.
See also; `*regexp-clean-xml-parse*'."
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

;;;test-me;(call-interactively 'mon-cln-xml<-parsed)
;;;(unintern 'mon-cln-xml<-parsed)

;;; ==============================
;;; CREATED: <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-clnBIG-whitespace (start end &optional intrp)
  "Rudely fixes whitespace in region. More comprehensive than `mon-cln-whitespace'
with treatement of leading and trailing whitespace but can't be trusted to DTRT.
For interactive cleaning of trailing tabs and spaces in *buffer* See:
`mon-kill-whitespace', `mon-cln-trail-whitespace', `mon-cln-blank-lines'\n
Regexps held by `regexp-cleanBIG-whitespace' VAR in: mon-regexp-symbols.el\n
Used in `naf-mode'. See also; `mon-cln-imdb', `mon-trans_cp1252_to_latin1',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month'."
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
  "Clean whitespace in region. More comprehensive is `mon-clnBIG-whitespace'
which is preferred handles leading _and_ trailing whitespace _but_ can't always
be trusted to DTRT.\n
Regexps held by `regexp-clean-whitespace' VAR in mon-regexp-symbols.el\n
Used in `naf-mode'. See also; `mon-kill-whitespace', `mon-cln-trail-whitespace',
`mon-cln-blank-lines', `mon-cln-imdb', `mon-trans_cp1252_to_latin1',
`mon-replace-common-abrevs', `mon-abr-to-month', `mon-num-to-month'."
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
or `mon-cln-whitespace'. Used in `naf-mode'."
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
See also; `mon-cln-uniq-lines'.\nUsed in `naf-mode'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))
;;
;;(defalias ' ' )

;;; ==============================
;;; CREATED: <Timestamp: Thursday May 07, 2009 @ 04:21.17 PM - by MON KEY>
(defun mon-cln-blank-lines (start end &optional intrp)
  "Delete blank and empty lines in region from START to END.
See also; `mon-cln-uniq-lines', `delete-blank-lines'."
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
See also; `mon-cln-blank-lines', `delete-blank-lines'. 
See; `uniq-remove-dup-lines' in package \"uniq.el\"."
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
;;; CREATED: <Timestamp: Friday May 08, 2009 @ 05:32.08 PM - by MON KEY>
(defun mon-cln-spc-tab-eol (&optional intrp)
  "Clean current-line of TAB (char 9) and SPC (char 32) at EOL.
See also: `mon-cln-spc-tab-at-eol-in-region'
`mon-spacep-at-eol', `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol-then-graphic'."
  (interactive "p")
  (while (mon-spacep-at-eol)
    (goto-char (point-at-eol))
    (delete-char -1)
    (goto-char (point-at-bol))))

;;; ==============================
;;; NOTE: !!!Used in: `mon-get-proc-buffers-directories' - be careful about modifying.
;;; CREATED: <Timestamp: Friday May 08, 2009 @ 07:01.07 PM - by MON KEY>
(defun mon-cln-spc-tab-at-eol-in-region (start end)
  "Clean region of TAB (char 9) and SPC (char 32) at EOL.
Used in: `mon-get-proc-buffers-directories' 
See also: `mon-cln-spc-tab-eol'`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic'."
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
;;; COURTESY: Stefan Reichor <stefan@xsteve.at> HIS: xsteve-functions.el
(defun mon-cln-control-M (&optional intrp)
  "Remove ^M at EOL in current-buffer."
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
(defun mon-num-to-month (start end &optional intrp)
  "Replace Month's with number of Month. Number of form MM includes leading 0.
Only catches on month nums 0-9 when zero padded e.g. 01 02 03 04 05 06 07 08 09.\n
Regexps held by `regexp-MM2month' VAR in `mon-regexp-symbols.el.\n
Other date related variables used in `naf-mode' include: `regexp-month2MM',
`regexp-month2canonical-ws', `regexp-abrv-dotted-month2canonical',
`regexp-simple-abrv-month2canonical', `philsp-fix-month-dates'."
  (interactive "r\np")
  (mon-replace-region-regexp-lists-nonint regexp-MM2month)
  (when intrp (message "Month Number to Month Names Strings Complete.")))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-num-to-month-whitespace (start end &optional intrp)
 "Replace Month number with Month Name. Whitespace aware version of
`mon-num-to-month'. Only catches on month nums 0-9 when zero padded e.g.
01 02 03 04 05 06 07 08 09. Regexps held by `regexp-MM2month-whitespace-aware'
VAR in mon-regexp-symbols.el
Other date related variables used in `naf-mode' include: `regexp-MM2month',
`regexp-month2canonical-ws', `regexp-abrv-dotted-month2canonical',
`regexp-simple-abrv-month2canonical', `philsp-fix-month-dates'."
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
   (mon-replace-region-regexp-lists-nonint regexp-MM2month-whitespace-aware)
   (when llm-off (longlines-mode 1))
   (when intrp (message "Month Number (whitespace aware) to Month Names Strings Complete."))))
;;
;;(defalias ' ' )

;;; ==============================
(defun mon-month-to-num (start end &optional intrp)
  "Replace Month's with number of Month. Number of form MM includes leading 0.\n
Regexps held by `regexp-month2MM' VAR in mon-regexp-symbols.el\n
Other date related variables used in `naf-mode' include: `regexp-MM2month',
`regexp-month2canonical-ws', `regexp-abrv-dotted-month2canonical',
`regexp-simple-abrv-month2canonical', `philsp-fix-month-dates'."
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
Catches on abbreviated months - with/out trailing `.'
Additionally will catch with/out leading/trailing whitespace.\n
Regexp held by `regexp-month2MM' VAR in naf-mode/mon-regexp-symbols.el\n
Used in `naf-mode'. See also; `mon-num-to-month', `mon-cln-wiki', `mon-cln-imdb',
`mon-trans_cp1252_to_latin1', `mon-replace-common-abrevs'."
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
  "Convert cp1252 encoded chars to latin1-iso-8859-*.
Regexps held by `*regexp-cp1252-to-latin1*' VAR in mon-regexp-symbols.el.\n
See also; `mon-cln-wiki', `mon-cln-imdb',`mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month'.\nUsed in `naf-mode'."
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
Regexps held by `*regexp-ital-to-eng*' VAR in mon-regexp-symbols.el\n
See also; `mon-cln-wiki', `mon-cln-imdb', `mon-replace-common-abrevs',
`mon-abr-to-month', `mon-num-to-month',`mon-defranc-places'.
Used in `naf-mode'."
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
Regexps held by `*regexp-defranc-dates*' VAR in mon-regexp-symbols.el
Catches day of the week, months, abbrevd months, and months with/out diacritics.
Used in `naf-mode'. See also; `naf-mode-french-months', `mon-ital-date-to-eng',
`mon-defranc-benezit', `mon-defranc-places', `non-posting-benezit-source'."
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
  "Convert French place names into equivalent English place names.\n
Catches on French language place names with/out diacritics. 
Conversions include with/out all uppercase styled names - for Benezit auctions.
Regexps held by VARIABLE: `*regexp-defranc-places*' in
FILE: `./naf-mode/mon-regexp-symbols.el')\n
See also; `mon-defranc-benezit', `mon-defranc-dates', `mon-ital-date-to-eng',
`non-posting-benezit-source', `naf-mode-french-months'.\nUsed in `naf-mode'."
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
Regexps at `*regexp-defranc-benezit*' VAR in mon-regexp-symbols.el\n
See also; `mon-clean-benezit-fields', `*regexp-clean-benezit-fields*'
`mon-defranc-dates',`mon-defranc-places', `non-posting-benezit-source'.
\nUsed in `naf-mode'."
 (interactive "r\np")
 (let ((is-on (mon-is-naf-mode-and-llm-p))
       (llm-off))
   (when is-on (longlines-mode 0) (setq llm-off t))
  (mon-replace-region-regexp-lists-nonint *regexp-defranc-benezit*)
  (when llm-off (longlines-mode 1))
  (when intrp (message "Benezit terms have been de-francified."))))
;;
;;; CREATED: <Timestamp: #{2009-09-18T15:21:40-04:00Z}#{09385} - by MON KEY>
(defalias 'mon-cln-benezit 'mon-defranc-benezit)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-18T15:15:31-04:00Z}#{09385} - by MON KEY>
(defun mon-cln-benezit-fields (start end &optional intrp)
  "Normalize Benezit fields in region.
Regexps at `*regexp-clean-benezit-fields*' VAR in mon-regexp-symbols.el\n
See also; `mon-defranc-benezit', `mon-defranc-dates',`mon-defranc-places', 
`non-posting-benezit-source'.\nUsed in `naf-mode'."
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
  "Replace common abbreviations. Esp. useful for those with `.' at end of string.\n
REGEXPS-IN-VARIABLE: `*regexp-common-abbrevs*'.
VARIABLE-IN-FILE: ./mon-regexp-symbols.el\n
See also; `mon-cln-wiki', `mon-cln-imdb',`mon-abr-to-month', `mon-num-to-month'
`mon-trans_cp1252_to_latin1'.\nUsed in `naf-mode'. "
  (interactive "r\np")
  (let ((is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
    (when is-on (longlines-mode 0) (setq llm-off t))
    (mon-replace-region-regexp-lists-nonint *regexp-common-abbrevs*)
    (when llm-off (longlines-mode 1))
    (when intrp (message "Buffer's pesky abbreviations now fully expanded words aka readable Engrish."))))
;;
;;; CREATED: <Timestamp: #{2009-09-18T15:31:19-04:00Z}#{09385} - by MON KEY>
(defalias 'mon-cln-common-abbrevs 'mon-replace-common-abbrevs)

;;; ==============================
;;; COURTESY: Noah Friedman HIS: buffer-fns.el
;;; NOTE: zippify has _so_ many other applications _if_ change the 
;;; `upcase-word' in the `while' clause :).
(defun mon-zippify-region (beg end &optional rand-limit)
  "Randomly capitalize certain words in the region.
From Lisp, wants BEG and END. Optional third arg RAND-LIMIT means capitalize
roughly one out of every RAND-LIMIT words."
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
;;; TODO: Function should be extend to append the scrape to a user supplied filename
;;; and clean any redundant or pre-existing URLs, and optionally pass the file on to
;;; shell process. Also, need to adjust the script to account for rename rules on
;;; files wget pulls.
;;; MODIFICATIONS: <Timestamp: #{2009-08-11T16:52:26-04:00Z}#{09332} - by MON KEY>
(defun bug-cln-gilt-group (start end)
  "Cleans image links from html source at (URL `http://www.gilt.com').
Use to get a working list to pass to a useable wget file for:
\"wget -np -A.jpg -i wget-file\".\nSee also; `*regexp-clean-gilt-group*'."
  (interactive "r")
  (progn
    (mon-replace-region-regexp-lists-nonint *regexp-clean-gilt-group*)
    (keep-lines "^.*/lg\.jpg$" start end)))

;;; ==============================
(provide 'naf-mode-replacements)
;;; ==============================

;;; =======================
;;; naf-mode-replacements.el ends here
;;; =======================
;;; EOF

;;; This is mon-insertion-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-insertion-utils.el Provides Insertion related utilities. 
;;; These are templates and string building utilities that insert into
;;; a buffer and which have become to unwieldly or are otherwise to large
;;; to maintain in the calling file(s).
;;;
;;; FUNCTIONS:►►►
;;; `mon-insert-string-n-fancy-times', `mon-number-lines-region',
;;; `mon-insert-numbers-padded', `mon-insert-string-incr', `mon-line-drop-in-words',
;;; `mon-string-ify-interactively', `mon-insert-test-cases', 
;;; `mon-insert-string-n-times', `mon-lisp-evald', `comment-divider',
;;; `comment-divider-to-col-four', `php-comment-divider', `mon-insert-copyright',
;;; `mon-insert-file-in-dirs', `mon-insert-dirs-in-path', `mon-insert-wht-spc',
;;; `mon-insert-newlines', `mon-insert-defclass-template'
;;; `mon-insert-regexp-template-yyyy'`mon-insert-regexp-template'
;;; `mon-insert-CL-file-template', `mon-insert-CL-package-template',
;;; `mon-insert-user-name-cond', `mon-insert-system-type-cond',
;;; `mon-insert-gnu-licence', `mon-insert-gnu-licence-gfdl' 
;;; `mon-build-copyright-string', `mon-comput-33', `mon-comput-45'
;;; `mon-insert-hgignore-template'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS: 
;;;
;;; CLASSES: 
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*mon-gnu-licences-header*'
;;; `*mon-gnu-license-header-gfdl*'
;;; `*mon-hgignore-template*'
;;; 
;;; ALIASES:
;;;
;;; RENAMED OR MOVED:
;;; Following moved to `mon-doc-help-utils.el' and renamed *insert* -> *help*
;;; `mon-insert-file-dir-functions'  -> `mon-help-file-dir-functions'
;;; `mon-insert-install-info-incantation' -> `mon-help-install-info-incantation'
;;; `mon-insert-rename-incantation' -> `mon-help-rename-incantation'
;;; `mon-insert-tar-incantation' -> `mon-help-tar-incantation'
;;; `mon-insert-info-incantation' -> `mon-help-info-incantation'
;;; `mon-insert-diacritics' -> `mon-help-diacritics'
;;; `mon-insert-naf-mode-file-template' ->`mon-insert-file-template'
;;; `mon-user-evald' -> `mon-lisp-evald'
;;; `mon-incr' -> `mon-insert-string-incr'
;;;
;;; `mon-interactively-stringify' -> `mon-insert-string-ify'
;;; `php-comment-divider' -> `mon-insert-php-comment-divider'
;;; `mon-insert-naf-mode-file-template' -> `mon-insert-file-template'
;;;
;;; MOVED: 
;;; `mon-insert-user-name-cond'   <- ../mon-w32-load.el
;;; `mon-insert-system-type-cond' <- ../mon-w32-load.el
;;; `mon-insert-regexp-template-yyyy' <- ./naf-mode-replacement-utils.el
;;;
;;; `mon-accessed-stamp'          -> ./mon-time-utils.el
;;; `mon-stamp'                   -> ./mon-time-utils.el
;;; `mon-timestamp'               -> ./mon-time-utils.el
;;; `mon-accessed-time-stamp'     -> ./mon-time-utils.el
;;;
;;; DEPRECATED:
;;;
;;; REQUIRES:
;;; ./mon-time-utils.el  ;anything that uses a time-stamp
;;; LINK: (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')

;;; ./mon-utils.el 
;;; LINK: (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;; `mon-sublist-gutted' :CALLED-BY `mon-insert-gnu-licence-gfdl' 
;;;
;;; TODO:
;;; Build a wget template.
;;; (defun mon-wget-url (extension url put-in)
;;; wget -nd -r -l1 --noparent -A."file-extension" "url"
;;; See; Stefan Reichor's defun in `mon-url-utils' (commented - not active)
;;;
;;; Build a progn makunbound unintern template, e.g.:
;;; ;;;(progn (makunbound 'VAR-OR-FUNCTION) 
;;; ;;; (unintern 'VAR-OR-FUNCTION))
;;; 
;;; PUBLIC-LINK:
;;; (URL `http://www.emacswiki.org/emacs/mon-insertion-utils.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-08-27} - by MON KEY> ;;+/-
;;;
;;; FILE-CREATED: 
;;; <Timestamp: Wednesday April 08, 2009 @ 12:37.06 PM - by MON KEY>
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
(eval-when-compile (require 'cl)) ;why not?
;;; ==============================

;;; ==============================
;;; :LINK (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;; Anything that uses a time-stamp.
(require 'mon-time-utils) 


;;; ==============================
(defun mon-insert-dirs-in-path (dir-list dir-path)
  "Directory elts held by symbol DIR-LIST (list of strings) are inserted in DIR-PATH.
Does minimal error checking, shouldn't be trusted for unfamilar pathnames.\n►►►"
  (interactive "XSymbol holding list of directorys to create:\nDPath to insert to :")
  (let ((make-list dir-list) 
	(into-path dir-path)	
	(do-it))
    (progn
      (cond
        ((file-exists-p (directory-file-name (file-name-as-directory into-path)))
         (setq do-it 1))
        ((not (file-exists-p (directory-file-name (file-name-as-directory  into-path)))) 
         ;;(setq do-it '()) do-it))
         (setq do-it ())))
      (cond 
        (do-it
            (while make-list
              (let ((put-list (car make-list)))
                (make-directory (concat into-path "/" put-list)))
              (setq make-list (cdr make-list))))
        ((not do-it)
         (message "Directory '%s' doesn't exist" into-path))))))
;;
;;; :TEST-ME (setq t-dir-list '("abc" "def" "ghi")) 
;;; :TEST-ME (mon-insert-dirs-in-path t-dir-list default-directory)

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 06, 2009 @ 03:32.25 PM - by MON KEY>
(defun mon-insert-file-in-dirs (make-dir-list insert-text extension) ;&optional starting-dir
  "Each element in list MAKE-DIR-LIST inserts a directory, file, and file text.
A more general solution is `mon-insert-dirs-in-path'. This function is a 
generic version of `mon-insert-naf-file-in-dirs'. However, this function 
is _NOT_ appropriate for creation of `naf-mode' specific files.\n
Directory's name and file's name are taken from elt in MAKE-DIR-LIST. Directory
is created relative to current buffer's DEFAULT-DIRECTORY. File's contents are
automatically inserted according to text held in INSERT-TEXT. EXTENSION argument
provides the file extension for file.\n
When generating the 'directory-name' and 'file-name' components from the elts
of MAKE-DIR-LIST tests for the presence of:\n
   '(', ')', '_', '.', ':', ';', ' '        <--Removed and/or replaced by '-'.\n

Generation of file-name the EXTENSION string tests for the presence of:\n
   '(', ')', '_', '.', ':', ';', ' '   <--Removed and/or replaced by '-' and ',_'.\n
A second pass checks EXTENSION for leading '.' and inserts it when missing.\n
Format of list for MAKE-DIR-LIST should be as follows:\n
 (setq make-my-dirs 
   '(\"Name_Part1\" \"Name (Part2)\"
     \"Name, :Part3\" \"Name Part4;\"))\n\n
:EXAMPLE\nAssuming buffer's default directory is \"c:/home/my-dirs\"
Invoking the form with args make-my-dirs my-insert-text \".dbc\":\n
\(mon-insert-file-in-dirs make-my-dirs my-insert-text \".dbc\")\n
Or, interactively; M-x mon-insert-naf-file-in-dirs\n
=> minibuffer-prompt: Give Symbol holind dir/file list : make-my-dirs
=> minibuffer-prompt: Give Symbol holing insert-text : my-insert-text
=> minibuffer-prompt: File extenison :dbc \n
Creates the following directors and files in c:/home/my-dirs :
              .
              |-- Name-(Part2)
              |   `-- Name-Part2.dbc
              |-- Name-Part1
              |   `-- Name-Part1.dbc
              |-- Name-Part3
              |   `-- Name,_Part3.dbc
              `-- Name-Part4
                  `-- Name-Part4.dbc\n
 NOTE: dir & file names with 'flagged' characters are transformed where appropriate."
  (interactive "XGive Symbol holding dir/file list :\nXGive Symbol holing insert-text :\nsFile extenison :")
  (while make-dir-list 
    (let* ((file-dir make-dir-list)
	   (current-file (car make-dir-list))
	   (dotted-ext extension)
	   (clean-it)
	   (cf))
      (let ((cf current-file)
	    (clean-it current-file))
	(setq cf (replace-regexp-in-string "[,_.:;[:space:]]" "-" cf))
	(when (string-match "-+" cf)
	  (setq cf (replace-match "-" nil nil cf)))
	(when (string-match "-$" cf)
	  (setq cf (replace-match "" nil nil cf)))
	;;    (setq clean-it cf)
	(setq clean-it (replace-regexp-in-string "[_().:;[:space:]]" "-" clean-it))
	(when (string-match "-+" clean-it)
	  (setq clean-it (replace-match "-" nil nil clean-it)))
	(when (string-match ",-" clean-it)
	  (setq clean-it (replace-match ",_" nil nil clean-it)))
	(when (string-match "-$" clean-it)
	  (setq clean-it (replace-match "" nil nil clean-it)))
	(when (string-match "\\([\(\)_\:[:space:]]\\)" dotted-ext) 
	  (setq dotted-ext (replace-match "." nil nil dotted-ext)))
	(when (string-match "\\(^[^.]\\)" dotted-ext) 
	  (setq dotted-ext (concat "." dotted-ext)))
	(make-directory cf)
	(with-temp-file 
	    (concat default-directory cf "/" clean-it dotted-ext)
	  (insert (format "%s" insert-text))))
      (pop make-dir-list))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 11, 2009 @ 02:23.20 PM - by MON KEY>
(defun mon-insert-wht-spc (spc-cnt)
  "Insert a space char SPC-CNT times.
Called interactively SPC-CNT is a prefix arg.
space is:\n- character: SPC (32, #o40, #x20);\n- code point: 0x20\n- name: SPACE\n
Note: does not inherit stickiness of adjoining chars `text-properties-at'.\n
:SEE-ALSO `mon-insert-newlines', `mon-insert-unicode',
`mon-insert-string-n-times', `mon-insert-string-n-fancy-times'.\n►►►"
  (interactive "N")
  (insert-char ? spc-cnt nil))
;;
;;; :TEST-ME (mon-insert-wht-spc 8)

;;; ==============================
;;; :CREATED <Timestamp: Friday June 12, 2009 @ 11:18.21 AM - by MON KEY>
(defun mon-insert-newlines (nl-cnt)
  "Insert a newline char NL-CNT times.
Called interactively NL-CNT is a prefix arg.
newline is:\n- code point: '0x0A';\n- character: C-j (10, #o12, #xa);\n- old-name: LINE FEED (LF);\n
Note: does not inherit stickiness of adjoining chars `text-properties-at'\n
:SEE-ALSO `mon-insert-wht-spc', `mon-insert-unicode', 
`mon-insert-string-n-times', `mon-insert-string-n-fancy-times'.\n►►►"
  (interactive "N")
  (insert-char ?\n nl-cnt nil))
;;
;;; :TEST-ME (mon-insert-newlines 3)

;;; ==============================
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-insert-string-n-fancy-times 
  (&optional intr-p put-count string-to-put nlp with-delim delim whtsp-p)
  "Insert a string n times with optional delimters and/or newlines. 
When interactive or INTR-P non-nil queries for: PUT-COUNT (number to insert);
STRING-TO-PUT (string to insert); NLP intersperse strings with newlines
When NLP is non-nil the arg to WHTSP-P is discarded (prevents trailing whitespace).
WITH-DELIM interleave STRING-TO-PUT with the supplied delimiter. When WITH-DELIM 
is non-nil the arg to WHTSP-P is discarded so supply DELIM with with any
required whitespace as part of the DELIMs arg if this is what you want. When NLP
and WITH-DELIM are nil and WHTSP-P is non-nil interleave STRING-TO-PUT with
whitespace.\nExamples:\n
\(mon-insert-string-n-fancy-times nil 4 \"bubba\" nil nil nil t)\n==> bubba bubba bubba bubba\n
\(mon-insert-string-n-fancy-times nil 4 \"bubba\" t nil nil)\n==>bubba\nbubba\nbubba\nbubba\n
\(mon-insert-string-n-fancy-times nil 3 \"bubba\" nil t \" | \")\n==>bubba | bubba | bubba |\n
\(mon-insert-string-n-fancy-times nil 3 \"bubba\" nil t \", \")\n==>bubba, bubba, bubba, \n 
\(mon-insert-string-n-fancy-times nil 3 \"bubba\" t t \" |\")\n==>bubba |\nbubba |\nbubba |\n
:SEE-ALSO `mon-insert-string-n-fancy-times', `mon-insert-string-incr', `mon-re-number-region',
`mon-insert-wht-spc', `mon-insert-newlines', `mon-insert-unicode'.\n►►►"
  (interactive "p")
  (let*   ((call-int-p (if intr-p t ))  ;(message "%s" call-int-p)))
           (put-cnt     (if call-int-p (read-number "How many puts? :") put-count))
           (put-string  (if call-int-p (read-string "String to put :") string-to-put))
           (delimp (if call-int-p (yes-or-no-p "With delimiter? :") with-delim))
           (nlp (if call-int-p (yes-or-no-p "Insert newlines? :") nlp))
           (delim-put (let* ((dt (and delimp))     ; t when delim t
                             (df (and (not delimp))) ; t when delim nil
                             (nlt (and nlp))         ; t when newlines t
                             (nlf (and (not nlp))))  ; t when newlines nil
                        (cond 
                         ;; do newline, do delim	                   
                         ((and nlt dt)	
                          ((lambda () (concat 
                                       (if call-int-p  
                                           (read-string "Delimit range with (add whitespace if needed) :") 
                                         delim)
                                       "\n"))))
                         ;; do delim, no newline 	 	
                         ((and dt nlf)		
                          ((lambda () 
                             (if call-int-p 
                                 (read-string "Delimit range with (add whitespace if needed) :") 
                               delim))))
                         ;; do newline, no delim
                         ((and nlt df) "\n")	
                         ;;no delim ; no newline ;whatabout whitespace?
                         ((and df nlf)	   
                          (if call-int-p 
                              (if (yes-or-no-p "No Delim, No Newline, Wanna Interleave range with whitespace? :")
                                  " "
                                "")
                            (if whtsp-p
                                " "
                              ""))))))
           (test-llm (and (buffer-local-value longlines-mode (current-buffer))))
           (is-on (and test-llm))
           (llm-off))
    (progn
      (when is-on (longlines-mode 0) (setq llm-off 't))
      (save-excursion
        (while (> put-cnt 0)
          (insert (format "%s%s" put-string delim-put))
          (setq put-cnt (1- put-cnt))))
      (when llm-off
        (longlines-mode 1) (setq llm-off 'nil)))))

;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:41.37 PM - by MON KEY>
(defun mon-insert-string-n-times (put-count string-to-put)
"Inserts the string provided n times.
Prompt for: How many puts? <-- number of times to put string.
             String to put: <--- String  to insert.\n
:SEE-ALSO `mon-insert-string-n-fancy-times', `mon-insert-string-incr', `mon-re-number-region',
`mon-insert-wht-spc', `mon-insert-newlines', `mon-insert-unicode'.\n►►►"
  (interactive (list (read-number "How many puts?")
		     (read-string "String to put:")))
    (dotimes (i
	      put-count
	      t)
      (insert string-to-put)))

;;; ==============================
(defun mon-number-lines-region (start end &optional beg)
  "Numbers the lines in a region. The default value is 1.
To begin at zero, type M-0 \\[mon-number-lines-region].
Negative numbers are also supported. Numbers are right aligned followed by a
period and a space. If point is at the beginning of the region, the lines
will be numbered in descending order. If a line is already prefixed with a
number, it will be overwritten with the new value.\n
Unlike `mon-insert-string-incr' allows prefix starting value - numeric argument.\n
:SEE-ALSO `mon-insert-numbers-padded', `mon-re-number-region', 
`mon-rectangle-sum-column'.\n►►►"
  (interactive "*r\np")
  (let* ((lines (count-lines start end))
	 (from (or beg 1))
	 (to (+ lines (1- from)))
	 (numbers (number-sequence from to))
	 (width (max (length (int-to-string lines))
		     (length (int-to-string from)))))
    (if (= start (point))
	(setq numbers (reverse numbers)))
    (goto-char start)
    (dolist (n numbers)
      (beginning-of-line)
      (save-match-data
	(if (looking-at " *-?[0-9]+\\. ")
	    (replace-match "")))
      (insert (format (concat "%" (int-to-string width) "d. ") n))
      (forward-line))))

;;; ==============================
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
(defun mon-insert-numbers-padded (start end &optional padp)
  "Insert the numbers from START to END (inclusive) in the current buffer.
Each number is inserted on a separate line.  START may be less than END, in
which case counting is backward.\n
If given a prefix argument or optional arg PADP is non-nil, pad all numbers
with sufficient leading zeros so they are the same width.\n
:SEE-ALSO `mon-insert-string-incr',`mon-number-lines-region', `mon-re-number-region', 
`mon-rectangle-sum-column'.\n►►►"
  (interactive "nStart: \nnEnd: \nP")
  (let ((add-func (if (<= start end) '1+ '1-))
        (comp-func (if (<= start end) '<= '>=))
        (i start)
        (fmt (and padp (format "%%.%dd"
                               (length (int-to-string (max (abs start)
                                                           (abs end))))))))
    (while (funcall comp-func i end)
      (insert (if fmt (format fmt i) (int-to-string i)) "\n")
      (setq i (funcall add-func i)))))

;;; ==============================
;;; :CREATED <Timestamp: Tuesday February 03, 2009 @ 03:41.17 PM - by MON KEY>
;;; :RENAMED :WAS `mon-incr' -> `mon-insert-string-incr'
(defun mon-insert-string-incr ()
  "Inserts to current buffer the range of number provided. 
Prompts for; \"Increment from number:\", \"Increment to number:\",
\"Step by increments of:\",\"With delimiter?\", \"Insert newlines?\".
When the range will return over 1000 values throws an error.
When \"With delimiter?\" is t, prompts \"Delimit range with:\".
When both \"With delimiter?\" and \"Insert newlines?\" are nil promts with,
\"Interleave range with whitespace? \" as whitespace isn't a delimter by default.\n
:SEE-ALSO `mon-insert-numbers-padded', `mon-number-lines-region', 
`mon-re-number-region', `mon-rectangle-sum-column'.\n►►►"
  (interactive)
  (catch 'mon-incr-big-range 
    (let ((oldbuf (current-buffer)))    
      (let* ((test-llm (and (buffer-local-value longlines-mode (current-buffer))))
	   (is-on (and test-llm))
	   (llm-off))
      (progn
	(when is-on (longlines-mode 0) (setq llm-off 't))
      (save-excursion
	(let* ((start-incr (read-number "Increment from number: "))
	       (end-incr (read-number "Increment to number: "))
	       (step-value (read-number "Step by increments of: " 1))
	       (range (- end-incr start-incr))
	       (range-check
		(when  (> (+ (/ range step-value) (mod range step-value)) 1000)
		  (progn
		    (message 
                     (concat 
                      "Ranges with over 1000 valuse not fun for the little man in Emacs.\n"
                      "He won't schlep that range!"))
		    (throw 'mon-incr-big-range t))))
	       (delimp (yes-or-no-p "With delimiter? "))
	       (nlp (yes-or-no-p "Insert newlines? "))
	       (delim
		(let* ((dt (and delimp))     ; t when delim y
		       (df (and (not delimp))) ; t when delim n
		       (nlt (and nlp))	       ; t when nl t
		       (nlf (and (not nlp))))  ; t when nl n
		  (cond
		   ((and nlt dt)	; do newline, do delim
		    ((lambda () (concat (read-string "Delimit range with: ") "\n"))))
		   ((and dt nlf)	; do delim, no newline 
		    ((lambda () (read-string "Delimit range with: "))))
		   ((and nlt df) "\n")	; do newline, no delim
		   ((and df nlf)
		    (if (yes-or-no-p "Interleave range with whitespace? ")
			" "  ""))))))
	  (while (<= start-incr end-incr)
	    (when (= start-incr end-incr) ; kick out when we reach top
	      (princ (format "%d" start-incr) (current-buffer))
	      (setq start-incr (+ step-value end-incr)))
	    (when (< start-incr end-incr) ; keep going
	      (princ (format "%d%s" start-incr delim) (current-buffer)))
	    (setq start-incr (+ step-value start-incr)))))
      (when llm-off (longlines-mode 1) (setq llm-off 'nil)))))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday February 19, 2009 @ 06:31.47 PM - by MON KEY>
;;; MODIFICATIONS-OF: Drew Adams' :HIS strings.el
(defun mon-line-drop-in-words (&optional buffer) 
 "Split current line of text in BUFFER into single words.
Split line is inserted with each word on a new line.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-string-ify-list',
`mon-insert-string-ify',`mon-string-split-line', `mon-get-word-list-buffer'.\n►►►"
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (save-excursion
      (set-buffer buffer)
      (let* ((line-as-string (buffer-substring-no-properties
			      (progn (end-of-line 1) (point))
			      (progn (beginning-of-line 1) (point))))
	   (line-o-strings (split-string line-as-string)))
	(progn 
	  (newline)
	  (mapc
	   (lambda (arg)
	     (princ arg (current-buffer))
	     (newline))
	  line-o-strings))))))

;;; =======================
;;; :RENAMED `mon-interactively-stringify' -> `mon-insert-string-ify'
(defun mon-insert-string-ify (the-string)
  "Read a string at minibuffer and split it into tokens.
Minibuffer Prompt with \"Stringify This-->\" returns a
list of strings obtained by breaking the string the user entered at the
space boundaries.\n\n:EXAMPLE
\(mon-insert-string-ify \"a b c\"\)\n=>(\"a\" \"b\" \"c\"\)\).\n
:SEE-ALSO `mon-string-ify-list'.\n►►►"
(interactive "sStringify This-->")
(print (mon-string-ify-list the-string) (current-buffer)))
;;
;;; :TEST-ME (mon-insert-string-ify "a b c")

;;; ==============================
;;; utf-8-auto-unix, utf-8-emacs-unix, encode-coding-region, describe-coding-system
;;; ==============================
;;; :NOTE Now inlined with the let in `unicode-insert'.
;;; (setq read-quoted-char-radix 16)
;;; prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
(defun mon-insert-unicode (char &optional insertp intrp)
  "Reads a unicode code point for CHAR and insert CHAR at point.\n
:EXAMPLE\n(mon-insert-unicode \"25BA\")\n
Uses `read-quoted-char-radix' with val set to 16. This allows easy coping of
values from Unicode charts e.g. (URL `http://www.decodeunicode.org/u+2019').\n
For access/alteration to encoding/coding information :SEE
`encode-coding-region', `describe-coding-system', `unicode-insert'.\n►►►"
  (interactive `(,@(let* ((rd-types (list "1 Unicode char in radix 16 e.g. '25BA'"
                                          "2 Unicode char by name w/ completion (octal, hex, decimal)"))
                          (rd-type (completing-read "read Unicode as " rd-types nil t))
                          (char-rd (cond ((string= (substring rd-type 0 1) "1")
                                          (let ((read-quoted-char-radix 16))
                                            (char-to-string (read-quoted-char "Char :"))))
                                         ((string= (substring rd-type 0 1) "2")
                                          (let (the-char)
                                            (setq the-char (read-char-by-name "Char (tab completes) :"))
                                            (cond ((not (integerp the-char))
                                                   (error "Not a Unicode character code: %S" the-char))
                                                  ((or (< arg 0) (> arg #x10FFFF))
                                                   (error "Not a Unicode character code: 0x%X" the-char)))
                                            (char-to-string the-char)))))) `(,char-rd nil t))) )
  (let (t-char)
    (setq t-char
          (cond ((integerp char) char)
                ;;(cdr (assoc-string "BLACK RIGHT-POINTING POINTER" (ucs-names) t))
                ((cdr (assoc-string char (ucs-names) t)))
                ((stringp char) 
                 (cond ((numberp (mon-string-to-symbol char))
                        (mon-string-to-symbol char)) ;; ((string-to-number char 16))
                       ((not (integerp char))
                        (error "Not a Unicode character code: %S" char))
                       ((or (< char 0) (>= char 65536)) ;;(or (< 9658 0) (> 9658 65536))
                        (error "Not a Unicode character code: %d" char))
                       ((or (< char 0) (> char #x10FFFF)) ;;(or (< #x25ba 0) (> #x25ba #x10FFFF))
                        (error "Not a Unicode character code: 0x%X" char))
                       ;;(t (setq t-char (char-to-string t-char))))
                       ))))
    (setq t-char (char-to-string t-char))
    (if (or insertp intrp)
        (save-excursion (insert-and-inherit t-char))
      t-char)
    t-char))
;;
;;; :TEST-ME (mon-insert-unicode "#o22672" t)►
;;; :TEST-ME (mon-insert-unicode #o22672 t)►
;;; :TEST-ME (mon-insert-unicode "#x25ba" t)►
;;; :TEST-ME (mon-insert-unicode 9658 t)►
;;; :TEST-ME (mon-insert-unicode "#x25ba" t)►
;;; :TEST-ME (mon-insert-unicode "BLACK RIGHT-POINTING POINTER" t)►►
;;; :TEST-ME (mon-insert-unicode "bubba" t)
;;; :TEST-ME (mon-insert-unicode "#o22672")
;;; :TEST-ME (mon-insert-unicode #o22672)
;;; :TEST-ME (mon-insert-unicode "#x25ba")
;;; :TEST-ME (mon-insert-unicode 9658)
;;; :TEST-ME (mon-insert-unicode #x25ba)
;;; :TEST-ME (mon-insert-unicode "BLACK RIGHT-POINTING POINTER")
;;; :TEST-ME (mon-insert-unicode "bubba")
;;; :TEST-ME (mon-insert-unicode "bubba" t)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T13:57:43-04:00Z}#{09352} - by MON KEY>
(defun split-designator (&optional insertp intrp)
  "Return string \"---\\n\". 
When INSERTP is non-nil or called-interactively inserts at point. 
Does not move point.\n\n:EXAMPLE\n\(split-designator\)\n
:SEE-ALSO `non-posting-source', `non-posting-ebay-source', `naf-comment-prefix'.
`non-posting-wiki-source', `npps', `benezit-naf-template'.\n
Used in `naf-mode'.\n►►►"
  (interactive "i\np")
  (if (or insertp intrp)
      (save-excursion
      (insert "---\n"))
    "---\n"))
;;
;;; :TEST-ME (split-designator)
;;; :TEST-ME (call-interactively 'split-designator)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T14:09:37-04:00Z}#{09352} - by MON KEY>
(defun comment-divider (&optional not-insert intrp)
  "Insert default comment divider at point.
When called-interactively insert the following at point:\n
;;; ==============================\n
When NOT-INSERT is non-nil return comment divider as string.\n
:EXAMPLE\n\(comment-divider t\)\n
:SEE-ALSO `mon-comment-divide->col', `mon-lisp-comment-to-col'
`mon-insert-php-comment-divider', `mon-insert-lisp-stamp'.\n►►►"
  (interactive "i\np")
  (if (or (not not-insert) intrp) 
      (insert ";;; ==============================")
    ";;; =============================="))
;;
;;; :TEST-ME (comment-divider t)
;;; :TEST-ME (comment-divider)
;;; :TEST-ME (call-interactively 'comment-divider)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-25T19:03:44-04:00Z}#{09352} - by MON KEY>
(defun comment-divider-to-col (to-col &optional start end op-prefix insertp intrp)
  "Return region or next line with `comment-divider' indented TO-COL.
When START and END are non-nil or called-interactively and region is active 
indent and comment content of region to COL-N, else indent and comment next line.
When op-prefix \(a string\) is non-nil or called-interactively with prefix arg 
use a an alternative comment prefix for region or next line and the comment divider
default prefix is `;' as per `comment-divider' which normally returns:\n
`;;; =============================='\n
Here a prefix `;; ' is used in subesequent line/region according lisp convention.\n
Where the OP-PREFIX arg is non-nil for example using `*' comment divider is:\n
`*** =============================='\n
with subesequent line or region commented with `** '. 
When INSERTP is non-nil insert commented region at point, doesn't move point.\n
:SEE-ALSO `mon-lisp-comment-to-col' for a specifically lisp centric interactive
implementation.\n►►►"
  (interactive "i\nr\nP\ni\np")
  (let ((strt-frm   (make-marker))
        (s-ident-at (make-marker))
        (e-ident-at (make-marker))
        (prfx      
         (cond ((and intrp op-prefix)
                (concat 
                 (make-string 2 (string-to-char (substring (read-string "use alternative prefix :") 0 1))) 
                 " "))
               ((and op-prefix (not intrp))
                (concat (make-string 2 (string-to-char (substring op-prefix 0 1))) " "))
               (t ";; ")))
        (2col (cond ((and intrp (or (not to-col) (not (numberp to-col))))
                     (read-number "indent to column number :"))
                    ((and (not intrp) (not (numberp to-col)))
                     (error "to-col must be a number. supplied arg was : %s" to-col))
                    (t to-col)))
        (cd2cN)
        (w/reg))
    (cond ((use-region-p)
           (set-marker strt-frm    (region-beginning))
           (set-marker s-ident-at  (region-beginning))
           (set-marker e-ident-at  (region-end)))
          ((and start end)
           (set-marker strt-frm    start)
           (set-marker s-ident-at  start)
           (set-marker e-ident-at  end))
          ((and intrp (not (use-region-p)) (not start) (not end))
           (set-marker strt-frm    (point))
           (set-marker s-ident-at  (line-beginning-position 2))
           (set-marker e-ident-at  (line-end-position 2)))
          ((not intrp)
           (set-marker strt-frm    (line-beginning-position))
           (set-marker s-ident-at  (line-beginning-position 2))
           (set-marker e-ident-at  (line-end-position 2))))
    (setq w/reg (buffer-substring-no-properties s-ident-at e-ident-at))
    (setq cd2cN (with-temp-buffer
                  (let ((cln-wspc-s (make-marker))
                        (cln-wspc-e (make-marker))
                        (dvdr (concat (substring prfx 0 1) prfx (substring (comment-divider t) 4)))
                        (more t)
                        (pad)
                        (ssf))
                    (insert w/reg)
                    (goto-char (point-min))
                    (insert dvdr)
                    (open-line 1)
                    (goto-char (point-min))
                    (while more
                      (when (= (point) (point-min)) (indent-to-column 2col) (line-move 1 t))
                      (beginning-of-line)
                      (set-marker cln-wspc-s (point))
                      (setq ssf (skip-syntax-forward "-"))
                      (set-marker cln-wspc-e (point))
                      (when (> ssf 0) (delete-region cln-wspc-s cln-wspc-e))
                      (goto-char cln-wspc-s)
                      (insert prfx)
                      (insert (make-string (cond ((> ssf 3) (- ssf 3))
                                                 ((<= ssf 3) 0)) 32))
                      (goto-char cln-wspc-s)
                      (indent-to-column 2col)
                      (end-of-line)
                      (if (eobp) (setq more nil)
                        (line-move 1 t)))
                    (buffer-string))))
    (if (or insertp intrp)
        (save-excursion 
          (delete-region strt-frm e-ident-at)
          (goto-char strt-frm)
          (insert cd2cN))
      cd2cN)))
;;
(defalias 'mon-comment-divide->col 'comment-divider-to-col)
;;
;;;(progn (makunbound 'comment-divider-to-col) (unintern 'comment-divider-to-col)
;;;       (makunbound 'mon-comment-divide->col) (unintern 'mon-comment-divide->col))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-25T18:17:18-04:00Z}#{09352} - by MON KEY>
(defun mon-lisp-comment-to-col (&optional col-n)
  "Insert `comment-divider' at COL-N comment and indent region/next line to col.
COL-N is a prefix arg. When region is active indent and comment content of region 
to COL-N else indent and comment next line. Comment prefix is `;; '.
To provide an alternative comment prefix use `comment-divider-to-col'.
:SEE-ALSO `comment-divider'.\n►►►"
  (interactive "P")
  (comment-divider-to-col col-n nil nil alt-prefix nil t))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T19:17:39-04:00Z}#{09352} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-25T14:48:38-04:00Z}#{09352} - by MON KEY>
(defun comment-divider-to-col-four () 
  ":DEPRECATED This function has been replaced by `comment-divider-to-col'.
Insert `comment-divider' indentented to column 4(four).
Move contents current line forward 1(one) line indent it to column 4(four).\n►►►"
(interactive)
(save-excursion
  (open-line 1)
  (line-move 1)
  (beginning-of-line)
  (indent-to-column 4)
  (comment-divider)
  (open-line 1)))

;;; ==============================
;;; :RENAMED `php-comment-divider' -> `mon-insert-php-comment-divider'
(defun mon-insert-php-comment-divider ()
  "Inserts a PHP(C style) comment divider at point. Inserts at point:\n
 //***************************//\n
See; `comment-divider', `comment-divider-to-col-four'.\n►►►"
  (interactive)
  (insert "//***************************//"))
;;
;;; :TEST-ME (call-interactively 'mon-insert-php-comment-divider)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T13:24:27-0400Z - by MON KEY>
(defun mon-insert-user-name-cond (&optional insertp intrp)
  "Inserts a cond template to evaluate user type of current system.
Test for `IS-BUG-P' or `IS-MON-P'.\n
:EXAMPLE\n(mon-insert-user-name-cond)\n►►►"
  (interactive "i\np")
  (let ((unc (concat
              "\n(cond\n"
              " ((equal user-real-login-name \""
              (cadr (assoc 6 *BUG-NAME*))"\") ...do-something-here)\n"
              " ((equal user-real-login-name \""
              (cadr (assoc 5 *MON-NAME*)) "\") ...do-something-here))\n")))
    (if (or insertp intrp)
        (save-excursion(insert unc))
      unc)))
;;
;;; :TEST-ME (mon-insert-user-name-cond)
;;; :TEST-ME (mon-insert-user-name-cond t)
;;; :TEST-ME (call-interactively 'mon-insert-user-name-cond)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T13:24:13-0400Z - by MON KEY>
(defun mon-insert-system-type-cond (&optional insertp intrp)
  "Inserts a cond template to test for system type OS type. Curenlty tests for
GNU/Linux and windows-nt only.\n
:EXAMPLE\n\(mon-insert-system-type-cond\)\n
See `win32p' and `gnu-linuxp' for constants that return t or nil.\n►►►"
  (interactive "i\np")
  (let ((stc (concat
              "\n(cond\n"
              " ((equal system-type 'windows-nt) \"I'm a slave to the w32.\")\n"
              " ((equal system-type 'gnu/linux) \"I'm a GNU!\"))\n")))
    (if (or insertp intrp)
        (save-excursion (insert stc))
      stc)))
;;
;;; :TEST-ME (mon-insert-system-type-cond)
;;; :TEST-ME (mon-insert-system-type-cond t)
;;; :TEST-ME (call-interactively 'mon-insert-system-type-cond)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-18T13:56:27-04:00Z}#{09385} - by MON>
(defun mon-insert-regexp-template (&optional insertp intrp)
  "Returna template for quickly writing regexp routine.
When INSERTP or called-interactively inserts template at point.\n
:EXAMPLE
;; (while\n(progn\n  (search-forward-regexp \"\")\n  (replace-match \"\"))\n
:SEE-ALSO `mon-insert-regexp-template-yyyy'. ►►►"
  (interactive "i\np")
  (let ((regxp-t
	 ";; (while\n(progn\n  (search-forward-regexp \"\")\n  (replace-match \"\"))"))
    (if (or insertp intrp)
	(save-excursion (newline) (insert regxp-t))
      regxp-t)))
;;
;;; :TEST-ME (mon-insert-regexp-template t)
;;; :TEST-ME (call-interactively 'mon-insert-regexp-template)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-18T15:41:36-04:00Z}#{09385} - by MON KEY>
(defun mon-insert-regexp-template-yyyy (&optional query-rep insertp intrp)
  "Insert at point regexps for searching whitespace delimited years 20thC.
Regexps are specific to the range 1900-1999. Returned regexp template
includes \\nth numbered capture groups. Regexps defaults to insertion
for  programattic elisp. When INSERTP in non-nil or called-interactively 
insert a second line beneath the regexp indicating  grouping sections. 
When QUERY-REP is non-nil returns a regexp to the kill-ring. 
This regexp will be in the kill ring and is ready formatted for yanking into 
the minibuffer of a query-replace-regexp prompt. When query-rep argument is
non-nil no regexp is inserted into buffer at point. 
Yank it back from the kill-ring if that is what you want.\n
:EXAMPLE\n\(mon-insert-regexp-template-yyyy\)\n
:SEE-ALSO `mon-insert-regexp-template'.\n►►►"
  (interactive "P\ni\np")
  (let* ((dbl-slash
	  (concat
	   ";(while\n(progn\n  (search-forward-regexp\n"
	   "   \"\\\\_<\\\\(19\\\\)\\\\([0-9]\\\\{2,2\\\\}\\\\)\\\\_>\" nil t)\n"
	   "   ;;^^^^^^1^^^^^^^2^^^^^^^^^^^^^^^^^^^^^\n"
	   "  (replace-match \"\\\\1\\\\2\"))"))
	 (sngl-slash 
	  (concat "\\_<\\(19\\)\\([0-9]\\{2,2\\}\\)\\_> \\1\\2")))
	 (cond ((and intrp query-rep)
		(kill-new sngl-slash)
		(message "regexp is in the kill ring. Yank it back."))
	       (intrp (save-excursion (newline) (insert dbl-slash)))
	       ((and insertp query-rep)
		(prog1 (beginning-of-line 2)
		  (save-excursion (insert sngl-slash))))
	       ((or insertp intrp)
		(save-excursion (newline) (insert dbl-slash)))
	       (query-rep sngl-slash)
	       (t dbl-slash))))
;;
;;; :TEST-ME (mon-insert-regexp-template-yyyy)
;;; :TEST-ME (mon-insert-regexp-template-yyyy t)
;;; :TEST-ME (mon-insert-regexp-template-yyyy t nil t)
;;; :TEST-ME (mon-insert-regexp-template-yyyy nil nil t)
;;; :TEST-ME (call-interactively 'mon-insert-regexp-template-yyyy)

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:15.02 PM - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T16:11:16-0400Z - by MON KEY>
(defun mon-insert-lisp-CL-file-template (&optional insertp intrp)
  "Insert a CL header at top of file.\n
:EXAMPLE\(mon-insert-lisp-CL-file-template\)\n
:SEE-ALSO `mon-insert-lisp-stamp', `mon-insert-lisp-testme',
`mon-insert-lisp-evald',`mon-insert-copyright', `comment-divider',
`comment-divider-to-col-four', `mon-stamp', `mon-file-stamp'.\n►►►" 
  (interactive "i\np")
  (let* ((modeline ";;-*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-")
         (fname (if (not (buffer-file-name)) 
                        (buffer-name)
                      (file-name-nondirectory (buffer-file-name))))
         (timestring (concat ";;; <Timestamp: " (mon-timestamp :naf t)))
         (copyright (mon-build-copyright-string nil nil t))
         (cl-template (concat
                       modeline "\n"
                       ";;; ================================================================\n"
                       ";;; This is " fname "\n"
                       ";;; \n"
                       ";;; FILE-CREATED:\n" 
                       timestring 
                       copyright
                       ";;; CODE:\n"
                       ";;; \n"
                       ";;; {...}\n"
                       ";;;\n" 
                       ";;; ==============================\n"
                       ";;; EOF"))) ;modeline timestring copyright)))
         (when (or insertp intrp)
           (progn   
             (goto-char (point-min))
             (insert cl-template)))
           cl-template))
;;
;;; :TEST-ME (mon-insert-lisp-CL-file-template)
;;; :TEST-ME (mon-insert-lisp-CL-file-template t)
;;; :TEST-ME (call-interactively 'mon-insert-lisp-CL-file-template)

;;; ==============================
;;; :COURTESY Zach Beane :HIS scratch-lisp-file.el
;;; (URL `http://www.xach.com/lisp/scratch-lisp-file.el')
;;; :MODIFICATIONS <Timestamp: Tuesday July 14, 2009 @ 02:12.25 AM - by MON KEY>
;;;               changed the buffer-file-name to check if we have a name.
(defun mon-insert-CL-package-template (&optional insertp without-header intrp)
  "Return or insert a CL package-template a template. 
Builds (with DEFPACKAGE and IN-PACKAGE forms) into the current buffer.
When called-interactively or INSERTP non-nil assumes current buffer is empty and
insert a file template with `mon-insert-lisp-CL-file-template'.\n
:EXAMPLE\n(mon-insert-CL-package-template)\n►►►"
  (interactive "i\nP\np")
  (let* ((file (if (not (buffer-file-name)) 
                   (buffer-name)
                 (file-name-nondirectory (buffer-file-name))))
         (package (file-name-sans-extension file))
         (package-template (concat ";;;; " file "\n"
                                   "\n(defpackage #:" package "\n  (:use #:cl))\n\n"
                                   "(in-package #:" package ")\n\n")))
    (when (or insertp intrp)
      (cond ((not without-header)
               (mon-insert-lisp-CL-file-template t)
               (if (search-backward-regexp "^;;; CODE:$" nil t)
                   (progn (beginning-of-line 2)
                          (newline)
                          (insert package-template))
                 (if (search-backward-regexp "^;;; {...}$" nil t)
                     (progn (beginning-of-line)
                            (newline)
                            (insert package-template))
                   (if (search-forward-regexp "^;;; EOF$" nil t)
                       (progn
                         (line-move -3)
                         (end-of-line)
                         (newline)
                         (insert package-template))))))
              (without-header
               (goto-char (point-min))
               (insert package-template))))
      package-template))
;;
;;; :TEST-ME (mon-insert-CL-package-template)
;;; :TEST-ME (mon-insert-CL-package-template t)
;;; :TEST-ME (call-interactively 'mon-insert-CL-package-template)

;;; ==============================
;;; :CREATED <Timestamp: Friday June 12, 2009 @ 12:24.26 PM - by MON KEY>
(defun mon-insert-lisp-stamp (&optional insertp intrp modifications)
  "Return or insert at point a `comment-divider' newline and `mon-stamp'.
When INSERTP is non-nil or called interactively insert at point. 
When MODIFICATIONS is non-nil or called interactively with Prefix Arg.
Return with ';;; :MODIFICATIONS ' prefix - default is to return ';;; :CREATED '
Use after creating new elisp functions to delimit and date them.\n
:EXAMPLE\n\(mon-insert-lisp-stamp\)\n\n\(mon-insert-lisp-stamp nil nil t\)\n
:SEE-ALSO `mon-lisp-stamp',`mon-file-stamp', `mon-insert-copyright',
`mon-insert-lisp-testme', `mon-insert-lisp-CL-file-template',
`comment-divider', `comment-divider-to-col-four', `mon-insert-lisp-evald'.\n►►►"
  (interactive "i\np\nP")
      (if (or insertp intrp)
          (if modifications 
              (mon-lisp-stamp t nil t)
            (mon-lisp-stamp t))            
        (if modifications 
            (mon-lisp-stamp nil nil t)
          (mon-lisp-stamp))))
;;
;;; :TEST-ME (mon-insert-lisp-stamp)
;;; :TEST-ME (mon-insert-lisp-stamp t)
;;; :TEST-ME (mon-insert-lisp-stamp nil nil t)
;;; :TEST-ME (mon-lisp-stamp t nil t)
;;; :TEST-ME (call-interactively 'mon-insert-lisp-stamp)

;;; ==============================
;;; :NOTE See below @BOF for unfinished version mon-insert-lisp-testme-fancy
;;; :CREATED <Timestamp: 2009-07-31-W31-5T13:53:52-0400Z - by MON KEY>
(defun mon-insert-lisp-testme (&optional search-func test-me-count insertp intrp)
  "Insert at point a newline and commented test-me string.
When non-nil SEARCH-FUNC will search backward for a function name and include it
in the test-me string.
When non-nil TEST-ME-COUNT insert test-me string N times. Default is 1\(one\).
When prefix arg TEST-ME-COUNT is non-nil inerts N number of ';;; :TEST-ME ' strings
and prompts y-or-n-p if we want to include the function name in insertions.
When INSERTP is non-nil insert the test-me string(s) in current buffer at point.
Use at the end of newly created elisp functions to provide example test cases.
Regexp held by global var `*regexp-symbol-defs*'.\n
:SEE-ALSO `mon-insert-doc-help-tail', `mon-test->*regexp-symbol-defs*'
`mon-insert-doc-help-tail', `mon-insert-lisp-stamp', `mon-insert-copyright',
`mon-insert-lisp-CL-file-template', `comment-divider',
`comment-divider-to-col-four', `mon-insert-lisp-evald'.\n►►►"
  (interactive "i\np\ni\np")
  (let* ((get-func)
         (tmc (cond ((and intrp (> test-me-count 1))
                      (if ((lambda () (yes-or-no-p "Search-function-name? :")))
                          (progn (setq get-func t) test-me-count)
                        (progn (setq get-func nil) test-me-count)))
                    ((not test-me-count) 1)
                    (t  test-me-count)))
         (func (if (or search-func get-func)
                   (save-excursion
                     (search-backward-regexp  *regexp-symbol-defs*)
                     (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (test-me-string (if (or search-func get-func)
                             (format ";;; :TEST-ME (%s )" func)
                           ";;; :TEST-ME "))
         (limit (make-marker))
         (cnt tmc)
         (return-tms))
    (while (>= cnt 1)
      (setq return-tms (concat test-me-string "\n" return-tms))
      (setq cnt (1- cnt)))
    (if (or intrp insertp)
          (progn
            (save-excursion
              (when insertp (newline))
              (when (not (bolp))(beginning-of-line))
              (princ return-tms (current-buffer))
              (set-marker limit (point)))
          (search-forward-regexp
           (format "%s$" test-me-string) (marker-position limit) t)
          t) ; t needed here to prevent returning buffer position when called externally?
        return-tms)))
;;
;;; :TEST-ME (mon-insert-lisp-testme)
;;; :TEST-ME (mon-insert-lisp-testme t 3 )
;;; :TEST-ME (mon-insert-lisp-testme nil 3)
;;; :TEST-ME (mon-insert-lisp-testme nil 3 t)
;;; :TEST-ME (mon-insert-lisp-testme t 3 t)
;;; :TEST-ME (mon-insert-lisp-testme t nil t)
;;; :TEST-ME (mon-insert-lisp-testme nil nil t)
;;; :TEST-ME (mon-insert-lisp-testme nil nil nil)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T16:04:36-04:00Z}#{09411} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:44:23-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:41.53 PM - by MON KEY>
(defun mon-build-copyright-string (&optional insertp intrp monkey no-nl with-dcp short-form)
  "Return a copyright string built conditionally on users name.
When INSERTP is non-nil or called interactively insert copyright at point.
On MONish systems when MONKEY is non-nil or called-interactively with Prefix arg
use longform. Default is to abbreviate MONish names. 
When NO-NL is non-nil do not return with newlines.
Default is with newline concatenated to head and tail of return string.
When WITH-DCP is non-nil return with an institutional C/O string appended.
When short form is non-nil do not pad the comment delim to col 68. 
Returns with a padded comment delim of either 34, 44, 54 chars depending on the 
value of preceding args MONKEY and WITH-DCP.\n
:SEE-ALSO `mon-insert-copyright'.\n►►►"
  (interactive "i\np\nP") 
  (let ((name (cond (monkey 
                     (cond 
                       (IS-MON-P (cadr (assoc 6 *MON-NAME*)))
                       (IS-BUG-P (cadr (assoc 5 *BUG-NAME*)))))
                    (IS-MON-P (cadr (assoc 7 *MON-NAME*)))
                    (IS-BUG-P (cadr (assoc 1 *BUG-NAME*)))
                    (IS-BUG-P-REMOTE (cadr (assoc 1 *BUG-NAME*)))
                    (t "<NAME>")))
        (w-dcp (cond (monkey 
                      (if with-dcp (substring (cadr (assoc 1 *DCP-NAME*)) 1) ""))
                     (with-dcp (cadr (assoc 2 *DCP-NAME*)))
                     (t "")))
        (year (mon-get-current-year)) ;(format-time-string "%Y"))
        (cls-comment (cond ((and short-form (not monkey) (not with-dcp))
                            ";;; ==============================")  ;; 34
                           ((and short-form monkey (not with-dcp))
                            ";;; ==============================")  ;; 34
                           ((and short-form (not monkey) with-dcp) 
                            ";;; ========================================") ;; 44
                           ((and short-form monkey with-dcp) 
                            ";;; ==================================================") ;; 54
                           (t ";;; ================================================================"))) ;; 68
        (cpy))
    (setq cpy
          (if no-nl
              (concat
               cls-comment "\n"
               ;; ";;; ================================================================\n"
               ";;; Copyright © " year " " name " "  w-dcp "\n"
               cls-comment)
              (concat
               "\n" cls-comment "\n"
               ";;; Copyright © "  year " " name " " w-dcp "\n"
               cls-comment "\n")))
    (when (or insertp intrp) 
      (save-excursion (insert cpy)))
    cpy))
;;
;;;(&optional insertp intrp monkey no-nl with-dcp short-form)
;;
;;; :TEST-ME (mon-build-copyright-string)
;;; :TEST-ME (mon-build-copyright-string nil nil t nil nil t)
;;; :TEST-ME (mon-build-copyright-string nil nil t t)
;;; :TEST-ME (mon-build-copyright-string t nil t nil t )
;;; :TEST-ME (mon-build-copyright-string t nil t nil t t)
;;; :TEST-ME (mon-build-copyright-string nil nil t t nil t)
;;; :TEST-ME (mon-build-copyright-string t nil nil nil nil t)
;;; :TEST-ME (mon-build-copyright-string nil nil t t t t)
;;; :TEST-ME (mon-build-copyright-string nil nil nil t t t)
;;; :TEST-ME (mon-build-copyright-string nil nil t t nil t)
;;; :TEST-ME (let ((IS-BUG-P t)) (mon-build-copyright-string nil nil t))
;;; :TEST-ME (call-interactively 'mon-build-copyright-string)

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:35.37 PM - by MON KEY>
;;; :DEPRECATED :USE (mon-build-copyright-string t)
(defun mon-insert-copyright (&optional monkey w-short-form insertp intrp)
  "Insert copyright with relevant details.
Conditional on user's system name.
When MONKEY is non-nil use longform of MONish username.
Default is abbreviated nameform.
When W-SHORT-FORM is non-nil insert with 34 char length comment divider.
Default is to return with 68 char length comment dividers.\n
:SEE-ALSO `mon-build-copyright-string'.\nUsed in `naf-mode'.\n►►►" 
  (interactive "p\nP\ni\np")
(if (or insertp intrp)
    (save-excursion 
      (mon-build-copyright-string t nil monkey nil nil w-short-form))
    (mon-build-copyright-string nil nil monkey nil nil w-short-form)))
;;
(defalias 'bug-insert-copyright 'mon-insert-copyright
  "Insert a copyright string with relevant details.
Conditional upon `IS-BUG-P' returning t.
:SEE-ALSO `mon-build-copyright-string'.\nUsed in `naf-mode'.\n►►►")
;;
;;; :TEST-ME (mon-insert-copyright t)
;;; :TEST-ME (mon-insert-copyright nil t)
;;; :TEST-ME (mon-insert-copyright nil t t)
;;; :TEST-ME (mon-insert-copyright t nil t)
;;; :TEST-ME (call-interactively 'mon-insert-copyright)
;;; :TEST-ME (bug-insert-copyright)
;;; :TEST-ME (let ((IS-BUG-P t)) (bug-insert-copyright t))
;;; :TEST-ME (let ((IS-BUG-P t)) (bug-insert-copyright))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-02T16:06:03-04:00Z}#{09405} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T15:53:51-04:00Z}#{09411} - by MON KEY>
;;; :CREATED <Timestamp: Thursday April 09, 2009 @ 05:52.10 PM - by MON KEY>
;;; :RENAMED `mon-insert-naf-mode-file-template' ->`mon-insert-file-template'
(defun mon-insert-file-template (&optional with-fname insertp intrp)
"Insert an elisp file template. 
Template includes GPLv3+ clause from `*mon-gnu-license-header*'
GFDLv1.3 clause w/ Copyright <YYYY> <NAME> from: `*mon-gnu-license-header-gfdl*'\n
:EXAMPLE\n(mon-insert-file-template)\n
:SEE-ALSO `mon-insert-texi-template', `mon-insert-lisp-CL-file-template',
`mon-file-stamp', `mon-build-copyright-string', `mon-insert-copyright', 
`mon-insert-gnu-licence'.\n►►►"
  (interactive "i\ni\np")  
    (let* ((cur-nm (buffer-name))
           (fname (if (and intrp (mon-buffer-written-p))
                      cur-nm
                    "<PKG-NAME>.el"))
           (fname-sans 
            (if (and intrp (mon-buffer-written-p))
                (file-name-sans-extension cur-nm)
                cur-nm
              "<PKG-NAME>"))
           (fl-template
            (format 
";; -*- mode: EMACS-LISP; -*-
;;; this is %s
;;; ================================================================
;;; DESCRIPTION:
;;; %s provides {description here}.\n;;;
;;; FUNCTIONS:►►►\n;;;\n;;; FUNCTIONS:◄◄◄\n;;;
;;; MACROS:\n;;;
;;; METHODS:\n;;;
;;; CLASSES:\n;;;
;;; CONSTANTS:\n;;;
;;; VARIABLES:\n;;;
;;; ALIASED/ADVISED/SUBST'D:\n;;;
;;; DEPRECATED:\n;;;
;;; RENAMED:\n;;;
;;; MOVED:\n;;;
;;; TODO:\n;;;
;;; NOTES:\n;;;
;;; SNIPPETS:\n;;;
;;; REQUIRES:\n;;;
;;; THIRD PARTY CODE:\n;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY\n;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/%s)
;;; FIRST-PUBLISHED:\n;;;
;;; FILE-CREATED:\n%s
;;; ================================================================
%s
%s
;;; CODE:\n
;   {...\n;   %s Contents here\n;   ...}\n
;;; ==============================
;;; (provide '%s)
;;; ==============================\n
;;; ================================================================
;;; %s ends here\n;;; EOF"
fname
fname-sans
fname
(concat ";;; <Timestamp: " (mon-timestamp :naf t)) 
*mon-gnu-license-header* 
(mon-insert-gnu-licence-gfdl t)
fname-sans
fname-sans
fname)))
(if (or insertp intrp)
    (insert fl-template)
  fl-template)))
;;
(defalias 'mon-insert-naf-mode-file-template 'mon-insert-file-template)
;;
;;; :TEST-ME (mon-insert-file-template)
;;; :TEST-ME (mon-insert-file-template t)
;;; :TEST-ME (call-interactively 'mon-insert-file-template)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-09T11:26:24-04:00Z}#{09327} - by MON KEY>
(defvar *mon-gnu-license-header* nil
  "Default GNU license to insert in newly created file headers.
Insertion provides GPLv3+ clause.\n
:EXAMPLE\n*mon-gnu-license-header*\n
:CALLED-BY `mon-insert-file-template',`mon-insert-gnu-licence'.\n
:SEE `*mon-gnu-license-header-gfdl*',`mon-insert-gnu-licence-gfdl' GFDL clause.
►►►")
;;
(when (not (bound-and-true-p *mon-gnu-license-header*))
           (setq *mon-gnu-license-header*
";;; This file is not part of GNU Emacs.
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
;;; Floor, Boston, MA 02110-1301, USA."))
;;
;;; :TEST-ME *mon-gnu-license-header*
;;;(progn (makunbound '*mon-gnu-lincense-header*)(unintern '*mon-gnu-license-header*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-02T12:01:00-04:00Z}#{09405} - by MON KEY>
(defvar *mon-gnu-license-header-gfdl* nil
  "Insert default GNU Free Documentation license in newly created file headers.
Insertion provides GFDL clause.\n
:EXAMPLE\n*mon-gnu-license-header-gfdl*\n
:CALLED-BY `mon-insert-file-template',`mon-insert-gnu-licence-gfdl'\n.
:SEE `*mon-gnu-license-header*'`mon-insert-gnu-licence' for GPLv3+ clause.\n►►►")
;;
(when (not (bound-and-true-p *mon-gnu-license-header-gfdl*))
  (setq *mon-gnu-license-header-gfdl*
        (concat
         ";;; Permission is granted to copy, distribute and/or modify this"
         "\n;;; document under the terms of the GNU Free Documentation License,"
         "\n;;; Version 1.3 or any later version published by the Free Software"
         "\n;;; Foundation; with no Invariant Sections, no Front-Cover Texts,"
         "\n;;; and no Back-Cover Texts. A copy of the license is included in"
         "\n;;; the section entitled \"GNU Free Documentation License\"."
         "\n;;; A copy of the license is also available from the Free Software"
         "\n;;; Foundation Web site at:"
         "\n;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').")))
;;
;;; :TEST-ME *mon-gnu-license-header-gfdl*
;;;(progn (makunbound '*mon-gnu-lincense-header-gfdl*)
;;;       (unintern '*mon-gnu-license-header-gfdl*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-09T11:26:16-04:00Z}#{09327} - by MON KEY>
(defun mon-insert-gnu-licence (&optional insertp intrp)
  "Returns `*mon-gnu-license-header*'. 
When INSERTP non-nil or called-interactively insert GNU license at point, but
does not move point.\n\n:EXAMPLE\n(mon-insert-gnu-licenceg-gfdl)\n
:SEE-ALSO `mon-insert-gnu-licence-gfdl', `mon-insert-file-template'.\n►►►"
  (interactive "i\np")
  (if (or insertp intrp)
(save-excursion
  (newline)
      (insert *mon-gnu-license-header*))
    *mon-gnu-license-header*))
;;
;;; :TEST-ME (mon-insert-gnu-licence)
;;; :TEST-ME (mon-insert-gnu-licence t)
;;; :TEST-ME (call-interactively 'mon-insert-gnu-licence)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-02T12:01:00-04:00Z}#{09405} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T15:43:43-04:00Z}#{09411} - by MON KEY>
(defun mon-insert-gnu-licence-gfdl (&optional  w-divider insertp intrp)
  "Returns `*mon-gnu-license-header-gfdl*'. 
When INSERTP non-nil or called-interactively insert GNU license at point, but
does not move point.\n
:EXAMPLE\n(mon-insert-gnu-licence-gfdl)\n\(mon-insert-gnu-licence-gfdl t\)\n
:SEE-ALSO `mon-insert-gnu-licence' `mon-insert-file-template'.\n►►►"
  (interactive "P\ni\np")
(let ((bld-gfdl 
       (cond (w-divider
              (concat 
               ";;; ================================================================\n"
               *mon-gnu-license-header-gfdl*
               "\n;;; ================================================================\n"
               (mapconcat 'identity
                          (mon-sublist-gutted
                           0 1 
                           (split-string (mon-build-copyright-string nil nil t t nil t) "\n"))
                          "\n")))
             (t  *mon-gnu-license-header-gfdl*))))
  (if (or insertp intrp)
      (save-excursion
        (newline)
        (insert bld-gfdl))
    bld-gfdl)))
;;
;;; :TEST-ME (mon-insert-gnu-licence-gfdl t)
;;; :TEST-ME (mon-insert-gnu-licence-gfdl nil t)
;;; :TEST-ME (mon-insert-gnu-licence-gfdl t t)
;;; :TEST-ME (call-interactively 'mon-insert-gnu-licence-gfdl)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T09:33:51-04:00Z}#{09407} - by MON>
(defun mon-insert-defclass-template (&optional class-pfx slot-count insertp intrp)
  "Return an eieio `defclass' template.
When non-nil CLASS-PFX is a class-name for template. Default is <CLASS-NAME>.
SLOT-COUNT is the number of slot templates returned.
When INSERTP in non-nil or called-interactively insert template at point.
Does not move point.\n:EXAMPLE\n(mon-insert-defclass-template nil 2)\n
:SEE-ALSO `mon-insert-naf-mode-class-template', `mon-help-eieio-defclass'.\n►►►"
  (interactive "i\nP\ni\np")
  (let ((c-nm (if class-pfx (format "%s" class-pfx) "<CLASS-NAME>"))
        (s-nm (if class-pfx (concat "<SLOT-"      
                                    (cond ((<= (length class-pfx) 2) class-sfx)
                                          ((> (length class-pfx) 2)
                                           (concat (substring class-pfx 0 1) 
                                                   (format "%d"(- (length class-pfx) 2))
                                                   (substring class-pfx -1))))
                                    "-")
                  "<SLOT-"))
        (sng-slot 
         (mapconcat 
          'identity 
          '("(%s%d>\n"
            "    :initarg :%s%d>\n"
            ;; :SEE `typep', `type-of' 
            ;; Also accepts: boolean, vector, hash-table, array, class-obj, sequence, etc.
            "    :type            ; {t, null, symbol, list, function, string ,integer, number, float}\n"
            "    :initform\n"
            "    :accessor        ; {generic-function-name}\n"
            "    :writer          ; {generic-function-name}\n"
            "    :reader          ; {generic-function-name}\n"
            "    :custom          ; {string}\n"
            "    :label           ; {string}\n"
            "    :group           ; {customization-group}\n"
            "    :custom-groups   ; {list}\n"
            "    :printer         ; {function}\n"
            "    :protection      ; {:public, :protected, :private}\n"
            "    :documentation \"\")") ""))
        (mlt-slot 
         (mapconcat 
          'identity 
          '("\n   (%s%d>\n"            ;; :accessor :writer :reader     
            "    :initarg :%s%d>\n"  ;; :custom   :group  :custom-groups
            "    :type\n"                  ;; :printer
            "    :initform\n"              ;; :protection
            "    :label\n"
            "    :documentation \"\")") ""))
        (cnt-p (if slot-count slot-count 1))
        (gathered))
    (do ((i cnt-p (1- i))
         (j (1- cnt-p) (1- j))
         (k "" (concat (format mlt-slot s-nm j s-nm j) k))
         (m "" (setq gathered (concat 
                               (mon-insert-lisp-stamp)
                               "\n(defclass " c-nm
                               " ()\n  (" 
                               (format sng-slot s-nm 0 s-nm 0)
                               k 
                               ")\n  \"\")"))))
        ((= i 0) m))
    (if (or insertp intrp)
        (save-excursion (newline) (princ gathered (current-buffer)))
        gathered)))
;;                   
;;; :TEST-ME (mon-insert-defclass-template nil nil t)
;;; :TEST-ME (mon-insert-defclass-template "BUBBA" nil t)
;;; :TEST-ME (mon-insert-defclass-template nil 2)
;;; :TEST-ME (mon-insert-defclass-template nil 2 t)

;;; ==============================
;;; :COURTESY Aaron S. Hawley 
;;; (URL `http://www.emacswiki.org/emacs/AutoInsertForTexinfo')
;;; Texinfo template. Based on "Emacs Lisp Header" in auto-insert.el
;;; :MODIFICATIONS <Timestamp: #{2009-08-24T11:38:35-04:00Z}#{09351} - by MON>
;;; Turned this into a function so now without reliance on autoinsertmode.
;;; ==============================
(defun mon-insert-texi-template (title short-description top &optional intrp)
  "Insert Texinfo template in buffer at point.
TITLE, SHORT-DESCRIPTION, and TOP are per Texinfo spec.
When called-interactively prompts for TITLE, SHORT-DESCRIPTION, and TOP.
Try to DTRT when buffer is not visiting file and prompts for filename to write
buffer to before proceeding with insertion.\n
:SEE-ALSO `mon-file-stamp', `mon-insert-file-template', `mon-insert-gnu-licence',
`mon-insert-ebay-dbc-template',`mon-insert-CL-package-template'.\n►►►"
  (interactive "\i\n\i\n\i\nP")
  (let ((mail-addr (cadr (assoc 9 *DCP-NAME*))) ;WAS: `user-mail-address'
        (organization (getenv "ORGANIZATION"))
        (u-name (cadr (assoc 6 *MON-NAME*))) ;;WAS: `user-full-name' 
        (this-year (substring (current-time-string) -4))
      
        (title (when intrp (read-string "set @title to: ")))
        (shrt-desc (when intrp (read-string "Short description :")))
        (top (when intrp (read-string "set @top (master menu) to :")))
        (fname (if (mon-buffer-written-p)
                   (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                 (progn
                   (write-file 
                    (concat 
                     (read-file-name "Buffer not written - set file name:" nil (buffer-name) nil (buffer-name))
                     ".texi")
                    t)
                   (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))
    (save-excursion
      ;;(goto-char (point-min))
      (insert (concat
               "\\input texinfo   @c -*-texinfo-*-
  @c %**start of header
  @setfilename " fname ".info\n"
               "@settitle " title "\n"
               "@c %**end of header
  @copying\n"
               shrt-desc".\n\n"        ;WAS: (read-string "Short description :")
               "Copyright @copyright{} " this-year "  "
               ;; WAS: ;;(getenv "ORGANIZATION") | (progn user-full-name) "
               organization " | " u-name
               "@quotation
  Permission is granted to copy, distribute and/or modify this document
  under the terms of the GNU Free Documentation License, Version 1.3 or
  any later version published by the Free Software Foundation; with no
  Invariant Sections, and no Cover Texts.  A copy of the license is
  included in the section entitled ``GNU Free Documentation License.''

  A copy of the license is also available from the Free Software
  Foundation Web site at @url{http://www.gnu.org/licenses/fdl-1.3.txt}.

  @end quotation

  The document was typeset with
  @uref{http://www.texinfo.org/, GNU Texinfo}.

  @end copying

  @titlepage
  @title " title "
  @subtitle " shrt-desc "
  @author " organization u-name   " |  <" mail-addr ">\n"
  ;; WAS ;; (getenv "ORGANIZATION") | (progn user-full-name)  " <" (progn user-mail-address) ">
               "@page
  @vskip 0pt plus 1filll
  @insertcopying
  @end titlepage

  @c Output the table of the contents at the beginning.
  @contents

  @ifnottex
  @node Top
  @top " top "

  @insertcopying
  @end ifnottex

  @c Generate the nodes for this menu with `C-c C-u C-m'.
  @menu
  @end menu

  @c Update all node entries with `C-c C-u C-n'.
  @c Insert new nodes with `C-c C-c n'.
  @node Chapter One
  @chapter Chapter One

  @node Copying This Manual
  @appendix Copying This Manual

  @menu
  * GNU Free Documentation License::  License for copying this manual.
  @end menu

  @c Copies of the GNU FDL at http://www.gnu.org/licenses/fdl.html
  @include fdl.texi

  @node Index
  @unnumbered Index

  @printindex cp

  @bye

  @c " (file-name-nondirectory (buffer-file-name)) " ends here\n")))))
;;
;;; :TEST-ME (call-interactively 'mon-insert-texi-template)


;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-23T20:17:17-04:00Z}#{09436} - by MON KEY>
(defvar *mon-hgignore-template* 
  '("syntax: glob"
    "**.7z"
    "**.bmp"
    "**.bz2"
    "**.dvi"
    "**.elc"
    "**.eps"
    "**.exe"
    "**.gif"
    "**.gz"
    "**.jar"
    "**.jpg"
    "**.jpeg"
    "**.last"
    "**.msi"
    "**.naf~"
    "**.nosearch"
    "**.pdf"
    "**.png"
    "**.ps"
    "**.psd"
    "**.rar"
    "**.reg"
    "**.tar"
    "**.tgz"
    "**.xbm"
    "**.xpm"
    "**.zip"
    "**.Z"
    "**#TAGS#"
    "syntax: regexp"
    "(.*\\~\\)"
    "(.*\\#\\)")
  "*List of strings containing glob and regexp patterns for insertion into .hgignore files.
:CALLED-BY `mon-insert-hgignore-template'.")
;;
;;; :TEST-ME *mon-hgignore-template* 
;;; (progn (makunbound '*mon-hgignore-template*) (unintern '*mon-hgignore-template*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-23T16:06:04-04:00Z}#{09435} - by MON KEY>
(defun mon-insert-hgignore-template (&optional insrtp intrp)
  "Insert MON standard list of patterns for .hgignore files.
When INSERTP is non-nil or called-interactively insert at point.
Does not move point. Patterns consist primarily of file extensions in glob and
regexp formats.  Patterns held as a list of strings in variable
`*mon-hgignore-template*'\n\n:EXAMPLE\n(mon-insert-hgignore-template)\n►►►"
  (interactive "i\np")
  (let ((hgignr (mapconcat 'identity *mon-hgignore-template* "\n")))
    (if (or insrtp intrp)
        (save-excursion (newline)(princ hgignr (current-buffer)))
        hgignr)))
;;
;;; :TEST-ME (mon-insert-hgignore-template)

;;; ==============================
;;; :CREATED <Timestamp: Wednesday March 04, 2009 @ 06:16.40 PM - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T16:03:23-0400Z - by MON KEY>
(defun mon-insert-test-cases (&optional insertp intrp)
  "Easily identified tracing vars for debugging defuns mon-*- and naf-mode-*.
Unbinds all previously bound variables:
test-aaa, test-bbb, test-ccc, test-ddd,
test-aa, test-AA, test-ag, test-AG, test-a4, test-A4\n
Rebinds vars 'default values'.  Called-interactively or INSERTP non-nil insert
test-cases at point.\n
:EXAMPLE\n\(mon-insert-test-cases\)\n:SEE-ALSO `mon-insert-lisp-testme'.\n►►►"
  (interactive "i\np") 
  (let ((put-tests))
    (mapc 'makunbound  '(test-aaa test-bbb test-ccc test-ddd test-aa
                         test-AA test-ag test-AG test-a4 test-A4))
    (let ((testing-list 
           '((setq test-aaa '(("1111" "2222") ("3333" "4444") ("5555" "6666") ("7777" "8888") ("9999" "x0x0")))
             (setq test-bbb '(("aaaa" "bbbb") ("cccc" "dddd") ("eeee" "ffff") ("gggg" "hhhh") ("iiii" "jjjj")))
             (setq test-ccc '(("AAAA" "BBBB") ("CCCC" "DDDD") ("EEEE" "FFFF") ("GGGG" "HHHH") ("IIII" "JJJJ")))
             (setq test-ddd '(("a1A1" "b2B2") ("c3C3" "d4D4") ("e5E5" "f6F6") ("g7G7" "h8H8") ("i9I9" "j1J0")))
             (setq test-aa '((a a)(b b)(c c)(d d))) (setq test-AA '((A A)(B B)(C C)(D D)))
             (setq test-ag '((a b)(b c)(d e)(f g))) (setq test-AG '((A B)(B C)(D E)(F G)))
             (setq test-a4 '((a 1) (b 2)(c 3)(d 4))) (setq test-A4 '((A 1)(B 2)(C 3)(D 4))))))
      (setq put-tests (mapconcat (lambda (x)  (format ";;; %S" x)) testing-list "\n")))
    (when (or insertp intrp)
      (save-excursion (newline)(insert put-tests)))
    put-tests))
;;
;;; :TEST-ME (mon-insert-test-cases t)
;;; :TEST-ME (mon-insert-test-cases)
;;; :TEST-ME (call-interactively 'mon-insert-test-cases)

;;; ==============================
;;; :NOTE (local-set-key "\C-c4" 'mon-comput-45)
;;; CREATED: <Timestamp: #{2009-10-17T11:30:32-04:00Z}#{09426} - by MON>
(defun mon-comput-45 (dollar &optional insrtp intrp)
  "Given a DOLLAR amount compute 45% retained by DBC and Customer.
When INSRTP is non-nil or called-interactively insert at point.
Does not move-point.\n
:EXAMPLE\n\(mon-comput-45 600 nil\)\n
:RETURN
:TO-PRICE-AT $600 (45% not-on-linen)
:THEM-AT-LOW $330
:OURS-AT-LOW $270\n
:SEE-ALSO `mon-comput-33'\n►►►"
  (interactive "nprice at low :\ni\np")
  (let ((comp-45 
         (format 
          ":TO-PRICE-AT $%d (45%%)\n:THEM-AT-LOW $%d\n:OURS-AT-LOW $%d"
          dollar
          (abs (- (* dollar .45) dollar))
          (abs (- (* dollar .55) dollar)))))
    (if (or insrtp intrp)
        (save-excursion (newline)
                        (princ comp-45
                               (current-buffer)))
        comp-45)))
;;
;;; :TEST-ME (mon-comput-45 600)
;;; :TEST-ME (mon-comput-33 600 t)
;;

;;; ==============================
;;; :NOTE (local-set-key "\C-c3" mon-comput-33)
;;; CREATED: <Timestamp: #{2009-10-17T11:30:06-04:00Z}#{09426} - by MON>
(defun mon-comput-33 (dollar &optional insrtp intrp) 
  "Given a DOLLAR amount compute 45% retained by DBC and Customer.
When INSRTP is non-nil or called-interactively insert at point.
Does not move-point.\n
:EXAMPLE\n\(mon-comput-33 600 nil\)\n
:RETURN
:TO-PRICE-AT $750 (33%)
:THEM-AT-HIGH $502
:OURS-AT-HIGH $247\n
:SEE-ALSO `mon-comput-45'\n►►►"
  (interactive "nprice at high :\ni\np")
  (let ((comp-33 
         (format 
          ":TO-PRICE-AT $%d (33%%)\n:THEM-AT-HIGH $%d\n:OURS-AT-HIGH $%d"
          dollar
          (abs (- (* dollar .33) dollar))
          (abs (- (* dollar .67) dollar)))))
    (if (or insrtp intrp)
        (save-excursion (newline)
                        (princ comp-33
                               (current-buffer)))
        comp-33)))
;;
;;; :TEST-ME (mon-comput-33 600)
;;; :TEST-ME (mon-comput-33 600 t)

;;; ==============================
;;; WORKING-ON-THIS: AS-OF: 2009-07-18
;;  (defun mon-insert-lisp-evald (&optional gthrp semicolons-p nlp intrp)
;; "GTHRP -> gather-line-p; NLP -> new-lines-p;"
;;  (interactive "i\ni\ni\p")
;;  (let ((line-posns)
;;       (str-len)
;;       (multi-ln-p)
;;       (evald-string)
;;       (semi-c (if semicolons-p
;;                   (cond ((numberp semicolons-p)  semicolons-p)
;;                         ((not (numberp semicolons-p))
;;                          (error "passed `%s' - a %S - as arg value. SEMICOLONS-P need a number."
;;                                 semicolons-p (type-of semicolons-p))))
;;                 1))
;;   ;; check if we have a region and/or are collecting the line
;;   (when (or gthrp use-region-p)
;;     (if use-region-p
;;         (setq line-posns `(,(region-beginning) ,(region-end)))
;;       (setq line-posns
;;             (cond ((bolp)
;;                    `(,(line-beginning-position)
;;                      ,(if (mon-line-bol-is-eol)
;;                           (line-beginning-position)
;;                         (line-end-position))))
;;                   ((not (bolp))
;;                    (progn
;;                      (goto-char (line-beginning-position))
;;                      `(,(line-beginning-position) ,(line-end-position))))))))
;;   ;; new we can set the string
;;   (setq evald-string (buffer-substring-no-properties (car line-posns) (cadr line-posns)))
;;   ;;get the string's length so we can cut it up if needed
;;   (setq str-len (length evald-string))
;;   (cond ((and use-region-p                                  ;region is active
;;               (< str-len fill-column)                   ;don't split at fill column
;;               (= (line-number-at-pos  (car line-posns)) ;not spanning multiple lines
;;                  (line-number-at-pos  (cadr line-posns))))
;;          (setq multi-ln-p nil))
;;         ((and use-region-p
;;               (/= (line-number-at-pos  (car line-posns))
;;                   (line-number-at-pos  (cadr line-posns))))
;;          (progn
;;            (setq multi-ln-p t)
;;            (setq evald-string  (split-string evald-string "\n"))))
;;         ;;region prob. isn't active but,
;;         ((> str-len fill-column) ;string is LONG, so split it.
;;          (let* ((split-at fill-column)
;;                 (split-times (/ str-len fill-column))
;;                 (remn (mod str-len fill-column))
;;                 (sub-str-pos)
;;                 (split-on))
;;            ;;build divisors of string length  |-and remainder
;;            ;;=>(0 80 160 240 320 400 480 560  579)
;;              (setq sub-str-pos
;;                    (do ((i 0 (+ i 1))
;;                         (j 0 (+ fill-column j))
;;                         (k '() (cons j k)))
;;                        ((= split-times (1- i)) k)))
;;              (setq sub-str-pos (cons (+ (car sub-str-pos) remn) l))
;;              (setq sub-str-pos (nreverse sub-str-pos))
;;              (while sub-str-pos
;;                (push (substring evald-string (car sub-str-pos) (cadr sub-str-pos)) split-on)
;;                (pop sub-str-pos))
;;              (setq split-on (reverse split-on))
;;              (setq evald-string split-on)
;;              (setq multi-ln-p t)))
;;            )
;;   ;;finish me
;;   (let* ((semi-top (if ((and use-region-p (> 1 semi-c))
;;                         (concat (make-string (1- semi-c 59)))
;;                         (make-string semi-c 59))))
;;          (top-ln (if nlp
;;                      (concat "\n" semi-top "=>")
;;                    (concat semi-top "=>"))
;;                  ;;(pop evald-string)
;;                  ;;(mapconcat function sequence separator)
;;                  ;; (mapconcat (lambda (x) (concat ";;; " x)) split-on  "\n"))
;;                  (mapconcat (lambda (x) (concat (semiconcancat) (car x)))
;;                             evald-string "$")
;; ;;    (delete-and-extract-region (car line-posns) (cadr line-posns)
;;                  ))))))
;;; ==============================
;;; ==============================

;; (setq (str-len-b (length my-str))
;; (setq my-str
;; "Phasellus porta auctor urna sed elementum. Etiam vel aliquam magna. Nam posuere, quam eu rhoncus tempus, tellus metus vulputate lacus, ac mattis arcu felis ut enim. Suspendisse potenti. Fusce massa diam, semper ut tempus sit amet, feugiat a arcu. Maecenas quis arcu eu felis porttitor interdum sit amet sed ipsum. Cras semper nunc enim, et iaculis urna. Nunc at purus eros, sit amet vulputate lectus. Proin viverra ante vel mi rutrum in posuere tortor mattis. Aenean quis quam sapien. Fusce sed erat leo, vitae feugiat dolor. Ut id mi massa. Curabitur sollicitudin tristique tortor, tincidunt vulputate nibh sagittis a. Proin turpis leo, fringilla sit amet sodales at, malesuada vitae diam. Cras facilisis, odio a elementum varius, nunc dolor porttitor massa, at condimentum metus purus vel ligula. Ut quam est, ornare at congue eu, hendrerit id nulla. Nunc id ligula sapien. Integer non tortor id mauris consectetur accumsan. Donec in orci vel purus pharetra pulvinar ut nec nunc.")


;; (let*  (;temporary
;;         ;;my-str ;evald-string
;;         ;;stre-len-b strn-len
;;         (str-len-b (length my-str))
;;         (split-at fill-column)
;;         (split-times (/ str-len-b split-at))
;;         (remn (mod str-len-b fill-column))
;;         (l)
;;         (split-on))
;;   (setq l
;;         (do ((i 0 (+ i 1))
;;              (j 0 (+ fill-column j))
;;              (k '() (cons j k)))
;;             ((= split-times (1- i)) k)))
;;   (setq l (cons (+ (car l) remn) l))
;;   (setq l (nreverse l)) ;=>(0 80 160 240 320 400 480 560 579)
;;   (while l
;;     (push (substring my-str (car l) (cadr l)) split-on)
;;     (pop l))
;;   (setq split-on (reverse split-on))
;;   (mapconcat (lambda (x) (concat ";;; " x)) split-on  "\n"))
;;; ==============================

;;; ==============================
;;; beginning-of-defun
;;; FIXME: REGEXP doesn't catch on cases where the lambda list is on the next line.
;;; FIXME: Insertion of defvar test-me's should either:
;;;        use a (symbol-value VAR), or; 
;;;        not be placed in side a list form.
;;;
;;; TODO: 
;;; - Consider use of defmacro/defvar/defun* .defvar will need to 
;;;   track on varnames with '*"
;;;
;;; - Provide a facility to include unbinding strings or add a new func e.g.:
;;;   (progn(makunbound 'some-func-or-var) (unintern 'some-func-or-var))
;;;
;;; - The test-me subr should insert differently depending on symbol 'type' 
;;;   and cnt esp. for use with mon-insert-doc-help tail.
;;;   4 'test-me's for functions
;;;   "test-me;(<FNAME>)"
;;;   "test-me;(<FNAME> t)"
;;;   "test-me;(describe-function '<FNAME>)"
;;;   "test-me;;(call-interactively '<FNAME>)"
;;;   When a variable is found: should insert:
;;;   "test-me; <VARNAME>"
;;;   "test-me;(describe-variable '<VARNAME>)"
;;;   When a face is found: should insert:
;;;   "test-me;(describe-face '<FACENAME>)"
;;; ==============================
;;; :UNFINISHED-AS-OF 
;;; <Timestamp: #{2009-10-05T16:03:34-04:00Z}#{09411} - by MON KEY>
;;; :CREATED <Timestamp: 2009-07-31-W31-5T13:53:52-0400Z - by MON KEY>
(defun mon-insert-lisp-testme-fancy (&optional search-func test-me-count insertp intrp)
  "Insert at point a newline and commented test-me string.
When non-nil SEARCH-FUNC will search backward for a function name and include it 
in the test-me string.
When non-nil TEST-ME-COUNT will insert test-me string N times. 
Default is to insert 1\(one\) time.
When prefix arg TEST-ME-COUNT is non-nil inerts N number of ';;;test-me' strings 
and prompt y-or-n-p if we should include the function name with insertion.
When INSERTP is non-nil insert the test-me string(s) in current buffer at point.
Use at the end of newly created elisp functions to provide example test cases.
Regexp held by global var `*regexp-symbol-defs*'.\n
:SEE-ALSO `mon-insert-doc-help-tail', `mon-test->*regexp-symbol-defs*'
`mon-insert-doc-help-tail', `mon-insert-lisp-stamp', `mon-insert-copyright',
`mon-insert-lisp-CL-file-template', `comment-divider',
`comment-divider-to-col-four', `mon-insert-lisp-evald'.\n►►►"
  (interactive "i\np\ni\np")
  (let* ((get-func)
         (tmc (cond ((and intrp (> test-me-count 1))
                      (if ((lambda () (yes-or-no-p "Search-function-name? :")))
                          (progn (setq get-func t) test-me-count)
                        (progn (setq get-func nil) test-me-count)))
                    ((not test-me-count) 1)
                    (t  test-me-count)))
         (sym-nm (if (or search-func get-func)
                   (save-excursion 
                     (search-backward-regexp  *regexp-symbol-defs*)
                     (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (sym-typ (match-string-no-properties 2))
         (fun-syms '("defun" "defun*" "defmacro" "defmacro*" "defsubst" "defsubst*"))
         (var-syms '("defvar" "defconst" "defcustom"))
         ;;(sym-name &key :alt-cookie :do-var :insertp :do-face :do-group :do-theme
         (sym-typ-cond (cond ((car (member found fun-syms))
                              (concat 
                               ";;; :TEST-ME (%s)"
                               (when (car (assoc 'interactive (symbol-function sym-nm)))
                                 "\n;;; :TEST-ME \(call-interactively '%s\)")
                               ;; ==============================
                               ;;Working on this- So finish it then already... 
                               (let ((tst-intrp-l (mon-help-function-args sym-nm))
                                     (intrp-posn (car (intersection 
                                                       '(insertp insert-p insrtp insrt-p 
                                                         :insertp :insert-p :insrtp :insrt-p)
                                                       tst-intrp)))
                                     ;; :UNFINISHED-AS-OF 
                                     ;; <Timestamp: #{2009-10-05T16:03:34-04:00Z}#{09411} - by MON KEY>
                                     )
                                     (if intrp-posn
                                     (position inrp-posn (mon-help-function-args sym-nm)))
                                     )
                               )) ;close first cond
                             ;;
                             ((car (member found var-syms)) 
                              ";;; :TEST-ME %s")
                             ((string= found "defface")
                              (concat ";;; :TEST-ME \(facep '%s\)\n"
                                      ";;; :TEST-ME \(face-default-spec '%s\)"))
                             ((string= found "defgroup")
                              ";;; :TEST-ME %s")
                             ((string= found "deftheme")
                              ";;; :TEST-ME \(custom-theme-p '%s\)")
                             (t ";;; :TEST-ME %s")))
         (test-me-string (if (or search-func get-func)
                             ;;  (format  ";;; :TEST-ME (%s )" sym-nm)
                             (format sym-typ-cond sym-nm)
                           ";;; :TEST-ME "))
         (limit (make-marker))
         (cnt tmc)
         (return-tms))
    (while (>= cnt 1)
      (setq return-tms (concat test-me-string "\n" return-tms))
      (setq cnt (1- cnt)))
    (if (or intrp insertp)
          (progn
            (save-excursion
              (when insertp (newline))
              (when (not (bolp))(beginning-of-line))
              (princ return-tms (current-buffer))
              (set-marker limit (point)))
          (search-forward-regexp 
           (format "%s$" test-me-string) (marker-position limit) t) 
          t) ; t needed here to prevent returning buffer position when called externally?
        return-tms)))
;;;
;;; ==============================
;;; TEMPLATE FOR GATHERING data re: previous function:
;;; ==============================
;; (search-backward-regexp *regexp-symbol-defs*)
;; (sym (match-string-no-properties 3)) ;symbol name
;; (found (match-string-no-properties 2)) ;symbol type
;;
;; (cond ((functionp sym-name)
;;        (or (documentation sym-name)
;;            (when (stringp (caddr (symbol-function 'mon-help-function-spit-doc)))
;;              (stringp (caddr (symbol-function 'mon-help-function-spit-doc)))))) ;funcs, macros
;;       (do-var (or (plist-get (symbol-plist sym-name) 'variable-documentation)
;;                   (documentation-property sym-name 'variable-documentation))) ;var, const, customs
;;       (do-face (or (face-documentation sym-name)
;;                    (plist-get (symbol-plist sym-name) 'face-documentation)
;;                    (documentation-property sym-name 'face-documentation))) ;; faces
;;       (do-group (or (plist-get (symbol-plist sym-name) 'group-documentation)
;;                     (documentation-property sym-name 'group-documentation))) ;; groups
;;       (do-theme (or (plist-get (symbol-plist sym-name) 'theme-documentation)
;;                     (documentation-property sym-name 'theme-documentation))) ;; consider 'theme-settings
;;       (t (documentation sym-name)))
;;
;; ;;(mon-help-function-spit-doc (sym-name &key :alt-cookie :do-var :insertp :do-face :do-group :do-theme)
;; (sym-str-cond (cond ((car (member found '("defun" "defun*" "defmacro" "defmacro*")))
;;                           "      (mon-help-function-spit-doc '%s :insertp t)\n")
;;                      ((car (member found '("defvar" "defconst" "defcustom")))
;;                      "      (mon-help-function-spit-doc '%s :do-var t :insertp t)\n")
;;                      ((string= found "defface")
;;                      "      (mon-help-function-spit-doc '%s :do-face t :insertp t)\n")
;;                      ((string= found "defgroup")
;;                       "      (mon-help-function-spit-doc '%s :do-group t :insertp t)\n")
;;                      ((string= found "deftheme")
;;                       "      (mon-help-function-spit-doc '%s :do-theme t :insertp t)\n")
;;                      (t "      (mon-help-function-spit-doc '%s :insertp t)\n")))
;;; ==============================

;;; ==============================
(provide 'mon-insertion-utils)
;;; ==============================

;;; ==============================
;;; mon-insertion-utils.el ends here
;;; EOF

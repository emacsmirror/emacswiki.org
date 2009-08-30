;;; This is mon-insertion-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-insertion-utils.el Provides Insertion related utilities. 
;;; These are templates and string building utilities that insert into
;;; a buffer and which have become to unwieldly or are otherwise to large
;;; to maintain in the calling file(s).
;;;
;;; CONSTANTS or VARIABLES:
;;; `*mon-gnu-licences-header*'
;;; ;;; (mon-call-tree-analyze) *mon-call-tree-alist*
;;; MACROS: font-lock-function-name-face
;;; (mon-call-tree-analyze)
;;;   (mon-call-tree-analyze) *mon-call-tree-alist* 
;;;
;;; FUNCTIONS:►►►
;;; `mon-insert-string-n-fancy-times', `mon-number-lines-region',
;;; `mon-insert-numbers-padded', `mon-insert-string-incr', `mon-dropin-line-words',
;;; `mon-string-ify-interactively', `mon-insert-test-cases', 
;;; `mon-insert-string-n-times', `mon-lisp-evald', `comment-divider',
;;; `comment-divider-to-col-four', `php-comment-divider', `mon-insert-copyright',
;;; `mon-insert-file-in-dirs', `mon-insert-naf-file-in-dirs'
;;; `mon-insert-dirs-in-path', `mon-insert-naf-mode-constant-template',
;;; `mon-insert-wht-spc', `mon-insert-newlines', `mon-build-copyright-string'
;;; `mon-insert-naf-mode-file-template', `mon-insert-CL-file-template',
;;; `mon-insert-CL-package-template', `mon-insert-user-name-cond', 
;;; `mon-insert-system-type-cond', `mon-insert-gnu-licence'
;;; FUNCTIONS:◄◄◄
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
;;; `mon-insert-naf-mode-file-template' ->`mon-insert-file-template'
;;;
;;; MOVED: 
;;; `mon-insert-user-name-cond'   <- ../mon-w32-load.el
;;; `mon-insert-system-type-cond' <- ../mon-w32-load.el
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
;;;
;;; TODO:
;;; Build a wget template.
;;; (defun mon-wget-url (extension url put-in)
;;; wget -nd -r -l1 --noparent -A."file-extension" "url"
;;; See; Stefan Reichor's defun in `naf-url-utils' (commented - not active)
;;;
;;; Build a progn makunbound unintern template e.g.:
;;; ;;;(progn (makunbound 'VAR-OR-FUNCTION) 
;;; ;;; (unintern 'VAR-OR-FUNCTION))
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
;;; Copyright (C) 2009 - MON KEY
;;; ============================
;;; CODE:

;;; ==============================
(eval-when-compile (require 'cl)) ;why not?
;;; ==============================

;;; ==============================
(require 'mon-time-utils) ;anything that uses a time-stamp
;;; ==============================

;;; ==============================
(defun mon-insert-dirs-in-path (dir-list dir-path)
  "Directory elts held by symbol DIR-LIST (list of strings) are inserted in DIR-PATH.
This function does minimal error checking but shouldn't be trusted for unfamilar pathnames."
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

;;;test-me;(setq t-dir-list '("abc" "def" "ghi")) 
;;;test-me;(mon-insert-dirs-in-path t-dir-list default-directory)
;;; ==============================

;;; ================================================================
;;; CREATED: <Timestamp: Wednesday May 06, 2009 @ 03:32.25 PM - by MON KEY>
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
EXAMPLE:\nAssuming buffer's default directory is \"c:/home/my-dirs\"
Invoking the form with args make-my-dirs my-insert-text \".dbc\":\n
\(mon-insert-file-in-dirs make-my-dirs my-insert-text \".dbc\")\n
Or, interactively; M-x mon-insert-naf-file-in-dirs\n
=> minibuffer-prompt: Give Symbol holing dir/file list : make-my-dirs
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
;;; CREATED: <Timestamp: Thursday June 11, 2009 @ 02:23.20 PM - by MON KEY>
(defun mon-insert-wht-spc (spc-cnt)
  "Insert a space char SPC-CNT times.
Called interactively SPC-CNT is a prefix arg.
space is:\n- character: SPC (32, #o40, #x20);\n- code point: 0x20\n- name: SPACE\n
Note: does not inherit stickiness of adjoining chars `text-properties-at'.\n
See also; `mon-insert-newlines', `mon-insert-unicode',
`mon-insert-string-n-times', `mon-insert-string-n-fancy-times'."
  (interactive "N")
  (insert-char ? spc-cnt nil))

;;;test-me;(mon-insert-wht-spc 8)

;;; ==============================
;;; <Timestamp: Friday June 12, 2009 @ 11:18.21 AM - by MON KEY>
(defun mon-insert-newlines (nl-cnt)
  "Insert a newline char NL-CNT times.
Called interactively NL-CNT is a prefix arg.
newline is:\n- code point: '0x0A';\n- character: C-j (10, #o12, #xa);\n- old-name: LINE FEED (LF);\n
Note: does not inherit stickiness of adjoining chars `text-properties-at'\n
See also; `mon-insert-wht-spc', `mon-insert-unicode', 
`mon-insert-string-n-times', `mon-insert-string-n-fancy-times'."
  (interactive "N")
  (insert-char ?\n nl-cnt nil))

;;;test-me;(mon-insert-newlines 3)

;;; ================================================================
;;; CREATED: <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
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
See also; `mon-insert-string-n-fancy-times', `mon-insert-string-incr', `mon-re-number-region',
`mon-insert-wht-spc', `mon-insert-newlines', `mon-insert-unicode'."
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
;;; CREATED: <Timestamp: Monday February 09, 2009 @ 09:41.37 PM - by MON KEY>
(defun mon-insert-string-n-times (put-count string-to-put)
"Inserts the string provided n times.
Prompts for: How many puts? <-- number of times to put string.
             String to put: <--- String  to insert.\n
See also; `mon-insert-string-n-fancy-times', `mon-insert-string-incr', `mon-re-number-region',
`mon-insert-wht-spc', `mon-insert-newlines', `mon-insert-unicode'."
  (interactive (list (read-number "How many puts?")
		     (read-string "String to put:")))
    (dotimes (i
	      put-count
	      t)
      (insert string-to-put)))

;(info "(elisp)documentation")

;;; ==============================
(defun mon-number-lines-region (start end &optional beg)
  "Numbers the lines in a region. The default value is 1.
To begin at zero, type M-0 \\[mon-number-lines-region].
Negative numbers are also supported. Numbers are right aligned followed by a
period and a space. If point is at the beginning of the region, the lines
will be numbered in descending order. If a line is already prefixed with a
number, it will be overwritten with the new value.\n
Unlike `mon-insert-string-incr' allows prefix starting value - numeric argument.\n
See also; `mon-insert-numbers-padded', `mon-re-number-region', 
`mon-rectangle-sum-column'."
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
;;; COURTESY: Noah Friedman HIS: buffer-fns.el
(defun mon-insert-numbers-padded (start end &optional padp)
  "Insert the numbers from START to END (inclusive) in the current buffer.
Each number is inserted on a separate line.  START may be less than END, in
which case counting is backward.\n
If given a prefix argument or optional arg PADP is non-nil, pad all numbers
with sufficient leading zeros so they are the same width.\n
See also; `mon-insert-string-incr',`mon-number-lines-region', `mon-re-number-region', 
`mon-rectangle-sum-column'."
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

;;;=========================
;;; CREATED: <Timestamp: Tuesday February 03, 2009 @ 03:41.17 PM - by MON KEY>
;;; RENAMED: was: `mon-incr' -> `mon-insert-string-incr'
(defun mon-insert-string-incr ()
  "Inserts to current buffer the range of number provided. 
Prompts for; \"Increment from number:\", \"Increment to number:\",
\"Step by increments of:\",\"With delimiter?\", \"Insert newlines?\".
When the range will return over 1000 values throws an error.
When \"With delimiter?\" is t, prompts \"Delimit range with:\".
When both \"With delimiter?\" and \"Insert newlines?\" are nil promts with,
\"Interleave range with whitespace? \" as whitespace isn't a delimter by default.\n
See also; `mon-insert-numbers-padded', `mon-number-lines-region', 
`mon-re-number-region', `mon-rectangle-sum-column'."
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
      (when llm-off (longlines-mode 1) (setq llm-off 'nil)))  ))))

;;; ==============================
;;; CREATED: <Timestamp: Thursday February 19, 2009 @ 06:31.47 PM - by MON KEY>
;;; MODIFICATIONS-OF: Drew Adams' strings.el
(defun mon-dropin-line-words (&optional buffer) 
 "Split current line of text in BUFFER into single words.
Split line is inserted with each word on a new line."
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
;;; RENAMED: `mon-interactively-stringify' -> `mon-insert-string-ify'
(defun mon-insert-string-ify (the-string)
  "Read a string at minibuffer and split it into tokens.
Minibuffer Prompt with \"Stringify This-->\" returns a
list of strings obtained by breaking the string the user entered at the
space boundaries.\n\nEXAMPLE:
\(mon-insert-string-ify \"a b c\"\)\n=>(\"a\" \"b\" \"c\"\)\).\n
See also; `mon-string-ify-list'."
(interactive "sStringify This-->")
(print (mon-string-ify-list the-string) (current-buffer)))

;;;test-me;(mon-insert-string-ify "a b c")

;;; ==============================
;;; utf-8-auto-unix, utf-8-emacs-unix, encode-coding-region, describe-coding-system
;;; ==============================
;;; NOTE: Now inlined with the let in `unicode-insert'.
;;; (setq read-quoted-char-radix 16)
;; prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
(defun mon-insert-unicode (char &optional insertp intrp)
  "Reads a unicode code point for CHAR and insert CHAR at point.\n
EXAMPLE:\n(mon-insert-unicode #x25ba)\n
Uses `read-quoted-char-radix' with val set to 16. This allows easy coping of
values from Unicode charts e.g. (URL `http://www.decodeunicode.org/u+2019').\n
See also; `mon-insert-wht-spc', `mon-insert-newlines'.\n
For access/alteration to encoding/coding information see:
`encode-coding-region', `describe-coding-system', `unicode-insert'."
  (interactive `(,@(let* ((rd-types (list "1 Unicode char in radix 16 e.g. '25BA'"
                                          "2 Unicode char by name w/ completion (octal, hex, decimal)"))
                          (rd-type (completing-read "read Unicode as " rd-types nil t))
                          (char-rd (cond ((string= (substring rd-type 0 1) "1")
                                          (let ((read-quoted-char-radix 16))
                                            (char-to-string (read-quoted-char "Char :"))))
                                         ((string= (substring rd-type 0 1) "2")
                                          (let (the-char)
                                            (setq (the-char (read-char-by-name "Char (tab completes) :")))
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
                        (mon-string-to-symbol char))
                                        ; ((string-to-number char 16))
                       ((not (integerp char))
                        (error "Not a Unicode character code: %S" char))
                       ((or (< char 0) (>= char 65536)) ;;(or (< 9658 0) (> 9658 65536))
                        (error "Not a Unicode character code: %d" char))
                       ((or (< char 0) (> char #x10FFFF)) ;;(or (< #x25ba 0) (> #x25ba #x10FFFF))
                        (error "Not a Unicode character code: 0x%X" char))))
                ;;(t (setq t-char (char-to-string t-char))))
                ))
    (setq t-char (char-to-string t-char))
    (if (or insertp intrp)
        (save-excursion (insert-and-inherit t-char))
      t-char)
    t-char))

;;;(mon-insert-unicode "#o22672" t)►
;;;(mon-insert-unicode #o22672 t)►
;;;(mon-insert-unicode "#x25ba" t)►
;;;(mon-insert-unicode 9658 t)►
;;;(mon-insert-unicode #x25ba t)►
;;;(mon-insert-unicode "BLACK RIGHT-POINTING POINTER" t)►►
;;;(mon-insert-unicode "bubba" t)
;;;(mon-insert-unicode "#o22672" )
;;;(mon-insert-unicode #o22672 )
;;;(mon-insert-unicode "#x25ba" )
;;;(mon-insert-unicode 9658)
;;;(mon-insert-unicode #x25ba)
;;;(mon-insert-unicode "BLACK RIGHT-POINTING POINTER")
;;;(mon-insert-unicode "bubba" )
;;;(mon-insert-unicode "bubba" t)
;;; ==============================


;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-25T14:09:37-04:00Z}#{09352} - by MON KEY>
(defun comment-divider (&optional not-insert intrp)
  "Insert default comment divider at point.
When called-interactively inserts at point:\n
;;; ==============================\n
When optional arg not-insert is non-nil return comment divider as string.\n
EXAMPLE:\n\(comment-divider t\)\n
See also; `mon-comment-divide->col', `mon-lisp-comment-to-col'
`mon-insert-php-comment-divider', `mon-insert-lisp-stamp'."
  (interactive "i\np")
  (if (or (not not-insert) intrp) 
      (insert ";;; ==============================")
    ";;; =============================="))

;;;test-me;(comment-divider t)
;;;test-me;(comment-divider)
;;;test-me;(call-interactively 'comment-divider)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-25T19:03:44-04:00Z}#{09352} - by MON KEY>
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
See also; `mon-lisp-comment-to-col' for a specifically lisp centric interactive
implementation."
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

;;;(progn (makunbound 'comment-divider-to-col) (unintern 'comment-divider-to-col))
;;
(defalias 'mon-comment-divide->col 'comment-divider-to-col)
;;
;;;(progn (makunbound 'mon-comment-divide->col) (unintern 'mon-comment-divide->col))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-25T18:17:18-04:00Z}#{09352} - by MON KEY>
(defun mon-lisp-comment-to-col (&optional col-n)
  "Insert `comment-divider' at COL-N comment and indent region/next line to col.
COL-N is a prefix arg. When region is active indent and comment content of region 
to COL-N else indent and comment next line. Comment prefix is `;; '.
To provide an alternative comment prefix use `comment-divider-to-col'.
See also; `comment-divider'"
  (interactive "P")
  (comment-divider-to-col col-n nil nil alt-prefix nil t))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-25T19:17:39-04:00Z}#{09352} - by MON KEY>
;;; CREATED: <Timestamp: #{2009-08-25T14:48:38-04:00Z}#{09352} - by MON KEY>
(defun comment-divider-to-col-four () 
  "DEPRECATED: This function has been replaced by `comment-divider-to-col'.
Insert `comment-divider' indentented to column 4(four).
Move contents current line forward 1(one) line indent it to column 4(four).\n"
(interactive)
(save-excursion
  (open-line 1)
  (line-move 1)
  (beginning-of-line)
  (indent-to-column 4)
  (comment-divider)
  (open-line 1)))

;;; ==============================
;;; RENAMED: `php-comment-divider' -> `mon-insert-php-comment-divider'
(defun mon-insert-php-comment-divider ()
  "Inserts a PHP(C style) comment divider at point. Inserts at point:\n
 //***************************//\n
See; `comment-divider' and See; `comment-divider-to-col-four' for lispy version."
  (interactive)
  (insert "//***************************//"))

;;;test-me;(call-interactively 'mon-insert-php-comment-divider)

;;; ==============================

;;; ==============================
;;; MODIFICATIONS: <Timestamp: 2009-08-04-W32-2T13:24:27-0400Z - by MON KEY>
(defun mon-insert-user-name-cond (&optional insertp intrp)
  "Inserts a cond template to evaluate user-name - IS-BUG-P or IS-MON-P."
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

;;;test-me;(mon-insert-user-name-cond)
;;;test-me;(mon-insert-user-name-cond t)
;;;test-me;(call-interactively 'mon-insert-user-name-cond)

;;; ==============================
;;; MODIFICATIONS: <Timestamp: 2009-08-04-W32-2T13:24:13-0400Z - by MON KEY>
(defun mon-insert-system-type-cond (&optional insertp intrp)
  "Inserts a cond template to test for system type OS type. Curenlty tests for
GNU/Linux and windows-nt only. See `win32p' and `gnu-linuxp' for constants
that return t or nil." 
  (interactive "i\np")
  (let ((stc (concat
              "\n(cond\n"
              " ((equal system-type 'windows-nt) \"I'm a slave to the w32.\")\n"
              " ((equal system-type 'gnu/linux) \"I'm a GNU!\"))\n")))
    (if (or insertp intrp)
        (save-excursion (insert stc))
      stc)))

;;;test-me;(mon-insert-system-type-cond)
;;;test-me;(mon-insert-system-type-cond t)
;;;test-me;(call-interactively 'mon-insert-system-type-cond)

;;; ==============================
;;; CREATED: <Timestamp: Saturday July 11, 2009 @ 12:15.02 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T16:11:16-0400Z - by MON KEY>
(defun mon-insert-lisp-CL-file-template (&optional insertp intrp)
  "Inserts a CL header top of file.
See also; `mon-insert-lisp-stamp', `mon-insert-lisp-test-me',
`mon-insert-lisp-evald',`mon-insert-copyright', `comment-divider',
`comment-divider-to-col-four', `mon-stamp', `mon-file-stamp'." 
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

;;;test-me;(mon-insert-lisp-CL-file-template)
;;;test-me;(mon-insert-lisp-CL-file-template t)
;;;test-me;(call-interactively 'mon-insert-lisp-CL-file-template)

;;; ==============================
;;; COURTESY: Zach Beane his: scratch-lisp-file.el
;;; (URL `http://www.xach.com/lisp/scratch-lisp-file.el')
;;; MODIFICATIONS: <Timestamp: Tuesday July 14, 2009 @ 02:12.25 AM - by MON KEY>
;;;               changed the buffer-file-name to check if we have a name.
(defun mon-insert-CL-package-template (&optional insertp without-header intrp)
  "Return or insert a CL package-template a template. 
Builds (with DEFPACKAGE and IN-PACKAGE forms) into the current buffer.
Called interactively or INSERTP non-nil
Assumes current buffer is empty and inserts a file template with
`mon-insert-lisp-CL-file-template'."
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

;;;test-me;(mon-insert-CL-package-template)
;;;test-me;(mon-insert-CL-package-template t)
;;;test-me;(call-interactively 'mon-insert-CL-package-template)

;;; ==============================
;;; CREATED: <Timestamp: Friday June 12, 2009 @ 12:24.26 PM - by MON KEY>
(defun mon-insert-lisp-stamp (&optional insertp intrp modifications)
  "Retrun or Insert at point a `comment-divider' newline and `mon-stamp'.
When INSERTP is non-nil or called interactively insert at point. 
When MODIFICATIONS is non-nil or called interactively with Prefix Arg.
Returns with a ';;; MODIFICATIONS: ' prefix -default is ';;; CREATED: '
Use after creating new elisp functions to delimit and date them.\n
EXAMPLE:
\(mon-insert-lisp-stamp\)
\(mon-insert-lisp-stamp nil nil t\)\n
See also; `mon-lisp-stamp',`mon-file-stamp', `mon-insert-copyright',
`mon-insert-lisp-test-me', `mon-insert-lisp-CL-file-template',
`comment-divider', `comment-divider-to-col-four', `mon-insert-lisp-evald'."
  (interactive "i\np\nP")
      (if (or insertp intrp)
          (if modifications 
              (mon-lisp-stamp t nil t)
            (mon-lisp-stamp t))            
        (if modifications 
            (mon-lisp-stamp nil nil t)
          (mon-lisp-stamp))))

;;;test-me;(mon-insert-lisp-stamp)
;;;test-me;(mon-insert-lisp-stamp t)
;;;test-me;(mon-insert-lisp-stamp nil nil t)
;;;test-me;(mon-lisp-stamp t nil t)
;;;test-me;(call-interactively 'mon-insert-lisp-stamp)

;;; ==============================
;;; working on this as of 7 18 2009
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





;;; ==============================
;;; FIXME: REGEXP doesn't catch on cases where the lambda list is on the next line.
;;; FIXME: Insertion of defvar test-me's should either:
;;;        use a (symbol-value VAR), or; 
;;;        not be placed in side a list form.
;;;
;;; TODO: 
;;; Consider use of defmacro/defvar/defun* .defvar will need to 
;;; track on varnames with '*"
;;;
;;; Provide a facility to include unbinding strings or add a new func.
;;; (progn
;;; (makunbound 'some-func-or-var)
;;; (unintern 'some-func-or-var))

;;; ==============================
;;; CREATED: <Timestamp: 2009-07-31-W31-5T13:53:52-0400Z - by MON KEY>
(defun mon-insert-lisp-testme (&optional with-func test-me-count insertp intrp)
  "Insert at point a newline and commented test-me string.
When non-nil WITH-FUNC will search backward for a function name and include it 
in the test-me string.
When non-nil TEST-ME-COUNT insert test-me string N times. Default is 1\(one\).
When prefix arg TEST-ME-COUNT is non-nil inerts N number of ';;;test-me' strings 
and prompts y-or-n-p if we want to include the function name in insertions.
When INSERTP is non-nil insert the test-me string(s) in current buffer at point.
Use at the end of newly created elisp functions to provide example test cases.
Regexp held by global var `*regexp-symbol-defs*'.\n
See also; mon-doc-help `mon-insert-lisp-stamp', `mon-insert-copyright',
`mon-insert-lisp-CL-file-template', `comment-divider',
`comment-divider-to-col-four', `mon-insert-lisp-evald'."
  (interactive "i\np\ni\np")
  (let* ((get-func)
         (tmc (cond ((and intrp (> test-me-count 1))
                      (if ((lambda () (yes-or-no-p "Search-function-name? :")))
                          (progn (setq get-func t) test-me-count)
                        (progn (setq get-func nil) test-me-count)))
                    ((not test-me-count) 1)
                    (t  test-me-count)))
         (func (if (or with-func get-func)
                   (save-excursion 
                     (search-backward-regexp  *regexp-symbol-defs*)
                     (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (test-me-string (if (or with-func get-func)
                             (format ";;;test-me;(%s )" func)
                           ";;;test-me;"))
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

;;;Uncomment to test:
;;;(defun some-function (&optional optional)
;;;(defun some-function-22 (&optional optional)
;;;(defun *some/-symbol:->name<-2* (somevar
;;;(defmacro some-macro ()
;;;(defmacro some-macro*:22 (&rest)
;;;(defun *some/-symbol:->name<-2* (somevar
;;;(defvar *some-var* 'var
;;;(defun *some/-symbol:->name<-2* 'somevar

;;;test-me;
;;(let ((find-def* *regexp-symbol-defs*))
;; (search-backward-regexp find-def*))

;;
;;;test-me;`(,(match-beginning 3) ,(match-end 3))
;;;test-me;(match-sring 1) ;grp 1=>"(defun* some-func:name* ("
;;;test-me;(match-sring 2) ;grp 2=>"(defun* "
;;;test-me;(match-string 3) ;grp 3=>"some-macro*:22"
;;;test-me;(match-sring 4) ;grp 4=>" (" 
    
;;;test-me;(mon-insert-lisp-testme)
;;;test-me;(mon-insert-lisp-testme t 3 )
;;;test-me;(mon-insert-lisp-testme nil 3)
;;;test-me;(mon-insert-lisp-testme nil 3 t)
;;;test-me;(mon-insert-lisp-testme t 3 t)
;;;test-me;(mon-insert-lisp-testme t nil t)
;;;test-me;(mon-insert-lisp-testme nil nil t)
;;;test-me;(mon-insert-lisp-testme nil nil nil)

;;; ==============================
;;; CREATED: <Timestamp: Saturday July 11, 2009 @ 12:41.53 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T13:44:23-0400Z - by MON KEY>
(defun mon-build-copyright-string (&optional insertp intrp monkey no-nl with-dcp short-form)
  "Return a copyright string built conditionally on users name.
if insertp or INSERTP is non-nil or called interactively insert 
copyright string at point.
When MONKEY is non-nil or called-interactively with Prefix arg.
use \"MON KEY\". See also; `mon-insert-copyright'."
  (interactive "i\np\nP") 
  (let ((name (cond (monkey (cadr (assoc 6 *MON-NAME*)))
                    (IS-BUG-P (cadr (assoc 1 *BUG-NAME*)))
                    (IS-BUG-P-REMOTE (cadr (assoc 1 *BUG-NAME*)))
                    (IS-MON-P (cadr (assoc 3 *MON-NAME*)))
                    (t "")))
        (with-dcp (cond (monkey 
                        (if with-dcp (cadr (assoc 1 *DCP-NAME*)) ""))
                        (t (cadr (assoc 1 *DCP-NAME*)))))
        (year (mon-get-current-year)) ;(format-time-string "%Y"))
        (cls-comment (cond ((and short-form monkey (not with-dcp)) 
                            ";;; ==============================")
                           ((and short-form (not with-dcp)) 
                            ";;; ==============================")
                           ((and monkey with-dcp) 
                            ";;; ================================================================")
                           ((and short-form with-dcp) 
                            ";;; ================================================================")
                           (t ";;; ================================================================")))
        (cpy))
    (setq cpy
          (if no-nl
            (concat
             ";;; ================================================================\n"
             ";;; ©opyright (C) - " name " - " year with-dcp "\n"
             cls-comment)
            (concat
               "\n;;; ================================================================\n"
               ";;; ©opyright (C) - " name " - " year with-dcp "\n"
               cls-comment"\n")))
    (when (or insertp intrp) 
      (save-excursion (insert cpy)))
    cpy))

;;;(&optional insertp intrp monkey no-nl with-dcp short-form)

;;test-me;(mon-build-copyright-string)
;;test-me;(mon-build-copyright-string nil nil t)
;;test-me;(mon-build-copyright-string nil nil t t)
;;test-me;(mon-build-copyright-string nil nil t t)
;;test-me;(mon-build-copyright-string t nil t nil t )
;;test-me;(mon-build-copyright-string t nil t nil t t)
;;test-me;(mon-build-copyright-string nil nil t t nil t)
;;test-me;(mon-build-copyright-string nil nil t nil nil t)
;;test-me;(mon-build-copyright-string nil nil t t t t)
;;test-me;(mon-build-copyright-string nil nil t t nil t)
;;test-me;(call-interactively 'mon-build-copyright-string)


;;; ==============================
;;; CREATED: <Timestamp: Saturday July 11, 2009 @ 12:35.37 PM - by MON KEY>
;;; DEPRECATED: USE: (mon-build-copyright-string t)
(defun mon-insert-copyright (&optional monkey)
  "Insert copyright with relevant details, conditional on user's system name.
See also; `mon-build-copyright-string'.\nUsed in `naf-mode'." 
  (interactive "P")
  (mon-build-copyright-string t nil monkey))

;;test-me;(mon-insert-copyright)
;;test-me;(mon-insert-copyright t)
;;test-me;(call-interactively 'mon-insert-copyright)

;;; ==============================
;;; CREATED: <Timestamp: Thursday April 09, 2009 @ 05:52.10 PM - by MON KEY>
;;; RENAMED: `mon-insert-naf-mode-file-template' ->`mon-insert-file-template'
(defun mon-insert-file-template (&optional with-fname insertp intrp)
"Insert a file template. Template is `naf-mode' DCP biased.
See also; `mon-insert-lisp-CL-file-template', `mon-file-stamp'."
  (interactive "i\ni\np")  
    (let* ((cur-nm (buffer-name))
           (fname (if (and intrp (mon-buffer-written-p))
                      cur-nm
                    "FILENAME.el"))
           (fname-sans 
            (if (and intrp (mon-buffer-written-p))
                (file-name-sans-extension cur-nm)
                cur-nm
              "FILENAME"))
           (fl-template
            (format 
";; -*- mode: EMACS-LISP; -*-
;;; this is %s
;;; ================================================================
;;; DESCRIPTION:
;;; %s {description here}.\n;;;
;;; FUNCTIONS:\n;;;
;;; CONSTANTS or VARIABLES:\n;;;
;;; MACROS:\n;;;
;;; SUBST or ALIASES:\n;;;
;;; DEPRECATED, RENAMED, OR MOVED:\n;;;
;;; REQUIRES:\n;;;
;;; TODO:\n;;;
;;; NOTES:\n;;;
;;; SNIPPETS:\n;;;
;;; THIRD PARTY CODE:\n;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY\n;;;
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
(concat ";;; <Timestamp: " (mon-timestamp :naf t)) ;(format-time-string "%A %B %d, %Y @ %I:%M.%S %p")
*mon-gnu-license-header* 
(mon-build-copyright-string nil nil t t nil t)
fname-sans
fname-sans
fname)))
(if (or insertp intrp)
    (insert fl-template)
  fl-template)))
;;
(defalias 'mon-insert-naf-mode-file-template 'mon-insert-file-template)

;;;test-me;(mon-insert-file-template)
;;;test-me;(mon-insert-file-template t)
;;;test-me;(call-interactively 'mon-insert-file-template)

;;; ==============================
;;; COURTESY: Aaron S. Hawley 
;;; (URL `http://www.emacswiki.org/emacs/AutoInsertForTexinfo')
;;; Texinfo template. Based on "Emacs Lisp Header" in auto-insert.el
;;; MODIFICATIONS: <Timestamp: #{2009-08-24T11:38:35-04:00Z}#{09351} - by MON>
;;; Turned this into a function so now without reliance on autoinsertmode.
;;; ==============================
(defun mon-insert-texi-template (title short-description top &optional intrp)
  "Insert Texinfo template in buffer at point.
TITLE, SHORT-DESCRIPTION, and TOP are per Texinfo spec.
When called-interactively prompts for TITLE, SHORT-DESCRIPTION, and TOP.
Try to DTRT when buffer is not visiting file and prompts for filename to write
buffer to before proceeding with insertion.\n
See also; `mon-file-stamp', `mon-insert-file-template', `mon-insert-gnu-licence',
`mon-insert-ebay-dbc-template',`mon-insert-CL-package-template'."
  (intractive "\i\n\i\n\i\nP")
  (let ((mail-addr (cadr (assoc 9 *DCP-NAME*))) ;WAS: `user-mail-address'
        (organization (getenv "ORGANIZATION"))
        (u-name (cadr (assoc 6 *MON-NAME*));;WAS: `user-full-name' 
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
  under the terms of the GNU Free Documentation License, Version 1.1 or
  any later version published by the Free Software Foundation; with no
  Invariant Sections, and no Cover Texts.  A copy of the license is
  included in the section entitled ``GNU Free Documentation License.''

  A copy of the license is also available from the Free Software
  Foundation Web site at @url{http://www.gnu.org/licenses/fdl.html}.

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

;;;test-me;(call-interactively 'mon-insert-texi-template)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday March 04, 2009 @ 06:16.40 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T16:03:23-0400Z - by MON KEY>
(defun mon-insert-test-cases (&optional insertp intrp)
  "Easily identified tracing vars for debugging defuns mon-*- and naf-mode-*.
Unbinds all previously bound variables:
test-aaa, test-bbb, test-ccc, test-ddd,
test-aa, test-AA, test-ag, test-AG, test-a4, test-A4\n
Rebinds vars 'default values'.  Called-interactively or INSERTP non-nil insert
test-cases at point.\n
EXAMPLE:\n\(mon-insert-test-cases\)\nSee also; `mon-insert-lisp-testme'."
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

;;;test-me;(mon-insert-test-cases t)
;;;test-me;(mon-insert-test-cases)
;;;test-me;(call-interactively 'mon-insert-test-cases)


;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-09T11:26:16-04:00Z}#{09327} - by MON KEY>
(defun mon-insert-gnu-licence (&optional insertp intrp)
  "Returns `*mon-gnu-license-header*'. 
When INSERTP non-nil or called-interactively insert GNU license at point, but
does not move point.\n
See also; `mon-insert-file-template'."
  (interactive "i\np")
  (if (or insertp intrp)
(save-excursion
  (newline)
      (insert *mon-gnu-license-header*))
    *mon-gnu-license-header*))

;;;test-me;(mon-insert-gnu-licence)
;;;test-me;(mon-insert-gnu-licence t)
;;;test-me;(call-interactively 'mon-insert-gnu-licence)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-09T11:26:24-04:00Z}#{09327} - by MON KEY>
(defvar *mon-gnu-license-header* 'nil
  "Default gnu license to insert in newly created file headers.")
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

;;;test-me; *mon-gnu-license-header*
;;;(progn (makunbound '*mon-gnu-lincense-header*)(unintern '*mon-gnu-license-header*))

;;; ==============================
(provide 'mon-insertion-utils)
;;; ==============================

;;; ==============================
;;; mon-insertion-utils.el ends here
;;; EOF

;;; mon-insertion-utils.el --- insertion utils, licences, file templates, etc.
;;; ================================================================
;;; DESCRIPTION:
;;; mon-insertion-utils.el Provides insertion related utilities, templates and
;;; string building/manipulation procedures that ease routine chores and
;;; interactive command invocation.
;;; Many larger 'texty' functions are included here because they have become to
;;; unwieldly or are otherwise to large to maintain in their original source
;;; location.
;;;
;;; FUNCTIONS:►►►
;;; `mon-insert-string-n-fancy-times', `mon-line-number-region',
;;; `mon-string-incr-padded', `mon-string-incr', `mon-line-drop-in-words',
;;; `mon-insert-string-n-times', `mon-lisp-evald', `comment-divider',
;;; `mon-comment-divider-to-col-four', `php-comment-divider', `mon-insert-copyright',
;;; `mon-insert-file-in-dirs', `mon-insert-dirs-in-path', `mon-insert-wht-spc',
;;; `mon-insert-newlines', `mon-insert-defclass-template'
;;; `mon-insert-regexp-template-yyyy'`mon-insert-regexp-template'
;;; `mon-insert-CL-file-template', `mon-insert-CL-package-template',
;;; `mon-insert-user-name-cond', `mon-insert-system-type-cond',
;;; `mon-insert-gnu-licence', `mon-insert-gnu-licence-gfdl' 
;;; `mon-build-copyright-string', `mon-comput-33', `mon-comput-45'
;;; `mon-split-designator', `mon-build-copyright-string-TEST'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS: 
;;; `mon-print-in-buffer-if-p'
;;;
;;; METHODS:
;;;
;;; CLASSES: 
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*mon-gnu-licences-header*'
;;; `*mon-gnu-license-header-gfdl*'
;;; 
;;; ALIASED/ADIVISED/SUBST'D:
;;; `mon-comment-divider->col' -> `mon-comment-divider-to-col'
;;; `bug-insert-copyright'     -> `mon-insert-copyright'
;;;
;;; RENAMED:
;;; `mon-lisp-comment-to-col'             -> `mon-comment-lisp-to-col'
;;; `split-designator'                    -> `mon-split-designator'
;;; `mon-interactively-stringify'         -> `mon-insert-string-ify'
;;; `php-comment-divider'                 -> `mon-insert-php-comment-divider'
;;; `mon-insert-naf-mode-file-template'   -> `mon-insert-file-template'
;;; `mon-insert-string-incr'              -> `mon-string-incr'
;;; `mon-insert-numbers-padded'           -> `mon-string-incr-padded'
;;;
;;; RENAMED-AND-MOVED:
;;; Following moved to `mon-doc-help-utils.el' and renamed *insert* -> *help*
;;; `mon-insert-file-dir-functions'       -> `mon-help-file-dir-functions'
;;; `mon-insert-install-info-incantation' -> `mon-help-install-info-incantation'
;;; `mon-insert-rename-incantation'       -> `mon-help-rename-incantation'
;;; `mon-insert-tar-incantation'          -> `mon-help-tar-incantation'
;;; `mon-insert-info-incantation'         -> `mon-help-info-incantation'
;;; `mon-insert-diacritics'               -> `mon-help-diacritics'
;;; `mon-insert-naf-mode-file-template'   -> `mon-insert-file-template'
;;; `mon-user-evald'                      -> `mon-lisp-evald'
;;; `mon-incr'                            -> `mon-string-incr'
;;;
;;; MOVED: 
;;; `mon-insert-user-name-cond'       <- mon-w32-load.el
;;; `mon-insert-system-type-cond'     <- mon-w32-load.el
;;; `mon-insert-regexp-template-yyyy' <- naf-mode-replacement-utils.el
;;; `mon-accessed-stamp'              -> mon-time-utils.el
;;; `mon-stamp'                       -> mon-time-utils.el
;;; `mon-timestamp'                   -> mon-time-utils.el
;;; `mon-accessed-time-stamp'         -> mon-time-utils.el
;;; `*mon-hgignore-template*'         -> mon-bzr-utils.el
;;; `mon-insert-hgignore-template'    -> mon-bzr-utils.el
;;; `mon-insert-lisp-testme-fancy'    -> mon-testme-utils.el
;;; `mon-insert-test-cases'           -> mon-testme-utils.el
;;; `mon-insert-lisp-testme'          -> mon-testme-utils.el
;;;
;;; REQUIRES:
;;; :FILE ./mon-time-utils.el  
;;; :LINK (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;;; :CALLED-BY 'anything-that-uses-a-time-stamp'
;;;
;;; :FILE ./mon-utils.el 
;;; :LINK (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;; :FUNCTION `mon-sublist-gutted' :CALLED-BY `mon-insert-gnu-licence-gfdl' 
;;;
;;; :FILE ./mon-site-local
;;; *MON-NAME*
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-insertion-utils.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-08-27} - by MON>
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
(eval-when-compile (require 'cl))

;;; ==============================
;;; :CALLED-BY Anything that uses a time-stamp.
(require 'mon-time-utils) 
;;
;;; 
;;; (unless (featurep 'mon-default-loads)
;;;   (when (locate-library "mon-default-loads")
;;;     (require 'mon-default-loads)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-20T19:38:47-05:00Z}#{09476} - by MON KEY>
(defmacro mon-print-in-buffer-if-p (print-type form)
  "When buffer is writable print FORM with PRINT-TYPE.\n
Otherwise, just return the printed value of form.\n
:SEE-ALSO .\n►►►"
  `(,print-type 
    ,form
    ,(if (and (not buffer-read-only)
              (buffer-file-name)
              (file-writable-p (buffer-file-name))
              (file-exists-p (buffer-file-name)))
         (current-buffer))))
;;
;;; :TEST-ME
;;; (let ((this (get-buffer (current-buffer))))
;;;   (unwind-protect
;;;        (progn
;;;          (set-buffer (get-buffer "*Help*"))
;;;          (mon-print-in-buffer-if-p princ (buffer-name)))
;;;     (set-buffer this)))

;;; ==============================
(defun mon-insert-dirs-in-path (dir-list dir-path)
  "Directory elts held by symbol DIR-LIST (list of strings) are inserted in DIR-PATH.\n
Does minimal error checking, shouldn't be trusted for unfamilar pathnames.\n
:SEE-ALSO .\n►►►"
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
;;; :TEST-ME (let ((t-dir-list '("abc" "def" "ghi")))                 
;;;            (mon-insert-dirs-in-path t-dir-list default-directory))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 06, 2009 @ 03:32.25 PM - by MON KEY>
(defun mon-insert-file-in-dirs (make-dir-list insert-text extension) ;&optional starting-dir
  "Each element in list MAKE-DIR-LIST inserts a directory, file, and file text.\n
A more general solution is `mon-insert-dirs-in-path'. This function is a 
generic version of `mon-insert-naf-file-in-dirs'. However, this function 
is _NOT_ appropriate for creation of `naf-mode' specific files.\n
Directory's name and file's name are taken from elt in MAKE-DIR-LIST. Directory
is created relative to current buffer's `default-directory'. File's contents are
automatically inserted according to text held in INSERT-TEXT. EXTENSION argument
provides the file extension for file.\n
When generating the 'directory-name' and 'file-name' components from the elts
of MAKE-DIR-LIST tests for the presence of:\n
   '(', ')', '_', '.', ':', ';', ' '        <- Removed and/or replaced w/ '-'.\n

Generation of file-name the EXTENSION string tests for the presence of:\n
   '(', ')', '_', '.', ':', ';', ' '   <- Remove and/or replace w/ '-' and ',_'.\n
A second pass checks EXTENSION for leading '.' and inserts it when missing.\n
Format of list for MAKE-DIR-LIST should be as follows:\n
 \(setq make-my-dirs 
   '(\"Name_Part1\" \"Name \(Part2\)\"
     \"Name, :Part3\" \"Name Part4;\"\)\)\n\n
:EXAMPLE\nAssuming buffer's default directory is \"c:/home/my-dirs\"
Invoking the form with args make-my-dirs my-insert-text \".dbc\":\n
\(mon-insert-file-in-dirs make-my-dirs my-insert-text \".dbc\")\n
Or, interactively; M-x mon-insert-naf-file-in-dirs\n
=> minibuffer-prompt: Give Symbol holind dir/file list : make-my-dirs
=> minibuffer-prompt: Give Symbol holing insert-text : my-insert-text
=> minibuffer-prompt: File extenison :dbc \n
Creates the following directorys and files in c:/home/my-dirs :
              .
              |-- Name-\(Part2\)
              |   `-- Name-Part2.dbc
              |-- Name-Part1
              |   `-- Name-Part1.dbc
              |-- Name-Part3
              |   `-- Name,_Part3.dbc
              `-- Name-Part4
                  `-- Name-Part4.dbc\n
:NOTE dir & file names with 'flagged' characters are transformed where
appropriate.\n
:SEE-ALSO .\n►►►"
  (interactive 
   "XGive Symbol holding dir/file list :\nXGive Symbol holing insert-text :\nsFile extenison :")
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
When called-interactively SPC-CNT is a prefix arg.
space is:\n- character: SPC (32, #o40, #x20);\n- code point: 0x20\n- name: SPACE\n
:NOTE does not inherit stickiness of adjoining chars `text-properties-at'.\n
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
When called-interactively NL-CNT is a prefix arg.
newline is:\n- code point: '0x0A';\n- character: C-j (10, #o12, #xa);\n- old-name: LINE FEED (LF);\n
:NOTE does not inherit stickiness of adjoining chars `text-properties-at'\n
:SEE-ALSO `mon-insert-wht-spc', `mon-insert-unicode', 
`mon-insert-string-n-times', `mon-insert-string-n-fancy-times'.\n►►►"
  (interactive "N")
  (insert-char ?\n nl-cnt nil))
;;
;;; :TEST-ME (mon-insert-newlines 3)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-12T15:30:25-05:00Z}#{10065} - by MON KEY>
;;; Cleaned up some un-needed local vars. Only insert return value when intrp insrtp.
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-insert-string-n-fancy-times (&optional intrp put-count string-to-put
                                        w-newline w-delim delim w-whtspc insrtp)
  "Insert a string n times with optional delimters and/or newlines.\n
PUT-COUNT a number indicating the times to fancify STRING-TO-PUT. Default is 1.\n
STRING-TO-PUT is the string to fancify.\n
W-NEWLINE when non-nil adds newlines for each PUT-COUNT.\n
W-DELIM when non-nil interleaves with a DELIM.\n
DELIM a string is interleaved between each STRING-TO-PUT.\n
W-WHTSPC interleaves STRING-TO-PUT when W-DELIM is nil.\n
When W-DELIM is non-nil the arg to W-WHTSPC is discarded. You should supply
DELIM with with any required whitespace to DELIM arg if this is what you want.\n
When W-NEWLINE and W-DELIM are nil and WHTSP-P is non-nil interleave STRING-TO-PUT
with whitespace.\n
When W-NEWLINE is non-nil and arg to W-WHTSPC is non-nil discard the W-WHTSPC
arg \(prevents trailing whitespace).\n
When called-interactive prompt for:
 PUT-COUNT STRING-TO-PUT W-NEWLINE W-DELIM DELIM W-WHTSPC\n
When INSERTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n\n;; Put 4 bubba's without delim, newlines, or inter-bubba whitespace.
\(mon-insert-string-n-fancy-times nil 4 \"bubba\" nil nil nil\)\n
;; Put 4 bubba's without delim, or newlines add inter-bubba whitespace.
\(mon-insert-string-n-fancy-times nil 4 \"bubba\" nil nil nil t\)\n
;; Put 4 bubba's add a newline after each bubba.
\(mon-insert-string-n-fancy-times nil 4 \"bubba\" t nil nil\)\n
;; Put 3 bubba's delimit each bubba with a \\\" ,\\\" no newlines.
\(mon-insert-string-n-fancy-times nil 3 \"bubba\" nil t \" ,\"\)\n
;; Put 3 bubba's delimit each bubba with \\\" ,\\\" no newlines.
\(mon-insert-string-n-fancy-times nil 3 \"bubba\" nil t \" | \"\)\n
;; Put 3 bubba's delimit with each bubba \" \\|\" followed by newlines.
\(mon-insert-string-n-fancy-times nil 3 \"bubba\" t t \" |\" nil t\)\n
:SEE-ALSO `mon-insert-string-n-fancy-times', `mon-string-incr',
`mon-line-number-region-incr', `mon-insert-wht-spc', `mon-insert-newlines',
`mon-insert-unicode'.\n►►►"
  (interactive "p")
  (let*   ((this-func ":FUNCTION mon-insert-string-n-fancy-times")
           (put-cnt     (cond ((and put-count (stringp put-count))
                               (string-to-number put-count))
                              (put-count put-count)
                              (intrp (read-number "How many puts?: "))
                              (t 1)))
           (put-string  (cond (string-to-put 
                               (unless (stringp string-to-put)
                                 (error "%s - Arg STRING-TO-PUT is not a string" this-func))
                               string-to-put)
                              (intrp (read-string "String to put: "))))
           (delimp      (cond (w-delim w-delim)
                              (intrp (yes-or-no-p "With delimiter?: "))))
           (nlp         (cond (w-newline w-newline)
                              (intrp (yes-or-no-p "Insert newlines?: "))))
           (delim-put 
            (let ((dt  delimp)       ;; t when delim is t.
                  (df  (not delimp)) ;; t when delim is nil.
                  (nlt nlp)          ;; t when newlines is t.
                  (nlf (not nlp))   ;; t when newlines is nil.
                  (dlm (cond ((and (not delim) (not intrp))) ;delim is nil 
                             ((and delim (stringp delim)) delim) ;delim is a string
                             ((and delim (not (stringp delim))) ;delim is in error
                              (error "%s - arg DELIM is not a string" this-func)))))
              (cond ((and nlt dt)  ;; Do newline, do delim.
                     (concat (if intrp 
                                 (read-string 
                                  "Delimit range with (add whitespace if needed): ")
                                 delim)
                             "\n"))
                    ((and dt nlf) ;; Do delim, no newline.
                     (if intrp 
                         (read-string "Delimit range with (add whitespace if needed): ")
                         delim))
                    ((and nlt df) "\n") ;; Do newline, no delim.
                    ((and df nlf) ;; No delim,  no newline. What about whitespace??
                     (if intrp 
                         (if (yes-or-no-p 
                              "No Delim. No Newline. Interleave the put string with whitespace? :")
                             " " "")
                         (if w-whtspc " " ""))))))
           rtn-put)
    (setq rtn-put (with-temp-buffer  
                  (while (> put-cnt 0)
                    (insert (format "%s%s" put-string delim-put))
                    (setq put-cnt (1- put-cnt)))
                  (buffer-substring-no-properties (buffer-end 0)(buffer-end 1))))
  (if (or insrtp intrp)
      (let ((llm-tgl (buffer-local-value longlines-mode (current-buffer))))
        (unwind-protect
             (save-excursion
               (when llm-tgl (longlines-mode 0))
               (newline)
               (insert rtn-put))
          (when llm-tgl (longlines-mode))))
      rtn-put)))
;;
;;; :TEST-ME (mon-insert-string-n-fancy-times nil 4 "bubba" nil nil nil)
;;; :TEST-ME (mon-insert-string-n-fancy-times nil 4 "bubba" nil nil nil t)
;;; :TEST-ME (mon-insert-string-n-fancy-times nil 4 "bubba" t nil nil)
;;; :TEST-ME (mon-insert-string-n-fancy-times nil 3 "bubba" nil t " ,")
;;; :TEST-ME (mon-insert-string-n-fancy-times nil 3 "bubba" nil t " | ")
;;; :TEST-ME (mon-insert-string-n-fancy-times nil 3 "bubba" t t " |" nil t)

;;; ==============================
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:41.37 PM - by MON KEY>
(defun mon-insert-string-n-times (put-count string-to-put)
  "Insert the STRING-TO-PUT PUT-COUNT times.\n
Prompt for: How many puts? <-- number of times to put string.
             String to put: <--- String  to insert.\n
:SEE-ALSO `mon-insert-string-n-fancy-times', `mon-string-incr', `mon-line-number-region-incr',
`mon-insert-wht-spc', `mon-insert-newlines', `mon-insert-unicode'.\n►►►"
  (interactive (list (read-number "How many puts?")
		     (read-string "String to put:")))
    (dotimes (i put-count t)
      (insert string-to-put)))

;;; ==============================
(defun mon-line-number-region (start end &optional begin-w)
  "Number the lines in region from START END.\n
When BEGIN-W is non-nil or called interactively with a prefix arg
numbering will start from BEGIN-W. The default value is 1.\n
Negative numbers are also supported. Numbers are right aligned followed by a
period and a space. If point is at the beginning of the region, the lines
will be numbered in descending order. If a line is already prefixed with a
number, it will be overwritten with the new value.\n
Unlike `mon-string-incr' allows prefix starting value - numeric argument.\n
:SEE-ALSO `mon-string-incr-padded', `mon-line-number-region-incr', 
`mon-rectangle-sum-column'.\n►►►"
  (interactive "*r\np")
  (let* ((lines (count-lines start end))
	 (from (or begin-w 1))
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
      (insert (format (concat "%" (number-to-string width) "d. ") n))
      (forward-line))))

;;; ==============================
;;; :COURTESY Noah Friedman :HIS buffer-fns.el
(defun mon-string-incr-padded (start-num end-num &optional padp insrtp intrp)
  "Return numbers from START-NUM to END-NUM \(inclusive\).\n
Each number is returned on a separate line.
START-NUM may be less than END-NUM, in which case counting is backward.\n
When PADP is non-nil or called-interactively with prefix arg, pad all numbers
with sufficient leading zeros so they are the same width.\n
When INSRTP is non-nil or called-interactively insert current-buffer.
Does not move point.\n
:EXAMPLE\n\(mon-string-incr-padded 88 120 1\)\n
:SEE-ALSO `mon-string-incr',`mon-line-number-region', `mon-line-number-region-incr', 
`mon-rectangle-sum-column'.\n►►►"
  (interactive "nSTART-NUM: \nnEND-NUM: \nP\ni\np")
  (let* ((add-func (if (<= start-num end-num) '1+ '1-))
         (comp-func (if (<= start-num end-num) '<= '>=))
         (i start-num)
         (fmt (and padp (format "%%.%dd"
                                (length (int-to-string (max (abs start-num)
                                                            (abs end-num)))))))
         rtn)
    (setq rtn    
          (with-temp-buffer
            (while (funcall comp-func i end-num)
              (insert (if fmt (format fmt i) (int-to-string i)) "\n")
              (setq i (funcall add-func i)))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if (or insrtp intrp)
        (save-excursion (newline) (insert rtn))
        rtn)))
;;
;;; :TEST-ME (mon-string-incr-padded 88 120 1)


;;; ==============================
;;; :CREATED <Timestamp: Tuesday February 03, 2009 @ 03:41.17 PM - by MON KEY>
(defun mon-string-incr (start-w end-w step-w &optional w-delim delimiter
                        w-newln w-wspc insertp intrp)
  "Increment a range of numbers from START-W to END-W by STEP-W.\n
START-W a number to increment from.\n
END-W a number to increment to.\n
STEP-W a number to step increments by.\n
W-DELIM is non-nil when incremented range should be delmited.\n
DELIMITER a string to delimit the range with.\n
W-NEWLN is non-nil when increment range should have newlines added.\n
W-WSPC is non-nil if incrment range should be delimited with whitespace when
args W-NEWLN and W-DELIM are ommited.\n
When called-interactively or INSERTP is non-nil toggle `longlines-mode' if
active in current-buffer and insert to current buffer the incrmented range.
Does not move point.\n
When called-interactively prompt with:\n
 \"Increment from number: \"\n \"Increment to number: \"
 \"Step by increments of: \"\n \"With delimiter?: \"\n \"Insert newlines?: \"\n
When \"With delimiter?: \" is non-nil prompt:\n\n \"Delimit range with: \"\n
When both \"With delimiter?\" and \"Insert newlines?\" are nil prompt:\n
 \"Interleave range with whitespace?: \"\n
:EXAMPLE\n\n;;  1-100 by 5
\(mon-insert-string-incr 1 100 5\)\n
;;  1-100 by 5 inteleaved with \" | \"
\(mon-insert-string-incr 1 100 5 t \" | \"\)\n
;; 1-100 by 5 inteleaved with \".\" add newline
\(mon-insert-string-incr 1 100 5 t \".\" t\)\n
;; 1-100 by 5 add whitespace
\(mon-insert-string-incr 1 100 5 nil nil nil t\)\n
;; 1-100 by 5 w/bogus args (but don't fail)
\(mon-insert-string-incr 1 100 5 t nil nil t\)\n
\(mon-insert-string-incr 1 100 5 t t nil nil\)\n
\(mon-insert-string-incr 1 100 5 nil t nil t\)\n
\(mon-insert-string-incr 1 100 5 nil nil t t\)\n
;; 1-5005 by 5 - Signal an error
\(mon-insert-string-incr 1 5005 5\)\n
:NOTE If the range specified will step over 1000 increments signal an error.\n
:SEE-ALSO `mon-string-incr-padded', `mon-line-number-region', 
`mon-line-number-region-incr', `mon-rectangle-sum-column'.\n►►►"
  (interactive `(,(read-number "Increment from number: ")  ;;start-w
                  ,(read-number "Increment to number: ")   ;; end-w
                  ,(read-number "Step by increments of: ") ;; step-w
                  ,(yes-or-no-p "With delimiter? ")        ;; w-delim
                  ,(read-string "Delimit range with: ")    ;; delimiter
                  ,(yes-or-no-p "Insert newlines? ")       ;; w-newln
                  nil                                      ;; w-wspc
                  nil                                      ;; insertp
                  t))                                      ;; intrp
  (catch 'mon-incr-big-range 
    (let* ((this-func ":FUNCTION mon-insert-string-incr")
           (start-incr (if intrp start-w 
                           (cond ((numberp start-w) start-w)
                                 ((stringp start-w) (string-to-number start-w))
                                 (t (error "%s - Arg START-W not a number" this-func)))))
           (end-incr  (if intrp end-w
                          (cond ((numberp end-w) end-w)
                                ((stringp end-w) (string-to-number end-w))
                                (t (error "%s - Arg END-W not a number" this-func)))))
           (step-value (if intrp step-w
                           (cond ((numberp step-w) step-w)
                                 ((stringp step-w) (string-to-number step-w))
                                 (t (error "%s - Arg STEP-W not a number" this-func)))))
           (range (- end-incr start-incr))
           (range-check
            (when  (> (+ (/ range step-value) (mod range step-value)) 1000)
              (if intrp (progn
                          (message (concat "Ranges with values over 1000 not fun"
                                           "for the little man in Emacs.\n"
                                           "He won't schlep that range!"))
                          (sit-for 2)
                          (throw 'mon-incr-big-range (funcall 'mon-insert-string-incr)))
                  (error "%s - Arg STEP-W steps over 1000 times" this-func))))
           (dlm (if intrp delimiter
                    (cond ((stringp delimiter) delimiter)
                          ((numberp delimiter) (number-to-string delimiter))
                          ;; ((and (characterp delimiter) (not (null delimiter) (not delimiter)))
                          ;;  (char-to-string delimiter))
                          ((booleanp delimiter)
                           (if (and delimiter w-wspc) " " "")))))
           (delim (let ((dt w-delim)            ; t when delim y
                        (df (not w-delim))      ; t when delim n
                        (nlt w-newln)           ; t when nl t
                        (nlf (not w-newln)))    ; t when nl n
                    (cond ((and nlt dt w-delim) ; do newline, do delim
                           (concat dlm "\n"))
                          ((and nlt df) "\n") ; do newline, no delim
                          ((and dt nlf w-delim (not (eq (length dlm) 0))) dlm)
                          ((and dt nlf dlm w-wspc) ;; t nil nil t
                           (if (eq (length dlm) 0) " "))
                          ((and df nlf)
                           (cond (intrp (if (yes-or-no-p 
                                             "Interleave range with whitespace? ")
                                            " "  ""))
                                 (w-wspc " ")
                                 ((null w-wspc) "")))
                          ((eq (length dlm) 0) ""))))
           the-incr)
      (setq the-incr
            (with-temp-buffer 
              (while (<= start-incr end-incr)
                (when (= start-incr end-incr) ; kick out when we reach top
                  (princ (format "%d" start-incr) (current-buffer))
                  (setq start-incr (+ step-value end-incr)))
                (when (< start-incr end-incr) ; keep going
                  (princ (format "%d%s" start-incr delim) (current-buffer)))
                (setq start-incr (+ step-value start-incr)))
              (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
      (if (or insertp intrp)
          (save-excursion 
            (unwind-protect
                 (let ((is-on (buffer-local-value longlines-mode (current-buffer)))
                       llm-off)
                   (when is-on (longlines-mode 0) (setq llm-off 't))
                   (newline) (insert the-incr)
                   (when llm-off (longlines-mode 1) (setq llm-off 'nil)))))
          the-incr))))
;;
;;;  1-100 by 5 
;;; :TEST-ME  (mon-insert-string-incr 1 100 5)
;;;  1-100 by 5 inteleaved with " | "
;;; :TEST-ME (mon-insert-string-incr 1 100 5 t " | ")
;;;  1-100 by 5 inteleaved with "." add newline
;;; :TEST-ME (mon-insert-string-incr 1 100 5 t "." t)
;;;  1-100 by 5 add whitespace
;;; :TEST-ME (mon-insert-string-incr 1 100 5 nil nil nil t)
;;;  1-100 by 5 w/bogus args (but don't fail)
;;; :TEST-ME (mon-insert-string-incr 1 100 5 t nil nil  t) 
;;; :TEST-ME (mon-insert-string-incr 1 100 5 t t nil nil) 
;;; :TEST-ME (mon-insert-string-incr 1 100 5 nil t nil t)
;;; :TEST-ME (mon-insert-string-incr 1 100 5 nil nil t t)

;;; ==============================
;;; :MODIFICATIONS-OF Drew Adams' :HIS strings.el
;;; :CREATED <Timestamp: Thursday February 19, 2009 @ 06:31.47 PM - by MON KEY>
(defun mon-line-drop-in-words (&optional buffer) 
 "Split current line of text in BUFFER into single words.\n
The split line inserted with each word on a new line.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-string-ify-list',
`mon-insert-string-ify',`mon-string-split-line', `mon-word-get-list-in-buffer'.\n►►►"
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
	  (mapc #'(lambda (arg)
                    (princ arg (current-buffer))
                    (newline))
                line-o-strings))))))

;;; =======================
;;; :RENAMED `mon-interactively-stringify' -> `mon-insert-string-ify'
(defun mon-insert-string-ify (the-string)
  "Read a string split it into tokens.\n
Return a list of strings obtained by breaking the string at space boundaries.
Minibuffer Prompt with \"Stringify This-->\" \n
:EXAMPLE\n\n\(mon-insert-string-ify \"a b c\"\)\n=>(\"a\" \"b\" \"c\"\)\).\n
:SEE-ALSO `mon-string-ify-list'.\n►►►"
  (interactive "sStringify This-->")
  (print (mon-string-ify-list the-string) (current-buffer)))
;;
;;; :TEST-ME (mon-insert-string-ify "a b c")

;;; ==============================
;;; :NOTE Now inlined with the let in `unicode-insert'.
;;; :MODIFICATIONS <Timestamp: #{2009-11-27T15:17:34-05:00Z}#{09485} - by MON KEY>
(defun mon-insert-unicode (char &optional insertp intrp)
  "Read a Anicode code point for CHAR and insert CHAR at point.\n
Called interactively prompts 2 options:
i) Read char in radix 16 as with `read-quoted-char' e.g.:
   code point: 0x2019 can be passed as 2019
ii) Read char as with `read-char-by-name'
    by its Unicode name, octal, hex, or decimal value e.g.
    Code point can be passed as:
    HEAVY BLACK HEART <- name 
    #o21430           <- octal
    #x2318            <- hex
    #10r8984          <- decimal\n
:EXAMPLE\n(mon-insert-unicode \"25BA\")\n
\(call-interactively 'mon-insert-unicode\)\n
option 1 radix 16 w/ arg 2019  => (\"’\" nil t)
option 2 w/ arg #o20031 => (\"’\" nil t)
option 2 w/ arg #x2019 => (\"’\" nil t)
option 2 w/ arg #10r8217 => (\"’\" nil t)
option 2 w/ arg RIGHT SINGLE QUOTATION MARK => (\"’\" nil t)\n
:NOTE When called-interactively and option 1 is selected invokes
`read-quoted-char-radix' bound to 16. This allows easy coping of values
from Unicode charts :SEE (URL `http://www.decodeunicode.org/u+2019').\n
For access/alteration to encoding/coding information
:SEE `encode-coding-region', `describe-coding-system', `unicode-insert'.
:SEE-ALSO .\n►►►"
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
                                                  ((or (< the-char 0) (> the-char #x10FFFF))
                                                   (error "Not a Unicode character code: 0x%X" the-char)))
                                            (char-to-string the-char)))
                                         )))
                         `(,char-rd nil t))))
  (let (t-char)
    (setq t-char
          (cond (intrp char)
                ((integerp char) char)
                ;; (cdr (assoc-string "BLACK RIGHT-POINTING POINTER" (ucs-names) t))
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
                       ;; (t (setq t-char (char-to-string t-char))))
                       ))))
    (setq t-char (cond (intrp char)
                       (t (char-to-string t-char))))
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
;;; :TEST-ME (mon-insert-unicode "#o22672" t)►
;;; :TEST-ME (mon-insert-unicode #o22672 t)►
;;; :TEST-ME (mon-insert-unicode "#x25ba" t)►
;;; :TEST-ME (mon-insert-unicode 9658 t)►
;;; :TEST-ME (mon-insert-unicode #x25ba t)►
;;; :TEST-ME (mon-insert-unicode "BLACK RIGHT-POINTING POINTER" t)►
;;; :TEST-ME (mon-insert-unicode "#10r10084" t)❤
;;; :TEST-ME (mon-insert-unicode "#x2764" t)❤
;;; :TEST-ME (call-interactively 'mon-insert-unicode)
;;; :TEST-ME (mon-insert-unicode "bubba")
;;; :TEST-ME (mon-insert-unicode "bubba" t)

;;; ==============================
;;; :RENAMED :FROM `split-designator' :TO `mon-split-designator'
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T13:57:43-04:00Z}#{09352} - by MON KEY>
(defun mon-split-designator (&optional insrtp intrp)
  "Return string \"---\\n\".\n
When INSERTP is non-nil or called-interactively inserts at point. 
Does not move point.\n\n:EXAMPLE\n\n(split-designator\)\n\n
:SEE-ALSO `non-posting-source', `non-posting-ebay-source', `naf-comment-prefix'.
`non-posting-wiki-source', `npps', `benezit-naf-template'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "i\np")
  (if (or insrtp intrp)
      (save-excursion
      (insert "---\n"))
    "---\n"))
;;
;;
;;; :TEST-ME (mon-split-designator)
;;; :TEST-ME (call-interactively 'mon-split-designator)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T14:09:37-04:00Z}#{09352} - by MON KEY>
(defun comment-divider (&optional not-insert intrp)
  "Insert default comment divider at point.
When called-interactively insert the following at point:\n
;;; ==============================\n
When NOT-INSERT is non-nil return comment divider as string.\n
:EXAMPLE\n\(comment-divider t\)\n
:SEE-ALSO `*mon-default-comment-divider*' `mon-comment-divide->col',
`mon-comment-lisp-to-col' `mon-insert-php-comment-divider',
`mon-insert-lisp-stamp'.\n►►►"
  (interactive "i\np")
  (if (or (not not-insert) intrp)
      (insert *mon-default-comment-divider*)
      *mon-default-comment-divider*))
;;
;;; :TEST-ME (comment-divider t)
;;; :TEST-ME (comment-divider)
;;; :TEST-ME (call-interactively 'comment-divider)

;;; ==============================
;; :TODO In lieu of the refactoring of commint-divider functions 
;;; E.g. `*mon-default-comment-start*', `mon-comment-divider-w-len' 
;;; `*mon-default-comment-divider*', etc. some of this funcs subrs can be
;;; formulated differently and/or ommitted. For example, the local var `dvdr'
;;; can now rebind temporarily `*mon-default-comment-start*' i.e.:
;;; (let ((*mon-default-comment-start* "%% "))
;;;             (mon-comment-divider-w-len 30))         
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T14:25:06-04:00Z}#{09436} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-25T19:03:44-04:00Z}#{09352} - by MON KEY>
(defun mon-comment-divider-to-col (to-col &optional start end op-prefix insertp intrp)
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
:SEE `mon-comment-lisp-to-col' for a specifically lisp centric interactive
implementation.\n
:SEE-ALSO `*mon-default-comment-divider*', `mon-line-strings-indent-to-col',
`mon-line-indent-from-to-col', `mon-line-strings-pipe-to-col',
`mon-string-fill-to-col'.\n►►►"
  (interactive "i\nr\nP\ni\np")
  (let ((strt-frm   (make-marker))
        (s-ident-at (make-marker))
        (e-ident-at (make-marker))
        (prfx      
         (cond ((and intrp op-prefix)
                (concat 
                 (make-string 2 (string-to-char (substring (read-string "Use alternative prefix: ") 0 1))) 
                 " "))
               ((and op-prefix (not intrp))
                (concat (make-string 2 (string-to-char (substring op-prefix 0 1))) " "))
               (t ";; ")))
        (2col (cond ((and intrp (or (not to-col) (not (numberp to-col))))
                     (read-number "Indent to column number: "))
                    ((and (not intrp) (not (numberp to-col)))
                     (error "Arg to-col must be a number, but got: %s" to-col))
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
                        (dvdr (concat 
                               (substring prfx 0 1) 
                               prfx 
                               ;; :NOTE This was brittle.
                               ;; Would break if user has prfx other than ";;; ".
                               ;; Two new variables created which help lets us get around that.
                               ;; `*mon-default-comment-start*', `*mon-default-comment-divider*'
                               (substring *mon-default-comment-divider* (length *mon-default-comment-start*))))
                                                 
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
(defalias 'mon-comment-divider->col 'mon-comment-divider-to-col)


;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-12-10T14:20:52-05:00Z}#{09504} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-25T18:17:18-04:00Z}#{09352} - by MON KEY>
(defun mon-comment-lisp-to-col (&optional col-n)
  "Insert `comment-divider' at COL-N comment and indent region/next line to col.
COL-N is a prefix arg. When region is active indent and comment content of region 
to COL-N else indent and comment next line. Comment prefix is `;; '.
To provide an alternative comment prefix use `mon-comment-divider-to-col'.\n
:SEE-ALSO `comment-divider', `*mon-default-comment-divider*'.
`mon-line-strings-indent-to-col', `mon-line-indent-from-to-col', 
`mon-line-strings-pipe-to-col', `mon-string-fill-to-col'.\n►►►"
  (interactive "P")
  (if (not (region-active-p))
      (progn
        (line-move -1)
        (mon-comment-divider-to-col col-n nil nil nil nil t))
      (mon-comment-divider-to-col col-n nil nil nil nil t)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T19:17:39-04:00Z}#{09352} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-25T14:48:38-04:00Z}#{09352} - by MON KEY>
(defun mon-comment-divider-to-col-four () 
  ":DEPRECATED This function has been replaced by `mon-comment-divider-to-col'.\n
Insert `comment-divider' indentented to column 4(four).
Move contents current line forward 1(one) line indent it to column 4(four).\n
:SEE-ALSO `*mon-default-comment-divider*'\n►►►"
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
(defun mon-insert-php-comment-divider (&optional insrtp intrp)
  "Insert a PHP(C style) comment divider at point.\n
Insert at point:\n
 //***************************//\n
:SEE-ALSO `comment-divider', `mon-comment-divider-to-col-four'.\n►►►"
  (interactive "i\np")
  (if (or insrtp intrp)
      (insert "//***************************//")
      "//***************************//"))
;;
;;; :TEST-ME (call-interactively 'mon-insert-php-comment-divider)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T13:24:27-0400Z - by MON KEY>
(defun mon-insert-user-name-cond (&optional insertp intrp)
  "Insert a cond template to evaluate user type of current system.\n
Test for `IS-BUG-P' or `IS-MON-P'.\n
:EXAMPLE\n(mon-insert-user-name-cond)\n
:SEE-ALSO `mon-user-name-conditionals', `mon-system-type-conditionals'.\n►►►"
  (interactive "i\np")
  (let ((unc (concat
              "\n(cond\n"
              " ((equal user-real-login-name \""
              (cadr (assoc 6 *BUG-NAME*))"\") ...do-something-here)\n"
              " ((equal user-real-login-name \""
              (cadr (assoc 5 *MON-NAME*)) "\") ...do-something-here))\n")))
    (if (or insertp intrp)
        (save-excursion (insert unc))
      unc)))
;;
;;; :TEST-ME (mon-insert-user-name-cond)
;;; :TEST-ME (mon-insert-user-name-cond t)
;;; :TEST-ME (call-interactively 'mon-insert-user-name-cond)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T13:24:13-0400Z - by MON KEY>
(defun mon-insert-system-type-cond (&optional insertp intrp)
  "Insert a cond template to test for system type OS type. Curenlty tests for
GNU/Linux and windows-nt only.\n
:EXAMPLE\n\(mon-insert-system-type-cond\)\n
:SEE `IS-W32-P' and `IS-GNU-P' for constants that return t or nil.
:SEE-ALSO `mon-user-name-conditionals', `mon-system-type-conditionals'.\n►►►"
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
  "Return template for quickly writing regexp routine.
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
;;; :MODIFICATIONS <Timestamp: #{2010-02-01T16:42:46-05:00Z}#{10051} - by MON KEY>r
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T16:11:16-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:15.02 PM - by MON KEY>
(defun mon-insert-lisp-CL-file-template (&optional insrtp intrp)
  "Return a file header template for use with Common Lisp.\n
When optional arg INSRTP is non-nil or called-interactively insert template at
top of file.\n
:EXAMPLE\(mon-insert-lisp-CL-file-template\)\n
:SEE-ALSO `mon-insert-lisp-stamp', `mon-insert-lisp-testme',
`mon-insert-lisp-evald',`mon-insert-copyright', `comment-divider',
`mon-comment-divider-to-col-four', `mon-stamp', `mon-file-stamp'.\n►►►" 
  (interactive "i\np")
  (let* ((modeline ";;-*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-")
         (fname (if (not (buffer-file-name)) 
                        (buffer-name)
                      (file-name-nondirectory (buffer-file-name))))
         (timestring (concat ";;; <Timestamp: " (mon-timestamp :naf t)))
         (copyright (mon-build-copyright-string nil nil t))
         (cl-template (concat
                       modeline "\n"
                       (mon-comment-divider-w-len 54) "\n"
                       ";;; This is " fname "\n"
                       ";;; \n"
                       ";;; FILE-CREATED:\n;;; <Timestamp: " (mon-timestamp :naf t) "\n"
                       ;; (mon-comment-divider-w-len 64) "\n"
                       ;; :WAS *mon-gnu-license-header* "\n"
                       (mon-build-copyright-string-license 'bsd) "\n"
                       (mon-insert-gnu-licence-gfdl t) "\n"
                       ";;; CODE:\n\n"
                       ";;; \n"
                       ";;; {...}\n"
                       ";;;\n" 
                       (mon-file-stamp-vrfy-put-eof  nil))))
         (when (or insrtp intrp)
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
;;; :SEE (URL `http://www.xach.com/lisp/scratch-lisp-file.el')
;;; :MODIFICATIONS <Timestamp: Tuesday July 14, 2009 @ 02:12.25 AM - by MON KEY>
;;; :CHANGED The `buffer-file-name' to check if we have a name.
(defun mon-insert-CL-package-template (&optional insrtp without-header intrp)
  "Return or insert a CL package-template a template.\n
Builds template with `DEFPACKAGE' and `IN-PACKAGE' forms for the current buffer.
When called-interactively or INSRTP non-nil assumes current buffer is empty and
insert a file template with `mon-insert-lisp-CL-file-template'.\n
:EXAMPLE\n(mon-insert-CL-package-template)\n
:SEE-ALSO `mon-insert-file-template'\n►►►"
  (interactive "i\nP\np")
  (let* ((file (if (not (buffer-file-name)) 
                   (buffer-name)
                 (file-name-nondirectory (buffer-file-name))))
         (package (file-name-sans-extension file))
         (package-template (concat ";;;; " file "\n"
                                   "\n(defpackage #:" package "\n  (:use #:cl))\n\n"
                                   "(in-package #:" package ")\n\n")))
    (when (or insrtp intrp)
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
;;; :CREATED <Timestamp: #{2009-10-24T18:33:41-04:00Z}#{09436} - by MON>
(defun mon-insert-lisp-doc-eg-xref (&optional insrtp intrp as-kill)
  "Return documentation keywords for insertion to dostrings.
Default is to return with prin1.\n
When INTRP is non-nil or called-interactively return as with princ.\n
When INSERTP is non-nil return for as with prin1.\n
When AS-KILL is non-nil or called-interactively with prefix arg also put return
value on the kill-ring.\n
:EXAMPLE\n\(mon-insert-lisp-doc-eg-xref\)\n
 |=> :EXAMPLE\\\\n
 |   :SEE-ALSO .\\\\n◄◄◄\\\" 
                  ;^^^ Here the cookie is reversed! 
                  ;    Actual value is returned per the cookie below.\n\n►►►"
  (interactive "i\np\nP")
  (cond (intrp
         (save-excursion 
           (newline)
           (princ (concat ":EXAMPLE\\n"  "\n" ":SEE-ALSO .\\n►►►\"") (current-buffer))
           (when as-kill (kill-new (concat ":EXAMPLE\\n"  "\n" ":SEE-ALSO .\\n►►►\"")))))
        (insrtp
         (prin1 (concat ":EXAMPLE\\n"  "\n" ":SEE-ALSO .\\n►►►\"") (current-buffer))
         (when as-kill (kill-new (concat ":EXAMPLE\\n"  "\n" ":SEE-ALSO \\n►►►\""))))
        (t (when as-kill (kill-new (concat ":EXAMPLE\\n"  "\n" ":SEE-ALSO .\\n►►►\"")))
           ":EXAMPLE\\n\n:SEE-ALSO \\n►►►\"")))
;;
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref)
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref nil nil t)
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref t)
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref nil t)

;;; ==============================
;;; :CREATED <Timestamp: Friday June 12, 2009 @ 12:24.26 PM - by MON KEY>
(defun mon-insert-lisp-stamp (&optional insertp intrp modifications)
  "Return or insert at point a `comment-divider' newline and `mon-stamp'.\n
When INSERTP is non-nil or called interactively insert at point.\n
When MODIFICATIONS is non-nil or called interactively with prefix arg
Prepend return value with ';;; :MODIFICATIONS ' prefix.
The default is to return withe only ';;; :CREATED '\n
MON uses to delimit and date newly created/modified procedures.\n
:EXAMPLE\n\n\(mon-insert-lisp-stamp\)\n\n\(mon-insert-lisp-stamp nil nil t\)\n
:NOTE MON limits use of the `;;; :MODIFICATIONS' prefix to situations where a
code change may be breaking or otherwise alters the semantics of the procedure.\n
:SEE-ALSO `mon-lisp-stamp',`mon-file-stamp', `mon-insert-copyright',
`mon-insert-lisp-testme', `mon-insert-lisp-CL-file-template',
`comment-divider', `mon-comment-divider-to-col-four', `mon-insert-lisp-evald'.\n►►►"
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
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T13:05:47-04:00Z}#{09436} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T16:04:36-04:00Z}#{09411} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:44:23-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:41.53 PM - by MON KEY>
(defun mon-build-copyright-string (&optional insertp intrp monkey no-nl w-org short-form)
  "Return a copyright string built conditionally on users name.\n
When INSERTP is non-nil or called interactively insert copyright at point.
On MONish systems when MONKEY is non-nil or called-interactively with Prefix arg
use longform. Default is to abbreviate MONish names.\n
When NO-NL is non-nil do not return with newlines.
Default is with newline concatenated to head and tail of return string.\n
When W-ORG is non-nil return with an institutional C/O string appended.\n
When short form is non-nil do not pad the comment delim to col 68.\n
Construct Comment delims `mon-comment-divider-w-len' padded to 34, 44, or 54
chars depending value of arg MONKEY or W-ORG.\n
:EXAMPLE\n\(mon-build-copyright-string-TEST\)\n
:SEE-ALSO `mon-insert-copyright'.\n►►►"
  (interactive "i\np\nP") 
  (let ((name (cond (monkey 
                     (cond (IS-MON-P (cadr (assoc 6 *MON-NAME*)))
                           (IS-BUG-P (cadr (assoc 5 *BUG-NAME*)))))
                       
                    (IS-MON-P (cadr (assoc 7 *MON-NAME*)))
                    (IS-BUG-P (cadr (assoc 1 *BUG-NAME*)))
                    (IS-BUG-P-REMOTE (cadr (assoc 1 *BUG-NAME*)))
                    (t "<NAME>")))
        (w-dcp (cond (monkey 
                      (if w-org (substring (cadr (assoc 1 *MON-ORG-NAME*)) 1) ""))
                     (w-org (cadr (assoc 2 *MON-ORG-NAME*)))
                     (t "")))
        (year (mon-get-current-year))   ;(format-time-string "%Y"))
        (cls-comment (cond ((and short-form (not monkey) (not w-org))
                            *mon-default-comment-divider*) ;; 34
                           ((and short-form monkey (not w-org))
                            *mon-default-comment-divider*) ;; 34
                           ((and short-form (not monkey) w-org)
                            (mon-comment-divider-w-len 40)) ;; 44
                           ((and short-form monkey w-org) 
                            (mon-comment-divider-w-len 50)) ;; 54
                           (t (mon-comment-divider-w-len 64)))) ;; 68
        (cpy))
    (setq cpy
          (if no-nl
              (concat
               cls-comment "\n"
               ";; Copyright © " year " " name " "  w-dcp "\n"
               cls-comment)
              (concat
               "\n" cls-comment "\n"
               ";; Copyright © "  year " " name " " w-dcp "\n"
               cls-comment "\n")))
    (when (or insertp intrp) 
      (save-excursion (insert cpy)))
    cpy))
;;
;;; &OPTIONAL insertp intrp monkey no-nl w-org short-form
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
;;; :TEST-ME (let ((IS-MON-P nil) (IS-BUG-P t)) (mon-build-copyright-string nil nil t))
;;; :TEST-ME (call-interactively 'mon-build-copyright-string)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-10T12:24:33-05:00Z}#{10063} - by MON KEY>
(defun mon-build-copyright-string-TEST ()
  "Test function for `mon-build-copyright-string'.\n
:SEE-ALSO .\n►►►"    
  (interactive)
  (let* ((mbcst (get-buffer-create "*COPYRIGHT-STRING-TEST*"))
         (mbcs-args (format "%s\n" (help-function-arglist 'mon-build-copyright-string)))
           (formt-tests `(,(mapconcat #'identity
                                      (save-match-data 
                                        (split-string
                                         (concat 
                                          "| Arglist for `mon-build-copyright-string':\n"
                                          mbcs-args "\n\n"
                                          "Documentation:\n\n"
                                          (documentation 'mon-build-copyright-string)
                                           ) "\n"))
                                       "\n| ")
                           ;; (&optional insertp intrp monkey no-nl w-org short-form)
                        (":DEFAULT" 
                         . ,(mon-build-copyright-string))
                       (":WITH MONKEY" 
                        . ,(mon-build-copyright-string nil nil t))
                       (":WITH MONKEY :WITH SHORT-FORM" 
                        . ,(mon-build-copyright-string nil nil t nil nil t))
                       (":WITH MONKEY :WITH NO-NL" 
                        . ,(mon-build-copyright-string nil nil t t))
                       (":WITH MONKEY :WITH W-ORG" 
                        . ,(mon-build-copyright-string nil nil t nil t ))
                       (":WITH MONKEY :WITH W-ORG :WITH SHORT-FORM" 
                        . ,(mon-build-copyright-string nil nil t nil t t))
                       (":WITH MONKEY :WITH NO-NL :WITH SHORT-FORM"  
                        . ,(mon-build-copyright-string nil nil t t nil t))
                       (":WITH SHORT-FORM" 
                        . ,(mon-build-copyright-string nil nil nil nil nil t))
                       (":WITH MONKEY :WITH NO-NL :WITH W-ORG :WITH SHORT-FORM" 
                        . ,(mon-build-copyright-string nil nil t t t t))
                       (":WITH NO-NL :WITH W-ORG :WITH SHORT-FORM" 
                        . ,(mon-build-copyright-string nil nil nil t t t))))
        (divd (make-string 67 95)))
        ;;formt-tests)
    (with-current-buffer mbcst
      (erase-buffer)
      (princ (concat (pop formt-tests) "\n") (current-buffer))
      (princ (concat 
              (mapconcat #'(lambda (e) 
                            (concat " " divd "\n|\n| " mbcs-args "| " (car e) "\n| =>\n" (cdr e)))
                         formt-tests "\n")
              "\n" divd)
             (current-buffer)))
    (display-buffer mbcst t)))
;;
;;; :TEST-ME (mon-build-copyright-string-TEST)

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:35.37 PM - by MON KEY>
;;; :DEPRECATED :USE (mon-build-copyright-string t)
(defun mon-insert-copyright (&optional monkey w-short-form insrtp intrp)
  "Insert copyright with relevant details.\n
Conditional on user's system name.
When MONKEY is non-nil use longform of MONish username.
Default is abbreviated nameform.
When W-SHORT-FORM is non-nil insert with 34 char length comment divider.
Default is to return with 68 char length comment dividers.\n
:ALIASED-BY `bug-insert-copyright'\n
:SEE-ALSO `mon-build-copyright-string'.\n:USED-IN `naf-mode'.\n►►►" 
  (interactive "p\nP\ni\np")
(if (or insrtp intrp)
    (save-excursion 
      (mon-build-copyright-string t nil monkey nil nil w-short-form))
    (mon-build-copyright-string nil nil monkey nil nil w-short-form)))
;;
(defalias 'bug-insert-copyright 'mon-insert-copyright
  "Insert a copyright string with relevant details.\n
Conditional upon `IS-BUG-P' returning t.\n
:ALIAS-OF `mon-insert-copyright'\n
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
;;; :NOTE Strip or reformat with regexps on these commonly employed 'TAGS':
;;; TAGS-APPEARING-IN-COMMENTS:
;;;  :CLEANUP :CLOSE :COURTESY :CREATED :DATE :EMACS-WIKI :EVAL-BELOW-TO-TEST
;;;  :FIXES :FIXME :HIS :IF-NOT-FEATURE-P :KEYWORD-REGEXPS-IN
;;;  :LOAD-SPECIFIC-PROCEDURES :MODIFICATIONS :RENAMED :SEE-BELOW :SUBJECT :TODO
;;;  :TEST-ME :UNCOMMENT-BELOW-TO-TEST :VERSION :WAS
;;; TAGS-APPEARING-IN-DOCSTRINGS:
;;;  :ALIASED-BY :CALLED-BY :EXAMPLE :FACE-DEFINED-IN :FACE-DOCUMENTED-IN
;;;  :FILE :IDIOM :KEYWORD-REGEXPS-IN :NOTE :SEE :SEE-ALSO :SOURCE :USED-BY
;;;
;;; :MODIFICATIONS <Timestamp: #{2010-02-01T16:33:23-05:00Z}#{10051} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T13:49:28-04:00Z}#{09436} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-02T16:06:03-04:00Z}#{09405} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T15:53:51-04:00Z}#{09411} - by MON KEY>
;;; :CREATED <Timestamp: Thursday April 09, 2009 @ 05:52.10 PM - by MON KEY>
;;; :RENAMED `mon-insert-naf-mode-file-template' ->`mon-insert-file-template'
(defun mon-insert-file-template (&optional with-fname insertp intrp)
  "Insert an elisp file template.\n
Template includes GPLv3+ clause from `*mon-gnu-license-header*'
GFDLv1.3 clause w/ Copyright <YYYY> <NAME> from: `*mon-gnu-license-header-gfdl*'\n
:EXAMPLE\n(mon-insert-file-template)\n
:NOTE Ideally, filenames should be 13 characters or less (extension
inclusive). This allows compiled filename lengths of 14 characters or less, this
helps ensure multi-os portability.\n
:SEE-ALSO `mon-insert-texi-template', `mon-insert-lisp-CL-file-template',
`mon-file-stamp', `mon-insert-gnu-licence' `mon-build-copyright-string-license'
`mon-build-copyright-string', `mon-insert-copyright',
`*mon-bsd-license-header*', `*mon-mit-license-header*'.\n►►►"
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
          (concat
           ";;; " fname " --- <one-line description of <PKG-NAME>.\n"
           ";; -*- mode: EMACS-LISP; -*-\n"
           (mon-comment-divider-w-len 64)"\n"
           ";;; DESCRIPTION:\n"
           ";;; " fname-sans " provides {description here}.\n;;;\n"
           ";;; AUTHOR: MON KEY\n"
           ";;; MAINTAINER: MON KEY\n;;;\n"
           ";;; FUNCTIONS:►►►\n;;;\n;;; FUNCTIONS:◄◄◄\n;;;\n"
           ";;; MACROS:\n;;;\n"
           ";;; METHODS:\n;;;\n"
           ";;; CLASSES:\n;;;\n"
           ";;; CONSTANTS:\n;;;\n"
           ";;; VARIABLES:\n;;;\n"
           ";;; ALIASED/ADVISED/SUBST'D:\n;;;\n"
           ";;; DEPRECATED:\n;;;\n"
           ";;; RENAMED:\n;;;\n"
           ";;; MOVED:\n;;;\n"
           ";;; TODO:\n;;;\n"
           ";;; NOTES:\n;;;\n"
           ";;; SNIPPETS:\n;;;\n"
           ";;; REQUIRES:\n;;;\n"
           ";;; THIRD-PARTY-CODE:\n;;;\n" 
           ";; URL: http://www.emacswiki.org/emacs/" fname "\n"
           ";;; FIRST-PUBLISHED:\n;;;\n"
           ";; EMACSWIKI: {URL of an EmacsWiki describing" fname-sans ".}\n"
           ";; CREATED:\n;;; <Timestamp: " (mon-timestamp :naf t) "\n"
           (mon-comment-divider-w-len 64) "\n"
           ;; :WAS *mon-gnu-license-header* "\n"
           (mon-build-copyright-string-license 'gpl) "\n"
           (mon-insert-gnu-licence-gfdl t) "\n"
           ";;; CODE:\n\n"
           "\(eval-when-compile \(require 'cl\)\)\n\n"
           *mon-default-comment-divider*
           "\n;   {...\n;   " fname-sans " Contents here\n;   ...}\n\n"
           *mon-default-comment-divider* "\n" 
           ";;; (provide '" fname-sans ")\n"
           *mon-default-comment-divider* "\n\n"
           (mon-comment-divider-w-len 64) "\n"
           ";;; " fname " ends here\n;;; EOF")))
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
  "Default GNU license to insert in newly created file headers.\n
Presented with the GPLv3+ clause.\n
:EXAMPLE\n*mon-gnu-license-header*\n
:SEE-ALSO `*mon-mit-license-header*',`*mon-bsd-license-header*'
`mon-build-copyright-string-license' `mon-insert-file-template',
`mon-insert-gnu-licence', `*mon-gnu-license-header-gfdl*'
,`mon-insert-gnu-licence-gfdl'.\n►►►")
;;
(unless (bound-and-true-p *mon-gnu-license-header*)
  (setq *mon-gnu-license-header*
        '(;;""
          "\n\n;; This file is not part of GNU Emacs.\n"
          ;;""
          "This program is free software; you can redistribute it and/or"
          "modify it under the terms of the GNU General Public License as"
          "published by the Free Software Foundation; either version 3, or"
          "(at your option) any later version.\n"
          ;;""
          "This program is distributed in the hope that it will be useful,"
          "but WITHOUT ANY WARRANTY; without even the implied warranty of"
          "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
          "General Public License for more details.\n"
          ;;""
          "You should have received a copy of the GNU General Public License"
          "along with this program; see the file COPYING.  If not, write to"
          "the Free Software Foundation, Inc., 51 Franklin Street, Fifth"
          "Floor, Boston, MA 02110-1301, USA.")))
;;
;;; :TEST-ME *mon-gnu-license-header*
;;; (progn (makunbound '*mon-gnu-license-header*)
;;;        (unintern '*mon-gnu-license-header*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-01T13:34:04-05:00Z}#{10051} - by MON KEY>
(defvar *mon-mit-license-header* nil
  "Default MIT license aka x11 License for insertion in newly created file headers.\n
:EXAMPLE\n*mon-mit-license-header*\n
:CALLED-BY .\n
:SEE-ALSO `*mon-gnu-license-header*',`*mon-bsd-license-header*',
`mon-build-copyright-string-license' `mon-insert-file-template',
`mon-insert-gnu-licence',`mon-insert-gnu-licence-gfdl',
`*mon-gnu-license-header-gfdl*'.\n►►►")
;;
(unless (bound-and-true-p *mon-min-license-header*)
  (setq *mon-mit-license-header*
        '(;;""
          "\n\n;; Permission is hereby granted, free of charge, to any person"
          "obtaining a copy of this software and associated documentation"
          "files (the ``Software''), to deal in the Software without"
          "restriction, including without limitation the rights to use,"
          "copy, modify, merge, publish, distribute, sublicense, and/or sell"
          "copies of the Software, and to permit persons to whom the"
          "Software is furnished to do so, subject to the following"
          "conditions:\n"
          ;;""
          "The above copyright notice and this permission notice shall be"
          "included in all copies or substantial portions of the Software.\n"
          ;;""
          "THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,"
          "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES"
          "OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND"
          "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT"
          "HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,"
          "WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING"
          "FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR"
          "OTHER DEALINGS IN THE SOFTWARE.")))
;;
;;; (progn (makunbound '*mon-mit-license-header*)
;;;        (unintern '*mon-mit-license-header*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-01T13:36:19-05:00Z}#{10051} - by MON KEY>
(defvar *mon-bsd-license-header* nil
  "Default BSD sytle license to insert in newly created file headers.\n
The Simplified two clause BSD License aka the FreeBSD License without
advertising clause three.\n
:EXAMPLE\n*mon-bsd-license-header*\n
:SEE-ALSO `*mon-gnu-license-header*',`*mon-mit-license-header*',
`*mon-gnu-license-header-gfdl*', `mon-build-copyright-string-license',
`mon-insert-file-template', `mon-insert-gnu-licence',
`mon-insert-gnu-licence-gfdl'.\n►►►")
;;
(unless (bound-and-true-p *mon-bsd-license-header*)
  (setq *mon-bsd-license-header*        
        '(;;""
          "\n\n;; Redistribution and use in source and binary forms, with or without"
          "modification, are permitted provided that the following conditions"
          "are met:\n"
          ;;""
          "   1. Redistributions of source code must retain the above copyright"
          "      notice, this list of conditions and the following disclaimer.\n"
          ;;""
          "   2. Redistributions in binary form must reproduce the above"
          "      copyright notice, this list of conditions and the following"
          "      disclaimer in the documentation and/or other materials"
          "      provided with the distribution.\n"
          ;;""
          "THIS SOFTWARE IS PROVIDED BY <COPYRIGHT HOLDER> ``AS IS'' AND ANY"
          "EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE"
          "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR"
          "PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> OR"
          "CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,"
          "SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT"
          "LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF"
          "USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND"
          "ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,"
          "OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT"
          "OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF"
          "SUCH DAMAGE.\n"
          ;;""
          "The views and conclusions contained in the software and"
          "documentation are those of the authors and should not be interpreted"
          "as representing official policies, either expressed or implied, of"
          "<COPYRIGHT HOLDER>.")))
;;
;;; (progn (makunbound '*mon-bsd-license-header*)
;;;        (unintern '*mon-bsd-license-header*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-02T12:01:00-04:00Z}#{09405} - by MON KEY>
(defvar *mon-gnu-license-header-gfdl* nil
  "Insert default GNU Free Documentation license in newly created file headers.
Insertion provides GFDL clause.\n
:EXAMPLE\n*mon-gnu-license-header-gfdl*\n
:CALLED-BY `mon-insert-file-template',`mon-insert-gnu-licence-gfdl'\n.
:SEE `*mon-gnu-license-header*', `mon-insert-gnu-licence' for GPLv3+ clause.
:SEE-ALSO `*mon-gnu-license-header*',`*mon-mit-license-header*',
`*mon-bsd-license-header*', `mon-build-copyright-string-license',
`mon-insert-gnu-licence'.\n►►►")
;;
(unless (bound-and-true-p *mon-gnu-license-header-gfdl*)
  (setq *mon-gnu-license-header-gfdl*
        '(;; :NOTE Don't remove semi-colons on line1 e.g. ";;; Permission"
          ";;; Permission is granted to copy, distribute and/or modify this"
          "document under the terms of the GNU Free Documentation License,"
          "Version 1.3 or any later version published by the Free Software"
          "Foundation; with no Invariant Sections, no Front-Cover Texts,"
          "and no Back-Cover Texts. A copy of the license is included in"
          "the section entitled ``GNU Free Documentation License''."
          ""
          "A copy of the license is also available from the Free Software"
          "Foundation Web site at:"
          "(URL `http://www.gnu.org/licenses/fdl-1.3.txt').")))
;;
;;; :TEST-ME *mon-gnu-license-header-gfdl*
;;; (progn (makunbound '*mon-gnu-lincense-header-gfdl*)
;;;        (unintern '*mon-gnu-license-header-gfdl*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-01T14:38:38-05:00Z}#{10051} - by MON KEY>
(defun mon-build-copyright-string-license (&optional mit-bsd-gfdl)
  "Return a copyright license string of type GPL, MIT, BSD, or GFDL.
When optional arg GPL, MIT, BSD, or GFDL \(quoted symbol\) is non-nil generate that
license. In no license is specified use the default arg GPL with `GNU-GPLv3'.\n
:EXAMPLE\n\n\(mon-build-copyright-string-license\)
\(mon-build-copyright-string-license t\)
\(mon-build-copyright-string-license nil t\)
\(mon-build-copyright-string-license nil nil t\)\n
License are mapped from the list of strings in:
:VARIALBE `*mon-mit-license-header*'
:VARIALBE `*mon-bsd-license-header*'
:VARIALBE `*mon-gnu-license-header-gfdl*'\n
:VARIALBE `*mon-gnu-license-header*'\n
:SEE-ALSO `*mon-gnu-license-header*',`*mon-bsd-license-header*',
`mon-insert-file-template', `mon-insert-gnu-licence',
`mon-insert-gnu-licence-gfdl'.\n►►►"
  (let ((lic (if mit-bsd-gfdl 
                (cond ((eq mit-bsd-gfdl 'mit)  'mit)
                      ((eq mit-bsd-gfdl 'bsd)  'bsd)
                      ((eq mit-bsd-gfdl 'gfdl) 'gfdl)
                      (t 'gpl))
                'gpl)))    
    (concat (unless (eq lic 'gfdl)
              (replace-regexp-in-string " $" ". All rights reserved." 
                                        (mon-build-copyright-string nil nil t t)))
            (mapconcat #'(lambda (s)
                           (if (eq lic 'bsd)
                               (replace-regexp-in-string  "<COPYRIGHT HOLDER>" 
                                                          (cadr (assoc 6 *MON-NAME*)) s)
                               (identity s)))
                       (case lic 
                         ('mit  *mon-mit-license-header*)     
                         ('bsd  *mon-bsd-license-header*)     
                         ('gfdl *mon-gnu-license-header-gfdl*)
                         ('gpl  *mon-gnu-license-header*))
                       "\n;; " ))))
;;
;;;`mon-build-copyright-string-license'
;;
;;; :TEST-ME (mon-build-copyright-string-license)
;;; :TEST-ME (mon-build-copyright-string-license 'mit)
;;; :TEST-ME (mon-build-copyright-string-license 'bsd)
;;; :TEST-ME (mon-build-copyright-string-license 'gfdl)
;;; :TEST-ME (mon-build-copyright-string-license 'gpl)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-09T11:26:16-04:00Z}#{09327} - by MON KEY>
(defun mon-insert-gnu-licence (&optional insrtp intrp)
  "Return `*mon-gnu-license-header*'.\n 
When INSRTP non-nil or called-interactively insert GNU license at point, but
does not move point.\n
:EXAMPLE\n\n(mon-insert-gnu-licenceg-gfdl)\n
:SEE-ALSO `*mon-bsd-license-header*', `*mon-gnu-license-header-gfdl*',
`mon-insert-gnu-licence-gfdl' `mon-build-copyright-string-license',
`mon-insert-file-template', `mon-insert-gnu-licence-gfdl',
`mon-insert-file-template'.\n►►►"
  (interactive "i\np")
  (if (or insrtp intrp)
      (save-excursion
        (newline)
        (insert (mon-build-copyright-string-license 'gpl)))
      (mon-build-copyright-string-license 'gpl)))
;;
;;; :TEST-ME (mon-insert-gnu-licence)
;;; :TEST-ME (mon-insert-gnu-licence t)
;;; :TEST-ME (call-interactively 'mon-insert-gnu-licence)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-01T16:27:20-05:00Z}#{10051} - by MON KEY>y
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T15:43:43-04:00Z}#{09411} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-02T12:01:00-04:00Z}#{09405} - by MON KEY>
(defun mon-insert-gnu-licence-gfdl (&optional w-divider insrtp intrp)
  "Return GNU-GFDL as string from `*mon-gnu-license-header-gfdl*'.\n
When INSRTP non-nil or called-interactively insert at point, but
does not move point.\n
:EXAMPLE\n(mon-insert-gnu-licence-gfdl)\n\(mon-insert-gnu-licence-gfdl t\)\n
:SEE-ALSO `mon-insert-gnu-licence', `mon-insert-file-template'.
`*mon-gnu-license-header*', `*mon-bsd-license-header*',
`*mon-mit-license-header*', `mon-build-copyright-string-license'
`mon-insert-file-template'.\n►►►"
  (interactive "P\ni\np")
  (let ((bld-gfdl (cond (w-divider
                         (concat 
                          (mon-comment-divider-w-len 64) "\n"
                          (mon-build-copyright-string-license 'gfdl) "\n"
                          ;; :WAS *mon-gnu-license-header-gfdl*  "\n"
                          (mon-comment-divider-w-len 64) "\n"
                          (mapconcat 'identity
                                     (mon-sublist-gutted
                                      0 1 
                                      (split-string (mon-build-copyright-string nil nil t t nil t) "\n"))
                                     "\n")))
                        ;; :WAS (t *mon-gnu-license-header-gfdl*))))
                        (t  (mon-build-copyright-string-license 'gfdl)))))
    (if (or insrtp intrp)
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
(defun mon-insert-defclass-template (&optional class-pfx slot-count insrtp intrp)
  "Return an `EIEIO' `defclass' template.\n
When non-nil CLASS-PFX is a class-name for template. Default is <CLASS-NAME>.
SLOT-COUNT is the number of slot templates returned.
When INSERTP in non-nil or called-interactively insert template at point.
Does not move point.\n:EXAMPLE\n(mon-insert-defclass-template nil 2)\n
:SEE-ALSO `mon-insert-naf-mode-class-template', `mon-help-eieio-defclass'.\n►►►"
  (interactive "i\nP\ni\np")
  (let ((c-nm (if class-pfx (format "%s" class-pfx) "<CLASS-NAME>"))
        (s-nm (if class-pfx (concat "<SLOT-"      
                                    (cond ((<= (length class-pfx) 2) class-pfx)
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
    (if (or insrtp intrp)
        (save-excursion (newline) (princ gathered (current-buffer)))
        gathered)))
;;                   
;;; :TEST-ME (mon-insert-defclass-template nil nil t)
;;; :TEST-ME (mon-insert-defclass-template "BUBBA" nil t)
;;; :TEST-ME (mon-insert-defclass-template nil 2)
;;; :TEST-ME (mon-insert-defclass-template nil 2 t)

;;; ==============================
;;; :COURTESY Aaron S. Hawley 
;;; :SEE (URL `http://www.emacswiki.org/emacs/AutoInsertForTexinfo')
;;; :NOTE Texinfo template. Based on "Emacs Lisp Header" in auto-insert.el
;;; :MODIFICATIONS <Timestamp: #{2009-08-24T11:38:35-04:00Z}#{09351} - by MON>
;;; Turned this into a function so now without reliance on autoinsertmode.
;;; ==============================
(defun mon-insert-texi-template (title short-description top &optional intrp)
  "Insert Texinfo template in buffer at point.\n
TITLE, SHORT-DESCRIPTION, and TOP are per Texinfo spec.\n
When called-interactively prompts for TITLE, SHORT-DESCRIPTION, and TOP.
Try to DTRT when buffer is not visiting file and prompts for filename to write
buffer to before proceeding with insertion.\n
:SEE-ALSO `mon-file-stamp', `mon-insert-file-template', `mon-insert-gnu-licence',
`mon-insert-ebay-dbc-template',`mon-insert-CL-package-template'.\n►►►"
  (interactive "\i\n\i\n\i\nP")
  (let ((mail-addr (cadr (assoc 9 *MON-ORG-NAME*))) ;WAS: `user-mail-address'
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
                   (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
        (gfdl
         (replace-regexp-in-string ":\n.URL.*" " @url{http://www.gnu.org/licenses/fdl-1.3.txt}."
           (replace-regexp-in-string "^;;; " "" (mon-build-copyright-string-license 'gfdl)))))
    (save-excursion
      ;;(goto-char (point-min))
      (insert (concat
               "\\input texinfo   @c -*-texinfo-*-
  @c %**start of header
  @setfilename " fname ".info\n"
               "@settitle " title "\n"
               "@c %**end of header
  @copying\n"
               ;; :WAS (read-string "Short description :")
               shrt-desc".\n\n"        
               "Copyright @copyright{} " this-year "  "
               ;; :WAS ;;(getenv "ORGANIZATION") | (progn user-full-name) "
               organization " | " u-name
               "@quotation \n" gfdl "\n
  @end quotation

  The document was typeset with
  @uref{http://www.texinfo.org/, GNU Texinfo}.

  @end copying

  @titlepage
  @title " title "
  @subtitle " shrt-desc "
  @author " organization u-name   " |  <" mail-addr ">\n"
  ;; :WAS ;; (getenv "ORGANIZATION") | (progn user-full-name)  " <" (progn user-mail-address) ">
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
;;; :NOTE (local-set-key "\C-c4" 'mon-comput-45)
;;; :CREATED <Timestamp: #{2009-10-17T11:30:32-04:00Z}#{09426} - by MON>
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

;;; ==============================
;;; :NOTE (local-set-key "\C-c3" mon-comput-33)
;;; :CREATED <Timestamp: #{2009-10-17T11:30:06-04:00Z}#{09426} - by MON>
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
(provide 'mon-insertion-utils)
;;; ==============================

;;; ==============================
;;; mon-insertion-utils.el ends here
;;; EOF

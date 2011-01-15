;;; mon-insertion-utils.el --- insertion utils, licences, file templates, etc.
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-insertion-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-04-08T12:37:06-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: convenience, editing, 

;;; ================================================================

;;; COMMENTARY: 

;; ================================================================
;; DESCRIPTION:
;; mon-insertion-utils.el Provides insertion related utilities, templates and
;; string building/manipulation procedures that ease routine chores and
;; interactive command invocation.
;; Many larger 'texty' functions are included here because they have become to
;; unwieldly or are otherwise to large to maintain in their original source
;; location.
;;
;; FUNCTIONS:►►►
;; `mon-insert-string-n-fancy-times', `mon-line-number-region',
;; `mon-string-incr',
;; `mon-insert-string-n-times', `mon-comment-divider',
;; `mon-comment-divider-to-col-four', `php-comment-divider', `mon-insert-copyright',
;;
;; `mon-insert-whitespace', `mon-insert-newlines', 
;; `mon-split-designator',
;;
;; `mon-insert-file-in-dirs', `mon-insert-dirs-in-path', 
;;
;; `mon-insert-user-name-cond', `mon-insert-system-type-cond',
;; `mon-insert-gnu-licence', `mon-insert-gnu-licence-gfdl' ,
;; `mon-build-copyright-string', `mon-comput-33', `mon-comput-45',
;; `mon-build-copyright-string-license', `mon-insert-lisp-doc-eg-xref',
;; `mon-insert-regexp-template-yyyy'`mon-insert-regexp-template',
;;
;; `mon-lisp-evald',
;; `mon-insert-defclass-template',
;; `mon-insert-CL-file-template', `mon-insert-lisp-CL-package-template',
;; `mon-insert-lisp-CL-mode-line-template', `mon-lisp-CL-package-complete',
;; `mon-insert-lisp-CL-jump-doc', `mon-insert-lisp-CL-debug',
;; `mon-insert-lisp-CL-eval-when',
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
;; VARIABLES:
;; `*mon-gnu-license-header*', `*mon-gnu-license-header-emacs*',
;; `*mon-mit-license-header*', `*mon-bsd-license-header*',
;; `*mon-gnu-license-header-gfdl*', `*mon-bsd-license-header-COPYRIGHT-HOLDER*',
;;
;; GROUPS:
;; `mon-insertion-utils'
;; 
;; ALIASED/ADIVISED/SUBST'D:
;; `comment-divider'                     -> `mon-comment-divider'
;; `mon-comment-divider->col'            -> `mon-comment-divider-to-col'
;; `bug-insert-copyright'                -> `mon-insert-copyright'
;; `mon-insert-doc-xref-eg'              -> `mon-insert-lisp-doc-eg-xref'
;; `mon-lisp-comment-to-col'             -> `mon-comment-lisp-to-col'
;; `mon-CL-package-complete'             -> `mon-lisp-CL-package-complete'
;; `mon-insert-CL-mode-line-template'    -> `mon-insert-lisp-CL-mode-line-template'
;; `mon-insert-CL-file-template'         -> `mon-insert-lisp-CL-file-template'
;; `mon-insert-CL-package-template'      -> `mon-insert-lisp-CL-package-template'
;; `mon-insert-CL-debug'                 -> `mon-insert-lisp-CL-debug'
;; `mon-insert-CL-eval-when'             -> `mon-insert-lisp-CL-eval-when'
;; `mon-insert-jump-lisp-doc'            -> `mon-insert-lisp-CL-jump-doc'
;; `mon-add-lisp-CL-file-local-prop-template' ->  `mon-insert-lisp-CL-mode-line-template'
;; `mon-string-n-fancy-times'            -> `mon-insert-string-n-fancy-times'
;; `mon-string-insert-n-times'           -> `mon-insert-string-n-times'
;;
;; RENAMED:
;; `mon-lisp-comment-to-col'             -> `mon-comment-lisp-to-col'
;; `split-designator'                    -> `mon-split-designator'
;; `mon-interactively-stringify'         -> `mon-insert-string-ify'
;; `php-comment-divider'                 -> `mon-insert-php-comment-divider'
;; `mon-insert-naf-mode-file-template'   -> `mon-insert-file-template'
;; `mon-insert-string-incr'              -> `mon-string-incr'

;; `mon-insert-wht-spc'                  -> `mon-insert-whitespace'
;; `mon-comment-divider'                 -> `comment-divider'
;; `mon-insert-lisp-package-template'    -> `mon-insert-lisp-CL-package-template'
;; `mon-insert-CL-mode-line-template'    -> `mon-insert-lisp-CL-mode-line-template'
;;
;; RENAMED-AND-MOVED:
;; Following moved to `mon-doc-help-utils.el' and renamed *insert* -> *Help*
;; `mon-insert-file-dir-functions'       -> `mon-help-file-dir-functions'
;; `mon-insert-install-info-incantation' -> `mon-help-install-info-incantation'
;; `mon-insert-rename-incantation'       -> `mon-help-rename-incantation'
;; `mon-insert-tar-incantation'          -> `mon-help-tar-incantation'
;; `mon-insert-info-incantation'         -> `mon-help-info-incantation'
;; `mon-insert-diacritics'               -> `mon-help-diacritics'
;; `mon-insert-naf-mode-file-template'   -> `mon-insert-file-template'
;; `mon-user-evald'                      -> `mon-lisp-evald'
;; `mon-incr'                            -> `mon-string-incr'
;;
;; MOVED: 
;; `mon-insert-user-name-cond'           <- mon-w32-load.el
;; `mon-insert-system-type-cond'         <- mon-w32-load.el
;; `mon-insert-regexp-template-yyyy'     <- naf-mode-replacement-utils.el
;; `mon-accessed-stamp'                  -> mon-time-utils.el
;; `mon-stamp'                           -> mon-time-utils.el
;; `mon-timestamp'                       -> mon-time-utils.el
;; `mon-accessed-time-stamp'             -> mon-time-utils.el
;; `*mon-hgignore-template*'             -> mon-bzr-utils.el
;; `mon-insert-hgignore-template'        -> mon-bzr-utils.el
;; `mon-insert-lisp-testme-fancy'        -> mon-testme-utils.el
;; `mon-insert-test-cases'               -> mon-testme-utils.el
;; `mon-insert-lisp-testme'              -> mon-testme-utils.el
;; `mon-build-copyright-string-TEST'     -> mon-insertion-utils.el
;; `mon-line-drop-in-words'              -> mon-line-utils.el
;; `mon-string-incr-padded'              -> mon-line-utils.el
;; `mon-print-in-buffer-if-p'            -> mon-macs.el
;;
;; REQUIRES:
;; :FILE mon-time-utils.el  
;; :LINK (URL `http://www.emacswiki.org/emacs/mon-time-utils.el')
;; :CALLED-BY 'anything-that-uses-a-time-stamp'
;;
;; :FILE mon-utils.el 
;; :LINK (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;; :FUNCTION `mon-sublist-gutted' :CALLED-BY `mon-insert-gnu-licence-gfdl' 
;;
;; :FILE mon-site-local-defaults.el
;; `*MON-NAME*'
;; 
;; TODO:
;; Build a progn makunbound unintern template, e.g.:
;; ;;;(progn ([f]makunbound 'VAR-OR-FUNCTION) 
;; ;;;       (unintern 'VAR-OR-FUNCTION))
;; 
;; URL: http://www.emacswiki.org/emacs/mon-insertion-utils.el
;; FILE-PUBLISHED: <Timestamp: #{2009-08-27} - by MON>
;;
;; FILE-CREATED: 
;; <Timestamp: Wednesday April 08, 2009 @ 12:37.06 PM - by MON KEY>
;;
;; ================================================================

;;; LICENSE:

;; ================================================================
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; ================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled "GNU Free Documentation License".
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;; Copyright © 2009-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :REQUIRED-BY Anything that uses a time-stamp.
(require 'mon-time-utils) 

(declare-function slime-current-connection "ext:slime.el" t t)
(declare-function slime-current-package    "ext:slime.el" t t)
(declare-function slime-buffer-package     "ext:slime.el" t t)

;; (declare-function mon-error-string-err-format "mon-error-utils.el")

;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-04T14:18:03-04:00Z}#{10401} - by MON KEY>
(defgroup mon-insertion-utils nil
  "Variables and settings for `mon-*' functions which insert.\n
:SEE-ALSO .\n►►►"
  :link '(url-link :tag ":EMACSWIKI-FILE" 
                   "http://www.emacswiki.org/emacs/mon-insertion-utils.el")
  :link '(emacs-library-link "mon-insertion-utils.el")
  :group 'mon-base)

;;; ==============================
(defun mon-insert-dirs-in-path (dir-list dir-path)
  "Directory elts held by symbol DIR-LIST (list of strings) are inserted in DIR-PATH.\n
Does minimal error checking, shouldn't be trusted for unfamilar pathnames.\n
:SEE-ALSO `mon-insert-file-in-dirs', `directory-files',
`file-expand-wildcards'.\n►►►"
  (interactive 
   "X:FUNCTION `mon-insert-dirs-in-path' -- symbol holding list of directorys to create: \nDPath to insert to: ")
  (let ((make-list dir-list) 
	(into-path dir-path)	
	midip-do-dir)
    (progn
      (cond ((file-exists-p (directory-file-name (file-name-as-directory into-path)))
             (setq midip-do-dir 1))
            ((not (file-exists-p (directory-file-name (file-name-as-directory  into-path)))) 
             ;;(setq midip-do-dir '()) midip-do-dir))
             (setq midip-do-dir ())))
      (cond (midip-do-dir (while make-list
                     (let ((put-list (car make-list)))
                       (make-directory (concat into-path "/" put-list)))
                     (setq make-list (cdr make-list))))
            ((not midip-do-dir)
             (message (concat ":FUNCTION `mon-insert-dirs-in-path' "
                              " -- non-existent directory: %s") into-path))))))
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
:SEE-ALSO `mon-write-jg-file-in-path', `mon-make-jg-dir-in-path',
`mon-format-jg-file-for-write', `directory-files',
`file-expand-wildcards'.\n►►►"
  (interactive 
   "X:FUNCTION `mon-insert-file-in-dirs' -- Symbol holding dir/file list: \nXGive Symbol holing insert-text :\nsFile extenison :")
  (while make-dir-list 
    (let* ((mifid-file-dir make-dir-list)
	   (mifid-cur-file (car make-dir-list))
	   (mifid-dot-ext extension)
	   mifid-cln-it mifid-cf)
      (let ((mifid-cf mifid-cur-file)
	    (mifid-cln-it mifid-cur-file))
	(setq mifid-cf (replace-regexp-in-string "[,_.:;[:space:]]" "-" mifid-cf))
	(when (string-match "-+" mifid-cf)
	  (setq mifid-cf (replace-match "-" nil nil mifid-cf)))
	(when (string-match "-$" mifid-cf)
	  (setq mifid-cf (replace-match "" nil nil mifid-cf)))
	;; (setq mifid-cln-it mifid-cf)
	(setq mifid-cln-it (replace-regexp-in-string "[_().:;[:space:]]" "-" mifid-cln-it))
	(when (string-match "-+" mifid-cln-it)
	  (setq mifid-cln-it (replace-match "-" nil nil mifid-cln-it)))
	(when (string-match ",-" mifid-cln-it)
	  (setq mifid-cln-it (replace-match ",_" nil nil mifid-cln-it)))
	(when (string-match "-$" mifid-cln-it)
	  (setq mifid-cln-it (replace-match "" nil nil mifid-cln-it)))
	(when (string-match "\\([\(\)_\:[:space:]]\\)" mifid-dot-ext) 
	  (setq mifid-dot-ext (replace-match "." nil nil mifid-dot-ext)))
	(when (string-match "\\(^[^.]\\)" mifid-dot-ext) 
	  (setq mifid-dot-ext (concat "." mifid-dot-ext)))
	(make-directory mifid-cf)
	(with-temp-file 
	    (concat default-directory mifid-cf "/" mifid-cln-it mifid-dot-ext)
	  (insert (format "%s" insert-text))))
      (pop make-dir-list))))

;;; ==============================
;;; :RENAMED `mon-insert-wht-spc' -> `mon-insert-whitespace'
;;; :MODIFICATIONS <Timestamp: #{2010-03-26T12:38:53-04:00Z}#{10125} - by MON KEY>
;;; :CREATED <Timestamp: Thursday June 11, 2009 @ 02:23.20 PM - by MON KEY>
(defun mon-insert-whitespace (spc-cnt &optional insrtp intrp)
  "Return space char 32 SPC-CNT times.\n
SPC-CNT is the number of whitespace chars to return.\n
When called-interactively SPC-CNT may be given as prefix arg.\n
When optional arg INSRTP is non-nil or called-interactively insert whitespace at
point.  Moves point.\n
A whitespace is:\n- character: SPC (32, #o40, #x20);\n- code point: 0x20\n- name: SPACE\n
:NOTE Does not inherit stickiness of adjoining chars `text-properties-at'.\n
:EXAMPLE\n\n(mon-insert-whitespace 8\)\n
 (mon-inhibit-read-only (mon-insert-whitespace 8 t))
:SEE-ALSO `mon-insert-newlines', `mon-insert-unicode',
`mon-insert-string-n-times', `mon-insert-string-n-fancy-times',
`whitespace-cleanup', `untabify'.\n►►►"
  (interactive "N:FUNCTION `mon-insert-whitespace' -- number of spaces to insert: \ni\np")
  (if (or intrp insrtp)
      (insert-char 32 spc-cnt nil)
      (make-string spc-cnt 32)))
;;
;;; :TEST-ME (mon-insert-whitespace 8)
;;; :TEST-ME (mon-insert-whitespace 8 t)
;;; :TEST-ME (call-interactively 'mon-insert-whitespace)

;;; ==============================
;;; :CREATED <Timestamp: Friday June 12, 2009 @ 11:18.21 AM - by MON KEY>
(defun mon-insert-newlines (nl-cnt &optional insrtp intrp)
  "Return newline char NL-CNT times.\n
NL-CNT is the number of newline chars to return.
When called-interactively NL-CNT may be given as prefix arg.
When optional arg INSRTP is non-nil or called-interactively insert newlines at
point.  Moves point.\n
A newline is:\n - code point: '0x0A';\n - character: C-j (10, #o12, #xa);
 - old-name: LINE FEED (LF);\n
:NOTE Does not inherit stickiness of adjoining chars `text-properties-at'.\n
:EXAMPLE\n\n\(mon-insert-newlines 5\)\n
:SEE-ALSO `mon-insert-whitespace', `mon-insert-unicode',
`mon-insert-string-n-times', `mon-insert-string-n-fancy-times', `newline'.\n►►►"
  (interactive "N:FUNCTION `mon-insert-newlines' -- number of newlines to insert: \ni\np")
  (if (or intrp insrtp)
      (insert-char ?\n nl-cnt nil)
      (make-string nl-cnt 10)))
;;
;;; :TEST-ME (mon-insert-newlines 3)
;;; :TEST-ME (mon-insert-whitespace 3)
;;; :TEST-ME (mon-insert-newlines 3 t) 
;;; :TEST-ME (call-interactively 'mon-insert-newlines)


;;; ==============================
;;; :TODO Consider dispatching from a keyword'd version for non-interactive calls w/:
;;;  :put-count :string-to-put :w-newline :delim :w-whtspc :insrtp
;;;
;;; :CHANGESET 2174 <Timestamp: #{2010-10-07T18:28:35-04:00Z}#{10404} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-02-12T15:30:25-05:00Z}#{10065} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-03-17T17:56:25-04:00Z}#{10113} - by MON KEY>
;;; :CREATED <Timestamp: Tuesday April 07, 2009 @ 11:35.38 AM - by MON KEY>
(defun mon-insert-string-n-fancy-times (&optional intrp put-count string-to-put
                                                  w-newline w-delim delim
                                                  w-whtspc insrtp)
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
When INSRTP is non-nil or called-interactively insert return value at point.
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
:ALIASED-BY `mon-string-n-fancy-times'\n
:SEE-ALSO `mon-insert-string-n-fancy-times', `mon-string-incr',
`mon-line-number-region-incr', `mon-insert-whitespace', `mon-insert-newlines',
`mon-insert-unicode'.\n►►►"
  (interactive "p")
  (let*   ((mistnft-fncn "mon-insert-string-n-fancy-times")
           (mistnft-prmpt   (concat ":FUNCTION " mistnft-fncn " -- "))
           (mistnft-put-cnt     
            (cond ((and put-count (stringp put-count))
                   (string-to-number put-count))
                  (put-count put-count)
                  (intrp (read-number 
                          (concat mistnft-prmpt "how many puts?: ")))
                  (t 1)))
           (mistnft-put-str  
            (cond (string-to-put 
                   (unless (stringp string-to-put)
                     (error (mon-error-string-err-format mistnft-fncn "STRING-TO-PUT" string-to-put)))
                   string-to-put)
                  (intrp (read-string (concat mistnft-prmpt "string to put: ")))))
           (mistnft-dlm-p 
            (cond (w-delim w-delim)
                  (intrp (yes-or-no-p  (concat mistnft-prmpt "with delimiter?: ")))))
           (mistnft-nlp
            (cond (w-newline w-newline)
                  (intrp (yes-or-no-p (concat mistnft-prmpt "insert newlines?: ")))))
           (mistnft-dlm-put 
            (let ((mistnft-dt  mistnft-dlm-p)        ;; t when delim is t.
                  (mistnft-df  (not mistnft-dlm-p))  ;; t when delim is nil.
                  (mistnft-nlt mistnft-nlp)          ;; t when newlines is t.
                  (mistnft-nlf (not mistnft-nlp))    ;; t when newlines is nil.
                  ;; Only used to signal an error before leaving let-binding.
                  (mistnft-dlm (cond ((and (not delim) (not intrp))) ;delim is nil 
                                     ((and delim (stringp delim)) delim) ;delim is a string
                                     ((and delim (not (stringp delim))) ;delim is in error
                                      (error (mon-error-string-err-format mistnft-fncn "DELIM" delim))))))
              (cond ((and mistnft-nlt mistnft-dt) ;; Do newline, do delim.
                     (concat (if intrp 
                                 (read-string 
                                  (concat mistnft-prmpt "delimit range with (add whitespace if needed): "))
                               delim)
                             "\n"))
                    ((and mistnft-dt mistnft-nlf) ;; Do delim, no newline.
                     (if intrp 
                         (read-string 
                          (concat mistnft-prmpt "delimit range with (add whitespace if needed): "))
                       delim))
                    ((and mistnft-nlt mistnft-df) "\n") ;; Do newline, no delim.
                    ((and mistnft-df mistnft-nlf) ;; No delim,  no newline. What about whitespace??
                     (if intrp 
                         (if (yes-or-no-p  (concat mistnft-prmpt "no delim, no newline, "
                                                   "interleave the put string with whitespace?: "))
                             " " "")
                       (if w-whtspc " " ""))))))
           mistnft-rtn-put)
    (setq mistnft-rtn-put 
          (with-temp-buffer  
            (while (> mistnft-put-cnt 0)
              (insert (format "%s%s" mistnft-put-str mistnft-dlm-put))
              (setq mistnft-put-cnt (1- mistnft-put-cnt)))
            (mon-buffer-sub-no-prop)))
    ;; :WAS (if (or insrtp intrp)
    ;; (let ((llm-tgl (buffer-local-value longlines-mode (current-buffer))))
    ;; (unwind-protect
    ;;      (save-excursion
    ;;        (when llm-tgl (longlines-mode 0))
    ;;        (newline)
    ;;        (insert mistnft-rtn-put))
    ;;   (when llm-tgl (longlines-mode))))
    ;; mistnft-rtn-put)))
    (if (or insrtp intrp)
        (mon-naf-mode-toggle-restore-llm nil 
          (save-excursion (newline)(insert mistnft-rtn-put)))
        mistnft-rtn-put)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (string-equal 
;; |  (mon-insert-string-n-fancy-times nil 4 "bubba" nil nil nil)
;; |  "bubbabubbabubbabubba")
;; | (string-equal 
;; |  (mon-insert-string-n-fancy-times nil 4 "bubba" nil nil nil t)
;; |  "bubba bubba bubba bubba ")
;; | (string-equal
;; |  (mon-insert-string-n-fancy-times nil 4 "bubba" t nil nil)
;; |  "bubba\nbubba\nbubba\nbubba\n")
;; | (string-equal
;; |  (mon-insert-string-n-fancy-times nil 3 "bubba" nil t ", ")
;; |  "bubba, bubba, bubba, ")
;; | (string-equal
;; |  (mon-insert-string-n-fancy-times nil 3 "bubba" nil t " | ")
;; |  "bubba | bubba | bubba | ")
;; | (string-equal
;; |  (with-temp-buffer
;; |    (mon-insert-string-n-fancy-times nil 3 "bubba" t t " |" nil t)
;; |    (mon-buffer-sub-no-prop))
;; |  "\nbubba |\nbubba |\nbubba |\n")
;; | ;; Following fail successfully
;; | (not (ignore-errors (mon-insert-string-n-fancy-times nil 4 'sisi nil nil nil)))
;; | (not (ignore-errors (mon-insert-string-n-fancy-times  nil 4 "sisi" nil t 88 nil nil)))
;; `----


;;; ==============================
;;; :PREFIX "misnt-"
;;; :CREATED <Timestamp: Monday February 09, 2009 @ 09:41.37 PM - by MON KEY>
(defun mon-insert-string-n-times (put-count string-to-put &optional insrtp intrp)
  "Insert the STRING-TO-PUT PUT-COUNT times.\n
PUT-COUNT is a number indicating the N-times to put string.\n
STRING-TO-PUT is the string to insert N-times.\n
When optional arg INSRTP is non-nil or called-interactively non-nil insert
string in current-buffer. Moves point.\n
When called-interactively prompt for:\n -- how many puts?:\n -- string to put:\n
:EXAMPLE\n\n\(mon-insert-string-n-times 3 \"bubba \"\)\n
:ALIASED-BY `mon-string-insert-n-times'\n
:SEE-ALSO `mon-insert-string-n-fancy-times', `mon-string-incr',
`mon-line-number-region-incr', `mon-insert-whitespace', `mon-insert-newlines',
`mon-insert-unicode'.\n►►►"
  (interactive (list (read-number (concat ":FUNCTION `mon-insert-string-n-times' "
                                          "-- how many puts?: "))
                     (read-string (concat ":FUNCTION `mon-insert-string-n-times' "
                                          "-- string to put: "))
                     nil t))
  (or (and (stringp string-to-put) string-to-put)
      (error (mon-error-string-err-format 
              "mon-insert-string-n-times" "string-to-put" string-to-put)))
  (if (or insrtp intrp)
      (dotimes (misnt-i put-count t)
        (insert string-to-put)))
  (let (misnt-gthr)
    (dotimes (misnt-i put-count
                      (setq misnt-gthr (mapconcat #'identity misnt-gthr "")))
      (push string-to-put misnt-gthr))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (string-equal 
;; |  (mon-insert-string-n-times 3 "bubba ")
;; |  "bubba bubba bubba ")
;; | 
;; | (string-equal
;; |  (with-temp-buffer
;; |    (mon-insert-string-n-times 3 "bubba " t)
;; |    (newline)
;; |      (mon-insert-string-n-times 3 "bubba " nil t)
;; |    (mon-buffer-sub-no-prop))
;; |  "bubba bubba bubba \nbubba bubba bubba ")
;; | Following fails successfully
;; | (not (ignore-errors (mon-insert-string-n-times 3 88)))
;; `----


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
:SEE-ALSO `mon-line-string-incr-padded', `mon-line-number-region-incr',
`mon-rectangle-sum-column'.\n►►►"
  (interactive "*r\np")
  (let* ((mlnr-lns (count-lines start end))
	 (mlnr-frm (or begin-w 1))
	 (mlnr-to (+ mlnr-lns (1- mlnr-frm)))
	 (mlnr-nums (number-sequence mlnr-frm mlnr-to))
	 (mlnr-width (max (length (number-to-string mlnr-lns))
		     (length (number-to-string mlnr-frm)))))
    (if (= start (point))
	(setq mlnr-nums (reverse mlnr-nums)))
    (goto-char start)
    (dolist (n mlnr-nums)
      (beginning-of-line)
      (save-match-data
	(if (looking-at " *-?[0-9]+\\. ")
	    (replace-match "")))
      (insert (format (concat "%" (number-to-string mlnr-width) "d. ") n))
      (forward-line))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-17T17:59:42-04:00Z}#{10113} - by MON KEY>
;;; :CREATED <Timestamp: Tuesday February 03, 2009 @ 03:41.17 PM - by MON KEY>
(defun mon-string-incr (start-w end-w step-w &optional w-delim delimiter
                               w-newln w-wspc insrtp intrp)
  "Increment a range of numbers from START-W to END-W by STEP-W.\n
START-W a number to increment from.\n
END-W a number to increment to.\n
STEP-W a number to step increments by.\n
W-DELIM is non-nil when incremented range should be delmited.\n
DELIMITER a string to delimit the range with.\n
W-NEWLN is non-nil when increment range should have newlines added.\n
W-WSPC is non-nil if incrment range should be delimited with whitespace when
args W-NEWLN and W-DELIM are ommited.\n
When called-interactively or INSRTP is non-nil toggle `longlines-mode' if
active in current-buffer and insert to current buffer the incrmented range.
Does not move point.\n
When called-interactively prompt with:\n
 \"Increment from number: \"\n \"Increment to number: \"
 \"Step by increments of: \"\n \"With delimiter?: \"\n \"Insert newlines?: \"\n
When \"With delimiter?: \" is non-nil prompt:\n\n \"Delimit range with: \"\n
When both \"With delimiter?\" and \"Insert newlines?\" are nil prompt:\n
 \"Interleave range with whitespace?: \"\n
:EXAMPLE\n\n;;  1-100 by 5
\(mon-string-incr 1 100 5\)\n
;;  1-100 by 5 inteleaved with \" | \"
\(mon-string-incr 1 100 5 t \" | \"\)\n
;; 1-100 by 5 inteleaved with \".\" add newline
\(mon-string-incr 1 100 5 t \".\" t\)\n
;; 1-100 by 5 add whitespace
\(mon-string-incr 1 100 5 nil nil nil t\)\n
;; 1-100 by 5 w/bogus args (but don't fail)
\(mon-string-incr 1 100 5 t nil nil t\)\n
\(mon-string-incr 1 100 5 t t nil nil\)\n
\(mon-string-incr 1 100 5 nil t nil t\)\n
\(mon-string-incr 1 100 5 nil nil t t\)\n
;; 1-5005 by 5 - Signal an error
\(mon-string-incr 1 5005 5\)\n
:NOTE If the range specified will step over 1000 increments signal an error.\n
:SEE-ALSO `mon-line-string-incr-padded', `mon-line-number-region', 
`mon-line-number-region-incr', `mon-rectangle-sum-column'.\n►►►"
  (interactive `(,(read-number "Increment from number: ")  ;; start-w
                  ,(read-number "Increment to number: ")   ;; end-w
                  ,(read-number "Step by increments of: ") ;; step-w
                  ,(yes-or-no-p "With delimiter?: ")       ;; w-delim
                  ,(read-string "Delimit range with: ")    ;; delimiter
                  ,(yes-or-no-p "Insert newlines?: ")      ;; w-newln
                  nil                                      ;; w-wspc
                  nil                                      ;; insrtp
                  t))                                      ;; intrp
  (catch 'mon-incr-big-range 
    (let* ((msi-fncn "mon-string-incr")
           (msi-str-incr (if intrp 
                             start-w
                           (cond ((and (integerp start-w) (>= start-w 0)) start-w)
                                 ((stringp start-w) (string-to-number start-w))
                                 (t (error (concat ":FUNCTION `%s' "
                                                   "-- arg START-W not a number") msi-fncn)))))
           (msi-end-incr  (if intrp 
                              end-w
                            (cond ((and (integerp end-w) (>= end-w 0)) end-w)
                                  ((stringp end-w) (string-to-number end-w))
                                  (t (error (concat ":FUNCTION `%s' "
                                                    "-- arg END-W not a number") msi-fncn)))))
           (msi-stp-val (if intrp 
                            step-w
                          (cond ((and (integerp step-w) (>= step-w 0)) step-w)
                                ((stringp step-w) (string-to-number step-w))
                                (t (error (concat ":FUNCTION `%s' "
                                                  "-- arg STEP-W not a number") msi-fncn)))))
           (msi-rng (- msi-end-incr msi-str-incr))
           (msi-rng-chk
            (when  (> (+ (/ msi-rng msi-stp-val) (mod msi-rng msi-stp-val)) 1000)
              (if intrp 
                  (progn
                    (message (concat ":FUNCTION `%s' "
                                     "-- ranges with values over 1000 not fun "
                                     "for the little man in Emacs.\n"
                                     "He won't schlep that range!") msi-fncn)
                    (sit-for 2)
                    ;; :WAS (throw 'mon-incr-big-range (funcall 'mon-string-incr)))
                    (throw 'mon-incr-big-range 
                           (call-interactively 'mon-string-incr)))
                (error  (concat ":FUNCTION `%s' "
                                "-- arg STEP-W steps over 1000 times") msi-fncn))))
           (msi-dlm (if intrp 
                        delimiter
                      (cond ((stringp delimiter) delimiter)
                            ((numberp delimiter) (number-to-string delimiter))
                            ;; ((and (characterp delimiter) (not (null delimiter) (not delimiter)))
                            ;;  (char-to-string delimiter))
                            ((booleanp delimiter)
                             (if (and delimiter w-wspc) " " "")))))
           (msi-delim (let ((msi-dt w-delim)            ; t when delim y
                            (msi-df (not w-delim))      ; t when delim n
                            (msi-nlt w-newln)           ; t when nl t
                            (msi-nlf (not w-newln)))    ; t when nl n
                        (cond ((and msi-nlt msi-dt w-delim) ; do newline, do delim
                               (concat msi-dlm "\n"))
                              ((and msi-nlt msi-df) "\n") ; do newline, no delim
                              ((and msi-dt msi-nlf w-delim (not (eq (length msi-dlm) 0))) msi-dlm)
                              ((and msi-dt msi-nlf msi-dlm w-wspc) ;; t nil nil t
                               (if (eq (length msi-dlm) 0) " "))
                              ((and msi-df msi-nlf)
                               (cond (intrp (if (yes-or-no-p 
                                                 (concat ":FUNCTION `mon-string-incr' "
                                                 "-- interleave range with whitespace?: "))
                                                " "  ""))
                                     (w-wspc " ")
                                     ((null w-wspc) "")))
                              ((eq (length msi-dlm) 0) ""))))
           msi-incr)
      (setq msi-incr
            (with-temp-buffer 
              (while (<= msi-str-incr msi-end-incr)
                (when (= msi-str-incr msi-end-incr) ; Kick out when we reach top.
                  (princ (format "%d" msi-str-incr) (current-buffer))
                  (setq msi-str-incr (+ msi-stp-val msi-end-incr)))
                (when (< msi-str-incr msi-end-incr) ; Keep going.
                  (princ (format "%d%s" msi-str-incr msi-delim) (current-buffer)))
                (setq msi-str-incr (+ msi-stp-val msi-str-incr)))
              ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) ))
              (mon-buffer-sub-no-prop) ))
      ;; :WAS
      ;; (if (or insrtp intrp)
      ;;     (save-excursion 
      ;;       (unwind-protect
      ;;            (let ((is-on (buffer-local-value longlines-mode (current-buffer)))
      ;;                  llm-off)
      ;;              (when is-on (longlines-mode 0) (setq llm-off 't))
      ;;              (newline) (insert msi-incr)
      ;;              (when llm-off (longlines-mode 1) (setq llm-off 'nil)))))
      ;;     msi-incr)
      (if (or insrtp intrp)
          (mon-naf-mode-toggle-restore-llm nil 
            (save-excursion 
              (newline) (insert msi-incr)))
          msi-incr)
      )))
;;
;;;  1-100 by 5 
;;; :TEST-ME  (mon-string-incr 1 100 5)
;;;  1-100 by 5 inteleaved with " | "
;;; :TEST-ME (mon-string-incr 1 100 5 t " | ")
;;;  1-100 by 5 inteleaved with "." add newline
;;; :TEST-ME (mon-string-incr 1 100 5 t "." t)
;;;  1-100 by 5 add whitespace
;;; :TEST-ME (mon-string-incr 1 100 5 nil nil nil t)
;;;  1-100 by 5 w/bogus args (but don't fail)
;;; :TEST-ME (mon-string-incr 1 100 5 t nil nil  t) 
;;; :TEST-ME (mon-string-incr 1 100 5 t t nil nil) 
;;; :TEST-ME (mon-string-incr 1 100 5 nil t nil t)
;;; :TEST-ME (mon-string-incr 1 100 5 nil nil t t)


;;; =======================
;;; :RENAMED `mon-interactively-stringify' -> `mon-insert-string-ify'
(defun mon-insert-string-ify (the-string &optional insrtp intrp)
  "Return THE-STRING as a list of tokens.\n
Return a list of strings obtained by breaking THE-STRING at space boundaries.\n
When optional arg INSRTP is non-nil or called-interactively insert list at point
as if by `print'.\n
:EXAMPLE\n\n\(mon-insert-string-ify \"a b c\"\)\n=>(\"a\" \"b\" \"c\"\)\).\n
:SEE-ALSO `mon-string-ify-list', `mon-line-strings-to-list',
`mon-line-strings-one-list'.\n►►►"
  (interactive "s:FUNCTION `mon-insert-string-ify' -- stringify: \ni\p")
  (if (or insrtp intrp)
      (print (mon-string-ify-list the-string) (current-buffer))
    (mon-string-ify-list the-string)))
;;
;;; :TEST-ME (mon-insert-string-ify "a b c")

;;; ==============================
;;; :NOTE Now inlined with the let in `unicode-insert'.
;;; :MODIFICATIONS <Timestamp: #{2009-11-27T15:17:34-05:00Z}#{09485} - by MON KEY>
(defun mon-insert-unicode (char &optional insrtp intrp)
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
For access/alteration to encoding/coding information:
:SEE `encode-coding-region', `describe-coding-system', `unicode-insert'.\n
:SEE-ALSO `ucs-insert', `mon-help-char-functions', `mon-help-diacritics', 
`mon-help-char-representation'.\n►►►"
  (interactive `(,@(let* ((rd-types (list "1 Unicode char in radix 16 e.g. '25BA'"
                                          "2 Unicode char by name w/ completion (octal, hex, decimal)"))
                          (rd-type (completing-read "read Unicode as " rd-types nil t))
                          (char-rd (cond ((string= (substring rd-type 0 1) "1")
                                          (let ((read-quoted-char-radix 16))
                                            (char-to-string (read-quoted-char "Char (radix-16): "))))
                                         ((string= (substring rd-type 0 1) "2")
                                          (let (miu-this-char)
                                            (setq miu-this-char (read-char-by-name "Char (tab completes): "))
                                            (cond ((not (integerp miu-this-char))
                                                   (error 
                                                    (concat ":FUNCTION `mon-insert-unicode' "
                                                            "-- not a Unicode character code: %S") miu-this-char))
                                                  ((or (< miu-this-char 0) (> miu-this-char #x10FFFF))
                                                   (error (concat ":FUNCTION `mon-insert-unicode' "
                                                                  "-- not a Unicode character code: 0x%X") miu-this-char)))
                                            (char-to-string miu-this-char))))))
                     `(,char-rd nil t))))
  (let (miu-t-char)
    (setq miu-t-char
          (cond (intrp char)
                ((integerp char) char)
                ;; (cdr (assoc-string "BLACK RIGHT-POINTING POINTER" (ucs-names) t))
                ((cdr (assoc-string char (ucs-names) t)))
                ((stringp char) 
                 (cond ((numberp (mon-string-to-symbol char))
                        (mon-string-to-symbol char)) ;; ((string-to-number char 16))
                       ((not (integerp char))
                        (error (concat ":FUNCTION `mon-insert-unicode' "
                                 "-- not a Unicode character code: %S") char))
                       ((or (< char 0) (>= char 65536)) ;;(or (< 9658 0) (> 9658 65536))
                        (error (concat ":FUNCTION `mon-insert-unicode' "
                                       "-- not a Unicode character code: %d") char))
                       ((or (< char 0) (> char #x10FFFF)) ;;(or (< #x25ba 0) (> #x25ba #x10FFFF))
                        (error (concat ":FUNCTION `mon-insert-unicode' "
                                       "-- not a Unicode character code: 0x%X") char))
                       ;; (t (setq miu-t-char (char-to-string miu-t-char))))
                       ))))
    (setq miu-t-char (cond (intrp char)
                       (t (char-to-string miu-t-char))))
    (if (or insrtp intrp)
        (save-excursion (insert-and-inherit miu-t-char))
        miu-t-char)
    miu-t-char))
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
When INSRTP is non-nil or called-interactively inserts at point.\n
Does not move point.\n
:EXAMPLE\n\n\(split-designator\)\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `non-posting-source', `non-posting-ebay-source', `*naf-comment-prefix*',
`non-posting-wiki-source', `npps', `benezit-naf-template',
`mon-comment-divider'.\n►►►"
  (interactive "i\np")
  (if (or insrtp intrp)
      (save-excursion
      (insert "---\n"))
    "---\n"))
;;
;;; :TEST-ME (mon-split-designator)
;;; :TEST-ME (call-interactively 'mon-split-designator)


;;; ==============================
;;; :RENAMED `comment-divider' -> `mon-comment-divider'
;;; :CHANGESET 1807 <Timestamp: #{2010-06-01T14:46:36-04:00Z}#{10222} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T14:09:37-04:00Z}#{09352} - by MON KEY>
(defun mon-comment-divider (&optional no-insrtp intrp)
  "Insert default comment divider at point.\n
When called-interactively insert the following at point:\n
;;; ==============================\n
When NO-INSRTP is non-nil return comment divider as string.\n
:EXAMPLE\n\(mon-comment-divider t\)\n
:ALIASED-BY `comment-divider'.\n
:SEE-ALSO `*mon-default-comment-divider*' `mon-comment-divide->col',
`mon-comment-lisp-to-col' `mon-insert-php-comment-divider',
`mon-insert-lisp-stamp', `mon-split-designator'.\n►►►"
  (interactive "i\np")
  (if (or (not no-insrtp) intrp)
      (insert *mon-default-comment-divider*)
      *mon-default-comment-divider*))
;;
;;; :TEST-ME (mon-comment-divider t)
;;; :TEST-ME (mon-comment-divider)
;;; :TEST-ME (call-interactively 'mon-comment-divider)

;;; ==============================
;;; :TODO In lieu of the refactoring of `mon-comment-divider' functions 
;;; E.g. `*mon-default-comment-start*', `mon-comment-divider-w-len' 
;;; `*mon-default-comment-divider*', etc. some of this fncns routines may be
;;; formulated differently and/or ommitted. For example, the local var `dvdr'
;;; can now rebind temporarily `*mon-default-comment-start*' i.e.:
;;; (let ((*mon-default-comment-start* "%% "))
;;;             (mon-comment-divider-w-len 30))         
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T14:25:06-04:00Z}#{09436} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-25T19:03:44-04:00Z}#{09352} - by MON KEY>
(defun mon-comment-divider-to-col (to-col &optional start end op-prefix insrtp intrp)
  "Return region or next line with `mon-comment-divider' indented TO-COL.
When START and END are non-nil or called-interactively and region is active 
indent and comment content of region to COL-N, else indent and comment next line.
When op-prefix \(a string\) is non-nil or called-interactively with prefix arg 
use a an alternative comment prefix for region or next line and the comment divider
default prefix is `;' as per `mon-comment-divider' which normally returns:\n
`;;; =============================='\n
Here a prefix `;; ' is used in subesequent line/region according lisp convention.\n
Where the OP-PREFIX arg is non-nil for example using `*' comment divider is:\n
`*** =============================='\n
with subesequent line or region commented with `** '. 
When INSRTP is non-nil insert commented region at point, doesn't move point.\n
:SEE `mon-comment-lisp-to-col' for a specifically lisp centric interactive
implementation.\n
:SEE-ALSO `*mon-default-comment-divider*', `mon-line-strings-indent-to-col',
`mon-line-indent-from-to-col', `mon-line-strings-pipe-to-col',
`mon-string-fill-to-col', `mon-string-justify-left', `mon-split-designator'.\n►►►"
  (interactive "i\nr\nP\ni\np")
  (let ((mcd2c-s-frm   (make-marker))
        (mcd2c-s-ident-at (make-marker))
        (mcd2c-e-ident-at (make-marker))
        (mcd2c-prfx      
         (cond ((and intrp op-prefix)
                (concat 
                 (make-string 2 (string-to-char (substring (read-string "Use alternative prefix: ") 0 1))) 
                 " "))
               ((and op-prefix (not intrp))
                (concat (make-string 2 (string-to-char (substring op-prefix 0 1))) " "))
               (t ";; ")))
        (mcd2c-2col (cond ((and intrp (or (not to-col) (not (numberp to-col))))
                     (read-number "Indent to column number: "))
                    ((and (not intrp) (not (numberp to-col)))
                     (error (concat ":FUNCTION `mon-comment-divider-to-col' "
                                    "-- Arg to-col must be a number but got: %s") to-col))
                    (t to-col)))
        mcd2c->colN mcd2c-w/reg)
    (cond ((use-region-p)
           (set-marker mcd2c-s-frm    (region-beginning))
           (set-marker mcd2c-s-ident-at  (region-beginning))
           (set-marker mcd2c-e-ident-at  (region-end)))
          ((and start end)
           (set-marker mcd2c-s-frm    start)
           (set-marker mcd2c-s-ident-at  start)
           (set-marker mcd2c-e-ident-at  end))
          ((and intrp (not (use-region-p)) (not start) (not end))
           (set-marker mcd2c-s-frm    (point))
           (set-marker mcd2c-s-ident-at  (line-beginning-position 2))
           (set-marker mcd2c-e-ident-at  (line-end-position 2)))
          ((not intrp)
           (set-marker mcd2c-s-frm    (line-beginning-position))
           (set-marker mcd2c-s-ident-at  (line-beginning-position 2))
           (set-marker mcd2c-e-ident-at  (line-end-position 2))))
    (setq mcd2c-w/reg (buffer-substring-no-properties mcd2c-s-ident-at mcd2c-e-ident-at))
    (setq mcd2c->colN (with-temp-buffer
                  (let ((cln-wspc-s (make-marker))
                        (cln-wspc-e (make-marker))
                        (mcd2c-dvdr (concat 
                               (substring mcd2c-prfx 0 1) 
                               mcd2c-prfx 
                               ;; :NOTE This was brittle.
                               ;; Would break if user has mcd2c-prfx other than ";;; ".
                               ;; Two new variables created which help lets us get around that.
                               ;; `*mon-default-comment-start*', `*mon-default-comment-divider*'
                               (substring *mon-default-comment-divider* (length *mon-default-comment-start*))))
                                                 
                        (mcd2c-more t)
                        ;; mcd2c-pad ;; Unused
                        mcd2c-ssf)
                    (insert mcd2c-w/reg)
                    (mon-g2be -1)
                    (insert mcd2c-dvdr)
                    (open-line 1)
                    (mon-g2be -1)
                    (while mcd2c-more
                      (when (= (point) (mon-g2be -1 t)) 
                        (indent-to-column mcd2c-2col) (line-move 1 t))
                      (beginning-of-line)
                      (set-marker cln-wspc-s (point))
                      (setq mcd2c-ssf (skip-syntax-forward "-"))
                      (set-marker cln-wspc-e (point))
                      (when (> mcd2c-ssf 0) (delete-region cln-wspc-s cln-wspc-e))
                      (goto-char cln-wspc-s)
                      (insert mcd2c-prfx)
                      (insert (make-string (cond ((> mcd2c-ssf 3) (- mcd2c-ssf 3))
                                                 ((<= mcd2c-ssf 3) 0)) 32))
                      (goto-char cln-wspc-s)
                      (indent-to-column mcd2c-2col)
                      (end-of-line)
                      (if (eobp) (setq mcd2c-more nil)
                        (line-move 1 t)))
                    (buffer-string))))
    (if (or insrtp intrp)
        (save-excursion 
          (delete-region mcd2c-s-frm mcd2c-e-ident-at)
          (goto-char mcd2c-s-frm)
          (insert mcd2c->colN))
      mcd2c->colN)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-12-10T14:20:52-05:00Z}#{09504} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-25T18:17:18-04:00Z}#{09352} - by MON KEY>
(defun mon-comment-lisp-to-col (&optional col-n)
  "Insert `mon-comment-divider' at COL-N comment and indent region/next line to col.
COL-N is a prefix arg. When region is active indent and comment content of region 
to COL-N else indent and comment next line. Comment prefix is `;; '.
To provide an alternative comment prefix use `mon-comment-divider-to-col'.\n
:ALIASED-BY `mon-lisp-comment-to-col'\n
:SEE-ALSO `mon-comment-divider', `*mon-default-comment-divider*'.
`mon-line-strings-indent-to-col', `mon-line-indent-from-to-col', 
`mon-line-strings-pipe-to-col', `mon-string-fill-to-col',
`mon-string-justify-left'.\n►►►"
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
Insert `mon-comment-divider' indentented to column 4(four).
Move contents current line forward 1(one) line indent it to column 4(four).\n
:SEE-ALSO `*mon-default-comment-divider*', `mon-string-justify-left',
`mon-line-indent-from-to-col'.\n►►►"
  (interactive)
  (save-excursion
    (open-line 1)
    (line-move 1)
    (beginning-of-line)
    (indent-to-column 4)
    (mon-comment-divider)
    (open-line 1)))

;;; ==============================
;;; :RENAMED `php-comment-divider' -> `mon-insert-php-comment-divider'
(defun mon-insert-php-comment-divider (&optional insrtp intrp)
  "Insert a PHP(C style) comment divider at point.\n
When called-interactively or INSRTP is non-nil insert at point. Moves point.\n
:EXAMPLE\n\n(mon-insert-php-comment-divider)\n
:SEE-ALSO `mon-comment-divider', `mon-comment-divider-to-col-four',
`split-designator', `*mon-default-comment-divider*'.\n►►►"
  (interactive "i\np")
  (if (or insrtp intrp)
      (insert "//***************************//")
    "//***************************//"))
;;
;;; :TEST-ME (call-interactively 'mon-insert-php-comment-divider)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T13:24:27-0400Z - by MON KEY>
(defun mon-insert-user-name-cond (&optional insrtp intrp)
  "Insert a cond template to evaluate user type of current system.\n
Test for `IS-BUG-P' or `IS-MON-P'.\n
:EXAMPLE\n\n(mon-insert-user-name-cond)\n
:SEE-ALSO `mon-user-name-conditionals', `mon-system-type-conditionals',
`IS-MON-SYSTEM-P', `IS-NOT-A-MON-SYSTEM'.\n►►►"
  (interactive "i\np")
  (let ((miunc-unc (concat
                    "\n(cond\n"
                    " ((equal user-real-login-name \""
                    (or (and (intern-soft "IS-BUG-P" obarray) ;; *IS-MON-OBARRAY*
                             (bound-and-true-p IS-BUG-P)
                             (bound-and-true-p *BUG-NAME*)
                             (cadr (assoc 6 *BUG-NAME*)))
                        (user-real-login-name)) 
                    "\") ...do-something-here)\n"
                    " ((equal user-real-login-name \""
                    (or (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
                             (bound-and-true-p IS-MON-P)
                             (bound-and-true-p *MON-NAME*)
                             (cadr (assoc 5 *MON-NAME*)))
                        (user-real-login-name))
                    "\") ...do-something-here))\n")))
    (if (or insrtp intrp)
        (save-excursion (insert miunc-unc))
      miunc-unc)))
;;
;;; :TEST-ME (mon-insert-user-name-cond)
;;; :TEST-ME (mon-insert-user-name-cond t)
;;; :TEST-ME (call-interactively 'mon-insert-user-name-cond)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T13:24:13-0400Z - by MON KEY>
(defun mon-insert-system-type-cond (&optional insrtp intrp)
  "Insert a conditional template to test for system OS type.\n
Currenlty tests for GNU/Linux and windows-nt only.\n
:EXAMPLE\n\n\(mon-insert-system-type-cond\)\n
:SEE `IS-W32-P' and `IS-GNU-P' for constants that return t or nil.\n
:SEE-ALSO `mon-user-name-conditionals', `mon-system-type-conditionals'.\n►►►"
  (interactive "i\np")
  (let ((mistc-stc (concat
              "\n(cond\n"
              " ((equal system-type 'windows-nt) \"I'm a slave to the w32.\")\n"
              " ((equal system-type 'gnu/linux) \"I'm a GNU!\"))\n")))
    (if (or insrtp intrp)
        (save-excursion (insert mistc-stc))
      mistc-stc)))
;;
;;; :TEST-ME (mon-insert-system-type-cond)
;;; :TEST-ME (mon-insert-system-type-cond t)
;;; :TEST-ME (call-interactively 'mon-insert-system-type-cond)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-18T13:56:27-04:00Z}#{09385} - by MON>
(defun mon-insert-regexp-template (&optional insrtp intrp)
  "Return template for quickly writing regexp routines.\n
When INSRTP or called-interactively inserts template at point.\n
:EXAMPLE\n\n(while\n(progn\n  (search-forward-regexp \"\")\n  (replace-match \"\"))\n
:SEE-ALSO `mon-insert-regexp-template-yyyy', `mon-insert-lisp-stamp',
`mon-insert-lisp-doc-eg-xref', `mon-insert-lisp-testme', `mon-insert-copyright',
`mon-insert-file-template'.\n►►►"
  (interactive "i\np")
  (let ((mirt-regexp
	 ";; (while\n(progn\n  (search-forward-regexp \"\")\n  (replace-match \"\"))"))
    (if (or insrtp intrp)
	(save-excursion (newline) (insert mirt-regexp))
      mirt-regexp)))
;;
;;; :TEST-ME (mon-insert-regexp-template t)
;;; :TEST-ME (call-interactively 'mon-insert-regexp-template)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-18T15:41:36-04:00Z}#{09385} - by MON KEY>
(defun mon-insert-regexp-template-yyyy (&optional query-rep insrtp intrp)
  "Insert at point regexps for searching whitespace delimited years 20thC.\n
Regexps are specific to the range 1900-1999.\n
Returned regexp template includes nth numbered capture groups. Regexps defaults
to insertion for programmatic elisp.\n
When INSRTP in non-nil or called-interactively insert a second line beneath the
regexp indicating grouping sections.\n
When QUERY-REP is non-nil returns a regexp to the kill-ring.
The regexp will be in the kill ring and is ready formatted for yanking into 
the minibuffer of a query-replace-regexp prompt. When query-rep argument is
non-nil no regexp is inserted into buffer at point.
Yank it back from the kill-ring if that is what you want.\n
:EXAMPLE\n\n\(mon-insert-regexp-template-yyyy\)\n
:SEE-ALSO `mon-insert-regexp-template', `mon-insert-lisp-stamp',
`mon-insert-lisp-doc-eg-xref', `mon-insert-lisp-testme', `mon-insert-copyright',
`mon-insert-file-template'.\n►►►"
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
		(message 
                 (concat ":FUNCTION `mon-insert-regexp-template-yyyy' "
                         "-- regexp is in the kill ring, Yank it back")))
	       (intrp (save-excursion (newline) (insert dbl-slash)))
	       ((and insrtp query-rep)
		(prog1 (beginning-of-line 2)
		  (save-excursion (insert sngl-slash))))
	       ((or insrtp intrp)
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
;;; :CHANGESET 1935 <Timestamp: #{2010-07-03T11:53:54-04:00Z}#{10266} - by MON KEY>
;;; :CHANGESET 1921
;;; :CREATED <Timestamp: #{2010-06-26T18:06:22-04:00Z}#{10256} - by MON KEY>
(defun mon-lisp-CL-package-complete ()
  "Helper function to complete a CL package name to insert.\n
Attempts to build completions from value of `slime-buffer-package',
`slime-current-connection', and return value of `swank:list-all-package-names'
when `slime-current-connection' is non-nil.\n
:EXAMPLE\n\n\(mon-lisp-CL-package-complete\)\n
:ALIASED-BY `mon-CL-package-complete'\n
:SEE-ALSO `mon-insert-lisp-CL-jump-doc', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-CL-mode-line-template', `mon-insert-lisp-CL-package-template',
`mon-lisp-CL-package-complete', `mon-insert-lisp-CL-debug',
`mon-insert-lisp-CL-eval-when', `mon-insert-lisp-doc-eg-xref',
`mon-insert-lisp-CL-jump-doc', `quicklisp-system-complete',
`mon-help-CL-pkgs'.\n►►►"
  (let ((pkg-cmplt (if (or (slime-current-connection)
                           (buffer-local-value 'slime-mode (current-buffer)))
                       `(,(or slime-buffer-package (slime-current-package))
                         ,@(if (slime-current-connection)
                               (slime-eval `(swank:list-all-package-names t))
                             '("CL-USER")))
                     '("CL-USER")))
        (completion-ignore-case t)
        pkg-cmpltd)
    (setq pkg-cmpltd (completing-read (concat ":FUNCTION `mon-lisp-CL-package-complete' " 
                                              "-- which CL package name: ")
                                      pkg-cmplt))
    (if (or (null pkg-cmpltd)
            (eq pkg-cmpltd t)
            (<= (length (format "%s" pkg-cmpltd)) 1))
        nil
      (upcase pkg-cmpltd))))
;; 
;;; :TEST-ME (mon-lisp-CL-package-complete)

;;; ==============================
;;; :CHANGESET 1917
;;; :CREATED <Timestamp: #{2010-06-26T16:28:00-04:00Z}#{10256} - by MON KEY>
(defun mon-insert-lisp-CL-mode-line-template (&optional cl-package insrtp intrp)
  "Return a Common-Lisp sylte mode-line for insertion at BOF/BOB.\n
When optional arg CL-PACKAGE in non-nil, it is a symbol or string having a
length >= 1 and naming a CL package.\n
When optional INSRTP is non-nil insert modeline at point moving point.\n
When optional arg INTRP is non-nil or called-interactively insert modeline at
BOB but do not move-point.\n
Strip datestrings from  CL-PACKAGE when it matches the regexp:\n
 \(format-time-string \"[-]?%Y-%m-%d$\"\)\n
:EXAMPLE\n\n(mon-insert-lisp-CL-mode-line-template 'cl-bubba\)\n
\(mon-insert-lisp-CL-mode-line-template \"cl-bubba\"\)\n
:ALIASED-BY `mon-insert-CL-mode-line-template'\n
:ALIASED-BY `mon-add-lisp-CL-file-local-prop-template'
:SEE info-node `(emacs)Specifying File Variables'.\n
:SEE :FILE files-x.el lisp/files.el\n
:SEE-ALSO `mon-insert-lisp-CL-jump-doc', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-CL-package-template', `mon-lisp-CL-package-complete',
`mon-insert-lisp-CL-debug', `mon-insert-lisp-CL-eval-when'
`mon-insert-lisp-doc-eg-xref', `mon-insert-lisp-CL-jump-doc',
`generated-autoload-file', `add-file-local-variable-prop-line',
`delete-file-local-variable-prop-line',
`copy-dir-locals-to-file-locals-prop-line', `set-auto-mode', `set-auto-mode-0',
`set-auto-mode-1'.\n►►►"
  (interactive "P\ni\np")
  (let* ((mdln-pkg (when (or cl-package current-prefix-arg)
                     (cond ((or (and current-prefix-arg 
                                     (or intrp (called-interactively-p 'interactive)))
                                ;; kludge for interactive calls from `mon-insert-cl-package-template'
                                ;; (mon-insert-lisp-CL-mode-line-template t t t) 
                                ;; completes and returns but doesn't insert
                                (and cl-package insrtp intrp))
                            (let ((cmplt-pkg (mon-lisp-CL-package-complete)))
                              (unless (or (null cmplt-pkg) 
                                          (<= (length cmplt-pkg) 1))
                                (format " Package: %s" cmplt-pkg))))
                           ((and cl-package 
                                 (not (null cl-package))
                                 (not (eq cl-package t))
                                 (not (<= (length (format "%s" cl-package)) 1)))
                            (format " Package: %s" 
                                    (upcase (format "%s" cl-package)))))))
         (mdln-pkg-no-dt (concat
                          (if mdln-pkg
                              (substring mdln-pkg 0 (string-match-p (format-time-string "[-]?%Y-%m-%d$") mdln-pkg))
                            mdln-pkg)
                          "; "))
         (mdln (format
                ";;-*- Mode: LISP; Syntax: COMMON-LISP;%sEncoding: utf-8; Base: 10 -*-"
                (if (and cl-package mdln-pkg-no-dt) mdln-pkg-no-dt " "))))
    (cond ((and intrp (not insrtp))
           (progn
             (save-excursion (goto-char (buffer-end 0))
                             (insert mdln)
                             (unless (eq (char-after) 10)
                               (newline)))
             (message (concat ":FUNCTION `mon-insert-lisp-CL-mode-line-template' "
                              "-- CL mode-line inserted at BOB"))))
          ((and intrp (not (and cl-package insrtp intrp)))
           (insert mdln))
          ((and cl-package insrtp (not intrp))
           (insert mdln))
          ((or (and cl-package insrtp intrp) t)
           mdln))))
;;
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'cl-bubba)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template "cl-bubba")
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'cl-bubba nil)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'cl-bubba t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template "cl-bubba" t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template t nil)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template nil t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'p)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'pi)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template t t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'cl-bubba nil t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'cl-bubba t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template "cl-bubba" t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template 'cl-bubba nil t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template "cl-bubba" nil t)
;;; :TEST-ME (mon-insert-lisp-CL-mode-line-template t nil t)

;;; ==============================
;;; :NOTE See the :TODO below regarding Quickproject.
;;; :CHANGESET 1935 <Timestamp: #{2010-07-03T11:58:11-04:00Z}#{10266} - by MON KEY>
;;; :CHANGESET 1922 <Timestamp: #{2010-06-26T19:34:08-04:00Z}#{10256} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-02-01T16:42:46-05:00Z}#{10051} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T16:11:16-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:15.02 PM - by MON KEY>
(defun mon-insert-lisp-CL-file-template (&optional insrtp intrp w-pkg-name)
  "Return a file header template for use with Common Lisp.\n
When optional arg INSRTP is non-nil or called-interactively insert template at
top of file.\n
When optional arg W-PKG-NAME is non-nil it is a string or symbol naming a
package as per `mon-insert-lisp-CL-mode-line-template'.\n
:EXAMPLE\n\n\(mon-insert-lisp-CL-file-template\)\n
:ALIASED-BY `mon-insert-CL-file-template'\n
:SEE-ALSO `mon-lisp-CL-package-complete',
`mon-insert-lisp-CL-mode-line-template', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-CL-package-template', `mon-lisp-CL-package-complete',
`mon-insert-lisp-CL-debug', `mon-insert-lisp-CL-eval-when'
`mon-insert-lisp-CL-jump-doc', `mon-insert-lisp-doc-eg-xref',
`mon-insert-lisp-evald',`mon-insert-copyright', `mon-comment-divider',
`mon-comment-divider-to-col-four', `mon-insert-lisp-stamp', `mon-stamp',
`mon-file-stamp', `mon-insert-lisp-doc-eg-xref', `mon-insert-lisp-testme',
`mon-insert-copyright', `mon-insert-file-template'.\n►►►"
  (interactive "i\np\nP")
  (let* ((milcft-modln (if (and w-pkg-name current-prefix-arg)
                           (mon-insert-lisp-CL-mode-line-template t t t)
                         (mon-insert-lisp-CL-mode-line-template w-pkg-name)))
         (milcft-fname (if (not (buffer-file-name)) 
                           (buffer-name)
                         (file-name-nondirectory (buffer-file-name))))
         (milcft-t-str (concat ";;; <Timestamp: " (mon-timestamp :naf t)))
         (milcft-cpyrt  (mon-build-copyright-string nil nil t))
         (milcft-tmplt (concat
                       milcft-modln "\n"
                       (mon-comment-divider-w-len 54) "\n"
                       ";;; This is " milcft-fname "\n"
                       ";;; \n"
                       ";;; FILE-CREATED:\n;;; <Timestamp: " (mon-timestamp :naf t) "\n"
                       ;; (mon-comment-divider-w-len 64) "\n"
                       ;; :WAS *mon-gnu-license-header* "\n"
                       (mon-build-copyright-string-license 'bsd) "\n"
                       (mon-insert-gnu-licence-gfdl t) "\n\n"
                       "\xc\n;;; CODE:\n\n"
                       ";;; \n"
                       "\xc\n"
                       ";;; {...}\n"
                       ";;;\n" 
                       "\xc\n"
                       ";; Local Variables:\n"
                       ;; emacs-lisp/lisp-mode.el has a defalias 'common-lisp-mode -> 'lisp-mode
                       ;; Might as well make use of it :)
                       ";; mode: COMMON-LISP\n"
                       ;; ";; syntax: Common-Lisp\n"
                       ;; ";; base: 10\n"
                       ;; ";; coding: utf-8\n"
                       "indent-tabs-mode: nil\n"
                       ";; End:\n"
                       (mon-file-stamp-vrfy-put-eof  nil)
                       )))
         (when (or insrtp intrp)
           (progn   
             (save-excursion
               (mon-g2be -1)
               (insert milcft-tmplt)
               (mon-g2be 1)
               ;;(insert "\n"
               )))
           milcft-tmplt))
;;
;;; :TEST-ME (mon-insert-lisp-CL-file-template)
;;; :TEST-ME (mon-insert-lisp-CL-file-template t)
;;; :TEST-ME (call-interactively 'mon-insert-lisp-CL-file-template)

;;; ==============================
;;; :COURTESY Zach Beane :HIS scratch-lisp-file.el
;;; :SEE (URL `http://www.xach.com/lisp/scratch-lisp-file.el')
;;; :TODO Incorporate with Zach Beane's Quickproject and remove the Emacs
;;; Lisp side which it might otherwise better accomodate. Quickproject creates a
;;; skeleton of a Common Lisp project by automatically creating several files.
;;; :SEE (URL `http://github.com/xach/quickproject/')
;;; :SEE (URL `git://github.com/xach/quickproject.git')
;;; :SEE (URL `http://xach.livejournal.com/269028.html')
;;; :CHANGESET 1922 <Timestamp: #{2010-06-26T19:33:58-04:00Z}#{10256} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: Tuesday July 14, 2009 @ 02:12.25 AM - by MON KEY>
(defun mon-insert-lisp-CL-package-template (&optional insrtp without-header intrp
                                                 package-nm)
  "Return or insert a CL package-template a template.\n
Builds template with `DEFPACKAGE' and `IN-PACKAGE' forms for the current buffer.
When called-interactively or INSRTP non-nil assumes current buffer is empty and
insert a file template with `mon-insert-lisp-CL-file-template'.\n
When optional arg PACKAGE-NM is non-nil it is a symbol or string naming a
package.\n
If ommited package name for template defaults to `buffer-file-name' if buffer is
visiting a file else defaults to `buffer-name'.\n
When either PACKAGE-NM or buffer-name contain a datestring matching the regexp:\n
 \(format-time-string \"[-]?%Y-%m-%d$\"\)\n
the date string will be stripped.
:EXAMPLE\n\n(mon-insert-lisp-CL-package-template)\n
\(mon-insert-lisp-CL-package-template nil nil nil \"cl-bubba-as-string\"\)\n
:ALIASED-BY `mon-insert-CL-package-template'\n
:SEE-ALSO `mon-insert-lisp-CL-mode-line-template',
`mon-lisp-CL-package-complete', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-CL-jump-doc', `mon-insert-lisp-doc-eg-xref',
`mon-insert-lisp-CL-debug', `mon-insert-lisp-CL-eval-when',
`mon-insert-file-template', `mon-insert-copyright', `mon-insert-lisp-stamp',
`mon-insert-lisp-testme', `mon-insert-lisp-evald'.\n►►►"
  (interactive "i\nP\np")
  (let* ((micpt-fname (cond ((and package-nm intrp (not insrtp))
                             (if (or (stringp package-nm)
                                     (not (eq package-nm t))) ;; test for t
                                 (format "%s" package-nm)
                               (mon-lisp-CL-package-complete)))
                            ((and package-nm                     
                                  (not (eq package-nm t))
                                  (>= (length (format "%s" package-nm)) 2))
                             (format "%s" package-nm))
                            ((not (buffer-file-name)) (buffer-name))
                            (t (file-name-nondirectory (buffer-file-name)))))
         (micpt-pkg (let* ((mict-pkg-nm-or-fnm
                            (if package-nm 
                                micpt-fname
                              (file-name-sans-extension micpt-fname)))
                           (mict-pkg-srp-dt
                            (substring mict-pkg-nm-or-fnm 0 
                                       (string-match-p (format-time-string "[-]?%Y-%m-%d$") mict-pkg-nm-or-fnm))))
                           mict-pkg-srp-dt))
         (micpt-tmplt (concat ";;;; " micpt-fname "\n"
                              "\n(in-package :cl-user)\n"
                              "\n(defpackage #:" micpt-pkg "\n  (:use :common-lisp))\n\n"
                              "(in-package :" micpt-pkg ")\n"
                              ";; (:nicknames #:<SOME-NICKNAME>*)\n"
                              ";; (:export #:<EXPORTED-SYMBOL-NAME>*)\n"
                              ";; (:import-from package-name :<PKG-NM> #:<SYMBOL-NAME>*)\n"
                              ";; (:shadow #:<SHADOWED-SYMBOL>*)\n"
                              ";; (:shadowing-import-from :<PKG-NM> #:<SYMBOL-NAME>*)\n"
                              ;; (:intern #:<STRING-NAMING-SYMBOL>*)
                              ;; (:documentation <"DOC-STRING">)
                              ;; (:intern {symbol-name}*)* |  
                              ;; (:size integer) 
                              )))
    (if (or insrtp intrp)
        (save-excursion
          (cond ((not without-header)
                 (mon-insert-lisp-CL-file-template t nil micpt-pkg)
                 (if (search-backward-regexp "^;;; CODE:$" nil t)
                     (progn (beginning-of-line 2)
                            (newline)
                            (insert micpt-tmplt))
                   (if (search-backward-regexp "^;;; {...}$" nil t)
                       (progn (beginning-of-line)
                              (newline)
                              (insert micpt-tmplt))
                     (if (search-forward-regexp "^;;; EOF$" nil t)
                         (progn
                           (line-move -3)
                           (end-of-line)
                           (newline)
                           (insert micpt-tmplt))))))
                (without-header (mon-g2be -1)
                                ;; To insert a mode-line, don't insert a package name,
                                ;; and don't look/prompt for it use:
                                ;; (mon-insert-lisp-CL-mode-line-template t t)
                                (mon-insert-lisp-CL-mode-line-template micpt-pkg t)
                                (newline)
                                (insert micpt-tmplt))))
      micpt-tmplt)))
;;
;;; :TEST-ME (mon-insert-lisp-CL-package-template)
;;; :TEST-ME (mon-insert-lisp-CL-package-template t)
;;; :TEST-ME (mon-insert-lisp-CL-package-template nil nil t "cl-bubba")
;;; :TEST-ME (call-interactively 'mon-insert-lisp-CL-package-template)

;;; ==============================
;;; :CHANGESET 2325
;;; :CREATED <Timestamp: #{2010-11-21T09:10:46-05:00Z}#{10467} - by MON>
(defun mon-insert-lisp-CL-eval-when (&optional no-insert)
  "Insert a Common Lisp `eval-when' template at BOL.\n
Return inserted template.\n
If after insertion current sexp is preceded by inserted template indent it as if
by `indent-for-tab-command' where feasible. Does not move point.\n
Inserts following form \(including the newline\):\n
 \"\(eval-when \(:compile-toplevel :load-toplevel :execute\)\\n\"\n
:EXAMPLE\n\n\(mon-insert-lisp-CL-eval-when t\)\n
\(mon-with-inhibit-buffer-read-only 
    \(forward-line 1\)
    \(mon-insert-lisp-CL-eval-when\)\)\n
\(mon-with-inhibit-buffer-read-only 
    \(forward-line 2\)
    \(mon-insert-lisp-CL-eval-when\)\)

\(this form is indent\)\n
;; Mimic invocation as command with prefix-arg NO-INSERT non-nil:
\(let \(\(current-prefix-arg \(kbd \"M-x 2\"\)\)\)
  \(with-output-to-string 
    \(call-interactively #'mon-insert-lisp-CL-eval-when nil\)\)\)\n
:ALIASED-BY `mon-insert-CL-eval-when'\n
:SEE-ALSO `mon-insert-lisp-CL-mode-line-template',
`mon-insert-lisp-CL-file-template', `mon-insert-lisp-CL-package-template',
`mon-lisp-CL-package-complete', `mon-insert-lisp-CL-debug',
`mon-insert-lisp-CL-eval-when' `mon-insert-lisp-doc-eg-xref',
`mon-insert-lisp-CL-jump-doc'.\n►►►"
  (interactive "P")
  (let ((micew-tmplt "(eval-when (:compile-toplevel :load-toplevel :execute)\n"))
    (and (or (and (not no-insert)
                  (save-excursion 
                    (and (not (bolp)) 
                         (beginning-of-line))
                    (insert micew-tmplt)
                    (indent-for-tab-command)))
             t)
         (or (and (called-interactively-p 'interactive)
                  (or (and current-prefix-arg (prin1 (substring micew-tmplt 0 -1)))
                      (minibuffer-message (substring micew-tmplt 0 -1))))
             (substring micew-tmplt 0 -1)))))

;;; ==============================
;;; :CHANGESET 2325
;;; :CREATED <Timestamp: #{2010-11-21T15:27:50-05:00Z}#{10467} - by MON>
(defun mon-insert-lisp-CL-debug (&optional no-insert)
  "Insert a Common Lisp `eval-when' template at BOL.\n
Return inserted template. 
If after insertion current sexp is preceded by inserted template indent it as if
by `indent-for-tab-command' where feasible. Does not move point.\n
Inserts following form \(including the newline\):\n
 \"\(declare \(optimize \(speed 0\) \(safety 0\) \(compilation-speed 0\) \(debug 3\)\)\)\\n\"\n
:EXAMPLE\n\n
:ALIASED-BY `mon-insert-CL-debug'\n
:SEE-ALSO `mon-insert-lisp-CL-eval-when',
`mon-insert-lisp-CL-mode-line-template', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-CL-package-template', `mon-lisp-CL-package-complete',
`mon-insert-lisp-CL-debug', `mon-insert-lisp-doc-eg-xref',
`mon-insert-lisp-CL-jump-doc'.\n►►►"
  (interactive "P")
  (let ((micd-bg 
         "(declare (optimize (speed 0) (safety 0) (compilation-speed 0) (debug 3)))\n"))
    (and (or (and (not no-insert)
                  (save-excursion 
                    (and (not (bolp)) 
                         (beginning-of-line))
                    (insert micd-bg )
                    (indent-for-tab-command)))
             t)
         (or (and (called-interactively-p 'interactive)
                  (or (and current-prefix-arg (prin1 (substring micd-bg 0 -1)))
                      (minibuffer-message (substring micd-bg 0 -1))))
             (substring micd-bg 0 -1)))))

;;; ==============================
;;; :KEYBINDING "\C-c\C-dc" in :FILE mon-keybindings.el
;;; :CREATED <Timestamp: #{2009-10-24T18:33:41-04:00Z}#{09436} - by MON>
(defun mon-insert-lisp-doc-eg-xref (&optional insrtp intrp as-kill)
  "Return documentation keywords for insertion to dostrings.\n
Default is to return with prin1.\n
When INTRP is non-nil or called-interactively return as with princ.\n
When INSRTP is non-nil return for as with prin1.\n
When AS-KILL is non-nil or called-interactively with prefix arg put return
value on the kill-ring.\n
:EXAMPLE\n\n(let ((inhibit-read-only t)
                  (o-frm (1+ (point)))
                  o-to)
              (unwind-protect 
                  (progn
                    (mon-insert-lisp-doc-eg-xref nil t)
                    (forward-sexp) (setq o-to (1+ (point)))
                    (mon-help-overlay-result 
                     o-frm o-to ?Q (mon-buffer-sub-no-prop o-frm o-to)))
                (delete-region o-frm (or o-to (point)))))\n
:NOTE When current-buffer's major mode `lisp-mode' or `lisp-interaction-mode'
with `slime-mode' enabled and `slime-current-connection' is non-nil return value
is as per Common-Lisp docstrings and use CL format spec `~%' for newlines and is
prefixed with:\n
 \(setf \(documentation '<SYM> '<DOC-TYPE>\)
     #.\(format\"\n
With the full return value having the format:\n
 \(setf \(documentation '<SYM> '<DOC-TYPE>\)
     #.\(format nil
    \" <DOCSTR> ~%~@
 :EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
 :SEE-ALSO `<XREF>'.~%►►►\"\)\)\n
The slot templates '<SYM> and '<DOC-TYPE> are inserted literally with the intent
that these be be manually adjusted according to context, such that where <SYM> is
a symbol-name or list of the form '\(setf <SYM>\) and <DOC-TYPE> is a type
specifier indicating symbol-name's documentation type, being one of:\n
 { <VARIABLE> | <FUNCTION> | <STRUCTURE> | <TYPE> | 
   <SETF> | <METHOD-COMBINATION> | <COMPILER-MACRO> | <T> }\n
one would replace <SYM> a symbol-name with 'some-symbol and <DOC-TYPE> as 'some-type.\n
So, for example a <DOC-TYPE> argument specifying 'function is applicable when <SYM> is
a function, macro, generic-function, or special-form.\n
A <DOC-TYPE> argument specifying 'setf is applicable when <SYM> has an associated
`defsetf' or `define-setf-method' definition.\n
A <DOC-TYPE> argument specifying 'variable is applicable when <SYM> names a
variable or constant.\n
A <DOC-TYPE> argument specifying 'variable is applicable when <SYM> names a
variable or constant.
The remaining <DOC-TYPE> specifiers are as per the spec.
:SEE info-node `(ansicl)documentation; (setf documentation)'\n
:ALIASED-BY `mon-insert-doc-xref-eg'\n
:SEE-ALSO `mon-insert-lisp-CL-jump-doc', `mon-insert-lisp-doc-eg-xref',
`mon-insert-lisp-CL-mode-line-template', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-CL-package-template', `mon-lisp-CL-package-complete',
`mon-insert-lisp-CL-debug', `mon-insert-lisp-CL-eval-when'
`mon-insert-lisp-stamp', `mon-insert-lisp-testme',
`mon-insert-lisp-evald'.\n►►►"
  (interactive "i\np\nP")
  ;; :NOTE Consider refactoring to use following instead: 
  ;; `mon-buffer-check-local-value', `mon-buffer-check-major-mode', 
  (let* ((not-elisp (case (buffer-local-value 'major-mode (current-buffer))
                      (lisp-interaction-mode t)
                      (lisp-mode t)
                      (t (and (buffer-local-value 'slime-mode (current-buffer))
                              (slime-current-connection)))))
         (mildeg-xref (if not-elisp
                          (concat "(setf (documentation '<SYM> '<DOC-TYPE>)\n"
                                  "      #.(format nil\n"
                                  "\" <DOCSTR> ~%~@\n"
                                  ":EXAMPLE~%~@\n"
                                  " { ... <EXAMPLE> ... } ~%~@\n"
                                  ":SEE-ALSO `<XREF>'.~%►►►\"))")
                        (concat "\"\n:EXAMPLE\\n\\n"  "\n" ":SEE-ALSO .\\n►►►\""))))
    (cond (intrp (save-excursion 
                   (newline)
                   (princ mildeg-xref (current-buffer)))
                 (when as-kill (kill-new mildeg-xref)))
          (insrtp (prin1 mildeg-xref (current-buffer))
                  (when as-kill (kill-new mildeg-xref)))
          (t (when as-kill (kill-new mildeg-xref))
             mildeg-xref))))
;;
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref)
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref nil nil t)
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref t)
;;; :TEST-ME (mon-insert-lisp-doc-eg-xref nil t)


;;; ==============================
;;; :CHANGESET 2170
;;; :CREATED <Timestamp: #{2010-10-01T18:19:44-04:00Z}#{10395} - by MON KEY>
(defun mon-insert-lisp-CL-jump-doc ()  
  "Insert or jump to a CL documentation template for symbol at/around point.\n
Documentation template is as per return value of `mon-insert-lisp-doc-eg-xref'.\n
If a doc-template is found set register D to position of symbol's defining form
jump to the documentation form moving point.\n
If a doc-template is not found insert a documentation form at end of symbol's
defining form moving point.\n
Signal an error if current-buffer's `major-mode' is `emacs-lisp-mode' or if
`slime-mode' is not enabled or major-mode is not either `lisp-mode' or
`lisp-interaction-mode'.\n
When `IS-MON-SYSTEM-P' bound in `slime-mode-map' by `mon-keybind-slime'.\n
:ALIASED-BY `mon-insert-jump-lisp-doc'\n
:SEE-ALSO `mon-insert-lisp-CL-package-template', `*regexp-symbol-defs-big*'
`mon-insert-lisp-CL-mode-line-template', `mon-insert-lisp-CL-file-template',
`mon-lisp-CL-package-complete', `mon-insert-lisp-CL-debug',
`mon-insert-lisp-CL-eval-when', `mon-insert-lisp-doc-eg-xref',
`mon-insert-lisp-CL-jump-doc'.\n►►►"
  (interactive)
  (eval-when-compile (require 'thingatpt))
  ;;
  ;; :NOTE Consider refactoring to use following instead:
  ;; `mon-buffer-check-local-value', `mon-buffer-check-major-mode', 
  (unless (and (not (eq major-mode 'emacs-lisp-mode)) 
               (or (buffer-local-value 'slime-mode (current-buffer))
                   (eq major-mode 'lisp-mode)
                   (eq major-mode 'lisp-interaction-mode)))
    (error (concat ":FUNCTION `mon-insert-lisp-CL-jump-doc' "
                   "-- current major-mode not relevant to this functions return value")))
  (let ((pre-nrrw-mrk (make-marker))
        (in-nrrw-mrk  (make-marker))
        (mvd-mrk      (make-marker))
        (srch-doc     "^\\((setf (documentation '\\)")
        bodfn
        doc-mrk
        is-var
        caught-sym-nm)
    ;; To deterimine later if narrowing is in effect.
    (set-marker pre-nrrw-mrk (mon-g2be -1 t))
    (unwind-protect
        (progn 
          (set-marker mvd-mrk (point))
          (ignore-errors
            (end-of-defun)
            (narrow-to-defun)
            (setq bodfn (beginning-of-defun)))
          (cond ((and bodfn (not (eq (marker-position mvd-mrk) (point))))
                 (setq mvd-mrk t)
                 (set-marker in-nrrw-mrk (point)))
                ((and bodfn (eq (marker-position mvd-mrk) (point)))
                 (set-marker in-nrrw-mrk (marker-position mvd-mrk))
                 (setq mvd-mrk t)))
          ;; (cond (mvd-mrk
          ;;        ;; ((and (not bodfn)
          ;;        ;;       (or (= (marker-position pre-nrrw-mrk) (point))
          ;;        ;;           (= (marker-position mvd-mrk) (point))))
          ;;        ;; (setq mvd-mrk)
          ;;        (set-marker in-nrrw-mrk (point))))
          (when mvd-mrk
            (or (and (save-match-data 
                       (looking-at *regexp-symbol-defs-big*)
                       (setq caught-sym-nm (match-string-no-properties 3)))
                     ;; If caught-sym-nm is prfxd with *, +, ?, ., etc. we need to `regexp-quote' it
                     (progn 
                       (when (memq (aref caught-sym-nm 0) '(42 43 46 63)) (setq is-var t))
                       t))
                (let* ((myb-fc-prps
                        (mon-get-text-properties-region-prop 'face 
                                                             (line-beginning-position) 
                                                             (line-end-position)))
                       (asqc-fc-prps (and myb-fc-prps
                                          (or (assq 'font-lock-function-name-face myb-fc-prps)
                                              (and (assq 'font-lock-variable-name-face myb-fc-prps)
                                                   (setq is-var t))
                                              (assq 'font-lock-type-face myb-fc-prps))))
                       (loc-w-fc-prps (and asqc-fc-prps (cadr asqc-fc-prps)))
                       (str-at-fc-prps (and loc-w-fc-prps
                                            (buffer-substring-no-properties (car loc-w-fc-prps)
                                                                            (cdr loc-w-fc-prps)))))
                  (and str-at-fc-prps (setq caught-sym-nm str-at-fc-prps)))))
          (widen)
          (cond ((and mvd-mrk caught-sym-nm)
                 (save-excursion
                   (when (search-forward-regexp 
                          (if is-var (concat srch-doc (regexp-quote caught-sym-nm))
                            (concat srch-doc caught-sym-nm))
                            nil t)
                     (setq doc-mrk (point))
                     (set-register ?D in-nrrw-mrk)))
                 (if doc-mrk
                     (prog1 
                         (message (concat ":FUNCTION `mon-jump-to-cl-doc' "
                                          "jumped to documentation form for symbol: `%s'\n"
                                          "Location of symbols defining form stored in register D, "
                                          "to return there type %s D") 
                                  caught-sym-nm (substitute-command-keys "\\[jump-to-register]"))
                       (progn (goto-char doc-mrk)
                              (skip-syntax-forward "^\"")))
                   (progn
                     (goto-char in-nrrw-mrk)
                     (end-of-defun)
                     (newline)
                     (insert (mon-insert-lisp-doc-eg-xref))
                     (newline)
                     (if (search-backward-regexp (concat srch-doc "\\(.* '\\)") 
                                                 (car (save-match-data (bounds-of-thing-at-point 'sexp))) t)
                         (progn (replace-match (concat (match-string-no-properties 1) caught-sym-nm " '"))
                                (skip-syntax-forward "^\""))
                       (progn
                         (skip-syntax-backward "^\"" (car (save-match-data (bounds-of-thing-at-point 'sexp))))
                         (backward-sexp)))
                     (message (concat ":FUNCTION `mon-jump-to-cl-doc' "
                                      "could not locate symbol `%s's documentation form. "
                                      "Inserted doc form at `end-of-defun'")
                              caught-sym-nm))))
                (mvd-mrk
                 (end-of-defun)
                 (newline)
                 (insert (mon-insert-lisp-doc-eg-xref))
                 (newline)
                 (skip-syntax-backward "^\"" (car (save-match-data (bounds-of-thing-at-point 'sexp))))
                 (message (concat ":FUNCTION `mon-jump-to-cl-doc' "
                                  "could not locate a symbol nor its documentation form. "
                                  "Inserted doc form at `end-of-defun'")))
                ((not mvd-mrk)
                 (goto-char in-nrrw-mrk))))
      (widen))))

;;; ==============================
;;; :CREATED <Timestamp: Friday June 12, 2009 @ 12:24.26 PM - by MON KEY>
(defun mon-insert-lisp-stamp (&optional insrtp intrp modifications)
  "Return or insert at point a `mon-comment-divider' newline and `mon-stamp'.\n
When INSRTP is non-nil or called interactively insert at point.\n
When MODIFICATIONS is non-nil or called interactively with prefix arg
Prepend return value with ';;; :MODIFICATIONS ' prefix.\n
The default is to return with only ';;; :CREATED '\n
MON uses to delimit and date newly created/modified procedures.\n
:EXAMPLE\n\n\(mon-insert-lisp-stamp\)\n\n\(mon-insert-lisp-stamp nil nil t\)\n
:NOTE MON limits use of the `;;; :MODIFICATIONS' prefix to situations where a
code change may be breaking or otherwise alters the semantics of the procedure.\n
:SEE-ALSO `mon-lisp-stamp',`mon-file-stamp', `mon-insert-copyright',
`mon-insert-lisp-testme', `mon-insert-lisp-CL-file-template',
`mon-comment-divider', `mon-comment-divider-to-col-four', `mon-insert-lisp-evald',
`mon-insert-lisp-doc-eg-xref'.\n►►►"
  (interactive "i\np\nP")
      (if (or insrtp intrp)
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
;;; :CHANGESET 1841 <Timestamp: #{2010-06-10T14:45:00-04:00Z}#{10234} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T13:05:47-04:00Z}#{09436} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T16:04:36-04:00Z}#{09411} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:44:23-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:41.53 PM - by MON KEY>
(defun mon-build-copyright-string (&optional insrtp intrp monkey no-nl w-org short-form)
  "Return a copyright string built conditionally on users name.\n
When INSRTP is non-nil or called interactively insert copyright at point.
On MONish systems when MONKEY is non-nil or called-interactively with prefix arg
use longform. Default is to abbreviate MONish names.\n
When NO-NL is non-nil do not return with newlines.
Default is with newline concatenated to head and tail of return string.\n
When W-ORG is non-nil return with an institutional C/O string appended.\n
When short form is non-nil do not pad the comment delim to col 68.\n
Construct Comment delims `mon-comment-divider-w-len' padded to 34, 44, or 54
chars depending value of arg MONKEY or W-ORG.\n
:EXAMPLE\n\n\(mon-build-copyright-string-TEST\)\n
:SEE-ALSO `mon-insert-copyright'.\n►►►"
  (interactive "i\np\nP") 
  (let ((mbcs-nm (cond (monkey (cond ((and (intern-soft "IS-MON-P")
                                           (bound-and-true-p IS-MON-P))
                                      (cadr (assoc 6 *MON-NAME*)))
                                     ((and (intern-soft "IS-BUG-P")
                                           (bound-and-true-p IS-BUG-P))
                                      (cadr (assoc 5 *BUG-NAME*)))
                                     (t (upcase user-real-login-name))))
                       ((and (intern-soft "IS-MON-P")
                             (bound-and-true-p IS-MON-P))
                        (cadr (assoc 7 *MON-NAME*)))
                       ((or (and (intern-soft "IS-BUG-P")
                                 (bound-and-true-p IS-BUG-P))
                            (and (intern-soft "IS-BUG-P-REMOTE")
                                 (bound-and-true-p IS-BUG-P-REMOTE)))
                        (cadr (assoc 1 *BUG-NAME*)))  
                       (t  (or (unless (eq (length user-full-name) 0) (upcase user-full-name))
                               (upcase user-real-login-name)
                               "<NAME>"))))
        (w-org ;; This local is wrong, but renaming it is touchy.
         (let ((chk-org-name
                (or (and (intern-soft "*MON-ORG-NAME*")
                         (bound-and-true-p *MON-ORG-NAME*))
                    ;; :NOTE Could default to using hostname here, e.g. (system-name)
                    '((1 " <SOME-ORGANIZATION>") (2 " <SOME-ORGANIZATION>")))))
           (cond (monkey (if w-org 
                             (substring (cadr (assoc 1 chk-org-name)) 1)
                           ""))
                 (w-org (cadr (assoc 2 chk-org-name))))))
        (mbcs-yr (mon-get-current-year)) ;(format-time-string "%Y"))
        (mbcs-cmnt-cls (cond ((and short-form (not monkey) (not w-org))
                              *mon-default-comment-divider*)      ;; 34
                             ((and short-form monkey (not w-org))
                              *mon-default-comment-divider*)      ;; 34
                             ((and short-form (not monkey) w-org)
                              (mon-comment-divider-w-len 40))     ;; 44
                             ((and short-form monkey w-org) 
                              (mon-comment-divider-w-len 50))     ;; 54
                             (t (mon-comment-divider-w-len 64)))) ;; 68
        mbcs-cpy)
    (setq mbcs-cpy
          (if no-nl
              (concat
               mbcs-cmnt-cls "\n"
               ";; Copyright © " mbcs-yr " " mbcs-nm " "  w-org "\n"
               mbcs-cmnt-cls)
            (concat
             "\n" mbcs-cmnt-cls "\n"
             ";; Copyright © "  mbcs-yr " " mbcs-nm " " w-org "\n"
             mbcs-cmnt-cls "\n")))
    (unless (and w-org short-form)
      ;; Don't bother with re-entrant tricks here...
      (setq mbcs-cpy (replace-regexp-in-string " $" ". All rights reserved." mbcs-cpy)))
    (when (or insrtp intrp) 
      (save-excursion (insert mbcs-cpy)))
    mbcs-cpy))
;;
;;; &OPTIONAL insrtp intrp monkey no-nl w-org short-form
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
;;; :TEST-ME (let ((IS-MON-P nil)) (mon-build-copyright-string))
;;; :TEST-ME (let ((IS-MON-P nil)) (mon-build-copyright-string nil nil t))
;;; :TEST-ME (let ((IS-MON-P nil) (IS-BUG-P t)) (mon-build-copyright-string nil nil t))
;;; :TEST-ME (call-interactively 'mon-build-copyright-string)

;;; ==============================
;;; :CREATED <Timestamp: Saturday July 11, 2009 @ 12:35.37 PM - by MON KEY>
;;; :DEPRECATED :USE (mon-build-copyright-string t)
(defun mon-insert-copyright (&optional w-user w-short-form insrtp intrp)
  "Insert copyright with relevant details.\n
Conditional on user's system name.\n
When W-USER is non-nil use longform of MONish username.
Default is abbreviated nameform.\n
When W-SHORT-FORM is non-nil insert with 34 char length comment divider.
Default is to return with 68 char length comment dividers.\n
:ALIASED-BY `bug-insert-copyright'\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-build-copyright-string', `mon-build-copyright-string-license',
`mon-build-copyright-string-TEST', `mon-insert-file-template',
`mon-insert-lisp-doc-eg-xref', `mon-insert-gnu-licence-gfdl',
`mon-insert-gnu-licence', `*mon-mit-license-header*',
`*mon-bsd-license-header*', `*mon-gnu-license-header-gfdl*',
`*mon-gnu-license-header*'.\n►►►" 
  (interactive "p\nP\ni\np")
  (if (or insrtp intrp)
      (save-excursion 
        (mon-build-copyright-string t nil w-user nil nil w-short-form))
    (mon-build-copyright-string nil nil w-user nil nil w-short-form)))
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
;;; :TODO This procedure should be refactored to accomodate other types of
;;;  ``known'' files.  If this is dont the curent elisp specific version should
;;;  be renamed `mon-insert-file-template-elisp'
;;;
;;; :RENAMED `mon-insert-naf-mode-file-template' -> `mon-insert-file-template'
;;; :MODIFICATIONS <Timestamp: #{2010-03-03T15:34:05-05:00Z}#{10093} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-02-01T16:33:23-05:00Z}#{10051} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-24T13:49:28-04:00Z}#{09436} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-05T15:53:51-04:00Z}#{09411} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-02T16:06:03-04:00Z}#{09405} - by MON KEY>
;;; :CREATED <Timestamp: Thursday April 09, 2009 @ 05:52.10 PM - by MON KEY>
(defun mon-insert-file-template (&optional w-fname insrtp intrp)
  "Insert an elisp file template.\n
Template includes GPLv3+ clause from `*mon-gnu-license-header*'.\n
GFDLv1.3 clause w/ Copyright <YYYY> <NAME> from: `*mon-gnu-license-header-gfdl*'\n
:EXAMPLE\n\n(mon-insert-file-template)\n
:NOTE Ideally, filenames are < 13 characters (extension inclusive).
This allows compiled filename lengths of 14 characters or less and
helps ensure multi-os portability.\n
:SEE-ALSO `mon-insert-texi-template', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-doc-eg-xref', `mon-file-stamp', `mon-insert-gnu-licence',
`mon-insert-gnu-licence-gfdl', `mon-insert-copyright',
`mon-build-copyright-string-license', `mon-build-copyright-string',
`mon-build-copyright-string-TEST', `*mon-bsd-license-header*',
`*mon-mit-license-header*', `*mon-gnu-license-header-gfdl*',
`*mon-gnu-license-header*'.\n►►►"
  (interactive "i\nP\np")  
  (let* ((mift-cur-nm (buffer-name))
         (mift-fname (cond ((and w-fname (not intrp))
                       (if (stringp w-fname)
                           (concat (file-name-sans-extension w-fname) ".el")
                           (error (concat ":FUNCTION `mon-insert-file-template' " 
                                          "-- arg w-fname not a string"))))
                      ((and intrp w-fname)
                       (concat (file-name-sans-extension (read-string "Filename for template: ")) ".el"))
                      ((and (mon-buffer-written-p) (string-match-p ".*\\.el" mift-cur-nm)) mift-cur-nm)
                      (t "<PKG-NAME>.el")))
         (mift-fnm-sans (file-name-sans-extension mift-fname))
         (mift-tmplt
          (concat
           ";;; " mift-fname " --- { A one line description of: " mift-fnm-sans ". }\n"
           ";; -*- mode: EMACS-LISP; -*-\n"
           (mon-build-copyright-string nil nil t) "\n"
           ";; FILENAME: " mift-fname "\n"
           ";; AUTHOR: MON KEY\n"
           ";; MAINTAINER: MON KEY\n"
           ";; CREATED: "
           (replace-regexp-in-string "#{\\(.*\\)}#.*" "\\1" (mon-timestamp :naf t)) "\n"
           ";; VERSION: 1.0.0\n"
           ";; COMPATIBILITY: " (concat "Emacs" (format "%s" emacs-major-version) ".*") "\n"
           ";; KEYWORDS: \n\n"
           (mon-comment-divider-w-len 64)"\n\n"
           ";;; COMMENTARY: \n\n"
           (substring (mon-comment-divider-w-len 65 ) 1) "\n"
           ";; DESCRIPTION:\n"
           ";; " mift-fnm-sans " provides { some description here. }\n;;\n"
           ";; FUNCTIONS:►►►\n;;\n;; FUNCTIONS:◄◄◄\n;;\n"
           ";; MACROS:\n;;\n"
           ";; METHODS:\n;;\n"
           ";; CLASSES:\n;;\n"
           ";; CONSTANTS:\n;;\n"
           ";; FACES:\n;;\n"
           ";; VARIABLES:\n;;\n"
           ";; GROUPS:\n;;\n"
           ";; ALIASED/ADVISED/SUBST'D:\n;;\n"
           ";; DEPRECATED:\n;;\n"
           ";; RENAMED:\n;;\n"
           ";; MOVED:\n;;\n"
           ";; TODO:\n;;\n"
           ";; NOTES:\n;;\n"
           ";; SNIPPETS:\n;;\n"
           ";; REQUIRES:\n;;\n"
           ";; THIRD-PARTY-CODE:\n;;\n" 
           ";; URL: http://www.emacswiki.org/emacs/" mift-fname "\n"
           ";; FIRST-PUBLISHED:\n;;\n"
           ";; EMACSWIKI: { URL of an EmacsWiki describing " mift-fnm-sans ". }\n;;\n"
           ";; FILE-CREATED:\n;; <Timestamp: " (mon-timestamp :naf t) "\n;;\n"
           (substring (mon-comment-divider-w-len 65 ) 1) "\n\n"
           ";;; LICENSE:\n\n"
           (substring (mon-comment-divider-w-len 65 ) 1)
           (mon-build-copyright-string-license 'gpl-emacs t) "\n"
           (substring (mon-comment-divider-w-len 65 ) 1) "\n"
           (mon-insert-gnu-licence-gfdl) "\n"
           (mon-build-copyright-string nil nil t t nil t) "\n\n"
           ";;; CODE:\n\n"
           "\(eval-when-compile \(require 'cl\)\)\n\n"
           *mon-default-comment-divider*
           "\n;;  { ...\n;;   "  mift-fnm-sans " Contents here\n;;   ... }\n\n"
           *mon-default-comment-divider* "\n" 
           ";;; (provide '" mift-fnm-sans ")\n"
           *mon-default-comment-divider* "\n\n"
           "\xc\n;; Local Variables:\n"
           ";; mode: EMACS-LISP\n"
           ";; coding: utf-8\n"
           (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
                      (bound-and-true-p IS-MON-SYSTEM-P))
             ";; generated-autoload-file: \"./mon-loaddefs.el\"\n"             
             )
           ;; ";; version-control: never\n"
           ;; ";; no-byte-compile: t\n"
           ";; End:\n\n"
           (mon-comment-divider-w-len 68) "\n"
           ";;; " mift-fname " ends here\n;;; EOF")))
    (if (or insrtp intrp)
        (progn (goto-char (buffer-end 0))
               (save-excursion (insert mift-tmplt)))
        mift-tmplt)))
;;
;;; :TEST-ME (mon-insert-file-template)
;;; :TEST-ME (mon-insert-file-template "bubba")
;;; :TEST-ME (mon-insert-file-template nil t)
;;; :TEST-ME (mon-insert-file-template nil nil t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-09T11:26:24-04:00Z}#{09327} - by MON KEY>
(defvar *mon-gnu-license-header* nil
  "*Default GNU license to insert in newly created file headers.\n
Presented with the GPLv3+ clause.\n
:EXAMPLE\n\n(mapconcat #'identity *mon-gnu-license-header* \"\\n\")\n
:SEE-ALSO `*mon-gnu-license-header-emacs*',
`*mon-mit-license-header*',`*mon-bsd-license-header*',
`*mon-gnu-license-header-gfdl*', `mon-insert-gnu-licence-gfdl',
`mon-insert-gnu-licence', `mon-build-copyright-string',
`mon-build-copyright-string-license', `mon-build-copyright-string-TEST',
`mon-insert-file-template'.\n►►►")
;;
(unless (bound-and-true-p *mon-gnu-license-header*)
  (setq *mon-gnu-license-header*
        '(;;""
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
;;; :TEST-ME (mapconcat #'identity *mon-gnu-license-header* "\n")
;;
;;; (progn (makunbound '*mon-gnu-license-header*)
;;;        (unintern '*mon-gnu-license-header*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-03T12:10:38-05:00Z}#{10093} - by MON KEY>
(defvar *mon-gnu-license-header-emacs* nil
  "*Default GNU Emacs license to insert in newly created file headers.\n
Presented with the GPLv3+ clause.\n
:EXAMPLE\n\n\(mapconcat #'identity *mon-gnu-license-header-emacs* \"\\n\"\)\n
:SEE-ALSO `*mon-mit-license-header*',`*mon-bsd-license-header*',
`*mon-gnu-license-header-gfdl*', `mon-insert-gnu-licence-gfdl',
`mon-insert-gnu-licence', `mon-build-copyright-string',
`mon-build-copyright-string-license', `mon-build-copyright-string-TEST',
`mon-insert-file-template'.\n►►►")
;;
(unless (bound-and-true-p *mon-gnu-license-header-emacs*)
  (setq *mon-gnu-license-header-emacs*
        `("\n;; This file is not part of GNU Emacs.\n"
          ,@*mon-gnu-license-header*)))
;;
;;; :TEST-ME (mapconcat #'identity *mon-gnu-license-header-emacs* "\n")
;; 
;;;(progn (makunbound '*mon-gnu-license-header-emacs*)
;;;       (unintern   '*mon-gnu-license-header-emacs*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-01T13:34:04-05:00Z}#{10051} - by MON KEY>
(defvar *mon-mit-license-header* nil
  "*Default MIT license aka x11 License for insertion in newly created file headers.\n
:EXAMPLE\n\n\(mapconcat #'identity *mon-mit-license-header* \"\\n\"\)\n
:CALLED-BY .\n
:SEE-ALSO `*mon-gnu-license-header-emacs*', `*mon-mit-license-header*',
`*mon-bsd-license-header*', `*mon-gnu-license-header-gfdl*',
`mon-insert-gnu-licence-gfdl', `mon-insert-gnu-licence',
`mon-build-copyright-string', `mon-build-copyright-string-license',
`mon-build-copyright-string-TEST', `mon-insert-file-template'.\n►►►")
;;
(unless (bound-and-true-p *mon-min-license-header*)
  (setq *mon-mit-license-header*
        '(;;""
          "\n;; Permission is hereby granted, free of charge, to any person"
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
;;; :TEST-ME (mapconcat #'identity *mon-mit-license-header* "\n")
;;
;;; (progn (makunbound '*mon-mit-license-header*)
;;;        (unintern '*mon-mit-license-header*) )


;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: #{2010-10-04T14:14:00-04:00Z}#{10401} - by MON KEY>
(defcustom *mon-bsd-license-header-COPYRIGHT-HOLDER* nil
  "Name substituted for \"<COPYRIGHT HOLDER>\" `*mon-bsd-license-header*'.\n
:SEE-ALSO `*mon-gnu-license-header-emacs*', `*mon-mit-license-header*',
`*mon-bsd-license-header*', `*mon-gnu-license-header-gfdl*',
`mon-insert-gnu-licence-gfdl', `mon-insert-gnu-licence',
`mon-build-copyright-string', `mon-build-copyright-string-license',
`mon-build-copyright-string-TEST', `mon-insert-file-template'.\n►►►"
  :type 'string
  :group 'mon-insertion-utils)
;;
(when (and (and (intern-soft "IS-MON-SYSTEM-P") (bound-and-true-p IS-MON-SYSTEM-P))
           (and (intern-soft "*MON-NAME*") (bound-and-true-p *MON-NAME*)))
  (unless (and (intern-soft "*mon-bsd-license-header-COPYRIGHT-HOLDER*")
               (bound-and-true-p *mon-bsd-license-header-COPYRIGHT-HOLDER*))
    (setq *mon-bsd-license-header-COPYRIGHT-HOLDER* (cadr (assoc 6 *MON-NAME*)))))


;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-01T13:36:19-05:00Z}#{10051} - by MON KEY>
(defvar *mon-bsd-license-header* nil
  "*Default BSD sytle license to insert in newly created file headers.\n
The simplified two clause BSD License aka the FreeBSD License without
advertising clause three.\n
:EXAMPLE\n\n\(mapconcat #'identity *mon-bsd-license-header* \"\\n\"\)\n
:NOTE The text field \"<COPYRIGHT HOLDER>\" can/should be modified accordingly
:SEE `*mon-bsd-license-header-COPYRIGHT-HOLDER*'.\n
:SEE-ALSO `*mon-gnu-license-header-emacs*', `*mon-mit-license-header*',
`*mon-bsd-license-header*', `*mon-gnu-license-header-gfdl*',
`*mon-gnu-license-header*', `mon-insert-gnu-licence-gfdl',
`mon-insert-gnu-licence', `mon-build-copyright-string',
`mon-build-copyright-string-license', `mon-build-copyright-string-TEST',
`mon-insert-file-template'.\n►►►")
;;
(unless (and (intern-soft "*mon-bsd-license-header*")
             (bound-and-true-p *mon-bsd-license-header*))
  (setq *mon-bsd-license-header*        
        '(;;""
          "\n;; Redistribution and use in source and binary forms, with or without"
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
;;; :TEST-ME (mapconcat #'identity *mon-bsd-license-header* "\n")
;;
;;; (progn (makunbound '*mon-bsd-license-header*)
;;;        (unintern '*mon-bsd-license-header*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-02T12:01:00-04:00Z}#{09405} - by MON KEY>
(defvar *mon-gnu-license-header-gfdl* nil
  "*Insert default GNU Free Documentation license in newly created file headers.\n
Insertion provides GFDL clause.\n
:EXAMPLE\n\n\(mapconcat #'identity *mon-gnu-license-header-gfdl* \"\\n\"\)\n
:CALLED-BY `mon-insert-file-template', `mon-insert-gnu-licence-gfdl'\n.
:SEE `*mon-gnu-license-header*', `mon-insert-gnu-licence' for GPLv3+ clause.
:SEE-ALSO `*mon-gnu-license-header-emacs*', `*mon-mit-license-header*',
`*mon-bsd-license-header*', `mon-insert-gnu-licence',
`mon-build-copyright-string', `mon-build-copyright-string-license',
`mon-build-copyright-string-TEST'.\n►►►")
;;
(unless (and (intern-soft "*mon-gnu-license-header-gfdl*")
             (bound-and-true-p *mon-gnu-license-header-gfdl*))
  (setq *mon-gnu-license-header-gfdl*
        '(;; :NOTE Don't remove semi-colons on line1 e.g. ";; Permission"
          ";; Permission is granted to copy, distribute and/or modify this"
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
;;; :TEST-ME (mapconcat #'identity *mon-gnu-license-header-gfdl* "\n")
;;
;;; (progn (makunbound '*mon-gnu-lincese-header-gfdl*)
;;;        (unintern '*mon-gnu-license-header-gfdl*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-01T14:38:38-05:00Z}#{10051} - by MON KEY>
(defun mon-build-copyright-string-license (&optional gpl-mit-bsd-gfdl w-out-copy-delim)
  "Return a copyright license string of type GPL, MIT, BSD, or GFDL.\n
When optional arg GPL, MIT, BSD, or GFDL \(quoted symbol\) is non-nil generate that
license. If no license is specified use the default arg GPL with `GNU-GPLv3'.\n
If W-OUT-COPY-DELIM is non-nil or value of GPL-MIT-BSD-GFDL is 'GFDL do not prepend 
value of \(mon-build-copyright-string nil nil t t\).\n
:EXAMPLE\n\n\(mon-build-copyright-string-license\)\n
\(mon-build-copyright-string-license 'gpl-emacs\)\n
\(mon-build-copyright-string-license 'gpl\)\n
\(mon-build-copyright-string-license 'gfdl\)\n
\(mon-build-copyright-string-license 'mit\)\n
\(mon-build-copyright-string-license 'bsd\)\n
License are mapped from the list of strings in:
:VARIALBE `*mon-gnu-license-header*'
:VARIABLE `*mon-gnu-license-header-emacs*'
:VARIALBE `*mon-gnu-license-header-gfdl*'
:VARIALBE `*mon-mit-license-header*'
:VARIALBE `*mon-bsd-license-header*'\n
:SEE-ALSO `mon-build-copyright-string', `mon-build-copyright-string-TEST',
`mon-insert-gnu-licence', `mon-insert-gnu-licence-gfdl',
`mon-insert-file-template'.\n►►►"
  (let ((mbcsl-lic (if gpl-mit-bsd-gfdl 
                 (cond ((eq gpl-mit-bsd-gfdl 'gpl)  'gpl)
                       ((eq gpl-mit-bsd-gfdl 'mit)  'mit)
                       ((eq gpl-mit-bsd-gfdl 'bsd)  'bsd)
                       ((eq gpl-mit-bsd-gfdl 'gfdl) 'gfdl)
                       (t 'gpl-emacs))
                 'gpl-emacs)))
    (concat (unless (or (eq mbcsl-lic 'gfdl) w-out-copy-delim)
              (mon-build-copyright-string nil nil t t))
            (mapconcat #'(lambda (lic-hdr)
                           (if (and (eq mbcsl-lic 'bsd)
                                    *mon-bsd-license-header-COPYRIGHT-HOLDER*)
                               (replace-regexp-in-string 
                                "<COPYRIGHT HOLDER>" 
                                *mon-bsd-license-header-COPYRIGHT-HOLDER*
                                lic-hdr)
                               (identity lic-hdr)))
                       (case mbcsl-lic 
                         ('gpl-emacs  *mon-gnu-license-header-emacs*)
                         ('gpl        *mon-gnu-license-header*)
                         ('gfdl       *mon-gnu-license-header-gfdl*)
                         ('mit        *mon-mit-license-header*)
                         ('bsd        *mon-bsd-license-header*))
                       "\n;; " ))))
;;
;;; :TEST-ME (mon-build-copyright-string-license)
;;; :TEST-ME (mon-build-copyright-string-license 'gpl-emacs)
;;; :TEST-ME (mon-build-copyright-string-license 'gpl)
;;; :TEST-ME (mon-build-copyright-string-license 'gfdl)
;;; :TEST-ME (mon-build-copyright-string-license 'mit)
;;; :TEST-ME (mon-build-copyright-string-license 'mit t)
;;; :TEST-ME (mon-build-copyright-string-license 'bsd)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-09T11:26:16-04:00Z}#{09327} - by MON KEY>
(defun mon-insert-gnu-licence (&optional insrtp intrp not-emacs)
  "Return `*mon-gnu-license-header-emacs*'.\n 
When optional arg NOT-EMACS return `*mon-gnu-license-header*'.\n
When INSRTP non-nil or called-interactively insert GNU license at point, but
does not move point.\n
:EXAMPLE\n\n(mon-insert-gnu-licence)\n
\(mon-insert-gnu-licence nil nil t\)\n
:SEE-ALSO `mon-insert-gnu-licence-gfdl', `mon-build-copyright-string-license',
`mon-insert-file-template', `mon-build-copyright-string',
`mon-build-copyright-string-license', `mon-build-copyright-string-TEST',
`mon-insert-file-template', `*mon-gnu-license-header-emacs*',
`*mon-gnu-license-header*', `*mon-gnu-license-header-gfdl*',
`*mon-mit-license-header*', `*mon-bsd-license-header*'.\n►►►"
  (interactive "i\np")
  (let ((migl-lic (if not-emacs 
                  (mon-build-copyright-string-license 'gpl)
                  (mon-build-copyright-string-license 'gpl-emacs))))
    (if (or insrtp intrp)
        (save-excursion
          (newline)
          (insert migl-lic))
     migl-lic)))
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
When INSRTP non-nil or called-interactively insert at point. 
Does not move point.\n
:EXAMPLE\n\n(mon-insert-gnu-licence-gfdl)\n\(mon-insert-gnu-licence-gfdl t\)\n
:SEE-ALSO `mon-insert-gnu-licence', `mon-build-copyright-string-license',
`mon-insert-file-template', `mon-build-copyright-string',
`mon-build-copyright-string-license', `mon-build-copyright-string-TEST',
`mon-insert-file-template', `*mon-gnu-license-header*',
`*mon-gnu-license-header-gfdl*', `*mon-mit-license-header*',
`*mon-bsd-license-header*'.\n►►►"
  (interactive "P\ni\np")
  (let ((bld-gfdl (cond (w-divider
                         (concat 
                          (mon-comment-divider-w-len 64) "\n"
                          (mon-build-copyright-string-license 'gfdl) "\n"
                          ;; :WAS *mon-gnu-license-header-gfdl*  "\n"
                          (mon-comment-divider-w-len 64) "\n"
                          (mapconcat 'identity
                                     (mon-sublist-gutted ;; `mon-sublist-gutted' <- mon-utils.el
                                      0 1 
                                      (save-match-data
                                        (split-string (mon-build-copyright-string nil nil t t nil t) "\n")))
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
;;; :TODO Should insert condidtionally according to the Lisp dialect CL vs Elisp.
;;; :CREATED <Timestamp: #{2009-10-04T09:33:51-04:00Z}#{09407} - by MON>
(defun mon-insert-defclass-template (&optional class-pfx slot-count insrtp intrp)
  "Return an `EIEIO' `defclass' template.\n
When non-nil CLASS-PFX is a class-name for template. Default is <CLASS-NAME>.\n
SLOT-COUNT is the number of slot templates returned.\n
When INSRTP in non-nil or called-interactively insert template at point.
Does not move point.\n
:EXAMPLE\n\n(mon-insert-defclass-template nil 2)\n
:SEE-ALSO `mon-insert-naf-mode-class-template', `mon-help-eieio-defclass',
`mon-insert-lisp-doc-eg-xref', `mon-insert-file-template',
`mon-insert-lisp-CL-mode-line-template', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-CL-package-template', `mon-lisp-CL-package-complete',
`mon-insert-lisp-stamp', `mon-insert-lisp-evald' `mon-insert-regexp-template',
`mon-comment-divider'.\n►►►"
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
;; (mail-addr (cadr (assoc 9 *MON-ORG-NAME*))) ;WAS: `user-mail-address'
;; (organization (getenv "ORGANIZATION"))

;; u-name
;; (cadr (apply 'assq (or (and (and (intern-soft "IS-BUG-P")
;;                                  (bound-and-true-p IS-BUG-P))
;;                             `(1 ,*BUG-NAME*))
;;                        (and (and (intern-soft "IS-MON-P")
;;                                  (bound-and-true-p IS-MON-P))
;;                             `(6 ,*MON-NAME*))
;;                        (list nil nil))))



;; (when (and (intern-soft "IS-MON-SYSTEM-P") (bound-and-true-p IS-MON-SYSTEM-P))
;; (mail-addr       (cadr (assoc 9 *MON-ORG-NAME*))) ;WAS: `user-mail-address'
;; (organization    (getenv "ORGANIZATION")) *MON-ORG-NAME*
;; (u-name          
;; (cadr (apply 'assq  (or (and (and (intern-soft "IS-BUG-P")
;;                                   (bound-and-true-p IS-BUG-P))
;;                              `(1 ,*BUG-NAME*))
;;                         (and (and (intern-soft "IS-MON-P")
;;                                   (bound-and-true-p IS-MON-P))
;;                              `(6 ,*MON-NAME*))
;;                         (list nil nil))))

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
When called-interactively prompts for TITLE, SHORT-DESCRIPTION, and TOP.\n
Try to DTRT when buffer is not visiting file and prompts for filename to write
buffer to before proceeding with insertion.\n
:SEE-ALSO `mon-file-stamp', `mon-insert-file-template',
`mon-insert-gnu-licence', `mon-insert-ebay-dbc-template',
`mon-insert-lisp-CL-package-template', `mon-insert-lisp-doc-eg-xref',
`mon-insert-file-template', `mon-insert-lisp-CL-file-template',
`mon-insert-lisp-stamp', `mon-insert-lisp-evald' `mon-insert-regexp-template',
`mon-comment-divider'.\n►►►"
  (interactive "\i\n\i\n\i\nP")
  (let ((mail-addr (cadr (assoc 9 *MON-ORG-NAME*))) ;WAS: `user-mail-address'
        (organization (getenv "ORGANIZATION"))
        (u-name (cadr (assoc 6 *MON-NAME*))) ;;WAS: `user-full-name' 
        (this-year (substring (current-time-string) -4))
        (title (when intrp (read-string "set @title to: ")))
        (shrt-desc (when intrp (read-string "Short description: ")))
        (top (when intrp (read-string "set @top (master menu) to: ")))
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
      ;; (mon-g2be -1)
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
  "Given a DOLLAR amount compute 45% retained by partyA and partyB.\n
When INSRTP is non-nil or called-interactively insert at point.\n
Does not move-point.\n
:EXAMPLE\n\(mon-comput-45 600 nil\)\n
:RETURN
:TO-PRICE-AT $600 (45% not-on-linen)
:THEM-AT-LOW $330
:OURS-AT-LOW $270\n
:SEE-ALSO `mon-comput-33'.\n►►►"
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
  "Given a DOLLAR amount compute 45% retained by partyA and partyB.\n
When INSRTP is non-nil or called-interactively insert at point.
Does not move-point.\n
:EXAMPLE\n\(mon-comput-33 600 nil\)\n
:RETURN
:TO-PRICE-AT $750 (33%)
:THEM-AT-HIGH $502
:OURS-AT-HIGH $247\n
:SEE-ALSO `mon-comput-45'.\n►►►"
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

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ==============================
;;; mon-insertion-utils.el ends here
;;; EOF

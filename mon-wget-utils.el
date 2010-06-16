;;; mon-wget-utils.el --- routines for pulling files with wget 
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-wget-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-02-18T17:09:29-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: execute, external, comm, hypermedia, processes, wget

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-wget-utils provides routines for pulling files with wget.
;;
;; Good God man! I just remembered: 
;; (url-copy-file "SOME-URL" "SOME-FILE" t)
;; 
;; Why the *%$!@#*# doesn't Emacs have a better code dictionary/thesaurus!?
;;
;; FUNCTIONS:►►►
;; `mon-wget-list-to-script', `mon-wget-list-to-script-TEST',
;; `mon-wget-list-to-script-shell-command',
;; `mon-wget-list-give-script-to-shell-command',
;; `mon-wget-rfc', `mon-wget-unicodedata-files',
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
;; `*mon-show-wget-script-temp*'
;; `*mon-unidata-file-list*'
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;;
;; NOTES:
;; There is wget.el for more extented functionality (locate-library "wget")
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-wget-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-02-18T17:10:38-05:00Z}#{10074} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-wget-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-02-18T17:09:29-05:00Z}#{10074} - by MON KEY>
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

(require 'mon-url-utils nil t) ;; <- `*mon-url-search-paths*'

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-18T16:20:22-05:00Z}#{10074} - by MON KEY>
(defvar *mon-show-wget-script-temp* nil
  "*Buffer name for displaying output of `mon-wget-list-to-script-TEST'
:SEE-ALSO `mon-wget-list-give-script-to-shell-command', 
`mon-wget-list-to-script', `mon-wget-list-to-script-TEST',
`mon-wget-list-to-script-shell-command', `mon-wget-rfc',
`*mon-show-wget-script-temp*'.\n►►►")

;;; ==============================
;;; :TODO Add &rest to allow incorporationg other flags, e.g. -w SECONDS
;;; :CREATED <Timestamp: #{2010-02-18T15:49:08-05:00Z}#{10074} - by MON KEY>
(defun mon-wget-list-to-script (get-list &optional base-url wget-fname &rest flags)
  "Return a wget script built from contents of GET-LIST.\n
GET-LIST is a list of files for wget to pull.\n
BASE-URL is a url to prepend to each element fo FILE-LIST.
WGET-FNAME filename to write the wget-script to, if ommitted a wget script is
written to to a file name with the format \"wget-script-YYYY-NN-NN\" in
`default-directory'.\n
When non-nil FLAGS are additional flags to pass to wget. These have the form:
\"--wait=8\" \"-w 5\" etc. each is a string.\n
:SEE-ALSO `mon-wget-list-give-script-to-shell-command', 
`mon-wget-list-to-script', `mon-wget-list-to-script-TEST',
`mon-wget-list-to-script-shell-command', `mon-wget-rfc',
`*mon-show-wget-script-temp*'.\n►►►"
  (let ((wget-p (executable-find "wget"))
        (wget-fname (if wget-fname wget-fname
                      (concat default-directory 
                              "wget-script-" 
                              (format-time-string "%Y-%m-%d"))))
        (sys (cond ((eq system-type 'windows-nt) 'wnz)
                   ((or (eq system-type 'gnu/linux) 
                        (eq system-type 'linux)) 'gnu)))
        (unravel-flags (if flags (mapconcat #'identity flags " ")))
        the-get)
    ;;(when (and base-url (not (string= base-url "")))
    (when base-url
      (dolist (tg get-list (setq the-get (nreverse the-get)))
        (if (string-match-p base-url tg)
            (push tg the-get)
          (push (concat base-url tg) the-get))))
    (setq the-get (mapconcat #'identity the-get "\n"))
    (with-temp-file wget-fname
      (insert the-get)
      (goto-char (buffer-end 0))
      (when wget-p 
        (cond ((eq sys 'wnz)
               (insert (concat "# " wget-p  " --no-parent " "--no-directories "  
                               unravel-flags
                               " -i " (file-name-nondirectory wget-fname) "\n")))
              ((eq sys 'gnu)
               (insert 
                (concat "#! /bin/sh\nwget --no-parent --no-directories" unravel-flags " \x5c\n"))
               (while (search-forward-regexp "[a-z]$" nil t) 
                 (insert " \x5c"))
               (goto-char (buffer-end 1))
               (skip-chars-backward "^ \\")
               (delete-char 1))))
      (setq the-get (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (when (and wget-p) (set-file-modes wget-fname 480))
    `(,wget-fname . ,the-get)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-18T16:19:54-05:00Z}#{10074} - by MON KEY>
(defun mon-wget-list-to-script-TEST (get-list &optional base-url)
  "Helper function to verify `mon-wget-list-to-script' is functioning as expected.\n
Performs the following checks:\n
o Writes a temp file with output from mon-help-wget-cl-pkgs
o Return inserted contents of temp file in a temporary buffer
o Display that buffer with `file-attributes' in header
o Kills temp-buffer and file on exit\n
:EXAMPLE\n\nmon-wget-list-to-script-TEST\n
:NOTE On exit this function should cleanup the temp file/buffer objects below:\n
 o A temp file written to:
   /PATH/TO/`default-directory'/tmp-wget-YY-MM-DD\n
 o A temp-buffer with the name \"*MON-SHOW-WGET-SCRIPT-TEMP*\".\n
:SEE-ALSO `mon-wget-list-give-script-to-shell-command', 
`mon-wget-list-to-script', `mon-wget-list-to-script-shell-command',
`mon-wget-rfc', `*mon-show-wget-script-temp*'.\n►►►"
  (let* ((mswst-str (symbol-name '*mon-show-wget-script-temp*))
         (tmp-wget-script (concat default-directory 
                                   (substring mswst-str 1 26)
                                   (format-time-string "-%Y-%M-%d")))
         (mswst (upcase mswst-str))
         show-wget-script
         tmp-wget-spec)
    (if (file-exists-p tmp-wget-script)
        (error (concat  ":FUNCTION `mon-wget-list-to-script-TEST' "
                        "-- wget-list-test file already written"))
        (progn
          (mon-wget-list-to-script get-list base-url tmp-wget-script)
          (setq show-wget-script
                (with-temp-buffer
                  (insert-file-contents tmp-wget-script)
                  (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
          (setq show-wget-script
                (multiple-value-bind
                      (d l UID GID ACCESSED MODIFIED s SIZE MODE gmod inod dev)
                    (file-attributes tmp-wget-script) ;; (buffer-file-name))
                  (format (concat "## :FILE #P %s\n## :UID %s\n## :GID %s\n"
                                  "## :ACCESSED %s\n## :MODIFIED %s\n"
                                  "## :SIZE %s\n## :MODE %s\n"
                                  "## :CONTENTS-OF-TEMP-FILE-BELOW\n"
                                  "### ==============================\n%s")
                          tmp-wget-script 
                          UID GID 
                          (format-time-string "%Y-%m-%d %H:%M:%S" ACCESSED)
                          (format-time-string "%Y-%m-%d %H:%M:%S" MODIFIED)
                          SIZE MODE
                          show-wget-script)))
          (delete-file tmp-wget-script)
          (with-current-buffer (get-buffer-create mswst)
            (erase-buffer)
            (save-excursion (princ show-wget-script (current-buffer)))
            (set (make-local-variable 'buffer-read-only) t)
            (display-buffer mswst t 'visible))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-18T16:26:03-05:00Z}#{10074} - by MON KEY>
(defun mon-wget-list-to-script-shell-command (wget-fname)
  "Return a string\(s\) for passing to `shell-command' for wget'ing.
:SEE-ALSO `mon-wget-list-to-script', `mon-wget-list-to-script-TEST', 
`mon-wget-list-give-script-to-shell-command', `*mon-show-wget-script-temp*'.\n►►►"
  (let ((fnm-tst-wgt (file-name-nondirectory wget-fname))
        (sys (cond ((eq system-type 'windows-nt) 'wnz)
                   ((or (eq system-type 'gnu/linux)
                        (eq system-type 'linux)) 'gnu)
                   ((not (executable-find "wget")) 'no-exec)))
        read-wget-string)
    (unless (directory-files default-directory nil (concat fnm-tst-wgt "$"))
      (error (concat  ":FUNCTION `mon-wget-list-to-script-shell-command' "
                      "-- file non-existent invoked outside file's directory")))
    (with-temp-buffer
      (save-excursion (insert-file-contents wget-fname))
      (when (eq sys 'wnz) (delete-char (- (skip-chars-forward "# "))))
      (setq read-wget-string 
            `(,(cond ((eq sys 'no-exec) '("### NO wget executable in path"))
                     ((eq sys 'gnu)
                      (delete-region (buffer-end 0) (1+ (line-end-position 1)))
                      (delete-and-extract-region (buffer-end 0) (1+ (line-end-position 1))))
                     ((eq sys 'wnz)
                      (delete-and-extract-region (buffer-end 0) (line-end-position))))
              . ,(cond ((eq sys 'gnu) 
                        (replace-regexp-in-string 
                         " \\\n" "\n"
                         (concat 
                          (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))
                          "\n")))
                       ((eq sys 'wnz)
                        (subst-char-in-string 10 32
                                              (buffer-substring-no-properties 
                                               (buffer-end 0) (buffer-end 1)) t))))))
    read-wget-string))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-18T16:56:02-05:00Z}#{10074} - by MON>
(defun mon-wget-list-give-script-to-shell-command (w-this-list 
                                                   &optional w-base-url w-wget-fname
                                                   &rest w-these-flags)
  "Invoke wget with WGET-FNAME containing W-THIS-LIST concat'd W-BASE-URL.\n
W-THIS-LIST, W-BASE-URL, W-WGET-FNAME, and  W-THESE-FLAGS are as per
`mon-wget-list-to-script'.\n
:NOTE When WGET-FNAME is non-nil this fncn must be invoked from within the
directory containing or which is to contain WGET-FNAME.\n
:SEE-ALSO`mon-wget-list-to-script', `mon-wget-list-to-script-TEST',
`mon-wget-list-to-script-shell-command', `mon-wget-rfc',
`*mon-show-wget-script-temp*'.\n►►►"
  (let ((UCmswst (get-buffer-create (upcase (symbol-name '*mon-show-wget-script-temp*)))))
    ;; If its read-only then we've already been here once.
    ;; Make it writable and erase it.
    (when (buffer-local-value 'buffer-read-only UCmswst)
      (with-current-buffer UCmswst
        (set (make-local-variable 'buffer-read-only) nil)
        (erase-buffer)))
    (let ((mwlts 
           (mon-wget-list-to-script w-this-list w-base-url w-wget-fname (car w-these-flags))))
      (case system-type
        (windows-nt
         (shell-command 
          (concat (car (mon-wget-list-to-script-shell-command (car mwlts))) " &")
          UCmswst))
        (gnu/linux
         (mon-wget-list-to-script-shell-command (car mwlts))
         (shell-command (concat (car mwlts) " &") UCmswst))))))
;;  
;;; :TEST-ME (cd (mon-wget-list-give-script-to-shell-command *mon-el-library* 
;;;  "http://www.emacswiki.org/emacs/download/" 
;;;  (format-time-string "mon-wget-el-%y-%m-%d-%H%M%S")
;; 
;;  '("04.01.01" "04.01.02" "04.01.03" "04.01.04" "04.01.05" "04.01.06" "04.01.07")
;;  "http://tunes.org/~nef/logs/lisp/")
;
;;; ==============================
;;; :TODO This is buggy on w32 paths. incorporate use the procedures above.
;;; :CHANGESET 1849 <Timestamp: #{2010-06-12T11:31:11-04:00Z}#{10236} - by MON KEY>
;;; :CREATED <Timestamp: Tuesday June 30, 2009 @ 02:30.21 PM - by MON KEY>
(defun mon-wget-rfc (rfc-num &optional url-insert)
  "Fetch an RFC with RFC-NUM with wget.\n
When optional arg URL-INSERT is non-nil return the contents of RFC-NUM as if by
`url-insert-file-contents'.\n
:NOTE The wget routine is buggy with w32 paths.\n
:SEE-ALSO `*mon-url-search-paths*', `mon-wget-list-to-script', `mon-wget-rfc',
`*mon-show-wget-script-temp*' `mon-wget-list-to-script-TEST',
`mon-wget-list-to-script-shell-command',
`mon-wget-list-give-script-to-shell-command'.\n►►►"
  (interactive "sRFC number: ")
  (let* ((the-rfc (cond ((stringp rfc-num) rfc-num)
                        ((numberp rfc-num) (number-to-string rfc-num))))
         (fetch-from (format 
                      (or (when (and (intern-soft "*mon-url-search-paths*")
                                     (bound-and-true-p *mon-url-search-paths*))
                            (cdr (assoc 'ietf *mon-url-search-paths*)))
                          "http://tools.ietf.org/rfc/rfc%s.txt")
                      the-rfc)))
    (if url-insert
        (url-insert-file-contents fetch-from)
      (shell-command  (format "wget %s" fetch-from)))))
;;
;;; :TEST-ME (mon-wget-rfc 2616)

;;; ==============================
;;; (mon-get-rfc-insert RFC-NUM-TO-INSERT

;; (url-insert-file-contents fetch-from)

;;; ==============================
;;; :CHANGESET 1792
;;; :CREATED <Timestamp: #{2010-05-29T15:35:16-04:00Z}#{10216} - by MON KEY>
(defun mon-wget-unicodedata-files (&optional in-wget-dir save-wget-file-name)
  "Generate a wget script and fetch URL list in variable
`*mon-unidata-file-list*'.  SAVE-WGET-FILE-NAME is file to save the wget script
to.  Fetched urls will be saved to this files directory.  When SAVE-WGET-FILE-NAME
is ommitted and `IS-MON-SYSTEM-P'is null default is to save to default-directory
and write the wget script to the file named mon-wget-unicode-data-YYYY-MM-DD.\n
:SEE-ALSO `mon-set-unicodedata-init', `mon-help-diacritics',
`mon-wget-list-give-script-to-shell-command', `mon-wget-rfc'.\n►►►"
  (let ((mwucs (cond ((and in-wget-dir save-wget-file-name)
                      `(in-wget-dir . save-wget-file-name))
                     ((intern-soft "IS-MON-SYSTEM-P")
                      `(,(concat 
                          (file-name-directory describe-char-unicodedata-file)
                          (format-time-string "wget-unicode-data-%Y-%m-%d")) .
                          ,(format-time-string "mon-wget-unicode-data-%Y-%m-%d")))
                     (t `(,(concat 
                            default-directory
                            (format-time-string "wget-unicode-data-%Y-%m-%d")) .
                            ,(format-time-string "mon-wget-unicode-data-%Y-%m-%d"))))))
    (unless (file-directory-p (car mwucs))
      (mkdir (car mwucs)))
    (let ((dir-now default-directory))
      (unwind-protect
          (progn
            (cd (car mwucs))
            (mon-wget-list-give-script-to-shell-command 
             *mon-unidata-file-list* "" 
             (concat (car mwucs) "/" (cdr mwucs))))
        (cd dir-now)))))
;;
;; :TEST-ME (mon-wget-unicodedata-files)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-29T15:08:20-04:00Z}#{10216} - by MON KEY>
(defvar *mon-unidata-file-list* nil
  "A list of unicode related file lists.
This list is current to the Unicode 5.2.0 final data files for the Unicode
Character Database \(UCD\) circa Summer/Autumn 2009.\n
:SEE (URL `http://www.unicode.org/Public/UNIDATA/')
:SEE (URL `http://www.unicode.org/Public/UNIDATA/UnicodeData.txt').
:SEE (URL `ftp://www.unicode.org/Public/zipped/5.2.0/UCD.zip').
:SEE-ALSO `describe-char-unicodedata-file', `describe-char-unidata-list',
`mon-help-diacritics', `mon-help-char-representation'.\n►►►")
;;
(unless (and (not (intern-soft "IS-MON-SYSTEM-P"))
             (bound-and-true-p *mon-unidata-file-list*))
  (setq *mon-unidata-file-list*  
        (let ((mufl-files 
               '((nil ;; :TOP-DIR 
                  "ArabicShaping.txt" "BidiMirroring.txt"
                  "BidiTest.txt" "Blocks.txt" "CJKRadicals.txt" "CaseFolding.txt"
                  "CompositionExclusions.txt" "DerivedAge.txt"
                  "DerivedCoreProperties.txt" "DerivedNormalizationProps.txt"
                  "EastAsianWidth.txt" "HangulSyllableType.txt" "Index.txt"
                  "Jamo.txt" "LineBreak.txt" "NameAliases.txt"
                  "NamedSequences.txt" "NamedSequencesProv.txt" "NamesList.txt"
                  "NormalizationCorrections.txt" "NormalizationTest.txt"
                  "PropList.txt" "PropertyAliases.txt" "PropertyValueAliases.txt"
                  "ReadMe.txt" "Scripts.txt" "SpecialCasing.txt"
                  "StandardizedVariants.txt" "UnicodeData.txt")
                 (auxiliary ;; :AUXILLARY-DIR 
                  "GraphemeBreakProperty.txt" "GraphemeBreakTest.txt"
                  "LineBreakTest.txt" "SentenceBreakProperty.txt"
                  "SentenceBreakTest.txt" "WordBreakProperty.txt"
                  "WordBreakTest.txt")
                 (extracted ;; :EXTRACTED-DIR 
                  "DerivedBidiClass.txt" "DerivedBinaryProperties.txt"
                  "DerivedCombiningClass.txt" "DerivedDecompositionType.txt"
                  "DerivedEastAsianWidth.txt" "DerivedGeneralCategory.txt"
                  "DerivedJoiningGroup.txt" "DerivedJoiningType.txt"
                  "DerivedLineBreak.txt" "DerivedNumericType.txt"
                  "DerivedNumericValues.txt")))
              (mufl-url (or (when (and (intern-soft "*mon-url-search-paths*")
                                       (bound-and-true-p *mon-url-search-paths*))
                              (cdr (assoc 'unicode *mon-url-search-paths*)))
                            "http://www.unicode.org/Public/UNIDATA/"))
              mufl-urls)
          (dolist (mf mufl-files (setq mufl-urls (nreverse mufl-urls)))
            (let ((sub-url
                   (cond ((and (null (car mf)) (consp (cdr mf)))
                          (pop mf) "")
                         ((and (not (stringp (car mf))) (consp (cdr mf)))
                          (format "%s/" (pop mf)))
                         (t ""))))
              (dolist (mf-sub mf)
                (push (concat mufl-url sub-url mf-sub) mufl-urls)))))))

;;; ==============================
;;; :COURTESY Stefan Reichoer <stefan@xsteve.at> :HIS .emacs 
;;; ==============================
;; ;;; wget
;; (add-site-lisp-load-path "wget/")
;; (autoload 'wget "wget" "wget interface for Emacs." t)
;; (autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
;; (load "w3m-wget" t)

;; ;;The file is downloaded to the folder wget-download-directory (= ~/download)
;; (defun wget-open-downloaded-file ()
;;   (let* ((dir  (cdr (assoc proc wget-process-dir-alist)))
;;          (file (or (cdr (assoc proc wget-process-saved-alist))
;;                    (wget-process-file-name proc)))
;;          (full-file-name (expand-file-name file dir)))
;;     (message "downloaded %s" full-file-name)
;;     (find-file full-file-name)))

;; ;; Open the file after the download
;; (add-hook 'wget-after-hook 'wget-open-downloaded-file)
;;; ==============================

;;; ==============================
(provide 'mon-wget-utils)
;;; ==============================

;;; :NOTE Now evaluated with `mon-after-mon-utils-loadtime' :SEE :FILE mon-utils.el
;; (eval-after-load "mon-wget-utils" 
;;   '(mon-check-feature-for-loadtime 'mon-get-mon-packages))

;;; ================================================================
;;; mon-wget-utils.el ends here
;;; EOF

;;; naf-mode-insertion-utils.el --- insertion procedures for working in `naf-mode' buffer
;; -*- mode: EMACS-LISP; -*-
;;; ================================================================
;;; DESCRIPTION:
;;; naf-insertion-utils common insertion procedures and miscellaneous
;;; tools for working in a `naf-mode' buffer.
;;;
;;; FUNCTIONS:►►►
;;; `naf-tab-region', `naf-comment-line', `naf-uncomment-line',
;;; `naf-comment-region', `naf-uncomment-region', `non-posting-source', `npps',
;;; `non-posting-wiki-source', `non-posting-ebay-source',
;;; `non-posting-imdb-source', `non-posting-benezit-source',
;;; `non-posting-internet-source', `mon-insert-naf-mode-constant-template',
;;; `mon-insert-naf-mode-face-template', `mon-insert-naf-mode-faces-as-displayed',
;;; `mon-insert-naf-file-in-dirs',
;;; `mon-insert-naf-mode-xref-template', `mon-build-naf-mode-xref'
;;; `mon-insert-naf-mode-var-const-template', `mon-insert-naf-mode-class-template'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS: 
;;; `*naf-mode-faces-as-displayed*'
;;;
;;; VARIABLES:
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;; `npes' -> `non-posting-ebay-source'
;;; `npps' -> `non-posting-philsp-source' 
;;; `npws' -> `non-posting-wiki-src'
;;; `constance-insert-copyright' -> `mon-insert-copyright'
;;; `mon-insert-naf-mode-file-template' -> `mon-insert-file-template'
;;;
;;; DEPRECATED:
;;; `npps' -> `mon-cln-philsp'
;;; `mon-insert-naf-mode-constant-template' 
;;;   -> `mon-insert-mon-insert-naf-mode-var-const-template'
;;;
;;; RENAMED: 
;;; `mon-insert-naf-mode-var-const-templt' -> `mon-insert-naf-mode-var-const-template'
;;; `mon-insert-face-as-displayed'         -> `mon-insert-naf-mode-faces-as-displayed'
;;;
;;; MOVED:
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;; 
;;; NOTES:
;;; It appears the macro for toggling longlines mode was yanked in incorrectly as:
;;; `mon-is-naf-mode-and-llm-p' ;<-WRONG!
;;; it should have been:
;;; `mon-naf-mode-toggle-restore-llm'
;;; We are testing this again, AS-OF:
;;; <Timestamp: #{2009-09-26T18:19:57-04:00Z}#{09396} - by MON KEY>
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK:
;;; (URL `http://www.emacswiki.org/emacs/naf-mode-insertion-utils.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-26T21:55:13-04:00Z}#{09397} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-25T19:46:02-04:00Z}#{09352} - by MON KEY>
;;; ================================================================
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
;;; ================================================================
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
;;; Copyright © 2009, 2010 MON KEY
;;; ==============================
;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-29T20:02:37-04:00Z}#{09403} - by MON KEY>
(defvar *naf-mode-insertion-utils-xrefs* nil
"*Xrefing list of functions and variables for naf-mode-insertion-utils.\n
:EXAMPLE\n\*naf-mode-insertion-utils-xrefs*\n
\(nth 3 *naf-mode-insertion-utils-xrefs*\)\n
:SEE :FILE naf-mode-insertion-utils.el\n
:SEE-ALSO `*naf-mode-xref-of-xrefs*'\n►►►.")
;;
(unless (bound-and-true-p *naf-mode-insertion-utils-xrefs*)
  (setq *naf-mode-insertion-utils-xrefs*
        '(naf-tab-region
          naf-comment-line
          naf-uncomment-line
          naf-comment-region
          naf-uncomment-region
          non-posting-source
          non-posting-wiki-source
          non-posting-ebay-source
          non-posting-philsp-source
          non-posting-imdb-source
          non-posting-benezit-source
          non-posting-internet-source
          mon-insert-naf-file-in-dirs
          mon-build-naf-mode-xref
          mon-insert-naf-mode-xref-template
          mon-insert-naf-mode-var-const-templ
          mon-insert-naf-mode-constant-template
          mon-insert-naf-mode-face-template
          mon-insert-naf-mode-faces-as-displayed
          *naf-mode-faces-as-displayed* 
          *naf-mode-insertion-utils-xrefs*)))
;;
;;; :TEST-ME (symbol-value '*naf-mode-insertion-utils-xrefs*)
;;; :TEST-ME (nth 3 *naf-mode-insertion-utils-xrefs*)
;;
;;;(progn (makunbound '*naf-mode-insertion-utils-xrefs*)
;;;       (unintern '*naf-mode-insertion-utils-xrefs*) )

;;; ==============================
(defun naf-tab-region (beg end &optional arg)   
  "Indent region by one tab in a `naf-mode' buffer.\n
:SEE-ALSO \n►►►"
  (interactive "r\nP")
  (indent-rigidly beg end tab-width)
  (exchange-point-and-mark))

;;; =======================
(defun naf-comment-line ()
  "Comment line in a NAF file.\n
:SEE-ALSO `naf-uncomment-line', `naf-comment-prefix', `naf-uncomment-region',
`naf-comment-region'.\n:USED-IN `naf-mode'.\n►►►"
  (interactive)
  (save-excursion 
    (back-to-indentation) 
    (insert naf-comment-prefix)))

;;; =======================
(defun naf-uncomment-line ()
  "Uncomment line in a NAF file.\n
:SEE-ALSO `naf-comment-prefix',`naf-uncomment-line', `naf-uncomment-region',
`naf-comment-region'.\n:USED-IN `naf-mode'.\n►►►"
  (interactive)
  (save-excursion 
    (back-to-indentation)
    (while (eq (char-after) 59) (delete-char 1))))


;;; =======================
(defun naf-comment-region (beg end &optional arg)
  "Comment out region in a NAF file.\n
:SEE-ALSO`naf-comment-prefix', `naf-uncomment-region',`naf-comment-line',
`naf-uncomment-line'.\n:USED-IN `naf-mode'.\n►►►"
  (interactive "r\nP")
  (let ((comment-start naf-comment-prefix))
    (comment-region beg end arg)))

;;; =======================
(defun naf-uncomment-region (beg end &optional arg)
  "Uncomment region in a NAF file.\n
:SEE-ALSO `naf-comment-prefix',`naf-comment-region', `naf-comment-line',
`naf-uncomment-line'.
:USED-IN `naf-mode'.\n►►►"
  (interactive "r\nP")
  (let ((comment-start naf-comment-prefix))
    (comment-region beg end -1)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T12:53:18-0400Z - by MON KEY>
(defun non-posting-source (&optional insrtp intrp)
  "Return vanilla non-posting-source flag.\n
When INSRTP is non-nil or called-interactively insert w/ newline after :(colon).
:EXAMPLE\n\(non-posting-source\)\n
:SEE-ALSO `nps', `non-posting-internet-source', `non-posting-wiki-source',
`non-posting-ebay-source', `non-posting-imdb-source',
`non-posting-philsp-source', `non-posting-benezit-source',
`benezit-naf-template'.
:USED-IN `naf-mode'.\n►►►"
  (interactive "i\np")
  (mon-naf-mode-toggle-restore-llm
      (let ((non-ps (format "\n-\nnon-posting-source:\n")))
        (if (or insrtp intrp)
            (save-excursion (insert non-ps))
            non-ps))))
;;
;; :NOTE This defalias is probably better as an abrev.
(defalias 'nps 'non-posting-source)
;;
;;; :TEST-ME (non-posting-source)
;;; :TEST-ME (non-posting-source t)
;;; :TEST-ME (call-interactively 'non-posting-source)

;;; =======================
;;; :MODIFICATIONS <Timestamp: #{2010-02-20T18:15:51-05:00Z}#{10076} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:00:15-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
(defun non-posting-ebay-source (&optional insrtp intrp)
  "Return a non-posting ebay stamp.\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n(non-posting-ebay-source)\n
:SEE-ALSO `non-posting-source', `non-posting-internet-source',
`non-posting-wiki-source', `non-posting-imdb-source',
`non-posting-philsp-source', `non-posting-benezit-source',
`benezit-naf-template'.
:USED-IN `naf-mode'.\n►►►"
  (interactive "i\np")
  (mon-naf-mode-toggle-restore-llm
      (let ((non-pes (mapconcat #'identity 
                                `("-" 
                                  "non-posting-ebay-source:" 
                                  "ebay-item-number: " 
                                  "ebay-item-seller: "
                                  "ebay-item-realized: " 
                                  "ebay-item-ended: " 
                                  ,(mon-accessed-stamp) 
                                  "---") "\n")))
        (if (or insrtp intrp)
            (save-excursion (insert non-pes))
            non-pes))))
;;
;; This defalias is probably better as an abbrev.
(defalias 'npes 'non-posting-ebay-source)
;;
;;; :TEST-ME (non-posting-ebay-source)
;;; :TEST-ME (non-posting-ebay-source t)
;;; :TEST-ME (call-interactively 'non-posting-ebay-source)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:04:43-0400Z - by MON KEY>
(defun non-posting-wiki-source (&optional insrtp intrp)
  "Return a non-posting Wikipedia source timestamp.\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n(non-posting-wiki-source)\n
:SEE-ALSO `non-posting-source', `non-posting-internet-source',
`non-posting-ebay-source', `non-posting-imdb-source', `non-posting-philsp-source',
`non-posting-benezit-source', `non-posting-philsp-source', `benezit-naf-template'.
:USED-IN `naf-mode'.\n►►►"
  (interactive "i\np") 
  (mon-naf-mode-toggle-restore-llm
      (let ((non-pws (mapconcat #'identity `("-"
                                             "non-posting-wiki-source:"
                                             ,(mon-accessed-stamp)) "\n")))
        (if (or insrtp intrp)
            (save-excursion (insert non-pws))
            non-pws))))
;;
;;; :NOTE This defalias is probably better as an abbrev. 
(defalias 'npws 'non-posting-wiki-src)
;;
;;; :TEST-ME (non-posting-wiki-source)
;;; :TEST-ME (non-posting-wiki-source t)
;;; :TEST-ME (call-interactively 'non-posting-wiki-source)

;;; ==============================
;;; :WAS `npps'
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:09:26-0400Z - by MON KEY>
(defun non-posting-philsp-source (&optional insrtp intrp)
  "Return a philsp non-posting-source timestamp.\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n(non-posting-philsp-source\)\n
:DEPRECATED Should only be invoked after manual cleansing.
:USE `mon-cln-philsp' which both replaces and stamps.\n
:SEE-ALSO `non-posting-source', `non-posting-wiki-source',
`non-posting-internet-source', `non-posting-ebay-source',
`non-posting-benezit-source', `benezit-naf-template' 
`non-posting-imdb-source'\n:USED-IN `naf-mode'.\n►►►"
  (interactive "i\np") 
  (mon-naf-mode-toggle-restore-llm
   (let ((non-pps (mapconcat #'identity 
                             `("-"
                               "non-posting-philsp-source:"
                               "(URL `http://www.philsp.com/homeville/FMI/a7.htm')"
                               ,(mon-accessed-stamp)
                               "---") "\n")))
     (if (or insrtp intrp)
         (save-excursion (insert non-pps))
         non-pps))))
;;
;; :NOTE This defalias is probably better as an abbrev.
(defalias 'npps 'non-posting-philsp-source)
;;
;;; :TEST-ME (non-posting-philsp-source) 
;;; :TEST-ME (non-posting-philsp-source t) 
;;; :TEST-ME (call-interactively 'non-posting-philsp-source) 

;;; ==============================
;;; :CREATED <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
(defun non-posting-imdb-source (&optional insrtp intrp)
  "Return an IMDB non-posting-source timestamp.\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n(non-posting-imdb-source)\n -\n non-posting-imdb-source:
\(URL `http://www.IMDB.com'\)\n accessed: Monday March 30, 2009 - MON\n
:SEE-ALSO `non-posting-source', `non-posting-wiki-source',
`non-posting-internet-source', `non-posting-ebay-source',
`non-posting-benezit-source', `benezit-naf-template'
`non-posting-philsp-source'.\n:USED-IN `naf-mode'.\n►►►"
  (interactive "i\np")
  (mon-naf-mode-toggle-restore-llm
   (let ((npis (mapconcat #'identity 
                             `("" ;; preceding newline
                               "-"
                               "non-posting-imdb-source:"
                               "(URL `http://www.IMDB.com')"
                               ,(mon-accessed-stamp)) "\n")))
     (if (or intrp intrp)
         (save-excursion (insert npis))
       npis))))
;;
;;; :TEST-ME (non-posting-imdb-source)
;;; :TEST-ME (non-posting-imdb-source t)
;;; :TEST-ME (call-interactively 'non-posting-imdb-source)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:18:40-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
(defun non-posting-benezit-source (benezit-name volume page &optional insrtp intrp)
  "Return Benezit non-posting-source timestamp with VOLUME and PAGE details.\n
VOLUME is a Benezit volume PAGE is a page reference therein.\n
When called-interactively prompt for VOLUME and PAGE.\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n\n\(non-posting-benezit-source \"Cappiello, Leonetto\" 3 444\)\n
\(non-posting-benezit-source \"Cappiello, Leonetto\" \"3\" \"444\"\)\n
:SEE-ALSO `non-posting-source', `non-posting-wiki-source', `non-posting-internet-source',
`non-posting-ebay-source', `non-posting-imdb-source', `benezit-naf-template'.
:USED-IN `naf-mode'.\n►►►"
  (interactive "sArtist Name (Lastname, Firstname):\nnVolume number:\nnPage number: \ni\np")
  (mon-naf-mode-toggle-restore-llm
   (let ((non-pbs
          (mapconcat #'identity 
                     `("" ;; preceding newline
                       "-"
                       "non-posting-benezit-source:"
                       ,(concat benezit-name " - Benezit: Volume "
                                (if (numberp volume) 
                                    (number-to-string volume) 
                                    volume)
                               " page" (cond ((numberp page) 
                                               (concat " " (number-to-string page)))
                                              ((not (eq (string-to-char (subseq page 0 1)) 32))
                                               (concat " " page))))
                       ,(mon-accessed-stamp)
                       "-") "\n")))
     (if (or insrtp intrp)
         (save-excursion (insert non-pbs))
       non-pbs))))
;;
;;; :TEST-ME (non-posting-benezit-source "Cappiello, Leonetto" 3 444)
;;; :TEST-ME (non-posting-benezit-source "Cappiello, Leonetto" "3" "444")
;;; :TEST-ME (non-posting-benezit-source "Cappiello, Leonetto" "3" "444" t)
;;; :TEST-ME (call-interactively 'non-posting-benezit-source)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T13:59:43-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
(defun non-posting-internet-source (&optional non-posting-url insrtp intrp)
  "Return a timestamped Emacs style URL reference.\n
Called interactively prompts for a URL name to wrap.
When NON-POSTING-URL is non-nil wraps URL name.
When INSRTP is non-nil or when called interactively inserts the wrapped url.
When NON-POSTING-URL is nil defaults to \"(URL `')\".\n
When INSRTP is non-nil or called-interactively insert return value at point.
Does not move point.\n
:EXAMPLE\n\(non-posting-internet-source \"http://www.emacswiki.com\")\n
\(non-posting-internet-source)\n
:SEE-ALSO `non-posting-source', `non-posting-wiki-source', `non-posting-imdb-source',
`non-posting-benezit-source', `non-posting-ebay-source', `benezit-naf-template'.\n
:USED-IN `naf-mode'.\n►►►"
  (interactive "sURL:\ni\np")
  (mon-naf-mode-toggle-restore-llm
      (let ((non-pis 
             (mapconcat #'identity
                        `("" "-" "non-posting-internet-source:"
                             ,(cond (intrp (concat "(URL `" non-posting-url "')"))
                                    (insrtp (if non-posting-url 
                                                (concat "(URL `" non-posting-url "')")
                                                "(URL `')\n"))
                                    ((and non-posting-url (not insrtp) (not intrp))
                                     (concat "(URL `" non-posting-url "')"))
                                    ;; Assume this is http as `naf-mode-url-flag'
                                    ;;;  and `naf-mode-url-wrapper-flag' expect.
                                    (t "(URL `http:// {... INSERT-URL-HERE... } ')"))
                             ,(mon-accessed-stamp)) "\n")))
        (if (or insrtp intrp)
            (save-excursion (insert non-pis))
            non-pis))))
;;
;;; :TEST-ME (non-posting-internet-source)
;;; :TEST-ME (non-posting-internet-source nil t)
;;; :TEST-ME (non-posting-internet-source "http://www.derbycityprints.com")
;;; :TEST-ME (non-posting-internet-source "http://www.derbycityprints.com" t)
;;; :TEST-ME (call-interactively 'non-posting-internet-source)

;;; ==============================
;;; :TODO Add optional STARTING-DIR arg to default-dir and possibly an alt.
;;; conditional insertion routine of file's text e.g. artist-naf, brand-naf, etc.
;;; :CREATED <Timestamp: Thursday April 16, 2009 @ 07:48.26 PM - by MON KEY>
;;; ==============================
(defun mon-insert-naf-file-in-dirs (make-dir-list) ;&optional starting-dir
  "Each element in list MAKE-DIR-LIST inserts a directory and a file in directory.
Directory's name and file's name are taken from elt in MAKE-DIR-LIST. Directory
is created relative to current buffer's DEFAULT-DIRECTORY. File's contents are
automatically inserted as:\n
   ;; -*- mode: NAF; -*-\n   naf-name {a MAKE-DIR-LIST elt}\n   ---\n   ;;; naf EOF\n
Format of list for MAKE-DIR-LIST should be as follows:\n
\(setq make-my-dirs 
      '(\"Lastname (Firstname Middlename Other)\"
	\"Lastname2 (Firstname2 Middlename2 Other2)\"
	\"Lastname3 (Firstname3 Middlename3 Other3)\"
	\"Lastname4 (Firstname4 Middlename4 Other4)\"))\n
Invokation for creating dirname/filename. Assuming buffer's default directory is
\"/home/my-dirs\"
Invoking the form with symbol list 'make-my-dirs' as argument to MAKE-DIR-LIST:\n
   \(mon-insert-naf-file-in-dirs make-my-dirs)\n
Or, interactively; M-x mon-insert-naf-file-in-dirs 
                   minibuffer-prompt: Give Symbol holind dir/file list :make-my-dirs\n
Creates the following directors and files in /home/my-dirs\n
   /home/my-dirs:
  |-- Lastname (Firstname Middlename Other)
  |   `-- Lastname, Firstname Middlename Other.naf
  |-- Lastname2 (Firstname2 Middlename2 Other2)
  |   `-- Lastname2, Firstname2 Middlename2 Other2.naf
  |-- Lastname3 (Firstname3 Middlename3 Other3)
  |   `-- Lastname3, Firstname3 Middlename3 Other3.naf
  `-- Lastname4 (Firstname4 Middlename4 Other4)
    `-- Lastname4, Firstname4 Middlename4 Other4.naf\n►►►"
  (interactive "XGive Symbol holind dir/file list :")
  (while make-dir-list 
    (let* ((file-dir make-dir-list)
           (current-file (car make-dir-list))
           (clean-it)
           (naf-name))
      (setq clean-it current-file)
      (when (string-match " \(" clean-it)
	(setq clean-it (replace-match	", " nil nil clean-it)))
      (when (string-match ")" clean-it)
	(setq clean-it  (replace-match "" nil nil clean-it)))
      ;;(while (string-match "\\^" clean-it) ;(replace-match ":" nil nil clean-it)))
      (setq naf-name clean-it)
      (make-directory current-file)
      (with-temp-file 
          (concat default-directory current-file "/" clean-it ".naf")
        (insert (format ";; -*- mode: NAF; -*-\n\n%s\n---\n;;; naf EOF" naf-name))))
    (pop make-dir-list)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T10:16:59-04:00Z}#{09407} - by MON>
(defun mon-insert-naf-mode-class-template (&optional class-sfx slot-count insrtp intrp)
  "Return an eieio `defclass' template for use with `naf-mode'.\n
When non-nil CLASS-SFX is a suffix to concatenate onto `naf-mode-'.
Default is 'naf-mode-'. SLOT-COUNT is the number of slot templates returned.
When INSRTP in non-nil or called-interactively insert template at point.
Does not move point.\n\n:EXAMPLE\n\(mon-insert-naf-mode-class-template\)\n
:SEE-ALSO `mon-insert-defclass-template', `mon-help-eieio-defclass'.\n►►►"
  (interactive "P\ni\ni\np")
  (mon-insert-defclass-template 
   (if class-sfx
       (cond (intrp
              (concat 
               "naf-mode-"
               (read-string "class-suffix to concatenate onto `naf-made-' :")))
             (t (concat "naf-mode-" (format "%s" class-sfx))))
       "naf-mode-")
   slot-count insrtp intrp))
;;
;;; :TEST-ME (mon-insert-naf-mode-class-template)
;;; :TEST-ME (mon-insert-naf-mode-class-template nil nil t)
;;; :TEST-ME (mon-insert-naf-mode-class-template "bubba" 3)
;;; :TEST-ME (mon-insert-naf-mode-class-template "bubba" 3 t)
;;; :TEST-ME (mon-insert-naf-mode-class-template "bubba" nil t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-26T20:00:09-04:00Z}#{09397} - by MON KEY>
(defun mon-build-naf-mode-xref ()
  "Return a list suitable for naf-mode variable xref template creation.\n
variable name is generated from current naf-mode filename.
Signals an error if filename is void or not a filename with 'naf-mode-' prefix.
Elements of list are returned as three strings:\n
 \(\"*naf-mode-insertion-utils-xrefs*\"  ; <- :XREF-NAME
   \"naf-mode-insertion-utils\"         ; <- :PKG-NAME
   \"naf-mode-insertion-utils.el\"\)     ; <- :FILENAME\n
:EXAMPLE\n\n\(let \(\(buffer-file-name \"naf-mode-xref-example.el\"\)\)
  \(mon-build-naf-mode-xref\)\)\n
:CALLED-BY `mon-insert-naf-mode-xref-template'
:CALLED-BY `mon-insert-naf-mode-var-const-template'
:SEE-ALSO .\n►►►"
  (if (buffer-file-name)
    (let* ((nm-match-str (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
           (nm-match-on  (string-match "naf-mode-" nm-match-str))
           (the-nm-matched (if nm-match-on (match-string nm-match-on nm-match-str)))
           to-next-let)
      (when the-nm-matched (setq to-next-let
                                 `(,nm-match-str 
                                   . ,(cadr (save-match-data
                                              (split-string nm-match-str the-nm-matched))))))
            (let* ((xref-p (if to-next-let ;; (mon-build-naf-mode-xref)
                               to-next-let ;; (mon-build-naf-mode-xref)
                               (error "not a naf-mode-file")))
                   (xref-fname (if xref-p (concat (car xref-p) ".el")))
                   (xref-pkg (if xref-p (car xref-p)))
                   (xref-var (if xref-p (concat "*naf-mode-" (cdr xref-p) "-xrefs*")))
                   );; (test-xref-no-bnd (read xref-concat)))
              `(,xref-var ,xref-pkg ,xref-fname)))
    (error "not a naf-mode-file")))
;;
;;; :TEST-ME (mon-build-naf-mode-xref)
;;; :TEST-ME (let ((buffer-file-name "naf-mode-xref-example.el"))
;;;            (mon-build-naf-mode-xref))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-26T20:59:14-04:00Z}#{09397} - by MON KEY>
(defun mon-insert-naf-mode-xref-template (&optional insrtp intrp)
  "Return a naf-mode variable template for xrefing variable names in current
`naf-mode' file.\n
When INSRTP is non-nil or called-interactively insert xref template at point.
Does not move point.\n
:EXAMPLE\n\n\(let \(\(buffer-file-name \"naf-mode-dir/naf-mode-xref-example.el\"\)\)
  \(mon-insert-naf-mode-xref-template\)\)\n
:SEE-ALSO `mon-build-naf-mode-xref', `mon-insert-naf-mode-xref-template'.\n►►►"
  (interactive "i\np")
  (let* ((xref-l (mon-build-naf-mode-xref))
         (xref-sym (car xref-l))
         (pkg-nm (cadr xref-l))
         (fnm (caddr xref-l))
         (xref-template
          (concat
           ";; Make sure where using CL's `eval-when'\n"
           "(eval-when-compile (require 'cl))\n\n"
           (mon-lisp-stamp)
           "\n(eval-and-compile\n"
           "(defvar " xref-sym "\n"
           "  '(" xref-sym "\n"
           "    mon-help-naf-mode-faces)\n"
           "  \"*List of symbol names of variables which xref each other in the\n"
           "`" pkg-nm"' package.\n:SEE :FILE " fnm "\"" "))\n"
           ";;\n;;; :TEST-ME " xref-sym "\n;;\n;;;(progn (makunbound '" xref-sym ")\n"
           ";;;       (unintern '" xref-sym ") \)")))
    (if (or insrtp intrp)
        (save-excursion (newline) (princ xref-template (current-buffer)))
      xref-template)))
;;
;;; :TEST-ME (mon-insert-naf-mode-xref-template)
;;; :TEST-ME (mon-insert-naf-mode-xref-template t)


;;; ==============================
;;; :RENAMED `mon-insert-naf-mode-var-const-templt' -> `mon-insert-naf-mode-var-const-template'
;;; :TODO Add subr to search backward for xref-name to see if it already exists.
;;; :CREATED <Timestamp: #{2009-09-26T21:27:46-04:00Z}#{09397} - by MON KEY>
(defun mon-insert-naf-mode-var-const-template (naf-symbol-name &optional insrtp intrp)
  "Return code building template for variable and constant with NAF-SYMBOL-NAME.\n
NAF-SYMBOL-NAME - a string - should be suitable for concatenation as:
*naf-<NAF-SYMBOL-NAME>*    ;VARIABLE\nnaf-mode-<NAF-SYMBOL-NAME> ;CONSTANT
Do not include `-', `*', etc. This function does not check the value given.
When INSRTP is non-nil or called-interactively insert templates at point.
Does not move point.\n
:SEE-ALSO `mon-build-naf-mode-xref', `mon-insert-naf-mode-xref-template'.\n►►►"
  (interactive "sSymbol name for template do not prefix with -, *, etc. :\np")
  (let* ((v-name (concat "*naf-" naf-symbol-name "*"))
         (c-name (concat "naf-mode-" naf-symbol-name))
         (xref-name (car (mon-build-naf-mode-xref)))
         (v-c-template
          (concat 
           (mon-lisp-stamp)
           "\n(eval-and-compile\n"
           "(defvar " v-name "\n"
           "  '(\n"
           "    ;; <ELTS-OF `" v-name "' HERE>\n"
           "    )\n"
           "  \"*Keyword list of for `naf-mode' font-locking.\"))\n"
           ";;\n"
           "(eval-and-compile\n"
           "(defconst " c-name "\n"
           "  (regexp-opt " v-name " 'paren)))\n"
           ";;\n"
           "(eval-and-compile\n"
           "  (mon-help-swap-var-doc-const-val\n"
           "      " v-name " " c-name "\n"
           "      " xref-name "  )) ;; <INSERT-FACE-NAME-AFTER-XREF>\n"
           ";;\n"
           ";;(progn (makunbound '" v-name ") (unintern '" v-name ")\n"
           ";;       (makunbound '" c-name ") (unintern '" c-name ") \)")))
    (if (or insrtp intrp)
        (save-excursion 
          (newline) 
          (princ v-c-template (current-buffer)))
      v-c-template)))
;;
;;; :TEST-ME (mon-insert-naf-mode-var-const-template "test-template")
;;; :TEST-ME (mon-insert-naf-mode-var-const-template "test-template" t)

;;; ==============================
;;; :DEPRECATED :USE: `mon-insert-naf-mode-var-const-template'
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T14:36:47-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Thursday April 09, 2009 @ 05:52.20 PM - by MON KEY>
(defun mon-insert-naf-mode-constant-template (&optional constant-name insrtp intrp)
  "Insert elisp template for defining new font-lock constants for `naf-mode'.\n
:EXAMPLE\n(mon-insert-naf-mode-constant-template \"some-constant\")\n
:SEE-ALSO `*naf-mode-faces-as-displayed*', `mon-insert-naf-mode-faces-as-displayed',
`mon-insert-naf-mode-face-template'.\n►►►"
  (interactive "sGive the value for * naf-mode-*-flags: \ni\np")
  (let* ((cnst (if constant-name constant-name "!CONSTANT!"))
         (lcl (replace-regexp-in-string  " " "-" (concat "naf-" cnst "-flags")))
	 (con (replace-regexp-in-string " " "-" (concat "naf-mode-" cnst "-flags")))
	 (naf-lcl (replace-regexp-in-string "--" "-" lcl)) ; Match on trailing whitespace.
	 (naf-con (replace-regexp-in-string "--" "-" con))
	 (put-temp
	  (concat
          "\n" (mon-lisp-stamp)"\n"
          "\(let \(\(" naf-lcl "\n"        
          "       \(list \n"
          "	\"make\" \"a\" \"list\" \"of\"\n"
          "	\"double-quoted\" \"strings\" \"here\"\n"
          "        \)\)\)\n;;\n"
          "\(defconst "naf-con"                                        ;{Choice one of three}\n"
          ";;; (concat \"\\\\(\" (regexp-opt " naf-lcl" 'paren) \"\\\\(,?\\\\)\\\\)\")  ;Packs into a list\n"
          ";;; (concat \"^\"  (regexp-opt " naf-lcl" 'paren) )                ;Anchors at head of line\n"
          ";;; (concat \"\\\\<\" \(regexp-opt " naf-lcl " 'paren) \"\\\\>\"\)          ;Empty string word boundaries\n"
          "    \"Keywords for {describe " naf-lcl "}.\n"
          ":USED-IN `naf-mode' for font-locking with `NAF-MODE-*-FFACE'.\"\)\)\n\n"
          ";;; :TEST-ME " naf-con "\n\n"
          ";;(progn (makunbound \'" naf-con"\)\n"
          ";;  (unintern \'" naf-con "\) \)\n"
          ;; (mon-comment-divider-w-len 30)
          ";;; ==============================")))
    (when (or insrtp intrp) (save-excursion (insert put-temp)))
    put-temp))
;;
;;; :TEST-ME (mon-insert-naf-mode-constant-template "some-constant")
;;; :TEST-ME (mon-insert-naf-mode-constant-template "some-constant" t)
;;; :TEST-ME (mon-insert-naf-mode-constant-template nil)
;;; :TEST-ME (mon-insert-naf-mode-constant-template nil t)
;;; :TEST-ME (call-interactively 'mon-insert-naf-mode-constant-template)

;;; ==============================
(defun mon-insert-naf-mode-face-template (&optional face-name insrtp intrp)
  "Insert Elisp template for new face definitions and constants.\n
Use to make face templates for fontlocking `naf-mode' keywords.\n
:EXAMPLE\n\(mon-insert-naf-mode-face-template \"some-face-name\")
:SEE-ALSO `*naf-mode-faces-as-displayed*',`mon-insert-naf-mode-faces-as-displayed' 
`mon-insert-naf-mode-constant-template'.\n►►►"
  (interactive "sGive the value for * in naf-mode-*-face: \ni\np")
  (let* ((fc-nme   (if face-name face-name "!FACE-NAME!"))
        (the-face  (concat "naf-mode-" fc-nme "-face"))
        (the-fface (concat "naf-mode-" fc-nme "-fface"))
        (put-fc-temp
         (concat
          "\n" (mon-lisp-stamp) "\n"
          "\(defface " the-face "\n"
          "  '(;;\(t \(:inherit naf-mode-*-face\)\)\)\n"
          "    ;;OR:\n"
          "    ;;\(\(\(class color\) \(background light\)\) \(:foreground \"SOME-COLOR\"\)\)\n"
          "    ;; \(\(\(class color\) \(background dark\)\) \(:foreground \"SOME-COLOR\"\)\)\n"
          "    ;;   \(t \(:bold t :italic t\)\)\)\n"
          "    \"*Face for font-locking of {DESCRIBE} in .naf files.\n"
	  ":KEYWORD-REGEXPS-IN\n"
          ":KEYWORD-LISTS-IN\n"
          ":FACE-DOCUMENTED-IN `" the-fface"'.\n:SEE-ALSO\n:USED-IN `naf-mode'.\"\n"
          "  	    :group \'naf-mode\n"
          "  	    :group \'naf-mode-faces\)\n;;\n"
          "(defvar " the-fface " '" the-face "\n"
          "    \"*Face for font-locking of {DESCRIBE} in `naf-mode'.\n"
	  ":KEYWORD-REGEXPS-IN\n"
          ":KEYWORD-LISTS-IN\n"
          ":FACE-DEFINED-IN `" the-face "'.\n:SEE-ALSO\n.\")\n\n"
          ";;; :TEST-ME (describe-face '" the-face ")\n\n"
          ";;(progn (makunbound \'" the-face "\)\n" 
          ";;  (makunbound \'" the-fface"\)\n"
          ";;  (unintern \'" the-face "\)\n"
          ";;  (unintern \'" the-fface "\) \)\n\n"
          ;; (mon-comment-divider-w-len 30)
          ";;; ==============================")))
    (when (or insrtp intrp) (save-excursion (insert put-fc-temp)))
    put-fc-temp))
;;
;;; :TEST-ME (mon-insert-naf-mode-face-template "some-face-name" t)
;;; :TEST-ME (mon-insert-naf-mode-face-template  nil t)
;;; :TEST-ME (call-interactively 'mon-insert-naf-mode-face-template)
;;; :TEST-ME (mon-insert-naf-mode-face-template "some-face-name")

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-20T17:47:38-05:00Z}#{10076} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-09-01T15:47:05-04:00Z}#{09362} - by MON KEY>
(defun mon-insert-naf-mode-faces-as-displayed (&optional insrtp intrp)
  "Insert font-locked keywords to test fruitsaladness `naf-mode' face/constants.\n
:SEE-ALSO `*naf-mode-faces-as-displayed*',`mon-insert-naf-mode-face-template',
`mon-insert-naf-mode-constant-template'.\n►►►"
 (interactive "i\np")
 (let ((i-fad (mapconcat #'identity *naf-mode-faces-as-displayed* "\n")))
   (if (or insrtp intrp)
       (mon-naf-mode-toggle-restore-llm
           (save-excursion (insert i-fad)))
       i-fad)))             
;;
;;; :TEST-ME (mon-insert-naf-mode-faces-as-displayed)
;;; :TEST-ME (mon-insert-naf-mode-faces-as-displayed t)
;;; :TEST-ME (call-interactively 'mon-insert-naf-mode-faces-as-displayed)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-01T17:56:21-04:00Z}#{09362} - by MON KEY>
(defvar *naf-mode-faces-as-displayed* 
  '(";;; =============================="
    "---"
    ":NAF-MODE-FACE `naf-mode-field-face-db-entry'"
    "FRBNF14942139 "
    "---"
    ":NAF-MODE-FACE `naf-mode-field-face'"
    "Uploaded-by: "
    "---"
    ":NAF-MODE-FACE `naf-mode-db-entry-face'"
    "Artist-naf: "
    "---"
    ":NAF-MODE-FACE `naf-mode-group-period-style-face'"
    "Bauhaus "
    "---"
    ":NAF-MODE-FACE `naf-mode-event-face'"
    "Salon de "
    "---"
    ":NAF-MODE-FACE `naf-mode-institution-face'"
    "Royal Academy "
    "---"
    ":NAF-MODE-FACE `naf-mode-nationality-face'"
    "français "
    "---"
    ":NAF-MODE-FACE `naf-mode-name-divider-face'"
    "Lastname, Firstname | Fname Lname | F. Lname " 
    "---"
    ":NAF-MODE-FACE `naf-mode-delim-face'"
    "---"
    "\n---"
    ":NAF-MODE-FACE "
    "Washington ;`naf-mode-place-face'"
    "---"
    ":NAF-MODE-FACE `naf-mode-date-face'"
    "\(1885-1940\)"
    "---"
    ":NAF-MODE-FACE `naf-mode-secondary-role-face'"
    "affichiste "
    "---"
    ":NAF-MODE-FACE `naf-mode-primary-role-face'"
    "Auteur "
    "---"
    ":NAF-MODE-FACE `naf-mode-art-keywords-role-face'"
    "illustrations "
    "---"
    ":NAF-MODE-FACE `naf-mode-benezit-face'"
    "VENTES PUBLIQUES :"
    "---"
    ":NAF-MODE-FACE `naf-mode-alternate-name-face'"
    "Portrait de "
    "---"
    ":NAF-MODE-FACE `naf-mode-publication-periodical-face'"
    "Berliner Illustrierte "
    "---"
    ":NAF-MODE-FACE `naf-mode-awards-prizes-face'"
    "Design Centre Awards Scheme "
    "---"
    ":NAF-MODE-FACE `naf-mode-timestamp-face'"
    "<Timestamp: #{2009-08-13T17:30:19-04:00Z}#{09334} - by MON>"
    "accessed: #{2009-09-01T16:31:39-04:00Z}#{09362} - MON KEY"
    "(URL `http://catalog.loc.gov/')"
    "(URL `http://authorities.loc.gov/')"
    "(URL `http://catalogue.bnf.fr/ark:/12148/cb123349648/PUBLIC')"
    "accessed:"
    ""
    "---"
    ":NAF-MODE-VARIABLE `naf-mode-field-names'"
    ":NAF-MODE-FACE     `naf-mode-db-entry-face'"
    "---"
    ""
    ";; -\*- mode: NAF; -\*-"
    "BNF:"
    "DNB:"
    "LOC:"
    "LOC-P&P:"
    "ULAN:"
    "OCLC:"
    "OTHER-DB:"
    "COPAC:"
    "Bios:"
    "Artist-naf:"
    "Artist-doc:"
    "Book-naf:"
    "Book-doc:"
    "People-naf:"
    "People-doc:"
    "Author-naf:"
    "Author-doc:"
    "Brand-naf:"
    "Brand-doc:"
    "non-posting-source:"
    "non-posting-wiki-source:"
    "non-posting-ebay-source:"
    "non-posting-philsp-source:"
    "non-posting-imdb-source:"
    "non-posting-internet-source:"
    "non-posting-benezit-source:"
    "references:"
    "source:"
    "Source:"
    ";;; brand-naf EOF"
    ";;; artist-naf EOF"
    ";;; people-naf EOF"
    ";;; book-naf EOF"
    ";;; author-naf EOF"
    ";;; item-naf EOF"
    ""
    "---"
    ":NAF-MODE-VARIABLE `naf-mode-db-entry'"
    ":NAF-MODE-FACE `naf-mode-db-field-face'"
    "---"
    ""
    "Abbreviated Title:"
    "Accession No:"
    "Author(s):"
    "CALL NUMBER:"
    "CONTROL #:"
    "CREATED/PUBLISHED:"
    "CREATOR:"
    "Class Descriptors:"
    "Content-and-subjects:"
    "Continues:"
    "Corporate Name:"
    "Current Frequency:"
    "DIGITAL ID:"
    "Database:"
    "Description:"
    "Descriptor:"
    "Dewey Class No.:"
    "Display-name:"
    "Display-title:"
    "Document Type:"
    "Edition Information:"
    "Entry:"
    "FORMAT:"
    "Found In:"
    "Frequency:"
    "Genre/Form:"
    "Genre:"
    "Geographic Area Code:"
    "HEADING:"
    "Heading:"
    "ISSN:"
    "Identifier:"
    "Language:"
    "Location-country:"
    ""
    "---"
    ":NAF-MODE-FIELDS-MEDIUM"
    "---"
    ""
    "Main Title:"
    "Main author:"
    "Material Type:"
    "Movie-posters:"
    "NOTES:"
    "Named Person:"
    "Note(s):"
    "Note:"
    "Notes:"
    "Number-of-illustrations:"
    "Number-of-pages:"
    "Number-of-volumes:"
    "Other Edition Available:"
    "Other System No.:"
    "Other Titles:"
    "Other names:"
    "Other-roles:"
    "Personal Name:"
    "Physical desc\.:"
    "Preceding Title:"
    "Publication :"
    "Publication Dates:"
    "Publication:"
    "Published-by:"
    "Published/Created:"
    "Publisher Location:"
    "Quality Code:"
    "REPOSITORY:"
    "REPRODUCTION NUMBER:"
    "RIGHTS INFORMATION:"
    "Relevance:"
    "Repository:"
    "Reproduction No./Source:"
    "Responsibility:"
    "SUBJECTS:"
    "SUMMARY:"
    "Scope Note:"
    "Search Also Under:"
    "Series Title:"
    "Special Note:"
    "Special-notes:"
    "Standard No:"
    "Subjects:"
    "Succeeding Title:"
    "Summary:"
    ""
    "---"
    ":NAF-MODE-FIELDS-TITLE"
    "---"
    ""
    "Title details:"
    "Title:"
    "Type :"
    "Type of Material:"
    "Uniform Title:"
    "Update:"
    "Used For/See From:"
    "Year:"
    ""
    "---"
    ":NAF-MODE-FIELDS-NAF"
    "---"
    ""
    "Ads-for:                                          ; :NAF-FIELD"
    "Appeared-in:                                      ; :NAF-FIELD"
    "Artists-associated:                               ; :NAF-FIELD"
    "Auction-records:                                  ; :NAF-FIELD"
    "Authors-associated:                               ; :NAF-FIELD"
    "Book-notes:                                       ; :NAF-FIELD"
    "Brand-name:                                       ; :NAF-FIELD"
    "Contents:                                         ; :NAF-FIELD"
    "Date-founded:                                     ; :NAF-FIELD"
    "Founded-by:                                       ; :NAF-FIELD"
    "Full-title:                                       ; :NAF-FIELD"
    "Location-published:                               ; :NAF-FIELD"
    "Products-associated:                              ; :NAF-FIELD"
    "Publisher:                                        ; :NAF-FIELD"
    "Slogans:                                          ; :NAF-FIELD"
    "Uploaded-by:                                      ; :NAF-FIELD"
    "Used-for:                                         ; :NAF-FIELD"
    "ebay-item-ended:                                  ; :NAF-FIELD"
    "ebay-item-number:                                 ; :NAF-FIELD"
    "ebay-item-realized:                               ; :NAF-FIELD"
    "ebay-item-seller:                                 ; :NAF-FIELD"
    ""
    "---"
    ":NAF-MODE-FIELDS-SHARED"
    "---"
    ""
    "Born:                                             ; :NAF-FIELD :ULAN-FIELD"
    "Died:                                             ; :NAF-FIELD :ULAN-FIELD"
    "Roles:                                            ; :NAF-FIELD :ULAN-FIELD"
    ""
    "---"
    ":NAF-MODE-FIELDS-ULAN"
    "---"
    ""
    "Biographies:                                      ; :ULAN-FIELD"
    "Birth and Death Places:                           ; :ULAN-FIELD"
    "Events:                                           ; :ULAN-FIELD"
    "Gender:                                           ; :ULAN-FIELD"
    "ID:                                               ; :ULAN-FIELD"
    "List/Hierarchical Position:                       ; :ULAN-FIELD"
    "Names:                                            ; :ULAN-FIELD"
    "Nationalities:                                    ; :ULAN-FIELD"
    "Record Type:                                      ; :ULAN-FIELD"
    "Related Names:"
    "Related People and Corporate Bodies:              ; :ULAN-FIELD"
    "Related People or Corporate Bodies:               ; :ULAN-FIELD"
    "Sources and Contributors:                         ; :ULAN-FIELD"
    "Subject:                                          ; :ULAN-FIELD"
    "education:                                        ; :ULAN-FIELD :TRL-WSPC"
    ""
    "---"
    ":NAF-MODE-FIELDS-LOC"
    "---"
    ""
    "Biographical/Historical Note:                     ; :LOC-FIELD"
    "LC Class Number:                                  ; :LOC-FIELD"
    "LC Classification:                                ; :LOC-FIELD"
    "LC Control No.                                    ; :LOC-FIELD"
    "LC Control Number:                                ; :LOC-FIELD"
    "LC Copy:                                          ; :LOC-FIELD"
    "LCCN Permalink:"
    ""
    "---"
    ":NAF-MODE-VARIABLE `naf-mode-field-names-bnf'"
    ":NAF-MODE-FACE     `naf-mode-field-bnf-face'"
    "---"
    ""
    "<Employé pour :"
    "Appartient au recueil :"
    "Auteur(s) :                                       ; :BNF-FIELD :TRL-WSPC"
    "Autre(s) auteur(s) :                              ; :BNF-FIELD"
    "Autre(s) forme(s) du titre :                      ; :BNF-FIELD"
    "Circuit de distribution :                         ; :BNF-FIELD :TRL-WSPC"
    "Classement géographique :                         ; :BNF-FIELD"
    "Comprend :                                        ; :BNF-FIELD :TRL-WSPC"
    "Coordonnées géographiques :                       ; :BNF-FIELD"
    "Cote(s) BnF :                                     ; :BNF-FIELD"
    "Création :                                        ; :BNF-FIELD"
    "Description matérielle :                          ; :BNF-FIELD"
    "Distributeur :                                    ; :BNF-FIELD :TRL-WSPC"
    "Domaine(s) :                                      ; :BNF-FIELD :TRL-WSPC"
    "Domaine(s) :                                      ; :BNF-FIELD"
    "Domaine(s) d'expression artistique :              ; :BNF-FIELD"
    "Enregistrement :                                  ; :BNF-FIELD"
    "Forme(s) rejetée(s) :                             ; :BNF-FIELD"
    "Genre :                                           ; :BNF-FIELD :TRL-WSPC"
    "Indice de l'Histoire de France :                  ; :BNF-FIELD"
    "Indice(s) Dewey :                                 ; :BNF-FIELD"
    "Interprète(s) :                                   ; :BNF-FIELD"
    "Langue(s) :                                       ; :BNF-FIELD"
    "Lien au titre d'ensemble :                        ; :BNF-FIELD"
    "Lien à la collection :                            ; :BNF-FIELD"
    "Marque :                                          ; :BNF-FIELD :TRL-WSPC"
    "Mort :                                            ; :BNF-FIELD"
    "Naissance :                                       ; :BNF-FIELD"
    "Nation(s) :                                       ; :BNF-FIELD :TRL-WSPC"
    "Nationalité(s) :                                  ; :BNF-FIELD"
    "Note(s) :                                         ; :BNF-FIELD"
    "Notice n° :                                       ; :BNF-FIELD"
    "Numérotation :                                    ; :BNF-FIELD"
    "Participant(s) :                                  ; :BNF-FIELD :TRL-WSPC"
    "Producteur(s) :                                   ; :BNF-FIELD"
    "Profession(s) :                                   ; :BNF-FIELD"
    "Projection :                                      ; :BNF-FIELD :TRL-WSPC"
    "Publication :                                     ; :BNF-FIELD :TRL-WSPC"
    "Périodicité :                                     ; :BNF-FIELD"
    "Responsabilité(s) exercée(s) sur les documents :  ; :BNF-FIELD"
    "Référence(s) commerciale(s) :                     ; :BNF-FIELD"
    "Réunit :                                          ; :BNF-FIELD :TRL-WSPC"
    "Sexe :                                            ; :BNF-FIELD"
    "Source(s) :                                       ; :BNF-FIELD :TRL-WSPC"
    "Sujet(s) :                                        ; :BNF-FIELD"
    "Sujet(s) géographique(s) :                        ; :BNF-FIELD"
    "Technique(s) privilégiée(s) :                     ; :BNF-FIELD"
    "Thème(s) :                                        ; :BNF-FIELD :TRL-WSPC"
    "Titre clé :                                       ; :BNF-FIELD"
    "Titre d'ensemble :                                ; :BNF-FIELD"
    "Titre(s) :                                        ; :BNF-FIELD"
    "Titre(s) en liaison :                             ; :BNF-FIELD"
    "Type de la collectivité officielle :              ; :BNF-FIELD"
    "Type de publication :                             ; :BNF-FIELD"
    "Type de ressource électronique :                  ; :BNF-FIELD"
    "Typologie :                                       ; :BNF-FIELD :TRL-WSPC"
    "Échelle(s) :                                      ; :BNF-FIELD"
    "Éditeur :                                         ; :BNF-FIELD :TRL-WSPC"
    "Édition :                                         ; :BNF-FIELD :TRL-WSPC"
    ""
    "---"
    ":NAF-MODE-VARIABLE `naf-mode-db-field-flags-bnf'"
    "---"
    ""
    "forme internationale                              ; :BNF-FIELD-ENTRY"
    "Mise à jour :                                     ; :BNF-FIELD-ENTRY"
    "masculin                                          ; :BNF-FIELD-ENTRY"
    "féminin                                           ; :BNF-FIELD-ENTRY"
    ""
    "---"
    ":NAF-MODE-FACE `naf-mode-db-field-entry-ulan-face'"
    "---"
    ""
    "[500006383]                                       ; :ULAN-FIELD-ENTRY"
    "NAFL2001060907 NAFR8914343 NAFR907811             ; :ULAN-FIELD-ENTRY"
    "(preferred, index, display, V)                    ; :ULAN-FIELD-ENTRY"
    "(inhabited place)                                 ; :ULAN-FIELD-ENTRY"
    "(preferred, index, V)                             ; :ULAN-FIELD-ENTRY"
    "(preferred, index)                                ; :ULAN-FIELD-ENTRY"
    "(preferred)                                       ; :ULAN-FIELD-ENTRY"
    "(display, V)                                      ; :ULAN-FIELD-ENTRY"
    "(display)                                         ; :ULAN-FIELD-ENTRY"
    "(index)                                           ; :ULAN-FIELD-ENTRY"
    "(V)                                               ; :ULAN-FIELD-ENTRY"
    "---"
    "#{American painter, illustrator, and printmaker, 1879-1941} #{500008013})"
    "(:TEACHER-OF #{Weber, Sarah S. Stilwell}"
    ""
    "---"
    ":NAF-MODE-FACE `naf-mode-ulan-ppl-corp-face'"
    "---"
    ""
    ":APPRENTICE-OF                                    ; :ULAN-FIELD-ENTRY"
    ":APPRENTICE-WAS                                   ; :ULAN-FIELD-ENTRY"
    ":ASSISTED-BY                                      ; :ULAN-FIELD-ENTRY"
    ":ASSOCIATE-OF                                     ; :ULAN-FIELD-ENTRY"
    ":CHILD-OF                                         ; :ULAN-FIELD-ENTRY"
    ":COLLABORATED-WITH                                ; :ULAN-FIELD-ENTRY"
    ":FOUNDER-OF                                       ; :ULAN-FIELD-ENTRY"
    ":GRANDCHILD-OF                                    ; :ULAN-FIELD-ENTRY"
    ":GRANDPARENT-OF                                   ; :ULAN-FIELD-ENTRY"
    ":GRANDPARENT-WAS                                  ; :ULAN-FIELD-ENTRY"
    ":INFLUENCE                                        ; :ULAN-FIELD-ENTRY"
    ":MEMBER-OF                                        ; :ULAN-FIELD-ENTRY"
    ":PARENT-OF                                        ; :ULAN-FIELD-ENTRY"
    ":PARTNER-OF                                       ; :ULAN-FIELD-ENTRY"
    ":SIBLING-OF                                       ; :ULAN-FIELD-ENTRY"
    ":SPOUSE-OF                                        ; :ULAN-FIELD-ENTRY"
    ":STUDENT-OF                                       ; :ULAN-FIELD-ENTRY"
    ":STUDENT-WAS                                      ; :ULAN-FIELD-ENTRY"
    ":TEACHER-OF                                       ; :ULAN-FIELD-ENTRY"
    ":TEACHER-WAS                                      ; :ULAN-FIELD-ENTRY"
    ":WORKED-WITH                                      ; :ULAN-FIELD-ENTRY"
    ""
    "---"
    ":NAF-MODE-CURRENCIES"
    "---"
    ""
    "USD 50 000 000"
    "USD 5,000,000"
    "GBP 5 000"
    "USD 5,000"
    "USD 5000"
    "USD 40 000"
    "USD 40,000"
    "USD 1 000 000"
    "ARS 50 000 000"
    "ATS 5,000"
    "AUD 500 100 000"
    "BEF 5,000"
    "BRL 5,000"
    "CAD 5 000"
    "CHF 5,000,000"
    "DEM 5,000"
    "DKK 5 000"
    "EGP 40 000"
    "ESP 40,000"
    "FRF 1 000 000"
    "GBP 50 000 000"
    "GRD 5,000"
    "HKD 500 100 000"
    "HUF 5,000"
    "IEP 5,000"
    "ILS 5 000"
    "ITL 5,000,000"
    "JPY 5,000"
    "NLG 5 000"
    "PTE 40 000"
    "SEK 40,000"
    "SGD 1 000 000"
    "TWD 50 000 000"
    "USD 5,000"
    "UYU 500 100 000"
    "ZAR 5,000"
    ""
    "---"
    ":NAF-MODE-VARIABLE-DATE `*naf-mode-dates-xref*'"
    ":NAF-MODE-VARIABLE-DATE `naf-mode-french-days'"
    ":NAF-MODE-VARIABLE-DATE `naf-weekday-alist'"
    ":NAF-MODE-VARIABLE-DATE `naf-mode-english-dates'"
    ":NAF-MODE-VARIABLE-DATE `naf-month-abbrev-alist'"
    ":NAF-MODE-VARIABLE-DATE `naf-weekday-alist'"
    "---"
    ""
    "Jan." "January"           "Jan. 01, 1899" "January 10, 1946"		          "Janvier"  
    "Feb." "February" 	       "Feb. 22, 1922" "February 29, 1704"		          "Février"  
    "Mar." "March"	       "Mar. 15, 1917" "March 5, 1933"			          "Mars"     
    "Apr." "April" 	       "Apr. 20, 1902" "April 20, 1884"			          "Avril"    
    "May" 		       "May 9, 1934"					          "Mai"      
    "Jun." "June" 	       "Jun. 30, 1895" "June 29, 1946"			          "Juin"     
    "Jul." "July"	       "Jul. 31, 1901" "July 30, 1975"			          "Juillet"  
    "Aug." "August" 	       "Aug. 6th, 1855" "August 24, 1995"		          "Août"     
    "Sep." "Sept." "September" "Sep. 16th, 1992" "Sept. 11th, 2001" "September 03, 1929"  "Septembre"
    "Oct." "October" 	       "Oct. 24th, 1928" "October 19, 1987" "October 3rd, 2008"   "Octobre"  
    "Nov." "November"	       "Nov. 4th, 1901" "November 21st, 1920"		          "Novembre" 
    "Dec." "December"          "Dec. 3rd, 1901" "December 14, 1825"                       "Décembre" 
    ""
    "---"
    ""
    "Monday     Lundi"	  
    "Tuesday    Mardi"	  
    "Wednesday  Mercredi"
    "Thursday   Jeudi"	  
    "Friday     Vendredi"
    "Saturday   Samedi"  
    "Sunday     Dimanche"
    ""
    "---"
    ":NAF-MODE-DATE-VARIABLE `naf-mode-active-date'"
    ":NAF-MODE-DATE-VARIABLE `naf-mode-active-date-flags-solo'"
    ":NAF-MODE-DATE-VARIABLE `naf-mode-circa-dates'"
    "---"
    ""
    " Actif en"
    " Actif à"
    " actif en "
    " actif à "
    " active circa "
    " active Circa  "
    " active c. "
    " (active ca. "
    " active ca "
    " active cca. ) "
    " Active c. "
    " Active ca.  "
    " Active ca "
    " Active cca. "
    " (active circa "
    " (active Circa "
    " (active c. 1888-1992)"
    " (active ca. "
    " (active ca "
    " (active cca. "
    " (Active c. "
    " (Active ca. "
    " (Active ca "
    " (Active cca. "
    " (c "
    " (c. "
    " (ca. "
    " (ca "
    " (cca. "
    " (ca. "
    " (ca "
    " (cca. "
    "  cca. "
    "  ca. ")
  "List of `naf-mode' keywords.\n
List includes the vars that define them or hold regexps that do.
List also identifies the type of field and and faces that light them up.
Use to test naf-mode font-locking.\n
:CALLED-BY `mon-insert-naf-mode-faces-as-displayed'.
:SEE-ALSO `mon-insert-naf-mode-face-template',
`mon-insert-naf-mode-constant-template'.\n►►►")
;;
;;; :TEST-ME  *naf-mode-faces-as-displayed* 
;;;(progn (makunbound '*naf-mode-faces-as-displayed*)
;;;       (unintern '*naf-mode-faces-as-displayed*) )

;;; ==============================
(provide 'naf-mode-insertion-utils)
;;; ==============================

;;; ================================================================
;;; naf-mode-insertion-utils.el ends here
;;; EOF

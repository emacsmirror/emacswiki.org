;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-mode-insertion-utils.el
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
;;; `mon-insert-naf-mode-face-template', `mon-insert-face-as-displayed',
;;; `mon-insert-naf-file-in-dirs',
;;; `mon-insert-naf-mode-xref-template', `mon-build-naf-mode-xref'
;;; `naf-mode-variable-constant-template'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS: 
;;; `*naf-mode-faces-as-displayed*'
;;; VARIABLES:
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;; `npes' -> `non-posting-ebay-source'
;;; `non-posting-philsp-source' ->  `npps'
;;; `npws' -> `non-posting-wiki-src'
;;; `constance-insert-copyright' -> `mon-insert-copyright'
;;; `mon-insert-naf-mode-file-template' -> `mon-insert-file-template'
;;;
;;; DEPRECATED:
;;; `npps' -> `mon-cln-philsp'
;;; `mon-insert-naf-mode-constant-template' 
;;;   -> `mon-insert-naf-mode-variable-constant-template'
;;;
;;; RENAMED: 
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
;;; Snippet for dealing with longlines-mode.
;;; 	 (test-llm (buffer-local-value longlines-mode (current-buffer)))
;;; 	 (is-on (and test-llm))
;;; 	 (llm-off))
;;; (if (or insertp intrp)
;;; 	(save-excursion
;;; 	  (when is-on (longlines-mode 0) (setq llm-off 't))
;;; 	  (insert non-ps)
;;; 	  (when llm-off (longlines-mode 1) (setq llm-off 'nil)))
;;;   non-ps)))
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
;;; ©opyright (C) - MON KEY - 2009
;;; ==============================
;;; CODE:

;;; ==============================
(defun naf-tab-region (beg end &optional arg)   
  "Indent a region by one tab in a NAF buffer.\n
Used in `naf-mode'."
  (interactive "r\nP")
  (indent-rigidly beg end tab-width)
  (exchange-point-and-mark))

;;; =======================
(defun naf-comment-line ()
  "Comment out line in a NAF file.\n
See also; `naf-uncomment-line', `naf-comment-prefix', `naf-uncomment-region',
`naf-comment-region'.\nUsed in `naf-mode'."
  (interactive)
  (save-excursion
    (back-to-indentation)
  	(insert naf-comment-prefix)))

;;; =======================
(defun naf-uncomment-line ()
  "Uncomment line in a NAF file.\n
See also; `naf-comment-prefix',`naf-uncomment-line', `naf-uncomment-region',
`naf-comment-region'.\nUsed in `naf-mode'."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (while (eq (char-after) 59) (delete-char 1))))

;;; =======================
(defun naf-comment-region (beg end &optional arg)
  "Comment out region in a NAF file.\n
See also;`naf-comment-prefix', `naf-uncomment-region',`naf-comment-line',
`naf-uncomment-line'.\nUsed in `naf-mode'."
  (interactive "r\nP")
  (let ((comment-start naf-comment-prefix))
    (comment-region beg end arg)))

;;; =======================
(defun naf-uncomment-region (beg end &optional arg)
  "Uncomment region in a NAF file.\n
See also; `naf-comment-prefix',`naf-comment-region',
`naf-comment-line',`naf-uncomment-line'.\nUsed in `naf-mode'."
  (interactive "r\nP")
  (let ((comment-start naf-comment-prefix))
    (comment-region beg end -1)))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T12:53:18-0400Z - by MON KEY>
(defun non-posting-source (&optional insertp intrp)
  "Inserts the vanilla non-posting-source flag.
Inserts newline after :(colon)\nEXAMPLE:
\"-\nnon-posting-source:\"\n 
See also; `nps', `non-posting-internet-source', `non-posting-wiki-source',
`non-posting-ebay-source', `non-posting-imdb-source', `non-posting-philsp-source',
`non-posting-benezit-source',`benezit-naf-template'.\nUsed in `naf-mode'. "
  (interactive "i\np")
(mon-naf-mode-toggle-restore-llm
 (let* ((non-ps (format "\n-\nnon-posting-source:\n")))
   ;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
   ;; (is-on (and test-llm))
   ;; (llm-off))
   (if (or insertp intrp)
       (save-excursion
         ;;(when is-on (longlines-mode 0) (setq llm-off 't))
         (insert non-ps))
     ;;(when llm-off (longlines-mode 1) (setq llm-off 'nil)))
     non-ps))))
;;
;; This defalias is probably better as an abbrev.
;;
(defalias 'nps 'non-posting-source)

;;;test-me;(non-posting-source)
;;;test-me;(non-posting-source t)
;;;test-me;(call-interactively 'non-posting-source)

;;; =======================
;;; CREATED: <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T13:00:15-0400Z - by MON KEY>
(defun non-posting-ebay-source (&optional insertp intrp)
  "Inserts the non-posting ebay stamp into a NAF buffer. 
\"-\nnon-posting-ebay-source:\nebay-item-number:\nebay-item-seller:\nebay-item-realized:
ebay-item-ended:\naccessed: Tuesday April 01, 2009\"\n
Used in `naf-mode'. See also; `non-posting-source', `non-posting-internet-source',
`non-posting-wiki-source', `non-posting-imdb-source', `non-posting-philsp-source',
`non-posting-benezit-source',`benezit-naf-template'."
  (interactive "i\np")
(mon-naf-mode-toggle-restore-llm
(let* ((non-pes 
       (concat 
        "-\n"                         ;; (insert "-")(newline)                                  
        "non-posting-ebay-source:\n"  ;; (insert "non-posting-ebay-source:")(newline) 
        "ebay-item-number: \n"        ;; (insert "ebay-item-number: ") (newline)      
        "ebay-item-seller: \n"        ;; (insert "ebay-item-seller: ") (newline)      
        "ebay-item-realized: \n"      ;; (insert "ebay-item-realized: ") (newline)    
        "ebay-item-ended: \n"         ;; (insert "ebay-item-ended: ") (newline)       
        (mon-accessed-stamp)"\n"     ;; (mon-accessed-stamp t)                      
        "---"))
 	 ;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
 	 ;; (is-on (and test-llm))
 	 ;; (llm-off)
       )
  (if (or insertp intrp)
      (save-excursion
	;;(when is-on (longlines-mode 0) (setq llm-off 't))
	(insert non-pes))
    ;; (when llm-off (longlines-mode 1) (setq llm-off 'nil)))
   non-pes))))
;;
;; This defalias is probably better as an abbrev.
(defalias 'npes 'non-posting-ebay-source)

;;;test-me:(non-posting-ebay-source)
;;;test-me:(non-posting-ebay-source t)
;;;test-me:(call-interactively 'non-posting-ebay-source)

;;; ==============================
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T13:04:43-0400Z - by MON KEY>
(defun non-posting-wiki-source (&optional insertp intrp)
  "Inserts the non-posting-wiki-source timestamp in a NAF buffer. e.g.\n
\"-\nnon-posting-wiki-source:
accessed:  Saturday January 10, 2009 - by MON\n---\"\n
See also; `non-posting-source', `non-posting-internet-source',
`non-posting-ebay-source', `non-posting-imdb-source', `non-posting-philsp-source',
`non-posting-benezit-source', `non-posting-philsp-source',`benezit-naf-template'.\n
Used in `naf-mode'."
  (interactive "i\np") 
(mon-naf-mode-toggle-restore-llm
(let* ((non-pws 
       (concat "-\n"                        ;;(insert "-")(newline)
               "non-posting-wiki-source:\n" ;;(insert "non-posting-wiki-source:")(newline)
               (mon-accessed-stamp)"\n"    ;;(mon-accessed-stamp t) (newline)
               "---"))
 	 ;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
 	 ;; (is-on (and test-llm))
 	 ;; (llm-off)
	 )
  (if (or insertp intrp)
      (save-excursion
	;;(when is-on (longlines-mode 0) (setq llm-off 't))
	(insert non-pws))
    ;;(when llm-off (longlines-mode 1) (setq llm-off 'nil)))
   non-pws))))
;;
;; This defalias is probably better as an abbrev. 
(defalias 'npws 'non-posting-wiki-src)

;;;test-me;(non-posting-wiki-source)
;;;test-me;(non-posting-wiki-source t)
;;;test-me;(call-interactively 'non-posting-wiki-source)

;;; ==============================
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T13:09:26-0400Z - by MON KEY>
(defun npps (&optional insertp intrp)
  "Insert philsp non-posting-source timestamp.\n
EXAMPLE:\n\(non-posting-philsp-source\)\n-\n non-posting-philsp-source:\n
\(URL `http://www.philsp.com/homeville/FMI/a7.htm')
accessed:  Saturday January 10, 2009 - by MON\n ---\n
Use is deprecated in lieu of `mon-cln-philsp'. Used only for manual cleaning.\n
See also; `non-posting-source', `non-posting-wiki-source',
`non-posting-internet-source', `non-posting-ebay-source',
`non-posting-benezit-source', `benezit-naf-template' 
 `non-posting-imdb-source'\nUsed in `naf-mode'."
  (interactive "i\np") 
  (mon-naf-mode-toggle-restore-llm
   (let* ((non-pps  (concat 
		     "-\n"                                                
		     "non-posting-philsp-source:\n"
		     "(URL `http://www.philsp.com/homeville/FMI/a7.htm')\n"
		     (mon-accessed-stamp)"\n"
		     "---"))
	  ;;WAS:
	  ;;(insert "-")(newline)                                                 
	  ;;(insert "non-posting-philsp-source:")(newline)                        
	  ;;(insert "(URL `http://www.philsp.com/homeville/FMI/a7.htm')")(newline) 
	  ;;(mon-accessed-stamp t)(newline)                                      
	  ;;(insert "---")))                                                       
	  ;; ==============================
	  ;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
	  ;; (is-on (and test-llm))
	  ;; (llm-off)
	  )
     (if (or insertp intrp)
	 (save-excursion
	   ;;(when is-on (longlines-mode 0) (setq llm-off 't))
	   (insert non-pps))
       ;;(when llm-off (longlines-mode 1) (setq llm-off 'nil)))
       non-pps))))
;;
;; This aliasing scheme is backwards headed. 
;; However, I'm not sure if there was a kbd-macro somewhere that used `npps'
;; `non-posting-philsp-source' should be the base function with `npps' taking the alias.
;; Moreover, in either case the defalias is probably better as an abbrev.
;;
(defalias 'non-posting-philsp-source 'npps)

;;;test-me;(non-posting-philsp-source) 
;;;test-me;(non-posting-philsp-source t) 
;;;test-me;(call-interactively 'non-posting-philsp-source) 

;;; ==============================
;;; CREATED: <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
(defun non-posting-imdb-source (&optional insertp interp)
  "Insert the IMDB non-posting-source timestamp.\n
EXAMPLE:\n(non-posting-imdb-source)\n -\n non-posting-imdb-source:
\(URL `http://www.IMDB.com'\)\n accessed: Monday March 30, 2009 - MON\n
See also; `non-posting-source', `non-posting-wiki-source',
`non-posting-internet-source', `non-posting-ebay-source',
`non-posting-benezit-source', `benezit-naf-template'
`non-posting-philsp-source'.\nUsed in `naf-mode'."
  (interactive "i\np")
  (mon-naf-mode-toggle-restore-llm
   (let* ((npis (concat
                 "\n-\n"
                 "non-posting-imdb-source:\n"
                 "(URL `http://www.IMDB.com')\n"
                 (mon-accessed-stamp) "\n"))
          ;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
          ;; (is-on (and test-llm))
          ;; (llm-off)
          )
     (if (or insertp intrp)
         (save-excursion
           ;; (when is-on (longlines-mode 0) (setq llm-off 't))
           (insert non-pis))
       ;; (when llm-off (longlines-mode 1) (setq llm-off 'nil)))
       npis))))
;;
;;;test-me;(non-posting-imdb-source)
;;;test-me;(non-posting-imdb-source t)
;;;test-me;(call-interactively 'non-posting-imdb-source)

;;; ==============================
;;; CREATED: <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T13:18:40-0400Z - by MON KEY>
(defun non-posting-benezit-source (benezit-name volume page &optional insertp intrp)
  "Inserts the non-posting-benezit-source stamp.\n
EXAMPLE:\n\(non-posting-benezit-source \"Cappiello, Leonetto\" \"3\" \"444\" t\)
\n-\nnon-posting-benezit-source:
Cappiello, Leonetto - Benezit Volume 3 page 210\naccessed: Wednsday April 1, 2009 - MON\n
See also; `non-posting-source', `non-posting-wiki-source', `non-posting-internet-source',
`non-posting-ebay-source', `non-posting-imdb-source', `benezit-naf-template'.
Used in `naf-mode'."
  (interactive "sArtist Name (Lastname, Firstname):\nnVolume number:\nnPage number: \ni\np")
  (mon-naf-mode-toggle-restore-llm
   (let* ((non-pbs
           (concat
            "\n-\n"
            "non-posting-benezit-source:\n"
            benezit-name 
            " - Benezit: Volume " (if (numberp volume) (number-to-string volume) volume)
            " page" (cond ((numberp page) (concat " " (number-to-string page)))
                          ((not (eq (string-to-char (subseq page 0 1)) 32))(concat " " page)))
            "\n"
            (mon-accessed-stamp)"\n"
            "-\n"))
          ;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
          ;; (is-on (and test-llm))
          ;; (llm-off)
          )
     (if (or insertp intrp)
         (save-excursion
           ;;(when is-on (longlines-mode 0) (setq llm-off 't))
           (insert non-pbs))
       ;;(when llm-off (longlines-mode 1) (setq llm-off 'nil)))
       non-pbs))))

;;;test-me;(non-posting-benezit-source "Cappiello, Leonetto" 3 444 t)
;;;test-me;(non-posting-benezit-source "Cappiello, Leonetto" "3" "444" t)
;;;test-me;(call-interactively 'non-posting-benezit-source)

;;; ==============================
;;; CREATED: <Timestamp: Monday March 30, 2009 @ 04:32.15 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T13:59:43-0400Z - by MON KEY>
(defun non-posting-internet-source (&optional non-posting-url insertp intrp)
  "Returns a timestamped Emacs style url reference. 
Called interactively prompts for a URL name to wrap. 
When NON-POSTING-URL is non-nil wraps URL name.
When INSERTP is non-nil or when called interactively inserts the wrapped url.
When NON-POSTING-URL is nil defaults to \"(URL `')\".\n
EXAMPLE:
\(non-posting-internet-source \"http://www.emacswiki.com\")
\(non-posting-internet-source)\n
See also; `non-posting-source', `non-posting-wiki-source', `non-posting-imdb-source',
`non-posting-benezit-source', `non-posting-ebay-source', `benezit-naf-template'.\n
Used in `naf-mode'."
  (interactive "sURL:\ni\np")
  (mon-naf-mode-toggle-restore-llm
   (let* ((non-pis (concat 
		    "\n-\n"
		    "non-posting-internet-source:\n"
		    (cond (intrp (concat "(URL `" non-posting-url "')\n"))
			  (insertp (if non-posting-url 
				       (concat "(URL `" non-posting-url "')\n")
				     "(URL `')\n"))
			  ((and non-posting-url (not insertp) (not intrp))
			   (concat "(URL `" non-posting-url "')\n"))
			  (t "(URL `')\n"))
		    (mon-accessed-stamp)))
	  ;; (test-llm (buffer-local-value longlines-mode (current-buffer)))
	  ;; (is-on (and test-llm))
	  ;; (llm-off)
	  )
     (if (or insertp intrp)
	 (save-excursion
	   ;;(when is-on (longlines-mode 0) (setq llm-off 't))
	   (insert non-pis))
       ;;(when llm-off (longlines-mode 1) (setq llm-off 'nil)))
       non-pis))))

;;;test-me;(non-posting-internet-source)
;;;test-me;(non-posting-internet-source nil t)
;;;test-me;(non-posting-internet-source "http://www.derbycityprints.com")
;;;test-me;(non-posting-internet-source "http://www.derbycityprints.com" t)
;;;test-me;(call-interactively 'non-posting-internet-source)

;;; ==============================
;;; TODO: take an optional STARTING-DIR arg to default-dir and possibly an alt.
;;; conditional insertion routine of file's text e.g. artist-naf, brand-naf, etc.
;;; CREATED: <Timestamp: Thursday April 16, 2009 @ 07:48.26 PM - by MON KEY>
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
\"c:/home/my-dirs\"
Invoking the form with symbol list 'make-my-dirs' as argument to MAKE-DIR-LIST:\n
   \(mon-insert-naf-file-in-dirs make-my-dirs)\n
Or, interactively; M-x mon-insert-naf-file-in-dirs 
                   minibuffer-prompt: Give Symbol holind dir/file list :make-my-dirs\n
Creates the following directors and files in c:/home/my-dirs\n
  c:/home/my-dirs:
  |-- Lastname (Firstname Middlename Other)
  |   `-- Lastname, Firstname Middlename Other.naf
  |-- Lastname2 (Firstname2 Middlename2 Other2)
  |   `-- Lastname2, Firstname2 Middlename2 Other2.naf
  |-- Lastname3 (Firstname3 Middlename3 Other3)
  |   `-- Lastname3, Firstname3 Middlename3 Other3.naf
  `-- Lastname4 (Firstname4 Middlename4 Other4)
    `-- Lastname4, Firstname4 Middlename4 Other4.naf"
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

;;;(dired "c:\\Documents and Settings\\")
;;;(dired "c:/Documents and Settings/All Users/Start Menu")

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-26T20:00:09-04:00Z}#{09397} - by MON KEY>
(defun mon-build-naf-mode-xref ()
  "Return a list suitable for naf-mode variable xref template creation.
variable name is generated from current naf-mode filename.
Signals an error if filename is void or not a 'naf-mode-' prefixed filename.
Elements of list are returned as three strings:
\(\"*naf-mode-insertion-utils-xrefs*\"  ;xref-name
 \"naf-mode-insertion-utils\"          ;pkg-name
 \"naf-mode-insertion-utils.el\"\)      ;filename
CALLED-BY: `mon-insert-naf-mode-xref-template'
           `naf-mode-variable-constant-template'."
  (if (buffer-file-name)
    (let* ((nm-match-str (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
           (nm-match-on  (string-match "naf-mode-" nm-match-str))
           (the-nm-matched (if nm-match-on (match-string nm-match-on nm-match-str)))
           (to-next-let))
           (setq to-next-let
                 (if the-nm-matched (cons nm-match-str
                                          (cadr (split-string nm-match-str the-nm-matched)))))
           (let* ((xref-p (if to-next-let;(mon-build-naf-mode-xref)
                              to-next-let;(mon-build-naf-mode-xref)
                            (error "not a naf-mode-file")))
                  (xref-fname (if xref-p (concat (car xref-p) ".el")))
                  (xref-pkg (if xref-p (car xref-p)))
                  (xref-var (if xref-p (concat "*naf-mode-" (cdr xref-p) "-xrefs*")))
                  );; (test-xref-no-bnd (read xref-concat)))
             `(,xref-var ,xref-pkg ,xref-fname)))
    (error "not a naf-mode-file")))

;;;test-me;(mon-build-naf-mode-xref)
;;=> ("*naf-mode-insertion-utils-xrefs*" 
;;    "naf-mode-insertion-utils" 
;;    "naf-mode-insertion-utils.el")

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-26T20:59:14-04:00Z}#{09397} - by MON KEY>
(defun mon-insert-naf-mode-xref-template (&optional insertp intrp)
  "Return a naf-mode variable template for xrefing
variable names in current naf-mode file.\n
See also; `mon-build-naf-mode-xref', `mon-insert-naf-mode-xref-template'."
  (interactive "i\np")
  (let* ((xref-l (mon-build-naf-mode-xref))
         (xref-sym (car xref-l))
         (pkg-nm (cadr xref-l))
         (fnm (caddr xref-l))
         (xref-template
          (concat
           (mon-lisp-stamp)
           "\n(eval-and-compile\n"
           "(defvar " xref-sym "\n"
           "  '(" xref-sym "\n"
           "    mon-help-naf-mode-faces)\n"
           "  \"*List of symbol names of variables which xref each other in the\n"
          "`" pkg-nm"' package.\nSee FILE: \\\"./" fnm "\\\".\"))\n"
           ";;\n;;;test-me; " xref-sym "\n;;\n;;;(progn (makunbound '"xref-sym ")\n"
           ";;;       (unintern '" xref-sym "))")))
    (if (or insertp intrp)
        (save-excursion 
          (newline)
          (princ xref-template (current-buffer)))
      xref-template)))
;;
;;;test-me;(mon-insert-naf-mode-xref-template)
;;;test-me;(mon-insert-naf-mode-xref-template t)

;;; ==============================
;;; TODO: Add subr to search backward for xref-name to see if it already exists.
;;; CREATED: <Timestamp: #{2009-09-26T21:27:46-04:00Z}#{09397} - by MON KEY>
(defun naf-mode-variable-constant-template (naf-symbol-name &optional insertp intrp)
  "Return code building template for variable and constant with NAF-SYMBOL-NAME.
NAF-SYMBOL-NAME - a string - should be suitable for concatenation as:
*naf-<NAF-SYMBOL-NAME>*    ;VARIABLE\nnaf-mode-<NAF-SYMBOL-NAME> ;CONSTANT
Do not include `-', `*', etc. This function does not check the value given.
When INSERTP is non-nil or called-interactively insert templates at point.
Does not move point. 
See also; `mon-build-naf-mode-xref', `mon-insert-naf-mode-xref-template'."
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
           ";;       (makunbound '" c-name ") (unintern '" c-name "))")))
    (if (or insertp intrp)
        (save-excursion 
          (newline) 
          (princ v-c-template (current-buffer)))
      v-c-template)))
;;
;;;test-me;(naf-mode-variable-constant-template "test-template")
;;;test-me;(naf-mode-variable-constant-template "test-template" t)

;;; ==============================
;;; CREATED: <Timestamp: Thursday April 09, 2009 @ 05:52.20 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T14:36:47-0400Z - by MON KEY>
;;; DEPRECATED: USE: `naf-mode-variable-constant-template'
(defun mon-insert-naf-mode-constant-template (&optional constant-name insertp intrp)
  "Insert Elisp template for defining new font-lock constants for `naf-mode'.\n
EXAMPLE:\n(mon-insert-naf-mode-constant-template \"some-constant\")\n
See also; `*naf-mode-faces-as-displayed*', `mon-insert-face-as-displayed',
`mon-insert-naf-mode-face-template'."
  (interactive "sGive the value for * naf-mode-*-flags: \ni\np")
  (let* ((cnst (if constant-name constant-name "!CONSTANT!"))
         (lcl (replace-regexp-in-string  " " "-" (concat "naf-" cnst "-flags")))
	 (con (replace-regexp-in-string " " "-" (concat "naf-mode-" cnst "-flags")))
	 (naf-lcl (replace-regexp-in-string "--" "-" lcl)) ;catch on trailing whitespace 
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
          "Used in `naf-mode' for font-locking with `NAF-MODE-*-FFACE'.\"\)\)\n\n"
          ";;;test-me; " naf-con "\n\n"
          ";;(progn (makunbound \'" naf-con"\)\n"
          ";;  (unintern \'" naf-con "\)\n"
          ";;; ==============================")))
    (when (or insertp intrp) (save-excursion (insert put-temp)))
    put-temp))

;;;test-me;(mon-insert-naf-mode-constant-template "some-constant")
;;;test-me;(mon-insert-naf-mode-constant-template "some-constant" t)
;;;test-me;(mon-insert-naf-mode-constant-template nil)
;;;test-me;(mon-insert-naf-mode-constant-template nil t)
;;;test-me;(call-interactively 'mon-insert-naf-mode-constant-template)

;;; ==============================
(defun mon-insert-naf-mode-face-template (&optional face-name insertp intrp)
  "Insert Elisp template for new face definitions and constants.
Used to make face templates for fontlocking `naf-mode' keywords.\n
EXAMPLE:\n\(mon-insert-naf-mode-face-template \"some-face-name\")
See also; `*naf-mode-faces-as-displayed*',`mon-insert-face-as-displayed' 
`mon-insert-naf-mode-constant-template'."
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
	  "KEYWORDS-IN: the regexp(s) defined in `NAF-MODE-CONSTANT-NAME' constant.\n"
          "FACE-DOCUMENTED-IN: `" the-fface"'.\nSee also;\nUsed in `naf-mode'.\"\n"
          "  	    :group \'naf-mode\n"
          "  	    :group \'naf-mode-faces\)\n;;\n"
          "(defvar " the-fface " '" the-face "\n"
          "    \"*Face for font-locking of {DESCRIBE} in `naf-mode'.\n"
	  "KEYWORDS-IN: the regexp(s) defined in `NAF-MODE-CONSTANT-NAME' constant.\n"
          "FACE-DEFINED-IN: `" the-face "'.\nSee also;\n.\")\n\n"
          ";;;test-me;(describe-face '" the-face ")\n\n"
          ";;(progn (makunbound \'" the-face "\)\n" 
          ";;  (makunbound \'" the-fface"\)\n"
          ";;  (unintern \'" the-face "\)\n"
          ";;  (unintern \'" the-fface "\))\n\n"
          ";;; ==============================")))
    (when (or insertp intrp) (save-excursion (insert put-fc-temp)))
    put-fc-temp))

;;;test-me;(mon-insert-naf-mode-face-template "some-face-name" t)
;;;test-me;(mon-insert-naf-mode-face-template  nil t)
;;;test-me;(call-interactively 'mon-insert-naf-mode-face-template)
;;;test-me;(mon-insert-naf-mode-face-template "some-face-name")

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-01T15:47:05-04:00Z}#{09362} - by MON KEY>
(defun mon-insert-face-as-displayed (&optional insertp intrp)
  "Insert fontlocked keywords to test fruitsaladness `naf-mode' face/constants.
See also; `*naf-mode-faces-as-displayed*',`mon-insert-naf-mode-face-template',
`mon-insert-naf-mode-constant-template'."
 (interactive "i\np")
 (let* ((i-fad *naf-mode-faces-as-displayed*)
	(is-on (mon-is-naf-mode-and-llm-p))
	(llm-off))
   (setq i-fad (mapconcat 'identity i-fad "\n"))
   (if (or insertp intrp)
      (save-excursion
	(when is-on (longlines-mode 0) (setq llm-off 't))
	(insert i-fad))
	(when llm-off (longlines-mode 1) (setq llm-off 'nil)))
     i-fad))
;;
;;;test-me;(mon-insert-face-as-displayed)
;;;test-me;(mon-insert-face-as-displayed t)
;;;test-me;(call-interactively 'mon-insert-face-as-displayed)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-01T17:56:21-04:00Z}#{09362} - by MON KEY>
(eval-and-compile
(defvar *naf-mode-faces-as-displayed* 
  '(";;; =============================="
    "FRBNF14942139 ;naf-mode-field-face-db-entry"
    "Uploaded-by: ;naf-mode-field-face"
    "Artist-naf: ;naf-mode-db-entry-face"
    "Bauhaus ;naf-mode-group-period-style-face"
    "Salon de ; naf-mode-event-face"
    "Royal Academy ;naf-mode-institution-face"
    "français ;naf-mode-nationality-face"
    "Lastname, Firstname | Fname Lname | F. Lname ;naf-mode-name-divider-face" 
    "--- ;naf-mode-delim-face"
    "Washington ;naf-mode-place-face"
    "\(1885-1940\);naf-mode-date-face"
    "affichiste ;naf-mode-secondary-role-face"
    "Auteur ;naf-mode-primary-role-face"
    "illustrations ;naf-mode-art-keywords-role-face"
    "VENTES PUBLIQUES :;naf-mode-benezit-face"
    "Portrait de; naf-mode-alternate-name-face"
    "Berliner Illustrierte ;naf-mode-publication-periodical-face"
    "Design Centre Awards Scheme ;naf-mode-awards-prizes-face"
    "<Timestamp: #{2009-08-13T17:30:19-04:00Z}#{09334} - by MON>  ;naf-mode-timestamp-face"
    "accessed: #{2009-09-01T16:31:39-04:00Z}#{09362} - MON KEY"
    "(URL `http://catalog.loc.gov/')"
    "(URL `http://authorities.loc.gov/')"
    "(URL `http://catalogue.bnf.fr/ark:/12148/cb123349648/PUBLIC')"
    "accessed:"
    "---"
    "---"
    "`naf-mode-field-names' using ;`naf-mode-db-entry-face'"
    "---"
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
    "---"
    "`naf-mode-db-entry' using `naf-mode-db-field-face'"
    "---"
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
    "MEDIUM:"
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
    "TITLE:"
    "Title details:"
    "Title:"
    "Type :"
    "Type of Material:"
    "Uniform Title:"
    "Update:"
    "Used For/See From:"
    "Year:"
    "---"
    "NAF-FIELDS"
    "---"
    "Ads-for:                                          ;NAF-FIELD"
    "Appeared-in:                                      ;NAF-FIELD"
    "Artists-associated:                               ;NAF-FIELD"
    "Auction-records:                                  ;NAF-FIELD"
    "Authors-associated:                               ;NAF-FIELD"
    "Book-notes:                                       ;NAF-FIELD"
    "Brand-name:                                       ;NAF-FIELD"
    "Contents:                                         ;NAF-FIELD"
    "Date-founded:                                     ;NAF-FIELD"
    "Founded-by:                                       ;NAF-FIELD"
    "Full-title:                                       ;NAF-FIELD"
    "Location-published:                               ;NAF-FIELD"
    "Products-associated:                              ;NAF-FIELD"
    "Publisher:                                        ;NAF-FIELD"
    "Slogans:                                          ;NAF-FIELD"
    "Uploaded-by:                                      ;NAF-FIELD"
    "Used-for:                                         ;NAF-FIELD"
    "ebay-item-ended:                                  ;NAF-FIELD"
    "ebay-item-number:                                 ;NAF-FIELD"
    "ebay-item-realized:                               ;NAF-FIELD"
    "ebay-item-seller:                                 ;NAF-FIELD"
    "---"
    "SHARED FIELDS:"
    "---"
    "Born:                                             ;NAF-FIELD, ;ULAN"
    "Died:                                             ;NAF-FIELD, ;ULAN"
    "Roles:                                            ;NAF-FIELD, ;ULAN"
    "---"
    "ULAN-FIELDS:"
    "---"
    "Biographies:                                      ;ULAN"
    "Birth and Death Places:                           ;ULAN"
    "Events:                                           ;ULAN"
    "Gender:                                           ;ULAN"
    "ID:                                               ;ULAN"
    "List/Hierarchical Position:                       ;ULAN"
    "Names:                                            ;ULAN"
    "Nationalities:                                    ;ULAN"
    "Record Type:                                      ;ULAN"
    "Related Names:"
    "Related People and Corporate Bodies:              ;ULAN"
    "Related People or Corporate Bodies:               ;ULAN"
    "Sources and Contributors:                         ;ULAN"
    "Subject:                                          ;ULAN"
    "education:                                        ;ULAN *TRL-WSP"
    "---"
    "LOC-FIELDS:"
    "---"
    "Biographical/Historical Note:                     ;LOC-field"
    "LC Class Number:                                  ;LOC-field"
    "LC Classification:                                ;LOC-field"
    "LC Control No.                                    ;LOC-field"
    "LC Control Number:                                ;LOC-field"
    "LC Copy:                                          ;LOC-field"
    "LCCN Permalink:"
    "---"
    "`naf-mode-field-names-bnf' using `naf-mode-field-bnf-face'"
    "---"
    "<Employé pour :"
    "Appartient au recueil :"
    "Auteur(s) :                                       ;BNF *TRL-WSP"
    "Autre(s) auteur(s) :                              ;BNF"
    "Autre(s) forme(s) du titre :                      ;BNF"
    "Circuit de distribution :                         ;BNF  *TRL-WSP"
    "Classement géographique :                         ;BNF"
    "Comprend :                                        ;BNF  *TRL-WSP"
    "Coordonnées géographiques :                       ;BNF"
    "Cote(s) BnF :                                     ;BNF"
    "Création :                                        ;BNF"
    "Description matérielle :                          ;BNF"
    "Distributeur :                                    ;BNF *TRL-WSP"
    "Domaine(s) :                                      ;BNF *TRL-WSP"
    "Domaine(s) :                                      ;BNF"
    "Domaine(s) d'expression artistique :              ;BNF"
    "Enregistrement :                                  ;BNF"
    "Forme(s) rejetée(s) :                             ;BNF"
    "Genre :                                           ;BNF *TRL-WSP"
    "Indice de l'Histoire de France :                  ;BNF"
    "Indice(s) Dewey :                                 ;BNF"
    "Interprète(s) :                                   ;BNF"
    "Langue(s) :                                       ;BNF"
    "Lien au titre d'ensemble :                        ;BNF"
    "Lien à la collection :                            ;BNF"
    "Marque :                                          ;BNF *TRL-WSP"
    "Mort :                                            ;BNF"
    "Naissance :                                       ;BNF"
    "Nation(s) :                                       ;BNF *TRL-WSP"
    "Nationalité(s) :                                  ;BNF"
    "Note(s) :                                         ;BNF"
    "Notice n° :                                       ;BNF"
    "Numérotation :                                    ;BNF"
    "Participant(s) :                                  ;BNF *TRL-WSP"
    "Producteur(s) :                                   ;BNF"
    "Profession(s) :                                   ;BNF"
    "Projection :                                      ;BNF *TRL-WSP"
    "Publication :                                     ;BNF *TRL-WSP"
    "Périodicité :                                     ;BNF"
    "Responsabilité(s) exercée(s) sur les documents :  ;BNF"
    "Référence(s) commerciale(s) :                     ;BNF"
    "Réunit :                                          ;BNF *TRL-WSP"
    "Sexe :                                            ;BNF"
    "Source(s) :                                       ;BNF *TRL-WSP"
    "Sujet(s) :                                        ;BNF"
    "Sujet(s) géographique(s) :                        ;BNF"
    "Technique(s) privilégiée(s) :                     ;BNF"
    "Thème(s) :                                        ;BNF *TRL-WSP"
    "Titre clé :                                       ;BNF"
    "Titre d'ensemble :                                ;BNF"
    "Titre(s) :                                        ;BNF"
    "Titre(s) en liaison :                             ;BNF"
    "Type de la collectivité officielle :              ;BNF"
    "Type de publication :                             ;BNF"
    "Type de ressource électronique :                  ;BNF"
    "Typologie :                                       ;BNF *TRL-WSP"
    "Échelle(s) :                                      ;BNF"
    "Éditeur :                                         ;BNF *TRL-WSP"
    "Édition :                                         ;BNF *TRL-WSP"
    "---"
    "`naf-mode-db-field-flags-bnf'"
    "---"
    "forme internationale                              ;BNF-FIELD-ENTRY"
    "Mise à jour :                                     ;BNF-FIELD-ENTRY"
    "masculin                                          ;BNF-FIELD-ENTRY"
    "féminin                                           ;BNF-FIELD-ENTRY"
    "---"
    "[500006383] ;naf-mode-db-field-entry-ulan-face    ;ULAN-FIELD-ENTRY"
    "NAFL2001060907 NAFR8914343 NAFR907811             ;ULAN-FIELD-ENTRY"
    "(preferred, index, display, V)                    ;ULAN-FIELD-ENTRY"
    "(inhabited place)                                 ;ULAN-FIELD-ENTRY"
    "(preferred, index, V)                             ;ULAN-FIELD-ENTRY"
    "(preferred, index)                                ;ULAN-FIELD-ENTRY"
    "(preferred)                                       ;ULAN-FIELD-ENTRY"
    "(display, V)                                      ;ULAN-FIELD-ENTRY"
    "(display)                                         ;ULAN-FIELD-ENTRY"
    "(index)                                           ;ULAN-FIELD-ENTRY"
    "(V)                                               ;ULAN-FIELD-ENTRY"
    "---"
    "#{American painter, illustrator, and printmaker, 1879-1941} #{500008013})"
    "(:TEACHER-OF #{Weber, Sarah S. Stilwell}"
    "---"
    ";naf-mode-ulan-ppl-corp-face"
    "---"
    ":APPRENTICE-OF                                    ;ULAN-FIELD-ENTRY"
    ":APPRENTICE-WAS                                   ;ULAN-FIELD-ENTRY"
    ":ASSISTED-BY                                      ;ULAN-FIELD-ENTRY"
    ":ASSOCIATE-OF                                     ;ULAN-FIELD-ENTRY"
    ":CHILD-OF                                         ;ULAN-FIELD-ENTRY"
    ":COLLABORATED-WITH                                ;ULAN-FIELD-ENTRY"
    ":FOUNDER-OF                                       ;ULAN-FIELD-ENTRY"
    ":GRANDCHILD-OF                                    ;ULAN-FIELD-ENTRY"
    ":GRANDPARENT-OF                                   ;ULAN-FIELD-ENTRY"
    ":GRANDPARENT-WAS                                  ;ULAN-FIELD-ENTRY"
    ":INFLUENCE                                        ;ULAN-FIELD-ENTRY"
    ":MEMBER-OF                                        ;ULAN-FIELD-ENTRY"
    ":PARENT-OF                                        ;ULAN-FIELD-ENTRY"
    ":PARTNER-OF                                       ;ULAN-FIELD-ENTRY"
    ":SIBLING-OF                                       ;ULAN-FIELD-ENTRY"
    ":SPOUSE-OF                                        ;ULAN-FIELD-ENTRY"
    ":STUDENT-OF                                       ;ULAN-FIELD-ENTRY"
    ":STUDENT-WAS                                      ;ULAN-FIELD-ENTRY"
    ":TEACHER-OF                                       ;ULAN-FIELD-ENTRY"
    ":TEACHER-WAS                                      ;ULAN-FIELD-ENTRY"
    ":WORKED-WITH                                      ;ULAN-FIELD-ENTRY"
    "---"
    "Currencies:"
    "---"
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
    "---"
    ;;; `*naf-mode-dates-xref*' `naf-mode-french-days' 
    ;;; `naf-weekday-alist', `naf-mode-english-dates' 
    ;;; `naf-month-abbrev-alist', `naf-weekday-alist'
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
    "---"
    "Monday     Lundi"	  
    "Tuesday    Mardi"	  
    "Wednesday  Mercredi"
    "Thursday   Jeudi"	  
    "Friday     Vendredi"
    "Saturday   Samedi"  
    "Sunday     Dimanche"
    "---"
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
  "List of `naf-mode' keywords. 
CALLED-BY: `mon-insert-face-as-displayed'.
List includes the vars that define them or hold regexps that do.
List also identifies the type of field and and faces that light them up.
Use to test naf-mode font-locking.\nSee also: `mon-insert-naf-mode-face-template',
`mon-insert-naf-mode-constant-template'."))
;;
;;;test-me; *naf-mode-faces-as-displayed* 
;;;(progn (makunbound '*naf-mode-faces-as-displayed*) (unintern '*naf-mode-faces-as-displayed*)) 

;;; ==============================
(provide 'naf-mode-insertion-utils)
;;; ==============================

;;; ================================================================
;;; naf-mode-insertion-utils.el ends here
;;; EOF

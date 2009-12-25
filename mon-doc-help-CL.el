;;; this is mon-doc-help-CL.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-doc-help-CL.el
;;; 
;;; FUNCTIONS:►►►
;;; `mon-help-CL:LOCAL-TIME', `mon-help-CL:LOOP', `mon-help-CL:TIME'
;;; `mon-help-slime-keys', `mon-help-swank-functions'
;;; `mon-help-wget-cl-pkgs', `mon--test--help-wget-cl-pkgs',
;;; `mon-help-wget-cl-pkgs-for-shell-command'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*cl-cmu-ai-repo*',`*cl-ext-pkg-map*'
;;; 
;;; ALIASED/ADVISED/SUBST'D:
;;; 
;;; MOVED:
;;; `mon-help-CL-time', `mon-help-CL-loop', `mon-help-slime-keys' -> mon-doc-help-CL.el
;;; `mon-help-CL:TIME', `mon-help-CL:LOOP',
;;;
;;; RENAMED:
;;; `mon-help-CL-loop' -> `mon-help-CL:LOOP'
;;; `mon-help-CL-time' -> `mon-help-CL:TIME' 
;;; 
;;; REQUIRES:
;;; mon-doc-help-utils.el
;;;
;;; TODO:
;;;
;;; SNIPPETS:
;;;(defun mon-insert-doc-help-tail (&optional fname)
;;; (interactive "P")
;;; (if intrp 
;;;     (reference-sheet-help-function-spit-doc 'xxxxxxxxxxx)
;;;   (message "pass non-nil for optional arg INTRP")))
;;;
;;; THIRD PARTY CODE:
;;; Pascal Bourguignon his: `pjb-cl.el' 
;;; WAS: `loop-doc'->`reference-sheet-help-loop'
;;; (URL `http://www.informatimago.com/develop/emacs/index.html')
;;; 
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;; 
;;; PUBLIC-LINK: 
;;; (URL `http://www.emacswiki.org/emacs-en/ReferenceSheetHelpUtils')
;;; FILE-PUBLISHED: <Timestamp: #{2009-08-24}
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-doc-help-CL.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-08-25} - by MON KEY>
;;;
;;; 
;;; FILE-CREATED:
;;; <Timestamp: Thursday July 16, 2009 @ 10:56.13 AM - by MON KEY>
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
;;; :CREATED <Timestamp: #{2009-12-23T17:47:48-05:00Z}#{09523} - by MON KEY>
(defvar *cl-cmu-ai-repo* "http://www.cs.cmu.edu/Groups/AI/lang/lisp/"
  ;; :NOTE Uncomment below if the former isn't available:
  ;; "http://www-cgi.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/"
  "*Partial path for building a URL string to the relevant Lisp portions of the
  Carnegie Mellon Artificial Intelligence Repository:
:SEE (URL `http://www.cs.cmu.edu/Groups/AI/0.html').\n
:SEE-ALSO `*cl-ext-pkg-map*', `mon-help-cl-pkgs', `mon-help-wget-cl-pkgs'.\n►►►")
;;
;;; :TEST-ME *cl-cmu-ai-repo*
;;;(progn (makunbound '*cl-cmu-ai-repo* ) (unintern '*cl-cmu-ai-repo* ) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-23T20:36:35-05:00Z}#{09524} - by MON KEY>
(defvar *cl-ext-pkg-map*
  '((cltl2-html 
     "Guy Steele's Common Lisp the Language Second Edition in HTML." 
     "doc/cltl/cltl_ht.tgz")
    (cltl2tex 
     "CLTL2 TeX macros. Contains Perl script lisp2tex and the example contrl.tex" 
     "util/tex/cltl2tex.tgz")
    (cltl2-src 
     "Guy Steele's Common Lisp the Language Second Edition in HTML."
     "doc/cltl/cltl_src.tgz")
    (dpans3 
     "Common Lisp ANSI Standard X3J13 committe TeX sources Draft 3." 
     "doc/standard/ansi/dpans/dpans3.tgz")
    (dpans3r 
     "Common Lisp X3J13 Draft rev3 changes from :VERSION 15.17 and 15.17R."
     "doc/standard/ansi/dpans/dpans3r.tgz")
    (dpans-clos 
     "CLOS SPEC: Common Lisp Object System Specification TeX sources."
     "doc/standard/ansi/clos/clos.tgz")
    (dpans-amop 
     "CLOS Metaobject Protocol - Common Lisp 'Specification'."
     "doc/standard/ansi/mop/mop_spec.tgz")
    (islisp 
     "ISO: Committee Draft Standard for ISO Lisp :VERSION 11.4 of 1994-08-15."
     "doc/standard/iso/islsp114.pgz") ;; :NOTE weird format but can be extracted.
    (vgrind 
     "vgrind/tgrind entries for Common Lisp - Like vgrind -lisp :)"
     "util/vgrind/vgrind.txt")
    (refcard 
     "TeX source of Franz's Emacs reference card for Allegro :VERSION 4.x"
     "util/emacs/refcard/refcard.tgz")
    (cl-shell 
     "Emacs major mode for running Common Lisp as an Emacs subprocess."
     "util/emacs/cl_shell/cl_shell.tgz")
    (eli 
     "Franz Inc's GNU Emacs Allegro Common Lisp interface :VERSION 2.0.16." 
     "/util/emacs/franz/v2016/eli_2016.tgz")
    (cldoc 
     "An `eldoc' utility for Common Lisp."
     ;; Yuji `bmonkey' Minejima's <ggb01164@nifty.ne.jp>    
     "http://homepage1.nifty.com/bmonkey/emacs/elisp/cldoc.el")
    (cl-lookup 
     ;; http://www.phys.au.dk/~harder/dpans.html    
     "Common Lisp Hyperlink Specification document lookup utility"
     "http://homepage1.nifty.com/bmonkey/emacs/elisp/cl-lookup.tar.gz")
    (dpans2texi 
     "Common Lisp dpANS TeX -> .texi converter using elisp :COURTESY Jesper Harder."
     "http://www.phys.au.dk/~harder/dpans2texi-1.05.tar.gz")
    (cl-gcl-info 
     "Common Lisp ANSI standard in .texi and .info format GNU Common Lisp.\n:COURTESY William F. Schelter 1994."
     "ftp://ftp.ma.utexas.edu/pub/gcl/gcl-info+texi.tgz")
    (cl-hs 
     "Emacs lisp library from GNU Clisp for Hyperlink Specification access."
     "http://clisp.cvs.sourceforge.net/*checkout*/clisp/clisp/emacs/clhs.el")
    (cl-hyperspec-v3
     "Common Lisp Hyperspec v3 the variable length version of 2005-April-12."
     ;; http://www.cs.cmu.edu/Groups/AI/html/hyperspec/clspec.html
     "http://www.cs.cmu.edu/Groups/AI/html/hyperspec/clspec30.tgz")
    (cl-hyperspec-v7
     "Common Lisp Hyperspec v7 the 8.3 version of 2005-April-12."
     "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
    ;;; (cl-hyperspec-v6
    ;;;  "Common Lisp Hyperspec v6 of 2004-August-15."
    ;;;  "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-6-0.tar.gz")
    (cl-cookbook
     "VIEW CVS of the cl-cookbook at sourceforge."
     ;; http://cl-cookbook.sourceforge.net/
     "http://cl-cookbook.cvs.sourceforge.net/viewvc/cl-cookbook/cl-cookbook/")
    (ilisp
     "Ilisp source from git with updates for current Emacsen.\nRepo maintainance courtesy Barak A. Pearlmutter <barak@cs.nuim.ie>"
    ;; "http://sourceforge.net/projects/ilisp/files/ilisp/ilisp-snapshot/ilisp-20021222.tar.gz"
    ;; "http://sourceforge.net/projects/ilisp/files/ilisp/ilisp-snapshot/ilisp-doc-20021222.tar.gz"
    ;; git://git.debian.org/collab-maint/ilisp.git
     "http://git.debian.org/?p=collab-maint/ilisp.git;a=summary")
    (oaklisp 
     "Oaklisp is an OOP Scheme dialect with lexical scoping and first-class classes."
     ;; "http://www.bcl.hamilton.ie/~barak/oaklisp/"
     ;; "http://www.bcl.hamilton.ie/~barak/oaklisp/binaries/dpkg/sources/oaklisp_1.3.1.tar.gz"
     ;; "https://alioth.debian.org/projects/oaklisp/"     
     ;; "https://alioth.debian.org/snapshots.php?group_id=100056"
     "http://www.bcl.hamilton.ie/~barak/oaklisp/release/oaklisp-07-Jan-2000.tar.gz"))
  "*List of Common Lisp documentation packages and legacy/historic packages of
use with Emacs.  Each sublist element of has the format:\n
\(SYMBOL DESCRIPTION URL\)\n
:SEE-ALSO `*cl-cmu-ai-repo*', `mon-help-cl-pkgs', `mon-help-wget-cl-pkgs'.\n►►►")
;;
;;; :TEST-ME *cl-ext-pkg-map*
;;;(progn (makunbound '*cl-ext-pkg-map*) (unintern '*cl-ext-pkg-map*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-23T20:41:48-05:00Z}#{09524} - by MON KEY>
(defun mon-help-cl-pkgs (&optional insrtp intrp)
  (interactive "i\np")
  (let (prtty)
    (setq prtty
          (mapconcat #'(lambda (cl) 
                         (format ":%s\n - %s\n:SEE (URL `%s')\n"
                                 (upcase (format "%s" (car cl))) 
                                 (cadr cl) 
                                 (let* ((pth-p (caddr cl))
                                        (tst-url (substring pth-p 0 3)))
                                   (if (member tst-url '("htt" "ftp"))
                                       pth-p
                                       (concat *cl-cmu-ai-repo* pth-p)))))
                     *cl-ext-pkg-map* "\n"))
    (if (or insrtp intrp)
        (save-excursion 
          (newline)          
          (princ prtty (current-buffer)))
        (prin1 prtty))))
;;
;;; :TEST-ME (mon-help-cl-pkgs t)
;;
(eval-when-compile
  (put 'mon-help-cl-pkgs 'function-documentation 
       (concat     
        "Return a pretty printed list of historic and Common Lisp packages and Specs."
        "\nIncludes dpANSI Spec, ISO Spec, CLTL2, Ilisp, CL-cookbook, eli, cl-shell etc.\n\n"
        (mon-help-cl-pkgs)
        "\n:SEE-ALSO `*cl-cmu-ai-repo*',`*cl-ext-pkg-map*', `mon-help-wget-cl-pkgs'.\n►►►")))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-24T01:10:48-05:00Z}#{09524} - by MON>
(defun mon-help-wget-cl-pkgs (&optional cl-wget-fname)
  "Write a wget script to file CL-WGET-FNAME return contents of CL-WGET-FNAME.
Used with wget to snarf files with URL's returned per `mon-help-cl-pkgs'.\n
When CL-WGET-FNAME is non-nil it should be a full pathname which current user
with `user-login-name' has sufficient permissions to write to and execute
from.\n
When CL-WGET-FNAME is nil use timestamp suffixed filename in default-directory:\n
/PATH-IS/`default-directory'/wget-script-YY-MM-DD\n
Contents of file are formatted as follows:\n
 o When GNU/Linux: 
   - #! /bin/sh                        ;<- shebang on Line1
   - wget --no-directories --no-parent ;<- Line2
   - A backslash appended to all but the last of each line e.g. ' \\'
   - Set CL-WGET-FNAME with user exec permissons with mode 760 \(u+rwx,g+r\).\n 
 o When W32 
   - Do not include shebang line
   - # /PATH/TO/wget.exe --no-directories --no-parent -i CL-WGET-FNAME ;<- Line1
   - Do not append trailing backslashes
   - Do not alter permissions of CL-WGET-FNAME\n 
 o When a wget executable does not exist in path:
  - Do not include shebang line
  - Do append trailing backslashes
  - Do not alter permissions of CL-WGET-FNAME\n
:EXAMPLE\n\(mon--test--help-wget-cl-pkgs\) ;<- helper test function.\n
:NOTE This function does not write the URLs for *cl-ext-pkg-map* keys
cl-cookbook, oaklisp, and ilisp as these point to web pages and are not files.
Download or add these manually if that is what you want. The following loop
returns the absent URL's:\n
\(let \(manual-dl\)
  \(dolist \(cl-wpgs '\(ilisp cl-cookbook oaklisp\) manual-dl\)
    \(setq manual-dl
          \(concat \(caddr \(assoc cl-wpgs *cl-ext-pkg-map*\)\) \"\\n\" manual-dl\)\)\)\)\n
:SEE-ALSO `mon--test--help-wget-cl-pkgs', `mon-help-cl-pkgs', 
`*cl-cmu-ai-repo*', `*cl-ext-pkg-map*'.\n►►►"
  (let ((rmv-wb-pgs (mapcar 'car *cl-ext-pkg-map*))
        (wget-p (executable-find "wget"))
        (cl-wget-fname (if cl-wget-fname cl-wget-fname
                           (concat default-directory 
                                   "wget-script-" 
                                   (format-time-string "%Y-%m-%d"))))
        (sys (cond ((eq system-type 'windows-nt) 'wnz)
                   ((or (eq system-type 'gnu/linux) 
                        (eq system-type 'linux)) 'gnu))))
    (mapc #'(lambda (wb)
              (setq rmv-wb-pgs (delq wb rmv-wb-pgs)))
          '(ilisp cl-cookbook oaklisp))
    (setq rmv-wb-pgs
          (mapconcat #'(lambda (clurl)
                         (let* ((this-cl (assoc clurl *cl-ext-pkg-map*))
                                (this-pth-p (caddr this-cl))
                                (tst-url (substring this-pth-p 0 3)))
                           (if (member tst-url '("htt" "ftp"))
                               this-pth-p
                               (concat *cl-cmu-ai-repo* this-pth-p))))
                     rmv-wb-pgs "\n"))
    (with-temp-file cl-wget-fname
      (insert rmv-wb-pgs)
      (goto-char (buffer-end 0))
      (when wget-p 
        (cond ((eq sys 'wnz)
               (insert (concat "# " wget-p  " --no-parent " "--no-directories "  
                               "-i " (file-name-nondirectory cl-wget-fname) "\n")))
              ((eq sys 'gnu)
               (insert "#! /bin/sh\nwget --no-parent --no-directories \x5c\n")
               (while (search-forward-regexp "[a-z]$" nil t) (insert " \x5c"))
               (goto-char (buffer-end 1))
               (skip-chars-backward "^ \\")
               (delete-char 1))))
      (setq rmv-wb-pgs 
            (buffer-substring-no-properties (buffer-end 0)(buffer-end 1))))
    (when (and wget-p) (set-file-modes cl-wget-fname 480))
    rmv-wb-pgs))
;;
;;; :TEST-ME (mon-help-wget-cl-pkgs (concat default-directory "test-wget-cl-pkgs"))
;;; :TEST-ME (mon--test--help-wget-cl-pkgs)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-24T14:27:52-05:00Z}#{09524} - by MON KEY>
(defun mon--test--help-wget-cl-pkgs ()
  "Helper function to verify `mon-help-wget-cl-pkgs' is functioning as expected.\n
Performs the following checks:\n
o Writes a temp file with output from mon-help-wget-cl-pkgs
o Return inserted contents of temp file in a temporary buffer
o Display that buffer with `file-attributes' in header
o Kills temp-buffer and file on exit\n
:EXAMPLE\n\nmon--test--help-wget-cl-pkgs\n
:NOTE On exit this function should cleanup the temp file/buffer objects below:\n
 o A temp file written to:
   /PATH/TO/`default-directory'/tmp-wget-YY-MM-DD\n
 o A temp-buffer with the name *SHOW-WGET-TEMP*.\n
:SEE-ALSO `mon-help-cl-pkgs',`*cl-cmu-ai-repo*',`*cl-ext-pkg-map*'.\n►►►"
  (save-excursion
    (let ((tmp-wget-cl-pkgs (concat default-directory 
                                    "tmp-wget-"
                                    (format-time-string "%Y-%M-%d")))
          (show-wget-cl-pkgs)
          (tmp-wget-spec))
      (if (file-exists-p tmp-wget-cl-pkgs)
          (error "The wget-cl-pkgs test file is already written")
          (progn
            (mon-help-wget-cl-pkgs tmp-wget-cl-pkgs)
            (setq show-wget-cl-pkgs
                  (with-temp-buffer
                    (insert-file-contents tmp-wget-cl-pkgs)
                    (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
            (setq show-wget-cl-pkgs
                  (multiple-value-bind
                        (d l UID GID ACCESSED MODIFIED s SIZE MODE gmod inod dev)
                      (file-attributes tmp-wget-cl-pkgs) ;; (buffer-file-name))
                    (format (concat "## :FILE #P %s\n## :UID %s\n## :GID %s\n"
                                    "## :ACCESSED %s\n## :MODIFIED %s\n"
                                    "## :SIZE %s\n## :MODE %s\n"
                                    "## :CONTENTS-OF-TEMP-FILE-BELOW\n"
                                    "### ==============================\n%s")
                            tmp-wget-cl-pkgs 
                            UID GID 
                            (format-time-string "%Y-%m-%d %H:%M:%S" ACCESSED)
                            (format-time-string "%Y-%m-%d %H:%M:%S" MODIFIED)
                            SIZE MODE
                            show-wget-cl-pkgs)))
            (delete-file tmp-wget-cl-pkgs)
            (with-output-to-temp-buffer 
                (buffer-name (get-buffer-create "*SHOW-WGET-TEMP*"))
              (princ show-wget-cl-pkgs))
            (sit-for 10)))
      (when (get-buffer "*SHOW-WGET-TEMP*")
        (kill-buffer "*SHOW-WGET-TEMP*")))))
;;
;;; :TEST-ME (mon--test--help-wget-cl-pkgs)j

;;; ==============================
;;; :TODO If 'no-exec is t dl the files in `wget-fname' some other way.
;;;
;;; :NOTE In the consing conidtional below there are three options here: 
;;; - spit out the script name and call shell on that.
;;;   (delete-region (buffer-end 0) (1+ (line-end-position 2)))
;;;    wget-fname)
;;; - spit out the existing command in file and rebuild list with cdr
;;;   (delete-region (buffer-end 0) (line-end-position 2))
;;;   (delete-and-extract-region (buffer-end 0) (1+ (line-end-position 1)))
;;; - spit w/ the `-i wget-fname' as per w32...
;;;   (delete-region (buffer-end 0) (1+ (line-end-position 2)))
;;;  (concat "wget --no-parent --no-directories -i " wget-fname ))
;;;
;;; :CREATED <Timestamp: #{2009-12-24T18:03:48-05:00Z}#{09524} - by MON>
(defun mon-help-wget-cl-pkgs-for-shell-command (wget-fname)
  "Return a string\(s\) for passing to `shell-command' for wget'ing CL docs.\n
:SEE-ALSO `mon-help-wget-cl-pkgs', `*cl-cmu-ai-repo*',`*cl-ext-pkg-map*'
`mon--test--help-wget-cl-pkgs'\n►►►"
  (let ((fnm-tst-wgt (file-name-nondirectory wget-fname))
        (sys (cond ((eq system-type 'windows-nt) 'wnz)
                   ((or (eq system-type 'gnu/linux)
                        (eq system-type 'linux)) 'gnu)
                   ((not (executable-find "wget")) 'no-exec)))
        (read-wget-string ))
    (unless (directory-files default-directory nil (concat fnm-tst-wgt "$"))
      (error "File does not exist or function invoked outside file's directory"))
    (with-temp-buffer
      (save-excursion (insert-file-contents wget-fname))
      (when (eq sys 'wnz) (delete-char (- (skip-chars-forward "# "))))
      (setq read-wget-string 
            `(,(cond ((eq sys 'no-exec) ("### NO wget executable in path"))
                     ((eq sys 'gnu)
                        (delete-region (buffer-end 0) (1+ (line-end-position 1)))
                        (delete-and-extract-region (buffer-end 0) (1+ (line-end-position 1))))
                     ((eq sys 'wnz)
                      (delete-and-extract-region (buffer-end 0) (line-end-position))))
               . ,(cond ((eq sys 'gnu) 
                         (replace-regexp-in-string 
                          " \\\n" "\n"
                          (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
                        ((eq sys 'wnz)
                         (subst-char-in-string 10 32
                                               (buffer-substring-no-properties 
                                                (buffer-end 0) (buffer-end 1)) t))))))
    read-wget-string))
;;;
;;; :TEST-ME 
;;; (let ((system-type 'gnu/linux)) 
;;;    ;;((system-type 'windows-nt))
;;;  (mon-help-wget-cl-pkgs-for-shell-command "wget-script-2009-12-24"))

;;; ==============================
;;; COURTESY: Pascal Bourguignon HIS: `pjb-cl.el' WAS: `loop-doc'
;;; ADDED: <Timestamp: Tuesday June 23, 2009 @ 03:22.54 PM - by MON KEY>
;;; MODIFICATIONS: REPLACED: empty lines with '\n' escaped lisp forms in docstring
(defun mon-help-CL:LOOP (&optional insertp intrp)
  "The Common Lisp `loop' macro.
See info node `(cl)Loop Facility'\n
\(loop CLAUSE...):
Valid clauses are:\n
    for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM
    for VAR in LIST by FUNC
    for VAR on LIST by FUNC
    for VAR = INIT then EXPR
    for VAR across ARRAY
    with VAR = INIT\n
 Miscellaneous Clauses:\n
    named NAME
    initially EXPRS...\n
 Accumulation Clauses:\n
    collect EXPR into VAR
    append EXPR into VAR
    nconc EXPR into VAR
    sum EXPR into VAR
    count EXPR into VAR
    maximize EXPR into VAR
    minimize EXPR into VAR\n
 Termination Test Clauses:\n
    repeat NUM
    while COND
    until COND
    always COND
    never COND
    thereis COND\n
 Unconditional Execution Clause:\n
    do EXPRS...\n
 Conditional Execution Clauses:\n
    if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]\n
 Miscellaneous Clauses:\n
    finally EXPRS...
    return EXPR
    finally return EXPR ►►►\n
;;; \(loop for i in '\(1 2 3 4\)
;;;       collect  i into col
;;;       append   i into app
;;;       nconc    i into nco
;;;       sum      i into sum
;;;       count    i into cnt
;;;       maximize i into max
;;;       minimize i into min
;;;       do \(printf \\\"%d \\\" i\)
;;;       return \(progn \(printf \\\"\\n\\\" i\)
;;;                     \(values col app nco sum cnt max min\)\)\)"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-CL:LOOP :insertp t)
  (message "pass non-nil for optional arg INTRP")))

;;;test-me;(describe-function 'mon-help-CL:LOOP)

;;; ==============================
(defun mon-help-CL:DO (&optional insertp intrp)
"The common lisp do loop.\n
CLTL2-SYNTAX:
\(do \(\({var | \(var [init [step]]\)}*\)\)
    \(end-test {result}*\)
 body-form\)\n
EMACS-DOCS-SYNTAX:
\(do \(\(var init [step]\)...\) \(end-test [result...]\) body...\)\n
DO - {VARIABLE*} 
DO's variable bindings are like LET's with a twist. 
With LET you set up vars e.g.\n
\(let \(\(i 0\) \(j 1\) \(k 2\)\) {...BODY-O-LET...} \)\n
With DO you set initial values of VAR\(s\) _and_ dynamic 'step-values' e.g.\n
\(do \(\(i 0 \(+ i 1\)\) \(j 1 \(+ j 5\)\)\) (end-test {result}) BODY-O-DO)\n
As with LET, DO's return values accumulate within the body except, DO 
includes built-in support for iterative testing and returning within the 
DO form itself. Basically what you would otherwise do inside the body of 
LET get's DOne in the test-form result-form section \(which is why the form
looks so much like the body of a LET and why it happens in a seperate list.)\n
DO - {TEST-FORM RESULT-FORM*}
DO has a step test with each pass. This is like Elisp's `while' but the test 
is for NIL rather than T. i.e. a while form which loops if TEST-FORM yields NIL.
IOW, \"While it don't test true - keep on DOing what needs to get DOne!`\".
The RESULT-FORM\(s\) are the DOings to be DOne.
RESULT-FORM\(s\) are given after the TEST-FORM.\n
DO - {STATEMENT*}
This is the return phase. With LET you would do TEST-FORM RESULT-FORM
mojo here as well - instead DO allows for multiple statements to occur here 
this is cool because we can reflect back into the DO bindings without needing to 
bind additional values just to pass around our results. This is also why DO
often doesn't have a statement body - all the work is already finished.\n
EXAMPLE:\n\(let \(k\)
  \(do \(\(i 0 \(+ i 1\)\)
       \(j 0 \(+ 80 j\)\)\)
      \(\(> i 7\) j\)
    \(setq k \(cons j k\)\)\)
  \(nreverse k\)\)
;=>\(0 80 160 240 320 400 480 560\)\n\n
EXAMPLE-DISASSEMBLED:
\(let (k)
  (do (                  ;; begin DO's var bidings.
       (i 0 (+ i 1))     ;; start var I at 0 - with each pass step I by 1.
       (j 0 (+ 80 j))    ;; start var J at 1 - with each pass add 80 to J's previous value.
       )                 ;; end DO's variable bindings.
      (                  ;; begin DO's {test-form restult-form}.
       (> i 7)           ;; We're DOing something if TEST-FORM yields NIL.
       j                 ;; RESULT-FORM gets DOne as long as stepper is not true. In this case J gets DOne.
       )                 ;; DOing is DOne. 
    (setq k (cons j k))) ;; STATEMENT - cons the results of DOing - this is side-effect oriented.
\(nreverse k))\n\n►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-CL:DO :insertp t)
  (message "pass non-nil for optional arg INTRP")))
;;
;;;test-me;(mon-help-CL:DO)
;;;test-me;(mon-help-CL:DO t)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday July 15, 2009 @ 12:50.16 PM - by MON KEY>
(defun mon-help-CL:TIME (&optional insertp intrp)
"CL: `GET-DECODED-TIME' 
Return nine values specifying the current time as follows:
second, minute, hour, date, month, year, day of week \(0 = Monday\), T
\(daylight savings times\) or NIL \(standard time\), and timezone.►►►
\(get-decoded-time\) =>
14     ;second\n44     ;minute\n12     ;hour\n15     ;date\n7      ;month
2009   ;year\n2      ;day\nT      ;dayligt-p\n5      ;zone"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-CL:TIME :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;;
;;;test-me;(describe-function 'mon-help-CL:TIME)

;;; ==============================
;; (while (search-forward-regexp 
;;        ;;....1..2.........3.........
;;        "^\\(\\(.*\t\\)\\(.*\\)\\)$")
;;  (replace-match "\\2`\\3'"))
;;; ==============================
;;; CRATED: <Timestamp: Wednesday July 08, 2009 @ 06:11.12 PM - by MON KEY>
(defun mon-help-slime-keys (&optional insertp intrp)
  "See also; `slime-cheat-sheet' ►►►
SLIME REPL mode keys: 
key              binding
---              -------
C-a		`slime-repl-bol''
C-c		 Prefix Command
TAB		`slime-indent-and-complete-symbol'
C-j		`slime-repl-newline-and-indent'
RET		`slime-repl-return'
C-x		 Prefix Command
ESC		 Prefix Command
SPC		`slime-space'
,		`slime-handle-repl-shortcut'
DEL		`backward-delete-char-untabify'
<C-down>	`slime-repl-forward-input'
<C-return>	`slime-repl-closing-return'
<C-up>		`slime-repl-backward-input'
<home>		`slime-repl-bol'
<return>	`slime-repl-return'
C-c C-b .. C-c C-c  `slime-interrupt'
C-c C-d		`slime-doc-map'
C-c C-e		`slime-interactive-eval'
C-c TAB		`slime-complete-symbol'
C-c C-l		`slime-load-file'
C-c RET		`slime-macroexpand-1'
C-c C-n		`slime-repl-next-prompt'
C-c C-o		`slime-repl-clear-output'
C-c C-p		`slime-repl-previous-prompt'
C-c C-r		`slime-eval-region'
C-c C-t		`slime-toggle-trace-fdefinition'
C-c C-u		`slime-repl-kill-input'
C-c C-w		`slime-who-map'
C-c C-x		 Prefix Command
C-c C-z		`slime-nop'
C-c ESC		 Prefix Command
C-c :		`slime-interactive-eval'
C-c <		`slime-list-callers'
C-c >		`slime-list-callees'
C-c E		`slime-edit-value'
C-c I		`slime-inspect'
M-TAB		`slime-complete-symbol'
M-RET		`slime-repl-closing-return'
C-M-q		`indent-sexp'
C-M-x		`slime-eval-defun'
M-*		`slime-edit-definition'
M-,		`slime-pop-find-definition-stack'
M-.		`slime-edit-definition'
M-n		`slime-repl-next-input'
M-p		`slime-repl-previous-input'
M-r		`slime-repl-previous-matching-input'
M-s		`slime-repl-next-matching-input'
C-M-.		`slime-next-location'
C-c C-d		`slime-doc-map'
C-c C-w		`slime-who-map'
C-c C-x		 Prefix Command
C-c C-z		`slime-switch-to-output-buffer'
C-c ESC		 Prefix Command
C-x C-e		`slime-eval-last-expression'
C-x 4		 Prefix Command
C-x 5		 Prefix Command
C-c C-z		`run-lisp'
C-M-x		`lisp-eval-defun'
C-c M-d		`slime-disassemble-symbol'
C-c M-m		`slime-macroexpand-all'
C-c M-o		`slime-repl-clear-buffer'
C-c M-p		`slime-repl-set-package'
C-c C-w C-a	`slime-who-specializes'
C-c C-w C-b	`slime-who-binds'
C-c C-w C-c	`slime-who-calls'
C-c C-w RET	`slime-who-macroexpands'
C-c C-w C-r	`slime-who-references'
C-c C-w C-s	`slime-who-sets'
C-c C-w C-w	`slime-calls-who'
C-c C-w a	`slime-who-specializes'
C-c C-w b	`slime-who-binds'
C-c C-w c	`slime-who-calls'
C-c C-w m	`slime-who-macroexpands'
C-c C-w r	`slime-who-references'
C-c C-w s	`slime-who-sets'
C-c C-w w	`slime-calls-who'
C-c C-d C-a	`slime-apropos'
C-c C-d C-d	`slime-describe-symbol'
C-c C-d C-f	`slime-describe-function'
C-c C-d C-p	`slime-apropos-package'
C-c C-d C-z	`slime-apropos-all'
C-c C-d #	`common-lisp-hyperspec-lookup-reader-macro'
C-c C-d a	`slime-apropos'
C-c C-d d	`slime-describe-symbol'
C-c C-d f	`slime-describe-function'
C-c C-d h	`slime-hyperspec-lookup'
C-c C-d p	`slime-apropos-package'
C-c C-d z	`slime-apropos-all'
C-c C-d ~	`common-lisp-hyperspec-format'
C-c C-d C-#	`common-lisp-hyperspec-lookup-reader-macro'
C-c C-d C-~	`common-lisp-hyperspec-format'
C-c C-x c	`slime-list-connections'
C-c C-x t	`slime-list-threads'
C-x 5 .		`slime-edit-definition-other-frame'
C-x 4 .		`slime-edit-definition-other-window'"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-slime-keys :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;;
;;;test-me;(mon-help-slime-keys)
;;;test-me;(mon-help-slime-keys t)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-09-06T05:29:03-04:00Z}#{09367} - by MON KEY>
(defun mon-help-swank-functions (&optional insertp intrp)
 ";;CL-LISP-SWANK-SIDE
`*connections*'
`*emacs-connection*'
`*swank-state-stack*'
`*swank-wire-protocol-version*'
`*readtable-alist*'
`*swank-pprint-bindings*'
`*echo-area-prefix*'
`*find-module*'
`*coding-system*'
`*listener-sockets*'
`*communication-style*'
`*pending-continuations*'
`swank:load-file'
`swank:swank-require'
`swank:eval-and-grab-output'
`swank:interactive-eval'
`swank:slime-default-connection'
`swank:slime-current-connection'
`swank:describe-symbol-for-emacs'
\(slime-compute-connection-state 'slime-current-connection\)
\(swank:connection-info\)
\(swank:list-all-package-names\)\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
    	(mon-help-function-spit-doc 'mon-help-swank-functions :insertp t)
        (message "pass non-nil for optional arg INTRP")))
;;
;;;test-me;(mon-help-swank-functions)
;;;test-me;(mon-help-swank-functions t)

;;; ==============================
(defun mon-help-CL:LOCAL-TIME (&optional insertp intrp)
"LOCAL-TIME:*DEFAULT-TIMEZONE*
 Variable: \(not documented\)
LOCAL-TIME:+ASCTIME-FORMAT+
 Variable: \(not documented\)
LOCAL-TIME:+DAY-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+DAYS-PER-WEEK+
 Variable: \(not documented\)
LOCAL-TIME:+GMT-ZONE+
 Variable: \(not documented\)
LOCAL-TIME:+HOURS-PER-DAY+
 Variable: \(not documented\)
LOCAL-TIME:+ISO-8601-FORMAT+
 Variable: \(not documented\)
LOCAL-TIME:+MINUTES-PER-DAY+
 Variable: \(not documented\)
LOCAL-TIME:+MINUTES-PER-HOUR+
 Variable: \(not documented\)
LOCAL-TIME:+MONTH-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+RFC-1123-FORMAT+
 Variable: Please note that you should use the +GMT-ZONE+ timezone to format a
 proper RFC 1123 timestring. See the RFC for the details about the possible
 values of the timezone field.
LOCAL-TIME:+RFC3339-FORMAT+
 Variable: \(not documented\)
LOCAL-TIME:+RFC3339-FORMAT/DATE-ONLY+
 Variable: \(not documented\)
LOCAL-TIME:+SECONDS-PER-DAY+
 Variable: \(not documented\)
LOCAL-TIME:+SECONDS-PER-HOUR+
 Variable: \(not documented\)
LOCAL-TIME:+SECONDS-PER-MINUTE+
 Variable: \(not documented\)
LOCAL-TIME:+SHORT-DAY-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+SHORT-MONTH-NAMES+
 Variable: \(not documented\)
LOCAL-TIME:+UTC-ZONE+
 Variable: \(not documented\)
LOCAL-TIME:ADJUST-TIMESTAMP
 Macro: \(not documented\)
LOCAL-TIME:ADJUST-TIMESTAMP!
 Macro: \(not documented\)
LOCAL-TIME:ASTRONOMICAL-JULIAN-DATE
 Function: Returns the astronomical julian date referred to by the timestamp.
LOCAL-TIME:DAY-OF
 Generic Function: \(not documented\)
LOCAL-TIME:DAYS-IN-MONTH
 Function: Returns the number of days in the given month of the specified year.
LOCAL-TIME:DECODE-TIMESTAMP
 Function: Returns the decoded time as multiple values: nsec, ss, mm, hh, day,
 month, year, day-of-week
LOCAL-TIME:DEFINE-TIMEZONE
 Macro: Define zone-name \(a symbol or a string\) as a new timezone, lazy-loaded
 from zone-file \(a pathname designator relative to the zoneinfo directory on
 this system.  If load is true, load immediately.
LOCAL-TIME:ENABLE-READ-MACROS
 Function: Enables the local-time reader macros for literal timestamps and
 universal time.
LOCAL-TIME:ENCODE-TIMESTAMP
 Function: Return a new TIMESTAMP instance corresponding to the specified time
 elements.
LOCAL-TIME:FIND-TIMEZONE-BY-LOCATION-NAME
 Function: \(not documented\)
LOCAL-TIME:FORMAT-HTTP-TIMESTRING
 Function: \(not documented\)
LOCAL-TIME:FORMAT-RFC3339-TIMESTRING
 Function: Formats a timestring in the RFC 3339 format, a restricted form of
 the ISO-8601 timestring specification for Internet timestamps.
LOCAL-TIME:FORMAT-TIMESTRING
 Function: Constructs a string representation of TIMESTAMP according to FORMAT
 and returns it.  If destination is T, the string is written to
 *standard-output*.  If destination is a stream, the string is written to the
 stream.
LOCAL-TIME:MAKE-TIMESTAMP
 Macro: \(not documented\)
LOCAL-TIME:MODIFIED-JULIAN-DATE
 Function: Returns the modified julian date referred to by the timestamp.
LOCAL-TIME:NOW
 Function: Returns a timestamp representing the present moment.
LOCAL-TIME:NSEC-OF
 Generic Function: \(not documented\)
LOCAL-TIME:PARSE-RFC3339-TIMESTRING
 Function: \(not documented\)
LOCAL-TIME:PARSE-TIMESTRING
 Function: Parse a timestring and return the corresponding TIMESTAMP.
See split-timestring for details. Unspecified fields in the timestring
are initialized to their lowest possible value, and timezone offset is
0 \(UTC\) unless explicitly specified in the input string.
LOCAL-TIME:SEC-OF
 Generic Function: \(not documented\)
LOCAL-TIME:TIMESTAMP
 Type: \(not documented\)
LOCAL-TIME:TIMESTAMP+
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-CENTURY
 Function: Returns the ordinal century upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-DAY
 Function: Returns the day of the month upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-DAY-OF-WEEK
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-DECADE
 Function: Returns the cardinal decade upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-DIFFERENCE
 Function: Returns the difference between TIME-A and TIME-B in seconds
LOCAL-TIME:TIMESTAMP-HOUR
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MAXIMIZE-PART
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MAXIMUM
 Function: Returns the latest timestamp
LOCAL-TIME:TIMESTAMP-MICROSECOND
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MILLENNIUM
 Function: Returns the ordinal millennium upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-MILLISECOND
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MINIMIZE-PART
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MINIMUM
 Function: Returns the earliest timestamp
LOCAL-TIME:TIMESTAMP-MINUTE
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-MONTH
 Function: Returns the month upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP-SECOND
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP-SUBTIMEZONE
 Function: Return as multiple values the time zone as the number of seconds
east of UTC, a boolean daylight-saving-p, and the customary abbreviation of the
timezone.
LOCAL-TIME:TIMESTAMP-TO-UNIVERSAL
 Function: Return the UNIVERSAL-TIME corresponding to the TIMESTAMP
LOCAL-TIME:TIMESTAMP-TO-UNIX
 Function: Return the Unix time corresponding to the TIMESTAMP
LOCAL-TIME:TIMESTAMP-WHOLE-YEAR-DIFFERENCE
 Function: Returns the number of whole years elapsed between time-a and time-b
 \(hint: anniversaries\).
LOCAL-TIME:TIMESTAMP-YEAR
 Function: Returns the cardinal year upon which the timestamp falls.
LOCAL-TIME:TIMESTAMP/=
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP<
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP<=
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP=
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP>
 Function: \(not documented\)
LOCAL-TIME:TIMESTAMP>=
 Function: \(not documented\)
LOCAL-TIME:TO-HTTP-TIMESTRING
 Function: \(not documented\)
LOCAL-TIME:TODAY
 Function: Returns a timestamp representing the present day.
LOCAL-TIME:UNIVERSAL-TO-TIMESTAMP
 Function: Returns a timestamp corresponding to the given universal time.
LOCAL-TIME:UNIX-TO-TIMESTAMP
 Function: Return a TIMESTAMP corresponding to UNIX,
which is the number of seconds since the unix epoch, 1970-01-01T00:00:00Z.
LOCAL-TIME:WITH-DECODED-TIMESTAMP
 Macro: This macro binds variables to the decoded elements of TIMESTAMP.
The TIMEZONE argument is used for decoding the timestamp, and is not bound by
the macro.  The value of DAY-OF-WEEK starts from 0 which means Sunday."
(interactive "i\nP")
(if (or insertp intrp)
   (mon-help-function-spit-doc 'mon-help-CL-loop :insertp t)
 (message "pass non-nil for optional arg INTRP")))

;;; ==============================
(provide 'mon-doc-help-CL)
;;; ==============================

;;; ================================================================
;;; mon-doc-help-CL.el ends here
;;; EOF

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
;;; `mon-hspec-plain-p', `mon-hspec-bld-p'
;;; `mon-hspec-it-p', `mon-hspec-header-line-p'
;;; `mon-hspec-href-p', `mon-w3m-spec-p'
;;; `mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-out'
;;; `mon-hspec-parse-w3m', `mon-hspec-unparse-w3m'
;;; `mon-hspec-unparse-w3m-to-buffer'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*cl-cmu-ai-repo*',`*cl-ext-pkg-map*'
;;; `*mon-hs-root-dir*', `*mon-hs-parse-buffer*', `*mon-hs-unprs-buffer*'
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
;;; :FILE mon-doc-help-utils.el
;;; :SEE (URL `http://www.emacswiki.org/emacs-en/ReferenceSheetHelpUtils')
;;;
;;; OPTIONAL:
;;; :FILE hyperspec.el in current distributions of Slime.
;;; :SEE (URL `http://common-lisp.net/project/slime/snapshots/slime-current.tgz')
;;; :SEE (URL `http://www.cliki.net/SLIME')
;;;
;;; emacs-w3m
;;; :SEE (URL `http://emacs-w3m.namazu.org/')
;;; :SEE (URL `http://cvs.namazu.org/emacs-w3m.tar.gz')
;;;
;;; w3m
;;; :SEE (URL `http://w3m.sourceforge.net/index.en.html')
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
;;; :COURTESY Pascal Bourguignon :HIS `pjb-cl.el' 
;;; :WAS `loop-doc'->`reference-sheet-help-loop'
;;; :SEE (URL `http://www.informatimago.com/develop/emacs/index.html')
;;; 
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;; 
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
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
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
;;(eval-and-compile
(defvar *cl-ext-pkg-map*
  `((cltl2-html 
     "Guy Steele's Common Lisp the Language Second Edition in HTML.\n" 
     "doc/cltl/cltl_ht.tgz")
    (cltl2tex 
     "CLTL2 TeX macros. Contains Perl script lisp2tex and the example contrl.tex\n" 
     "util/tex/cltl2tex.tgz")
    (cltl2-src 
     "Guy Steele's Common Lisp the Language Second Edition in HTML.\n"
     "doc/cltl/cltl_src.tgz")
    (X3J13-archive
     ,(concat
       "Working files for the X3J13 ANSI Common Lisp committee - cannonical archive.\n"
       "   :SEE-ALSO (URL `ftp://ftp.parc.xerox.com/pub/cl/cleanup/issue-status')\n")
     "ftp://ftp.parc.xerox.com/pub/cl/")
    (cl-ansi-naggum
     ,(concat 
      "ANSI X3.226-1994 final draft PostScript conversion landscape orientation 2up.\n"
      "   :COURTESY Erik Naggum\n")
     ;; http://naggum.no/ANSI-CL/
     "http://naggum.no/ANSI-CL.tar.gz")
    (dpans3 
     "Common Lisp ANSI Standard X3J13 committe TeX sources Draft 3.\n" 
     "doc/standard/ansi/dpans/dpans3.tgz")
    (dpans3r 
     "Common Lisp X3J13 Draft rev3 changes from :VERSION 15.17 and 15.17R.\n"
     "doc/standard/ansi/dpans/dpans3r.tgz")
    (dpans-clos 
     "CLOS SPEC: Common Lisp Object System Specification TeX sources.\n"
     "doc/standard/ansi/clos/clos.tgz")
    (dpans-amop 
     "CLOS Metaobject Protocol - Common Lisp 'Specification'.\n"
     "doc/standard/ansi/mop/mop_spec.tgz")
    (islisp 
     ,(concat
       "ISO Committee Draft Standard for ISO Lisp :VERSION 11.4 of 1994-08-15.\n"
       "   :NOTE if the .pgz format but can't be extracted rename to .gz and try again.\n")
     "doc/standard/iso/islsp114.pgz") 
    (vgrind 
     "vgrind/tgrind entries for Common Lisp - Like vgrind -lisp :)\n"
     "util/vgrind/vgrind.txt")
    (refcard 
     "TeX source of Franz's Emacs reference card for Allegro :VERSION 4.x\n"
     "util/emacs/refcard/refcard.tgz")
    (cl-shell 
     "Emacs major mode for running Common Lisp as an Emacs subprocess.\n"
     "util/emacs/cl_shell/cl_shell.tgz")
    (eli 
     "Franz Inc's GNU Emacs Allegro Common Lisp interface :VERSION 2.0.16.\n"
     "/util/emacs/franz/v2016/eli_2016.tgz")
    (cldoc 
     ,(concat
       "An `eldoc' utility for Common Lisp.\n"
       "   :COURTESY Yuji `bmonkey' Minejima's <ggb01164@nifty.ne.jp>\n")
     "http://homepage1.nifty.com/bmonkey/emacs/elisp/cldoc.el")
    (cl-lookup 
     "Common Lisp Hyperlink Specification document lookup utility.\n"
     "http://homepage1.nifty.com/bmonkey/emacs/elisp/cl-lookup.tar.gz")
    (dpans2texi 
     ,(concat
      "Common Lisp dpANS TeX -> .texi converter using elisp.\n"
      "   :COURTESY Jesper Harder.\n"
      "   :SEE-ALSO (URL `http://www.phys.au.dk/~harder/dpans.html')\n")
     "http://www.phys.au.dk/~harder/dpans2texi-1.05.tar.gz")
    (cl-gcl-info 
     ,(concat "Common Lisp ANSI standard in .texi and .info format GNU Common Lisp.\n"
              "   :COURTESY William F. Schelter 1994.\n")
     "ftp://ftp.ma.utexas.edu/pub/gcl/gcl-info+texi.tgz")
    (cltl1-history
     ,(concat 
       "Traces early CLTL1 history/development from circa 1981-03-12.\n"
       "   Early emails from \(among others\): Weinreb, Steele, Fahlman, Gabriel, Hedrick,\n"
       "   JonL White, Feigenbaum, Masinter, Moon, Cannon, etc. Includes reference to\n" 
       "   Engelmore invitation for ARPA member attendance at March 1981 SRI conference.\n"
       "   :SEE-ALSO comp.lang.lisp :DATE 1994-10-20T16:53:41-05:00Z\n"
       "             :SUBJECT CL History (was Re: Why do people like C?)\n")
     "http://www.cs.cmu.edu/Groups/AI/lang/lisp/doc/history/cl.txt")
    (cl-hs 
     "Emacs lisp library from GNU Clisp for Hyperlink Specification access.\n"
     "http://clisp.cvs.sourceforge.net/*checkout*/clisp/clisp/emacs/clhs.el")
    (cl-hyperspec-v3
     ,(concat     
      "Common Lisp Hyperspec v3 the variable length version circa 1996.\n"
      "   :SEE-ALSO (URL `http://www.cs.cmu.edu/Groups/AI/html/hyperspec/clspec.html')\n")
     "http://www.cs.cmu.edu/Groups/AI/html/hyperspec/clspec30.tgz")
    (cl-hyperspec-v7
     "Common Lisp Hyperspec v7 the 8.3 version of 2005-April-12.\n"
     "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
    ;;; (cl-hyperspec-v6
    ;;;  "Common Lisp Hyperspec v6 of 2004-August-15."
    ;;;  "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-6-0.tar.gz")
    (cl-hyperspec-lookup 
     ,(concat
       "Common Lisp interface mapping symbols to URLs of Hspec and CLOS MOP.\n"
       "   :COURTESY Erik Enge, David Lichteblau, Nikodemus Siivola\n"
       "   :SEE-ALSO (URL `http://common-lisp.net/project/hyperspec-lookup/')\n")
     "http://common-lisp.net/cgi-bin/viewcvs.cgi/root.tar.gz")
    (cl-slime 
     ,(concat 
       "Current distribution of Slime from CVS.\n"
       "   :SEE-ALSO (URL `http://www.cliki.net/SLIME')\n")
     "http://common-lisp.net/project/slime/snapshots/slime-current.tgz")
    (cl-cookbook
     ,(concat
       "VIEW CVS of the cl-cookbook at sourceforge.\n"
       "   :SEE-ALSO (URL `http://cl-cookbook.sourceforge.net/')\n")
     "http://cl-cookbook.cvs.sourceforge.net/viewvc/cl-cookbook/cl-cookbook/")
    (cl-onlisp
     ,(concat     
       "Paul Graham's On Lisp re-purposed as HTML.\n"
       "   :SEE-ALSO (URL `http://www.bookshelf.jp/texi/onlisp/onlisp.html')\n"
       "   :NOTE If unable to extract file rename it to onlisp.tar and retry.\n")
     "http://www.bookshelf.jp/texi/onlisp/onlisp.tar.gz")
    (cl-gentle
     ,(concat
       "Common Lisp - Gentle Intro to Symbolic Computation.\n"
       "   :COURTESY David Touretzky.\n"
       "   :SEE-ALSO (URL `http://www-2.cs.cmu.edu/~dst/LispBook/')\n"
       "   :SEE-ALSO (URL `http://www.cs.cmu.edu/~dst/Lisp/code/')\n"
       "   :SEE-ALSO (URL `http://www.cs.cmu.edu/~dst')\n")
     "http://www-2.cs.cmu.edu/~dst/LispBook/book.ps")
    (lispdoc
     ,(concat
       "Search engine for documentation of assored Common Lisp related material.\n"
       "   Includes xrefs across the following media, books, packages, libraries etc.:\n"
       "   Common Lisp HyperSpec\n"
       "   Common Lisp the Language - Second Edition \(CLTL2\) <- Guy Steele.\n"
       "   On Lisp <- Paul Graham\n"
       "   Paradigms of Artificial Intelligence Programming (\PAIP\) <- Peter Norvig.\n"
       "   Succesful Lisp <- David Lamkins\n"
       "   Practical Common Lisp \(PCL\) <- Peter Seibel.\n\n"
       "   The follwoing Common Lisp extesion packages:\n"
       "   ASDF, CFFI, CFFI-FEATURES, CFFI-SYS, CFFI-UTILS, CHUNGA, CL+SSL, CL+SSL-SYSTEM,\n"
       "   CL-BASE64, CL-PPCRE, CL-PPCRE-TEST, CL-WHO, COMMON-LISP, COMMON-LISP-CONTROLLER,\n"
       "   FLEXI-STREAMS, HUNCHENTOOT, HUNCHENTOOT-ASD, MD5, RFC2388, S-XML,\n"
       "   TRIVIAL-GRAY-STREAMS, URL-REWRITE\n\n"
       "   Steel Bank Common Lisp \(SBCL\) <- The Venerable\n"
       "   :COURTESY Bill Moorier\n")
     "http://lispdoc.com/")
    (lispy
     ,(concat
       "Lispy is a library manager for Common Lisp, written in Common Lisp.\n"
       "   :COURTESY Matthew Kennedy\n"
       "   :SEE-ALSO (URL `http://common-lisp.net/project/lispy/')\n")
     "http://common-lisp.net/project/lispy/repository/map.lisp-expr")
    (ilisp
     ,(concat
       "Ilisp source from git with updates for current Emacsen.\n"
       "   :COURTESY Barak A. Pearlmutter's <barak@cs.nuim.ie> repo maintainance.\n")
     ;; "http://sourceforge.net/projects/ilisp/files/ilisp/ilisp-snapshot/ilisp-20021222.tar.gz"
     ;; "http://sourceforge.net/projects/ilisp/files/ilisp/ilisp-snapshot/ilisp-doc-20021222.tar.gz"
     ;; git://git.debian.org/collab-maint/ilisp.git
     "http://git.debian.org/?p=collab-maint/ilisp.git;a=summary")
    (oaklisp 
     "Oaklisp is an OOP Scheme dialect with lexical scoping and first-class classes.\n"
     ;; "http://www.bcl.hamilton.ie/~barak/oaklisp/"
     ;; "http://www.bcl.hamilton.ie/~barak/oaklisp/binaries/dpkg/sources/oaklisp_1.3.1.tar.gz"
     ;; "https://alioth.debian.org/projects/oaklisp/"     
     ;; "https://alioth.debian.org/snapshots.php?group_id=100056"
     "http://www.bcl.hamilton.ie/~barak/oaklisp/release/oaklisp-07-Jan-2000.tar.gz")
    (cl-kyoto
     ,(concat     
       "Kyoto Common Lisp circa 1987/09.\n"
       "   :COURTESY Taiichi Yuasa and Masami Hagiya\n"
       "   :SEE-ALSO (URL `ftp://ftp.sra.co.jp/pub/lang/lisp/kcl/Sep-87/')\n")
     "ftp://ftp.sra.co.jp/pub/lang/lisp/kcl/Sep-87/compressed_portkit.gz")
    (cl-wcl
     ,(concat     
       "WCL Common Lisp circa 1992-10.\n"
       "   :COURTESY Wade L. Hennessey\n"
       "   :SEE-ALSO (URL `ftp://ftp.sra.co.jp/pub/lang/lisp/wcl/lfp-paper.ps')\n")
     "ftp://ftp.sra.co.jp/pub/lang/lisp/wcl/wcl-2.14.tar.gz")
    (cl-akcl
     ,(concat
       "Austin Kyoto Common Lisp circc 1992-06-19.\n"
       "   :COURTESY William Schelter's KCL additions.\n"
       "   :NOTE The interesting files `find-doc.el', `DOC-keys.el', `doc-com.el',\n"
       "   `dbl.el', `docstrings', `edoc' `DOC' in archive's `./doc' directory.\n")
     "ftp://ftp.sra.co.jp/pub/lang/lisp/akcl/akcl-1-615.tar.gz"))
  "*List of Common Lisp documentation packages and legacy/historic packages of
use with Emacs.  Each sublist element of has the format:\n
\(SYMBOL DESCRIPTION URL\)\n
:NOTE The following keys point to webpages or dvc archives:\n
 ilisp cl-cookbook oaklisp lispdoc lispy\n
:SEE-ALSO `*cl-cmu-ai-repo*', `mon-help-cl-pkgs', `mon-help-wget-cl-pkgs'.\n►►►")
;;)
;;
;;; :TEST-ME *cl-ext-pkg-map*
;;;(progn (makunbound '*cl-ext-pkg-map*) (unintern '*cl-ext-pkg-map*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-23T20:41:48-05:00Z}#{09524} - by MON KEY>
;;(eval-when (compile eval)
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
;;) ;; :CLOSE eval-when
;;
;;; :TEST-ME (mon-help-cl-pkgs t)
;;

(eval-when (load)
(put 'mon-help-cl-pkgs 'function-documentation 
       (concat     
        "Return a pretty printed list of historic and Common Lisp packages and Specs."
        "\nIncludes dpANSI Spec, ISO Spec, CLTL2, Ilisp, CL-cookbook, eli, cl-shell etc.\n\n"
        (mon-help-cl-pkgs)
        "\n:SEE-ALSO `*cl-cmu-ai-repo*',`*cl-ext-pkg-map*', `mon-help-wget-cl-pkgs'.\n►►►"))
)

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
cl-cookbook, oaklisp, ilisp, lisp, lispdoc. These point to web pages and are not
files.  Download or add these manually if that is what you want. The following
loop returns the absent URL's:\n
\(let \(manual-dl\)
  \(dolist \(cl-wpgs '\(ilisp cl-cookbook oaklisp lispdoc lispy\) manual-dl\)
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
          '(cl-cookbook ilisp lispy lispdoc oaklisp))
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
`mon--test--help-wget-cl-pkgs'.\n►►►"
  (let ((fnm-tst-wgt (file-name-nondirectory wget-fname))
        (sys (cond ((eq system-type 'windows-nt) 'wnz)
                   ((or (eq system-type 'gnu/linux)
                        (eq system-type 'linux)) 'gnu)
                   ((not (executable-find "wget")) 'no-exec)))
        (read-wget-string))
    (unless (directory-files default-directory nil (concat fnm-tst-wgt "$"))
      (error "File does not exist or function invoked outside file's directory"))
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


;; :CL-HSPEC-W3M-PARSING
;;; ============================================================
;;; I wasn't even a teenager when development of ANSI-CL began over 23 yrs ago.
;;; I was entering college when the standard was formalized circa 1994 over 15
;;; years ago. As lispers go I'd bet I'm on the young end of the spectrum...
;;; Growing/Cultivating Lisp requires cultivation of Lisp documentation.
;;;
;;; Emacs Lisp is super at documentation. Common Lisp not so.
;;; Emacs thrives in large part because it is accessible and open.
;;; Common Lisp not so much so.
;;;
;;; The two documents which best document the standard are locked up in an
;;; antiquated copyright doubtless few CL originators would endorse today.
;;; As such while other more contemporary programming languages and associated
;;; technical documents allow re-purposing the _format_ of their respective
;;; contents the ANSI-CL is hog-tied to circa 1990 intellectual property paradigms
;;;
;;; According to copyright disclaimers on the various CL Hyperspecs versions (v3,
;;; v6, v7) while large portions of the Hyperspec (as originally sanctioned by XJ313
;;; at Harlequin's request) do copy in a nearly verbatim manner from the ANSI spec
;;; (itself derived of CLTL[1&2]. This inclusion was originally allowed with the
;;; permission of X3 and produced under the auspices of the XJ313 Cleanup
;;; subcommittee. No public digital record of such a formal approval remains
;;; separate from the rights claim distributed with the HyperSpec itself and it is
;;; unclear from what little remains of the XJ313 Charter (it's organization was a
;;; hasty/controversial affair) whether such extension of copyright was even
;;; permissible within the scope of the charter. Claims to copyright on Govt. funded
;;; projects strike us as odd given that CLTL, X3J13, and ANSI-CL would not have
;;; been produced without sufficient and prolonged funding from DARPA, ARPA,
;;; Etc. IOW, some degree of the ANSI-CL 'product' is a public deliverable funded by
;;; U.S. Taxpayers. It is doubtful that such material would meet a rigorous
;;; copyright challenge... Regardless, since 2005 when Lispworks Limited a UK based
;;; company acquired the rights to Harlequin's claim on the HyperSpec they have
;;; maintained a slippery and somewhat dubious claim of ownership w/re the
;;; HyperSpec. As such, while it is not generally believed permissible to distribute
;;; the the HyperSpec in an derivative format there seems little to prevent one from
;;; reversing the Hyperspec's HTML to a privately accessible format.  Following is
;;; an attempt at using w3m and Emacs' w3m extension package to do so...
;;;
;;; The end goal is to use produce code capable of stripping the hyperspec and
;;; re-purposing it for suitable presentation in a dedicated Emacs *Help* buffer for
;;; _personal_ presentation of the Hyperspec on my machine for my needs.
;;; IOW _not_ using TeXinfo and not using a web-browser to access a fully
;;; hyperlinked and xref'd documentation of the standard. If I'm able to succeed
;;; others could probably use similar such code to similar ends :)
;;; 
;;; ============================================================
;; :USEFUL-LINKS-REFERENCES-SOURCES-QUOTES
;;;
;; :HYPERSPEC-V3 
;;; :NOTE This is the variable length version circa 1996.
;;; :SEE (URL `http://www.cs.cmu.edu/Groups/AI/html/hyperspec/clspec.html')
;;; :SEE (URL `http://www.cs.cmu.edu/Groups/AI/html/hyperspec/clspec30.tgz')
;;; 
;;;
;; :CL-ANSI-SPEC-COPYRIGHT-P
;;; According to prevailing wisdom:
;;; :SEE (URL 
;;; (concat "http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/"
;;; "48865a78e6dfabc0/b5d0b025348d5b63?lnk=gst&q=Hyperspec+copyright#b5d0b025348d5b63")
;;;
;;; And,
;;; :SEE (URL `http://lists.nongnu.org/archive/html/axiom-developer/2007-06/msg00456.html')
;;;
;; :X3J13-ARCHIVES
;;; :SEE (URL `ftp://ftp.parc.xerox.com/pub/cl/')
;;;
;; :HSPEC-RESTRICTED-RIGHTS-LEGEND
;;;
;;; The Common Lisp HyperSpec is subject to the following Restricted Rights Legend:
;;;
;;;     ``Use, duplication, or disclosure by the United States Government is
;;;     subject to the restrictions set forth in (i) FAR 52.227-14 Alt III, (ii) 
;;;     FAR 52.227-19, (iii) DFARS 252.7013(c)(1)(ii), or (iv) the accompanying
;;;     license Agreement, as applicable. For purposes of the FAR, the Software
;;;     shall be deemed to be ``unpublished'' and licensed with disclosure
;;;     prohibitions, rights reserved under the copyright laws of the United
;;;     States. Harlequin Incorporated, One Cambridge Center, Cambridge,
;;;     Massachusetts 02142.''
;;;
;;; :SEE (concat "http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/"
;;;              "HyperSpec/FrontMatter/About-HyperSpec.html#Legal")
;;;
;;; :SOURCE From Hspec v3 @ MIT Intelligent Information Infrastructure Project.
;;; :SEE (URL `http://www.ai.mit.edu/projects/iiip/home-page.html')
;;;
;; :CL-SPEC-FROM-ANSI-THEY-ARE-LOST!
;;; (concat "http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/"
;;;  "48865a78e6dfabc0/b5d0b025348d5b63?lnk=gst&q=Hyperspec+copyright#b5d0b025348d5b63")
;;;
;;;
;; :X3J13-CHARTER
;;; Kent Pitman, Sunday, April 18, 1999 notes on the X3J13 Charter
;;; :SEE (URL `http://www.nhplace.com/kent/CL/x3j13-sd-05.html')
;;;
;;;
;; :GFDL-ANSI-CL
;;;
;;; ----Original Message-----
;;; From: Joe Corneli [mailto:address@hidden 
;;; Sent: Thursday, June 16, 2005 4:40 PM
;;; To: Garner, Jennifer
;;; Subject: ansi common lisp standard?
;;;
;;;
;;; Hi Jennifer,
;;;
;;; in October or November of last year, we sent the appended request,
;;; concerning the ANSI common lisp standard, to you, in hopes that you
;;; could communicate the request to the ANSI J13 management committee.
;;; Our aim was to obtain a copy of the Common Lisp standard with
;;; permissions that would enable us to use it as the basis of new Lisp
;;; documentation.  Can you tell me what the status of this request is
;;; currently?
;;;
;;; Thank you,
;;; Joe Corneli
;;;
;;;   To the members of the ANSI J13 management committee:
;;;
;;;   We wish to file a request on the behalf of Lisp users world wide
;;;   that ANSI release the text of the Common Lisp standard in a way
;;;   that would make it legal to use the standard as the foundation
;;;   for a system of documentation which adequately describes Common
;;;   Lisp as a living language.
;;;
;;;   We recommend the use of the GNU Free Documentation License, which
;;;   was specifically designed to apply to documentation and standards
;;;   documents.  Using the GFDL, the copyright holder grants anyone
;;;   permission to publish both changed and unchanged versions of the
;;;   document, but requires that they all be distributed under the
;;;   same license.
;;;
;;;   The GFDL has a special feature intended for standards documents.
;;;   The document can have an Endorsements section which must be
;;;   removed from any modified version; ANSI's endorsement could say
;;;   that the document contains the official definition of ANSI Common
;;;   Lisp.  Other provisions of the license require giving credit to
;;;   the authors of earlier versions.  Thus, modified versions would
;;;   give credit to ANSI but could not claim to be the standard.
;;;
;;;   We would be glad to explore ways to resolve any issues or
;;;   uncertainties that may arise as you consider this request.
;;;
;;;    Joseph Corneli
;;;    Richard Stallman
;;;    Camm McGuire
;;;    Richard Gabriel
;;;    Bruno Haible
;;;    Sam Steingold
;;;
;;; :SOURCE (URL `http://lists.nongnu.org/archive/html/axiom-developer/2007-06/msg00456.html')
;;; ============================================================
;;;
;;; ============================================================
;;; The following section provides parsing routines for extracting 
;;; text-properties and their ranges from Common-Lisp HyperSpec xrefs,
;;; italicized strings, bold strings and plain-text strings in buffer formatted
;;; with w3m and assocated Emacs package emac-w3m.
;;;
;;; :FUNCTION `mon-hspec-plain-p',`mon-hspec-bld-p',
;;; `mon-hspec-header-line-p',`mon-hspec-href-p', `mon-w3m-spec-p',
;;; `mon-hspec-prop-type', `mon-hspec-it-p', `mon-hspec-out'
;;; `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m'
;;; `mon-hspec-unparse-w3m-to-buffer'
;;; :VARIABLE
;;; `*mon-hs-root-dir*', `*mon-hs-parse-buffer*', `*mon-hs-unprs-buffer*'
;;;
;;; The mon-hspec-* procedures were tested with:
;;; (emacs-version)
;;; => "GNU Emacs 23.1.90.1 
;;;  | (i486-slackware-linux-gnu, GTK+ Version 2.14.7)  of 2009-12-20"
;;;
;;; w3m-version
;;; => "w3m/0.5.2"
;;;
;;; w3m-fill-column
;;; => 80
;;;
;;; :NOTE The tests for the presence of face text-property with value
;;;       'w3m-anchor may be suspect as `w3m-anchor' is _also_ a macro defined
;;;       in :FILE w3m-util.el We test for the presence of this property and
;;;       code is provided which invokes `' however it is unlikely the
;;;       conditional predicates below will ever actually reach a point there
;;;       they will pass this symbol as a value.
;;;
;;; ============================================================

(eval-when-compile
  (when (locate-library "hyperspec.el") ;<- `*mon-hs-root-dir*'
    (require 'hyperspec)))

;;; :NOTE The symbol `common-lisp-hyperspec-root' needs to be present.
;;; We shouldn't bind it if user hasn't loaded slime/hyperspec but _will_ later.
(unless (or (bound-and-true-p common-lisp-hyperspec-root)
            (featurep 'hyperspec))
  (setq common-lisp-hyperspec-root nil))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:27-05:00Z}#{09527} - by MON>
(defvar *mon-hs-root-dir* common-lisp-hyperspec-root
  "*The base directory path that the local Hyper Linked Common Lisp specification
resides under. For example, the following unwieldy and long local path:
\"/<SOME>/<PATH>/CL-documentation/CL-hyperspec-html/HyperSpec-v3/\"
:EXAMPLE\n*mon-hs-root-dir*\n
:NOTE MON uses the same value of the path set with the 
:VARIABLE `common-lisp-hyperspec-root' wich is defined in the 
:FILE hyperspec.el and provided with current Slime distribution.\n
:SEE-ALSO `*mon-hs-parse-buffer*', `*mon-hs-unprs-buffer*',
`mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-out',
`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer'.\n►►►")
;;
;;; :TEST-ME *mon-hs-root-dir*
;;;(progn (makunbound '*mon-hs-root-dir*) (unintern '*mon-hs-root-dir*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:29-05:00Z}#{09527} - by MON>
(defvar *mon-hs-parse-buffer* "*CL-HSPEC-CONV*"
  "*Name of temporary buffer holding the parsed text properties of the current
  w3m text under examination.\n
:SEE-ALSO `*mon-hs-unprs-buffer*', `*mon-hs-root-dir*',`mon-hspec-plain-p',
`mon-hspec-bld-p', `mon-hspec-it-p', `mon-hspec-header-line-p',
`mon-hspec-href-p', `mon-w3m-spec-p',`mon-hspec-prop-type', 
`mon-hspec-stk-n-mv', `mon-hspec-out', `mon-hspec-parse-w3m',
`mon-hspec-unparse-w3m',`mon-hspec-unparse-w3m-to-buffer'.\n►►►")
;;
;;; :TEST-ME *mon-hs-parse-buffer*
;;;(progn (makunbound '*mon-hs-parse-buffer*) (unintern '*mon-hs-parse-buffer*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-29T12:08:40-05:00Z}#{09532} - by MON KEY>
(defvar *mon-hs-unprs-buffer* "*CL-HSPEC-UNPARSE*"
  "*Name of temporary buffer holding the round tripped unparsed text properties
  in return value generated with `mon-hspec-parse-w3m'.\n
:CALLED-BY `mon-hspec-unparse-w3m'\n
:SEE-ALSO `*mon-hs-root-dir*', `*mon-hs-parse-buffer*'
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-it-p', `mon-hspec-header-line-p',`mon-hspec-href-p',
`mon-w3m-spec-p',`mon-hspec-prop-type', `mon-hspec-stk-n-mv',
`mon-hspec-out'.\n►►►")
;;
;;; :TEST-ME *mon-hs-unprs-buffer*
;;;(progn (makunbound '*mon-hs-unprs-buffer*) (unintern '*mon-hs-unprs-buffer*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:32-05:00Z}#{09527} - by MON>
(defun mon-hspec-href-p ()
  "Are we looking at an w3m-href-anchor text property (with/without bold/italic).
When t return a two element list of the form:\n
\(:xref-on   \"<XREF-STRING>\"\n  :xref-to \"<XREF-PATH>\"
  :xref-range \(<START> . <END>\)\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-w3m-spec-p', `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-parse-w3m',`mon-hspec-unparse-w3m'
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `*mon-hs-parse-buffer*',
`*mon-hs-root-dir*', `mon-line-test-content', `mon-test-props',
`mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (let (this-xref-prop)
    (when (get-text-property (point) 'w3m-href-anchor)
      (let* ((tp-xrf-frm  (point))
             (tp-xrf-to   (next-single-property-change tp-xrf-frm 'w3m-href-anchor))
             (tp-xrf-str  (buffer-substring-no-properties tp-xrf-frm tp-xrf-to))
             (tp-xrf-prop (get-text-property tp-xrf-frm 'w3m-href-anchor)))
        (setq this-xref-prop
              `(:xref-on ,tp-xrf-str 
                         :xref-to ,(replace-regexp-in-string 
                                    (concat "file://" *mon-hs-root-dir*) "" tp-xrf-prop)
                         :xref-range (,tp-xrf-frm . ,tp-xrf-to)))))))

;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:37-05:00Z}#{09527} - by MON>
(defun mon-hspec-header-line-p ()
  "Are we looking at a w3m header line.\n
Check if `w3m-header-line-location-content' face and 
`w3m-header-line-location-title' face properties are present.\n
When t return a two element list of the form:\n
\(\(:location     \"Location: \"  :location-range     \(<START> . <END>\)\)
 \(:location-url \"<URL>\"       :location-url-range \(<END> . <END>\)\)\)\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p',
`mon-hspec-href-p', `mon-w3m-spec-p',`mon-hspec-prop-type',`mon-hspec-stk-n-mv',
`mon-hspec-parse-w3m',`mon-hspec-unparse-w3m',`mon-hspec-unparse-w3m-to-buffer',
`mon-hspec-out', `*mon-hs-parse-buffer*',`*mon-hs-root-dir*',
`mon-line-test-content',`mon-test-props', `mon-plist-keys',
`mon-list-all-properties-in-buffer',`mon-nuke-text-properties-buffer'.\n►►►"
  (when (eq (car (get-text-property (point) 'face)) 'w3m-header-line-location-title)
    (let ((loc-info `(:location (,(point) . ,(next-single-property-change (point) 'face)))))
      (setq loc-info `(,(car loc-info) 
                        ,(buffer-substring-no-properties (caadr loc-info) (cdadr loc-info))
                        :location-range ,@(cdr loc-info)))
      (when (eq (car (get-text-property (cdr (plist-get loc-info :location-range)) 'face))
                'w3m-header-line-location-content)
        (let ((loc-path `(:location-url
                          (,(cdr (plist-get loc-info :location-range))
                            . ,(next-single-property-change (cdr (plist-get loc-info :location-range)) 'face))))
              (rplc-pth-str)
              (rplc-pth))
          (setq rplc-pth-str (buffer-substring-no-properties (caadr loc-path) (cdadr loc-path)))
          (setq rplc-pth (replace-regexp-in-string 
                          (concat "\\(file:///?" *mon-hs-root-dir* "\\)\\(.*\\.htm?\\)")
                          "\\2" rplc-pth-str))
          (setq loc-path `(,(car loc-path)
                            ,(concat (or rplc-pth rplc-pth-str) "\n")
                            :location-url-range 
                            ,(if rplc-pth
                                 ;; :NOTE (cons (caadr '(:loc (11 . 153))) ;=> 11
                                 ;;       (cdadr '(:loc (11 . 153))) ;=> 153 
                                  ;;       ) ;=> (11 . 153)
                                 ;; :NOTE Should we 1+ these lengths b/c of concat'd newline?
                                 (cons (caadr loc-path)
                                        (- (cdadr loc-path)
                                           (- (length rplc-pth-str)
                                              (length rplc-pth))))
                                  (cdr loc-path))))
          (when rplc-pth 
            (let* ((ofst-frm (plist-get loc-path :location-url-range))
                  (ofst-lst (cons (car ofst-frm)
                                  (+ (car ofst-frm)
                                     (length rplc-pth-str)))))
              (setq loc-path (reverse loc-path))
              (push :location-url-offset loc-path)
              (push ofst-lst loc-path)
              (setq loc-path (reverse loc-path))))
          (setq loc-info `(,loc-info ,loc-path)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:40-05:00Z}#{09527} - by MON>
(defun mon-hspec-it-p ()
  "Are we looking at only a w3m-italic face text-property.\n
When t return a two element list of the form:\n
\(:italics-on \"<ITALICIZED-STRING>\"  :italics-range \(<START> . <END>\)\n
:SEE-ALSO `w3m-fontify-italic',`mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-header-line-p',`mon-hspec-href-p', `mon-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m',
`mon-hspec-unparse-w3m',`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out',
`*mon-hs-parse-buffer*', `*mon-hs-root-dir*', `mon-line-test-content',
`mon-test-props', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (let ((face-it-p (get-text-property (point) 'face)))
    (when (and (member 'w3m-italic face-it-p)
               (and (not (member 'w3m-bold face-it-p))
                    (not (member 'w3m-anchor face-it-p))
                    (not (null face-it-p))))
      (let* ((tp-it-frm (point))
             (tp-it-to  (next-single-property-change tp-it-frm 'face))
             (tp-it-str (buffer-substring-no-properties tp-it-frm tp-it-to))
             (this-it-prop))
        (setq this-it-prop
              `(:italics-on ,tp-it-str :italics-range (,tp-it-frm . ,tp-it-to)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:43-05:00Z}#{09527} - by MON>
(defun mon-hspec-bld-p ()
  "Are we looking at only a w3m-bold face text-property.\n
When t return a two element list of the form:\n
\(:bold-on \"<BOLD-STRING>\"  :bold-range \(<START> . <END>\)\n
:SEE-ALSO `w3m-fontify-bold', `mon-hspec-plain-p',`mon-hspec-it-p',
`mon-hspec-header-line-p',`mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m',
`mon-hspec-out', `mon-hspec-unparse-w3m',`*mon-hs-parse-buffer*',
`mon-hspec-unparse-w3m-to-buffer', `*mon-hs-root-dir*',
`mon-line-test-content', `mon-test-props', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
   (let ((face-bold-p (get-text-property (point) 'face)))
     (when (and (member 'w3m-bold face-bold-p)
                (and (not (member 'w3m-italic face-bold-p))
                     (not (member 'w3m-anchor face-bold-p))
                     (not (null face-bold-p))))
       (let* ((tp-bld-frm (point))
              (tp-bld-to (next-single-property-change tp-bld-frm 'face))
              (tp-bld-str (buffer-substring-no-properties tp-bld-frm tp-bld-to))
              (this-bld-prop))
         (setq this-bld-prop
               `(:bold-on ,tp-bld-str :bold-range (,tp-bld-frm . ,tp-bld-to)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:47-05:00Z}#{09527} - by MON>
(defun mon-hspec-plain-p ()
  "Are we looking at plain-text sans other w3m text-properties we are parsing.
When t return a two element list of the form:\n
\(:plain-on \"<SOME-PLAIN-TEXT>\"  :plain-range \(<START> . <END>\)\n
:SEE-ALSO `mon-hspec-bld-p',`mon-hspec-it-p', `mon-hspec-header-line-p',
`mon-hspec-href-p', `mon-w3m-spec-p' `mon-hspec-prop-type', 
`mon-hspec-stk-n-mv',`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `*mon-hs-parse-buffer*',
`*mon-hs-root-dir*', `mon-line-test-content', `mon-test-props', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (when (and (not (get-text-property (point) 'face))
             (not (get-text-property (point) 'w3m-href-anchor))
             (next-property-change (point))
             (not (= (point) (buffer-end 1)))
             (not (= (next-property-change (point)) (buffer-end 1))))
    (let* ((tp-pln-frm (point))
           (tp-pln-to  (next-property-change tp-pln-frm))
           (tp-pln-str (buffer-substring-no-properties tp-pln-frm tp-pln-to))
           (this-pln-prop))
      (setq this-pln-prop
            `(:plain-on ,tp-pln-str :plain-range (,tp-pln-frm . ,tp-pln-to))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:52-05:00Z}#{09527} - by MON>
(defun mon-w3m-spec-p (spec spec-list)
  "Helper function for `mon-hspec-prop-type'.
When SPEC-LIST contains SPEC return the sublist of SPEC-LIST as per `memq'.\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p', 
`mon-hspec-header-line-p', `mon-hspec-href-p',, `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `*mon-hs-parse-buffer*',
`*mon-hs-root-dir*', `mon-line-test-content', `mon-test-props', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (memq spec spec-list))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:56-05:00Z}#{09527} - by MON>
(defun mon-hspec-prop-type ()
  "Are we looking at a w3m bold, italic, plain, or href text-property.\n
Return one five values conditional on value of text-properties-at-point:\n
  o Return `w3m-href-anchor' when there is an href anchor.
  o Return `w3m-italic' when there is a w3m-italics face property.
  o Return `w3m-bold'   when there is a w3m-bold face property.
  o Return `w3m-anchor' when there is a w3m-anchor face property.
  o Return `plain-text' when there aren't any relevant w3m properties.\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-stk-n-mv',`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `*mon-hs-parse-buffer*',
`*mon-hs-root-dir*' `mon-line-test-content', `mon-test-props', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (let ((the-spec (text-properties-at (point)))
        (w3m-face-spec))
    (setq w3m-face-spec
          `(,(car (mon-w3m-spec-p 'w3m-href-anchor the-spec))
             ,@(cadr (mon-w3m-spec-p 'face the-spec))))
    (cond ((car w3m-face-spec) (car w3m-face-spec))
          ((cdr w3m-face-spec)
           (let ((mp-fc (cdr w3m-face-spec)))
             (car (or (mon-w3m-spec-p 'w3m-italic mp-fc)
                      (mon-w3m-spec-p 'w3m-bold   mp-fc)           
                      (mon-w3m-spec-p 'w3m-anchor mp-fc)))))
          ((not (cdr w3m-face-spec))  'plain-text))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:59-05:00Z}#{09527} - by MON>
(defun mon-hspec-out (prop)
  "Helper function for `mon-hspec-stk-n-mv' to direct parses to temp buffer.
Temporary buffer takes the value specified in:\n:VARIABLE `*mon-hs-parse-buffer*'\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-parse-w3m',
`mon-hspec-unparse-w3m', `mon-hspec-unparse-w3m-to-buffer', `*mon-hs-root-dir*',
`mon-line-test-content', `mon-test-props', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (get-buffer-create *mon-hs-parse-buffer*)
  (terpri (get-buffer *mon-hs-parse-buffer*))
  (prin1 prop (get-buffer *mon-hs-parse-buffer*)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:15:02-05:00Z}#{09527} - by MON>
(defun mon-hspec-stk-n-mv ()
  "Test Hyperlink Specification properties with mon-hspec-* predicates.
This function is the point mover and conditionally marshalls a predicate 
plist constructor function conditional on return value of `mon-hspec-prop-type'.\n
The constructors called are one of the following:
 o `mon-hspec-plain-p'\n o `mon-hspec-bld-p'
 o `mon-hspec-it-p'\n o `mon-hspec-href-p'\n
Return value of the predicate constructor is routed to the temporary parse buffer 
specifed by `*mon-hs-parse-buffer*' via the function `mon-hspec-out'.\n
:CALLED-BY `mon-hspec-parse-w3m' ;<- wrapper interface.
:SEE-ALSO `mon-hspec-unparse-w3m-to-buffer' `mon-hspec-header-line-p', 
`mon-w3m-spec-p', `mon-hspec-prop-type', `mon-hspec-unparse-w3m', 
`*mon-hs-root-dir*', `mon-line-test-content', `mon-test-props',`mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (let ((keep-looking t)
        (curr-prop)) 
    (cond ((eq (point) (buffer-end 1))
           (setq keep-looking nil))
          ((not (next-property-change (point)))
           (goto-char (buffer-end 1))
           (setq keep-looking nil))
          (t (goto-char (next-property-change (point)))))
    (setq curr-prop (mon-hspec-prop-type))
    (if keep-looking
        (cond ((eq curr-prop 'plain-text)      (mon-hspec-out (mon-hspec-plain-p)))
              ((eq curr-prop 'w3m-href-anchor) (mon-hspec-out (mon-hspec-href-p)))
              ((eq curr-prop 'w3m-italic)      (mon-hspec-out (mon-hspec-it-p)))
              ((eq curr-prop 'w3m-bold)        (mon-hspec-out (mon-hspec-bld-p)))
              ((eq curr-prop 'w3m-anchor)      (mon-hspec-out (mon-hspec-href-p)))
              (t (if (or (not (next-property-change (point)))
                         (eq (point) (buffer-end 1)))
                      (goto-char (buffer-end 1))
                      (goto-char (next-property-change (point)))))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:15:07-05:00Z}#{09527} - by MON>
(defun mon-hspec-parse-w3m ()
  "Parse the Hyperlink Specification in current w3m buffer.
Invokes `mon-hspec-stk-n-mv' repeatedly on the contents of current w3m buffer
until `eobp' or there are no more text-properties left to parse.
Contents returned in buffer specified by `*mon-hs-parse-buffer*'.\n
:SEE-ALSO `mon-hspec-unparse-w3m', `mon-hspec-unparse-w3m-to-buffer',
`mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p', 
`mon-hspec-header-line-p' `mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv' `mon-hspec-out',`*mon-hs-root-dir*',
`mon-line-test-content', `mon-test-props', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (progn
    (goto-char (buffer-end 0))
    (let ((hdr (mon-hspec-header-line-p)))
      (when hdr 
        (mon-hspec-out (car hdr))
        (mon-hspec-out  (cadr hdr))))
    (while (not (eobp)) (mon-hspec-stk-n-mv))))

;;; ==============================
;;; :NOTE Our original pre-parse Hspec path may be BIG. So we shorten it.
;;; Shortening is handy for maintaining portability across systems and is
;;; especially useful where we may not be parsing 8.3 style paths of Hspec v 6+
;;; and are using the older variable length paths of Hspec v3 of which there a
;;; case can be made for using.  That said, while Hspec v3 paths are largish,
;;; their v6+ counterpartds do not designate context as usefully i.e: 
;;;
;;; "/f_set_pp.htm" as opposed to "/fun_set-pprint-dispatch.html"
;;;
;;; Regardless, it is likely the path will be needlesly large as with this one:
;;;
;;; (length 
;;;  "file:///<SOME>/<REALLY>/<LONG>/<PATH>/Body/fun_set-pprint-dispatch.html")
;;; ;=> 71
;;;
;;; The new shortened path is smaller and more portable across systems.
;;; (length "Body/fun_set-pprint-dispatch.html") ;=> 33
;;;
;;; But, now we've screwed up our original buffers text-property locations :(
;;; Lets fix that. Whenever the the big path is shortened, the cdr of the list
;;; returned by `mon-hspec-unparse-w3m' will contain an extra property
;;; `:location-url-offset'. We can use the values of this property to adust the
;;; rest of our parse list with some quick lookups. For example, given the path
;;; values above our list will contain the following properties:
;;;
;;; :location-url-range (11 . 44)   (- 44 11) => 33
;;; :location-url-offset (11 . 115) (- 115 11) => 104
;;;
;;; To get the length of the orginal path, take the difference of cdr of offset
;;; against cdr of range above: (- 115 44) => 71  (+ 71 33) => 104
;;; To get the real next property change add cdr of :range to the length of
;;; original path: (+ 44 71) => 115
;;;
;;; So, to adjust the offsets for our text-property locations we add (in this
;;; case) 71 to the car and cdr each `:*-range' value in our list.  To see this
;;; is so verify by comparing the output of `mon-hspec-unparse-w3m' against
;;; `mon-hspec-parse-w3m'.
;;;
;;; :NOTE We currently shorten paths using a regexp lookup against a local path
;;; however, the code can be easily adapted for parsing and shortening remote
;;; paths wth "http://" in lieu of local paths with "file://"
;;; :SEE `mon-hspec-header-line-p's regexp for local variable `rplc-pth'
;;;
;;; :CREATED <Timestamp: #{2009-12-29T02:38:01-05:00Z}#{09532} - by MON>
(defun mon-hspec-unparse-w3m (parse-w3m-buffer-or-file) ; return-parse-in-buffer
  "Round trip snarfed HTML lisp data output of `mon-hspec-parse-w3m' to plain text.
Return a two valued list with the form:
\(\"<HSPEC-ENTRY-AS-PLAIN-TEXT>\" \( \(HSPEC-PLIST-1\) {...} \(HSPEC-PLIST-N\) \)\)\n
Read successive lisp lists in PARSE-W3M-BUFFER-OR-FILE extract the text element
in each list accumulate results to string at car of return value. Push each lisp list
containing the text-properties needed for facificaton, buttonizing, and xrefing
to the list at cdr of return value.\n
When the local URL path has been shortened adjust text-propery offsets accordingly.
:SEE Comments in source for additional discussion and details.\n
:SEE-ALSO `mon-hspec-unparse-w3m-to-buffer', `mon-hspec-stk-n-mv',
 `mon-hspec-out', `mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p', 
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-w3m-spec-p',
`mon-hspec-prop-type', `*mon-hs-parse-buffer*', `*mon-hs-root-dir*'.\n►►►"
  (let ((unprs-mrk (make-marker))
        (onward t)
        (big-parse)
        (gthr-unprs 
         ;; :WAS (get-buffer-create *mon-hs-unprs-buffer*))) ;;"*CL-HSPEC-UNPARSE*"
         (if (buffer-live-p *mon-hs-unprs-buffer*)
             (with-current-buffer (get-buffer *mon-hs-unprs-buffer*) (erase-buffer))
             (get-buffer-create *mon-hs-unprs-buffer*))))
    (with-current-buffer (get-buffer parse-w3m-buffer-or-file)  ;; "fun_float")
      (goto-char (buffer-end 0))
      (set-marker unprs-mrk (point))
      (goto-char (buffer-end 1))
      (insert "\n(\"►►►\")"))
    (newline)
    (while (and unprs-mrk onward)
      (let (prs-sexp)
        (setq prs-sexp (read unprs-mrk))
        (if (equal "►►►" (car prs-sexp))
            (setq onward nil)
            (progn      
              (push prs-sexp big-parse)
              (princ (plist-get prs-sexp (car prs-sexp)) gthr-unprs))))) ;; (current-buffer))))))
    (with-current-buffer (get-buffer parse-w3m-buffer-or-file) ;; "fun_float")
      (goto-char (buffer-end 1))
      (search-backward-regexp "(\"►►►\")")
      (replace-match "")
      (goto-char (buffer-end 0)))
    (setq big-parse (reverse big-parse))
    (with-current-buffer (get-buffer gthr-unprs)
      (let (fin-prs)
        (setq fin-prs(buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))
        (push fin-prs big-parse)))
    (kill-buffer (get-buffer gthr-unprs))
    ;;    big-parse))
    ;; rebuild the offsets against the local url in file line.
    (when (and (assoc :location (cdr big-parse))
               (assoc :location-url (cdr big-parse)))
      (let* ((rebld-ofst (assoc :location-url (cdr big-parse)))
             (calc-ofst (- (cdr (plist-get rebld-ofst :location-url-offset))
                           (cdr (plist-get rebld-ofst :location-url-range)))))
        (mapc #'(lambda (rng)
                  (let* ((rng-type 
                          (cadr (or (plist-member rng :plain-range)
                                    (plist-member rng :italics-range)                    
                                    (plist-member rng :xref-range)
                                    (plist-member rng :bold-range)))))
                    (unless (or (null rng-type)
                                (null (car rng-type))
                                (null (cdr rng-type)))
                      (setf (car rng-type) (abs(- calc-ofst (car rng-type)))
                            (cdr rng-type) (abs (- calc-ofst (cdr rng-type)))))))
              (cdr big-parse))))
    big-parse))
;;
;;; :TEST-ME (progn (with-current-buffer "*w3m*" (mon-hspec-parse-w3m))
;;;           (mon-hspec-unparse-w3m *mon-hs-parse-buffer*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-29T13:09:57-05:00Z}#{09532} - by MON KEY>
(defun mon-hspec-unparse-w3m-to-buffer (&optional return-parse-in-buffer)
  "Insert car and cdr of return value from `mon-hspec-unparse-w3m' in buffer.
Default is to insert to the buffer named by:
:VARIABLE `*mon-hs-unprs-buffer*'.
When RETURN-PARSE-IN-BUFFER is non-nil create the buffer if it does not exist
and insert in that buffer.
:NOTE Unless RETURN-PARSE-IN-BUFFER is current-buffer this procedure erases the
contents of buffer `*mon-hs-unprs-buffer*' or RETURN-PARSE-IN-BUFFER prior to
insertion.
:SEE-ALSO `mon-hspec-parse-w3m', `mon-hspec-stk-n-mv', `mon-hspec-out',
`mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-w3m-spec-p',
`mon-hspec-prop-type', `*mon-hs-parse-buffer*', `*mon-hs-root-dir*'.\n►►►"
  (unwind-protect
       (let ((dump-bfr (if return-parse-in-buffer
                           return-parse-in-buffer
                           *mon-hs-unprs-buffer*))
             (unp))
         (with-current-buffer "*w3m*" (mon-hspec-parse-w3m))
         (setq unp (mon-hspec-unparse-w3m *mon-hs-parse-buffer*))
         (if (buffer-live-p dump-bfr)
             (unless (eq (current-buffer) (get-buffer dump-bfr))
               (with-current-buffer (get-buffer dump-bfr) (erase-buffer)))
             (get-buffer-create dump-bfr))
         (princ (car unp) (get-buffer dump-bfr))
         (princ (concat "\n\n\n;;; ==============================\n"
                        ";;; :BEGIN-PARSED\n\n\n")
                (get-buffer dump-bfr))
         (pp (cdr unp) (get-buffer dump-bfr))
         (unless (eq (current-buffer) (get-buffer dump-bfr))
           (pop-to-buffer dump-bfr t t)))
    (kill-buffer *mon-hs-parse-buffer*)))
;;
;;; :TEST-ME (mon-hspec-unparse-w3m-to-buffer)
;;; :TEST-ME (mon-hspec-unparse-w3m-to-buffer (current-buffer))
;;; :TEST-ME (mon-hspec-unparse-w3m-to-buffer "*prob-some-non-existent-buffer*")

;;; ============================================================
;;; :NOTE Following is an example parse from the w3m HTML of HyperSpec v3:
;;; :FILE ../Body/fun_get-properties.html
;;;
;;; (:location "Location: " :location-range
;;; (1 . 11))
;;; (:location-url "/HyperSpec-v3/Body/acc_bitcm_sbit.html" 
;;;  :location-url-range (11 . 144))
;;; (:xref-on "[HARLEQU" :xref-to "http://www.harlequin.com/" :xref-range
;;; (145 . 153))
;;; (:xref-on "[Common " :xref-to "/FrontMatter/index.html" :xref-range
;;; (153 . 161))
;;; (:bold-on " " :bold-range
;;; (161 . 162))
;;; (:xref-on "[Previou" :xref-to "/Body/fun_vectorp.html" :xref-range
;;; (162 . 170))
;;; (:xref-on "[Up]    " :xref-to "/Body/sec_the_arrays_dictionary.html" :xref-range
;;; (170 . 178))
;;; (:xref-on "[Next]  " :xref-to "/Body/fun_bit-andcm_c2cm_bit-xor.html" :xref-range
;;; (178 . 186))
;;; (:plain-on "
;;;
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; " :plain-range
;;; (186 . 268))
;;; (:italics-on "Accessor" :italics-range
;;; (268 . 276))
;;; (:location "Location: " :location-range
;;; (1 . 11))
;;; (:location-url "/HyperSpec-v3/Body/fun_get-properties.html" 
;;;  :location-url-range (11 . 148))
;;; (:xref-on "[HARLEQU" :xref-to "http://www.harlequin.com/" :xref-range
;;; (149 . 157))
;;; (:xref-on "[Common " :xref-to "/FrontMatter/index.html" :xref-range
;;; (157 . 165))
;;; (:bold-on " " :bold-range
;;; (165 . 166))
;;; (:xref-on "[Previou" :xref-to "/Body/fun_rassoccm__assoc-if-not.html" :xref-range
;;; (166 . 174))
;;; (:xref-on "[Up]    " :xref-to "/Body/sec_the_conses_dictionary.html" :xref-range
;;; (174 . 182))
;;; (:xref-on "[Next]  " :xref-to "/Body/acc_getf.html" :xref-range
;;; (182 . 190))
;;; (:plain-on "
;;;
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; " :plain-range
;;; (190 . 272))
;;; (:italics-on "Function" :italics-range
;;; (272 . 280))
;;; (:plain-on " " :plain-range
;;; (280 . 281))
;;; (:bold-on "GET-PROPERTIES" :bold-range
;;; (281 . 295))
;;; (:plain-on "
;;;
;;; " :plain-range
;;; (295 . 297))
;;; (:bold-on "Syntax:" :bold-range
;;; (297 . 304))
;;; (:plain-on "
;;;
;;; " :plain-range
;;; (304 . 306))
;;; (:bold-on "get-properties" :bold-range
;;; (306 . 320))
;;; (:plain-on " " :plain-range
;;; (320 . 321))
;;; (:italics-on "plist indicator-list" :italics-range
;;; (321 . 341))
;;; (:plain-on " => " :plain-range
;;; (341 . 345))
;;; (:italics-on "indicator, value, tail" :italics-range
;;; (345 . 367))
;;; (:plain-on "
;;;
;;; " :plain-range
;;; (367 . 369))
;;; (:bold-on "Arguments and Values:" :bold-range
;;; (369 . 390))
;;; (:plain-on "
;;;
;;; " :plain-range
;;; (390 . 392))
;;; (:italics-on "plist" :italics-range
;;; (392 . 397))
;;; (:plain-on "---a " :plain-range
;;; (397 . 402))
;;; (:xref-on "property list" :xref-to "/Body/glo_p.html#property_list" :xref-range
;;; (402 . 415))
;;; (:plain-on ".
;;;
;;; " :plain-range
;;; (415 . 418))
;;; (:italics-on "indicator-list" :italics-range
;;; (418 . 432))
;;; (:plain-on "---a " :plain-range
;;; (432 . 437))
;;; (:xref-on "proper list" :xref-to "/Body/glo_p.html#proper_list" :xref-range
;;; (437 . 448))
;;; (:plain-on " (of " :plain-range
;;; (448 . 453))
;;; (:xref-on "indicators" :xref-to "/Body/glo_i.html#indicator" :xref-range
;;; (453 . 463))
;;; (:plain-on ").
;;;
;;; " :plain-range
;;; (463 . 467))
;;; (:italics-on "indicator" :italics-range
;;; (467 . 476))
;;; (:plain-on "---an " :plain-range
;;; (476 . 482))
;;; (:xref-on "object" :xref-to "/Body/glo_o.html#object" :xref-range
;;; (482 . 488))
;;; (:plain-on " that is an " :plain-range
;;; (488 . 500))
;;; (:xref-on "element" :xref-to "/Body/glo_e.html#element" :xref-range
;;; (500 . 507))
;;; (:plain-on " of " :plain-range
;;; (507 . 511))
;;; (:italics-on "indicator-list" :italics-range
;;; (511 . 525))
;;; (:plain-on ".
;;;
;;; " :plain-range
;;; (525 . 528))
;;; (:italics-on "value" :italics-range
;;; (528 . 533))
;;; (:plain-on "---an " :plain-range
;;; (533 . 539))
;;; (:xref-on "object" :xref-to "/Body/glo_o.html#object" :xref-range
;;; (539 . 545))
;;; (:plain-on ".
;;;
;;; " :plain-range
;;; (545 . 548))
;;; (:italics-on "tail" :italics-range
;;; (548 . 552))
;;; (:plain-on "---a " :plain-range
;;; (552 . 557))
;;; (:xref-on "list" :xref-to "/Body/glo_l.html#list" :xref-range
;;; (557 . 561))
;;; (:plain-on ".
;;;
;;; " :plain-range
;;; (561 . 564))
;;; (:bold-on "Description:" :bold-range
;;; (564 . 576))
;;; (:plain-on "
;;;
;;; " :plain-range
;;; (576 . 578))
;;; (:xref-on "get-properties" :xref-to "/Body/fun_get-properties.html#get-properties" :xref-range
;;; (578 . 592))
;;; (:plain-on " is used to look up any of several " :plain-range
;;; (592 . 627))
;;; (:xref-on "property list" :xref-to "/Body/glo_p.html#property_list" :xref-range
;;; (627 . 640))
;;; (:plain-on " entries all at
;;; once.
;;;
;;; It searches the " :plain-range
;;; (640 . 679))
;;; (:italics-on "plist" :italics-range
;;; (679 . 684))
;;; (:plain-on " for the first entry whose " :plain-range
;;; (684 . 711))
;;; (:xref-on "indicator" :xref-to "/Body/glo_i.html#indicator" :xref-range
;;; (711 . 720))
;;; (:plain-on " is " :plain-range
;;; (720 . 724))
;;; (:xref-on "identical" :xref-to "/Body/glo_i.html#identical" :xref-range
;;; (724 . 733))
;;; (:plain-on " to one
;;; of the " :plain-range
;;; (733 . 748))
;;; (:xref-on "objects" :xref-to "/Body/glo_o.html#object" :xref-range
;;; (748 . 755))
;;; (:plain-on " in " :plain-range
;;; (755 . 759))
;;; (:italics-on "indicator-list" :italics-range
;;; (759 . 773))
;;; (:plain-on ". If such an entry is found, the " :plain-range
;;; (773 . 806))
;;; (:italics-on "indicator" :italics-range
;;; (806 . 815))
;;; (:plain-on " and 
;;; " :plain-range
;;; (815 . 821))
;;; (:italics-on "value" :italics-range
;;; (821 . 826))
;;; (:plain-on " returned are the " :plain-range
;;; (826 . 844))
;;; (:xref-on "property indicator" :xref-to "/Body/glo_p.html#property_indicator" :xref-range
;;; (844 . 862))
;;; (:plain-on " and its associated " :plain-range
;;; (862 . 882))
;;; (:xref-on "property value" :xref-to "/Body/glo_p.html#property_value" :xref-range
;;; (882 . 896))
;;; (:plain-on ",
;;; and the " :plain-range
;;; (896 . 906))
;;; (:italics-on "tail" :italics-range
;;; (906 . 910))
;;; (:plain-on " returned is the " :plain-range
;;; (910 . 927))
;;; (:xref-on "tail" :xref-to "/Body/glo_t.html#tail" :xref-range
;;; (927 . 931))
;;; (:plain-on " of the " :plain-range
;;; (931 . 939))
;;; (:italics-on "plist" :italics-range
;;; (939 . 944))
;;; (:plain-on " that begins with the found entry
;;; (i.e., whose " :plain-range
;;; (944 . 991))
;;; (:xref-on "car" :xref-to "/Body/glo_c.html#car" :xref-range
;;; (991 . 994))
;;; (:plain-on " is the " :plain-range
;;; (994 . 1002))
;;; (:italics-on "indicator" :italics-range
;;; (1002 . 1011))
;;; (:plain-on "). If no such entry is found, the " :plain-range
;;; (1011 . 1045))
;;; (:italics-on "indicator" :italics-range
;;; (1045 . 1054))
;;; (:plain-on ", 
;;; " :plain-range
;;; (1054 . 1057))
;;; (:italics-on "value" :italics-range
;;; (1057 . 1062))
;;; (:plain-on ", and " :plain-range
;;; (1062 . 1068))
;;; (:italics-on "tail" :italics-range
;;; (1068 . 1072))
;;; (:plain-on " are all " :plain-range
;;; (1072 . 1081))
;;; (:xref-on "nil" :xref-to "/Body/any_nil.html#nil" :xref-range
;;; (1081 . 1084))
;;; (:plain-on ".
;;;
;;; " :plain-range
;;; (1084 . 1087))
;;; (:bold-on "Examples:" :bold-range
;;; (1087 . 1096))
;;; (:plain-on "
;;;
;;;  (setq x '()) =>  NIL
;;;  (setq *indicator-list* '(prop1 prop2)) =>  (PROP1 PROP2)
;;;  (getf x 'prop1) =>  NIL
;;;  (setf (getf x 'prop1) 'val1) =>  VAL1
;;;  (eq (getf x 'prop1) 'val1) =>  " :plain-range
;;; (1096 . 1274))
;;; (:xref-on "true" :xref-to "/Body/glo_t.html#true" :xref-range
;;; (1274 . 1278))
;;; (:plain-on "
;;;  (get-properties x *indicator-list*) =>  PROP1, VAL1, (PROP1 VAL1)
;;;  x =>  (PROP1 VAL1)
;;;
;;; " :plain-range
;;; (1278 . 1367))
;;; (:bold-on "Side Effects:" :bold-range
;;; (1367 . 1380))
;;; (:plain-on " None.
;;;
;;; " :plain-range
;;; (1380 . 1388))
;;; (:bold-on "Affected By:" :bold-range
;;; (1388 . 1400))
;;; (:plain-on " None.
;;;
;;; " :plain-range
;;; (1400 . 1408))
;;; (:bold-on "Exceptional Situations:" :bold-range
;;; (1408 . 1431))
;;; (:plain-on " None.
;;;
;;; " :plain-range
;;; (1431 . 1439))
;;; (:bold-on "See Also:" :bold-range
;;; (1439 . 1448))
;;; (:plain-on "
;;;
;;; " :plain-range
;;; (1448 . 1450))
;;; (:xref-on "get" :xref-to "/Body/acc_get.html#get" :xref-range
;;; (1450 . 1453))
;;; (:plain-on ", " :plain-range
;;; (1453 . 1455))
;;; (:xref-on "getf" :xref-to "/Body/acc_getf.html#getf" :xref-range
;;; (1455 . 1459))
;;; (:plain-on "
;;;
;;; " :plain-range
;;; (1459 . 1461))
;;; (:bold-on "Notes:" :bold-range
;;; (1461 . 1467))
;;; (:plain-on " None.
;;;
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; The following " :plain-range
;;; (1467 . 1569))
;;; (:xref-on "X3J13 cleanup issue" :xref-to "/FrontMatter/X3J13-Issues.html" :xref-range
;;; (1569 . 1588))
;;; (:plain-on ", " :plain-range
;;; (1588 . 1590))
;;; (:italics-on "not part of the specification" :italics-range
;;; (1590 . 1619))
;;; (:plain-on ", applies to
;;; this section:
;;;
;;;   • " :plain-range
;;; (1619 . 1651))
;;; (:xref-on "PLIST-DUPLICATES:ALLOW" :xref-to "/Issues/iss269.html" :xref-range
;;; (1651 . 1673))
;;; (:plain-on "
;;;   
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; " :plain-range
;;; (1673 . 1758))
;;; (:xref-on "[Startin" :xref-to "/FrontMatter/Starting-Points.html" :xref-range
;;; (1758 . 1766))
;;; (:xref-on "[Content" :xref-to "/FrontMatter/Chapter-Index.html" :xref-range
;;; (1766 . 1774))
;;; (:xref-on "[Index] " :xref-to "/FrontMatter/Master-Index.html" :xref-range
;;; (1774 . 1782))
;;; (:xref-on "[Symbols" :xref-to "/FrontMatter/Symbol-Index.html" :xref-range
;;; (1782 . 1790))
;;; (:xref-on "[Glossar" :xref-to "/Body/sec_26-1.html" :xref-range
;;; (1790 . 1798))
;;; (:xref-on "[Issues]" :xref-to "/Issues/Issues-Categorized.html" :xref-range
;;; (1798 . 1806))
;;; (:plain-on "
;;; " :plain-range
;;; (1806 . 1807))
;;; (:xref-on "Copyright 1996, The Harlequin Group Limited. All Rights Reserved." 
;;;  :xref-to "/FrontMatter/About-HyperSpec.html#Legal" :xref-range (1807 . 1872))
;;; (:plain-on " " :plain-range
;;; ============================================================

;;; ============================================================
;; :TODO Following will need to be refontified post parse:
;;; :SEE-ALSO `common-lisp-hyperspec-symbol-table' in :FILE hyperspec.el
;;;
;; :MATCH-THESE
;;;
;; "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
;;; :NOTE Following :AT-BOL's clarified in:
;;; :SEE (info "(ansicl)Interpreting Dictionary Entries")
;; :AT-BOL 
;;;^Affected By: 
;;;^Arguments and Values:   ;1.4.4.3
;;;^Class Precedence List:
;;;^Compound Type Specifier Arguments:
;;;^Compound Type Specifier Kind:
;;;^Compound Type Specifier Description:
;;;^Compound Type Specifier Syntax:
;;;^Description:            ;1.4.4.8
;;;^Examples:               ;1.4.4.9
;;;^Exceptional Situations: ;1.4.4.10
;;;^Method Signatures:      ;1.4.4.13
;;;^Notes:                  ;1.4.4.15
;;;^See Also:               ;1.4.4.17
;;;^Side Effects:           ;1.4.4.18
;;;^Supertypes:
;;;^Syntax:                 ;1.4.4.20
;;
;;; :X3J13-CLEANUP-ISSUE occur in :FILE /Issues/*
;; 
;;;^Issue <SOME-ISSUE-WITH-PROPS> Summary
;;;^Issue <SOME-ISSUE> Writeup
;;;^Proposal (<SOME-ISSUE>):
;;;^Issue:
;;;^Reference:
;;;^Category:
;;;^Edit History:
;;;^Status:
;;;^Problem Description:
;;;^Note:
;;;^Editorial Impact:
;;;^Rationale:
;;;^Current Practice:
;;;^Cost to Implementors:
;;;^Cost to Users:
;;;^Performance Impact:
;;;^Aesthetics:
;;;^Discussion:
;;
;;; (p.n-NN)|(p.7-9)|(p.7.2)
;;; AMOP (p.33, p.310)
;;; Section N.N.N.*
;;; Version N, M/DD/YY,
;;; Proposal <SOME-ISSUE> passed N-N, MONTH YEAR
;;;  X3J13 cleanup issue 
;;
;; :DIR-HIEARCHY 
;;; /Body
;;; /glo_[a-z].html#<glos-xref>
;;;
;;;
;; :REPLACE
;;; "[Startin[Content[Index] [Symbols[Glossar[Issues]
;;; Copyright 1996, The Harlequin Group Limited. All Rights Reserved."
;;;"[HARLEQU[Common  [Previou[Up]    [Next]  "
;;;"[HARLEQU[Common  [No Prev[Up]    [No Next"
;;;
;;; ==============================
;; :DPAN2TEXI-OUTPOUT
;;; ^Pronunciation: (appears in dpans2texi output) ;1.4.4.16
;;; ^or→ 
;;; ^See <some-xref>
;;; , see <some-xref>
;;; ============================================================

;;; ==============================
;;; :COURTESY Pascal Bourguignon HIS: `pjb-cl.el' WAS: `loop-doc'
;;; :MODIFICATIONS REPLACED: empty lines with '\n' escaped lisp forms in docstring
;;; :ADDED <Timestamp: Tuesday June 23, 2009 @ 03:22.54 PM - by MON KEY>
(defun mon-help-CL:LOOP (&optional insertp intrp)
  "The Common Lisp `loop' macro.
A CL loop has the form:\n \(loop CLAUSE...)\n
;; :VALID-CLAUSES\n
    for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM
    for VAR in LIST by FUNC
    for VAR on LIST by FUNC
    for VAR = INIT then EXPR
    for VAR across ARRAY
    with VAR = INIT\n
;; :MISCELLANEOUS-CLAUSES
    named NAME
    initially EXPRS...\n
;; :ACCUMULATION-CLAUSES\n
    collect EXPR into VAR
    append EXPR into VAR
    nconc EXPR into VAR
    sum EXPR into VAR
    count EXPR into VAR
    maximize EXPR into VAR
    minimize EXPR into VAR\n
;; :TERMINATION-TEST-CLAUSES\n
    repeat NUM
    while COND
    until COND
    always COND
    never COND
    thereis COND\n
;; :UNCONDITIONAL-EXECUTION-CLAUSE:\n
    do EXPRS...\n
;; :CONDITIONAL-EXECUTION-CLAUSES\n
    if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]\n
;; :MISCELLANEOUS-CLAUSES\n
    finally EXPRS...
    return EXPR
    finally return EXPR\n
; \(loop for i in '\(1 2 3 4\)
;       collect  i into col
;       append   i into app
;       nconc    i into nco
;       sum      i into sum
;       count    i into cnt
;       maximize i into max
;       minimize i into min
;       do \(printf \\\"%d \\\" i\)
;       return \(progn \(printf \\\"\\n\\\" i\)
;                     \(values col app nco sum cnt max min\)\)\)\n
:SEE info node `(cl)Loop Facility'\n
Peter D. Karp's CL Loop Tutorial at:
:SEE (URL `http://www.ai.sri.com/~pkarp/loop.html')\n
Yusuke Shinyama's CL Loop examples:
:SEE (URL `http://www.unixuser.org/~euske/doc/cl/loop.html')\n
Cl-Cookbook review of loop at:
:SEE (URL `http://cl-cookbook.sourceforge.net/loop.html')\n►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-CL:LOOP :insertp t)
  (message "pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (describe-function 'mon-help-CL:LOOP)

;;; ==============================
(defun mon-help-CL:DO (&optional insertp intrp)
"The Common Lisp do loop.\n
;; :CLTL2-SYNTAX
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
:EXAMPLE\n\(let \(k\)
  \(do \(\(i 0 \(+ i 1\)\)
       \(j 0 \(+ 80 j\)\)\)
      \(\(> i 7\) j\)
    \(setq k \(cons j k\)\)\)
  \(nreverse k\)\)
;=>\(0 80 160 240 320 400 480 560\)\n\n
:EXAMPLE (disassembled)
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
;;; :TEST-ME (mon-help-CL:DO)
;;; :TEST-ME (mon-help-CL:DO t)

;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 15, 2009 @ 12:50.16 PM - by MON KEY>
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
;;; :TEST-ME (describe-function 'mon-help-CL:TIME)

;;; ==============================
;; (while (search-forward-regexp 
;;        ;;....1..2.........3.........
;;        "^\\(\\(.*\t\\)\\(.*\\)\\)$")
;;  (replace-match "\\2`\\3'"))
;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 08, 2009 @ 06:11.12 PM - by MON KEY>
(defun mon-help-slime-keys (&optional insertp intrp)
  "Show slime-mode deys and their bindings.\n
;; :SLIME-REPL-MODE-KEYS
:KEY             :BINDING
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
C-x 4 .		`slime-edit-definition-other-window'\n
:SEE-ALSO `slime-cheat-sheet'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-slime-keys :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;;
;;; :TEST-ME (mon-help-slime-keys)
;;; :TEST-ME (mon-help-slime-keys t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-06T05:29:03-04:00Z}#{09367} - by MON KEY>
(defun mon-help-swank-functions (&optional insertp intrp)
 "Functions for working with Slime interface to Common Lisp swank.\n
;; :CL-LISP-SWANK-SIDE
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
;;; :TEST-ME (mon-help-swank-functions)
;;; :TEST-ME (mon-help-swank-functions t)

;;; ==============================
(defun mon-help-CL:LOCAL-TIME (&optional insertp intrp)
"Help function for working with the Common Lisp `LOCAL-TIME' package.\n
LOCAL-TIME:*DEFAULT-TIMEZONE*
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

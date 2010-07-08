;;; mon-doc-help-CL.el --- utils for documenting Common Lisp from within Emacs
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-doc-help-CL.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-07-16T10:56:13
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, docs, help, hypermedia, programming, languages, external

;;; ================================================================

;;; COMMENTARY:

;; ================================================================
;; DESCRIPTION: mon-doc-help-CL provides utils for documenting Common Lisp from
;; within Emacs
;; 
;; FUNCTIONS:►►►
;; `mon-help-CL:LOCAL-TIME', `mon-help-CL:LOOP', `mon-help-CL:TIME',
;; `mon-help-CL-file-dir-functions', `mon-help-CL-minion', `mon-help-CL-symbols', 
;; `mon-help-CL-pkgs', `mon-bind-mon-help-CL-pkgs-loadtime'
;; `mon-help-slime-keys', `mon-help-swank-functions',
;; `mon-help-wget-cl-pkgs', `mon-help-wget-cl-pkgs-TEST',
;; `mon-help-wget-cl-pkgs-for-shell-command',
;; `mon-hspec-plain-p', `mon-hspec-bld-p',
;; `mon-hspec-it-p', `mon-hspec-header-line-p',
;; `mon-hspec-href-p', `mon-w3m-spec-p',
;; `mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-out',
;; `mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
;; `mon-hspec-unparse-w3m-to-buffer',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*cl-cmu-ai-repo*',`*cl-ext-pkg-map*', `*mon-hs-root-dir*',
;; `*mon-hs-parse-buffer*', `*mon-hs-unprs-buffer*', `*clhs-symbol-v3-or-v7*',
;; `*mon-cl-symbols*',
;; 
;; ALIASED/ADVISED/SUBST'D:
;; `mon-help-cl-packages' -> `mon-help-CL-pkgs'
;; `mon-help-cl-symbols'  -> `mon-help-CL-symbols'
;; `mon-hyperspec-lookup' -> `mon-help-CL-symbols'
;; 
;; MOVED:
;; `mon-help-CL-time', `mon-help-CL-loop', `mon-help-slime-keys' <- mon-doc-help-utils.el
;; `mon-help-CL:TIME', `mon-help-CL:LOOP',
;;
;; RENAMED:
;; `mon-help-CL-loop'    -> `mon-help-CL:LOOP'
;; `mon-help-CL-time'    -> `mon-help-CL:TIME' 
;; `mon-help-cl-pkgs'    -> `mon-help-CL-pkgs'
;; `mon-help-cl-symbols' -> `mon-help-CL-symbols'
;; `mon--test--help-wget-cl-pkgs' -> `mon-help-wget-cl-pkgs-TEST'
;; 
;; REQUIRES:
;; The fontlocking and text-property routines of `mon-help-CL-pkgs' need:
;; :FUNCTION `mon-help-propertize-tags', `mon-help-temp-docstring-display'
;; :VARIABLE `*regexp-mon-doc-help-comment-tags*' 
;; :FACE `mon-help-INNER-KEY-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag'
;; :FILE mon-doc-help-utils.el
;; :SEE (URL `http://www.emacswiki.org/emacs-en/ReferenceSheetHelpUtils')
;;
;; OPTIONAL:
;; :FILE hyperspec.el in current distributions of Slime.
;; :SEE (URL `http://common-lisp.net/project/slime/snapshots/slime-current.tgz')
;; :SEE (URL `http://www.cliki.net/SLIME')
;;
;; emacs-w3m
;; :SEE (URL `http://emacs-w3m.namazu.org/')
;; :SEE (URL `http://cvs.namazu.org/emacs-w3m.tar.gz')
;;
;; w3m
;; :SEE (URL `http://w3m.sourceforge.net/index.en.html')
;;
;; TODO:
;;
;; NOTES: 
;;
;; SNIPPETS:
;;
;; THIRD PARTY CODE:
;; :COURTESY Pascal Bourguignon :HIS `pjb-cl.el' 
;; :WAS `loop-doc'->`reference-sheet-help-loop'
;; :SEE (URL `http://www.informatimago.com/develop/emacs/index.html')
;; 
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-CL.el
;; FILE-PUBLISHED: <Timestamp: #{2009-08-25} - by MON KEY>
;; 
;; FILE-CREATED:
;; <Timestamp: Thursday July 16, 2009 @ 10:56.13 AM - by MON KEY>
;;
;; ================================================================

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

;;; CODE:


;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-23T17:47:48-05:00Z}#{09523} - by MON KEY>
(defvar *cl-cmu-ai-repo* nil
  ;; :NOTE use following URL if the URL of binding form below isn't available:
  ;; "http://www-cgi.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/"
  "*Partial path for building a URL string to the relevant Lisp portions of the
Carnegie Mellon Artificial Intelligence Repository:
:SEE (URL `http://www.cs.cmu.edu/Groups/AI/0.html').\n
:SEE-ALSO `*cl-ext-pkg-map*', `mon-help-CL-pkgs', `mon-help-wget-cl-pkgs'.\n►►►")
;;
(unless (bound-and-true-p *cl-cmu-ai-repo*)
  (setq *cl-cmu-ai-repo* "http://www.cs.cmu.edu/Groups/AI/lang/lisp/"))
;;
;;; :TEST-ME *cl-cmu-ai-repo*
;;;(progn (makunbound '*cl-cmu-ai-repo* ) (unintern '*cl-cmu-ai-repo* ) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-23T20:36:35-05:00Z}#{09524} - by MON KEY>
;;(eval-and-compile
(defvar *cl-ext-pkg-map*  nil
  "*List of Common Lisp documentation packages and legacy/historic packages of
use with Emacs.  Each sublist element of has the format:\n
\(SYMBOL DESCRIPTION URL\)\n
:NOTE The variable `*cl-ext-pkg-map-no-pull*' holds a list of the keys which
should not be pulled with `mon-help-wget-cl-pkgs'.\n
:SEE-ALSO `*cl-cmu-ai-repo*', `mon-help-CL-pkgs', `mon-help-wget-cl-pkgs'.\n►►►")
;;
(unless (bound-and-true-p *cl-ext-pkg-map*)
  (setq 
   *cl-ext-pkg-map*
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
        "   :COURTESY Erik Naggum\n"
        "   :SEE-ALSO (URL `http://naggum.no/ANSI-CL)'\n")
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
        "   :COURTESY Yuji `bmonkey' Minejima's\n") ;<ggb01164@nifty.ne.jp>
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
     ;; (cl-hyperspec-v6
     ;;  "Common Lisp Hyperspec v6 of 2004-August-15."
     ;;  "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-6-0.tar.gz")
     (cl-hyperspec-lookup 
      ,(concat
        "Common Lisp interface mapping symbols to URLs of Hspec and CLOS MOP.\n"
        "   :COURTESY Erik Enge, David Lichteblau, Nikodemus Siivola\n"
        "   :SEE-ALSO (URL `http://common-lisp.net/project/hyperspec-lookup/')\n")
      "http://common-lisp.net/cgi-bin/viewcvs.cgi/root.tar.gz")
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
     (cl-quick-reference
      ,(concat
        "Common Lisp Quick Reference provides short descriptions of the ANSI standard.\n"
        "   Includes a comprehensive index of CL symbols. Licensed under GFDL v 1.2.\n"
        "   :COURTESY Bert Burgemeister\n" ;; trebb@users.berlios.de. 
        "   :SEE-ALSO (URL `http://clqr.berlios.de')             <- Project Homepage\n"
        "   :SEE-ALSO (URL `http://repo.or.cz/r/clqr.git')       <- GIT repo\n"
        "   :SEE-ALSO (URL `http://clqr.berlios.de/clqr.tar.gz') <- LaTeX Sources\n")
      "http://clqr.berlios.de/clqr-letter-consec.pdf")
     (l1sp.org
      ,(concat
        "L1sp.org is a redirect service for Common Lisp documentation.\n"
        "   :COURTESY Zach Beane\n"
        "   :SEE-ALSO (URL `http://l1sp.org/mop')   <- Meta Object Protocoln\n"
        "   :SEE-ALSO (URL `http://l1sp.org/cffi')  <- Foreign Function Interface\n"       
        "   :SEE-ALSO (URL `http://l1sp.org/clim')  <- CL Interface Manager\n"
        "   :SEE-ALSO (URL `http://l1sp.org/clx')   <- Common Lisp X Interface\n"
        "   :SEE-ALSO (URL `http://l1sp.org/pcl')   <- Practical Common Lisp\n"
        "   :SEE-ALSO (URL `http://l1sp.org/cl')    <- Common Lisp hspec\n" 
        "   :SEE-ALSO (URL `http://l1sp.org/clisp') <- CLisp \n"
        "   :SEE-ALSO (URL `http://l1sp.org/sbcl')  <- Steel Bank Common Lisp\n"
        "   :SEE-ALSO (URL `http://l1sp.org/ccl')   <- Clozure Common Lisp\n")
      "http://l1sp.org/html/")
     (lispdoc
      ,(concat
        "Search engine for documentation of assorted Common Lisp related material.\n"
        "   Includes Xrefs across the following media, books, packages, libraries etc.:\n"
        "   Common Lisp HyperSpec                                   <- The Acclaimed\n" 
        "   Common Lisp the Language - Second Edition \(CLTL2\)       <- Guy Steele.\n"
        "   On Lisp                                                 <- Paul Graham\n"
        "   Paradigms of Artificial Intelligence Programming (\PAIP\) <- Peter Norvig.\n"
        "   Succesful Lisp                                          <- David Lamkins\n"
        "   Practical Common Lisp \(PCL\)                             <- Peter Seibel.\n"
        "   Steel Bank Common Lisp \(SBCL\)                           <- The Venerable.\n\n"
        "   The following Common Lisp extesion packages:\n\n"
        "   ASDF, CFFI, CFFI-FEATURES, CFFI-SYS, CFFI-UTILS, CHUNGA, CL+SSL, CL+SSL-SYSTEM,\n"
        "   CL-BASE64, CL-PPCRE, CL-PPCRE-TEST, CL-WHO, COMMON-LISP, COMMON-LISP-CONTROLLER,\n"
        "   FLEXI-STREAMS, HUNCHENTOOT, HUNCHENTOOT-ASD, MD5, RFC2388, S-XML,\n"
        "   TRIVIAL-GRAY-STREAMS, URL-REWRITE\n\n"
        "   :COURTESY Bill Moorier\n")
      "http://lispdoc.com/")
     (cl-naggum
      ,(concat
        "Browse the 5,000+ comp.lang.lisp articles by the late Erik Naggum.\n"
        "   :COURTESY Zach Beane\n"
        "   :SEE-ALSO (URL `http://data.xach.com/naggum-articles.tgz')\n"
        "   :SEE-ALSO (URL `http://www.xach.com/naggum/articles/notes.html')\n"
        "   :SEE-ALSO (URL `http://en.wikipedia.org/wiki/Erik_Naggum')\n")
      "http://www.xach.com/naggum/articles/")
     (cl-comp-lang-lisp
      ,(concat
        "Archive of comp.lang.lisp usenet messages through 2009.\n"
        "   :COURTESY Ron Garret and Zach Beane\n"
        "   :NOTE Compressed archive weighs 185MB uncompressed a heavy 700MB!\n"
        "         There is a not insignificant amount of spam to filter out.")
      "http://data.xach.com/cll.txt.gz")
     (clbuild
      ,(concat
        "clbuild is a shell script to install, build, invoke Common Lisp applications.\n"
        "   :COURTESY David' Lichteblau\n"
        "   :SEE-ALSO (URL `http://common-lisp.net/project/clbuild/clbuild/')\n")
      "http://common-lisp.net/project/clbuild/clbuild/doc/")
     (lispy
      ,(concat
        "Lispy is a library manager for Common Lisp, written in Common Lisp.\n"
        "   :COURTESY Matthew Kennedy\n"
        "   :SEE-ALSO (URL `http://common-lisp.net/project/lispy/repository/map.lisp-expr')\n"
        "   :SEE-ALSO (URL `http://common-lisp.net/project/lispy/sc1.html')\n"
        "   :SEE-ALSO (URL `http://common-lisp.net/pipermail/lispy-devel/'\n")
      "http://common-lisp.net/project/lispy/")
     (libcl
      ,(concat 
        "LibCL is a self-contained collection of portable, free CL libraries.\n"
        "   :COURTESY Daniel Herring\n"
        "   :SEE-ALSO (URL `http://libcl.com/')\n"
        "   :SEE-ALSO (URL `http://libcl.com/libcl-current/index.html')\n"
        "   :SEE-ALSO (URL `http://git.libcl.com/')\n"
        "   :SEE-ALSO (URL `http://dir.gmane.org/gmane.lisp.libcl.devel')\n"
        "   :SEE-ALSO (URL `http://dir.gmane.org/gmane.lisp.libcl.user')\n")
      ;; :NOTE Current Beta release as of 2010-02-13. Bleeders use the git.
      "http://libcl.com/libcl-2009-10-27-beta.tar.bz2")
     (cl-slime 
      ,(concat 
        "Current distribution of Slime from CVS.\n"
        "   :SEE-ALSO (URL `http://www.cliki.net/SLIME')\n")
      "http://common-lisp.net/project/slime/snapshots/slime-current.tgz")
     (ilisp
      ,(concat
        "Ilisp source from git with updates for current Emacsen.\n"
        "   :COURTESY Barak A. Pearlmutter's repo maintainance.\n") ;<barak@cs.nuim.ie>
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
      "ftp://ftp.sra.co.jp/pub/lang/lisp/akcl/akcl-1-615.tar.gz"))))
;;
;;; :TEST-ME *cl-ext-pkg-map*
;;;(progn (makunbound '*cl-ext-pkg-map*) (unintern '*cl-ext-pkg-map*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-13T11:24:58-05:00Z}#{10066} - by MON KEY>
(defvar *cl-ext-pkg-map-no-pull* nil
  "*List of keys in var `*cl-ext-pkg-map*' which do not point to file archives.\n
Elements of this list point to webpages, webpaths, and version controlled source
archives. These include:\n
 lispy clbuild cl-naggum lispdoc l1sp.org\n cl-cookbook X3J13-archive ilisp\n
These _are_ diplayed in documentation of `mon-help-CL-pkgs' but should not be
pulled with `mon-help-wget-cl-pkgs'.
:SEE-ALSO `*cl-cmu-ai-repo*'.\n►►►")
;;
(unless (bound-and-true-p *cl-ext-pkg-map-no-pull*)
  (setq *cl-ext-pkg-map-no-pull* 
        '(lispy clbuild cl-naggum lispdoc l1sp.org cl-cookbook X3J13-archive ilisp)))
;;
;;; :TEST-ME *cl-ext-pkg-map-no-pull**
;;;(progn (makunbound '*cl-ext-pkg-map-no-pull*) (unintern '*cl-ext-pkg-map-no-pull*) )
  
;;; ==============================
;;; :REQUIRES
;;; :FUNCTION `mon-help-propertize-tags', `mon-help-temp-docstring-display'
;;; :VARIABLE `*regexp-mon-doc-help-comment-tags*' 
;;; :FACE `mon-help-INNER-KEY-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag'
;;; :SEE :FILE mon-doc-help-utils.el 
;;; :CREATED <Timestamp: #{2009-12-23T20:41:48-05:00Z}#{09524} - by MON KEY>
(defun mon-help-CL-pkgs (&optional insrtp intrp)
  (interactive "i\np")
  (let ((divd (make-string 79 95))
        (dspbf (get-buffer-create (upcase (symbol-name '*cl-ext-pkg-map*))))
        prtty)
    (setq prtty
          (concat 
           (mapconcat #'(lambda (cl) 
                          (format "%s\n\n:%s\n - %s\n:SEE (URL `%s')\n"
                                 divd
                                 (upcase (format "%s" (car cl))) 
                                 (cadr cl) 
                                 (let* ((pth-p (caddr cl))
                                        (tst-url (substring pth-p 0 3)))
                                   (if (member tst-url '("htt" "ftp"))
                                       pth-p
                                       (concat *cl-cmu-ai-repo* pth-p)))))
                     *cl-ext-pkg-map* "\n")
           "\n" divd))
    ;(when (or intrp insrtp)
    (setq prtty (with-temp-buffer 
                  (insert prtty)
                  (mon-help-propertize-tags 
                   '(*regexp-mon-doc-help-comment-tags*  0 mon-help-INNER-KEY-tag)
                   '("^_\\{79\\}$" 0 mon-help-PNTR-tag)
                   '("^\\:[A-Z0-9-]+$" 0 mon-help-DYNATAB-tag))
                  ;; Don't zap tp's with `buffer-substring-no-properties'
                  (buffer-substring (buffer-end 0)(buffer-end 1)))) ;)
    (cond (intrp (mon-help-temp-docstring-display prtty dspbf))
          (insrtp (save-excursion  
                    (newline)
                    (princ prtty (current-buffer))))
          (t (prin1 prtty)))))
;;
(defalias 'mon-help-cl-packages 'mon-help-CL-pkgs)
;;
;;; :TEST-ME (mon-help-CL-pkgs)
;;; :TEST-ME (mon-help-CL-pkgs nil t)

;;; ==============================
;;; :CHANGESET 1946
;;; :CREATED <Timestamp: #{2010-07-07T13:56:57-04:00Z}#{10273} - by MON KEY>
(defun mon-bind-mon-help-CL-pkgs-loadtime (&optional w-msg-user)
  "Build the propertiezed documentation for `mon-help-CL-pkgs' at loadtime.\n
When optional arg W-MSG-USER is non-nil and/or if If `documentation-property' of
mon-help-CL-pkgs is null inform that the 'function-documentation property was
\(re\)bound.\n
:EXAMPLE\n
\(progn \(plist-put \(symbol-plist 'mon-help-CL-pkgs\) 'function-documentation  nil\)
      \(unless \(documentation-property 'mon-help-CL-pkgs 'function-documentation\)
        \(mon-bind-mon-help-CL-pkgs-loadtime\)\)\)
\(mon-help-CL-pkgs nil t\)\n
Idiomatic loadtime evaulation of this fncn has the following form:\n
 \(eval-after-load \"mon-doc-help-CL\" '\(mon-bind-mon-help-CL-pkgs-loadtime\)\)\n
:SEE-ALSO `*cl-ext-pkg-map*', `mon-help-utils-CL-loadtime',
`mon-run-post-load-hooks', `mon-after-mon-utils-loadtime',
`mon-check-feature-for-loadtime', `mon-CL-cln-colon-swap'.\n►►►"
  (setq w-msg-user (or w-msg-user 
                       (null (documentation-property 
                              'mon-help-CL-pkgs 'function-documentation))))
  (put 'mon-help-CL-pkgs 'function-documentation 
       (concat     
        "A formatted list of historic and Common Lisp packages and Specs.\n\n"
        "Following enumerates URLs, locations, and brief descriptions of various Common\n"
        "Lisp libraries, extensions, distributions, packages, documentation search\n"
        "engines, etc.\n\n"
        (mon-help-CL-pkgs)
        "\nThe return value of this function can be inserted to a dedicated buffer by\n"
        "evaluating with the optional args INTRP non-nil.\n"
        ":EXAMPLE\n\(mon-help-CL-pkgs nil t\)\n\n"
        ":SEE-ALSO `*cl-cmu-ai-repo*', `*cl-ext-pkg-map*', `*cl-ext-pkg-map-no-pull*',\n"
        "`mon-help-wget-cl-pkgs', `mon-CL-package-complete', `quicklisp-system-complete'"
        ".\n►►►"))
  (when w-msg-user 
    (message 
     (concat ":FUNCTION `mon-bind-mon-help-CL-pkgs-loadtime' "
             "-- evaluated to generate docstring for `mon-help-CL-pkgs'"))))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;|(progn (plist-put (symbol-plist 'mon-help-CL-pkgs) 'function-documentation  nil)
;;|       (unless (documentation-property 'mon-help-CL-pkgs 'function-documentation)
;;|         (mon-bind-mon-help-CL-pkgs-loadtime)))
;;`----

;;; ==============================
;;; :RENAMED `mon--test--help-wget-cl-pkgs' -> `mon-help-wget-cl-pkgs-TEST'
;;; :CREATED <Timestamp: #{2009-12-24T01:10:48-05:00Z}#{09524} - by MON>
(defun mon-help-wget-cl-pkgs (&optional cl-wget-fname)
  "Write a wget script to file CL-WGET-FNAME return contents of CL-WGET-FNAME.
Used with wget to snarf files with URL's returned per `mon-help-CL-pkgs'.\n
When CL-WGET-FNAME is non-nil it should be a full pathname which current user
with `user-login-name' has sufficient permissions to writing-to/executing-from.\n
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
:EXAMPLE\n\(mon-help-wget-cl-pkgs-TEST\) ;<- helper test function.\n
:NOTE This function does not write the URLs for *cl-ext-pkg-map* keys
cl-cookbook, oaklisp, ilisp, lisp, lispdoc. These point to web pages and are not
files.  Download or add these manually if that is what you want. The following
loop returns the absent URL's:\n
\(let \(manual-dl\)
  \(dolist \(cl-wpgs '\(ilisp cl-cookbook oaklisp lispdoc lispy\) manual-dl\)
    \(setq manual-dl
          \(concat \(caddr \(assoc cl-wpgs *cl-ext-pkg-map*\)\) \"\\n\" manual-dl\)\)\)\)\n
:SEE-ALSO `mon-help-wget-cl-pkgs-TEST', `mon-help-CL-pkgs', `*cl-cmu-ai-repo*',
`*cl-ext-pkg-map*', `mon-wget-list-give-script-to-shell-command',
`mon-wget-list-to-script', `mon-wget-list-to-script-TEST',
`mon-wget-list-to-script-shell-command', `mon-wget-mon-pkgs',
`mon-wget-rfc'.\n►►►"
  (let ((rmv-wb-pgs (mapcar #'car *cl-ext-pkg-map*))
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
          '(lispy clbuild cl-naggum lispdoc l1sp.org 
            cl-cookbook X3J13-archive ilisp))
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
;;; :TEST-ME (mon-help-wget-cl-pkgs-TEST)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-24T14:27:52-05:00Z}#{09524} - by MON KEY>
(defun mon-help-wget-cl-pkgs-TEST ()
  "Helper function to verify `mon-help-wget-cl-pkgs' is functioning as expected.\n
Performs the following checks:\n
o Writes a temp file with output from mon-help-wget-cl-pkgs
o Return inserted contents of temp file in a temporary buffer
o Display that buffer with `file-attributes' in header
o Kills temp-buffer and file on exit\n
:EXAMPLE\n\nmon-help-wget-cl-pkgs-TEST\n
:NOTE On exit this function should cleanup the temp file/buffer objects below:\n
 o A temp file written to:
   /PATH/TO/`default-directory'/tmp-wget-YY-MM-DD\n
 o A temp-buffer with the name *SHOW-WGET-TEMP*.\n
:SEE-ALSO `mon-help-CL-pkgs',`*cl-cmu-ai-repo*',`*cl-ext-pkg-map*'.\n►►►"
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
;;; :TEST-ME (mon-help-wget-cl-pkgs-TEST)

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
`mon-help-wget-cl-pkgs-TEST'.\n►►►"
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
;;; (mon-help-wget-cl-pkgs-for-shell-command "wget-script-2010-02-18")

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
;;; :SEE (URL `http://web.archive.org/web/20071031055357/http://wiki.alu.org/Project_FreeSpec')
;;;
;;; And,
;;; :SEE (URL `http://lists.nongnu.org/archive/html/axiom-developer/2007-06/msg00456.html')
;;;
;;; (concat "http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/5f105c469acd00fe"
;;; "/696699b7e471b169?lnk=gst&q=5828f58fa34e2ce8#696699b7e471b169")
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
  ;;(when (locate-library "hyperspec.el") ;<- `*mon-hs-root-dir*'
  (require 'hyperspec nil t))

;;; :NOTE The symbol `common-lisp-hyperspec-root' needs to be present.
;;; We shouldn't bind it if user hasn't loaded slime/hyperspec but _will_ later.
(unless (or (bound-and-true-p common-lisp-hyperspec-root)
            (featurep 'hyperspec))
  (setq common-lisp-hyperspec-root nil))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:27-05:00Z}#{09527} - by MON>
(defvar *mon-hs-root-dir* nil
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
;
(unless (bound-and-true-p *mon-hs-root-dir*)
  (setq *mon-hs-root-dir* common-lisp-hyperspec-root))
;;
;;; :TEST-ME *mon-hs-root-dir*
;;;(progn (makunbound '*mon-hs-root-dir*) (unintern '*mon-hs-root-dir*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:29-05:00Z}#{09527} - by MON>
(defvar *mon-hs-parse-buffer* nil
  "*Name of temporary buffer holding the parsed text properties of the current
  w3m text under examination.\n
:SEE-ALSO `*mon-hs-unprs-buffer*', `*mon-hs-root-dir*',`mon-hspec-plain-p',
`mon-hspec-bld-p', `mon-hspec-it-p', `mon-hspec-header-line-p',
`mon-hspec-href-p', `mon-w3m-spec-p',`mon-hspec-prop-type', 
`mon-hspec-stk-n-mv', `mon-hspec-out', `mon-hspec-parse-w3m',
`mon-hspec-unparse-w3m',`mon-hspec-unparse-w3m-to-buffer'.\n►►►")
;;
(unless (bound-and-true-p *mon-hs-parse-buffer*)
  (setq *mon-hs-parse-buffer* "*CL-HSPEC-CONV*"))
;;
;;; :TEST-ME *mon-hs-parse-buffer*
;;;(progn (makunbound '*mon-hs-parse-buffer*) (unintern '*mon-hs-parse-buffer*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-29T12:08:40-05:00Z}#{09532} - by MON KEY>
(defvar *mon-hs-unprs-buffer* nil
  "*Name of temporary buffer holding the round tripped unparsed text properties
  in return value generated with `mon-hspec-parse-w3m'.\n
:CALLED-BY `mon-hspec-unparse-w3m'\n
:SEE-ALSO `*mon-hs-root-dir*', `*mon-hs-parse-buffer*'
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-it-p', `mon-hspec-header-line-p',`mon-hspec-href-p',
`mon-w3m-spec-p',`mon-hspec-prop-type', `mon-hspec-stk-n-mv',
`mon-hspec-out'.\n►►►")
;;
(unless (bound-and-true-p *mon-hs-unprs-buffer*)
  (setq *mon-hs-unprs-buffer* "*CL-HSPEC-UNPARSE*"))
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
  "Are we looking at plain-text sans other w3m text-properties we are parsing.\n
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
  "Helper function for `mon-hspec-prop-type'.\n
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
:CALLED-BY `mon-hspec-parse-w3m' ;<- wrapper interface.\n
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
  "Parse the Hyperlink Specification in current w3m buffer.\n
Invoke `mon-hspec-stk-n-mv' repeatedly on the contents of current *w3m* buffer
until `eobp' or there are no more text-properties left to parse.\n
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
;;; HEY. We just cicum-invented ten's complement!
;;; :SEE (URL `http://en.wikipedia.org/wiki/Method_of_complements')
;;;
;;; :NOTE We currently shorten paths using a regexp lookup against a local path
;;; however, the code can be easily adapted for parsing and shortening remote
;;; paths wth "http://" in lieu of local paths with "file://"
;;; :SEE `mon-hspec-header-line-p's regexp for local variable `rplc-pth'
;;;
;;; :CREATED <Timestamp: #{2009-12-29T02:38:01-05:00Z}#{09532} - by MON>
(defun mon-hspec-unparse-w3m (parse-w3m-buffer-or-file) ; return-parse-in-buffer
  "Round trip snarfed HTML lisp data output of `mon-hspec-parse-w3m' to plain text.\n
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
  "Insert car and cdr of return value from `mon-hspec-unparse-w3m' in buffer.\n
Default is to insert to the buffer named by the variabl `*mon-hs-unprs-buffer*'.\n
When RETURN-PARSE-IN-BUFFER is non-nil create the buffer if it does not exist
and insert in that buffer.
:NOTE Unless RETURN-PARSE-IN-BUFFER is current-buffer this procedure erases the
contents of buffer `*mon-hs-unprs-buffer*' or RETURN-PARSE-IN-BUFFER prior to
insertion.\n
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
;;; :CHANGESET 1926
;;; :CREATED <Timestamp: #{2010-06-29T12:49:56-04:00Z}#{10262} - by MON KEY>
(defun mon-help-CL-file-dir-functions (&optional insertp intrp)
  "List of functions for working with CL's pathnames.\n
;; :CL-PATHNAME-FILE-HANDLERS
`load'
`open'
`compile-file'
`delete-file'
`rename-file'
`with-open-file'\n
;; :CL-PATHNAME-FILE-ACCESSORS
`probe-file'
`file-length'
`file-position'
`file-string-length'
`file-write-date'
`file-author'
`file-error-pathname'\n
;; :CL-PATHNAME-NAMESTRINGS
`namestring'
`directory-namestring'
`enough-namestring'
`file-namestring'
`host-namestring'
`parse-namestring'\n
;; :CL-PATHNAME-ACCESSORS
`pathnamep'
`pathname-name'
`pathname-type'         
`pathname-version'      
`pathname-device'
`pathname-directory'
`pathname-host'
`user-homedir-pathname'
`*default-pathname-defaults*' <SPECIAL-VARIABLE>\n
;; :CL-PATHNAME-HANDLERS
`pathname'
`make-pathname'
`merge-pathnames'
`directory'
`ensure-directories-exist'
`#P'                       <READER-MACRO>\n
;; :CL-PATHNAME-WILD
`truename'
`pathname-match-p'
`wild-pathname-p'
`translate-pathname'\n
;; :CL-PATHNAME-LOGICAL
`logical-pathname'
`translate-logical-pathname'
`logical-pathname-translations'\n
;; :ASDF-PATHNAMES-EXTERNAL        ;; :SEE :FILEsbcl/contrib/asdf/asdf.lisp
`asdf:determine-system-pathname'
`asdf:component-pathname'
`asdf:relative-pathname'\n
;; :ASDF-PATHNAMES-INTERNAL
`asdf::hidden-file-p'
`asdf::user-homedir'
`asdf::lispize-pathname'
`asdf::wilden'
`asdf::*wild-path*'           <PARAMETER>
`asdf::default-directory'
`asdf::split-name-type'
`asdf::component-name-to-pathname-components'
`asdf::ensure-directory-pathname'
`asdf::pathname-root'
`asdf::delete-file-if-exists'
`asdf::pathname-directory-pathname'\n
;; :SB-POSIX-PATHNAMES    ;; :SEE :FILE sbcl/contrib/sb-posix/interface.lisp
`sb-posix:chdir'
`sb-posix:getcwd'
`sb-posix:mkdir'
`sb-posix:rmdir'
`sb-posix:stat'
`sb-posix:filename'
`sb-posix:file-descriptor'
`sb-posix:rename'
`sb-posix:symlink'
`sb-posix:link'
`sb-posix:readlink'\n
;; :CL-FAD-PATHNAMES
`cl-fad:copy-file'
`cl-fad:copy-stream'
`cl-fad:delete-directory-and-files'
`cl-fad:directory-exists-p'
`cl-fad:directory-pathname-p'
`cl-fad:file-exists-p'
`cl-fad:list-directory'
`cl-fad:pathname-as-directory'
`cl-fad:pathname-as-file'
`cl-fad:walk-directory'
`cl-fad::directory-wildcard'\n
:SEE-ALSO `mon-help-CL:LOCAL-TIME', `mon-help-CL:LOOP', `mon-help-CL:TIME'
`mon-help-CL-symbols', `mon-help-slime-keys', `mon-help-swank-functions'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-file-dir-function :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-CL-file-dir-function)
;;; :TEST-ME (mon-help-CL-file-dir-function t)
;;; :TEST-ME (describe-function 'mon-help-CL-file-dir-function)
;;; :TEST-ME (apply 'mon-help-CL-file-dir-function nil '(t))

;;; ==============================
;;; :COURTESY Pascal Bourguignon HIS: `pjb-cl.el' WAS: `loop-doc'
;;; :MODIFICATIONS REPLACED: empty lines with '\n' escaped lisp forms in docstring
;;; :ADDED <Timestamp: Tuesday June 23, 2009 @ 03:22.54 PM - by MON KEY>
(defun mon-help-CL:LOOP (&optional insertp intrp)
  "The Common Lisp `loop' macro.
A CL loop has the form:\n \(loop CLAUSE...)\n
;; :LOOP-VALID-CLAUSES\n
    for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM
    for VAR in LIST by FUNC
    for VAR on LIST by FUNC
    for VAR = INIT then EXPR
    for VAR across ARRAY
    with VAR = INIT\n
;; :LOOP-MISCELLANEOUS-CLAUSES
    named NAME
    initially EXPRS...\n
;; :LOOP-ACCUMULATION-CLAUSES\n
    collect EXPR into VAR
    append EXPR into VAR
    nconc EXPR into VAR
    sum EXPR into VAR
    count EXPR into VAR
    maximize EXPR into VAR
    minimize EXPR into VAR\n
;; :LOOP-TERMINATION-TEST-CLAUSES\n
    repeat NUM
    while COND
    until COND
    always COND
    never COND
    thereis COND\n
;; :LOOP-UNCONDITIONAL-EXECUTION-CLAUSE:\n
    do EXPRS...\n
;; :LOOP-CONDITIONAL-EXECUTION-CLAUSES\n
    if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]\n
;; :LOOP-FINALLY/RETURN-CLAUSES\n
    finally EXPRS...
    return EXPR
    finally return EXPR\n
;; :LOOP-IDIOMS
 \(loop for i in '\(1 2 3 4\)
       collect  i into col
       append   i into app
       nconc    i into nco
       sum      i into sum
       count    i into cnt
       maximize i into max
       minimize i into min
       do \(printf \\\"%d \\\" i\)   ; :NOTE Elisp!
       return \(progn \(printf \\\"\\n\\\" i\)
                     \(values col app nco sum cnt max min\)\)\)\n

I need to iterate a lis some elements will be dropped
others will stay. What is an elegant form using `loop'?:

 \(loop for element 
        in list 
        when \(test element\) 
        collect element\)

:SEE info node `(cl)Loop Facility'\n
Peter D. Karp's CL Loop Tutorial at:
:SEE (URL `http://www.ai.sri.com/~pkarp/loop.html')\n
Yusuke Shinyama's CL Loop examples:
:SEE (URL `http://www.unixuser.org/~euske/doc/cl/loop.html')\n
Cl-Cookbook review of loop at:
:SEE (URL `http://cl-cookbook.sourceforge.net/loop.html')
:SEE-ALSO `mon-help-CL:TIME', `mon-help-CL:DO'.\n►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-CL:LOOP :insertp t)
  (message "pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (describe-function 'mon-help-CL:LOOP)

;;; ==============================
(defun mon-help-CL:DO (&optional insertp intrp)
"The Common Lisp do loop.\n
;; :DO-CLTL2-SYNTAX
\(do \(\({var | \(var [init [step]]\)}*\)\)
    \(end-test {result}*\)
 body-form\)\n
;; :DOO-EMACS-LISP-SYNTAX
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
\(nreverse k))\n
:SEE-ALSO `mon-help-CL:LOOP', `mon-help-CL:TIME'.\n►►►"
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
"CL: `GET-DECODED-TIME'\n 
Return nine values specifying the current time as follows:
second, minute, hour, date, month, year, day of week \(0 = Monday\), T
\(daylight savings times\) or NIL \(standard time\), and timezone.
\(get-decoded-time\) =>
14     ;second\n44     ;minute\n12     ;hour\n15     ;date\n7      ;month
2009   ;year\n2      ;day\nT      ;dayligt-p\n5      ;zone
:SEE `SB-POSIX:TIME'\n
:SEE-ALSO `mon-help-CL:LOOP', `mon-help-CL:DO',
`mon-help-CL-file-dir-functions', `mon-help-CL-symbols',
`mon-help-slime-keys'.\n►►►"
(interactive "i\nP")
(if (or insertp intrp)
    (mon-help-function-spit-doc 'mon-help-CL:TIME :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;;
;;; :TEST-ME (describe-function 'mon-help-CL:TIME)

;;; :FILE slime.el
;;; `when-let'
;;; `destructure-case'
;;; `with-struct'
;;; `slime-curry'
;;; `slime-rcurry'

;;; ==============================
;; (while (search-forward-regexp 
;;        ;;....1..2.........3.........
;;        "^\\(\\(.*\t\\)\\(.*\\)\\)$")
;;  (replace-match "\\2`\\3'"))
;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 08, 2009 @ 06:11.12 PM - by MON KEY>
(defun mon-help-slime-keys (&optional insertp intrp)
  "Help for `slime-mode' keybindings prefix keymaps and the related vars.\n
;; :SLIME-KEYMAPS
`slime-parent-map'
`slime-parent-bindings'
`slime-prefix-map'
`slime-who-map'\n
;; :SLIME-KEYBINDINGS
`slime-prefix-bindings'
`slime-editing-keys'
`slime-keys'
`slime-doc-bindings'
`slime-who-bindings'\n
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
:SEE :FILE `mon-keybindings.el'\n
:SEE-ALSO `slime-cheat-sheet', `mon-help-CL-file-dir-functions',
`mon-help-CL-symbols', `mon-help-swank-functions'.\n►►►"
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
`swank:describe-symbol-for-emacs'\n
:EXAMPLE\n\n\(slime-compute-connection-state 'slime-current-connection\)\n
\(swank:connection-info\)\n
\(swank:list-all-package-names\)\n
:SEE-ALSO `mon-help-slime-keys', `mon-keybind-slime', `mon-slime-setup-init', 
`mon-help-CL-file-dir-functions', `mon-help-CL-symbols'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-swank-functions :insertp t)
    (message "pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-swank-functions)
;;; :TEST-ME (mon-help-swank-functions t)

;;; ==============================
;;; :CHANGESET 1928 <Timestamp: #{2010-06-29T13:20:07-04:00Z}#{10262} - by MON KEY>
(defun mon-help-CL:LOCAL-TIME (&optional insertp intrp)
  "Common Lisp  package `LOCAL-TIME' functions, macros, generics, variables.\n
;; :LOCAL-TIME-MACROS 
`local-time:define-timezone'
Define zone-name \(a symbol or a string\) as a new timezone, lazy-loaded from
zone-file \(a pathname designator relative to the zoneinfo directory on this
system.  If load is true, load immediately.\n
`local-time:with-decoded-timestamp'
This macro binds variables to the decoded elements of TIMESTAMP.
The TIMEZONE argument is used for decoding the timestamp, and is not bound by
the macro.  The value of DAY-OF-WEEK starts from 0 which means Sunday.\n
`local-time:make-timestamp'
`local-time:adjust-timestamp'
`local-time:adjust-timestamp!'\n
;; :LOCAL-TIME-FUNCTIONS-GENERIC-FUNCTION
`local-time:nsec-of'
`local-time:sec-of'
`local-time:day-of'\n
;; :LOCAL-TIME-FUNCTIONS
`local-time:astronomical-julian-date'
Return astronomical julian date referred to by the timestamp.\n
`local-time:days-in-month'
Return number of days in the given month of the specified year.\n
`local-time:decode-timestamp'
Return decoded time as multiple values: nsec, ss, mm, hh, day, month, year,
day-of-week.\n
`local-time:enable-read-macros'
Enable the local-time reader macros for literal timestamps and universal time.\n
`local-time:encode-timestamp'
Return a new TIMESTAMP instance corresponding to the specified time elements.\n
`local-time:format-rfc3339-timestring'
Format a timestring in the RFC 3339 format, a restricted form of the ISO-8601
timestring specification for Internet timestamps.\n
`local-time:format-timestring'
Return a string representation of TIMESTAMP according to FORMAT.
If destination is T, string is written to  *standard-output*.
If destination is a stream, string is written to the stream.\n
`local-time:modified-julian-date'
Return modified Julian date referred to by the timestamp.\n
`local-time:now'
Return a timestamp representing the present moment.\n
`local-time:parse-timestring'
Parse a timestring and return the corresponding TIMESTAMP.
Unspecified fields in the timestring are initialized to their lowest possible
value, and timezone offset is 0 \(UTC\) unless explicitly specified in the input
string.\n
`local-time:timestamp-century'
Return ordinal century upon which the timestamp falls.\n
`local-time:timestamp-day'
Return day of the month upon which the timestamp falls.\n
`local-time:timestamp-decade'
Return cardinal decade upon which the timestamp falls.\n
`local-time:timestamp-difference'
Return difference between TIME-A and TIME-B in seconds\n
`local-time:timestamp-maximum'
Return latest timestamp.\n
`local-time:timestamp-millennium'
Return ordinal millennium upon which the timestamp falls.\n
`local-time:timestamp-minimum'
Return earliest timestamp.\n
`local-time:timestamp-month'
Return month upon which the timestamp falls.\n
`local-time:timestamp-subtimezone'
Return as multiple values the time zone as the number of seconds east of UTC, a
boolean daylight-saving-p, and the customary abbreviation of the timezone.\n
`local-time:timestamp-to-universal'
Return UNIVERSAL-TIME corresponding to the TIMESTAMP.\n
`local-time:timestamp-to-unix'
Return Unix time corresponding to the TIMESTAMP.\n
`local-time:timestamp-whole-year-difference'
Return number of whole years elapsed between time-a and time-b.
:NOTE Useful for use with anniversaries, birthdays, etc.\n
`local-time:timestamp-year'
Return cardinal year upon which the timestamp falls.\n
`local-time:today'
Return a timestamp representing the present day.\n
`local-time:universal-to-timestamp'
Return a timestamp corresponding to the given universal time.\n
`local-time:unix-to-timestamp'
Return a TIMESTAMP corresponding to UNIX, which is the number of seconds since
the Unix epoch, 1970-01-01T00:00:00Z.\n
;; :LOCAL-TIME-FUNCTIONS-UNDOCUMENTED
`local-time:find-timezone-by-location-name'
`local-time:format-http-timestring'
`local-time:parse-rfc3339-timestring'
`local-time:timestamp+'
`local-time:timestamp-'
`local-time:timestamp-day-of-week'
`local-time:timestamp-hour'
`local-time:timestamp-maximize-part'
`local-time:timestamp-microsecond'
`local-time:timestamp-millisecond'
`local-time:timestamp-minimize-part'
`local-time:timestamp-minute'
`local-time:timestamp-second'
`local-time:timestamp/='
`local-time:timestamp<'
`local-time:timestamp<='
`local-time:timestamp='
`local-time:timestamp>'
`local-time:timestamp>='
`local-time:to-http-timestring'\n
;; :LOCCAL-TIME-VARIABLES
`local-time:timestamp'                   <TYPE>
`local-time:*default-timezone*'
`local-time:+asctime-format+'
`local-time:+day-names+'
`local-time:+days-per-week+'
`local-time:+gmt-zone+'
`local-time:+hours-per-day+'
`local-time:+iso-8601-format+'
`local-time:+minutes-per-day+'
`local-time:+minutes-per-hour+'
`local-time:+month-names+'
`local-time:+rfc3339-format+'
`local-time:+rfc3339-format/date-only+'
`local-time:+seconds-per-day+'
`local-time:+seconds-per-hour+'
`local-time:+seconds-per-minute+'
`local-time:+short-day-names+'
`local-time:+short-month-names+'
`local-time:+utc-zone+'\n
`local-time:+rfc-1123-format+'
RFC 1123 timestring format.
:NOTE Use the +GMT-ZONE+ timezone to format a proper RFC 1123 timestring.
:SEE RFC-1123 for the details about the possible values of the timezone field.\n
:SEE (URL `http://tools.ietf.org/rfc/rfc1123.txt')\n
\(with-current-buffer \(get-buffer-create \"*RFC-1123*\"\)
   \(url-insert-file-contents  \"http://tools.ietf.org/rfc/rfc1123.txt\"\)
   \(display-buffer \(current-buffer\) t\)\)\n
:SEE info node `(coreutils)Date input formats'\n\n
:SEE `SB-POSIX:TIME'\n
:SEE-ALSO `mon-help-iso-8601', `mon-help-CL:DO', `mon-help-CL:LOOP',
`mon-help-CL-file-dir-functions', `mon-help-CL-symbols',
`mon-help-slime-keys'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL:LOCAL-TIME :insertp t)
    (message "pass non-nil for optional arg INTRP")))

;;; ==============================
;;; :CHANGESET 1942
;;; :CREATED <Timestamp: #{2010-07-07T13:13:58-04:00Z}#{10273} - by MON KEY>
(defun mon-help-CL-minion (&optional insertp intrp)
  "Notes and usage idioms for #lisp's electronically composed helper `minion'.\n
;; :MINION-TERM-LOOKUP
To have minon look up a term in the internal database and/or try to retrieve the
first sentence of a similiarly name pageon CLiki do:
  minion: term?\n
;; :MINION-HELP-OTHERS
To have minion tell another user about something do:
   minion: show <NICK> <SOMETHING>
   minion: tell <NICK> about <TOPIC/TERM/SYMBOL/PKG>
   minion: please tell <USER> about <TOPIC/TERM/SYMBOL/PKG>\n
:NOTE Minion will respond to many differently pharsed queries and can (within
reason) show pretty much anything to another user.\n
;; :MINION-TERM-ADD
To have minon remember a term do: 
  minion: add \"<TERM>\" as: <TERM-DEFINITION>\n
;; :MINION-TERM-ALIAS
To have minion remember that a term is an alias for another term do:
  minion: alias \"<TERM>\" as: <SOME-OTHER-TERM>\n
;; :MINION-FORGET
To have minion forget a term or nickname do:
  minion: forget <TERM>|<NICK>\n
;; :MINION-MEMO
To record a memo for minon to give to some nick when next they speak do:
  minion: memo for <NICK>: <MEMO-CONTENTS>\n
;; :MINION-NICKNAMES
If you have multiple nicknames and want to get your memos at any of them do:
  minion: <NICK1> is another nick for <NICK2>\n
If you decide to give up a nick to have minion forget it do:
  minion: forget <NICK2>\n
;; :MINION-MEMO-AVOIDANCE
To flush all your memos without delivery do:
  minion: discard my memos\n
To flush only memos from a specific person do: 
  minion: discard my memos from <NICK>\n
;; :MINION-ADVICE
To get advice from minion: 
  minion: advice #<INTEGER>\n
;; :MINION-APROPOS
To search for all small definitions containing 'foo':
  minion: apropos <FOO>\n
;; :MINION-ACRONYMS
See an acronym you don't recognize? To find out what it means do:
  minion: what does <ACRONYM> stand for?\n
;; :MINION-HELP
/msg minion help\n
/msg minion help <TOPIC>\n
<TOPIC> := \"acronyms\" \"adding terms\" \"advice\" \"aliasing terms\" \"apropos\"
	   | \"avoiding memos\" \"eliza\" \"forgetting\" \"goodies\" \"helping others\"
	   | \"lookups\" \"memos\" \"nicknames\"\n
:EXAMPLE\n\n<USER1>  minion: please tell <USER-W-NICK2> about minion\n
<minion> <USER-W-NICK2>: look at minion: minion is an IRC robot \(who prefers the
	 term \"electronically composed.\"\) For online help, try /msg minion help
	 Minion is hosted at common-lisp.net and is usually connected to the
	 #lisp IRC channel. (URL `http://www.cliki.net/minion')\n
;; :MINION-SOURCES
For minion sources, look at Cliki-bot sources written by Brian Mastenbrook in
the example directory of cl-irc:
:SEE (URL `http://www.cliki.net/cl-irc')
:SEE (URL `http://common-lisp.net/project/cl-irc/')
:SEE (URL `http://common-lisp.net/project/cl-irc/cl-irc_latest.tar.gz')
:SEE (URL `http://paste.lisp.org/')
:SEE (URL `http://common-lisp.net/project/lisppaste/')\n
:SEE-ALSO `mon-wget-freenode-lisp-logs', `mon-help-CL:LOCAL-TIME',
`mon-help-CL:LOOP', `mon-help-CL:TIME', `mon-help-CL-file-dir-functions',
`mon-help-CL-minion', `mon-help-CL-symbols', `mon-help-slime-keys',
`mon-help-swank-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-minion :insertp t)
    (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-CL-minion)
;;; :TEST-ME (mon-help-CL-minion t)
;;; :TEST-ME (describe-function 'mon-help-CL-minion)
;;; :TEST-ME (apply #'mon-help-CL-minion nil '(t))

;;; ==============================
;;; (string-match-p "file:" (bound-and-true-p common-lisp-hyperspec-root))
;;; `common-lisp-hyperspec-root'
;;; `common-lisp-hyperspec-symbol-table'
;;; `intern-clhs-symbol' 
;;; `common-lisp-hyperspec-strip-cl-package'
;;; `common-lisp-hyperspec-symbols'
;;; :CREATED <Timestamp: #{2010-01-29T00:42:00-05:00Z}#{10045} - by MON>
(defvar *clhs-symbol-v3-or-v7* nil)
;;
(when (bound-and-true-p IS-MON-P)
  (unless (bound-and-true-p *clhs-symbol-v3-or-v7*)
    (setq *clhs-symbol-v3-or-v7*
          (funcall ;; #'(lambda () 
           (cond ((string-match-p "Hyperspec-v3" common-lisp-hyperspec-root) 'car)
                 ((string-match-p "Hyperspec-v7" common-lisp-hyperspec-root) 'cadr)
                 ;; Lispworks, MIT, NON-MON, etc.
                 ((or (string-match-p "HyperSpec" common-lisp-hyperspec-root) t) 'cadr))
           ;; :NOTE The car maps to hyperspec-v3 the cadr hyperspec-v7.\n
           '( ;; :HYPERSPEC-v3
             (("&whole" "sec_3-4-4.htm")
              ("*" "any_st.htm")
              ("**" "var_stcm_ststcm_ststst.htm")
              ("***" "var_stcm_ststcm_ststst.htm")
              ("*break-on-signals*" "var_stbreak-on-signalsst.htm")
              ("*compile-file-pathname*" "var_stcompile_e-truenamest.htm")
              ("*compile-file-truename*" "var_stcompile_e-truenamest.htm")
              ("*compile-print*" "var_stcompile_le-verbosest.htm")
              ("*compile-verbose*" "var_stcompile_le-verbosest.htm")
              ("*debug-io*" "var_stdebug-i_ace-outputst.htm")
              ("*debugger-hook*" "var_stdebugger-hookst.htm")
              ("*default-pathname-defaults*" "var_stdefault_e-defaultsst.htm")
              ("*error-output*" "var_stdebug-i_ace-outputst.htm")
              ("*features*" "var_stfeaturesst.htm")
              ("*gensym-counter*" "var_stgensym-counterst.htm")
              ("*load-pathname*" "var_stload-pa_d-truenamest.htm")
              ("*load-print*" "var_stload-pr_ad-verbosest.htm")
              ("*load-truename*" "var_stload-pa_d-truenamest.htm")
              ("*load-verbose*" "var_stload-pr_ad-verbosest.htm")
              ("*macroexpand-hook*" "var_stmacroexpand-hookst.htm")
              ("*modules*" "var_stmodulesst.htm")
              ("*package*" "var_stpackagest.htm")
              ("*print-array*" "var_stprint-arrayst.htm")
              ("*print-base*" "var_stprint-b_rint-radixst.htm")
              ("*print-case*" "var_stprint-casest.htm")
              ("*print-circle*" "var_stprint-circlest.htm")
              ("*print-escape*" "var_stprint-escapest.htm")
              ("*print-gensym*" "var_stprint-gensymst.htm")
              ("*print-length*" "var_stprint-l_int-lengthst.htm")
              ("*print-level*" "var_stprint-l_int-lengthst.htm")
              ("*print-lines*" "var_stprint-linesst.htm")
              ("*print-miser-width*" "var_stprint-miser-widthst.htm")
              ("*print-pprint-dispatch*" "var_stprint-p_t-dispatchst.htm")
              ("*print-pretty*" "var_stprint-prettyst.htm")
              ("*print-radix*" "var_stprint-b_rint-radixst.htm")
              ("*print-readably*" "var_stprint-readablyst.htm")
              ("*print-right-margin*" "var_stprint-right-marginst.htm")
              ("*query-io*" "var_stdebug-i_ace-outputst.htm")
              ("*random-state*" "var_strandom-statest.htm")
              ("*read-base*" "var_stread-basest.htm")
              ("*read-default-float-format*" "var_stread-de_oat-formatst.htm")
              ("*read-eval*" "var_stread-evalst.htm")
              ("*read-suppress*" "var_stread-suppressst.htm")
              ("*readtable*" "var_streadtablest.htm")
              ("*standard-input*" "var_stdebug-i_ace-outputst.htm")
              ("*standard-output*" "var_stdebug-i_ace-outputst.htm")
              ("*terminal-io*" "var_stterminal-iost.htm")
              ("*trace-output*" "var_stdebug-i_ace-outputst.htm")
              ("+" "any_pl.htm")
              ("++" "var_plcm_plplcm_plplpl.htm")
              ("+++" "var_plcm_plplcm_plplpl.htm")
              ("-" "any_-.htm")
              ("/" "any_sl.htm")
              ("//" "var_slcm_slslcm_slslsl.htm")
              ("///" "var_slcm_slslcm_slslsl.htm")
              ("/=" "fun_eqcm_sleq__lteqcm_gteq.htm")
              ("1+" "fun_1plcm_1-.htm")
              ("1-" "fun_1plcm_1-.htm")
              ("<" "fun_eqcm_sleq__lteqcm_gteq.htm")
              ("<=" "fun_eqcm_sleq__lteqcm_gteq.htm")
              ("=" "fun_eqcm_sleq__lteqcm_gteq.htm")
              (">" "fun_eqcm_sleq__lteqcm_gteq.htm")
              (">=" "fun_eqcm_sleq__lteqcm_gteq.htm")
              ("abort" "any_abort.htm")
              ("abs" "fun_abs.htm")
              ("acons" "fun_acons.htm")
              ("acos" "fun_asincm_acoscm_atan.htm")
              ("acosh" "fun_sinhcm_co_coshcm_atanh.htm")
              ("add-method" "stagenfun_add-method.htm")
              ("adjoin" "fun_adjoin.htm")
              ("adjust-array" "fun_adjust-array.htm")
              ("adjustable-array-p" "fun_adjustable-array-p.htm")
              ("allocate-instance" "stagenfun_all_ate-instance.htm")
              ("allow-other-keys" "sec_3-4-1.htm")
              ("alpha-char-p" "fun_alpha-char-p.htm")
              ("alphanumericp" "fun_alphanumericp.htm")
              ("and" "any_and.htm")
              ("append" "fun_append.htm")
              ("apply" "fun_apply.htm")
              ("apropos" "fun_aproposcm_apropos-list.htm")
              ("apropos-list" "fun_aproposcm_apropos-list.htm")
              ("aref" "acc_aref.htm")
              ("arithmetic-error" "contyp_arithmetic-error.htm")
              ("arithmetic-error-operands" "fun_arithmeti_or-operation.htm")
              ("arithmetic-error-operation" "fun_arithmeti_or-operation.htm")
              ("array" "syscla_array.htm")
              ("array-dimension" "fun_array-dimension.htm")
              ("array-dimension-limit" "convar_array-_ension-limit.htm")
              ("array-dimensions" "fun_array-dimensions.htm")
              ("array-displacement" "fun_array-displacement.htm")
              ("array-element-type" "fun_array-element-type.htm")
              ("array-has-fill-pointer-p" "fun_array-has_ll-pointer-p.htm")
              ("array-in-bounds-p" "fun_array-in-bounds-p.htm")
              ("array-rank" "fun_array-rank.htm")
              ("array-rank-limit" "convar_array-rank-limit.htm")
              ("array-row-major-index" "fun_array-row-major-index.htm")
              ("array-total-size" "fun_array-total-size.htm")
              ("array-total-size-limit" "convar_array-_l-size-limit.htm")
              ("arrayp" "fun_arrayp.htm")
              ("ash" "fun_ash.htm")
              ("asin" "fun_asincm_acoscm_atan.htm")
              ("asinh" "fun_sinhcm_co_coshcm_atanh.htm")
              ("assert" "mac_assert.htm")
              ("assoc" "fun_assoccm_a_assoc-if-not.htm")
              ("assoc-if" "fun_assoccm_a_assoc-if-not.htm")
              ("assoc-if-not" "fun_assoccm_a_assoc-if-not.htm")
              ("atan" "fun_asincm_acoscm_atan.htm")
              ("atanh" "fun_sinhcm_co_coshcm_atanh.htm")
              ("atom" "any_atom.htm")
              ("aux" "sec_3-4-1.htm")
              ("base-char" "typ_base-char.htm")
              ("base-string" "typ_base-string.htm")
              ("bignum" "typ_bignum.htm")
              ("bit" "any_bit.htm")
              ("bit-and" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-andc1" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-andc2" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-eqv" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-ior" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-nand" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-nor" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-not" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-orc1" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-orc2" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("bit-vector" "syscla_bit-vector.htm")
              ("bit-vector-p" "fun_bit-vector-p.htm")
              ("bit-xor" "fun_bit-andcm_c2cm_bit-xor.htm")
              ("block" "speope_block.htm")
              ("body" "sec_3-4-4.htm")
              ("boole" "fun_boole.htm")
              ("boole-1" "convar_boole-_cm_boole-xor.htm")
              ("boole-2" "convar_boole-_cm_boole-xor.htm")
              ("boole-and" "convar_boole-_cm_boole-xor.htm")
              ("boole-andc1" "convar_boole-_cm_boole-xor.htm")
              ("boole-andc2" "convar_boole-_cm_boole-xor.htm")
              ("boole-c1" "convar_boole-_cm_boole-xor.htm")
              ("boole-c2" "convar_boole-_cm_boole-xor.htm")
              ("boole-clr" "convar_boole-_cm_boole-xor.htm")
              ("boole-eqv" "convar_boole-_cm_boole-xor.htm")
              ("boole-ior" "convar_boole-_cm_boole-xor.htm")
              ("boole-nand" "convar_boole-_cm_boole-xor.htm")
              ("boole-nor" "convar_boole-_cm_boole-xor.htm")
              ("boole-orc1" "convar_boole-_cm_boole-xor.htm")
              ("boole-orc2" "convar_boole-_cm_boole-xor.htm")
              ("boole-set" "convar_boole-_cm_boole-xor.htm")
              ("boole-xor" "convar_boole-_cm_boole-xor.htm")
              ("boolean" "typ_boolean.htm")
              ("both-case-p" "fun_upper-cas__both-case-p.htm")
              ("boundp" "fun_boundp.htm")
              ("break" "fun_break.htm")
              ("broadcast-stream" "syscla_broadcast-stream.htm")
              ("broadcast-stream-streams" "fun_broadcast_ream-streams.htm")
              ("built-in-class" "syscla_built-in-class.htm")
              ("butlast" "fun_butlastcm_nbutlast.htm")
              ("byte" "fun_bytecm_by_yte-position.htm")
              ("byte-position" "fun_bytecm_by_yte-position.htm")
              ("byte-size" "fun_bytecm_by_yte-position.htm")
              ("caaaar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caaadr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caaar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caadar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caaddr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caadr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cadaar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cadadr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cadar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caddar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cadddr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("caddr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cadr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("call-arguments-limit" "convar_call-a_uments-limit.htm")
              ("call-method" "locmac_call-m__make-method.htm")
              ("call-next-method" "locfun_call-next-method.htm")
              ("car" "acc_carcm_cdr_darcm_cddddr.htm")
              ("case" "mac_casecm_ccasecm_ecase.htm")
              ("catch" "speope_catch.htm")
              ("ccase" "mac_casecm_ccasecm_ecase.htm")
              ("cdaaar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdaadr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdaar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdadar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdaddr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdadr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cddaar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cddadr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cddar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdddar" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cddddr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdddr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cddr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("cdr" "acc_carcm_cdr_darcm_cddddr.htm")
              ("ceiling" "fun_floorcm_f_undcm_fround.htm")
              ("cell-error" "contyp_cell-error.htm")
              ("cell-error-name" "fun_cell-error-name.htm")
              ("cerror" "fun_cerror.htm")
              ("change-class" "stagenfun_change-class.htm")
              ("char" "acc_charcm_schar.htm")
              ("char-code" "fun_char-code.htm")
              ("char-code-limit" "convar_char-code-limit.htm")
              ("char-downcase" "fun_char-upca_har-downcase.htm")
              ("char-equal" "fun_chareqcm__ar-not-lessp.htm")
              ("char-greaterp" "fun_chareqcm__ar-not-lessp.htm")
              ("char-int" "fun_char-int.htm")
              ("char-lessp" "fun_chareqcm__ar-not-lessp.htm")
              ("char-name" "fun_char-name.htm")
              ("char-not-equal" "fun_chareqcm__ar-not-lessp.htm")
              ("char-not-greaterp" "fun_chareqcm__ar-not-lessp.htm")
              ("char-not-lessp" "fun_chareqcm__ar-not-lessp.htm")
              ("char-upcase" "fun_char-upca_har-downcase.htm")
              ("char/=" "fun_chareqcm__ar-not-lessp.htm")
              ("char<" "fun_chareqcm__ar-not-lessp.htm")
              ("char<=" "fun_chareqcm__ar-not-lessp.htm")
              ("char=" "fun_chareqcm__ar-not-lessp.htm")
              ("char>" "fun_chareqcm__ar-not-lessp.htm")
              ("char>=" "fun_chareqcm__ar-not-lessp.htm")
              ("character" "any_character.htm")
              ("characterp" "fun_characterp.htm")
              ("check-type" "mac_check-type.htm")
              ("cis" "fun_cis.htm")
              ("class" "syscla_class.htm")
              ("class-name" "stagenfun_class-name.htm")
              ("class-of" "fun_class-of.htm")
              ("clear-input" "fun_clear-input.htm")
              ("clear-output" "fun_finish-ou_clear-output.htm")
              ("close" "fun_close.htm")
              ("clrhash" "fun_clrhash.htm")
              ("code-char" "fun_code-char.htm")
              ("coerce" "fun_coerce.htm")
              ("compilation-speed" "dec_optimize.htm")
              ("compile" "fun_compile.htm")
              ("compile-file" "fun_compile-file.htm")
              ("compile-file-pathname" "fun_compile-file-pathname.htm")
              ("compiled-function" "typ_compiled-function.htm")
              ("compiled-function-p" "fun_compiled-function-p.htm")
              ("compiler-macro" "stagenfun_doc_umentationcp.htm")
              ("compiler-macro-function" "acc_compiler-_cro-function.htm")
              ("complement" "fun_complement.htm")
              ("complex" "any_complex.htm")
              ("complexp" "fun_complexp.htm")
              ("compute-applicable-methods" "stagenfun_com_able-methods.htm")
              ("compute-restarts" "fun_compute-restarts.htm")
              ("concatenate" "fun_concatenate.htm")
              ("concatenated-stream" "syscla_concatenated-stream.htm")
              ("concatenated-stream-streams" "fun_concatena_ream-streams.htm")
              ("cond" "mac_cond.htm")
              ("condition" "contyp_condition.htm")
              ("conjugate" "fun_conjugate.htm")
              ("cons" "any_cons.htm")
              ("consp" "fun_consp.htm")
              ("constantly" "fun_constantly.htm")
              ("constantp" "fun_constantp.htm")
              ("continue" "any_continue.htm")
              ("control-error" "contyp_control-error.htm")
              ("copy-alist" "fun_copy-alist.htm")
              ("copy-list" "fun_copy-list.htm")
              ("copy-pprint-dispatch" "fun_copy-pprint-dispatch.htm")
              ("copy-readtable" "fun_copy-readtable.htm")
              ("copy-seq" "fun_copy-seq.htm")
              ("copy-structure" "fun_copy-structure.htm")
              ("copy-symbol" "fun_copy-symbol.htm")
              ("copy-tree" "fun_copy-tree.htm")
              ("cos" "fun_sincm_coscm_tan.htm")
              ("cosh" "fun_sinhcm_co_coshcm_atanh.htm")
              ("count" "fun_countcm_c_count-if-not.htm")
              ("count-if" "fun_countcm_c_count-if-not.htm")
              ("count-if-not" "fun_countcm_c_count-if-not.htm")
              ("ctypecase" "mac_typecasec_cm_etypecase.htm")
              ("debug" "dec_optimize.htm")
              ("decf" "mac_incfcm_decf.htm")
              ("declaim" "mac_declaim.htm")
              ("declaration" "dec_declaration.htm")
              ("declare" "sym_declare.htm")
              ("decode-float" "fun_decode-fl_decode-float.htm")
              ("decode-universal-time" "fun_decode-universal-time.htm")
              ("defclass" "mac_defclass.htm")
              ("defconstant" "mac_defconstant.htm")
              ("defgeneric" "mac_defgeneric.htm")
              ("define-compiler-macro" "mac_define-compiler-macro.htm")
              ("define-condition" "mac_define-condition.htm")
              ("define-method-combination" "mac_define-me_-combination.htm")
              ("define-modify-macro" "mac_define-modify-macro.htm")
              ("define-setf-expander" "mac_define-setf-expander.htm")
              ("define-symbol-macro" "mac_define-symbol-macro.htm")
              ("defmacro" "mac_defmacro.htm")
              ("defmethod" "mac_defmethod.htm")
              ("defpackage" "mac_defpackage.htm")
              ("defparameter" "mac_defparametercm_defvar.htm")
              ("defsetf" "mac_defsetf.htm")
              ("defstruct" "mac_defstruct.htm")
              ("deftype" "mac_deftype.htm")
              ("defun" "mac_defun.htm")
              ("defvar" "mac_defparametercm_defvar.htm")
              ("delete" "fun_removecm__elete-if-not.htm")
              ("delete-duplicates" "fun_remove-du_e-duplicates.htm")
              ("delete-file" "fun_delete-file.htm")
              ("delete-if" "fun_removecm__elete-if-not.htm")
              ("delete-if-not" "fun_removecm__elete-if-not.htm")
              ("delete-package" "fun_delete-package.htm")
              ("denominator" "fun_numerator__denominator.htm")
              ("deposit-field" "fun_deposit-field.htm")
              ("describe" "fun_describe.htm")
              ("describe-object" "stagenfun_describe-object.htm")
              ("destructuring-bind" "mac_destructuring-bind.htm")
              ("digit-char" "fun_digit-char.htm")
              ("digit-char-p" "fun_digit-char-p.htm")
              ("directory" "fun_directory.htm")
              ("directory-namestring" "fun_namestrin_h-namestring.htm")
              ("disassemble" "fun_disassemble.htm")
              ("division-by-zero" "contyp_division-by-zero.htm")
              ("do" "mac_docm_dost.htm")
              ("do*" "mac_docm_dost.htm")
              ("do-all-symbols" "mac_do-symbol_-all-symbols.htm")
              ("do-external-symbols" "mac_do-symbol_-all-symbols.htm")
              ("do-symbols" "mac_do-symbol_-all-symbols.htm")
              ("documentation" "stagenfun_doc_umentationcp.htm")
              ("dolist" "mac_dolist.htm")
              ("dotimes" "mac_dotimes.htm")
              ("double-float" "typ_short-flo_m_long-float.htm")
              ("double-float-epsilon" "convar_short-_tive-epsilon.htm")
              ("double-float-negative-epsilon" "convar_short-_tive-epsilon.htm")
              ("dpb" "fun_dpb.htm")
              ("dribble" "fun_dribble.htm")
              ("dynamic-extent" "dec_dynamic-extent.htm")
              ("ecase" "mac_casecm_ccasecm_ecase.htm")
              ("echo-stream" "syscla_echo-stream.htm")
              ("echo-stream-input-stream" "fun_echo-stre_utput-stream.htm")
              ("echo-stream-output-stream" "fun_echo-stre_utput-stream.htm")
              ("ed" "fun_ed.htm")
              ("eighth" "acc_firstcm_s_inthcm_tenth.htm")
              ("elt" "acc_elt.htm")
              ("encode-universal-time" "fun_encode-universal-time.htm")
              ("end-of-file" "contyp_end-of-file.htm")
              ("endp" "fun_endp.htm")
              ("enough-namestring" "fun_namestrin_h-namestring.htm")
              ("ensure-directories-exist" "fun_ensure-di_tories-exist.htm")
              ("ensure-generic-function" "fun_ensure-ge_ric-function.htm")
              ("environment" "sec_3-4-4.htm")
              ("eq" "fun_eq.htm")
              ("eql" "any_eql.htm")
              ("equal" "fun_equal.htm")
              ("equalp" "fun_equalp.htm")
              ("error" "any_error.htm")
              ("etypecase" "mac_typecasec_cm_etypecase.htm")
              ("eval" "fun_eval.htm")
              ("eval-when" "speope_eval-when.htm")
              ("evenp" "fun_evenpcm_oddp.htm")
              ("every" "fun_everycm_s_erycm_notany.htm")
              ("exp" "fun_expcm_expt.htm")
              ("export" "fun_export.htm")
              ("expt" "fun_expcm_expt.htm")
              ("extended-char" "typ_extended-char.htm")
              ("fboundp" "fun_fboundp.htm")
              ("fceiling" "fun_floorcm_f_undcm_fround.htm")
              ("fdefinition" "acc_fdefinition.htm")
              ("ffloor" "fun_floorcm_f_undcm_fround.htm")
              ("fifth" "acc_firstcm_s_inthcm_tenth.htm")
              ("file-author" "fun_file-author.htm")
              ("file-error" "contyp_file-error.htm")
              ("file-error-pathname" "fun_file-error-pathname.htm")
              ("file-length" "fun_file-length.htm")
              ("file-namestring" "fun_namestrin_h-namestring.htm")
              ("file-position" "fun_file-position.htm")
              ("file-stream" "syscla_file-stream.htm")
              ("file-string-length" "fun_file-string-length.htm")
              ("file-write-date" "fun_file-write-date.htm")
              ("fill" "fun_fill.htm")
              ("fill-pointer" "acc_fill-pointer.htm")
              ("find" "fun_findcm_fi__find-if-not.htm")
              ("find-all-symbols" "fun_find-all-symbols.htm")
              ("find-class" "acc_find-class.htm")
              ("find-if" "fun_findcm_fi__find-if-not.htm")
              ("find-if-not" "fun_findcm_fi__find-if-not.htm")
              ("find-method" "stagenfun_find-method.htm")
              ("find-package" "fun_find-package.htm")
              ("find-restart" "fun_find-restart.htm")
              ("find-symbol" "fun_find-symbol.htm")
              ("finish-output" "fun_finish-ou_clear-output.htm")
              ("first" "acc_firstcm_s_inthcm_tenth.htm")
              ("fixnum" "typ_fixnum.htm")
              ("flet" "speope_fletcm_scm_macrolet.htm")
              ("float" "any_float.htm")
              ("float-digits" "fun_decode-fl_decode-float.htm")
              ("float-precision" "fun_decode-fl_decode-float.htm")
              ("float-radix" "fun_decode-fl_decode-float.htm")
              ("float-sign" "fun_decode-fl_decode-float.htm")
              ("floating-point-inexact" "contyp_floati_oint-inexact.htm")
              ("floating-point-invalid-operation" "contyp_floati_id-operation.htm")
              ("floating-point-overflow" "contyp_floati_int-overflow.htm")
              ("floating-point-underflow" "contyp_floati_nt-underflow.htm")
              ("floatp" "fun_floatp.htm")
              ("floor" "fun_floorcm_f_undcm_fround.htm")
              ("fmakunbound" "fun_fmakunbound.htm")
              ("force-output" "fun_finish-ou_clear-output.htm")
              ("format" "fun_format.htm")
              ("formatter" "mac_formatter.htm")
              ("fourth" "acc_firstcm_s_inthcm_tenth.htm")
              ("fresh-line" "fun_terpricm_fresh-line.htm")
              ("fround" "fun_floorcm_f_undcm_fround.htm")
              ("ftruncate" "fun_floorcm_f_undcm_fround.htm")
              ("ftype" "dec_ftype.htm")
              ("funcall" "fun_funcall.htm")
              ("function" "any_function.htm")
              ("function-keywords" "stagenfun_fun_ion-keywords.htm")
              ("function-lambda-expression" "fun_function-_a-expression.htm")
              ("functionp" "fun_functionp.htm")
              ("gcd" "fun_gcd.htm")
              ("generic-function" "syscla_generic-function.htm")
              ("gensym" "fun_gensym.htm")
              ("gentemp" "fun_gentemp.htm")
              ("get" "acc_get.htm")
              ("get-decoded-time" "fun_get-unive_decoded-time.htm")
              ("get-dispatch-macro-character" "fun_set-dispa_ro-character.htm")
              ("get-internal-real-time" "fun_get-internal-real-time.htm")
              ("get-internal-run-time" "fun_get-internal-run-time.htm")
              ("get-macro-character" "fun_set-macro_ro-character.htm")
              ("get-output-stream-string" "fun_get-outpu_tream-string.htm")
              ("get-properties" "fun_get-properties.htm")
              ("get-setf-expansion" "fun_get-setf-expansion.htm")
              ("get-universal-time" "fun_get-unive_decoded-time.htm")
              ("getf" "acc_getf.htm")
              ("gethash" "acc_gethash.htm")
              ("go" "speope_go.htm")
              ("graphic-char-p" "fun_graphic-char-p.htm")
              ("handler-bind" "mac_handler-bind.htm")
              ("handler-case" "mac_handler-case.htm")
              ("hash-table" "syscla_hash-table.htm")
              ("hash-table-count" "fun_hash-table-count.htm")
              ("hash-table-p" "fun_hash-table-p.htm")
              ("hash-table-rehash-size" "fun_hash-table-rehash-size.htm")
              ("hash-table-rehash-threshold" "fun_hash-tabl_sh-threshold.htm")
              ("hash-table-size" "fun_hash-table-size.htm")
              ("hash-table-test" "fun_hash-table-test.htm")
              ("host-namestring" "fun_namestrin_h-namestring.htm")
              ("identity" "fun_identity.htm")
              ("if" "speope_if.htm")
              ("ignorable" "dec_ignorecm_ignorable.htm")
              ("ignore" "dec_ignorecm_ignorable.htm")
              ("ignore-errors" "mac_ignore-errors.htm")
              ("imagpart" "fun_realpartcm_imagpart.htm")
              ("import" "fun_import.htm")
              ("in-package" "mac_in-package.htm")
              ("incf" "mac_incfcm_decf.htm")
              ("initialize-instance" "stagenfun_ini_ize-instance.htm")
              ("inline" "dec_inlinecm_notinline.htm")
              ("input-stream-p" "fun_input-str_put-stream-p.htm")
              ("inspect" "fun_inspect.htm")
              ("integer" "syscla_integer.htm")
              ("integer-decode-float" "fun_decode-fl_decode-float.htm")
              ("integer-length" "fun_integer-length.htm")
              ("integerp" "fun_integerp.htm")
              ("interactive-stream-p" "fun_interactive-stream-p.htm")
              ("intern" "fun_intern.htm")
              ("internal-time-units-per-second" "convar_intern_s-per-second.htm")
              ("intersection" "fun_intersect_intersection.htm")
              ("invalid-method-error" "fun_invalid-method-error.htm")
              ("invoke-debugger" "fun_invoke-debugger.htm")
              ("invoke-restart" "fun_invoke-restart.htm")
              ("invoke-restart-interactively" "fun_invoke-re_nteractively.htm")
              ("isqrt" "fun_sqrtcm_isqrt.htm")
              ("key" "sec_3-4-1.htm")
              ("keyword" "typ_keyword.htm")
              ("keywordp" "fun_keywordp.htm")
              ("labels" "speope_fletcm_scm_macrolet.htm")
              ("lambda" "any_lambda.htm")
              ("lambda-list-keywords" "convar_lambda_ist-keywords.htm")
              ("lambda-parameters-limit" "convar_lambda_meters-limit.htm")
              ("last" "fun_last.htm")
              ("lcm" "fun_lcm.htm")
              ("ldb" "acc_ldb.htm")
              ("ldb-test" "fun_ldb-test.htm")
              ("ldiff" "fun_ldiffcm_tailp.htm")
              ("least-negative-double-float" "convar_most-p_d-long-float.htm")
              ("least-negative-long-float" "convar_most-p_d-long-float.htm")
              ("least-negative-normalized-double-float" "convar_most-p_d-long-float.htm")
              ("least-negative-normalized-long-float" "convar_most-p_d-long-float.htm")
              ("least-negative-normalized-short-float" "convar_most-p_d-long-float.htm")
              ("least-negative-normalized-single-float" "convar_most-p_d-long-float.htm")
              ("least-negative-short-float" "convar_most-p_d-long-float.htm")
              ("least-negative-single-float" "convar_most-p_d-long-float.htm")
              ("least-positive-double-float" "convar_most-p_d-long-float.htm")
              ("least-positive-long-float" "convar_most-p_d-long-float.htm")
              ("least-positive-normalized-double-float" "convar_most-p_d-long-float.htm")
              ("least-positive-normalized-long-float" "convar_most-p_d-long-float.htm")
              ("least-positive-normalized-short-float" "convar_most-p_d-long-float.htm")
              ("least-positive-normalized-single-float" "convar_most-p_d-long-float.htm")
              ("least-positive-short-float" "convar_most-p_d-long-float.htm")
              ("least-positive-single-float" "convar_most-p_d-long-float.htm")
              ("length" "fun_length.htm")
              ("let" "speope_letcm_letst.htm")
              ("let*" "speope_letcm_letst.htm")
              ("lisp-implementation-type" "fun_lisp-impl_tion-version.htm")
              ("lisp-implementation-version" "fun_lisp-impl_tion-version.htm")
              ("list" "any_list.htm")
              ("list*" "fun_listcm_listst.htm")
              ("list-all-packages" "fun_list-all-packages.htm")
              ("list-length" "fun_list-length.htm")
              ("listen" "fun_listen.htm")
              ("listp" "fun_listp.htm")
              ("load" "fun_load.htm")
              ("load-logical-pathname-translations" "fun_load-logi_translations.htm")
              ("load-time-value" "speope_load-time-value.htm")
              ("locally" "speope_locally.htm")
              ("log" "fun_log.htm")
              ("logand" "fun_logandcm__rc2cm_logxor.htm")
              ("logandc1" "fun_logandcm__rc2cm_logxor.htm")
              ("logandc2" "fun_logandcm__rc2cm_logxor.htm")
              ("logbitp" "fun_logbitp.htm")
              ("logcount" "fun_logcount.htm")
              ("logeqv" "fun_logandcm__rc2cm_logxor.htm")
              ("logical-pathname" "any_logical-pathname.htm")
              ("logical-pathname-translations" "acc_logical-p_translations.htm")
              ("logior" "fun_logandcm__rc2cm_logxor.htm")
              ("lognand" "fun_logandcm__rc2cm_logxor.htm")
              ("lognor" "fun_logandcm__rc2cm_logxor.htm")
              ("lognot" "fun_logandcm__rc2cm_logxor.htm")
              ("logorc1" "fun_logandcm__rc2cm_logxor.htm")
              ("logorc2" "fun_logandcm__rc2cm_logxor.htm")
              ("logtest" "fun_logtest.htm")
              ("logxor" "fun_logandcm__rc2cm_logxor.htm")
              ("long-float" "typ_short-flo_m_long-float.htm")
              ("long-float-epsilon" "convar_short-_tive-epsilon.htm")
              ("long-float-negative-epsilon" "convar_short-_tive-epsilon.htm")
              ("long-site-name" "fun_short-sit_ng-site-name.htm")
              ("loop" "mac_loop.htm")
              ("loop-finish" "locmac_loop-finish.htm")
              ("lower-case-p" "fun_upper-cas__both-case-p.htm")
              ("machine-instance" "fun_machine-instance.htm")
              ("machine-type" "fun_machine-type.htm")
              ("machine-version" "fun_machine-version.htm")
              ("macro-function" "acc_macro-function.htm")
              ("macroexpand" "fun_macroexpa_acroexpand-1.htm")
              ("macroexpand-1" "fun_macroexpa_acroexpand-1.htm")
              ("macrolet" "speope_fletcm_scm_macrolet.htm")
              ("make-array" "fun_make-array.htm")
              ("make-broadcast-stream" "fun_make-broadcast-stream.htm")
              ("make-concatenated-stream" "fun_make-conc_nated-stream.htm")
              ("make-condition" "fun_make-condition.htm")
              ("make-dispatch-macro-character" "fun_make-disp_ro-character.htm")
              ("make-echo-stream" "fun_make-echo-stream.htm")
              ("make-hash-table" "fun_make-hash-table.htm")
              ("make-instance" "stagenfun_make-instance.htm")
              ("make-instances-obsolete" "stagenfun_mak_ces-obsolete.htm")
              ("make-list" "fun_make-list.htm")
              ("make-load-form" "stagenfun_make-load-form.htm")
              ("make-load-form-saving-slots" "fun_make-load_saving-slots.htm")
              ("make-method" "locmac_call-m__make-method.htm")
              ("make-package" "fun_make-package.htm")
              ("make-pathname" "fun_make-pathname.htm")
              ("make-random-state" "fun_make-random-state.htm")
              ("make-sequence" "fun_make-sequence.htm")
              ("make-string" "fun_make-string.htm")
              ("make-string-input-stream" "fun_make-stri_input-stream.htm")
              ("make-string-output-stream" "fun_make-stri_utput-stream.htm")
              ("make-symbol" "fun_make-symbol.htm")
              ("make-synonym-stream" "fun_make-synonym-stream.htm")
              ("make-two-way-stream" "fun_make-two-way-stream.htm")
              ("makunbound" "fun_makunbound.htm")
              ("map" "fun_map.htm")
              ("map-into" "fun_map-into.htm")
              ("mapc" "fun_mapccm_ma_istcm_mapcon.htm")
              ("mapcan" "fun_mapccm_ma_istcm_mapcon.htm")
              ("mapcar" "fun_mapccm_ma_istcm_mapcon.htm")
              ("mapcon" "fun_mapccm_ma_istcm_mapcon.htm")
              ("maphash" "fun_maphash.htm")
              ("mapl" "fun_mapccm_ma_istcm_mapcon.htm")
              ("maplist" "fun_mapccm_ma_istcm_mapcon.htm")
              ("mask-field" "acc_mask-field.htm")
              ("max" "fun_maxcm_min.htm")
              ("member" "any_member.htm")
              ("member-if" "fun_membercm__ember-if-not.htm")
              ("member-if-not" "fun_membercm__ember-if-not.htm")
              ("merge" "fun_merge.htm")
              ("merge-pathnames" "fun_merge-pathnames.htm")
              ("method" "syscla_method.htm")
              ("method-combination" "any_method-combination.htm")
              ("method-combination-error" "fun_method-co_nation-error.htm")
              ("method-qualifiers" "stagenfun_met_d-qualifiers.htm")
              ("min" "fun_maxcm_min.htm")
              ("minusp" "fun_minuspcm_plusp.htm")
              ("mismatch" "fun_mismatch.htm")
              ("mod" "any_mod.htm")
              ("most-negative-double-float" "convar_most-p_d-long-float.htm")
              ("most-negative-fixnum" "convar_most-p_ative-fixnum.htm")
              ("most-negative-long-float" "convar_most-p_d-long-float.htm")
              ("most-negative-short-float" "convar_most-p_d-long-float.htm")
              ("most-negative-single-float" "convar_most-p_d-long-float.htm")
              ("most-positive-double-float" "convar_most-p_d-long-float.htm")
              ("most-positive-fixnum" "convar_most-p_ative-fixnum.htm")
              ("most-positive-long-float" "convar_most-p_d-long-float.htm")
              ("most-positive-short-float" "convar_most-p_d-long-float.htm")
              ("most-positive-single-float" "convar_most-p_d-long-float.htm")
              ("muffle-warning" "any_muffle-warning.htm")
              ("multiple-value-bind" "mac_multiple-value-bind.htm")
              ("multiple-value-call" "speope_multiple-value-call.htm")
              ("multiple-value-list" "mac_multiple-value-list.htm")
              ("multiple-value-prog1" "speope_multip_-value-prog1.htm")
              ("multiple-value-setq" "mac_multiple-value-setq.htm")
              ("multiple-values-limit" "convar_multip_values-limit.htm")
              ("name-char" "fun_name-char.htm")
              ("namestring" "fun_namestrin_h-namestring.htm")
              ("nbutlast" "fun_butlastcm_nbutlast.htm")
              ("nconc" "fun_nconc.htm")
              ("next-method-p" "locfun_next-method-p.htm")
              ("nil" "any_nil.htm")
              ("nintersection" "fun_intersect_intersection.htm")
              ("ninth" "acc_firstcm_s_inthcm_tenth.htm")
              ("no-applicable-method" "stagenfun_no-_cable-method.htm")
              ("no-next-method" "stagenfun_no-next-method.htm")
              ("not" "any_not.htm")
              ("notany" "fun_everycm_s_erycm_notany.htm")
              ("notevery" "fun_everycm_s_erycm_notany.htm")
              ("notinline" "dec_inlinecm_notinline.htm")
              ("nreconc" "fun_revappendcm_nreconc.htm")
              ("nreverse" "fun_reversecm_nreverse.htm")
              ("nset-difference" "fun_set-diffe_t-difference.htm")
              ("nset-exclusive-or" "fun_set-exclu_exclusive-or.htm")
              ("nstring-capitalize" "fun_string-up_g-capitalize.htm")
              ("nstring-downcase" "fun_string-up_g-capitalize.htm")
              ("nstring-upcase" "fun_string-up_g-capitalize.htm")
              ("nsublis" "fun_subliscm_nsublis.htm")
              ("nsubst" "fun_substcm_s_subst-if-not.htm")
              ("nsubst-if" "fun_substcm_s_subst-if-not.htm")
              ("nsubst-if-not" "fun_substcm_s_subst-if-not.htm")
              ("nsubstitute" "fun_substitut_itute-if-not.htm")
              ("nsubstitute-if" "fun_substitut_itute-if-not.htm")
              ("nsubstitute-if-not" "fun_substitut_itute-if-not.htm")
              ("nth" "acc_nth.htm")
              ("nth-value" "mac_nth-value.htm")
              ("nthcdr" "fun_nthcdr.htm")
              ("null" "any_null.htm")
              ("number" "syscla_number.htm")
              ("numberp" "fun_numberp.htm")
              ("numerator" "fun_numerator__denominator.htm")
              ("nunion" "fun_unioncm_nunion.htm")
              ("oddp" "fun_evenpcm_oddp.htm")
              ("open" "fun_open.htm")
              ("open-stream-p" "fun_open-stream-p.htm")
              ("optimize" "dec_optimize.htm")
              ("optional" "sec_3-4-1.htm")
              ("or" "any_or.htm")
              ("otherwise" "mac_casecm_ccasecm_ecase.htm")
              ("output-stream-p" "fun_input-str_put-stream-p.htm")
              ("package" "syscla_package.htm")
              ("package-error" "contyp_package-error.htm")
              ("package-error-package" "fun_package-error-package.htm")
              ("package-name" "fun_package-name.htm")
              ("package-nicknames" "fun_package-nicknames.htm")
              ("package-shadowing-symbols" "fun_package-s_wing-symbols.htm")
              ("package-use-list" "fun_package-use-list.htm")
              ("package-used-by-list" "fun_package-used-by-list.htm")
              ("packagep" "fun_packagep.htm")
              ("pairlis" "fun_pairlis.htm")
              ("parse-error" "contyp_parse-error.htm")
              ("parse-integer" "fun_parse-integer.htm")
              ("parse-namestring" "fun_parse-namestring.htm")
              ("pathname" "any_pathname.htm")
              ("pathname-device" "fun_pathname-_name-version.htm")
              ("pathname-directory" "fun_pathname-_name-version.htm")
              ("pathname-host" "fun_pathname-_name-version.htm")
              ("pathname-match-p" "fun_pathname-match-p.htm")
              ("pathname-name" "fun_pathname-_name-version.htm")
              ("pathname-type" "fun_pathname-_name-version.htm")
              ("pathname-version" "fun_pathname-_name-version.htm")
              ("pathnamep" "fun_pathnamep.htm")
              ("peek-char" "fun_peek-char.htm")
              ("phase" "fun_phase.htm")
              ("pi" "convar_pi.htm")
              ("plusp" "fun_minuspcm_plusp.htm")
              ("pop" "mac_pop.htm")
              ("position" "fun_positionc_ition-if-not.htm")
              ("position-if" "fun_positionc_ition-if-not.htm")
              ("position-if-not" "fun_positionc_ition-if-not.htm")
              ("pprint" "fun_writecm_p_rintcm_princ.htm")
              ("pprint-dispatch" "fun_pprint-dispatch.htm")
              ("pprint-exit-if-list-exhausted" "locmac_pprint_st-exhausted.htm")
              ("pprint-fill" "fun_pprint-fi_rint-tabular.htm")
              ("pprint-indent" "fun_pprint-indent.htm")
              ("pprint-linear" "fun_pprint-fi_rint-tabular.htm")
              ("pprint-logical-block" "mac_pprint-logical-block.htm")
              ("pprint-newline" "fun_pprint-newline.htm")
              ("pprint-pop" "locmac_pprint-pop.htm")
              ("pprint-tab" "fun_pprint-tab.htm")
              ("pprint-tabular" "fun_pprint-fi_rint-tabular.htm")
              ("prin1" "fun_writecm_p_rintcm_princ.htm")
              ("prin1-to-string" "fun_write-to-_nc-to-string.htm")
              ("princ" "fun_writecm_p_rintcm_princ.htm")
              ("princ-to-string" "fun_write-to-_nc-to-string.htm")
              ("print" "fun_writecm_p_rintcm_princ.htm")
              ("print-not-readable" "contyp_print-not-readable.htm")
              ("print-not-readable-object" "fun_print-not_dable-object.htm")
              ("print-object" "stagenfun_print-object.htm")
              ("print-unreadable-object" "mac_print-unr_dable-object.htm")
              ("probe-file" "fun_probe-file.htm")
              ("proclaim" "fun_proclaim.htm")
              ("prog" "mac_progcm_progst.htm")
              ("prog*" "mac_progcm_progst.htm")
              ("prog1" "mac_prog1cm_prog2.htm")
              ("prog2" "mac_prog1cm_prog2.htm")
              ("progn" "speope_progn.htm")
              ("program-error" "contyp_program-error.htm")
              ("progv" "speope_progv.htm")
              ("provide" "fun_providecm_require.htm")
              ("psetf" "mac_setfcm_psetf.htm")
              ("psetq" "mac_psetq.htm")
              ("push" "mac_push.htm")
              ("pushnew" "mac_pushnew.htm")
              ("quote" "speope_quote.htm")
              ("random" "fun_random.htm")
              ("random-state" "syscla_random-state.htm")
              ("random-state-p" "fun_random-state-p.htm")
              ("rassoc" "fun_rassoccm__assoc-if-not.htm")
              ("rassoc-if" "fun_rassoccm__assoc-if-not.htm")
              ("rassoc-if-not" "fun_rassoccm__assoc-if-not.htm")
              ("ratio" "syscla_ratio.htm")
              ("rational" "any_rational.htm")
              ("rationalize" "fun_rationalcm_rationalize.htm")
              ("rationalp" "fun_rationalp.htm")
              ("read" "fun_readcm_re_g-whitespace.htm")
              ("read-byte" "fun_read-byte.htm")
              ("read-char" "fun_read-char.htm")
              ("read-char-no-hang" "fun_read-char-no-hang.htm")
              ("read-delimited-list" "fun_read-delimited-list.htm")
              ("read-from-string" "fun_read-from-string.htm")
              ("read-line" "fun_read-line.htm")
              ("read-preserving-whitespace" "fun_readcm_re_g-whitespace.htm")
              ("read-sequence" "fun_read-sequence.htm")
              ("reader-error" "contyp_reader-error.htm")
              ("readtable" "syscla_readtable.htm")
              ("readtable-case" "acc_readtable-case.htm")
              ("readtablep" "fun_readtablep.htm")
              ("real" "syscla_real.htm")
              ("realp" "fun_realp.htm")
              ("realpart" "fun_realpartcm_imagpart.htm")
              ("reduce" "fun_reduce.htm")
              ("reinitialize-instance" "stagenfun_rei_ize-instance.htm")
              ("rem" "fun_modcm_rem.htm")
              ("remf" "mac_remf.htm")
              ("remhash" "fun_remhash.htm")
              ("remove" "fun_removecm__elete-if-not.htm")
              ("remove-duplicates" "fun_remove-du_e-duplicates.htm")
              ("remove-if" "fun_removecm__elete-if-not.htm")
              ("remove-if-not" "fun_removecm__elete-if-not.htm")
              ("remove-method" "stagenfun_remove-method.htm")
              ("remprop" "fun_remprop.htm")
              ("rename-file" "fun_rename-file.htm")
              ("rename-package" "fun_rename-package.htm")
              ("replace" "fun_replace.htm")
              ("require" "fun_providecm_require.htm")
              ("rest" "acc_rest.htm")
              ("rest" "sec_3-4-1.htm")
              ("restart" "syscla_restart.htm")
              ("restart-bind" "mac_restart-bind.htm")
              ("restart-case" "mac_restart-case.htm")
              ("restart-name" "fun_restart-name.htm")
              ("return" "mac_return.htm")
              ("return-from" "speope_return-from.htm")
              ("revappend" "fun_revappendcm_nreconc.htm")
              ("reverse" "fun_reversecm_nreverse.htm")
              ("room" "fun_room.htm")
              ("rotatef" "mac_rotatef.htm")
              ("round" "fun_floorcm_f_undcm_fround.htm")
              ("row-major-aref" "acc_row-major-aref.htm")
              ("rplaca" "fun_rplacacm_rplacd.htm")
              ("rplacd" "fun_rplacacm_rplacd.htm")
              ("safety" "dec_optimize.htm")
              ("satisfies" "typspe_satisfies.htm")
              ("sbit" "acc_bitcm_sbit.htm")
              ("scale-float" "fun_decode-fl_decode-float.htm")
              ("schar" "acc_charcm_schar.htm")
              ("search" "fun_search.htm")
              ("second" "acc_firstcm_s_inthcm_tenth.htm")
              ("sequence" "syscla_sequence.htm")
              ("serious-condition" "contyp_serious-condition.htm")
              ("set" "fun_set.htm")
              ("set-difference" "fun_set-diffe_t-difference.htm")
              ("set-dispatch-macro-character" "fun_set-dispa_ro-character.htm")
              ("set-exclusive-or" "fun_set-exclu_exclusive-or.htm")
              ("set-macro-character" "fun_set-macro_ro-character.htm")
              ("set-pprint-dispatch" "fun_set-pprint-dispatch.htm")
              ("set-syntax-from-char" "fun_set-syntax-from-char.htm")
              ("setf" "any_setf.htm")
              ("setq" "spefor_setq.htm")
              ("seventh" "acc_firstcm_s_inthcm_tenth.htm")
              ("shadow" "fun_shadow.htm")
              ("shadowing-import" "fun_shadowing-import.htm")
              ("shared-initialize" "stagenfun_sha_d-initialize.htm")
              ("shiftf" "mac_shiftf.htm")
              ("short-float" "typ_short-flo_m_long-float.htm")
              ("short-float-epsilon" "convar_short-_tive-epsilon.htm")
              ("short-float-negative-epsilon" "convar_short-_tive-epsilon.htm")
              ("short-site-name" "fun_short-sit_ng-site-name.htm")
              ("signal" "fun_signal.htm")
              ("signed-byte" "typ_signed-byte.htm")
              ("signum" "fun_signum.htm")
              ("simple-array" "typ_simple-array.htm")
              ("simple-base-string" "typ_simple-base-string.htm")
              ("simple-bit-vector" "typ_simple-bit-vector.htm")
              ("simple-bit-vector-p" "fun_simple-bit-vector-p.htm")
              ("simple-condition" "contyp_simple-condition.htm")
              ("simple-condition-format-arguments" "fun_simple-co_at-arguments.htm")
              ("simple-condition-format-control" "fun_simple-co_at-arguments.htm")
              ("simple-error" "contyp_simple-error.htm")
              ("simple-string" "typ_simple-string.htm")
              ("simple-string-p" "fun_simple-string-p.htm")
              ("simple-type-error" "contyp_simple-type-error.htm")
              ("simple-vector" "typ_simple-vector.htm")
              ("simple-vector-p" "fun_simple-vector-p.htm")
              ("simple-warning" "contyp_simple-warning.htm")
              ("sin" "fun_sincm_coscm_tan.htm")
              ("single-float" "typ_short-flo_m_long-float.htm")
              ("single-float-epsilon" "convar_short-_tive-epsilon.htm")
              ("single-float-negative-epsilon" "convar_short-_tive-epsilon.htm")
              ("sinh" "fun_sinhcm_co_coshcm_atanh.htm")
              ("sixth" "acc_firstcm_s_inthcm_tenth.htm")
              ("sleep" "fun_sleep.htm")
              ("slot-boundp" "fun_slot-boundp.htm")
              ("slot-exists-p" "fun_slot-exists-p.htm")
              ("slot-makunbound" "fun_slot-makunbound.htm")
              ("slot-missing" "stagenfun_slot-missing.htm")
              ("slot-unbound" "stagenfun_slot-unbound.htm")
              ("slot-value" "fun_slot-value.htm")
              ("software-type" "fun_software-_ware-version.htm")
              ("software-version" "fun_software-_ware-version.htm")
              ("some" "fun_everycm_s_erycm_notany.htm")
              ("sort" "fun_sortcm_stable-sort.htm")
              ("space" "dec_optimize.htm")
              ("special" "dec_special.htm")
              ("special-operator-p" "fun_special-operator-p.htm")
              ("speed" "dec_optimize.htm")
              ("sqrt" "fun_sqrtcm_isqrt.htm")
              ("stable-sort" "fun_sortcm_stable-sort.htm")
              ("standard" "sec_7-6-6-2.htm")
              ("standard-char" "typ_standard-char.htm")
              ("standard-char-p" "fun_standard-char-p.htm")
              ("standard-class" "syscla_standard-class.htm")
              ("standard-generic-function" "syscla_standa_ric-function.htm")
              ("standard-method" "syscla_standard-method.htm")
              ("standard-object" "cla_standard-object.htm")
              ("step" "mac_step.htm")
              ("storage-condition" "contyp_storage-condition.htm")
              ("store-value" "any_store-value.htm")
              ("stream" "syscla_stream.htm")
              ("stream-element-type" "fun_stream-element-type.htm")
              ("stream-error" "contyp_stream-error.htm")
              ("stream-error-stream" "fun_stream-error-stream.htm")
              ("stream-external-format" "fun_stream-external-format.htm")
              ("streamp" "fun_streamp.htm")
              ("string" "any_string.htm")
              ("string-capitalize" "fun_string-up_g-capitalize.htm")
              ("string-downcase" "fun_string-up_g-capitalize.htm")
              ("string-equal" "fun_stringeqc_ng-not-lessp.htm")
              ("string-greaterp" "fun_stringeqc_ng-not-lessp.htm")
              ("string-left-trim" "fun_string-tr_g-right-trim.htm")
              ("string-lessp" "fun_stringeqc_ng-not-lessp.htm")
              ("string-not-equal" "fun_stringeqc_ng-not-lessp.htm")
              ("string-not-greaterp" "fun_stringeqc_ng-not-lessp.htm")
              ("string-not-lessp" "fun_stringeqc_ng-not-lessp.htm")
              ("string-right-trim" "fun_string-tr_g-right-trim.htm")
              ("string-stream" "syscla_string-stream.htm")
              ("string-trim" "fun_string-tr_g-right-trim.htm")
              ("string-upcase" "fun_string-up_g-capitalize.htm")
              ("string/=" "fun_stringeqc_ng-not-lessp.htm")
              ("string<" "fun_stringeqc_ng-not-lessp.htm")
              ("string<=" "fun_stringeqc_ng-not-lessp.htm")
              ("string=" "fun_stringeqc_ng-not-lessp.htm")
              ("string>" "fun_stringeqc_ng-not-lessp.htm")
              ("string>=" "fun_stringeqc_ng-not-lessp.htm")
              ("stringp" "fun_stringp.htm")
              ("structure" "stagenfun_doc_umentationcp.htm")
              ("structure-class" "syscla_structure-class.htm")
              ("structure-object" "cla_structure-object.htm")
              ("style-warning" "contyp_style-warning.htm")
              ("sublis" "fun_subliscm_nsublis.htm")
              ("subseq" "acc_subseq.htm")
              ("subsetp" "fun_subsetp.htm")
              ("subst" "fun_substcm_s_subst-if-not.htm")
              ("subst-if" "fun_substcm_s_subst-if-not.htm")
              ("subst-if-not" "fun_substcm_s_subst-if-not.htm")
              ("substitute" "fun_substitut_itute-if-not.htm")
              ("substitute-if" "fun_substitut_itute-if-not.htm")
              ("substitute-if-not" "fun_substitut_itute-if-not.htm")
              ("subtypep" "fun_subtypep.htm")
              ("svref" "acc_svref.htm")
              ("sxhash" "fun_sxhash.htm")
              ("symbol" "syscla_symbol.htm")
              ("symbol-function" "acc_symbol-function.htm")
              ("symbol-macrolet" "speope_symbol-macrolet.htm")
              ("symbol-name" "fun_symbol-name.htm")
              ("symbol-package" "fun_symbol-package.htm")
              ("symbol-plist" "acc_symbol-plist.htm")
              ("symbol-value" "acc_symbol-value.htm")
              ("symbolp" "fun_symbolp.htm")
              ("synonym-stream" "syscla_synonym-stream.htm")
              ("synonym-stream-symbol" "fun_synonym-stream-symbol.htm")
              ("t" "any_t.htm")
              ("tagbody" "speope_tagbody.htm")
              ("tailp" "fun_ldiffcm_tailp.htm")
              ("tan" "fun_sincm_coscm_tan.htm")
              ("tanh" "fun_sinhcm_co_coshcm_atanh.htm")
              ("tenth" "acc_firstcm_s_inthcm_tenth.htm")
              ("terpri" "fun_terpricm_fresh-line.htm")
              ("the" "speope_the.htm")
              ("third" "acc_firstcm_s_inthcm_tenth.htm")
              ("throw" "speope_throw.htm")
              ("time" "mac_time.htm")
              ("trace" "mac_tracecm_untrace.htm")
              ("translate-logical-pathname" "fun_translate_cal-pathname.htm")
              ("translate-pathname" "fun_translate-pathname.htm")
              ("tree-equal" "fun_tree-equal.htm")
              ("truename" "fun_truename.htm")
              ("truncate" "fun_floorcm_f_undcm_fround.htm")
              ("two-way-stream" "syscla_two-way-stream.htm")
              ("two-way-stream-input-stream" "fun_two-way-s_utput-stream.htm")
              ("two-way-stream-output-stream" "fun_two-way-s_utput-stream.htm")
              ("type" "any_type.htm")
              ("type-error" "contyp_type-error.htm")
              ("type-error-datum" "fun_type-erro_xpected-type.htm")
              ("type-error-expected-type" "fun_type-erro_xpected-type.htm")
              ("type-of" "fun_type-of.htm")
              ("typecase" "mac_typecasec_cm_etypecase.htm")
              ("typep" "fun_typep.htm")
              ("unbound-slot" "contyp_unbound-slot.htm")
              ("unbound-slot-instance" "fun_unbound-slot-instance.htm")
              ("unbound-variable" "contyp_unbound-variable.htm")
              ("undefined-function" "contyp_undefined-function.htm")
              ("unexport" "fun_unexport.htm")
              ("unintern" "fun_unintern.htm")
              ("union" "fun_unioncm_nunion.htm")
              ("unless" "mac_whencm_unless.htm")
              ("unread-char" "fun_unread-char.htm")
              ("unsigned-byte" "typ_unsigned-byte.htm")
              ("untrace" "mac_tracecm_untrace.htm")
              ("unuse-package" "fun_unuse-package.htm")
              ("unwind-protect" "speope_unwind-protect.htm")
              ("update-instance-for-different-class" "stagenfun_upd_ferent-class.htm")
              ("update-instance-for-redefined-class" "stagenfun_upd_efined-class.htm")
              ("upgraded-array-element-type" "fun_upgraded-_element-type.htm")
              ("upgraded-complex-part-type" "fun_upgraded-_ex-part-type.htm")
              ("upper-case-p" "fun_upper-cas__both-case-p.htm")
              ("use-package" "fun_use-package.htm")
              ("use-value" "any_use-value.htm")
              ("user-homedir-pathname" "fun_user-homedir-pathname.htm")
              ("values" "any_values.htm")
              ("values-list" "fun_values-list.htm")
              ("variable" "stagenfun_doc_umentationcp.htm")
              ("vector" "any_vector.htm")
              ("vector-pop" "fun_vector-pop.htm")
              ("vector-push" "fun_vector-pu_-push-extend.htm")
              ("vector-push-extend" "fun_vector-pu_-push-extend.htm")
              ("vectorp" "fun_vectorp.htm")
              ("warn" "fun_warn.htm")
              ("warning" "contyp_warning.htm")
              ("when" "mac_whencm_unless.htm")
              ("wild-pathname-p" "fun_wild-pathname-p.htm")
              ("with-accessors" "mac_with-accessors.htm")
              ("with-compilation-unit" "mac_with-compilation-unit.htm")
              ("with-condition-restarts" "mac_with-cond_ion-restarts.htm")
              ("with-hash-table-iterator" "mac_with-hash_ble-iterator.htm")
              ("with-input-from-string" "mac_with-input-from-string.htm")
              ("with-open-file" "mac_with-open-file.htm")
              ("with-open-stream" "mac_with-open-stream.htm")
              ("with-output-to-string" "mac_with-output-to-string.htm")
              ("with-package-iterator" "mac_with-package-iterator.htm")
              ("with-simple-restart" "mac_with-simple-restart.htm")
              ("with-slots" "mac_with-slots.htm")
              ("with-standard-io-syntax" "mac_with-stan_rd-io-syntax.htm")
              ("write" "fun_writecm_p_rintcm_princ.htm")
              ("write-byte" "fun_write-byte.htm")
              ("write-char" "fun_write-char.htm")
              ("write-line" "fun_write-str_m_write-line.htm")
              ("write-sequence" "fun_write-sequence.htm")
              ("write-string" "fun_write-str_m_write-line.htm")
              ("write-to-string" "fun_write-to-_nc-to-string.htm")
              ("y-or-n-p" "fun_y-or-n-pcm_yes-or-no-p.htm")
              ("yes-or-no-p" "fun_y-or-n-pcm_yes-or-no-p.htm")
              ("zerop" "fun_zerop.htm"))
             ( ;; Hyperspec-v7                
              ("&allow-other-keys" "03_da.htm")
              ("&aux" "03_da.htm")
              ("&body" "03_dd.htm")
              ("&environment" "03_dd.htm")
              ("&key" "03_da.htm")
              ("&optional" "03_da.htm")
              ("&rest" "03_da.htm")
              ("&whole" "03_dd.htm")
              ("*" "a_st.htm")
              ("**" "v__stst_.htm")
              ("***" "v__stst_.htm")
              ("*break-on-signals*" "v_break_.htm")
              ("*compile-file-pathname*" "v_cmp_fi.htm")
              ("*compile-file-truename*" "v_cmp_fi.htm")
              ("*compile-print*" "v_cmp_pr.htm")
              ("*compile-verbose*" "v_cmp_pr.htm")
              ("*debug-io*" "v_debug_.htm")
              ("*debugger-hook*" "v_debugg.htm")
              ("*default-pathname-defaults*" "v_defaul.htm")
              ("*error-output*" "v_debug_.htm")
              ("*features*" "v_featur.htm")
              ("*gensym-counter*" "v_gensym.htm")
              ("*load-pathname*" "v_ld_pns.htm")
              ("*load-print*" "v_ld_prs.htm")
              ("*load-truename*" "v_ld_pns.htm")
              ("*load-verbose*" "v_ld_prs.htm")
              ("*macroexpand-hook*" "v_mexp_h.htm")
              ("*modules*" "v_module.htm")
              ("*package*" "v_pkg.htm")
              ("*print-array*" "v_pr_ar.htm")
              ("*print-base*" "v_pr_bas.htm")
              ("*print-case*" "v_pr_cas.htm")
              ("*print-circle*" "v_pr_cir.htm")
              ("*print-escape*" "v_pr_esc.htm")
              ("*print-gensym*" "v_pr_gen.htm")
              ("*print-length*" "v_pr_lev.htm")
              ("*print-level*" "v_pr_lev.htm")
              ("*print-lines*" "v_pr_lin.htm")
              ("*print-miser-width*" "v_pr_mis.htm")
              ("*print-pprint-dispatch*" "v_pr_ppr.htm")
              ("*print-pretty*" "v_pr_pre.htm")
              ("*print-radix*" "v_pr_bas.htm")
              ("*print-readably*" "v_pr_rda.htm")
              ("*print-right-margin*" "v_pr_rig.htm")
              ("*query-io*" "v_debug_.htm")
              ("*random-state*" "v_rnd_st.htm")
              ("*read-base*" "v_rd_bas.htm")
              ("*read-default-float-format*" "v_rd_def.htm")
              ("*read-eval*" "v_rd_eva.htm")
              ("*read-suppress*" "v_rd_sup.htm")
              ("*readtable*" "v_rdtabl.htm")
              ("*standard-input*" "v_debug_.htm")
              ("*standard-output*" "v_debug_.htm")
              ("*terminal-io*" "v_termin.htm")
              ("*trace-output*" "v_debug_.htm")
              ("+" "a_pl.htm")
              ("++" "v_pl_plp.htm")
              ("+++" "v_pl_plp.htm")
              ("-" "a__.htm")
              ("/" "a_sl.htm")
              ("//" "v_sl_sls.htm")
              ("///" "v_sl_sls.htm")
              ("/=" "f_eq_sle.htm")
              ("1+" "f_1pl_1_.htm")
              ("1-" "f_1pl_1_.htm")
              ("<" "f_eq_sle.htm")
              ("<=" "f_eq_sle.htm")
              ("=" "f_eq_sle.htm")
              (">" "f_eq_sle.htm")
              (">=" "f_eq_sle.htm")
              ("abort" "a_abort.htm")
              ("abs" "f_abs.htm")
              ("acons" "f_acons.htm")
              ("acos" "f_asin_.htm")
              ("acosh" "f_sinh_.htm")
              ("add-method" "f_add_me.htm")
              ("adjoin" "f_adjoin.htm")
              ("adjust-array" "f_adjust.htm")
              ("adjustable-array-p" "f_adju_1.htm")
              ("allocate-instance" "f_alloca.htm")
              ("alpha-char-p" "f_alpha_.htm")
              ("alphanumericp" "f_alphan.htm")
              ("and" "a_and.htm")
              ("append" "f_append.htm")
              ("apply" "f_apply.htm")
              ("apropos" "f_apropo.htm")
              ("apropos-list" "f_apropo.htm")
              ("aref" "f_aref.htm")
              ("arithmetic-error" "e_arithm.htm")
              ("arithmetic-error-operands" "f_arithm.htm")
              ("arithmetic-error-operation" "f_arithm.htm")
              ("array" "t_array.htm")
              ("array-dimension" "f_ar_dim.htm")
              ("array-dimension-limit" "v_ar_dim.htm")
              ("array-dimensions" "f_ar_d_1.htm")
              ("array-displacement" "f_ar_dis.htm")
              ("array-element-type" "f_ar_ele.htm")
              ("array-has-fill-pointer-p" "f_ar_has.htm")
              ("array-in-bounds-p" "f_ar_in_.htm")
              ("array-rank" "f_ar_ran.htm")
              ("array-rank-limit" "v_ar_ran.htm")
              ("array-row-major-index" "f_ar_row.htm")
              ("array-total-size" "f_ar_tot.htm")
              ("array-total-size-limit" "v_ar_tot.htm")
              ("arrayp" "f_arrayp.htm")
              ("ash" "f_ash.htm")
              ("asin" "f_asin_.htm")
              ("asinh" "f_sinh_.htm")
              ("assert" "m_assert.htm")
              ("assoc" "f_assocc.htm")
              ("assoc-if" "f_assocc.htm")
              ("assoc-if-not" "f_assocc.htm")
              ("atan" "f_asin_.htm")
              ("atanh" "f_sinh_.htm")
              ("atom" "a_atom.htm")
              ("base-char" "t_base_c.htm")
              ("base-string" "t_base_s.htm")
              ("bignum" "t_bignum.htm")
              ("bit" "a_bit.htm")
              ("bit-and" "f_bt_and.htm")
              ("bit-andc1" "f_bt_and.htm")
              ("bit-andc2" "f_bt_and.htm")
              ("bit-eqv" "f_bt_and.htm")
              ("bit-ior" "f_bt_and.htm")
              ("bit-nand" "f_bt_and.htm")
              ("bit-nor" "f_bt_and.htm")
              ("bit-not" "f_bt_and.htm")
              ("bit-orc1" "f_bt_and.htm")
              ("bit-orc2" "f_bt_and.htm")
              ("bit-vector" "t_bt_vec.htm")
              ("bit-vector-p" "f_bt_vec.htm")
              ("bit-xor" "f_bt_and.htm")
              ("block" "s_block.htm")
              ("boole" "f_boole.htm")
              ("boole-1" "v_b_1_b.htm")
              ("boole-2" "v_b_1_b.htm")
              ("boole-and" "v_b_1_b.htm")
              ("boole-andc1" "v_b_1_b.htm")
              ("boole-andc2" "v_b_1_b.htm")
              ("boole-c1" "v_b_1_b.htm")
              ("boole-c2" "v_b_1_b.htm")
              ("boole-clr" "v_b_1_b.htm")
              ("boole-eqv" "v_b_1_b.htm")
              ("boole-ior" "v_b_1_b.htm")
              ("boole-nand" "v_b_1_b.htm")
              ("boole-nor" "v_b_1_b.htm")
              ("boole-orc1" "v_b_1_b.htm")
              ("boole-orc2" "v_b_1_b.htm")
              ("boole-set" "v_b_1_b.htm")
              ("boole-xor" "v_b_1_b.htm")
              ("boolean" "t_ban.htm")
              ("both-case-p" "f_upper_.htm")
              ("boundp" "f_boundp.htm")
              ("break" "f_break.htm")
              ("broadcast-stream" "t_broadc.htm")
              ("broadcast-stream-streams" "f_broadc.htm")
              ("built-in-class" "t_built_.htm")
              ("butlast" "f_butlas.htm")
              ("byte" "f_by_by.htm")
              ("byte-position" "f_by_by.htm")
              ("byte-size" "f_by_by.htm")
              ("caaaar" "f_car_c.htm")
              ("caaadr" "f_car_c.htm")
              ("caaar" "f_car_c.htm")
              ("caadar" "f_car_c.htm")
              ("caaddr" "f_car_c.htm")
              ("caadr" "f_car_c.htm")
              ("caar" "f_car_c.htm")
              ("cadaar" "f_car_c.htm")
              ("cadadr" "f_car_c.htm")
              ("cadar" "f_car_c.htm")
              ("caddar" "f_car_c.htm")
              ("cadddr" "f_car_c.htm")
              ("caddr" "f_car_c.htm")
              ("cadr" "f_car_c.htm")
              ("call-arguments-limit" "v_call_a.htm")
              ("call-method" "m_call_m.htm")
              ("call-next-method" "f_call_n.htm")
              ("car" "f_car_c.htm")
              ("case" "m_case_.htm")
              ("catch" "s_catch.htm")
              ("ccase" "m_case_.htm")
              ("cdaaar" "f_car_c.htm")
              ("cdaadr" "f_car_c.htm")
              ("cdaar" "f_car_c.htm")
              ("cdadar" "f_car_c.htm")
              ("cdaddr" "f_car_c.htm")
              ("cdadr" "f_car_c.htm")
              ("cdar" "f_car_c.htm")
              ("cddaar" "f_car_c.htm")
              ("cddadr" "f_car_c.htm")
              ("cddar" "f_car_c.htm")
              ("cdddar" "f_car_c.htm")
              ("cddddr" "f_car_c.htm")
              ("cdddr" "f_car_c.htm")
              ("cddr" "f_car_c.htm")
              ("cdr" "f_car_c.htm")
              ("ceiling" "f_floorc.htm")
              ("cell-error" "e_cell_e.htm")
              ("cell-error-name" "f_cell_e.htm")
              ("cerror" "f_cerror.htm")
              ("change-class" "f_chg_cl.htm")
              ("char" "f_char_.htm")
              ("char-code" "f_char_c.htm")
              ("char-code-limit" "v_char_c.htm")
              ("char-downcase" "f_char_u.htm")
              ("char-equal" "f_chareq.htm")
              ("char-greaterp" "f_chareq.htm")
              ("char-int" "f_char_i.htm")
              ("char-lessp" "f_chareq.htm")
              ("char-name" "f_char_n.htm")
              ("char-not-equal" "f_chareq.htm")
              ("char-not-greaterp" "f_chareq.htm")
              ("char-not-lessp" "f_chareq.htm")
              ("char-upcase" "f_char_u.htm")
              ("char/=" "f_chareq.htm")
              ("char<" "f_chareq.htm")
              ("char<=" "f_chareq.htm")
              ("char=" "f_chareq.htm")
              ("char>" "f_chareq.htm")
              ("char>=" "f_chareq.htm")
              ("character" "a_ch.htm")
              ("characterp" "f_chp.htm")
              ("check-type" "m_check_.htm")
              ("cis" "f_cis.htm")
              ("class" "t_class.htm")
              ("class-name" "f_class_.htm")
              ("class-of" "f_clas_1.htm")
              ("clear-input" "f_clear_.htm")
              ("clear-output" "f_finish.htm")
              ("close" "f_close.htm")
              ("clrhash" "f_clrhas.htm")
              ("code-char" "f_code_c.htm")
              ("coerce" "f_coerce.htm")
              ("compilation-speed" "d_optimi.htm")
              ("compile" "f_cmp.htm")
              ("compile-file" "f_cmp_fi.htm")
              ("compile-file-pathname" "f_cmp__1.htm")
              ("compiled-function" "t_cmpd_f.htm")
              ("compiled-function-p" "f_cmpd_f.htm")
              ("compiler-macro" "f_docume.htm")
              ("compiler-macro-function" "f_cmp_ma.htm")
              ("complement" "f_comple.htm")
              ("complex" "a_comple.htm")
              ("complexp" "f_comp_3.htm")
              ("compute-applicable-methods" "f_comput.htm")
              ("compute-restarts" "f_comp_1.htm")
              ("concatenate" "f_concat.htm")
              ("concatenated-stream" "t_concat.htm")
              ("concatenated-stream-streams" "f_conc_1.htm")
              ("cond" "m_cond.htm")
              ("condition" "e_cnd.htm")
              ("conjugate" "f_conjug.htm")
              ("cons" "a_cons.htm")
              ("consp" "f_consp.htm")
              ("constantly" "f_cons_1.htm")
              ("constantp" "f_consta.htm")
              ("continue" "a_contin.htm")
              ("control-error" "e_contro.htm")
              ("copy-alist" "f_cp_ali.htm")
              ("copy-list" "f_cp_lis.htm")
              ("copy-pprint-dispatch" "f_cp_ppr.htm")
              ("copy-readtable" "f_cp_rdt.htm")
              ("copy-seq" "f_cp_seq.htm")
              ("copy-structure" "f_cp_stu.htm")
              ("copy-symbol" "f_cp_sym.htm")
              ("copy-tree" "f_cp_tre.htm")
              ("cos" "f_sin_c.htm")
              ("cosh" "f_sinh_.htm")
              ("count" "f_countc.htm")
              ("count-if" "f_countc.htm")
              ("count-if-not" "f_countc.htm")
              ("ctypecase" "m_tpcase.htm")
              ("debug" "d_optimi.htm")
              ("decf" "m_incf_.htm")
              ("declaim" "m_declai.htm")
              ("declaration" "d_declar.htm")
              ("declare" "s_declar.htm")
              ("decode-float" "f_dec_fl.htm")
              ("decode-universal-time" "f_dec_un.htm")
              ("defclass" "m_defcla.htm")
              ("defconstant" "m_defcon.htm")
              ("defgeneric" "m_defgen.htm")
              ("define-compiler-macro" "m_define.htm")
              ("define-condition" "m_defi_5.htm")
              ("define-method-combination" "m_defi_4.htm")
              ("define-modify-macro" "m_defi_2.htm")
              ("define-setf-expander" "m_defi_3.htm")
              ("define-symbol-macro" "m_defi_1.htm")
              ("defmacro" "m_defmac.htm")
              ("defmethod" "m_defmet.htm")
              ("defpackage" "m_defpkg.htm")
              ("defparameter" "m_defpar.htm")
              ("defsetf" "m_defset.htm")
              ("defstruct" "m_defstr.htm")
              ("deftype" "m_deftp.htm")
              ("defun" "m_defun.htm")
              ("defvar" "m_defpar.htm")
              ("delete" "f_rm_rm.htm")
              ("delete-duplicates" "f_rm_dup.htm")
              ("delete-file" "f_del_fi.htm")
              ("delete-if" "f_rm_rm.htm")
              ("delete-if-not" "f_rm_rm.htm")
              ("delete-package" "f_del_pk.htm")
              ("denominator" "f_numera.htm")
              ("deposit-field" "f_deposi.htm")
              ("describe" "f_descri.htm")
              ("describe-object" "f_desc_1.htm")
              ("destructuring-bind" "m_destru.htm")
              ("digit-char" "f_digit_.htm")
              ("digit-char-p" "f_digi_1.htm")
              ("directory" "f_dir.htm")
              ("directory-namestring" "f_namest.htm")
              ("disassemble" "f_disass.htm")
              ("division-by-zero" "e_divisi.htm")
              ("do" "m_do_do.htm")
              ("do*" "m_do_do.htm")
              ("do-all-symbols" "m_do_sym.htm")
              ("do-external-symbols" "m_do_sym.htm")
              ("do-symbols" "m_do_sym.htm")
              ("documentation" "f_docume.htm")
              ("dolist" "m_dolist.htm")
              ("dotimes" "m_dotime.htm")
              ("double-float" "t_short_.htm")
              ("double-float-epsilon" "v_short_.htm")
              ("double-float-negative-epsilon" "v_short_.htm")
              ("dpb" "f_dpb.htm")
              ("dribble" "f_dribbl.htm")
              ("dynamic-extent" "d_dynami.htm")
              ("ecase" "m_case_.htm")
              ("echo-stream" "t_echo_s.htm")
              ("echo-stream-input-stream" "f_echo_s.htm")
              ("echo-stream-output-stream" "f_echo_s.htm")
              ("ed" "f_ed.htm")
              ("eighth" "f_firstc.htm")
              ("elt" "f_elt.htm")
              ("encode-universal-time" "f_encode.htm")
              ("end-of-file" "e_end_of.htm")
              ("endp" "f_endp.htm")
              ("enough-namestring" "f_namest.htm")
              ("ensure-directories-exist" "f_ensu_1.htm")
              ("ensure-generic-function" "f_ensure.htm")
              ("eq" "f_eq.htm")
              ("eql" "a_eql.htm")
              ("equal" "f_equal.htm")
              ("equalp" "f_equalp.htm")
              ("error" "a_error.htm")
              ("etypecase" "m_tpcase.htm")
              ("eval" "f_eval.htm")
              ("eval-when" "s_eval_w.htm")
              ("evenp" "f_evenpc.htm")
              ("every" "f_everyc.htm")
              ("exp" "f_exp_e.htm")
              ("export" "f_export.htm")
              ("expt" "f_exp_e.htm")
              ("extended-char" "t_extend.htm")
              ("fboundp" "f_fbound.htm")
              ("fceiling" "f_floorc.htm")
              ("fdefinition" "f_fdefin.htm")
              ("ffloor" "f_floorc.htm")
              ("fifth" "f_firstc.htm")
              ("file-author" "f_file_a.htm")
              ("file-error" "e_file_e.htm")
              ("file-error-pathname" "f_file_e.htm")
              ("file-length" "f_file_l.htm")
              ("file-namestring" "f_namest.htm")
              ("file-position" "f_file_p.htm")
              ("file-stream" "t_file_s.htm")
              ("file-string-length" "f_file_s.htm")
              ("file-write-date" "f_file_w.htm")
              ("fill" "f_fill.htm")
              ("fill-pointer" "f_fill_p.htm")
              ("find" "f_find_.htm")
              ("find-all-symbols" "f_find_a.htm")
              ("find-class" "f_find_c.htm")
              ("find-if" "f_find_.htm")
              ("find-if-not" "f_find_.htm")
              ("find-method" "f_find_m.htm")
              ("find-package" "f_find_p.htm")
              ("find-restart" "f_find_r.htm")
              ("find-symbol" "f_find_s.htm")
              ("finish-output" "f_finish.htm")
              ("first" "f_firstc.htm")
              ("fixnum" "t_fixnum.htm")
              ("flet" "s_flet_.htm")
              ("float" "a_float.htm")
              ("float-digits" "f_dec_fl.htm")
              ("float-precision" "f_dec_fl.htm")
              ("float-radix" "f_dec_fl.htm")
              ("float-sign" "f_dec_fl.htm")
              ("floating-point-inexact" "e_floa_1.htm")
              ("floating-point-invalid-operation" "e_floati.htm")
              ("floating-point-overflow" "e_floa_2.htm")
              ("floating-point-underflow" "e_floa_3.htm")
              ("floatp" "f_floatp.htm")
              ("floor" "f_floorc.htm")
              ("fmakunbound" "f_fmakun.htm")
              ("force-output" "f_finish.htm")
              ("format" "f_format.htm")
              ("formatter" "m_format.htm")
              ("fourth" "f_firstc.htm")
              ("fresh-line" "f_terpri.htm")
              ("fround" "f_floorc.htm")
              ("ftruncate" "f_floorc.htm")
              ("ftype" "d_ftype.htm")
              ("funcall" "f_funcal.htm")
              ("function" "a_fn.htm")
              ("function-keywords" "f_fn_kwd.htm")
              ("function-lambda-expression" "f_fn_lam.htm")
              ("functionp" "f_fnp.htm")
              ("gcd" "f_gcd.htm")
              ("generic-function" "t_generi.htm")
              ("gensym" "f_gensym.htm")
              ("gentemp" "f_gentem.htm")
              ("get" "f_get.htm")
              ("get-decoded-time" "f_get_un.htm")
              ("get-dispatch-macro-character" "f_set__1.htm")
              ("get-internal-real-time" "f_get_in.htm")
              ("get-internal-run-time" "f_get__1.htm")
              ("get-macro-character" "f_set_ma.htm")
              ("get-output-stream-string" "f_get_ou.htm")
              ("get-properties" "f_get_pr.htm")
              ("get-setf-expansion" "f_get_se.htm")
              ("get-universal-time" "f_get_un.htm")
              ("getf" "f_getf.htm")
              ("gethash" "f_gethas.htm")
              ("go" "s_go.htm")
              ("graphic-char-p" "f_graphi.htm")
              ("handler-bind" "m_handle.htm")
              ("handler-case" "m_hand_1.htm")
              ("hash-table" "t_hash_t.htm")
              ("hash-table-count" "f_hash_1.htm")
              ("hash-table-p" "f_hash_t.htm")
              ("hash-table-rehash-size" "f_hash_2.htm")
              ("hash-table-rehash-threshold" "f_hash_3.htm")
              ("hash-table-size" "f_hash_4.htm")
              ("hash-table-test" "f_hash_5.htm")
              ("host-namestring" "f_namest.htm")
              ("identity" "f_identi.htm")
              ("if" "s_if.htm")
              ("ignorable" "d_ignore.htm")
              ("ignore" "d_ignore.htm")
              ("ignore-errors" "m_ignore.htm")
              ("imagpart" "f_realpa.htm")
              ("import" "f_import.htm")
              ("in-package" "m_in_pkg.htm")
              ("incf" "m_incf_.htm")
              ("initialize-instance" "f_init_i.htm")
              ("inline" "d_inline.htm")
              ("input-stream-p" "f_in_stm.htm")
              ("inspect" "f_inspec.htm")
              ("integer" "t_intege.htm")
              ("integer-decode-float" "f_dec_fl.htm")
              ("integer-length" "f_intege.htm")
              ("integerp" "f_inte_1.htm")
              ("interactive-stream-p" "f_intera.htm")
              ("intern" "f_intern.htm")
              ("internal-time-units-per-second" "v_intern.htm")
              ("intersection" "f_isec_.htm")
              ("invalid-method-error" "f_invali.htm")
              ("invoke-debugger" "f_invoke.htm")
              ("invoke-restart" "f_invo_1.htm")
              ("invoke-restart-interactively" "f_invo_2.htm")
              ("isqrt" "f_sqrt_.htm")
              ("keyword" "t_kwd.htm")
              ("keywordp" "f_kwdp.htm")
              ("labels" "s_flet_.htm")
              ("lambda" "a_lambda.htm")
              ("lambda-list-keywords" "v_lambda.htm")
              ("lambda-parameters-limit" "v_lamb_1.htm")
              ("last" "f_last.htm")
              ("lcm" "f_lcm.htm")
              ("ldb" "f_ldb.htm")
              ("ldb-test" "f_ldb_te.htm")
              ("ldiff" "f_ldiffc.htm")
              ("least-negative-double-float" "v_most_1.htm")
              ("least-negative-long-float" "v_most_1.htm")
              ("least-negative-normalized-double-float" "v_most_1.htm")
              ("least-negative-normalized-long-float" "v_most_1.htm")
              ("least-negative-normalized-short-float" "v_most_1.htm")
              ("least-negative-normalized-single-float" "v_most_1.htm")
              ("least-negative-short-float" "v_most_1.htm")
              ("least-negative-single-float" "v_most_1.htm")
              ("least-positive-double-float" "v_most_1.htm")
              ("least-positive-long-float" "v_most_1.htm")
              ("least-positive-normalized-double-float" "v_most_1.htm")
              ("least-positive-normalized-long-float" "v_most_1.htm")
              ("least-positive-normalized-short-float" "v_most_1.htm")
              ("least-positive-normalized-single-float" "v_most_1.htm")
              ("least-positive-short-float" "v_most_1.htm")
              ("least-positive-single-float" "v_most_1.htm")
              ("length" "f_length.htm")
              ("let" "s_let_l.htm")
              ("let*" "s_let_l.htm")
              ("lisp-implementation-type" "f_lisp_i.htm")
              ("lisp-implementation-version" "f_lisp_i.htm")
              ("list" "a_list.htm")
              ("list*" "f_list_.htm")
              ("list-all-packages" "f_list_a.htm")
              ("list-length" "f_list_l.htm")
              ("listen" "f_listen.htm")
              ("listp" "f_listp.htm")
              ("load" "f_load.htm")
              ("load-logical-pathname-translations" "f_ld_log.htm")
              ("load-time-value" "s_ld_tim.htm")
              ("locally" "s_locall.htm")
              ("log" "f_log.htm")
              ("logand" "f_logand.htm")
              ("logandc1" "f_logand.htm")
              ("logandc2" "f_logand.htm")
              ("logbitp" "f_logbtp.htm")
              ("logcount" "f_logcou.htm")
              ("logeqv" "f_logand.htm")
              ("logical-pathname" "a_logica.htm")
              ("logical-pathname-translations" "f_logica.htm")
              ("logior" "f_logand.htm")
              ("lognand" "f_logand.htm")
              ("lognor" "f_logand.htm")
              ("lognot" "f_logand.htm")
              ("logorc1" "f_logand.htm")
              ("logorc2" "f_logand.htm")
              ("logtest" "f_logtes.htm")
              ("logxor" "f_logand.htm")
              ("long-float" "t_short_.htm")
              ("long-float-epsilon" "v_short_.htm")
              ("long-float-negative-epsilon" "v_short_.htm")
              ("long-site-name" "f_short_.htm")
              ("loop" "m_loop.htm")
              ("loop-finish" "m_loop_f.htm")
              ("lower-case-p" "f_upper_.htm")
              ("machine-instance" "f_mach_i.htm")
              ("machine-type" "f_mach_t.htm")
              ("machine-version" "f_mach_v.htm")
              ("macro-function" "f_macro_.htm")
              ("macroexpand" "f_mexp_.htm")
              ("macroexpand-1" "f_mexp_.htm")
              ("macrolet" "s_flet_.htm")
              ("make-array" "f_mk_ar.htm")
              ("make-broadcast-stream" "f_mk_bro.htm")
              ("make-concatenated-stream" "f_mk_con.htm")
              ("make-condition" "f_mk_cnd.htm")
              ("make-dispatch-macro-character" "f_mk_dis.htm")
              ("make-echo-stream" "f_mk_ech.htm")
              ("make-hash-table" "f_mk_has.htm")
              ("make-instance" "f_mk_ins.htm")
              ("make-instances-obsolete" "f_mk_i_1.htm")
              ("make-list" "f_mk_lis.htm")
              ("make-load-form" "f_mk_ld_.htm")
              ("make-load-form-saving-slots" "f_mk_l_1.htm")
              ("make-method" "m_call_m.htm")
              ("make-package" "f_mk_pkg.htm")
              ("make-pathname" "f_mk_pn.htm")
              ("make-random-state" "f_mk_rnd.htm")
              ("make-sequence" "f_mk_seq.htm")
              ("make-string" "f_mk_stg.htm")
              ("make-string-input-stream" "f_mk_s_1.htm")
              ("make-string-output-stream" "f_mk_s_2.htm")
              ("make-symbol" "f_mk_sym.htm")
              ("make-synonym-stream" "f_mk_syn.htm")
              ("make-two-way-stream" "f_mk_two.htm")
              ("makunbound" "f_makunb.htm")
              ("map" "f_map.htm")
              ("map-into" "f_map_in.htm")
              ("mapc" "f_mapc_.htm")
              ("mapcan" "f_mapc_.htm")
              ("mapcar" "f_mapc_.htm")
              ("mapcon" "f_mapc_.htm")
              ("maphash" "f_maphas.htm")
              ("mapl" "f_mapc_.htm")
              ("maplist" "f_mapc_.htm")
              ("mask-field" "f_mask_f.htm")
              ("max" "f_max_m.htm")
              ("member" "a_member.htm")
              ("member-if" "f_mem_m.htm")
              ("member-if-not" "f_mem_m.htm")
              ("merge" "f_merge.htm")
              ("merge-pathnames" "f_merge_.htm")
              ("method" "t_method.htm")
              ("method-combination" "a_method.htm")
              ("method-combination-error" "f_meth_1.htm")
              ("method-qualifiers" "f_method.htm")
              ("min" "f_max_m.htm")
              ("minusp" "f_minusp.htm")
              ("mismatch" "f_mismat.htm")
              ("mod" "a_mod.htm")
              ("most-negative-double-float" "v_most_1.htm")
              ("most-negative-fixnum" "v_most_p.htm")
              ("most-negative-long-float" "v_most_1.htm")
              ("most-negative-short-float" "v_most_1.htm")
              ("most-negative-single-float" "v_most_1.htm")
              ("most-positive-double-float" "v_most_1.htm")
              ("most-positive-fixnum" "v_most_p.htm")
              ("most-positive-long-float" "v_most_1.htm")
              ("most-positive-short-float" "v_most_1.htm")
              ("most-positive-single-float" "v_most_1.htm")
              ("muffle-warning" "a_muffle.htm")
              ("multiple-value-bind" "m_multip.htm")
              ("multiple-value-call" "s_multip.htm")
              ("multiple-value-list" "m_mult_1.htm")
              ("multiple-value-prog1" "s_mult_1.htm")
              ("multiple-value-setq" "m_mult_2.htm")
              ("multiple-values-limit" "v_multip.htm")
              ("name-char" "f_name_c.htm")
              ("namestring" "f_namest.htm")
              ("nbutlast" "f_butlas.htm")
              ("nconc" "f_nconc.htm")
              ("next-method-p" "f_next_m.htm")
              ("nil" "a_nil.htm")
              ("nintersection" "f_isec_.htm")
              ("ninth" "f_firstc.htm")
              ("no-applicable-method" "f_no_app.htm")
              ("no-next-method" "f_no_nex.htm")
              ("not" "a_not.htm")
              ("notany" "f_everyc.htm")
              ("notevery" "f_everyc.htm")
              ("notinline" "d_inline.htm")
              ("nreconc" "f_revapp.htm")
              ("nreverse" "f_revers.htm")
              ("nset-difference" "f_set_di.htm")
              ("nset-exclusive-or" "f_set_ex.htm")
              ("nstring-capitalize" "f_stg_up.htm")
              ("nstring-downcase" "f_stg_up.htm")
              ("nstring-upcase" "f_stg_up.htm")
              ("nsublis" "f_sublis.htm")
              ("nsubst" "f_substc.htm")
              ("nsubst-if" "f_substc.htm")
              ("nsubst-if-not" "f_substc.htm")
              ("nsubstitute" "f_sbs_s.htm")
              ("nsubstitute-if" "f_sbs_s.htm")
              ("nsubstitute-if-not" "f_sbs_s.htm")
              ("nth" "f_nth.htm")
              ("nth-value" "m_nth_va.htm")
              ("nthcdr" "f_nthcdr.htm")
              ("null" "a_null.htm")
              ("number" "t_number.htm")
              ("numberp" "f_nump.htm")
              ("numerator" "f_numera.htm")
              ("nunion" "f_unionc.htm")
              ("oddp" "f_evenpc.htm")
              ("open" "f_open.htm")
              ("open-stream-p" "f_open_s.htm")
              ("optimize" "d_optimi.htm")
              ("or" "a_or.htm")
              ("otherwise" "m_case_.htm")
              ("output-stream-p" "f_in_stm.htm")
              ("package" "t_pkg.htm")
              ("package-error" "e_pkg_er.htm")
              ("package-error-package" "f_pkg_er.htm")
              ("package-name" "f_pkg_na.htm")
              ("package-nicknames" "f_pkg_ni.htm")
              ("package-shadowing-symbols" "f_pkg_sh.htm")
              ("package-use-list" "f_pkg_us.htm")
              ("package-used-by-list" "f_pkg__1.htm")
              ("packagep" "f_pkgp.htm")
              ("pairlis" "f_pairli.htm")
              ("parse-error" "e_parse_.htm")
              ("parse-integer" "f_parse_.htm")
              ("parse-namestring" "f_pars_1.htm")
              ("pathname" "a_pn.htm")
              ("pathname-device" "f_pn_hos.htm")
              ("pathname-directory" "f_pn_hos.htm")
              ("pathname-host" "f_pn_hos.htm")
              ("pathname-match-p" "f_pn_mat.htm")
              ("pathname-name" "f_pn_hos.htm")
              ("pathname-type" "f_pn_hos.htm")
              ("pathname-version" "f_pn_hos.htm")
              ("pathnamep" "f_pnp.htm")
              ("peek-char" "f_peek_c.htm")
              ("phase" "f_phase.htm")
              ("pi" "v_pi.htm")
              ("plusp" "f_minusp.htm")
              ("pop" "m_pop.htm")
              ("position" "f_pos_p.htm")
              ("position-if" "f_pos_p.htm")
              ("position-if-not" "f_pos_p.htm")
              ("pprint" "f_wr_pr.htm")
              ("pprint-dispatch" "f_ppr_di.htm")
              ("pprint-exit-if-list-exhausted" "m_ppr_ex.htm")
              ("pprint-fill" "f_ppr_fi.htm")
              ("pprint-indent" "f_ppr_in.htm")
              ("pprint-linear" "f_ppr_fi.htm")
              ("pprint-logical-block" "m_ppr_lo.htm")
              ("pprint-newline" "f_ppr_nl.htm")
              ("pprint-pop" "m_ppr_po.htm")
              ("pprint-tab" "f_ppr_ta.htm")
              ("pprint-tabular" "f_ppr_fi.htm")
              ("prin1" "f_wr_pr.htm")
              ("prin1-to-string" "f_wr_to_.htm")
              ("princ" "f_wr_pr.htm")
              ("princ-to-string" "f_wr_to_.htm")
              ("print" "f_wr_pr.htm")
              ("print-not-readable" "e_pr_not.htm")
              ("print-not-readable-object" "f_pr_not.htm")
              ("print-object" "f_pr_obj.htm")
              ("print-unreadable-object" "m_pr_unr.htm")
              ("probe-file" "f_probe_.htm")
              ("proclaim" "f_procla.htm")
              ("prog" "m_prog_.htm")
              ("prog*" "m_prog_.htm")
              ("prog1" "m_prog1c.htm")
              ("prog2" "m_prog1c.htm")
              ("progn" "s_progn.htm")
              ("program-error" "e_progra.htm")
              ("progv" "s_progv.htm")
              ("provide" "f_provid.htm")
              ("psetf" "m_setf_.htm")
              ("psetq" "m_psetq.htm")
              ("push" "m_push.htm")
              ("pushnew" "m_pshnew.htm")
              ("quote" "s_quote.htm")
              ("random" "f_random.htm")
              ("random-state" "t_rnd_st.htm")
              ("random-state-p" "f_rnd_st.htm")
              ("rassoc" "f_rassoc.htm")
              ("rassoc-if" "f_rassoc.htm")
              ("rassoc-if-not" "f_rassoc.htm")
              ("ratio" "t_ratio.htm")
              ("rational" "a_ration.htm")
              ("rationalize" "f_ration.htm")
              ("rationalp" "f_rati_1.htm")
              ("read" "f_rd_rd.htm")
              ("read-byte" "f_rd_by.htm")
              ("read-char" "f_rd_cha.htm")
              ("read-char-no-hang" "f_rd_c_1.htm")
              ("read-delimited-list" "f_rd_del.htm")
              ("read-from-string" "f_rd_fro.htm")
              ("read-line" "f_rd_lin.htm")
              ("read-preserving-whitespace" "f_rd_rd.htm")
              ("read-sequence" "f_rd_seq.htm")
              ("reader-error" "e_rder_e.htm")
              ("readtable" "t_rdtabl.htm")
              ("readtable-case" "f_rdtabl.htm")
              ("readtablep" "f_rdta_1.htm")
              ("real" "t_real.htm")
              ("realp" "f_realp.htm")
              ("realpart" "f_realpa.htm")
              ("reduce" "f_reduce.htm")
              ("reinitialize-instance" "f_reinit.htm")
              ("rem" "f_mod_r.htm")
              ("remf" "m_remf.htm")
              ("remhash" "f_remhas.htm")
              ("remove" "f_rm_rm.htm")
              ("remove-duplicates" "f_rm_dup.htm")
              ("remove-if" "f_rm_rm.htm")
              ("remove-if-not" "f_rm_rm.htm")
              ("remove-method" "f_rm_met.htm")
              ("remprop" "f_rempro.htm")
              ("rename-file" "f_rn_fil.htm")
              ("rename-package" "f_rn_pkg.htm")
              ("replace" "f_replac.htm")
              ("require" "f_provid.htm")
              ("rest" "f_rest.htm")
              ("restart" "t_rst.htm")
              ("restart-bind" "m_rst_bi.htm")
              ("restart-case" "m_rst_ca.htm")
              ("restart-name" "f_rst_na.htm")
              ("return" "m_return.htm")
              ("return-from" "s_ret_fr.htm")
              ("revappend" "f_revapp.htm")
              ("reverse" "f_revers.htm")
              ("room" "f_room.htm")
              ("rotatef" "m_rotate.htm")
              ("round" "f_floorc.htm")
              ("row-major-aref" "f_row_ma.htm")
              ("rplaca" "f_rplaca.htm")
              ("rplacd" "f_rplaca.htm")
              ("safety" "d_optimi.htm")
              ("satisfies" "t_satisf.htm")
              ("sbit" "f_bt_sb.htm")
              ("scale-float" "f_dec_fl.htm")
              ("schar" "f_char_.htm")
              ("search" "f_search.htm")
              ("second" "f_firstc.htm")
              ("sequence" "t_seq.htm")
              ("serious-condition" "e_seriou.htm")
              ("set" "f_set.htm")
              ("set-difference" "f_set_di.htm")
              ("set-dispatch-macro-character" "f_set__1.htm")
              ("set-exclusive-or" "f_set_ex.htm")
              ("set-macro-character" "f_set_ma.htm")
              ("set-pprint-dispatch" "f_set_pp.htm")
              ("set-syntax-from-char" "f_set_sy.htm")
              ("setf" "a_setf.htm")
              ("setq" "s_setq.htm")
              ("seventh" "f_firstc.htm")
              ("shadow" "f_shadow.htm")
              ("shadowing-import" "f_shdw_i.htm")
              ("shared-initialize" "f_shared.htm")
              ("shiftf" "m_shiftf.htm")
              ("short-float" "t_short_.htm")
              ("short-float-epsilon" "v_short_.htm")
              ("short-float-negative-epsilon" "v_short_.htm")
              ("short-site-name" "f_short_.htm")
              ("signal" "f_signal.htm")
              ("signed-byte" "t_sgn_by.htm")
              ("signum" "f_signum.htm")
              ("simple-array" "t_smp_ar.htm")
              ("simple-base-string" "t_smp_ba.htm")
              ("simple-bit-vector" "t_smp_bt.htm")
              ("simple-bit-vector-p" "f_smp_bt.htm")
              ("simple-condition" "e_smp_cn.htm")
              ("simple-condition-format-arguments" "f_smp_cn.htm")
              ("simple-condition-format-control" "f_smp_cn.htm")
              ("simple-error" "e_smp_er.htm")
              ("simple-string" "t_smp_st.htm")
              ("simple-string-p" "f_smp_st.htm")
              ("simple-type-error" "e_smp_tp.htm")
              ("simple-vector" "t_smp_ve.htm")
              ("simple-vector-p" "f_smp_ve.htm")
              ("simple-warning" "e_smp_wa.htm")
              ("sin" "f_sin_c.htm")
              ("single-float" "t_short_.htm")
              ("single-float-epsilon" "v_short_.htm")
              ("single-float-negative-epsilon" "v_short_.htm")
              ("sinh" "f_sinh_.htm")
              ("sixth" "f_firstc.htm")
              ("sleep" "f_sleep.htm")
              ("slot-boundp" "f_slt_bo.htm")
              ("slot-exists-p" "f_slt_ex.htm")
              ("slot-makunbound" "f_slt_ma.htm")
              ("slot-missing" "f_slt_mi.htm")
              ("slot-unbound" "f_slt_un.htm")
              ("slot-value" "f_slt_va.htm")
              ("software-type" "f_sw_tpc.htm")
              ("software-version" "f_sw_tpc.htm")
              ("some" "f_everyc.htm")
              ("sort" "f_sort_.htm")
              ("space" "d_optimi.htm")
              ("special" "d_specia.htm")
              ("special-operator-p" "f_specia.htm")
              ("speed" "d_optimi.htm")
              ("sqrt" "f_sqrt_.htm")
              ("stable-sort" "f_sort_.htm")
              ("standard" "07_ffb.htm")
              ("standard-char" "t_std_ch.htm")
              ("standard-char-p" "f_std_ch.htm")
              ("standard-class" "t_std_cl.htm")
              ("standard-generic-function" "t_std_ge.htm")
              ("standard-method" "t_std_me.htm")
              ("standard-object" "t_std_ob.htm")
              ("step" "m_step.htm")
              ("storage-condition" "e_storag.htm")
              ("store-value" "a_store_.htm")
              ("stream" "t_stream.htm")
              ("stream-element-type" "f_stm_el.htm")
              ("stream-error" "e_stm_er.htm")
              ("stream-error-stream" "f_stm_er.htm")
              ("stream-external-format" "f_stm_ex.htm")
              ("streamp" "f_stmp.htm")
              ("string" "a_string.htm")
              ("string-capitalize" "f_stg_up.htm")
              ("string-downcase" "f_stg_up.htm")
              ("string-equal" "f_stgeq_.htm")
              ("string-greaterp" "f_stgeq_.htm")
              ("string-left-trim" "f_stg_tr.htm")
              ("string-lessp" "f_stgeq_.htm")
              ("string-not-equal" "f_stgeq_.htm")
              ("string-not-greaterp" "f_stgeq_.htm")
              ("string-not-lessp" "f_stgeq_.htm")
              ("string-right-trim" "f_stg_tr.htm")
              ("string-stream" "t_stg_st.htm")
              ("string-trim" "f_stg_tr.htm")
              ("string-upcase" "f_stg_up.htm")
              ("string/=" "f_stgeq_.htm")
              ("string<" "f_stgeq_.htm")
              ("string<=" "f_stgeq_.htm")
              ("string=" "f_stgeq_.htm")
              ("string>" "f_stgeq_.htm")
              ("string>=" "f_stgeq_.htm")
              ("stringp" "f_stgp.htm")
              ("structure" "f_docume.htm")
              ("structure-class" "t_stu_cl.htm")
              ("structure-object" "t_stu_ob.htm")
              ("style-warning" "e_style_.htm")
              ("sublis" "f_sublis.htm")
              ("subseq" "f_subseq.htm")
              ("subsetp" "f_subset.htm")
              ("subst" "f_substc.htm")
              ("subst-if" "f_substc.htm")
              ("subst-if-not" "f_substc.htm")
              ("substitute" "f_sbs_s.htm")
              ("substitute-if" "f_sbs_s.htm")
              ("substitute-if-not" "f_sbs_s.htm")
              ("subtypep" "f_subtpp.htm")
              ("svref" "f_svref.htm")
              ("sxhash" "f_sxhash.htm")
              ("symbol" "t_symbol.htm")
              ("symbol-function" "f_symb_1.htm")
              ("symbol-macrolet" "s_symbol.htm")
              ("symbol-name" "f_symb_2.htm")
              ("symbol-package" "f_symb_3.htm")
              ("symbol-plist" "f_symb_4.htm")
              ("symbol-value" "f_symb_5.htm")
              ("symbolp" "f_symbol.htm")
              ("synonym-stream" "t_syn_st.htm")
              ("synonym-stream-symbol" "f_syn_st.htm")
              ("t" "a_t.htm")
              ("tagbody" "s_tagbod.htm")
              ("tailp" "f_ldiffc.htm")
              ("tan" "f_sin_c.htm")
              ("tanh" "f_sinh_.htm")
              ("tenth" "f_firstc.htm")
              ("terpri" "f_terpri.htm")
              ("the" "s_the.htm")
              ("third" "f_firstc.htm")
              ("throw" "s_throw.htm")
              ("time" "m_time.htm")
              ("trace" "m_tracec.htm")
              ("translate-logical-pathname" "f_tr_log.htm")
              ("translate-pathname" "f_tr_pn.htm")
              ("tree-equal" "f_tree_e.htm")
              ("truename" "f_tn.htm")
              ("truncate" "f_floorc.htm")
              ("two-way-stream" "t_two_wa.htm")
              ("two-way-stream-input-stream" "f_two_wa.htm")
              ("two-way-stream-output-stream" "f_two_wa.htm")
              ("type" "a_type.htm")
              ("type-error" "e_tp_err.htm")
              ("type-error-datum" "f_tp_err.htm")
              ("type-error-expected-type" "f_tp_err.htm")
              ("type-of" "f_tp_of.htm")
              ("typecase" "m_tpcase.htm")
              ("typep" "f_typep.htm")
              ("unbound-slot" "e_unboun.htm")
              ("unbound-slot-instance" "f_unboun.htm")
              ("unbound-variable" "e_unbo_1.htm")
              ("undefined-function" "e_undefi.htm")
              ("unexport" "f_unexpo.htm")
              ("unintern" "f_uninte.htm")
              ("union" "f_unionc.htm")
              ("unless" "m_when_.htm")
              ("unread-char" "f_unrd_c.htm")
              ("unsigned-byte" "t_unsgn_.htm")
              ("untrace" "m_tracec.htm")
              ("unuse-package" "f_unuse_.htm")
              ("unwind-protect" "s_unwind.htm")
              ("update-instance-for-different-class" "f_update.htm")
              ("update-instance-for-redefined-class" "f_upda_1.htm")
              ("upgraded-array-element-type" "f_upgr_1.htm")
              ("upgraded-complex-part-type" "f_upgrad.htm")
              ("upper-case-p" "f_upper_.htm")
              ("use-package" "f_use_pk.htm")
              ("use-value" "a_use_va.htm")
              ("user-homedir-pathname" "f_user_h.htm")
              ("values" "a_values.htm")
              ("values-list" "f_vals_l.htm")
              ("variable" "f_docume.htm")
              ("vector" "a_vector.htm")
              ("vector-pop" "f_vec_po.htm")
              ("vector-push" "f_vec_ps.htm")
              ("vector-push-extend" "f_vec_ps.htm")
              ("vectorp" "f_vecp.htm")
              ("warn" "f_warn.htm")
              ("warning" "e_warnin.htm")
              ("when" "m_when_.htm")
              ("wild-pathname-p" "f_wild_p.htm")
              ("with-accessors" "m_w_acce.htm")
              ("with-compilation-unit" "m_w_comp.htm")
              ("with-condition-restarts" "m_w_cnd_.htm")
              ("with-hash-table-iterator" "m_w_hash.htm")
              ("with-input-from-string" "m_w_in_f.htm")
              ("with-open-file" "m_w_open.htm")
              ("with-open-stream" "m_w_op_1.htm")
              ("with-output-to-string" "m_w_out_.htm")
              ("with-package-iterator" "m_w_pkg_.htm")
              ("with-simple-restart" "m_w_smp_.htm")
              ("with-slots" "m_w_slts.htm")
              ("with-standard-io-syntax" "m_w_std_.htm")
              ("write" "f_wr_pr.htm")
              ("write-byte" "f_wr_by.htm")
              ("write-char" "f_wr_cha.htm")
              ("write-line" "f_wr_stg.htm")
              ("write-sequence" "f_wr_seq.htm")
              ("write-string" "f_wr_stg.htm")
              ("write-to-string" "f_wr_to_.htm")
              ("y-or-n-p" "f_y_or_n.htm")
              ("yes-or-no-p" "f_y_or_n.htm")
              ("zerop" "f_zerop.htm")))))
    ;; :NOTE Following are the `common-lisp-hyperspec-reader-macros'
    ;; with addition of for Hspec v3 entries.     
    (setq *clhs-symbol-v3-or-v7*
          (append *clhs-symbol-v3-or-v7*
                  (case (length (cadr (assoc-string "&whole" *clhs-symbol-v3-or-v7*)))
                    (13 ;; (eq (length "sec_3-4-4.htm") 13) <- Hspec v3
                     '(("#"   "sec_2-4-8.html")    ;; 2.4.8
                       ("##"  "sec_2-4-16.html")   ;; 2.4.8.16   Sharpsign Sharpsign
                       ("#'"  "sec_2-4-2.html")    ;; 2.4.8.2    Sharpsign Single-Quote
                       ("#("  "sec_2-4-3.html")    ;; 2.4.8.3    Sharpsign Left-Parenthesis
                       ("#)"  "sec_2-4-22.html")   ;; 2.4.8.22   Sharpsign Right-Parenthesis
                       ("#*"  "sec_2-4-8-4.html" ) ;; 2.4.8.4    Sharpsign Asterisk
                       ("#:"  "sec_2-4-8-5.html" ) ;; 2.4.8.5    Sharpsign Colon
                       ("#."  "sec_2-4-8-6.html" ) ;; 2.4.8.6    Sharpsign Dot
                       ("#="  "sec_2-4-8-15.html") ;; 2.4.8.15   Sharpsign Equal-Sign
                       ("#+"  "sec_2-4-8-17.html") ;; 2.4.8.17   Sharpsign Plus
                       ("#-"  "sec_2-4-8-18.html") ;; 2.4.8.18   Sharpsign Minus
                       ("#<"  "sec_2-4-8-20.html") ;; 2.4.8.20   Sharpsign Less-Than-Sign
                       ("#A"  "sec_2-4-8-12.html") ;; 2.4.8.12   Sharpsign A
                       ("#B"  "sec_2-4-8-7.html" ) ;; 2.4.8.7    Sharpsign B
                       ("#C"  "sec_2-4-8-11.html") ;; 2.4.8.11   Sharpsign C
                       ("#O"  "sec_2-4-8-8.html" ) ;; 2.4.8.8    Sharpsign O
                       ("#P"  "sec_2-4-8-14.html") ;; 2.4.8.14   Sharpsign P
                       ("#R"  "sec_2-4-8-10.html") ;; 2.4.8.10   Sharpsign R
                       ("#S"  "sec_2-4-8-13.html") ;; 2.4.8.13   Sharpsign S
                       ("#X"  "sec_2-4-8-9.html" ) ;; 2.4.8.9    Sharpsign X
                       ("#\\" "sec_2-4-8-1.html" ) ;; 2.4.8.1    Sharpsign Backslash
                       ("#|"  "sec_2-4-8-19.html") ;; 2.4.8.19   Sharpsign Vertical-Bar
                       ("\""  "sec_2-4-8-21.html") ;; 2.4.8.21   Sharpsign Whitespace
                       ("'"   "sec_2-4-3.html")    ;; 2.4.3      Single-Quote
                       ("`"   "sec_2-4-6.html")    ;; 2.4.6      Backquote
                       (","   "sec_2-4-7.html")    ;; 2.4.7      Comma
                       ("("   "sec_2-4-1.html")    ;; 2.4.1      Left-Parenthesis
                       (")"   "sec_2-4-2.html")    ;; 2.4.2      Right-Parenthesis
                       (";"   "sec_2-4-4.html")    ;; 2.4.4      Semicolon
                       ))
                    (9 ;; (eq (length "03_dd.htm") 9) <- Hspec v7
                     '(("#" "02_dh.htm") ("##" "02_dhp.htm") ("#'" "02_dhb.htm")
                       ("#(" "02_dhc.htm") ("#*" "02_dhd.htm") ("#:" "02_dhe.htm")
                       ("#." "02_dhf.htm") ("#=" "02_dho.htm") ("#+" "02_dhq.htm")
                       ("#-" "02_dhr.htm") ("#<" "02_dht.htm") ("#A" "02_dhl.htm")
                       ("#B" "02_dhg.htm") ("#C" "02_dhk.htm") ("#O" "02_dhh.htm")
                       ("#P" "02_dhn.htm") ("#R" "02_dhj.htm") ("#S" "02_dhm.htm")
                       ("#X" "02_dhi.htm") ("#\\" "02_dha.htm") ("#|" "02_dhs.htm")
                       ("\"" "02_de.htm") ("'" "02_dc.htm") ("`" "02_df.htm") 
                       ("," "02_dg.htm") ("(" "02_da.htm") (")" "02_db.htm") 
                       (";" "02_dd.htm"))) ) )) )) ;)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-20T16:19:35-04:00Z}#{10116} - by MON KEY>
(defun mon-help-utils-CL-loadtime ()
  "Loadtime function to unbind symbols from mon-doc-help-CL.el.\n
Unbind `*clhs-symbol-v3-or-v7*' as variable `*mon-cl-symbols*' holds a hashtable
of all CL symbol/Hspec mappings at loadtime.\n
SEE-ALSO `mon-help-utils-CL-loadtime', `mon-after-mon-utils-loadtime',
`mon-check-feature-for-loadtime', `mon-bind-nefs-photos-at-loadtime',
`mon-bind-cifs-vars-at-loadtime',
`mon-bind-doc-help-proprietery-vars-at-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-set-register-tags-loadtime',
`mon-CL-cln-colon-swap'.\n►►►"
  (progn 
    (makunbound '*clhs-symbol-v3-or-v7*)
    (unintern '*clhs-symbol-v3-or-v7*)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-29T00:47:51-05:00Z}#{10045} - by MON>
(defvar *mon-cl-symbols* nil
  "List of Common-Lisp symbol names mapped to their respective Hspec .html files.\n
Bound at compile/loadtime with var `*clhs-symbol-v3-or-v7*' according to the
value of `common-lisp-hyperspec-root'.\n
:NOTE `*clhs-symbol-v3-or-v7*' unloaded with `mon-help-utils-CL-loadtime'.\n
:SEE-ALSO `mon-help-CL-symbols', `common-lisp-hyperspec-format-characters'
`common-lisp-hyperspec-reader-macros', `common-lisp-hyperspec-root'.\n►►►")
;;;
(unless (bound-and-true-p *mon-cl-symbols*)
  (setq *mon-cl-symbols* (make-hash-table :test #'equal :size 1024))
  (mapc #'(lambda (k) (puthash (car k) (cadr k) *mon-cl-symbols*)) *clhs-symbol-v3-or-v7*))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-29T00:45:31-05:00Z}#{10045} - by MON>
(defun mon-help-CL-symbols (&optional cl-symbol-string ffox intrp)
  "Completion function for the Common Lisp symbols in the hspec.\n
CL-SYMBOL-STRING is a string to associate with a value in `*mon-cl-symbols*'.\n
When `IS-MON-P-GNU' or a w3m executable is in path and `w3m-browse-url' is fboundp
browse th hspec in Emacsw3m.\n
When FFOX is non-nil or called-interactively with prefix arg ensure browsing with firefox.
:EXAMPLE\n\n\(mon-help-CL-symbols \"defclass\"\)\n\(mon-help-CL-symbols \"#<\"\)
\(mon-help-CL-symbols nil nil\)\n\(mon-help-CL-symbols \"defclass\" t\)
 \(mon-help-CL-symbols nil nil t\)\n\(apply 'mon-help-CL-symbols nil nil '\(t\)\)\n
:NOTE When using Hspec v3 and Emacs-w3m you may want to comment out the
java-applet e.g.\n
 <APPLET HEIGHT=80 WIDTH=450 CODE=\"CLIndex.class\" CODEBASE=\"../Data/\"></APPLET>\n
:SEE :FILE <`common-lisp-hyperspec-root'>FrontMatter/Symbol-Index.html\n
:SEE-ALSO `*mon-cl-symbols*', `mon-help-utils-CL-loadtime',
`mon-CL-package-complete', `quicklisp-system-complete',
`mon-help-CL-pkgs'.\n►►►"
  (interactive "\i\nP\np")
  ;;; :WAS (let ((rd-cl-sym (cond ((and cl-symbol-string (stringp cl-symbol-string))
  ;;;                         (member cl-symbol-string *mon-cl-symbols*)
  ;;;                         (cadr (assoc-string cl-symbol-string *clhs-symbol-v3-or-v7*)))
  ;;;                        ((or intrp t)
  ;;;                         (cadr (assoc (completing-read "cl-cymbol :" *mon-cl-symbols*) 
  ;;;                                      *clhs-symbol-v3-or-v7*))))))
  (let ((rd-cl-sym (cond ((and cl-symbol-string (stringp cl-symbol-string))
                          (gethash cl-symbol-string *mon-cl-symbols*))
                         ((or intrp t) (gethash 
                                        (completing-read "cl-cymbol :" *mon-cl-symbols*)
                                        *mon-cl-symbols*)))))
    (setq rd-cl-sym (concat 
                     (unless (or (string-match-p "file://" common-lisp-hyperspec-root) 
                                 (string-match-p "http://" common-lisp-hyperspec-root))
                       "file://")
                     common-lisp-hyperspec-root "Body/" rd-cl-sym))
    (cond (ffox (browse-url-firefox rd-cl-sym))
          ((or (bound-and-true-p IS-MON-P-GNU)
               (and (executable-find "w3m") 
                    (intern-soft "w3m-browse-url")
                    (fboundp 'w3m-browse-url)))
           (w3m-browse-url rd-cl-sym))
          (t (browse-url-generic rd-cl-sym)))))
;; 
(defalias 'mon-help-cl-symbols 'mon-help-CL-symbols)
(defalias 'mon-hyperspec-lookup 'mon-help-CL-symbols)
;;
;;; :TEST-ME (mon-help-CL-symbols "defclass")
;;; :TEST-ME (mon-help-CL-symbols "#<")
;;; :TEST-ME (mon-help-CL-symbols nil nil)
;;; :TEST-ME (mon-help-CL-symbols "defclass" t)
;;; :TEST-ME (mon-help-CL-symbols nil nil t)

;;; ==============================
(provide 'mon-doc-help-CL)
;;; ==============================

;;; ================================================================
;;; mon-doc-help-CL.el ends here
;;; EOF

;;; Following is a list of Common Lisp symbols which coref Egnlish Natural language.
;;; Approx. 1 in 7 of the ~1000 CL symbols.
;;; (abort adjoin and append apply array assert atom bit boolean break byte car
;;;  class close coerce compile complement complex concatenate condition conjugate
;;;  constantly continue count debug declare defvar delete denominator describe do
;;;  documentation dribble eighth equal error eval every export fifth fill find
;;;  first fixnum flet float floor format formatter fourth function get go identity
;;;  if ignorable ignore import inline inspect integer intern keyword labels last
;;;  length let listen load locally log loop map member merge method mismatch not
;;;  number numerator open optimize or otherwise package phase pop print proclaim
;;;  push quote random ratio read real reduce remove replace require rest restart
;;;  return room rotatef round safety satisfies search second set seventh shadow
;;;  signal sin sixth sleep some sort space special speed standard step stream
;;;  string structure svref symbol tenth the third throw time trace truncate type
;;;  union unless values variable vector warn warning when write)

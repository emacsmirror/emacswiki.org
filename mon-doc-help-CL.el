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
;;; `mon-hspec-prop-type', `mon-hspec-stk-n-mv'
;;; `mon-hspec-parse-w3m',`mon-hspec-out'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*cl-cmu-ai-repo*',`*cl-ext-pkg-map*'
;;; `*mon-hs-temp-buffer*', `*mon-hs-root-dir*'
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
    (X3J13-archive
     "Working files for the X3J13 ANSI Common Lisp committee - cannonical archive."
     ;; ftp://ftp.parc.xerox.com/pub/cl/cleanup/issue-status
     "ftp://ftp.parc.xerox.com/pub/cl/")
    (cl-ansi-naggum
     "ANSI X3.226-1994 final draft PostScript conversion landscape orientation 2up.\n:COURTESY Erik Naggum"
     ;; http://naggum.no/ANSI-CL/
     "http://naggum.no/ANSI-CL.tar.gz")
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
     "Common Lisp Hyperspec v3 the variable length version circa 1996."
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
;;; As such while other more contemporary programmin languages and associated
;;; technical documents allow re-purposing the _format_ of their respective
;;; contents the ANSI-CL is hog-tied to circa 1990 intellecutal property paradigms
;;;
;;; According to copyright disclaimers on the various CL Hyperspecs versions (v3,
;;; v6, v7) while large portions of the Hyperspec (as originally sanctioned by XJ313
;;; at Harlequin's request) do copy in a nearly verbatim manner from the ANSI spec
;;; (itself derived of CLTL[1&2]. This inclusion was originally allowed with the
;;; permission of X3 and produced under the auspices of the XJ313 Cleanup
;;; subcommittee. No public digital record of such a formal approval remains
;;; separate from the rihgts claim distributed with the HyperSpec itself and it is
;;; unclear from what little remains of the XJ313 Charter (it's organization was a
;;; hasty/controversial affair) whether such extenion of copyright was even
;;; permissible within the scope of the charter. Claims to copyright on Govt. funded
;;; projects strike us as odd given that CLTL, X3J13, and ANSI-CL would not have
;;; been produced without sufficient and prolonged funding from DARPA, ARPA,
;;; Etc. IOW, some degree of the ANSI-CL 'product' is a public deliverable funded by
;;; U.S. Taxpayers. It is doubtful that such material would meet a rigourous
;;; copywrite challenge... Regardless, since 2005 when Lispworks Limited a UK based
;;; company acquired the rights to Harlequin's claim on the HyperSpec they have
;;; maintained a slippery and somewhat dubious claim of ownership w/re the
;;; HyperSpec. As such, while it is not generally believed permissible to distribute
;;; the the HyperSpec in an derivative format there seems little to prevent one from
;;; reversing the Hyperspec's HTML to a privately accessible format.  Following is
;;; an attempt at using w3m and Emacs' w3m extension package to do so...
;;;
;;; The end goal is to use produce code capable of stripping the hyperspec and
;;; repurposing it for suitable presentation in a dedicated Emacs *Help* buffer for
;;; _personal_ presentation of the Hyperspec on my machine for my needs.
;;; IOW _not_ using TeXinfo and not using a web-browser to access a fully
;;; hyperlinked and xref'd documentation of the standard. If I'm able to succeed
;;; others could probably use similar such code to similar ends :)
;;; 
;;; ============================================================
;;; :USEFUL-LINKS-REFERENCES-SOURCES-QUOTES
;;;
;;; HyperSpec-v3 (The variable length version circa 1996)
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
;;; :X3J13-ARCHIVES
;;; :SEE (URL `ftp://ftp.parc.xerox.com/pub/cl/')
;;;
;;; :HSPEC-RESTRICTED-RIGHTS-LEGEND
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
;;; :SOURCE From the Hspec v3 @ MIT
;;;
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
;;;    From: Joe Corneli [mailto:address@hidden 
;;;    Sent: Thursday, June 16, 2005 4:40 PM
;;;    To: Garner, Jennifer
;;;    Subject: ansi common lisp standard?
;;;
;;;
;;;    Hi Jennifer,
;;;
;;;    in October or November of last year, we sent the appended request,
;;;    concerning the ANSI common lisp standard, to you, in hopes that you
;;;    could communicate the request to the ANSI J13 management committee.
;;;    Our aim was to obtain a copy of the Common Lisp standard with
;;;    permissions that would enable us to use it as the basis of new Lisp
;;;    documentation.  Can you tell me what the status of this request is
;;;    currently?
;;;
;;;    Thank you,
;;;
;;;    Joe Corneli
;;;
;;;
;;;      To the members of the ANSI J13 management committee:
;;;
;;;      We wish to file a request on the behalf of Lisp users world wide
;;;      that ANSI release the text of the Common Lisp standard in a way
;;;      that would make it legal to use the standard as the foundation
;;;      for a system of documentation which adequately describes Common
;;;      Lisp as a living language.
;;;
;;;      We recommend the use of the GNU Free Documentation License, which
;;;      was specifically designed to apply to documentation and standards
;;;      documents.  Using the GFDL, the copyright holder grants anyone
;;;      permission to publish both changed and unchanged versions of the
;;;      document, but requires that they all be distributed under the
;;;      same license.
;;;
;;;      The GFDL has a special feature intended for standards documents.
;;;      The document can have an Endorsements section which must be
;;;      removed from any modified version; ANSI's endorsement could say
;;;      that the document contains the official definition of ANSI Common
;;;      Lisp.  Other provisions of the license require giving credit to
;;;      the authors of earlier versions.  Thus, modified versions would
;;;      give credit to ANSI but could not claim to be the standard.
;;;
;;;      We would be glad to explore ways to resolve any issues or
;;;      uncertainties that may arise as you consider this request.
;;;
;;;       Joseph Corneli
;;;       Richard Stallman
;;;       Camm McGuire
;;;       Richard Gabriel
;;;       Bruno Haible
;;;       Sam Steingold
;;;
;;; :SOURCE (URL `http://lists.nongnu.org/archive/html/axiom-developer/2007-06/msg00456.html')
;;; ==============================

;;; ============================================================
;;; Following procedures tested with:
;;; (emacs-version)
;;; => "GNU Emacs 23.1.90.1 
;;;  | (i486-slackware-linux-gnu, GTK+ Version 2.14.7)  of 2009-12-20"
;;;
;;; w3m-version
;;; => "w3m/0.5.2"
;;;
;;; w3m-fill-column
;;; => 80
;;; ============================================================

(require 'hyperspec)
;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:27-05:00Z}#{09527} - by MON>
(defvar *mon-hs-root-dir* common-lisp-hyperspec-root
  "*The base directory path that the local Hyper Linked Common Lisp specification
resides under.
:NOTE I use the path set with the variable defined in the `hyperspec' file
provided with Slime.\n
:SEE-ALSO `*mon-hs-temp-buffer*', `mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-it-p', `mon-hspec-header-line-p', `mon-hspec-href-p',
`mon-w3m-spec-p', `mon-hspec-prop-type', `mon-hspec-stk-n-mv',
`mon-hspec-parse-w3m',`mon-hspec-out'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:29-05:00Z}#{09527} - by MON>
(defvar *mon-hs-temp-buffer* "*CL-HSPEC-CONV*"
  "*The temporary buffer holding the parsed text properties of the current w3m
  text under examination.\n
:SEE-ALSO `*mon-hs-root-dir*',`mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-it-p', `mon-hspec-header-line-p',`mon-hspec-href-p',
`mon-w3m-spec-p',`mon-hspec-prop-type', `mon-hspec-stk-n-mv',
`mon-hspec-parse-w3m',`mon-hspec-out'.\n►►►")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:32-05:00Z}#{09527} - by MON>
(defun mon-hspec-href-p ()
  "Are we looking at an w3m-href-anchor text property (with/without bold/italic).
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-w3m-spec-p', `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-parse-w3m',`mon-hspec-out' ,
`*mon-hs-temp-buffer*',`*mon-hs-root-dir*'.\n►►►"
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

;;; ==============================
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
`mon-hspec-parse-w3m',`mon-hspec-out', `*mon-hs-temp-buffer*',
`*mon-hs-root-dir*'.\n►►►"
  (when (eq (car (get-text-property (point) 'face)) 'w3m-header-line-location-title)
    (let ((loc-info `(:location (,(point) . ,(next-single-property-change (point) 'face)))))
      (setq loc-info `(,(car loc-info) 
                        ,(buffer-substring-no-properties (caadr loc-info) (cdadr loc-info))
                        :location-range ,@(cdr loc-info)))
      (when (eq (car (get-text-property (cdr (plist-get loc-info :location-range)) 'face))
                'w3m-header-line-location-content)
        (let ((loc-path `(:location-url
                          (,(cdr (plist-get loc-info :location-range))
                            . ,(next-single-property-change (cdr (plist-get loc-info :location-range)) 'face)))))
          (setq loc-path `(,(car loc-path)
                            ;; ,(replace-regexp-in-string (concat "file://" *mon-hs-root-dir*) "" {...}
                            ,(buffer-substring-no-properties (caadr loc-path) (cdadr loc-path))
                            :location-url-range ,@(cdr loc-path)))
          (setq loc-info `(,loc-info ,loc-path)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:40-05:00Z}#{09527} - by MON>
(defun mon-hspec-it-p ()
  "Are we looking at only a w3m-italic face text-property.\n
:SEE-ALSO `w3m-fontify-italic',`mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-header-line-p',`mon-hspec-href-p', `mon-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m',
`mon-hspec-out',`*mon-hs-temp-buffer*',`*mon-hs-root-dir*'.\n►►►"
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
              `(:it-on ,tp-it-str :it-range (,tp-it-frm . ,tp-it-to)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:43-05:00Z}#{09527} - by MON>
(defun mon-hspec-bld-p ()
  "Are we looking at only a w3m-bold face text-property.\n
:SEE-ALSO `w3m-fontify-bold', `mon-hspec-plain-p',`mon-hspec-it-p',
`mon-hspec-header-line-p',`mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m',
`mon-hspec-out',`*mon-hs-temp-buffer*',`*mon-hs-root-dir*'.\n►►►"
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
               `(:bld-on ,tp-bld-str :bld-range (,tp-bld-frm . ,tp-bld-to)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:47-05:00Z}#{09527} - by MON>
(defun mon-hspec-plain-p ()
  "Are we looking at plain-text sans w3m text-property.
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p',
`mon-hspec-header-line-p',`mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m',
`mon-hspec-out', `*mon-hs-temp-buffer*', `*mon-hs-root-dir*'.\n►►►"
  (when (and (not (get-text-property (point) 'face))
             (not (get-text-property (point) 'w3m-href-anchor))
             (next-property-change (point))
             (not (= (point) (buffer-end 1)))
             (not (= (next-property-change (point)) (buffer-end 1))))
    (let* ((tp-pln-frm (point))
           (tp-pln-to  (next-property-change tp-pln-frm))
           (tp-pln-str (buffer-substring-no-properties tp-pln-frm tp-pln-to)))
      (setq this-pln-prop
            `(:pln-on ,tp-pln-str :pln-range (,tp-pln-frm . ,tp-pln-to))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:52-05:00Z}#{09527} - by MON>
(defun mon-w3m-spec-p (spec spec-list)
  "Helper function for `mon-hspec-prop-type'.
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p', 
`mon-hspec-header-line-p', `mon-hspec-href-p',, `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-parse-w3m', `mon-hspec-out',
`*mon-hs-temp-buffer*', `*mon-hs-root-dir*'.\n►►►"
  (memq spec spec-list))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:56-05:00Z}#{09527} - by MON>
(defun mon-hspec-prop-type ()
  "Are we looking at a w3m bold, italic, plain, or href text-property.\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-stk-n-mv',`mon-hspec-parse-w3m',`mon-hspec-out',
`*mon-hs-temp-buffer*', `*mon-hs-root-dir*'.\n►►►"
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
Temporary buffer is specified in:\n:VARIABLE `*mon-hs-temp-buffer*'\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-parse-w3m',
`*mon-hs-root-dir*'.\n►►►"
  (get-buffer-create *mon-hs-temp-buffer*)
  (terpri (get-buffer *mon-hs-temp-buffer*))
  (prin1 prop (get-buffer *mon-hs-temp-buffer*)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:15:02-05:00Z}#{09527} - by MON>
(defun mon-hspec-stk-n-mv ()
  "Test Hyperlink Specification properties with mon-hspec-* predicates.
This function is the point mover and conditionally marshalls related functions.\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p',`mon-hspec-href-p', `mon-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-parse-w3m',`mon-hspec-out',
`*mon-hs-root-dir*', `*mon-hs-temp-buffer*'.\n►►►"
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
Contents returned in buffer specified by `*mon-hs-temp-buffer*'.\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p' `mon-hspec-it-p',
`mon-hspec-header-line-p' `mon-hspec-href-p', `mon-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv' `mon-hspec-out',
`*mon-hs-root-dir*'.\n►►►"
  (progn
    (goto-char (buffer-end 0))
    (let ((hdr (mon-hspec-header-line-p)))
      (when hdr 
        (mon-hspec-out (car hdr))
        (mon-hspec-out  (cadr hdr)))
      (goto-char (cdr (plist-get (cadr hdr) :location-url-range))))
    (while (not (eobp)) (mon-hspec-stk-n-mv))))

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
;;; (:bld-on " " :bld-range
;;; (161 . 162))
;;; (:xref-on "[Previou" :xref-to "/Body/fun_vectorp.html" :xref-range
;;; (162 . 170))
;;; (:xref-on "[Up]    " :xref-to "/Body/sec_the_arrays_dictionary.html" :xref-range
;;; (170 . 178))
;;; (:xref-on "[Next]  " :xref-to "/Body/fun_bit-andcm_c2cm_bit-xor.html" :xref-range
;;; (178 . 186))
;;; (:pln-on "
;;;
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; " :pln-range
;;; (186 . 268))
;;; (:it-on "Accessor" :it-range
;;; (268 . 276))
;;; (:location "Location: " :location-range
;;; (1 . 11))
;;; (:location-url "/HyperSpec-v3/Body/fun_get-properties.html" 
;;;  :location-url-range (11 . 148))
;;; (:xref-on "[HARLEQU" :xref-to "http://www.harlequin.com/" :xref-range
;;; (149 . 157))
;;; (:xref-on "[Common " :xref-to "/FrontMatter/index.html" :xref-range
;;; (157 . 165))
;;; (:bld-on " " :bld-range
;;; (165 . 166))
;;; (:xref-on "[Previou" :xref-to "/Body/fun_rassoccm__assoc-if-not.html" :xref-range
;;; (166 . 174))
;;; (:xref-on "[Up]    " :xref-to "/Body/sec_the_conses_dictionary.html" :xref-range
;;; (174 . 182))
;;; (:xref-on "[Next]  " :xref-to "/Body/acc_getf.html" :xref-range
;;; (182 . 190))
;;; (:pln-on "
;;;
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; " :pln-range
;;; (190 . 272))
;;; (:it-on "Function" :it-range
;;; (272 . 280))
;;; (:pln-on " " :pln-range
;;; (280 . 281))
;;; (:bld-on "GET-PROPERTIES" :bld-range
;;; (281 . 295))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (295 . 297))
;;; (:bld-on "Syntax:" :bld-range
;;; (297 . 304))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (304 . 306))
;;; (:bld-on "get-properties" :bld-range
;;; (306 . 320))
;;; (:pln-on " " :pln-range
;;; (320 . 321))
;;; (:it-on "plist indicator-list" :it-range
;;; (321 . 341))
;;; (:pln-on " => " :pln-range
;;; (341 . 345))
;;; (:it-on "indicator, value, tail" :it-range
;;; (345 . 367))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (367 . 369))
;;; (:bld-on "Arguments and Values:" :bld-range
;;; (369 . 390))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (390 . 392))
;;; (:it-on "plist" :it-range
;;; (392 . 397))
;;; (:pln-on "---a " :pln-range
;;; (397 . 402))
;;; (:xref-on "property list" :xref-to "/Body/glo_p.html#property_list" :xref-range
;;; (402 . 415))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (415 . 418))
;;; (:it-on "indicator-list" :it-range
;;; (418 . 432))
;;; (:pln-on "---a " :pln-range
;;; (432 . 437))
;;; (:xref-on "proper list" :xref-to "/Body/glo_p.html#proper_list" :xref-range
;;; (437 . 448))
;;; (:pln-on " (of " :pln-range
;;; (448 . 453))
;;; (:xref-on "indicators" :xref-to "/Body/glo_i.html#indicator" :xref-range
;;; (453 . 463))
;;; (:pln-on ").
;;;
;;; " :pln-range
;;; (463 . 467))
;;; (:it-on "indicator" :it-range
;;; (467 . 476))
;;; (:pln-on "---an " :pln-range
;;; (476 . 482))
;;; (:xref-on "object" :xref-to "/Body/glo_o.html#object" :xref-range
;;; (482 . 488))
;;; (:pln-on " that is an " :pln-range
;;; (488 . 500))
;;; (:xref-on "element" :xref-to "/Body/glo_e.html#element" :xref-range
;;; (500 . 507))
;;; (:pln-on " of " :pln-range
;;; (507 . 511))
;;; (:it-on "indicator-list" :it-range
;;; (511 . 525))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (525 . 528))
;;; (:it-on "value" :it-range
;;; (528 . 533))
;;; (:pln-on "---an " :pln-range
;;; (533 . 539))
;;; (:xref-on "object" :xref-to "/Body/glo_o.html#object" :xref-range
;;; (539 . 545))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (545 . 548))
;;; (:it-on "tail" :it-range
;;; (548 . 552))
;;; (:pln-on "---a " :pln-range
;;; (552 . 557))
;;; (:xref-on "list" :xref-to "/Body/glo_l.html#list" :xref-range
;;; (557 . 561))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (561 . 564))
;;; (:bld-on "Description:" :bld-range
;;; (564 . 576))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (576 . 578))
;;; (:xref-on "get-properties" :xref-to "/Body/fun_get-properties.html#get-properties" :xref-range
;;; (578 . 592))
;;; (:pln-on " is used to look up any of several " :pln-range
;;; (592 . 627))
;;; (:xref-on "property list" :xref-to "/Body/glo_p.html#property_list" :xref-range
;;; (627 . 640))
;;; (:pln-on " entries all at
;;; once.
;;;
;;; It searches the " :pln-range
;;; (640 . 679))
;;; (:it-on "plist" :it-range
;;; (679 . 684))
;;; (:pln-on " for the first entry whose " :pln-range
;;; (684 . 711))
;;; (:xref-on "indicator" :xref-to "/Body/glo_i.html#indicator" :xref-range
;;; (711 . 720))
;;; (:pln-on " is " :pln-range
;;; (720 . 724))
;;; (:xref-on "identical" :xref-to "/Body/glo_i.html#identical" :xref-range
;;; (724 . 733))
;;; (:pln-on " to one
;;; of the " :pln-range
;;; (733 . 748))
;;; (:xref-on "objects" :xref-to "/Body/glo_o.html#object" :xref-range
;;; (748 . 755))
;;; (:pln-on " in " :pln-range
;;; (755 . 759))
;;; (:it-on "indicator-list" :it-range
;;; (759 . 773))
;;; (:pln-on ". If such an entry is found, the " :pln-range
;;; (773 . 806))
;;; (:it-on "indicator" :it-range
;;; (806 . 815))
;;; (:pln-on " and 
;;; " :pln-range
;;; (815 . 821))
;;; (:it-on "value" :it-range
;;; (821 . 826))
;;; (:pln-on " returned are the " :pln-range
;;; (826 . 844))
;;; (:xref-on "property indicator" :xref-to "/Body/glo_p.html#property_indicator" :xref-range
;;; (844 . 862))
;;; (:pln-on " and its associated " :pln-range
;;; (862 . 882))
;;; (:xref-on "property value" :xref-to "/Body/glo_p.html#property_value" :xref-range
;;; (882 . 896))
;;; (:pln-on ",
;;; and the " :pln-range
;;; (896 . 906))
;;; (:it-on "tail" :it-range
;;; (906 . 910))
;;; (:pln-on " returned is the " :pln-range
;;; (910 . 927))
;;; (:xref-on "tail" :xref-to "/Body/glo_t.html#tail" :xref-range
;;; (927 . 931))
;;; (:pln-on " of the " :pln-range
;;; (931 . 939))
;;; (:it-on "plist" :it-range
;;; (939 . 944))
;;; (:pln-on " that begins with the found entry
;;; (i.e., whose " :pln-range
;;; (944 . 991))
;;; (:xref-on "car" :xref-to "/Body/glo_c.html#car" :xref-range
;;; (991 . 994))
;;; (:pln-on " is the " :pln-range
;;; (994 . 1002))
;;; (:it-on "indicator" :it-range
;;; (1002 . 1011))
;;; (:pln-on "). If no such entry is found, the " :pln-range
;;; (1011 . 1045))
;;; (:it-on "indicator" :it-range
;;; (1045 . 1054))
;;; (:pln-on ", 
;;; " :pln-range
;;; (1054 . 1057))
;;; (:it-on "value" :it-range
;;; (1057 . 1062))
;;; (:pln-on ", and " :pln-range
;;; (1062 . 1068))
;;; (:it-on "tail" :it-range
;;; (1068 . 1072))
;;; (:pln-on " are all " :pln-range
;;; (1072 . 1081))
;;; (:xref-on "nil" :xref-to "/Body/any_nil.html#nil" :xref-range
;;; (1081 . 1084))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (1084 . 1087))
;;; (:bld-on "Examples:" :bld-range
;;; (1087 . 1096))
;;; (:pln-on "
;;;
;;;  (setq x '()) =>  NIL
;;;  (setq *indicator-list* '(prop1 prop2)) =>  (PROP1 PROP2)
;;;  (getf x 'prop1) =>  NIL
;;;  (setf (getf x 'prop1) 'val1) =>  VAL1
;;;  (eq (getf x 'prop1) 'val1) =>  " :pln-range
;;; (1096 . 1274))
;;; (:xref-on "true" :xref-to "/Body/glo_t.html#true" :xref-range
;;; (1274 . 1278))
;;; (:pln-on "
;;;  (get-properties x *indicator-list*) =>  PROP1, VAL1, (PROP1 VAL1)
;;;  x =>  (PROP1 VAL1)
;;;
;;; " :pln-range
;;; (1278 . 1367))
;;; (:bld-on "Side Effects:" :bld-range
;;; (1367 . 1380))
;;; (:pln-on " None.
;;;
;;; " :pln-range
;;; (1380 . 1388))
;;; (:bld-on "Affected By:" :bld-range
;;; (1388 . 1400))
;;; (:pln-on " None.
;;;
;;; " :pln-range
;;; (1400 . 1408))
;;; (:bld-on "Exceptional Situations:" :bld-range
;;; (1408 . 1431))
;;; (:pln-on " None.
;;;
;;; " :pln-range
;;; (1431 . 1439))
;;; (:bld-on "See Also:" :bld-range
;;; (1439 . 1448))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (1448 . 1450))
;;; (:xref-on "get" :xref-to "/Body/acc_get.html#get" :xref-range
;;; (1450 . 1453))
;;; (:pln-on ", " :pln-range
;;; (1453 . 1455))
;;; (:xref-on "getf" :xref-to "/Body/acc_getf.html#getf" :xref-range
;;; (1455 . 1459))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (1459 . 1461))
;;; (:bld-on "Notes:" :bld-range
;;; (1461 . 1467))
;;; (:pln-on " None.
;;;
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; The following " :pln-range
;;; (1467 . 1569))
;;; (:xref-on "X3J13 cleanup issue" :xref-to "/FrontMatter/X3J13-Issues.html" :xref-range
;;; (1569 . 1588))
;;; (:pln-on ", " :pln-range
;;; (1588 . 1590))
;;; (:it-on "not part of the specification" :it-range
;;; (1590 . 1619))
;;; (:pln-on ", applies to
;;; this section:
;;;
;;;   • " :pln-range
;;; (1619 . 1651))
;;; (:xref-on "PLIST-DUPLICATES:ALLOW" :xref-to "/Issues/iss269.html" :xref-range
;;; (1651 . 1673))
;;; (:pln-on "
;;;   
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; " :pln-range
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
;;; (:pln-on "
;;; " :pln-range
;;; (1806 . 1807))
;;; (:xref-on "Copyright 1996, The Harlequin Group Limited. All Rights Reserved." 
;;;  :xref-to "/FrontMatter/About-HyperSpec.html#Legal" :xref-range (1807 . 1872))
;;; (:pln-on " " :pln-range
;;; (276 . 277))
;;; (:bld-on "BIT, SBIT" :bld-range
;;; (277 . 286))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (286 . 288))
;;; (:bld-on "Syntax:" :bld-range
;;; (288 . 295))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (295 . 297))
;;; (:bld-on "bit" :bld-range
;;; (297 . 300))
;;; (:pln-on " " :pln-range
;;; (300 . 301))
;;; (:it-on "bit-array " :it-range
;;; (301 . 311))
;;; (:pln-on "&rest" :pln-range
;;; (311 . 316))
;;; (:it-on " subscripts" :it-range
;;; (316 . 327))
;;; (:pln-on " => " :pln-range
;;; (327 . 331))
;;; (:it-on "bit" :it-range
;;; (331 . 334))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (334 . 336))
;;; (:bld-on "sbit" :bld-range
;;; (336 . 340))
;;; (:pln-on " " :pln-range
;;; (340 . 341))
;;; (:it-on "bit-array " :it-range
;;; (341 . 351))
;;; (:pln-on "&rest" :pln-range
;;; (351 . 356))
;;; (:it-on " subscripts" :it-range
;;; (356 . 367))
;;; (:pln-on " => " :pln-range
;;; (367 . 371))
;;; (:it-on "bit" :it-range
;;; (371 . 374))
;;; (:pln-on "
;;;
;;; (setf (" :pln-range
;;; (374 . 383))
;;; (:bld-on "bit" :bld-range
;;; (383 . 386))
;;; (:pln-on " " :pln-range
;;; (386 . 387))
;;; (:it-on "bit-array " :it-range
;;; (387 . 397))
;;; (:pln-on "&rest" :pln-range
;;; (397 . 402))
;;; (:it-on " subscripts" :it-range
;;; (402 . 413))
;;; (:pln-on ") " :pln-range
;;; (413 . 415))
;;; (:it-on "new-bit" :it-range
;;; (415 . 422))
;;; (:pln-on ")
;;;
;;; (setf (" :pln-range
;;; (422 . 432))
;;; (:bld-on "sbit" :bld-range
;;; (432 . 436))
;;; (:pln-on " " :pln-range
;;; (436 . 437))
;;; (:it-on "bit-array " :it-range
;;; (437 . 447))
;;; (:pln-on "&rest" :pln-range
;;; (447 . 452))
;;; (:it-on " subscripts" :it-range
;;; (452 . 463))
;;; (:pln-on ") " :pln-range
;;; (463 . 465))
;;; (:it-on "new-bit" :it-range
;;; (465 . 472))
;;; (:pln-on ")
;;;
;;; " :pln-range
;;; (472 . 475))
;;; (:bld-on "Arguments and Values:" :bld-range
;;; (475 . 496))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (496 . 498))
;;; (:it-on "bit-array" :it-range
;;; (498 . 507))
;;; (:pln-on "---for " :pln-range
;;; (507 . 514))
;;; (:xref-on "bit" :xref-to "/Body/acc_bitcm_sbit.html#bit" :xref-range
;;; (514 . 517))
;;; (:pln-on ", a " :pln-range
;;; (517 . 521))
;;; (:xref-on "bit array" :xref-to "/Body/glo_b.html#bit_array" :xref-range
;;; (521 . 530))
;;; (:pln-on "; for " :pln-range
;;; (530 . 536))
;;; (:xref-on "sbit" :xref-to "/Body/acc_bitcm_sbit.html#sbit" :xref-range
;;; (536 . 540))
;;; (:pln-on ", a " :pln-range
;;; (540 . 544))
;;; (:xref-on "simple bit array" :xref-to "/Body/glo_s.html#simple_bit_array" :xref-range
;;; (544 . 560))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (560 . 563))
;;; (:it-on "subscripts" :it-range
;;; (563 . 573))
;;; (:pln-on "---a " :pln-range
;;; (573 . 578))
;;; (:xref-on "list" :xref-to "/Body/glo_l.html#list" :xref-range
;;; (578 . 582))
;;; (:pln-on " of " :pln-range
;;; (582 . 586))
;;; (:it-on "valid array indices" :it-range
;;; (586 . 605))
;;; (:pln-on " for the " :pln-range
;;; (605 . 614))
;;; (:it-on "bit-array" :it-range
;;; (614 . 623))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (623 . 626))
;;; (:it-on "bit" :it-range
;;; (626 . 629))
;;; (:pln-on "---a " :pln-range
;;; (629 . 634))
;;; (:xref-on "bit" :xref-to "/Body/glo_b.html#bit" :xref-range
;;; (634 . 637))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (637 . 640))
;;; (:bld-on "Description:" :bld-range
;;; (640 . 652))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (652 . 654))
;;; (:xref-on "bit" :xref-to "/Body/acc_bitcm_sbit.html#bit" :xref-range
;;; (654 . 657))
;;; (:pln-on " and " :pln-range
;;; (657 . 662))
;;; (:xref-on "sbit" :xref-to "/Body/acc_bitcm_sbit.html#sbit" :xref-range
;;; (662 . 666))
;;; (:pln-on " " :pln-range
;;; (666 . 667))
;;; (:xref-on "access" :xref-to "/Body/glo_a.html#access" :xref-range
;;; (667 . 673))
;;; (:pln-on " the " :pln-range
;;; (673 . 678))
;;; (:it-on "bit-array" :it-range
;;; (678 . 687))
;;; (:pln-on " " :pln-range
;;; (687 . 688))
;;; (:xref-on "element" :xref-to "/Body/glo_e.html#element" :xref-range
;;; (688 . 695))
;;; (:pln-on " specified by " :pln-range
;;; (695 . 709))
;;; (:it-on "subscripts" :it-range
;;; (709 . 719))
;;; (:pln-on ".
;;;
;;; These " :pln-range
;;; (719 . 728))
;;; (:xref-on "functions" :xref-to "/Body/glo_f.html#function" :xref-range
;;; (728 . 737))
;;; (:pln-on " ignore the " :pln-range
;;; (737 . 749))
;;; (:xref-on "fill pointer" :xref-to "/Body/glo_f.html#fill_pointer" :xref-range
;;; (749 . 761))
;;; (:pln-on " when " :pln-range
;;; (761 . 767))
;;; (:it-on "accessing" :it-range
;;; (767 . 776))
;;; (:pln-on " " :pln-range
;;; (776 . 777))
;;; (:xref-on "elements" :xref-to "/Body/glo_e.html#element" :xref-range
;;; (777 . 785))
;;; (:pln-on ".
;;;
;;; " :pln-range
;;; (785 . 788))
;;; (:bld-on "Examples:" :bld-range
;;; (788 . 797))
;;; (:pln-on "
;;;
;;;  (bit (setq ba (make-array 8 
;;;                             :element-type 'bit 
;;;                             :initial-element 1))
;;;        3) =>  1
;;;  (setf (bit ba 3) 0) =>  0
;;;  (bit ba 3) =>  0
;;;  (sbit ba 5) =>  1
;;;  (setf (sbit ba 5) 1) =>  1
;;;  (sbit ba 5) =>  1
;;;
;;; " :pln-range
;;; (797 . 1054))
;;; (:bld-on "Affected By:" :bld-range
;;; (1054 . 1066))
;;; (:pln-on " None.
;;;
;;; " :pln-range
;;; (1066 . 1074))
;;; (:bld-on "Exceptional Situations:" :bld-range
;;; (1074 . 1097))
;;; (:pln-on " None.
;;;
;;; " :pln-range
;;; (1097 . 1105))
;;; (:bld-on "See Also:" :bld-range
;;; (1105 . 1114))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (1114 . 1116))
;;; (:xref-on "aref" :xref-to "/Body/acc_aref.html#aref" :xref-range
;;; (1116 . 1120))
;;; (:pln-on ", " :pln-range
;;; (1120 . 1122))
;;; (:xref-on "Section 3.2.1 (Compiler Terminology)" :xref-to "/Body/sec_3-2-1.html" :xref-range
;;; (1122 . 1158))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (1158 . 1160))
;;; (:bld-on "Notes:" :bld-range
;;; (1160 . 1166))
;;; (:pln-on "
;;;
;;; " :pln-range
;;; (1166 . 1168))
;;; (:xref-on "bit" :xref-to "/Body/acc_bitcm_sbit.html#bit" :xref-range
;;; (1168 . 1171))
;;; (:pln-on " and " :pln-range
;;; (1171 . 1176))
;;; (:xref-on "sbit" :xref-to "/Body/acc_bitcm_sbit.html#sbit" :xref-range
;;; (1176 . 1180))
;;; (:pln-on " are like " :pln-range
;;; (1180 . 1190))
;;; (:xref-on "aref" :xref-to "/Body/acc_aref.html#aref" :xref-range
;;; (1190 . 1194))
;;; (:pln-on " except that they require " :pln-range
;;; (1194 . 1220))
;;; (:it-on "arrays" :it-range
;;; (1220 . 1226))
;;; (:pln-on " to be a " :pln-range
;;; (1226 . 1235))
;;; (:xref-on "bit array" :xref-to "/Body/glo_b.html#bit_array" :xref-range
;;; (1235 . 1244))
;;; (:pln-on "
;;; and a " :pln-range
;;; (1244 . 1251))
;;; (:xref-on "simple bit array" :xref-to "/Body/glo_s.html#simple_bit_array" :xref-range
;;; (1251 . 1267))
;;; (:pln-on ", respectively.
;;;
;;; " :pln-range
;;; (1267 . 1284))
;;; (:xref-on "bit" :xref-to "/Body/acc_bitcm_sbit.html#bit" :xref-range
;;; (1284 . 1287))
;;; (:pln-on " and " :pln-range
;;; (1287 . 1292))
;;; (:xref-on "sbit" :xref-to "/Body/acc_bitcm_sbit.html#sbit" :xref-range
;;; (1292 . 1296))
;;; (:pln-on ", unlike " :pln-range
;;; (1296 . 1305))
;;; (:xref-on "char" :xref-to "/Body/acc_charcm_schar.html#char" :xref-range
;;; (1305 . 1309))
;;; (:pln-on " and " :pln-range
;;; (1309 . 1314))
;;; (:xref-on "schar" :xref-to "/Body/acc_charcm_schar.html#schar" :xref-range
;;; (1314 . 1319))
;;; (:pln-on ", allow the first argument to be an " :pln-range
;;; (1319 . 1355))
;;; (:xref-on "array" :xref-to "/Body/glo_a.html#array" :xref-range
;;; (1355 . 1360))
;;; (:pln-on " of
;;; any " :pln-range
;;; (1360 . 1368))
;;; (:xref-on "rank" :xref-to "/Body/glo_r.html#rank" :xref-range
;;; (1368 . 1372))
;;; (:pln-on ".
;;;
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; The following " :pln-range
;;; (1372 . 1469))
;;; (:xref-on "X3J13 cleanup issue" :xref-to "/FrontMatter/X3J13-Issues.html" :xref-range
;;; (1469 . 1488))
;;; (:pln-on ", " :pln-range
;;; (1488 . 1490))
;;; (:it-on "not part of the specification" :it-range
;;; (1490 . 1519))
;;; (:pln-on ", applies to
;;; this section:
;;;
;;;   • " :pln-range
;;; (1519 . 1551))
;;; (:xref-on "CONSTANT-MODIFICATION:DISALLOW" :xref-to "/Issues/iss083.html" :xref-range
;;; (1551 . 1581))
;;; (:pln-on "
;;;   
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; " :pln-range
;;; (1581 . 1666))
;;; (:xref-on "[Startin" :xref-to "/FrontMatter/Starting-Points.html" :xref-range
;;; (1666 . 1674))
;;; (:xref-on "[Content" :xref-to "/FrontMatter/Chapter-Index.html" :xref-range
;;; (1674 . 1682))
;;; (:xref-on "[Index] " :xref-to "/FrontMatter/Master-Index.html" :xref-range
;;; (1682 . 1690))
;;; (:xref-on "[Symbols" :xref-to "/FrontMatter/Symbol-Index.html" :xref-range
;;; (1690 . 1698))
;;; (:xref-on "[Glossar" :xref-to "/Body/sec_26-1.html" :xref-range
;;; (1698 . 1706))
;;; (:xref-on "[Issues]" :xref-to "/Issues/Issues-Categorized.html" :xref-range
;;; (1706 . 1714))
;;; (:pln-on "
;;; " :pln-range
;;; (1714 . 1715))
;;; (:xref-on "Copyright 1996, The Harlequin Group Limited. All Rights Reserved." 
;;;  :xref-to "/FrontMatter/About-HyperSpec.html#Legal" :xref-range
;;; (1715 . 1780))
;;; ============================================================



;;; ============================================================
;; :TODO Following will need to be refontified post parse:
;;; :SEE-ALSO `common-lisp-hyperspec-symbol-table' in :FILE hyperspec.el
;;;
;;; :MATCH-THESE
;;;
;; "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
;;; :NOTE Following clarified in :SEE (info "(ansicl)Interpreting Dictionary Entries")
;;; :AT-BOL 
;;;^Affected By: 
;;;^Arguments and Values:   ;1.4.4.3
;;;^Class Precedence List:
;;;^Compound Type Specifier Arguments:
;;;^Compound Type Specifier Kind:
;;;^Compound Type Specifier Description:
;;;^ Compound Type Specifier Syntax:
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
;;; ==============================
;; :DPAN2TEXI-OUTPOUT
;; ^Pronunciation: (appears in dpans2texi output) ;1.4.4.16
;; ^or→ 
;; ^See <some-xref>
;; , see <some-xref>
;;; ============================================================

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

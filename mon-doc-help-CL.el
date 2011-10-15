;;; mon-doc-help-CL.el --- utils for documenting Common Lisp from within Emacs
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2011 MON KEY. All rights reserved.
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
;; `mon-help-CL-local-time', `mon-help-CL-loop', `mon-help-CL-time',
;; `mon-help-CL-file-dir-functions', `mon-help-CL-minion', `mon-help-CL-symbols', 
;; `mon-help-CL-pkgs', `mon-bind-mon-help-CL-pkgs-loadtime',
;; `mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
;; `mon-help-CL-wget-pkgs', 
;; `mon-help-CL-wget-pkgs-for-shell-command',
;; `mon-hspec-plain-p', `mon-hspec-bld-p',
;; `mon-hspec-it-p', `mon-hspec-header-line-p',
;; `mon-hspec-href-p', `mon-hspec-w3m-spec-p',
;; `mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-out',
;; `mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
;; `mon-hspec-unparse-w3m-to-buffer', `mon-hspec-find-w3m',
;; `mon-help-CL-error-condition-restart', `mon-help-CL-emacs-functions',
;; `mon-help-CL-sequence-predicates', `mon-help-CL-lispdoc',
;; `mon--CL-no-pull-p', `mon-help-CL-bit-byte-bool-logic',
;; `mon-help-CL-stream-keywords', `mon-help-CL-sequences',
;; `mon-help-CL-iteration', `mon-help-CL-conses', `mon-help-CL-hash-tables',
;; `mon-help-CL-print', `mon-help-CL-streams', `mon-help-CL-reader',
;; `mon-help-CL-chars', `mon-help-CL-strings', `mon-help-CL-structures',
;; `mon-help-CL-arrays', `mon-help-CL-numbers', `mon-help-CL-object-CLOS',
;; `mon-help-CL-control-flow', `mon-help-CL-eval-compile',
;; `mon-help-CL-load-compile', `mon-help-CL-environment',
;; `mon-help-CL-package-functions', `mon-help-CL-intern-symbol',
;; `mon-help-CL-sharpsign-syntax', `mon-help-CL-types', `mon-help-CL-format',
;; `mon-help-CL-format-usage', `mon-cln-ansi-info',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-help-CL-cmu-ai-repo*', `*mon-help-CL-ext-pkg-map*',
;; `*mon-hspec-root-dir*', `*mon-hspec-parse-buffer*', `*mon-hspec-unparse-buffer*',
;; `*clhs-symbol-v3-or-v7*', `*mon-help-CL-symbols*',  `*mon-CL-indent-specs*',
;; `*regexp-ansicl-info*'
;;
;; GROUPS:
;; `mon-doc-help-CL', `mon-doc-help-CL-hspec-parse',
;; 
;; ALIASED/ADVISED/SUBST'D:
;; `mon-help-cl-packages'      -> `mon-help-CL-pkgs'
;; `mon-help-cl-symbols'       -> `mon-help-CL-symbols'
;; `mon-hyperspec-lookup'      -> `mon-help-CL-symbols'
;; `mon-help-slime-keys'       -> `mon-help-CL-slime-keys'
;; `mon-help-swank-functions'  -> `mon-help-CL-swank-functions'
;; 
;; MOVED:
;; `mon-help-CL-time'           <- mon-doc-help-utils.el 
;; `mon-help-CL-loop'           <- mon-doc-help-utils.el
;; `mon-help-CL-slime-keys'     <- mon-doc-help-utils.el
;; `mon-help-CL-wget-pkgs-TEST' -> mon-testme-utils.el
;;
;; RENAMED:
;; `*cl-cmu-ai-repo*'                        -> `*mon-help-CL-cmu-ai-repo*'
;; `*cl-ext-pkg-map*'                        -> `*mon-help-CL-ext-pkg-map*'
;; `*cl-ext-pkg-map-no-pull*'                -> `*mon-help-CL-ext-pkg-map-no-pull*'
;; `*mon-cl-symbols*'                        -> `*mon-help-CL-symbols*'
;; `*mon-hs-unprs-buffer*'                   -> `*mon-hspec-unparse-buffer*'
;; `*mon-hs-parse-buffer*'                   -> `*mon-hspec-parse-buffer*'
;; `*mon-hs-root-dir*'                       -> `*mon-hspec-root-dir*'
;; `mon-help-cl-emacs-functions'             -> `mon-help-CL-emacs-functions'
;; `mon-help-slime-keys'                     -> `mon-help-CL-slime-keys'
;; `mon-help-swank-functions'                -> `mon-help-CL-swank-functions'
;; `mon-help-wget-cl-pkgs'                   -> `mon-help-CL-wget-pkgs'
;; `mon-help-wget-cl-pkgs-for-shell-command' -> `mon-help-CL-wget-pkgs-for-shell-command'
;; `mon-w3m-spec-p'                          -> `mon-hspec-w3m-spec-p'
;; `mon-help-CL:LOOP'                        -> `mon-help-CL-loop'
;; `mon-help-CL:DO'                          -> `mon-help-CL-do'
;; `mon-help-CL:LOCAL-TIME'                  -> `mon-help-CL-local-time'
;; `mon-help-cl-pkgs'                        -> `mon-help-CL-pkgs'
;; `mon-help-cl-symbols'                     -> `mon-help-CL-symbols'
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
;; The variable `*mon-hspec-root-dir*' used with hyperspec parsing routines need:
;; :VARIABLE `common-lisp-hyperspec-root'
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
;;
;; NOTES: 
;; If Jesper Harder's dpans2texi's ansicl manual is installed, e.g.:
;;
;; @direntry
;; * ANSI Common Lisp: (ansicl).    Draft ANSI Common Lisp standard (dpANS3R).
;; @end direntry
;;
;; (info-lookup-add-help
;;  :mode 'lisp-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))
;;
;; :SEE (URL `http://www.phys.au.dk/~harder/dpans.html')
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
;; Copyright © 2008-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

 
;;; ==============================
;;; :CHANGESET 2069
;;; :CREATED <Timestamp: #{2010-08-16T14:30:02-04:00Z}#{10331} - by MON KEY>
(defgroup mon-doc-help-CL nil
  "Extensions for help and documentation of Common Lisp related procedures.\n
:SEE-ALSO .\n►►►"
  :link '(url-link 
          :tag "\n:EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-doc-help-CL.el')" 
          "http://www.emacswiki.org/emacs/mon-doc-help-CL.el")
  :link '(emacs-library-link 
          :tag "\n:FILE mon-doc-help-CL.el" 
          "mon-doc-help-CL.el")
  ;; :prefix "mon-help-CL" ;; "*mon-help-CL-" ;; :prefix "*mon-hspec-"
  :group 'mon-base
  :group 'mon-doc-help-utils)

;;; ==============================
;;; :CHANGESET 2069
;;; :CREATED <Timestamp: #{2010-08-16T14:30:45-04:00Z}#{10331} - by MON KEY>
(defgroup mon-doc-help-CL-hspec-parse nil
  "Use emacs-w3m to parse Common Lisp hyperspec HTML data to lisp forms.\n
:SEE-ALSO .\n►►►"
  :link '(url-link 
          :tag "\n:EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-doc-help-CL.el')" 
          "http://www.emacswiki.org/emacs/mon-doc-help-CL.el")
  :link '(emacs-library-link 
          :tag "\n:FILE mon-doc-help-CL.el"
          "mon-doc-help-CL.el")
  ;; :prefix "mon-help-CL" ;; "*mon-help-CL-" ;; :prefix "*mon-hspec-"
  :group 'mon-doc-help-CL)

;;; ==============================
;;; :CHANGESET 2406
;;; :CREATED <Timestamp: #{2011-01-20T18:47:43-05:00Z}#{11034} - by MON KEY>
(defcustom  *mon-doc-help-CL-xrefs* 
  '(mon--CL-no-pull-p mon-help-CL-pkgs mon-bind-mon-help-CL-pkgs-loadtime
    mon-cln-ansi-info mon-help-CL-wget-pkgs mon-help-CL-wget-pkgs-for-shell-command
    mon-hspec-href-p mon-hspec-header-line-p mon-hspec-it-p mon-hspec-bld-p
    mon-hspec-plain-p mon-hspec-w3m-spec-p mon-hspec-prop-type mon-hspec-out
    mon-hspec-stk-n-mv mon-hspec-parse-w3m mon-hspec-find-w3m mon-hspec-unparse-w3m
    mon-hspec-unparse-w3m-to-buffer mon-help-CL-emacs-functions
    mon-help-CL-file-dir-functions mon-help-CL-stream-keywords
    mon-help-CL-error-condition-restart mon-help-CL-sequence-predicates
    mon-help-CL-bit-byte-bool-logic mon-help-CL-loop mon-help-CL-loop-usage
    mon-help-CL-do mon-help-CL-time mon-help-CL-sequences mon-help-CL-iteration
    mon-help-CL-conses mon-help-CL-hash-tables mon-help-CL-print mon-help-CL-streams
    mon-help-CL-reader mon-help-CL-chars mon-help-CL-strings mon-help-CL-structures
    mon-help-CL-arrays mon-help-CL-numbers mon-help-CL-object-CLOS
    mon-help-CL-control-flow mon-help-CL-eval-compile mon-help-CL-load-compile
    mon-help-CL-environment mon-help-CL-package-functions mon-help-CL-intern-symbol
    mon-help-CL-types mon-help-CL-type-declarations mon-help-CL-sharpsign-syntax
    mon-help-CL-format mon-help-CL-format-usage mon-help-CL-slime-keys
    mon-help-CL-swank-functions mon-help-CL-local-time mon-help-CL-minion
    mon-help-utils-CL-loadtime mon-help-CL-symbols mon-help-CL-lispdoc
    ;; :VARIABLES
    *mon-help-CL-cmu-ai-repo* *mon-help-CL-ext-pkg-map*
    *mon-help-CL-ext-pkg-map-no-pull* mon-hspec-root-dir* *mon-hspec-parse-buffer*
    **mon-hspec-unparse-buffer* mon-CL-indent-specs* clhs-symbol-v3-or-v7*
    *mon-help-CL-symbols*
    *mon-doc-help-CL-xrefs*)
  "Xrefing list of `mon-help-CL-*' functions, constants, and variables.\n
The symbols contained of this list are defined in :FILE <FILE>\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-dir-utils-xrefs*',
`*mon-keybindings-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-bzr-utils-xrefs*' `*mon-buffer-utils-xrefs*',
`*mon-env-proc-utils-xrefs*', `*mon-error-utils-xrefs*',
`*mon-line-utils-xrefs*', `*mon-macs-xrefs*', `*mon-plist-utils-xrefs*',
`*mon-post-load-hooks-xrefs*', `*mon-seq-utils-xrefs*',
`*mon-string-utils-xrefs*', `*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*', `*mon-slime-xrefs*',
`*mon-url-utils-xrefs*', `*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*',
`*mon-ulan-utils-xrefs*', `*google-define-redux-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-doc-help-CL
  :group 'mon-xrefs)


;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-23T17:47:48-05:00Z}#{09523} - by MON KEY>
(defcustom *mon-help-CL-cmu-ai-repo* "http://www.cs.cmu.edu/Groups/AI/lang/lisp/"
  "Partial path to the Carnegie Mellon Artificial Intelligence Repository.\n
Evaluated by `mon-bind-mon-help-CL-pkgs-loadtime' to build the full path lists
for some elements in `*mon-help-CL-ext-pkg-map*' which `mon-help-CL-pkgs' needs
for proper URL presentation.\n
Following elements in `*mon-help-CL-ext-pkg-map*' contain relative subpaths of this URL:\n
 cltl2-html cltl2-src cltl2tex 
 dpans3 dpans-clos dpans-amop
 islisp vgrind refcard cl-shell eli\n
:SEE (URL `http://www.cs.cmu.edu/Groups/AI/0.html').\n
:SEE-ALSO `*mon-help-CL-ext-pkg-map*', `mon-help-CL-pkgs', `mon-help-CL-wget-pkgs'.\n►►►"
  :type '(choice 
          (string :tag "CMU AI Default path" 
                  :value "http://www.cs.cmu.edu/Groups/AI/0.html")
          (string :tag  "CMU AI AFS path"
                  :value "http://www-cgi.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/"))
  :group 'mon-doc-help-CL
  :link '(url-link :tag "\nCMU AI repo index (URL `http://www.cs.cmu.edu/Groups/AI/0.html')"
                   "http://www.cs.cmu.edu/Groups/AI/0.html"))
;;
;; (unless (bound-and-true-p *mon-help-CL-cmu-ai-repo*)
;;   (setq *mon-help-CL-cmu-ai-repo* "http://www.cs.cmu.edu/Groups/AI/lang/lisp/"))
;;
;;; :TEST-ME *mon-help-CL-cmu-ai-repo*
;;; (progn (makunbound '*mon-help-CL-cmu-ai-repo* ) (unintern "*mon-help-CL-cmu-ai-repo*" obarray ) )

;;; ==============================
;;; :CHANGESET 2069
;;; :CREATED <Timestamp: #{2010-08-18T16:41:46-04:00Z}#{10333} - by MON KEY>
;(defcustom *mon-help-CL-ext-pkg-map-loadtime* nil
;  ""
;  :type '(boolean)
;  :group 'mon-doc-help-CL)

;;; ==============================
;;; :NOTE If we continue using `defcustom' here we need to ensure that 
;;; it re-evaluates `mon-bind-mon-help-CL-pkgs-loadtime' after updating...
;;; :TODO This could maybe refactored to an EIEIO or defstruct.
;;; :TODO The property `mon-help-CL-pkgs-buffer-name` needs a defcustom setter.
;;; :CREATED <Timestamp: #{2009-12-23T20:36:35-05:00Z}#{09524} - by MON KEY>
;; :WAS (defvar *mon-help-CL-ext-pkg-map*  nil
(defcustom *mon-help-CL-ext-pkg-map* nil
  "List of Common Lisp documentation packages and legacy/historic packages.\n
Each sublist element has the format:\n
 \(<SYMBOL> <DESCRIPTION> <URL>\)\n
 - <SYMBOL> is a key satisfying the predicate `eq' and designating a CL file or
   package name;\n
 - <DESCRIPTION> is a string describing <SYMBOL>\n
 - <URL> is a string identifying the URL from which the package designated by
   <SYMBOL> can be downloaded using `mon-help-CL-wget-pkgs'\n
When <DESCRIPTION> contains multiple lines of text it may be a `concat' form
that is splicable as a list element for use in a `backquote' template and
with all string elements in the concat form terminated by a \"\\n\" \(char 10\).
Such sulist elements have the format:\n
 (, (concat <TITLE-LINE> <COURTESY-LINE> <SEE-ALSO-LINE>* )\n
When a <COURTESY-LINE> is present it is prefixed by three spaces (char 32)
followed by \":COURTESY\" and the package author(s) name and has the format:\n
\"   \":COURTESY <PKG-AUTHOR-NAME>*\n
When a <SEE-ALSO-LINE> is present it is prefixed by three spaces (char 32)
followed by \":SEE-ALSO\" followed by a URL formated as (URL `<SOME-URL>')
such that each <SEE-ALSO-LINE> has the format:\n
\"   \":SEE-ALSO (URL `http://some-see-also.com/')\n  
For example, following is an entry for Bubba's hypothetical library CL-bubba:\n
\(CL-bubba                                                      ;; <SYMBOL>
 ,\(concat                                                      ;; <DESCRIPTION>
  \"Bubba is a chewing gum library for Common lisp.\\n\"          ;; <TITLE-LINE>
  \"   :COURTESY Bubba Hubba\\n\"                                 ;; <COURTESY-LINE>
  \"   :SEE-ALSO \(URL `http://bubbas-blog.com/'\)\\n\"             ;; <SEE-ALSO-LINE>
  \"   :SEE-ALSO \(URL `http://github.com/bubba/cl-bubba'\)\\n\"
  \"   :SEE-ALSO \(URL `git://github.com/bubba/cl-bubba.git'\)\\n\"\)
 \"http://www.bubba.com/less-bleeding/edge/cl-bubba.tgz\"\)       ;; <URL>\n
The net effect will be to produce pretty output for `mon-help-CL-pkgs' while
retaining a parseable list for `mon-help-CL-wget-pkgs'.\n
:EXAMPLE\n\n\(mon-help-CL-pkgs nil t\)\n
\(assq 'cl-akcl *mon-help-CL-ext-pkg-map*\)\n
:NOTE Some values for <URL> may not appear as a part of a FQDN, these are paths
relative to a root directory specified in variable `*mon-help-CL-cmu-ai-repo*'
and their values interpolated by `mon-help-CL-pkgs'.\n
:NOTE The variable `*mon-help-CL-ext-pkg-map-no-pull*' holds a list of the keys
in this list which should not be pulled with `mon-help-CL-wget-pkgs' either
because they are too large or point into a DVC.\n
:NOTE This variable's plist holds the property `mon-help-CL-pkgs-buffer-name`.
The property value is used as the buffer-name for returning/displaying results
generated with `mon-help-CL-pkgs', and is also accessed at loadtime by
`mon-bind-mon-help-CL-pkgs-loadtime' and `mon-purge-cl-symbol-buffers-on-load'
when the predicate `IS-MON-SYSTEM-P' returns non-nil.
The current property valuecan be accessed with the form:\n
 \(get '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name\)\n
:SEE-ALSO `*mon-help-CL-cmu-ai-repo*', `mon-help-CL-pkgs',
`mon-help-CL-symbols', `mon-help-CL-lispdoc'.\n►►►"
  :type '(repeat
          (list (symbol :tag "<KEY>")
                (string :tag "<DESCRIPTION>")
                (string :tag "<URL>")))
  :group 'mon-doc-help-CL)
;;
(unless (bound-and-true-p *mon-help-CL-ext-pkg-map*)
  (setq *mon-help-CL-ext-pkg-map*
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
           "ftp://ftp.parc.xerox.com/pub/cl/cleanup")
          (cl-ansi-naggum
           ,(concat 
             "ANSI X3.226-1994 final draft PostScript conversion landscape orientation 2up.\n"
             "   :COURTESY Erik Naggum\n"
             "   :SEE-ALSO (URL `http://naggum.no/ANSI-CL)'\n"
             "   :SEE-ALSO (URL `http://quimby.gnus.org/circus/cl/ansi-cl/)'\n"
             "   :SEE-ALSO (URL `http://quimby.gnus.org/circus/cl/ansi-cl.tar.gz')\n")
           "http://naggum.no/ANSI-CL.tar.gz")
          (dpans3 
           ,(concat "Common Lisp ANSI Standard X3J13 committe TeX sources Draft 3.\n" 
                                        ;; :COURTESY Lars Magne Ingebrigtsen
                    "   :SEE-ALSO (URL `http://quimby.gnus.org/circus/cl/dpANS3/')\n"
                    "   :SEE-ALSO (URL `http://quimby.gnus.org/circus/cl/dpANS3.tar.gz')\n")
           "doc/standard/ansi/dpans/dpans3.tgz")
          (dpans3r 
           ,(concat
             "Common Lisp X3J13 Draft rev3 changes from :VERSION 15.17 and 15.17R.\n"
                                        ;; :COURTESY Lars Magne Ingebrigtsen
             "   :SEE-ALSO (URL `http://quimby.gnus.org/circus/cl/dpANS3R/')\n"
             "   :SEE-ALSO (URL `http://quimby.gnus.org/circus/cl/ans3r-ps.tar.gz')\n"
             "   :SEE-ALSO (URL `http://quimby.gnus.org/circus/cl/dpANS3R.tar.gz')\n")
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
           ,(concat 
             "Common Lisp ANSI standard in .texi and .info format GNU Common Lisp.\n"
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
             "   :COURTESY David Touretzky\n"
             "   :SEE-ALSO (URL `http://www-2.cs.cmu.edu/~dst/LispBook/')\n"
             "   :SEE-ALSO (URL `http://www.cs.cmu.edu/~dst/Lisp/code/')\n"
             "   :SEE-ALSO (URL `http://www.cs.cmu.edu/~dst')\n")
           "http://www-2.cs.cmu.edu/~dst/LispBook/book.ps")
          (cl-quick-reference
           ,(concat
             "Common Lisp Quick Reference provides short descriptions of the ANSI standard.\n"
             "   Includes a comprehensive index of CL symbols. Licensed under GFDL v 1.2.\n"
             "   :COURTESY Bert Burgemeister\n" ;; trebbu@googlemail.com. 
             "   :SEE-ALSO (URL `http://clqr.boundp.org')             <- Project Homepage\n"
             "   :SEE-ALSO (URL `http://repo.or.cz/r/clqr.git')       <- GIT repo\n"
             "   :SEE-ALSO (URL `http://clqr.boundp.org/clqr.tar.gz') <- LaTeX Sources\n")
           "http://clqr.boundp.org/clqr-letter-consec.pdf")
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
             "   ASDF, CFFI, CFFI-FEATURES, CFFI-SYS, CFFI-UTILS, CHUNGA, CL+SSL,\n"
             "   CL+SSL-SYSTEM, CL-BASE64, CL-PPCRE, CL-PPCRE-TEST, CL-WHO, COMMON-LISP,\n"
             "   COMMON-LISP-CONTROLLER, FLEXI-STREAMS, HUNCHENTOOT, HUNCHENTOOT-ASD, MD5,\n"
             "   RFC2388, S-XML, TRIVIAL-GRAY-STREAMS, URL-REWRITE\n"
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
             "   :COURTESY David Lichteblau\n"
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
           "ftp://ftp.sra.co.jp/pub/lang/lisp/akcl/akcl-1-615.tar.gz")
          (repo-install 
           ,(concat 
             "Repo-install is a Common Lisp DVC oriented package manager tracking.\n"
             "   :COURTESY Jeff Palmucci\n"
             "   :SEE-ALSO (URL `http://github.com/jpalmucci/repo-install')\n"
             "   :SEE-ALSO (URL `git://github.com/jpalmucci/repo-install.git')\n"
             "   :SEE-ALSO (URL `http://www.machineinsight.com/repo-install/')\n") 
           "http://www.machineinsight.com/repo-install/repo-install-bootstrap.tgz")
          (slurp 
           ,(concat 
             "Slurp databases/acceses public Common Lisp repositories.\n"
             "   :COURTESY Robert Brown\n"
             "   :SEE-ALSO (URL `http://github.com/brown/slurp')\n")
           "git://github.com/brown/slurp.git")
           (quicklisp-setup 
            ,(concat "Quicklisp Setup for rapid deployment of community-developed Common Lisp libraries.\n"
                     "   :COURTESY Zach Beane and CL communtiy\n"
                     "   :SEE-ALSO (URL `http://quicklisp.org/')\n"
                     "   :SEE-ALSO (URL `http://blog.quicklisp.org/')\n"
                     "   :SEE-ALSO (URL `http://groups.google.com/group/quicklisp')\n"
                     (concat "   :SEE-ALSO (URL `http://www.youtube.com/"
                             "watch?v=11wYPAy9qNw&feature=PlayList&p"
                             "=9A2D7E31B7D039AF&index=0&playnext=1&hd=1')\n")
                     "   :SEE-ALSO (URL `http://github.com/quicklisp/quicklisp-projects')\n")
            "http://beta.quicklisp.org/quicklisp.lisp")
           (quicklisp-projects
           ,(concat "Quicklisp for rapid deployment of community-developed Common Lisp libraries.\n"
                    "   :COURTESY Zach Beane and CL communtiy\n"
                    "   :SEE-ALSO (URL `http://quicklisp.org/')\n"
                    "   :SEE-ALSO (URL `http://blog.quicklisp.org/')\n"
                    "   :SEE-ALSO (URL `http://groups.google.com/group/quicklisp')\n"
                    "   :SEE-ALSO (URL `http://github.com/quicklisp/quicklisp-projects')\n")
           "git://github.com/quicklisp/quicklisp-projects.git")))
  (put '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name
       (upcase (symbol-name '*mon-help-CL-ext-pkg-map*)))
  (custom-note-var-changed '*mon-help-CL-ext-pkg-map*))
;;
(when (bound-and-true-p *mon-help-CL-ext-pkg-map*)
  (unless (get '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name)
    (put '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name
         (upcase (symbol-name '*mon-help-CL-ext-pkg-map*)))))
;;
;;; :TEST-ME (stringp (cadr (assq 'cl-akcl *mon-help-CL-ext-pkg-map*)))
;;;(progn (makunbound '*mon-help-CL-ext-pkg-map*) (unintern "*mon-help-CL-ext-pkg-map*" obarray) )

;;; ==============================
;;; :CHANGESET 2069
;;; :CREATED <Timestamp: #{2010-08-18T18:16:19-04:00Z}#{10333} - by MON KEY>
(defun mon--CL-no-pull-p (sym-mem)
  "Predicate helper for `defcustom'izing `*mon-help-CL-ext-pkg-map-no-pull*'.\n
Return non-nil when SYM-MEM is a `memq'  `*mon-help-CL-ext-pkg-map*'
:EXAMPLE\n\n\(mon--CL-no-pull-p cl-naggum\)\n
:SEE-ALSO .\n►►►"
  (memq sym-mem (mapcar #'car *mon-help-CL-ext-pkg-map*)))
;;
;;; TEST-ME \(mon--CL-no-pull-p 'cl-naggum\)

;;; ==============================
;;; :CHANGESET 2069 <Timestamp: #{2010-08-16T14:39:18-04:00Z}#{10331} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-02-13T11:24:58-05:00Z}#{10066} - by MON KEY>
;;; (defvar *mon-help-CL-ext-pkg-map-no-pull* nil
(defcustom *mon-help-CL-ext-pkg-map-no-pull* 
  '(lispy clbuild slurp quicklisp-projects cl-naggum 
   lispdoc l1sp.org cl-cookbook X3J13-archive ilisp)
  "Keys in `*mon-help-CL-ext-pkg-map*' not meant for `mon-help-CL-wget-pkgs'.\n
Elements of this list point to webpages, webpaths, version controlled repos, or
large file archives. These include:\n
 lispy clbuild cl-naggum lispdoc l1sp.org\n cl-cookbook X3J13-archive ilisp\n
:NOTE These _are_ diplayed in documentation of `mon-help-CL-pkgs' and thus the
user is able to yank the URL but `mon-help-CL-wget-pkgs' won't pull them.\n 
When adding elements of this list via `customize' additions must satisfy the
predicate `mon--CL-no-pull-p'.\n
:SEE-ALSO `*mon-help-CL-cmu-ai-repo*'.\n►►►"
  :type '(repeat (restricted-sexp :match-alternatives  (mon--CL-no-pull-p)))
  :group 'mon-doc-help-CL)
;;
;; (unless (bound-and-true-p *mon-help-CL-ext-pkg-map-no-pull*)
;;   (setq *mon-help-CL-ext-pkg-map-no-pull* 
;;         '(lispy clbuild slurp quicklisp-projects
;;          cl-naggum lispdoc l1sp.org cl-cookbook X3J13-archive ilisp)))
;;
;;; :TEST-ME *mon-help-CL-ext-pkg-map-no-pull**
;;;(progn (makunbound '*mon-help-CL-ext-pkg-map-no-pull*) 
;;;       (unintern "*mon-help-CL-ext-pkg-map-no-pull*" obarray) )


;; (defcustom *mon-help-CL-do-hash* nil
;; :type 
;; "Hyperspec-v3"
;; "Hyperspec-v7"
  
;;; ==============================
;;; :REQUIRES
;;; :FUNCTION `mon-help-propertize-tags', `mon-help-temp-docstring-display'
;;; :VARIABLE `*regexp-mon-doc-help-comment-tags*' 
;;; :FACE `mon-help-INNER-KEY-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag'
;;; :SEE :FILE mon-doc-help-utils.el 
;;; :CHANGESET 2026 <Timestamp: #{2010-08-03T12:37:04-04:00Z}#{10312} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-12-23T20:41:48-05:00Z}#{09524} - by MON KEY>
(defun mon-help-CL-pkgs (&optional insrtp intrp w-msg-string)
  (interactive "i\np")
  (let ((mhcp-divd (make-string 79 95))
        (mhcp-dsp-bfr (get-buffer-create 
                (get '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name)))
        mhcp-prtty)
    (setq mhcp-prtty
          (concat 
           (mapconcat #'(lambda (cl) 
                          (format "%s\n\n:%s\n - %s\n:SEE (URL `%s')\n"
                                  mhcp-divd
                                  (upcase (format "%s" (car cl))) 
                                  (cadr cl) 
                                  (let* ((pth-p (caddr cl))
                                         (tst-url (substring pth-p 0 3)))
                                    (if (member tst-url '("htt" "ftp"))
                                        pth-p
                                      (concat *mon-help-CL-cmu-ai-repo* pth-p)))))
                      *mon-help-CL-ext-pkg-map* "\n")
           "\n" mhcp-divd))
                                        ;(when (or intrp insrtp)
    (setq mhcp-prtty (with-temp-buffer 
                  (insert mhcp-prtty)
                  (mon-help-propertize-tags 
                   '(*regexp-mon-doc-help-comment-tags*  0 mon-help-INNER-KEY-tag)
                   '("^_\\{79\\}$" 0 mon-help-PNTR-tag)
                   '("^\\:[A-Z0-9-]+$" 0 mon-help-DYNATAB-tag))
                  ;; Don't zap tp's with `buffer-substring-no-properties'
                  (buffer-substring (mon-g2be -1 t) (mon-g2be 1 t)))) ;)
    (cond (w-msg-string (message w-msg-string))
          (intrp (mon-help-temp-docstring-display mhcp-prtty mhcp-dsp-bfr))
          (insrtp (save-excursion  
                    (newline)
                    (princ mhcp-prtty (current-buffer))))
          (t (prin1 mhcp-prtty)))))
;;
;;; :TEST-ME (mon-help-CL-pkgs)
;;; :TEST-ME (mon-help-CL-pkgs nil t)

 
;;; ==============================
;;; :CHANGESET 2026 <Timestamp: #{2010-08-03T12:35:37-04:00Z}#{10312} - by MON KEY>
;;; :CHANGESET 1946
;;; :CREATED <Timestamp: #{2010-07-07T13:56:57-04:00Z}#{10273} - by MON KEY>
(defun mon-bind-mon-help-CL-pkgs-loadtime (&optional w-msg-user)
  "Build the propertized documentation for `mon-help-CL-pkgs' at loadtime.\n
When optional arg W-MSG-USER is non-nil and/or if `documentation-property' of
`mon-help-CL-pkgs' is null inform that the 'function-documentation property was
\(re\)bound.\n
:EXAMPLE\n
\(progn \(plist-put \(symbol-plist 'mon-help-CL-pkgs\) 'function-documentation  nil\)
      \(unless \(documentation-property 'mon-help-CL-pkgs 'function-documentation\)
        \(mon-bind-mon-help-CL-pkgs-loadtime\)\)\)
\(mon-help-CL-pkgs nil t\)\n
Idiomatic loadtime evaulation of this fncn has the following form:\n
 \(eval-after-load \"mon-doc-help-CL\" '\(mon-bind-mon-help-CL-pkgs-loadtime t\)\)\n
:SEE-ALSO `*mon-help-CL-ext-pkg-map*', `mon-help-utils-CL-loadtime',
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
        (if w-msg-user
            (mon-help-CL-pkgs nil nil
                              (concat 
                               ":FUNCTION `mon-bind-mon-help-CL-pkgs-loadtime' "
                               "-- evaluated to generate docstring for `mon-help-CL-pkgs'"))
          (mon-help-CL-pkgs))
        "\nThe return value of this function can be inserted to a dedicated buffer by\n"
        "evaluating with the optional args INTRP non-nil.\n"
        "When optional arg W-MSG-STRING is non-nil it is a string for message.\n\n"
        "The intent of this arg is for use with `mon-bind-mon-help-CL-pkgs-loadtime'.\n\n"
        ":EXAMPLE\n\(mon-help-CL-pkgs nil t\)\n\n"
        ":NOTE Results returned to buffer-named by the property\n"
        "`mon-help-CL-pkgs-buffer-name` on the variable `*mon-help-CL-ext-pkg-map*', e.g.:\n"
        " \(get '*mon-help-CL-ext-pkg-map* 'mon-help-CL-pkgs-buffer-name\)\n\n"
        ":SEE-ALSO `*mon-help-CL-cmu-ai-repo*', `*mon-help-CL-ext-pkg-map*',\n"
        "`*mon-help-CL-ext-pkg-map-no-pull*', `mon-help-CL-wget-pkgs',\n"
        "`mon-CL-package-complete', `quicklisp-system-complete',\n"
        "`mon-help-CL-symbols', `mon-help-CL-lispdoc'.\n►►►"))
  ;; :WAS
  ;; (when w-msg-user 
  ;;   (message 
  ;;    (concat ":FUNCTION `mon-bind-mon-help-CL-pkgs-loadtime' "
  ;;            "-- evaluated to generate docstring for `mon-help-CL-pkgs'")))
  )
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;|
;;|(progn (plist-put (symbol-plist 'mon-help-CL-pkgs) 'function-documentation  nil)
;;|       (unless (documentation-property 'mon-help-CL-pkgs 'function-documentation)
;;|         (mon-bind-mon-help-CL-pkgs-loadtime)))
;;|
;;`----


 
;;; ==============================
;;; :RENAMED `mon--test--help-wget-cl-pkgs' -> `mon-help-CL-wget-pkgs-TEST'
;;; :CREATED <Timestamp: #{2009-12-24T01:10:48-05:00Z}#{09524} - by MON>
(defun mon-help-CL-wget-pkgs (&optional cl-wget-fname)
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
:EXAMPLE\n\(mon-help-CL-wget-pkgs-TEST\) ;<- helper test function.\n
:NOTE This function does not write the URLs for *mon-help-CL-ext-pkg-map* keys
cl-cookbook, oaklisp, ilisp, lisp, lispdoc. These point to web pages and are not
files.  Download or add these manually if that is what you want. The following
loop returns the absent URL's:\n
\(let \(manual-dl\)
  \(dolist \(cl-wpgs '\(ilisp cl-cookbook oaklisp lispdoc lispy\) manual-dl\)
    \(setq manual-dl
          \(concat \(caddr \(assoc cl-wpgs *mon-help-CL-ext-pkg-map*\)\) \"\\n\" manual-dl\)\)\)\)\n
:SEE-ALSO `mon-help-CL-wget-pkgs-TEST', `mon-help-CL-pkgs', `*mon-help-CL-cmu-ai-repo*',
`*mon-help-CL-ext-pkg-map*', `mon-wget-list-give-script-to-shell-command',
`mon-wget-list-to-script', `mon-wget-list-to-script-TEST',
`mon-wget-list-to-script-shell-command', `mon-wget-mon-pkgs',
`mon-wget-rfc'.\n►►►"
  (let ((rmv-wb-pgs (mapcar #'car *mon-help-CL-ext-pkg-map*))
        (wget-p (executable-find "wget"))
        (cl-wget-fname (if cl-wget-fname cl-wget-fname
                           (concat default-directory 
                                   "wget-script-" 
                                   (format-time-string "%Y-%m-%d"))))
        (sys (case system-type
               ((gnu/linux linux gnu/kfreebsd) 'gnu)
               ;(darwin aix berkeley-unix hpux irix lynxos usg-unix-v 'nix-like)
               (windows-nt 'wnz)))) ;; ms-dos cygwin
    (mapc #'(lambda (wb)
              (setq rmv-wb-pgs (delq wb rmv-wb-pgs)))
          '(lispy clbuild cl-naggum lispdoc l1sp.org 
            cl-cookbook X3J13-archive ilisp))
    (setq rmv-wb-pgs
          (mapconcat #'(lambda (clurl)
                         (let* ((this-cl (assoc clurl *mon-help-CL-ext-pkg-map*))
                                (this-pth-p (caddr this-cl))
                                (tst-url (substring this-pth-p 0 3)))
                           (if (member tst-url '("htt" "ftp"))
                               this-pth-p
                               (concat *mon-help-CL-cmu-ai-repo* this-pth-p))))
                     rmv-wb-pgs "\n"))
    (with-temp-file cl-wget-fname
      (insert rmv-wb-pgs)
      (mon-g2be -1)
      (when wget-p 
        (cond ((eq sys 'wnz)
               (insert (concat "# " wget-p  " --no-parent " "--no-directories "  
                               "-i " (file-name-nondirectory cl-wget-fname) "\n")))
              ((eq sys 'gnu)
               (insert "#! /bin/sh\nwget --no-parent --no-directories \x5c\n")
               (while (search-forward-regexp "[a-z]$" nil t) (insert " \x5c"))
               (mon-g2be 1)               
               (skip-chars-backward "^ \\")
               (delete-char 1))))
      (setq rmv-wb-pgs  (mon-buffer-sub-no-prop)))
    (when (and wget-p) (set-file-modes cl-wget-fname 480))
    rmv-wb-pgs))
;;
;;; :TEST-ME (mon-help-CL-wget-pkgs (concat default-directory "test-wget-cl-pkgs"))
;;; :TEST-ME (mon-help-CL-wget-pkgs-TEST)


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
(defun mon-help-CL-wget-pkgs-for-shell-command (wget-fname)
  "Return a string\(s\) for passing to `shell-command' for wget'ing CL docs.\n
:SEE-ALSO `mon-help-CL-wget-pkgs', `*mon-help-CL-cmu-ai-repo*',
`*mon-help-CL-ext-pkg-map*', `mon-help-CL-wget-pkgs-TEST',
`mon-wget-list-give-script-to-shell-command', `mon-wget-list-to-script',
`mon-wget-list-to-script-shell-command', `mon-wget-mon-pkgs', `mon-wget-rfc',
`mon-wget-unicodedata-files'.\n►►►"
  (let ((fnm-tst-wgt (file-name-nondirectory wget-fname))
        (mjcwpfsc-sys (case system-type
               ((gnu/linux linux gnu/kfreebsd) 'gnu)
               ;;(darwin aix berkeley-unix hpux irix lynxos usg-unix-v 'nix-like)
               ;; ms-dos cygwin
               (windows-nt 'wnz)
               (t (and (not (executable-find "wget")) 'no-exec)))) 
        read-wget-string)
    (unless (directory-files default-directory nil (concat fnm-tst-wgt "$"))
      (mon-format :w-fun #'error 
                  :w-spec '(":FUNCTION `mon-help-CL-wget-pkgs-for-shell-command' "
                              "-- file does not exist or invoked outside file's directory")))
    (with-temp-buffer
      (save-excursion (insert-file-contents wget-fname))
      (when (eq mjcwpfsc-sys 'wnz) (delete-char (- (skip-chars-forward "# "))))
      (setq read-wget-string 
            `(,(cond ((eq mjcwpfsc-sys 'no-exec) '("### NO wget executable in path"))
                     ((eq mjcwpfsc-sys 'gnu)
                      (delete-region (mon-g2be -1 t) (1+ (line-end-position 1)))
                      (delete-and-extract-region (mon-g2be -1 t) (1+ (line-end-position 1))))
                     ((eq mjcwpfsc-sys 'wnz)
                      (delete-and-extract-region (mon-g2be -1 t) (line-end-position))))
              . ,(cond ((eq mjcwpfsc-sys 'gnu) 
                        (replace-regexp-in-string " \\\n" "\n" (mon-buffer-sub-no-prop)))
                       ((eq mjcwpfsc-sys 'wnz)
                        (subst-char-in-string 10 32 (mon-buffer-sub-no-prop) t))))))
    read-wget-string))
;;;
;;; :TEST-ME 
;;; (let ((system-type 'gnu/linux)) 
;;;    ;;((system-type 'windows-nt))
;;;  (mon-help-CL-wget-pkgs-for-shell-command "wget-script-2009-12-24"))
;;; (mon-help-CL-wget-pkgs-for-shell-command "wget-script-2010-02-18")


 
;;; ==============================
;; :CL-HSPEC-W3M-PARSING
;;; ==============================
;;;
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
 
;;; ==============================
;; :HSPEC-LINKS-REFERENCES-SOURCES-QUOTES
;;; ==============================
;;;
;; :HSPEC-V3 
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
 
;;; ============================================================
;;; The following section provides parsing routines for extracting 
;;; text-properties and their ranges from Common-Lisp HyperSpec xrefs,
;;; italicized strings, bold strings and plain-text strings in buffer formatted
;;; with w3m and assocated Emacs package emac-w3m.
;;;
;;; :FUNCTION `mon-hspec-plain-p',`mon-hspec-bld-p',
;;; `mon-hspec-header-line-p',`mon-hspec-href-p', `mon-hspec-w3m-spec-p',
;;; `mon-hspec-prop-type', `mon-hspec-it-p', `mon-hspec-out'
;;; `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m'
;;; `mon-hspec-unparse-w3m-to-buffer'
;;; :VARIABLE
;;; `*mon-hspec-root-dir*', `*mon-hspec-parse-buffer*', `*mon-hspec-unparse-buffer*'
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

 
;;; ==============================
;;; :NOTE This is a compile-time/load-time kluge MON needs to make sure the
;;; symbol `common-lisp-hyperspec-root' is present _now_.
;;; However, we shouldn't bind it if user hasn't loaded slime/hyperspec but
;;; _will_ later nor should we load hyperspec.el just for this one symbol...
;;;
(eval-when-compile (require 'hyperspec nil t))
;;
(unless (or (bound-and-true-p common-lisp-hyperspec-root)
            (featurep 'hyperspec))
  (setq common-lisp-hyperspec-root nil))

 
;;; ==============================
;;; (defcustom :type 'directory :group 'mon-doc-help-CL-hspec-parse)
;;; :CREATED <Timestamp: #{2009-12-27T22:14:27-05:00Z}#{09527} - by MON>
(defcustom *mon-hspec-root-dir* nil
  "Base _local_ directory path to the Hyper Linked Common Lisp specification.\n
This path is as per `common-lisp-hyperspec-root' and may begin with:\n
 \"file://\" or \"file:/\" :NOTE The latter is wrong and we won't recognize it.\n
Following is an example format of such a type of local path:\n
\"file:///<SOME>/<PATH>/CL-documentation/CL-hyperspec-html/HyperSpec-v3/\"\n
:EXAMPLE\n\n*mon-hspec-root-dir*\n
This variable defaults to the `common-lisp-hyperspec-root' defined in the
:FILE hyperspec.el as provided with current Slime distributions.\n
:SEE-ALSO `*mon-hspec-parse-buffer*', `*mon-hspec-unparse-buffer*',
`mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-hspec-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-out',
`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-find-w3m'.\n►►►"
  :type 'directory
  :group 'mon-doc-help-CL-hspec-parse)
;
(unless (bound-and-true-p *mon-hspec-root-dir*)
  (setq *mon-hspec-root-dir* common-lisp-hyperspec-root))
;;
;;; :TEST-ME *mon-hspec-root-dir*
;;;(progn (makunbound '*mon-hspec-root-dir*) (unintern "*mon-hspec-root-dir*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:29-05:00Z}#{09527} - by MON>
;;; :WAS (defvar *mon-hspec-parse-buffer* nil "{ ... DOCS ...}")
;;;       (unless (bound-and-true-p *mon-hspec-parse-buffer*)
;;;        (setq *mon-hspec-parse-buffer* "*CL-HSPEC-CONV*"))
(defcustom *mon-hspec-parse-buffer* "*CL-HSPEC-CONV*" ;nil
  "String name used to generate a temporary buffer.\n
Buffer for emacs-w3m text properties from current Common Lisp Hyperspec parse.\n
:SEE-ALSO `*mon-hspec-unparse-buffer*', `*mon-hspec-root-dir*',`mon-hspec-plain-p',
`mon-hspec-bld-p', `mon-hspec-it-p', `mon-hspec-header-line-p',
`mon-hspec-href-p', `mon-hspec-w3m-spec-p', `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-out', `mon-hspec-parse-w3m',
`mon-hspec-unparse-w3m', `mon-hspec-unparse-w3m-to-buffer',
`mon-hspec-find-w3m'.\n►►►"
  :type 'string 
  :group 'mon-doc-help-CL-hspec-parse)
;;
;;; :TEST-ME *mon-hspec-parse-buffer*
;;;(progn (makunbound '*mon-hspec-parse-buffer*) (unintern "*mon-hspec-parse-buffer*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-29T12:08:40-05:00Z}#{09532} - by MON KEY>
;;; :WAS (defvar *mon-hspec-unparse-buffer* nil "{ ... Docs ...}")
;;;       (unless (bound-and-true-p *mon-hspec-unparse-buffer*)
;;;         (setq *mon-hspec-unparse-buffer* "*CL-HSPEC-UNPARSE*"))
(defcustom  *mon-hspec-unparse-buffer* "*CL-HSPEC-UNPARSE*" ;nil
  "String name used to generate a buffer for `mon-hspec-unparse-w3m'.
Buffer holds the to hold the round tripped unparsed text properties from return
value generated with `mon-hspec-parse-w3m'.\n
:SEE-ALSO `*mon-hspec-root-dir*', `*mon-hspec-parse-buffer*'
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-it-p', `mon-hspec-header-line-p', `mon-hspec-href-p',
`mon-hspec-w3m-spec-p', `mon-hspec-prop-type', `mon-hspec-stk-n-mv',
`mon-hspec-out'.\n►►►"
  :type 'string 
  :group 'mon-doc-help-CL-hspec-parse)
;;
;;; :TEST-ME *mon-hspec-unparse-buffer*
;;;(progn (makunbound '*mon-hspec-unparse-buffer*) (unintern "*mon-hspec-unparse-buffer*" obarray) )

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:32-05:00Z}#{09527} - by MON>
(defun mon-hspec-href-p ()
  "Are we looking at an w3m-href-anchor text property (with/without bold/italic).\n
When non-nil return a two element list of the form:\n
\(:xref-on   \"<XREF-STRING>\"\n  :xref-to \"<XREF-PATH>\"
  :xref-range \(<START> . <END>\)\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-w3m-spec-p', `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `*mon-hspec-parse-buffer*',
`mon-hspec-find-w3m', `*mon-hspec-root-dir*', `mon-line-test-content',
`mon-get-text-properties-category', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (let (this-xref-prop)
    (when (get-text-property (point) 'w3m-href-anchor)
      (let* ((tp-xrf-frm  (point))
             (tp-xrf-to   (next-single-property-change tp-xrf-frm 'w3m-href-anchor))
             ;; (tp-xrf-str  (buffer-substring-no-properties tp-xrf-frm tp-xrf-to))
             (tp-xrf-str  (mon-buffer-sub-no-prop tp-xrf-frm tp-xrf-to))
             (tp-xrf-prop (get-text-property tp-xrf-frm 'w3m-href-anchor)))
        (setq this-xref-prop
              `(:xref-on ,tp-xrf-str 
                :xref-to ,(replace-regexp-in-string 
                           (concat "file://" *mon-hspec-root-dir*) "" tp-xrf-prop)
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
`mon-hspec-href-p', `mon-hspec-w3m-spec-p', `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `mon-hspec-find-w3m',
`*mon-hspec-parse-buffer*', `*mon-hspec-root-dir*', `mon-line-test-content',
`mon-get-text-properties-category', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (when (eq (car (get-text-property (point) 'face)) 'w3m-header-line-location-title)
    (let ((loc-info `(:location (,(point) . ,(next-single-property-change (point) 'face)))))
      (setq loc-info `(,(car loc-info) 
                       ,(mon-buffer-sub-no-prop (caadr loc-info) (cdadr loc-info))
                       :location-range ,@(cdr loc-info)))
      (when (eq (car (get-text-property (cdr (plist-get loc-info :location-range)) 'face))
                'w3m-header-line-location-content)
        (let ((loc-path `(:location-url
                          (,(cdr (plist-get loc-info :location-range))
                           . ,(next-single-property-change (cdr (plist-get loc-info :location-range)) 'face))))
              (rplc-loc-vig (line-end-position))
              rplc-pth-str
              rplc-pth)
          ;; This is whatever whitespace remains after the URL in the location bar. 
          ;; No clue what length someone else might have as their hspec path,
          ;; nor how w3m will treat it. I've tried a coupla different
          ;; approaches for dealing with this on a couple of different machines
          ;; GNU and w32. Best is just to punt by putting back the whitespace after the URL.
          ;; This way the buffer size stays constant without any more silly plist maths.
          (setq rplc-loc-vig `(:location-url-vig ,(concat (make-string (- rplc-loc-vig (cdadr loc-path)) 32)) ;"\n")
                               :location-vig-range (,(cdadr loc-path) . ,rplc-loc-vig)))
          (setq rplc-pth-str (mon-buffer-sub-no-prop (caadr loc-path) (cdadr loc-path)))
          (setq rplc-pth (replace-regexp-in-string 
                          (concat "\\(file:///?" *mon-hspec-root-dir* "\\)\\(.*\\.htm?\\)")
                          "\\2" rplc-pth-str))
          (setq loc-path `(,(car loc-path)
                           ;; :WAS  ,(concat (or rplc-pth rplc-pth-str) "\n")
                           ,(or rplc-pth rplc-pth-str)
                           :location-url-range 
                           ,(if rplc-pth
                                ;; :NOTE (cons (caadr '(:loc (11 . 153))) ;=> 11
                                ;;       (cdadr '(:loc (11 . 153))) ;=> 153 
                                ;;       ) ;=> (11 . 153)
                                ;; :NOTE Should we 1+ these lengths b/c of concat'd newline?
                                ;;       Yes. Turns out it is needed. _IF_ we uncomment the \n in the vig
                                ;; (cons (caadr loc-path)
                                ;;       (1+ (- (cdadr loc-path)
                                ;;              (- (length rplc-pth-str)
                                ;;                 (length rplc-pth)))))
                                (cons (caadr loc-path)
                                      (- (cdadr loc-path)
                                         (- (length rplc-pth-str)
                                            (length rplc-pth))))
                              (cdr loc-path))))
          (when rplc-pth 
            (let* ((ofst-frm (plist-get loc-path :location-url-range))
                   (ofst-lst (cons (car ofst-frm)
                                   (+ (car ofst-frm) (length rplc-pth-str)))))
              (setq loc-path (reverse loc-path))
              (push :location-url-offset loc-path)
              (push ofst-lst loc-path)
              (setq loc-path (reverse loc-path))))
          (setq loc-info `(,loc-info ,loc-path ,rplc-loc-vig)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:40-05:00Z}#{09527} - by MON>
(defun mon-hspec-it-p ()
  "Are we looking at only a w3m-italic face text-property.\n
When t return a two element list of the form:\n
 \(:italics-on \"<ITALICIZED-STRING>\"  :italics-range \(<START> . <END>\)\n
:SEE-ALSO `w3m-fontify-italic', `mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-header-line-p',`mon-hspec-href-p', `mon-hspec-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-parse-w3m',
`mon-hspec-unparse-w3m',`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out',
`mon-hspec-find-w3m', `*mon-hspec-parse-buffer*', `*mon-hspec-root-dir*',
`mon-line-test-content', `mon-get-text-properties-category', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (let ((face-it-p (get-text-property (point) 'face)))
    (when (and (member 'w3m-italic face-it-p)
               (and (not (member 'w3m-bold face-it-p))
                    (not (member 'w3m-anchor face-it-p))
                    (not (null face-it-p))))
      (let* ((tp-it-frm (point))
             (tp-it-to  (next-single-property-change tp-it-frm 'face))
             (tp-it-str (mon-buffer-sub-no-prop tp-it-frm tp-it-to))
             this-it-prop)
        (setq this-it-prop
              `(:italics-on ,tp-it-str :italics-range (,tp-it-frm . ,tp-it-to)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:43-05:00Z}#{09527} - by MON>
(defun mon-hspec-bld-p ()
  "Are we looking at only a w3m-bold face text-property.\n
When t return a two element list of the form:\n
\(:bold-on \"<BOLD-STRING>\"  :bold-range \(<START> . <END>\)\n
:SEE-ALSO `w3m-fontify-bold', `mon-hspec-plain-p',`mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-hspec-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv',`mon-hspec-parse-w3m',
`mon-hspec-out', `mon-hspec-unparse-w3m', `mon-hspec-unparse-w3m-to-buffer',
`mon-hspec-find-w3m', `*mon-hspec-parse-buffer*', `*mon-hspec-root-dir*',
`mon-line-test-content', `mon-get-text-properties-category', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
   (let ((face-bold-p (get-text-property (point) 'face)))
     (when (and (member 'w3m-bold face-bold-p)
                (and (not (member 'w3m-italic face-bold-p))
                     (not (member 'w3m-anchor face-bold-p))
                     (not (null face-bold-p))))
       (let* ((tp-bld-frm (point))
              (tp-bld-to  (next-single-property-change tp-bld-frm 'face))
              (tp-bld-str (mon-buffer-sub-no-prop tp-bld-frm tp-bld-to))
              this-bld-prop)
         (setq this-bld-prop
               `(:bold-on ,tp-bld-str :bold-range (,tp-bld-frm . ,tp-bld-to)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:47-05:00Z}#{09527} - by MON>
(defun mon-hspec-plain-p ()
  "Are we looking at plain-text sans other w3m text-properties we are parsing.\n
When t return a two element list of the form:\n
 \(:plain-on \"<SOME-PLAIN-TEXT>\"  :plain-range \(<START> . <END>\)\n
:SEE-ALSO `mon-hspec-bld-p',`mon-hspec-it-p', `mon-hspec-header-line-p',
`mon-hspec-href-p', `mon-hspec-w3m-spec-p' `mon-hspec-prop-type', 
`mon-hspec-stk-n-mv',`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `mon-hspec-find-w3m',
`*mon-hspec-parse-buffer*', `*mon-hspec-root-dir*', `mon-line-test-content',
`mon-get-text-properties-category', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (when (and (not (get-text-property (point) 'face))
             (not (get-text-property (point) 'w3m-href-anchor))
             (next-property-change (point))
             (not (= (point) (mon-g2be 1 t)))
             (not (= (next-property-change (point)) (mon-g2be 1 t))))
    (let* ((tp-pln-frm (point))
           (tp-pln-to  (next-property-change tp-pln-frm))
           (tp-pln-str (mon-buffer-sub-no-prop tp-pln-frm tp-pln-to))
           this-pln-prop)
      (setq this-pln-prop
            `(:plain-on ,tp-pln-str :plain-range (,tp-pln-frm . ,tp-pln-to))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:52-05:00Z}#{09527} - by MON>
(defun mon-hspec-w3m-spec-p (spec spec-list)
  "Helper function for `mon-hspec-prop-type'.\n
When SPEC-LIST contains SPEC return the sublist of SPEC-LIST as per `memq'.\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p',`mon-hspec-it-p', 
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-hspec-prop-type',
`mon-hspec-stk-n-mv', `mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `mon-hspec-find-w3m',
`*mon-hspec-parse-buffer*', `*mon-hspec-root-dir*', `mon-line-test-content',
`mon-get-text-properties-category', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
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
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-hspec-w3m-spec-p'
`mon-hspec-stk-n-mv',`mon-hspec-parse-w3m', `mon-hspec-unparse-w3m',
`mon-hspec-unparse-w3m-to-buffer', `mon-hspec-out', `mon-hspec-find-w3m',
`*mon-hspec-parse-buffer*', `*mon-hspec-root-dir*', `mon-line-test-content',
`mon-get-text-properties-category', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (let ((the-spec (text-properties-at (point)))
        w3m-face-spec)
    (setq w3m-face-spec
          `(,(car (mon-hspec-w3m-spec-p 'w3m-href-anchor the-spec))
             ,@(cadr (mon-hspec-w3m-spec-p 'face the-spec))))
    (cond ((car w3m-face-spec) (car w3m-face-spec))
          ((cdr w3m-face-spec)
           (let ((mp-fc (cdr w3m-face-spec)))
             (car (or (mon-hspec-w3m-spec-p 'w3m-italic mp-fc)
                      (mon-hspec-w3m-spec-p 'w3m-bold   mp-fc)           
                      (mon-hspec-w3m-spec-p 'w3m-anchor mp-fc)))))
          ((not (cdr w3m-face-spec))  'plain-text))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:14:59-05:00Z}#{09527} - by MON>
(defun mon-hspec-out (prop)
  "Helper function for `mon-hspec-stk-n-mv' to direct parses to temp buffer.
Temporary buffer takes the value specified in:\n:VARIABLE `*mon-hspec-parse-buffer*'\n
:SEE-ALSO `mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p',
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-hspec-w3m-spec-p'
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-parse-w3m',
`mon-hspec-unparse-w3m', `mon-hspec-unparse-w3m-to-buffer', `mon-hspec-find-w3m',
`*mon-hspec-root-dir*', `mon-line-test-content', `mon-get-text-properties-category', `mon-plist-keys',
`mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer'.\n►►►"
  (get-buffer-create *mon-hspec-parse-buffer*)
  (terpri (get-buffer *mon-hspec-parse-buffer*))
  (if (null prop)
      (prin1 `(:plain-on " ") (get-buffer *mon-hspec-parse-buffer*))
    (prin1 prop (get-buffer *mon-hspec-parse-buffer*))))

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
specifed by `*mon-hspec-parse-buffer*' via the function `mon-hspec-out'.\n
:CALLED-BY `mon-hspec-parse-w3m' ;<- wrapper interface.\n
:SEE-ALSO `mon-hspec-unparse-w3m-to-buffer' `mon-hspec-header-line-p', 
`mon-hspec-w3m-spec-p', `mon-hspec-prop-type', `mon-hspec-unparse-w3m', 
`mon-hspec-find-w3m', `*mon-hspec-root-dir*', `mon-line-test-content',
`mon-get-text-properties-category', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (let ((keep-looking t)
        (curr-prop)) 
    (cond ((eq (point) (mon-g2be 1 t))
           (setq keep-looking nil))
          ((not (next-property-change (point)))
           (mon-g2be 1)
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
                         (eq (point) (mon-g2be 1 t)))
                     (mon-g2be 1)
                   (goto-char (next-property-change (point)))))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-27T22:15:07-05:00Z}#{09527} - by MON>
(defun mon-hspec-parse-w3m ()
  "Parse the Hyperlink Specification in current w3m buffer.\n
Invoke `mon-hspec-stk-n-mv' repeatedly on the contents of current *w3m* buffer
until `eobp' or there are no more text-properties left to parse.\n
Contents returned in buffer specified by `*mon-hspec-parse-buffer*'.\n
:SEE-ALSO `mon-hspec-unparse-w3m', `mon-hspec-unparse-w3m-to-buffer',
`mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p', 
`mon-hspec-header-line-p' `mon-hspec-href-p', `mon-hspec-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-out',
`mon-hspec-find-w3m', `*mon-hspec-root-dir*', `mon-line-test-content',
`mon-get-text-properties-category', `mon-plist-keys', `mon-list-all-properties-in-buffer',
`mon-nuke-text-properties-buffer'.\n►►►"
  (progn
    (mon-g2be -1)
    (let ((mhpw-hdr (mon-hspec-header-line-p)))
      (when mhpw-hdr 
        (mon-hspec-out (car mhpw-hdr))
        (mon-hspec-out (cadr mhpw-hdr))
        ;; Manually subst in the vig prop names by hand b/c
        ;; we may want to frob these differently later-on.
        (let ((mhpw-vig (caddr mhpw-hdr)))
          (mon-hspec-out 
           `(:plain-on ,(plist-get mhpw-vig :location-url-vig)
                       :plain-range ,(plist-get mhpw-vig :location-vig-range)))))
      (while (not (eobp)) (mon-hspec-stk-n-mv)))))

;;; ==============================
;;; :CHANGESET 1975
;;; :CREATED <Timestamp: #{2010-07-13T16:23:12-04:00Z}#{10282} - by MON KEY>
(defun mon-hspec-find-w3m ()
  "Return the first w3m buffer with a matching a CLHS page-title regexp.\n
Invokes `w3m-buffer-title' on the list returned by `w3m-list-buffers', signals
an error if that list is void or there are no matches.\n
Preference is weighted to `current-buffer' if it is a w3m buffer.\n
:CALLED-BY `mon-hspec-parse-w3m'
:SEE-ALSO `mon-hspec-unparse-w3m', `mon-hspec-unparse-w3m-to-buffer',
`mon-hspec-plain-p', `mon-hspec-bld-p', `mon-hspec-it-p', 
`mon-hspec-header-line-p', `mon-hspec-href-p', `mon-hspec-w3m-spec-p',
`mon-hspec-prop-type', `mon-hspec-stk-n-mv', `mon-hspec-out',
`mon-hspec-find-w3m', `*mon-hspec-root-dir*', `*mon-hspec-parse-buffer*'.\n►►►"
  (let* ((mhfw-wlb (w3m-list-buffers))
         (mhfw-hs-ttl-chk #'(lambda (mhfw-L-1)
                              (unless (null mhfw-L-1)
                                (or (and (string-match-p 
                                          "CLHS:.*" (w3m-buffer-title mhfw-L-1)) mhfw-L-1)
                                    (and (string-match-p 
                                          "Common Lisp HyperSpec \x28\x54\x4d\x29"
                                          (w3m-buffer-title mhfw-L-1)) mhfw-L-1)))))
         hspec-buf-myb)
    (if (null mhfw-wlb)
        (mon-format :w-fun  #'error
                    :w-spec '(":FUNCTION `mon-hspec-find-w3m' "
                              "-- can not find a *w3m* buffer"))
      (if (funcall mhfw-hs-ttl-chk (car (memq (current-buffer) (w3m-list-buffers))))
          (setq hspec-buf-myb (current-buffer))
        (dolist (mhfw-D-1 mhfw-wlb
                          (if hspec-buf-myb 
                              hspec-buf-myb
                            (mon-format :w-fun #'error
                                        :w-spec '(":FUNCTION `mon-hspec-find-w3m' "
                                                  "-- can not find a *w3m* buffer"))))
          (unless hspec-buf-myb
            (when (funcall mhfw-hs-ttl-chk mhfw-D-1)
              (setq hspec-buf-myb mhfw-D-1))))))))

 
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
;;; HEY. We just circum-invented ten's complement!
;;; :SEE (URL `http://en.wikipedia.org/wiki/Method_of_complements')
;;;
;;; :NOTE We currently shorten paths using a regexp lookup against a local path
;;; however, the code can be easily adapted for parsing and shortening remote
;;; paths wth "http://" in lieu of local paths with "file://"
;;; :SEE `mon-hspec-header-line-p's regexp for local variable `rplc-pth'
;;;
;;; :CHANGESET 1975 <Timestamp: #{2010-07-13T13:44:47-04:00Z}#{10282} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-12-29T02:38:01-05:00Z}#{09532} - by MON>
(defun mon-hspec-unparse-w3m (parse-w3m-buffer-or-file) ; return-parse-in-buffer
  "Round trip snarfed HTML lisp data output of `mon-hspec-parse-w3m' to plain text.\n
Return a two valued list with the form:\n
 \(\"<HSPEC-ENTRY-AS-PLAIN-TEXT>\" \( \(HSPEC-PLIST-1\) {...} \(HSPEC-PLIST-N\) \)\)\n
Read successive lisp lists in PARSE-W3M-BUFFER-OR-FILE extract the text element
in each list accumulate results to string at car of return value. Push each lisp list
containing the text-properties needed for facificaton, buttonizing, and xrefing
to the list at cdr of return value.\n
When the local URL path has been shortened adjust text-property offsets accordingly.\n
:SEE Comments in source for additional discussion and details.\n
:SEE-ALSO `mon-hspec-find-w3m', `mon-hspec-unparse-w3m-to-buffer',
`mon-hspec-stk-n-mv', `mon-hspec-out', `mon-hspec-plain-p', `mon-hspec-bld-p',
`mon-hspec-it-p', `mon-hspec-header-line-p', `mon-hspec-href-p',
`mon-hspec-w3m-spec-p', `mon-hspec-prop-type', `*mon-hspec-parse-buffer*',
`*mon-hspec-root-dir*'.\n►►►"
  (let ((unprs-mrk (make-marker))
        (onward t)
        (gthr-unprs 
         ;; :WAS (get-buffer-create *mon-hspec-unparse-buffer*))) ;;"*CL-HSPEC-UNPARSE*"
         (if (buffer-live-p *mon-hspec-unparse-buffer*)
             (with-current-buffer (get-buffer *mon-hspec-unparse-buffer*) (erase-buffer))
           (get-buffer-create *mon-hspec-unparse-buffer*)))
        big-parse)
    ;; :NOTE PARSE-W3M-BUFFER-OR-FILE is probably always `*mon-hspec-parse-buffer*'
    (with-current-buffer (get-buffer parse-w3m-buffer-or-file)
      (mon-g2be -1)
      (set-marker unprs-mrk (point))
      (mon-g2be 1)
      (insert "\n(\"►►►\")")
      (newline)
      (while (and unprs-mrk onward)
        (let (prs-sexp)
          (setq prs-sexp (read unprs-mrk))
          (if (equal "►►►" (car prs-sexp))
              (setq onward nil)
            (progn      
              (push prs-sexp big-parse)
              (princ (plist-get prs-sexp (car prs-sexp)) gthr-unprs)))))
      (mon-g2be 1)
      (search-backward-regexp "(\"►►►\")")
      (replace-match "")
      (mon-g2be -1))
    (setq big-parse (nreverse big-parse))
    (with-current-buffer (get-buffer gthr-unprs)
      ;; :NOTE why not just do:
      ;; `(,big-parse ,(mon-buffer-sub-no-prop))
      (let (fin-prs)
        (setq fin-prs (mon-buffer-sub-no-prop))
        (push fin-prs big-parse))
      (kill-buffer (get-buffer gthr-unprs)))
    ;; Rebuild the offsets against the local url in file line.
    (when (and (assoc :location (cdr big-parse))
               (assoc :location-url (cdr big-parse)))
      (let* ((rebld-ofst (assoc :location-url (cdr big-parse)))
             (calc-ofst (- (cdr (plist-get rebld-ofst :location-url-offset))
                           (cdr (plist-get rebld-ofst :location-url-range)))))
        (mapc #'(lambda (mhw-L-1)
                  (let* ((rng-type 
                          (cadr (or (plist-member mhw-L-1 :plain-range)
                                    (plist-member mhw-L-1 :italics-range)                    
                                    (plist-member mhw-L-1 :xref-range)
                                    (plist-member mhw-L-1 :bold-range)))))
                    (unless (or (null rng-type)
                                (null (car rng-type))
                                (null (cdr rng-type)))
                      (setf (car rng-type) (abs (- calc-ofst (car rng-type)))
                            (cdr rng-type) (abs (- calc-ofst (cdr rng-type)))))))
              (cdr big-parse))))
    big-parse))
;;
;;; :TEST-ME (progn (with-current-buffer "*w3m*" (mon-hspec-parse-w3m))
;;;           (mon-hspec-unparse-w3m *mon-hspec-parse-buffer*))

;;; ==============================
;;; :PREFIX "mhuwtb-"
;;; :CREATED <Timestamp: #{2009-12-29T13:09:57-05:00Z}#{09532} - by MON KEY>
(defun mon-hspec-unparse-w3m-to-buffer (&optional return-parse-in-buffer)
  "Entry point for parsing w3m CLHS buffers.\n
Signals an error when `mon-hspec-find-w3m' can not find a valid CLHS *w3m*
buffer to parse.\n
Returns roundtripped parse as the car and cdr of return value from
`mon-hspec-unparse-w3m' in a buffer and display the results.
When RETURN-PARSE-IN-BUFFER is non-nil it is a buffer-name in which to return
results. If RETURN-PARSE-IN-BUFFER does not exist it is created.\n
Default is to insert return value to the buffer named by the variable
`*mon-hspec-unparse-buffer*'.\n
:NOTE Unless RETURN-PARSE-IN-BUFFER is non-nil and current-buffer this procedure
will erase the existing contents of buffer `*mon-hspec-unparse-buffer*' or
RETURN-PARSE-IN-BUFFER prior to insertion.\n
:SEE-ALSO `mon-hspec-stk-n-mv', `mon-hspec-out', `mon-hspec-plain-p',
`mon-hspec-bld-p', `mon-hspec-it-p', `mon-hspec-header-line-p',
`mon-hspec-href-p', `mon-hspec-w3m-spec-p', `mon-hspec-prop-type',
`mon-hspec-find-w3m', `*mon-hspec-parse-buffer*', `*mon-hspec-root-dir*'.\n►►►"
  ;; mhuwtb-fnd
  (unwind-protect
      (let ((mhuwtb-fnd         (mon-hspec-find-w3m))
            (mhuwtb-dump-bfr    (if return-parse-in-buffer
                                    return-parse-in-buffer
                                  *mon-hspec-unparse-buffer*))
            mhuwtb-un-prs)
        (with-current-buffer mhuwtb-fnd (save-excursion (mon-hspec-parse-w3m)))
        (setq mhuwtb-un-prs (mon-hspec-unparse-w3m *mon-hspec-parse-buffer*))
        (if (buffer-live-p mhuwtb-dump-bfr)
            (unless (eq (current-buffer) (get-buffer mhuwtb-dump-bfr))
              (with-current-buffer (get-buffer mhuwtb-dump-bfr) (erase-buffer)))
          (get-buffer-create mhuwtb-dump-bfr))
        (princ (car mhuwtb-un-prs) (get-buffer mhuwtb-dump-bfr))
        (princ (concat "\n\n\n;;; ==============================\n"
                       ";;; :BEGIN-PARSED\n\n\n")
               (get-buffer mhuwtb-dump-bfr))
        (pp (cdr mhuwtb-un-prs) (get-buffer mhuwtb-dump-bfr))
        (unless (eq (current-buffer) (get-buffer mhuwtb-dump-bfr))
          (pop-to-buffer mhuwtb-dump-bfr)
          (mon-g2be -1))) ; t t)))
    (kill-buffer *mon-hspec-parse-buffer*)))
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
;;; :CREATED <Timestamp: #{2010-08-03T17:38:09-04:00Z}#{10312} - by MON>
;;;###autoload
(defun mon-help-CL-emacs-functions (&optional insertp intrp)
  "Emacs lisp functions equivalent to a cl.el feature but without warnings.\n
;; :EMACS-CL-NO-WARN
`completion--some' ;<- `some'
`edmacro-subseq'   ;<- `subseq'\n
:SEE :FILE mon-cl-compat.el mon-cl-compat-regexps.el mon-doc-help-CL.el
:SEE-ALSO `mon-help-CL-error-condition-restart',
`mon-help-CL-file-dir-functions', `mon-help-CL-pkgs', `mon-help-CL-symbols',
`mon-help-CL-loop', `mon-help-CL-do', `mon-help-CL-time',
`mon-help-CL-local-time', `mon-help-CL-swank-functions', `mon-help-CL-minion'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-emacs-functions :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-emacs-functions' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-emacs-functions)
;;; :TEST-ME (mon-help-CL-emacs-functions t)
;;; :TEST-ME (describe-function 'mon-help-CL-emacs-functions)
;;; :TEST-ME (apply 'mon-help-CL-emacs-functions '(t))

 
;;; ==============================
;;; :CHANGESET 1926
;;; :CREATED <Timestamp: #{2010-06-29T12:49:56-04:00Z}#{10262} - by MON KEY>
;;;###autoload
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
;; :ASDF-PATHNAMES-EXTERNAL        ;; :SEE :FILE sbcl/contrib/asdf/asdf.lisp
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
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-local-time', `mon-help-CL-loop', `mon-help-CL-time',
`mon-help-CL-symbols', `mon-help-CL-lispdoc', `mon-help-CL-slime-keys',
`mon-help-CL-swank-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-file-dir-functions :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-file-dir-functions' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-file-dir-function)
;;; :TEST-ME (mon-help-CL-file-dir-function t)
;;; :TEST-ME (describe-function 'mon-help-CL-file-dir-function)
;;; :TEST-ME (apply 'mon-help-CL-file-dir-function nil '(t))

 
;;; ==============================
;;; :CHANGESET 2077
;;; :CREATED <Timestamp: #{2010-08-24T17:03:29-04:00Z}#{10342} - by MON KEY>
;;;###autoload
(defun mon-help-CL-stream-keywords (&optional insertp intrp)
  "Table describing the keyword args to CL stream handlers, e.g. `open'.\n

Table has to format:
<KEYWORD> <ARG> <DESCRIPTION>

;; :STREAM-DIRECTION
:direction  { :input :output :io :probe }
            :input    input (default)
            :output   output
            :io       input & output
            :probe    none, returns a closed stream

;; :CL-STREAM-KEYWORD :IF-EXISTS
:if-exists  { nil :error :new-version :rename :supersede 
	      :rename-and-delete :overwrite :append }

nil                  return NIL
:error               signal an error
:new-version         next version (or error)
:rename              rename existing, create new
:supersede           replace file upon CLOSE
:rename-and-delete   rename and delete existing, create new
:overwrite           reuse existing file (position at start)
:append              reuse existing file (position at end)

;; :CL-STREAM-KEYWORD  :IF-DOES-NOT-EXIST
:if-does-not-exist { nil :error :create }
nil       return NIL
:error    signal an error
:create   create the file

;; :CL-STREAM-KEYWORD :ELEMENT-TYPE
:element-type

:element-type   :default            character (default)
:element-type   'character          character
:element-type   'signed-byte        signed byte
:element-type   'unsigned-byte      unsigned byte
:element-type   character subtype   character subtype
:element-type   integer subtype     integer subtype
:element-type   other               implementation-dependent

;; :CL-STREAM-KEYWORD :EXTERNAL-FORMAT
:external-format { default other }

:default   default (default)
other      implementation-dependent

:SEE info node `(ansicl) '\n
:SEE (URL `http://psg.com/~dlamkins/sl/chapter19.html')
:SEE-ALSO `mon-help-cl-types', `mon-help-CL-symbols', `mon-help-CL-sequences',
`mon-help-CL-iteration', `mon-help-CL-conses', `mon-help-CL-hash-tables',
`mon-help-CL-print', `mon-help-CL-streams', `mon-help-CL-reader',
`mon-help-CL-chars', `mon-help-CL-strings', `mon-help-CL-structures',
`mon-help-CL-arrays', `mon-help-CL-numbers', `mon-help-CL-object-CLOS',
`mon-help-CL-control-flow', `mon-help-CL-eval-compile',
`mon-help-CL-load-compile', `mon-help-CL-environment',
`mon-help-CL-package-functions', `mon-help-CL-intern-symbol',
`mon-help-CL-sharpsign-syntax', `mon-help-CL-loop', `mon-help-CL-time',
`mon-help-CL-error-condition-restart', `mon-help-CL-emacs-functions',
`mon-help-CL-sequence-predicates', `mon-help-CL-bit-byte-bool-logic',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-stream-keywords :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-stream-keywords' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-stream-keywords)
;;; :TEST-ME (mon-help-CL-stream-keywords t)
;;; :TEST-ME (apply 'mon-help-CL-stream-keywords '(t))

 
;;; ==============================
;;; :CHANGESET 2027
;;; :CREATED <Timestamp: #{2010-08-03T13:18:06-04:00Z}#{10312} - by MON KEY>
;;;###autoload
(defun mon-help-CL-error-condition-restart (&optional insertp intrp)
  "List of CL's error, condition, restart, debugger, and optimize symbols.\n
;; :CL-CONDITION-TYPES
`arithmetic-error'
`cell-error'
`condition'
`control-error'
`division-by-zero'
`end-of-file'
`error'
`file-error'
`floating-point-inexact'
`floating-point-invalid-operation'
`floating-point-overflow'
`floating-point-underflow'
`package-error'
`parse-error'
`print-not-readable'
`program-error'
`reader-error'
`serious-condition'
`simple-condition'
`simple-error'
`simple-type-error'
`simple-warning'
`storage-condition'
`stream-error'
`type-error'
`unbound-slot'
`unbound-variable'
`undefined-function'
`style-warning'
`warning'\n
;; :CL-ERROR-SIGNALERS
`singal'
`warn'
`error'
`cerror'
`package-error-package'
`invalid-method-error'
`method-combination-error'
`*break-on-signals*'
`*error-output*'\n
;; :CL-ERROR-SIGNALERS-ASSERTIONS
`assert'
`ccase'
`check-type'        ; \(:CLTL2 \(pp . 670-671\) \(pp . 889-890\)\)
`ctypecase'
`ecase'
`etypecase'\n
;; :CL-CONDITION
`condition'
`define-condition'  ; :NOTE Option `:report` not `defmethod'  for `print-object'
`handler-bind'
`handler-case'
`ignore-errors'
`make-condition'  ; `:format-control` `:format-arguments`\n
;; :CL-CONDITION-SLOT-READERS
`arithmetic-error-operands'
`arithmetic-error-operation'
`simple-condition-format-control'
`simple-condition-format-arguments'
`cell-error-name'
`stream-error-stream'
`file-error-pathname'
`type-error-datum'
`package-error-package'
`type-error-expected-type'
`print-not-readable-object'
`unbound-slot-instance'\n
;; :CL-RESTART
`compute-restarts'
`find-restart'
`invoke-restart'
`invoke-restart-interactively'
`restart-bind'
`restart-case'
`restart-name'
`with-condition-restarts'
`with-simple-restart'
`*query-io*'
;; :CL-RESTART-RESTARTS
`abort'
`continue'
`muffle-warning'
`store-value'
`use-value'
`warn'\n
;; :CL-DEBUG
`break'
`step'
`*debug-io*'
`*debugger-hook*'
`invoke-debugger'\n
;; :CL-OPTIMIZE
`optimize'
`speed'
`safety'
`debug'
`space'
`compilation-speed'\n
;; CL-CONTITION-PRECEDENCE
                      condition t
              warning condition t
style-warning warning condition t
    serious-condition condition t
error serious-condition condition t
‘cell-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘parse-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘storage-condition’, ‘serious-condition’, ‘condition’, ‘t’
‘simple-error’, ‘simple-condition’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘simple-condition’, ‘condition’, ‘t’
‘simple-warning’, ‘simple-condition’, ‘warning’, ‘condition’, ‘t’
‘restart’, ‘t’
‘type-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘simple-type-error’, ‘simple-condition’, ‘type-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘program-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘control-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘undefined-function’, ‘cell-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’
‘unbound-variable’, ‘cell-error’, ‘error’, ‘serious-condition’, ‘condition’, ‘t’

reader-error
package-error

:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-errors', `*mon-help-emacs-errors*', `mon-help-CL-time',
`mon-help-CL-local-time', `mon-help-CL-loop', `mon-help-CL-do',
`mon-help-CL-file-dir-functions', `mon-help-CL-pkgs', `mon-help-CL-symbols',
`mon-help-CL-lispdoc', `mon-help-CL-swank-functions', `mon-help-CL-slime-keys'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-error-condition-restart :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-error-condition-restart' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-error-condition-restart)
;;; :TEST-ME (mon-help-CL-error-condition-restart t)
;;; :TEST-ME (describe-function 'mon-help-CL-error-condition-restart)
;;; :TEST-ME (apply 'mon-help-CL-error-condition-restart '(t))

 
;;; ==============================
;;; :COURTEYS Ariel Badichi comp.lang.lisp
;;; :DATE 2010-08-06T20:39:48+03:00Z
;;; :SUBJECT Re: how to test for nil input in a recursive function?
;;; :CREATED <Timestamp: #{2010-08-07T17:39:07-04:00Z}#{10316} - by MON>
;;; :SEE (URL `http://coding.derkeiler.com/pdf/Archive/Lisp/comp.lang.lisp/2010-08/msg00114.pdf')
;;;###autoload
(defun mon-help-CL-sequence-predicates (&optional insertp intrp)
  "A Description of sequence predicate equivalences and truth constraints.\n
For a given predicate P with elements of a sequence S as universe of discourse:\n
 `every'    returns true iff (∀x)Px 
            ≡ 
 `notany'   (predicate proseq &rest proseqs)
            returns true iff (∀x)¬Px
            ≡ \(not \(every P s¹ ⃨ sⁿ\)\)\n
 `some'     returns true iff (∃x)Px
            ≡ \(or (
 `notevery' returns true iff (∃x)¬Px 
            ≡
\n
The latter two return false for the empty list, because truth of an
existentially qualified statement requires at least one member in the
universe of discourse.  They do not imply anything about every element
of a sequence, only about a particular one, if it exists.\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-char-logic' `mon-help-CL-time', `mon-help-CL-local-time',
`mon-help-CL-loop', `mon-help-CL-do', `mon-help-CL-bit-byte-bool-logic',
`mon-help-CL-file-dir-functions', `mon-help-CL-pkgs', `mon-help-CL-symbols',
`mon-help-CL-lispdoc', `mon-help-CL-swank-functions',
`mon-help-CL-slime-keys'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-sequence-predicates :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-sequence-predicates' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-sequence-predicates)
;;; :TEST-ME (mon-help-CL-sequence-predicates t)
;;; :TEST-ME (apply 'mon-help-CL-sequence-predicates (t))

 
;;; ==============================
;;; :CHANGESET 2077
;;; :CREATED <Timestamp: #{2010-08-24T14:35:39-04:00Z}#{10342} - by MON KEY>
;;;###autoload
(defun mon-help-CL-bit-byte-bool-logic (&optional insertp intrp)
  "List of Common Lisp bit and byte related functions.\n 
;; :CL-BYTE-FUNCTIONS
`byte'
`byte-position'
`byte-size'
`dpb'
`ldb'
`ldb-test'
`deposit-field'
`mask-field'
`read-byte'
`write-byte'\n
;; CL-BIT-FUNCTIONS
`#*'
`bit'
`sbit'
`bit-vector-p'
`simple-bit-vector-p'\n
 ___________________________________
|             |                     |
| :BIT-OP     |  :BIT-OP-PERFORMED  |
|_____________|_____________________|_________________________
¦             ¦                                               ¦
¦ `bit-eqv'   ¦ equivalence (exclusive nor)		      ¦
¦ `bit-not'   ¦ complement				      ¦
¦ `bit-ior'   ¦ inclusive or				      ¦
¦ `bit-xor'   ¦ exclusive or				      ¦
¦ `bit-nand'  ¦ complement of BIT-ARRAY1 and BIT-ARRAY2	      ¦
¦ `bit-nor'   ¦ complement of BIT-ARRAY1 or BIT-ARRAY2	      ¦
¦ `bit-andc1' ¦ and complement of BIT-ARRAY1 with BIT-ARRAY2  ¦
¦ `bit-andc2' ¦ and BIT-ARRAY1 with complement of BIT-ARRAY2  ¦
¦ `bit-orc1'  ¦ or complement of BIT-ARRAY1 with BIT-ARRAY2   ¦
¦ `bit-orc2'  ¦ or BIT-ARRAY1 with complement of BIT-ARRAY2   ¦
¦_____________¦_______________________________________________¦\n
;; :CL-BOOL-FUNCTIONS
 ____________________________________ 
|                                    |
|     (BOOLE <op> #b0011 #b0101)     |
|____________________________________| 
|               |      |      |      |
| :BOOL-OP      | :DEC | :BIN | :BIT |
|_______________|______|______|______|
¦               ¦      ¦      ¦      ¦
¦ `boole'       ¦      ¦      ¦      ¦
¦ `boole-1'     ¦  3   ¦   11 ¦ 0011 ¦
¦ `boole-2'     ¦  5   ¦  101 ¦ 0101 ¦
¦ `boole-and'   ¦  1   ¦    1 ¦ 0001 ¦
¦ `boole-andc1' ¦  4   ¦  100 ¦ 0100 ¦
¦ `boole-andc2' ¦  2   ¦   10 ¦ 0010 ¦
¦ `boole-c1'    ¦ -4   ¦ -100 ¦ 1100 ¦
¦ `boole-c2'    ¦ -6   ¦ -110 ¦ 1010 ¦
¦ `boole-clr'   ¦  0   ¦    0 ¦ 0000 ¦
¦ `boole-eqv'   ¦ -7   ¦ -111 ¦ 1001 ¦
¦ `boole-ior'   ¦  7   ¦  111 ¦ 0111 ¦
¦ `boole-nand'  ¦ -2   ¦  -10 ¦ 1110 ¦
¦ `boole-nor'   ¦ -8   ¦-1000 ¦ 1000 ¦
¦ `boole-orc1'  ¦ -3   ¦  -11 ¦ 1101 ¦
¦ `boole-orc2'  ¦ -5   ¦ -101 ¦ 1011 ¦
¦ `boole-set'   ¦ -1   ¦   -1 ¦ 1111 ¦
¦ `boole-xor'   ¦  6   ¦  110 ¦ 0110 ¦
¦_______________¦______¦______¦______¦\n
;; :CL-BIT-LOG-FUNCTIONS
`logcount'
`logbitp'
`logtest'
 ____________________________________________________________________
|            |                                             |         |
| :LOG-OP    | :LOG-OP-PERFORMED                           | :NO-ARG |
|____________|_____________________________________________|_________|
¦            ¦                                             ¦         ¦
¦ `logandc1’ ¦ and complement of INTEGER-1 with INTEGER-2  ¦  --     ¦
¦ `logandc2’ ¦ and INTEGER-1 with complement of INTEGER-2  ¦  --     ¦
¦ `logand’   ¦ and					   ¦  `-1`   ¦
¦ `logeqv’   ¦ equivalence (exclusive nor)		   ¦  `-1`   ¦
¦ `logior’   ¦ inclusive or				   ¦  `0`    ¦
¦ `lognand’  ¦ complement of INTEGER-1 and INTEGER-2	   ¦  --     ¦
¦ `lognor’   ¦ complement of INTEGER-1 or INTEGER-2	   ¦  --     ¦
¦ `lognot’   ¦ complement				   ¦  --     ¦
¦ `logorc1’  ¦ or complement of INTEGER-1 with INTEGER-2   ¦  --     ¦
¦ `logorc2’  ¦ or INTEGER-1 with complement of INTEGER-2   ¦  --     ¦
¦ `logxor’   ¦ exclusive or				   ¦  `0`    ¦
¦____________¦_____________________________________________¦_________¦\n
;; :CL-BIT-TYPES
`array-element-type'
`upgraded-array-element-type'
`stream-element-type'
`bit-vector'          ;<SYSTEM-CLASS>
`simple-bit-vector` 
`bit`
`signed-byte`
`unsigned-byte`
`boolean`
`simple-array`\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-char-logic',`mon-help-CL-time', `mon-help-CL-local-time',
`mon-help-CL-loop', `mon-help-CL-do', `mon-help-CL-sequence-predicates',
`mon-help-CL-file-dir-functions', `mon-help-CL-pkgs', `mon-help-CL-symbols',
`mon-help-CL-lispdoc', `mon-help-CL-swank-functions',
`mon-help-CL-slime-keys'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-bit-byte-bool-logic :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-bit-byte-bool-logic' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-bit-byte-bool-logic)
;;; :TEST-ME (mon-help-CL-bit-byte-bool-logic t)
;;; :TEST-ME (apply 'mon-help-CL-bit-byte-bool-logic '(t))

 
;;; ==============================
;;; :COURTESY Pascal Bourguignon HIS: `pjb-cl.el' WAS: `loop-doc'
;;; :MODIFICATIONS REPLACED: empty lines with '\n' escaped lisp forms in docstring
;;; :ADDED <Timestamp: Tuesday June 23, 2009 @ 03:22.54 PM - by MON KEY>
;;;###autoload
(defun mon-help-CL-loop (&optional insertp intrp)
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

;; :LOOP-SYNTAX-CLAUSES
A loop consists of the following types of clauses:

initial-final ::= initially | finally 

variables     ::= with | initial-final | for-as | repeat 

main          ::= unconditional | accumulation | conditional 
                  | termination | initial-final 

loop          ::= (loop [named name] {variables}* {main}*)

;; :LOOP-SYNTAX-ITERATION

for-as                 ::= {for | as} for-as-subclause {and for-as-subclause}*

for-as-subclause       ::= for-as-arithmetic | for-as-in-list 
                           | for-as-on-list | for-as-equals-then 
                           | for-as-across | for-as-hash | for-as-package 

for-as-arithmetic      ::= var [type-spec] [{from | downfrom | upfrom} expr1 ]
                           [{to | downto | upto | below | above} expr2]
                           [by expr3]

for-as-in-list         ::= var [type-spec] in expr1 [by step-fun]

for-as-on-list         ::= var [type-spec] on expr1 [by step-fun]

for-as-equals-then     ::= var [type-spec] = expr1 [then step-fun]

for-as-across          ::= var [type-spec] across vector

for-as-hash            ::= var [type-spec] being {each | the}
                           {hash-key | hash-keys | hash-value | hash-values}
                           {in | of} hash-table 
                           [using ({hash-value | hash-key} other-var)]

for-as-package         ::= var [type-spec] being {each | the}
                           for-as-package-keyword 
                           {in | of} package

for-as-package-keyword ::= symbol | present-symbol | external-symbol 
                           | symbols | present-symbols | external-symbols 

:LOOP-SYNTAX-ITERATION-FOR/AS-1

for var [type-spec] [{from | downfrom | upfrom} expr1]
        [{to | downto | upto | below | above} expr2]
        [by expr3]

as var [type-spec] [{from | downfrom | upfrom} expr1]
       [{to | downto | upto | below | above} expr2]
       [by expr3]

:LOOP-SYNTAX-ITERATION-FOR/AS-2

for var [type-spec] in expr1 [by step-fun]

as var  [type-spec] in expr1 [by step-fun]

:LOOP-SYNTAX-ITERATION-FOR/AS-3

for var [type-spec] on expr1 [by step-fun]

as var  [type-spec] on expr1 [by step-fun]

:LOOP-SYNTAX-ITERATION-FOR/AS-4

for var [type-spec] = expr1 [then expr2]

as var  [type-spec] = expr1 [then expr2]

:LOOP-SYNTAX-ITERATION-FOR/AS-5

for var [type-spec] across vector

as var  [type-spec] across vector

:LOOP-SYNTAX-ITERATION-FOR/AS-6

for var [type-spec] being {each | the} 
        {hash-key | hash-keys | hash-value | hash-values}
        {in | of} hash-table [using ({hash-value | hash-key} other-var)] 

as var [type-spec] being {each | the} 
       {hash-key | hash-keys | hash-value | hash-values}
       {in | of} hash-table [using ({hash-value | hash-key} other-var)] 


:LOOP-SYNTAX-ITERATION-FOR/AS-7

for var [type-spec] being {each | the}
        {symbol | present-symbol | external-symbol |
        symbols | present-symbols | external-symbols}
       {in | of} package

as var [type-spec] being {each | the}
       {symbol | present-symbol | external-symbol |
       symbols | present-symbols | external-symbols}
       {in | of} package

;; LOOP-SYNTAX-VALUE-ACCUMULATION

collect    expr [into var]
collecting expr [into var]
append     expr [into var]
appending  expr [into var]
nconc      expr [into var]
nconcing   expr [into var]
count      expr [into var] [type-spec]
counting   expr [into var] [type-spec]
sum        expr [into var] [type-spec]
summing    expr [into var] [type-spec]
maximize   expr [into var] [type-spec]
maximizing expr [into var] [type-spec]
minimize   expr [into var] [type-spec]
minimizing expr [into var] [type-spec]

;; :LOOP-VARIABLE-INITIALIZATION

with var [type-spec] [= expr] {and var [type-spec] [= expr]}*

;; :LOOP-CONDITIONAL-EXECUTION

if     expr clause {and clause}*
       [else clause {and clause}*] [end] 

when   expr clause {and clause}*
       [else clause {and clause}*] [end] 

unless expr clause {and clause}*
       [else clause {and clause}*] [end]

;; :LOOP-UNCONDITIONAL-EXECUTION

do     {expr}*
doing  {expr}*
return expr

;; :LOOP-TYPE-DECLARATION
type-spec   ::= of-type d-type-spec
d-type-spec ::= type-specifier | (d-type-spec . d-type-spec)

:SEE info node `(cl)Loop Facility'\n
:SEE info node `(ansicl) '\n
- Peter D. Karp's CL Loop Tutorial at:
:SEE (URL `http://www.ai.sri.com/~pkarp/loop.html')\n
- Yusuke Shinyama's CL Loop examples:
:SEE (URL `http://www.unixuser.org/~euske/doc/cl/loop.html')\n
- Cl-Cookbook review of loop at:
:SEE (URL `http://cl-cookbook.sourceforge.net/loop.html')\n
:SEE-ALSO `mon-help-CL-time', `mon-help-CL-local-time', `mon-help-CL-loop',
`mon-help-CL-do', `mon-help-CL-file-dir-functions', `mon-help-CL-pkgs',
`mon-help-CL-sequence-predicates', `mon-help-CL-symbols', `mon-help-CL-lispdoc',
`mon-help-CL-swank-functions', `mon-help-CL-slime-keys'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-loop :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-loop' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (describe-function 'mon-help-CL-loop)

;;; ==============================
;;; :CHANGESET 2384
;;; :CREATED <Timestamp: #{2011-01-08T13:41:48-05:00Z}#{11016} - by MON KEY>
;;;###autoload
(defun mon-help-CL-loop-usage (&optional insertp intrp)
"
;; :CLTL2-LOOP-ITERATION-CONTROL

Following examples illustrate control clauses for directed loop iteration.

;; Step two variables x and y, variable x is stepped before y is stepped; thus,
;; the value of y reflects the updated value of x:\n
  (loop for x from 1 to 5
        for y = nil then x
        collect (list x y))
  ;=> \(\(1 NIL\) \(2 2\) \(3 3\) \(4 4\) \(5 5\)\)\n
;; Step two variables x and y, in parallel:\n
  \(loop for x from 1 to 9 
        and y = nil then x 
        collect \(list x y\)\)\n 
  ;=> \(\(1 NIL\) \(2 1\) \(3 2\) \(4 3\) \(5 4\) \(6 5\) \(7 6\) \(8 7\) \(9 8\)\)\n
;; Print some numbers. Prints 5 lines.
  \(loop as i from 1 to 5 
       do \(print i\)\)\n 
 ; 1\n 2\n 3\n 4\n 5\n => NIL\n
;; Print every third number. Prints 4 lines.\n
  \(loop for i from 10 downto 1 by 3
        do \(print i\)\)\n
  ; 10\n   ; 7\n   ; 4\n   ; 1\n   ; => NIL\n
;; Step incrementally from the default starting value. Prints 5 lines.\n
  \(loop as i below 5 
        do \(print i\)\)\n
  ; 0\n  ; 1\n  ; 2\n  ; 3\n  ; 4\n  ;=> NIL\n
;; Print every item in a list. Prints 5 lines.\n
   \(loop for item in '\(1 2 3 4 5\)
         do \(print item\)\)\n
   ; 1\n   ; 2\n;   3\n;   4\n;   5\n   ;=> NIL\n
;; Print every other item in a list. Prints 3 lines.\n
  \(loop for item in '\(1 2 3 4 5\) by #'cddr 
        do \(print item\)\)\n
   ; 1\n   ; 3\n   ; 5\n   ;=> NIL\n
;; Destructure items of a list, and sum the x values using fixnum arithmetic.\n
   \(loop for \(item . x\) \(t . fixnum\)
         in '\(\(A . 1\) \(B . 2\) \(C . 3\)\)
         unless \(eq item 'B\) sum x\)\n
   ;=> 4\n
;; Collect successive tails of a list.\n
  \(loop for sublist on '\(a b c d\) 
        collect sublist\)\n 
  ;=> ((A B C D) (B C D) (C D) (D))\n
;; Print a list by using destructuring with the loop keyword ON. Prints 3 lines.\n
   \(loop for \(item\) on '\(1 2 3\) 
        do \(print item\)\)\n
   ; 1\n \n   ; 2\n \n   ; 3\n \n   ;=> NIL\n
;; Print items in a list without using destructuring. Prints 3 lines.\n
  \(loop for item in '\(1 2 3\) 
        do \(print item\)\)\n
   ; 1 \n   ; 2 \n   ; 3 \n   ; => NIL\n
;; Collect some numbers, initializing the loop variable `item` by setting it to
;; the result of evaluating expr1 on the first iteration, then setting it to the
;; result of evaluating expr2 on the second and subsequent iterations.\n
  \(loop for item = 1 then \(+ item 10\)
        repeat 5
        collect item\)\n
   ;=> \(1 11 21 31 41\)\n
;; Bind the variable char to the value of each element in an array (here
;; declared to be a simple strang), upcase each char and write the result to
;; `*standard-output*':\n
  \(loop for char across \(the simple-string \"A simple string\"\)
        do \(write-char \(char-upcase char\) *standard-output*\)\)\n
  ; A SIMPLE STRING \n ; NIL\n
;; Iterate over the symbols that are present but not external in the package
;; :COMMON-LISP-USER.  The loop variable x takes on the value of each symbol in
;; the specified package. :NOTE The package is referenced by its `package-name'.\n
  \(loop for x being each present-symbol of \\\"COMMON-LISP-USER\\\"
        do \(print x\)\)\n
   ; 1 \n   ; 2 \n   ; 3 \n   ; 4 \n   ; 5\n
;; Repeat an iteration terminating after a specified number of times.\n
   \(loop repeat 2
         do \(format t \"What I say twice is true.~%\"\)\)\n
   ; What I say twice is true.  \n   ; What I say twice is true.  \n   ; => NIL\n

;; Repeat an impossible number of times and return the result of doing so.\n
   \(loop repeat -15 
         do \(format t \"What you see is what you expect~%\"\)\)\n 
   ;=> NIL

;; :CLTL2-LOOP-END-TEST-CONTROL

;; Collect while values exist.\n
   \(let \(\(stack '\(a b c d e f\)\)\) 
     \(loop while stack 
           for item = \(length stack\) then \(pop stack\) 
           collect item\)\)\n
   ; => \(6 A B C D E F\)\n
;; Collect until some test is satisfied.
;; :NOTE Here we use until to terminate a loop that otherwise wouldn't.
   \(loop for x from 0 
         until \(eql x 3\) collect x\)\n
   ;=> \(0 1 2\)\n
;; Collect until some test is not satisfied.
;; :NOTE This is equivalent to a loop which collects while test is satisfied.\n
   \(loop for x from 0
         until \(not \(< x 3\)\) collect x\)\n
   ;=> \(0 1 2\)\n
;; Collect things using while to terminate a loop that otherwise wouldn't.
;; :NOTE that the loop keyword while occurs after the loop keyword when.\n
    \(loop for i fixnum from 3 
          when \(oddp i\) collect i 
          while \(< i 5\)\)\n
    ;=> \(3 5\)\n
;;; Deterimine if some test is always satisfied terminating the loop if the
;;; test form ever evaluates to nil.\n
    \(loop for i from 0 to 10 
          ;; The for construct terminates the loop.
          always \(< i 11\)\)\n
    ;=> T\n
   \(loop for i from 0 to 10 
         always \(< i 9\) 
         ;; The finally clause does not get evaluated.
         finally \(print \"you won't see this\"\)\)\n
   ;=> NIL\n
;; Determine if some test i never satisfied, terminating the loop if the test
;; form ever evaluates to non-nil.\n
   \(loop for i from 0 to 10 
         ;; The for construct terminates the loop.
         never \(> i 11\)\)\n
   ;=> T\n
   \(loop never t 
         ;; The finally clause does not get evaluated.
         finally \(print \"you won't see this\"\)\)\n
   ;=> NIL\n
;; Deterimine if thereis a value which satisfies a test terminating the loop if
;; the test form ever evaluates to non-nil.\n
   \(loop for i from 0 
         thereis \(when \(> i 10\) i\)\)\n
   ;=> 11\n
   \(loop thereis \"Here is my value\" 
         ;; The finally clause does not get evaluated.
         finally \(print \"you won't see this\"\)\)\n
   ;=> \"Here is my value\"\n

   (loop for i from 1 to 10 
         ;; The for construct terminates this loop, 
         thereis (> i 11)
         ;; So, the finally clause _is_ evaluated.
         finally (print i))
   ; 11\n   ;=> NIL

;; :CLTL2-LOOP-VALUE-ACCUMULATION

The loop keywords append, appending, collect, collecting, nconc, and nconcing
designate clauses that accumulate values in lists and return them.

;; Collect all the symbols in a list. 
   \(loop for i in '\(bird 3 4 turtle \(1 . 4\) horse cat\) 
          when \(symbolp i\) collect i\) 
   ;=> \(BIRD TURTLE HORSE CAT\)\n

;; Collect and return odd numbers.\n
   \(loop for i from 1 to 10
          if \(oddp i\) collect i\)\n
   ;=> \(1 3 5 7 9\)\n
;; Collect items into local variable, but don't return them.\n
   \(loop for i in '\(a b c d\) by #'cddr 
         collect i into my-list 
         finally \(print my-list\)\)\n
   ; (A C)   ;<- Printed to *standard-output*
   ; => NIL  ;<- Return value\n
;; Use loop append keyword to concatenate sublists.\n
   \(loop for x in '\(\(a\) \(b\) \(\(c\)\)\)
         append x\)\n
   ;=> \(A B \(C\)\)\n
;;; nconc sublists together. 
;; :NOTE Only lists made by the call to `cl:list' are modified.
   \(loop for i upfrom 0
         as x in '\(a b \(c\)\)
         nconc \(if \(evenp i\)
                   \(list x\)
                 '\(\)\)\)\n
;; Count the number of non-nil items in a list.\n
   \(loop for i in '\(a b nil c nil d e\)
         count i\)\n
   ;=> \(A \(C\)\)\n
;; Count the number of occurences of a charcater in a simple-string.\n
   \(loop for chr across \\\(the simple-string \\\"simple string\\\"\\\)
         count chr into cnt
         count \(char= chr #\\s\) into s
         count \(char= chr #\\i\) into i
         finally \(return \(list :total cnt :s-cnt s :i-cnt i\)\)\)\n
   ;=> \(:TOTAL 13 :S-CNT 2 :I-CNT 2\)
;; Sum the elements of a list.
   \(loop for i fixnum in '\(1 2 3 4 5\) ; With i declared a type `cl:fixnum'
         sum i\)\n
   ;=> 15\n
   \(loop for v float in '\(1.2 4.3 5.7\) ; With v declared as type `cl:float'
         sum \(* 2.0 v\)\)\n
   ;=> 22.4\n
;; Determine the upper bounds of integers in a list
   (loop for i in '(2 1 5 3 4) 
         maximize i) 
   ;=> 5\n
;; Determine the lower bounds of floats in a simple-vector\n
   \(loop for i across #\(2.0 1.3010 1.3002 1.35 3.1 4.0\)
         minimize i\)\n
   ;=> 1.3002\n
   \(loop for v in '\(1.2 4.3 5.7\)
         ;; Here the fixnum declaration applies to the implicit internal loop
         ;; variable holding the maximum value.
         maximize \(round v\) fixnum\)\n
   ;=> 6\n
   \(loop for v float in series 
         ;; Here the fixnum declaration applies to the loop variable result.
         minimize \(round v\) into result fixnum
         finally \(return result\)\)
   ;=> 1\n
;; Interleave collected values of two lists into one list using loop keywords
;; collect and append :NOTE That the items accumulated by the collect and
;; append clauses are interleaved in the result list, according to the order in
;; which the clauses were executed.\n
   \(loop for letters in '\(A      B  C  D    E\)
         for nums    in '\(\(1 2\) \(\) \(\) \(3 4\) \(\)\)
         collect letters
         append nums\)
   ;=> \(A 1 2 B C D 3 4 E\)\n
;; Count and collect names and ages into variables, return as if by `cl:values'.\n
    \(loop for name in '\(fred sue alice joe june\)
          for age in '\(22 26 19 20 10\) 
          collect \(nconc \(list name\) age\) into name-and-age-list
          count name into name-count 
          sum age into total-age
          finally \(return \(values 
                           \(nconc \(list 'AVERAGE-AGE\) \(round total-age name-count\)\)
                           name-and-age-list\)\)\)\n
    ;=>  \(AVERAGE-AGE . 19\)
    ;    \(\(FRED . 22\) \(SUE . 26\) \(ALICE . 19\) \(JOE . 20\) \(JUNE . 10\)\)\n

;; :CLTL2-LOOP-VARIABLE-INITIALIZATIONS

;; Use sequential binding of variables to local to a loop to initialize some
;; variables depending on the values of previously bound variables.
;; These bindings occur in sequence.\n
   \(loop with a = 1
         with b = \(+ a 2\)
         with c = \(+ b 3\)
         with d = \(+ c 4\)
         return \(list a b c d\)\)\n
   ;=> \(1 3 6 10\)\n
;; The and bindings occur in parallel and their initialized values rely on the
;; lexical bindings established in the let form.
;; The with/and idiom is similiar to `cl:let*'\n
   \(let \(\(a 5\) \(b 10\) \(c 1729\)\)
     \(loop with a = 1 
           and  b = \(+ a 2\) 
           and  c = \(+ b 3\) 
           and  d = \(+ c 4\) 
           return \(list a b c d\)\)\)
   ;=> \(1 7 13 1733\)

;; :CLTL2-LOOP-CONDITIONAL-EXECUTION

;; Group conditional clauses into a block.\n
   \(loop for i in \(loop for num-list upfrom 5 upto 30 by 3 collect num-list\)
         when \(oddp i\) 
           do \(terpri\)
           and do \(format t \"got odd -> ~d\" i\)
           and collect i into odd-numbers 
         else     ;I is even           ;
          collect i into even-numbers 
         finally 
         \(return \(values odd-numbers even-numbers\)\)\)\n
   ; got odd -> 5
   ; got odd -> 11
   ; got odd -> 17
   ; got odd -> 23
   ; got odd -> 29
   ; => (5 11 17 23 29)
   ;    (8 14 20 26)

;; Collect numbers larger than 3 from a list of numbers.\n
   \(loop for i in '\(1 2 3 4 5 6\)
         when \(and \(> i 3\) i\)
         ;; Here the loop keyword it refers to \(and \(> i 3\) i\)
         collect it\)\n
    ;=> \(4 5 6\)\n

;; Find the first occurence of a number in a list and return it immediately.\n
   \(loop for i in '\(1 2 3 4 5 6\) 
          ;; Here the and form is used so the loop keyword it can refer back
         when \(and \(= i 3\) i\)
         return it\)\n
    ;=> 4\n

;; The preceding example is similar to the following one.\n
   \(loop for i in '\(1 2 3 4 5 6\) 
         thereis \(and \(= i 3\) i\)\)
    ;=> 3\n
;; Nesting conditional clauses using loop keyword end.\n
    \(loop for x from 0 to 3  
      do \(print x\) 
      if \(zerop \(mod x 2\)\)
        do \(princ \" a\"\) 
          and if \(zerop \(floor x 2\)\)
                do \(princ \" b\"\)
             ;; Without the END marker here, the last AND would apply to the 
             ;; inner IF rather than the outer one.
              end
          and do \(princ \" c\"\)\)
   ; 0  a b c
   ; 1 
   ; 2  a c
   ; 3 
   ;=> NIL

;; :CLTL2-LOOP-UNCONDITIONAL-EXECUTION

evaluates the specified expressions wherever they occur
in the expanded form of loop.

;; Print some numbers. Prints 5 lines.
   \(loop for i from 1 to 5 
         do \(print i\)\)\n
   ; 1 \n   ; 2 \n   ; 3 \n   ; 4 \n   ; 5 \n   ;=> NIL\n


;; Print numbers and squares. Apply loop do construct to multiple forms.
   (loop for i from 1 to 4 
         do (print i) 
            (print (* i i)))
   ; 1 \n   ; 1 \n   ; 2 \n   ; 4 \n   ; 3 \n   ; 9 \n   ; 4 \n   ; 16
    ;=>NIL\n

;; Signal an exceptional condition by terminating an iteration with return
   \(define-condition bogus-number \(error\)
     \(\(argument :reader bogus-number-argument :initarg :argument\)\)
     \(:report \(lambda \(condition stream\)
                \(format stream \"~S is not a number.\"
                        \(bogus-number-argument condition\)\)\)\)\)\n
    \(let \(\(n 'a\)\)
      \(loop when \(numberp n\) 
            return n
            else
             do \(cerror \"Enter a number.\"
                       'bogus-number :argument n\)
                \(format t \"~&Type a number: \"\)
                \(setf n \(read\)\)
               \(fresh-line\)\)\)\n
  ; Type a number: <????>
  ;=> <????>

;; :CLTL2-LOOP-DATA-TYPE-SPECIFIERS

;; Iterate using old style loop type type specifier syntax:\n
   \(loop for i fixnum upfrom 3 to 10 collect i\)\n
   ;=> \(3 4 5 6 7 8 9 10\)\n

;; Iterate using new style `of-type` loop type specifier syntax:\n
   (loop for i of-type fixnum upfrom 3 to 6 by 2 collect i)
   ;=> \(3 5\)\n

;; :CLTl2-LOOP-DESTRUCTURING

;; Use destructuring to bind loop variables in parallel on each loop iteration
;; with type declarations to boot.\n
   \(loop for \(a b c\) \(float integer simple-string\)
          in '\(\(1.0 2 \"4\"\) \(5.0 6 \"8\"\) \(8.0 2 \"10\"\)\) 
          collect \(list a c b c c a\)\)\n
   ;=> \(\(1.0 \"4\" 2 \"4\" \"4\" 1.0\)
   ;    \(5.0 \"8\" 6 \"8\" \"8\" 5.0\)
   ;    \(8.0 \"10\" 2 \"10\" \"10\" 8.0\)\)\n


  (loop with (a b) of-type float = '(1.0 2.0)
        and (c d) of-type integer = '(3 4)
        and (e f)
        return (list a b c d e f))

I need to iterate a list some elements will be dropped
others will stay. What is an elegant form using `loop'?:\n

 \(loop :for <ELEMENT> in <LIST>
        :when \(<TEST> <ELEMENT>\) 
        :collect <ELEMENT>\)\n

:NOTE The blocks of loop usage examples presented above when preceded with the
prefix \":CLTL2-LOOP\" are excerpted from from the LaTeX to HTML extraction of:
 Steel, Guy L. \"Common Lisp the Language, Second Edition\". Butterworth-Heinemann, 1990.
:SEE (URL `http://lccn.loc.gov/89026016')
:SEE (URL `ftp://ftp.cs.cmu.edu/user/ai/lang/lisp/doc/cltl/cltl_ht.tgz')


:SEE-ALSO `mon-help-CL-loop', `mon-help-CL-do', `mon-help-CL-iteration'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-loop-usage :insertp t)
    (message (concat ":FUNCTION `mon-help-CL-loop-usage' " 
                     "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-loop-usage)
;;; :TEST-ME (mon-help-CL-loop-usage t)
;;; :TEST-ME (describe-function 'mon-help-CL-loop-usage)

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-do (&optional insertp intrp)
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
As with LET, DO's return values accumulate within the body except, DO includes
built-in support for iterative testing and returning within the DO form
itself.\n
Basically what you would otherwise do inside the body of LET get's DOne in the
test-form result-form section \(which is why the form looks so much like the
body of a LET and why it happens in a seperate list.)\n
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
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-time', `mon-help-CL-local-time', `mon-help-CL-loop',
`mon-help-CL-do', `mon-help-CL-file-dir-functions', `mon-help-CL-pkgs',
`mon-help-CL-sequence-predicates', `mon-help-CL-symbols', `mon-help-CL-lispdoc',
`mon-help-CL-swank-functions', `mon-help-CL-slime-keys'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-do :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-do' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-do)
;;; :TEST-ME (mon-help-CL-do t)

 
;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 15, 2009 @ 12:50.16 PM - by MON KEY>
;;;###autoload
(defun mon-help-CL-time (&optional insertp intrp)
  "List of CL's time related symbols.\n
;; :CL-TIME  
`decode-universal-time'
`encode-universal-time'
`get-decoded-time'
`get-internal-real-time'
`get-universal-time'
`internal-time-units-per-second'\n
:NOTE Common Lisp's time is not like Unix' time. For example, the function
`get-decoded-time' returns nine values specifying the current time as follows:
second, minute, hour, date, month, year, day of week \(0 = Monday\), T
\(daylight savings times\) or NIL \(standard time\), and timezone.\n
CL-USER> \(get-decoded-time\)\n
 => 14     ;second\n44     ;minute\n12     ;hour\n15     ;date\n7      ;month
    2009   ;year\n2      ;day\nT      ;dayligt-p\n5      ;zone\n
:SEE `SB-POSIX:TIME'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-time', `mon-help-CL-local-time', `mon-help-CL-loop',
`mon-help-CL-do', `mon-help-CL-file-dir-functions', `mon-help-CL-pkgs',
`mon-help-CL-sequence-predicates', `mon-help-CL-symbols', `mon-help-CL-lispdoc',
`mon-help-CL-swank-functions', `mon-help-CL-slime-keys'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-time :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-time' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;;
;;; :TEST-ME (describe-function 'mon-help-CL-time)

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-sequences (&optional insertp intrp)
  "
:: CL-SEQUENCES
`sequence'
`copy-seq'
`elt'
`fill'
`make-sequence'
`subseq'
`map'
`map-into'
`reduce'
`count'
`length'
`reverse'
`sort'
`find'
`position'
`search'
`mismatch'
`replace'
`substitute'
`concatenate'
`merge'
`remove'
`remove-duplicates'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-sequences :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-sequences' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-sequences)     
;;; :TEST-ME (mon-help-CL-sequences t)   
;;; :TEST-ME (mon-help-CL-sequences '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-iteration (&optional insertp intrp)
  "
:CL-ITERATION
`do'
`dotimes'
`dolist'
`loop'
`loop-finish'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-iteration :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-iteration' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-iteration)     
;;; :TEST-ME (mon-help-CL-iteration t)   
;;; :TEST-ME (mon-help-CL-iteration '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-conses (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-CONSES
`list'
`cons'
`consp'
`atom'
`rplaca'
`car'
`copy-tree'
`sublis'
`subst'
`tree-equal'
`copy-list'
`list'
`list-length'
`listp'
`make-list'
`push'
`pop'
`first'
`nth'
`endp'
`null'
`nconc'
`append'
`revappend'
`butlast'
`last'
`ldiff'
`nthcdr'
`rest'
`member'
`mapc'
`acons'
`assoc'
`copy-alist'
`pairlis'
`rassoc'
`get-properties'
`getf'
`remf'
`intersection'
`adjoin'
`pushnew'
`set-difference'
`set-exclusive-or'
`subsetp'
`union'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-conses :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-conses' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-conses)     
;;; :TEST-ME (mon-help-CL-conses t)   
;;; :TEST-ME (mon-help-CL-conses '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-hash-tables (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-HASH-TABLES
`hash-table'
`make-hash-table'
`hash-table-p'
`hash-table-count'
`hash-table-rehash-size'
`hash-table-rehash-threshold'
`hash-table-size'
`hash-table-test'
`gethash'
`remhash'
`maphash'
`with-hash-table-iterator'
`clrhash'
`sxhash'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-hash-tables :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-hash-tables' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-hash-tables)     
;;; :TEST-ME (mon-help-CL-hash-tables t)   
;;; :TEST-ME (mon-help-CL-hash-tables '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-print (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-PRINT
`copy-pprint-dispatch'
`format'
`formatter'
`pprint-dispatch'
`pprint-exit-if-list-exhausted'
`pprint-fill'
`pprint-indent'
`pprint-logical-block'
`pprint-newline'
`pprint-pop'
`pprint-tab'
`print-object'
`print-unreadable-object'
`print-not-readable'
`print-not-readable-object'
`set-pprint-dispatch'
`write'
`write-to-string'
`*print-array*'
`*print-base*'
`*print-case*'
`*print-circle*'
`*print-escape*'
`*print-gensym*'
`*print-level*'
`*print-lines*'
`*print-miser-width*'
`*print-pprint-dispatch*'
`*print-pretty*'
`*print-readably*'
`*print-right-margin*'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-format', `mon-help-CL-symbols', `mon-help-CL-sequences',
`mon-help-CL-iteration', `mon-help-CL-conses', `mon-help-CL-hash-tables',
`mon-help-CL-print', `mon-help-CL-streams', `mon-help-CL-reader',
`mon-help-CL-chars', `mon-help-CL-strings', `mon-help-CL-structures',
`mon-help-CL-arrays', `mon-help-CL-numbers', `mon-help-CL-object-CLOS',
`mon-help-CL-control-flow', `mon-help-CL-eval-compile',
`mon-help-CL-load-compile', `mon-help-CL-environment',
`mon-help-CL-package-functions', `mon-help-CL-intern-symbol',
`mon-help-CL-sharpsign-syntax', `mon-help-CL-loop', `mon-help-CL-time',
`mon-help-CL-error-condition-restart', `mon-help-CL-emacs-functions',
`mon-help-CL-sequence-predicates', `mon-help-CL-bit-byte-bool-logic',
`mon-help-CL-stream-keywords', `mon-help-CL-slime-keys',
`mon-help-CL-swank-functions', `mon-help-CL-local-time',
`mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-print :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-print' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-print)     
;;; :TEST-ME (mon-help-CL-print t)   
;;; :TEST-ME (mon-help-CL-print '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-streams (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-STREAMS
`stream'
`broadcast-stream'
`concatenated-stream'
`echo-stream'
`file-stream'
`string-stream'
`synonym-stream'
`two-way-stream'
`input-stream-p'
`interactive-stream-p'
`open-stream-p'
`stream-element-type'
`streamp'
`read-byte'
`write-byte'
`peek-char'
`read-char'
`read-char-no-hang'
`terpri'
`unread-char'
`write-char'
`read-line'
`write-string'
`read-sequence'
`write-sequence'
`file-length'
`file-position'
`file-string-length'
`open'
`stream-external-format'
`with-open-file'
`close'
`with-open-stream'
`listen'
`clear-input'
`finish-output'
`y-or-n-p'
`make-synonym-stream'
`synonym-stream-symbol'
`broadcast-stream-streams'
`make-broadcast-stream'
`make-two-way-stream'
`two-way-stream-input-stream'
`echo-stream-input-stream'
`make-echo-stream'
`concatenated-stream-streams'
`make-concatenated-stream'
`get-output-stream-string'
`make-string-input-stream'
`make-string-output-stream'
`with-input-from-string'
`with-output-to-string'
`stream-error'
`stream-error-stream'
`end-of-file'
`*debug-io*'
`*terminal-io*'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-print', `mon-help-CL-format', `mon-help-CL-symbols',
`mon-help-CL-sequences', `mon-help-CL-iteration', `mon-help-CL-conses',
`mon-help-CL-hash-tables', `mon-help-CL-print', `mon-help-CL-reader',
`mon-help-CL-chars', `mon-help-CL-strings', `mon-help-CL-structures',
`mon-help-CL-arrays', `mon-help-CL-numbers', `mon-help-CL-object-CLOS',
`mon-help-CL-control-flow', `mon-help-CL-eval-compile',
`mon-help-CL-load-compile', `mon-help-CL-environment',
`mon-help-CL-package-functions', `mon-help-CL-intern-symbol',
`mon-help-CL-sharpsign-syntax', `mon-help-CL-loop', `mon-help-CL-time',
`mon-help-CL-error-condition-restart', `mon-help-CL-emacs-functions',
`mon-help-CL-sequence-predicates', `mon-help-CL-bit-byte-bool-logic',
`mon-help-CL-stream-keywords', `mon-help-CL-slime-keys',
`mon-help-CL-swank-functions', `mon-help-CL-local-time',
`mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-streams :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-streams' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-streams)
;;; :TEST-ME (mon-help-CL-streams t)   
;;; :TEST-ME (mon-help-CL-streams '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-reader (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-READER
`copy-readtable'
`make-dispatch-macro-character'
`read'
`read-delimited-list'
`read-from-string'
`readtable-case'
`readtablep'
`set-dispatch-macro-character'
`set-macro-character'
`set-syntax-from-char'
`with-standard-io-syntax'
`reader-error'
`*read-base*'
`*read-default-float-format*'
`*read-eval*'
`*read-suppress*'
`*readtable*'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-streams', `mon-help-CL-print', `mon-help-CL-format',
`mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-reader :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-reader' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-reader)     
;;; :TEST-ME (mon-help-CL-reader t)   
;;; :TEST-ME (mon-help-CL-reader '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-chars (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-CHARS
`character'       <TYPE>
`base-char'       <TYPE>
`standard-char'   <TYPE>
`extended-char'   <TYPE>
`char'
`char='
`character'       ;<FUNCTION>
`characterp'
`alpha-char-p'
`alphanumericp'
`digit-char'
`digit-char-p'
`graphic-char-p'
`standard-char-p'
`char-upcase'
`upper-case-p'
`char-code'
`char-int'
`code-char'
`char-code-limit'
`char-name'
`name-char'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-chars :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-chars' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-chars)     
;;; :TEST-ME (mon-help-CL-chars t)   
;;; :TEST-ME (mon-help-CL-chars '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-strings (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-STRINGS
`base-string'
`simple-string'
`simple-base-string'
`simple-string-p'
`char'
`string'
`string-upcase'
`string-trim'
`string='
`stringp'
`make-string'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-strings :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-strings' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-strings)     
;;; :TEST-ME (mon-help-CL-strings t)   
;;; :TEST-ME (mon-help-CL-strings '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-structures (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-STRUCTURES
`defstruct'
`copy-structure'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-structures :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-structures' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-structures)     
;;; :TEST-ME (mon-help-CL-structures t)   
;;; :TEST-ME (mon-help-CL-structures '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-arrays (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-ARRAYS
`array'
`simple-array'
`simple-vector'
`bit-vector'
`simple-bit-vector'
`make-array'
`adjust-array'
`adjustable-array-p'
`aref'
`array-dimension'
`array-dimensions'
`array-element-type'
`array-has-fill-pointer-p'
`array-displacement'
`array-in-bounds-p'
`array-rank'
`array-row-major-index'
`array-total-size'
`arrayp'
`fill-pointer'
`row-major-aref'
`upgraded-array-element-type'
`array-dimension-limit'
`array-rank-limit'
`array-total-size-limit'
`simple-vector-p'
`svref'
`vector'
`vector-pop'
`vector-push'
`vectorp'
`bit'
`bit-and'
`bit-vector-p'
`simple-bit-vector-p'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-arrays :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-arrays' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-arrays)     
;;; :TEST-ME (mon-help-CL-arrays t)   
;;; :TEST-ME (mon-help-CL-arrays '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-numbers (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-NUMBERS
`number'
`real'
`short-float'
`rational'
`ratio'
`integer'
`signed-byte'
`unsigned-byte'
`mod'
`bit'
`fixnum'
`bignum'
`='
`max'
`minusp'
`zerop'
`floor'
`sin'
`asin'
`pi'
`sinh'
`*'
`+'
`-'
`/'
`1+'
`abs'
`evenp'
`exp'
`gcd'
`incf'
`lcm'
`log'
`mod'
`signum'
`sqrt'
`random-state'
`make-random-state'
`random'
`random-state-p'
`*random-state*'
`numberp'
`cis'
`complex'
`complexp'
`conjugate'
`phase'
`realpart'
`upgraded-complex-part-type'
`realp'
`numerator'
`rational'
`rationalp'
`ash'
`integer-length'
`integerp'
`parse-integer'
`boole'
`boole-1'
`logand'
`logbitp'
`logcount'
`logtest'
`byte'
`deposit-field'
`dpb'
`ldb'
`ldb-test'
`mask-field'
`most-positive-fixnum'
`decode-float'
`float'
`floatp'
`most-positive-short-float'
`short-float-epsilon'
`arithmetic-error'
`arithmetic-error-operands'
`division-by-zero'
`floating-point-invalid-operation'
`floating-point-inexact'
`floating-point-overflow'
`floating-point-underflow'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-numbers :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-numbers' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-numbers)     
;;; :TEST-ME (mon-help-CL-numbers t)   
;;; :TEST-ME (mon-help-CL-numbers '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-object-CLOS (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-OBJECTS
`function-keywords'
`ensure-generic-function'
`allocate-instance'
`reinitialize-instance'
`shared-initialize'
`update-instance-for-different-class'
`update-instance-for-redefined-class'
`change-class'
`slot-boundp'
`slot-exists-p'
`slot-makunbound'
`slot-missing'
`slot-unbound'
`slot-value'
`method-qualifiers'
`no-applicable-method'
`no-next-method'
`remove-method'
`make-instance'
`make-instances-obsolete'
`make-load-form'
`make-load-form-saving-slots'
`with-accessors'
`with-slots'
`defclass'
`defgeneric'
`defmethod'
`find-class'
`next-method-p'
`call-method'
`call-next-method'
`compute-applicable-methods'
`define-method-combination'
`find-method'
`add-method'
`initialize-instance'
`class-name'
`class-of'
`unbound-slot'
`unbound-slot-instance'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-object-CLOS :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-object-CLOS' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-object-CLOS)     
;;; :TEST-ME (mon-help-CL-object-CLOS t)   
;;; :TEST-ME (mon-help-CL-object-CLOS '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-control-flow (&optional insertp intrp)
  "
;; CL-CONTROL-FLOW
`apply'
`defun'
`fdefinition'
`fboundp'
`fmakunbound'
`flet'
`funcall'
`function'                    <SPECIAL-OPERATOR>
`function-lambda-expression'
`functionp'
`compiled-function-p'
`call-arguments-limit'
`lambda-list-keywords'
`lambda-parameters-limit'
`defconstant'
`defparameter'
`destructuring-bind'
`let'
`let*'
`progv'
`setq'
`psetq'
`block'
`catch'
`go'
`return-from'
`return'
`tagbody'
`throw'
`unwind-protect'
`nil'
`not'
`t'
`eq'
`eql'
`equal'
`equalp'
`identity'
`complement'
`constantly'
`every'
`and'
`cond'
`if'
`or'
`when'
`case'
`typecase'
`multiple-value-bind'
`multiple-value-call'
`multiple-value-list'
`multiple-value-prog1'
`multiple-value-setq'
`values'
`values-list'
`multiple-values-limit'
`nth-value'
`prog'
`prog1'
`progn'
`define-modify-macro'
`defsetf'
`define-setf-expander'
`get-setf-expansion'
`setf'
`shiftf'
`rotatef'
`control-error'
`program-error'
`undefined-function'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-control-flow :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-control-flow' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-control-flow)     
;;; :TEST-ME (mon-help-CL-control-flow t)   
;;; :TEST-ME (mon-help-CL-control-flow '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-eval-compile (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-EVAL-COMPILE
`lambda'
`compile'
`eval'
`eval-when'
`load-time-value'
`quote'
`compiler-macro-function'
`define-compiler-macro'
`defmacro'
`macro-function'
`macroexpand'
`define-symbol-macro'
`symbol-macrolet'
`proclaim'
`declaim'
`declare'
`ignore'
`dynamic-extent'
`type'
`inline'
`ftype'
`declaration'
`optimize'
`special'
`locally'
`the'
`special-operator-p'
`constantp'
`*macroexpand-hook*'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-eval-compile :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-eval-compile' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-eval-compile)     
;;; :TEST-ME (mon-help-CL-eval-compile t)   
;;; :TEST-ME (mon-help-CL-eval-compile '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-load-compile (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-LOAD-COMPILE
`compile-file'
`compile-file-pathname'
`load'
`provide'
`with-compilation-unit'
`*features*'
`*compile-file-pathname*'
`*load-pathname*'
`*compile-print*'
`*load-print*'
`*modules*'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-load-compile :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-load-compile' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-load-compile)     
;;; :TEST-ME (mon-help-CL-load-compile t)   
;;; :TEST-ME (mon-help-CL-load-compile '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-environment (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-ENVIRONMENT-INTROSPECT
`decode-universal-time'
`encode-universal-time'
`get-universal-time'
`sleep'
`apropos'
`describe'
`describe-object'
`trace'
`step'
`time'
`internal-time-units-per-second'
`get-internal-real-time'
`get-internal-run-time'
`disassemble'
`documentation'
`room'
`ed'
`inspect'
`dribble'
`-'
`+'
`*'
`/'
`lisp-implementation-type'
`short-site-name'
`machine-instance'
`machine-type'
`machine-version'
`software-type'
`user-homedir-pathname'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-environment :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-environment' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-environment) 
;;; :TEST-ME (mon-help-CL-environment t)   
;;; :TEST-ME (mon-help-CL-environment '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-package-functions (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-PACKAGE
`export'
`find-symbol'
`find-package'
`find-all-symbols'
`import'
`list-all-packages'
`rename-package'
`shadow'
`shadowing-import'
`delete-package'
`make-package'
`with-package-iterator'
`unexport'
`unintern'
`in-package'
`unuse-package'
`use-package'
`defpackage'
`do-symbols'
`intern'
`package-name'
`package-nicknames'
`package-shadowing-symbols'
`package-use-list'
`package-used-by-list'
`packagep'
`package-error'
`package-error-package'
`*package*'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'..\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-package-functions :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-package-functions' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-package-functions)
;;; :TEST-ME (mon-help-CL-package-functions t)
;;; :TEST-ME (mon-help-CL-package-functions '(t))

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-intern-symbol (&optional insertp intrp)
  "
;; :CL-FUNCTIONS-INTERN
`keyword'
`symbolp'
`keywordp'
`make-symbol'
`copy-symbol'
`gensym'
`gentemp'
`symbol-function'
`symbol-name'
`symbol-package'
`symbol-plist'
`symbol-value'
`get'
`remprop'
`boundp'
`makunbound'
`set'
`unbound-variable'
`*gensym-counter*'\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-intern-symbol :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-intern-symbol' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-intern-symbol)
;;; :TEST-ME (mon-help-CL-intern-symbol t)
;;; :TEST-ME (mon-help-CL-intern-symbol '(t))

 
;;; ==============================
;;; :CHANGESET 2357
;;; :CREATED <Timestamp: #{2010-12-08T16:50:24-05:00Z}#{10493} - by MON KEY>
;;;###autoload
(defun mon-help-CL-types (&optional insertp intrp)
  "The Common Lisp hierarhy of types.\n
;; :COMMON-LISP-TYPE-HIERARCHY\n
T
*	nil           ;; all types
*	boolean       ;; ≣ \(MEMBER t nil\)
*	\(not <TYPE>\)
*	\(and {<TYPE>}*\)
*	\(or {<TYPE>}*\)
*	\(values {<TYPE>}* [&optional {<TYPE>}*] [&rest <TYPE>] [&allow-other-keys]\)
*	\(satisfies <PREDICATE-SYMBOL>\)	;; symbol is a predicate
*	\(member {<OBJECT>}*\)
*	\(eql <OBJECT>\)
C	random-state
C	readtable
C	hash-table
C	structure-object
C	package
C	method
C	method-combination
C	standard-object
C		class
C			built-in-class
C			structure-class
C			standard-class
C		standard-method \(method, standard-object\)
C	symbol
*		keyword
C	function
*		compiled-function
C		generic-function
C			standard-generic-function
C	number
C*		complex	[<TYPE>]
C*		real    [<MIN-BND> [<MAX-BND>]]
C*			float [<MIN-BND> [<MAX-BND>]]
*				short-float  [<MIN-BND> [<MAX-BND>]]
*				single-float [<MIN-BND> [<MAX-BND>]]
*				double-float [<MIN-BND> [<MAX-BND>]]
*				long-float   [<MIN-BND> [<MAX-BND>]]
C*			rational [<MIN-BND> [<MAX-BND>]]
C				ratio
C*				integer [<MIN-BND> [<MAX-BND>]]
*					fixnum
*					bignum
*				    signed-byte [<LENGTH> | *]
*					unsigned-byte [<LENGTH> | *]
*					    bit ;; ≣ \(unsigned-byte 1\)
*				    mod <MAX-BND-EXCLUSIVE>
C	character
*		base-char
*			standard-char
*		extended-char
*	atom ;; ≣ \(not cons\)
C	sequence
C		list
C			null \(symbol list\)
C*			cons [{<TYPE> | *} [{<TYPE> | *}]]
C*	array [{<TYPE-ELT> | *} [<RANK> | * | \({<DIMENSION> | * }*\)]]
*		simple-array [{<TYPE-ELT> | *} [<RANK> | * | \({<DIMENSION> | * }*\)]]
C*		vector [{<TYPE-ELT> | *} [{<SIZE> | *}]]
*			simple-vector \(simple-array vector\)
C*			bit-vector [{<SIZE> | *}]
*				simple-bit-vector [{<SIZE> | *}]
C*			string [{<SIZE> | *}]
*				simple-string [{<SIZE> | *}]
*				base-string [{<SIZE> | *}]
*					simple-base-string [{<SIZE> | *}]
C	pathname
C		logical-pathname
C	stream
C		broadcast-stream
C		concatenated-stream
C		echo-stream
C		file-stream
C		string-stream
C		synonym-stream
C		two-way-stream\\n
;; :CL-CONDITIONS ;; NOTE Conditions are not part of CLOS' `standard-object`
	restart
	condition
		warning
			style-warning
		serious-condition
			storage-condition
			error
				type-error
				parse-error
				control-error
				program-error
				cell-error
					undefined-function
					unbound-slot
					unbound-variable
				package-error
				arithmetic-error
					division-by-zero
					floating-point-invalid-operation
					floating-point-inexact
					floating-point-overflow
					floating-point-underflow
				file-error
				stream-error
					reader-error \(parse-error stream-error\)
				print-not-readable
		simple-condition
			simple-warning \(warning simple-condition\)
			simple-error \(error simple-condition\)
			simple-type-error \(type-error simple-condition\)
:SOURCE :FILE ecl/src/doc/types-and-classes\n
:SEE info node `(ansicl)Type Specifiers'\n
:SEE-ALSO `mon-help-CL-type-declarations', `mon-help-CL-symbols',
`mon-help-CL-sequences', `mon-help-CL-iteration', `mon-help-CL-conses',
`mon-help-CL-hash-tables', `mon-help-CL-print', `mon-help-CL-streams',
`mon-help-CL-reader', `mon-help-CL-chars', `mon-help-CL-strings',
`mon-help-CL-structures', `mon-help-CL-arrays', `mon-help-CL-numbers',
`mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-types :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-types' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-types)
;;; :TEST-ME (mon-help-CL-types t)
;;; :TEST-ME (describe-function 'mon-help-CL-types )

 
;;; ==============================
;;; :CHANGESET 2364
;;; :CREATED <Timestamp: #{2010-12-18T15:11:01-05:00Z}#{10506} - by MON KEY>
;;;###autoload
(defun mon-help-CL-type-declarations (&optional insertp intrp)
  "
           <TYPESPEC>                     <VAR>
 (declare ((array      (signed-byte 5) 1) an-array))
 (declare (simple-array                   array)

 (declare (fixnum                         some-num)
 (declare (special                         *spcl*))
 (declare ((or null simple-string)       <MAYBE-STRING>))

:SEE \(:CLTL2 \(pp . 215-237\)\)\n
:SEE info node `(ansicl) '\n
:SEE-ALSO `mon-help-cl-types', `mon-help-CL-symbols',
`mon-help-CL-sequences', `mon-help-CL-iteration', `mon-help-CL-conses',
`mon-help-CL-hash-tables', `mon-help-CL-print', `mon-help-CL-streams',
`mon-help-CL-reader', `mon-help-CL-chars', `mon-help-CL-strings',
`mon-help-CL-structures', `mon-help-CL-arrays', `mon-help-CL-numbers',
`mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-type-declarations :insertp t)
    (mon-message :msg-spec  
                 '(":FUNCTION `mon-help-CL-type-declarations' " 
                   "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-type-declarations)
;;; :TEST-ME (mon-help-CL-type-declarations t)
;;; :TEST-ME (describe-function 'mon-help-CL-type-declarations)

 
;;; ==============================
;;;###autoload
(defun mon-help-CL-sharpsign-syntax (&optional insertp intrp)
  "List of Common Lisp's sharpsign reader syntax.\n
;; CL-SHARPSIGN
`#\\'  ;<SHARPSIGN-BACKSLASH>         object character 
`#''   ;<SHARPSIGN-SINGLE-QUOTE>     `function' sugar
`#('   ;<SHARPSIGN-LEFT-PARENTHESIS>  vector  Simple 
`#*'   ;<SHARPSIGN-ASTERISK>          vector Bit 
`#A'   ;<SHARPSIGN-A>                 array
`#S'   ;<SHARPSIGN-S>                 structure
`#P'   ;<SHARPSIGN-P>                 pathname
`#B'   ;<SHARPSIGN-B>                 rational Binary
`#O'   ;<SHARPSIGN-O>                 rational Octal
`#X'   ;<SHARPSIGN-X>                 rational Hexadecimal
`#Rn'  ;<SHARPSIGN-R>                 rational Radix-<N>
`#C'   ;<SHARPSIGN-C>                 number Complex
`#='   ;<SHARPSIGN-EQUAL-SIGN>        object Label
`#n#'  ;<SHARPSIGN-SHARPSIGN>         object Label reference 
`#:'   ;<SHARPSIGN-COLON>             symbol Uninterned 
`#.'   ;<SHARPSIGN-DOT>               read-time Evaluation
`#|'   ;<SHARPSIGN-VERTICAL-BAR>      balanced Comment
`#+'   ;<SHARPSIGN-PLUS>              read-time Conditional
`#-'   ;<SHARPSIGN-MINUS>             read-time Conditional\n
:SEE info node `(ansicl)Sharpsign'
:SEE info node `(ansicl)Features'
:SEE info node `(ansicl)Use of Read-Time Conditionals'\n
:SEE-ALSO `mon-help-CL-symbols', `mon-help-CL-sequences', `mon-help-CL-iteration',
`mon-help-CL-conses', `mon-help-CL-hash-tables', `mon-help-CL-print',
`mon-help-CL-streams', `mon-help-CL-reader', `mon-help-CL-chars',
`mon-help-CL-strings', `mon-help-CL-structures', `mon-help-CL-arrays',
`mon-help-CL-numbers', `mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-sharpsign-syntax :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-sharpsign-syntax' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-sharpsign-syntax)
;;; :TEST-ME (mon-help-CL-sharpsign-syntax t)
;;; :TEST-ME (apply 'mon-help-CL-sharpsign-syntax '(t))

 
;;; ==============================
;;; :CHANGESET 2364
;;; :CREATED <Timestamp: #{2010-12-14T13:14:44-05:00Z}#{10502} - by MON KEY>
;;;###autoload
(defun mon-help-CL-format (&optional insertp intrp)
  "Help for Common Lisp format and format specs.\n
List contains info links into ansicl info node (which `Info-index' has trouble
finding) and the relevant html file names for the relevant hspec nodes.\n
;; :CL-FUNCTIONS-FORMAT
`format'
`formatter'\n
;; :CL-FUNCTIONS-FORMAT-INFO-BASIC-OUTPUT
:SEE info node `(ansicl)FORMAT Basic Output'

~C Character
   ~C    -- as if by `write-char'
   ~:C   -- char(s) name \"spelled out\"
   ~@C   -- print char readably
   ~:@C  -- char(s) name \"spelled out\" w/ key modifiers \(implementation specific\)

~% Newline
   ~%
   ~N%

~& Fresh-line
   ~&
   ~N&
   ~0&

~| Page
   ~|
   ~N|

~~ Tilde Literal
   ~~ 
   ~N~

;; :CL-FUNCTIONS-FORMAT-INFO-PRINTER-OPERATIONS
:SEE info node `(ansicl)FORMAT Printer Operations'

~A Aesthetic
   ~mincol,colinc,minpad,padcharA

~S Standard
   ~mincol,colinc,minpad,padcharS

~W Write

;; :CL-FUNCTIONS-FORMAT-INFO-RADIX-CONTROL
:SEE info node `(ansicl)FORMAT Radix Control'

~D Decimal
   ~mincol,padchar,commachar,comma-interval@signedD
   ~N,'<PAD-CHAR>,'<PAD-CHAR>N:@D
   ~@D  -- always print sign
   ~:D  -- print separater between digit groups
   ~N:D -- print separater every N  digit groups

~B Binary
   ~mincol,padchar,commachar,comma-interval@signedB
   ~N,'<PAD-CHAR>,'<PAD-CHAR>N:@B

~O Octal
   ~mincol,padchar,commachar,comma-interval@signedO
   ~N,'<PAD-CHAR>,'<PAD-CHAR>N:@O

~X Hexadecimal
   ~mincol,padchar,commachar,comma-interval@signedX
   ~N,'<PAD-CHAR>,'<PAD-CHAR>N:@X

~R Radix
   ~radix,mincol,padchar,commachar,comma-intervalR
   ~R   -- cardinal
   ~:R  -- ordinal
   ~@R  -- Roman
   ~@R  -- Old Roman

;; :CL-FUNCTIONS-FORMAT-INFO-FLOATING-POINT
:SEE info node `(ansicl)FORMAT Floating-Point Printers'

~F Fixed-Format Floating-Point
   ~w,d,k,overflowchar,padcharF

~E Exponential Floating-Point
   ~w,d,e,k,overflowchar,padchar,exponentcharE

~G General Floating-Point
   ~w,d,e,k,overflowchar,padchar,exponentcharG

~$ Monetary Floating-Point
   ~d,n,w,padchar$

;; :CL-FUNCTIONS-FORMAT-INFO-PRETTY-PRINTER-OPERATIONS
:SEE info node `(ansicl)FORMAT Pretty Printer Operations'

~_ Conditional Newline
   ~_   ⬄ \(pprint-newline :linear\)
   ~@_  ⬄ \(pprint-newline :miser\)
   ~:_  ⬄ \(pprint-newline :fill\)
   ~:@_ ⬄ \(pprint-newline :mandatory\)

~< Logical Block
   ~<PREFIX~;BODY~;SUFFIX~:>   
   ~<...:~>
   ~<...~:@>
   ~@<...~:>
   ~<...:~>  ⬄ `pprint-logical-bloc' \(except when terminated with \"~:@>\"\)

~I Tilde-I - Indent
   ~NI  ⬄ (pprint-indent :block N)
   ~N:I ⬄ (pprint-indent :current N)

~/NAME/ Call Function
        ~/NAME/ ~/PACKAGE:NAME/ ~/PACKAGE::NAME/ 
        :SEE `pprint-linear' `pprint-fill', `pprint-tabular'

;; :CL-FUNCTIONS-FORMAT-INFO-LAYOUT-CONTROL
:SEE info node `(ansicl)FORMAT Layout Control'

~T Tabulate
   ~colnum,colincT
   ~colrel,colinc@T
   ~colrel,colinc:T
   ~colrel,colinc:@T
   ~N,M:T  ⬄ \(pprint-tab :section N M\)
   ~N,M:@T ⬄ \(pprint-tab :section-relative N M\)

~< Justification
~> End of Justification
   ~mincol,colinc,minpad,padchar<str~>

;; :CL-FUNCTIONS-FORMAT-INFO-CONTROL-FLOW-OPERATIONS
:SEE info node `(ansicl)FORMAT Control-Flow Operations'

~* Go-To
   ~N* ~:* ~N:* ~N@*

~[ Conditional Expression
~] End Conditional Expression
   ~[str0~;str1~;...~;strN~]

~{ Iteration
~} End Iteration
   ~{str~}  ~:{str~}  ~@{str~} ~:@{str~}

~? Recursive Processing
   ~? ~@?

;; :CL-FUNCTIONS-FORMAT-INFO-MISCELLANEOUS-OPERATIONS
:SEE info node `(ansicl)FORMAT Miscellaneous Operations'

~\( Case Conversion
~\) End Case Conversion
   ~\(str~\)  ~\( ~:\( ~@\( ~:@\(

~P Tilde-P Plural
   ~P ~:P ~@P ~:@P

;; :CL-FUNCTIONS-FORMAT-INFO-MISCELLANEOUS-PSEUDOU-OPERATIONS
:SEE info node `(ansicl)FORMAT Miscellaneous Pseudo-Operations'

~; Clause Separator

~^ Escape Upward
   ~^ ~:^

~`NL` Ignored Newline
       ~`NL` ~:`NL` ~@`NL` ;; :NOTE Here `NL` implies ASCII the char 10,#o12,#xa

;; :CL-FUNCTIONS-FORMAT-INFO
:SEE info node `(ansicl)Formatted Output'
:SEE info node `(ansicl)Additional Information about FORMAT Operations'
:SEE info node `(ansicl)Examples of FORMAT'
:SEE info node `(ansicl)Notes about FORMAT'

:SEE-ALSO `mon-help-CL-format-usage', `mon-help-CL-streams',
`mon-help-CL-print', `mon-help-cl-reader', `mon-help-CL-symbols',
`mon-help-CL-sequences', `mon-help-CL-iteration', `mon-help-CL-conses',
`mon-help-CL-hash-tables', `mon-help-CL-chars', `mon-help-CL-strings',
`mon-help-CL-structures', `mon-help-CL-arrays', `mon-help-CL-numbers',
`mon-help-CL-object-CLOS', `mon-help-CL-control-flow',
`mon-help-CL-eval-compile', `mon-help-CL-load-compile',
`mon-help-CL-environment', `mon-help-CL-package-functions',
`mon-help-CL-intern-symbol', `mon-help-CL-sharpsign-syntax', `mon-help-CL-loop',
`mon-help-CL-time', `mon-help-CL-error-condition-restart',
`mon-help-CL-emacs-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-bit-byte-bool-logic', `mon-help-CL-stream-keywords',
`mon-help-CL-slime-keys', `mon-help-CL-swank-functions',
`mon-help-CL-local-time', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-format :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-format' " 
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-format)
;;; :TEST-ME (mon-help-CL-format t)
;;; :TEST-ME (describe-function 'mon-help-CL-format)

;;; ==============================
(defun mon-help-CL-format-usage (&optional insertp intrp)
  "Example usage idioms for Common Lisp's format control strings.\n

;; :CL-FORMAT-ITERATION
`~{` Iteration 

 \(format nil \"~{~A~}\" '\(1 2 3 4 5\)\) 
  ;=> \"12345\"

Is roughly equivalent to:

 \(let \(\(foo '\(1 2 3 4 5\)\)\)
   \(with-output-to-string \(out\) 
     \(dolist \(x foo out\)  \(format out \"~A\" x\)\)\)\)
  ;=> \"12345\"

:SEE info node `(ansicl)Examples of FORMAT'\n
:SEE-ALSO `mon-help-CL-format', `mon-help-CL-streams', `mon-help-CL-print',
`mon-help-CL-reader', `mon-help-CL-strings', `mon-help-CL-chars'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-cl-format-usage :insertp t)
    (message (concat ":FUNCTION `mon-help-cl-format-usage' " 
                     "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-cl-format-usage)
;;; :TEST-ME (mon-help-cl-format-usage t)
;;; :TEST-ME (describe-function 'mon-help-cl-format-usage)

 
;;; ==============================
;;; :FILE slime.el
;;; `when-let'
;;; `destructure-case'
;;; `with-struct'
;;; `slime-curry'
;;; `slime-rcurry'
;;; ==============================

;;; ==============================
;; (while (search-forward-regexp 
;;        ;;....1..2.........3.........
;;        "^\\(\\(.*\t\\)\\(.*\\)\\)$")
;;  (replace-match "\\2`\\3'"))
;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 08, 2009 @ 06:11.12 PM - by MON KEY>
;;;###autoload
(defun mon-help-CL-slime-keys (&optional insertp intrp)
  "Help for `slime-mode' keybindings prefix keymaps and the related vars.\n
;; :SLIME-KEYMAPS            :SLIME-KEY-BINDINGS 
`slime-mode-map'            `slime-keys'
`slime-mode-indirect-map'
`slime-parent-map'          `slime-parent-bindings'
`slime-prefix-map'          `slime-prefix-bindings'
`slime-editing-map'         `slime-editing-keys'
`slime-doc-map'             `slime-doc-bindings'
`slime-who-map'             `slime-who-bindings'\n
;; :SLIME-KEYMAP-FUNCTIONS
`slime-init-keymaps'
`slime-init-keymap'
`slime-bind-keys'
`slime-define-keys'\n
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
:ALIASED-BY `mon-help-slime-keys'\n
:SEE :FILE mon-keybindings.el slime.el\n
:SEE-ALSO `slime-cheat-sheet', `mon-help-key-functions', `mon-help-keys',
`mon-help-CL-time', `mon-help-CL-local-time', `mon-help-CL-loop',
`mon-help-CL-do', `mon-help-CL-file-dir-functions', `mon-help-CL-pkgs',
`mon-help-CL-sequence-predicates', `mon-help-CL-symbols', `mon-help-CL-lispdoc',
`mon-help-CL-swank-functions', `mon-purge-slime-swank-port-file'.\n►►►"
(interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-slime-keys :insertp t)
    (mon-message :msg-spec  '(":FUNCTION `mon-help-CL-slime-keys' "
                              "-- pass non-nil for optional arg INTRP"))))
;;
;;;
;;; :TEST-ME (mon-help-CL-slime-keys)
;;; :TEST-ME (mon-help-CL-slime-keys t)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-06T05:29:03-04:00Z}#{09367} - by MON KEY>
;;;###autoload
(defun mon-help-CL-swank-functions (&optional insertp intrp)
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
:ALIASED-BY `mon-help-swank-functions'\n
:SEE-ALSO `mon-help-CL-slime-keys', `mon-keybind-slime', `mon-slime-setup-init',
`mon-help-CL-file-dir-functions', `mon-help-CL-sequence-predicates',
`mon-help-CL-symbols', `mon-help-CL-lispdoc'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-swank-functions :insertp t)
    (mon-message :msg-spec  '(":FUNCTION `mon-help-CL-swank-functions' "
                              "-- pass non-nil for optional arg INTRP"))))
;;
;;
;;; :TEST-ME (mon-help-CL-swank-functions)
;;; :TEST-ME (mon-help-CL-swank-functions t)

 
;;; ==============================
;;; :CHANGESET 1928 <Timestamp: #{2010-06-29T13:20:07-04:00Z}#{10262} - by MON KEY>
;;;###autoload
(defun mon-help-CL-local-time (&optional insertp intrp)
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
:SEE-ALSO `mon-help-time-functions', `mon-help-mon-time-functions',
`mon-help-iso-8601', `mon-help-CL-time', `mon-help-CL-loop', `mon-help-CL-do',
`mon-help-CL-file-dir-functions', `mon-help-CL-pkgs',
`mon-help-CL-sequence-predicates', `mon-help-CL-symbols', `mon-help-CL-lispdoc',
`mon-help-CL-swank-functions', `mon-help-CL-slime-keys'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-local-time :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-local-time' "
                             "-- pass non-nil for optional arg INTRP"))))

 
;;; ==============================
;;; :CHANGESET 1942
;;; :CREATED <Timestamp: #{2010-07-07T13:13:58-04:00Z}#{10273} - by MON KEY>
;;;###autoload
(defun mon-help-CL-minion (&optional insertp intrp)
  "Notes and usage idioms for #lisp's electronically composed helper `minion'.\n
;; :CL-MINION-TERM-LOOKUP
To have minon look up a term in the internal database and/or try to retrieve the
first sentence of a similiarly name pageon CLiki do:
  minion: term?\n
;; :CL-MINION-HELP-OTHERS
To have minion tell another user about something do:
   minion: show <NICK> <SOMETHING>
   minion: tell <NICK> about <TOPIC/TERM/SYMBOL/PKG>
   minion: please tell <USER> about <TOPIC/TERM/SYMBOL/PKG>\n
:NOTE Minion will respond to many differently pharsed queries and can (within
reason) show pretty much anything to another user.\n
;; :CL-MINION-TERM-ADD
To have minon remember a term do: 
  minion: add \"<TERM>\" as: <TERM-DEFINITION>\n
;; :CL-MINION-TERM-ALIAS
To have minion remember that a term is an alias for another term do:
  minion: alias \"<TERM>\" as: <SOME-OTHER-TERM>\n
;; :CL-MINION-FORGET
To have minion forget a term or nickname do:
  minion: forget <TERM>|<NICK>\n
;; :CL-MINION-MEMO
To record a memo for minon to give to some nick when next they speak do:
  minion: memo for <NICK>: <MEMO-CONTENTS>\n
;; :CL-MINION-NICKNAMES
If you have multiple nicknames and want to get your memos at any of them do:
  minion: <NICK1> is another nick for <NICK2>\n
If you decide to give up a nick to have minion forget it do:
  minion: forget <NICK2>\n
;; :CL-MINION-MEMO-AVOIDANCE
To flush all your memos without delivery do:
  minion: discard my memos\n
To flush only memos from a specific person do: 
  minion: discard my memos from <NICK>\n
;; :CL-MINION-ADVICE
To get advice from minion: 
  minion: advice #<INTEGER>\n
;; :CL-MINION-APROPOS
To search for all small definitions containing 'foo':
  minion: apropos <FOO>\n
;; :CL-MINION-ACRONYMS
See an acronym you don't recognize? To find out what it means do:
  minion: what does <ACRONYM> stand for?\n
;; :CL-MINION-HELP
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
;; :CL-MINION-SOURCES
For minion sources, look at Cliki-bot sources written by Brian Mastenbrook in
the example directory of cl-irc:
:SEE (URL `http://www.cliki.net/cl-irc')
:SEE (URL `http://common-lisp.net/project/cl-irc/')
:SEE (URL `http://common-lisp.net/project/cl-irc/cl-irc_latest.tar.gz')
:SEE (URL `http://paste.lisp.org/')
:SEE (URL `http://common-lisp.net/project/lisppaste/')\n
:SEE-ALSO `mon-wget-freenode-lisp-logs', `mon-help-CL-local-time',
`mon-help-CL-loop', `mon-help-CL-time', `mon-help-CL-file-dir-functions',
`mon-help-CL-minion', `mon-help-CL-symbols', `mon-help-CL-slime-keys',
`mon-help-CL-swank-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-CL-minion :insertp t)
    (mon-message :msg-spec '(":FUNCTION `mon-help-CL-local-time' "
                             "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-CL-minion)
;;; :TEST-ME (mon-help-CL-minion t)
;;; :TEST-ME (describe-function 'mon-help-CL-minion)
;;; :TEST-ME (apply #'mon-help-CL-minion nil '(t))

 
;;; ==============================
;;; `common-lisp-hyperspec-root'
;;; `common-lisp-hyperspec-symbol-table'
;;; `intern-clhs-symbol' 
;;; `common-lisp-hyperspec-strip-cl-package'
;;; `common-lisp-hyperspec-symbols'
;;; :CREATED <Timestamp: #{2010-01-29T00:42:00-05:00Z}#{10045} - by MON>
(defvar *clhs-symbol-v3-or-v7* nil)
;;
(when (and (intern-soft "IS-MON-P") (bound-and-true-p IS-MON-P))
  (unless (bound-and-true-p *clhs-symbol-v3-or-v7*)
    (setq *clhs-symbol-v3-or-v7*
          (funcall ;; #'(lambda () 
           (cond ((string-match-p "Hyperspec-v3" common-lisp-hyperspec-root) #'car)
                 ((string-match-p "Hyperspec-v7" common-lisp-hyperspec-root) #'cadr)
                 ;; Lispworks, MIT, NON-MON, etc.
                 ((or (string-match-p "HyperSpec" common-lisp-hyperspec-root) t) #'cadr))
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
                       (";" "02_dd.htm"))) ) )) ))
;; ;)

;; mon-bind-mon-help-CL-pkgs-loadtime

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-20T16:19:35-04:00Z}#{10116} - by MON KEY>
(defun mon-help-utils-CL-loadtime (&optional w-msg-user)
  "Loadtime function to unbind CL symbols of :FILE mon-doc-help-CL.el.\n
Unbind `*clhs-symbol-v3-or-v7*' as variable `*mon-help-CL-symbols*' now holds a
hashtable of all CL symbol/Hspec mappings post loadtime.\n
When optional arge W-MSG-USER is non-nil message user that function was
evaluated at loadtime.\n
SEE-ALSO `mon-help-utils-CL-loadtime', `mon-after-mon-utils-loadtime',
`mon-check-feature-for-loadtime', `mon-bind-nefs-photos-at-loadtime',
`mon-bind-cifs-vars-at-loadtime',
`mon-bind-doc-help-proprietery-vars-at-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-set-register-tags-loadtime',
`mon-CL-cln-colon-swap'.\n►►►"
  (when (or (intern-soft "*clhs-symbol-v3-or-v7*")
            (boundp '*clhs-symbol-v3-or-v7*))
    (setq w-msg-user t)
    (makunbound '*clhs-symbol-v3-or-v7*)
    (unintern "*clhs-symbol-v3-or-v7*" obarray)
    (when w-msg-user 
      (message 
       (concat ":FUNCTION `mon-help-utils-CL-loadtime' " 
               "-- uninterned  variable `*clhs-symbol-v3-or-v7*' at loadtime")))))
;;
;;; :TEST-ME (mon-help-utils-CL-loadtime t)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-29T00:47:51-05:00Z}#{10045} - by MON>
(defvar *mon-help-CL-symbols* nil
  "hash-table of Common-Lisp symbol names mapped to their respective Hspec .html files.\n
Bound at compile/loadtime with var `*clhs-symbol-v3-or-v7*' according to the
value of `common-lisp-hyperspec-root'.\n
:NOTE `*clhs-symbol-v3-or-v7*' unloaded with `mon-help-utils-CL-loadtime'.\n
:CALLED-BY `mon-help-CL-symbols'\n
:CALLED-BY `mon-help-CL-lispdoc'\n
:SEE-ALSO `common-lisp-hyperspec-format-characters',
`common-lisp-hyperspec-reader-macros', `common-lisp-hyperspec-root',
`mon-set-common-lisp-hspec-init'.\n►►►")
;;;
(unless (bound-and-true-p *mon-help-CL-symbols*)
  (setq *mon-help-CL-symbols* (make-hash-table :test #'equal :size 1024))
  (mapc #'(lambda (k) 
            (puthash (car k) (cadr k) *mon-help-CL-symbols*)) 
        *clhs-symbol-v3-or-v7*))

;;; ==============================
;;; :TODO Add optional arg that allows lookup via info-look instead of hyperspec
;;
;; (info-lookup-add-help
;;  :mode 'lisp-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))
;; 
;;  (<HELP-TOPIC> . <HELP-DATA>)
;;   <HELP-TOPIC> =:  <SYMBOL>
;;   <HELP-DATA>  =: <ALIST>
;;                 | (<MODE> <REGEXP> <IGNORE-CASE> <DOC-SPEC> 
;;                 |  <PARSE-RULE> <OTHER-MODES>)
;;                 <DOC-SPEC>   =: LIST
;;                               | (<INFO-NODE> <TRANS-FUNC> <PREFIX> <SUFFIX>)
;;                                 <INFO-NODE>   =: <FILENAME>
;;                                 <TRANS-FUNC>  =: { nil | <STRING> }
;;                                 <PREFIX>      =: <STRING>
;;                                 <SUFFIX>      =: <STRING>
;;                 <PARSE-RULE>  =: { <FUNCTION> | <REGEXP> }
;;                 <OTHER-MODES> =: <LIST>
;;
;; `info-lookup-add-help' 
;;; KEYWORD is either `:topic', `:mode', `:regexp', `:ignore-case',
;;  `:doc-spec', `:parse-rule', or `:other-modes'.

 
;;; ==============================
;;; :NOTE Consider optimizing this w/ `lazy-completion-table' macro
;;; :CREATED <Timestamp: #{2010-01-29T00:45:31-05:00Z}#{10045} - by MON>
(defun mon-help-CL-symbols (&optional cl-symbol-string ffox intrp)
  "Completion function for the Common Lisp symbols in the hspec.\n
CL-SYMBOL-STRING is a string to associate with a value in `*mon-help-CL-symbols*'.\n
When `IS-MON-P-GNU' or a w3m executable is in path and `w3m-browse-url' is fboundp
browse th hspec in Emacsw3m.\n
When FFOX is non-nil or called-interactively with prefix arg ensure browsing with Firefox.\n
:EXAMPLE\n\n\(mon-help-CL-symbols \"defclass\"\)\n\(mon-help-CL-symbols \"#<\"\)\n
\(mon-help-CL-symbols nil nil\)\n\(mon-help-CL-symbols \"defclass\" t\)\n
\(mon-help-CL-symbols nil nil t\)\n\(apply 'mon-help-CL-symbols nil nil '\(t\)\)\n
:NOTE When using Hspec v3 and Emacs-w3m you may want to comment out the
java-applet e.g.\n
 <APPLET HEIGHT=80 WIDTH=450 CODE=\"CLIndex.class\" CODEBASE=\"../Data/\"></APPLET>\n
:SEE :FILE <`common-lisp-hyperspec-root'>FrontMatter/Symbol-Index.html\n
:SEE-ALSO `*mon-help-CL-symbols*', `mon-help-CL-lispdoc',
`mon-CL-package-complete', `common-lisp-hyperspec-issuex-table',
`common-lisp-hyperspec-symbol-table' `quicklisp-system-complete',
`mon-help-CL-pkgs', `mon-help-CL-file-dir-functions', `mon-help-CL-time',
`mon-help-CL-loop', `mon-help-CL-do', `mon-help-CL-local-time',
`mon-help-CL-swank-functions', `mon-help-CL-slime-keys',
`common-lisp-hyperspec-root', `mon-help-utils-CL-loadtime',
`mon-purge-cl-symbol-buffers-on-load', `mon-set-common-lisp-hspec-init'.\n►►►"
  (interactive "\i\nP\np")
  ;;; :WAS (let ((rd-cl-sym (cond ((and cl-symbol-string (stringp cl-symbol-string))
  ;;;                         (member cl-symbol-string *mon-help-CL-symbols*)
  ;;;                         (cadr (assoc-string cl-symbol-string *clhs-symbol-v3-or-v7*)))
  ;;;                        ((or intrp t)
  ;;;                         (cadr (assoc (completing-read "cl-cymbol :" *mon-help-CL-symbols*) 
  ;;;                                      *clhs-symbol-v3-or-v7*))))))
  (let ((rd-cl-sym (cond ((and cl-symbol-string (stringp cl-symbol-string))
                          (gethash cl-symbol-string *mon-help-CL-symbols*))
                         ((or intrp t) 
                          (gethash 
                           (completing-read "CL Symbol (tab completes): " *mon-help-CL-symbols*)
                           *mon-help-CL-symbols*)))))
    (setq rd-cl-sym (concat 
                     (unless (or (string-match-p "file://" common-lisp-hyperspec-root) 
                                 (string-match-p "http://" common-lisp-hyperspec-root))
                       "file://")
                     common-lisp-hyperspec-root "Body/" rd-cl-sym))
    (cond (ffox (browse-url-firefox rd-cl-sym))
          ((or (and (intern-soft "IS-MON-P-GNU")
                    (bound-and-true-p IS-MON-P-GNU))
               (and (executable-find "w3m") 
                    (intern-soft "w3m-browse-url")
                    (fboundp 'w3m-browse-url)))
           (w3m-browse-url rd-cl-sym))
          (t (browse-url-generic rd-cl-sym)))))
;; 
;;
;;; :TEST-ME (mon-help-CL-symbols "defclass")
;;; :TEST-ME (mon-help-CL-symbols "#<")
;;; :TEST-ME (mon-help-CL-symbols nil nil)
;;; :TEST-ME (mon-help-CL-symbols "defclass" t)
;;; :TEST-ME (mon-help-CL-symbols nil nil t)

 
;;; ==============================
;;; :COURTESY Jose Antonio Ortega Ruiz jao@gnu.org  :HIS jao-lisp.el :WAS `lispdoc'
;;; :NOTE Adapted to look for CL symbols from within w3m buffers as well.  The
;;;      original `word-at-point' routine wasn't reliable/robust in non
;;;      lisp-mode buffers, e.g. *w3m*.
;;; (define-key lisp-mode-map (kbd "C-c l") 'mon-help-CL-lispdoc)
;;; :CREATED <Timestamp: #{2010-08-15T17:50:10-04:00Z}#{10327} - by MON>
(defun mon-help-CL-lispdoc ()
  "Search lispdoc.com for a CL symbol.\n
Default is to search the `symbol-at-point' or regexp matching the regexp:\n
 \"[A-Za-z0-9*%+-]+\"\n
When there is no CL `symbol-at-point' prompts for a CL symbol to search for.\n
If the variable `*mon-help-CL-symbols*' is bound and satisfies the predicate
`hash-table-p' try completing-read for a CL symbols in hash-table keys.\n
Prompt for the type of search type for lispdoc.com as either:
  basic [b]  full-text [f]\n
:SEE (URL `http://lispdoc.com')\n
:SEE-ALSO `mon-help-CL-symbols', `mon-CL-package-complete',
`common-lisp-hyperspec-root', `common-lisp-hyperspec-issuex-table',
`common-lisp-hyperspec-symbol-table' `quicklisp-system-complete',
`mon-help-CL-pkgs', `mon-help-CL-file-dir-functions', `mon-help-CL-time',
`mon-help-CL-loop', `mon-help-CL-do', `mon-help-CL-local-time',
`mon-help-CL-swank-functions', `mon-help-CL-slime-keys'.\n►►►"
  (interactive)
  (let* ((symbol-chk "[A-Za-z0-9*%+-]+") ;; ! not testing scheme         
         (msg-pfx ":FUNCTION `mon-CL-lispdoc' ")
         (wap (save-excursion 
                (while (car (memq (char-after) '(10 32 9))) 
                  (forward-char))            
                (when (looking-at-p symbol-chk)
                  (looking-at symbol-chk)
                  (let ((wap-chk (match-string-no-properties 0)))
                    (if (and (symbol-at-point)
                             wap-chk
                             (>= (length (format "%s" (symbol-at-point))) 
                                 (length wap-chk)))
                        (symbol-at-point)
                      wap-chk)))))
         (symbol-at-point (symbol-at-point))
         (default (or wap (symbol-name symbol-at-point)))
         (inp  (if (or wap symbol-at-point)
                   (read-from-minibuffer
                    (concat msg-pfx "-- symbol (default " (format "%s" default) "): ")
                    (format "%s" default))
                 (if (and (bound-and-true-p *mon-help-CL-symbols*)
                          (hash-table-p *mon-help-CL-symbols*))
                     (completing-read (concat msg-pfx  "-- symbol (no default): ") 
                                      *mon-help-CL-symbols*)
                   (read-from-minibuffer (concat msg-pfx "-- symbol (no default): "))))))
    (if (and (string= inp "") (not wap) (not symbol-at-point))
        (message  (concat msg-pfx "-- must enter a symbol"))
      (let ((search-type 
             (read-from-minibuffer ; :NOTE defaulting to b sucks
              (concat msg-pfx
                      "-- search type for lispdoc.com - basic [b]  full-text [f]: "))))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search;="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

;;; ==============================
;;; :CHANGESET 2406
;;; :CREATED <Timestamp: #{2011-01-20T18:35:46-05:00Z}#{11034} - by MON KEY>
(defun mon-cln-ansi-info (start end)
  "Replace formatting chars from ansicl info node from START to END.\n
Match car of elts in `*regexp-ansicl-info*' replace with cdr.\n
:EXAMPLE\n;; In following example the region delimited by `►` and `◄` is cleaned.\n
\(mon-with-inhibit-buffer-read-only
  \(mon-cln-ansi-info \(mon-g2be -1 t\) \(mon-g2be 1 t\)\)\)\n
►\n
The \"extended\" ‘loop’ form:

 -- Macro: loop [↓name-clause] {↓variable-clause}* {↓main-clause}* →
          {result}*\n
→ SQRT-ADVISOR
 \(sqrt-advisor\)\n
▷ Number: 5↩
▷ The square root of 5 is 2.236068.
▷ Number: 4↩
▷ The square root of 4 is 2.
▷ Number: done↩
→ NIL
\n◄\n
:NOTE The above formatting is from the loop section Jesper Harder's dpans2texi.\n
:SEE info node `(ansicl)loop'\n
:SEE-ALSO `mon-cln-freenode-log' `mon-cln-BIG-whitespace', `mon-cln-ansi-info',
`mon-cln-benezit', `mon-cln-benezit-fields', `mon-cln-bib',
`mon-cln-blank-lines', `mon-cln-common-abbrevs', `mon-cln-control-M',
`mon-cln-csv-fields', `mon-cln-duplicate-lines', `mon-cln-ebay-time-string',
`mon-cln-eight-bit-raw', `mon-cln-file-name-string', `mon-cln-freenode-log',
`mon-cln-html-chars', `mon-cln-html-tags', `mon-cln-imdb',
`mon-cln-img-magic-hex', `mon-cln-iptables-long-form',
`mon-cln-iptables-short-form', `mon-cln-iso-latin-1', `mon-cln-loc',
`mon-cln-mail-headers', `mon-cln-philsp', `mon-cln-piped-list', `mon-cln-pipes',
`mon-cln-pipes-get-field-col', `mon-cln-pipes-map-field-pairs',
`mon-cln-smith-trailing-wps-dbc-item', `mon-cln-spc-tab-at-eol-in-region',
`mon-cln-spc-tab-eol', `mon-cln-tgm-xml-LF', `mon-cln-trail-whitespace',
`mon-cln-ulan', `mon-cln-uniq-lines', `mon-cln-up-colon', `mon-cln-whitespace',
`mon-cln-wiki', `mon-cln-xml-escapes', `mon-cln-xml<-parsed',
`mon-cln-xml<-parsed-strip-nil'.\n►►►"
  (interactive "r")
  (save-excursion 
    (save-restriction
      (narrow-to-region start end)
      (mon-g2be -1)
      (dolist (mcai-D-0 *regexp-ansicl-info*)
        (while (search-forward-regexp (car mcai-D-0) nil t)
          (replace-match (cdr mcai-D-0)))
        (mon-g2be -1)))))

 
;;; ==============================
;;; :CHANGESET 2180
;;; :CREATED <Timestamp: #{2010-10-16T15:45:01-04:00Z}#{10416} - by MON KEY>
(defcustom *mon-CL-indent-specs* 
  '((dosequence . 1)
    (dosublists . 1)
    (make-space-instance . 2))
  "Consed list of Common-Lisp function indentation rules.\n
:SEE-ALSO `mon-lisp-set-indent-hook', `mon-lisp-set-indent'.\n►►►"
  :type '(alist :key-type symbol :value-type integer)
  :group 'mon-doc-help-CL
  :group 'mon-base)

;;; ==============================
;;; Following is a list of Common Lisp symbols which coref Egnlish Natural language.
;;; Approx. 1 in 7 of the ~1000 CL symbols:
;;;
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
;;;
;;; ==============================

;;; ==============================
;; :COURTESY John Cowan (URL `http://recycledknowldege.blogspot.com') :DATE 2009-09-21
;;; Common Lisp symbols bound in more than one namespace. 
;;; These are the Common Lisp symbols which are bound in more than one
;;; namespace: for example, + is both a function (addition) and a variable 
;;; (the most recent form evaluated by the REPL).
;;;
;;; (abort and atom bit character complex cons continue eql error float function
;;;  lambda list logical-pathname member method-combination mod muffle-warning nil
;;;  not null or pathname rational setf store-value string type use-value values
;;;  vector t * + - /)

;;; ==============================
(provide 'mon-doc-help-CL)
;;; ==============================

(unless (and (intern-soft "IS-MON-SYSTEM-P")
             (bound-and-true-p IS-MON-SYSTEM-P))
  (eval-after-load "mon-doc-help-CL" (mon-help-utils-CL-loadtime t)))

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ================================================================
;;; mon-doc-help-CL.el ends here
;;; EOF

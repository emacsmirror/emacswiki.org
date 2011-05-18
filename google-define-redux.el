;;; google-define-redux.el --- extends google-define.el 
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright (c) 2007,2008,2009,2010 Jeremy English <jhe@jeremyenglish.org>
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: google-define-redux.el
;; AUTHOR: Jeremy English, MON KEY 
;; MAINTAINER: MON KEY
;; CREATED: 2010-02-03T13:41:11-05:00Z}#{10053
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: applications, comm, external, processes, hypermedia

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; google-define-redux provides extensions for google-define.el
;; Extends google-define with fontification of definition buffer.
;; No longer relies on `with-output-to-temp-buffer'
;; Adds parsing tokens for programatically extracting definitions.
;
;; FUNCTIONS:►►►
;; `google-define-get-command', `google-define-parse-buffer', `google-define',
;; `google-define-font-lock', `google-define-kill-def-buffers',
;; `google-define-word-at-point',
;; `google-define-make-query-url', `google-define-clean-string',
;; `google-define-button-action', `google-define-button-do-xref',
;; `google-define-insert-xref-button', `google-define-find-next-heading',
;; `google-define-make-heading-replacement', `google-define-find-headings',
;; `google-define-related-language-url', `google-define-find-itemized',
;; `google-define-find-next-url-in-heading',
;; `google-define-parse-related-languages',
;; `google-define-related-language-xref', `%google-define-clean-term-for-split',
;; `%google-define-find-next-related-language-xref',
;; `%google-define-find-itemized-regexp-for-search-term',
;; `%google-define-verify-query-url-lang-params',
;; `%google-define-verify-query-url-xref-type-params',
;; `%google-define-set-url-current-object'
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; FACES:
;; `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
;; `gg-def-inition', `gg-def-heading',
;;
;; :BUTTONS
;; `google-define-button', `google-related-lang-button',
;;
;; CONSTANTS:
;; `*google-define-html-entry-table*',
;;
;; VARIABLES:
;; `*google-define-view-map*', `*google-define-get-buffer*',
;; `*google-define-buffer-suffix*', `*get-google-defined*',
;; `*regexp-google-defined-fontlock*', `*google-define-redux-xrefs*',
;; `*regexp-google-define-headings*',
;; `*regexp-google-define-dictionary-heading-tag*',
;; `*regexp-google-define-itemized*',
;; `*regexp-google-define-next-url-in-heading*',
;; `*regexp-google-define-source-ref*',
;;
;; GROUPS:
;; `google-define-redux', `google-define-redux-faces',
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `google-define-get-command-TEST'            -> mon-testme-utils.el
;;
;; TODO:
;; Integrate Wordnet features. 
;; :SEE (URL `http://wordnet.cs.princeton.edu')
;;
;; Google's "define:<SOME-WORD>" returns wordnet definitions.  These appear in
;; the `*google-define-get-buffer*' returned by `google-define-get-command' as:
;; <a href="/url?q=http://wordnetweb.princeton.edu/perl/webwn{WORDNET-PARAMS}>
;; It might be interesting to integrate/adapt William Xu's wordnet interface: 
;; :SEE (URL `http://xwl.appspot.com/ref/wordnet.el')
;; :SEE (URL `http://github.com/xwl/xwl-elisp/raw/master/wordnet.el')
;; Or maybe Henry G. Weller's adaptation of above for use w/ `org-mode':
;; :SEE (URL `http://www.emacswiki.org/emacs/wn-org.el')
;; using the REST
;;
;; NOTES:
;; The required functions listed below are also provided in a dedicated library
;; with :FILE google-define-redux-supplemental.el 
;; :SEE (URL `http://www.emacswiki.org/emacs/google-define-redux-supplemental.el')
;; Though, that package not always be current and it is preferred/recommended to
;; load the required packages instead.
;; 
;; SNIPPETS:
;;
;; REQUIRES:
;; font-lock.el 
;;
;; :REQUIRES `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
;; :REQUIRED-BY `google-define'
;;
;; `*mon-help-docstring-help-bffr*'            <- :FILE mon-doc-help-utils.el
;; :REQUIRED-BY `mon-help-temp-docstring-display'
;;
;; :REQUIRES `mon-help-KEY-tag'                <- :FILE mon-doc-help-utils.el
;; :REQUIRED-BY `gg-def-base'
;;
;; :REQUIRES `mon-string-justify-left'         <- :FILE mon-utils.el
;; :REQUIRED-BY `google-define-parse-buffer'
;;
;; :REQUIRES `mon-g2be'                        <- :FILE mon-utils.el
;; :REQUIRED-BY `google-define-font-lock', `google-define-parse-buffer'
;;
;; :REQUIRES `mon-buffer-exists-p'             <- :FILE mon-utils.el
;; :REQUIRED-BY `google-define-kill-def-buffers'
;;
;; :REQUIRES `mon-buffer-exists-so-kill'       <- :FILE mon-utils.el
;; :REQUIRED-BY `google-define-kill-def-buffers'
;;
;; THIRD-PARTY-CODE:
;; Following are the modifications made to Jeremy English's google-define.el 
;; :SEE (URL `http://www.emacswiki.org/emacs/google-define')
;; :SEE (URL `http://www.emacswiki.org/emacs/google-define.el')
;; SEE BOF for original versions.
;; These are provided verbatim:
;; `google-define-word-at-point'
;; `*google-define-html-entry-table*'
;; Following functions are modified:
;; `google-define-get-command'
;; `google-define-parse-buffer'
;; `google-define'
;; Following functions were replaced with inline forms:
;; `google-define-replace-unicode'
;; `google-define-replace-html'
;; `google-define-replace-string'
;; `google-define-ascii-entry'
;; `google-define-number-entry'
;;
;; URL: https://github.com/mon-key/mon-emacs/raw/master/emacs-load-files/naf-mode/google-define-redux.el
;; EMACSWIKI-URL: http://www.emacswiki.org/emacs/google-define-redux.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-02-04T21:39:15-05:00Z}#{10055} - by MON>
;;
;; EMACSWIKI: 
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-02-03T13:41:11-05:00Z}#{10053} - by MON>
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
;; Copyright © 2010-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(require 'font-lock)

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-20T11:44:49-04:00Z}#{10116} - by MON KEY>
(defgroup google-define-redux nil
  "Extensions for `google-define'.\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `*google-define-buffer-suffix*'
`*google-define-get-buffer*', `*google-define-html-entry-table*', `gg-def-base',
`gg-def-num', `gg-def-delim', `gg-def-inition', `gg-def-defined'.\n►►►"
  :group 'mon-base  
  :group 'google-define
  :prefix "google-define-"
  :link '(url-link 
          :tag ":GITHUB-FILE" 
          "https://github.com/mon-key/mon-emacs/raw/master/emacs-load-files/naf-mode/google-define-redux.el")
  :link '(url-link 
          :tag ":EMACSWIKI-FILE"
          "http://www.emacswiki.org/emacs/google-define-redux.el")
  :link '(emacs-library-link 
          :tag ":FILE google-define-redux.el"
          "google-define-redux.el"))
;;
;;; :TEST-ME (symbol-plist 'google-define-redux)
;;; :TEST-ME (documentation-property 'google-define-redux 'group-documentation)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-20T11:44:49-04:00Z}#{10116} - by MON KEY>
(defgroup google-define-redux-faces nil
  "Face definitions for extensions to `google-define'.\n
:SEE :FILE google-define-redux.el\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-num', `gg-def-inition',
`gg-def-defined', `mon-doc-help-utils-faces'.\n►►►"
  :group 'faces
  :group 'google-define-redux
  :group  (or (when (featurep 'mon-doc-help-utils)  'mon-doc-help-utils-faces) nil)  
  :prefix "gg-def-")
;;
;;; :TEST-ME (symbol-plist 'google-define-redux-faces)
;;; :TEST-ME (documentation-property 'google-define-redux-faces 'group-documentation)


;;; ==============================
;;; :CHANGESET 2395
;;; :CREATED <Timestamp: #{2011-01-15T18:39:32-05:00Z}#{11026} - by MON KEY>
(defcustom *google-define-redux-xrefs* 
  '(google-define-get-command google-define-parse-buffer google-define-font-lock
    google-define google-define-kill-def-buffers 
    google-define-make-query-url google-define-clean-string
    google-define-button-action google-define-button-do-xref
    google-define-insert-xref-button google-define-find-next-heading
    google-define-make-heading-replacement google-define-find-headings
    google-define-related-language-url google-define-find-itemized
    google-define-find-next-url-in-heading
    google-define-parse-related-languages google-define-related-language-xref
    %google-define-clean-term-for-split
    %google-define-find-next-related-language-xref
    %google-define-find-itemized-regexp-for-search-term
    %google-define-verify-query-url-lang-params
    %google-define-verify-query-url-xref-type-params
    %google-define-set-url-current-object
    ;; :FACES
    gg-def-defined gg-def-base gg-def-delim gg-def-num gg-def-inition gg-def-heading
    ;; :BUTTONS
    google-define-button google-related-lang-button
   ;; :VARIABLES
    *google-define-get-buffer* *google-define-buffer-suffix* *get-google-defined*
    *regexp-google-defined-fontlock* 
    *regexp-google-define-headings*
    *regexp-google-define-dictionary-heading-tag*
    *regexp-google-define-itemized* *regexp-google-define-next-url-in-heading*
    *regexp-google-define-source-ref* *google-define-redux-xrefs*)
  "Xrefing list of mon `google-defin-*' symbols, functions, and variables.\n
The symbols contained of this list are defined in :FILE google-define-redux.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-dir-utils-xrefs*', `*mon-keybindings-xrefs*',
`*mon-testme-utils-xrefs*', `*mon-button-utils-xrefs*', `*mon-bzr-utils-xrefs*'
`*mon-buffer-utils-xrefs*', `*mon-error-utils-xrefs*', `*mon-line-utils-xrefs*',
`*mon-macs-xrefs*', `*mon-plist-utils-xrefs*', `*mon-post-load-hooks-xrefs*', 
`*mon-seq-utils-xrefs*', `*mon-string-utils-xrefs*', `*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*', `*mon-slime-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'google-define-redux
  :group 'mon-xrefs)

 
;;; ==============================
;;; :GOOGLE-DEFINE-ADDITIONS-EXTENSIONS-MODIFICATIONS

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-03T13:43:28-05:00Z}#{10053} - by MON>
(defcustom *google-define-get-buffer* nil 
  "A buffer `google-define-get-command' should return results in.\n
Default is \"*GOOGLE-DEFINE-GET-BUFFER*\".\n
Dynamically allocated as needed.\n
:SEE-ALSO `*google-define-buffer-suffix*'.\n►►►"
  :type 'string
  :group 'google-define-redux)
;;
(unless (and (intern-soft "*google-define-get-buffer*" obarray)
             (bound-and-true-p *google-define-get-buffer*))
  (setq *google-define-get-buffer* (upcase (symbol-name '*google-define-get-buffer*)))
  (custom-note-var-changed '*google-define-get-buffer*))
;;
;;; :TEST-ME (symbol-value '*google-define-get-buffer*)
;;;(progn (makunbound '*google-define-get-buffer*) (unintern "*google-define-get-buffer*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T14:55:41-05:00Z}#{10055} - by MON>
(defcustom *google-define-buffer-suffix* '("*" . ":gg-definition*")
  "A consof prefix and suffix strings to attach to definition buffers
created with `google-define'.\n
Default is:\n\n\(\"*\" . \":gg-definition*\"\)\n
:NOTE Don't change this var without verifying that `google-define-entry-setup'
can still match the buffer on entry else we can't frob the `help-mode-hook'.\n
:CALLED-BY `google-define-kill-def-buffers' when cleaning up definition buffers.\n
:SEE-ALSO `*google-define-get-buffer*' `*google-define-html-entry-table*',
`google-define-get-command'.\n►►►"
  :type '(cons string string)
  :group 'google-define-redux)
;;
;;; :TEST-ME *google-define-buffer-suffix*
;;
;;;(progn (makunbound '*google-define-buffer-suffix*) (unintern "*google-define-buffer-suffix*" obarray) )

;;; ==============================
;;; :CHANGESET 1838
;;; :CREATED <Timestamp: #{2010-06-09T18:07:19-04:00Z}#{10233} - by MON KEY>
(defvar *get-google-defined* nil
  "*A process name for `google-define-get-command'. \n
The value of this variable (a string) is used as `make-network-process's keyword
arg ``:name''.\n
Useful for recovering/debugging hung processes e.g. with:\n
 \(get-process *get-google-defined*\)\n
 \(process-status \(get-process *get-google-defined*\)\)\n
:SEE-ALSO `*google-define-get-buffer*', `*google-define-buffer-suffix*'.\n►►►")
;;
(unless (and (intern-soft "*get-google-defined*")
             (bound-and-true-p *get-google-defined*))
  (setq *get-google-defined*  (symbol-name '*get-google-defined*)))
;;
;;; :TEST-ME (symbol-value '*get-google-defined*)
;;;(progn (makunbound '*google-define-get-buffer*) (unintern "*google-define-get-buffer*" obarray) )


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T18:50:48-04:00Z}#{11201} - by MON KEY>
(defvar *regexp-google-define-headings* 
  (concat 
   "\\("
   "\\(<h[[:digit:]]>\\)"
   (regexp-opt '("Usage examples" "Related languages" 
                 "Web definitions" "Related phrases") t) ;; what about"<h4>Images</h4>"
   "\\(</h[[:digit:]]>\\)"
   "\\)")
  "Regexp matching any of the following:\n
 \"<h3>Usage examples</h3>\"\n
 \"<h3>Related languages</h3>\"\n
 \"<h3>Web definitions</h3>\"\n
 \"<h3>Related phrases</h3>\"\n
:EXAMPLE\n\n\(while \(search-forward-regexp *regexp-google-define-headings* nil t\)\)\n
 <h3>Usage examples</h3>\n
 <h3>Related languages</h3>\n
 <h3>Web definitions</h3>\n
 <h3>Related phrases</h3>\n
:NOTE As implemented regexp will match any heading number not just \"<h3>\".\n
:SEE-ALSO `*regexp-google-define-itemized*'.\n►►►")

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T12:55:08-04:00Z}#{11202} - by MON KEY>
(defvar *regexp-google-define-itemized*
  ;; :NOTE If this regexp changes we _must_ adjust the indexes for "<<TERM>>"!
  (concat "\\("                               ; grp 1
          "\\("                               ; grp 2
          "\\(<li>\\)"                        ; grp 3
          "\\([[:cntrl:]]\\|[[:blank:]]\\)?*" ; grp 4
          "\\)"                               ; cls 2

          "\\("                               ; grp 5
          "\\(<<TERM>>\\)?"                   ; grp6 
          "\\(.*\\)"                          ; grp 7
          "\\)"                               ; cls 5
          "\\("                               ; grp 8
          "\\([[:cntrl:]]\\|[[:blank:]]\\)?*" ; grp 9
          "\\(</li>\\)"                       ; grp 10
          "\\([[:cntrl:]]\\|[[:blank:]]\\)?*" ; grp 11
          "\\)"                               ; cls 8
          "\\)")
"Template regexp for use with `%google-define-find-itemized-regexp-for-search-term'.\n
Subseq at indexes 48-56 is \"<<TERM>>\" it is replaced dynamically with a valid
search term for use with `google-define-find-itemized'.\n
EXAMPLE\n\n\(substring *regexp-google-define-itemized* 48 56\)\n
\(replace-regexp-in-string \"<<TERM>>\" \"REPLACEMET-TERM\" *regexp-google-define-itemized*\)\n
:SEE-ALSO `*regexp-google-define-itemized*'.\n►►►")

;;; ==============================
;; :NOTE In the new GG definitions presentation the first definition is:
;;;   <span id="sd" style="float:left">Dictionary</span>
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T13:41:26-04:00Z}#{11202} - by MON KEY>
(defvar *regexp-google-define-dictionary-heading-tag*
  (concat 
   "<"
   "\\(div\\|li\\|span\\)"
   "[[:blank:]]+"
        
   "\\(" ;; class="dct-srch-rslt"
   "class=\"dct-\\([[:alpha:]]+\\)"
   "\""
   "\\([[:cntrl:]]\\|[[:blank:]]\\)?+"
   "\\)?"

   ;; ;; class="dct-tlb" title="Part-of-speech"
   "\\("
   "\\([[:blank:]]?+[[:alnum:]-]+\\)" ;; title-4 id-3
   "=\"[[:alnum:]-]+\"\\)?+"
   "\\([[:cntrl:]]\\|[[:blank:]]\\)?+"
   ">")
  "Finds occurences of the \"Dictionary\" tables terms and parts of speech in HTML tags.\n
:EXAMPLE\n\n\(while \(search-forward-regexp *regexp-google-define-dictionary-heading-tag* nil t\)\)\n
<div class=\"dct-srch-otr\">\n
<div class=\"dct-srch-inr rt-sct-exst\">\n
<div class=\"dct-srch-rslt\">\n
<ul class=\"dct-e2\" id=\"pr-root\" >\n
<li class=\"dct-eh\"
             >\n
<div  class=\"dct-eh\">\n
<span class=\"dct-tt\">yo·gurt\n
<span class=\"dct-tlb\" title=\"Part-of-speech\">Noun</span></span>\n
:SEE-ALSO .\n►►►")


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T14:23:34-04:00Z}#{11202} - by MON KEY>
(defvar *regexp-google-define-next-url-in-heading*
  (concat "\\(" ; grp 
          "\\(" ; grp 
          "\\(<a[[:blank:]]\\)" ;grp
          "\\("                               ; grp
          "\\(.*\\)=\"\\(.*\\)\"[[:blank:]]+" ; grp grp
          "\\)?*" ; cls
          "\\)" ; cls 
          "\\("                          ; grp
          "\\("  "href=\"/dictionary"    ; grp  href="<STUFF>"
          "\\(.*\\)"                     ; grp
          "\\([[:blank:][:cntrl:]]*\\)?" ; grp
          "\"" ">" 
          "\\(.*\\)" ; grp
          "</a>"
          "\\)" ; cls
          "\\|"
          "\\(" ; grp
          "href=\"http://www.google.com/url\\?q="
          "\\(.*\\)" ; grp
          "\"" ">" 
          "\\(.*\\)?" ; grp 14 
          "</a>" 
          "\\)" ;cls
          "\\)" ;cls
          "\\)" ;cls
          )
"Regexp for for use with `google-define-find-next-url-in-heading'.\n
First group matches patterns with the form:\n
 ,----
 | <a href=\"/dictionary?q=yogurt&hl=en&sl=es&tl=en
 | \">español</a>
 `----\n
Second group matches patterns with the form:\n
 ,----
 | <a class=\"lightblue\" href=\"http://www.google.com/url?q=<SOME-NON-GG-URL>\">Kirsten Gillibrand</a>
 `----\n
:EXAMPLE\n\n\(while \(search-forward-regexp *regexp-google-define-next-url-in-heading* nil t\)\)\n
<a href=\"/dictionary?q=yogurt&hl=en&sl=es&tl=en
\">español</a>\n
<a class=\"lightblue\" href=\"http://www.google.com/url?q=<SOME-NON-GG-URL>\">Kirsten Gillibrand</a>\n
:SEE-ALSO .\n►►►")

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T13:42:15-04:00Z}#{11202} - by MON KEY>
(defvar *regexp-google-define-source-ref*
  ;;(setq *regexp-google-define-source-ref*
  (concat 
   "\\("                                 ; group 1
   "\\(<div>"                            ; group 2
   "\\([[:cntrl:]]\\|[[:blank:]]\\)?*"   ; group 3
   "\\)" 
   "\\("                        ; group 4
   "<a" 
   "[[:blank:]]+" 
   "\\(.*=\".*\"[[:blank:]]+\\)?"
   "href=\"http://www.google.com/url\\?q="
   "\\(.*\\)\""                                     ; group 
   ">\\)"                                           ; close 
   "\\([[:cntrl:]]\\|[[:blank:]]\\)?*"              ; group 
   "\\(.*\\)"                                       ; group 
   ;;
   "\\("                                            ; group 
   "\\(\\([[:cntrl:]]\\|[[:blank:]]\\)?*</a>\\)"    ; group 
   "\\(\\([[:cntrl:]]\\|[[:blank:]]\\)?</div>\\)"   ; group 
   "\\)"                                            ; close 
   "\\)"                                            ; close 
   )
  ;;)
"Regexp matching HTML <div> tags with URL links to sources for definition.\n
Matches the follwing pattterns:\n
 ,----
 | <div>
 | <a href=\"http://www.google.com/url?q=<SOURCE-URL>\">
 | en.wiktionary.org/wiki/snarf
 | </a>
 | </div>
 `----\n
:EXAMPLE\n\n(search-forward-regexp *regexp-google-define-source-ref* nil t)\n
<div>
<a href=\"http://www.google.com/url?q=<SOURCE-URL>\">
en.wiktionary.org/wiki/snarf
</a>
</div>\n
:SEE-ALSO .\n►►►")
;;
;; :NOTE a variant of `*regexp-google-define-source-ref*' without match on <div>
;; (concat 
;;  "\\("
;;  "\\([[:blank:][:cntrl:]]*\\)?"
;;  "\\("
;;  "\\(<a[[:blank:]]\\)"
;;  "\\("
;;  "\\(.*\\)=\"\\(.*\\)\"[[:blank:]]+"
;;  "\\)?*"
;;  "\\)" 
;; "\\(" 
;;  "href=\"http://www.google.com/url\\?q="
;;  "\\(.*\\)"
;;  "\"" ">" 
;;  "\\([[:blank:][:cntrl:]]*\\)?"
;;  "\\(.*\\)?"
;;  "\\([[:blank:][:cntrl:]]*\\)?"
;;  "</a>" 
;;  "\\)"
;; "\\([[:blank:][:cntrl:]]*\\)?"          
;;  "\\)")
;;
;;; ==============================

;;; ==============================
;;; :REQUIRES `mon-help-KEY-tag' <- :FILE mon-doc-help-utils.el
;;; :CREATED <Timestamp: #{2010-02-04T20:22:51-05:00Z}#{10055} - by MON>
(defface gg-def-base
    (if (featurep 'mon-doc-help-utils)
        '((t :inherit mon-help-KEY-tag))
      ;; else
      '(( ((class color) (min-colors 88)) 
          (:foreground "light steel blue" :weight extrabold)) ))
  "*Base face for font-locking buffers returned by `google-define'.\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition', `mon-doc-help-utils-faces'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-base)
;;;(progn (makunbound 'gg-def-base) (unintern "gg-def-base" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:13-05:00Z}#{10055} - by MON KEY>
(defface gg-def-delim 
    '((t :inherit gg-def-base :foreground "sky blue"))
  "*Provides fontlocking of space seperated delmiter charcaters.\n
Default delimiters characters include:\n ►, |, ◄\n
when preceded and followed by whitespace e.g.:\n
 \" ► \" \" | \" \" ◄ \"\n
:NOTE\n\(assq 'definition-delim *regexp-google-defined-fontlock*\)\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-delim)
;;;(progn (makunbound 'gg-def-delim) (unintern "gg-def-delim" obarray) )


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T17:24:16-04:00Z}#{11201} - by MON KEY>
(defface gg-def-heading
    '((t :inherit gg-def-base :foreground "steel blue"))
    "*Provided for fontlocking heading sections of a definition.\n
:EXAMPLE\n\n\(describe-face 'gg-def-heading\)\n
:NOTE\n\(assq 'definition-heading *regexp-google-defined-fontlock*\)\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
    :group 'google-define-redux-faces)
;;
;; (describe-face 'gg-def-heading)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:16-05:00Z}#{10055} - by MON KEY>
(defface gg-def-num 
    '((t :inherit gg-def-base  :foreground "powder blue"))
  "*Provides fontlocking of enumerated numbers preceding a definition.\n
:NOTE\n\(assq 'definition-num *regexp-google-defined-fontlock*\)\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-num)
;;;(progn (makunbound 'gg-def-num) (unintern "gg-def-num" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T16:06:20-05:00Z}#{10055} - by MON KEY>
(defface gg-def-inition
  '((t :inherit gg-def-base  :foreground "wheat1" :weight normal))
  "*Provides fontlocking of google definition content. 
Default is anything after the default \"    | \".\n
:NOTE\n\(assq 'definition-line *regexp-google-defined-fontlock*\)\n
\(assq 'definition-line-w/o *regexp-google-defined-fontlock*\)\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-inition)
;;;(progn (makunbound 'gg-def-inition) (unintern "gg-def-inition" obarray) )

;;; ==============================
;;; :TODO See notes below about matching alternate reasonable definition suffixes.
;;; :CREATED <Timestamp: #{2010-02-05T16:05:47-05:00Z}#{10055} - by MON KEY>
(defface gg-def-defined
    '((t :inherit gg-def-base :foreground "cadet blue"))
  "*Provides fontlocking of word defined by definition.\n
:SEE-ALSO `gg-def-base', `gg-def-delim', `gg-def-defined', `gg-def-num',
`gg-def-inition'.\n►►►"
  :group 'google-define-redux-faces)
;;
;;; :TEST-ME (describe-face 'gg-def-defined)
;;;(progn (makunbound 'gg-def-defined) (unintern "gg-def-defined" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-10-17T13:30:43-04:00Z}#{10417} - by MON KEY>g
(defcustom *regexp-google-defined-fontlock*
  `((definition-hdr-w/o "Definitions for: %s\n\n" 7 (face  default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-hdr    "^Definitions for:"        0 (face gg-def-base))
    (definition-num    "^ +\\([0-9]\\{1,2\\}\\) " 1 (face gg-def-num))
    (definition-delim  " \\(►\\||\\|◄\\) ?"       1 (face gg-def-delim))
    (definition-delim-top  "►"                    7 (face default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-delim-btm  "◄"                    7 (face default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-line-w/o  "    | "                7 (face default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-line   "^    | \\(.*\\)$"         1 (face gg-def-inition))
    ;; :NOTE (concat " \\(" <SEARCHED-WORD>  "\\)[\\[:punct:]\\[:blank:]\n]")
    (definition-word    " \\(<SEARCHED-WORD>\\)[\\[:punct:]\\[:blank:]\n]" 1 (face gg-def-defined))
    (definition-heading ,(regexp-opt (list ":USAGE-EXAMPLES" ":RELATED-LANGUAGES" ":WEB-DEFINITIONS") t)
       1 (face gg-def-heading))
    )
  "A list of font-lock rules for `google-define-font-lock'.\n
Elts of list have the form:\n
 \(<KEY> <REGEXP> <MATCH-GRP> \(face <FACE>\)\)\n
The car last element in list \(e.g. `face`\) is a constant and must be present.\n
Required values of <KEY> are:\n
 definition-hdr  definition-hdr-w/o
 definition-num  definition-delim  
 definition-line definition-word\n
<REGEXP> and <MATCH-GRP> should match according to fontlocks for <FACE>.\n
The regexp of key definition-word must contain the token \"<SEARCHED-WORD>\",
this is used as a marker to split on b/c `defcustom' doesn't provide a clean way

to conditionally pass/specify a cons instead of a regexp.\n
:NOTE The regexp for key `definition-hdr-w/o` associates a format string spec
for use by `google-define-parse-buffer' and should be reflected by the regexp in
key `definition-hdr`.\n
:NOTE The regexp for key `definition-line-w/o' associates a `fill-prefix' for
use by `google-define-parse-buffer' and should be reflected by the regexp in key
`definition-line`.\n
:NOTE The regexp for keys `definition-delim-top`/`definition-delim-btm`
associates delimiters for use by `google-define-parse-buffer' and should be
reflected by the regexp in key `definition-delim`.\n
:EXAMPLE\n\n(assq 'definition-hdr *regexp-google-defined-fontlock*)\n
\(nth 1 \(assq 'definition-hdr *regexp-google-defined-fontlock*\)\)\n
:SEE-ALSO .\n►►►"
  :type  '(repeat (list symbol regexp integer (list (const face) face)))
  :group 'google-define-redux
  :group 'google-define-redux-faces)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-04T21:28:01-05:00Z}#{10055} - by MON>
(defun google-define-get-command (host path)
  "Retrieve google defnitions to process-buffer `*google-define-get-buffer*'.\n
:EXAMPLE\n\n(google-define-get-command-TEST)\n
:SEE-ALSO `google-define', `google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `google-define-get-command-TEST'.\n►►►"
  (let* ((timeout 60) ;; 180)
         (port 80)  ;; http
         (post-cmd
          (concat "GET " path " HTTP/1.0\r\n"
                  "Host: " host "\r\n"
                  "User-Agent: Emacs\r\n"
                  ;; "User-Agent: wget\r\n" ;; fails
                  ;; "User-Agent: lynx\r\n"
                  ;; "User-Agent: w3m\r\n"
                  ;; "User-Agent: links\r\n"
                  ;; (concat "User-Agent: Mozilla/5.0 "
                  ;;         "(Windows; U; Windows NT 5.0; en-US; rv:1.8.1.1) "
                  ;;         "Gecko/20061204 Firefox/2.0.0.1\r\n")
                  (concat "Accept: text/xml,application/xml,application/"
                          "xhtml+xml,text/html;q=0.9,text/plain;q=0.8,"
                          "image/png,*/*;q=0.5\r\n")
                  "Accept-Language: en-us,en;q=0.5\r\n"
                  "Accept-Encoding: gzip,deflate\r\n"
                  ;;"Accept-Charset: UTF-8;q=0.8,ISO-8859-1;q=0.7,*;q=0.7\r\n"
                  "Accept-Charset: ISO-8859-1;q=0.7,*;q=0.7\r\n"
                  "\r\n"))
         proc
         buf)
    (progn
      (when (get-buffer *google-define-get-buffer*)
        (kill-buffer *google-define-get-buffer*))
      (setq proc (make-network-process :name *get-google-defined* 
                                       :buffer *google-define-get-buffer*
                                       :host host
                                       :service port))
      (setq buf (process-buffer proc))
      (process-send-string proc post-cmd)
      (mon-message :msg-spec '(":FUNCTION `google-define-get-command' "
                             " -- Snarfing %s -- waiting for response...")
                   :msg-args host)
      (while (equal (process-status proc) 'open)
        (unless (accept-process-output proc timeout)
          (unwind-protect
              (mon-format :w-fun #'error
                          :w-spec '(":FUNCTION `google-define-get-command' "
                                    "-- host %s timed out, deleting network process %S")
                          :w-args `(,host ,proc))
            (delete-process proc))
          (mon-message :msg-spec '(":FUNCTION `google-define-get-command' "
                                 "-- response received: processing..."))))
      
    buf)))
;;
;;; :TEST-ME (google-define-get-command-TEST)


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T19:52:52-04:00Z}#{11201} - by MON KEY>
(defun %google-define-verify-query-url-lang-params (lang)
  "Verify the language parameter for use when constructing a google-define URL.\n
When LANG is a string comprised of two characters return it.\n
If it is not return \"en\".\n
:EXAMPLE\n\n\(%google-define-verify-query-url-lang-params \"fr\"\)\n
 \(%google-define-verify-query-url-lang-params \"esp\"\)\n
 \(%google-define-verify-query-url-lang-params \"\"\)\n
 \(%google-define-verify-query-url-lang-params nil\)\n
 \(%google-define-verify-query-url-lang-params 8\)\n
:SEE-ALSO .\n►►►"
  (or (and (or (null lang)
               (not (stringp lang))
               (not (eql (length lang) 2)))
           "en")
      lang))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T20:21:16-04:00Z}#{11201} - by MON KEY>
(defun %google-define-verify-query-url-xref-type-params (xref-key)
  ;; (%google-define-verify-query-url-xref-type-params :related-phrase)
  ;; => "&oi=dict_re"
  ;; (%google-define-verify-query-url-xref-type-params :synonym)
  ;; => "&oi=dict_lk"
  ;; (url-parse-query-string (%google-define-verify-query-url-xref-type-params :synonym))
  ;; => (("oi" "dict_lk"))
  (let ((xref-if
         (assoc xref-key
                '((:related-phrase . "re")
                  (:synonym        . "lk")))))
    (and xref-if (concat "&oi=dict_" (cdr xref-if) ))))

;;; ==============================
;;; A poor mans `CL:STRING-TRIM'...
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T15:38:20-04:00Z}#{11202} - by MON KEY>
(defun google-define-clean-string (from-char to-char clean-string)
  "Replace all occurences of FROM-CHAR in clean-string with TO-CHAR.\n
FROM-CHAR and TO-CHAR are characters.
CLEAN-STRING is an object satisfying `stringp', if not return value is \"\".\n
When CLEAN-STRING is prefixed or suffixed by FROM-CHAR all leading and trailing
occurences are removed such that return value will only be contained of TO-CHAR
when FROM-CHAR occurs in a non-leading or trailing position.\n
when multiple occurences of from-char occur in a non-leading or trailing
position these are reduced to a single occurence.\n
:EXAMPLE\n
\(google-define-clean-string 32 45 \"Related Languages\"\)\n
\(google-define-clean-string 32 43\"  Web Definitions  \"\)\n
\(google-define-clean-string 32 45 \"Related  -   Languages\"\)\n
\(google-define-clean-string 32 43\"  Web   +  Definitions  \"\)\n
\(google-define-clean-string nil\)\n
\(google-define-clean-string \"\"\)\n
\(google-define-clean-string [?a ? ?b ? ?c]\)\n
\(google-define-clean-string 8\)\n
:SEE-ALSO `%google-define-clean-term-for-split', `url-eat-trailing-space',
`url-strip-leading-spaces'.\n►►►"
  (catch '%google-define-clean-string
    (let* ((gdcs-str (or (and (or (null clean-string)
                                  (not (stringp clean-string))
                                  (zerop (length clean-string)))
                              (throw '%google-define-clean-string (make-string 0 0)))
                         (subst-char-in-string from-char to-char clean-string)))
           (gdcs-lst (unless (string= gdcs-str clean-string)
                       (append gdcs-str nil))))
      (save-match-data
        (replace-regexp-in-string (string to-char 43)
                                  (string to-char)
                                  (if gdcs-lst
                                      (progn
                                        (while (eq (car gdcs-lst) to-char)
                                          (pop gdcs-lst))
                                        (setq gdcs-lst (nreverse gdcs-lst))
                                        (while (eq (car gdcs-lst) to-char)
                                          (pop gdcs-lst))
                                        (concat (setq gdcs-lst (nreverse gdcs-lst)) nil))
                                    clean-string) t)))))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T20:55:04-04:00Z}#{11201} - by MON KEY>
(defun %google-define-clean-term-for-split (term)
  "Replace all space characters \(char 32\) in TERM with a plus sign \(char 43\).\n
Helper function for .\n
TERM should satisfy `stringp', if not return \"\".\n
Return value is as if by `google-define-make-heading-replacement'.\n
:EXAMPLE\n\n\(%google-define-clean-term-for-split \"absorbent cotton\"\)\n
\(%google-define-clean-term-for-split \" absorbent cotton \"\)\n
\(%google-define-clean-term-for-split \" absorbent    cotton \"\)\n
\(%google-define-clean-term-for-split \" absorbent + + cotton \"\)\n
\(%google-define-clean-term-for-split \" \"\)\n
\(%google-define-clean-term-for-split \" + \"\)\n
\(%google-define-clean-term-for-split \"\"\)\n
\(%google-define-clean-term-for-split nil\)\n
\(%google-define-clean-term-for-split 8\)\n
:SEE-ALSO `%google-define-clean-term-for-split'.\n►►►"
  (google-define-clean-string 32 43 term))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T12:22:06-04:00Z}#{11202} - by MON KEY>
(defun %google-define-clean-heading-for-replace (heading-match)
  "Replace all space characters \(char 32\) in HEADING-MATCH with a dash \(char 45\).\n
Helper function for `google-define-make-heading-replacement'.\n
HEADING-MATCH should satisfy `stringp', if not return \"\".\n
Return value is as if by `google-define-make-heading-replacement'.\n
:EXAMPLE\n\n\(%google-define-clean-heading-for-replace \"Related Languages\"\)\n
\(%google-define-clean-heading-for-replace \" Web Definitions    \"\)\n
:SEE-ALSO `%google-define-clean-term-for-split'.\n►►►"
  (google-define-clean-string 32 45 heading-match))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T20:21:13-04:00Z}#{11201} - by MON KEY>
(defun* google-define-make-query-url (search-term &key 
                                                  (host-lang "en")
                                                  (source-lang "en")
                                                  (target-lang "en") 
                                                  xref-type)
  "Return a URL path suitable for use with `google-define-get-command'
search-term is a string naming a term to retrieve a definition for.\n
Keyword SOURCE-LANG is a two character string identifying the language of
SEARCH-TERM's source. Default is \"en\".\n
Keyword HOST-LANG is a two character string identifying the host language
for SEARCH-TERM's definition. Default is \"en\".\n
Keyword TARGET-LANG is a two character string identifying the target language
for SEARCH-TERM's definition. Default is \"en\".\n
Keyword XREF-TYPE is a keyword either :related-phrase or :synonym. It is used
for generating urls when xrefing from an existing definition.\n
:EXAMPLE\n
\(google-define-make-query-url \"yogurt\"\)\n
\(google-define-make-query-url \"yogurt\" :source-lang \"es\"\)\n
\(google-define-make-query-url \"yogurt\" :source-lang \"es\" :target-lang \"it\"\)\n
\(google-define-make-query-url \"absorbent cotton\" :xref-type :related-phrase\)\n
\(google-define-make-query-url \"yoghurt\" :xref-type :synonym\)\n
\(url-parse-query-string
 \(google-define-make-query-url \"absorbent cotton\" 
                               :host-lang \"en\" 
                               :source-lang \"es\" 
                               :target-lang \"it\" 
                               :xref-type :related-phrase\)\)\n
:SEE-ALSO `url-parse-query-string', `url-insert-entities-in-string'.\n►►►"
  (setq host-lang (%google-define-verify-query-url-lang-params host-lang))
  (setq source-lang (%google-define-verify-query-url-lang-params source-lang))
  (setq target-lang (%google-define-verify-query-url-lang-params target-lang))
  (and xref-type (setq xref-type 
                       (%google-define-verify-query-url-xref-type-params xref-type)))
  
   ;; :NOTE Wonder what the "aq=f" param is? 
   ;;  http://www.google.com/dictionary?aq=f&hl=en&langpair=en%7Cen&q=yogurt
   ;; We'll use this format instead:
   ;; http://www.google.com/dictionary?hl=fr&sl=pt&tl=en&q=car      
  ;; url-insert-entities-in-string
  (concat  "/dictionary?"
            "hl=" host-lang
            "&sl=" source-lang
            "&tl=" target-lang
            ;; http://www.google.com/dictionary?hl=en&sl=pt&tl=en&q=car
            "&q="
            ;; (replace-regexp-in-string " +" "+" search-term)
            (%google-define-clean-term-for-split search-term)
            xref-type))

;;; ==============================
;; (google-define-parse-buffer "buffer" (get-buffer-create "*gg-example-buffer*"))
;;; :PREFIX "gdpb-"
;;; :REQUIRES `mon-g2be', `mon-string-justify-left' <- mon-utils.el
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T13:41:22-05:00Z}#{10053} - by MON>
(defun google-define-parse-buffer (search-word def-disp-buffer command-params)
  "Pull all of the definitions out of the data returned from google.\n
Print in a temp-buffer, parse, and convert for presentation.\n
COMMAND-PARAMS is a cons pair suitable for use with `google-define-get-command'.
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-font-lock', `google-define-kill-def-buffers',
`mon-string-justify-left' `*google-define-buffer-suffix*'
`*google-define-get-buffer*', `*google-define-html-entry-table*'.\n►►►"
  (let* ((gdpb-cnt 0)
         (gdpb-host-params command-params)
         (gdpb-gthr-bffr ;; <- *standard-output* is here.
          (prog1 
              def-disp-buffer
            (when (buffer-live-p (get-buffer def-disp-buffer))
              (with-current-buffer (get-buffer def-disp-buffer)
                (let ((buffer-read-only nil))
                  (erase-buffer))))))
         ;; :NOTE `gdpb-gthr-bffr' _should_ be empty, don't bother checking.
         (standard-output (get-buffer-create gdpb-gthr-bffr)))
    (princ (format (cadr (assq 'definition-hdr-w/o *regexp-google-defined-fontlock*)) search-word))
    ;; Should this be using `with-current-buffer' instead of `set-buffer' here??
    ;; <Timestamp: #{2010-06-09T18:47:09-04:00Z}#{10233} - by MON KEY>
    ;; :NOTE After using the current features for a few months it seems
    ;; `with-current-buffer' is prob. NTRT and may even cause problems w/ Emacs
    ;; 24 as m. rudalics, j. linkov et al are currently muddifying their hands
    ;; with emacs display backend w/re behaviour of other-buffer,
    ;; set-window-buffer, etc.  In particular, this will affect how we intend to
    ;; eventually add help-mode style xrefs on the returned *<WORD>:gg-definition* buffer.
    ;;
    ;; :NOTE as of <Timestamp: #{2011-05-16T14:14:09-04:00Z}#{11201} - by MON KEY>
    ;; Google has changed the request parameters for its define/dictionary interface and presentation!
    ;;
    ;; :WAS
    ;; (set-buffer 
    ;; (google-define-get-command               
    ;;  "www.google.com" 
    ;;  (concat 
    ;;   ;; :NOTE "http://www.google.com/search?num=100&hl=en&q=define%3A%22big%22&btnG=Search"
    ;;   ;;  is equivalent to entering at the GG search form: define:"big".
    ;;   "/search?num=100&hl=en&q=define%3A%22"
    ;;   (replace-regexp-in-string " +" "+" search-word)
    ;;   "%22&btnG=Search")))
    ;;
    (set-buffer (google-define-get-command (car gdpb-host-params) (cdr gdpb-host-params))) 
    (unwind-protect
        (progn
          (mon-g2be -1)
          (google-define-find-headings search-word)
          (mon-g2be -1)
          (while ;; :WAS (search-forward-regexp "<li>\\([^<]+\\)" nil t)
              (google-define-find-itemized search-word)
            (incf gdpb-cnt)
            (let ((gdpb-wrd-def 
                   ;; :WAS  (replace-regexp-in-string "\\(\n\\|\r\\|\t\\)" "" (match-string 1))))
                   (replace-regexp-in-string "\\(\n\\|\r\\|\t\\|<br>\\)" "" (match-string 5) t)))
              (princ ;; <- spitting to our "*<WORD>:gg-definition*" buffer.
               (with-temp-buffer
                 (let ((gdpb-gt-pnt (make-marker))
                       (gdpb-lt-pnt (make-marker))
                       (fill-column 68)
                       (fill-prefix (or (cadr (assq 'definition-line-w/o *regexp-google-defined-fontlock*))
                                        "    | "))
                       (GDRH ;; :WAS `google-define-replace-html'
                        #'(lambda ()
                            (dolist (gdpb-DL-1 *google-define-html-entry-table*)
                              (let ((gdpb-ascii (caddr gdpb-DL-1))
                                    (GD-RPS ;; :WAS `google-define-replace-string'
                                     #'(lambda (frm-str to-str) 
                                         (progn 
                                           (mon-g2be -1)
                                           (while (search-forward frm-str nil t)
                                             (replace-match to-str nil t))))))
                                (funcall GD-RPS (car gdpb-DL-1) gdpb-ascii)
                                (funcall GD-RPS (cadr gdpb-DL-1) gdpb-ascii)))))
                       (GDRU ;; :WAS `google-define-replace-unicode'
                        #'(lambda ()
                            (mon-g2be -1)
                            (while (search-forward-regexp "&#\\([0-9]+\\);" nil t)
                              (let* ((GDRU-ucs (string-to-number (match-string 1)))
                                     (GDRU-rep (char-to-string (or (decode-char 'ucs GDRU-ucs) ?~))))
                                (replace-match GDRU-rep nil t))))))
                   (let ((gdpb-wspc 
                          (make-string
                           (or (string-match-p "[^[:space:]]" 
                                               (cadr (assq 'definition-line-w/o *regexp-google-defined-fontlock*)))
                               (string-match-p "[^[:space:]]" fill-prefix)
                               4) 32)))
                     (save-excursion              
                       (insert (format (concat 
                                        "%" (number-to-string (1- (length gdpb-wspc)))
                                        "d %s\n")
                                       gdpb-cnt 
                                       (cadr (assq 'definition-delim-top *regexp-google-defined-fontlock*))))
                       (set-marker gdpb-gt-pnt (point))
                       (insert (replace-regexp-in-string 
                                (concat "^" gdpb-wspc) ;; :WAS "^    " 
                                (cadr (assq 'definition-line-w/o *regexp-google-defined-fontlock*))
                                (mon-string-justify-left gdpb-wrd-def 64 4) t))
                       (set-marker gdpb-lt-pnt (point))
                       (insert "\n" gdpb-wspc ;; :WAS "\n    " 
                               (cadr (assq 'definition-delim-btm *regexp-google-defined-fontlock*))
                               "\n\n")))
                   (fill-region-as-paragraph gdpb-gt-pnt gdpb-lt-pnt)
                   (funcall GDRH)
                   (funcall GDRU))
                 (mon-buffer-sub-no-prop) )
               (get-buffer gdpb-gthr-bffr))))
          (set-buffer gdpb-gthr-bffr))      ; :CLOSE progn
      ;;
      ;; Close current TCP process of current `google-define-get-command' buffer.
      ;; (process-buffer (process-buffer "url-get-command"))
      ;; "url-get-command"
      ;; :WAS (delete-process nil))
      ;; (delete-process (process-buffer "url-get-command"))
      ;;
      ;; Make _sure_ we closed the process.
      (when (get-buffer *google-define-get-buffer*)
        (kill-buffer *google-define-get-buffer*))
      ) ;; :CLOSE unwind-protect
    (setq gdpb-gthr-bffr 
          (with-current-buffer gdpb-gthr-bffr 
            (google-define-font-lock search-word)
            ;; keep properties!
            (buffer-substring (mon-g2be -1 t) (mon-g2be 1 t)) ))
    ;; Put name of `gdpb-gthr-bffr' on the `gdpb-cnt' var and kill that buffer too. 
    (setq gdpb-cnt (current-buffer))
    (when (get-buffer gdpb-cnt)
      (kill-buffer gdpb-cnt))
    gdpb-gthr-bffr))

;;; ==============================
;;; :PREFIX "gdfl-"
;;; :REQUIRES `mon-g2be' <- :FILE mon-utils.el
;;; :TODO The word definition fontlocking should also find the prefix of word 
;;; e.g. affixed -> "ed\\_w" "ing\\_w" "ing\\_w" ".tion\\_w" "edness\\_w"
;;;                 "ity\\_w" "ify\\_w" "ies\\_w"
;;; :CREATED <Timestamp: #{2010-02-04T21:33:14-05:00Z}#{10055} - by MON>
(defun google-define-font-lock (search-word)
  "Add fontification text-properties to the definitions.\n
Fontify with the following faces:\n
 `gg-def-base', `gg-def-num', `gg-def-delim',
 `gg-def-inition', `gg-def-defined'\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-kill-def-buffers',
`*regexp-google-defined-fontlock*'.\n►►►"
  (let* ((gdfl-help-props
          `( ;; :NOTE `google-define-parse-buffer' spat strings matched by elt 0 and 3.
            ,(cdr (assq 'definition-hdr *regexp-google-defined-fontlock*))
            ,(cdr (assq 'definition-num *regexp-google-defined-fontlock*))
            ,(cdr (assq 'definition-delim *regexp-google-defined-fontlock*))
            ,(cdr (assq 'definition-line *regexp-google-defined-fontlock*))
            ,(cdr (assq 'definition-heading *regexp-google-defined-fontlock*))
            ;; :WAS (,(concat " \\(" search-word "\\)[\\[:punct:]\\[:blank:]\n]") 1 (face gg-def-defined)))          
            (,(save-match-data 
                (replace-regexp-in-string "<SEARCHED-WORD>" search-word
                                          (cadr (assq 'definition-word *regexp-google-defined-fontlock*)) t t))
             ,@(cddr (assq 'definition-word *regexp-google-defined-fontlock*)))
            )))
    (mapc #'(lambda (gdfl-L-1)
              (mon-g2be -1)
              (while (search-forward-regexp  (elt gdfl-L-1 0) nil t)
                (add-text-properties 
                 (match-beginning (elt gdfl-L-1 1)) (match-end (elt gdfl-L-1 1)) 
                 (elt gdfl-L-1 2))))
          gdfl-help-props)))


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T20:16:14-04:00Z}#{11202} - by MON KEY>
(defun %google-define-set-url-current-object (definition-buffer url-current-object-params)
  "Set `url-current-object's buffer-local-value in DEFINITION-BUFFER.\n
DEFINITION-BUFFER is a buffer holding parsed definitionn results as returned
by `google-define'.\n
URL-CURRENT-OBJECT-PARAMS is a consed pair consisting of HOST and query
parameters.\n
Return value is as if by `url-generic-parse-url'.\n
:NOTE The purpose of this function is to allow us to expand all relative 
URL query paramters we put on a text property or button in some buffer
with `url-expand-file-name' by doing:\n
 \(url-expand-file-name \"dictionary?q=parse&hl=en&sl=en&tl=en\"\)\n
Likewise, we can get at the current-buffers original query parameter string with
`url-filename', e.g.:\n
 \(url-filename url-current-object\)\n
and in turn we can then extract the individutal params with
`url-parse-query-string', e.g.:\n
 \(url-parse-query-string \(url-filename url-current-object\)\)\n
or reconstruct the whole thing with `url-recreate-url', e.g.:\n
 \(url-recreate-url url-current-object\)\n
:SEE-ALSO .\n►►►"
  (let ( ;; paranoia
        (gdsuco-bfr (get-buffer definition-buffer)))
    (and gdsuco-bfr
         (with-current-buffer gdsuco-bfr
           (setq url-current-object
                 (url-generic-parse-url 
                  (concat "http://" 
                          (car url-current-object-params) 
                          (cdr url-current-object-params))))))))

;;; ==============================
;;; :PREFIX "ggdfn-"
;;; :REQUIRES `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
;;; :CREATED <Timestamp: #{2010-02-03T13:41:17-05:00Z}#{10053} - by MON>
(defun google-define (search-word &optional intrp
                                  host-language
                                  source-language
                                  target-language
                                  xrefing-type)
  "Ask Google for the definition of a word.\n
Return and display definition in new buffer other-window.\n
Buffer name constructed by prepending car of `*google-define-buffer-suffix*'
to SEARCH-WORD and appending cdr of `*google-define-buffer-suffix*' e.g.:
\n *damned:gg-definitions*\n
When called-interactively prompt for SEARCH-WORD with \"Define: \"
If there is a word at point us it as default value for prompt.\n
Optional args SOURCE-LANGUAGE TARGET-LANGUAGE XREFING-TYPE correspond to the
keyword parameters of `google-define-make-query-url' as follows:\n
 :source-lang SOURCE-LANGUAGE\n
 :target-lang TARGET-LANGUAGE\n
 :xref-type XREFING-TYPE\n
:EXAMPLE\n\n\(google-define \"define\"\)\n
\(google-define nil t\)define\n
:SEE-ALSO `google-define-font-lock', `google-define-parse-buffer',
`google-define-kill-def-buffers', `google-define-get-command',
`mon-help-temp-docstring-display',`*google-define-view-map*' `gg-def-base',
`gg-def-num', `gg-def-delim', `gg-def-inition', `gg-def-defined'.\n►►►"
  (interactive "i\np")
  (let* ((ggdfn-sw (cond (intrp
                          (read-from-minibuffer
                           (concat ":FUNCTION `google-define' " "-- define: ")
                           (thing-at-point 'word)))
                         (search-word search-word)))
         (ggdfn-hdr (concat ":FUNCTION `google-define' " "-- definitions for: " 
                            ggdfn-sw))
         (ggdfn-dfb (concat (car *google-define-buffer-suffix*)
                            ggdfn-sw
                            (cdr *google-define-buffer-suffix*)))
         (ggdfn-params
          (cons "www.google.com"
                (google-define-make-query-url ggdfn-sw
                                              :host-lang   host-language 
                                              :source-lang source-language 
                                              :target-lang target-language 
                                              :xref-type   xrefing-type)))         
         (ggdfn-defs (google-define-parse-buffer ggdfn-sw ggdfn-dfb ggdfn-params)))
    (prog1 
        (message ggdfn-hdr)
      (mon-help-temp-docstring-display ggdfn-defs ggdfn-dfb t)
      (%google-define-set-url-current-object ggdfn-dfb ggdfn-params))))
;;
;;; :TEST-ME (google-define "define")
;;; :TEST-ME (google-define nil t)define

;;; ==============================
;;; :PREFIX "gdkdb-"
;;; :REQUIRES `mon-buffer-exists-p'       <- :FILE mon-utils.el
;;; :REQUIRES `mon-buffer-exists-so-kill' <- :FILE mon-utils.el
;;; :CREATED <Timestamp: #{2010-02-05T14:45:57-05:00Z}#{10055} - by MON>
(defun google-define-kill-def-buffers (&optional intrp)
  "Kill all google define buffers with `*google-define-buffer-suffix*'.\n
Return list of buffer names killed.\n
When called-interactively message with the buffers killed.\n
:EXAMPLE\n\n\(save-window-excursion
  \(dolist \(i '\(\"damned\" \"data\" \"delta\"\) 
           \(google-define-kill-def-buffers t\)\)
    \(google-define i\)\)\)\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `mon-buffer-exists-p',
`mon-buffer-exists-so-kill'.\n►►►"
  (let ((gdkdb-bfrs (mapcar #'(lambda (gdkdb-L-1)
                           (buffer-name (get-buffer gdkdb-L-1))) (buffer-list)))
        gdkdb-def-bfrs)
    (mapc #'(lambda (gdkdb-L-2) 
              (when (string-match-p (concat ".*" (cdr *google-define-buffer-suffix*)) gdkdb-L-2)
                (push gdkdb-L-2 gdkdb-def-bfrs)))
          gdkdb-bfrs)
    (setq gdkdb-bfrs nil)  
    (mapc #'(lambda (gdkdb-L-3) 
              (when (mon-buffer-exists-so-kill gdkdb-L-3)
                (push gdkdb-L-3 gdkdb-bfrs)))
          gdkdb-def-bfrs)
    (if intrp 
        (message (mapconcat #'(lambda (gdkdb-L-4) 
                                (format (concat ":FUNCTION `google-define-kill-def-buffers' "
                                                "-- killed buffer %s") gdkdb-L-4))
                            gdkdb-bfrs "\n"))
      gdkdb-bfrs)))
;;
;;; :TEST-ME (google-define-kill-def-buffers t) 
;;; :TEST-ME (save-window-excursion
;;;            (dolist (i '("damned" "data" "delta") 
;;;                     (google-define-kill-def-buffers t))
;;;              (google-define i)))

;;; ==============================



;;; ==============================
(defun google-define-button-action (button)
  "Call BUTTON's associated function and args.\n
Invoke `google-define-button-do-xref' using buttons google-def-fun property as
function and google-def-args property as its argument.\n
:EXAMPLE\n\n
:SEE-ALSO .\n►►►"
  (google-define-button-do-xref ;; (button-start button)
   (button-get button 'google-def-fun)
   (button-get button 'google-def-args)))

(defun google-define-button-do-xref (function args) ;; (pos function args)
  ;; There is a reference at point.  Follow it.
  "Evaluate a button's associated FUNCTION and ARGS.\n
:EXAMPLE\n\n
:CALLED-BY `google-define-button-action'\n
:SEE-ALSO .\n►►►"
  (apply function args))

(defun google-define-insert-xref-button (string type &rest args)
  "Insert a google-define button with `insert-text-button'.\n
Inserted button is part of the text instead of being a property of the buffer.\n
STRING is a label for the button.\n
TYPE is button-type from which to inherit other properties.\n
:EXAMPLE\n\n
 \(google-define-insert-xref-button \"French\" 'google-related-lang-button <ARGS>\)
:SEE-ALSO `define-button-type', `insert-button'.\n►►►"
  (insert-text-button string 'type type 'google-def-args args))


(define-button-type 'google-define-button
  'follow-link t
  'action #'google-define-button-action)

(define-button-type 'google-related-lang-button
  :supertype 'google-define-button
  'google-def-fun ;; #'browse-url
  #'google-define-related-language-xref
  'help-echo "mouse-2, RET: view this URL in a browser")

;; (defun google-define-related-phrases-xref (term url )
;; Make xref for a URL with TERM found in current "Related phrases" heading.
;; :NOTE the href has the relative URL on the next line.  This is in contrast to
;; what we see for "Related languages" which is otherwise nearly identical. :(
;; ,----
;; | <h3>Related phrases</h3>
;; | <ul class="rlt-snt">
;; | <li>
;; | <div>
;; | <b><a href="
;; |   /dictionary?q=absorbent+cotton&hl=en&sl=en&tl=en&oi=dict_re
;; | ">absorbent cotton</a></b>
;; | </div>
;; | Fluffy wadding of a kind originally made from raw cotton, used for cleansing wounds, removing cosmetics, and padding delicate objects
;; | </li>
;; | <li>
;; | <div>
;; | <b><a href="
;; |   /dictionary?q=cotton+grass&hl=en&sl=en&tl=en&oi=dict_re
;; | ">cotton grass</a></b>
;; `----
;; following is from "Related languages"
;; ,----
;; | <a href="/dictionary?q=yogurt&hl=en&sl=it&tl=en
;; | ">italiano</a>
;; `----
;;
;;; (%google-define-clean-term-for-split term)
;;
;;

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T18:54:27-04:00Z}#{11201} - by MON KEY>
(defun google-define-find-next-heading ()
  "Search forward for a heading matching `*regexp-google-define-headings*'.\n
If a match is found moves point and returns a plist with the following format:\n
 \(:string \(<MATCHED-HEADING> \(<INT> . <INT>\)\) :heading-range \(<INT> . <INT>\)\)
The property :string associates with a list containing the match properties for
string matched.
The elt at car or first property <MATCHED-HEADING> is the string matched,
e.g. if match was for:\n 
 \"<h3>Related languages</h3>\"\n
Then <MATCHED-HEADING> would be \"Related languages\"\n
The consed pair at cadr of first property are buffer indexes for the beginning
and end range of <MATCHED-HEADING>.
The property :heading-range is a consed for the range of the entire match
including the opening HTML heading tags.\n
Plists :STRING property has the form:
  car            cadr 
            caadr    cdadr
 \(\"<TERM>\" \(<INT> . <INT>\)\)\n
:EXAMPLE\n
\(let \(\(rng \(plist-get \(google-define-find-next-heading\) :string\)\)\)
  \(string= \(car rng\) \(mon-buffer-sub-no-prop \(caadr  rng\) \(cdadr rng\)\)\)\)\n
:SEE-ALSO .\n►►►" 
  (and (search-forward-regexp *regexp-google-define-headings* nil t)
       `(:string (,(match-string-no-properties 3)
                  (,(match-beginning 3) . ,(match-end 3)))
                 :heading-range (,(match-beginning 0) . ,(match-end 0)))))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T12:22:08-04:00Z}#{11202} - by MON KEY>
(defun google-define-make-heading-replacement (match-string)
  "Return a string suitable for use a replacement for MATCH-STRING.\n
MATCH-STRING should satisfy either `stringp' and have length greater
than 0, if not return \"\".\n
:EXAMPLE\n\n\(google-define-make-heading-replacement \"Related Languages\"\)\n
\(google-define-make-heading-replacement \"\"\)\n
\(google-define-make-heading-replacement nil\)\n
\(google-define-make-heading-replacement 8\)\n
\(google-define-make-heading-replacement [?a ? ?b ? ?c]\)\n
:SEE-ALSO .\n►►►"
  (or (and (or (stringp match-string)
               (not (zerop (length match-string))))
           (concat (make-string 3 32)
                   (make-string 1 58)
                   (upcase (%google-define-clean-heading-for-replace match-string))))
      ""))

;; (google-define-make-heading-replacement 
;; (replace-match (concat (make-string 3 32)
;;                        (make-string 1 58)
;;                        (upcase (subst-char-in-string 32 45 gdfh-match-cur))))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T21:57:04-04:00Z}#{11201} - by MON KEY>
(defun google-define-find-next-url-in-heading ()
"Find next URL matching regexp `*regexp-google-define-next-url-in-heading*'.\n
Matches patterns with either of these formats:\n
 ,----
 | <a href=\"/dictionary?q=yogurt&hl=en&sl=es&tl=en
 | \">español</a>
 `----\n
 ,----
 | <a class=\"lightblue\" href=\"http://www.google.com/url?q=<SOME-NON-GG-URL>\">Kirsten Gillibrand</a>
 `----\n
If a match is found move point and retrun `match-data' as a list of integers.\n
Length of match-data is variadic according to whether match was for first
pattern or second.\n
:EXAMPLE\n\n\(let \(\(md-old '\(\)\)
       \(md-new '\(\)\)\)
   \(while \(setq md-old \(google-define-find-next-url-in-heading\)\)
     \(setq md-new md-old\)\)
   md-new\)\n
<a href=\"/dictionary?q=yogurt&hl=en&sl=es&tl=en
\">español</a>\n
<a class=\"lightblue\" href=\"http://www.google.com/url?q=<SOME-NON-GG-URL>\">Kirsten Gillibrand</a>\n
:SEE-ALSO .\n►►►"
  (when (search-forward-regexp *regexp-google-define-next-url-in-heading* nil t)
    (match-data t) ))

;; (defun google-define-next-url-in-heading-match-groups ();;(next-url-md)
;;  (let* ((md (google-define-find-next-url-in-heading))
;;         (last-md (and md (car (last md))))
;;         (md-grps '()))
;;    (and md 
;; (loop 
;;  with md = '(16351 16413 16351 16413 16351 16354 16351 16354 
;;                    nil nil nil nil nil nil 16354 16413 16354 16413 16371 16398 16398 16399 16401 16409)
;;  for idx in (number-sequence 0 (/ (length md) 2))
;;  collect idx)
;;
;;    (list last-md md))
;;
;; (let ((md (google-define-find-next-url-in-heading)))
;;   (and md (princ md (current-buffer))))


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T18:31:27-04:00Z}#{11201} - by MON KEY>
(defun google-define-find-headings (match-word) ;; output-buffer (princ <RESULTS> (get-buffer output-buffer))
  (with-current-buffer (current-buffer)
    (save-excursion
      (mon-g2be -1)
      (let ((gdfh-matchedp '()))
        (while (setq gdfh-matchedp (google-define-find-next-heading))
          ;; (search-forward-regexp headings-regex  nil t)
          (let* ((gdfh-plist (plist-get gdfh-matchedp :string))
                 (gdfh-match-cur (car gdfh-plist))
                 (gdfh-match-rng (cadr gdfh-plist))
                 (gdfh-beg (car gdfh-match-rng))
                 (gdfh-end (cdr gdfh-match-rng))
                 ;;
                 (gdfh-heading
                  (google-define-make-heading-replacement gdfh-match-cur)))
            ;; 
            (cond ((string-equal gdfh-match-cur "Related languages")
                   ;; make google-define-related-language-xref's
                   ;; (let (next-beg (google-define-find-next-heading)
                   ;; (save-excursion 
                   ;;   (save-restriction 
                   ;;     (narrow-to-region gdfh-end (search-forward-regexp "</ul>"))
                   ;;     (mon-g2be -1)
                   "<DO-SOMETHING-HERE>"
                   )
                  ((string-equal gdfh-match-cur "Usage examples")
                   "<DO-SOMETHING-HERE>"
                   )
                  ((string-equal gdfh-match-cur "Web definitions")
                   "<DO-SOMETHING-HERE>"
                   )))))
      )))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T12:52:31-04:00Z}#{11202} - by MON KEY>
(defun %google-define-find-itemized-regexp-for-search-term (term)
  "Create a regexp from TERM matching a google-define itemized list section.\n
Returned regexp matches either for the following patterns:\n
 ,----
 | <li>
 | a custard-like food made from curdled milk<br>
 | </li>
 `----\n
 ,----
 | <li>
 | <TERM> a custard-like food made from curdled milk<br>
 | </li>
 `----\n
Helper function for `google-define-find-itemized'.\n
:EXAMPLE\n\n\(%google-define-find-itemized-regexp-for-search-term \"yogurt\"\)\n
:SEE-ALSO .\n►►►"
  (save-match-data 
    (replace-regexp-in-string 
     (substring *regexp-google-define-itemized* 48 56) term *regexp-google-define-itemized* t)))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-16T19:15:27-04:00Z}#{11201} - by MON KEY>
(defun google-define-find-itemized (search-term)
  "Search for an itemezed google-define HTML section.\n
Moves point when a match is found for either of the following patterns:\n
 ,----
 | <li>
 | a custard-like food made from curdled milk<br>
 | </li>
 `----\n
 ,----
 | <li>
 | <SEARCH-TERM> a custard-like food made from curdled milk<br>
 | </li>
 `----\n
Search is performed with the regexp returned from
`*google-define-itemized-template-regexp*' after substituting SEARCH-TERM into
the template with `%google-define-find-itemized-regexp-for-search-term'.\n
:EXAMPLE\n\n(while \(google-define-find-itemized \"Yogurt\"\)\)\n
<li>
Yogurt is a custard-like food made from curdled milk<br>
</li>\n
<li>
a custard-like food made from curdled milk<br>
</li>\n
:SEE-ALSO .\n►►►"
  (search-forward-regexp 
   (%google-define-find-itemized-regexp-for-search-term search-term) nil t))

(defun google-define-parse-related-languages (heading-match-data search-word output-buffer)
  (let* ((gdfpr-str-plist (plist-get heading-match-data :string))
         (gdfpr-hdng-plist (plist-get heading-match-data :heading-range))
         (gdfpr-match-str (car gdfpr-str-plist))
         (gdfpr-match-rng (cadadr gdfpr-str-plist))
         (gdfpr-beg (car gdfpr-match-rng))
         (gdfpr-end (cdr gdfpr-match-rng))
         (gdfpr-heading (google-define-make-heading-replacement gdfpr-match-str))
         (gdfh-matchedp '())
         )
    ;;
    (with-current-buffer (current-buffer)
      (save-excursion
        ;; (save-restriction 
        (while (setq gdfh-matchedp (google-define-find-next-heading))
          "<DO-SOMETHING-HERE>"
          )
        ))))

;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T16:53:00-04:00Z}#{11202} - by MON KEY>
(defun %google-define-find-next-related-lang-regexp-for-search-term (current-term)
  ;; Create a regexp with search-term matching a language xref in current "Related languages" section.
  ;;
  ;; Returned regexp matches the following patterns:
  ;; ,----
  ;; | <a href="/dictionary?q=yogurt&hl=en&sl=it&tl=en
  ;; | ">italiano</a>
  ;; `----
  ;; For use with `google-define-find-next-related-language-xref'.
  (let ((gdfnrlrfst-clean
         (%google-define-clean-term-for-split current-term)))
    ;; (setq *tt--gpu* (url-generic-parse-url "http://www.google.com/dictionary?q=absorbent+cotton&hl=en&sl=en&tl=en&oi=dict_re"))
    ;; (url-expand-file-name "dictionary?q=absorbent+cotton&hl=en&sl=en&tl=en&oi=dict_re" "http://www.google.com")
    ;; (url-expand-file-name "dictionary?q=absorbent+cotton&hl=en&sl=en&tl=en&oi=dict_re" "http://www.google.com")

    
    ;; (url-expand-file-name "dictionary?q=absorbent+cotton&hl=en&sl=en&tl=en&oi=dict_re");; "http://www.google.com")    
    ;; (url-expander-remove-relative-links "../dictionary?q=absorbent+cotton&hl=en&sl=en&tl=en&oi=dict_re")
    ;; (url-generic-parse-url "http://www.google.com/dictionary?q=absorbent+cotton&hl=en&sl=en&tl=en&oi=dict_re")
    ;;
    ;; :NOTE following is a  variant of `*regexp-google-define-source-ref*'.
    (concat 
  "\\("
  "\\([[:blank:][:cntrl:]]*\\)?"
  "\\("
  "\\(<a[[:blank:]]\\)"
   ;; \\([[:alnum:]-_]?+=\"[[:alnum:]-_]+\"[[:blank:]]+\\)?*
  "\\(" "\\(.*\\)=\"\\(.*\\)\"[[:blank:]]+" "\\)?*"
  "\\)" 
 "\\(" 
 ;; (eval-when-compile (regexp-quote "?")) 
 ;;
 ;; "href=\"" 
 ;; "\\(?http://www.google.com\\)?
 ;; "/dictionary?"
 ;; 
 ;; \( \\(.*\\)
 ;; q=\\(.*\\)
 ;; "\\(.*\\)?q=\\(.*\\)?
 ;; gdfnrlrfst-clean 
 ;;
  "\\(.*\\)"
  "\"" ">" 
  "\\([[:blank:][:cntrl:]]*\\)?"
  "\\(.*\\)?"
  "\\([[:blank:][:cntrl:]]*\\)?"
  "</a>" 
  "\\)"
 "\\([[:blank:][:cntrl:]]*\\)?"          
  "\\)"))
    )


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T16:49:25-04:00Z}#{11202} - by MON KEY>
(defun google-define-find-next-related-language-xref (current-term-regexp)
  ;; Find next URL xrefing a related languages definition.
  ;; Section found by matching CURRENT-TERM-REGEXP.
  ;; CURRENT-TERM-REGEXP is a regexp as returned by 
  ;; `%google-define-find-next-related-lang-regexp-for-search-term'
  ;; (let ((gdctfs-term (%google-define-clean-term-for-split current-term)))
  (search-forward-regexp current-term-regexp nil t))


;;; ==============================
;;; :CHANGESET 2443
;;; :CREATED <Timestamp: #{2011-05-17T12:38:20-04:00Z}#{11202} - by MON KEY>
(defun google-define-related-language-url (url)
  "Return a four property plist of URLs language values and their indexes in URL.\n
Return value has the form:\n
 \(:url <URL> 
  :host-lang   \(<INT> . \"&hl=<TWO-CHAR-LANG>\"\) 
  :source-lang \(<INT> . \"&sl=<TWO-CHAR-LANG>\"\) 
  :target-lang \(<INT> . \"&tl=<TWO-CHAR-LANG>\"\)\)\n
:EXAMPLE\n
\(google-define-related-language-url \"dictionary?q=yogurt&hl=en&sl=it&tl=en\"\)\n
:NOTE Don't forget about `url-parse-query-string'!\n
 \(url-parse-query-string \"dictionary?q=yogurt&hl=en&sl=it&tl=en\"\)\n
:SEE-ALSO `url-parse-query-string'.\n►►►"
  ;; :NOTE (url-parse-query-string "dictionary?aq=f&hl=en&sl=en&tl=en&oi=dict_re&q=absorbent+cotton")
  ;; (url-parse-query-string "dictionary?q=yogurt&hl=en&sl=it&tl=en")
  ;; (url-parse-query-string "dictionary?aq=f&hl=en&langpair=en%7Cen&q=absorbent+cotton")
  (let ((gdrlu-source (string-match-p "&sl=[A-z]\\{2,2\\}"
                                      url))
        (gdrlu-target (string-match-p "&tl=[A-z]\\{2,2\\}"
                                      url))
        (gdrlu-host (string-match-p "&hl=[A-z]\\{2,2\\}"
                                    url)))
    (and gdrlu-host
         (setq gdrlu-host (cons gdrlu-host (substring url gdrlu-host (+ gdrlu-host 6)))))
    (and gdrlu-source 
         (setq gdrlu-source (cons gdrlu-source (substring url gdrlu-source (+ gdrlu-source 6)))))
    (and gdrlu-target
         (setq gdrlu-target (cons gdrlu-target (substring url gdrlu-target (+ gdrlu-target 6)))))
    (list :url  url :host-lang gdrlu-host :source-lang gdrlu-source :target-lang gdrlu-target)))

;; url-hexify-string
(defun google-define-related-language-xref (term url source-language target-language)
  ;; Make google-define xrefs found in current "Related languages" heading
  ;; 
  ;; Related languages
  ;;     * yogurt is also a word in: italiano, español
  ;; italiano => http://www.google.com/dictionary?q=yogurt&hl=en&sl=it&tl=en
  ;; español =>  http://www.google.com/dictionary?q=yogurt&hl=en&sl=es&tl=en
  ;;
  ;; ,----
  ;; | <a href="/dictionary?q=yogurt&hl=en&sl=it&tl=en
  ;; | ">italiano</a>
  ;; `----
  ;;
  ;; ,----
  ;; | <a href="/dictionary?q=yogurt&hl=en&sl=es&tl=en
  ;; | ">español</a>
  ;; `----
  ;; `google-define-related-language-url'
  "<DO-SOMETHING-HERE>"
  )

 
;;; ==============================
;;; ==============================
;;; :TODO finish building the keymap and possibly incorporating an xref stack.
;;
;;; :NOTE `help-mode's `help-make-xrefs' already steals some keys
;;;  from `view-mode-map' using `minor-mode-overriding-map-alist' e.g.:
;;;   (set (make-local-variable 'minor-mode-overriding-map-alist)
;;;           (list (cons 'view-mode help-xref-override-view-map)))
;;; IOW No need to bother with our own heavy keymap... just piggy-back it.
;;; :CREATED <Timestamp: #{2010-02-03T16:20:02-05:00Z}#{10053} - by MON>
;;; (defvar *google-define-view-map* 
;;;   (let ((gg-vm (make-sparse-keymap)))
;;;     (set-keymap-parent gg-vm help-mode-map);;help-xref-override-view-map) ;; help-mode-map)
;;;     (define-key gg-vm "Q" 'View-kill-and-leave)
;;;     (define-key gg-vm "q" 'View-kill-and-leave)
;;;     (define-key gg-vm "c" 'View-kill-and-leave)
;;;     (define-key gg-vm "C" 'View-kill-and-leave)
;;;     (define-key gg-vm "K" 'View-kill-and-leave)
;;;     (define-key gg-vm "k" 'View-kill-and-leave)
;;;     gg-vm)
;;;   "Keybinding for `view-mode'/`help-mode' in `google-define' help buffers.\n
;;; :SEE-ALSO `*google-define-get-comman', `*google-define-html-entry-table*'.\n►►►")
;;
;;; :TEST-ME *google-define-view-map* 
;;;(progn (makunbound '*google-define-view-map*) (unintern "*google-define-view-map*" obarray) )

;;; ==============================
;;: :NOTE Really _BAD_ stuff can happen when goofing around with `view-mode's
;;; buffer/window stack esp. in conjunction w/ `view-kill-and-leave' and gang.
;;; Following are notes from my first attempt to piggyback an xref stack of
;;; definitions on top of view/help. When the stack gets corrupted it hell is
;;; wrought upon the running Emacsen. I'm not ready for the voodoo required to
;;; do this yet.  Maybe someone else is ???
;;;
;;; ==============================
;;; (view-mode-exit) (view-mode-exit (nil nil t) (quit-window)
;;; (window-buffer) 
;;; (set-keymap-parent *google-define-view-map* view-mode-map)
;;; (define-key *google-define-view-map* "q" help-mode-map
;;; (view-mode-exit `(,(selected-window) nil . 'quit-window))
;;; (view-mode-exit (list (selected-window) nil 'quit-window))
;;; ==============================
;;;
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-01T20:06:49-04:00Z}#{10135} - by MON KEY>
;;; (defun google-define-entry-setup ()
;;;   "Make help-mode more google-define friendly.
;;; Set the local keymap to `*google-define-view-map*'. 
;;; Bind 'view-kill-and-leave to various keys.
;;; Set the view-exit-action to kill the buffer.
;;; :SEE-ALSO `*google-define-buffer-suffix*'.\n►►►"
;;;   (when (string-match-p   ;; :NOTE Consider passing in the current-buffers name :)
;;;          (concat ".*" (cdr *google-define-buffer-suffix*))
;;;          (buffer-name (get-buffer (current-buffer))))
;;;     (with-current-buffer (current-buffer)    
;;;           (use-local-map *google-define-view-map*))))
;;;
;;; ==============================
;;; (add-hook 'help-mode-hook ;; 'view-mode-hook
;;; (set (make-local-variable 'view-no-disable-on-exit) t)
;;; 
;;; :NOTE Following _undoes_ the `help-mode' binding of 
;;; `view-exit-action'. This is the last form eval'd just before
;;; running the `help-mode-hook'...
;;; (set (make-local-variable 'view-exit-action)
;;;      #'(lambda ()
;;;          (with-current-buffer (current-buffer)
;;;            (kill-buffer (current-buffer))))
;;;      );;;t t)))
;;; ==============================
;;; ==============================

;;; ==============================

;;; ==============================
;;; :NOTE simplified version of `google-define-replace-html' with all functions inlined.
;;; :CREATED <Timestamp: #{2010-03-20T12:16:04-04:00Z}#{10116} - by MON KEY>
;;; (defun google-define-replace-html ()
;;;   (dolist (x *google-define-html-entry-table*)
;;;     (let ((ascii (caddr x))
;;;           (GD-RPS #'(lambda (frm-str to-str) 
;;;                       (progn 
;;;                         (mon-g2be -1) ;; mon-g2be <- mon-utils.el
;;;                         (while (search-forward frm-str nil t)
;;;                           (replace-match to-str nil t))))))
;;;       (funcall GD-RPS (car x) ascii)
;;;       (funcall GD-RPS (cadr x) ascii))))
;;; ==============================

;;; ==============================
;;; `google-define-replace-html' as lambda form:
;;; (let ((gd-nam ;; :WAS `google-define-name-entry'
;;;        #'(lambda (nam-entry) (cadr nam-entry)))
;;;       (gd-num ;; :WAS `google-define-number-entry'
;;;        #'(lambda (num-entry) (car num-entry)))
;;;       (gd-asc ;; :WAS google-define-ascii-entry
;;;        #'(lambda (asc-entry) (caddr asc-entry))))
;;;
;;;            (google-define-ascii-entry x)))
;;;            (funcall asc-entry x)))
;;;
;;;       (google-define-replace-string (google-define-number-entry x) ascii)
;;;       (google-define-replace-string (funcall gd-num x) ascii)
;;;       (google-define-replace-string (car x) ascii)
;;;
;;;       (google-define-replace-string (google-define-name-entry x) ascii)
;;;       (google-define-replace-string (funcall gd-nam x) ascii)
;;;       (google-define-replace-string (cadr x) ascii)
;;; ==============================

 
;;; ==============================
;;; :COURTESY Jeremy English :HIS google-define.el 
;;; :NOTE When `IS-MON-SYSTEM-P' this constant _should_ be bound in:
;;; :FILE mon-regexp-symbols.el It is provided here for completentess.
;;; :MODIFICATIONS <Timestamp: #{2010-03-20T12:34:07-04:00Z}#{10116} - by MON KEY>
;;;  Replaced Character literal for SOFT HYPHEN (173, #o255, #xad) with hex representation.
;;; :CREATED <Timestamp: #{2010-03-20T12:15:24-04:00Z}#{10116} - by MON KEY>
(unless (and (intern-soft "*google-define-html-entry-table*" obarray)
             (bound-and-true-p *google-define-html-entry-table*))
(defconst *google-define-html-entry-table*
  `(("&#34;"  "&quot;" "\"")  ("&#38;"  "&amp;" "&")    ("&#39;" "&yow;" "'")
    ("&#62;"  "&gt;" ">")     ("&#161;" "&iexcl;" "¡")
    ("&#162;" "&cent;" "¢")   ("&#163;" "&pound;" "£")  ("&#164;" "&curren;" "¤")
    ("&#165;" "&yen;" "¥")    ("&#166;" "&brvbar;" "¦") ("&#167;" "&sect;" "§")
    ("&#168;" "&uml;" "¨")    ("&#169;" "&copy;" "©")   ("&#170;" "&ordf;" "ª")
    ("&#171;" "&laquo;" "«")  ("&#172;" "&not;" "¬")    ("&#173;" "&shy;" "\xad") ;<- :CHANGED
    ("&#174;" "&reg;" "®")    ("&#175;" "&macr;" "¯")   ("&#176;" "&deg;" "°")
    ("&#177;" "&plusmn;" "±") ("&#178;" "&sup2;" "²")   ("&#179;" "&sup3;" "³")
    ("&#180;" "&acute;" "´")  ("&#181;" "&micro;" "µ")  ("&#182;" "&para;" "¶")
    ("&#183;" "&middot;" "·") ("&#184;" "&cedil;" "¸")  ("&#185;" "&sup1;" "¹")
    ("&#186;" "&ordm;" "º")   ("&#187;" "&raquo;" "»")  ("&#188;" "&frac14;" "¼")
    ("&#189;" "&frac12;" "½") ("&#190;" "&frac34;" "¾") ("&#191;" "&iquest;" "¿")
    ("&#192;" "&Agrave;" "À") ("&#193;" "&Aacute;" "Á") ("&#194;" "&Acirc;" "Â")
    ("&#195;" "&Atilde;" "Ã") ("&#196;" "&Auml;" "Ä")   ("&#197;" "&Aring;" "Å")
    ("&#198;" "&AElig;" "Æ")  ("&#199;" "&Ccedil;" "Ç") ("&#200;" "&Egrave;" "È")
    ("&#201;" "&Eacute;" "É") ("&#202;" "&Ecirc;" "Ê")  ("&#203;" "&Euml;" "Ë")
    ("&#204;" "&Igrave;" "Ì") ("&#205;" "&Iacute;" "Í") ("&#206;" "&Icirc;" "Î")
    ("&#207;" "&Iuml;" "Ï")   ("&#208;" "&ETH;" "Ð")    ("&#209;" "&Ntilde;" "Ñ")
    ("&#210;" "&Ograve;" "Ò") ("&#211;" "&Oacute;" "Ó") ("&#212;" "&Ocirc;" "Ô")
    ("&#213;" "&Otilde;" "Õ") ("&#214;" "&Ouml;" "Ö")   ("&#215;" "&times;" "×")
    ("&#216;" "&Oslash;" "Ø") ("&#217;" "&Ugrave;" "Ù") ("&#218;" "&Uacute;" "Ú")
    ("&#219;" "&Ucirc;" "Û")  ("&#220;" "&Uuml;" "Ü")   ("&#221;" "&Yacute;" "Ý")
    ("&#222;" "&THORN;" "Þ")  ("&#223;" "&szlig;" "ß")  ("&#224;" "&agrave;" "à")
    ("&#225;" "&aacute;" "á") ("&#226;" "&acirc;" "â")  ("&#227;" "&atilde;" "ã")
    ("&#228;" "&auml;" "ä")   ("&#229;" "&aring;" "å")  ("&#230;" "&aelig;" "æ")
    ("&#231;" "&ccedil;" "ç") ("&#232;" "&egrave;" "è") ("&#233;" "&eacute;" "é")
    ("&#234;" "&ecirc;" "ê")  ("&#235;" "&euml;" "ë")   ("&#236;" "&igrave;" "ì")
    ("&#237;" "&iacute;" "í") ("&#238;" "&icirc;" "î")  ("&#239;" "&iuml;" "ï")
    ("&#240;" "&eth;" "ð")    ("&#241;" "&ntilde;" "ñ") ("&#242;" "&ograve;" "ò")
    ("&#243;" "&oacute;" "ó") ("&#244;" "&ocirc;" "ô")  ("&#245;" "&otilde;" "õ")
    ("&#246;" "&ouml;" "ö")   ("&#247;" "&divide;" "÷") ("&#248;" "&oslash;" "ø")
    ("&#249;" "&ugrave;" "ù") ("&#250;" "&uacute;" "ú") ("&#251;" "&ucirc;" "û")
    ("&#252;" "&uuml;" "ü")   ("&#253;" "&yacute;" "ý") ("&#254;" "&thorn;" "þ")
    ("&#255;" "&yuml;" "ÿ")   ("&#60;" "&lt;" "<")
    ;; ("&#160;" "&nbsp;" "\xa0")
    ("&#160;" "&nbsp;" " "))
  "*When `IS-MON-SYSTEM-P' *google-define-html-entry-table* _should_ be in:
:FILE mon-regexp-symbols.el It is provided here for completeness.\n\n
A list of triples mapping HTML character refrences to text characters.\n
elt0 of triple is an HTML numeric decimal char ref of the form: \"&#<NNNN>\"\n
elt1 of triple is an HTML4 DTD named char entity of the form: \"&<CHAR-NAME>\"\n
elt2 of triple is an unescaped character literal.\n
:EXAMPLE\n\n`\(:entity \":&#160;\" 
  :equivalences
  \(,\(when \(equal 
           \(cadr \(assoc-string \"&#160;\" *google-define-html-entry-table*\)\) \"&nbsp;\"\)
      '\(\"&nbsp;\" . t\)\)
   ,\(when \(eq \(string-to-char 
               \(caddr \(assoc-string \"&#160;\" *google-define-html-entry-table*\)\)\) 32\) 
      '\(\" \" . t\)\)
   ,\(unless \(eq \(string-to-char 
                 \(caddr \(assoc-string \"&#160;\" *google-define-html-entry-table*\)\)\) 160\)
      \(list \(char-to-string #xa0\) nil\)\)\)\)\n
:NOTE The entities \"&#160;\" \"&nbsp;\" are NO-BREAK SPACE
e.g. return value of: \"\\xa0\" -> code point: 0xA0 character: \(160, #o240, #xa0\)
It is not clear that we want this char to appear in return values as these would
display with the face `nobreak-space' \(describe-face 'nobreak-space\).
This type of display may not be what is expected/wanted so we punt and use a
vanilla \" \" (char 32) instead.\n
:CALLED-BY `*regexp-clean-html-decimal-char-entity*', `*regexp-clean-html-named-char-entity*'\n
:SEE (URL `http://en.wikipedia.org/wiki/HTML_encoding')\n
:SEE (URL `http://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references')\n
:SEE (URL `http://www.w3.org/TR/xhtml1/DTD/xhtml-lat1.ent')\n
:SEE (URL `http://www.w3.org/TR/xhtml1/DTD/xhtml-symbol.ent')\n
:SEE (URL `http://www.w3.org/TR/xhtml1/DTD/xhtml-special.ent')\n
:SEE-ALSO `*regexp-wrap-url-schemes*', `*regexp-clean-xml-parse*',
`*regexp-percent-encoding-reserved-chars*', `*regexp-clean-ulan-diacritics*',
`*regexp-cp1252-to-latin1*'.\n►►►")
) ;; :CLOSE unless

 
;;; ==============================
;;; :COURTESY Jeremy English :HIS google-define.el 
;;; (defun google-define-number-entry (entry)
;;;   (car entry))
;;; 
;;; (defun google-define-name-entry (entry)
;;;   (cadr entry))
;;; 
;;; (defun google-define-ascii-entry (entry)
;;;   (cadr (cdr entry)))
;;; 
;;; (defun google-define-replace-string (from-string to-string)
;;;   (goto-char (point-min))
;;;   (while (search-forward from-string nil t)
;;;     (replace-match to-string nil t)))
;;; 
;;; (defun google-define-replace-html ()
;;;   (dolist (x *google-define-html-entry-table*)
;;;     (let ((ascii (google-define-ascii-entry x)))
;;;       (google-define-replace-string
;;;        (google-define-number-entry x) ascii)
;;;       (google-define-replace-string
;;;        (google-define-name-entry x) ascii))))
;;;
;;; (defun google-define-replace-unicode ()
;;;   (goto-char (point-min))
;;;   (while (search-forward-regexp "&#\\([0-9]+\\);" nil t)
;;;     (let* ((ucs (string-to-number (match-string 1)))
;;;            (rep (char-to-string (or (decode-char 'ucs ucs) ?~))))
;;;     (replace-match rep nil t))))
;;;
;;; (defun google-define-word-at-point ()
;;;  (let ((word-at-point (thing-at-point 'word)))
;;;    (set-text-properties 0 (length word-at-point) nil word-at-point)
;;;    word-at-point))
;;; ==============================
;;; ==============================

;;; ==============================
(provide 'google-define-redux)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ==============================
;;; google-define-redux.el ends here
;;; EOF

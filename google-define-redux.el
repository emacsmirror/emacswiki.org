;;; google-define-redux.el --- extends google-define.el 
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright (c) 2007,2008,2009,2010 Jeremy English <jhe@jeremyenglish.org>
;; Copyright © 2010 MON KEY. All rights reserved.
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
;; `gg-def-inition',
;;
;; CONSTANTS:
;; `*google-define-html-entry-table*',
;;
;; VARIABLES:
;; `*google-define-view-map*', `*google-define-get-buffer*',
;; `*google-define-buffer-suffix*', `*get-google-defined*',
;;  `*regexp-google-defined-fontlock*',
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
;; URL: http://www.emacswiki.org/emacs/google-define-redux.el
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

(require 'font-lock)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-20T11:44:49-04:00Z}#{10116} - by MON KEY>
(defgroup google-define-redux nil
  "*Extensions for `google-define'.\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers', `mon-string-justify-left'
`*google-define-buffer-suffix*' `*google-define-get-buffer*',
`*google-define-html-entry-table*', `gg-def-base', `gg-def-num', `gg-def-delim',
`gg-def-inition', `gg-def-defined'.\n►►►"
  :group 'mon-base  
  :group 'google-define
  :prefix "google-define-"
  :link '(url-link :tag ":EMACSWIKI-LINK" "http://www.emacswiki.org/emacs/google-define-redux.el")
  :link '(emacs-library-link "google-define-redux.el"))
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
;;; :COURTESY Jeremy English :HIS google-define.el 
;;; :NOTE When `IS-MON-SYSTEM-P' this constant _should_ be bound in:
;;; :FILE mon-regexp-symbols.el It is provided here for completentess.
;;; :MODIFICATIONS <Timestamp: #{2010-03-20T12:34:07-04:00Z}#{10116} - by MON KEY>
;;;  Replaced Character literal for SOFT HYPHEN (173, #o255, #xad) with hex representation.
;;; :CREATED <Timestamp: #{2010-03-20T12:15:24-04:00Z}#{10116} - by MON KEY>
(unless (and (intern-soft "*google-define-html-entry-table*")
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
;;; :GOOGLE-DEFINE-ADDITIONS-EXTENSIONS-MODIFICATIONS

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
;;; :CREATED <Timestamp: #{2010-02-03T13:43:28-05:00Z}#{10053} - by MON>
(defvar *google-define-get-buffer* nil 
  "A buffer `google-define-get-command' should return results in.\n
Default is \"*GOOGLE-DEFINE-GET-BUFFER*\".\n
Dynamically allocated as needed.\n
:SEE-ALSO `*google-define-buffer-suffix*'.\n►►►")
;;
(unless (and (intern-soft "*google-define-get-buffer*")
             (bound-and-true-p *google-define-get-buffer*))
  (setq *google-define-get-buffer* (symbol-name '*google-define-get-buffer*)))
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
  '((definition-hdr-w/o "Definitions for: %s\n\n" 7 (face  default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-hdr    "^Definitions for:"        0 (face gg-def-base))
    (definition-num    "^ +\\([0-9]\\{1,2\\}\\) " 1 (face gg-def-num))
    (definition-delim  " \\(►\\||\\|◄\\) ?"       1 (face gg-def-delim))
    (definition-delim-top  "►"                    7 (face default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-delim-btm  "◄"                    7 (face default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-line-w/o  "    | "                7 (face default)) ;; :NOTE <INT> and <FACE> unused.
    (definition-line   "^    | \\(.*\\)$"         1 (face gg-def-inition))
    ;; :NOTE (concat " \\(" <SEARCHED-WORD>  "\\)[\\[:punct:]\\[:blank:]\n]")
    (definition-word    " \\(<SEARCHED-WORD>\\)[\\[:punct:]\\[:blank:]\n]" 1 (face gg-def-defined)))
  "A list of font lock rules for `google-define-font-lock'.\n
Elts of list have the form:\n
 \(<KEY> <REGEXP> <MATCH-GRP> \(face <FACE>\)\)\n
The car last element in list (e.g. `face`)l is a constant and must be present.\n
Required values of <KEY> are:\n
 definition-hdr  definition-hdr-w/o
 definition-num  definition-delim  
 definition-line definition-word\n
<REGEXP> and <MATCH-GRP> should match according to fontlocks for <FACE>.\n
The regexp of key definition-word must contain the token \"<SEARCHED-WORD>\"
this is used as a marker to split on b/c `defcustom' doesn't provide a clean way
to conditionally pass/specify a cons instead of a regexp.\n
:NOTE The regexp for key `definition-hdr-w/o` associates a format string spec for use by
`google-define-parse-buffer' and should be reflected by the regexp in key
`definition-hdr`.\n
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
         proc buf)
    (progn
      (when (get-buffer *google-define-get-buffer*)
        (kill-buffer *google-define-get-buffer*))
      ;; :WAS (setq proc (open-network-stream
      ;;                  "url-get-command" *google-define-get-buffer* host port))
      (setq proc (make-network-process :name *get-google-defined* 
                                       :buffer *google-define-get-buffer*
                                       :host host
                                       :service port))
      (setq buf (process-buffer proc))
      (process-send-string proc post-cmd)
      (message (concat ":FUNCTION `google-define-get-command' "
                       " -- Snarfing %s -- waiting for response...") host)
      (while (equal (process-status proc) 'open)
        (unless (accept-process-output proc timeout)
          (unwind-protect
              (error (concat ":FUNCTION `google-define-get-command' "
                         "-- host %s timed out, deleting network process %S")
                         host proc)
            (delete-process proc))
          (message (concat ":FUNCTION `google-define-get-command' "
                         " -- response received: processing..."))))
    buf)))
;;
;;; :TEST-ME (google-define-get-command-TEST)

;;; ==============================
;;; :PREFIX "gdpb-"
;;; :REQUIRES `mon-g2be', `mon-string-justify-left' <- mon-utils.el
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T13:41:22-05:00Z}#{10053} - by MON>
(defun google-define-parse-buffer (search-word def-disp-buffer)
  "Pull all of the definitions out of the data returned from google.\n
Print in a temp-buffer, parse, and convert for presentation.\n
:SEE-ALSO `google-define', `google-define-get-command',
`google-define-font-lock', `google-define-kill-def-buffers',
`mon-string-justify-left' `*google-define-buffer-suffix*'
`*google-define-get-buffer*', `*google-define-html-entry-table*'.\n►►►"
  (let* ((gdpb-cnt 0)
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
    (set-buffer 
     (google-define-get-command                 
      "www.google.com" 
      (concat 
       ;; :NOTE "http://www.google.com/search?num=100&hl=en&q=define%3A%22big%22&btnG=Search"
       ;;  is equivalent to entering at th gg search form: define:"big".
       "/search?num=100&hl=en&q=define%3A%22"
       (replace-regexp-in-string " +" "+" search-word)
       "%22&btnG=Search")))
    (unwind-protect
        (progn
          (mon-g2be)
          (while (search-forward-regexp "<li>\\([^<]+\\)" nil t)
            (incf gdpb-cnt)
            (let ((gdpb-wrd-def 
                    (replace-regexp-in-string "\\(\n\\|\r\\|\t\\)" "" (match-string 1))))
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
                                           (mon-g2be)
                                           (while (search-forward frm-str nil t)
                                             (replace-match to-str nil t))))))
                                (funcall GD-RPS (car gdpb-DL-1) gdpb-ascii)
                                (funcall GD-RPS (cadr gdpb-DL-1) gdpb-ascii)))))
                       (GDRU ;; :WAS `google-define-replace-unicode'
                        #'(lambda ()
                            (mon-g2be)
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
                                (mon-string-justify-left gdpb-wrd-def 64 4)))
                       (set-marker gdpb-lt-pnt (point))
                       (insert "\n" gdpb-wspc ;; :WAS "\n    " 
                               (cadr (assq 'definition-delim-btm *regexp-google-defined-fontlock*))
                               "\n\n")))
                   (fill-region-as-paragraph gdpb-gt-pnt gdpb-lt-pnt)
                   (funcall GDRH)
                   (funcall GDRU))
                 (buffer-substring (buffer-end 0) (buffer-end 1)))
               (get-buffer gdpb-gthr-bffr))))
          (set-buffer gdpb-gthr-bffr))    ; :CLOSE progn
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
            (buffer-substring (buffer-end 0) (buffer-end 1))))
    ;; Put name of `gdpb-gthr-bffr' on the `gdpb-cnt' var and kill that buffer too. 
    (setq gdpb-cnt (current-buffer))
    (when (get-buffer gdpb-cnt) (kill-buffer gdpb-cnt))
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
`google-define-parse-buffer', `google-define-font-lock',
`google-define-kill-def-buffers'.\n►►►"
  (let* ((gdfl-help-props
          `(;; :NOTE `google-define-parse-buffer' spat stings matched by elt 0 and 3.
            ,(cdr (assq 'definition-hdr *regexp-google-defined-fontlock*))
            ,(cdr (assq 'definition-num *regexp-google-defined-fontlock*))
            ,(cdr (assq 'definition-delim *regexp-google-defined-fontlock*))
            ,(cdr (assq 'definition-line *regexp-google-defined-fontlock*))
            ;; :WAS (,(concat " \\(" search-word "\\)[\\[:punct:]\\[:blank:]\n]") 1 (face gg-def-defined)))          
            (,(save-match-data 
                 (replace-regexp-in-string "<SEARCHED-WORD>" search-word
                                           (cadr (assq 'definition-word *regexp-google-defined-fontlock*)) t t))
              ,@(cddr (assq 'definition-word *regexp-google-defined-fontlock*))
              ))))
    (mapc #'(lambda (gdfl-L-1)
              (mon-g2be)
              (while (search-forward-regexp  (elt gdfl-L-1 0) nil t)
                (add-text-properties 
                 (match-beginning (elt gdfl-L-1 1)) (match-end (elt gdfl-L-1 1)) 
                 (elt gdfl-L-1 2))))
          gdfl-help-props)))

;;; ==============================
;;; :PREFIX "ggdfn-"
;;; :REQUIRES `mon-help-temp-docstring-display' <- :FILE mon-doc-help-utils.el
;;; :CREATED <Timestamp: #{2010-02-03T13:41:17-05:00Z}#{10053} - by MON>
(defun google-define (search-word &optional intrp)
  "Ask Google for the definition of a word.\n
Return and display definition in new buffer other-window.\n
Buffer name constructed by prepending car of `*google-define-buffer-suffix*'
to SEARCH-WORD and appending cdr of `*google-define-buffer-suffix*' e.g.:
\n *damned:gg-definitions*\n
When called-interactively prompt for SEARCH-WORD with \"Define: \"
If there is a word at point us it as default value for prompt.\n
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
         (ggdfn-hdr ;; :WAS (concat "Definitions for " ggdfn-sw))
          (concat ":FUNCTION `google-define' " "-- definitions for: " ggdfn-sw))
         (ggdfn-dfb (concat (car *google-define-buffer-suffix*)
                      ggdfn-sw
                      (cdr *google-define-buffer-suffix*)))
         (ggdfn-defs (google-define-parse-buffer ggdfn-sw ggdfn-dfb)))
    (prog1 
        (message ggdfn-hdr)
      ;;  (progn
      ;;  (mon-help-temp-docstring-display ggdfn-defs ggdfn-dfb t))))
      ;; (with-current-buffer (current-buffer)
      ;; (google-define-entry-setup))))))
      (mon-help-temp-docstring-display ggdfn-defs ggdfn-dfb t))))
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
;;;                         (mon-g2be) ;; mon-g2be <- mon-utils.el
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
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ================================================================
;;; google-define-redux.el ends here
;;; EOF

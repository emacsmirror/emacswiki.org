;;; mon-doc-help-utils.el --- documentation enabling and generation extensions
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-doc-help-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-06-17T11:29:15-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: docs, matching, lisp

;;; ================================================================

;;; COMMENTARY: 

;; ================================================================
;; DESCRIPTION:
;; mon-doc-help-utils consolidates functions that offer documentation and or
;; enable generation of documentation. Generally these are for assistance
;; generating Elisp as opposed to programmatic use in Elisp functions and
;; programs.
;;
;; Additional utilities are provided to help with modification of Aaron
;; Hawley's Reference card page on the EmacsWiki.
;; :SEE (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley')
;; :SEE (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;
;; :MOTIVATIONS :FROM :FILE /etc/TODO:
;; ,----
;; | ** Have a command suggestion help system that recognizes patterns
;; |    of commands which could be replaced with a simpler common command.
;; |    It should not make more than one suggestion per 10 minutes.
;; `----
;;
;; FUNCTIONS:►►►
;; `mon-help-insert-documentation', `mon-insert-doc-help-tail',
;; `mon-insert-doc-help-cookie', `mon-help-function-spit-doc',
;; `mon-help-message-intrp',
;; `mon-help-function-args', `mon-help-xref-symbol-value',
;; `mon-help-function-arity', `mon-help-parse-interactive-spec',
;; `mon-help-view-file', `mon-help-temp-docstring-display',
;; `mon-help-get-mon-help-buffer', `mon-help-overlay-result',
;; `mon-help-find-result-for-overlay', `mon-help-overlay-on-region',
;; `mon-help-propertize-tags', 
;; `mon-help-mon-tags', `mon-help-insert-tags', `mon-tags-apropos',
;; `mon-tags-naf-apropos', `mon-help-regexp-syntax', `mon-help-syntax-class',
;; `mon-help-search-functions', `mon-help-type-predicates',
;; `mon-help-plist-functions', `mon-help-plist-properties',
;; `mon-help-text-property-functions', `mon-help-text-property-stickyness',
;; `mon-help-text-property-functions-ext', `mon-help-text-property-properties',
;; `mon-help-frame-functions', `mon-help-window-functions',
;; `mon-help-easy-menu', `mon-help-widgets', `mon-help-buffer-functions',
;; `mon-help-faces-basic', `mon-help-faces-themes', `mon-help-faces',
;; `mon-help-faces-font-lock', `mon-help-font-lock',
;; `mon-help-font-lock-functions', `mon-help-file-dir-functions-usage',
;; `mon-help-read-functions', `mon-help-print-functions',
;; `mon-help-process-functions', `mon-help-server-functions',
;; `mon-help-xml-functions', `mon-help-color-functions', `mon-help-color-chart',
;; `mon-help-char-representation', `mon-help-char-iso-8859-1',
;; `mon-help-char-ascii', `mon-help-char-ecma-35', `mon-help-char-ecma-48',
;; `mon-help-format-width', `mon-help-package-keywords',
;; `mon-index-elisp-symbol', `mon-help-mon-help', `mon-help-emacs-introspect',
;; `mon-help-crontab', `mon-help-permissions', `mon-help-ipv4-header',
;; `mon-help-unix-commands', `mon-help-w32-env', `mon-help-eieio-defclass',
;; `mon-help-eieio-functions', `mon-help-eieio-methods',
;; `mon-help-nclose-functions', `mon-help-iso-8601',
;; `mon-help-info-incantation', `mon-help-install-info-incantation',
;; `mon-help-tar-incantation', `mon-help-rename-incantation',
;; `mon-help-du-incantation' ,`mon-help-hg-archive', `mon-help-diacritics',
;; `mon-help-keys-wikify', `mon-help-escape-for-ewiki',
;; `mon-help-unescape-for-ewiki', `mon-help-overlay-functions',
;; `mon-help-overlay-for-example', `mon-help-delimited-region',
;; `mon-help-syntax-functions', `mon-help-utils-loadtime',
;; `mon-help-char-functions', `mon-help-key-functions',
;; `mon-help-custom-keywords', `mon-help-sequence-functions',
;; `mon-help-keys-wikify-anchors', `mon-help-keys-wikify-heading',
;; `mon-get-next-face-property-change',
;; `mon-get-next-face-property-change-if', `mon-get-all-face-property-change',
;; `mon-get-text-properties-print', `mon-get-text-properties-read-temp',
;; `mon-get-text-properties-region', `mon-get-text-properties-elisp-string',
;; `mon-get-text-properties-elisp-string-pp',
;; `mon-get-text-properties-parse-prop-val-type-chk',
;; `mon-get-text-properties-parse-buffer', `mon-get-text-properties-parse-sym',
;; `mon-get-text-properties-parse-buffer-or-sym',
;; `mon-get-text-properties-map-ranges', `mon-update-tags-tables',
;; `mon-help-load-functions', `mon-help-w32-functions',
;; `mon-help-make-network-process', `mon-help-binary-representation',
;; `mon-help-char-raw-bytes', `mon-help-char-coding-functions',
;; `mon-help-char-composition', `mon-help-time-functions',
;; `mon-help-mon-time-functions', `mon-help-char-unidata-table',
;; `mon-help-errors', `mon-help-buffer-spc-*DOC*', `mon-help-mode-functions',
;; `mon-help-permanent-locals-find', `mon-help-symbol-functions',
;; `mon-help-byte-optimizer-find', `mon-help-byte-code-vector-symbols',
;; `mon-help-bookmark-functions', `mon-help-help-functions',
;; `mon-help-inhibit-functions', `mon-help-char-logic',
;; `mon-help-char-charset-functions', `mon-help-char-table-functions',
;; `mon-help-display-table-functions', `mon-help-bind-help-keys-loadtime',
;; `mon-help-byte-compile-functions', `mon-update-tags-tables-loadtime',
;; `mon-help-marker-functions', `mon-help-predicate-functions', 
;; `mon-help-number-functions',
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;; `mon-help-swap-var-doc-const-val', `mon-help-put-var-doc-val->func',
;; 
;; METHODS:
;;
;; CLASSES:
;;
;; FACES:
;; `mon-help-KEY-tag', `mon-help-DYNATAB-tag', `mon-help-META-tag',
;; `mon-help-PNTR-tag', `mon-help-INNER-KEY-tag', `mon-help-COMMENT-tag',
;; `mon-help-URL-wrap-tag', `mon-help-BUILTIN-tag', `mon-help-OLAY-RESULT',
;; `mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show',
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-help-mon-tags-alist*', `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
;; `*regexp-mon-doc-help-docstring-tags-TABLES*',
;; `*regexp-mon-doc-help-docstring-tags*', `*regexp-mon-doc-help-meta-tags*',
;; `*regexp-mon-doc-help-comment-tags*', `*regexp-mon-doc-help-pointer-tags*',
;; `*regexp-mon-doc-help-docstring-tags-URL*', `*regexp-symbol-defs*',
;; `*mon-help-propertize-tags-triples*',
;; `*mon-help-interactive-spec-alist*', `*mon-doc-cookie*',
;; `*mon-help-docstring-help-bffr*', `*regexp-clean-du-flags*',
;; `*mon-help-reference-keys*', `*w32-env-variables-alist',
;; `*regexp-mon-doc-help-builtin-dynamic-tags*',
;; `*mon-help-custom-faces-builtins-tags*',
;; `*regexp-mon-doc-help-builtin-static-tags*', `*mon-help-reference-keywords*',
;; `*mon-tags-table-list*', `*mon-help-side-effect-free*',
;; `*mon-help-side-effect-and-error-free*', `*mon-help-pure-functions*',
;; `*mon-help-emacs-errors*', `*mon-help-subrs*', `*mon-help-autoload-vars*',
;; `*mon-help-permanent-locals*', `*mon-help-byte-optimizer-vals*',
;; `*mon-help-risky-local-variables*',
;; :GROUPS
;; `mon-doc-help-utils', `mon-doc-help-utils-faces',
;;
;; ALIASED:
;; `mon-insert-documentation'                -> `mon-help-insert-documentation'
;; `mon-help-reference-sheet'                -> `mon-help-mon-help'
;; `*mon-help-keys-reference*'               -> `*mon-help-reference-keys*'
;; `mon-help-dir-file-function-usage'        -> `mon-help-file-dir-functions-usage'
;; `mon-line-strings-region-delimited'       -> `mon-help-delimited-region'
;; `mon-line-strings-get-delimited-region'   -> `mon-help-delimited-region'
;; `mon-help-face-next-property-change'      -> `mon-get-next-face-property-change'
;; `mon-help-directory-file-functions-usage' -> `mon-help-file-dir-functions-usage'
;; `mon-help-finder-keywords'                -> `mon-help-package-keywords'
;; `mon-help-ecma-48-chars-cntl->hex'        -> `mon-help-char-ecma-48'
;; `mon-help-ecma-48-chars-cntl->hex'        -> `mon-help-char-ecma-48'
;; `mon-help-ascii-chars'                    -> `mon-help-char-ascii' 
;; `mon-help-iso-8859-1-chars'               -> `mon-help-char-iso-8859-1'
;; `mon-help-time-iso-8601'                  -> `mon-help-iso-8601'
;; `mon-help-face-functions'                 -> `mon-help-faces'
;; `mon-help-bitwise-functions'              -> `mon-help-binary-representation'
;; `mon-help-charset-coding-functions'       -> `mon-help-char-coding-functions'
;; `*doc-cookie*'                            -> `*mon-doc-cookie*'
;; `mon-help-network-process'                -> `mon-help-make-network-process'
;; `mon-help-types'                          -> `mon-help-type-predicates'
;; `mon-function-args'                       -> `mon-help-function-args'
;; `mon-function-arity'                      -> `mon-help-function-arity'
;;
;; RENAMED:
;; `*emacs-reference-sheet-A-HAWLEY*'        -> `*mon-help-reference-keys*'
;; `emacs-wiki-fy-reference-sheet'           -> `mon-help-keys-wikify'
;; `emacs-wiki-escape-lisp-string-region'    -> `mon-help-escape-for-ewiki'
;; `emacs-wiki-unescape-lisp-string-region'  -> `mon-help-unescape-for-ewiki'
;; `mon-help-make-faces'                     -> `mon-help-faces'
;; `mon-help-basic-faces'                    -> `mon-help-faces-basic'
;; `mon-help-file-dir-functions'             -> `mon-help-file-dir-functions-usage'
;; `mon-help-ASCII-chars'                    -> `mon-help-char-ascii'
;; `mon-help-ISO-8859-1-chars'               -> `mon-help-char-iso-8859-1'
;; `mon-help-cntl->hex->ecma-48'             -> `mon-help-char-ecma-48'
;; `mon-help-cntl->hex->ecma-35'             -> `mon-help-char-ecma-35' 
;; `mon-help-print'                          -> `mon-help-print-functions'
;; `*doc-cookie*'                            -> `*mon-doc-cookie*'
;;
;; MOVED:
;; `mon-help-CL-time', `mon-help-CL-loop', `mon-help-CL-slime-keys' -> mon-doc-help-CL.el
;;
;; `mon-index-elisp-symbol'                  <- mon-utils.el
;;
;; `mon-help-pacman-Q', `mon-help-pacman-S' `mon-help-pacman-commands',
;; `*regexp-clean-pacman-Q*'`*regexp-clean-pacman-S*' -> mon-doc-help-pacman.el
;;
;; `*mon-tags-table-list*'                   <- mon-default-start-loads.el
;; `mon-update-tags-tables'                  <- mon-default-start-loads.el
;;
;; `mon-help-propertize-regexp-symbol-defs-TEST' -> mon-testme-utils.el
;; `mon-help-regexp-symbol-defs-TEST'            -> mon-testme-utils.el
;; `mon-help-propertize-tags-TEST'               -> mon-testme-utils.el
;; `mon-help-keys-wikify-TEST'                   -> mon-testme-utils.el
;; 
;; Following moved from `mon-insertion-utils.el' and :RENAMED *insert* -> *help*
;; `mon-insert-file-dir-functions'           -> `mon-help-file-dir-functions-usage'
;; `mon-insert-install-info-incantation'     -> `mon-help-install-info-incantation'
;; `mon-insert-rename-incantation'           -> `mon-help-rename-incantation'
;; `mon-insert-tar-incantation'              -> `mon-help-tar-incantation'
;; `mon-insert-info-incantation'             -> `mon-help-info-incantation'
;; `mon-insert-diacritics'                   -> `mon-help-diacritics'
;;
;; Following moved to mon-text-property-utils.el
;; `mon-get-next-face-property-change'
;; `mon-get-next-face-property-change-if'
;; `mon-get-text-properties-region'
;; `mon-get-text-properties-print'
;; `mon-get-text-properties-read-temp'
;; `mon-get-text-properties-elisp-string-pp'
;; `mon-get-text-properties-elisp-string'
;; `mon-get-text-properties-parse-prop-val-type-chk'
;; `mon-get-text-properties-parse-buffer'
;; `mon-get-text-properties-parse-sym'
;; `mon-get-text-properties-parse-buffer-or-sym'
;; `mon-get-text-properties-map-ranges'
;; `mon-get-text-properties-map-ranges-string'
;; `mon-get-text-property-bounds'
;;
;; REQUIRES:
;;
;; :FILE boxquote.el 
;;       | ->`mon-help-regexp-symbol-defs-TEST'
;;  
;; mon-doc-help-pacman.el (Loaded only if it exists in load-path)
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-doc-help-pacman.el')
;;
;; mon-doc-help-proprietary.el (Loaded only if it exists in load-path)
;; :NOTE This packages provides documentation functions which can't possibly be
;; GPL/GFDL e.g MS-C0RP API etc. Contact MON if you wish to have this package
;; made avaiable to you.
;;
;; mon-doc-help-css.el (Loaded only if it exists in load-path)
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-doc-help-css.el')
;; 
;; mon-doc-help-tidy.el (Loaded only if it exists in load-path)
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-doc-help-tidy.el')
;;
;; mon-doc-help-mail.el (Loaded only if it exists in load-path)
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-doc-help-mail.el')
;;
;; mon-doc-help-utils-supplemental.el
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-doc-help-utils-supplemental.el')
;; This package is required and should be present in Emacs load-path when using
;; mon-doc-help-utils. It provides the specific subfeatures required to
;; bootstrap mon-doc-help-utils.  In order to load and byte-compile
;; mon-doc-help-utils a few subfeatures need to be present. If you do not wish
;; to load the full feauture set of the following packages
;; mon-doc-help-utils-supplemental.el is careful to load only the neccesary
;; functions and variables listed below:
;;
;; :FILE mon-testme-utils.el
;;       | -> `mon-insert-lisp-testme'
;; :FILE mon-insertion-utils.el 
;;       | -> `mon-comment-divider'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-insertion-utils.el')
;;
;; :FILE mon-regexp-symbols.el 
;;       | -> `*regexp-symbol-defs*'          
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-regexp-symbols.el')
;;
;; :FILE mon-utils.el
;;       | -> `mon-string-index'      
;;       | -> `mon-string-upto-index' 
;;       | -> `mon-string-after-index'
;;       | -> `mon-string-justify-left'
;;       | -> `mon-check-feature-for-loadtime'
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;
;; :FILE mon-text-property-utils.el
;;       | -> `mon-get-next-face-property-change'              
;;       | -> `mon-get-next-face-property-change-if'           
;;       | -> `mon-get-text-properties-region'                 
;;       | -> `mon-get-text-properties-print'                  
;;       | -> `mon-get-text-properties-read-temp'              
;;       | -> `mon-get-text-properties-elisp-string-pp'        
;;       | -> `mon-get-text-properties-elisp-string'           
;;       | -> `mon-get-text-properties-parse-prop-val-type-chk'
;;       | -> `mon-get-text-properties-parse-buffer'           
;;       | -> `mon-get-text-properties-parse-sym'              
;;       | -> `mon-get-text-properties-parse-buffer-or-sym'    
;;       | -> `mon-get-text-properties-map-ranges'             
;;       | -> `mon-get-text-properties-map-ranges-string'      
;;       | -> `mon-get-text-property-bounds'                   
;; :SEE (URL `http://www.emacswiki.org/emacs/mon-text-property-utils.el')
;;
;; Incorporate features from :FILE info-look.el 
;; This might include text-propertization to be run on the `help-mode-hook' to
;; change/inform the args to `info-lookup-add-help'/`info-lookup-alist'.
;;
;; NOTES:
;; 
;; :NOTE MON doesn't like the new 23.0 style UPCASE args in *Help*.
;;
;; (setq help-downcase-arguments t)
;;
;; Where clarity is the concern it is nicer to font-lock arguments in
;; `help-mode' with a face that readily stands out from the surrounding
;; docstring. Following are the face customizations for `button' and
;; `help-argument-name' which MON finds help make help-mode more helpful.
;;  These are provided as quoted forms straight from MON's custom file:
;;
;; '(button 
;;   ((((supports :underline t)) (:background "gray5" 
;;                                :foreground "LightSkyBlue" 
;;                                :underline "Aquamarine" 
;;                                :weight normal))))
;;
;; '(help-argument-name 
;;   ((((supports :slant italic)) (:inherit italic 
;;                                 :foreground "SteelBlue" 
;;                                 :weight normal))))
;;
;; :NOTE MON doesn't like not getting back a reasonably long list and when
;; evaluating `:EXAMPLE's in help-mode *Help* buffers. Following hook ensures
;; that these types of evaluations return a nice long list:
;
;; (add-hook 'help-mode-hook 
;;          #'(lambda () (set (make-local-variable 'print-length) nil)))
;;
;;
;; KEYBINDINGS:
;; Assigning a keybinding for `mod-doc-help-insert-cookie'
;; and make inserting the `*mon-doc-cookie*' variable easier esp. when
;; the doc-cookie is not default and/or you have trouble remembering
;; the ucs char-code for inserting '►►►' \(ucs-insert \"25BA\"\) ;=> ►
;;
;; Assigning a keybinding for `mon-insert-lisp-testme'
;; will make insertion of ';;;test-me;(.*)' strings at end of function defuns
;; easier. This utility trys to DWIM.
;;
;; Alternatively, assigning a keybinding for `mon-insert-doc-help-tail'
;; will give most of the functionality of `mon-insert-lisp-testme'
;; by helping in generation of introspecting function bodies which utilize the
;; tools included herein.
;;
;; :COURTESY Pascal Bourguignon :WAS `space-doc'
;; :SUBJECT Re: How to see that a variable holds t :DATE 2010-01-04
;; :SEE (URL `http://lists.gnu.org/archive/html/help-gnu-emacs/2010-01/msg00060.html')
;; "You could even bind space to automatically find the doc for you:
;;      (global-set-key (kbd "SPC") 'space-doc)
;;  Or, use local-set-key in mode hooks where you write emacs lisp code
;;  such as emacs-lisp-mode-hook."
;; (defun mon-space-doc ()
;;   (interactive)
;;   (save-excursion
;;     (backward-sexp) (backward-char)
;;     (when (looking-at "(")
;;       (forward-char)
;;       (when (thing-at-point 'symbol)
;;         (let* ((start (point))
;;                (end (progn (forward-sexp) (point)))
;;                (symname (buffer-substring start end)))
;;           (ignore-errors (describe-function (intern symname)))))))
;;   (insert " "))
;;
;; SNIPPETS:
;; (while (search-forward-regexp "\\(mon-help-function-spit-doc '\\(.*\\) nil nil t\\)")
;;   (replace-match "mon-help-function-spit-doc '\\2 :insertp t"))
;;
;; :COMPILE-COMMAND 
;; "emacs -batch -L . -eval '(byte-compile-file \"mon-doc-help-utils.el\")' 
;;  rm -v mon-doc-help-utils.elc"
;;
;; THIRD-PARTY-CODE:
;; Portions herein from Aaron Hawley :HIS Reference Sheet By Aaron Hawley:
;; :SEE (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;
;; I think I remember lifting the `mon-help-escape-for-ewiki'
;; from Pascal Bourguignon but I can no longer find the reference
;; to the source... I have it elsewhere as `escape-lisp-string-region'.
;;
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-utils.el
;; FILE-PUBLISHED: <Timestamp: #{2009-08-15} - by MON KEY>
;;
;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/MonDocHelpUtilsDictionary')
;; FIRST-PUBLISHED <Timestamp: #{2010-01-09T01:03:52-05:00Z}#{10016} - by MON>
;;
;; PUBLIC-LINK: 
;; (URL `http://www.emacswiki.org/emacs/mon-doc-help-utils-supplemental.el')
;; FIRST-PUBLISHED: <Timestamp: #{2009-12-21T21:20:06-05:00Z}#{09522} - by MON>
;;
;; FILE-CREATED:
;; <Timestamp: Wednesday June 17, 2009 @ 11:29.15 AM - by MON KEY>
;;
;; ================================================================

;;; LICENSE:

;; ================================================================
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
;; ================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled "GNU Free Documentation License".
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;; Copyright © 2009-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;; (eval-when-compile (require 'mon-cl-compat nil t))

(require 'elint)
(require 'bytecomp)
(require 'mon-text-property-utils)
(require 'mon-utils)
(require 'mon-regexp-symbols)
(require 'mon-insertion-utils)

(declare-function mon-check-feature-for-loadtime "mon-utils" (feature-as-symbol &optional req-w-filname))
(declare-function mon-string-index               "mon-utils" (string-to-idx needle &optional frompos))
(declare-function mon-string-upto-index          "mon-utils" (in-string upto-string))
(declare-function mon-string-after-index         "mon-utils" (in-str after-str))
(declare-function mon-string-justify-left        "mon-utils" (justify-string &optional justify-width lft-margin no-rmv-trail-wspc))
(declare-function mon-comment-divider            "mon-insertion-utils" (&optional no-insrtp intrp))
(declare-function mon-insert-lisp-testme         "mon-testme-utils" (&optional search-func test-me-count insertp intrp))
(declare-function mon-g2be                       "mon-buffer-utils"    (&optional min/max-go no-go))
(declare-function mon-buffer-sub-no-prop         "mon-buffer-utils"        (&optional buf-beg buf-end))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-06T11:43:27-05:00Z}#{10056} - by MON KEY>
(defgroup mon-doc-help-utils nil
  "Extensions for help and documentation related procedures.\n
:SEE (URL `http://www.emacswiki.org/emacs/MonDocHelpUtilsDictionary')\n
:SEE-ALSO `mon-doc-help-utils-faces', `mon-doc-help-CL'.\n►►►"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-doc-help-utils.el')" 
          "http://www.emacswiki.org/emacs/mon-doc-help-utils.el")
  :link '(url-link 
          :tag ":EMACSWIKI-DICTIONARY (URL `http://www.emacswiki.org/emacs/MonDocHelpUtilsDictionary')" 
          "http://www.emacswiki.org/emacs/MonDocHelpUtilsDictionary")
  :link '(emacs-library-link 
          :tag ":FILE mon-doc-help-utils.el" 
          "mon-doc-help-utils.el")
  :link '(custom-group-link mon-doc-help-utils-faces)
  :prefix "mon-help-"
  :group 'mon-base
  :group 'docs
  :group 'help)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-06T11:26:37-05:00Z}#{10056} - by MON KEY>
(defgroup mon-doc-help-utils-faces nil
  "The faces for mon-doc-help-utils library.\n
;; :REGEXP-LIST                                :FACE 
`*regexp-mon-doc-help-pointer-tags*'           `mon-help-PNTR-tag'
`*regexp-mon-doc-help-docstring-tags-DYNAMIC*' `mon-help-DYNATAB-tag'
`*regexp-mon-doc-help-docstring-tags-TABLES*'  `mon-help-DYNATAB-tag' 
`*regexp-mon-doc-help-docstring-tags*'         `mon-help-KEY-tag'
`*regexp-mon-doc-help-comment-tags*'           `mon-help-COMMENT-TAG'
`*regexp-mon-doc-help-meta-tags*'              `mon-help-META-tag'
`*regexp-mon-doc-help-docstring-tags-URL*'     `mon-help-URL-wrap-tag'
`*regexp-mon-doc-help-builtin-dynamic-tags*'   `mon-help-BUILTIN-tag'
`*regexp-mon-doc-help-builtin-static-tags*'    `mon-help-BUILTIN-tag'
 unaffiliated                                  `mon-help-INNER-KEY-tag'
 inlined-regexp                                `mon-help-OLAY-RESULT'
 inlined-regexp                                `mon-help-OLAY-RESULT-string-show'
 inlined-regexp                                `mon-help-OLAY-RESULT-match-show'\n
:SEE :FILE mon-doc-help-utils.el\n
:SEE-ALSO `mon-help-propertize-tags', `mon-help-mon-tags', `mon-help-insert-tags',
`*mon-help-mon-tags-alist*', `mon-help-view-file', `mon-help-temp-docstring-display',
`*mon-help-docstring-help-bffr*'.\n►►►"
  :link '(emacs-library-link 
          :tag ":FILE mon-doc-help-utils.el"
          "mon-doc-help-utils.el")
  :link '(url-link 
          :tag ":EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-doc-help-utils.el')"
          "http://www.emacswiki.org/emacs/mon-doc-help-utils.el")
  :group 'mon-base
  :group 'mon-doc-help-utils
  :group 'faces)

;;; ==============================
;;; :CHANGESET 2394
;;; :CREATED <Timestamp: #{2011-01-15T13:53:58-05:00Z}#{11026} - by MON KEY>
(defcustom *mon-doc-help-utils-xrefs*
    '(mon-map-subrs-and-hash mon-help-permanent-locals-find
      mon-help-byte-optimizer-find mon-help-propertize-tags mon-help-mon-tags
      mon-help-insert-tags mon-help-insert-tags-comment
      mon-help-overlay-for-example mon-help-delimited-region
      mon-help-overlay-on-region mon-help-overlay-result
      mon-help-find-result-for-overlay mon-help-temp-docstring-display
      mon-help-buffer-spc-*DOC* mon-help-get-mon-help-buffer mon-help-view-file
      mon-update-tags-tables mon-update-tags-tables-loadtime
      mon-tags-naf-apropos mon-tags-apropos mon-insert-doc-help-cookie
      mon-help-put-var-doc-val->func mon-help-swap-var-doc-const-val
      mon-help-xref-symbol-value mon-help-function-spit-doc
      mon-insert-doc-help-tail mon-help-message-intrp mon-help-function-arity
      mon-help-errors mon-help-function-args mon-help-parse-interactive-spec
      mon-help-insert-documentation mon-help-help-functions mon-help-mon-help
      mon-help-mon-functions mon-help-ebay-template-mode
      mon-help-emacs-introspect mon-help-package-keywords mon-help-regexp-syntax
      mon-help-syntax-class mon-help-syntax-functions mon-help-search-functions
      mon-help-hooks mon-help-file-dir-functions
      mon-help-file-dir-functions-usage mon-help-process-functions
      mon-help-make-network-process mon-help-server-functions
      mon-help-inhibit-functions mon-help-buffer-functions
      mon-help-frame-functions mon-help-window-functions mon-help-mode-functions
      mon-help-xml-functions mon-help-eieio-defclass mon-help-eieio-functions
      mon-help-eieio-methods mon-help-type-predicates mon-help-number-functions
      mon-help-predicate-functions mon-help-sequence-functions
      mon-help-key-functions mon-help-load-functions mon-help-read-functions
      mon-help-print-functions mon-help-marker-functions mon-help-hash-functions
      mon-help-plist-functions mon-help-plist-properties mon-help-faces-themes
      mon-help-faces mon-help-faces-basic mon-help-faces-font-lock
      mon-help-font-lock-functions mon-help-font-lock mon-help-overlay-functions
      mon-help-text-property-functions mon-help-text-property-properties
      mon-help-text-property-stickyness mon-help-text-property-functions-ext
      mon-help-color-functions mon-help-color-chart mon-help-easy-menu
      mon-help-widgets mon-help-custom-keywords mon-help-char-functions
      mon-help-char-charset-functions mon-help-char-coding-functions
      mon-help-char-table-functions mon-help-display-table-functions
      mon-help-char-unidata-table mon-help-char-composition mon-help-char-ascii
      mon-help-char-iso-8859-1 mon-help-char-ecma-48 mon-help-char-ecma-35
      mon-help-diacritics mon-help-char-logic mon-help-char-representation
      mon-help-char-raw-bytes mon-help-binary-representation
      mon-help-symbol-functions mon-help-byte-compile-functions
      mon-help-byte-code-vector-symbols mon-help-ipv4-header
      mon-help-nclose-functions mon-help-iso-8601 mon-help-time-functions
      mon-help-mon-time-functions mon-help-bookmark-functions
      mon-help-info-incantation mon-help-tar-incantation
      mon-help-rename-incantation mon-help-du-incantation
      mon-help-install-info-incantation mon-help-hg-archive mon-help-crontab
      mon-help-permissions mon-help-unix-commands mon-help-format-width
      mon-index-elisp-symbol mon-help-w32-functions mon-help-w32-env
      mon-help-bind-help-keys-loadtime mon-help-keys
      mon-help-keys-wikify-anchors mon-help-keys-wikify-heading
      mon-help-keys-wikify mon-help-escape-for-ewiki mon-help-unescape-for-ewiki
      mon-help-utils-loadtime
      ;; :FACES
      mon-help-KEY-tag mon-help-DYNATAB-tag mon-help-META-tag mon-help-PNTR-tag
      mon-help-COMMENT-tag mon-help-INNER-KEY-tag mon-help-URL-wrap-tag
      mon-help-BUILTIN-tag mon-help-OLAY-RESULT mon-help-OLAY-RESULT-string-show
      mon-help-OLAY-RESULT-match-show
      ;; :VARIABLES
      *mon-doc-help-utils-xrefs* *mon-doc-cookie* *mon-help-docstring-help-bffr*
      *mon-help-custom-faces-builtins-tags* *mon-help-mon-tags-alist*
      *mon-help-reference-keywords* *regexp-mon-doc-help-pointer-tags*
      *regexp-mon-doc-help-docstring-tags-URL*
      *regexp-mon-doc-help-docstring-tags-DYNAMIC*
      *regexp-mon-doc-help-docstring-tags-TABLES*
      *regexp-mon-doc-help-docstring-tags* *regexp-mon-doc-help-comment-tags*
      *regexp-mon-doc-help-meta-tags* regexp-mon-doc-help-builtin-dynamic-tags*
      *regexp-mon-doc-help-builtin-static-tags* *mon-help-side-effect-free*
      *mon-help-side-effect-and-error-free* *mon-help-pure-functions*
      *mon-help-subrs* *mon-help-subrs-false* mon-help-byte-optimizer-vals*
      **mon-help-risky-local-variables* *mon-help-permanent-locals*
      *mon-tags-table-list* *mon-help-emacs-errors*
      **mon-help-interactive-spec-alist* *mon-emacs-external-programs-vars*
      *regexp-clean-du-flags* mon-help-propertize-tags-triples*
      *mon-help-reference-keys* w32-env-variables-alist*
      *mon-doc-help-utils-xrefs*)
  "Xrefing list of `mon-help-*' symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-doc-help-utils.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-keybindings-xrefs*',
`*mon-testme-utils-xrefs*', `*mon-button-utils-xrefs*', `*mon-bzr-utils-xrefs*'
`*mon-buffer-utils-xrefs*', `*mon-error-utils-xrefs*', `*mon-line-utils-xrefs*',
`*mon-macs-xrefs*', `*mon-plist-utils-xrefs*', `*mon-post-load-hooks-xrefs*', 
`*mon-seq-utils-xrefs*', `*mon-string-utils-xrefs*', `*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*', `*mon-slime-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type  '(repeat symbol)
  :group 'mon-xrefs
  :group 'mon-doc-help-utils)

;;; ==============================
;;; :CHANGESET 2174
;;; :CREATED <Timestamp: Friday July 03, 2009 @ 01:11.47 PM - by MON KEY>
(defcustom *mon-doc-cookie* "►►►"
  "Default 'documentation cookie' used in `mon-*' functions and vars.\n
A Documentation cookie delimter for use with `mon-help-function-spit-doc'.\n
Used to delimit which portion of docstring should be commented out when
inserting into buffer.\n
Default value is \"\u25BA\u25BA\u25BA\"\n
:EXAMPLE\n
\(momentary-string-display 
 \(let \(cooky\)
   \(dotimes \(i 3 (concat \" The `*mon-doc-cookie*' -> \" cooky \)\)
     \(setq cooky \(concat \(char-to-string ?\\u25BA\) cooky\)\)\)\)
 \(point\)\)\n
:ALIASED-BY `*doc-cookie*'\n
:SEE-ALSO `mon-insert-doc-help-cookie', `mon-insert-doc-help-tail',
`mon-insert-ebay-field-trigger-l', `mon-insert-ebay-field-trigger-r',
`mon-insert-ebay-field-trigger-l-and-r'.\n►►►"
  :type  'string
  :group 'mon-doc-help-utils)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-20T17:42:27-05:00Z}#{09517} - by MON KEY>
(defcustom *mon-help-docstring-help-bffr* "*MON-HELP*"
  "A buffer name in which to return `mon-help-*' related docstrings.\n
Default is \"*MON-HELP*\".\n
:SEE-ALSO `mon-help-view-file', `mon-help-temp-docstring-display',
`mon-help-get-mon-help-buffer'.\n►►►"
  :type 'string
  :group 'mon-doc-help-utils)
;;
;;; :TEST-ME *mon-help-docstring-help-bffr*
;;
;;;(progn (makunbound *mon-help-docstring-help-bffr*)
;;;       (unintern "*mon-help-docstring-help-bffr*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-19T20:27:23-05:00Z}#{10076} - by MON KEY>
(defcustom *mon-help-custom-faces-builtins-tags*
  '(":action" ":args" ":background" ":box" ":button-face" ":button-prefix"
   ":button-suffix" ":color" ":doc" ":family" ":fontset" ":foreground" ":format"
   ":foundry" ":get" ":group" ":height" ":help-echo" ":inherit" ":initialize"
   ":inline" ":inverse-video" ":line-width" ":link" ":load" ":match"
   ":match-alternatives" ":options" ":overline" ":package-version" ":prefix"
   ":require" ":risky" ":save" ":set" ":set-after" ":slant" ":stipple"
   ":strike-through" ":style" ":style" ":tag" ":type" ":underline" ":validate"
   ":value" ":version" ":weight" ":width")
  "List of keyword styled strings used with `defcustom' and `defface' forms.\n 
:SEE-ALSO `mon-help-BUILTIN-tag', `*regexp-mon-doc-help-builtin-dynamic-tags*',
`*regexp-mon-doc-help-builtin-static-tags*'.\n►►►"
  :type '(repeat string)
  :group 'mon-doc-help-utils)
;;
;;; :TEST-ME *mon-help-custom-faces-builtins-tags*
;;
;;;(progn (makunbound '*mon-help-custom-faces-builtins-tags*)
;;;       (unintern "*mon-help-custom-faces-builtins-tags*" obarray) )

;;; ==============================
;;; :TODO These and their occurences in MON files should be replaced by:
;;;   [FUNCTION], [VARIABLE], [SOME-THING], etc.
;;; This should alter the standard tags in `meta-tags' key from:
;;;   <TAG> -> [TAG] For congruence with CLTL2, document-temp
;;;  Which is more inline with other Lisp2 documentation styles :)
;;; Ideally the elts of key 'meta-tags and 'meta-tags-keybindings
;;; would use "MATHEMATICAL LEFT/RIGHT ANGLE BRACKET" e.g.:
;;; ?\u27e8 "⟨" and ?\u27e9 "⟩"  (ucs-insert 10217) => ⟩ (ucs-insert 10216) => ⟨
;;; But, That code-point doesn't have glyph with our pref. font on current w32.
;;; :MODIFICATIONS <Timestamp: #{2010-02-26T14:52:42-05:00Z}#{10085} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-11-20T17:41:52-05:00Z}#{09475} - by MON>
(defcustom *mon-help-mon-tags-alist* nil
  "A list of commonly used MON tags.\n
The key `comment-tags' associates tags appearing in source comments.\n
The key `docstr-tags' associates tags appearing in docstrings.\n
The key `meta-tags' associates tags appearing in both source comments and 
within docstrings to indicate metasyntactic or idiomatic forms and types.\n
Each list element has the form:\n
 \( <KEY> \( \"<STRINGS>*\" \)\)\n
- <KEY> is a symbol.
- <STRINGS>* is one or more strings. 
These should be formatted in upcase as either:
 \":SOME-STRING\" or \"<SOME-STRING>\" 
:NOTE In the above example \"<\" and \">\" chars are not metasyntactic.\n
:EXAMPLE\n\n\(assq 'comment-tags *mon-help-mon-tags-alist*\)\n
\(assq 'docstr-tags *mon-help-mon-tags-alist*\)\n
\(assq 'meta-tags *mon-help-mon-tags-alist*\)\n
\(assq 'meta-tags-keybindings *mon-help-mon-tags-alist*\)\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
`*regexp-mon-doc-help-docstring-tags-TABLES*',
`*regexp-mon-doc-help-docstring-tags*', `*regexp-mon-doc-help-comment-tags*',
`*regexp-mon-doc-help-meta-tags*', `mon-help-insert-tags', `mon-help-mon-tags',
`*mon-help-reference-keywords*', `*mon-help-custom-faces-builtins-tags*'.\n►►►"
  :type '(repeat  (list (symbol :tag "<KEY>") (repeat :tag "<STRINGS>*" string)))
  :group 'mon-doc-help-utils)
;;
(unless (and (intern-soft "*mon-help-mon-tags-alist*" obarray)
             (bound-and-true-p *mon-help-mon-tags-alist*))
  (setq *mon-help-mon-tags-alist*
        '( ;; :NOTE When adding face props in emacs-lisp-mode make sure to 
          ;;  set ``font-lock-face'' property to SOME-FACE prop-val
          ;;  setting just a ``face'' property won't light up.
          ;;  With `set-text-properties' & `add-text-properties' add the quoted
          ;;  plist with prop propval pairs e.g:
          ;; '(face font-lock-constant-face font-lock-face font-lock-constant-face))
          ;;  With `put-text-property' add the quoted prop with prop-val e.g.:
          ;; (put-text-property START END 'font-lock-face font-lock-constant-face)
          ;; (put-text-property START END 'face font-lock-constant-face)
          (comment-tags ;; `*regexp-mon-doc-help-comment-tags*'
           (":AFTER" ":AS-OF" ":BEFORE" ":ADDED" 
            ":CHANGED" ":CHANGESET" ":CLEANUP" ":CLOSE" ":COMMENTED" ":COURTESY" ":CREATED"  
            ":DATE" ":DECLARED-SPECIAL" ":DEBUGGING" ":DEFAULT"
            ":EMACS-WIKI" ":EMACSWIKI-FILE" ":EVAL-BELOW-TO-TEST" 
            ":FIXES" ":FIXME" ":FIX-ME" ":FROM" ;; :NOTE also has ":FILE"
            ":HIS" ":IF-NOT-FEATURE-P" ":INSTALL-TO" ":INSTALLED-TO"
            ":LOAD-SPECIFIC-PROCEDURES" 
            ":MODIFICATIONS" 
            ":PREFIX"
            ":RENAMED" ":REQUIRES" ":REQUIRED-BY"
            ":SEE-BELOW" ":SUBJECT" ":SUBDIR-OF"
            ":TAGS-COMMON" ":TEST-ME" ":TESTING" ":TODO"  ":TO"
            ":UNCOMMENT-BELOW-TO-TEST" ":UNCOMMENT-TO-TEST"
            ":VERSION" 
            ;; :NOTE ":WANTING" is used w/ ":TEST-ME" for expected return value. 
            ;;  Has same length e.g. (eq (length ":TEST-ME") (length ":WANTING"))
            ":WANTING" ":WAS" ":W/OUT-MON-NAMESPACE"))
          ;; (alias-tags 
          ;;  ("<CORE-SYMBOL>" 
          ;;   "<PREFIX>-<NON-CORE-SYMBOL>" "<PREFIX>-<QUALIFIED>" 
          ;;   "<QUALIFIED-ALIAS>" "<UNQUALIFIED-ALIAS>")
          (docstr-tags ;; `*regexp-mon-doc-help-docstring-tags*'
           ("►►►" ":ALIASED-BY" ":ALIAS-OF"
            ":CALLED-BY" 
            ":DEPRECATED"
            ":EXAMPLE"
            ":FACE-DEFINED-IN" ":FACE-DOCUMENTED-IN" 
            ":FACE-INHERITED-BY" ":FACE-INHERITS-FROM" 
            ":FACE-FONT-LOCKING-WITH"
            ;; ":INHERITED-BY" ":FACE-INHERITS-FROM "
            ":FILE" 
            ":IDIOM"
            ":KEYWORD-REGEXPS-IN" ":KEYWORD-LISTS-IN" 
            ":NOTE"
            ":REGEXPS-IN" 
            ":SEE" ":SEE-ALSO" ":SOURCE" 
            ":USED-BY" ":USED-IN"))
          (meta-tags-keybindings ;; 
           ("<BEGINNING>" "<BUFFER>" "<CLASS>" "<COMMAND>" "<CONSTANT>"
            "<DEPRECATED>" "<DIRECTORY>" "<EXPRESSION>" "<FACE>" "<FILE>" "<FILES>"
            "<FUNCTION>" "<INTEGER>" "<KEY>" "<LIBRARY>" "<MACRO>" "<MARKER>"
            "<MATCH>" "<METHOD>" "<NAME>" "<NEW>" "<OLD>" "<PATH>" "<PATTERN>"
            "<PRINTER>" "<PROPERTY>" "<REGEXP>" "<STRING>" "<SYMBOL>" "<TERM>"
            "<TERMS>" "<TITLE>" "<VARIABLE>"))
          ;; :NOTE Push/append onto the cdr of this keys list to add new values.
          ;; This lets us partition `meta-tags-keybindings' from `meta-tags'
          ;; where the `meta-tags' key is the superset.
          (meta-tags ;; :CALLED-BY `*regexp-mon-doc-help-meta-tags*'
           ("<BOOLEAN>" "<CHAR>" "<CONS>" "<DIRECTORY>" "<FACE>" "<FILENAME>"
            "<INTEGER>" "<LIST>" "<PREDICATE>" "<PROPERTY>" "<PVAL>" "<REGION>"
            "<VECTOR>"))))
  (setf (cdr (assoc 'meta-tags *mon-help-mon-tags-alist*))
        `(,(sort (delete-dups
                  (append
                   (cadr (assoc 'meta-tags *mon-help-mon-tags-alist*))
                   (cadr (assoc 'meta-tags-keybindings *mon-help-mon-tags-alist*))))
                 'string-lessp)))
  (custom-note-var-changed '*mon-help-mon-tags-alist*)
  ) ;; :CLOSE unless
;;
;;; :TEST-ME (assq 'comment-tags *mon-help-mon-tags-alist*)
;;; :TEST-ME (assq 'docstr-tags *mon-help-mon-tags-alist*)
;;; :TEST-ME (assq 'meta-tags *mon-help-mon-tags-alist*)
;;
;;;(progn (makunbound '*mon-help-mon-tags-alist*) 
;;;       (unintern "*mon-help-mon-tags-alist*" obarray) )

;;; ==============================
;;; :TODO Finish the snarfing routine which snarfs the keys in other `mon-help-*' docstrings
;;; Currently limited to the keybinding related keys in `mon-help-keys'.\n
;;; :CREATED <Timestamp: #{2010-02-26T18:01:23-05:00Z}#{10085} - by MON KEY>
(defcustom *mon-help-reference-keywords* nil
  "A list of keywords appearing in `mon-help-.*' functions.\n
Includes the upper-cased colon prefixed keywords appearing at BOL and after `;; '.\n
Each key maps the keywords present in the docstrings function or /variable.\n
Elements of list have the form:\n
 \( <KEY> \( <:SOME-KEYWORD1>  <:SOME-KEYWORD2> <:SOME-KEYWORDN>* \) \)\n
Where <KEY> associates to a `mon-help-.*' function with a docstring containing
\";; :KEYWORDS\". So for example:\n
 \(assq 'mon-help-keys-keywords *mon-help-reference-keywords*\)\n
returns the list of keybinding related keywords appearing in the docstring of
the function `mon-help-keys' such that the value can then be mapped when
converting, translating, finding, replacing, etc. i.e. as is done with
`*mon-help-reference-keys*' (which see).\n
:EXAMPLE\n\n\(car \(memq :SORT-KEY-BINDINGS 
           \(cadr \(assq 'mon-help-keys-keywords *mon-help-reference-keywords*\)\)\)\)\n 
:SEE-ALSO `*mon-help-mon-tags-alist*'.\n►►►"
  :type  '(repeat (list (symbol :tag "<KEY>") (repeat :tag "<:SYMBOL>*" symbol)))
  :group 'mon-doc-help-utils)
;;
(unless (and (intern-soft "*mon-help-reference-keywords*" obarray) 
             (bound-and-true-p *mon-help-reference-keywords*))
  (setq *mon-help-reference-keywords* 
        '((mon-help-keys-keywords ;; `mon-help-keys' `*mon-help-reference-keys*'
           (:ABBREV-KEY-BINDINGS :ALIGN-KEY-BINDINGS :ANIMATION-KEY-BINDINGS
            :AUTOINSERT-KEY-BINDINGS :BROWSE-URL-KEY-BINDINGS :BUFFER-KEY-BINDINGS
            :BUFFER-MENU-KEY-BINDINGS :C-KEY-BINDINGS :CALC-KEY-BINDINGS
            :CALENDAR-KEY-BINDINGS :CANCEL-KEY-BINDINGS :CAPITALIZATION-KEY-BINDINGS
            :CHANGELOG-KEY-BINDINGS :CHAR-KEY-BINDINGS :CHAR-KEY-BINDINGS
            :COLUMN-KEY-BINDINGS :COMMAND-KEY-BINDINGS :COMMENT-KEY-BINDINGS
            :COMPILE-KEY-BINDINGS :COMPLETION-KEY-BINDINGS :COMPOSITION-KEY-BINDINGS
            :CUSTOMIZE-KEY-BINDINGS :DABBREV-KEY-BINDINGS :DELETE-KEY-BINDINGS
            :DELIMITED-TEXT-KEY-BINDINGS :DIARY-KEY-BINDINGS :DIFF-KEY-BINDINGS
            :DIRECTORY-FOLDER-KEY-BINDINGS :DIRECTORY-KEY-BINDINGS :EDIFF-KEY-BINDINGS
            :ELISP-DEBUG-KEY-BINDINGS :ELISP-INTERACTION-KEY-BINDINGS
            :EMACS-LISP-KEY-BINDINGS :EMAIL-KEY-BINDINGS :EMERGE-KEY-BINDINGS
            :ETAGS-KEY-BINDINGS :EXIT-KEY-BINDINGS :FILE-KEY-BINDINGS :FILE-KEY-BINDINGS
            :FINDER-KEY-BINDINGS :FRAME-REGISTER-KEY-BINDINGS :GAMES-KEY-BINDINGS
            :GDB-KEY-BINDINGS :GREP-KEY-BINDINGS :HELP-KEY-BINDINGS :HTML-KEY-BINDINGS
            :INDENT-KEY-BINDINGS :INFO-HELP-KEY-BINDINGS :ITERATIVE-COMMAND-KEY-BINDINGS
            :KEYS-KEY-BINDINGS :KILL-CUT-KEY-BINDINGS :KILL-CUT-KEY-BINDINGS
            :LINE-KEY-BINDINGS :LINE-KEY-BINDINGS :LISP-KEY-BINDINGS :LOCATE-KEY-BINDINGS
            :MACRO-KEY-BINDINGS :MARK-KEY-BINDINGS :MENU-KEY-BINDINGS
            :MERGE-CONFLICT-KEY-BINDINGS :MINIBUFFER-KEY-BINDINGS :MODE-KEY-BINDINGS
            :NEWLINE-KEY-BINDINGS :NON-INTERACTIVE-SEARCH-KEY-BINDINGS
            :NON-ITERATIVE-COMMAND-KEY-BINDINGS :NUMBER-REGISTER-KEY-BINDINGS
            :OCCUR-KEY-BINDINGS :OPEN-KEY-BINDINGS :OUTLINE-KEY-BINDINGS
            :PAGE-KEY-BINDINGS :PARAGRAPH-KEY-BINDINGS :POINT-REGISTER-KEY-BINDINGS
            :POSITION-REGISTER-KEY-BINDINGS :READ-ONLY-KEY-BINDINGS
            :RECTANGLE-KEY-BINDINGS :RECURSIVE-EDIT-KEY-BINDINGS
            :RECURSIVE-EDIT-KEY-BINDINGS :REDISPLAY-KEY-BINDINGS :REGION-KEY-BINDINGS
            :REGULAR-EXPRESSION-KEY-BINDINGS :REPLACE-KEY-BINDINGS :SEARCH-KEY-BINDINGS
            :SENTENCE-KEY-BINDINGS :SEXP-AND-PAREN-KEY-BINDINGS :SHELL-KEY-BINDINGS
            :SHELL-SCRIPT-KEY-BINDINGS :SORT-KEY-BINDINGS
            :SOURCE-LEVEL-DEBUGGER-KEY-BINDINGS :SPELL-CHECK-KEY-BINDINGS
            :SQL-KEY-BINDINGS :TABLE-KEY-BINDINGS :TEXT-REGISTER-KEY-BINDINGS
            :TIMECLOCK-KEY-BINDINGS :TRAMP-KEY-BINDINGS :TRANSPOSE-KEY-BINDINGS
            :UNDO-KEY-BINDINGS :VC-VERSION-CONTROL-KEY-BINDINGS ;; :W3-KEY-BINDINGS
            :WHITESPACE-KEY-BINDINGS :WINDOW-KEY-BINDINGS :WINDOW-REGISTER-KEY-BINDINGS
            :WORD-KEY-BINDINGS :YANK-PASTE-KEY-BINDINGS))))
  (custom-note-var-changed '*mon-help-reference-keywords*))
;;
;;; :TEST-ME *mon-help-reference-keywords*
;;
;;;(progn (makunbound '*mon-help-reference-keywords*) 
;;;       (unintern "*mon-help-reference-keywords*" obarray) )

;;; ==============================
;;; :NOTE Consider adding `→' and/or building separate facility to match/replace
;;; :CREATED <Timestamp: #{2009-11-21T15:07:32-05:00Z}#{09476} - by MON>
(defcustom *regexp-mon-doc-help-pointer-tags*
  ;;
  "\\([;\\[:space:]]?\\)\\(\\(<-\\{1,2\\}\\)\\|\\(-\\{1,2\\}>\\)\\|\\(=\\{1,2\\}>\\)\\)"
  ;;
  "Regexp for font-locking 'pointers' in docstrings and comments.\n
For `help-mode' views of MON functions, in particular those from 
:FILE `mon-doc-help-utils.el'.\n
:EXAMPLE\n\n\(save-excursion
  \(dotimes \(i 22\)
    \(mon-help-find-result-for-overlay *regexp-mon-doc-help-pointer-tags* 0 0 78\)\)\)\n
Matches the following:\n
 ->   ;->  ; -> \n =>   ;=>  ; => \n -->  ;-->  ; --> --->
 <--  <--  ; <--  <---\n <--  ;<-- ; <--  <--- \n ==>  ;==> ; ==>  ===>\n
:EXAMPLE\n\n \(progn 
   \(search-forward-regexp *regexp-mon-doc-help-pointer-tags*\)
   \(match-string-no-properties 2\)\)\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
`*regexp-mon-doc-help-docstring-tags-TABLES*',
`*regexp-mon-doc-help-docstring-tags*',`*regexp-mon-doc-help-comment-tags*',
`*regexp-mon-doc-help-meta-tags*'`mon-help-insert-tags', `mon-help-mon-tags',
`*mon-help-mon-tags-alist*'.\n►►►"
  :type 'regexp
  :group 'mon-doc-help-utils)
;;
;;;
;;; :TEST-ME (progn (search-forward-regexp *regexp-mon-doc-help-pointer-tags*)
;;;                 (match-string-no-properties 2))
;;;
;;; ,---- :UNCOMMENT-BELOW-TO-TEST
;;; | ->   ;->  ; ->
;;; | =>   ;=>  ; =>    
;;; | -->  ;-->  ; --> --->
;;; | <--  <--  ; <--  <---
;;; | <--  ;<-- ; <--  <--- 
;;; | ==>  ;==> ; ==>  ===> 
;;; `----
;;
;;; (progn (makunbound '*mon-help-mon-tags-alist*)
;;;        (unintern "*mon-help-mon-tags-alist*" obarray) ) 

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-15T20:38:17-05:00Z}#{10072} - by MON KEY>
(defcustom *regexp-mon-doc-help-docstring-tags-URL*
  ;;
  "^\\(:SEE \\)?\\((URL `\\)\\(.*\\)\\(')\\)" 
  ;;
  ;;^^1^^^^^^^^^^^2^^^^^^^^^^^3^^^^^^^4^^^^
  ;;"\\(^:SEE ?\\|[A-z] ?\\)\\((URL `\\)\\(.*\\)\\(')\\)"
  ;; grp2 -> (URL `
  ;; grp4 -> ')
  ;;
  "Regexp for matching the \"(URL `\" prefix and \"')\" suffix in docstrings.\n
For `help-mode' views of MON functions, in particular those from:
:FILE `mon-doc-help-utils.el'.\n
:MATCH-GROUP 1 -> ``:SEE''\n:MATCH-GROUP 2 -> ``(URL \x60''
:MATCH-GROUP 3 -> ``{... lotsa URL ...}''\n:MATCH-GROUP 4 -> ``\'\)''\n
:EXAMPLE\n
\(save-excursion  
  \(let \(\(ebnds \(1+ \(cdadr \(nth 1 \(mon-help-delimited-region t\)\)\)\)\)\)
    \(dolist \(i '\(1 2 3 4\)\) 
      \(message \"Next match group is %d\" i\)\(sit-for 1.25\)
      \(search-forward-regexp *regexp-mon-doc-help-docstring-tags-URL* ebnds t\)
      \(mon-help-overlay-result \(match-beginning i\) \(match-end i\)
                               78 \(match-string-no-properties i\)\)\)\)\)\n
►\n:SEE (URL `http://www.IWasArpanet.com/i-am-really-gopher.html'\)
:SEE (URL `http://www.ThisIsNotMilnet.com/not-for-u.html'\)
\(URL `http://www.IamTheInterWeb.com/wow-i-can-blah.htm'\)
\(URL `http://www.IamTheInterTubes.com/now-blah-is-blahging.htm'\)\n◄\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
`*regexp-mon-doc-help-docstring-tags-TABLES*',
`*regexp-mon-doc-help-docstring-tags*', `*regexp-mon-doc-help-comment-tags*',
`*regexp-mon-doc-help-meta-tags*', `*mon-help-mon-tags-alist*',
`*mon-help-propertize-tags-triples*', `mon-help-insert-tags',
`mon-help-mon-tags', `mon-help-propertize-tags', `help-xref-url-regexp'.\n►►►"
  :type 'regexp
  :group 'mon-doc-help-utils)
;;
;;; (progn (makunbound '*regexp-mon-doc-help-docstring-tags-URL*)
;;;        (unintern "*regexp-mon-doc-help-docstring-tags-URL*" obarray) ) 

;;; ==============================
;;; :TODO Adjust regexp to match multiple `:' prefixed tags per line.
;;; :CREATED <Timestamp: #{2009-11-21T14:24:06-05:00Z}#{09476} - by MON>
(defcustom *regexp-mon-doc-help-docstring-tags-DYNAMIC* "^\\(;; :[A-Z0-8-]+$\\)"
  ;;
  "Regexp for font-locking docstring keyword symbol tags.\n
For `help-mode' views of MON functions, in particular those from:
:FILE `mon-doc-help-utils.el'.\n
Matches colon prefixed symbols if preceded by two semicolons and a space at BOL.\n
:EXAMPLE\n\n(search-forward-regexp *regexp-mon-doc-help-docstring-tags-DYNAMIC* nil t)\n
;; :FINDS-ME\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags-TABLES*',
`*regexp-mon-doc-help-docstring-tags*', `*regexp-mon-doc-help-comment-tags*',
`*regexp-mon-doc-help-pointer-tags*', `*regexp-mon-doc-help-meta-tags*',
`mon-help-insert-tags', `mon-help-mon-tags', `*mon-help-mon-tags-alist*',
`*mon-help-propertize-tags-triples*'.\n►►►"
  :type 'regexp
  :group 'mon-doc-help-utils)
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (search-forward-regexp *regexp-mon-doc-help-docstring-tags-DYNAMIC* nil t)
;; | ;; :BUBBA
;; `----
;;;
;;; (progn (makunbound '*regexp-mon-doc-help-docstring-tags-DYNAMIC*)
;;;        (unintern "*regexp-mon-doc-help-docstring-tags-DYNAMIC*" obarray) ) 

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-21T14:24:02-05:00Z}#{09476} - by MON>
(defcustom *regexp-mon-doc-help-docstring-tags-TABLES*
  ;;
  ;; "\\([[:space:]]|[[:space:]]+\\(:[A-Z-]+\\)[[:space:]]+|\\)"
  "\\([\\[:space:]]|[\\[:space:]]+\\(:[A-Z-]+\\)[\\[:space:]]+|\\)"
    ;;
  "*Regexp for font-locking docstring keyword symbol tags in TABLES.\n
For `help-mode' views of MON functions, in particular those from:
:FILE `mon-doc-help-utils.el'.\n
:EXAMPLE\n \(progn
   \(search-forward-regexp *regexp-mon-doc-help-docstring-tags-TABLES*\)
   \(match-string-no-properties 2\)\)\n
 | :SOME-SECTIONA | :SOME-SECTIONB | :SOME-SECTIONC |\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
`*regexp-mon-doc-help-docstring-tags*', `*regexp-mon-doc-help-comment-tags*',
`*regexp-mon-doc-help-pointer-tags*', `*regexp-mon-doc-help-meta-tags*',
`*mon-help-mon-tags-alist*', `*mon-help-propertize-tags-triples*',
`mon-help-insert-tags', `mon-help-mon-tags', `mon-help-propertize-tags'.\n►►►"
  :type '(regexp)
  :group 'mon-doc-help-utils)
;;
;;; :TEST-ME (progn (search-forward-regexp *regexp-mon-doc-help-docstring-tags-TABLES*)
;;;           (match-string-no-properties 2))
;;;
;;; | :SOME-SECTIONA | :SOME-SECTIONB | :SOME-SECTIONC |
;;
;;; (progn (makunbound '*regexp-mon-doc-help-docstring-tags-TABLES*)
;;;        (unintern "*regexp-mon-doc-help-docstring-tags-TABLES*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-21T14:19:24-05:00Z}#{09476} - by MON>
(defcustom *regexp-mon-doc-help-docstring-tags*
  ;;
  (regexp-opt (cadr (assq 'docstr-tags  *mon-help-mon-tags-alist*)))
  ;;
  "*Regexp for locating \"meta-syntactic\" type tags.\n
For `help-mode' views of MON functions, in particular those from 
:FILE `mon-doc-help-utils.el'.\n
:KEYWORD-LISTS-IN `*mon-help-mon-tags-alist*'
Regexp generated from `docstr-tags' key:\n
 \(cadr \(assoc 'docstr-tags  *mon-help-mon-tags-alist*\)\)\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
`*regexp-mon-doc-help-docstring-tags-TABLES*', `*regexp-mon-doc-help-comment-tags*',
`*regexp-mon-doc-help-pointer-tags*', `*regexp-mon-doc-help-meta-tags*',
`*mon-help-mon-tags-alist*', `*mon-help-propertize-tags-triples*',
`mon-help-insert-tags', `mon-help-mon-tags', `mon-help-propertize-tags'.\n►►►"
  :type 'regexp
  :group 'mon-doc-help-utils)
;;
;;; :TEST-ME (search-backward-regexp *regexp-mon-doc-help-docstring-tags*)
;;  
;;; (progn (makunbound '*regexp-mon-doc-help-docstring-tags*)
;;;        (unintern "*regexp-mon-doc-help-docstring-tags*" obarray) )

;;; ==============================
;;; :TODO Build face and parsing routine for these. 
;;;       Should recognize "^;; " and "^;;; " by syntax _then_ regexp.
;;; :CREATED <Timestamp: #{2009-11-21T13:51:13-05:00Z}#{09476} - by MON>
(defcustom *regexp-mon-doc-help-comment-tags*
  ;;
  (regexp-opt (cadr (assq 'comment-tags  *mon-help-mon-tags-alist*)) t)
  ;;
  "*Regexp for locating \"meta-syntactic\" type tags.\n
For `help-mode' views of MON functions, in particular those from:
:FILE `mon-doc-help-utils.el'.\n
:KEYWORD-LISTS-IN `*mon-help-mon-tags-alist*'
Regexp generated from `comment-tags' key:
 \(cadr \(assoc 'docstr-tags  *mon-help-mon-tags-alist*\)\)\n
:NOTE These should be fontlocked in `emacs-lisp-mode'.\n
:SEE-ALSO `mon-help-insert-tags', `mon-help-mon-tags',
`mon-help-propertize-tags', `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
`*regexp-mon-doc-help-docstring-tags-TABLES*',
`*regexp-mon-doc-help-docstring-tags*', `*regexp-mon-doc-help-pointer-tags*',
`*regexp-mon-doc-help-meta-tags*', `*regexp-clean-mon-file-keywords*',
`*mon-help-propertize-tags-triples*'.\n►►►"
  :type 'regexp
  :group 'mon-doc-help-utils)
;;
;;; :TEST-ME (search-backward-regexp *regexp-mon-doc-help-comment-tags*)
;;
;;; (progn (makunbound '*regexp-mon-doc-help-comment-tags*)
;;;        (unintern "*regexp-mon-doc-help-comment-tags*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-21T13:51:15-05:00Z}#{09476} - by MON>
(defcustom *regexp-mon-doc-help-meta-tags*
  ;;
  (regexp-opt (cadr (assq 'meta-tags *mon-help-mon-tags-alist*)) t)
  ;;
  "*Regexp for locating \"meta-syntactic\" type tags.\n
For `help-mode' views of MON functions, in particular those from:
:FILE mon-doc-help-utils.el.\n
:KEYWORD-LISTS-IN `*mon-help-mon-tags-alist*'
Regexp generated from `meta-tags' key:\n
 \(cadr \(assoc 'docstr-tags  *mon-help-mon-tags-alist*\)\)\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags-DYNAMIC*',
`*regexp-mon-doc-help-docstring-tags-TABLES*',
`*regexp-mon-doc-help-docstring-tags*', `*regexp-mon-doc-help-comment-tags*',
`*regexp-mon-doc-help-pointer-tags*', `*mon-help-mon-tags-alist*',
`*mon-help-propertize-tags-triples*', `mon-help-propertize-tags',
`mon-help-insert-tags', `mon-help-mon-tags'.\n►►►"
  :type  'regexp
  :group 'mon-doc-help-utils)
;;
;;; (progn (makunbound '*regexp-mon-doc-help-meta-tags*)
;;;        (unintern "*regexp-mon-doc-help-meta-tags*" obarray) )

;;; ==============================
;;; :NOTE The constant `lisp-font-lock-keywords-2' in :FILE lisp/font-lock.el 
;;; has the regexp "\\<:\\sw+\\>" for identifying ``builtins''.
;;; However, that ``regexp'' depends on the syntax tables:
;;;  `emacs-lisp-mode-syntax-table' or `lisp-mode-syntax-table'
;;; Avoid worrying about `with-syntax-table' wrappers esp. wrt to the following
;;; two regexps:
;;;  `*regexp-mon-doc-help-docstring-tags-TABLES*'
;;;  `*regexp-mon-doc-help-docstring-tags-DYNAMIC*' 
;;; :CREATED <Timestamp: #{2010-02-19T18:10:56-05:00Z}#{10075} - by MON KEY>
(defcustom *regexp-mon-doc-help-builtin-dynamic-tags* 
  ;;
  "\\s-\\(\\s.\\sw[a-z-]+\\)\\s-"
  ;; whtspc.punct.downcase-char+.whtspc ; grp1 -> :some-symbol
  ;;
  "*Regexp for font-locking builtin keyword symbol tags.\n
Regexp has the form:\n
  whtspc.punct.downcase-char+.whtspc ; grp1 -> :some-symbol\n
For `help-mode' views of MON functions, in particular those from:
:FILE mon-doc-help-utils.el.\n
:NOTE When used alongside `*regexp-mon-doc-help-docstring-tags-TABLES*'
and `*regexp-mon-doc-help-docstring-tags-DYNAMIC*' and `case-fold-search' is
non-nil this regexp _will match_ but do so incorrectly by also matching
\" :FOO\" which is not what we want given the case-sensitivity of these other
two ``keyword'' regexps. Calling functions (or their expanders) should let-bind
`case-fold-search' if these associated regexps may come into play.\n
:FACE-FONT-LOCKING-WITH `mon-help-BUILTIN-tag'\n
:SEE-ALSO `*regexp-mon-doc-help-builtin-static-tags*',
`*mon-help-custom-faces-builtins-tags*'.\n►►►"
  :type 'regexp
  :group 'mon-doc-help-utils)
;;
;;; (progn (makunbound '*regexp-mon-doc-help-builtin-dynamic-tags*)
;;;        (unintern "*regexp-mon-doc-help-builtin-dynamic-tags*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-19T20:32:03-05:00Z}#{10076} - by MON KEY>
(defcustom *regexp-mon-doc-help-builtin-static-tags*
  ;;
  (regexp-opt *mon-help-custom-faces-builtins-tags*)
  ;;
  "*Regexp for font-locking builtin keyword symbol tags.\n
For `help-mode' views of MON functions, in particular those from:
:FILE mon-doc-help-utils.el\n
:NOTE When used alongside `*regexp-mon-doc-help-docstring-tags-TABLES*'
and `*regexp-mon-doc-help-docstring-tags-DYNAMIC*' and `case-fold-search' is
non-nil this regexp _will match_ but do so incorrectly by also matching
\" :FOO\" which is not what we want given the case-sensitivity of these other
two ``keyword'' regexps. Calling functions (or their expanders) should let-bind
`case-fold-search' if these associated regexps may come into play.\n
:KEYWORD-LISTS-IN `*mon-help-custom-faces-builtins-tags*'\n
:FACE-FONT-LOCKING-WITH `mon-help-BUILTIN-tag'\n
:SEE-ALSO `mon-help-BUILTIN-tag', `*mon-help-custom-faces-builtins-tags*',
`*regexp-mon-doc-help-builtin-dynamic-tags*'.\n►►►"
  :type 'regexp
  :group 'mon-doc-help-utils)
;;
;;; (progn (makunbound '*regexp-mon-doc-help-builtin-static-tags*)
;;;        (unintern "*regexp-mon-doc-help-builtin-static-tags*" obarray) )

;;; ==============================
;;; :NOTE The file lisp/emacs-lisp/unsafep.el
;;; :CHANGESET 1904
;;; :CREATED <Timestamp: #{2010-06-21T16:45:13-04:00Z}#{10251} - by MON KEY>
(defcustom *mon-help-side-effect-free* nil
  ;;
  "A list of functions the byte-code-optimizer considers \"side-effect-free\".\n
:EXAMPLE\n\n\(let \(\(some-random-elt
       \(elt *mon-help-side-effect-free*
            \(random \(length *mon-help-side-effect-free*\)\)\)\)\)
  `\(,some-random-elt ,\(when \(get some-random-elt 'side-effect-free\)
                        '\(side-effect-free t\)\)\)\)\n
:NOTE This list does not include the byte-code ops of the byte-code-optimizer,
e.g. those enumerated in: `byte-compile-side-effect-free-ops'.\n
 ,----
 | If you define a function which is side-effect free, update the code
 | in :FILE `byte-opt.el' which binds `side-effect-free-fns' and
 | `side-effect-and-error-free-fns' so that the compiler optimizer
 | knows about it.
 `----\n
:SEE info node `(elisp)Writing Emacs Primitives'\n
:SEE :FILE  emacs-lisp/unsafep.el lread.c\n
:SEE-ALSO `*mon-help-side-effect-and-error-free*', `cl-simple-funcs',
`cl-safe-funcs', `cl-simple-expr-p', `cl-simple-exprs-p', `cl-safe-expr-p',
`cl-const-expr-p', `cl-const-exprs-p', `*mon-help-pure-functions*',
`*mon-help-subrs*', `*mon-help-autoload-vars*', `*mon-help-permanent-locals*',
`*mon-help-risky-local-variables*', `*mon-function-object-types*',
`*mon-equality-or-predicate-function-types*', `*mon-non-mappable-object-types*',
`*mon-help-emacs-errors*', `*mon-help-byte-optimizer-vals*',
`mon-help-byte-code-vector-symbols', `mon-help-symbol-functions',
`mon-help-byte-optimizer-find', `mon-map-obarray-symbol-plist-props',
`cl-byte-compile-compiler-macro', `byte-compile-side-effect-and-error-free-ops',
`byte-compile-side-effect-free-ops', `byte-boolean-vars',
`byte-compile-delete-errors' `unsafep', `unsafep-function',
`unsafep-variable',`unsafep-progn', `unsafep-let', `unsafep-vars',
`safe-functions'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-doc-help-utils
  )
;;
(unless (and (intern-soft "*mon-help-side-effect-free*" obarray)
             (bound-and-true-p *mon-help-side-effect-free*))
  (setq *mon-help-side-effect-free*
        '(% * + - / /= 1+ 1- < <= = > >= abs acos append aref ash asin atan
            assoc assq boundp buffer-file-name buffer-local-variables
            buffer-modified-p buffer-substring byte-code-function-p capitalize
            car-less-than-car car cdr ceiling char-after char-before char-equal
            char-to-string char-width compare-strings concat
            coordinates-in-window-p copy-alist copy-sequence copy-marker cos
            count-lines decode-char decode-time default-boundp default-value
            documentation downcase elt encode-char exp expt encode-time
            error-message-string fboundp fceiling featurep ffloor
            file-directory-p file-exists-p file-locked-p file-name-absolute-p
            file-newer-than-file-p file-readable-p file-symlink-p
            file-writable-p float float-time floor format format-time-string
            frame-visible-p fround ftruncate get gethash get-buffer
            get-buffer-window getenv get-file-buffer hash-table-count
            int-to-string intern-soft keymap-parent length
            local-variable-if-set-p local-variable-p log log10 logand logb
            logior lognot logxor lsh langinfo make-list make-string make-symbol
            marker-buffer max member memq min mod multibyte-char-to-unibyte
            next-window nth nthcdr number-to-string parse-colon-path plist-get
            plist-member prefix-numeric-value previous-window prin1-to-string
            propertize degrees-to-radians radians-to-degrees rassq rassoc
            read-from-string regexp-quote region-beginning region-end reverse
            round sin sqrt string string< string= string-equal string-lessp
            string-to-char string-to-int string-to-number substring sxhash
            symbol-function symbol-name symbol-plist symbol-value
            string-make-unibyte string-make-multibyte string-as-multibyte
            string-as-unibyte string-to-multibyte tan truncate
            unibyte-char-to-multibyte upcase user-full-name user-login-name
            user-original-login-name user-variable-p vconcat window-buffer
            window-dedicated-p window-edges window-height window-hscroll
            window-minibuffer-p window-width zerop
            ;; :SEE :FILE lisp/cl-macs.el
            oddp evenp signum last butlast ldiff pairlis gcd lcm isqrt floor*
            ceiling* truncate* round* mod* rem* subseq list-length get* getf
            first second rest endp third fourth fifth sixth seventh eighth ninth
            tenth caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr
            caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar
            cdaddr cddaar cddadr cdddar cddddr))
  (custom-note-var-changed '*mon-help-side-effect-free*))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((some-random-elt
;; |        (elt *mon-help-side-effect-free*
;; |             (random (length *mon-help-side-effect-free*)))))
;; |   `(,some-random-elt ,(when (get some-random-elt 'side-effect-free)
;; |                         '(side-effect-free t))))
;; `----

;;; ==============================
;;; :CHANGESET 1904
;;; :CREATED <Timestamp: #{2010-06-21T16:45:59-04:00Z}#{10251} - by MON KEY>
(defcustom *mon-help-side-effect-and-error-free* nil
  ;;
  "List of functions the byte-code-optimizer considers \"side-effect-and-error-free\".\n
:EXAMPLE\n\n\(let \(\(some-random-elt
       \(elt *mon-help-side-effect-and-error-free*
            \(random \(length *mon-help-side-effect-and-error-free*\)\)\)\)\)
  `\(,some-random-elt ,\(when \(get some-random-elt 'side-effect-free\)
                        '\(side-effect-free t\)\)
                     ,\(when \(memq 'error-free \(symbol-plist some-random-elt\)\)
                        '\(error-free t\)\)\)\)\n
:NOTE This list does not include the byte-code ops of the byte-code-optimizer,
e.g. those enumerated in: `byte-compile-side-effect-and-error-free-ops'.\n
:SEE :FILE emacs-lisp/byte-opt.el emacs-lisp/unsafep.el lread.c\n
:SEE-ALSO `*mon-help-side-effect-free*', `cl-simple-funcs', `cl-safe-funcs',
`cl-simple-expr-p', `cl-simple-exprs-p', `cl-safe-expr-p', `cl-const-expr-p',
`cl-const-exprs-p',`*mon-help-pure-functions*', `*mon-help-subrs*',
`*mon-help-autoload-vars*', `*mon-help-permanent-locals*',`
`*mon-help-risky-local-variables*', `*mon-function-object-types*',
`*mon-equality-or-predicate-function-types*', `*mon-non-mappable-object-types*',
`*mon-help-emacs-errors*', `*mon-help-byte-optimizer-vals*',
`mon-help-byte-optimizer-find',` `mon-help-byte-code-vector-symbols',
`byte-boolean-vars', `mon-help-permanent-locals-find',
`byte-optimize-form-code-walker', `byte-compile-delete-errors',
`byte-compile-side-effect-and-error-free-ops',
`byte-compile-side-effect-free-ops', `cl-byte-compile-compiler-macro',
`unsafep', `unsafep-function', `unsafep-variable', `unsafep-progn',
`unsafep-let', `unsafep-vars', `safe-functions'.\n►►►"
  :type  '(repeat symbol)
  :group 'mon-doc-help-utils)
;;
(unless (and (intern-soft "*mon-help-side-effect-and-error-free*" obarray)
             (bound-and-true-p *mon-help-side-effect-and-error-free*))
  (setq *mon-help-side-effect-and-error-free*
        '(arrayp atom bobp bolp bool-vector-p buffer-end buffer-list buffer-size
                 buffer-string bufferp car-safe case-table-p cdr-safe char-or-string-p
                 characterp charsetp commandp cons consp current-buffer
                 current-global-map current-indentation current-local-map
                 current-minor-mode-maps current-time current-time-string
                 current-time-zone eobp eolp eq equal eventp floatp following-char
                 framep get-largest-window get-lru-window hash-table-p identity ignore
                 integerp integer-or-marker-p interactive-p invocation-directory
                 invocation-name keymapp line-beginning-position line-end-position list
                 listp make-marker mark mark-marker markerp max-char memory-limit
                 minibuffer-window mouse-movement-p natnump nlistp not null
                 number-or-marker-p numberp one-window-p overlayp point point-marker
                 point-min point-max preceding-char primary-charset processp
                 recent-keys recursion-depth safe-length selected-frame selected-window
                 sequencep standard-case-table standard-syntax-table stringp subrp
                 symbolp syntax-table syntax-table-p this-command-keys
                 this-command-keys-vector this-single-command-keys
                 this-single-command-raw-keys user-real-login-name user-real-uid
                 user-uid vector vectorp visible-frame-list wholenump
                 window-configuration-p window-live-p windowp
                 ;; :SEE :FILE  lisp/cl-macs.el
                 eql floatp-safe list* subst acons equalp random-state-p copy-tree sublis
                 ))
  (custom-note-var-changed '*mon-help-side-effect-and-error-free*))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((some-random-elt
;;|        (elt *mon-help-side-effect-and-error-free*
;;|             (random (length *mon-help-side-effect-and-error-free*)))))
;;|   `(,some-random-elt ,(when (get some-random-elt 'side-effect-free)
;;|                         '(side-effect-free t))
;;|                      ,(when (memq 'error-free (symbol-plist some-random-elt))
;;|                         '(error-free t))))
;;`----


;;; ==============================
;;; :CHANGESET 1908
;;; :CREATED <Timestamp: #{2010-06-22T11:59:47-04:00Z}#{10252} - by MON KEY>
(defcustom *mon-help-pure-functions* nil
  ;;
  "List of functions the byte-code-optimizer considers \"pure\".\n
These have the plist property 'pure indicating they are side-effect free
functions whose values depend only on their arguments. For these functions,
calls with constant arguments can be evaluated at compile time. This may shift
run time errors to compile time.\n
The `byte-optimize-form-code-walker' looks for this property as its final
conditional clause \(along with the predicates `byte-optimize-all-constp'
`byte-compile-constp'\) when making the determination that no args to the
function can be considered to be for-effect, even if the called function is
for-effect, because it does not know anything about that function.\n
:NOTE All but two of the elements of this list are also present in the list
enumerated by `*mon-help-side-effect-free*' and are considered by the
byte-compiler to be side-effect-free.\n
:EXAMPLE\n
\(intersection *mon-help-side-effect-free* *mon-help-pure-functions*\)\n
\(set-difference *mon-help-pure-functions* *mon-help-side-effect-free*\)\n
\(let \(purity-check\)
  \(dolist \(pf *mon-help-pure-functions* 
              \(setq purity-check \(nreverse purity-check\)\)\)
    \(let \(\(pf-pl \(reverse \(symbol-plist pf\)\)\)\)
      \(push `\(,pf \(:pure ,\(plist-get pf-pl 'pure\)
                   :side-effect-free ,\(plist-get pf-pl 'side-effect-free\)\)\)
            purity-check\)\)\)\)\n
:SEE :FILE `byte-opt.el' lread.c\n
:SEE-ALSO `*mon-help-side-effect-free*',
`*mon-help-side-effect-and-error-free*', `*mon-help-subrs*',
`*mon-help-autoload-vars*', `*mon-help-permanent-locals*',
`*mon-help-risky-local-variables*', `*mon-help-byte-optimizer-vals*',
`mon-help-byte-optimizer-find', `mon-help-byte-code-vector-symbols',
`byte-boolean-vars', `*mon-function-object-types*',
`*mon-equality-or-predicate-function-types*', `*mon-non-mappable-object-types*',
`*mon-help-emacs-errors*'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-doc-help-utils)
;;
(unless (and (intern-soft "*mon-help-pure-functions*" obarray)
             (bound-and-true-p *mon-help-pure-functions*))
  (setq *mon-help-pure-functions*
        '(concat symbol-name regexp-opt regexp-quote string-to-syntax))
  (custom-note-var-changed '*mon-help-pure-functions*))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let (purity-check)
;;|   (dolist (pf *mon-help-pure-functions* 
;;|               (setq purity-check (nreverse purity-check)))
;;|     (let ((pf-pl (reverse (symbol-plist pf))))
;;|       (push `(,pf (:pure ,(plist-get pf-pl 'pure)
;;|                    :side-effect-free ,(plist-get pf-pl 'side-effect-free)))
;;|             purity-check))))
;;`----


;;; ==============================
;; \(get \(car \(memq 'indirect-function *mon-help-subrs*\)\) 'elint-args\)\n
;; \(car \(memq 'indirect-function *mon-help-subrs*\)\)
;;;
;;; ,---- :NOTE No longer (or not currently) applicable:
;;; | Elts of this list have the property `elint-args` whenever
;;; | `elint-put-function-args' has been evalauted by `elint-scan-doc-file' which
;;; | normally happens whenever `elint-initialize' is invoked.\n
;;; | However, when the predicate `IS-MON-SYSTEM-P' returns non-nil the function
;;; | `mon-help-utils-loadtime' arranges to evaluate `elint-scan-doc-file' at loadtime
;;; | such that the following should return non-nil regardless of whether
;;; | `elint-initialize' has been evaluated:\n
;;; `----
;;;
;;; :CHANGESET 2019
;;; :CREATED <Timestamp: #{2010-07-31T19:13:24-04:00Z}#{10306} - by MON KEY>
(defvar *mon-help-subrs* (make-hash-table :size 1097 :test 'equal) ;; (mon-next-almost-prime 1095)
   "Hash table of all built-in Emacs functions (i.e. all subrs).\n
:EXAMPLE\n\n\(gethash \"region-beginning\" *mon-help-subrs*\)\n
:NOTE This var holds a hashtable to make it possible for `subrp' like checks
without the need to intern the putative symbol to standard `obarray'.\n
:SEE-ALSO `elint-find-builtins', `elint-find-builtin-args',
`elint-standard-variables', `elint-builtin-variables',
`*mon-help-autoload-vars*', `*mon-help-side-effect-free*',
`*mon-help-side-effect-and-error-free*', `*mon-help-pure-functions*',
`*mon-help-permanent-locals*', `*mon-help-byte-optimizer-vals*',
`mon-help-byte-code-vector-symbols', `mon-help-byte-optimizer-find',
`byte-boolean-vars'.\n►►►"
   ;; :type '(hashtable)  ;; :NOTE hashtable is not a custom type. :-{
   ;; :group 'mon-doc-help-utils
   )

;;; ==============================
;;; :CHANGESET 2067
;;; :CREATED <Timestamp: #{2010-08-13T16:21:05-04:00Z}#{10325} - by MON KEY>
(defvar *mon-help-subrs-false* (make-hash-table :test 'equal :size 149) ;; (mon-next-almost-prime 147)
  "Hash table of all symbols which `defalias' an Emacs built-in.\n
Hash keys map to the symbol-name of a false subr.
Hash value is a consed pair the car is the `sxhash' of the false subr's
`symbol-name' the cdr is a symbol.\n
Each hash entry has the form:\n
 \(<SYMBOL-NAME> \(<SXHASH> . <SYMBOL>\)\n
:EXAMPLE\n\n
:SEE-ALSO `mon-map-subrs-and-hash', `elint-find-builtins',
`elint-find-builtin-args', `elint-standard-variables',
`elint-builtin-variables', `*mon-help-autoload-vars*',
`*mon-help-side-effect-free*', `*mon-help-side-effect-and-error-free*',
`*mon-help-pure-functions*', `*mon-help-permanent-locals*',
`*mon-help-byte-optimizer-vals*', `mon-help-byte-code-vector-symbols',
`mon-help-byte-optimizer-find', `byte-boolean-vars'.\n►►►"
  ;; :type '(hashtable)  ;; :NOTE hashtable is not a custom type. :-{
  ;; :group 'mon-doc-help-utils
  )

;;; ==============================
;;; :CHANGESET 2261
;;; :CREATED <Timestamp: #{2010-11-02T19:42:40-04:00Z}#{10442} - by MON KEY>
(defun mon-map-subrs-and-hash ()
  ""
  ;; The true subrs should get written out to a file. 
  ;;; They don't change that often.
  (let ((false-gthr *mon-help-subrs-false*)
        (true-gthr  *mon-help-subrs*))
    (mapatoms #'(lambda (sbrp) 
                  (when (and (fboundp sbrp)
                             (symbol-function sbrp)
                             (indirect-function sbrp t)
                             (subrp (indirect-function sbrp t))
                             (let* ((sn (symbol-name sbrp))
                                    (sx (cons (sxhash sn) sbrp)))
                               (if (eq (symbol-function sbrp)
                                       (indirect-function sbrp))
                                   (puthash sn sx true-gthr)
                                 (puthash sn sx false-gthr)))))))
    `(,false-gthr ,true-gthr)))


;;; ==============================
;;; :CHANGESET 2019
;;; :CREATED <Timestamp: #{2010-07-31T19:16:15-04:00Z}#{10306} - by MON KEY>
;; (defvar *mon-help-autoload-vars* nil
;;   "A list of all autoloaded variables in environment at init.\n
;; :EXAMPLE\n\n\(car \(memq 'tags-table-list *mon-help-autoload-vars*\)\)\n
;; :SEE :FILE loaddefs.el
;; :SEE-ALSO `elint-find-autoloaded-variables', `elint-autoloaded-variables',
;; `*mon-help-subrs*', `*mon-help-side-effect-free*',
;; `*mon-help-side-effect-and-error-free*', `*mon-help-pure-functions*',
;; `*mon-help-permanent-locals*', `*mon-help-byte-optimizer-vals*',
;; `byte-boolean-vars'.\n►►►")
;;

;;; ==============================
;;; :CHANGESET 2067
;;; :CREATED <Timestamp: #{2010-08-13T13:41:11-04:00Z}#{10325} - by MON KEY>
;; *mon-help-subrs-false*
;; *mon-help-autoload-vars*
;; *mon-help-subrs*
;; (setq elint-builtin-variables (elint-scan-doc-file)
;;	  elint-autoloaded-variables (elint-find-autoloaded-variables))

;; (when (and (intern-soft "IS-MON-SYSTEM-P" obarray)
;; 	   (intern-soft "IS-MON-P" obarray)
;;            (bound-and-true-p IS-MON-P))
;;   (custom-set-variables '(elint-scan-preloaded t t)))

;; (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) 
;;            (bound-and-true-p IS-MON-SYSTEM-P))
;;   (when (not (and (intern-soft "IS-BUG-P" obarray)
;;                   (bound-and-true-p IS-BUG-P)))
;;     (elint-scan-doc-file)
;;     (push ":EVALUATED `elint-scan-doc-file'" myb-msg-usr)
;;     (setq w-msg-user t)))


;; (unless (bound-and-true-p *mon-help-subrs*)
;;   (if (and (intern-soft "elint-builtin-variables")
;;            (bound-and-true-p elint-builtin-variables))
;;       (setq *mon-help-subrs* elint-builtin-variables)
;;     (setq *mon-help-subrs* (elint-find-builtins)))
;;   (if (and (intern-soft "elint-standard-variables")
;;            (bound-and-true-p elint-standard-variables))
;;       (setq *mon-help-subrs* (append *mon-help-subrs* elint-standard-variables))))

;; (unless (bound-and-true-p *mon-help-autoload-vars*)
;;   (if (bound-and-true-p elint-autoloaded-variables)
;;       ;;elint-find-autoloaded-variables
;;       (setq *mon-help-autoload-vars* elint-autoloaded-variables)
;;     (setq *mon-help-autoload-vars* (elint-find-autoloaded-variables))))


;;; ==============================
;;; :CHANGESET 2063
;;; :CREATED <Timestamp: #{2010-08-10T21:11:50-04:00Z}#{10322} - by MON KEY>
(defvar *mon-help-byte-optimizer-vals* nil
  ;;
  "A hash-table of all symbols with a permanent-local property in environment.\n
:EXAMPLE\n\n\(gethash 'posn-point *mon-help-byte-optimizer-vals*\)\n
To update the hash-table evaluate:\n
\(mon-help-byte-optimizer-find\)\n
:NOTE Bound at loatdtime when the predicate `IS-MON-SYSTEM-P' is non-nil with
`mon-help-byte-optimier-find' by `mon-after-mon-utils-loadtime'.\n
:SEE-ALSO `mon-help-permanent-locals-find',
`mon-map-obarray-symbol-plist-props', `*mon-help-subrs*',
`*mon-help-side-effect-free*', `*mon-help-side-effect-and-error-free*',
`*mon-help-pure-functions*', `*mon-function-object-types*',
`*mon-help-emacs-errors*', `*mon-equality-or-predicate-function-types*',
`*mon-non-mappable-object-types*', `byte-boolean-vars'.\n►►►"
  ;; :type '(hashtable)  ;; :NOTE hashtable is not a custom type. :-{
  ;; :group 'mon-doc-help-utils
  )

;;; ==============================
;;; :CHANGESET 2245
;;; :CREATED <Timestamp: #{2010-10-30T13:50:18-04:00Z}#{10436} - by MON KEY>
(defcustom *mon-help-risky-local-variables*
  ;;
  '(after-load-alist buffer-auto-save-file-name buffer-file-name
    buffer-file-truename buffer-undo-list debugger default-text-properties eval
    exec-directory exec-path file-name-handler-alist frame-title-format
    global-mode-string header-line-format icon-title-format inhibit-quit
    load-path max-lisp-eval-depth max-specpdl-size minor-mode-map-alist
    minor-mode-overriding-map-alist mode-line-format mode-name
    overriding-local-map overriding-terminal-local-map process-environment
    standard-input standard-output unread-command-events)
  ;;
  "A list of symbols with `risky-local-variable` properties non-nil by default.\n
:EXAMPLE\n\n\(memq unread-command-events *mon-help-risky-local-variables*\)\n
:SEE :FILE lisp/files.el
:SEE-ALSO `mon-map-obarray-symbol-plist-props',
`mon-help-permanent-locals-find', `*mon-help-permanent-locals*',
`*mon-help-subrs*', `*mon-help-side-effect-free*',
`*mon-help-side-effect-and-error-free*', `*mon-help-pure-functions*',
`*mon-help-byte-optimizer-vals*', `*mon-function-object-types*',
`*mon-equality-or-predicate-function-types*',
`*mon-non-mappable-object-types*', `*mon-help-emacs-errors*'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-doc-help-utils)

;;; ==============================
;;; :CHANGESET 2031
;;; :CREATED <Timestamp: #{2010-08-07T15:07:50-04:00Z}#{10316} - by MON KEY>
(defvar *mon-help-permanent-locals* nil ;; :DECLARED-SPECIAL
  ;;
  "A list of all symbols with a permanent-local property in environment.\n
:EXAMPLE\n\n\(car \(memq 'comint-input-ring *mon-help-permanent-locals*\)\)\n
:NOTE Bound at loatdtime when the predicate `IS-MON-SYSTEM-P' is non-nil with
`mon-help-permanent-locals-find' by `mon-after-mon-utils-loadtime'.\n
:NOTE Of particular interest is the `custom-local-buffer' property which
`custom-set-default' abuses.\n
:SEE-ALSO `mon-help-permanent-locals-find', `mon-help-byte-optimizer-find',
`byte-boolean-vars', `*mon-help-risky-local-variables*',
`mon-map-obarray-symbol-plist-props', `*mon-help-subrs*',
`*mon-help-side-effect-free*', `*mon-help-side-effect-and-error-free*',
`*mon-help-pure-functions*', `*mon-help-byte-optimizer-vals*',
`*mon-function-object-types*', `*mon-equality-or-predicate-function-types*',
`*mon-help-emacs-errors*'.\n►►►")

;;; ==============================
;;; :CHANGESET 2031
;;; :CREATED <Timestamp: #{2010-08-07T15:16:31-04:00Z}#{10316} - by MON KEY>
(defun mon-help-permanent-locals-find (&optional regenerate w-msg-user)
  "Return list of symbols in `obarray' having a permanent local property.\n
When optional arg REGENERATE in ommitted return value is the value of the
variable `*mon-help-permanent-locals*'. If that is null bind it now.\n
When REGENERATE is non-nil regenerate the `*mon-help-permanent-locals*' list and
return its value.\n
When W-MSG-USER is non-nil or variable `*mon-help-permanent-locals*' is null,
or optional arg REGENERATE is non-nil and variable _is_ bound, message user that
list was \(re\)bound but do not return its value.\n
:EXAMPLE\n\n\(mon-help-permanent-locals-find\)\n
:NOTE When the predicate `IS-MON-SYSTEM-P' is non nil this function is called at
loadtime by `mon-after-mon-utils-loadtime' which wraps it in an
`eval-after-load' form such that whenever the :FILE mon-doc-help-utils.el is
\(re\)loaded a snapshot of the current symbols having permanent local props is
regenerated.\n 
:NOTE We take particular interest is the `custom-local-buffer' property which
`custom-set-default' is keen to abuse.\n
:SEE-ALSO `*mon-help-permanent-locals*', `*mon-help-risky-local-variables*',
`mon-map-obarray-symbol-plist-props', `*mon-help-byte-optimizer-vals*',
`mon-help-byte-optimizer-find', `mon-help-byte-code-vector-symbols'.\n►►►"
  (let* (mhplf-pl
         bnd-or-rgn-msg
         (mhplf-msg-p 
          (if (not (intern-soft "*mon-help-permanent-locals*" obarray))
              ;; Its void
              (setq bnd-or-rgn-msg 'msg-void)
            (if (bound-and-true-p *mon-help-permanent-locals*)
                ;; Its bound
                (cond (regenerate (setq bnd-or-rgn-msg 'regen-bnd))
                      (w-msg-user (setq bnd-or-rgn-msg 'msg-bnd)))
              ;; Its not bound
              (cond ((not w-msg-user) (setq bnd-or-rgn-msg 'msg-anyway))
                    (w-msg-user (setq bnd-or-rgn-msg 'msg-unbnd)))))))
    (when mhplf-msg-p
      (mapatoms #'(lambda (ma-sym)
                    (let ((ma-sym-plist (symbol-plist ma-sym)))
                      (when (plist-get ma-sym-plist 'permanent-local)
                        (push ma-sym mhplf-pl))))))
    (if mhplf-msg-p 
        (progn 
          (setq *mon-help-permanent-locals* mhplf-pl)
          (mon-message :msg-spec
                       `(":FUNCTION `mon-help-permanent-locals-find' "
                         "-- variable `*mon-help-permanent-locals*' "
                         ,(case bnd-or-rgn-msg
                            (regen-bnd  "was non-nil and regenerated")
                            (msg-bnd     "was non-nil now rebound")
                            (msg-anyway 
                             (concat "was null so bound it reinvoke "
                                     "`mon-help-permanent-locals-find' "
                                     "or evaluate `*mon-help-permanent-locals*' "
                                     "for return value"))
                            (msg-unbnd   "was null so bound it")
                            (msg-void    "was void it is now bound")))))
      *mon-help-permanent-locals*)))
;; 
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (makunbound '*mon-help-permanent-locals*)
;; | (unintern "*mon-help-permanent-locals*" obarray)
;; | (mon-help-permanent-locals-find)    
;; | (makunbound '*mon-help-permanent-locals*)
;; | (mon-help-permanent-locals-find nil t)
;; | (mon-help-permanent-locals-find t)
;; | (mon-help-permanent-locals-find)
;; `----

;;; ==============================
;;; :CHANGESET 2063
;;; :CREATED <Timestamp: #{2010-08-10T20:22:22-04:00Z}#{10322} - by MON KEY>
(defun mon-help-byte-optimizer-find ()
  "Hash symbols in `obarray' with a `byte-optimizer` property.\n
Return value is the variable `*mon-help-byte-optimizer-vals*' rehashed.\n
Its key value pairs map as follows:\n
 <SYMBOL> <BYTE-OPTERMIZER-PREPERTY>\n
:EXAMPLE\n\n\(mon-help-byte-optimizer-find\)\n
\(gethash 'caar *mon-help-byte-optimizer-vals*\)\n
\(hash-table-p *mon-help-byte-optimizer-vals*\)\n
\(hash-table-count *mon-help-byte-optimizer-vals*\)\n
\(hash-table-size *mon-help-byte-optimizer-vals*\)\n
\(hash-table-test *mon-help-byte-optimizer-vals*\)\n
\(hash-table-weakness *mon-help-byte-optimizer-vals*\)\n
:NOTE When the predicate `IS-MON-SYSTEM-P' is non-nil evaluated at loadtime by
by `mon-after-mon-utils-loadtime' which wraps it in an `eval-after-load' form
such that whenever the :FILE mon-doc-help-utils.el is \(re\)loaded a snapshot of
the current symbols in `obarray' having byte-optimizer props is rehashed.\n
:SEE-ALSO `mon-map-obarray-symbol-plist-props', `byte-boolean-vars',
`mon-help-permanent-locals-find', `mon-help-byte-code-vector-symbols',
`*mon-help-subrs*', `*mon-help-side-effect-free*',
`*mon-help-side-effect-and-error-free*', `*mon-help-pure-functions*',
`*mon-help-permanent-locals*', `*mon-function-object-types*',
`*mon-equality-or-predicate-function-types*', `*mon-non-mappable-object-types*',
`*mon-help-risky-local-variables*', `mon-help-symbol-functions'.\n►►►"
  (unless (and (intern-soft "*mon-help-byte-optimizer-vals*" obarray)
               (bound-and-true-p *mon-help-byte-optimizer-vals*)
               (hash-table-p *mon-help-byte-optimizer-vals*))
    (setq *mon-help-byte-optimizer-vals* 
          (make-hash-table :test 'eq :weakness 'key)))
  (mapatoms #'(lambda (bo)
                (when (get bo 'byte-optimizer)
                  (puthash bo (get bo 'byte-optimizer) 
                           *mon-help-byte-optimizer-vals*))))
  *mon-help-byte-optimizer-vals*)
;;
;; :TEST-ME (mon-help-byte-optimizer-find)

 
;;; ==============================
;; :MON-HELP-FACES
;;; ==============================

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-21T16:41:30-05:00Z}#{09476} - by MON>
(defface mon-help-KEY-tag
    '((((class color) (min-colors 88))
       (:foreground "light steel blue" :weight extrabold)) )
  "*A mon-help-symbol mon-help-symbol KEY face.\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-docstring-tags*'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-COMMENT-tag',
`mon-help-URL-wrap-tag', `mon-help-INNER-KEY-tag' `mon-help-DYNATAB-tag',
`mon-help-OLAY-RESULT', `mon-help-OLAY-RESULT-string-show',
`mon-help-OLAY-RESULT-match-show'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-KEY-tag)
;;
;;; (progn (makunbound 'mon-help-KEY-tag) (unintern "mon-help-KEY-tag" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-21T17:03:49-05:00Z}#{09476} - by MON>
(defface mon-help-DYNATAB-tag
    '((t :inherit mon-help-KEY-tag :foreground "cadet blue"))
  "*A mon-help-symbol mon-help-symbol DYNAMIC and TABLE tag face.\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-docstring-tags-TABLES*'\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-docstring-tags-DYNAMIC*'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-INNER-KEY-tag',
`mon-help-COMMENT-tag', `mon-help-URL-wrap-tag', `mon-help-DYNATAB-tag',
`mon-help-KEY-tag',  `mon-help-OLAY-RESULT',
`mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-DYNATAB-tag)
;;
;;; (progn (makunbound 'mon-help-DYNATAB-tag) (unintern "mon-help-DYNATAB-tag" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-21T17:03:30-05:00Z}#{09476} - by MON>
(defface mon-help-META-tag
    '((t :inherit mon-help-KEY-tag :foreground "sky blue"))
  "*A mon-help-symbol META tag face.\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-meta-tags*'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag',
`mon-help-KEY-tag', `mon-help-COMMENT-tag', `mon-help-URL-wrap-tag',
`mon-help-INNER-KEY-tag',  `mon-help-OLAY-RESULT',
`mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-PNTR-tag)
;;
;;; (progn (makunbound 'mon-help-META-tag) (unintern "mon-help-META-tag" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-21T17:37:44-05:00Z}#{09476} - by MON>
(defface mon-help-PNTR-tag
    '((t :inherit mon-help-KEY-tag :foreground "powder blue"))
  "*A mon-help-symbol pointer tag face.\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-pointer-tags*'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-INNER-KEY-tag',
`mon-help-COMMENT-tag', `mon-help-URL-wrap-tag', `mon-help-DYNATAB-tag',
`mon-help-KEY-tag',  `mon-help-OLAY-RESULT',
`mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-PNTR-tag)
;;
;;; (progn (makunbound 'mon-help-PNTR-tag) (unintern "mon-help-PNTR-tag" obarray))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-15T20:19:56-05:00Z}#{10072} - by MON KEY>
 (defface mon-help-COMMENT-tag
    '((t 
       :inherit mon-help-KEY-tag 
       :weight bold ;; :slant oblique
       :foreground "DarkSlateGray3"))
   "*A mon-help-symbol comment tag face.\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-comment-tags*'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag',
`mon-help-KEY-tag', `mon-help-COMMENT-tag', `mon-help-URL-wrap-tag',
`mon-help-INNER-KEY-tag', `mon-help-OLAY-RESULT',
`mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show'.\n►►►"
   :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-COMMENT-tag)
;;
;;; (progn (makunbound 'mon-help-COMMENT-tag) (unintern "mon-help-COMMENT-tag" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-15T18:03:30-05:00Z}#{10071} - by MON KEY>
(defface mon-help-INNER-KEY-tag
    '((t 
       :inherit mon-help-KEY-tag 
       :weight bold
       :foreground "PaleTurquoise2"))
  "*A mon-help-symbol INNER-KEYcomment tag face.\n
:KEYWORD-REGEXPS-IN `'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag',
`mon-help-KEY-tag', `mon-help-COMMENT-tag', `mon-help-URL-wrap-tag',
`mon-help-INNER-KEY-tag', `mon-help-OLAY-RESULT',
`mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-INNER-KEY-tag)
;;
;;; (progn (makunbound 'mon-help-INNER-KEY-tag) (unintern "mon-help-INNER-KEY-tag" obarray))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-15T19:31:22-05:00Z}#{10072} - by MON KEY>
(defface mon-help-URL-wrap-tag
    '((t 
       :inherit button 
       :slant oblique 
       :weight semi-bold 
       :underline nil))
  "*A mon-help-symbol URL tag face.\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-docstring-tags-URL*'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag',
`mon-help-KEY-tag', `mon-help-COMMENT-tag', `mon-help-URL-wrap-tag',
`mon-help-INNER-KEY-tag', `mon-help-OLAY-RESULT',
`mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-URL-wrap-tag)
;;
;;; (progn (makunbound 'mon-help-URL-wrap-tag) (unintern "mon-help-URL-wrap-tag" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-19T17:19:29-05:00Z}#{10075} - by MON KEY>
(defface mon-help-BUILTIN-tag
    '((t 
       :inherit font-lock-builtin-face 
       :weight semi-light
       :background "gray2"
       :overline "DodgerBlue4"))
  "*A mon-help-symbol BUILTIN tag face.\n
:KEYWORD-LISTS-IN `*mon-help-custom-faces-builtins-tags*'
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-builtin-dynamic-tags*'.\n
:KEYWORD-REGEXPS-IN `*regexp-mon-doc-help-builtin-static-tags*'\n
:SEE-ALSO `mon-help-META-tag', `mon-help-PNTR-tag', `mon-help-DYNATAB-tag',
`mon-help-KEY-tag', `mon-help-COMMENT-tag', `mon-help-URL-wrap-tag',
`mon-help-INNER-KEY-tag', `mon-help-OLAY-RESULT',
`mon-help-OLAY-RESULT-string-show', `mon-help-OLAY-RESULT-match-show'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-BUILTIN-tag)
;;
;;; (progn (makunbound 'mon-help-BUILTIN-tag) (unintern "mon-help-BUILTIN-tag" obarray) )

;;; SlateBlue3 AntiqueWhite3
;; :foreground "PaleTurquoise2")
;; :foreground "DeepSkyBlue4"))
;; :foreground "DarkSlateGray3"))
;;; :AliceBlue

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-20T15:24:32-05:00Z}#{10076} - by MON KEY>
(defface mon-help-OLAY-RESULT
    '((t 
      :foreground "lime green"
      :background "black" 
      :box))
  "*A mon-help overlay face for font-locks evaluated examples in help buffers.\n
:FACE-INHERITED-BY `mon-help-OLAY-RESULT-string-show'
:FACE-INHERITED-BY `mon-help-OLAY-RESULT-match-show'
:CALLED-BY `mon-help-overlay-for-example'
:CALLED-BY `mon-help-overlay-result'\n
:SEE-ALSO `mon-help-overlay-on-region', `mon-help-find-result-for-overlay'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-OLAY-RESULT)
;;
;;; (progn (makunbound 'mon-help-OLAY-RESULT) 
;;;        (unintern "mon-help-OLAY-RESULT" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-20T15:18:06-05:00Z}#{10076} - by MON KEY>
(defface mon-help-OLAY-RESULT-string-show
    '((t 
       :inherit 'mon-help-OLAY-RESULT
       :weight bold
       :box (:line-width 1
             :color "lime green"
             :style raised-button)))
  "*A mon-help overlay face font-locks evaluated examples in help buffers.\n
:FACE-INHERITS-FROM `mon-help-OLAY-RESULT'
:CALLED-BY `mon-help-overlay-for-example'
:CALLED-BY `mon-help-overlay-result'\n
:SEE-ALSO `mon-help-OLAY-RESULT-match-show'.\n►►►"
:group 'mon-doc-help-utils-faces)

;;
;;; :TEST-ME (describe-face 'mon-help-OLAY-RESULT-string-show)
;;
;;; (progn (makunbound 'mon-help-OLAY-RESULT-string-show)
;;;        (unintern "mon-help-OLAY-RESULT-string-show" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-20T15:18:03-05:00Z}#{10076} - by MON KEY>
(defface mon-help-OLAY-RESULT-match-show
    '((t 
       :inherit 'mon-help-OLAY-RESULT
       :weight ultra-bold 
       :box (:line-width 3 
             :color "yellow1" 
             :style released-button)))
  "*A mon-help overlay face for font-locks evaluated examples in help buffers.\n
:FACE-INHERITS-FROM `mon-help-OLAY-RESULT'
:CALLED-BY `mon-help-overlay-for-example'
:CALLED-BY `mon-help-overlay-result'\n
:SEE-ALSO `mon-help-OLAY-RESULT-string-show', `mon-help-overlay-on-region',
`mon-help-find-result-for-overlay'.\n►►►"
  :group 'mon-doc-help-utils-faces)
;;
;;; :TEST-ME (describe-face 'mon-help-OLAY-RESULT-match-show)
;;
;;; (progn (makunbound 'mon-help-OLAY-RESULT-match-show) 
;;;        (unintern "mon-help-OLAY-RESULT-match-show" obarray) )

 
;;; ==============================
;;; :CHANGESET 2394
;;; :CREATED <Timestamp: #{2011-01-15T13:29:42-05:00Z}#{11026} - by MON KEY>
(defcustom *mon-help-propertize-tags-triples*
  ;; :NOTE ( <REGEXP> <MATCH-GRP> <FACE>)  ;COLOR
  `((,*regexp-mon-doc-help-docstring-tags*         0 mon-help-KEY-tag)     ; "light steel blue"
    (,*regexp-mon-doc-help-meta-tags*              0 mon-help-META-tag)    ; "sky blue"   
    (,*regexp-mon-doc-help-pointer-tags*           2 mon-help-PNTR-tag)    ; "powder blue"
    (,*regexp-mon-doc-help-docstring-tags-TABLES*  2 mon-help-DYNATAB-tag) ; "cadet blue"        
    (,*regexp-mon-doc-help-docstring-tags-DYNAMIC* 0 mon-help-DYNATAB-tag) ; "cadet blue"
    ;;  &rest MORE-TRIPLES
    ;; (*regexp-mon-doc-help-comment-tags*           0 mon-help-COMMENT-tag)   ;"DarkSlateGray3"
    ;; (*regexp-mon-doc-help-comment-tags*           0 mon-help-INNER-KEY-tag) ;"PaleTurquoise2"
    ;; (*regexp-mon-doc-help-docstring-tags-URL*     2 mon-help-URL-wrap-tag)  ;"LightSkyBlue"
    ;; (*regexp-mon-doc-help-docstring-tags-URL*     4 mon-help-URL-wrap-tag)  ;"LightSkyBlue"
    ;; (*regexp-mon-doc-help-builtin-dynamic-tags*   1 mon-help-BUILTIN-tag)   ;"SteelBlue"
    ;; (*regexp-mon-doc-help-builtin-static-tags*    0 mon-help-BUILTIN-tag)   ;"SteelBlue"
    )
  "List of triples for `mon-help-propertize-tags' to font-lock with.\n
Elements of list have the form:\n
 \( <REGEXP> <MATCH-GRP> <FACE> \)\n
- <REGEXP> is a regexp to match for font-locking with <FACE>;
- <MATCH-GRP> is a match-group (an integer index) for matches <REGEXP> with;
- <FACE> is a face to font-lock <MATCH-GRP> of <REGEXP> with;\n
:EXAMPLE\n
`\(,*regexp-mon-doc-help-docstring-tags-DYNAMIC* 0 mon-help-DYNATAB-tag\)\n
:SEE-ALSO `*regexp-mon-doc-help-docstring-tags*',`mon-help-KEY-tag',
`*regexp-mon-doc-help-meta-tags*', `mon-help-META-tag',
`*regexp-mon-doc-help-pointer-tags*', `mon-help-PNTR-tag',
`*regexp-mon-doc-help-docstring-tags-TABLES*',`mon-help-DYNATAB-tag',
`*regexp-mon-doc-help-docstring-tags-DYNAMIC*', `mon-help-DYNATAB-tag'.\n►►►"
  :type '(repeat (list (regexp  :tag "<REGEXP>")
                       (integer :tag "<MATCH-GRP")
                       (face    :tag "<FACE>")))
  ;; :group 'mon-doc-help-utils-faces
  :group 'mon-doc-help-utils)

;;; ==============================
;;; :PREFIX "mhpt-"
;;; :CREATED <Timestamp: #{2009-11-21T18:15:49-05:00Z}#{09476} - by MON>
(defun mon-help-propertize-tags (&rest more-triples)
  "Propertize mon-help-tags with face values.\n
For advising `help-mode' of some more things to font-lock.\n
Font-locks values of `*mon-help-propertize-tags-triples*'.\n
When MORE-TRIPLES is non-nil args are three valued lists of:\n
 - A regular expression or symbol which evaluates to one;
 - The match group to propertize (a postive integer);
 - a symbol or string wich names a face;\n
Each list should have the form:\n
 \(some-regexp-or-var match-group face-name\)\n
:EXAMPLE\n\n(mon-help-propertize-tags-TEST\)\n
:SEE-ALSO `mon-help-propertize-tags-TEST', `mon-help-mon-tags',
`mon-help-insert-tags'.\n►►►"
  (let ((mhpt-props
         `(;;:WAS
           ;; (,*regexp-mon-doc-help-docstring-tags*         0 mon-help-KEY-tag)   
           ;; (,*regexp-mon-doc-help-meta-tags*              0 mon-help-META-tag)
           ;; (,*regexp-mon-doc-help-pointer-tags*           2 mon-help-PNTR-tag)
           ;; (,*regexp-mon-doc-help-docstring-tags-TABLES*  2 mon-help-DYNATAB-tag)
           ;; (,*regexp-mon-doc-help-docstring-tags-DYNAMIC* 0 mon-help-DYNATAB-tag)
           ,@*mon-help-propertize-tags-triples*
           ,@(when (and more-triples (consp (car more-triples)))
               (mapcar #'(lambda (mhpt-L-0)
                           `(,(if (stringp (car mhpt-L-0)) 
                                  (car mhpt-L-0) 
                                (symbol-value (car mhpt-L-0)))
                             ,(if (integerp (cadr mhpt-L-0)) ;; ,@(cdr mhpt-L-0))) mo-triples))))
                                  (cadr mhpt-L-0)
                                (mon-format :w-fun  #'error
                                            :w-spec '(":FUNCTION `mon-help-propertize-tags' "
                                                      "-- list value `%s' not a number")
                                            :w-args (cadr mhpt-L-0)))
                             ,(if (facep (caddr mhpt-L-0))
                                  (caddr mhpt-L-0)
                                (mon-format :w-fun  #'error
                                            :w-spec '(":FUNCTION `mon-help-propertize-tags' "
                                                      "-- list value `%s' not a face")
                                            :w-args (caddr mhpt-L-0)))))
                       more-triples)))))
    (let ((case-fold-search nil))
      (mapc #'(lambda (mhpt-L-1)              
                (mon-g2be -1)
                (while (search-forward-regexp  (elt mhpt-L-1 0) nil t)
                  (add-text-properties  
                   (match-beginning (elt mhpt-L-1 1)) (match-end (elt mhpt-L-1 1)) 
                   `(face ,(elt mhpt-L-1 2)))))
            mhpt-props))))
;;
;;; :TEST-ME (mon-help-propertize-tags-TEST)

;;; ==============================
;;; \(mon-help-mon-tags :meta-keys t\)\n 
;;; :CREATED <Timestamp: #{2009-11-20T17:55:35-05:00Z}#{09475} - by MON>
(defun* mon-help-mon-tags (&rest other-keys &key comment docs meta all &allow-other-keys) ;; meta-keys)
  "A list of MON's commonly used tags.\n
Valid keyword arguments are:\n
 :COMMENT :DOCS :META\n
These are equivalent to any of the following arguments for OTHER-KEYS:\n
 comment-tags docstr-tags meta-tags\n
When multiple keyword arguments are supplied choose first supplied value.\n
If other-keys are supplied with one or more keyword arguments choose first matching keyword.\n
When keyword arguments and OTHER-KEYS is omitted prompt for a category tag as if
by `completing-read'.\n
:EXAMPLE\n\n\(mon-help-mon-tags :docs t\)\n
\(mon-help-mon-tags :comment t\)\n
\(mon-help-mon-tags :meta t\)\n
\(mon-help-mon-tags :all t\)\n
\(mon-help-mon-tags 'comment-tags\)\n
\(mon-help-mon-tags 'docstr-tags\)\n
\(mon-help-mon-tags 'meta-tags\)\n
\(mon-help-mon-tags 'all\)
\(mon-help-mon-tags 'all-tags\)
\(mon-help-mon-tags 'comment-tags 'meta-tags :meta t\)\n
\(mon-help-mon-tags t\)\n
\(mon-help-mon-tags\)\n
:SEE-ALSO `mon-help-insert-tags', `mon-help-propertize-tags',
`mon-help-propertize-tags-TEST', `*mon-help-mon-tags-alist*',
`*mon-help-reference-keywords*'.\n►►►"
  (let ((tag-type
         (cond (comment   (assq 'comment-tags *mon-help-mon-tags-alist*))
               (docs      (assq 'docstr-tags *mon-help-mon-tags-alist*))
               (meta      (assq 'meta-tags *mon-help-mon-tags-alist*))
               (all       (list 'all (mon-flatten (mapcar 'cadr *mon-help-mon-tags-alist*))))
               ((and other-keys
                     (not (eq (car other-keys) t))
                     (or (and (memq (car other-keys) '(all all-tags))
                              (list all (mon-flatten (mapcar 'cadr *mon-help-mon-tags-alist*))))
                         (assq (car (memq (car other-keys) (mapcar #'car *mon-help-mon-tags-alist*)))
                               *mon-help-mon-tags-alist*))))
               (t       (assoc 
                         (read (completing-read 
                                (concat ":FUNCTION `mon-help-mon-tags' "
                                        "-- choose a tag category \(Tab completes\): ")
                                (mapcar #'(lambda (x) 
                                            (format "%s" (car x))) *mon-help-mon-tags-alist*)
                                nil t nil nil "docstr-tags"))
                         *mon-help-mon-tags-alist*)))))
    tag-type))
;;
;;; :TEST-ME (mon-help-mon-tags :docs t)
;;; :TEST-ME (mon-help-mon-tags :comment t)
;;; :TEST-ME (mon-help-mon-tags :meta t)
;;; :TEST-ME (mon-help-mon-tags :all t)
;;; :TEST-ME (mon-help-mon-tags 'comment-tags)
;;; :TEST-ME (mon-help-mon-tags 'docstr-tags)
;;; :TEST-ME (mon-help-mon-tags 'meta-tags)
;;; :TEST-ME (mon-help-mon-tags 'all)
;;; :TEST-ME (mon-help-mon-tags 'all-tags)
;;; :TEST-ME (mon-help-mon-tags t)

;; (comment-tags docstr-tags meta-tags-keybindings meta-tags)         
;;; ==============================
;;; :TODO Factor out the read-only-ness checks below to a dedicated macro/defsubst.
;;; :MODIFICATIONS <Timestamp: #{2010-03-23T15:29:06-04:00Z}#{10122} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-11-20T18:20:17-05:00Z}#{09475} - by MON>
;;;###autoload
(defun mon-help-insert-tags (&optional no-insrt intrp &rest tag-categ)
  "Insert a \"MON-TAG\" at point. Does not move point.\n
When &rest arg TAG-CATEG is non-nil it is a quoted symbol or keyword value pair.
Valid arguments can take either of the following forms:\n
 <SYMBOL>       <KEYWORD/VALUE>
 comment-tags   :comment t
 docstr-tags    :docs t
 meta-tags      :meta t
 all            :all  t
 all-tags       :all  t\n
If multiple arguments are supplied only the first is used.
Prompt twice:\n
 i) Complete from a choice of tag-type:
    - comment-tags\n    - docstr-tags\n    - meta-tags\n
 ii) With selected tag-type complete from a choice of tags.\n
Use to keep scope of MON-TAG's as a loosely \"controlled-vocabulary\".
Choice of tag type completed with `mon-help-mon-tags'.\n
When optional arg NO-INSRT is non-nil do not insert tag when called interactively.\n
When current-buffer is in not `view-mode' or `help-mode' do not insert tag. 
When called interactively with NO-INSRT omitted and current-buffer is `buffer-read-only'
but not in not `view-mode' or `help-mode' insert tag with `inhibit-read-only' bound t.\n
:EXAMPLE\n\n(mon-help-insert-tags)\n
\(apply #'mon-help-insert-tags nil '\(t\)\)\n
:SEE-ALSO `mon-help-propertize-tags', `mon-help-propertize-tags-TEST',
`*mon-help-propertize-tags-triples*', `*mon-help-mon-tags-alist*'.\n►►►"
  (interactive "P\np")
  (let ((mhit (let ((completion-ignore-case t))
                (upcase 
                 (completing-read (concat ":FUNCTION `mon-help-insert-tags' "
                                          "-- which tag type \(Tab completes\): ")
                                  (cadr (apply #'mon-help-mon-tags tag-categ))
                                  nil nil nil nil ":"))))
        ;; :NOTE Verbose lambda forms below are here future adaptation to macro/defsubst:
        (mhit-prnc-here #'(lambda (mhit-prn &optional inhbt-rd-only)
                            (if inhbt-rd-only
                                (let ((inhibit-read-only t))
                                  (save-excursion (princ mhit-prn (current-buffer))))
                              (save-excursion (princ mhit-prn (current-buffer))))))
        ;;; (mhit-wrtbl-p #'(lambda () (and (file-writable-p (buffer-file-name))
        ;;;                                 (file-exists-p   (buffer-file-name)))))
        (mhit-chk-mode #'(lambda (val &optional val-check)
                           (let ((chk-v (buffer-local-value val (current-buffer))))
                             (if val-check (eql chk-v val-check) chk-v)))))
    (if (or no-insrt (not intrp)
            (funcall mhit-chk-mode 'major-mode 'help-mode)
            (funcall mhit-chk-mode 'view-mode))
        (princ mhit)
        (with-current-buffer (current-buffer)
          (if (funcall mhit-chk-mode 'buffer-read-only)
              (funcall mhit-prnc-here mhit t)
              (funcall mhit-prnc-here mhit))))))
;;
;;; :TEST-ME (mon-help-insert-tags nil nil 'comment-tags)
;;; :TEST-ME (mon-help-insert-tags nil nil :comment t)
;;; :TEST-ME (mon-help-insert-tags nil nil :all t)
;;; :TEST-ME (apply 'mon-help-insert-tags nil '(t))

;;; ==============================
;;; :CHANGESET 2115
;;; :CREATED <Timestamp: #{2010-09-07T13:22:20-04:00Z}#{10362} - by MON KEY>
;;;###autoload
(defun mon-help-insert-tags-comment (&optional no-insrt intrp)
  "Convenience function for `mon-help-insert-tags'\n
Equivialent to passing the keyword arg \":comment t\".\n
:EXAMPLE\n\n\(mon-help-insert-tags-comment nil t\)\)\n
:SEE-ALSO `mon-help-propertize-tags', `mon-help-propertize-tags-TEST',
`*mon-help-propertize-tags-triples*', `*mon-help-mon-tags-alist*'.\n►►►"
  (interactive "P\np")
  (mon-help-insert-tags no-insrt intrp :comment t))

;;; ==============================
;;; :NOTE It is not entirely clear if the 'list arg is needed.
;;; :TESTING The 'list argument for LINE-REGION-OR-LIST as of:
;;; :CREATED <Timestamp: #{2010-01-26T21:18:01-05:00Z}#{10043} - by MON>
(defun mon-help-overlay-for-example (fun-lay fun-times line-region-or-list
                                             &rest fun-lay-args)
  "Create an overlay for displaying results of evaluating :EXAMPLE forms.\n
FUN-LAY is a function to call.\n
FUN-TIMES is the number of times to evaluate FUN-LAY.\n
LINE-REGION-OR-LIST is the type of arg FUN-LAY will evaluate with the region
between `►' and `◄'.\n
The arg to LINE-REGION-OR-LIST is either line, region, or a list form.\n
When the type arg to LINE-REGION-OR-LIST is `line` or `region` it is provided as
the quoted symbol, e.g.:\n 'line or 'region\n
When the type arg to LINE-REGION-OR-LIST is `list` it is provided as a quoted
proper list \(no dotted pairs\), e.g.:\n '(some list)\n
When LINE-REGION-OR-LIST is line evaluate FUN-LAY within bounds of each
successive line.\n
When LINE-REGION-OR-LIST is a list evaluate FUN-LAY searching the successive
cars of list by matching each elt of list within bounds of entire region.\n
When LINE-REGION-OR-LIST is region evaluate FUN-LAY within bounds of region.\n
:EXAMPLE\n
\(mon-help-overlay-for-example 'mon-line-number-region-incr nil 'region\)\n
►\n1Firstname Lastname\n2Firstname Lastname\n3Firstname Lastname\n◄\n
:EXAMPLE\n
\(mon-help-overlay-for-example 'mon-make-lastname-firstname 3 'line\)\n
►\nFirstname1 Lastname1\nFirstname2 Lastname2\nFirstname3 Lastname3\n◄\n\n
:NOTE when FUN-LAY-ARGS is an init-value from which FUN-LAY steps. It won't.\n
:FACE-FONT-LOCKING-WITH `mon-help-OLAY-RESULT-string-show'
:FACE-FONT-LOCKING-WITH `mon-help-OLAY-RESULT-match-show'\n
:SEE-ALSO `mon-help-overlay-result', `mon-help-find-result-for-overlay',
`mon-help-overlay-on-region', `mon-help-OLAY-RESULT', `mon-help-overlay-functions',
`mon-nuke-overlay-buffer', `momentary-string-display'.\n►►►"
  (let* ((lrol (cond ((eq line-region-or-list 'line)   'li)
                     ( ;; don't allow nil don't allow consed pairs
                      (and (consp line-region-or-list)
                           (or (mon-list-proper-p line-region-or-list)
                               (error 
                                (concat ":FUNCTION `mon-help-overlay-for-example' "
                                        "-- arg LINE-REGION-OR-LIST satisfies `consp' "
                                        " but not `mon-list-proper-p' (no dotted pairs), got: %S")
                                line-region-or-list))) 'ls)
                     ((eq line-region-or-list 'region) 're)
                     ((null line-region-or-list)
                      (error (concat ":FUNCTION `mon-help-overlay-for-example' "
                                     "-- arg LINE-REGION-OR-LIST is null, wanted: "
                                     " 'list 'region or a '\(quoted list\)")))
                     (t (error (concat ":FUNCTION `mon-help-overlay-for-example' "
                                       "-- bad value for arg LINE-REGION-OR-LIST, wanted: "
                                       " 'list 'region or a '\(quoted list\), got: %S, with-type: `%s`")
                               line-region-or-list (type-of line-region-or-list)))))
         (mhor-lst   (if (consp line-region-or-list) line-region-or-list))
         (mhor-start (save-excursion (1+ (funcall 'search-forward-regexp "^►$"  nil t))))
         (mhor-end   (save-excursion 
                       ;; The (1- { ... })puts point before `◄' i.e.:
                       ;; some-thing
                       ;; !◄
                       ;; (1- (funcall 'search-forward-regexp "^◄$" nil t)))) 
                       ;; The (- { ... } 2) Puts point at end prevous line from ◄ i.e.:
                       ;; some-thing!
                       ;; ◄
                       (- (funcall 'search-forward-regexp "^◄$" nil t) 2)))
         (mhor-botp  (cond ((eq lrol 'li) 
                            #'(lambda () `(,(line-beginning-position) . ,(line-end-position))))
                           ((eq lrol 'ls) 
                            #'(lambda (l)
                                (prog1 
                                    (progn 
                                      (search-forward-regexp l mhor-end t)
                                      `(,(match-beginning 0) . ,(match-end 0)))
                                  (if (match-beginning 0)
                                      (goto-char (match-beginning 0))))))
                           ((eq lrol 're) #'(lambda () (cons mhor-start mhor-end)))))
         (mhor-mhor #'(lambda (bd shw)
                        ;;        (if (and shw (> (length shw) (- (cdr bd) (car bd))))
                        ;;        (let ((new-bd (+ (- (length shw) (- (cdr bd) (car bd))) (cdr bd))))                        
                        (mon-help-overlay-result
                         (or (car bd) (+ mhor-start 0)) 
                         (or (cdr bd) (+ mhor-end 0)) 78 shw))))
    (save-excursion 
      (goto-char mhor-start)
      (dotimes (i (cond ((eq lrol 'li) fun-times)
                        ((eq lrol 'ls) (length mhor-lst))
                        ((eq lrol 're) 1)))
        (let* ((mhor-lst-pop mhor-lst)
               (mhor-bdN (cond ((or (eq lrol 'li)
                                    (eq lrol 're))
                                (funcall mhor-botp))
                               ((eq lrol 'ls) 
                                (funcall mhor-botp (cond ((stringp (car mhor-lst-pop))
                                                          (car mhor-lst))
                                                         (t (format "%s" (car mhor-lst)))))))))
          (funcall mhor-mhor mhor-bdN
                   (cond ((or (eq lrol 'li) (eq lrol 're))
                          (funcall #'(lambda ()
                                       (eval `(,fun-lay ,(car mhor-bdN) ,(cdr mhor-bdN) ,@fun-lay-args)))))
                         (t (unless (null (car mhor-lst-pop))
                              (funcall #'(lambda ()
                                           (eval `(,fun-lay ,(pop mhor-lst-pop) ,@fun-lay-args))))))))
                   (when (eq lrol 'li) (line-move-1 1)))))))

;;; ==============================
;;; :NOTE When arg NO-DROP-NL is non-nil newlines can be removed with:
;;; (mon-replace-char-in-string 10 (match-string-no-properties 1))
;;; (mon-replace-char-in-string 10 (match-string-no-properties 5))
;;; :CREATED <Timestamp: #{2010-02-15T14:19:57-05:00Z}#{10071} - by MON KEY>
(defun mon-help-delimited-region (&optional no-mv-point no-drop-nl)
  "Match region delimited by `►' and `◄' return list of match details.\n
When optional arg NO-MV-POINT is non-nil does not move point.\n
When optional arg NO-DROP-NL is ommitted return value has the form:\n
\(\(\(1 . \"►\"\) \(match-beg-1 . match-end-1\)\)
 \(\(2 . \"MATCH2-LINE0\nMATCH2-LINE1
MATCH2-LINE2\nMATCH2-LINE3\"\) \(match-beg-2 . match-end-2\)\)
 \(\(5 . \"◄\"\) \(match-beg-5 . match-end-5\)\)\n
When optional arg NO-DROP-NL is non-nil return value has the form:\n
\(\(\(1 . \"►\n\"\) \(match-beg-1 . match-end-1\)\)
 \(\(2 . \"MATCH2-LINE0\nMATCH2-LINE1
MATCH2-LINE2\nMATCH2-LINE3\"\) \(match-beg-2 . match-end-2\)\)
 \(\(5 . \"\n◄\"\) \(match-beg-5 . match-end-5\)\)\n
:EXAMPLE\n
;; Does not move point.
\(mon-help-delimited-region t\)\n
;; Does not move point. Does not drop newlines.
\(mon-help-delimited-region t t\)\n
;; Show match-groups & matches, Does drop newlines.
\(mapcar #'car \(mon-help-delimited-region t\)\)\n
;; Show conses for match-beginnings & match-ends.
\(mapcar #'cadr \(mon-help-delimited-region t\)\)\n
;; Return only bounds of match-group 2, e.g. the 'inner-region' between delims.
\(cadr \(nth 1 \(mon-help-delimited-region t\)\)\)\n
;; Does move point. Does not drop newlines.
\(mon-help-delimited-region nil t\)\n
►\nMATCH2-LINE0\nMATCH2-LINE1\nMATCH2-LINE2\nMATCH2-LINE3\n◄\n
:ALIASED-BY `mon-line-strings-region-delimited'
:ALIASED-BY `mon-line-strings-get-delimited-region'\n
:SEE-ALSO `mon-help-overlay-result', `mon-help-find-result-for-overlay',
`mon-help-overlay-on-region', `mon-help-overlay-for-example',
`mon-help-overlay-functions'.\n►►►"
  (let ((mhdr #'(lambda ()
                  (search-forward-regexp 
                   (if no-drop-nl  
                       "^\\(►\n\\)\\(\\(.*\n\\)+\\(.*\\)\\)\\(\n◄\\)"
                       ;;^^1^^^^^^^^2^^3^^^^^^^^^^4^^^^^^^^^^5^^^^^^
                       ;; :NOTE no-drop-nl couples leading and trailing `\n' (char 10)
                       ;;  with delimiter.
                       ;; grp1 -> ►\n  
                       ;; grp2 { ... junk-in-middle ... }
                       ;; grp5 -> \n◄
                       "^\\(►\\)\n\\(\\(.*\n\\)+\\(.*\\)\\)\n\\(◄\\)"
                       ;;^^1^^^^^^^^2^^3^^^^^^^^^^4^^^^^^^^^^^^5^^^^
                       ;; grp1 -> ►  ; drops trailing \n
                       ;; grp2 { ... junk-in-middle ... } 
                       ;; grp5 -> ◄ ; drops leading \n
                       ) nil t)
                  `(((1 . ,(match-string-no-properties 1)) (,(match-beginning 1) . ,(match-end 1)))
                    ((2 . ,(match-string-no-properties 2)) (,(match-beginning 2) . ,(match-end 2)))
                    ((5 . ,(match-string-no-properties 5)) (,(match-beginning 5) . ,(match-end 5)))))))
    (cond (no-mv-point (save-excursion (funcall mhdr)))
          (t (funcall mhdr)))))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (mon-help-delimited-region t t)
;;| (mon-help-delimited-region nil t)
;;| (mon-help-delimited-region t)
;;| (mon-help-delimited-region)
;;|
;;|►
;;|MATCH2-LINE0
;;|MATCH2-LINE1
;;|MATCH2-LINE2
;;|MATCH2-LINE3
;;|◄
;;`----

;;; ==============================
;;; :TODO add optional arg OSTRING-FROM-FUNC which:
;;; When optional arg OSTRING-FROM-FUNC is non-nil value of OSTRING is acquired with
;;; a function which accepts the bounds of a region as its first (and only two)
;;; required arguments, i.e. where START and END are the required args to somefunc:
;;;  (somefunc START END)\n
;;; :NOTE wrote `mon-help-delimited-region' for this, not incorporated yet.
;;; :CREATED <Timestamp: #{2010-02-09T11:57:07-05:00Z}#{10062} - by MON KEY>
(defun mon-help-overlay-on-region (ostring) ;&optional ostring-from-func)
  "Display an overlay for region containing OSTRING delimited by `►' and `◄'.
Display for 2 seconds then remove overlay.\n
Overlay displayed with the face `minibuffer-prompt'.\n
:EXAMPLE\n\n(mon-help-overlay-on-region \"Some string\")\n
►\nSome string\n◄\n
\(mon-help-overlay-on-region \"Some string\"\)\n
►Some string◄\n
:FACE-FONT-LOCKING-WITH `minibuffer-prompt'\n
:SEE-ALSO `mon-help-overlay-result', `mon-help-overlay-for-example',
`mon-nuke-overlay-buffer', `mon-help-find-result-for-overlay',
`mon-help-overlay-functions'.\n►►►"
  (unwind-protect
      (let* ((mhoor-sb 
              (save-excursion 
                (search-forward-regexp (concat "^►\n?.*" ostring ".*\n?◄$")  nil t)))
             (mhoor-olay (make-overlay (match-beginning 0) (match-end 0) (current-buffer) t t)))
        (overlay-put mhoor-olay 'face 'minibuffer-prompt)
        (sit-for 2))
    (remove-overlays (mon-g2be -1 t)  (mon-g2be 1 t) 'face 'minibuffer-prompt)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-01-12T12:36:15-05:00Z}#{10022} - by MON>
;;; :CREATED <Timestamp: #{2010-01-08T23:28:40-05:00Z}#{10015} - by MON>
(defun mon-help-overlay-result (show-here to-here exit-c &optional show-str) ;for-secs 
  "Return overlay from SHOW-HERE TO-HERE and remove it with exit char EXIT-C.\n
SHOW-HERE is the starting point in buffer to place overlay.\n
TO-HERE is the starting point in buffer to place overlay.\n
EXIT-C is the character corresponding the the keyboard key user must type
to exit from the overlay display.
When optional arg SHOW-STR \(a string) is non-nil display it but with a with
less vibrant overlay.\n
When function is invoked place the overlay and message user to:\n
 \"Type `<EXIT-CHAR>' to continue ... or C-g to exit\"\n
:EXAMPLE\n\n\(save-excursion \(forward-sexp 2\)
  \(let \(\(lbp #'\(lambda \(\) `\(,\(line-beginning-position\) . ,\(line-end-position\)\)\)\)
        \(olbp #'\(lambda \(bd\) \(mon-help-overlay-result \(car bd\) \(cdr bd\) 78\)\)\)\)
    \(dotimes \(i 2\)
      \(funcall olbp \(funcall lbp\)\)\(line-move-1 -1\)\)\)\)\n
\( ... LOTSA-JUNK-FOR-AN-OVERLAY ... \)\n\( ...  MORE-JUNK-FOR-AN-OVERLAY ... \)\n
:NOTE This functionality is modeled after `momentary-string-display' but with less
bounds error checking and restricts exiting from the loop until user provides
EXIT-CHAR or enters \7.\n
:CALLED-BY `mon-help-find-result-for-overlay'.\n
:FACE-FONT-LOCKING-WITH `mon-help-OLAY-RESULT-string-show'
:FACE-FONT-LOCKING-WITH `mon-help-OLAY-RESULT-match-show'\n
:SEE-ALSO `mon-help-overlay-for-example', `mon-help-overlay-on-region',
`mon-help-OLAY-RESULT', `mon-nuke-overlay-buffer', `mon-help-overlay-functions'.\n►►►"
  (let ((showlay (make-overlay show-here to-here nil t t))
        (max-mini-window-height 1))
    (unwind-protect
         (save-excursion 
           (if show-str
               (progn
                 (overlay-put showlay 'display show-str)
                 (overlay-put showlay 'face 'mon-help-OLAY-RESULT-string-show))
               (overlay-put showlay 'face 'mon-help-OLAY-RESULT-match-show))
           (goto-char show-here)
           (setq show-here (point))
           (recenter)
           (overlay-recenter show-here)
           (let (got-N)
             (while (not got-N)
               (when (eq (read-event 
                          (format (concat ":FUNCTION `mon-help-overlay-result' "
                                          "-- type `%c' to continue ... or %s to exit")
                                  exit-c (key-description [7])))
                         exit-c)
                 (setq got-N t)))))
      (delete-overlay showlay))))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((botp #'(lambda () `(,(line-beginning-position) . ,(line-end-position))))
;;|        (mhor #'(lambda (bd) (mon-help-overlay-result (car bd) (cdr bd) 78))))
;;|   (save-excursion (forward-sexp 2)
;;|               (dotimes (i 2) (funcall mhor (funcall botp))(line-move-1 -1))))
;;| 
;;| 
;;|( ... LOTSA-JUNK-FOR-AN-OVERLAY... )
;;|( .... MORE-JUNK-FOR-AN-OVERLAY... )
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-09T00:07:02-05:00Z}#{10016} - by MON>
(defun mon-help-find-result-for-overlay (search-it match-b match-e exit-char)
  "Find regexp SEARCH-IT placing overlay on the match from MATCH-B to MATCH-E.\n
SEARCH-IT is a regexp string to search.\n
MATCH-B is the match group for `match-beginning'\n
MATCH-E is the match group for `match-end'\n
EXIT-CHAR is the character corresponding the the keyboard key user must type to
exit from the overlay display.\n
:NOTE Looping procedures which invoke this function won't advance until kbd
input matching EXIT-CHAR is recieved or user enters \7 -- this is a feature!.\n
Useful for examining the result of a procedure inside a *HELP* buffer,
particulary anything that changes or alters text in a buffer i.e. regexp
oriented utilities.\n
:EXAMPLE\n\(save-excursion
  \(dotimes \(i 22\)
    \(mon-help-find-result-for-overlay 
     *regexp-mon-doc-help-pointer-tags* 0 0 78\)\)\)\n
Matches the following:
 ->   ;->  ; -> \n =>   ;=>  ; => \n -->  ;-->  ; --> --->\n <--  <--  ; <--  <---
 <--  ;<-- ; <--  <--- \n ==>  ;==> ; ==>  ===>\n
:FACE-FONT-LOCKING-WITH `mon-help-OLAY-RESULT-string-show'
:FACE-FONT-LOCKING-WITH `mon-help-OLAY-RESULT-match-show'\n
:SEE-ALSO `mon-help-overlay-for-example', `mon-help-overlay-result',
`mon-help-OLAY-RESULT', `mon-help-overlay-functions', 
`mon-nuke-overlay-buffer', `momentary-string-display'.\n►►►"
  (progn
    (search-forward-regexp search-it nil t)
    (let ((mb (match-beginning (or match-b 0)))
           (me  (match-end (or match-e 0))))
      (mon-help-overlay-result mb me exit-char))))
;;
;;; :TEST-ME 
;;; (save-excursion
;;;   (dotimes (i 22)
;;;     (mon-help-find-result-for-overlay *regexp-mon-doc-help-pointer-tags* 0 0 78)))
;;
;;,---- :MATCHES
;;| 
;;|  ->   ;->  ; -> 
;;|  =>   ;=>  ; => 
;;|  -->  ;-->  ; --> --->
;;|  <--  <--  ; <--  <---
;;|  <--  ;<-- ; <--  <--- 
;;|  ==>  ;==> ; ==>  ===>
;;`----

;;; ==============================
;;; :TODO Verify the logic on the arg KILL-EM-BEFORE-THEY-GROW no clear that its
;;;        doing anything or working correctly.
;;; :NOTE See the hidden fnctn: `make-help-screen'.
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T15:16:13-05:00Z}#{10053} - by MON>
;;; :CREATED <Timestamp: #{2009-12-20T17:50:49-05:00Z}#{09517} - by MON>
(defun mon-help-temp-docstring-display (the-help-doc &optional some-other-buffer
                                        kill-em-before-they-grow)
  "Display THE-HELP-DOC string formatted as per help-mode.\n
Leave existing *Help* buffer untouched.\n
Docstring is displayed in the buffer named by the the value of 
:VARIABLE `*mon-help-docstring-help-bffr*'.\n
When optional arg SOME-OTHER-BUFFER is non-nil display THE-HELP-DOC in that
buffer instead.\n
When optional arg KILL-EM-BEFORE-THEY-GROW is non-nil kill any existing
SOME-OTHER-BUFFER with name before displaying contents there.\n
:EXAMPLE\n\(mon-help-temp-docstring-display
 \(documentation 'mon-help-temp-docstring-display\)\)\n 
\(mon-help-temp-docstring-display 
 \(documentation 'mon-help-temp-docstring-display\)
 \(buffer-name \(get-buffer-create \"*BUBBA-TEST*\"\)\)\)\n
:CALLED-BY `google-define' \(MON's VERSION\)\n
:SEE `make-help-screen' \(hidden function\), `advertised-signature-table', etc. \n
:SEE-ALSO `mon-help-view-file', `mon-help-temp-docstring-display'.\n►►►"
  (let ((help-window-select 'always)
        (dhb (buffer-name 
              (if some-other-buffer
                 ;; :NOTE Going into view-mode don't know whats there or how it got there. 
                 ;;;      Don't try to erase the contents - Kill them. 
                  (if (and kill-em-before-they-grow
                           (buffer-live-p (get-buffer some-other-buffer)))
                      (progn 
                        (kill-buffer (get-buffer some-other-buffer))
                        (get-buffer-create some-other-buffer))
                      (get-buffer-create some-other-buffer))
                 ;; *mon-help-docstring-help-bffr* -> "*MON-HELP*"
                  (get-buffer-create *mon-help-docstring-help-bffr*)))))
    ;; `help-xref-following'
    ;; `help-buffer' 
    ;; (let ((help-xref-following t))
    ;; (buffer-name				;for with-output-to-temp-buffer
    ;;  (if help-xref-following
    ;;      (current-buffer)
    ;;      (get-buffer-create "*Help*")))
    (with-help-window dhb 
      (with-current-buffer dhb
        (insert the-help-doc)
        (help-window-setup-finish (selected-window) ))))) ;;t t)))))
;;
;;; :TEST-ME (mon-help-temp-docstring-display (documentation 'mon-help-temp-docstring-display))
;;; :TEST-ME (mon-help-temp-docstring-display 
;;;            (documentation 'mon-help-temp-docstring-display)
;;;            (buffer-name (get-buffer-create "*BUBBA-TEST*")))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-07-31T18:42:57-04:00Z}#{10306} - by MON KEY>
(defun mon-help-buffer-spc-*DOC* (&optional show-the-real no-display)
  "Examine contents of buffer \" *DOC*\".\n
If buffer exists display its contents in the buffer named
\"*MON-SHOW- *DOC*-BUFFER*\" along with a commented list of its
buffer-local-variables.\n
If buffer is non-existent message user.\n
When optional arg SHOW-THE-REAL is non-nil display the actual \" *DOC*\" buffer
instead but do not insert any addtional annotations.\n
When optional arg NO-DISPLAY is non-nil and SHOW-THE-REAL is ommitted do not
display the \"*MON-SHOW- *DOC*-BUFFER*\".\n
:EXAMPLE\n\n\(mon-help-buffer-spc-*DOC* nil t\)\n
\(mon-help-buffer-spc-*DOC*\)\n
\(mon-help-buffer-spc-*DOC* t\)\n
:SEE-ALSO `mon-help-hidden-buffers', `mon-help-parse-interactive-spec',
`mon-help-function-spit-doc', `mon-help-function-arity',
`mon-help-swap-var-doc-const-val', `mon-help-put-var-doc-val->func',
`doc-directory', `internal-doc-file-name', `elint-scan-doc-file',
`documentation-property', `byte-compile-output-docform', `lambda-list-keywords',
`subr-arity', `help-function-arglist', `byte-compile-arglist-signature',
`help-add-fundoc-usage', `apropos-documentation-property'.\n►►►"
  (let ((get*doc (get-buffer " *DOC*")))
    (if (not (get-buffer get*doc))
        (message (concat ":FUNCTION `mon-help-buffer-spc-*DOC*' '"
                         "-- non-existent buffer with name: \" *DOC*\" "))
      (if show-the-real
          (with-current-buffer (get-buffer get*doc)
            (display-buffer (current-buffer) t)
            `(,(current-buffer) . ,(get-buffer (current-buffer))))
        (with-current-buffer (get-buffer-create "*MON-SHOW- *DOC*-BUFFER*")
          (erase-buffer)
          (save-excursion
            (insert 
             ":BUFFER-NAME \" *DOC*\"\n"
             ":W-LOCAL-VARIABLES\n"
             (pp-to-string (buffer-local-variables get*doc))
             (make-string 68 59) "\n"
             ":W-CONTENTS\n\n\n")
            (let ((comment-start ";;"))
              (comment-region (mon-g2be -1 t) (mon-g2be 1 t)))
            (insert-buffer-substring get*doc))
          (unless no-display
            (display-buffer (current-buffer) t))
          `(,(current-buffer) . ,(get-buffer (current-buffer))))))))
;;
;;; :TEST-ME (mon-help-buffer-spc-*DOC*)
;;; :TEST-ME (mon-help-buffer-spc-*DOC* nil t)
;;; :TEST-ME (mon-help-buffer-spc-*DOC* t)

;;; ==============================
;;; :NOTE This is here mostly so I don't forget it.
;;; :COURTESY Thinkig Machines Corp. :HIS help-hacks.el :WAS `help-tmc-hacks'
;;; :SEE (URL `ftp://ftp.sra.co.jp/pub/lang/lisp/misc/gmacs/gmacs/')
;;; :CREATED <Timestamp: #{2009-12-31T13:12:41-05:00Z}#{09534} - by MON KEY>
(defun mon-help-get-mon-help-buffer (d-string) ;; :WAS ()
  "Display D-STRING in `view-mode' with buffer `*mon-help-docstring-help-bffr*'.\n
:SEE-ALSO `mon-help-temp-docstring-display', `mon-help-view-file'.\n►►►"
  (interactive)
  (let ((current-buffer (current-buffer)))
    (switch-to-buffer 
     (get-buffer-create *mon-help-docstring-help-bffr*))
    (delete-region (mon-g2be -1 t) (mon-g2be 1 t))
    (insert d-string)
    (mon-g2be -1)
    (view-mode current-buffer)))
;;
;;; :TEST-ME (mon-help-get-mon-help-buffer "Some Test String")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-20T20:34:49-05:00Z}#{09517} - by MON>
(defun mon-help-view-file (file-to-view &optional dir)
  "Visit FILE-TO-VIEW as if by `view-help-file'.\n
When DIR is non-nil it is a directory name in which to look for FILE-TO-VIEW.\n
Unlike `view-help-file' DIR's default is default-dirctory not `data-directory'.\n
Signal an error when FILE-TO-VIEW does not exist or is unreadable.\n
:SEE-ALSO `mon-help-temp-docstring-display', `*mon-help-docstring-help-bffr*',
`goto-address-mode'.\n►►►"
  (interactive "fWhich file: ")
  (let ((orig-data-directory data-directory))
    (unwind-protect 
         (let ((data-directory (if dir
                                   (directory-file-name dir)
                                 (directory-file-name (file-name-directory default-directory)))))
           (cond (dir (if (file-readable-p (concat data-directory "/" file-to-view))
                          (setq data-directory `(,file-to-view . ,data-directory))
                        (error  (concat ":FUNCTION `mon-help-view-file' "
                                        "-- unreadable or non-existent :FILE %s")
                                file-to-view)))
                 (t (setq data-directory `(,file-to-view . ,data-directory))))
           (view-help-file (car data-directory) (cdr data-directory)))
      (setq data-directory orig-data-directory))))
;;
;;; :TEST-ME (mon-help-view-file ".bashrc" "~/")
;;; :TEST-ME (mon-help-view-file ".bashrc" "/root")
;;; :TEST-ME (apply 'mon-help-view-file '(t))

 
;;; ==============================
;; :TAGS-TABLES

;;; ==============================
;;; :CHANGESET 2117 <Timestamp: #{2010-09-17T17:37:33-04:00Z}#{10375} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-21T19:02:12-04:00Z}#{09345} - by MON>
(defcustom *mon-tags-table-list* nil
  "Consed list of paths and paths for `tags-table-list'.
The car and cdr are as per the TAGS-TBL-LIST and TAGS-SPECS args to
`mon-update-tags-tables' where the car is a list of pathnames and 
the cdr is a list pathname transforms for format to use when generatign etags
shell commands.\n
:SEE-ALSO `mon-tags-apropos', `mon-tags-naf-apropos', `mon-update-tags-tables',
`mon-update-tags-tables-loadtime', `tags-file-name', `*mon-emacs-root*',
`*mon-site-lisp-root*', `*mon-naf-mode-root*',
`*mon-ebay-tmplt-mode-root*'.\n►►►"
  :type 'sexp  ;; How to specify this for custom???
  :group 'mon-doc-help-utils)
;;
(when (and (intern-soft "IS-MON-SYSTEM-P")   ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-SYSTEM-P))
  (unless (bound-and-true-p *mon-tags-table-list*)
    (setq *mon-tags-table-list*
          `((,*mon-emacs-root* 
             ,*mon-naf-mode-root* 
             ,*mon-ebay-tmplt-mode-root*
             ,*mon-site-lisp-root*) . ;; <- thats a dotted 
             (((3 E) (3 T O)) 
              ((2 E) (2 T O)) 
              ((1 E) (2 T I) (1 T O)) 
              ((0 E) (3 T I) (1 T I) (0 T O)))))
    (custom-note-var-changed '*mon-tags-table-list*)))
;;
;;; :TEST-ME *mon-tags-table-list* 
;;
;;;(progn (makunbound '*mon-tags-table-list*) (unintern "*mon-tags-table-list*" obarray) )


;;; ==============================
;;; :CHANGESET 2117 <Timestamp: #{2010-09-17T17:30:21-04:00Z}#{10375} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-21T19:27:44-04:00Z}#{09345} - by MON>
(defun mon-update-tags-tables (tags-tbl-list tags-specs &optional call-proc)
  "Return a list of etags shell commands.\n
Returned list is suitable for updating a nested tree tags files.\n
TAGS-TBL-LIST is a list of pathnames used when mapping TAGS-SPECS.\n
TAGS-SPECS is a list of specificatins mapping etags args to format strings.\n
Elts TAGS-SPECS have the form:\n
 \( <NTH> { E | T } { I | O | nil } \)\n
   <NTH> <-  elt of TAGS-TBL-LIST\n
   E     <- \"*.el\"\n   T     <- \"/TAGS\"
   I     <- \"--include=\"\n   O     <- \"--output=\"
Thus, an elemnt of the form:\n
 \(\(0 E\) \(3 T I\) \(1 T I\) \(0 T O\)\)\n
Would generate the following command for passing to `call-process':\n
 \"etags /TAGS-TBL-LIST/PATH/NTH-0/*.el \\
  --include=/TAGS-TBL-LIST/PATH/NTH-3/TAGS \\
  --include=/TAGS-TBL-LIST/PATH/NTH-1/TAGS \\
  --output=/TAGS-TBL-LIST/PATH/NTH-0/TAGS\"\n
When optional arg CALL-PROC is non-nil pass each generated shell-commands to
`call-process' with the following args:\n
 PROGRAM `shell-file-name' INFILE nil DISPLAY nil BUFFER discard 
 &REST `shell-command-switch' \"etags {...} \"\n
:SEE-ALSO `mon-tags-apropos',`mon-tags-naf-apropos', `*mon-tags-table-list*'.\n►►►"
  (let ((mttl          tags-tbl-list)
        (mttl-e        "etags %s/*.el")
        (mttl-i        "--include=%s/TAGS")
        (mttl-o        "--output=%s/TAGS")
        gthr)
    (dolist (spec-ls tags-specs (setq gthr (nreverse gthr)))
      (let (spec-cur)
        (dolist (spec-l spec-ls 
                        (if (memq 'bad-spec spec-cur) 
                            (setq spec-cur "")
                          ;; :NOTE Uncomment to background-it, not needed though:
                          ;; (push (concat (mapconcat #'identity (nreverse spec-cur) " ") " &") gthr)))
                          (push (mapconcat #'identity (nreverse spec-cur) " ") gthr)))
          (case (cadr spec-l)
            (E (push (format mttl-e (nth (car spec-l) mttl)) spec-cur))
            (T (case (caddr spec-l)
                 (O (push (format mttl-o (nth (car spec-l) mttl)) spec-cur))
                 (I (push (format mttl-i (nth (car spec-l) mttl)) spec-cur))
                 (t (push 'bad-spec spec-cur))))
            (t (push 'bad-spec spec-cur))))))
    (when call-proc
      (dolist (cp gthr)
        ;; :PROGRAM `shell-file-name' :INFILE  nil :DISPLAY nil :BUFFER discard 
        ;; :&REST-ARGS  `shell-command-switch' "etags {...} "
        (call-process shell-file-name nil 0 nil shell-command-switch cp))
      (mon-message :msg-spec '(":FUNCTION  `mon-update-tags-tables'" 
                               "-- executed etags shell-commands with args:\n %S")
                   :msg-args gthr))
    gthr))

;;; ==============================
;;; :CHANGESET 2117
;;; :CREATED <Timestamp: #{2010-09-17T17:30:44-04:00Z}#{10375} - by MON KEY>
(defun mon-update-tags-tables-loadtime ()
  "Update the MON 'TAGS' files in paths held by.\n
:NOTE When IS-MON-SYSTEM-P evaluated at startup per `mon-run-post-load-hooks'
with paths held by `*mon-tags-table-list*'.\n
:SEE-ALSO `mon-update-tags-tables', `tags-table-list', `tags-file-name'.\n►►►"
  (progn
    (mon-update-tags-tables 
     (car *mon-tags-table-list*) (cdr *mon-tags-table-list*) t)
    (mon-message :msg-spec '(":FUNCTION  `mon-update-tags-tables-loadtime'" 
                             "-- updated TAGS files with "
                             "`*mon-tags-table-list*' pathnames"))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-21T19:00:14-04:00Z}#{09345} - by MON KEY>
(defun mon-tags-naf-apropos ()
  "Search tags-tables in for occurences of regexp \"*naf-\" with `tags-apropos'.\n
:SEE-ALSO `mon-tags-apropos', `*mon-tags-table-list*', `mon-update-tags-tables'.
►►►"
  (interactive)
  (tags-apropos "naf.*\\|\*naf\\|.*-naf"))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-21T18:57:08-04:00Z}#{09345} - by MON KEY>
(defun mon-tags-apropos ()
  "Search tags-tables in for occurences of regexp \"*mon-\" with `tags-apropos'.\n
:EXAMPLE\n(mon-tags-apropos)
:SEE-ALSO `mon-tags-naf-apropos', `*mon-tags-table-list*', 
`mon-update-tags-tables'.\n►►►"
  (interactive)
  (tags-apropos "mon-.*\\|\*mon\\|.*-mon-.*"))

 
;;; ==============================
(defun mon-insert-doc-help-cookie ()
  "Insert default 'documentation cookie' at point.\n
Everything up to `*mon-doc-cookie*' is commented out when inserted into a buffer.
Default value for cookie is: \"\u25BA\u25BA\u25BA\".\n
:EXAMPLE\n\n\(momentary-string-display 
 \(concat \" Default doc-cookie to insert -> \"
         \(do* \(\(i ?\\u25BA\)
       \(j 0 \(1+ j\)\)
       \(ck \"\" \(concat \(char-to-string i\) ck\)\)\)
     \(\(= j 3\) ck\)\)\) \(point\)\)\n
:NOTE If default value is not acceptable set value of `*mon-doc-cookie*' variable
otherwise.\n
:CALLED-BY `mon-help-function-spit-doc'\n
:SEE-ALSO `mon-insert-ebay-field-trigger-l-and-r',
`mon-insert-ebay-field-trigger-l', `mon-insert-ebay-field-trigger-r'.\n►►►"
  (interactive)
  (insert "►►►"))
;;
;;; :TEST-ME (mon-insert-doc-help-cookie)
;;; :TEST-ME (apply 'mon-insert-doc-help-cookie '(t))

;;; ==============================
;;; :TODO Consider where `documentation-property' can be used instead of `plist-get'.
;;; :NOTE Also take a look at:
;;; `lisp-doc-string-elt-property' `apropos-documentation-property'
;;; :NOTE This idiom works for byte compiling in docstrings.
;;; (eval-and-compile
;;;  (<THE-DEFVAR>))
;;
;;; (eval-when (compile eval)
;;;  (mon-help-put-var-doc-val->func <THE-DEFVAR> <THE-FUNC>))
;;; :CHANGESET 1903 <Timestamp: #{2010-06-21T12:55:22-04:00Z}#{10251} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-01T18:57:13-04:00Z}#{09404} - by MON KEY>
(defmacro mon-help-put-var-doc-val->func (var-name func-name &optional 
                                          pre-v-str cut-v-str pst-v-str)
  "VAR-NAME is a variable whose value and docstring will be put on FUNC-NAME.
CUT-V-STR is a string on which to split the variable documentation of VAR-NAME.
When non-nil cut-v-str is removed from variables documentation with substring up
to CUT-V-STR at head of docstring, var-name's value is inserted, followed by the
substring occuring after CUT-V-STR. The substring CUT-V-STR is not placed on
func-name's documentation-property.\n
When non-nil PRE-V-STR is a string to insert before value string of var-name.\n
When non-nil PST-V-STR is a string to insert after value string of var-name.\n
:EXAMPLE\n
\(mon-help-put-var-doc-val->func '<VAR-NAME> '<FUNC-NAME>
  \"\\nThis is a PRE-V-STR\\n\"
  \"content of CUT-V-STR removed\"
  \"\\nThis ia PST-V-STR\\n\"\)\n\n
\(mon-help-put-var-doc-val->func '<VAR-NAME> '<FUNC-NAME>
  nil \"content of CUT-V-STR removed\" nil\)\n\n
\(mon-help-put-var-doc-val->func '<VAR-NAME> '<FUNC-NAME>
\"\\nPRE-V-STR\\n\" nil \"PST-V-STR\"\)\n
:SEE-ALSO `mon-help-swap-var-doc-const-val', `documentation-property',
`apropos-documentation-property', `lambda-list-keywords', `subr-arity',
`help-function-arglist', `byte-compile-arglist-signature',
`byte-compile-output-docform', `help-add-fundoc-usage',
`elint-put-function-args', `mon-help-buffer-spc-*DOC*'.\n►►►"
  (declare (indent 2) (debug t))
  (let ((putf-doc (make-symbol "putf-doc"))
        (getv-doc (make-symbol "getv-doc"))
        (getv-val (make-symbol "getv-val")))
    `(let ((,getv-val (symbol-value ,var-name))
           (,getv-doc (if ,cut-v-str
                          `(,(mon-string-upto-index
                              ;; (get ,var-name 'variable-documentation)
                              ;; (plist-get (symbol-plist ,var-name) 'variable-documentation)
                              (documentation-property ,var-name 'variable-documentation)
                              ,cut-v-str)
                            ,(mon-string-after-index
                              ;; (get ,var-name 'variable-documentation)
                              ;; (plist-get (symbol-plist ,var-name) 'variable-documentation)
                              (documentation-property ,var-name 'variable-documentation)
                              ,cut-v-str))
                        ;; (get ,var-name 'variable-documentation)
                        ;; (plist-get (symbol-plist ,var-name) 'variable-documentation)))
                        (documentation-property ,var-name 'variable-documentation)))
           ;; :WAS (,putf-doc))
           ,putf-doc)
       (when (listp ,getv-val)
         (setq ,getv-val
               (with-temp-buffer
                 (insert-char 32 1)
                 (save-excursion (pp ,getv-val (current-buffer)))
                 (indent-pp-sexp)
                 ;; :WAS (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) )))
                 (mon-buffer-sub-no-prop ))))
       (setq ,putf-doc
             (if (stringp ,getv-doc)
                 (concat ,getv-doc ,pre-v-str ,getv-val ,pst-v-str)
                 (concat (car ,getv-doc) ,pre-v-str ,getv-val ,pst-v-str (cadr ,getv-doc))))
       (put ,func-name 'function-documentation ,putf-doc))))

;;; (put 'mon-help-put-var-doc-val->func  'lisp-indent-function <INT>) 
;; 
;;; :TEST-ME (pp-macroexpand-expression '(mon-help-put-var-doc-val->func 

;;; ==============================;
;; :NOTE When compiling, defvar and defconst forms must be made known at
;;;       compile time.  Wrap them _and_ the macro call in an
;;;       `eval-when-compile' and make sure that: 
;;;       (eval-when-compile (require 'cl)) is at top of file. 
;;;       Otherwise, all of the args docstrings get doubled up at compile time.
;;;
;;; :MODIFICATIONS <Timestamp: #{2010-01-23T15:52:18-05:00Z}#{10036} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-03T14:31:54-04:00Z}#{09406} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-14T14:16:34-04:00Z}#{09381} - by MON KEY>
(defmacro mon-help-swap-var-doc-const-val (var-name const-name xrefs &optional face-name)
  "Swap the value of VAR-NAME's variable-documentation property onto
  CONST-NAME's variable-documentation property.\n
Put the symbol value of CONST-NAME on VAR-NAME's variable-documentation property.\n
Put the symbol value of VAR-NAME on CONST-NAME's variable-documentation property.\n
Put XREFS of packages related variables on VAR-NAME and CONST-NAME's
variable-documentation property. XREF's is  a symbol holding a list of related
symbol names which should have cross-reference to one another in documentation.
FACE-NAME is variable pointing bound to the symbol holding a face definintion.\n
For example, in `naf-mode' the variable `naf-mode-institution-fface' is bound to
the face `naf-mode-institution-face'. This is because face documentation isn't
accessible as a variable in *Help* buffers i.e. using \[`describe-variable'].\n
:EXAMPLE
\(mon-help-swap-var-doc-const-val
    *naf-school-names-english* naf-mode-school-names-english
    ;;^ VAR-NAME ^             ^ CONST-NAME ^
    *naf-mode-institution-xrefs* naf-mode-institution-fface)
    ;;^ XREF ^                   ^ FACE-NAME ^\n
:NOTE When compiling defvar and defconst forms mut be made known at compile time.
Wrap them _and_ the macro call in an `eval-when-compile' and make sure that
\(eval-when-compile \(require 'cl\)\) is at top of file. Otherwise, all of the
args docstrings get doubled up at compile time.\n
This procedure is implemented as a means of extending *Help* documentation of
`naf-mode' constants, variables, and faces. It is provided because naf-mode's
core mechanism of keyword lookup and identification occurs via font-locking and
`font-lock-extra-managed-props' manipulation of plists and text-properties.
Currently we leverage these facilities with the simple inheritance provided by
Emacs faces. In the future, as Emacs face implementation begins taking advantage
of CEDET and EIEIO class properties, `naf-mode' will use it's existing faces as a
gateway towards OO manipulation of text.  As such, this macro might be used to
similiar functionality to any derived mode which generates font-lock keywords
from lists bound variables.\n
:SEE-ALSO `mon-help-put-var-doc-val->func', `documentation-property',
`apropos-documentation-property', `byte-compile-output-docform',
`lambda-list-keywords', `subr-arity', `help-function-arglist',
`help-add-fundoc-usage', `elint-put-function-args',
`mon-help-buffer-spc-*DOC*'.\n►►►"
  ;;  (declare (indent 2) (debug t))
  (let ((v-doc  (make-symbol "v-doc"))
        (v-val  (make-symbol "v-val"))
	(c-val  (make-symbol "c-val"))
	(cr-val (make-symbol "cr-val"))
	(c-doc  (make-symbol "c-doc"))
        (x-ref  (make-symbol "x-ref"))
        (f-nam  (make-symbol "f-nam"))
        (f-doc  (make-symbol "f-doc")))
    `(let (,v-doc ,v-val ,c-val ,cr-val ,c-doc ,x-ref ,f-nam ,f-doc)
       (setq ,v-doc ;; ,(plist-get (symbol-plist var-name) 'variable-documentation))
             ,(documentation-property var-name 'variable-documentation))
       ;;
       ;; :WAS (setq ,v-val ,(format "%s" (symbol-value var-name)))
       ;; Not sure if format -> pp-to-string is redundant but it works...
       (setq ,v-val ,(format "%s" (pp-to-string  (symbol-value var-name))))
       ;;
       (setq ,c-doc ;; ,(plist-get (symbol-plist var-name) 'variable-documentation))
             ,(documentation-property var-name 'variable-documentation))
       ;;
       ;; :WAS (setq ,c-val ,(format "%s" (symbol-value const-name)))
       (setq ,c-val ,(format "%s" (pp-to-string (symbol-value const-name))))
       ;;
       ;; Replace "\\<" and "\\>" at beginning and end of string so *Help*
       ;; doesn't see them as keymap related xref'ing shite.
       (setq ,cr-val (replace-regexp-in-string "^\\\"\\\\\\\\<" "\\\\\\\=<" ,c-val nil t))
       (setq ,c-val (replace-regexp-in-string "\\\\\\\\>\\\"$" "\\\\\\\=>" ,cr-val nil t))
       (setq ,f-nam (when ,face-name
                      (list ,(symbol-name face-name)
                            (replace-regexp-in-string "fface" "face" ,(symbol-name face-name)))))
       (setq ,f-doc (when ,f-nam
                      (concat
                       "\n\n--------------------------\n"
                       "The keywords and regexps are font-locked with:\n"
                       ":FACE-DOCUMENTED-IN `" (car ,f-nam) "'\n"
                       ":FACE-DEFINED-IN    `" (cadr ,f-nam) "'\n"
                       "                    (describe-face '" (cadr ,f-nam) ")\n")))
       (setq ,x-ref (remove ',var-name ,xrefs))
       (setq ,x-ref (concat "\n:SEE-ALSO\n`" (mapconcat 'symbol-name ,x-ref "'\n`") "'\n►►►"))
       (setq ,v-doc ;; Put the vars's properties.
	     (concat
              ,v-doc
              "\n--------------------------\n"
              ":KEYWORD-REGEXPS-IN `" ,(symbol-name const-name) "' a <CONSTANT> with value:\n\n"
              ,c-val
              ,f-doc
              "\n--------"
              ,x-ref))
       (setq ,c-doc ;; Put the constant's properties.
	     (concat
	      ,c-doc
	      "\n\n--------------------------\n"
	      ":KEYWORD-LISTS-IN `" ,(symbol-name var-name) "' a <VARBIABLE> with value:\n\n"
	      ,v-val
	      ,f-doc
	      "\n--------"
	      ,x-ref))
       ;; (put const-name 'variable-documentation ,c-doc)
       (plist-put (symbol-plist ',const-name) 'variable-documentation ,c-doc)
       ;; (put var-name 'variable-documentation ,c-doc)
       (plist-put (symbol-plist ',var-name)   'variable-documentation ,v-doc))))
;;
;;; (put 'mon-help-swap-var-doc-const-val  'lisp-indent-function <INT>) 
;;
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (progn
;;|   (defface test-swap-var->const-face
;;|             '((((class color) (background light)) (:foreground "CornflowerBlue"))
;;|               (((class color) (background dark)) (:foreground "CornflowerBlue"))
;;|               (t (:bold t :italic t)))
;;|           "*Face font-locking of institution name keywords in .naf files.
;;| Additional documentation in var `test-swap-var->const-fface'")
;;| (defvar test-swap-var->const-fface 'test-swap-var->const-face
;;|            "*Face for `naf-mode' font-locking of institution name keywords
;;| :KEYWORDS-IN the regexp defined in: {:SEE-ALSO list of XREFD var names}.
;;| Face definition in `test-swap-var->const-face'.")
;;| (defvar *test-swap-var-xrefs*
;;|   '(*test-swap-var->const* *test-swap-var->const2* *test-swap-var->const3* *test-swap-var-xrefs*)
;;|   "List of symbol names of variables which xref each other in `test-swap-var-mode'.
;;| :SEE :FILE \"./test-swap-var-mode.el\".")
;;| (defvar *test-swap-var->const* '("list" "of" "keywords" "to" "fontlock")
;;|           "Did this docstring get swapped to docstring of `test-swap-var->cons'?.
;;|   Is its value at the top of docstring?")
;;| (defconst test-swap-var->const (regexp-opt *test-swap-var->const* 'paren)))
;;`----
;;
;;,---- :EVAL-BELOW-TO-TEST
;;| (mon-help-swap-var-doc-const-val *test-swap-var->const* test-swap-var->const)
;;| (mon-help-swap-var-doc-const-val
;;|     *test-swap-var->const* test-swap-var->const
;;|     *test-swap-var-xrefs*  test-swap-var->const-fface)
;;`----
;;
;;,---- :CLEANUP
;;| (progn (makunbound '*test-swap-var->const*)     (unintern "*test-swap-var->const*" obarray)
;;|        (makunbound 'test-swap-var->const)       (unintern "test-swap-var->const" obarray)
;;|        (makunbound '*test-swap-var-xrefs*)      (unintern "*test-swap-var-xrefs*" obarray)
;;|        (makunbound 'test-swap-var->const-face)  (unintern "test-swap-var->const-face" obarray)
;;|        (makunbound 'test-swap-var->const-fface) (unintern "test-swap-var->const-fface" obarray)))
;;`----
;;

;;; ==============================
;;; :NOTE Not entirely correct for fncns where byte symbol is byte compiled
;;;       compare output of interpreted:
;;;        (mon-help-xref-symbol-value 'mon-help-xref-symbol-value)
;;;       versus byte-compiled:
;;;        (byte-compile 'mon-help-xref-symbol-value)
;;;        (mon-help-xref-symbol-value 'mon-help-xref-symbol-value)
;;;        (symbol-function 'mon-help-xref-symbol-value)
;;;  
;;; :TODO Need to check if symbol is compiled e.g. `byte-code-function-p' and if
;;;       so, pull apart the vector. :SEE (info "(elisp) Byte-Code Objects")
;;; (aref (symbol-function 'mon-help-xref-symbol-value) 2)
;;; (vconcat (symbol-function 'mon-help-xref-symbol-value))
;;; (vconcat (indirect-function 'mon-help-xref-symbol-value))
;;; :NOTE `apropos-safe-documentation' returns the .elc file doc offset:
;;;  (apropos-safe-documentation 'mon-help-xref-symbol-value)
;;; :CREATED <Timestamp: #{2009-09-30T16:44:24-04:00Z}#{09403} - by MON KEY>
(defun mon-help-xref-symbol-value (sym)
  "Return the value of symbol SYM.\n
When SYM is a function return `symbol-function' of symbol.
When SYM is a variable return `symbol-value' of symbol.
value returned is of the form:
\(\(SYMBOL <FUNCTION>|<VARIABLE>\) \(VALUE-OF-FUNICTION-OR-VARIABLE\)\n
:EXAMPLE\n(mon-help-xref-symbol-value 'mon-help-xref-symbol-value)\n
\(mon-help-xref-symbol-value '*w32-env-variables-alist*\)\n
:SEE-ALSO `mon-help-function-spit-doc', `mon-help-swap-var-doc-const-val',
`mon-help-parse-interactive-spec', `mon-help-function-args',
`mon-help-function-arity', `symbol-function', `indirect-function',
`symbol-value', `indirect-variable', `apropos-safe-documentation'.\n►►►"
  (let* ((is-sym (intern-soft sym))
         (sym-type-val (cond (;; :TODO incorporate `mon-function-object-p' here:
                              ;; (byte-code-function-p (indirect-function 'is-sym))
                              (and (fboundp is-sym) (functionp is-sym)) 
                              `((,is-sym ,'<FUNCTION>) ,(symbol-function is-sym)))
                             (;; (indirect-variable 'is-sym)
                              (bound-and-true-p is-sym)
                              `((,is-sym '<VARIABLE>) ,(symbol-value is-sym))))))
    sym-type-val))

;; (indirect-variable *mon-tags-table-list*)
;; (symbol-value      '*mon-tags-table-list*) 
;; (symbol-function   'mon-help-insert-documentation)
;; (indirect-function 'mon-insert-documentation)
;; (eq ;; eql equal
;;  (symbol-function   'mon-help-insert-documentation)
;;  (indirect-function 'mon-insert-documentation))

;;; ==============================
;;; :NOTE The do-var arg doesn't work for byte-compiled vars in .elc files on:
;;; "GNU Emacs 23.1.50.1 (i386-mingw-nt5.1.2600)
;;;  of 2009-06-30 on LENNART-69DE564 (patched)"
;;;  Not sure what is happening with that. Interpreted vars are fine...
;;; :CREATED <Timestamp: Thursday July 02, 2009 @ 05:16.20 PM - by MON KEY>
(defun* mon-help-function-spit-doc (sym-name &key alt-cookie do-var insertp
                                             do-face do-group do-theme)
  "Return documentation for function with SYM-NAME.\n
When keyword :ALT-COOKIE \(a string\) is non-nil overrides the default comment
delimiter set in global var `*mon-doc-cookie*' - \"\u25BA\u25BA\u25BA\".\n
If :ALT-COOKIE is not present in SYM-NAME's docstring header of docstring is
inserted uncommented.\n
When keyword :INSERTP is non-nil insert documentation in current buffer.\n
When keyword  :DO-VAR is non-nil get documentation of a variable or constant.\n
:DO-VAR should be t when invoked for variable, constant, custom documentation,
e.g. symbols defined inside a defvar, defconst, or defcustom form.\n
When keyword :DO-FACE is non-nil get face documentation for sym-name.
:DO-FACE should be t when invoked for face documentation, e.g. symbols defined
inside a defface form.\n
When keyword :DO-GROUP is non-nil get face documentation for sym-name.
:DO-GROUP should be t when invoked for group documentation, e.g. symbols defined
inside a defgroup form.\n
When keyword :DO-THEME is non-nil get face documentation for sym-name.
:DO-THEME should be t when invoked for group documentation, e.g. symbols defined
inside a defgroup form.\n
:EXAMPLE\n\n\(mon-help-function-spit-doc 'mon-help-function-spit-doc\) ;defun
\(mon-help-function-spit-doc '*mon-doc-cookie* :do-var t\) ;defvar
\(mon-help-function-spit-doc 'eldoc-message-commands :do-var t\) ;defconst
\(mon-help-function-spit-doc 'completions-merging-modes :do-var t\) ;defcustom
\(mon-help-function-spit-doc 'font-lock-keyword-face :do-face t\) ;defface
\(mon-help-function-spit-doc 'apropos :do-group t\) ;defgroup\n
:SEE-ALSO `mon-insert-doc-help-cookie', `mon-insert-doc-help-tail',
`mon-help-xref-symbol-value', `mon-help-insert-documentation',
`mon-help-function-args', `mon-help-buffer-spc-*DOC*', `documentation-property',
`apropos-documentation-property', `byte-compile-output-docform',
`lambda-list-keywords', `subr-arity', `help-function-arglist',
`help-add-fundoc-usage', `elint-put-function-args'.\n►►►"
  ;; (eval-when-compile (require 'mon-cl-compat nil t))
  (let (mk-docstr)
    (save-excursion
      (setq mk-docstr
            (with-temp-buffer
              (emacs-lisp-mode)
              ;; :WAS (let* ((check-opt (if alt-cookie (string-to-list alt-cookie)))
              ;; (dc (if (and alt-cookie (stringp alt-cookie))
              ;;         (if (intern-soft "cl::intersection")
              ;;          (cond ((cl::intersection (string-to-list "[*?^.+$\\") check-opt)
              ;;                 (regexp-quote alt-cookie))
              ;;                (t alt-cookie))
              ;;        (cond ((intersection (string-to-list "[*?^.+$\\") check-opt)
              ;;               (regexp-quote alt-cookie))
              ;;              (t alt-cookie)))
              ;;    *mon-doc-cookie*))
              (let* ((dc  (if (and alt-cookie (stringp alt-cookie))
                              (let ((check-opt `(,(string-to-list alt-cookie) 
                                                 ,(string-to-list "[*?^.+$\\")))
                                    do-opt)
                                (dolist (altc (car check-opt)
                                              (setq check-opt 
                                                    (if do-opt 
                                                        (regexp-quote alt-cookie) 
                                                      alt-cookie)))
                                  (unless do-opt
                                    (when (memq altc (cadr check-opt))
                                      (setq do-opt t)))))
                            *mon-doc-cookie*))
                     (st-mrk (make-marker))
                     (cookie-mrk (make-marker))
                     put-help help-bnds ret-str)
              (setq put-help
                    (cond (;;(functionp sym-name)
                           (memq (mon-function-object-p sym-name) '(function lambda macro))
                           (or (documentation sym-name)
                               ;; (documentation-property sym-name 'function-documentation)
                               (unless (byte-code-function-p sym-name)
                                 (when ;; :WAS (stringp (caddr (symbol-function sym-name)))
                                     (stringp (caddr (indirect-function sym-name)))
                                   (caddr (symbol-function sym-name)) )))) ;; fncns, macros.
                          (do-var (or ;; (plist-get (symbol-plist sym-name) 'variable-documentation)
                                   (get sym-name 'variable-documentation)
                                   (documentation-property sym-name 'variable-documentation))) ;; var, const, customs.
                          (do-face (or (face-documentation sym-name)
                                       (plist-get (symbol-plist sym-name) 'face-documentation)
                                       (documentation-property sym-name 'face-documentation))) ;; faces.
                          (do-group (or (plist-get (symbol-plist sym-name) 'group-documentation)
                                        (documentation-property sym-name 'group-documentation))) ;; groups.
                          (do-theme (or (plist-get (symbol-plist sym-name) 'theme-documentation)
                                        ;; :NOTE consider `theme-settings' here.
                                        (documentation-property sym-name 'theme-documentation))) 
                          ;; Doubtful this makes any sense:
                          ;;(do-widget { ... } `widget-documentation` `widget-type`
                          (t (documentation sym-name))))
                (set-marker st-mrk (point))
                (setq help-bnds (+ (marker-position st-mrk) (length put-help)))
                (princ put-help (current-buffer))
                ;; (prin1 put-help (current-buffer))
                (goto-char st-mrk)
                (search-forward-regexp (concat "\\(" dc "\\)") help-bnds t)
                (set-marker cookie-mrk (point))
                (if (equal st-mrk cookie-mrk)
                    nil
                    (progn
                      (comment-region st-mrk cookie-mrk)
                      (goto-char st-mrk)
                      (search-forward-regexp (concat "\\(" dc "\\)") cookie-mrk t)
                      (replace-match "")))
                (setq ret-str  (buffer-string))
                ret-str))))
    (when insertp (save-excursion (newline) (princ mk-docstr (current-buffer))))
    mk-docstr))
;;
;;; :TEST-ME (mon-help-function-spit-doc 'mon-help-function-spit-doc :alt-cookie nil :do-var nil :insertp t)
;;; :TEST-ME (mon-help-function-spit-doc '*regexp-mon-doc-help-pointer-tags* :do-var t :insertp t)
;;; :TEST-ME (mon-help-function-spit-doc 'font-lock-keyword-face :do-face t :insertp t)
;;; :TEST-ME (mon-help-function-spit-doc 'apropos :do-group t :insertp t)

;;; ==============================
;;; :TODO
;;; - This function _should_ test if it found a variable.
;;;   If so, it should change the fomrat spec of 'fstrings'  to:
;;;   "\n(mon-help-function-spit-doc '%s nil t t)
;;; - The test-me subr should insert 4 ':TEST-ME's for functions as:
;;;   ":TEST-ME (<FNAME>)"
;;;   ":TEST-ME (<FNAME> t)"
;;;   ":TEST-ME (describe-function '<FNAME>)"
;;;   ":TEST-ME (call-interactively '<FNAME>)"
;;;   When a variable is found should insert:
;;;   ":TEST-ME <VARNAME>"
;;;   ":TEST-ME (describe-variable '<VARNAME>)"
;;;
;;; :NOTE Use (beginning-of-defun 2) w/ *regexp-symbol-defs*.
;;; :CREATED <Timestamp: Thursday July 16, 2009 @ 11:38.10 AM - by MON KEY>
(defun mon-insert-doc-help-tail (&optional fname test-me-cnt insertp intrp)
  "Return function body code template when body uses a docstring instrospection.\n
Additionally, for functions which call `mon-help-function-spit-doc' in the body 
insert ';;; :TEST-ME ' templates after the closing of defun.\n
When FNAME \(a string\) is non-nil don't search for function's name in head of
defun, instead substitute FNAME.\n
When TEST-ME-CNT is non-nil include N ';;; :TEST-ME' strings with returned template.
Default TEST-ME-CNT is 3 ';;; :TEST-ME's.\n
When optional arg INSERTP is non-nil or when called-interactively insert code
template in buffer at point. Do not move point.\n
:NOTE Regexp used to identify symbol-types in variable `*regexp-symbol-defs*'.\n
:SEE-ALSO `mon-insert-lisp-testme', `mon-help-regexp-symbol-defs-TEST'.\n►►►"
  (interactive "i\ni\ni\np")
  (let* ((midht-sym-nm 
          (progn 
            (setq fname 
                  (and fname 
                       (or (mon-string-not-null-nor-zerop fname)
                           (mon-format :w-fun #'error
                                       :w-spec '(":FUNCTION `mon-insert-doc-help-tail' "
                                                 "-- arg FNAME does not satisfy "
                                                 "`mon-string-not-null-nor-zerop', "
                                                 "got: %S type-of: %s")
                                       :w-args `(,fname ,(type-of fname))))
                       fname))
            (and (or (save-excursion (search-backward-regexp *regexp-symbol-defs-big* nil t))
                     (mon-format :w-fun #'error 
                                 :w-spec '(":FUNCTION `mon-insert-doc-help-tail' "
                                           "could not find match for `*regexp-symbol-defs-big*'"
                                           " search-back from, posn: %d")
                                 :w-args (point))))
            (or (and (not fname) (match-string-no-properties 3))
                (and fname 
                     (or (equal (match-string-no-properties 3) fname)
                         (mon-format :w-fun #'error 
                                     :w-spec '(":FUNCTION `mon-insert-doc-help-tail' "
                                               "nearest match for `*regexp-symbol-defs-big*'"
                                               "not like arg FNAME, %s"
                                               "matched: %S ")
                                     :w-args `(,fname ,(match-string-no-properties 0))
                                     :w-delim (concat "\n" (make-string 38 32) "-- "  )))
                     fname))))
         (midht-tm-cnt (or test-me-cnt 3))
         (midht-fnd       (match-string-no-properties 2))
         (midht-fun-typs '("defun" "defun*" "defmacro" 
                           "defmacro*" "defsubst" "defsubst*"))
         (var-typs       '("defvar" "defconst" "defcustom"))
         midht-msg-key
         ;; Signature (sym-name &key :alt-cookie :do-var :insertp :do-face :do-group :do-theme
         (sym-str-cond (cond ((car (member midht-fnd midht-fun-typs))
                              (let ((fun-typ (car (member midht-fnd midht-fun-typs))))
                                (setq midht-msg-key 
                                      (if (or (string= fun-typ "defmacro")
                                              (string= fun-typ "defmacro*"))
                                          ":MACRO"
                                        ":FUNCTION")))
                              "      (mon-help-function-spit-doc '%s :insertp t)\n")
                             ((car (member midht-fnd var-typs))
                              (setq midht-msg-key 
                                    (if (string= (car (member midht-fnd var-typs)) "defconst")
                                        ":CONSTANT"
                                      ":VARIABLE"))
                              "      (mon-help-function-spit-doc '%s :do-var t :insertp t)\n")
                              ((string= midht-fnd "defface")
                               (setq midht-msg-key ":FACE")
                              "      (mon-help-function-spit-doc '%s :do-face t :insertp t)\n")
                              ((string= midht-fnd "defgroup")
                               (setq midht-msg-key ":GROUP")
                               "      (mon-help-function-spit-doc '%s :do-group t :insertp t)\n")
                              ((string= midht-fnd "deftheme")
                               (setq midht-msg-key ":THEME")
                               "      (mon-help-function-spit-doc '%s :do-theme t :insertp t)\n")
                              (t (setq midht-msg-key ":SYMBOL")
                                 "      (mon-help-function-spit-doc '%s :insertp t)\n")))
         (fstring (concat 
                   (or (and (equal midht-msg-key ":FUNCTION")
                            (format
                             (concat "  (interactive \"i\\nP\")\n" 
                                     "  (if (or insertp intrp)\n"
                                     sym-str-cond
                                     "    (mon-help-message-intrp %S)))\n;;\n")
                             midht-sym-nm
                             midht-sym-nm))
                       (format sym-str-cond midht-sym-nm))
                   (if fname
                       (replace-regexp-in-string "$" (concat "(" fname " )")
                                                 (mon-insert-lisp-testme nil midht-tm-cnt nil))
                     (mon-insert-lisp-testme t midht-tm-cnt nil)))))
    (if (or intrp insertp)
        (progn
          (save-excursion 
            (or (eql (line-beginning-position) (line-end-position))
                (newline))
            (insert fstring))
          fstring)
      fstring)))

;; (mon-insert-doc-help-tail '88)
;;
;;; :TEST-ME (mon-insert-doc-help-tail)
;;; :TEST-ME (mon-insert-doc-help-tail nil 3)
;;; :TEST-ME (mon-insert-doc-help-tail nil nil t)
;;; :TEST-ME (mon-insert-doc-help-tail)
;;; :TEST-ME (mon-insert-doc-help-tail "some-function" 3)
;;; :TEST-ME (mon-insert-doc-help-tail "some-function-name" 3 t)
;;; :TEST-ME (mon-insert-doc-help-tail "some-function-name" nil t)
;;; :TEST-ME (apply 'mon-insert-doc-help-tail '(t))

;;; ==============================
;;; :CHANGESET 2356
;;; :CREATED <Timestamp: #{2010-12-08T17:58:55-05:00Z}#{10493} - by MON KEY>
(defun mon-help-message-intrp (fun-name) ;;  &optional is-macro) <- dumb!
  "Message user to evaluate `mon-help-*' FUN-NAME interactively.\n
Arg FUN-NAME is a string or symbol.\n
If it is a symbol it should satisfy `symbolp' but not `mon-booleanp'.
If it is  string it shouls satisfy `mon-string-not-null-nor-zerop'.\n
If it is neither signal an error.\n
:EXAMPLE\n\n\(mon-help-message-intrp \"bubba\"\)\n
\(mon-help-message-intrp 'bubba\)\n
;; Following fail successfully:
\(mon-help-message-intrp nil\)\n
\(mon-help-message-intrp t\)\n
\(mon-help-message-intrp \(\)\)\n
\(mon-help-message-intrp []\)\n
\(mon-help-message-intrp 8\)\n
:SEE-ALSO `mon-insert-doc-help-tail', `mon-format', `mon-message',
`mon-booleanp', `mon-string-not-null-nor-zerop',
`mon-error-string-err-format', `interactive-form'.\n►►►"
  (setq fun-name 
        (or (and (mon-string-not-null-nor-zerop fun-name))
            (or (and (or (not (atom fun-name)) 
                         ;; Don't permit t|nil 
                         (cadr (mon-booleanp fun-name))
                         ;; Don't allow non symbols
                         (not  (symbolp fun-name))
                         (null fun-name))
                     (mon-error-string-err-format "mon-help-message-intp"
                                                  "fun-name"
                                                  fun-name t))
                (format "%s" fun-name))))
  ;; This was stupid, its not possible to define a macro as interactive!!!
  ;; (setq is-macro
  ;;       (or (and is-macro 
  ;;                (eq (mon-function-object-p (intern-soft fun-name)) 'macro)
  ;;                ":MACRO `")
  ;;           ":FUNCTION `"))
  (mon-message :msg-spec `(":FUNCTION `" ,fun-name 
                           "' -- pass non-nil for optional arg INTRP")))

 
;;; ==============================
;;; :COURTESY Dave Love <fx@gnu.org> :HIS fx-misc.el :WAS `function-arity'
;;; :NOTE On the CL functions with &keys (mon-help-function-arity 'reduce)
;;; :CREATED <Timestamp: #{2009-12-19T01:06:00-05:00Z}#{09516} - by MON>
(defun mon-help-function-arity (function)
  "Return information on the arity \(argument numbers\) of FUNCTION.\n
The result is of the form returned by `subr-arity' or the symbol
`unknown' for an autoloaded function (whose arity is unknown).\n
FUNCTION must be a function \(or special form\) according to
`functionp', or else a macro.\n
:EXAMPLE\n\n\(mon-help-function-arity 'mon-help-function-arity)\n
\(mon-help-function-arity 'reduce\)\n
:NOTE The CL-seq functions with &keys e.g. `reduce' returns 'many'
as the cl-keys occurs in the &rest parameter position. This also occurs with
functions defined with the CL packages `defun*' macro.
:SEE `lambda-list-keywords'.\n
:ALIASED-BY `mon-function-arity'\n
:SEE-ALSO `mon-help-function-args', `documentation-property',
`apropos-documentation-property' `byte-compile-output-docform',
`help-function-arglist', `help-add-fundoc-usage', `subr-arity',
`elint-put-function-args', `mon-help-parse-interactive-spec', 
`interactive-form', `mon-help-buffer-spc-*DOC*'.\n►►►"
  (setq function (indirect-function function))
  (cond ((eq 'autoload (car-safe function))
	 'unknown)
	((subrp function)
	 (subr-arity function))
	(t				; macro, lambda or byte code
	 (let ((min-args 0)
	       lambda-list max-args &optional)
	   (if (eq 'macro (car-safe function))
	       (pop function)		; now byte code or lambda
	     (unless (functionp function)
	       (signal 'invalid-function (list function))))
	   (if (eq 'lambda (car-safe function))
	       (setq lambda-list (cadr function))
	     (if (not (byte-code-function-p function))
		 'unknown		; shouldn't happen
	       (setq lambda-list (aref function 0))))
	   ;; We've got a lambda list.
	   (while (and lambda-list (not (eq 'many max-args)))
	     (cond ((eq (car lambda-list) '&optional)
		    (setq &optional 0))
		   ((eq (car lambda-list) '&rest)
  		    (setq max-args 'many))
		   (t
		    (if &optional
			(setq &optional (1+ &optional))
		      (setq min-args (1+ min-args)))))
	     (pop lambda-list))
	   (unless max-args (setq max-args (+ min-args (or &optional 0))))
	   (cons min-args max-args)))))
;;
;;; :TEST-ME (mon-help-function-arity 'mon-file-stamp)

 
;;; ==============================
;;; :CHANGESET 1987
;;; :CREATED <Timestamp: #{2010-07-17T13:35:11-04:00Z}#{10286} - by MON KEY>
(defcustom *mon-help-emacs-errors* nil
  "A list of plists of emacs error-symbols.\n
The car of list is a plist of Emacs' \"standard\" error-symbols.\n
Elements of car are key/value pairs with the format:\n
 :ERROR-EMACS-<ERROR-SUPERCLASS> \(<ERROR-SYMS>*\)\n
where the key :ERROR-EMACS-<ERROR-SUPERCLASS> is a conceptual \"superclass\" of
error type, and the value \(<ERROR-SYMS>*\) is an enumerated list of symbols
which occupy the conceptual domain of the \"superclass\".\n
The cadr is as above, but instead identifies error-symbols defined in packages
Emacs which are not loaded by default. Elements of cadr are key/value pairs with
the format:\n
 :ERROR-EMACS-DIST-<PACKAGE-NAME> \(<ERROR-SYMS>*\)\n
where the key :ERROR-EMACS-DIST-<PACKAGE-NAME> has as its suffix the name of an
Emacs package, e.g. :ERROR-EMACS-DIST-JSON for the lisp/json.el package.\n
Packages not distributed with Emacs may also appear in this list but the key has
the format: :ERROR-<PACKAGE-NAME> e.g. :ERROR-SLIME for the slime.el package.\n
Used to generate docstring of `mon-help-errors'.\n
:EXAMPLE\n\n\(plist-get \(car *mon-help-emacs-errors*\)  :ERROR-EMACS-SYMBOL\)\n
\(plist-get \(cadr *mon-help-emacs-errors*\) :ERROR-EMACS-DIST-JSON\)\n
:SEE info node `(elisp)Standard Errors'\n
:SEE-ALSO `mon-help-CL-error-condition-restart'.\n►►►"
  :type '(repeat plist :value-type (repeat symbol))
  :group 'mon-doc-help-utils)
;;
(unless (bound-and-true-p *mon-help-emacs-errors*)
  (setq *mon-help-emacs-errors*
        '((:ERROR-EMACS-STANDARD
           (error quit no-catch)
           :ERROR-EMACS-ARGS
           (args-out-of-range wrong-number-of-arguments wrong-type-argument
            invalid-read-syntax invalid-regexp)
           :ERROR-EMACS-SYMBOL
           (invalid-function setting-constant void-function void-variable
            cyclic-function-indirection cyclic-variable-indirection)
           :ERROR-EMACS-BUFFER
           (beginning-of-buffer buffer-read-only end-of-buffer mark-inactive
            text-read-only)
           :ERROR-EMACS-FILE
           (file-error file-already-exists file-date-error end-of-file)
           :ERROR-EMACS-FILE-NO-PROPS 
           ;; :NOTE These w/out error-condition error-message props
           (file-locked file-supersession ftp-error)
           :ERROR-EMACS-MATHS
           (arith-error domain-error overflow-error range-error
            singularity-error underflow-error )
           :ERROR-EMACS-MISC
           (cl-assertion-failed coding-system-error scan-error search-failed
            undefined-color
            ;; :NOTE These conditions and messgae may not be found. 
            ;;        If not move back to :ERROR-EMACS-DIST-PACKAGES
            bookmark-error-no-filename epg-error compression-error))
          ;;
          (:ERROR-EMACS-DIST-PACKAGES
           (;; bookmark-error-no-filename
            ;; epg-error
            ;; compression-error
            ;; compression-error
            parse-error nntp-authinfo-rejected sasl-error error-file-not-found
            kkc-error quail-error js-moz-bad-rpc js-js-error mpc-proc-error)
           :ERROR-EMACS-DIST-JSON
           (json-error json-readtable-error json-unknown-keyword
            json-number-format json-string-escape json-string-format
            json-object-format)
           :ERROR-EMACS-DIST-CALC
           (inexact-result math-overflow math-underflow)
           :ERROR-EMACS-DIST-NXML
           (nxml-outline-error nxml-scan-error nxml-file-parse-error
            rng-c-incorrect-schema rng-compile-error rng-uri-error
            xmltok-markup-declaration-parse-error xsdre-invalid-regexp
            xsdre-parse-error)
           :ERROR-EMACS-DIST-EIEIO
           (no-method-definition no-next-method invalid-slot-name
            invalid-slot-type unbound-slot)
           :ERROR-SLIME
           (slime-incorrect-feature-expression
            slime-unknown-feature-expression))))
  (custom-note-var-changed '*mon-help-emacs-errors*))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-07-16T14:10:29-04:00Z}#{10285} - by MON KEY>
(defun mon-help-errors (&optional insertp intrp)
  ""
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-errors :insertp t)
    (mon-help-message-intrp "mon-help-errors")))
;;
;;; :PREFIX "mhe-
;;; Now build the docstring up from values in variable `*mon-help-emacs-errors*'.
(eval-when (compile load eval)
  (let (mhe-gthr mhe-gthr-msg mhe-gthr-tmp)
    (dolist (mhe-D-0 (car *mon-help-emacs-errors*)
                     (dolist (mhe-D-1 (setq mhe-gthr (nreverse mhe-gthr))
                                      (setq mhe-gthr (nreverse mhe-gthr-msg)))
                       (push mhe-D-1 mhe-gthr-msg)
                       (dolist (mhe-D-2 (plist-get (car *mon-help-emacs-errors*) mhe-D-1)
                                        (push (concat "\n" 
                                                      (mapconcat #'identity
                                                                 (setq mhe-gthr-tmp (nreverse mhe-gthr-tmp)) "\n"))
                                                
                                              mhe-gthr-msg))
                         (push (format "error-symbol:  `%s`\nerror-tower:   %S\nerror-message: %S\n"
                                       mhe-D-2 
                                       (or (get mhe-D-2 'error-conditions) 'NULL)
                                       (or (get mhe-D-2 'error-message) ""))
                               mhe-gthr-tmp))
                       (setq mhe-gthr-tmp)))
      (unless (consp mhe-D-0)
        (push mhe-D-0 mhe-gthr)))
    ;; Now build the second list, but don't look for error-conditions or
    ;; error-message props because the packages may not be in the environment.
    (setq mhe-gthr-tmp)
    (setq mhe-gthr-msg)
    (dolist (mhe-D-3 (cadr *mon-help-emacs-errors*)
                     (setq mhe-gthr-msg (mapconcat #'identity (nreverse mhe-gthr-msg) "")))
      (if (not (consp mhe-D-3))
          (push (format "\n\n;; %S\n" mhe-D-3) mhe-gthr-msg)
        (push (mapconcat #'(lambda (mhe-L-0) 
                             (format "error-symbol:  `%s`" mhe-L-0))
                         mhe-D-3 "\n")
              mhe-gthr-msg)))
    ;; Now gather it all up into a unified docstring.
    (setq mhe-gthr 
          (concat 
           "A list of \"standard\" emacs error-symbols and error-symbols.\n\n"
           "List also includes error-symbols defined in packages distributed with Emacs.\n"
           "Each \"standard\" error-symbol includes the symbol-name, and (when present) the\n"
           "symbols condition hiearchy e.g. its error-conditions property, and the symbols\n" 
           "error-message property.\n"
           (mapconcat #'identity
                      (mapcar #'(lambda (mhe-L-1)
                                  (if (stringp mhe-L-1)
                                      mhe-L-1
                                    (format "\n;; %s" mhe-L-1)))
                              mhe-gthr) "")
           ;; Remove the first "\n"
           (substring mhe-gthr-msg 1)
           (mapconcat #'identity 
            '("\n" 
              ";; :ERROR-FUNCTIONS"
              "`condition-case'"
              "`condition-case-no-debug'"
              "`signal'"
              "`ignore-errors'"
              "`init-file-had-error'"
              "`with-demoted-errors'"
              "`report-errors'"
              "`error-message-string'"
              "`Info-no-error'"
              "\n"
              ";; :ERROR-FUNCTIONS-FILE-LOCKED"  ;; :SEE lisp/userlock.el
              "`ask-user-about-lock'"
              "`ask-user-about-supersession-threat'"
              "`ask-user-about-supersession-help'"
              "`ask-user-about-lock-help'"
              "\n"
              ";; :ERROR-VARIABLES"
              "`command-error-function'"
              "`elint-extra-errors'"
              "`eval-expression-debug-on-error'"
              "`signal-hook-function'"
              "\n"
              ";; :ERROR-FUNCTIONS-DEBUG"
              "`backtrace'"
              "`backtrace-debug'"
              "`backtrace-frame'"
              "`cancel-debug-on-entry'"
              "`debug-on-entry'"
              "`toggle-debug-on-error'"
              "\n"
              ";; :ERROR-VARIABLES-DEBUG"
              "`*Backtrace*`              ;<BUFFER-NAME>"
              "`debugger'"
              "`debug-function-list'"
              "`debug-ignored-errors'"
              "`debug-on-exit'"
              "`debug-on-next-call'"
              "`debug-on-signal'"
              "`debug-on-error'"
              "`debugger-may-continue'"
              "`max-specpdl-size'"
              "`stack-trace-on-error'"
              "`memory-full'"
              "`exit'   ;:NOTE Interned from eval.c with: \"Qexit = intern_c_string \(\"exit\"\);\""
              "`debug'     \(symbol-plist 'debug\)"
              "`&rest'     \(symbol-plist '&rest\)"
              "`&optional' \(symbol-plist '&optional\)"
              "\n"
              ";; :ERROR-FUNCTIONS-OUTPUT"
              "`external-debugging-output'"
              "`redirect-debugging-output'"
              "\n"
              ";; :ERROR-FUNCTIONS-C-PRIMITIVES"
              ;; Fsignal 
              ;; emacs_strerror
              "report_file_error"
              ;; :FILE src/eval.c
              ;; Vsignaling_function
              ;; Vstack_trace_on_error
              ;; Vdebug_on_error
              ;; Qdebug_on_error
              ;; `grow_specpdl`    ->  "Variable binding depth exceeds max-specpdl-size"
              ;; `specbind`        -> "Frame-local vars cannot be let-bound"
              ;; "`fetch-bytecode' -> \"Invalid byte code in %%s\" | \"Invalid byte code\""
              ;; `defvaralias' -> "Cannot make a constant an alias" 
              ;;                  "Cannot make an internal variable an alias"
              ;;                  "Don't know how to make a localized variable an alias"
              ;;                  "Don't know how to make a let-bound variable an alias"
              ;; `defvar' -> "Too many arguments"
              ;;             "Constant symbol `%%s' specified in defvar"
              ;; `defconst' -> "Too many arguments"
              ;; `let*'     -> "`let' bindings can have only one value-form"
              ;; `let'      -> "`let' bindings can have only one value-form"
              ;; `condition-case' -> "Invalid condition handler"
              ;; `signal' -> "Cannot return from the debugger in an error"
              ;; `autoload' -> "Attempt to autoload %%s while preparing to dump"
              ;;               "Autoloading failed to define function %%s"
              ;; `eval' -> "Lisp nesting exceeds `max-lisp-eval-depth'"
              ;; `funcall' -> "Lisp nesting exceeds `max-lisp-eval-depth'"
              "\n"
              ":NOTE The \"non-standard\" error-symbols enumerated above do not include"
              "error-conditions and error-message properties as their defining package may not"
              "yet be in the environment and does not indicate that these properties are void"
              "on the error-symbol's plist.  Once an error-symbols defining package is present"
              "in the environment, its error properties are accessed with the following form:\n"
              " `\(<SYMBOL> \(get '<SYMBOL> 'error-conditions\)"
              "           \(get '<SYMBOL> 'error-message\)\)\n"
              ":SEE info node `(elisp)Standard Errors'"
              ":SEE :FILE src/eval.c"
              ":SEE-ALSO `*mon-help-emacs-errors*', `mon-help-CL-error-condition-restart'.\n►►►") "\n")))
    (put 'mon-help-errors 'function-documentation mhe-gthr)))
;;
;;; :TEST-ME (mon-help-errors)
;;; :TEST-ME (mon-help-errors t)
;;; :TEST-ME (describe-function 'mon-help-errors)

;;; ==============================
;;; <Timestamp: #{2010-07-29T15:03:13-04:00Z}#{10304} - by MON KEY>
;;; :TODO Add edebug related function.
;;;
;;; `def-edebug-spec'
;;; `edebug-eval-macro-args'
;;; (info "(elisp)Specification List")
;;; (info "(elisp)Definition of declare")
;;;
;;; ==============================

 
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-07T19:54:58-04:00Z}#{09371} - by MON KEY>
;;; :FIXES CL &key &aux args in the tail of Elisp &rest e.g.
;;;       (help-function-arglist 'mon-help-function-spit-doc)
;;;       ;=> (sym-name &rest --cl-rest--)
;;; :NOTE This isn't per se a problem but it does happen:
;;;       (help-function-arglist 'reduce)
;;;       ;=> (cl-func cl-seq &rest cl-keys)
;;;
;;;       With regards docstring snarfing following shows the commponents of a
;;;       byte-compiled-function:
;;;       `byte-compile-output-docform'  (preface name info form specindex quoted)
;;;        INFO is a list of three elements (PREFIX DOC-INDEX POSTFIX)
;;;        e.g. bind the free-variable OUTBUFFER to some dest and:
;;;        (let ((outbuffer (current-buffer)))
;;;              (byte-compile-output-docform nil nil '("@►►►" 0 "@◄◄◄")
;;;                                '( <SOME-LISP-FORM-HERE> )
;;;                                t t))
;;;       Further along that output gets translated to:
;;;       #@144 ;; chars left until we hit the `^_' eg char 31 ?\x1F
;;;       `^_' ;<- docstring terminator followed by newline 
;;;       (defalias 'FUNCNAME #[(arg1 arg2 &rest cl-keys) #@710 
;;;       chars left to end of list only when &keys ------'
;;; :CREATED <Timestamp: #{2009-08-20T21:24:31-04:00Z}#{09345} - by MON>
(defun mon-help-function-args (w-func)
  "Return arg list of W-FUNC.\n
:EXAMPLE\n\n\(mon-help-function-args 'mon-help-function-args\)\n
;; Following was defined with CL arg-list with &key\n
\(mon-help-function-args 'mon-help-function-spit-doc\)\n 
:NOTE May return misleading results when the CL marcros are in play.\n
:SEE `lambda-list-keywords'.\n
:CALLED-BY `mon-help-insert-documentation'.\n
:ALIASED-BY `mon-function-args'\n
:SEE-ALSO `mon-help-function-arity', `mon-help-xref-symbol-value',
`mon-help-parse-interactive-spec', `mon-help-function-spit-doc',
`mon-help-buffer-spc-*DOC*', `documentation-property',
`apropos-documentation-property' `subr-arity', `help-function-arglist',
`help-add-fundoc-usage', `byte-compile-arglist-signature',
`byte-compile-output-docform', `elint-put-function-args'
`lambda-list-keywords'.\n►►►"
  (let ((mhfa-def (help-function-arglist w-func))
        mhfa-tst-def)
    (when (and mhfa-def (memq '&rest mhfa-def))
      (mapc #'(lambda (mhfa-L-1) 
                (when (string-equal (format "%s" mhfa-L-1) "--cl-rest--")
                  (setq mhfa-tst-def t)))
            mhfa-def))
    (if mhfa-tst-def
        (let (mhfa-get-args)
          (setq mhfa-get-args
                (with-temp-buffer
                  (let (mhfa-sexp-bnds)
                    (princ (documentation w-func) (current-buffer))
                    (mon-g2be 1)
                    (backward-sexp)
                    (save-match-data (setq mhfa-sexp-bnds (bounds-of-thing-at-point 'sexp)))
                    (mon-buffer-sub-no-prop (car mhfa-sexp-bnds) (cdr mhfa-sexp-bnds)))))
          (setq mhfa-get-args (car (read-from-string (downcase mhfa-get-args))))
          (setq mhfa-get-args (delq 'fn mhfa-get-args)))
      (help-function-arglist w-func))))
;;
;;; :TEST-ME (mon-help-function-args 'mon-help-function-spit-doc)
;;; :TEST-ME (help-function-arglist 'mon-help-function-spit-doc)
;;; :TEST-ME (mon-help-function-args 'mon-help-function-spit-doc)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-30T17:22:54-04:00Z}#{09403} - by MON KEY>
(defcustom *mon-help-interactive-spec-alist* nil
  "A list of interactive spec arguments and values.\n
Key \(an intereractive spec letter\) maps to shortform a <SPEC-TYPE> tag which
is a string value delimited by `<' and `>'.\n
:CALLED-BY `mon-help-parse-interactive-spec'.\n
:NOTE Does not enumerate tags for the prefix chars `*`, `^`, and `@`.\n
:SEE :FILE src/callint.c\n
:SEE info node `(elisp)Interactive Call'\n
:SEE info node `(elisp)Using Interactive'\n
:SEE-ALSO `interactive-form', `prefix-numeric-value', `current-prefix-arg',
`called-interactively-p', `call-interactively', `noninteractive',
`mon-help-xref-symbol-value', `mon-help-insert-documentation',
`mon-help-function-spit-doc'.\n►►►"
  :type '(repeat symbol string)
  :group 'mon-doc-help-utils)
;;
(eval-and-compile
  (setq *mon-help-interactive-spec-alist*
        '( ;; Function name: symbol with a function definition.
          (a "<FUNCTION-NAME>")
          ;; Name of existing buffer.
          (b "<EXISTING-BUFFER-NAME>")
          ;; Name of buffer, possibly nonexistent.
          (B "<BUFFER-NAME-OR-NON-EXISTING>")
          ;; Character (no input method is used).
          (c "<CHARACTER-NO-INPUT-METHOD>")
          ;; Command name: symbol with interactive function definition.
          (C "<COMMAND-NAME>")
          ;; Value of point as number.  Does not do I/O.
          (d "<VALUE-POINT-AS-NUMBER-NO-I/O>")
          ;; Directory name.
          (D "<DIRECTORY-NAME>")
          ;; Parametrized event (i.e., one that's a list) that invoked this command.
          ;; If used more than once, the Nth `e' returns the Nth parameterized event.
          ;; This skips events that are integers or symbols.
          (e "<PARAMETRIZED-EVENT>")
          ;; Existing file name.
          (f "<EXISTING-FILE-NAME>")
          ;; Possibly nonexistent file name.
          (F "<FILE-NAME-OR-NON-EXISTING>")
          ;; Possibly nonexistent file name, default to directory alone.
          (G "<FILE-NAME-OR-NON-EXISTING-W/DIR-NAME>")
          ;; Ignore an argument -- Does not do I/O
          (i "<IGNORED-NOOP>") 
          ;; Key sequence -- downcase the last event if needed to get a definition.
          (k "<KEY-SEQUENCE-DOWNCASE-MAYBE>") 
          ;; Key sequence to be redefined -- do not downcase the last event.
          (K "<KEY-SEQUENCE-REDEFINE-NO-DOWNCASE>") 
          ;; Value of mark as number.  Does not do I/O.
          (m "<VALUE-MARK-AS-NUMBER>")
          ;; Any string.  Inherits the current input method.
          (M "<ANY-STRING-W/INPUT-METHOD>")
          ;; Number read using minibuffer.
          (n "<NUMBER<-MINIBUFFER>")
          ;; Numeric prefix arg, or if none, do like code `n'.
          (N "<NUMERIC-PREFIX-ARG>")
          ;; Prefix arg converted to number.  Does not do I/O.
          (p "<PREFIX-ARG->NUMBER>") 
          ;; Prefix arg in raw form.  Does not do I/O.
          (P "<PREFIX-ARG-RAW>")     
          ;; Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
          (r "<REGION>")             
          ;; Any string.  Does not inherit the current input method.
          (s "<ANY-STRING>")
          ;; Any symbol.
          (S "<ANY-SYMBOL>")         
          ;; Mouse up event discarded by a previous k or K argument.
          (U "<MOUSE-UP-EVENT>")     
          ;; Variable name: symbol that is user-variable-p.
          (v "<VARIABLE-NAME>")
          ;; Lisp expression read but not evaluated.
          (x "<READ-LISP-EXPRESSION-NO-EVALUATE>")
          ;; Lisp expression read and evaluated.
          (X "<READ-LISP-EXPRESSION-EVALUATE>")
          ;; Coding system.
          (z "<CODING-SYSTEM>")
          ;; Coding system, nil if no prefix arg.
          (Z "<CODING-SYSTEM-NIL-NO-PREFIX>")
          ;;
          ;; :NOTE It isn't clear how to incorporate the prefix chars `*`, `^`, and
          ;; `@` they are included here for completeness
          ;; 
          ;; When the string begins with `*', an error is signaled if
          ;; the buffer is read-only.
          ;; (* "<ERROR-ON-BUFFER-READ-ONLY>")
          ;;
          ;; If the string begins with `^' and `shift-select-mode' is non-nil, Emacs
          ;; first calls the function `handle-shift-selection'.
          ;; (^ "<HANDLE-SHIFT-SELECTION>")
          ;;
          ;; If the string begins with `@', Emacs searches the key sequence which
          ;; invoked the command for its first mouse click (or any other event
          ;; which specifies a window).
          ;; (@ "<KEY-OR-EVENT-W/WINDOW>")
          )
        ;;   "A list of interactive spec arguments and values.\n
        ;; Key \(an intereractive spec letter\) maps to shortform a <SPEC-TYPE> tag which
        ;; is a string value delimited by `<' and `>'.\n
        ;; :CALLED-BY `mon-help-parse-interactive-spec'.\n
        ;; :NOTE Does not enumerate tags for the prefix chars `*`, `^`, and `@`.\n
        ;; :SEE :FILE src/callint.c\n
        ;; :SEE info node `(elisp)Interactive Call'\n
        ;; :SEE info node `(elisp)Using Interactive'\n
        ;; :SEE-ALSO `interactive-form', `prefix-numeric-value', `current-prefix-arg',
        ;; `called-interactively-p', `call-interactively', `noninteractive',
        ;; `mon-help-xref-symbol-value', `mon-help-insert-documentation',
        ;; `mon-help-function-spit-doc'.\n►►►"
        )
  (custom-note-var-changed '*mon-help-interactive-spec-alist*))
;;
;;; :TEST-ME  *mon-help-interactive-spec-alist*
;;; :TEST-ME (assoc 'z *mon-help-interactive-spec-alist*)
;;
;;;(progn (makunbound '*mon-help-interactive-spec-alist*)
;;;       (unintern "*mon-help-interactive-spec-alist*" obarray) )


;;; ==============================
;;; *mon-help-interactive-spec-alist*
;;; (regexp-opt (mapcar 'cadr *mon-help-interactive-spec-alist*))

 
(eval-when-compile (require 'edmacro))
;;; ==============================
;;; :PREFIX "mhpis-"
;;; :CREATED <Timestamp: #{2009-09-07T20:04:57-04:00Z}#{09372} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-09-30T17:29:53-04:00Z}#{09403} - by MON KEY>
;;(eval-and-compile
(defun mon-help-parse-interactive-spec (fname)
  (let* ((mhpis-int-spec *mon-help-interactive-spec-alist*)
         (mhpis-int-t (interactive-form fname))
         (mhpis-int-has-spec
          (when mhpis-int-t
            (cond ((stringp (cadr mhpis-int-t))
                   (concat 
                    "<INTERACITVE-SPEC>\n;;; "
                    (mapconcat #'(lambda (mhpis-L-1)
                                   ;; :NOTE we can make `assoc-string' is
                                   ;; case-sensitive so we can extend the list
                                   ;; `*mon-help-interactive-spec-alist*' as needed.
                                   (concat (cadr (assoc-string (edmacro-subseq mhpis-L-1 0 1) mhpis-int-spec)) 
                                           " " (edmacro-subseq mhpis-L-1 1)))
                               (save-match-data (split-string (cadr mhpis-int-t) "\n"))
                               "\n;;; ")))
                  ;; :NOTE Leave the trailing line for `mon-insert-documentation'.
                  ((listp (cadr mhpis-int-t)) "<INTERACTIVE-SPEC-IS-LIST>")))))
    mhpis-int-has-spec))
;)
;;
;; Now put a doc-string on `mon-help-parse-interactive-spec'
;; using value & docstring of var `*mon-help-interactive-spec-alist*'.
(eval-when (compile load)
  (mon-help-put-var-doc-val->func
      '*mon-help-interactive-spec-alist*
      'mon-help-parse-interactive-spec
    ;; PRE-V-STR
    (concat "The arg FNAME names a function which has an interactive spec.\n\n"
            "Return spec of fname from value lookup in var `*mon-help-interactive-spec-alist*':\n\n")
    ;; CUT-V-STR
   ":CALLED-BY `mon-help-parse-interactive-spec'."
   ;; PST-V-STR
   "\n:EXAMPLE\n\n\(mon-help-parse-interactive-spec 'mon-insert-lisp-testme\)")
  )
;;
;;; :TEST-ME (describe-function 'mon-help-parse-interactive-spec)
;;; :TEST-ME (mon-help-parse-interactive-spec  'mon-insert-file-in-dirs)
;;; :TEST-ME (mon-help-parse-interactive-spec  'mon-help-mon-help)
;;; :TEST-ME (mon-help-parse-interactive-spec  'mon-insert-string-n-times)
;;; :TEST-ME (mon-help-parse-interactive-spec  'mon-insert-string-n-fancy-times)
;;; :TEST-ME (listp (cadr (interactive-form    'mon-insert-file-in-dirs)))
;;; :TEST-ME (listp (cadr (interactive-form    'mon-insert-string-n-times)))

 
;;; ==============================
;;; :PREFIX "mhid-"
;;; :CREATED <Timestamp: #{2009-08-20T21:30:15-04:00Z}#{09345} - by MON>
(defun mon-help-insert-documentation (&optional func-list var-list face-list alt-cookie)
  "Return documentation of symbols held by lists FUNC-LIST VAR-LIST FACE-LIST.\n
When non-nil ALT-COOKIE is a doc-cookie per `mon-help-function-spit-doc' spec.
Default is `*mon-doc-cookie*'.\n
:SEE-ALSO `mon-help-function-args', `mon-help-xref-symbol-value'
`apropos-library', `apropos-documentation-property',
`documentation-property'.\n►►►"
  (let ((mhid-fl func-list)
        (mhid-vl var-list)
        (mhid-fcl face-list)
        (mhid-alt-c (when alt-cookie (format ":alt-cookie %s" alt-cookie)))
        mhid-dlims
        mhid-dcstr)
    (setq mhid-dlims
          (mapcar #'(lambda (mhid-L-1)
                      (concat
                       "\n;;; ============================================================\n"
                       ";;; `%s' " mhid-L-1 "\n"
                       ";;; ============================================================\n"))
                  '("<FUNCTION>" "<VARIABLE>" "<FACE>")))
    (save-excursion
      (setq mhid-dcstr
            (with-temp-buffer
              (when mhid-fl
                (dolist (mhid-DL-0 mhid-fl)
                  (let ((mhid-f-dlim (format (car mhid-dlims) mhid-DL-0))
                        (mhid-int-specs (if (mon-help-parse-interactive-spec mhid-DL-0)
                                            (concat "\n;;; " (mon-help-parse-interactive-spec mhid-DL-0))
                                          ""))
                        (mhid-args-lst (if (mon-help-function-args mhid-DL-0)
                                           (concat ";;; <ARG-LIST>\n;;; " 
                                                   (format "%S" (mon-help-function-args mhid-DL-0)))
                                         ""))
                        (mhid-alt-ck (if mhid-alt-c
                                         (mon-help-function-spit-doc mhid-DL-0 mhid-alt-c)
                                       (mon-help-function-spit-doc mhid-DL-0))))
                    (princ
                     (concat mhid-f-dlim 
                             (format "%s%s\n%s\n%s" 
                                     mhid-args-lst mhid-int-specs (mon-comment-divider t) mhid-alt-ck)
                             "\n")
                     (current-buffer)))))
              (when mhid-vl
                (dolist (mhid-DL-1 mhid-vl)
                  (let ((mhid-v-dlim (format (cadr mhid-dlims) mhid-DL-1)))
                    (princ
                     (concat mhid-v-dlim
                             (replace-regexp-in-string 
                              (concat "^" (if mhid-alt-c 
                                              mhid-alt-c 
                                            *mon-doc-cookie*)) ""
                              (replace-regexp-in-string 
                               (concat (if mhid-alt-c 
                                           mhid-alt-c 
                                         *mon-doc-cookie*) "$") 
                               ""
                               ;; :WAS (get j 'variable-documentation)  "\n")            
                               (documentation-property mhid-DL-1 'variable-documentation) "\n")))
                     (current-buffer)))))
              (when mhid-fcl
                (dolist (mhid-DL-2 mhid-fcl)
                  (let ((mhid-fc-dlim (format (caddr mhid-dlims) mhid-DL-2)))
                    (princ
                     (concat mhid-fc-dlim  (get mhid-DL-2 'face-documentation) "\n")
                     (current-buffer)))))
              (buffer-string)))
      (insert mhid-dcstr))))
;;
;;; :TEST-ME
;; (setq mon-tmp-func-l (list 'mon-insert-string-n-times 'mon-help-mon-help
;;                           'mon-insert-file-in-dirs 'mon-help-insert-documentation
;;                           'mon-insert-string-n-times))
;;
;;; :TEST-ME (mon-help-insert-documentation mon-tmp-func-l)
;;
;;;(progn (makunbound 'mon-tmp-func-l) (unintern "mon-tmp-func-l" obarray) )

 
;;; ==============================
;; :TODO Finish!
;;; :CHANGESET 2067
;;; :CREATED <Timestamp: #{2010-08-16T17:26:40-04:00Z}#{10331} - by MON KEY>
;;;###autoload
(defun mon-help-help-functions (&optional insertp intrp)
  "List of functions and vars from the help packages.\n
;; :HELP-FUNCTIONS-MODE
`help-mode-setup'
`help-mode-finish'
`help-buffer'\n
;; :HELP-FUNCTIONS-HELP
`with-help-window'      ;<MACRO>\n
;; :HELP-FUNCTONS-FNS
`describe-function'
`describe-function-1'
`doc-file-to-man'
`doc-file-to-info'
`help-split-fundoc'
`help-add-fundoc-usage'
`help-function-arglist'
`help-make-usage'
`help-C-file-name'
`help-highlight-arg'
`help-do-arg-highlight'
`help-highlight-arguments'
`find-lisp-object-file-name'
`variable-at-point'
`describe-variable-custom-version-info'
`describe-variable'
`describe-syntax'
`describe-vector'
`internal-describe-syntax-value'
`help-describe-category-set'\n
;; :HELP-FUNCTIONS-KEYS
`where-is'
`where-is-internal'
`substitute-command-keys'
`describe-bindings'
`describe-bindings-internal'
`describe-buffer-bindings'
;; :HELP-FUNCTIONS
`documentation'
`documentation-property'
`describe-buffer-case-table'
;; :HELP-FUNCTIONS-APROPOS
`apropos-command'
`apropos-value'
`apropos-variable'
`apropos-library'
`apropos-documentation'
`apropos-documentation-check-doc-file'
`apropos-documentation-check-elc-file'
`apropos-safe-documentation'
`apropos-documentation-property'
`apropos-format-plist'
`apropos-symbol-button-display-help'
`apropos-internal'          ;; :NOTE This is `mapatoms' with a regexp test on symbol-name. 
                            ;; Its PREDICATE arg is similiar to the FUNCTION arg of mapatoms 
                            ;; However, it is hardwired to only grovel `obarray'.
                            ;; For example, following are the call sequences:
                            ;; `apropos-internal'
                            ;; map_obarray (Vobarray, apropos_accum, regexp);
                            ;; apropos_accum ---> call1 (apropos_predicate, symbol);
                            ;; `mapatoms' 
                            ;; map_obarray (obarray, mapatoms_1, function);
                            ;; mapatoms_1   ---> call1 (function, sym);
`apropos-symbols-internal'
`apropos-value-internal'
`apropos-documentation-internal'\n
;; :HELP-FACES
`help-argument-name'\n
;; :HELP-VARIABLES-FNS
`help-downcase-arguments'\n
;; :HELP-VARIABLES
`help-xref-stack'           ;:NOTE This and below also defined in :FILE faces.el
`help-xref-stack-item'       
`internal-doc-file-name'
`doc-directory'
`help-window'
`help-window-select'  ; [ never | other | always ]
`help-window-point-marker'\n
:FILE help.el help-mode.el help-fns.el help-macro.el 
:SEE-ALSO `mon-help-mon-help'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-help-functions :insertp t)
    (mon-help-message-intrp "mon-help-help-functions")))
;;
;;; :TEST-ME (mon-help-help-functions)
;;; :TEST-ME (mon-help-help-functions t)
;;; :TEST-ME (apply 'mon-help-help-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Friday July 03, 2009 @ 04:53.29 PM - by MON KEY>
;;;###autoload
(defun mon-help-mon-help (&optional insertp intrp)
  "Help `mon-help-*' to help you find help references. Why not! :).\n
;; :MON-DOC-FUNCTION-LISTS
`mon-help-bookmark-functions'
`mon-help-buffer-functions'
`mon-help-ebay-template-mode'
`mon-help-emacs-introspect'
`mon-help-file-dir-functions'
`mon-help-file-dir-functions-usage'
`mon-help-frame-functions'
`mon-help-help-functions'
`mon-help-inhibit-functions'
`mon-help-mon-functions'
`mon-help-mon-help'
`mon-help-hooks'
`mon-help-plist-functions'
`mon-help-plist-properties'
`mon-help-print-functions'
`mon-help-mail'
`mon-help-mode-functions'
`mon-help-make-network-process'
`mon-help-number-functions'
`mon-help-process-functions'
`mon-help-sequence-functions'
`mon-help-server-functions'
`mon-help-symbol-functions'
`mon-help-byte-code-vector-symbols'
`mon-help-type-predicates'
`mon-help-predicate-functions'
`mon-help-read-functions'
`mon-help-window-functions'
`mon-help-xml-functions'\n
;; :MON-DOC-SEARCHING
`mon-help-search-functions'
`mon-help-regexp-syntax'
`mon-help-syntax-class'
`mon-help-syntax-functions'\n
;; :MON-DOC-HELP
`mon-help-buffer-spc-*DOC*'
`mon-tags-apropos'
`mon-tags-naf-apropos'
`mon-help-package-keywords'\n
;; :MON-DOC-TIME
`mon-help-iso-8601'\n
`mon-help-CL-local-time'
`mon-help-CL-time'
`mon-help-time-functions'
`mon-help-mon-time-functions'\n
;; :MON-DOC-EIEIO
`mon-help-eieio-defclass'
`mon-help-eieio-functions'
`mon-help-eieio-methods'\n
;; :MON-DOC-CL                            ; :SEE :FILE mon-doc-help-CL.el
`mon-help-CL-arrays'
`mon-help-CL-bit-byte-bool-logic'
`mon-help-CL-chars'
`mon-help-CL-conses'
`mon-help-CL-control-flow'
`mon-help-CL-do'
`mon-help-CL-environment'
`mon-help-CL-error-condition-restart'
`mon-help-CL-eval-compile'
`mon-help-CL-file-dir-functions'
`mon-help-CL-hash-tables'
`mon-help-CL-intern-symbol'
`mon-help-CL-iteration'
`mon-help-CL-lispdoc'
`mon-help-CL-load-compile'
`mon-help-CL-local-time'
`mon-help-CL-loop'
`mon-help-CL-minion'
`mon-help-CL-numbers'
`mon-help-CL-object-CLOS'
`mon-help-CL-package-functions'
`mon-help-CL-pkgs'
`mon-help-CL-print'
`mon-help-CL-reader'
`mon-help-CL-sequence-predicates'
`mon-help-CL-sequences'
`mon-help-CL-sharpsign-syntax'
`mon-help-CL-slime-keys'
`mon-help-CL-stream-keywords'
`mon-help-CL-stream-keywords'
`mon-help-CL-streams'
`mon-help-CL-strings'
`mon-help-CL-structures'
`mon-help-CL-swank-functions'
`mon-help-CL-symbols'
`mon-help-CL-time'\n
;; :MON-DOC-CSS-HTML
`mon-help-css-color'
`mon-help-css-mode'
`mon-help-css-complete'
`mon-help-css-check'
`mon-help-css-properties'
`mon-help-tidy'\n
;; :MON-DOC-ASCII-ART
`mon-help-color-chart'
`mon-help-easy-menu'
`mon-help-font-lock'
`mon-help-ipv4-header'
`mon-help-widgets'\n
;; :MON-DOC-PRESENTATION
`mon-help-custom-keywords'
`mon-help-color-functions'
`mon-help-faces'
`mon-help-faces-basic'
`mon-help-faces-themes'
`mon-help-faces-font-lock'
`mon-help-font-lock'
`mon-help-font-lock-functions'
`mon-help-overlay-functions'
`mon-help-text-property-functions'
`mon-help-text-property-properties'
`mon-help-text-property-functions-ext'
`mon-help-text-property-stickyness'\n
;; :MON-DOC-KEYS
`mon-help-key-functions'
`mon-help-keys'
`mon-help-CL-slime-keys'\n
;; :MON-DOC-CHAR-TABLES
`mon-help-binary-representation'
`mon-help-char-functions'
`mon-help-char-representation'
`mon-help-char-composition',
`mon-help-char-raw-bytes'
`mon-help-char-unidata-table'
`mon-help-diacritics'
`mon-help-char-ascii'
`mon-help-char-iso-8859-1'
`mon-help-char-ecma-35'
`mon-help-char-ecma-48'\n
;; :MON-DOC-EXTERNAL
`mon-help-crontab'
`mon-help-du-incantation'
`mon-help-format-width'
`mon-help-hg-archive'
`mon-help-info-incantation'
`mon-help-install-info-incantation'
`mon-help-pacman-Q'
`mon-help-permissions'
`mon-help-rename-incantation'
`mon-help-tar-incantation'
`mon-help-unix-commands'\n
;; :MON-DOC-INTROSPECTION-AND-UTILITY
`mon-help-w32-env'
`mon-index-elisp-symbol'
`mon-help-function-args'
`mon-help-parse-interactive-spec'
`mon-help-xref-symbol-value'
`mon-help-swap-var-doc-const-val'
`mon-help-function-spit-doc'
`mon-help-insert-documentation'
`mon-help-permanent-locals-find'
`mon-insert-doc-help-tail'
`mon-insert-doc-help-cookie'
`mon-help-utils-loadtime'
`mon-help-escape-for-ewiki'
`mon-help-unescape-for-ewiki'\n
;; :MON-DOC-FACES
`mon-help-PNTR-tag'
`mon-help-DYNATAB-tag'
`mon-help-DYNATAB-tag'
`mon-help-KEY-tag'
`mon-help-COMMENT-TAG'
`mon-help-META-tag'
`mon-help-URL-wrap-tag'
`mon-help-BUILTIN-tag'
`mon-help-BUILTIN-tag'
`mon-help-INNER-KEY-tag'
`mon-help-OLAY-RESULT'
`mon-help-OLAY-RESULT-string-show'
`mon-help-OLAY-RESULT-match-show'\n
;; :MON-DOC-GROUPS
\n
;; :MON-DOC-VARIABLES
`*mon-doc-cookie*'
`*mon-help-interactive-spec-alist*'
`*mon-iptables-alst*'
`*mon-help-reference-keys*'
`*mon-help-side-effect-free*'
`*mon-help-side-effect-and-error-free*'
`*mon-help-pure-functions*'
`*mon-help-permanent-locals*'
`*mon-help-subrs*'
`*mon-help-byte-optimizer-vals*'
`*regexp-mon-doc-help-docstring-tags-DYNAMIC*'
`*regexp-mon-doc-help-docstring-tags-TABLES*'
`*regexp-mon-doc-help-docstring-tags*'
`*regexp-mon-doc-help-comment-tags*'
`*regexp-mon-doc-help-pointer-tags*'
`*regexp-mon-doc-help-meta-tags*'
`*regexp-clean-du-flags*'
`*regexp-symbol-defs*'
`*regexp-clean-pacman-Q*'
`*w32-env-variables-alist*'\n
:ALIASED-BY `mon-help-reference-sheet'\n
:SEE-ALSO `mon-help-mon-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-mon-help :insertp t)
        (mon-help-message-intrp "mon-help-mon-help")))
;;
;;; :TEST-ME (mon-help-mon-help)
;;; :TEST-ME (mon-help-mon-help t)
;;; :TEST-ME (describe-function 'mon-help-mon-help)
;;; :TEST-ME (apply 'mon-help-mon-help '(t))

 
;;; ==============================
;;; :TODO This should be build dynamically from the values of symbols in `*mon-xrefs-xrefs*'.
;;; :CREATED <Timestamp: #{2010-01-22T16:11:37-05:00Z}#{10035} - by MON KEY>
;;;###autoload
(defun mon-help-mon-functions (&optional insertp intrp)
  "Incomplete list of MON functions and their related variables.\n
;; :MON-FUNCTIONS-BUFFER
`mon-g2be'
`mon-get-buffer-hidden'
`mon-buffer-exists-so-kill'
`mon-print-in-buffer-if-p'
`mon-buffer-written-p'
`mon-buffer-exists-p'
`mon-buffer-name->kill-ring'
`mon-with-file-buffer'
`mon-get-proc-buffers-directories'
`mon-get-buffers-directories'
`mon-get-buffer-parent-dir'
`mon-get-buffer-w-mode'
`mon-get-new-buffer-w-stamp'
`mon-string-split-buffer-name'
`mon-string-split-buffer-parent-dir'
`mon-buffer-get-shell'
`mon-buffer-make-shell'
`mon-buffer-get-scratch'
`mon-print-buffer-object-readably'
`mon-with-inhibit-buffer-read-only'
`mon-with-buffer-undo-disabled'
`mon-buffer-get-mesages'
`mon-kill-completions'
`mon-buffer-empty-p'
`mon-buffer-narrowed-p'
`mon-buffer-sub-no-prop'
`mon-buffer-sub-no-prop-check'
`mon-get-buffer-window-if'

;; :MON-FUNCTIONS-CHARS
`mon-decode-coding-region-utf-8-unix'
`mon-word-count-chars-region'
`mon-char-code'

;; :MON-FUNCTIONS-COLUMN
`mon-indent-lines-from-to-col'
`mon-line-strings-pipe-to-col'
`mon-string-fill-to-col'
`mon-comment-lisp-to-col'
`mon-mysql-cln-pipes-map-col-field'
`mon-mysql-csv-map-col-field'
`mon-mysql-get-field-col'
`mon-rectangle-columns'
`mon-rectangle-sum-column'
`mon-show-columns'

;; :MON-FUNCTIONS-RECTANGLE
`mon-rectangle-apply-on-region-points'
`mon-rectangle-capitalize'
`mon-rectangle-columns'
`mon-rectangle-downcase'
`mon-rectangle-operate-on'
`mon-rectangle-sum-column'
`mon-rectangle-upcase'

;; :MON-FUNCTIONS-LINE
`mon-line-bol-is-eol'
`mon-line-count-matchp'
`mon-line-count-region'
`mon-line-count-buffer'
`mon-line-dolines'
`mon-line-move-n'
`mon-line-drop-in-words'
`mon-line-end-or-code-end'
`mon-line-eol-is-eob'
`mon-line-find-duplicates'
`mon-line-get-next'
`mon-line-indent-from-to-col'
`mon-line-length-max'
`mon-line-next-bol-is-eol'
`mon-line-number-region-incr'
`mon-line-previous-bol-is-eol'
`mon-line-string-insert-chars-under'
`mon-line-string-rotate-name'
`mon-line-string-rotate-namestrings'
`mon-line-string-rotate-namestrings-combine'
`mon-line-string-unrotate-namestrings'
`mon-line-strings'
`mon-line-strings-bq-qt-sym-bol'
`mon-line-strings-indent-to-col'
`mon-line-strings-one-list'
`mon-line-strings-pipe-bol'
`mon-line-strings-qt-region'
`mon-line-strings-region'
`mon-line-strings-to-list'
`mon-line-test-content'
`mon-toggle-truncate-line'

;; :MON-FUNCTIONS-STRING
`mon-string-spread'
`mon-region-split-commas'
`mon-string->strings-splice-sep'
`mon-string->symbol'
`mon-string-after-index'
`mon-string-alpha-list'
`mon-string-chop-spaces'
`mon-string-combine-and-quote'
`mon-string-csv-regexp'
`mon-string-csv-rotate'
`mon-string-fill-to-col'
`mon-string-from-hex-list'
`mon-string-from-sequence'
`mon-string-from-symbol'
`mon-string-has-suffix'
`mon-string-ify-current-line'
`mon-string-ify-list'
`mon-string-incr'
`mon-line-string-incr-padded'
`mon-string-index'
`mon-string-infix'
`mon-string-insert-string-at-idx'
`mon-string-justify-left'
`mon-string-or-null-and-zerop'
`mon-string-permute'
`mon-string-permute-line'
`mon-string-position'
`mon-string-read-match-string'
`mon-string-repeat'
`mon-string-replace-char'
`mon-string-rotate-name'
`mon-string-rotate-to-regexp'
`mon-string-set-char-at-idx'
`mon-string-sort-descending'
`mon-string-split-dir-recurse'
`mon-string-splice-sep'
`mon-string-split-and-unquote'
`mon-line-string-get'
`mon-string-split-on-regexp'
`mon-string-sub-old->new'
`mon-string-to-hex-list'
`mon-string-to-hex-string'
`mon-string-to-regexp'
`mon-string-to-sequence'
`mon-string-to-symbol'
`mon-string-upto-index'
`mon-string-wonkify'
`mon-string-canonical'

;; :MON-FUNCTIONS-LIST
`mon-assoc-replace'
`mon-combine'
`mon-deleql-dups'
`mon-delete-if'
`mon-delete-first'
`mon-delq-cons'
`mon-delq-dups'
`mon-elt-<'
`mon-elt-<elt'
`mon-elt->'
`mon-elt->elt'
`mon-every'
`mon-flatten'
`mon-intersection'
`mon-list-last'
`mon-list-make-unique'
`mon-list-match-tails'
`mon-list-match-tails'
`mon-list-proper-p'
`mon-list-reorder'
`mon-list-nshuffle'
`mon-list-shuffle-safe'
`mon-maybe-cons'
`mon-mismatch'
`mon-moveq'
`mon-list-merge'
`mon-list-add-non-nil'
`mon-pairlis'
`mon-recursive-apply'
`mon-remove-dups'
`mon-remove-if'
`mon-set-difference'
`mon-sequence-mappable-p'
`mon-sequence-reorder'
`mon-sublist'
`mon-subseq'
`mon-sublist-gutted'
`mon-transpose'
`mon-list-proper-p'
`mon-list-filter'

;; :MON-FUNCTIONS-LIST-MAP
`mon-map'
`mon-map1'
`mon-mapl'
`mon-maplist'
`mon-mapcar'
`mon-mapcan',
`mon-mapcon'
`mon-map-append'
`mon-maptree'
`mon-mapcar-mac'

;; MON-FUNCTION-ITERATORS
`mon-foreach'
`mon-for'
`mon-for-step'
`mon-loop'

;; :MON-FUNCTIONS-PLIST
`mon-alphabet-as-type'
`mon-help-permanent-locals-find'
`mon-plist-remove!'
`mon-plist-keys'
`mon-plist-remove-if'
`mon-map-obarray-symbol-plist-props'
`mon-plist-remove-consing'

:MON-FUNCTIONS-BIT-BYTE-BOOL
`mon-get-bit-table'
`mon-booleanp'
`mon-booleanp-to-binary'
`mon-one-or-zerop'
`mon-bool-vector-pp'

;; :MON-FUNCTIONS-VECTOR
`mon-nshuffle-vector'

;; :MON-FUNCTIONS-HASHTABLE
`mon-insert-naf-mode-faces-as-displayed'
`mon-hash-add-uniquify'
`mon-hash-all-keys'
`mon-hash-all-values'
`mon-hash-describe'
`mon-hash-describe-descend'
`mon-hash-get-items'
`mon-hash-get-keys'
`mon-hash-get-string-keys'
`mon-hash-get-symbol-keys'
`mon-hash-get-values'
`mon-hash-has-key'
`mon-dir-hash-images'
`mon-hash-make-size'
`mon-hash-put-CL'
`mon-hash-readlines-buffer'
`mon-hash-readlines-file'
`mon-hash-table-complete'
`mon-hash-to-list'
`mon-hash<-vector'

;; :MON-FUNCTIONS-RANDOMIZE
`mon-generate-prand-id'
`mon-generate-prand-seed'
`mon-string-wonkify'
`mon-make-random-state'
`mon-next-almost-prime'
`mon-gensym-counter-randomizer'

;; :MON-FUNCTIONS-SYMBOL-CREATE
`mon-quote-sexp'
`mon-with-gensyms'
`mon-gensym'
`defparameter'
`defconstant'
`mon-string-from-symbol'
`mon-symbol-to-string'

;; :MON-FUNCTIONS-SYMBOL-INSPECT
`mon-booleanp'
`mon-recover-nil-t-default-plist'
`mon-function-object-p'
`mon-macrop'

;; :MON-FUNCTIONS-ENVIRONMENT
`mon-get-env-vars-strings'
`mon-get-env-vars-symbols'
`mon-get-env-vars-emacs'
`mon-get-system-specs'
`mon-insert-system-type-cond'
`mon-help-emacs-introspect'

;; :MON-FUNCTIONS-FRAME
`mon-frame-live-visible-graphic-p'
`mon-dired-find-file-other-frame'

;; :MON-FUNCTIONS-WINDOW
`mon-map-windows->plist'

;; :MON-FUNCTIONS-FACE
`mon-face-bold->normal'
`mon-hexcolor-add-to-font-lock'
`mon-help-naf-mode-faces'

;; :MON-FUNCTIONS-OVERLAY
`mon-help-find-result-for-overlay'
`mon-help-overlay-for-example'
`mon-help-overlay-on-region'
`mon-help-overlay-result'
`mon-nuke-overlay-buffer'

;; :MON-FUNCTIONS-SYNTAX
`mon-get-syntax-class-at'

;; :MON-FUNCTIONS-TEXT-PROPERTY
`mon-get-all-face-property-change'
`mon-get-next-face-property-change'
`mon-get-next-face-property-change-if'
`mon-get-text-properties-region'
`mon-get-text-properties-region-to-kill-ring'
`mon-get-text-properties-print'
`mon-get-text-properties-read-temp'
`mon-get-text-properties-parse-buffer'
`mon-get-text-properties-parse-sym'
`mon-get-text-properties-parse-buffer-or-sym'
`mon-get-text-properties-elisp-string'
`mon-get-text-properties-elisp-string-pp'
`mon-list-all-properties-in-buffer'
`mon-nuke-text-properties-buffer'
`mon-nuke-text-properties-region'
`mon-remove-single-text-property'
`mon-remove-text-property'
`mon-get-text-properties-category'

;; :MON-FUNCTIONS-DIRED
`mon-abort-autosave-when-fucked'
`mon-dired-copy-files-to-list'
`mon-dired-copy-files-to-strings'
`mon-dired-find-file-other-frame'
`mon-dired-insert-dirs-recursive'
`mon-dired-kill-files-to-list'
`mon-dired-kill-files-to-strings'
`mon-dired-naf-artist-letter'
`mon-dired-naf-brand-letter'
`mon-dired-nef-dir'
`mon-dired-other-window'
`mon-dired-srt-alph'
`mon-dired-srt-chrn'
`mon-dired-srt-type'
`mon-dired-srt-type-alph'
`mon-dired-srt-type-chrn'
`mon-dired-uninsert-subdir'
`mon-dired-uninsert-subdir-all'
`mon-dired-unmark-elc'
`mon-dired-up-directory-this-buffer'
`mon-toggle-dired-dwim-target'

;; :MON-FUNCTIONS-FILE-DIRECTORY
`mon-add-subdirs-to-list'
`mon-async-du-dir'
`mon-build-path-for-load-path'
`mon-check-image-type'
`mon-cln-file-name-string'
`mon-copy-file-path'
`mon-dir-common-paths'
`mon-dir-save-current'
`mon-dir-save-current-to-file'
`mon-file-dir-attributes->plist'
`mon-file-ensure-extension-is-el'
`mon-file-map-elisp-fileset'
`mon-file-stamp'
`mon-file-stamp-buffer-filename'
`mon-file-stamp-minibuffer'
`mon-format-jg-file-for-write'
`mon-get-dir-name-absolute'
`mon-get-dir-size'
`mon-get-dir-subdir-default'
`mon-get-file-mod-times'
`mon-get-relative-w-absolute'
`mon-image-verify-type'
`mon-insert-dirs-in-path'
`mon-insert-file-in-dirs'
`mon-insert-naf-file-in-dirs'
`mon-insert-path'
`mon-insert-subdirs-in-buffer'
`mon-make-jg-dir-in-path'
`mon-rename-file-serial'
`mon-string-split-dir-recurse'
`mon-toggle-dired-dwim-target'
`mon-truncate-path-for-prompt'
`mon-write-jg-file-in-path'

;; :MON-FUNCTIONS-PROCESS
`mon-make-shell-buffer'
`mon-shell'
`mon-terminal'
`mon-cmd'
`mon-insert-sys-proc-list'
`mon-get-sys-proc-list'
`mon-get-proc-w-name'
`mon-get-process'

;; :MON-FUNCTIONS-INHIBIT
`mon-with-inhibit-buffer-read-only'
`mon-inhibit-read-only'
`mon-inhibit-point-motion-hooks'
`mon-inhibit-modification-hooks'

;; :MON-FUNCTIONS-TOGGLE
`mon-toggle-eval-length'
`mon-toggle-dired-dwim-target'
`mon-toggle-truncate-line'
`mon-toggle-read-only-point-motion'

;; :MON-FUNCTIONS-KEY-EVENT
`mon-read-keys-as-string'
`mon-test-keypresses'
`mon-abort-recursive-edit'
`mon-abort-autosave-when-fucked'

;; :MON-FUNCTIONS-INTERWEBS
`mon-firefox'
`mon-conkeror'
`mon-w3m-dired-file'
`mon-w3m-get-url-at-point'
`mon-w3m-get-url-at-point-maybe'
`mon-w3m-goto-url-at-point'
`mon-w3m-kill-url-at-point'
`mon-w3m-read-gnu-lists-nxt-prv'

;; :MON-FUNCTIONS-TEMPLATE
`mon-insert-drive-transfer-template'
`mon-insert-file-template'
`mon-insert-texi-template'
`mon-insert-lisp-CL-package-template'
`mon-insert-lisp-CL-mode-line-template'
`mon-insert-lisp-CL-file-template'
`mon-insert-defclass-template'
`mon-insert-ebay-template'
`mon-insert-hgignore-template'
`mon-insert-naf-mode-class-template'
`mon-insert-naf-mode-constant-template'
`mon-insert-naf-mode-face-template'
`mon-insert-naf-mode-file-template'
`mon-insert-naf-mode-var-const-templt'
`mon-insert-naf-mode-xref-template'
`mon-insert-smith-poster-template'\n

;; :MON-FUNCTIONS-REGION
`mon-decode-coding-region-utf-8-unix'
`mon-line-count-region'
`mon-line-number-region-incr'
`mon-line-strings-qt-region'
`mon-line-strings-region'
`mon-help-overlay-on-region'
`mon-get-text-properties-region'
`mon-get-text-properties-region-to-kill-ring'
`mon-nuke-text-properties-region'
`mon-word-reverse-region'
`mon-word-count-chars-region'
`mon-word-count-region'
`mon-region-position'
`mon-region-length'
`mon-region-unfill'
`mon-region-reverse'

;; :MON-FUNCTIONS-REGION-WRAP
`mon-wrap-selection'
`mon-wrap-with'
`mon-wrap-text'
`mon-wrap-url'
`mon-wrap-span'

;; :MON-FUNCTIONS-WORD
`mon-word-get-next'
`mon-word-get-list-in-buffer'
`mon-word-reverse-region'
`mon-word-iterate-over'
`mon-word-count-analysis'
`mon-word-count-chars-region'
`mon-word-count-region'
`mon-word-count-occurrences'

;; :MON-FUNCTIONS-WHITESPACE
`mon-spacep'
`mon-spacep-not-bol'
`mon-spacep-is-bol'
`mon-spacep-is-after-eol'
`mon-spacep-is-after-eol-then-graphic'
`mon-spacep-at-eol'
`mon-spacep-first'
`mon-line-bol-is-eol'
`mon-line-previous-bol-is-eol'
`mon-insert-whitespace'
`mon-cln-whitespace'
`mon-num-to-month-whitespace'
`mon-cln-tgm-xml-LF'
`mon-cln-spc-tab-eol'
`mon-cln-spc-tab-at-eol-in-region'
`mon-cln-blank-lines'
`mon-cln-BIG-whitespace'
`mon-cln-whitespace'
`mon-cln-trail-whitespace'
`mon-kill-whitespace'
`mon-num-to-month-whitespace'

;; :MON-FUNCTIONS-LINE-PIPED
`mon-cln-piped-list'
`mon-line-strings-pipe-to-col'
`mon-line-strings-pipe-bol'
`naf-backup-the-list'
`mon-delete-back-up-list'
`mon-line-pipe-lines'

;; :MON-FUNCTIONS-REPLACE
`mon-regexp-map-match'
`mon-regexp-map-match-in-region'
`mon-replace-char-in-string'
`mon-string-split-on-regexp'
`mon-string-csv-regexp'
`mon-string-rotate-to-regexp'
`mon-string-sub-old->new'
`mon-regexp-map-match'
`mon-replace-strings'
`mon-replace-regexp-while'
`mon-replace-string-while'
`mon-regexp-filter'
`mon-replace-region-regexp-lists'
`mon-replace-region-regexp-lists-nonint'
`mon-walk-regexps-in-file'
`mon-replace-regexps-in-file-list'
`replace-string-pairs-region-no-props'
`replace-string-pairs-region3'
`mon-replace-string-pairs-region-no-insert'
`mon-exchange-slash-and-backslash'
`mon-cln-control-M'
`mon-cln-csv-fields'
`mon-cln-file-name-string'
`mon-cln-html-chars'
`mon-cln-html-tags'
`mon-cln-iso-latin-1'
`mon-cln-mail-headers'
`mon-cln-up-colon'
`mon-cln-uniq-lines'
`mon-cln-whitespace'
`mon-cln-xml<-parsed'
`mon-cln-xml<-parsed-strip-nil'

;; MON-FUNCTIONS-REPLACE-NAF-MODE
`mon-cln-tgm-xml-LF'
`mon-cln-piped-list'
`mon-cln-philsp'
`mon-cln-ulan'
`mon-cln-imdb'
`mon-cln-loc'
`mon-cln-wiki'
`mon-cln-bib'
`mon-delete-back-up-list'
`mon-replace-common-abbrevs'

;; :MON-FUNCTIONS-REPLACE-DATES
`mon-ital-date-to-eng'
`mon-num-to-month'
`mon-num-to-month-whitespace'
`mon-month-to-num'
`mon-abr-to-month'
`mon-defranc-dates'

;; :MON-FUNCTIONS-REPLACE-TRANSLATE
`mon-defranc-places'
`mon-cln-benezit'
`mon-cln-benezit-fields'

;; :MON-FUNCTIONS-REPLACE-CHAR-ENCODING
`mon-decode-coding-region-utf-8-unix'
`mon-make-iso-latin-1-approximation'
`mon-trans-cp1252-to-latin1'
`mon-cln-iso-latin-1'
`deftransmogrify'
`mon-transmogrify'

;; :MON-FUNCTIONS-REPLACE-WHITESPACE
`mon-cln-tgm-xml-LF'
`mon-cln-spc-tab-eol'
`mon-cln-spc-tab-at-eol-in-region'
`mon-cln-blank-lines'
`mon-cln-BIG-whitespace'
`mon-cln-whitespace'
`mon-cln-trail-whitespace'
`mon-kill-whitespace'
`mon-num-to-month-whitespace'

;; :MON-FUNCTIONS-REPLACE-CASE
`mon-downcase-hex-values'
`mon-upcase-commented-lines'
`mon-toggle-case-regexp-region'
`mon-toggle-case-regexp'
`mon-downcase-regexp-region'
`mon-upcase-regexp-region'
`mon-downcase-regexp'
`mon-upcase-regexp'

;; :MON-FUNCTIONS-HEX
`mon-cln-img-magic-hex'
`mon-downcase-hex-values'
`mon-string-from-hex-list'
`mon-string-to-hex-list'
`mon-string-to-hex-string'
`mon-string-to-hex-list-cln-chars'
`mon-hexcolor-add-to-font-lock'

;; :MON-VARIABLES-HEX
`*css-color:hex-chars*'
`*regexp-rgb-hex*'
`*regexp-css-color-hex*'
`*regexp-hexcolor-keywords*'
`*regexp-hexcolor-keywords*'

;; :MON-FUNCTIONS-LOADTIME
`mon-utils-require-features-at-loadtime'
`mon-after-mon-utils-loadtime'
`mon-check-feature-for-loadtime'
`mon-help-utils-loadtime'
`mon-bind-nefs-photos-at-loadtime'
`mon-bind-cifs-vars-at-loadtime'
`mon-bind-doc-help-proprietery-vars-at-loadtime'
`mon-bind-iptables-vars-at-loadtime'
`mon-set-register-tags-loadtime'
`mon-CL-cln-colon-swap'

;; :MON-FUNCTIONS-TEST
`google-define-get-command-TEST'
`mon-build-copyright-string-TEST'
`mon-build-user-name-example-TEST'
`mon-drive-transfer-template-TEST'
`mon-file-stamp-buffer-filename-TEST'
`mon-gensym-counter-randomizer-TEST'
`mon-help-keys-wikify-TEST'
`mon-help-propertize-regexp-symbol-defs-TEST'
`mon-help-propertize-tags-TEST'
`mon-help-regexp-symbol-defs-TEST'
`mon-help-CL-wget-pkgs-TEST'
`mon-line-strings-bq-qt-sym-bol-TEST'
`mon-insert-lisp-testme'
`mon-insert-lisp-testme-fancy'
`mon-insert-test-cases'
`mon-line-dolines-setup-TEST'
`mon-line-dolines-TEST'
`mon-line-strings-to-list-TEST'
`mon-regexp-clean-ulan-dispatch-chars-TEST'
`mon-permute-combine-functions-TEST'
`mon-user-system-conditionals-TEST'
`mon-up/down-case-regexp-TEST'
`mon-wget-list-to-script-TEST'
`mon-with-inhibit-buffer-read-only-PP-TEST'
`mon-with-inhibit-buffer-read-only-TEST'
`mon-with-buffer-undo-disabled-TEST'

;; :MON-COMMNET\n
;; :MON-INSERT\n

:SEE-ALSO `mon-help-mon-help', `mon-help-emacs-introspect'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-mon-functions :insertp t)
    (mon-help-message-intrp "mon-help-mon-functions")
    ))
;;
;;; :TEST-ME (mon-help-mon-functions )
;;; :TEST-ME (mon-help-mon-functions t)
;;; :TEST-ME (describe-function 'mon-help-mon-functions)
;;; :TEST-ME (apply 'mon-help-mon-functions nil '(t))

 
;;; ==============================
;;; :CHANGESET 1821
;;; :CREATED <Timestamp: #{2010-06-01T19:06:13-04:00Z}#{10222} - by MON KEY>
;;;###autoload
(defun mon-help-ebay-template-mode (&optional insertp intrp)
  "Functions and variables for use with `ebay-template-mode'.\n
;; :FILE ebay-template-tools.el
`mon-insert-ebay-template'
`mon-choose-ebay-delims'
`mon-choose-ebay-account'
`mon-make-html-tree'
`mon-insert-ebay-html-tree'
`mon-make-ebay-dir-list',
`mon-make-ebay-dir-list-2'
`mon-insert-ebay-dirs'
`mon-insert-ebay-dbc-file',
`mon-check-ebay-template-path'
`mon-insert-ebay-dbc-template'
`mon-ebay-field-trigger'
`mon-insert-ebay-field-trigger-l'
`mon-insert-ebay-field-trigger-r'
`mon-insert-ebay-field-trigger-l-and-r'
`mon-insert-ebay-photo-per-scan-descr'
`mon-ebay-image-linkify',
`mon-ebay-image-linkify-lite'\n
;; :FILE ebay-template-mode.el
`*mon-ebay-template-mode-version*' ;<CONSTANT>
`*mon-ebay-template-font-lock-keywords*'
`*mon-ebay-field-entry*',
`*mon-ebay-field-delims*'
`*mon-ebay-line-delims*',
`*mon-ebay-template-mode-map*'
`*mon-ebay-template-mode-hook*',
`*mon-ebay-template-mode-syntax-table*',
`ebay-template-mode-menu'
`ebay-template-mode'\n
;; :FILE mon-dir-locals-alist.el
`*mon-ebay-images-bmp-path*'
`*mon-ebay-images-jpg-path*'
`*mon-ebay-images-lookup-path*'
`*mon-ebay-images-temp-path*'\n
;; :FILE mon-make-html-tree.el
`xmlgen'                    ; :NOTE These are from xmlgen.el
`xmlgen-escapees'
`xmlgen-attr-to-string'
`xmlgen-extract-plist'
`xmlgen-string-escape'
`html-lite-doctype'          ; :NOTE These are from html-lite.el
`html-lite-doctype-alist'
`html-lite-define-elements'
`html-lite-make-element'
`with-html-lite-header'
`with-html-lite-header'
`html-doctype'
`html-lite-make-name'
`html-lite-write-tree'
`html-lite-browse-tree'\n
:SEE (URL `http://www.shellarchive.co.uk/content/emacs.html')
:SEE (URL `http://repo.or.cz/w/ShellArchive.git/blob_plain/master:/xmlgen.el')
:SEE (URL `http://www.emacswiki.org/cgi-bin/wiki/download/html-lite.el')\n
:SEE-ALSO `mon-help-xml-functions', `mon-help-mon-functions', `mon-help-mon-help'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-ebay-template-mode :insertp t)
    (mon-help-message-intrp "mon-help-ebay-template-mode")))
;;
;;; :TEST-ME (mon-help-ebay-template-mode)
;;; :TEST-ME (mon-help-ebay-template-mode t)
;;; :TEST-ME (describe-function 'mon-help-ebay-template-mode)
;;; :TEST-ME (apply 'mon-help-ebay-template-mode '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Thursday July 02, 2009 @ 02:34.14 PM - by MON KEY>
;;;###autoload
(defun mon-help-emacs-introspect (&optional insertp intrp)
  "Variables and functions related to what this Emacs knows about this Emacs.\n
Unless indicated as a '<FUNCTION>' items listed are '<VARIABLE>'.\n
;; :EMACS-BUILD
`preloaded-file-list'
`emacs-build-system'
`emacs-build-time'
`emacs-major-version'
`emacs-minor-version'
`emacs-version'                        ;<FUNCTION>
`system-configuration'
`pure-space-overflow'\n
;; :EMACS-BUILD-PATH-DIR-FILE
`build-files'
`exec-directory'
`source-directory'
`installation-directory'\n
;; :EMACS-ENVIRONMENT
`initial-environment'
`getenv'
`setenv'
`system-shell'
`system-type'
`system-name'\n
;; :EMACS-ENVIRONMENT-DISPLAY
`x-get-resource'                       ;<FUNCTION>
`frame-initialize'                     ;<FUNCTION>
`frame-notice-user-settings'           ;<FUNCTION>
`emacs-basic-display'
`frame-initial-frame'
`handle-args-function-alist'
`initial-frame-alist'
`initial-window-system'
`tool-bar-originally-present'
`window-system-initialization-alist'\n
;; :EMACS-ENVIRONMENT-DOC
`Info-default-directory-list'
`configure-info-directory'
`doc-directory'
`find-function-C-source'               ;<FUNCTION>
`find-function-C-source-directory'
`internal-doc-file-name'
`load-force-doc-strings'
`tutorial-directory'\n
;; :EMACS-ENVIRONMENT-FEATURES
`advertised-signature-table'
`current-load-list'
`features'
`symbol-file-load-history-loaded'
`load-history'  ;; :NOTE Elements of return value have the format:
                ( <FILENAME>                             ;(and <ATOM> <STRING>)
                  { <VARIABLE>                           ;<ATOM>
                    (defface  . <FACE>)                  ;<CONS>
                    (defun    . <FUNCTION>)              ;<CONS>
                    (t        . <AUTOLOADED-FUNCTION>)   ;<CONS>
                    (autoload . <AUTOLOAD-FUNCTION>)     ;<CONS>
                    (require  . <FEATURE>)               ;<CONS>
                    (provide  . <FEATURE>)               ;<CONS>
                  }* )\n
;; :EMACS-ENVIRONMENT-FEATURES-CUSTOM
`custom-reevaluate-setting'            ;<FUNCTION>
`custom-delayed-init-variables'\n
;; :EMACS-ENVIRONMENT-CODING-SYSTEM
`coding-system-for-read'
`coding-system-for-write'
`file-coding-system-alist'
`process-coding-system-alist'
`network-coding-system-alist'\n
;; :EMACS-ENVIRONMENT-IN-OUT
`cannot-suspend'
`charset-list'
`global-map'
`glyph-table'
`initial-window-system'
`interprogram-cut-function'
`interprogram-paste-function'
`keyboard-type'
`null-device'\n
;; :EMACS-ENVIRONMENT-INIT-HOOKS
`after-init-hook'
`before-init-hook'
`emacs-startup-hook'
`term-setup-hook'
`window-setup-hook'\n
;; :EMACS-ENVIRONMENT-INIT-INHIBIT
`inhibit-x-resources'
`inhibit-startup-screen'
`inhibit-default-init'
`inhibit-startup-echo-area-message'
`inhibit-startup-hooks'
`inhibit-startup-buffer-menu'
;; :EMACS-ENVIRONMENT-INIT
`noninteractive'
`before-init-time'
`after-init-time'
`invocation-directory'
`invocation-name'
`emacs-init-time'
`init-file-debug'
`init-file-had-error'
`init-file-user'
`initial-scratch-message'
`initial-buffer-choice'\n
;; :EMACS-ENVIRONMENT-INIT-TOP-LEVEL
`top-level'
`normal-top-level'           
`normal-top-level-add-to-load-path'
`normal-top-level-add-subdirs-inode-list'\n
;; :EMACS-ENVIRONMENT-PATH-DIR-FILE
`term-file-prefix'
`site-run-file'
`abbreviated-home-dir'
`data-directory'
`exec-path'
`path-separator'
`temporary-file-directory'
`woman-path'
`load-path'
`get-load-suffixes'          ;<FUNCTION>
`load-suffixes'
`load-file-rep-suffixes'\n
;; :EMACS-ENVIRONMENT-PROCESS
`emacs-priority'
`emacs-pid'
`process-environment'
`server-process'
`server-name'
`daemon-initialized'
`daemonp'\n
;; :EMACS-ENVIRONMENT-PROCESS-COMMAND-LINE
`command-line'
`command-line-1'
`command-line-normalize-file-name'
`command-line-args'
`command-line-args-left'
`command-line-default-directory'
`command-switch-alist'
`command-line-ns-option-alist'
`command-line-x-option-alist'\n
;; :EMACS-ENVIRONMENT-SESSION
`emacs-save-session-functions'
`emacs-session-save'
`x-session-previous-id'
`emacs-session-restore'\n
;; :EMACS-ENVIRONMENT-STATE
`purify-flag'
`current-idle-time'
`emacs-uptime'                ;<FUNCTION>
`cons-cells-consed'
`floats-consed'
`intervals-consed'
`memory-limit'
`memory-use-counts'
`misc-objects-consed'
`strings-consed'
`string-chars-consed'
`vector-cells-consed'
`obarray'
`pure-space-overflow-message'
`memory-full'
`memory-signal-data'
`max-specpdl-size'
`garbage-collect'
`gc-cons-threshold'
`gc-cons-percentage'
`load-average'                 ;<FUNCTION>
`num-input-keys'
`num-nonmacro-input-events'
`max-lisp-eval-depth'
`recursion-depth'\n
;; :EMACS-ENVIRONMENT-STATE-COMMANDS
`face-name-history'
`read-expression-history'
`yes-or-no-p-history'
`extended-command-history'
`command-history'
`history-length'               ; :NOTE Also a <PROPERTY>
`last-command'
`real-last-command'
`this-command'
`this-command-keys'            ;<FUNCTION>
`this-command-keys-vector'     ;<FUNCTION>
`this-original-command'
`this-single-command-keys'     ;<FUNCTION>
`this-single-command-raw-keys' ;<FUNCTION>\n
;; :EMACS-ENVIRONMENT-USER
`abbreviated-home-dir'
`bookmark-default-file'
`custom-file'                ;<FUNCTION>
`locate-user-emacs-file'     ;<FUNCTION>
`user-emacs-directory'
`woman-man.conf-path'\n
;; :EMACS-ENVIRONMENT-MON-LOCAL
`mon-help-buffer-spc-*DOC*'
`mon-get-env-vars-strings'
`mon-get-env-vars-symbols'
`mon-get-env-vars-emacs'
`mon-get-system-specs'
`mon-help-emacs-introspect'\n
:SEE-ALSO `mon-help-load-functions', `mon-help-mon-help',
`mon-help-mon-functions', `mon-help-package-keywords'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-emacs-introspect :insertp t)
    (mon-help-message-intrp "mon-help-emacs-introspect")))
;;
;;; :TEST-ME (mon-help-emacs-introspect t)

 
;;; ==============================
;;; :CHANGESET 2327
;;; :CREATED <Timestamp: #{2010-11-24T13:22:55-05:00Z}#{10473} - by MON KEY>
(defcustom *mon-emacs-external-programs-vars* 
  '((find-program                     . "lisp/progmodes/grep.el")
    (xargs-program                    . "lisp/progmodes/grep.el")
    (grep-program                     . "lisp/progmodes/grep.el")
    (tramp-smb-program                . "lisp/net/tramp-smb.el")
    (jka-compr-compression-info-list  . "lisp/jka-cmpr-hook.el")
    (insert-directory-program         . "lisp/files.el")
    (directory-free-space-program     . "lisp/files.el")
    (inferior-lisp-program            . "slime.el")
    (ispell-program-name              . "lisp/textmodes/ispell.el")
    (thumbs-conversion-program        . "lisp/thumbs.el")
    (browse-url-firefox-program       . "lisp/net/browse-url.el")
    (browse-url-mozilla-program       . "lisp/net/browse-url.el")
    (browse-url-netscape-program      . "lisp/net/browse-url.el")
    (browse-url-galeon-program        . "lisp/net/browse-url.el")
    (browse-url-generic-program       . "lisp/net/browse-url.el")
    (browse-url-epiphany-program      . "lisp/net/browse-url.el")
    (browse-url-gnome-moz-program     . "lisp/net/browse-url.el")
    (browse-url-mosaic-program        . "lisp/net/browse-url.el")
    (browse-url-xterm-program         . "lisp/net/browse-url.el")
    (browse-url-gnudoit-program       . "lisp/net/browse-url.el")
    (browse-url-text-browser          .  "lisp/net/browse-url.el")
    (browse-url-kde-program           . "lisp/net/browse-url.el"))
 "List of variables that point to external executables used by Emacs.\n
Each elt of list is a cons the car is a symbol naming a variable, the cdr a
string designating the defining file name. Each consed pair has the form:\n
 ( <VARIABLE> . <FILENAME> )\n
For libraries distributed with Emacs it may be either a relative path,
e.g. lisp/emacs-lisp/*.el for external libraries it is a filename only.\n
:SEE-ALSO `mon-help-emacs-external-programs', `mon-help-process-functions',
`mon-help-network-process', `mon-help-server-functions'.\n►►►"
 :type  '(alist :key-type symbol :value-type file)
 :group 'mon-doc-help-utils)

;; 
;;; ==============================
;; (defun mon-help-emacs-external-programs  (&optional insertp intrp)
;;   "List of external programs Emacs variables and functions invoke or access.\n
;;  :SEE-ALSO `*mon-emacs-external-programs-vars*'.\n►►►"
;;   (interactive "i\nP")
;;   (if (or insertp intrp)
;;       (mon-help-function-spit-doc 'mon-help-emacs-introspect :insertp t)
;;     (mon-message :msg-spec '(":FUNCTION `mon-help-emacs-introspect' " 
;;                      "-- pass non-nil for optional arg INTRP"))))
;;
;;; :TEST-ME (mon-help-emacs-introspect )
;;; :TEST-ME (mon-help-emacs-introspect )
;;; :TEST-ME (mon-help-emacs-introspect )

 
;;; ==============================
;;; :TODO Maybe refactor this to build the docstring at loadtime.
;;; :NOTE Docs generated with:
;;; (dolist (i
;;;          (list 'abbrev 'alloc 'applications 'auto-save 'bib 'c 'calendar 'comm
;;;          'convenience 'data 'debug 'development 'dired 'display 'dnd 'docs
;;;          'editing 'editing-basics 'emacs 'emulations 'environment 'execute
;;;          'extensions 'external 'faces 'files 'frames 'games 'hardware 'help
;;;          'hypermedia 'i18n 'installation 'internal 'keyboard 'languages 'limits
;;;          'lisp 'local 'mail 'maint 'matching 'menu 'mode-line 'mouse 'mule
;;;          'multimedia 'news 'oop 'outlines 'processes 'processes-basics
;;;          'programming 'terminals 'tex 'tools 'undo 'unix 'windows 'wp 'x))
;;;   (princ (format "%s %s\n---\n" i (mon-help-function-spit-doc i :do-group t))
;;;          (current-buffer)))
;;;
;;; :CREATED <Timestamp: #{2009-09-03T19:27:41-04:00Z}#{09364} - by MON KEY>
;;;###autoload
(defun mon-help-package-keywords (&optional insertp intrp)
  "Find packages matching a given keyword using `finder-by-keyword'.\n
Keyword Search for Lisp Libraries with \\[finder-by-keyword] command
search the standard Emacs Lisp libraries by topic keywords.
:SEE info node `\(emacs\)Library Keywords\'\.\n
:EXAMPLE\n\(finder-by-keyword\)\n
Here is a partial list of keywords for use with finder-by-keyword:\n
abbrev        Abbreviation handling, typing shortcuts, macros.
alloc         Storage allocation and gc for GNU Emacs Lisp interpreter.
applications  Applications written in Emacs.
auto-save     Preventing accidental loss of data.
bib           Code related to the `bib' bibliography processor.
c             Support for the C language and related languages.
calendar      Calendar and time management support.
comm          Communications, networking, remote access to files.
convenience   Convenience features for faster editing.
data          Support for editing files of data.
development   Support for further development of Emacs.
dired         Directory \"Editor\"
display       How characters are displayed in buffers.
dnd           Handling data from drag and drop.
docs          Support for Emacs documentation.
editing       Basic text editing facilities.
emacs         Customization of the One True Editor.
emulations    Emulations of other editors.
environment   Fitting Emacs with its environment.
execute       Executing external commands.
extensions    Emacs Lisp language extensions.
external      Interfacing to external utilities.
faces         Support for multiple fonts.
files         Support for editing and manipulating files.
frames        Support for Emacs frames and window systems.
games         Games, jokes and amusements.
hardware      Support for interfacing with exotic hardware.
help          Support for on-line help systems and Help commands.
hypermedia    Support for links between text or other media types.
i18n          Internationalization and alternate character-set support.
installation  Emacs installation.
internal      Code for Emacs internals, build process, defaults.
keyboard      Input from the keyboard.
languages     Specialized modes for editing programming languages.
lisp          Lisp support, including Emacs Lisp.
local         Code local to your site.
mail          Modes for electronic-mail handling.
maint         Maintenance aids for the Emacs development group.
matching      Various sorts of searching and matching.
menu 	      Input from the menus.
mode-line     Content of the modeline.
mouse 	      Input from the mouse.
mule 	      MULE Emacs internationalization.
multimedia    Non-textual support, specifically images and sound.
news 	      Support for netnews reading and posting.
oop 	      Support for object-oriented programming.
outlines      Support for hierarchical outlining.
processes     Process, subshell, compilation, and job control support.
programming   Support for programming in other languages.
terminals     Support for terminal types.
tex           Supporting code related to the TeX formatter.
tools         Programming tools.
unix          Front-ends/assistants for, or emulators of, UNIX-like features.
windows       Windows within a frame.
wp            Word processing.
x 	      The X Window system.\n
;; :FINDER-FUNCTIONS
`finder-by-keyword'
`finder-commentary'
`finder-list-keywords'
`finder-commentary'
`finder-summary'
`finder-compile-keywords'
`finder-unknown-keywords'
;; :FINDER-VARIABLES
`finder-package-info'
`finder-known-keywords'
`generated-finder-keywords-file'\n
:ALIASED-BY `mon-help-finder-keywords'\n
:SEE :FILE finder.el finder-inf.el
:SEE-ALSO `mon-help-emacs-introspect', `mon-help-help-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-package-keywords :insertp t)
    (mon-help-message-intrp "mon-help-package-keywords")))
;;
;;; :TEST-ME (mon-help-package-keywords)
;;; :TEST-ME (mon-help-package-keywords t)
;;; :TEST-ME (describe-function 'mon-help-package-keywords)
;;; :TEST-ME (apply 'mon-help-package-keywords '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-08T13:00:11-04:00Z}#{09326} - by MON KEY>
;;;###autoload
(defun mon-help-regexp-syntax (&optional insertp intrp)
  "Regular Expression Syntax overview.\n
;; :REGEXP-SPECIAL-CHARS
\.              -> match ANY
\*              -> match Preceeding - ALL
\+              -> match Preceeding - AT LEAST once.
\?              -> match Preceeding - once OR not at all
\*\? \+\? \?\?       -> match Preceeding - NON-GREEDY
\\=[...\]          -> Character ALTERNATIVE
\\=[^...\]         -> COMPLEMENTed Character Alternative
^              -> match BOL
$              -> match EOL
\\              -> backslash QUOTE special chars\n
;; :REGEXP-BACKSLASH-CONSTRUCTS
\\|             -> ALTERNATIVE
\\\\={m\\}          -> REPEAT match exactly N times
\\\\={m,n\\}        -> REPEAT match n-N times
\\( ... \\)      -> GROUPING construct
\\(\?: ... \\\)    -> SHY Grouping construct
\\(\?NUM: ... \\) -> Explicitly NUMBERED Group
\\digit         -> match DIGITH occurence
\\w             -> match any WORD CONSTITUENT char
\\W             -> match any char NOT a Word Constituent
\\Scode         -> match any char with SYNTAX code
\\Scode         -> match any char NOT with Syntax code
\\cc            -> match any char with CATEGORY
\\Cc            -> match any char NOT with Category
\\`             -> match EMPTY String
\\\\\='             -> match Empty String only at EOB
\\\\==             -> match Empty String only at POINT
\\b             -> match Empty String only at BEGINNING OR END of Word
\\B             -> match Empty String NOT at beginning or end of Word
\\=\\<             -> match Empty String only at BEGINNING of Word
\\=\\>             -> match Empty String only at END of Word
\\_<            -> match Empty String only at BEGINNING of Symbol
\\_>            -> match Empty String only at END of Symbol\n
;; :REGEXP-CHARACTER-CLASSES
\\=[:ascii:] [:nonascii:]
\\=[:alnum:] [:digit:] [:xdigit:]
\\=[:alpha:] [:lower:] [:upper:]
\\=[:blank:] [:cntrl:] [:graph:]
\\=[:print:] [:punct:] [:space:] [:word:]
\\=[:multibyte:] [:unibyte:]\n
:SEE info node `(elisp)Syntax of Regexps'
:SEE info node `(elisp)Match Data'\n
:SEE-ALSO `mon-help-search-functions', `mon-help-syntax-class',
`mon-help-syntax-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-regexp-syntax :insertp t)
    (mon-help-message-intrp "mon-help-regexp-syntax")))
;;
;;; :TEST-ME (describe-function 'mon-help-regexp-syntax)
;;; :TEST-ME (mon-help-regexp-syntax)
;;; :TEST-ME (mon-help-regexp-syntax t)
;;; :TEST-ME (apply 'mon-help-regexp-syntax '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Wednesday June 17, 2009 @ 04:58.00 PM - by MON KEY>
;;;###autoload
(defun mon-help-syntax-class (&optional insertp intrp)
  "Syntax class mappings.\n
List one maps from Int->Class->Code-Char.
List two maps Syntax class code character arguments to SYNTAX.\n
:INT    :CLASS              :CODE-CHAR
0       whitespace         \(designated by ` ' or `-'\)
1       punctuation        \(designated by `.'\)
2       word               \(designated by `w'\)
3       symbol             \(designated by `_'\)
4       open parenthesis   \(designated by `\('\)
5       close parenthesi   \(designated by `\)'\)
6       expression prefi   \(designated by `''\)
7       string quote       \(designated by `\"'\)
8       paired delimiter   \(designated by `$'\)
9       escape             \(designated by `\\'\)
10      character quote    \(designated by `/'\)
11      comment-start      \(designated by `<'\)
12      comment-end        \(designated by `>'\)
13      inherit            \(designated by `@'\)
14      generic comment    \(designated by `!'\)
15      generic string     \(designated by `|'\)\n
;; :SYNTAX-CLASS-TABLE
              SYNTAX-CLASSCODE        CHARACTER-ARGUMENT
:SYNTAX-CLASS whitespace character; \(designated by ` ' or `-'\)
:SYNTAX-CLASS word constituent; \(designated by `w'\)
:SYNTAX-CLASS symbol constituent; \(designated by `_'\)
:SYNTAX-CLASS punctuation character; \(designated by `.'\)
:SYNTAX-CLASS open parenthesis character; \(designated by `\('\)
:SYNTAX-CLASS close parenthesis character; \(designated by `\)'\)
:SYNTAX-CLASS string quote; \(designated by `\"'\)
:SYNTAX-CLASS escape-syntax character: \(designated by `\\'\)
:SYNTAX-CLASS character quote; \(designated by `/'\)
:SYNTAX-CLASS paired delimiter; \(designated by `$'\)
:SYNTAX-CLASS expression prefix; \(designated by `''\)
:SYNTAX-CLASS comment starter; \(designated by `<'\)
:SYNTAX-CLASS comment ender; \(designated by `>'\)
:SYNTAX-CLASS inherit standard syntax; \(designated by `@'\)
:SYNTAX-CLASS generic comment delimiter; \(designated by `!'\)
:SYNTAX-CLASS generic string delimiter; \(designated by `|'\)\n
:NOTE To get the syntax class of the char after point do:
 \(logand \(car \(syntax-after \(point\)\)\) 65535\)(\n
This is different from getting the char's syntax:
\(char-syntax \(following-char\)\)(\n
;; char-syntax
:SEE info node `(elisp)Syntax Table Internals'\n
:SEE `syntax_spec_code', `syntax_code_spec' in :FILE src/syntax.c\n
:SEE :FILE emacs-lisp/syntax.el\n
:SEE-ALSO `mon-help-syntax-functions', `mon-help-search-functions',
`mon-help-regexp-syntax'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-syntax-class :insertp t)
    (mon-help-message-intrp "mon-help-syntax-class")))
;;
;;; :TEST-ME (mon-help-syntax-class)
;;; :TEST-ME (mon-help-syntax-class t)
;;; :TEST-ME (describe-function 'mon-help-syntax-class)
;;; :TEST-ME (apply 'mon-help-syntax-class '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-22T17:28:44-05:00Z}#{10081} - by MON KEY>
;;;###autoload
(defun mon-help-syntax-functions (&optional insertp intrp)
  "Functions related to syntax and syntax tables.\n
;; :SYNTAX-FUNCTIONS-MOTION
`backward-prefix-chars' 
`forward-same-syntax'
`skip-syntax-forward'
`skip-syntax-backward'
`forward-comment'\n
;; :SYNTAX-FUNCTIONS-INSPECT
`internal-describe-syntax-value'
`char-syntax'
`get-char-char'
`property-syntax'
`describe-syntax'
`describe-vector'
`internal-describe-syntax-value'
`syntax-after'
`syntax-class'
`syntax-table'
`syntax-table-p'\n
;; :SYNTAX-FUNCTIONS-HANDLERS
`lisp-mode-variables'  ;; :NOTE Arg LISP-SYNTAX affects lisp-mode initialization
`copy-syntax-table'
`make-syntax-table'
`modify-syntax-entry'
`set-syntax-table'
`string-to-syntax'
`with-syntax-table'\n
;; :SYNTAX-FUNCTIONS-CASE
`set-case-syntax'
`set-case-syntax-pair'
`set-case-table'
`set-case-syntax-delims'
`standard-case-table'\n
;; :SYNTAX-FUNCTIONS-CATEGORY
`define-category'
`modify-category-entry'
`describe-categories'
`category-docstring'
`category-set-mnemonics'
`char-category-set'
`get-unused-category'
`category-table'
`category-table-p'
`make-category-table'
`make-category-set'
`modify-category-entry'
`set-category-table'
`standard-category-table'\n
;; :SYNTAX-FUNCTIONS-PARSE
`scan-lists'
`scan-sexps'
`parse-partial-sexp'
`beginning-of-defun'
`beginning-of-defun-raw'
`end-of-defun'
`parse-partial-sexp'
`matching-paren'
`syntax-ppss'
`syntax-ppss-after-change-function'
`syntax-ppss-context'
`syntax-ppss-debug'
`syntax-ppss-depth'
`syntax-ppss-flush-cache'
`syntax-ppss-stats'
`syntax-ppss-toplevel-pos'
`scan-error`\n
;; :SYNTAX-VARIABLES-PARSING
`comment-enter-backward'
`comment-use-syntax'
`comment-use-global-state'
`comment-add'                                     ; :NOTE Also a <FUNCTION>
`open-paren-in-column-0-is-defun-start'
`multibyte-syntax-as-symbol'
`defun-prompt-regexp'
`end-of-defun-function'
`beginning-of-defun-function'
`open-paren-in-column-0-is-defun-start'
`parse-sexp-ignore-comments'
`parse-sexp-lookup-properties'
`syntax-begin-function'
`syntax-ppss-last'
`syntax-ppss-max-span'
`syntax-ppss-stats'
`font-lock-beginning-of-syntax-function'
`font-lock-syntactic-keywords'\n
;; :SYNTAX-TABLES               
`standard-syntax-table'         ;<FUNCTION> 
`c-mode-syntax-table'           ; :NOTE A syntax-table is of type `char-table`:
`emacs-lisp-mode-syntax-table'  ;       \(type-of emacs-lisp-mode-syntax-table\)
`text-mode-syntax-table'\n
;; :SYNTAX-VARIABLES
`before-change-functions'
`find-word-boundary-function-table'
`words-include-escapes'\n
;; :SYNTAX-PROPERTY
`doc-string-elt`              ;<PROPERTY>
`syntax-table`                ;<PROPERTY>
`text-clone-syntax`           \n
;; :SYNTAX-MON-LOCAL
`mon-get-syntax-class-at'
`mon-line-test-content'\n
:SEE info node `(elisp)Syntax Tables'\n
:SEE :FILE emacs-lisp/syntax.el src/syntax.c\n
:SEE-ALSO `mon-help-syntax-class', `mon-help-regexp-syntax',
`mon-help-search-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-syntax-functions :insertp t)
    (mon-help-message-intrp "mon-help-syntax-functions")))
;;
;;; :TEST-ME (mon-help-syntax-functions)
;;; :TEST-ME (mon-help-syntax-functions t)
;;; :TEST-ME (describe-function 'mon-help-syntax-functions)
;;; :TEST-ME (apply 'mon-help-syntax-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-07T18:16:16-04:00Z}#{09325} - by MON KEY>
;;;###autoload
(defun mon-help-search-functions (&optional insertp intrp)
  "Common functions, vars, commands for searching, replacing, substituting.\n
:SEE info node `(elisp)Regexp Search'.\n
;; :SEARCH
`search-forward'
`search-backward'
`search-forward-regexp'  ;-> `re-search-forward'  :SEE-ALSO `posix-search-forward'
`search-backward-regexp' ;-> `re-search-backward' :SEE-ALSO `posix-search-backward'
`word-search-forward'
`word-search-backward'
`word-search-backward-lax'
`word-search-forward-lax'
`apropos-words-to-regexp'
`apropos-parse-pattern'\n
;; :SEARCH-INSPECT
`looking-at-p'
`looking-at'             ; :SEE-ALSO `posix-looking-at'
`looking-back'
`subregexp-context-p'\n
;; :SEARCH-MATCH-DATA
`match-end'
`match-data'
`match-beginning'
`match-string'
`match-string-no-properties'
`match-substitute-replacement'
`replace-match'
`replace-match-data'
`replace-match-maybe-edit'
`replace-match-string-symbols'
`save-match-data'
`set-match-data'\n
;; :SEARCH-REPLACE-ACTIONS
`flush-lines'
`keep-lines'
`replace'
`replace-rectangle'
`replace-regexp'
`replace-string'
`replace-regexp-in-string'
`replace-eval-replacement'
`replace-loop-through-replacements'
`perform-replace'
`map-query-replace-regexp'
`filter-buffer-substring'\n
;; :SEARCH-STRINGS
`string-prefix-p'
`compare-strings'
`string-match'           ; :SEE-ALSO `posix-string-match'
`string-match-p'
`string-equal'
`string='\n
;; :SEARCH-CHARS
`search-unencodable-char'
`find-multibyte-characters'\n
;; :SEARCH-MODIFY
`regexp-opt'
`regexp-opt-depth'
`regexp-quote'\n
;; :SEARCH-SUBSTITUTION
`subst-char-in-region'
`subst-char-in-string'
`translate-region'\n
;; :SEARCH-DIRED
`dired-do-copy-regexp'
`dired-do-create-files'
`dired-do-create-files-regexp'
`dired-do-isearch'
`dired-do-isearch-regexp'
`dired-do-query-replace-regexp'
`dired-do-rename-regexp'
`dired-do-search'
`dired-flag-files-regexp'
`dired-glob-regexp'
`dired-mark-files-containing-regexp'
`dired-mark-files-regexp'
`dired-isearch-filenames'
`dired-isearch-filenames-regexp'\n
;; :SEARCH-TAGS
`tags-search'
`tags-query-replace'\n
;; :SEARCH-COUNT
`how-many'\n
;; :SEARCH-VARIABLES
`inhibit-changing-match-data'
`buffer-substring-filters'
`search-spaces-regexp'
`cache-long-line-scans'
`case-replace'
`case-fold-search'
`default-case-fold-search'
`page-delimiter'
`paragraph-separate'
`paragraph-start'
`regexp-search-ring'
`search-upper-case'
`search-invisible'
`search-ring'
`sentence-end'                  ; :NOTE Also a <FUNCTION>
`sentence-end-base'
`sentence-end-double-space'
`sentence-end-without-space'
`sentence-end-without-period'\n
;; :SEARCH-MON-LOCAL
`mon-looking-back-p'
`replace-in-string-mon'
`replace-char-in-string-mon'
`mon-walk-regexps-in-file'
`mon-replace-regexps-in-file-list'
`mon-string-split-on-regexp'
`mon-string-csv-regexp'
`mon-string-rotate-to-regexp'
`mon-string-sub-old->new'
`replace-string-pairs-region-no-props'
`replace-string-pairs-region3'\n
:NOTE The relatively un-advertised features `grep', `grep-find' and `locate'.
:SEE progmodes/grep.el and lisp/locate.el\n
:SEE-ALSO `mon-help-regexp-syntax', `mon-help-syntax-functions',
`mon-help-syntax-class'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-search-functions :insertp t)
    (mon-help-message-intrp "mon-help-search-functions")))
;;
;;; :TEST-ME (mon-help-search-functions)
;;; :TEST-ME (mon-help-search-functions t)
;;; :TEST-ME (apply 'mon-help-search-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-14T08:00:09-04:00Z}#{10195} - by MON KEY>
;;;###autoload
(defun mon-help-hooks (&optional insertp intrp)
  "A list of common Emacs hooks.\n
;; :HOOK-FUNCTIONS
`add-hook'
`delay-mode-hooks'
`make-local-hook'
`remove-hook'
`run-hook-with-args'
`run-hook-with-args-until-failure'
`run-hook-with-args-until-success'
`run-hooks'
`run-mode-hooks'
`with-wrapper-hook'\n
;; :HOOKS
`abbrev-expand-functions'
`activate-mark-hook'
`activate-menubar-hook'
`after-change-functions'
`after-change-major-mode-hook'
`after-init-hook'
`after-insert-file-functions'
`after-make-frame-functions'
`after-revert-hook'
`after-save-hook'
`after-setting-font-hook'
`auto-fill-function'
`auto-save-hook'
`before-change-functions'
`before-hack-local-variables-hook'
`before-init-hook'
`before-make-frame-hook'
`before-revert-hook'
`before-save-hook'
`blink-paren-function'
`buffer-access-fontify-functions'
`bytecomp-load-hook'
`change-major-mode-hook'
`clone-buffer-hook'
`clone-indirect-buffer-hook'
`command-hook-internal'
`command-line-functions'
`comment-indent-function'
`compilation-start-hook'
`completion-setup-hook'
`custom-define-hook'
`deactivate-mark-hook'
`delay-mode-hooks'             ; :NOTE Also a <FUNCTION>
`delayed-mode-hooks'
`delete-frame-functions'
`delete-frame-hook'
`delete-terminal-functions'
`desktop-after-read-hook'
`desktop-no-desktop-file-hook'
`desktop-save-hook'
`dired-load-hook'
`disabled-command-function'
`disabled-command-hook'
`display-time-hook'
`echo-area-clear-hook'
`emacs-startup-hook'
`eshell-pre-command-hook'
`exit-language-environment-hook'
`find-file-hook'
`find-file-hooks'
`find-file-not-found-functions'
`find-file-not-found-hooks'
`find-function-after-hook'
`find-tag-hook'
`first-change-hook'
`font-lock-mode-hook'
`font-lock-beginning-of-syntax-function'
`font-lock-fontify-buffer-function'
`font-lock-fontify-region-function'
`font-lock-mark-block-function'
`font-lock-syntactic-face-function'
`font-lock-unfontify-region-function'
`grep-setup-hook'
`hack-local-variables-hook'
`insert-behind-hooks'
`insert-in-front-hooks'
`kbd-macro-termination-hook'
`kill-buffer-hook'
`kill-buffer-query-functions'
`kill-emacs-hook'
`kill-emacs-query-functions'
`lisp-indent-function'
`lisp-interaction-mode-hook'
`local-write-file-hooks'
`menu-bar-update-hook'
`message-mode-hook'
`minibuffer-exit-hook'
`minibuffer-setup-hook'
`mode-line-hook'
`modification-hooks'
`mouse-leave-buffer-hook'
`mouse-position-function'
`next-error-hook'
`post-command-hook'
`post-gc-hook'
`pre-abbrev-expand-hook'
`pre-command-hook'
`resume-tty-functions'
`set-language-environment-hook'
`special-mode-hook'
`suspend-hook'
`suspend-resume-hook'
`suspend-tty-functions'
`temp-buffer-setup-hook'
`temp-buffer-show-function'
`temp-buffer-show-hook'
`term-setup-hook'
`tooltip-hook'
`unload-feature-special-hooks'
`url-load-hook'
`window-configuration-change-hook'
`window-scroll-functions'
`window-setup-hook'
`window-size-change-functions'
`write-contents-functions'
`write-contents-hooks'
`write-file-functions'
`write-file-hooks'
`write-region-annotate-functions'\n
;; :HOOKS-MODE
`Info-mode-hook'
`apropos-mode-hook'
`change-log-mode-hook'
`compilation-mode-hook'
`completion-list-mode-hook'
`custom-mode-hook'
`diff-mode-hook'
`dired-mode-hook'
`emacs-lisp-mode-hook'
`help-mode-hook'
`lisp-mode-hook'
`text-mode-hook'
`view-mode-hook'\n
:SEE info node `(emacs)Hooks'
:SEE info node `(elisp)Hooks'
:SEE info node `(elisp)Standard Hooks'\n
:SEE-ALSO `mon-help-buffer-functions', `mon-help-file-dir-functions',
`mon-help-file-dir-functions-usage'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-hooks :insertp t)
    (mon-help-message-intrp "mon-help-hooks")))
;;
;;; :TEST-ME (mon-help-hooks)
;;; :TEST-ME (mon-help-hooks t)
;;; :TEST-ME (documentation 'mon-help-hooks)
;;; :TEST-ME (apply 'mon-help-hooks '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-27T15:14:24-05:00Z}#{10086} - by MON KEY>
;;;###autoload
(defun mon-help-file-dir-functions (&optional insertp intrp)
  "List of functions related to files and directories.\n
;; :FILE-DIRECTORY-ACTION
`access-file'
`byte-compile-file'
`cd'
`cd-absolute'
`confirm-nonexistent-file-or-buffer'
`copy-file'
`file-local-copy'
`insert-file'
`load-file'
`make-temp-file'
`make-temp-name'
`make-symbolic-link'
`process-file'
`start-file-process'
`view-file'\n
;; FILE-DIRECTORY-ACTION-DESTRUCTIVE
`append-to-file'
`delete-file'
`make-directory'
`make-directory-internal'
`rename-file'
`revert-buffer'                 ; :NOTE :SEE `ediff-current-file'
`with-temp-file'                ; :NOTE Output to generated bufer \" *temp file*\"
`write-file'\n
;; :FILE-DIRECTORY-SETTERS
`set-buffer-file-coding-system'
`set-file-name-coding-system'
`set-default-file-modes'
`set-file-modes'
`set-file-times'
`set-visited-file-modtime'
`set-visited-file-name'\n
;; :FILE-DIRECTORY-READ
`desktop-read'
`file-readable-p'
`find-alternate-file'
`find-alternate-file-other-window'
`find-file-read-args'
`find-backup-file-name'
`next-read-file-uses-dialog-p'
`read-abbrev-file'
`read-directory-name'
`read-file-modes'
`read-file-name'
`read-file-name-defaults'
`read-file-name-internal'
`read-filename-at-point'\n
;; :FILE-DIRECTORY-MODTIME
`buffer-modified-p'
`ask-user-about-supersession-threat'
`set-visited-file-modtime'
`visited-file-modtime'
`verify-visited-file-modtime'
`clear-visited-file-modtime'\n
;; :FILE-DIRECTORY-INSERT
`insert-directory'
`insert-directory-adj-pos'
`insert-directory-safely'
`insert-default-directory'     ;<VARIABLE>
`insert-directory-program'     ;<VARIABLE>\n
;; :FILE-DIRECTORY-INSPECT
`prune-directory-list'
`directory-files'
`directory-files-and-attributes'
`executable-find'
`file-attributes'
`file-chase-links'
`file-modes'
`file-newest-backup'
`file-nlinks'
`file-set-intersect'               ; :SEE :FILE lisp/loadhist.el
`file-system-info'\n
;; :FILE-DIRECTORY-NAME-MODIFY
`abbreviate-file-name'
`add-name-to-file'
`convert-standard-filename'
`directory-file-name'
`expand-file-name'
`file-modes-char-to-right'
`file-modes-char-to-who'
`file-modes-rights-to-number'
`file-modes-symbolic-to-number'
`recode-file-name'
`substitute-in-file-name'
`unix-sync'
`unhandled-file-name-directory'\n
;; :FILE-DIRECTORY-NAME-INSPECT
`buffer-file-name'                   ; :NOTE Also a <VARIABLE>
`file-name-all-completions'
`file-name-as-directory'
`file-name-completion'
`file-name-directory'
`file-name-extension'
`file-name-handler-alist'
`file-name-non-special'
`file-name-nondirectory'
`file-name-sans-extension'
`file-name-sans-versions'
`file-relative-name'
`file-truename'
`minibuffer-completing-file-name'
`parse-colon-path'
`pwd'
`tramp-drop-volume-letter'
`x-file-dialog'\n
;; :FILE-DIRECTORY-PREDICATE
`file-name-absolute-p'
`file-ownership-preserved-p'
`file-newer-than-file-p'
`file-readable-p'
`file-regular-p'
`file-remote-p'
`file-symlink-p'
`file-writable-p'
`file-accessible-directory-p'
`file-attributes-lessp'
`file-compressed-p'
`file-directory-p'
`file-executable-p'
`file-exists-p'
`file-locked-p'
`next-read-file-uses-dialog-p'
`recent-auto-save-p'\n
;; :FILE-DIRECTORY-COMINT
`comint-replace-by-expanded-filename'
`comint-dynamic-list-filename-completions'
`comint-dynamic-complete-as-filename'
`comint-dynamic-complete-filename'
`comint-match-partial-filename'
`comint-completion-addsuffix'
`comint-directory'
`comint-file-name-quote-list' ;<VARIABLE>\n
;; :FILE-DIRECTORY-FIND
`file-expand-wildcards'
`erc-find-file'
`ffap'
`find-library'
`find-library-name'
`find-file'
`find-file-at-point'
`find-file-binary'
`find-file-existing'
`find-file-literally'
`find-file-name-handler'
`find-file-noselect'
`find-file-noselect-1'
`find-file-not-found-set-buffer-file-coding-system'
`find-file-other-frame'
`find-file-other-window'
`find-file-read-args'
`find-file-read-only'
`find-file-read-only-other-frame' 	
`find-file-read-only-other-window'
`find-file-text'                        ; :FILE lisp/dos-w32.el
`find-dired'
`hexl-find-file'
`locate-dominating-file'
`locate-file-completion'
`locate-file-completion-table'
`locate-library'
`wildcard-to-regexp'\n
;; :FILE-DIRECTORY-FIND-BUFFER
`create-file-buffer'
`find-buffer-visiting'
`find-buffer-file-type'                 ; :FILE lisp/files.el
`find-buffer-file-type-coding-system'   ; :FILE lisp/dos-w32.el
`find-buffer-file-type-match'           ; :FILE lisp/dos-w32.el
`find-library-suffixes'
`get-file-buffer'\n
;; :FILE-DIRECTORY-ELISP                ; :FILE lisp/loadhist.el
`file-dependents'
`file-loadhist-lookup'
`file-provides'
`file-requires'\n
;; :FILE-DIRECTORY-RECOVER
`after-find-file'
`recover-file'
`recover-this-file'\n
;; :FILE-DIRECTORY-RECOVER-AUTO-SAVE-FUNCTIONS
`auto-save-file-name-p'
`auto-save-mode'
`make-auto-save-file-name'\n
;; :FILE-DIRECTORY-RECOVER-AUTO-SAVE-VARIABLES
`auto-save-default'
`auto-save-file-name-transforms'
`auto-save-hook'
`auto-save-include-big-deletions'
`auto-save-interval'
`auto-save-list-file-name'
`auto-save-list-file-prefix'
`auto-save-timeout'
`auto-save-visited-file-name'
`delete-auto-save-files'\n
;; :FILE-DIRECTORY-BACKUP-VARIABLES
`make-backup-files'
`backup-inhibited'
`backup-by-copying'
`backup-by-copying-when-linked'
`backup-by-copying-when-mismatch'
`backup-by-copying-when-privileged-mismatch'
`backup-enable-predicate'
`dired-kept-versions'
`delete-old-versions'
`kept-old-versions'
`kept-new-versions'
`normal-backup-enable-predicate'  ;<FUNCTION>\n
;; :FILE-DIRECTORY-URL
`browse-url-of-file'
`url-file-directory'
`url-file-extension'
`url-file-nondirectory'
`url-generate-unique-filename'
`url-make-private-file'
`url-insert-file-contents'\n
;; :FILE-DIRECTORY-ERC
`erc-directory-writable-p'\n
;; :FILE-DIRECTORY-W32
`w32-get-true-file-attributes'
`w32-long-file-name'
`w32-short-file-name'\n
;; :FILE-DIRECTORY-VARIABLES-BUFFER
`add-log-buffer-file-name-function'
`buffer-auto-save-file-name'
`buffer-file-coding-system'
`buffer-file-coding-system-explicit'
`buffer-file-format'
`buffer-file-name'                   ; :NOTE Also a <FUNCTION>
`buffer-file-number'
`buffer-file-numbers-unique'
`buffer-file-read-only'
`buffer-file-truename'
`buffer-file-type'
`confirm-nonexistent-file-or-buffer' ; :NOTE Also a <FUNCTION>
`list-buffers-directory'\n
;; :FILE-DIRECTORY-VARIABLES
`abbreviated-home-dir'
`after-insert-file-functions'
`automount-dir-prefix'
`before-save-hook'
`buffer-offer-save'
`cd-path'
`coding-system-for-read'
`coding-system-for-write'
`completion-ignored-extensions'
`default-directory'
`default-directory-alist'
`directory-abbrev-alist'
`directory-free-space-program'
`directory-listing-before-filename-regexp'
`exec-suffixes'
`file-coding-system-alist'
`file-local-variables-alist'
`file-name-coding-system'
`file-name-handler-alist'
`file-name-history'
`file-name-invalid-regexp'
`file-precious-flag'
`find-directory-functions'
`find-file-existing-other-name'
`find-file-hook'
`find-file-not-found-functions'
`find-file-not-found-hooks'
`find-file-not-true-dirname-list'
`find-file-visit-truename'
`find-file-wildcards'
`file-name-version-regexp'
`inhibit-file-name-handlers'
`inhibit-file-name-operation'
`insert-default-directory'
`insert-directory-program'
`load-file-name'
`load-file-rep-suffixes'
`load-suffixes'
`locate-dominating-stop-dir-regexp'
`null-device'
`path-separator'
`process-file-side-effects'
`revert-buffer-function'
`revert-buffer-insert-file-contents-function'
`revert-without-query'
`temporary-file-directory'
`write-contents-functions'\n
;; :FILE-DIRECTORY-MON-LOCAL
`mon-file-reduce-name'
`mon-build-path'
`mon-buffer-written-p'
`mon-get-buffer-parent-dir'
`mon-truncate-path-for-prompt'
`mon-string-split-dir-recurse'
`mon-dir-common-paths'
`mon-add-subdirs-to-list'
`mon-insert-subdirs-in-buffer'
`mon-get-dir-subdir-default'
`mon-file-dir-attributes->plist'
`mon-file-stamp'
`mon-file-stamp-minibuffer'
`mon-file-stamp-buffer-filename'
`mon-get-file-mod-times'
`mon-insert-dirs-in-path'
`mon-insert-file-in-dirs'
`mon-insert-naf-file-in-dirs'
`mon-toggle-dired-dwim-target'
`mon-with-file-buffer'
`mon-copy-file-path'
`mon-copy-file-multiple'
`mon-file-truename-p'
`mon-file-dir-attributes->plist'
`mon-file-ensure-extension-is-el'
\n
:SEE info node `(elisp)Files'\n
:SEE-ALSO `mon-help-file-dir-functions-usage', `mon-help-process-functions',
`mon-help-buffer-functions', `mon-help-hooks'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-file-dir-functions :insertp t)
    (mon-help-message-intrp "mon-help-file-dir-functions")))
;;
;;; :TEST-ME (mon-help-file-dir-functions)
;;; :TEST-ME (mon-help-file-dir-functions t)
;;; :TEST-ME (describe-function 'mon-help-file-dir-functions)
;;; :TEST-ME (apply 'mon-help-file-dir-functions '(t))

 
;;; ==============================
;;; :RENAMED `mon-help-file-dir-functions' -> `mon-help-file-dir-functions-usage'
;;; :MODIFICATIONS <Timestamp: #{2009-10-28T14:44:24-04:00Z}#{09443} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday May 06, 2009 @ 01:13.41 PM - by MON KEY>
;;;###autoload
(defun mon-help-file-dir-functions-usage (&optional insertp intrp)
  "Examples of file/directory name related function usage.\n
:SEE info node `(elisp)Files'\n
:NOTE Indentation below is for readablility :).\n
;; :FILE-BUFFER-CONJUNCT-USAGE
`buffer-file-name'               ;<&optional BUFFER>
 (buffer-file-name)\n
`find-buffer-visiting'           ;<FILENAME &optional PREDICATE>
 \(find-buffer-visiting
   \(filename\)\n
;; :FILE-DIRECTORY-ACTION-USAGE
`find-file'                      ;<FILENAME &optional WILDCARDS>
 \(find-file
   \(buffer-file-name\)\)\n
;; :FILE-DIRECTORY-PREDICATE-USAGE
`file-directory-p'               ;<FILENAME>
 \(file-directory-p
   doc-directory\)\n
`file-executable-p'              ;<FILENAME>
 \(file-executable-p
  \(executable-find \"emacs\"\)\)\n
`file-exists-p'                  ;<FILENAME>
 \(file-exists-p
  \(buffer-file-name\)\)\n
`file-locked-p'                  ;<&rest IGNORE>
 \(file-locked-p\)\n
`file-name-absolute-p'           ;<FILENAME>
 \(file-name-absolute-p
   \(directory-file-name
     default-directory\)\)\n
`file-newer-than-file-p'         ;<FILE1 FILE2>
 \(file-newer-than-file-p
  \(buffer-file-name\)
     doc-directory\)\n
`file-regular-p'                 ;<FILENAME>
 \(file-regular-p doc-directory\)\n
 \(file-regular-p \"~/.emacs\"\)\n
`file-writable-p'                ;<FILENAME>
 \(file-writable-p
   default-directory\)\n
;; :FILE-DIRECTORY-NAME-INSPECT-USAGE
`expand-file-name'               ;<NAME &optional DEFAULT-DIRECTORY>
 \(expand-file-name \"../\"\)
 \(expand-file-name \"../../\"\)
 \(expand-file-name \"../../../\"\)\n
`file-relative-name'             ;<FILENAME &optional DIRECTORY>
 \(file-relative-name
    default-directory\)\n
 \(file-relative-name
   \(buffer-file-name\)\)\n
`file-expand-wildcards'          ;<PATTERN &optional FULL>
 \(file-expand-wildcards
  \(concat doc-directory
    \"nxml/*.el\"\)\)\n
`file-truename'                  ;<FILENAME &optional COUNTER PREV-DIRS>
 \(file-truename
   \(getenv \"HOME\"\)\)\n
`substitute-in-file-name'        ;<FILENAME>
 \(substitute-in-file-name
   \"$HOME\\.emacs\")\n
`file-name-directory'            ;<FILENAME>
 \(file-name-directory
   \(buffer-file-name\)\)\n
`file-name-nondirectory'         ;<FILENAME>
 \(file-name-nondirectory 
   \(directory-file-name
     default-directory\)\)\n
`file-name-as-directory'         ;<FILE>
 \(file-name-as-directory
    default-directory\)\n
`file-name-nondirectory'         ;<FILENAME>
 \(file-name-nondirectory
   \(buffer-file-name\)\)\n
`file-name-sans-extension'       ;<FILENAME>
 \(file-name-sans-extension
   \(buffer-file-name\)\)\n
`directory-file-name'            ;<DIRECTORY>
 \(directory-file-name
   default-directory\)\n
 \(directory-file-name
   \(buffer-file-name\)\)\n
;; :DIRECTORY-INSPECT-USAGE
`directory-files'                ;<DIRECTORY &optional FULL MATCH NOSORT>
 \(directory-files
   default-directory\)\n
 \(directory-files
   \(file-name-directory 
     \(buffer-file-name\)\) nil \".el\"\)\n
`directory-files-and-attributes' ;<DIRECTORY &optional FULL MATCH NOSORT ID-FORMAT>
 \(directory-files-and-attributes
   default-directory\) \n
;; :FILE-DIRECTORY-INSPECT-PROPERTY-USAGE
`set-visited-file-modtime'       ;<&optional TIME-LIST>\n
`file-attributes'                ;<FILENAME &optional ID-FORMAT>
 \(file-attributes
    default-directory)\n
  List-returned consists of 12 elements:\n
  :IS-DIRECTORY-P      ;<- nth 0  t|nil
  :NUM-NAMES-OF-FILE   ;<- nth 1
  :UID                 ;<- nth 2
  :GID                 ;<- nth 3
  :LAST-ACCESSED       ;<- nth 4
  :LAST-MODIFIED       ;<- nth 5
  :LAST-STATUS-CHANGE  ;<- nth 6 
  :SIZE-IN-BYTES       ;<- nth 7 
  :FILES-MODES         ;<- nth 8 
  :GID-CHANGES-P       ;<- nth 9  :NOTE t if delete causes change on recreate.
  :FILE-INODE-NUMBER   ;<- nth 10
  :FILE-SYSTEM-NUMBER  ;<- nth 11\n
:NOTE `file-attributes' elts nth 4, 5, and 6 are as per `current-time'.\n
 \(current-time\) -> \(HIGH LOW MICROSEC\)\n
 \(decode-time \(current-time\)\) -> \(SEC MINUTE HOUR DAY MONTH YEAR DOW DST ZONE\)\n
 \(decode-time \(nth 5 \(file-attributes default-directory\)\)\)\n
;; :FILE-DIRECTORY-W32-USAGE
`convert-standard-filename'      ;<FILENAME>
 \(convert-standard-filename 
   \(file-truename
    \(getenv \"PROGRAMFILES\"\)\)\n
`w32-get-true-file-attributes'   ;<VARIABLE>\n
`w32-long-file-name'             ;<FILENAME>
 \(w32-long-file-name 
  \(getenv \"HOME\"\)\)\n
`w32-short-file-name'            ;<FILENAME>
 \(w32-short-file-name 
   \(getenv \"PROGRAMFILES\"\)\)\n
;; :FILE-DIRECTORY-CONJUNCT-USAGE
`split-string'                   ;<STRING &optional SEPARATORS OMIT-NULLS>
 \(split-string 
   \(directory-file-name
     default-directory\) \"/\"\)\n
`thing-at-point'                 ;<THING>
 \(thing-at-point
   'filename\)~/.emacs \n
`bounds-of-thing-at-point'       ;<THING>
 \(bounds-of-thing-at-point
   'filename\)~/.emacs\n
`ffap'                           ;<&optional FILENAME>
 \(ffap\)~/.emacs\n
;; :FILE-DIRECTORY-MON-LOCAL-USAGE
`mon-toggle-dired-dwim-target'
 \(mon-toggle-dired-dwim-target\)\n
`mon-get-file-mod-times'         ;<FILE-OR-DIR>
 \(mon-get-file-mod-times
   user-emacs-directory\)\n
`mon-insert-dirs-in-path'        ;<DIR-LIST DIR-PATH>
 \(mon-insert-dirs-in-path
   symbol path\)\n
`mon-insert-file-in-dirs'        ;<MAKE-DIR-LIST INSERT-TEXT EXTENSION>
 \(mon-insert-file-in-dirs
   \(make-dir-list
     insert-text extension\)\)\n
`mon-insert-naf-file-in-dirs'    ;<MAKE-DIR-LIST>
 \(mon-insert-naf-file-in-dirs
   \(make-dir-list\)\)\n
:ALIASED-BY `mon-help-directory-file-functions-usage'\n
:SEE-ALSO `mon-help-file-dir-functions', `mon-help-buffer-functions',
`mon-help-hooks', `mon-help-process-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-file-dir-functions-usage :insertp t)
    (mon-help-message-intrp "mon-help-file-dir-functions-usage")))
;;
;;; :TEST-ME (mon-help-file-dir-functions-usage)
;;; :TEST-ME (mon-help-file-dir-functions-usage t)
;;; :TEST-ME (describe-function 'mon-help-file-dir-functions-usage)
;;; :TEST-ME (apply 'mon-help-file-dir-functions-usage '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Friday July 03, 2009 @ 04:45.34 PM - by MON KEY>
;;;###autoload
(defun mon-help-process-functions (&optional insertp intrp)
  "Process related functions.\n
;; :PROCESS-ACTIONS
`accept-process-output'
`call-process'
`call-process-shell-command'
`continue-process'
`delete-process'
`interrupt-process'
`kill-process'
`make-network-process'          :SEE `mon-help-make-network-process'
`process-coding-system'
`process-command'
`process-contact'
`process-file'
`process-file-shell-command'
`process-get'
`process-lines'
`process-put'
`process-send-eof'
`process-send-region'
`process-send-string'
`quit-process'
`set-process-buffer'
`set-process-coding-system'
`set-process-datagram-address'
`set-process-filter'
`set-process-plist'
`set-process-sentinel'
`shell-quote-argument'
`signal-process'
`start-file-process'
`start-file-process-shell-command'
`start-process-shell-command'
`stop-process'\n
;; :PROCESS-COMINT
`comint-check-proc'
`make-comint-in-buffer'\n
;; :PROCESS-INSPECT
`process-datagram-address'
`process-mark'
`process-buffer'
`get-process'
`get-buffer-process'
`getenv-internal'
`process-exit-status'
`process-command'
`process-id'
`process-filter'           
`process-plist'
`process-name'
`process-sentinel'
`process-status'
`process-tty-name'
`process-type'\n
;; :PROCESS-ENUMERATE
`list-processes'
`list-system-processes'
`process-attributes'
`process-list'
`system-process-attributes'\n
;; :PROCESS-FLAGS
`process-kill-without-query'
`set-network-process-option'
`set-process-query-on-exit-flag'
`process-query-on-exit-flag'
`process-inherit-coding-system-flag'\n
;; :PROCESS-PREDICATES
`process-filter-multibyte-p'
`process-running-child-p'
`processp'
`waiting-for-user-input-p'\n
;; :PROCESS-TRANSACTION-QUEUES
`tq-close'
`tq-create'
`tq-enqueue'\n
;; :PROCESS-VARIABLES
`delete-exited-processes'
`exec-path'
`exec-directory'
`exec-suffixes'
`mode-line-process'
`process-adaptive-read-buffering'
`process-connection-type'
`process-coding-system-alist'
`process-environment'
`process-file-side-effects'
`w32-quote-process-args'\n
;; :PROCESS-MON-LOCAL
`mon-get-proc-w-name'
`mon-get-sys-proc-list'
`mon-insert-sys-proc-list'\n
:NOTE :SEE :FILE lisp/vc/diff.el for usage examples\n
:SEE :FILE process.c\n
:SEE info node `(elisp)Processes'\n
:SEE-ALSO `mon-help-make-network-process', `mon-help-server-functions',
`mon-help-file-dir-functions', `mon-help-file-dir-functions',
`mon-help-buffer-functions', `mon-help-hooks'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-process-functions :insertp t)
    (mon-help-message-intrp "mon-help-process-functions")))
;;
;;; :TEST-ME (mon-help-process-functions)
;;; :TEST-ME (mon-help-process-functions t)
;;; :TEST-ME (describe-function  'mon-help-process-functions)
;;; :TEST-ME (apply 'mon-help-process-functions '(t))

 
;;; ==============================
;;; :CHANGESET 1706
;;; :CREATED <Timestamp: #{2010-04-12T13:19:25-04:00Z}#{10151} - by MON KEY>
;;;###autoload
(defun mon-help-make-network-process (&optional insertp intrp)
  "A short form enumeration of args for `make-network-process'.\n
;; :MAKE-NETWORK-PROCESS-ARGS\n
:name    <NAME>\n
:buffer  {<BUFFER>|<BUFFER-NAME>}\n
:type    {nil|datagram} ;When nil create a stream\n
:local   <ADDRESS> ;When non-nil overrides `:family', `:host', `:service' args\n
:remote  <ADDRESS> ;When non-nil overrides `:family', `:host', `:service' args\n
         ¦ IPv4-address -> <VECTOR> of five elts, four 8-bit one 16-bit e.g.:
         ¦                          [192 168 1 100 8080] -> 192.168.1.100:8080
         ¦ IPv6-address -> <VECTOR> of nine elts, each a 16-bit integer
         ¦ local-address      -> <STRING>
         ¦ unsupported-family -> <CONS> (F . AV) -> (<INTEGER> . <VECTOR>)\n
:family  {local|ipv4|ipv6|[address and protocol of service]}\n
:host    {local|[server-process-name{host-name|host-ip}]}\n
:service {name|ip|t[port{<STRING>|<INTEGER>}]}\n
:coding  {<SYMBOL>|<CONS>}\n
:nowait  {nil|t}\n
:noquery {nil|t}\n
:stop    {nil|t}\n
:filter  <FILTER>\n
:sentinel <SENTINEL>\n
:log      <FUNCTION> which accepts the args:
                    ¦ <SERVER>  ;<- A process
                    ¦ <CLIENT>  ;<- A process
                    ¦ <MESSAGE> ;<- A string\n
:plist  <PLIST>\n
:server When t <FAMILY>, <SERVICE>, connection type (a stream or datagram)
        When <INTEGER> length of connection queue\n
:filter-multibyte {nil|t}\n
;; :MAKE-NETWORK-PROCESS-CONNECTION-OPTIONS\n
:broadcast    <BOOLEAN>
:dontroute    <BOOLEAN>
:keepalive    <BOOLEAN>
:linger       <INTEGER>
:oobinline    <BOOLEAN>
:priority     <INTEGEER>
:reuseaddr    <BOOLEAN>
:bindtodevice <DEVICE-NAME>\n
;; :MAKE-NETWORK-PROCESS-CONNECTION-OPTIONS-TEST-KEY-VAL
Form1: (featurep 'make-network-process '(KEYWORD VALUE))
Return non-nil if make-network-process accepts <KEYWORD> with <VALUE>.\n
 \(featurep 'make-network-process '\(:nowait t\)\)
 \(featurep 'make-network-process '\(:type datagram\)\)
 \(featurep 'make-network-process '\(:family local\)\)
 \(featurep 'make-network-process '\(:family ipv6\)\)
 \(featurep 'make-network-process '\(:service t\)\)\n
;; :MAKE-NETWORK-PROCESS-CONNECTION-OPTIONS-TEST-KEYWORD
Form2: (featurep 'make-network-process 'KEYWORD)
Return non-nil if `make-network-process' accepts network option arg <KEYWORD>.\n
 \(featurep 'make-network-process :bindtodevice\)
 \(featurep 'make-network-process :broadcast\)
 \(featurep 'make-network-process :dontroute\)
 \(featurep 'make-network-process :keepalive\)
 \(featurep 'make-network-process :linger\)
 \(featurep 'make-network-process :oobinline\)
 \(featurep 'make-network-process :priority\)
 \(featurep 'make-network-process :reuseaddr\)\n
;; :MAKE-NETWORK-PROCESS-INTERFACE
`process-datagram-address'
`set-process-datagram-address'
`format-network-address'
`network-interface-info'
`network-interface-list'
`set-network-process-option'\n
:ALIASED-BY `mon-help-network-process'\n
:SEE :FILE process.c\n
:SEE info node `(elisp)Network Processes'.\n
:SEE-ALSO `mon-help-process-functions', `mon-help-server-functions',
`mon-help-process-functions', `mon-help-hooks', `mon-help-file-dir-functions',
`mon-help-buffer-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-make-network-process :insertp t)
    (mon-help-message-intrp "mon-help-make-network-process")))
;;
;;; :TEST-ME (mon-help-make-network-process)
;;; :TEST-ME (mon-help-make-network-process t)
;;; :TEST-ME (describe-function 'mon-help-make-network-process)
;;; :TEST-ME (appply 'mon-help-make-network-process nil '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-19T18:30:50-05:00Z}#{10032} - by MON KEY>
;;;###autoload
(defun mon-help-server-functions (&optional insertp intrp)
  "List of Emacs server related functions and variables.\n
;; :SERVER-FUNCTIONS
`server-add-client'
`server-buffer-done'
`server-clients-with'
`server-create-tty-frame'
`server-create-window-system-frame'
`server-delete-client'
`server-done'
`server-edit'
`server-ensure-safe-dir'
`server-eval-and-print'
`server-execute'
`server-execute-continuation'
`server-force-delete'
`server-goto-line-column'
`server-goto-toplevel'
`server-handle-delete-frame'
`server-handle-suspend-tty'
`server-kill-buffer'
`server-kill-buffer-query-function'
`server-kill-emacs-query-function'
`server-log'
`server-mode'
`server-process-filter'
`server-quote-arg'
`server-return-error'
`server-running-p' 
`server-save-buffers-kill-terminal'
`server-select-display'
`server-send-string'
`server-sentinel'
`server-start'
`server-switch-buffer'
`server-temp-file-p'
`server-unload-function'
`server-unquote-arg'
`server-unselect-display'
`server-visit-files'
`server-with-environment'\n
;; :SERVER-VARIABLES
`server-auth-dir'
`server-buffer'
`server-buffer-clients'
`server-clients'
`server-done-hook'
`server-existing-buffer'
`server-host'
`server-kill-buffer-running'
`server-kill-new-buffers'
`server-log'
`server-log-time-function'
`server-mode'
`server-name'
`server-process'
`server-raise-frame'
`server-socket-dir'
`server-switch-hook'  
`server-temp-file-regexp'
`server-use-tcp'
`server-visit-hook'
`server-window'\n
;; :SERVER-FUNCTION-USAGE
\(featurep 'make-network-process\)
\(locate-user-emacs-file \"server/\"\)\n
\\\(getenv \"USERDOMAIN\"\)\) ;<- W32
\(file-truename \(getenv \"APPDATA\"\)\) ;<- W32
\(getenv \"EMACS_SERVER_FILE\"\)
\(file-truename \(getenv \"EMACS_SERVER_FILE\"\)\)
\(getenv \"EMACSCLIENT_STARTING_SERVER\"\)
\(file-truename \(getenv \"EMACSCLIENT_STARTING_SERVER\"\)\)\n
:SEE info node `(emacs)Emacs Server'\n
:SEE-ALSO `mon-help-ipv4-header', `mon-help-process-functions',
`mon-help-make-network-process', `mon-help-file-dir-functions',
`mon-help-buffer-functions', `mon-help-hooks'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-server-functions :insertp t)
    (mon-help-message-intrp "mon-help-server-functions")))
;;
;;; :TEST-ME (mon-help-server-functions)
;;; :TEST-ME (mon-help-server-functions nil t)
;;; :TEST-ME (describe-function 'mon-help-server-functions)
;;; :TEST-ME (apply 'mon-help-server-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2067
;;; :CREATED <Timestamp: #{2010-08-17T12:55:57-04:00Z}#{10332} - by MON KEY>
;;;###autoload
(defun mon-help-inhibit-functions (&optional insertp intrp)
  "List of functions and variables for temporary inhibition of Emacs behavior.\n
;; :INHIBIT-FUNCTIONS
`with-silent-modifications'
`with-local-quit'\n
;; :INHIBIT-VARIABLES
`inhibit-changing-match-data'
`inhibit-debug-on-entry'
`inhibit-default-init'
`inhibit-eol-conversion'
`inhibit-eval-during-redisplay'
`inhibit-field-text-motion'
`inhibit-file-name-handlers'
`inhibit-file-name-operation'
`inhibit-first-line-modes-regexps'
`inhibit-first-line-modes-suffixes'
`inhibit-frame-set-background-mode'
`inhibit-free-realized-faces'
`inhibit-iso-escape-detection'
`inhibit-load-charset-map'
`inhibit-local-menu-bar-menus'
`inhibit-menubar-update'
`inhibit-modification-hooks'
`inhibit-null-byte-detection'
`inhibit-point-motion-hooks'
`inhibit-quit'
`inhibit-read-only'
`inhibit-redisplay'
`inhibit-splash-screen'
`inhibit-startup-buffer-menu'
`inhibit-startup-echo-area-message'
`inhibit-startup-hooks'
`inhibit-startup-message'
`inhibit-startup-screen'
`inhibit-x-resources'\n
;; :INHIBIT-PROPERTIES
`apropos-inhibit` ;:NOTE \(mon-map-obarray-symbol-plist-props 'apropos-inhibit\)\n
;; :INHIBIT-FUNCTIONS-MON-LOCAL
`mon-with-inhibit-buffer-read-only'
`mon-inhibit-read-only'
`mon-with-inhibit-buffer-read-only'
`mon-inhibit-modification-hooks'
`mon-inhibit-point-motion-hooks'
`mon-toggle-read-only-point-motion'
`mon-with-inhibit-buffer-read-only-TEST'\n
:SEE-ALSO `mon-help-emacs-introspect'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-inhibit-functions :insertp t)
    (mon-help-message-intrp "mon-help-inhibit-functions")))
;;
;;; :TEST-ME (mon-help-inhibit-functions)
;;; :TEST-ME (mon-help-inhibit-functions t)
;;; :TEST-ME (apply 'mon-help-inhibit-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-02T10:22:56-04:00Z}#{09363} - by MON KEY>
;;;###autoload
(defun mon-help-buffer-functions (&optional insertp intrp)
  "Buffer related functions.\n
;; :BUFFER-ACTIONS-ON
`ask-user-about-supersession-threat'
`ispell-buffer'
`create-file-buffer'
`eval-buffer'
`eval-current-buffer'
`generate-new-buffer'
`generate-new-buffer-name'
`find-buffer-visiting'
`get-file-buffer'
`lock-buffer'
`rename-buffer'
`revert-buffer'
`save-buffer'
`save-current-buffer'
`unlock-buffer'
`view-buffer'\n
;; :BUFFER-ACTIONS-SETTERS
`set-buffer'
`set-buffer-auto-saved'
`set-buffer-major-mode'
`set-buffer-multibyte'
`set-buffer-modified-p'\n
;; :BUFFER-ACTIONS-MOVEMENT
`barf-if-buffer-read-only'
`bury-buffer'
`clone-buffer'
`clone-indirect-buffer'
`clone-indirect-buffer-other-window'
`display-buffer'
`fit-window-to-buffer'
`get-buffer'
`get-buffer-create'
`make-indirect-buffer'
`next-buffer'
`other-buffer'
`pop-to-buffer'
`previous-buffer'
`read-buffer'
`read-buffer-to-switch'
`replace-buffer-in-windows'
`set-buffer'
`switch-to-buffer'
`switch-to-buffer-other-frame'
`switch-to-buffer-other-window'
`unbury-buffer'
`view-buffer-other-frame'
`view-buffer-other-window'\n
;; :BUFFER-ACTIONS-ON-MULITPLE
`buffer-list'
`buffer-menu'
`buffer-menu-other-window'
`grep-buffers'
`list-buffers'
`save-some-buffers'\n
;; :BUFFER-CONTENTS
`compare-buffer-substrings'
`insert-buffer'
`prepend-to-buffer'
`buffer-string'
`buffer-substring'
`filter-buffer-substring'
`buffer-substring-filters'        ;<VARIABLE>
`buffer-substring-no-properties'
`buffer-swap-text'
`erase-buffer'
`append-to-buffer'
`copy-to-buffer'
`with-temp-buffer'
`with-output-to-temp-buffer'
`get-buffer'              ; :NOTE \(read \(get-buffer \"SOME-BUFFER-NAME\"\)\)\n
;; :BUFFER-INSPECT
`buffer-base-buffer'
`buffer-chars-modified-tick'
`buffer-modified-tick'
`buffer-disable-undo'
`buffer-enable-undo'
`buffer-face-mode'
`buffer-face-set'
`buffer-face-toggle'
`buffer-file-name'
`buffer-has-markers-at'
`buffer-local-value'
`buffer-local-variables'
`buffer-name'
`overlay-buffer'
`buffer-size'
`current-buffer'
`gap-position'
`gap-size'
`get-buffer-process'
`get-buffer-window'
`get-buffer-window-list'
`internal-complete-buffer-except'
`internal-complete-buffer'
`list-buffers-directory'       ; :NOTE DVC's dvc-core.el sets this `permanent-local'
`process-buffer'
`verify-visited-file-modtime'
`window-buffer'\n
;; :BUFFER-FORMAT-AND-CODING
`format-encode-buffer'
`format-decode-buffer'
`buffer-file-format'              ;<VARIABLE>
`revert-buffer-with-coding-system'\n
;; :BUFFER-HOOKS
`buffer-save-hook'
`kill-buffer-hook'
`temp-buffer-setup-hook'
`temp-buffer-show-hook'\n
;; :BUFFER-KILLING
`kill-buffer'
`kill-buffer-and-window'
`kill-matching-buffers'
`kill-some-buffers'
`kill-this-buffer'
`kill-buffer-if-not-modified'\n
;; :BUFFFER-POSITIONS
`position-bytes'
`byte-to-position'
`mark-whole-buffer'
`beginning-of-buffer'
`beginning-of-buffer-other-window'
`end-of-buffer'
`end-of-buffer-other-window'
`eobp'
`buffer-end'
`point-min'
`point-max'
`point-max-marker'
`point-min-marker'\n
;; :BUFFER-TEMPORARY-FUNCTIONS
`completion-list-mode-finish'
`temp-buffer-resize-mode'
`resize-temp-buffer-window'
`with-output-to-temp-buffer'
`with-temp-buffer'                   : :NOTE Output to generated buffer \" *temp*\"\n
;; :BUFFER-TEMPORARY-VARIABLES
`temp-buffer-max-height'
`temp-buffer-resize-mode'
`temp-buffer-show-function'
`temp-buffer-setup-hook'
`temp-buffer-show-hook'\n
;; :BUFFER-PREDICATES
`buffer-live-p'
`buffer-modified-p'
`frame-or-buffer-changed-p'
`local-variable-p'         :SEE info node `(elisp)Creating Buffer-Local'
`local-variable-if-set-p'
`restore-buffer-modified-p'
`set-buffer-modified-p'\n
;; :BUFFER-WINDOWS
`display-buffer'
`get-buffer-window-list'
`kill-buffer-and-window'
`switch-to-buffer-other-window'
`window-buffer-height'
`window--display-buffer-1'
`window--display-buffer-2'\n
;; :BUFFER-FUNCTIONS-EXTERNAL
`bookmark-buffer-file-name'
`bookmark-buffer-name'
`with-buffer-modified-unmodified'    ;<MACRO>\n
;; :BUFFER-FUNCTIONS-MON-LOCAL
`mon-print-buffer-object-readably'
`mon-buffer-narrowed-p'
`mon-buffer-sub-no-prop'
`mon-buffer-sub-no-prop-check'
`mon-get-buffer-window-if'
`mon-get-buffer-hidden'
`mon-buffer-exists-so-kill'
`mon-buffer-exists-p'
`mon-get-buffer-w-mode'
`mon-with-file-buffer'
`mon-print-in-buffer-if-p'\n
;; :BUFFER-VARIABLES
`display-buffer-mark-dedicated'
`buffer-access-fontified-property'
`buffer-access-fontify-functions'
`buffer-auto-save-file-format'
`buffer-auto-save-file-name'
`buffer-backed-up'
`buffer-display-count'
`buffer-display-table'
`buffer-display-time'
`buffer-file-coding-system'
`buffer-file-coding-system-explicit'
`buffer-file-format'
`buffer-file-name'                    ; :NOTE Also a <FUNCTION>
`buffer-file-number'
`buffer-file-numbers-unique'
`buffer-file-read-only'
`buffer-file-truename'
`buffer-file-type'
`buffer-invisibility-spec'
`buffer-name-history'
`buffer-offer-save'
`buffer-quit-function'
`buffer-read-only'
`buffer-save-without-query'
`buffer-saved-size'
`buffer-substring-filters'
`buffer-stale-function'
`buffer-undo-list'
`clone-buffer-hook'
`indicate-buffer-boundaries'
`indicate-empty-lines'
`confirm-nonexistent-file-or-buffer'  ; :NOTE Also a <FUNCTION>
`read-buffer-function'
`save-some-buffers-action-alist'
`same-window-buffer-names'
`selective-display'
`selective-display-ellipses'
`special-display-function'
`special-display-buffer-names'
`temp-buffer-max-height'
`temp-buffer-resize-mode'
`temp-buffer-show-function'\n
:SEE info node `(elisp)Buffers and Windows'.\n
:SEE-ALSO `mon-help-window-functions', `mon-help-frame-functions',
`mon-help-file-dir-functions', `mon-help-hooks',
`mon-help-process-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-buffer-functions :insertp t)
    (mon-help-message-intrp "mon-help-buffer-functions")))
;;
;;; :TEST-ME (mon-help-buffer-functions)
;;; :TEST-ME (mon-help-buffer-functions t)
;;; :TEST-ME (describe-function 'mon-help-buffer-functions)
;;; :TEST-ME (apply 'mon-help-buffer-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2090 <Timestamp: #{2010-08-30T14:18:38-04:00Z}#{10351} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-16T15:07:43-04:00Z}#{09425} - by MON>
;;;###autoload
(defun mon-help-frame-functions (&optional insertp intrp)
  "Functions for working with frames.\n
;; :FRAME-FILES
`find-file-other-frame'
`find-file-read-only-other-frame'\n
;; :FRAME-BUFFERS
`display-buffer-other-frame'
`switch-to-buffer-other-frame'
`view-buffer-other-frame'\n
;; :FRAME-FACES
`frame-set-background-mode'
`frame-face-alist'
`frame-update-face-colors'
`frame-update-faces'\n
;; :FRAME-WINDOWS
`frame-first-window'
`frame-root-window'
`frame-selected-window'
`next-multiframe-window'
`previous-multiframe-window'
`set-frame-selected-window'
`window--frame-usable-p'
`window-frame'\n
;; :FRAME-INSPECT-FRAMES
`filtered-frame-list'
`frame-list'
`frames-on-display-list'
`make-frame-names-alist'
`minibuffer-frame-list'
`selected-frame'
`tool-bar-lines-needed'
`visible-frame-list'\n
;; :FRAME-HANDLER-GET-SELECT
`trace-redisplay'
`trace-to-stderr'
`dump-tool-bar-row'
`dump-frame-glyph-matrix'
`with-selected-frame'
`get-other-frame'
`next-frame'
`other-frame'
`select-frame'
`select-frame-by-name'
`select-frame-set-input-focus'\n
;; :FRAME-HANDLER-CREATE-DESTROY
`delete-frame'
`delete-other-frames'
`frame-initialize'
`make-frame'
`make-frame-command'
`make-frame-on-display'
`make-frame-visible'
`make-initial-minibuffer-frame'\n
;; :FRAME-LOWER-RAISE-ICONIFY
`auto-lower-mode'
`auto-raise-mode'
`iconify-frame'
`iconify-or-deiconify-frame'
`lower-frame'
`raise-frame'
`suspend-frame'\n
;; :FRAME-PREDICATES
`filtered-frame-list'
`frame-configuration-p'
`frame-live-p'
`frame-or-buffer-changed-p'
`frame-visible-p'
`framep'
`framep-on-display'
`window--frame-usable-p'\n
;; :FRAME-HANDLERS-SERVER
`frame-focus'
`frame-terminal'
`get-device-terminal'
`handle-delete-frame'
`handle-switch-frame'
`modify-all-frames-parameters'
`modify-frame-parameters'
`redirect-frame-focus'
`redraw-frame'
`selected-terminal'
`server-create-tty-frame'
`server-create-window-system-frame'
`server-handle-delete-frame'
`server-raise-frame'\n
;; FRAME-DISPLAY
`close-display-connection'
`display-mouse-p'
`display-popup-menus-p'
`display-graphic-p'
`display-images-p'
`display-selections-p'
`display-screens'
`display-pixel-height'
`display-pixel-width'
`display-mm-height'
`display-mm-width'
`display-backing-store'
`display-save-under'
`display-planes'
`display-color-cells'
`display-visual-class'
`x-get-resource'\n
;; :FRAME-CONFIGURATIONS
`frame-configuration-to-register'
`window-configuration-frame'
`set-frame-configuration'
`current-frame-configuration'\n
;; :FRAME-PARAMETER-INSPECT
`frame-char-width'
`frame-current-scroll-bars'
`frame-geom-spec-cons'
`frame-geom-value-cons'
`frame-height'
`frame-notice-user-settings'    ; :NOTE Also a <VARIABLE>
`frame-parameter'
`frame-parameters'
`frame-pixel-height'
`frame-pixel-width'
`frame-remove-geometry-params'
`frame-width'\n
;; :FRAME-PARAMETER-SETTERS
`modify-frame-parameters'
`set-frame-name'
`set-frame-font'
`set-frame-parameter'
`set-background-color'
`set-foreground-color'
`set-cursor-color'
`set-mouse-color'
`set-border-color'
`set-frame-width'
`set-frame-height'\n
;; FRAME-PARAMAMETER-PARAMETERS
`alpha`
`auto-lower`
`auto-raise`
`background-color`
`background-mode`
`border-color`
`border-width`
`buffer-list`        ; :NOTE \(assq 'buffer-list \(frame-parameters \(selected-frame\)\)\)
`buffer-predicate`   ; :NOTE \(frame-parameter 'buried-buffer-list \(selected-frame\)\)
`buried-buffer-list`
`cursor-color`
`cursor-type`
`display-type`
`display`
`environment`
`explicit-name`
`font-backend`
`font-parameter`
`font`
`foreground-color`
`fullscreen`
`height`
`horizontal-scroll-bars`
`icon-name`
`icon-type`
`internal-border-width`
`left-fringe`
`left`
`line-spacing`
`menu-bar-lines`
`minibuffer`
`modeline`
`mouse-color`
`name`
`outer-window-id`
`parent-id`
`right-fringe`
`screen-gamma`
`scroll-bar-background`
`scroll-bar-foreground`
`scroll-bar-width`
`sticky`
`title`
`tool-bar-lines`
`top`
`unsplittable`
`vertical-scroll-bars`
`visibility`
`wait-for-wm`
`width`
`window-id`
`window-system`\n
;; :FRAME-HOOKS
`after-setting-font-hook'
`before-make-frame-hook'
`before-make-frame-hook'
`delete-frame-hook'\n
;; :FRAME-VARIABLES
`after-make-frame-functions'
`delete-frame-functions'
`frame-alpha-lower-limit'
`frame-background-mode'
`frame-creation-function-alist'
`frame-inherited-parameters'
`frame-initial-frame'
`frame-initial-frame-alist'
`frame-initial-geometry-arguments'
`frame-name-history'
`frame-notice-user-settings'       ; :NOTE also a <FUNCTION>
`frame-title-format'
`inhibit-frame-set-background-mode'
`initial-frame-alist'
`initial-frame-alist'
`last-event-frame'
`minibuffer-frame-alist'
`pop-up-frames'
`pop-up-frame-alist'
`pop-up-frame-function'
`special-display-frame-alist'
`special-display-popup-frame'
`window-system-default-frame-alist'
`x-display-name'\n
;; :FRAME-FUNCTIONS-MON-LOCAL
`mon-dired-find-file-other-frame'
`mon-frame-live-visible-graphic-p'\n
:SEE info node `(elisp)Frames'.\n
:SEE :FILE lisp/frame.el src/frame.c src/dispnew.c\n
:SEE-ALSO `mon-help-window-functions', `mon-help-buffer-functions',
`mon-help-emacs-introspect'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-frame-functions :insertp t)
    (mon-help-message-intrp "mon-help-frame-functions")))
;;
;;; :TEST-ME (mon-help-frame-functions)
;;; :TEST-ME (mon-help-frame-functions t)
;;; :TEST-ME (describe-function 'mon-help-frame-functions)
;;; :TEST-ME (apply 'mon-help-frame-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-21T12:41:44-04:00Z}#{09433} - by MON>
;;;###autoload
(defun mon-help-window-functions (&optional insertp intrp)
  "Functions for working with windows.\n
;; :WINDOW-BUFFERS
`display-buffer'
`display-buffer-other-frame'
`get-buffer-window'
`get-buffer-window-list'
`kill-buffer-and-window'
`replace-buffer-in-windows'
`set-window-buffer'
`switch-to-buffer-other-window'
`window-buffer'
`window-buffer-height'
`window--display-buffer-1'
`window--display-buffer-2'\n
;; :WINDOW-DEL-KILL-QUIT
`delete-window'
`delete-windows-on'
`delete-other-windows-vertically'
`kill-buffer-and-window'
`quit-window'\n
;; :WINDOW-GETTERS
`count-windows'
`get-buffer-window'
`get-buffer-window-list'
`get-largest-window'
`get-lru-window'
`get-window-with-predicate'
`minibuffer-window'
`next-window'
`previous-window'
`selected-window'
`walk-windows'
`window-list'
`window-tree'\n
;; :WINDOW-SETTERS
`set-window-buffer'
`set-window-dedicated-p'\n
;; :WINDOW-CONFIGURATION
`compare-window-configurations'
`current-window-configuration'
`window-configuration-frame'
`window-configuration-to-register'\n
;; :WINDOW-INSPECT
`window-buffer'
`window-current-scroll-bars'
`window-display-table'
`window-frame'
`window-system'                    ; :NOTE Also a <VARIABLE>
`window-parameter'
`window-parameters'
`window-redisplay-end-trigger'     ;<DEPRECATED>\n
;; :WINDOW-HANDLER-MOVEMENT
`save-selected-window'
`save-window-excursion'
`with-selected-window'
`handle-select-window'\n
;; :WINDOW-HANDLER-MOVEMENT-TO
`display-buffer'
`display-buffer-other-frame'
`find-file-other-window'
`move-to-window-line-top-bottom'
`switch-to-buffer-other-window'
`window--display-buffer-1'
`window--display-buffer-2'
`x-popup-menu'                  :SEE :FILE src/menu.c\n
;; :WINDOW-HANDLER-MOVEMENT-OF
`adjust-window-trailing-edge'
`balance-windows'
`balance-windows-area'
`enlarge-window'
`enlarge-window-horizontally'
`fit-window-to-buffer'
`recenter-top-bottom'
`shrink-window-horizontally'
`shrink-window-if-larger-than-buffer'
`split-window-sensibly'
`split-window-horizontally'
`split-window-vertically'
`window-hscroll'
`window-vscroll'
`window--try-to-split-window'
`window--even-window-heights'\n
;; :WINDOW-POSITION-IN
`posn-window'
`pos-visible-in-window-p'
`window-point'
`window-dot'                ;<DEPRECATED>\n
;; :WINDOW-POSITION-OF
`window-at'
`window-start'
`window-end'\n
;; :WINDOW-PREDICATES
`same-window-p'
`get-window-with-predicate'
`minibuffer-window-active-p'
`one-window-p'
`pos-visible-in-window-p'
`set-window-dedicated-p'
`special-display-p'
`truncated-partial-width-window-p'
`window--frame-usable-p'
`window-configuration-p'
`window-dedicated-p'
`window-fixed-size-p'
`window-full-width-p'
`window-live-p'
`window-minibuffer-p'
`window-safely-shrinkable-p'
`window-splittable-p'
`windowp'\n
;; :WINDOW-SIZE-PIXEL
`window-pixel-edges'        ; :RETURN <LIST> => (LEFT TOP RIGHT BOTTOM)
`window-inside-pixel-edges' ; :RETURN <LIST> => (LEFT TOP RIGHT BOTTOM)
`window-scroll-bars'        ; :RETURN <LIST> => (WIDTH COLS V-TYPE H-TYPE)
`window-line-height'        ; :RETURN <LIST> => (HEIGHT V-POS Y-POS OFFBOT)
`window-fringes'            ; :RETURN <LIST> => (LEFT-WIDTH RIGHT-WIDTH OUTSIDE-MARGINS)
`window-margins'            ; :RETURN <CONS> => (LEFT-WIDTH . RIGHT-WIDTH)\n
;; :WINDOW-SIZE-LINE-COLUMN
`count-screen-lines'
`set-window-text-height'
`window-buffer-height'
`window-edges'              ; :RETURN <LIST> => (LEFT TOP RIGHT BOTTOM)
`window-inside-edges'       ; :RETURN <LIST> => (LEFT TOP RIGHT BOTTOM)
`window-width'              ; :RETURN <COL(S)>
`window-height'             ; :RETURN <LINE(S)>
`window-text-height'        ; :RETURN <LINE(S)>
`window-body-height'        ; :RETURN <LINE(S)>\n
;; :WINDOW-VARIABLES
`special-display-buffer-names'
`display-buffer-mark-dedicated'
`even-window-heights'
`pop-up-windows'
`recenter-last-op'
`same-window-buffer-names'
`same-window-regexps'
`split-window-keep-point'
`split-window-preferred-function'
`split-height-threshold'
`split-width-threshold'
`window-area-factor'
`window-min-height'
`window-min-width'
`window-point-insertion-type'
`window-scroll-functions'
`window-size-change-functions'
`window-size-fixed'
`window-system'                      ; :NOTE Also a <FUNCTION>
`window-system-initialization-alist'
`window-system-version'
`window-text-change-functions'\n
;; :WINDOW-VARIABLES-HOOKS
`window-configuration-change-hook' ;<VARIABLE>
`window-setup-hook'                ;<VARIABLE>\n
;; :WINDOW-FUNCTIONS-MON-LOCAL
`mon-map-windows->plist'`n
:SEE info node `(elisp)Windows'.\n
:SEE :FILE lisp/window.el src/window.c\n
:SEE-ALSO `mon-help-frame-functions', `mon-help-buffer-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-window-functions :insertp t)
    (mon-help-message-intrp "mon-help-window-functions")))
;;
;;; :TEST-ME (mon-help-window-functions)
;;; :TEST-ME (mon-help-window-functions t)
;;; :TEST-ME (describe-function 'mon-help-window-functions)
;;; :TEST-ME (apply 'mon-help-window-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-06T15:35:52-04:00Z}#{10315} - by MON>
;;;###autoload
(defun mon-help-mode-functions (&optional insertp intrp)
  "A list regarding definition and access of major/minor modes \(non-exhausitve\).\n
;; :MODE-FUNCTIONS
`add-minor-mode'
`define-compilation-mode'
`define-derived-mode'
`define-generic-mode'
`define-globalized-minor-mode'
`define-minor-mode'
`describe-mode'
`generic-mode'
`generic-mode-internal'
`hack-local-variables'
`indent-line-function'
`kill-all-local-variables'
`make-local-variable'
`make-variable-buffer-local'
`modify-syntax-entry'
`set-auto-mode'
`set-buffer-major-mode'
`use-local-map'\n
;; :MODE-BASE-MAJOR-MODES
`normal-mode'
`fundamental-mode'\n
;; :MODE-MODE-LINE
`force-mode-line-update'
`format-mode-line'
`default-mode-line-format'
`global-mode-string'
`header-line-format'
`mode-line-modified'
`mode-line-format'
`mode-line-buffer-identification'
`mode-line-position'\n
;; :MODE-VARIABLES
`auto-mode-alist'
`auto-mode-interpreter-regexp'
`default-major-mode'
`enable-local-variables'
`inhibit-first-line-modes-regexps'
`inhibit-first-line-modes-suffixes'
`initial-major-mode'
`interpreter-mode-alist'
`local-enable-local-variables'
`magic-mode-alist'
`magic-fallback-mode-alist'
`minor-mode-map-alist'
`mode-name'
`mode-specific-map'
`standard-syntax-table'\n
;; :MODE-HOOKS
`after-change-major-mode-hook'
`before-hack-local-variables-hook'
`change-major-mode-hook'
`delay-mode-hooks'
`delayed-mode-hooks'
`hack-local-variables-hook'
`run-mode-hooks'\n
;; :MODE-PLIST-PROPERTIES
`:minor-mode-function`
`definition-name`
`mode-class`
`derived-mode-unmerged`\n
;; :MODE-MON-LOCAL
`mon-get-buffer-w-mode'\n
:SEE info node `(elisp)Modes'\n
:SEE-ALSO `mon-help-file-dir-functions', `mon-help-buffer-functions',
`mon-help-hooks', `mon-help-syntax-functions', `mon-help-faces',
`mon-help-faces-basic', `mon-help-faces-font-lock', `mon-help-font-lock',
`mon-help-key-functions', `mon-help-easy-menu', `mon-help-custom-keywords'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-mode-functions :insertp t)
    (mon-help-message-intrp "mon-help-mode-functions")))
;;
;;; :TEST-ME (mon-help-mode-functions)
;;; :TEST-ME (mon-help-mode-functions t)
;;; :TEST-ME (apply 'mon-help-mode-functions '(t))

 
;;; ==============================
;;; :TODO add other XML related material from xml-rpc, and ./lisp/nxml
;;; :CREATED <Timestamp: #{2009-09-17T12:06:53-04:00Z}#{09384} - by MON KEY>
;;;###autoload
(defun mon-help-xml-functions (&optional insertp intrp)
  "XML related functions.\n
;; :XML-FILE.lisp.xml
`xml-get-children'
`xml-parse-attlist'
`xml-parse-dtd'
`xml-parse-elem-type'
`xml-parse-file'
`xml-parse-fragment'
`xml-parse-region'
`xml-parse-string'
`xml-parse-tag'
`xml-print'
`xml-debug-print'
`xml-escape-string'\n
;; :XML-FILE.nxml.xsd-regexp
`xsdre-translate'
`xsdre-parse-regexp'\n
;; :XML-FILE.nxml.xmltok
`xmltok-unicode-to-char'
`xmltok-forward'
`xmltok-forward-prolog'
`xmltok-forward-special'\n
;; :XML-FILE.nxml.nxml-parse
`nxml-parse-file'\n
;; :XML-FILE.eieio-xml
`eieio-xml-override-prin1'
`eieio-xml-list-prin1'
`object-write-xml'\n
:SEE-FILE nxml/xsd-regexp.el nxml/xmltok.el nxml/nxml-parse.el
:SEE-FILE lisp/xml.el lisp/json.el\n
:SEE-ALSO `mon-help-css-mode', `mon-help-css-color', `mon-help-css-complete',
`mon-help-css-check', `mon-help-ebay-template-mode', `mon-help-tidy'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-xml-functions :insertp t)
    (mon-help-message-intrp "mon-help-xml-functions")))
;;
;;; :TEST-ME (mon-help-xml-functions)
;;; :TEST-ME (mon-help-xml-functions t)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-04T06:24:23-04:00Z}#{09407} - by MON>
;;;###autoload
(defun mon-help-eieio-defclass (&optional insertp intrp)
  "Routine eieio functions, specs, args, etc.
:SEE info node `(eieio)Top'
:SEE :FILE ./cedet-cvs/ede/ede.el for examples of defining big classes.\n
;; :EIEIO-CLASS-SLOT-KEYWORDS
:initarg                 {tag, string}
:initform                {expression}
:type                    {t, null, symbol, list, function, string, character,
                          integer, fixnum, number, real, float, boolean}
                         :SEE `typep'`type-of' `deftype' `typecase' `check-type'
                         :SEE info node `(cl)Type Predicates'
                         :SEE info node `(elisp)Type Predicates'
:allocation              {:instance, :class}
:documentation           {string}\n
;; :EIEIO-CLOS-NON-COMPLIANT
:accessor                {generic-function-name}
:writer                  {generic-function-name}
:reader                  {generic-function-name}\n
;; :EIEIO-EMACS-SPECIFIC
:custom                  {string}
:label                   {string}
:group                   {customization-group}
:custom-groups           {list}
:printer                 {function}
:protection              {:public, :protected, :private}
:allow-nil-initform      {boolean}
:abstarct                {boolean}
:method-invocation-order {:breadth-first, :depth-first}\n
;; :EIEIO-CLOS-NON-IMPLIMENTED
:metaclass
:default-initargs\n
;; Additional class tags are added with: `class-option'\n
;; :EIEIO-BASE-CLASSES
`eieio-persistent'         (file file-header-line) <CLASS>
`eieio-instance-inheritor' (parent-instance)       <CLASS>
`eieio-instance-tracker'   (tracker-symbol)        <CLASS>
`eieio-speedbar'           (buttontype buttonface) <CLASS>
`eieio-singleton'                                  <CLASS>
`eieio-named'                                      <CLASS>\n
:SEE-ALSO `mon-insert-defclass-template', `mon-help-eieio-functions',
`mon-help-eieio-methods', `mon-help-custom-keywords', `mon-help-faces-themes',
`mon-help-widgets'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-eieio-defclass :insertp t)
    (mon-help-message-intrp "mon-help-eieio-defclass")))
;;
;;; :TEST-ME (mon-help-eieio-defclass)
;;; :TEST-ME (mon-help-eieio-defclass t)
;;; :TEST-ME (describe-function 'mon-help-eieio-defclass)
;;; :TEST-ME (apply 'mon-help-eieio-defclass '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-06T16:41:17-04:00Z}#{09412} - by MON KEY>
;;;###autoload
(defun mon-help-eieio-functions (&optional insertp intrp)
  "List of interface functions for work with EIEIO system of CEDET.\n
:SEE info node `(eieio)Function Index'.\n
:NOTE Below right-hand column elts are a symbol signature unless prefixed by `;<'.\n
;; :EIEIO-CLASSES               :SEE `mon-help-eieio-defclass'
`defclass'                       NAME SUPERCLASS SLOTS &rest OPTIONS-AND-DOC
`eieio-hook'                    ;<VARIABLE>\n
;; :EIEIO-MAKING-OBJECTS
`make-instance'                  CLASS &rest INITARGS
`class-constructor'              OBJECT-NAME &rest SLOTS
`initialize-instance'            OBJ &rest SLOTS
`shared-initialize'              OBJ &rest SLOTS\n
;; :EIEIO-METHODS                :SEE `mon-help-eieio-methods'
`defgeneric'                     METHOD ARGLIST [DOC-STRING]
`call-next-method'               &rest REPLACEMENT-ARGS
`defmethod'                      METHOD [:before|:primary|:after|:static]\n
;; :EIEIO-BASIC-METHODS
`clone'                          OBJ &rest PARAMS
`constructor'                    ;<GENERIC-FUNCTION :STATIC>
`object-print'                   THIS &rest STRINGS
`object-write'                   OBJ &optional COMMENT
`slot-missing'                   AB &rest FOO
`slot-unbound'                   OBJECT CLASS SLOT-NAME FN
`no-applicable-method'           OBJECT METHOD &rest ARGS
`no-next-method'                 OBJECT &rest ARGS\n
`eieio-generic-call-arglst'        ;<VARIABLE>
`eieio-pre-method-execution-hooks' ;<VARIABLE>\n
;; :EIEIO-ACCESSING-SLOTS
`oset'                           OBJECT SLOT VALUE ;-> `eieio-oset'
`slot-value'                     OBJECT SLOT       ;-> `oref' -> `eieio-oref'
`set-slot-value'                 OBJECT SLOT VALUE ;-> `eieio-oset'
`oset-default'                   CLASS SLOT VALUE  ;-> `eieio-oset-default'
`oref-default'                   OBJ SLOT          ;-> `eieio-oref-default'
`slot-makeunbound'               OBJECT SLOT       ;-> is `slot-makunbound'
`with-slots'                     SPEC-LIST OBJECT &rest BODY
`object-add-to-list'             OBJECT SLOT ITEM &optional APPEND
`object-remove-from-list'        OBJECT SLOT ITEM\n
;; :EIEIO-ASSOCIATION-LISTS
`object-assoc'                   KEY SLOT LIST
`object-assoc-list'              SLOT LIST
`eieio-build-class-alist'        &optional BASE-CLASS\n
;; :EIEIO-PREDICATES
`child-of-class-p'               CHILD CLASS
`class-abstract-p'               CLASS
`class-p'                        CLASS
`eieio-slot-originating-class-p' START-CLASS SLOT
`generic-p'                      METHOD-SYMBOL
`generic-primary-only-p'         METHOD
`generic-primary-only-one-p'     METHOD
`next-method-p'
`object-of-class-p'              OBJ CLASS
`object-p'                       OBJ    ;-> `eieio-object-p'
`same-class-fast-p'              OBJ CLASS
`same-class-p'                   OBJ CLASS
`slot-boundp'                    OBJECT SLOT
`slot-exists-p'                  OBJECT-OR-CLASS SLOT\n
;; :EIEIO-UTILITY
`class-v'                        CLASS
`class-constructor'              CLASS
`class-direct-superclasses'      CLASS  ;-> `class-parents'
`class-direct-subclasses'        CLASS  ;-> `class-children'
`class-children-fast'            CLASS
`class-name'                     CLASS
`class-method-invocation-order'  CLSSS
`class-option'                   CLASS OPTION
`class-option-assoc'             LIST OPTION
`class-parents-fast'             CLASS
`class-parent'                   CLASS  ;<DEPRECATED>
`class-slot-initarg'             CLASS SLOT
`eieio-set-defaults'             OBJ &optional SET-ALL
`eieio-initarg-to-attribute'     CLASS INITARG
`find-class'                     SYMBOL &optional ERRORP
`object-class'                   OBJ    ;->`class-of'
`object-class-fast'              OBJ
`object-class-name'              OBJ
`object-name'                    OBJ &optional EXTRA
`object-slots'                   OBJ
`object-name-string'             OBJ
`object-set-name-string'         OBJ NAME\n
;; :EIEIO-INTROSPECTION
`describe-class'                 CLASS   ;-> `eieio-describe-class'
`describe-generic'               GENERIC ;-> `eieio-describe-generic'
`describe-method'                GENERIC ;-> `eieio-describe-generic'
`eieiodoc-class'                 CLASS INDEXSTRING &optional SKIPLIST
`eieio-all-generic-functions'    &optional CLASS
`eieio-browse'                   ROOT-CLASS
`eieio-class-tree'               &optional ROOT-CLAS
`eieio-class-slot-name-index'    CLASS SLOT
`eieio-slot-name-index'          CLASS OBJ SLOT
`eieio-default-superclass'       ;<VARIABLE> ;<- :ALIASED-BY `standard-class'
`eieio-describe-class-slots'     CLASS
`eieio-describe-constructor'     FCN
`eieio-lambda-arglist'           FUNC
`eieio-method-documentation'     GENERIC CLASS
`eieio-version'                  ;<VARIABLE> & <FUNCTION>\n
;; :EIEIO-SIGNALS
`invalid-slot-name'              OBJ-OR-CLASS SLOT
`no-method-definition'           METHOD ARGUMENTS
`no-next-method'                 CLASS ARGUMENTS
`invalid-slot-type'              SLOT SPEC VALUE
`unbound-slot'                   OBJECT CLASS SLOT\n
;; :EIEIO-PRINTERS
`object-print'                   THIS &rest STRINGS    ;<METHOD>
`object-write'                   OBJ &optional COMMENT ;<METHOD>
`eieio-override-prin1'           THING
`eieio-list-prin1'
`eieio-xml-override-prin1'       THING
`eieio-xml-list-prin1'           LIST
`eieio-edebug-prin1-to-string'   OBJECT &optional NOESCAPE
`eieio-display-method-list'
`object-write-xml'               ;<METHOD>\n
;; :EIEIO-ADVISED-FUNCTIONS
`eieio-describe-class' -> `describe-variable'
`eieio-describe-generic' -> `describe-function'\n
;; :EIEIO-CLOS-UNIMPLEMENTED    :SEE info node `(eieio)CLOS compatibility'
`change-class'
`describe-object'               :SEE `object-write'\n
;; :EIEIO-UNINMPLEMENTED
`destructor'
`eieio-read-xml'\n
;; :EIEIO-LOAD
`eieio-defclass-autoload`       ;<PROPERTY>\n
:SEE-ALSO `mon-help-eieio-methods', `mon-help-eieio-defclass',
`mon-insert-defclass-template', `mon-help-custom-keywords',
`mon-help-faces-themes', `mon-help-widgets'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-eieio-functions :insertp t)
    (mon-help-message-intrp "mon-help-eieio-functions")))
;;
;;; :TEST-ME (mon-help-eieio-functions)
;;; :TEST-ME (mon-help-eieio-functions t)
;;; :TEST-ME (describe-function 'mon-help-eieio-functions)
;;; :TEST-ME (apply 'mon-help-eieio-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-08T21:20:46-04:00Z}#{09415} - by MON>
;;;###autoload
(defun mon-help-eieio-methods (&optional insertp intrp)
  "Help interrogating eieio's generic functions and methods.\n
Following presents examples for examining the underlying `vector' and `obarray'
structures. To run throught the examples evaluate below:\n
 ===================\n :CREATE-TWO-CLASSES\n ===================\n
\(defclass tt--367 \(\)\n  \(\(s-367-0\n    :initarg  :s-367-0
    :initform nil\n    :accessor acc-s367-0\)\n   \(s-367-1
    :initarg  :s-367-1\n    :initform nil
    :documentation \"doc s-367-1\"\)\)\n  \"Dummy class tt--367\"\)\n
\(defclass tt--367-sub \(tt--367\)\n  \(\(s-367-sub-0
    :initarg  :s-367-sub-0\n    :initform nil
    :accessor acc-s367-sub-0\)\n   \(s-367-sub-1\n    :initarg  :s-367-sub-1
    :initform nil\n    :type list
    :documentation \"s-367-sub-1 w/ type 'list.\"\)\)
  \"Dummy class tt--367-sub\"\)\n\n ==========================
 :INSTANTIATE-OBJECTS-SLOTS\n ==========================\n
\(setq test-tt--367
      \(tt--367 \"test-tt--367\" :s-367-1 '\(a list on second slot s-367-1\)\)\)\n
\(setf \(acc-s367-0 test-tt--367\) \"slot-value on s-367\"\)\n
\(acc-s367-0 test-tt--367\)\n;=> \"slot-value on s-367\"\n
\(slot-value test-tt--367 :s-367-0\)\n;=> \"slot-value on s-367\"\n
\(setq test-tt--367-sub \(tt--367-sub \"test-tt--367-sub\"\)\)
test-tt--367-sub\n;=> [object tt--367-sub \"test-tt--367-sub\" nil nil nil]\n
\(set-slot-value test-tt--367-sub :s-367-sub-0 [vec on vec]\)
\(acc-s367-sub-0 test-tt--367-sub\)\n;=> [vec on vec]\n
\(setf \(slot-value test-tt--367-sub :s-367-sub-1\) \"This list should fail\"\) \n
\(setf \(slot-value test-tt--367-sub :s-367-sub-1\) '\(this-list should pass\)\) \n
\(slot-value test-tt--367-sub :s-367-sub-1\)\n;=> \(this-list should pass\)\n
 ==============\n :CLASS-VECTORS\n ==============\n
By default an eieio class is instantiated as a vector.
To access this vector use `class-v':\n
:IDIOM \(class-v '<SOME-CLASS>\)\n\n:EXAMPLE\n\(class-v 'tt--367\)\n
:NOTE Class vectors are intialized with respect to `eieio-default-superclass' when
defined without a parent class.\n:SEE info node `(eieio)Default Superclass'.\n
The init value of eieio-default-superclass is hardwired at eieio build time as
a vector of 26 elements. It is bootstrapped from the values of 26 constants.
The 26th of these constants `class-num-slots' sets the vector size of eieio's
default superclass and the default vector size of all classes derived thereof.\n
:EXAMPLE\n\(length \(class-v 'eieio-default-superclass\)\)
\(length \(class-v 'tt--367\)\)\n
25 other constants are also evaluated to generate eieio-default-superclass.
This happens at build time so that the default superclass has a value while it
is _itself_ being built.\n
Because present eieio sytems derive all other classes from eieio's default
superclass current standard eieio systems allow direct access to the individual
elements of a class using the values of those same constants defined to build
eieio's default superclass.\n
Access these `aref' elements with the macro `class-v' with expressions of the form:\n
:IDIOM \(aref \(class-v '<SOME-CLASS>\) <CONSTANT>\)\n
:EXAMPLE\n\(aref \(class-v 'tt--367\) class-symbol\)\n
Assuming the two example classes and instances above are initialized a full
class vector deconstructs as follows:\n
:EXAMPLE\n(class-v 'tt--367)\n
\[defclass               ;; 0  ;<- This determines if `class-p'
 tt--367                ;; 1  `class-symbol' ;<- This is the `class-constructor'
 nil                    ;; 2  `class-parent'
 \(tt--367-sub\)          ;; 3  `class-children'
 [0 s-367-0 s-367-1]    ;; 4  `class-symbol-obarray'
 \(s-367-0 s-367-1\)      ;; 5  `class-public-a'
 \(nil nil\)              ;; 6  `class-public-d'
 \(nil \"doc :s-367-1\"\)   ;; 7  `class-public-doc'
 [t t]                  ;; 7  `class-public-doc'
 \(nil nil\)              ;; 8  `class-public-type'
 \(nil nil\)              ;; 9  `class-public-custom'
 \(\(default\) \(default\)\)  ;; 10 `class-public-custom-label'
 \(nil nil\)              ;; 11 `class-public-custom-group'
 \(nil nil\)              ;; 12 `class-public-printer'\n
 \(\(:s-367-0 . s-367-0\)
  \(:s-367-1 . s-367-1\)\) ;; 14 `class-initarg-tuples'\n
 nil                    ;; 15 `class-class-allocation-a'
 nil                    ;; 16 `class-class-allocation-doc'
 []                     ;; 17 `class-class-allocation-type'
 nil                    ;; 18 `class-class-allocation-custom'
 nil                    ;; 19 `class-class-allocation-custom-label'
 nil                    ;; 20 `class-class-allocation-custom-group'
 nil                    ;; 21 `class-class-allocation-printer'
 nil                    ;; 22 `class-class-allocation-protection'
 []                     ;; 23 `class-class-allocation-values'\n
 [object tt--367 default-cache-object nil nil] ;; 24 `class-default-object-cache'\n
 \(:custom-groups \(default\)
  :documentation \"Dummy class tt-367\"\)]        ;; 25 `class-options'\n
 ==================\n :EIEIO-METHOD-TREE\n ==================\n
eieio stores a generic function's methods in an eieio-method-tree. This is a
kind of property on the generic's 'base' method. It has the form:\n
\(eieio-method-tree . \n                   [BEFORE PRIMARY AFTER
                    genericBEFORE genericPRIMARY genericAFTER]\)\n
Examine a generic function's method-tree with its eieio-method-tree property.\n
:IDIOM (get <METHOD> 'eieio-method-tree)\n
:EXAMPLE\n(get 'acc-s367-0 'eieio-method-tree)\n
\[nil\n nil    ;<- :BEFORE\n \(\(tt--367 lambda \(this\)
           \"Retrieves the slot `s-367-0' from an object of class `tt--367'\"
           \(if \(slot-boundp this \(quote s-367-0\)\)
               \(eieio-oref this \(quote s-367-0\)\) nil\)\)\)
        ;^- :PRIMARY
 nil    ;<- :AFTER\n nil    ;<- genericBEFORE \n nil    ;<- genericPRIMARY
 nil]   ;<- genericAFTER\n
 =====================\n :EIEIO-METHOD-OBARRAY\n =====================\n
eieio's method obarrays are stored as a property of a generic function on its
`eieio-method-obarray' property. This property is a vector which contains a list
of method bindings.\n\nA generic function's eieio-method-obarray has the form:\n
\(eieio-method-obarray . [BEFORE PRIMARY AFTER
                        genericBEFORE genericPRIMARY genericAFTER]\)\n
:IDIOM \(get <METHOD> 'eieio-method-obarray\)\n
:EXAMPLE\n\(get 'acc-s367-0 'eieio-method-obarray\)\n
\[[0 0 0 0 0 0 0 0 0 0 0]     aref 0 - all static methods.
 [0 0 0 0 0 0 0 0 0 0 0]     aref 1 - all methods classified as :before
 [0 0 0 0 0 0 0 0 tt--367    aref 2 - all methods classified as :primary
  0 0 0 0 0 0 0 0 0                   aref 2 is a vector of length 41
  0 0 0 0 0 0 0 0 0          \n  0 0 0 0 0 0 0 0 0          \n  0 0 0 0 0]
 [0 0 0 0 0 0 0 0 0 0 0]     aref 3 - all methods classified as :after
 nil                         aref 4 -   a generic classified as :before
 nil                         aref 5 -   a generic classified as :primary
 nil]                        aref 6 -   a generic classified as :after\n
Examine a particular type or group of methods with a key lookup into
the 'obarray' (a vector) of a generic function.\n
To find primary methods of a generic function get the 2nd index of it's obarray.\n
:IDIOM \(get <METHOD> 'eieio-method-tree\)\n
:EXAMPLE\n\(aref \(get 'acc-s367-sub-0 'eieio-method-obarray\) 2\)\n
\(aref \(get 'acc-s367-0 'eieio-method-obarray\) 2\)\n
A generic form can be interrogated with `eieio-generic-form':\n
:IDIOM \(eieio-generic-form <METHOD> <KEY> <CLASS>\)\n
:EXAMPLE\n\(eieio-generic-form 'acc-s367 2 tt--367-sub\)\n
\(tt--367 . tt--367\) ;key 2\n\(tt--367 . tt--367\) ;key 4
\(tt--367 . tt--367\) ;key 5\n\(tt--367 . tt--367\) ;key 6\n
:SEE-ALSO
`eieiomt-method-list'                  METHOD KEY CLASS
`eieiomt-install'                      METHOD-NAME
`eieiomt-add'                          METHOD-NAME METHOD KEY CLASS
`eieiomt-next'                         CLASS
`eieiomt-method-list'                  METHOD KEY CLASS
`eieiomt-sym-optimize'                 S
`eieio-unbind-method-implementations'  METHOD
`eieiomt-optimizing-obarray'          ;<VARIABLE>\n
 ===============================\n :METHODS-AND-GENERICS-EXAMINING
 ===============================\n
:EXAMPLE\n
\(eieio-describe-constructor 'tt--367\) ; :NOTE A `constructor' is a 'static' method.\n
\(describe-variable 'test-tt--367\)\n
\(describe-function 'acc-s367\)\n
\(eieio-describe-generic 'acc-s367\)\n
\(describe-method 'acc-s367-0\)\n
\(eieio-describe-generic 'acc-s367-sub-0\)\n
\(eieio-describe-method 'acc-s367-sub-0\)\n
\(eieio-describe-method 'acc-s367-0\)\n
\(describe-function 'acc-s367-sub-0\)\n\n
:SEE-ALSO `mon-help-eieio-defclass', `mon-help-eieio-functions',
`mon-help-custom-keywords', `mon-help-faces-themes', `mon-help-widgets'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-eieio-methods :insertp t)
    (mon-help-message-intrp "mon-help-eieio-methods")))
;;
;;; :TEST-ME (mon-help-eieio-methods)
;;; :TEST-ME (mon-help-eieio-methods t)
;;; :TEST-ME (describe-function 'mon-help-eieio-methods)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-10T21:36:01-04:00Z}#{09417} - by MON>
;;;###autoload
(defun mon-help-type-predicates (&optional insertp intrp)
  "List of predicate functions for interrogating elisp types.\n
      _______________________                                       70.
     |                       |                                        
     | :TYPE-PREDICATES-SEQS |                                        
 ____|_______________________|____________                            
|                               --------  |                           
| `sequencep'                  |`listp' ¦-¦--| `consp'                 
|              __________      |`nlistp'| |  .-+ `atom'                
|             |          |      --------  |                           
|  ___________| `arrayp' |______________  |                           
| |   _________           ___________   | |                           
| |  |         |         |           |  | |                           
| |  |`vectorp'|         | `stringp' ¦--¦-¦--| `string-or-null-p'     
| |  |_____¦___|         |___________|  | |  . `char-or-string-p'     
| |        ¦                            | |                           
| | `vector-or-char-table-p'            | |                           
| |  ______¦_______    _______________  | |                           
| | |      '       |  |               | | |                           
| | |`char-table-p'|  |`bool-vector-p'| | |                           
| | |_¦____________|  |_______________| | |  ___________________      
| |___¦_________________________________| | |                   |     
|_____¦___________________________________| | :NUMERICAL-SHOWER |     
      ¦                                  ___|___________________|_____
      |-+ `keymapp'                     |                             |
      .--+ `case-table-p'               |       `zerop'               |
      .--+ `syntax-table-p'             |          |     `booleanp'   |
      .--+ `display-table-p'            |      `numberp'              |
            ____________                |          |                  |
           |            |               |        +-¦-+                |
           | `type-of'  |               | `floatp' | | `natnump'      |
   ________|____________|___________    |          | .-+ `wholenump'  |
  |                                 |   |          | .--+ `integerp'. |
  | bool-vector  <- `bool-vector-p' |   |          | .---+ `oddp'   | |
  | buffer       <- `bufferp'       |   |          | .---+ `evenp'  | |
  | char-table   <- `char-table-p'  |   |          |                | |
  | cons         <- `consp'         |   | `plusp'+-¦-+`minusp'      | |
  | float        <- `floatp'        |   |___________________________¦_|
  | font-entity  <- `fontp'         |                               ¦ 
  | font-object  <- `fontp'         |        `number-or-marker-p' +-| 
  | font-spec    <- `fontp'         |       `integer-or-marker-p' +-. 
  | frame        <- `framep'        |                                 
  | hash-table   <- `hash-table-p'  |                                 
  | integer      <- `integerp',     |                                 
  | marker       <- `markerp'       |                                 
  | overlay      <- `overlayp'      |                                 
  | process      <- `processp'      |                                 
  | string       <- `stringp'       |                                 
  | subr         <- `subrp'         |                                 
  | symbol       <- `symbolp'       |                                 
  | vector       <- `vectorp'       |                                 
  | window       <- `windowp'       |                                 
  | compiled-function               |
  |   ^-- `byte-code-function-p'    |
  |                                 |
  |_________________________________|                               70^

;; :TYPE-PREDICATE-FUNCTIONS
`typep'
`deftype'
`etypecase'
`typecase'
`check-type'
`widget-type'
`buttontype'
`button-has-type-p'
`button-type-subtype-p'\n
;; :TYPE-PREDICATE-FUNCTION
`functionp'
`mon-function-object-p'
`apropos-macrop'
`keywordp'
`commandp'
`byte-code-function-p' 
:NOTE Interrogation of an Emacs function objects foo can be tricky because we
may need to know any one or more of the following before we can effectively make
the query:
 - Is foo a symbol;
 - Is foo bound;
 - Does the symbol foo have a value in its function cell;
 - What is the type of value in foo's function cell
   -- Is foo: 
      --- a lambda or macro
          ---- is the lambda or macro a byte compiled function
      --- an autoload
      --- a subr

 \(defun tt--foo-eg \(foo-arg\)
  \(+ foo-arg foo-arg\)\)

  \(indirect-function 'tt--foo-eg\)
  ;=> (lambda (foo-arg) (+ foo-arg foo-arg))

 \(byte-compile 'tt--foo-eg\)

 \(indirect-function 'tt--foo-eg\)
 ;=> #[\(foo-arg\) \"\x8\\211\\\\\207\" [foo-arg] 2]

 \(defun tt--foo-eg \(foo-arg\)
   \(+ foo-arg foo-arg\)\)

Testing if a function is byte compiled:\n
 \(subrp \(indirect-function 'byte-code-function-p\)\)\n
Using type:\n
\(type-of \(indirect-function 'find-file\)\)\n
:ALIASED-BY `mon-help-types'\n
:SEE info node `(elisp)Type Predicates'
:SEE info node `(elisp)Programming Types'
:SEE info node `(CL)Type Predicates'\n
:SEE-ALSO `mon-help-predicate-functions', `mon-help-symbol-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-type-predicates :insertp t)
    (mon-help-message-intrp "mon-help-type-predicates")))
;;
;;; :TEST-ME (mon-help-type-predicates)
;;: :TEST-ME (mon-help-type-predicates t)
;;; :TEST-ME (describe-function 'mon-help-type-predicates)
;;; :TEST-ME (apply 'mon-help-type-predicates '(t))

 
;;; ==============================
;;; :CHANGESET 2370
;;; :CREATED <Timestamp: #{2010-12-30T14:03:07-05:00Z}#{10524} - by MON KEY>
;;;###autoload
(defun mon-help-number-functions (&optional insertp intrp)
  "List of number related functions and variables.\n
;; :NUMBER-FUNCTIONS-ARITHEMETIC
`+'
`-'
`1+'
`1-'
`incf'
`decf'
`*'
`\\'
`%'
`gcd'
`lcm'
`mod'
`mod*'
`rem*'\n
;; :NUMBER-FUNCTIONS-COMPARE
`>'
`<'
`<='
`>='
`='
`/='
`min'
`max'
`abs'\n
;; :NUMBER-FUNCTIONS-MATH
`atan'
`acos'
`asin'
`sin'
`exp'
`expt'
`log'
`logb'
`log10'
`sqrt'
`isqrt'
`degrees-to-radians'
`radians-to-degrees'\n
;; :NUMBER-FUNCTIONS-CONVERSION
`floor'
`floor*'
`ffloor'
`ceiling'
`ceiling*'
`fceiling'
`truncate'
`truncate*'
`ftruncate'
`round'
`round*'
`fround'\n
;; :NUMBER-FUNCTIONS-BITWISE
`ash'
`lsh'
`lognot'
`signum'\n
;; :NUMBER-FUNCTIONS-OTHER
`max-char'
`length'
`list-length'
`safe-length'
`random'
`random*'
`make-random-state'
`cl-float-limits'
`string-to-number'
`number-to-string'
`number-sequence'
`read-number'\n
;; :NUMBER-PREDICATES
`eql'
`equalp'
`floatp'
`floatp-safe'
`integerp' 
`natnump'      
`wholenump'
`numberp'              
`evenp'
`oddp'
`minusp'
`plusp'
`zerop'
`booleanp'   
`characterp'
`random-state-p'
`integer-or-marker-p'
`number-or-marker-p'\n
;; :NUMBER-VARIABLES
`pi'
`e'
`degrees-to-radians'    ;; :NOTE Also a <FUNCTION>
`radians-to-degrees'    ;; :NOTE Also a <FUNCTION>
`most-positive-fixnum'
`most-negative-fixnum'
`most-positive-float'
`most-negative-float'
`least-negative-float'
`most-positive-float'
`least-positive-normalized-float'
`least-negative-normalized-float'
`float-epsilon'
`float-negative-epsilon'\n
;; :NUMBER-FUNCTIONS-MON
`mon-get-bit-table'
`mon-booleanp-to-binary'
`mon-bool-vector-pp'\n
:SEE info node `(elisp)Numbers'\n
:SEE-ALSO `mon-help-binary-representation', `mon-help-predicate-functions',
`mon-help-type-predicates', `mon-help-types'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-number-functions :insertp t)
    (mon-help-message-intrp "mon-help-number-functions")))
;;
;;; :TEST-ME (mon-help-number-functions)
;;; :TEST-ME (mon-help-number-functions t)
;;; :TEST-ME (describe-function 'mon-help-number-functions)
;;; :TEST-ME (apply 'mon-help-number-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2211
;;; :CREATED <Timestamp: #{2010-10-27T12:32:39-04:00Z}#{10433} - by MON KEY>
;;;###autoload
(defun mon-help-predicate-functions (&optional insertp intrp)
  "List of predicate like functions these are mostly unary tests.\n
These may not have an `type-of' accessor and/or might otherwise be independent
of those predicates enumerated with `mon-help-type-predicates'.\n
;; :PREDICATE-FUNCTIONS-SEQUENCE
`anot-head-p'
`arrayp'
`atom'
`bool-vector-p'
`car-less-than-car'
`char-table-p'
`consp'
`hash-table-p'
`listp'
`nested-alist-p'
`nlistp'
`null'
`sequencep'
`vector-or-char-table-p'
`vectorp'\n
;; :PREDICATE-FUNCTIONS-STRING
`compare-strings'
`string-prefix-p'
`char-or-string-p'
`stringp'
`string-prefix-p'
`string-or-null-p'\n
;; :PREDICATE-FUNCTIONS-STRING-MATCHING
`looking-at-p'
`subregexp-context-p'
`string-match-p'\n
;; :PREDICATE-FUNCTIONS-NUMBERS
`booleanp'
`evenp'
`floatp'
`floatp-safe'
`integer-or-marker-p'
`integerp'
`markerp'
`minusp'
`natnump'
`number-or-marker-p'
`numberp'
`oddp'
`plusp'
`wholenump'
`zerop'\n
;; :PREDICATE-FUNCTIONS-CHAR
`case-table-p'
`multibyte-string-p'
`lgstring-shaped-p'
`char-table-p'
`charsetp'
`char-equal'
`char-displayable-p'
`char-valid-p'
`charsetp'
`char-table-p'
`characterp'
`char-or-string-p'
`buffer-chars-modified-tick'
`syntax-table-p'
`category-table-p'\n
;; :PREDICATE-FUNCTIONS-FILE-DIRECTORY
`auto-save-file-name-p'\n
`file-name-absolute-p'
`file-ownership-preserved-p'
`file-newer-than-file-p'
`file-readable-p'
`file-regular-p'
`file-remote-p'
`file-symlink-p'
`file-writable-p'
`file-accessible-directory-p'
`file-attributes-lessp'
`file-compressed-p'
`file-directory-p'
`file-executable-p'
`file-exists-p'
`file-locked-p'
`next-read-file-uses-dialog-p'\n
;; :PREDICATE-FUNCTIONS-BUFFER
`buffer-modified-p'
`buffer-modified-p'
`bufferp'
`eobp'
`frame-or-buffer-changed-p'
`local-variable-if-set-p'
`local-variable-p'               :SEE info node `(elisp)Creating Buffer-Local'
`restore-buffer-modified-p'
`set-buffer-modified-p'\n
;; :PREDICATE-FUNCTIONS-WINDOW
`same-window-p'
`get-window-with-predicate'
`minibuffer-window-active-p'
`one-window-p'
`set-window-dedicated-p'
`special-display-p'
`truncated-partial-width-window-p'
`window--frame-usable-p'
`window-configuration-p'
`window-dedicated-p'
`window-fixed-size-p'
`window-full-width-p'
`window-live-p'
`window-minibuffer-p'
`window-safely-shrinkable-p'
`window-splittable-p'
`windowp'
`window--frame-usable-p'
`pos-visible-in-window-p'
`w32-window-exists-p'\n
;; :PREDICATE-FUNCTIONS-FRAME
`filtered-frame-list'
`frame-configuration-p'
`frame-live-p'
`frame-or-buffer-changed-p'
`frame-visible-p'
`framep'
`framep-on-display'
`window--frame-usable-p'\n
;; :PREDICATE-FUNCTIONS-FACE
`facep'
`face-nontrivial-p'
`face-differs-from-default-p'
`face-attribute-relative-p'
`face-bold-p'
`face-italic-p'
`face-underline-p'
`face-inverse-video-p'
`face-spec-match-p'
`face-attr-match-p'
`bitmap-spec-p'
`internal-lisp-face-empty-p'
`internal-lisp-face-equal-p'
`internal-lisp-face-p'\n
;; :PREDICATE-FUNCTIONS-DISPLAY
`display-table-p'
`display-grayscale-p'
`display-mouse-p'
`display-popup-menus-p'
`display-graphic-p'
`display-images-p'
`display-selections-p'\n
;; :PREDICATE-FUNCTIONS-COLOR
`color-defined-p'
`xw-color-defined-p'
`xw-display-color-p'
`tty-display-color-p'
`display-color-p'
`display-graphic-p'
`display-grayscale-p'
`x-display-grayscale-p'\n
;; :PREDICATE-FUNCTIONS-SERVER
`server-running-p' 
`server-temp-file-p'\n
;; :PREDICATE-FUNCTIONS-PROCESS
`daemonp'
`process-filter-multibyte-p'
`process-running-child-p'
`processp'\n
;; :PREDICATE-FUNCTIONS-EVENT
`waiting-for-user-input-p'\n
`called-interactively-p'
`interactive-p'
`eventp'
`input-pending-p'
`keymapp'
`timerp'
`timeout-event-p'\n
;; :PREDICATE-FUNCTIONS-TIME
`date-leap-year-p'
`time-less-p'
`timezone-leap-year-p'
`bookmark-time-to-save-p'\n
;; :PREDICATES-FUNCTIONS-EIEIO
`child-of-class-p'
`class-abstract-p'
`class-p'
`eieio-slot-originating-class-p'
`generic-p'
`generic-primary-only-p'
`generic-primary-only-one-p'
`next-method-p'
`object-of-class-p'
`object-p'
`same-class-fast-p'
`same-class-p'
`slot-boundp'
`slot-exists-p'\n
;; :PREDICATE-FUNCTIONS-SYMBOL
`apropos-macrop'
`bound-and-true-p'
`boundp'
`byte-code-function-p'
`commandp'
`custom-theme-p'
`custom-variable-p'
`default-boundp'
`fboundp'
`featurep'
`functionp'                                   ;:NOTE C-Primitive in lexbind branch.
`keywordp'
`local-variable-if-set-p'
`local-variable-p'
`null'
`risky-local-variable-p'
`safe-local-variable-p'
`subrp'
`symbolp'
`user-variable-p'
`special-variable-p'                          ;<LEXBIND-FUCTION>\n
;; :PREDICATE-FUNCTIONS-BYTE-COMPILE
`byte-optimize-featurep'
`byte-optimize-all-constp'
`byte-compile-trueconstp'
`byte-compile-warning-enabled-p'
`byte-compile-const-symbol-p'
`byte-compile-arglist-signatures-congruent-p' 
`byte-compile-cl-file-p'
`byte-compile-const-symbol-p'            ;<DEFSUBST>
`byte-compile-constp'                    ;<MACRO>
`byte-optimize-all-constp'
`byte-compile-constp'
`byte-code-function-p'\n
;; :PREDICATE-FUNCTIONS-UNSAFEP
`unsafep'
`unsafep-function'
`unsafep-variable'
`unsafep-progn' 
`unsafep-let'
`unsafep-vars'\n
;; :PREDICATE-FUNCTIONS-CL-EXPR
`cl-simple-exprs-p'
`cl-simple-expr-p'
`cl-const-expr-p'
`cl-const-exprs-p'
`cl-safe-expr-p'\n
;; :PREDICATE-FUNCTIONS-TEXT-PROPERTY
`invisible-p'
`overlayp'\n
;; :PREDICATE-FUNCTIONS-BUTTON
`button-has-type-p'
`button-type-subtype-p'\n
;; :PREDICATE-FUNCTIONS-WIDGET
`widgetp'
`widget-type'\n
;; :PREDICATE-FUNCTIONS-RING
`ring-p'
`ring-empty-p'\n
;; :PREDICATE-FUNCTIONS-USER
`custom-theme-p'
`user-variable-p'\n
;; :PREDICATE-FUNCTIONS-MON-LOCAL
`mon-booleanp'
`mon-buffer-exists-p'
`mon-buffer-narrowed-p'
`mon-buffer-written-p'
`mon-file-older-than-file-p'
`mon-frame-live-visible-graphic-p'
`mon-function-object-p'
`mon-list-proper-p'
`mon-looking-back-p'
`mon-print-in-buffer-if-p'
`mon-string-or-null-and-zerop'\n
:SEE-ALSO `mon-help-type-predicates', `mon-help-CL-sequence-predicates'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-predicate-functions :insertp t)
    (mon-help-message-intrp "mon-help-predicate-functions")))
;;
;;; :TEST-ME (mon-help-predicate-functions)
;;; :TEST-ME (mon-help-predicate-functions t)
;;; :TEST-ME (apply 'mon-help-predicate-functions '(nil t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-08T13:28:30-05:00Z}#{10101} - by MON KEY>
;;;###autoload
(defun mon-help-sequence-functions (&optional insertp intrp)
  "List of functions related to sequences list, alist, array, vector, ring, etc.\n
;; :SEQUENCE-ALIST
`assoc'
`rassoc'
`assoc-string'
`assq'
`rassq'
`assoc-default'
`copy-alist'
`assq-delete-all'
`rassq-delete-all'\n
;; :SEQUENCE-ALIST-FILE.lisp.emacs-lisp.assoc
`asort'
`aelement'
`aheadsym'
`anot-head-p'
`aput'
`adelete'
`aget'
`amake'\n
;; :SEQUENCE-ALIST-FILE.lisp.international.mule-util
`lookup-nested-alist'
`set-nested-alist'
`nested-alist-p'\n
;; :SEQUENCE-ARRAY  :NOTE char-tables are arrays :SEE `mon-help-char-functions'
`arrayp'
`aset'
`aref'
`fillarray'
`clear-string'
`substring'         :NOTE \(concat \(substring \(vconcat \"substring\"\) 3\)\)\n
;; :SEQUENCE-STRING :NOTE A string is an array so most will work w/ arrays too.
`compare-strings'
`string-equal'
`concat'
`subst-char-in-string'
`string-prefix-p'
`substring'
`string-to-vector'
`string-to-list'\n
;; :SEQUENCE-BUILD
`cons'
`copy-sequence'
`copy-tree'
`append'
`list'
`make-list'\n
;; :SEQUCENCE-DESTROY
`butlast'
`nbutlast'
`pop'
`delq'
`remq'
`remove'
`delete'
`delete-dups'\n
;; :SEQUENCE-MAP
`do'
`dolist'
`dotimes'
`mapatoms'
`mapcar'
`mapconcat'
`mapc'\n
;; :SEQUENCE-MEMBER
`memq'
`memql'
`member'
`member-ignore-case'\n
;; :SEQUENCE-MODIFY
`setcar'
`setcdr'
`push'
`nconc'
`add-to-list'\n
;; :SEQUENCE-ORDER
`add-to-ordered-list'
`car-less-than-car'
`length'
`nreverse'
`nthcdr'
`number-sequence'
`reverse'
`safe-length'
`sort'
`sort-subr'\n
;; :SEQUENCE-PLACE
`car-safe'
`cdr-safe'
`elt'
`last'
`nth'\n
;; :SEQUENCE-PREDICATE
`atom'
`car-less-than-car'
`consp'
`listp'
`nlistp'
`null'
`sequencep'\n
;; :SEQUENCE-RING
`make-ring'
`ring-p'
`ring-size'
`ring-length'
`ring-elements'
`ring-copy'
`ring-empty-p'
`ring-ref'
`ring-insert'
`ring-remove'
`ring-insert-at-beginning'\n
;; :SEQUENCE-STRING
`string<'
`string'
`string-bytes'
`string-to-vector'
`string-to-list'
`concat'\n
;; :SEQUENCE-VECTOR
`vector'
`describe-vector'
`vconcat'
`vectorp'
`string-to-vector'
`make-vector'
`vector-or-char-table-p'\n
;; :SEQUENCE-VECTOR-BOOLEAN
`make-bool-vector'
`bool-vector-p'
`string-bytes'
`clear-string'\n
;; :SEQUENCE-VECTOR-CHAR-TABLE
`char-table-extra-slot'
`char-table-p'
`char-table-parent'
`char-table-range'
`char-table-subtype'
`make-char-table'
`map-char-table'
`set-char-table-extra-slot'
`set-char-table-parent'\n
:SEE info node `(elisp)Sequences Arrays Vectors'
:SEE info node (elisp)Lists'.\n
:SEE-ALSO `mon-help-hash-functions', `mon-help-plist-functions',
`mon-help-plist-properties', `mon-help-type-predicates'.\n►►►"
;;; caar, cadr, cdar, cddr 
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-sequence-functions :insertp t)
    (mon-help-message-intrp "mon-help-sequence-functions")))
;;
;;; :TEST-ME (mon-help-sequence-functions)
;;; :TEST-ME (mon-help-sequence-functions t)
;;; :TEST-ME (describe-function 'mon-help-sequence-functions)
;;; :TEST-ME (apply 'mon-help-sequence-functions nil '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-25T16:33:59-05:00Z}#{10084} - by MON KEY>
;;;###autoload
(defun mon-help-key-functions (&optional insertp intrp)
  "List of keyboard and key related functions, variables, keymaps.\n
;; :KEY-FUNCTIONS-INSPECT
`describe-bindings'
`key-binding'
`local-key-binding'
`lookup-key'
`minor-mode-key-binding'
`where-is'
`where-is-internal'
`suggest-key-bindings'\n
;; :KEY-FUNCTIONS-EVENT-READ
`edmacro-parse-keys'
`edmacro-sanitize-for-string'
`event-basic-type'
`event-convert-list'
`event-modifiers'
`eventp'
`event-start'
`event-end'
`extended-command-history'
`kbd'
`make-command-summary'
`listify-key-sequence'
`prefix-numeric-value'
`read-char-exclusive'
`read-event'
`read-kbd-macro'
`read-key-sequence'
`read-key-sequence-vector'
`redirect-frame-focus'
`this-command-keys'
`this-command-keys-vector'
`track-mouse'
`unread-command-events'
`unread-input-method-events'
`unread-post-input-method-events'
`x-dnd-handle-drag-n-drop-event'\n
;; :KEY-FUNCTIONS-EVENT-MODIFIER
`char-resolve-modifiers'
`event-modifiers'
`event-apply-modifier'
`event-apply-control-modifier'
`event-apply-super-modifier'
`event-apply-shift-modifier'
`event-apply-alt-modifier'  
`event-apply-meta-modifier'
`event-apply-hyper-modifier'
`internal-event-symbol-parse-modifiers'
`event-symbol-elements`         ;<PROPERTY> 
                                ; :USAGE \(get \(event-basic-type \(read-event\)\)
                                           'event-symbol-elements\)\n
;; :KEY-VARIABLES-EVENT
`last-input-event'
`input-method-function'
`input-method-previous-message'
`input-method-use-echo-area'
`input-method-exit-on-first-char'
`input-method-verbose-flag'
`last-nonmenu-event'
`last-event-frame'
`last-command-event'
`special-event-map'\n
;; :KEY-FUNCTIONS-STATE
`cannot-suspend'
`global-disable-point-adjustment'
`disable-point-adjustment'
`clear-this-command-keys'
`discard-input'
`input-pending-p'
`keyboard-quit'
`open-dribble-file'
`recent-keys'
`reset-this-command-lengths'
`this-command-keys-vector'
`this-command-keys'
`this-single-command-keys'
`this-single-command-raw-keys'
`key-description'
`recursion-depth'
`single-key-description' \(mapconcat 'single-key-description \(this-command-keys\) \" \"\)\n
;; :KEY-FUNCTIONS-UNIVERSAL-ARGS
`negative-argument'
`digit-argument'
`universal-argument'
`universal-argument-other-key'
`universal-argument-minus'
`universal-argument-more'\n
;; :KEY-VARIABLES-INTERRUPT
`set-quit-char'
`set-input-interrupt-mode'
`command-error-function'
`inhibit-quit'
`throw-on-input'
`quit-flag'
`input-pending-p'
`while-no-input'           ;<MACRO>
`with-local-quit'          ;<MACRO>\n
;; :KEY-VARIABLES-STATE
`deferred-action-function'
`deferred-action-list'
`last-command'
`real-last-command'
`last-repeatable-command'
`last-prefix-arg'
`this-command'
`this-original-command'
`this-command-keys-shift-translated'
`num-input-keys'
`prefix-arg'
`current-prefix-arg'
`key-substitution-in-progress'\n
;; :KEY-VARIABLES-HOOK
`command-hook-internal'
`deferred-action-function'
`deferred-action-list'
`echo-area-clear-hook'
`pre-command-hook'
`post-command-hook'\n
;; :KEY-VARIABLES-KEYBOARD
`keyboard-type'
`extra-keyboard-modifiers'
`keyboard-coding-system'
`keyboard-translate-table'
`system-key-alist'\n
;; :KEY-VARIABLES-HELP
`show-help-function'
`help-char'
`help-event-list'
`help-form'
`prefix-help-command'\n
;; :KEY-VARIABLES-TIMING
`timer-list'
`timer-idle-list'
`timer-event-last'
`timer-event-last-1'
`timer-event-last-2'
`timer-event-handler'
`timer-duration-words'
`timer-max-repeats'
`echo-keystrokes'
`polling-period'
`double-click-time'
`double-click-fuzz'\n
;; :KEY-FUNCTIONS-BIND
`command-remapping'
`define-key'
`define-key-after'
`define-prefix-command'
`global-key-binding'
`global-set-key'
`global-unset-key'
`keypad-setup'
`local-set-key'
`local-unset-key'
`undefined'
`set-keyboard-coding-system'\n
;; :KEY-FUNCTIONS-MAP-HANDLERS
`copy-keymap'
`keyboard-translate'
`keymap-canonicalize'
`make-keymap'
`make-sparse-keymap'
`map-keymap'
`map-keymap-sorted'
`set-keymap-parent'
`substitute-command-keys'
`substitute-key-definition'      ; :NOTE Undocumented optional arg PREFIX
`substitute-key-definition-key'
`suppress-keymap'
`use-global-map'
`use-local-map'\n
;; :KEY-FUNCTIONS-MAP-INSPECT
`accessible-keymaps'
`command-remapping'
`current-active-maps'
`current-global-map'
`current-local-map'
`current-minor-mode-maps'
`keymap-parent'
`keymapp'\n
;; :KEY-VARIABLES-INPUT-MOD
`listify-key-sequence-1'          ; :NOTE \(logxor <key> (logior 128 ?\\M-\\C-@\)\)
`extra-keyboard-modifiers'
`input-decode-map'
`keyboard-translate-table'
`keyboard-type'
`keypad-numlock-setup'
`keypad-numlock-shifted-setup'
`keypad-shifted-setup'
`meta-prefix-char'
`special-event-map'
`translation-table-for-input'
`where-is-preferred-modifier'\n
;; :KEY-VARIABLES-MODE-MAPS
`emulation-mode-map-alists'
`minor-mode-map-alist'
`minor-mode-overriding-map-alist'
`overriding-local-map'
`overriding-local-map-menu-flag'
`overriding-terminal-local-map'\n
;; :KEY-KEYMAPS                   :SEE info node `(elisp)Standard Keymaps'
`easy-menu-converted-items-table'
`Buffer-menu-mode-map'
`Helper-help-map'
`Info-edit-map'
`Info-mode-map'
`apropos-mode-map'
`c-mode-map'
`command-history-map'
`ctl-x-map'
`ctl-x-4-map'
`ctl-x-5-map'
`custom-mode-map'
`debugger-mode-map'
`dired-mode-map'
`edit-abbrevs-map'
`edit-tab-stops-map'
`electric-buffer-menu-mode-map'
`electric-history-map'
`emacs-lisp-mode-map'
`esc-map'
`facemenu-background-menu'
`facemenu-face-menu'
`facemenu-foreground-menu'
`facemenu-indentation-menu'
`facemenu-justification-menu'
`facemenu-menu'
`facemenu-special-menu'
`function-key-map'
`fundamental-mode-map'
`global-map'
`grep-mode-map'
`help-map'
`help-mode-map'
`input-decode-map'
`isearch-mode-map'
`key-translation-map'
`kmacro-map'
`lisp-interaction-mode-map'
`lisp-mode-map'
`local-function-key-map'
`menu-bar-edit-menu'
`menu-bar-files-menu'
`menu-bar-help-menu'
`menu-bar-mule-menu'
`menu-bar-search-menu'
`menu-bar-tools-menu'
`mode-specific-map'
`multi-query-replace-map'
`occur-mode-map'
`printable-chars'
`query-replace-map'
`read-key-empty-map'
`search-map'
`splash-screen-keymap'
`text-mode-map'
`tool-bar-map'
`view-mode-map'\n
;; :KEY-FUNCTIONS-W32
`w32-register-hot-key'
`w32-unregister-hot-key'
`w32-registered-hot-keys'
`w32-reconstruct-hot-key'
`w32-toggle-lock-key'\n
;; :KEY-VARIABLES-W32
`w32-alt-is-meta'
`w32-pass-alt-to-system'
`w32-quit-key'
`w32-phantom-key-code'
`w32-enable-num-loc'
`w32-enable-caps-lock'
`w32-scroll-lock-modifier'
`w32-apps-modifier'
`w32-mouse-button-tolerance'
`w32-mouse-move-interval'
`w32-pass-extra-mouse-buttons-to-system'
`w32-pass-multimedia-buttons-to-system'
`w32-pass-rwindow-to-system'
`w32-pass-lwindow-to-system'
`w32-rwindow-modifier'
`w32-pass-multimedia-buttons-to-system'\n
;; :KEYS-THAT-ARE-EASY-TO-NOT-FIND
<rwindow>
<lwindow>
<C-backspace>
<S-backspace>
\(kbd \"\\177\"\) -> \(kbd \"\\C-?\"\) => backward-kill-word 
<tab> <backtab> \[backtab] <S-iso-lefttab> \[(shift tab)] 
\[?\\t]        -> C-i TAB
\[?\\C-/]      -> \(get 'undo :advertised-binding\)
\[?\\C-x ?u]   ->  undo
\[?\\C- ]      -> set-mark-command
\[?\\C-\\S-v]   -> scroll-other-window
\[C-delete]
\[C-backspace]
\[select-window]
\[delete-frame]
\[iconify-frame]
\[make-frame-visible]
\[switch-frame]
\[mouse-1] [down-mouse-1]
\[mouse-2] [down-mouse-3]
\[mouse-2] [down-mouse-2]
\[right] [C-right][M-right] [C-M-left]
\[left]  [C-left] [M-left] [C-M-right]
\[next] [C-next]
\[up] [C-up] 
\[prior] [C-prior] 
\[down] [C-down]
\[end] [C-end] [M-end]
\[C-S-backspace]
\[home] [C-home]
\[begin]
`x-alt-keysym'
`x-meta-keysym'
`x-hyper-keysym'
`x-super-keysym'\n
;; :KEY-FUNCTIONS-MON-LOCAL
`mon-read-keys-as-string'
`mon-test-keypresses'\n
:KEYBINDING-HOOK-IDIOM
 
 \( [ add-hook | remove-hook ] 
  '<SOME-HOOK-TO-ADD/REMOVE>
  \(function     ;; :NOTE Using `function' or \"#'\" \(sharpquote\) w/ `add-hook'
   \(lambda \(\)   ;; allows later `remove-hook' removal of anonymous lambda forms.
    \(define-key '<SOME-MODE-MAP> { \(kbd <\"KEY-STR*\">\) |
                                    <[VECTOR-OF-KEY-CHARS]> |
                                    <\"KEY-STR*\"> }
                                   '<SOME-FUNCTION-NAME>\)\)\)\)\n
:KEY-EVENT-CONVERT-IDIOM\n
\(kbd \"<S-SPC>\"\) => [33554464]\n
\(event-basic-type 33554464\)\n => 32\n
\(event-modifiers 33554464\)\n  => \(shift\)\n
\(event-convert-list '\(shift 32\)\)\n  => 33554464\n
\(event-apply-modifier 32 'shift 25 \"S-\"\)\n => 33554464\n
\(let \(\(rnd-trp \(event-apply-modifier 32 'shift 25 \"<S-\"\)\)\)
  \(event-convert-list
   `\(,@\(event-modifiers rnd-trp\) ,\(event-basic-type rnd-trp\)\)\)\)
;=> 33554464\n\n
\(let \(\(kbd-event \(elt \(kbd \"<s-SPC>\"\) 0\)\)
      frob-evnt\)
  \(setq frob-evnt
        `\(,@\(event-modifiers kbd-event\)
          ,\(event-basic-type kbd-event\)\)\)
  \(and \(eq \(event-convert-list frob-evnt\) kbd-event\)
       \(list frob-evnt \(vector kbd-event\)\)\)\)\n;=> \(\(super 32\) [8388640]\)\n
:SEE info node `(emacs)Key Bindings'
:SEE info node `(elisp)Keymaps'
:SEE :FILE src/keymap.c lisp/bindings.el
:SEE-ALSO `mon-help-keys', `mon-help-diacritics', `mon-help-w32-functions',
`mon-help-CL-slime-keys'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-key-functions :insertp t)
    (mon-help-message-intrp "mon-help-key-functions")))
;;
;;; :TEST-ME (mon-help-key-functions)
;;; :TEST-ME (mon-help-key-functions t)
;;; :TEST-ME (describe-function 'mon-help-key-functions)
;;; :TEST-ME (apply 'mon-help-key-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-01T13:54:15-04:00Z}#{10134} - by MON KEY>
;;;###autoload
(defun mon-help-load-functions (&optional insertp intrp)
  "List of functions related to loading elisp files.\n
;; :LOAD-FUNCTIONS
`autoload'
`load'
`load-file'
`load-library'
`load-theme'
`load-with-code-conversion'\n
`provide'
`require'
`intern'
`intern-soft'\n
;; :LOAD-DECLARE
`declare'                       :SEE info node `(elisp)Declaring Functions'
`declare-function'
`check-declare'
`check-declare-file'
`check-declare-directory'
`byte-compile-declare-function'\n
;; :LOAD-EVAL
`eval'
`eval-and-compile'
`eval-buffer'
`eval-current-buffer'
`eval-when'
`eval-when-compile'
`eval-after-load'
`load-time-value'\n
;; :LOAD-FILE-DIRECTORY
`get-load-suffixes'
`locate-library'
`symbol-file'\n
\(getenv \"EMACSLOADPATH\"\)
`load-path'                     ;<VARIABLE> 
`path-separator'                ;<VARIABLE>\n
;; :LOAD-HISTORY
`load-history-regexp'
`load-history-filename-element'
`featurep'
`preloaded-file-list'           ;<VARIABLE>
`features'                      ;<VARIABLE>
`current-load-list'             ;<VARIABLE>
`load-history'                  ;<VARIABLE>\n
;; :LOAD-UNLOAD
`unload-feature'
`makunbound'
`unintern'
`unload-feature-special-hooks' ;<VARIABLE>\n
;; :LOAD-VARIABLES
`obarray'
`byte-compile-dynamic-docstrings'
`after-load-alist'
`force-load-messages'
`load-in-progress'
`load-force-doc-strings'
`load-file-rep-suffixes'
`load-read-function'
`load-source-file-function'
`load-suffixes'
`read-symbol-positions-list'
`read-with-symbol-positions'
`byte-compile-insert-header'\n
:SEE info node `elisp(Loading)'.\n
:SEE :FILE lread.c loaddefs.el 
:SEE-ALSO `mon-help-emacs-introspect', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-load-functions :insertp t)
    (mon-help-message-intrp "mon-help-load-functions")))
;;
;;; :TEST-ME (mon-help-load-functions)
;;; :TEST-ME (describe-function 'mon-help-load-functions)
;;; :TEST-ME (apply 'mon-help-load-functions nil '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-20T13:26:27-04:00Z}#{09387} - by MON>
;;;###autoload
(defun mon-help-read-functions (&optional insertp intrp)
  "List of functions for reading.\n
      _______                       ____________________  60.
     |       |                     |                    |   
     | :READ |                     | :STANDARD-READERS  |   
   __|_______|_______________     _|____________________|_  
  |                          |   |                        | 
  | `read'                   |   | `read-color'           | 
  | `read-from-string'       |   | `read-kbd-macro'       | 
  | `read-from-whole-string' |   | `read-number'          | 
  |__________________________|   | `read-passwd'          | 
        _________________        | `read-regexp'          | 
       |                 |       | `read-string'          | 
       | :STANDARD-INPUT |       | `read-shell-command'   | 
       | :READ-STREAMS   |       |________________________| 
   ____|_________________|________________________________  
  |                                                       | 
  |  `standard-input'--+ <VARIABLE>                       | 
  |    |                 ______________________________   | 
  |    .-+ <BUFFER>     |                              |  | 
  |    .-+ <MARKER>     | `read-circle'                |  | 
  |    .-+ <STRING>     |  |-+ <VARIABLE>              |  | 
  |    .-+ <FUNCTION>   | `read-with-symbol-positions' |  | 
  |    .-+ <SYMBOL>     |  |-+ <VARIABLE>              |  | 
  |    .-+ t            | `read-symbol-positions-list' |  | 
  |    .-+ nil          |  |-+ <VARIABLE>              |  | 
  |                     |______________________________|  | 
  |_______________________________________________________| 
     ______________                          __________     
    |              |                        |          |    
    | :MINI-BUFFER |                        | :BUFFERS |    
   _|______________|_______      ___________|__________|__  
  |                        |    |                         | 
  | `read-minibuffer'      |    | `read-buffer'           | 
  | `read-from-minibuffer' |    | `read-buffer-function'  | 
  | `read-no-blanks-input' |    | `read-buffer-to-switch' | 
  |________________________|    |_________________________|   
     ________                                  ________      
    |        |                                |        |     
    | :FILES |                                | :CHARS |     
   _|________|________________    ____________|________|____ 
  |                           |  |                          |
  | `read-file-name'          |  | `read-char'              |
  | `read-directory-name'     |  | `read-char-by-name'      |
  | `read-file-name-internal' |  | `read-char-excvusive'    |
  | `read-file-modes'         |  | `read-charset'           |
  | `file-readable-p'         |  | `read-quoted-char'       |
  | `desktop-read'            |  | `read-quoted-char-radix' |
  | `read-abbrev-file'        |  | `read-char-choice'       |
  |___________________________|  |__________________________|
    _______________                            ________
   |               |                          |        |
   | :KEY-EVENTS   |                          | :FACES |
 __|_______________|_________   ______________|________|____
|                            | |                            |
| `read-event'               | | `read-face-font'           |
| `read-key-sequence'        | | `read-face-name'           |
| `read-key-sequence-vector' | | `read-face-attribute'      |
| `read-command'             | | `read-all-face-attributes' |
| `unread-command-events'    | | `read-face-and-attribute'  |
|                            | |____________________________|
|____________________________|          ______________       
                                       |              |      
                                       | :ENVIRONMENT |      
                              _________|______________|_____ 
                             |                              |
                             | `coding-system-for-read'     |
                             | `read-coding-system'         |
                             | `read-expression-history'    |
                             | `read-expression-map'        |
                             | `read-envvar-name'           |
                             | `read-non-nil-coding-system' |
                             | `read-input-method-name'     |
                             | `read-multilinlual-string'   |
                             | `read-language-name'         |
                             | `read-with-symbol-positions' |
                             | `read-symbol-positions-list' |
                             |______________________________|
                                                          60^\n
:SEE info node `(elisp)Read and Print'.\n
:SEE :FILE `lread.c'
:SEE-ALSO `mon-help-print-functions', `mon-help-load-functions',
`mon-help-key-functions', `mon-help-char-representation'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-read-functions :insertp t)
    (mon-help-message-intrp "mon-help-read-functions")))
;;
;;; :TEST-ME (mon-help-read-functions)
;;; :TEST-ME (mon-help-read-functions t)
;;; :TEST-ME (describe-function 'mon-help-read-functions)
;;; :TEST-ME (apply 'mon-help-read-functions '(t))

;;; ==============================
;; (defun mon-help-minibuffer-completion (&optional insertp intrp)
;; `read-from-minibuffer' `completing-read'
;; `history-length`            <PROPERTY>
;; `add-to-history'
;; `history-delete-duplicates' <VARIABLE>
;; `history-add-new-input'     <VARIABLE>
;; `history-length'            <VARIABLE>
;; :HISTORY-LISTS
;; `minibuffer-history'
;; `regexp-history'
;; `query-replace-history'
;; `file-name-history'
;; `buffer-name-history'
;; `dired-regexp-history'
;; `extended-command-history'
;; `shell-command-history'
;; `read-expression-history'
;; `command-history'

;; :COMMINT
;; comint-add-to-input-history
;;
;; `Info-history' interesting to frob this with completion
;;; ==============================

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-13T12:21:21-05:00Z}#{10023} - by MON KEY>
;;;###autoload
(defun mon-help-print-functions (&optional insertp intrp)
  "Print related functions and variables which affect their output.\n
;; :PRINT-FUNCTIONS
`eval-print-last-sexp'
`message'
`prin1'
`prin1-char'
`prin1-to-string'
`princ'
`princ-list'
`print'
`terpri'
`with-output-to-string'     ; :NOTE Output to generated buffer \" *string-output*\"
`write-char'
`write-region'\n
;; :PRINT-FUNCTIONS-FORMAT
`format-mode-line'
`format'
`format-time-string'
`format-seconds'
`mode-line-format'         ;<VARIABLE>\n
;; :PRINT-FUNCTIONS-INSERT
`insert'
`insert-byte'
`insert-string'
`insert-tab'
`insert-char'
`insert-rectangle'
`insert-register'
`insert-kbd-macro'\n
;; :PRINT-FUNCTIONS-INSERT-BUFFER
`insert-buffer'
`insert-buffer-substring'
`insert-buffer-substring-no-properties'\n
;; :PRINT-FUNCTIONS-MARKER
`marker-insertion-type'
`make-marker'
`set-marker'
`set-marker-insertion-type'\n
;; :PRINT-FUNCTIONS-INSERT-YANK
`insert-buffer-substring-as-yank'
`insert-for-yank'
`insert-for-yank-1'\n
;; :PRINT-FUNCTIONS-INSERT-FILE-DIR
`insert-directory'
`insert-directory-adj-pos'
`insert-directory-safely'
`insert-file'
`insert-file-1'
`insert-file-contents'
`insert-file-contents-literally'
`insert-file-literally'\n
;; :PRINT-FUNCTIONS-INSERT-IMAGE
`insert-image'
`insert-image-file'
`insert-sliced-image'\n
;; :PRINT-FUNCTIONS-INSERT-BUTTON
`insert-button'
`insert-text-button'\n
;; :PRINT-FUNCTIONS-INSERT-STICKY
`insert-and-inherit'
`insert-before-markers'
`insert-before-markers-and-inherit'\n
;; :PRINT-FUNCTIONS-IO-STREAM
`message-box'
`message-or-box'
`external-debugging-output'
`redirect-debugging-output'
`send-string-to-terminal'
`standard-output'
`standard-input'\n
;; :PRINT-FUNCTIONS-PRETTY  :SEE :FILE emacs-lisp/pp.el
`pp'                        ; :NOTE Output to named stream or `standard-output'
`pp-buffer'
`pp-display-expression'     ; :NOTE Buffer output is `with-output-to-temp-buffer'
`pp-eval-expression'        ; :NOTE Output to buffer \"*Pp Eval Output*\"
`pp-eval-last-sexp'
`pp-last-sexp'
`pp-macroexpand-expression' ; :NOTE Output to buffer \"*Pp Macroexpand Output*\"
`pp-macroexpand-last-sexp'
`pp-to-string'              ; :NOTE Output to generated buffer \" pp-to-string\"
`pp-escape-newlines'        ;<VARIABLE>\n
;; :PRINT-FUNCTIONS-MULTI/UNIBYTE
`set-buffer-multibyte'
`enable-multibyte-characters'\n
;; :PRINT-VARIABLES
`eval-expression-print-length'
`eval-expression-print-level'
`float-output-format'
`use-dialog-box'
`printable-chars'
`print-charset-text-property'
`print-circle'
`print-continuous-numbering'
`print-escape-multibyte'
`print-escape-newlines'
`print-escape-nonascii'
`print-length'
`print-level'
`print-number-table'
`print-quoted'
`print-gensym'\n
\(let \(\(print-gensym t\)
       \(my-bubba \(make-symbol \"bubba\"\)\)\)
   \(princ my-bubba \(current-buffer\)\)\)\n ;=> #:bubba\n
:SEE info node `(elisp)Inserting Text'
:SEE info node `(elisp)Output Functions'
:SEE info node `(elisp)Output Variables'
:SEE info node `(elisp)Printed Representation'
:SEE info node `(elisp)Read and Print'
:SEE info node `(elisp)Streams Intro'\n
:SEE-ALSO `mon-help-read-functions', `mon-help-marker-functions',
`mon-help-load-functions', `mon-help-char-representation',
`mon-help-format-width', `gnus-bind-print-variables'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-print-functions :insertp t)
    (mon-help-message-intrp "mon-help-print-functions")))
;;
;;; :TEST-ME (mon-help-print-functions )
;;; :TEST-ME (mon-help-print-functions t)
;;; :TEST-ME (describe-function 'mon-help-print-functions)
;;; :TEST-ME (apply 'mon-help-print-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2199
;;; :CREATED <Timestamp: #{2010-10-19T16:18:21-04:00Z}#{10422} - by MON KEY>
;;;###autoload
(defun mon-help-marker-functions (&optional insertp intrp)
  "List of marker related functions and variables.\n
;; :MARKER-FUNCTIONS
`buffer-has-markers-at'
`copy-marker'
`make-marker'
`mark-marker'
`marker-buffer'
`marker-position'
`move-marker'
`point-marker'
`point-max-marker'
`point-min-marker'
`set-marker'\n
;; :MARKER-INSERTION
`marker-insertion-type'
`set-marker-insertion-type'
`insert-before-markers'
`insert-before-markers-and-inherit' ; :NOTE Also a <PROPERTY>\n
;; :MARKER-PREDICATES
`markerp'
`integer-or-marker-p'
`number-or-marker-p'\n
;; :MARKER-FUNCTIONS-SLIME
`slime-output-target-marker'
`slime-reset-repl-markers'
`slime-save-marker'\n
;; :MARK-FUNCTIONS
`mark'
`push-mark'
`pop-mark'
`set-mark'
`mark-paragraph'
`mark-whole-buffer'
`mark-word'
`mark-sexp'
`mark-page'
`mark-defun'\n
;; :MARK-FUNCTIONS-SLIME
`slime-mark-input-start'
`slime-mark-output-end'
`slime-mark-output-start'
`slime-mark-presentation-start'
`slime-mark-presentation-start-handler'
`slime-mark-presentation-end-handler'
`slime-mark-presentation'
`slime-mark-presentation-end'\n
;; :MARKER-VARIABLES
`help-window-point-marker'\n
:SEE info node `(elisp)Markers'\n
:SEE-ALSO `mon-help-print-functions', `mon-help-print-functions',
`mon-help-read-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-marker-functions :insertp t)
    (mon-help-message-intrp "mon-help-marker-functions")))
;;
;;; :TEST-ME (mon-help-marker-functions )
;;; :TEST-ME (mon-help-marker-functions t)
;;; :TEST-ME (apply 'mon-help-marker-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-16T15:11:07-05:00Z}#{10026} - by MON KEY>
;;;###autoload
(defun mon-help-hash-functions (&optional insertp intrp)
  "Hash table related functions.\n
;; :HASH-TABLE-EMACS
`clrhash'
`copy-hash-table',    
`define-hash-table-test'
`gethash'
`hash-table-count'
`hash-table-p'
`hash-table-rehash-size'
`hash-table-rehash-threshold'
`hash-table-size'
`hash-table-test'
`hash-table-weakness'
`make-hash-table'
`maphash'
`puthash'
`remhash'
`sxhash'
`hash-table-test`                 ;<PROPERTY>\n
;; :HASH-TABLE-EMACS-CL
`cl-not-hash-table'
:ALIASED-BY `cl-make-hash-table'  <- `make-hash-table'
:ALIASED-BY `cl-hash-table-p'     <- `hash-table-p'
:ALIASED-BY `cl-hash-table-count' <- `hash-table-count'
:ALIASED-BY `cl-gethash'          <- `gethash'
:ALIASED-BY `cl-puthash'          <- `puthash'
:ALIASED-BY `cl-remhash'          <- `remhash'
:ALIASED-BY `cl-clrhash'          <- `clrhash'
:ALIASED-BY `cl-maphash'          <- `maphash'\n
;; :HASH-TABLE-FUNCTIONS-MON-LOCAL
`mon-hash-add-uniquify'
`mon-hash-all-keys'
`mon-hash-all-values'
`mon-hash-describe'
`mon-hash-describe-descend'
`mon-hash-make-size'               :SEE-ALSO `sxhash'
`mon-hash-readlines-buffer'
`mon-hash-readlines-file'
`mon-hash-to-list'\n
;; :HASH-TABLE-MACROS-MON-LOCAL
`mon-hash-add-uniquify'
`mon-hash-all-keys'
`mon-hash-all-values'
`mon-hash-describe'
`mon-hash-describe-descend'
`mon-hash-get-items'
`mon-hash-get-keys'
`mon-hash-get-string-keys'
`mon-hash-get-symbol-keys'
`mon-hash-get-values'
`mon-hash-has-key'
`mon-dir-hash-images'
`mon-hash-make-size'
`mon-hash-put-CL'
`mon-hash-readlines-buffer'
`mon-hash-readlines-file'
`mon-hash-table-complete'
`mon-hash-to-list'
`mon-hash<-vector'\n
;; HASH-TABLE-CONSTRUCTION
`make-hash-table' <&rest KEYWORD-ARGS>
                 [ :rehash-size { integer | float } ]
                 [ :rehash-threshold float ] ;<- \(and \(<= F 1\) \(floatp F\)\)
                 [ :size integer ] 
                 [ :test { eq | eql | equal } ]
                 [ :weakness  { nil | key | value
                                | key-or-value | key-and-value } ]\n
;; :HASH-TABLE-EXAMPLE\n
\(let \(\(super-hash \(make-hash-table :test 'equal\)\)
        hotness\)
  \(dolist \(S '\(\(\"Christie Brinkley\" . 8\)
               \(\"Kim Alexis\" . 7\)
               \(\"Paulina Porizkova\" . 10\)
               \(\"Elle Macpherson\" . 9\)\)
           \(progn
             \(maphash #'\(lambda \(N R\) 
                          \(push \(format \"SUPERMODEL %s is a %d\" N R\) hotness\)\)
                      super-hash\)
             \(setq hotness \(mapconcat #'\(lambda \(H\) \(identity H\)\) hotness \"\\n\"\)\)\)\)
    \(puthash \(car S\) \(cdr S\) super-hash\)\)\)\n
;; :HASH-TABLE-TEST-PROPERTY
\(define-hash-table-test 'tt-htt 'eq 'eql\)
\(boundp  'tt-htt\)
\(fboundp 'tt-htt\)
\(symbol-plist 'tt-htt\)
\(get 'tt-htt 'hash-table-test\)\n
:NOTE As of `emacs-version' 23.2 hashtables have a read-syntax `#s'. 
The Lisp reader can read this printed representation, provided each element in
the specified hash table has a valid read syntax as illustrated with follwing:\n
\(setq bubbas-hash #s\(hash-table size 30 data \(key1 val1 key2 300\)\)\)
;=> #s\(hash-table {...} \)\)\n
bubbas-hash
;=> #s\(hash-table {...} \)\)=\n
\(gethash 'key1 bubbas-hash\)
;=> val1\n
\(type-of \(symbol-value 'bubbas-hash\)\)
;=> hash-table\n
\(unintern \"bubbas-hash\" obarray)
\(intern-soft \"bubbas-hash\"\)\n
:SEE info node `(elisp)hash functions'.
:SEE info node `(elisp)Hash Tables'.\n
:SEE-ALSO `mon-help-sequence-functions', `mon-help-plist-functions',
`mon-help-plist-properties', `mon-help-type-predicates'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-hash-functions :insertp t)
    (mon-help-message-intrp "mon-help-hash-functions")))
;;
;;; :TEST-ME (mon-help-hash-functions)
;;; :TEST-ME (mon-help-hash-functions t)
;;; :TEST-ME (describe-function 'mon-help-hash-functions)
;;; :TEST-ME (apply 'mon-help-hash-functions nil '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-19T17:02:02-04:00Z}#{09386} - by MON KEY>
;;;###autoload
(defun mon-help-plist-functions (&optional insertp intrp)
  "Help for plist and property list related functions.\n
;; :PLIST-FUNCTIONS-GETTERS
`get'
`plist-get'
`lax-plist-get'\n
;; :PLIST-FUNCTIONS-SETTERS
`setplist'
`put'
`plist-put'
`lax-plist-put'\n
;; :PLIST-FUNCTIONS-CL
`remf'
`setf'                 ; :USAGE (setf (get tt--plist :foo) '(new-foo))
`getf'
`get*'
`cl-set-getf'
`cl-do-remf'
`cl-remprop'
`remprop'\n
;; :PLIST-FUNCTION-INSPECT
`equal-including-properties'
`documentation-property'
`symbol-plist'
`plist-member'\n
;; :PLIST-FUNCTION-INSPECT-APROPOS
`apropos-property-face'
`apropos-format-plist'
`apropos-describe-plist'\n
;; :PLIST-FUNCTION-PROCESS
`process-plist'
`process-put'
`process-get'\n
;; :PLIST-FUNCTION-WIDGET
`widget-put'
`widget-get'
`widget-apply'\n
;; :PLIST-FUNCTION-CHARACTER        :SEE info node `(elisp)Character Properties'
`char-category-set'
`charset-plist'
`define-char-code-property'
`get-char-code-property'
`get-char-property-and-overlay'
`put-char-code-property'\n
;; :PLIST-FUNCTIONS-MON-LOCAL
`mon-plist-keys'
`mon-plist-remove!'
`mon-plist-remove-consing'
`mon-plist-remove-if'\n
:NOTE the docstring of `plist-put' has the odd phraseology:
 \"use `\(setq x \(plist-put x prop val\)\)' to be sure to use the new value.\"
plist-put 
the plist could be empty in the first place.

 \(setq tt--plist nil\)
 \(plist-put tt--plist :foo :bar\) ; => \(:foo :bar\)
 tt--plist ; => nil
:SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2009-08/msg01001.html')\n
:SEE info node `(elisp)Property Lists'
:SEE info node `(elisp)Symbol Plists'
:SEE info node `(elisp)Other Plists'\n
:SEE-ALSO `mon-help-plist-properties', `mon-help-text-property-functions',
`mon-help-hash-functions', `mon-help-sequence-functions', `mon-help-plist-functions',
`mon-help-type-predicates'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-plist-functions :insertp t)
    (mon-help-message-intrp "mon-help-plist-functions")))
;;
;;; :TEST-ME (mon-help-plist-functions)
;;; :TEST-ME (mon-help-plist-functions t)
;;; :TEST-ME (call-interatively 'mon-help-plist-functions)
;;; :TEST-ME (describe-function 'mon-help-plist-functions)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-08T12:07:55-05:00Z}#{10101} - by MON KEY>
;;;###autoload
(defun mon-help-plist-properties (&optional insertp intrp)
  "List of common Emacs elisp ``built-in'' properties appearing on plists.\n
Unless indicated otherwise, following list enumerates properties.
When a property-name is not also symbol-name it is distinguished as:\n
  `<PROPERTY-NAMM>` vs `<SYMBOL-NAME>'\n
;; :PLIST-PROPERTIES
`buffer-access-fontified-property' ; :NOTE Also a <FUNCTION>
;; :PLIST-PROPERTIES-FUNCTION
`interactive-form`
`lisp-indent-function`             ; :NOTE Also a <FUNCTION>\n
;; :PLIST-PROPERTIES-VARIABLE
`permanent-local`
`saved-value`
`saved-variable-comment`
`safe-local-variable`
`risky-local-variable`
`variable-interactive`              :SEE `set-variable'
`variable-comment`\n
;; :PLIST-PROPERTIES-CUSTOM
`custom-get`
`custom-set`
`custom-autoload`
`custom-dependencies`
`custom-group`
`custom-links`
`custom-loads`
`custom-mode-group`
`custom-options`
`custom-prefix`
`custom-requests`
`custom-tag`
`custom-type`
`custom-version`
`customized-value`
`custom-package-version`
`customized-variable-comment`
`force-value`
`standard-value`\n
;; :PLIST-PROPERTIES-THEME
`theme-documentation`
`theme-face`
`theme-feature`
`theme-settings`
`theme-value`\n
;; :PLIST-PROPERTIES-DOC
`apropos-inhibit`
`documentation`
`doc-string-elt`
`lisp-doc-string-elt-property'
`char-code-property-documentation`
`face-documentation'               ; :NOTE Also a <FUNCTION>
`group-documentation`
`setf-documentation`               ; :NOTE :SEE `define-setf-method'
`function-documentation`
`theme-documentation`
`theme-settings`
`variable-documentation`
`lisp-doc-string-elt-property'     ;<VARIABLE>\n
;; :PLIST-PROPERTIES-FACE
`customized-face`
`face-comment`
`face-alias`
`face-defface-spec`
`face-override-spec`
`face-modified`
`face-documentation'               ; :NOTE Also a <FUNCTION>
`obsolete-face`
`saved-face`
`saved-face-comment`
`theme-face`\n
;; :PLIST-PROPERTIES-KEYS
`event-symbol-element-mask`
`event-symbol-elements`
`modifier-cache`
`suppress-keymap
`:advertised-binding`  ;`dired-find-file' `widget-backward' `undo' `proced-mark'`\n
;; :PLIST-PROPERTIES-EIEIO
`protection`
`eieio-class-definition`
`eieio-method-obarray`
`eieio-method-tree`\n
;; :PLIST-PROPERTIES-MENU
`menu-enable`
`menu-prop`\n
;; :PLIST-PROPERTIES-MODE
`:minor-mode-function`
`definition-name`
`mode-class`
`derived-mode-unmerged`\n
;; :PLIST-PROPERTIES-BYTE-COMPILE
`byte-code-vector`
`byte-compile`
`byte-compile-format-like`
'byte-compile-inline-expand      ; <PVAL>
`byte-compile-negated-op`
`byte-hunk-handler`
`byte-obsolete-info`
`byte-obsolete-variable`
`byte-opcode-invert`
`byte-optimizer`
`byte-stack+-info`
`cl-compiler-macro`
`compiler-macro-file`
`cl-byte-compile-compiler-macro`   ;<PVAL>\n
;; :PLIST-PROPERTIES-ADVICE
`ad-advice-info`
`ad-subr-arglist`\n
;; :PLIST-THING-AT-POINT
`beginning-op`
`bounds-of-thing-at-point`
`end-op`
`thing-at-point`\n
;; :PLIST-PROPERTIES-HASH
`hash-table-test'                           ; :NOTE Also a <FUNCTION>
;; :PLIST-PROPERTIES-LISP
`disabled`
`find-tag-default-function`
`lisp-indent-function`
`lisp-indent-hook`
`list-order`
`no-self-insert`
`pure`
`safe-function`
;; :PLIST-PROPERTIES-LISP-CL
`cl-deftype-handler`
`setf-method`
`side-effect-free`
`compiler-macro-file`
`cl-byte-compile-compiler-macro`  ; <PVAL>
`error-free`                      ; <PVAL>
`cl-byte-compile-block`           ; <PVAL>
`cl-byte-compile-throw`           ; <PVAL>
`setf-documentation`              ; <PVAL>\n
;; :PLIST-PROPERTIES-EDEBUG
`debug`
`edebug`
`edebug-coverage`
`edebug-dependents`
`edebug-form-spec`
`edebug-freq-count`
`edebug-initial-mode`
`edebug-on-entry`\n
;; :PLIST-PROPERTIES-OVERLAY
`after-string`
`before-string`
`evaporate`
`isearch-open-invisible`
`isearch-open-invisible-temporary`
`overlay-arrow-string`
`priority`
`window`\n
;; :PLIST-PROPERTIES-MINIBUFFER
`history-length`                  ; :NOTE Also a <FUNCTION>
;; :PLIST-PROPERTIES-BUFFER
`no-clone-indirect`\n
;; :PLIST-PROPERTIES-SYNTAX
`syntax-table'                     ; :NOTE Also a <FUNCTION>
`text-clone-syntax`\n
;; :PLIST-PROPERTIES-EVENT
`event-kind`\n
;; :PLIST-PROPERTIES-WIDGET
`pr-widget-list`
`pr-widget`
`widget-keyword-printer`
`widget-type`
`widget-documentation`\n
;; :PLIST-PROPERTIES-CHAR
`ascii-character`
`bidi-class`
`canonical-combining-class`
`char-table-extra-slots`
`char-code-property-documentation`
`composition`
`decimal-digit-value`
`decomposition`
`digit`
`general-category`
`iso-10646-comment`
`lowercase`
`mirrored`
`name`
`numeric-value`  
`old-name`
`titlecase`
`uppercase`\n
;; :PLIST-PROPERTIES-ERROR
`error'                               :SEE info node `(elisp)Standard Errors'
       <PLIST> (symbol-plist 'error)
      ¦ `error-conditions`
      ¦ `error-message`
      ¦ `byte-compile-format-like`\n
;; :PLIST-PROPERTIES-TEXT-PROPERTIES-SPECIAL
:SEE `mon-help-text-property-properties'\n
:SEE-ALSO `mon-help-plist-functions', `mon-help-text-property-functions',
`mon-help-text-property-properties'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-plist-properties :insertp t)
    (mon-help-message-intrp "mon-help-plist-properties")))
;;
;;; :TEST-ME (mon-help-plist-properties)
;;; :TEST-ME (mon-help-plist-properties t)
;;; :TEST-ME (describe-function 'mon-help-plist-properties)
;;; :TEST-ME (apply 'mon-help-plist-functions nil '(t))

 
;;; ==============================
;;; :SOURCE from commented sections of :FILE `cus-face.el' and :FILE `custom.el'
;;; The Example theme, forest-theme.el
;;; :COURTESY Chong Yidong :SOURCE emacs-devel :SUBJECT Re: Color themes
;;; :DATE Sun, 09 Aug 2009 12:18:01 -0400
;;; :SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2009-08/msg00356.html')
;;; :CREATED <Timestamp: #{2009-08-24T18:30:56-04:00Z}#{09351} - by MON KEY>
;;;###autoload
(defun mon-help-faces-themes (&optional insertp intrp)
  "Functions for handling themes and their faces.\n
;; :THEME-FUNCTIONS
`deftheme'                    ;<MACRO>
`load-theme'
`provide-theme'
`enable-theme'
`disable-theme'\n
;; :FACE-CUSTOM
`custom-set-faces'
`custom-reset-faces'
`custom-declare-face'
`custom-face-attributes-get'\n
;; :FACE-CUSTOM-THEME
`custom-declare-theme'
`customize-create-theme'
`custom-push-theme'
`custom-make-theme-feature'
`custom-check-theme'
`custom-variable-theme-value'
`custom-theme-recalc-variable'
`custom-theme-p'
`custom-theme-set-faces'
`custom-theme-reset-faces'
`custom-theme-recalc-face'
`custom-theme-set-variables'\n
;; :FACE-CUSTOM-VARIABLES
`custom-face-attributes'      ;<CONSTANT>
`custom-enabled-themes'
`custom-enabling-themes'
`custom-theme-directory'
`custom-known-themes'\n
:SEE :FILE `cus-face.el', `custom.el', `cus-edit.el'.\n
;; :THEME-CUSTOM
Custom themes are collections of settings that can be enabled or
disabled as a unit.\n
Each Custom theme is defined by a symbol, called the theme name.
The `theme-settings' property of the theme name records the
variable and face settings of the theme.  This property is a list
of elements, each of the form:\n
    (PROP SYMBOL THEME VALUE)\n
- PROP is either `theme-value' or `theme-face'
- SYMBOL is the face or variable name
- THEME is the theme name (redundant, but simplifies the code)
- VALUE is an expression that gives the theme's setting for SYMBOL.\n
The theme name also has a `theme-feature' property, whose value is
specified when the theme is defined (see `custom-declare-theme').
Usually, this is just a symbol named THEME-theme.  This lets
external libraries call \(require 'foo-theme\).\n
In addition, each symbol (either a variable or a face) affected by
an *enabled* theme has a `theme-value' or `theme-face' property,
which is a list of elements each of the form:\n
    (THEME VALUE)\n
which have the same meanings as in `theme-settings'.\n
The `theme-value' and `theme-face' lists are ordered by decreasing
theme precedence.  Thus, the first element is always the one that
is in effect.\n
Each theme is stored in a theme file, with filename THEME-theme.el.
Loading a theme basically involves calling \(load \"THEME-theme\"\)
This is done by the function `load-theme'.  Loading a theme
automatically enables it.\n
When a theme is enabled, the `theme-value' and `theme-face'
properties for the affected symbols are set.  When a theme is
disabled, its settings are removed from the `theme-value' and
`theme-face' properties, but the theme's own `theme-settings'
property remains unchanged.\n
;; :THEME-DEFINING
A theme file should be named `THEME-theme.el' (where THEME is the theme
name), and found in either `custom-theme-directory' or the load path.
It has the following format:\n
\(deftheme THEME DOCSTRING\)
\(custom-theme-set-variables 'THEME  [THEME-VARIABLES]\)
\(custom-theme-set-faces 'THEME [THEME-FACES]\)
\(provide-theme 'THEME\)\n
;; :THEME-EXAMPLE :SEE :FILE forest-theme.el\n
\(deftheme forest \"Created 2009-08-09.\"\)\n
\(custom-theme-set-faces 'forest\n
 '\(default \(\(t \(:foreground \"wheat\" :background \"black\"\)\)\)\)\n
 '\(font-lock-comment-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground  \"medium sea green\"\)\)\)\)\n
 '\(font-lock-constant-face \(\(\(\(class color\) \(min-colors 88\)\)
                             \(:foreground \"turquoise\"\)\)\)\)\n
 '\(font-lock-function-name-face \(\(\(\(class color\) \(min-colors 88\)\)
                                  \(:foreground \"pale green\"\)\)\)\)\n
 '\(font-lock-keyword-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground \"white\"\)\)\)\)\n\n
 '\(font-lock-string-face \(\(\(\(class color\) \(min-colors 88\)\)
                           \(:foreground \"dark khaki\"\)\)\)\)\n
 '\(font-lock-type-face \(\(\(\(class color\) \(min-colors 88\)\)
                         \(:foreground \"medium aquamarine\"\)\)\)\)\n
 '\(font-lock-variable-name-face \(\(\(\(class color\) \(min-colors 88\)\)
                                  \(:foreground \"yellow green\"\)\)\)\)\n
 '\(font-lock-warning-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground \"salmon1\"\)\)\)\)\n
 '\(font-lock-builtin-face \(\(\(\(class color\) \(min-colors 88\)\)
                            \(:foreground \"LightSteelBlue\"\)\)\)\)\n
 '\(region \(\(\(\(class color\) \(min-colors 88\)\)
            \(:foreground \"white\" :background \"dark green\"\)\)\)\)\n
 '\(highlight \(\(\(\(class color\) \(min-colors 88\)\)
               \(:foreground \"white\" :background \"dark green\"\)\)\)\)\)\n
\(provide-theme 'forest\)\n
;;;; end forest-theme.el\n
:SEE info node `(elisp)Defining Faces'.\n:SEE info node `(elisp)Face Attributes'.
:SEE-ALSO `mon-help-custom-keywords', `mon-help-faces', `mon-help-faces-basic',
`mon-help-color-chart' `mon-help-font-lock-functions', `mon-help-font-lock',
`mon-help-custom-keywords', `mon-help-widgets', `mon-help-easy-menu',
`mon-help-plist-functions',`mon-help-eieio-defclass',
`mon-help-eieio-functions', `mon-help-eieio-methods'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-faces-themes :insertp t)
    (mon-help-message-intrp "mon-help-faces-themes")))
;;
;;; :TEST-ME (mon-help-faces-themes t)

 
;;; ==============================
;;; :RENAMED `mon-help-make-faces'  -> `mon-help-faces'
;;; :CREATED <Timestamp: #{2009-09-26T13:23:36-04:00Z}#{09396} - by MON KEY>
;;;###autoload
(defun mon-help-faces (&optional insertp intrp)
  "Functions, Variables, Properties, etc. for defining faces.\n
    _________________                                 
   |                 | :SEE info node `(elisp)Defining Faces'            
   | :FACE-DEFINING  | :SEE info node `(elisp)Face Functions'                             
 __|_________________|_________________________________________________73.
|                                         |                              |
| `defface' <MACRO>                       |   :FACE-DEFINING-FUNCTIONS   |
|  :SPEC                                  |______________________________|
|  | :DISPLAY                             |                              |
|  | |-+ defalut                          | `make-face'                  |
|  | |-+ t                                | `face-list'                  |
|  | |-+ <LIST> -> of form: (Key Value)   | `copy-face'                  |
|  |   ¦ (type {graphic, x, pc, w32, tty})| `face-id'                    |
|  |   ¦ (class [color|grayscale|mono})   | `face-documentation'         |
|  |   ¦ (background [light|dark])        | `face-equal'                 |
|  |   ¦ (min-colors <INTEGER>)           | `face-differs-from-default-p'|
|  |   ¦ (supports   <VALUE>)             |______________________________|
|  |                                                                     |
|  `---------------------.                                               |
|                         \\                                              |
|                          \\     :SEE info node `(elisp)Face Attributes' |
|__________________________ \\    ________________________________________|
|                          | `->|                                        |
| :FACE-ATTRIBUTE-KEYWORDS |    | :family - <STRING>                     |
|__________________________|__  | { Fontset name, Font family }          |
|                             | | :NOTE `*' and `?' wildcards allowed.   |
|  :family :foundry           | | :SEE `font-family-list', `x-list-fonts'|
|  :font   :fontset           | |________________________________________|
|  :height                    | |                                        |
|  :width :weight :slant      | | :foundry - <STRING>                    |
|  :foreground :background    | | :NOTE `*' and `?' wildcards allowed.   |
|  :underline  :overline      | |________________________________________|
|  :strike-through            | |                                        |
|  :box                       | | :font - (a font object)                |
|  :inverse-video             | | :SEE info node `(elisp)Font Selection' |
|  :stipple                   | | :SEE info node `(elisp)Fontsets'       |
| `face-attribute-name-alist' | |________________________________________|
|_____________________________| |                                        |
|                             | | :height - [<INTEGER>|<FLOAT>]          |
| :FACE-ATTRIBUTE-FUNCTIONS   | |  { 1/10 point, float }                 |
|                             | |________________________________________|
|-----------------------------| |                                        |
|                             | | :width - <SYMBOL>                      |
| `font-family-list'          | |  { normal, condensed, expanded         |
| `face-attribute'            | |    semi-condensed, semi-expanded       |
| `set-face-attribute'        | |    extra-condensed, extra-expanded     |
| `face-all-attributes'       | |    ultra-condensed, ultra-expanded }   |
| `merge-face-attribute'      | |________________________________________|
| `face-attribute-relative-p' | |                                        |
| `invert-face'               | | :weight - <SYMBOL>                     |
| `face-foreground'           | |  { normal, bold, light                 |
| `face-background'           | |    semi-bold, semi-light               |
| `face-stipple'              | |    extra-bold, extra-light             |
| `face-font'                 | |    ultra-bold, utltra-light }          |
| `face-bold-p'               | |________________________________________|
| `face-italic-p'             | |                                        |
| `face-underline-p'          | | :slant - <SYMBOL>                      |
| `face-inverse-video-p'      | |  { normal, italic, oblique,            |
| `face-attributes-as-vector' | |   reverse-italic, reverse-oblique }    |
|_____________________________| |________________________________________|
|                             | |                                        |
| :FACE-ATTRIBUTE-VARIABLES   | | :foreground - [<STRING>|<HEX-VAL>]     |
|                             | | :background - [<STRING>|<HEX-VAL>]     |
|-----------------------------| | :SEE info node `(elisp)Color Names'    |
|                             | |             `mon-help-color-chart'     |
| `bitmap-spec-p'             | |         `mon-help-color-functions'     |
| `face-attribute-name-alist' | |________________________________________|
| `custom-face-attributes'    | |                                        |
| `frame-background-mode'     | | :underline - [<BOOLEAN>|<STRING>]      |
| `underline-minimum-offset'  | | :overline  - [<BOOLEAN>|<STRING>]      |
| `x-bitmap-file-path'        | | :strike-through - [<BOOLEAN>|<STRING>] |
|                             | |  { t - using face's color              |
|_____________________________| |    <STRING> - Using specified <COLOR>  |
|                             | |    nil - do not apply effect }         |
| :FACE-PROPERTY-ATTRIBUTES   | |________________________________________|
|  (face symbol properties)   | |                                        |
|                             | | :box - [<BOOLEAN>|<STRING>|<LIST>]     |
|-----------------------------| |  { nil - no box                        |
|                             | |   t - linewidth in :foreground <COLOR> |
| `face-alias`                | |   color - box w/ line width in <COLOR> |
| `face-defface-spec`         | |   ( :line-width <WIDTH>                |
| `customized-face`           | |     :color <COLOR>                     |
| `face-documentation'        | |     :style <STYLE> ) }                 |
| `saved-face`                | |________________________________________|
| `obsolete-face`             | |                                        |
|_____________________________| | :inverse-video - <BOOLEAN>             |
                                |  { t   - yes                           |
                                |    nil - no }                          |
 _____________________________  |________________________________________|
|                             | |                                        |
|      :FACE-READERS          | | :stipple - [<BOOLEAN>|<STRING>]        |
|_____________________________| |  { <BITMAP> :SEE `x-bitmap-file-path'  |
|                             | |   ,----                                |
| `read-face-font'            | |   | <WIDTH>  - Width in pixels         |
| `read-face-name'            | |   | <HEIGHT> - Height in pixels        |
| `read-face-attribute'       | |   | <DATA>   - Data string of raw bits |
| `read-all-face-attributes'  | |   `----                                |
| `read-face-and-attribute'   | |  nil - no stipple }                    |
|_____________________________| |________________________________________|
                                |                                        |
                                | :inherit - [ <STRING> | <LIST> ]       |
 _____________________________  |  { face name, or list of face names }  |
|                             | |________________________________________|
|       :FACE-INTERNAL        |                                           
|_____________________________|_________     _________________            
|                                       |   |                 |           
| `internal-face-x-get-resource'        |   | :FACE-VARIABLES |            
| `internal-lisp-face-empty-p'          |  _|_________________|__________ 
| `internal-lisp-face-equal-p'          | |                              | 
| `internal-lisp-face-p'                | | `inhibit-free-realized-faces'| 
| `internal-make-lisp-face'             | |______________________________|  
| `internal-merge-in-global-face'       |                                 
| `internal-get-lisp-face-attribute'    | `self-insert-face'
| `internal-lisp-face-attribute-values' | `self-insert-face-command'
| `inhibit-free-realized-faces'         |
| `clear-face-cache'                    |
| `define-obsolete-face-alias'          |
|_______________________________________|                              73^\n
:ALIASED-BY `mon-help-face-functions'\n
:SEE-ALSO `mon-help-faces-basic', `mon-help-font-lock', `mon-help-faces-themes',
`mon-help-color-chart', `mon-help-custom-keywords', `mon-help-widgets',
`mon-help-easy-menu', `mon-help-plist-functions', `mon-help-plist-properties',
`mon-help-color-chart', `mon-help-eieio-defclass', `mon-help-eieio-functions',
`mon-help-eieio-methods'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-faces :insertp t)
    (mon-help-message-intrp "mon-help-faces")))
;;
;;; :TEST-ME (mon-help-faces)
;;; :TEST-ME (mon-help-faces t)
;;; :TEST-ME (describe-function 'mon-help-faces)
;;; :TEST-ME (apply 'mon-help-faces '(t))

 
;;; ==============================
;;; :RENAMED `mon-help-basic-faces' -> `mon-help-faces-basic'
;;; :CREATED <Timestamp: #{2009-09-04T17:34:43-04:00Z}#{09365} - by MON KEY>
;;;###autoload
(defun mon-help-faces-basic (&optional insertp intrp)
  "The custom-group `basic-faces' is utilized for inheriting faces.\n
These are the standard Emacs faces are defined in :FILE faces.el\n
:NOTE As of 23.1 there is no _formal_ indication that newly defined faces must
inherit from one of the basic-faces this practice is encouraged.\n
:SEE \(URL `http://lists.gnu.org/archive/html/emacs-devel/2009-08/msg00525.html').\n►►►
;; :FACE-BASIC
\(describe-face 'default\)
\(describe-face 'bold\)
\(describe-face 'italic\)
\(describe-face 'underline\)
\(describe-face 'bold-italic\)
\(describe-face 'fixed-pitch\)
\(describe-face 'variable-pitch\)
\(describe-face 'shadow\)
\(describe-face 'link\)
\(describe-face 'link-visited\)
\(describe-face 'highlight\)
\(describe-face 'region\)
\(describe-face 'mode-line\)
\(describe-face 'header-line\)
\(describe-face 'secondary-selection\)
\(describe-face 'trailing-whitespace\)
\(describe-face 'escape-glyph\)
\(describe-face 'nobreak-space\)
\(describe-face 'mode-line-inactive\)
\(describe-face 'mode-line-highlight\)
\(describe-face 'mode-line-emphasis\)
\(describe-face 'mode-line-buffer-id\)
\(describe-face 'vertical-border\)
\(describe-face 'minibuffer-prompt\)
\(describe-face 'minibuffer-noticeable-prompt\)
\(describe-face 'fringe\)
\(describe-face 'scroll-bar\)
\(describe-face 'border\)
\(describe-face 'cursor\)
\(describe-face 'mouse\)
\(describe-face 'tool-bar\)
\(describe-face 'menu\)
\(describe-face 'glyphless-char\)\n
:SEE-ALSO `mon-help-faces-basic', `mon-help-font-lock', `mon-help-faces-themes', 
`mon-help-font-lock-functions', `mon-help-naf-mode-faces'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-faces-basic :insertp t)
    (mon-help-message-intrp "mon-help-faces-basic")))
;;
;;; :TEST-ME (mon-help-faces-basic t)
;;; :TEST-ME (describe-function 'mon-help-faces-basic)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-08T15:36:26-05:00Z}#{10101} - by MON KEY>
;;;###autoload
(defun mon-help-faces-font-lock (&optional insertp intrp)
  "List of faces defined specifically for Font Lock mode.\n
:SEE info node `(elisp)Faces for Font Lock'.\n
;; :FACE-FONT-LOCK
\(describe-face 'font-lock-comment-face\)
\(describe-face 'font-lock-comment-delimiter-face\)
\(describe-face 'font-lock-doc-face\)
\(describe-face 'font-lock-string-face\)
\(describe-face 'font-lock-keyword-face\)
\(describe-face 'font-lock-builtin-face\)
\(describe-face 'font-lock-function-name-face\)
\(describe-face 'font-lock-variable-name-face\)
\(describe-face 'font-lock-type-face\)
\(describe-face 'font-lock-constant-face\)
\(describe-face 'font-lock-preprocessor-face\)
\(describe-face 'font-lock-negation-char-face\)
\(describe-face 'font-lock-warning-face\)\n
;; :FACE-FONT-LOCK-VARIABLES
`font-lock-builtin-face'
`font-lock-comment-delimiter-face'
`font-lock-constant-face'
`font-lock-doc-face'
`font-lock-function-name-face'
`font-lock-keyword-face'
`font-lock-negation-char-face'
`font-lock-preprocessor-face'
`font-lock-reference-face'
`font-lock-string-face'
`font-lock-type-face'
`font-lock-variable-name-face'
`font-lock-warning-face'\n
:SEE-ALSO `mon-help-font-lock', `mon-help-font-lock-functions',
`mon-help-faces', `mon-help-faces-themes', `mon-help-faces-basic'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-faces-font-lock :insertp t)
    (mon-help-message-intrp "mon-help-faces-font-lock")))
;;
;;; :TEST-ME (mon-help-faces-font-lock )
;;; :TEST-ME (mon-help-faces-font-lock t)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-20T16:01:38-05:00Z}#{10076} - by MON KEY>
;;;###autoload
(defun mon-help-font-lock-functions (&optional insertp intrp)
  "List of font-lock and jit-lock related functions.\n
:SEE info node `(elisp)Font Lock Basics'.\n
:FONT-LOCK-FUNCTIONS
`font-lock-add-keywords'
`font-lock-after-change-function'
`font-lock-after-fontify-buffer'
`font-lock-after-unfontify-buffer'
`font-lock-append-text-property'
`font-lock-apply-highlight'
`font-lock-apply-syntactic-highlight'
`font-lock-change-mode'
`font-lock-choose-keywords'
`font-lock-compile-keywords'
`font-lock-default-fontify-buffer'
`font-lock-default-fontify-region'
`font-lock-default-function'
`font-lock-default-unfontify-buffer'
`font-lock-default-unfontify-region'
`font-lock-defontify'
`font-lock-eval-keywords'
`font-lock-extend-jit-lock-region-after-change'
`font-lock-extend-region-multiline'
`font-lock-extend-region-wholelines'
`font-lock-fillin-text-property'
`font-lock-fontify-anchored-keywords'
`font-lock-fontify-block'
`font-lock-fontify-buffer'
`font-lock-fontify-keywords-region'
`font-lock-fontify-region'
`font-lock-fontify-syntactic-anchored-keywords'
`font-lock-fontify-syntactic-keywords-region'
`font-lock-fontify-syntactically-region'
`font-lock-match-c-style-declaration-item-and-skip-to-next'
`font-lock-mode'
`font-lock-mode-internal'
`font-lock-prepend-text-property'
`font-lock-remove-keywords'
`font-lock-set-defaults'
`font-lock-turn-off-thing-lock'
`font-lock-turn-on-thing-lock'
`font-lock-unfontify-buffer'
`font-lock-unfontify-region'
`font-lock-update-removed-keyword-alist'
`font-lock-value-in-major-mode'\n
;; :FONT-LOCK-JIT-LOCK-FUNCTIONS
`jit-lock-after-change'
`jit-lock-context-fontify'
`jit-lock-deferred-fontify'
`jit-lock-fontify-now'
`jit-lock-force-redisplay'
`jit-lock-function'
`jit-lock-mode'
`jit-lock-refontify'
`jit-lock-register'
`jit-lock-stealth-chunk-start'
`jit-lock-stealth-fontify'
`jit-lock-unregister'\n
;; :FONT-LOCK-VARIABLES
`fontification-functions'
`font-lock-keywords'
`font-lock-keywords-alist'
`font-lock-keywords-case-fold-search'
`font-lock-keywords-only'
`font-lock-mark-block-function'
`font-lock-maximum-decoration'
`font-lock-maximum-size'
`font-lock-mode'
`font-lock-mode-major-mode'
`font-lock-multiline'
`font-lock-removed-keywords-alist'
`font-lock-set-defaults'
`font-lock-support-mode'
`font-lock-syntactic-face-function'
`font-lock-syntactic-keywords'
`font-lock-syntactically-fontified'
`font-lock-syntax-table'
`font-lock-unfontify-buffer-function'
`font-lock-unfontify-region-function'
`font-lock-verbose'\n
;; :FONT-LOCK-JIT-LOCK-VARIABLES
`jit-lock-after-change-extend-region-functions'
`jit-lock-chunk-size'
`jit-lock-context-time'
`jit-lock-context-timer'
`jit-lock-context-unfontify-pos'
`jit-lock-contextually'
`jit-lock-defer-buffers'
`jit-lock-defer-contextually'
`jit-lock-defer-time'
`jit-lock-defer-timer'
`jit-lock-functions'
`jit-lock-mode'
`jit-lock-stealth-buffers'
`jit-lock-stealth-load'
`jit-lock-stealth-nice'
`jit-lock-stealth-repeat-timer'
`jit-lock-stealth-time'
`jit-lock-stealth-timer'
`jit-lock-stealth-verbose'\n
;; :FONT-LOCK-VARIABLES-NAMING-FACES
`font-lock-negation-char-face'
`font-lock-variable-name-face'
`font-lock-preprocessor-face'
`font-lock-reference-face'
`font-lock-string-face'
`font-lock-type-face'
`font-lock-warning-face'\n
:SEE :FILE font-lock.el :FILE jit-lock.el\n
:SEE info node `(elsip)Search-based Fontification'.\n
:SEE-ALSO `mon-help-font-lock', `mon-help-faces-font-lock', `mon-help-faces',
`mon-help-faces-basic', `mon-help-faces-themes', `mon-help-custom-keywords',
`mon-help-widgets', `mon-help-easy-menu', `mon-help-plist-functions',
`mon-help-color-chart', `mon-help-eieio-defclass', `mon-help-eieio-functions',
`mon-help-eieio-methods'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-font-lock-functions :insertp t)
    (mon-help-message-intrp "mon-help-font-lock-functions")))
;;
;;; :TEST-ME (mon-help-font-lock-functions)
;;; :TEST-ME (mon-help-font-lock-functions t)
;;; :TEST-ME (describe-function 'mon-help-font-lock-functions)
;;; :TEST-ME (apply 'mon-help-font-lock-functions nil '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Wednesday June 17, 2009 @ 05:37.52 PM - by MON KEY>
;;;###autoload
(defun mon-help-font-lock (&optional insertp intrp)
  "Display `font-lock-keywords' usage patterns.\n
A `font-lock-keywords' is either of the types: \"user-level\" or \"compiled\".\n
A \"user-level\" keywords list is specified as a list of font-locking \"rules\".\n
Typically such \"rules\" are compiled internally by by `font-lock-compile-keywords'
in which case these rules become a compiled keywords list which starts with `t'.\n
User-level \"rules\" in a `font-lock-keywords' list are elements with one of the
following formats:\n
 \( eval      . <FORM> \)
 \( <MATCHER> . <SUBEXP> \)
 \( <MATCHER> . <FACENAME> \)
 \( <MATCHER> . <HIGHLIGHT> \)
 \( <MATCHER> <HIGHLIGHT> {...} \)\n
\(eval . <FORM>\)
<FORM>  --  An expression which when evaluated returns a keyword element,
            e.g. for dynamic keyword generation at `font-lock-mode' activation.\n
When a \"rule\" is not of the format ( eval . <FORM> ) it is a list or cons.\n
When a cons, the pair is comprised of <MATCHER> and one of the elements:\n
 <SUBEXP> <FACENAME> <HIGHLIGHT>\n
When a list, it is comprised of a <MATCHER> and one or more <HIGHLIGHT> elements
this may include nested \"anchored\" <HIGHLIGHT> elts when <MATCH-HIGHLIGHT>
elts are specified.\n
Following enumeration of elt/\"rule\" patterns is _not_ a formal grammar:\n
<MATCHER> =: { <FUNCTION> | <REGEXP> }\n
<FUNCTION> ;; :ARG a bounds-limit 
           ;; :RETURN non-nil 
           ;; :DO move-point 
           ;; :SET `match-data' on success\n
<REGEXP>   -- A regexp possibly extended/generated with `regexp-opt'.\n
<HIGHLIGHT> =: { <MATCH-HIGHLIGHT> | <MATCH-ANCHORED> }\n
<MATCH-HIGHLIGHT> =: ( <SUBEXP> <FACENAME> [ <OVERRIDE> [ <LAXMATCH> ]] )\n
<SUBEXP>  -- <INTEGER> specifying the subexpression of <MATCHER> to highlight.\n
<FACENAME> -- Expression which either:
              a) evaluates to a value naming <FACE>.
              b) evaluates to a property list of the form:
                (face <FACE> PROP1 VAL1 PROP2 VAL2 { ... } )
                :NOTE Variant b affects/affected by: 
                `font-lock-extra-managed-props' `font-lock-unfontify-region-function'\n
<OVERRIDE> =: { t | keep | append | prepend }
                t -- When `t' overide existing fontification.
                keep --  non-fontified parts of a match highlighted are kept
                append -- existing fontification is merged with _old_ taking precedence.
                prepend -- existing fontification is merged with _new_ taking precedence.\n
<LAXMATCH>  =: <BOOLEAN> 
               `non-nil' -- ignore-errors when a <SUBEXP> in <MATCHER> fails to match.\n
<MATCH-ANCHORED> =: ( <MATCHER> <PRE-MATCH-FORM> <POST-MATCH-FORM> <MATCH-HIGHLIGHT> {...} )\n
<MATCHER> =: <REGEXP> ;; Bounds conditioned by <PRE-MATCH-FORM> <POST-MATCH-FORM>\n
<PRE-MATCH-FORM>  =: Expression evaluated _before_ <MATCH-ANCHORED>'s <MATCHER>\n
<POST-MATCH-FORM> =: Expression evaluated _after_ <MATCH-ANCHORED>'s <MATCHER>\n
:SEE info node `(elsip)Search-based Fontification'.\n
:SEE-ALSO `mon-help-font-lock-functions', `mon-help-faces-font-lock',
`mon-help-faces-basic', `mon-help-font-lock', `mon-help-faces-themes',
`mon-help-naf-mode-faces', `mon-help-overlay-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-font-lock :insertp t)
    (mon-help-message-intrp "mon-help-font-lock")))
;;
;;; :TEST-ME (mon-help-font-lock)
;;; :TEST-ME (mon-help-font-lock t)
;;; :TEST-ME (describe-function 'mon-help-font-lock)
;;; :TEST-ME (apply 'mon-help-font-lock '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-01T19:07:24-05:00Z}#{10052} - by MON KEY>
;;;###autoload
(defun mon-help-overlay-functions (&optional insertp intrp)
  "List of functions, variables, and properties related to overlays.\n
;; :OVERLAY-FUNCTIONS
`cl-map-overlays' 
`copy-overlay'
`delete-overlay'
`make-overlay'
`move-overlay'
`remove-overlays'
`overlay-put'
`overlay-recenter'\n
;; :OVERLAY-FUNCTIONS-GETTERS
`get-char-property'
`get-char-property-and-overlay'
`overlay-get'
`overlay-properties'
;; :OVERLAY-FUNCTIONS-POSITION
`overlay-buffer'
`next-overlay-change'
`overlay-end'
`overlay-lists'
`overlay-start'
`overlayp'
`overlays-at'
`overlays-in'
`previous-overlay-change'
;; :OVERLAY-FUNCTIONS-MODIFY-NOTIFY
`restore-buffer-modified-p'
`set-buffer-modified-p'
`inhibit-modification-hooks'\n
;; :OVERLAY-FUNCTIONS-TEXT-CLONE
`text-clone-maintain'
`text-clone-create'\n
;; :OVERLAY-VARIABLES
`overlay-arrow-position'
`overlay-arrow-string'
`overlay-arrow-variable-list'\n
;; :OVERLAY-PROPERTIES            :NOTE These are in addition to the 'special' text props:
`after-string`                    :SEE info node `(elisp)Special Properties'
`before-string`                   :SEE info node `(elisp)Overlay Properties'
`evaporate`
`isearch-open-invisible`
`isearch-open-invisible-temporary`
`priority`
`window`
`text-clone-spreadp`
`text-clone-syntax`
`text-clones`\n
;; :OVERLAY-FUNCTIONS-MON-LOCAL
`mon-help-find-result-for-overlay'
`mon-help-overlay-for-example'
`mon-help-overlay-on-region'
`mon-help-overlay-result'
`mon-nuke-overlay-buffer'
`mon-get-overlays-map-props'
`mon-get-overlays-region'
`mon-get-overlays-region-map-props'
`mon-get-overlays-buffer'\n
:SEE info node `(elisp) Overlays'.
:SEE info node `(elisp)Special Properties'
:SEE info node `(elisp)Overlay Properties'\n
:SEE :FILE buffer.c :FILE textprop.c\n
:SEE-ALSO `mon-help-faces', `mon-help-font-lock',
`mon-help-font-lock-functions', `mon-help-text-property-functions',
`mon-help-text-property-properties', `mon-help-text-property-functions-ext',
`mon-help-text-property-stickyness', `mon-help-plist-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-overlay-functions :insertp t)
    (mon-help-message-intrp "mon-help-overlay-functions")))
;;
;;; :TEST-ME (mon-help-overlay-functions )
;;; :TEST-ME (mon-help-overlay-functions t)
;;; :TEST-ME (apply 'mon-help-overlay-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-20T13:46:12-04:00Z}#{09387} - by MON>
;;;###autoload
(defun mon-help-text-property-functions (&optional insertp intrp)
  "List of text-property related functions and variables.\n
   ________________________                           ______________________79.  
  |                        |                         |                        |
  | :TEXT-PROPERTY-GETTERS |                         | :TEXT-PROPERTY-SETTERS |
 _|________________________|_______              ____|________________________|
|                                  |            |                             |
| `get-text-property'              |            | `propertize'                |
| `get-char-property'              |            | `add-text-properties'       |
| `get-char-property-and-overlay'  |            | `put-text-property'         |
|__________________________________|            | `set-text-properties'       |
   _______________________                      | `alter-text-property'       |
  |                       |                     |_____________________________|
  | :TEXT-PROPERTY-SEARCH |                
 _|_______________________|______________          _________________________    
|                                        |        |                         |  
| `next-property-change'                 |        | :TEXT-PROPERTY-DESCRIBE |  
| `next-single-property-change'          |      __|_________________________|_ 
| `next-char-property-change'            |     |                              |
| `next-single-char-property-change'     |     | `describe-text-sexp'         |
| `previous-property-change'             |     | `describe-text-widget'       |
| `previous-single-property-change'      |     | `describe-property-list'     |
| `previous-char-property-change'        |     | `describe-text-category'     |
| `previous-single-char-property-change' |     | `describe-text-properties'   |
| `text-properties-at'                   |     | `describe-text-properties-1' |   
| `text-property-any'                    |     |______________________________|
| `text-property-not-all'                |        __________________________   
|________________________________________|       |                          |  
   _______________________                       | :TEXT-PROPERTY-FONT-LOCK |  
  |                       |                ______|__________________________|_ 
  | :TEXT-PROPERTY-FIELDS |               |                                   |
 _|_______________________|____           | `font-lock-append-text-property'  |
|                              |          | `font-lock-extra-managed-props'   |
| `constrain-to-field'         |          | `font-lock-fillin-text-property'  |
| `delete-field'               |          | `font-lock-prepend-text-property' |
| `field-beginning'            |          | `last-sexp-setup-props'           |
| `field-end'                  |          |___________________________________|
| `field-string'               |                   _________________________   
| `field-string-no-properties' |                  |                         |  
|______________________________|                  |  :TEXT-PROPERTY-REMOVE  |  
   _________________________         _____________|_________________________|_ 
  |                         |       |                                         |
  | :TEXT-PROPERTY-INHIBIT  |       | `filter-buffer-substring'               |
 _|_________________________|__     | `buffer-substring-no-properties'        |
|                              |    | `remove-list-of-text-properties'        |
| `with-silent-modifications'  |    | `remove-text-properties'                |
| `inhibit-point-motion-hooks' |    | `remove-yank-excluded-properties'       |
| `inhibit-field-text-motion'  |    | `insert-buffer-substring-no-properties' |      
|______________________________|    |_________________________________________|    
   _________________________________                 _______________________   
  |                                 |               |                       |  
  | :TEXT-PROPERTY-STICKY-VARIABLES |               | :TEXT-PROPERTY-STICKY |  
 _|_________________________________|_   ___________|_______________________|_ 
|                                     | |                                     |
| `text-property-default-nonsticky'   | | `insert-and-inherit'                |
| `buffer-access-fontified-property'  | | `insert-before-markers'             |
|_____________________________________| | `insert-before-markers-and-inherit' |                                         
   __________________________           |_____________________________________| 
  |                          |              ________________________________    
  | :TEXT-PROPERTY-VARIABLES |             |                                |   
 _|__________________________|________     | :TEXT-PROPERTY-STICKY-PROPERTY |   
|                                     |  __|________________________________|_  
|  `buffer-access-fontify-functions'  | |                                     | 
|  `buffer-substring-filters'         | | `front-sticky'                      | 
|  `char-property-alias-alist'        | | `rear-nonsticky'                    | 
|  `default-text-properties'          | | `insert-and-inherit`                | 
|  `inhibit-read-only'                | | `insert-before-markers-and-inherit` | 
|  `minibuffer-allow-text-properties' | | `insert-in-front-hooks`             |
|  `show-help-function'               | | `insert-behind-hooks`               |
|  `use-hard-newlines'                | |___________________________________79^
|  `yank-excluded-properties'         |                                        
|_____________________________________| :SEE info node `(elisp)Text Properties'
                                        :SEE :FILE buffer.c :FILE textprop.c   \n
:TEXT-PROPERTY-PRIDCATES                                                       
`invisible-p'\n
:TEXT-PROPERTY-MODE-LINE
`propertized-buffer-identification'\n
:SEE-ALSO `mon-help-text-property-properties',
`mon-help-text-property-stickyness', `mon-help-text-property-functions-ext',
`mon-help-overlay-functions', `mon-help-plist-functions',
`mon-help-plist-properties', `mon-help-widgets'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-text-property-functions :insertp t)
    (mon-help-message-intrp "mon-help-text-property-functions")))
;;
;;; :TEST-ME (mon-help-text-property-functions)
;;; :TEST-ME (mon-help-text-property-functions t)
;;; :TEST-ME (describe-function 'mon-help-text-property-functions)
;;; :TEST-ME (apply 'mon-help-text-property-functions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-10T13:45:45-05:00Z}#{10103} - by MON KEY>
;;;###autoload
(defun mon-help-text-property-properties (&optional insertp intrp)
  "List of standard and \"special\" text-property properties.\n
     __________________________       
    |                          |      
    | :TEXT-PROPERTIES-SPECIAL |  :SEE info node `(elisp)Special Properties'     
 ___|__________________________|______________________________________________80. 
|                                                                               |
| `keymap` `local-map`                                                          |
| `syntax-table` `syntax-multiline` `font-lock-multiline`                       |
| `category` `field`                                                            |
| `face` `font-lock-face` `fontified`                                           |
| `pointer` `mouse-face`                                                        |
| `insert-in-front-hooks` `insert-behind-hooks` `modification-hooks`            |
| `isearch-open-invisible` `isearch-open-invisible-temporary`                   |
| `line-height` `line-spacing`                                                  |
| `line-prefix` `wrap-prefix`                                                   |
| `point-entered` `point-left`                                                  |
| `before-string` `after-string`                                                |
| `read-only`                            :NOTE On error sigals `text-read-only` |
| `composition` `intangible` `invisible` :NOTE These and `display` move point.  |
|            ____________________________________                               |
|           |                                    |                              |
|           | :TEXT-PROPERTIES-SPECIAL-HELP-ECHO |                              |
|  _________|____________________________________|____________________________  |
| |                                                                           | |
| | `help-echo` [ <STRING> | <FUNCTION> | <FORM> ]                            | |
| |             ¦ <FUNCTION> with args <WINDOW>, <OJBECT>, <POSITION>         | |
| |             ¦ <WINDOW>   <-  <WINDOW-NAME>  ;e.g. (selected-window)       | |
| |             ¦ <OBJECT>   <- [<BUFFER-NAME>|<OVERLAY-SYMBOL>|<STRING>]     | |
| |             ¦ <POSITION> <- [<BUFFER-POSN>|<OVERLAY-POSN>|<STRING-POSN>]  | |
| |___________________________________________________________________________| |
| :SEE info node `(emacs)Tooltips'                                              |
|            __________________________________      _________________________  |
|           |                                  |    |                         | |
|           | :TEXT-PROPERTIES-SPECIAL-DISPLAY |    | :TEXT-PROPERTIES-STICKY | |
|  _________|__________________________________|__  |_________________________| |
| |                                               | |                         | |
| | `display`  <STRING>                           | |      `front-sticky`     | |
| |           ¦ (image . <IMAGE-PROPS>)           | |     `rear-nonsticky`    | |
| |           ¦ (slice X Y <WIDTH> <HEIGHT>)      | |_________________________| |
| |           ¦ ((margin nil) <STRING>)           |  _________________________  |
| |           ¦ (space-width <FACTOR>)            | |                         | |
| |           ¦ (height [(+ N)|(- N)|             | | :TEXT-PROPERTIES-FORMAT | |
| |           ¦        <NUMBER>|<SYMBOL>|<FORM>]) | |_________________________| |
| |           ¦ (raise <FACTOR>                   | |                         | |
| |_______________________________________________| |        `hard`           | |
|  :SEE info node `(elisp)Display Property'         |   `right-margin`        | |
|            __________________________________     |    `left-margin`        | |   
|           |                                  |    |   `justification`       | |  
|           | :TEXT-PROPERTIES-BUTTON-DEFAULT  |    |_________________________| |  
|  _________|__________________________________|__                              |
| |                                               |                             |
| | `action`        <FUNCTION>                    |                             |
| | `mouse-action`  <FUNCTION>                    |                             |
| | `help-echo`     { <STRING>  | <FUNCTION> }    |                             |
| | `face`          <FACE>                        |                             |
| | `mouse-face`    <FACE>                        |                             |
| | `keymap`        <KEYMAP>                      |                             |
| | `type`          { :type :supertype }          |                             |
| | `follow-link`   <BOOLEAN>                     |                             |
| | `button`        <BOOLEAN>                     |                             |
| | `evaporate`     <BOOLEAN>                     |                             |
| | `rear-nonsticky <BOOLEAN>                     |                             |
| | `button-category-symbol`                      |                             |
| | :NOTE \(symbol-plist 'default-button\)        |                             |
| |_______________________________________________|                             |
| :SEE info node `(elisp)Buttons'                                               |
| :SEE info node (elisp)Defining Clickable Text'                                |
|_______________________________________________________________________________|                                                                                                                                           80^\n
:SEE-FILE mon-text-property-utils.el src/textprop.c
:SEE-ALSO `mon-help-text-property-functions', `mon-help-overlay-functions',
`mon-help-text-property-functions-ext', `mon-help-text-property-stickyness',
`mon-help-widgets', `mon-help-button-functions' `mon-help-plist-functions',
`mon-help-plist-properties', `mon-help-face-functions', `mon-help-faces-basic',
`mon-help-faces-font-lock', `mon-help-faces-themes',
`mon-help-font-lock-functions', `mon-help-syntax-functions',
`mon-help-syntax-class'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-text-property-properties :insertp t)
    (mon-help-message-intrp "mon-help-text-property-properties")))
;;
;;; :TEST-ME (mon-help-text-property-properties )
;;; :TEST-ME (mon-help-text-property-properties t)

 
;;; ==============================
;;; :COURTESY :FILE /emacs/*/src/intervals.c
;;; :CREATED <Timestamp: #{2009-08-25T18:33:48-04:00Z}#{09352} - by MON KEY>
;;;###autoload
(defun mon-help-text-property-stickyness (&optional insertp intrp)
  "List of text property stickyness functions, variables, and their usage.\n
;; :TEXT-PROPERTY-STICKYNESS-USAGE\n
Any property might be front-sticky on the left, rear-sticky on the left,
front-sticky on the right, or rear-sticky on the right; the 16 combinations
can be arranged in a matrix with rows denoting the left conditions and
columns denoting the right conditions:
      _  __  _
_     FR FR FR FR
FR__   0  1  2  3
 _FR   4  5  6  7
FR     8  9  A  B
  FR   C  D  E  F\n
:LEFT-PROPS  = '\(front-sticky \(p8 p9 pa pb pc pd pe pf\)
                rear-nonsticky \(p4 p5 p6 p7 p8 p9 pa pb\)
                p0 L p1 L p2 L p3 L p4 L p5 L p6 L p7 L
                p8 L p9 L pa L pb L pc L pd L pe L pf L\)\n
:RIGHT-PROPS = '\(front-sticky \(p2 p3 p6 p7 pa pb pe pf\)
                rear-nonsticky \(p1 p2 p5 p6 p9 pa pd pe\)
                p0 R p1 R p2 R p3 R p4 R p5 R p6 R p7 R
                p8 R p9 R pa R pb R pc R pd R pe R pf R\)\n
We inherit from whoever has a sticky side facing us.  If both sides
do \(cases 2, 3, E, and F\), then we inherit from whichever side has a
non-nil value for the current property.  If both sides do, then we take
from the left.\n
When we inherit a property, we get its stickiness as well as its value.
So, when we merge the above two lists, we expect to get this:\n
:RESULT      = '\(front-sticky \(p6 p7 pa pb pc pd pe pf\)
     	        rear-nonsticky \(p6 pa\)
                p0 L p1 L p2 L p3 L p6 R p7 R
                pa R pb R pc L pd L pe L pf L\)\n
The optimizable special cases are:
    left rear-nonsticky = nil, right front-sticky = nil \(inherit left\)
    left rear-nonsticky = t,   right front-sticky = t   \(inherit right\)
    left rear-nonsticky = t,   right front-sticky = nil \(inherit none\)\n
:NOTE `insert-char's optional arg INHERIT which allows inheriting
text-properties of adjoining text with sticky properties.\n
:SEE :FILE buffer.c textprop.c\n
:SEE-ALSO `mon-help-text-property-functions', `mon-help-text-property-functions-ext',
`mon-help-overlay-functions', `mon-help-plist-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-text-property-stickyness :insertp t)
    (mon-help-message-intrp "mon-help-text-property-stickyness")))
;;
;;; :TEST-ME (mon-help-text-property-stickyness)
;;; :TEST-ME (mon-help-text-property-stickyness t)
;;; :TEST-ME (describe-function 'mon-help-text-property-stickyness)
;;; :TEST-ME (apply 'mon-help-text-property-stickyness '(t))

;;; ==============================
;;; :NOTE Emacs lisp and Scheme suffer a similiar affliction which requires
;;; constant reinvention/reimplementation of common idioms. While the reasons
;;; for this are slightly different the outcome is the same.
;;; Emacs' lacks _primitive_ lexical scoping and has no _formal_ spec. 
;;; Scheme has both of the abobe, but being a lisp 1 can't get em right...
;;; For example of the later, consider Scheme's treatment of `call/cc' with
;;; `dynamic-wind' e.g. the `unwind-protect' problem.
;;; :SEE (URL `http://www.nhplace.com/kent/PFAQ/unwind-protect-vs-continuations-original.html')
;;; :SEE (URL `http://list.cs.brown.edu/pipermail/plt-scheme/2009-July/034408.html')
;;; :SEE (URL `http://mumble.net/~campbell/blag.txt')
;;; For example of the former :SEE `mon-help-text-property-functions-ext'

 
;;; ==============================
;;; :TODO Add the third party package functions that also manipulate tp.
;;; e.g. slime  erc, gnus, w3m, etc. 
;;; Also, what about provided functions e.g. from :FILE font-lock.el
;;; :CREATED <Timestamp: #{2010-02-04T13:03:36-05:00Z}#{10054} - by MON KEY>
;;;###autoload
(defun mon-help-text-property-functions-ext (&optional insertp intrp)
  "Text property related functions that are either not C level builtins,
provided in Emacs packages outside of lisp/emacs-lisp, and 3rd party packages.\n
;; :TEXT-PROPERTY-FUNCTIONS-FILE.lisp.facemenu.el
`facemenu-set-invisible'
`facemenu-set-intangible'
`facemenu-set-read-only'
`facemenu-remove-all'
`facemenu-remove-face-props'
`facemenu-remove-special'
;; :TEXT-PROPERTY-FUNCTIONS-FILE.lisp.font-lock
`font-lock-prepend-text-property'
`font-lock-append-text-property'
`font-lock-fillin-text-property'\n
;; :TEXT-PROPERTY-FUNCTIONS-FILE.composite.el
`compose-char'
`compose-string'
`compose-string-internal'
`decompose-string'\n
;; :TEXT-PROPERTY-FUNCTIONS-FILE.lisp.gnus
`gnus-put-text-property'
`gnus-put-text-property-excluding-characters-with-faces'
`gnus-put-text-property-excluding-newlines'
`gnus-remove-text-with-property'
`gnus-add-text-properties-when'
`gnus-remove-text-properties-when'
`gnus-string-remove-all-properties'\n
;; :TEXT-PROPERTY-FUNCTIONS-FILE.*.slime
`slime-property-bounds'
`slime-propertize-region'\n
;; :TEXT-PROPERTY-FUNCTIONS-FILE.lisp.erc\n
;; :TEXT-PROPERTY-FUNCTIONS-FILE.*.w3m\n
;; :TEXT-PROPERTY-FUNCTIONS-MON-LOCAL
`mon-get-all-face-property-change'
`mon-get-next-face-property-change'
`mon-get-next-face-property-change-if'
`mon-get-text-properties-region-to-kill-ring'
`mon-line-test-content'
`mon-list-all-properties-in-buffer'
`mon-nuke-text-properties-buffer'
`mon-nuke-text-properties-region'
`mon-remove-single-text-property'
`mon-remove-text-property'
`mon-get-text-properties-category'\n
:SEE-ALSO `mon-help-text-property-functions',
`mon-help-text-property-stickyness', `mon-help-overlay-functions',
`mon-help-plist-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-text-property-functions-ext :insertp t)
    (mon-help-message-intrp "mon-help-text-property-functions-ext")))
;;
;;; :TEST-ME (mon-help-text-property-functions-ext )
;;; :TEST-ME (mon-help-text-property-functions-ext t)
;;; :TEST-ME (describe-function 'mon-help-text-property-functions-ext)
;;; :TEST-ME (apply 'mon-help-text-property-functions-ext nil '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-04T17:54:39-04:00Z}#{09365} - by MON KEY>
;;;###autoload
(defun mon-help-color-functions (&optional insertp intrp)
  "Color related functions.\n
;; :COLOR-FUNCTIONS
`color-distance'
`color-values'
`xw-color-values'
`tty-color-values'
`ansi-color-apply'
`ansi-color-make-color-map'\n
;; :COLOR-READERS
`read-color'
`facemenu-read-color'
`background-color-at-point'
`foreground-color-at-point'\n
;; :COLOR-SETTERS
`set-cursor-color'
`set-border-color'
`set-background-color'
`set-foreground-color'
`set-mouse-color'\n
;; :COLOR-LIST
`defined-colors'
`xw-defined-colors'
`list-colors-display'
`list-colors-duplicates'
`list-colors-print'
`x-colors'                ;<VARIABLE>
`color-name-rgb-alist'    ;<CONSTANT>
`ansi-color-map'          ;<VARIABLE>\n
;; :COLOR-ON-DISPLAY
`display-backing-store'
`display-color-cells'
`display-planes'
`display-visual-class'
`x-display-planes'
`x-display-visual-class'
`x-display-backing-store'\n
;; :COLOR-PREDICATES
`color-defined-p'
`xw-color-defined-p'
`xw-display-color-p'
`tty-display-color-p'
`display-color-p'
`display-graphic-p'
`display-grayscale-p'
`x-display-grayscale-p'\n
;; :COLOR-FILE.lisp.term.tty-colors
`tty-color-off-gray-diag'
`tty-color-approximate'
`tty-color-standard-values' 
`tty-color-values'
`tty-defined-color-alist' ;<VARIABLE>
;; :COLOR-W32
`w32-color-map'
;; :COLOR-MON-LOCAL
`mon-color-mix'
`mon-defined-colors-without-duplicates'
`mon-rgb-to-hsv'
`mon-list-colors-key'
`mon-get-ebay-css-pp'
`mon-get-ebay-css-pp-region-to-file'
`mon-get-ebay-img-css'
`mon-insert-css-colors'
`mon-cln-img-magic-hex'
\n
:NOTE Following from commented code for `color-distance' in: :FILE xfaces.c\n
 This formula is from a paper title `Colour metric' by Thiadmer Riemersma.
 Quoting from that paper:\n
   This formula has results that are very close to L*u*v* (with the
   modified lightness curve) and, more importantly, it is a more
   even algorithm: it does not have a range of colours where it
   suddenly gives far from optimal results.
:SEE (URL `http://www.compuphase.com/cmetric.htm')\n
:SEE :FILE xfaces.c xfns.c etc/rgb.txt lisp/term/common-win.el
:SEE :FILE w23fns.c struct colormap_t w32_color_map.\n
:SEE-ALSO `mon-help-color-chart', `mon-help-css-color'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-color-functions :insertp t)
    (mon-help-message-intrp "mon-help-color-functions")))
;;
;;; :TEST-ME (mon-help-color-functions t)
;;;: TEST-ME (describe-function 'mon-help-color-functions)

 
;;; ==============================
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 12:09.40 PM - by MON KEY>
;;;###autoload
(defun mon-help-color-chart (&optional insertp intrp)
  "Chart of Netscape Color Names with their Color Values.\n
       ______________
      |              |                                                          
      | :COLOR-CHART |                                                          
 _____|______________|_______________________________________________________80.
|                                                                              |
| ,-+ :NETSCAPE-NAME                                                           |
| |                   ,-+ :HEX-TRIPLET                                         |
| |                   |       ,-+ :RGB-VALUE                                   |
| |                   |       |            ,-+ :DIRECTOR-MAC-SYS-APPROX        |
| |                   |       |            |    ,-+ :DIRECTOR-WIN-SYS-APPROX   |
| |                   |       |            |    |    ,-+ :SUPERCARD-APPROX     |
| |                   |       |            |    |    |    ,-+ :HEX-APPROX      |
| |                   |       |            |    |    |    |        :RGB-APPROX |
| |                   |       |            |    |    |    |       |            |
| aliceblue           F0F8FF  240,248,255  000  000  001  FFFFFF  255,255,255  |
| antiquewhite        FAEBD7  250,235,215  001  -    002  FFFFCC  255,255,204  |
| aquamarine          7FFFD4  127,255,212  009  016  110  66FFCC  102,255,204  |
| azure               F0FFFF  240,255,255  000  000  001  FFFFFF  255,255,255  |
| beige               F5F5DC  245,245,220  001  -    002  FFFFCC  255,255,204  |
| bisque              FFE4C4  255,228,196  001  -    002  FFFFCC  255,255,204  |
| black               000000  0,0,0        255  255  256  000000  0,0,0        |
| blanchedalmond      FFEBCD  255,235,205  001  -    002  FFFFCC  255,255,204  |
| blue                0000FF  0,0,255      210  003  211  0000FF  0,0,255      |
| blueviolet          8A2BE2  138,43,226   097  097  098  9933CC  153,51,204   |
| brown               A52A2A  165,42,42    100  100  101  993333  153,51,51    |
| burlywood           DEB887  222,184,135  44   44   045  CCCC99  204,204,153  |
| cadetblue           5F9EA0  95,158,160   122  122  123  669999  102,153,153  |
| chartreuse          7FFF00  127,255,0    113  113  114  66FF00  102,255,0    |
| chocolate           D2691E  210,105,30   058  058  059  CC6633  204,102,51   |
| coral               FF7F50  255,127,80   021  023  022  FF6666  255,102,102  |
| cornflowerblue      6495ED  100,149,237  120  120  121  6699FF  102,153,255  |
| cornsilk            FFF8DC  255,248,220  001  -    002  FFFFCC  255,255,204  |
| cyan                00FFFF  0,255,255    180  001  181  00FFFF  0,255,255    |
| darkgoldenrod       B8860B  184,134,11   053  053  054  CC9900  204,153,0    |
| darkgreen           006400  0,100,0      203  201  204  006600  0,102,0      |
| darkkhaki           BDB76B  189,183,107  045  045  046  CCCC66  204,204,102  |
| darkolivegreen      556B2F  85,107,47    130  130  131  666633  102,102,51   |
| darkorange          FF8C00  255,140,0    017  019  018  FF9900  255,153,0    |
| darkorchid          9932CC  153,50,204   097  097  098  9933CC  153,51,204   |
| darksalmon          E9967A  233,150,122  015  -    016  FF9966  255,153,102  |
| darkseagreen        8FBC8F  143,188,143  080  080  081  99CC99  153,204,153  |
| darkslateblue       483D8B  72,61,139    170  170  171  333399  51,51,153    |
| darkslategray       2F4F4F  47,79,79     165  165  166  336666  51,102,102   |
| darkturquoise       00CED1  0,206,209    187  185  188  00CCCC  0,204,204    |
| darkviolet          9400D3  148,0,211    103  103  104  9900CC  153,0,204    |
| deeppink            FF1493  255,20,147   032  033  033  FF0099  255,0,153    |
| deepskyblue         00BFFF  0,191,255    186  184  187  00CCFF  0,204,255    |
| dimgray             696969  105,105,105  129  129  130  666666  102,102,102  |
| dodgerblue          1E90FF  30,144,255   156  156  157  3399FF  51,153,255   |
| firebrick           B22222  178,34,34    100  100  101  993333  153,51,51    |
| floralwhite         FFFAF0  255,250,240  000  000  001  FFFFFF  255,255,255  |
| forestgreen         228B22  34,139,34    160  160  161  339933  51,153,51    |
| gainsboro           DCDCDC  220,220,220  043  043  044  CCCCCC  204,204,204  |
| ghostwhite          F8F8FF  248,248,255  000  000  001  FFFFFF  255,255,255  |
| gold                FFD700  255,215,0    011  -    012  FFCC00  255,204,0    |
| goldenrod           DAA520  218,165,32   052  052  053  CC9933  204,153,51   |
| gray                808080  128,128,128  086  086  087  999999  153,153,153  |
| green               008000  0,128,0      197  195  198  009900  0,153,0      |
| greenyellow         ADFF2F  173,255,47   076  076  077  99FF33  153,255,51   |
| honeydew            F0FFF0  240,255,240  000  000  001  FFFFFF  255,255,255  |
| hotpink             FF69B4  255,105,180  019  021  020  FF66CC  255,102,204  |
| indianred           CD5C5C  205,92,92    057  057  058  CC6666  204,102,102  |
| ivory               FFFFF0  255,255,240  000  000  001  FFFFFF  255,255,255  |
| khaki               F0E68C  240,230,140  002  244  003  FFFF99  255,255,153  |
| lavender            E6E6FA  230,230,250  000  000  001  FFFFFF  255,255,255  |
| lavenderblush       FFF0F5  255,240,245  000  000  001  FFFFFF  255,255,255  |
| lawngreen           7CFC00  124,252,0    113  113  114  66FF00  102,255,0    |
| lemonchiffon        FFFACD  255,250,205  001  -    002  FFFFCC  255,255,204  |
| lightblue           ADD8E6  173,216,230  078  078  079  99CCFF  153,204,255  |
| lightcoral          F08080  240,128,128  014  240  015  FF9999  255,153,153  |
| lightcyan           E0FFFF  224,255,255  036  036  037  CCFFFF  204,255,255  |
| lightgoldenrod      EEDD82  238,221,130  008  -    009  FFCC99  255,204,153  |
| lightgldnrodyellow  FAFAD2  250,250,210  001  -    002  FFFFCC  255,255,204  |
| lightgray           D3D3D3  211,211,211  043  043  044  CCCCCC  204,204,204  |
| lightpink           FFB6C1  255,182,193  007  -    008  FFCCCC  255,204,204  |
| lightsalmon         FFA07A  255,160,122  015  -    016  FF9966  255,153,102  |
| lightseagreen       20B2AA  32,178,170   160  160  161  339933  51,153,153   |
| lightskyblue        87CEFA  135,206,250  078  078  079  99CCFF  153,204,255  |
| lightslate          8470FF  132,112,255  090  090  091  9966FF  153,102,255  |
| lightslategray      778899  119,136,153  122  122  123  669999  102,153,153  |
| lightsteelblue      B0C4DE  176,196,222  078  078  079  99CCFF  153,204,255  |
| lightyellow         FFFFE0  255,255,224  000  000  001  FFFFFF  255,255,255  |
| limegreen           32CD32  50,205,50    154  154  155  33CC33  51,204,51    |
| linen               FAF0E6  250,240,230  000  000  001  FFFFFF  255,255,255  |
| magenta             FF00FF  255,0,255    030  031  031  FF00FF  255,0,255    |
| maroon              B03060  176,48,96    107  107  108  990000  153,0,0      |
| mediumaquamarine    66CDAA  102,205,170  116  116  117  66CC99  102,204,153  |
| mediumblue          0000CD  0,0,205      211  208  212  0000CC  0,0,204      |
| mediumorchid        BA55D3  186,85,211   055  055  056  CC66CC  204,102,204  |
| mediumpurple        9370DB  147,112,219  091  091  092  9966CC  153,102,204  |
| mediumseagreen      3CB371  60,179,113   153  153  154  33CC66  51,204,102   |
| mediumslateblue     7B68EE  123,104,238  11126126  127  6666FF  102,102,255  |
| mediumspringgreen   00FA9A  0,250,154    182  181  183  00FF99  0,255,153    |
| mediumturquoise     48D1CC  72,209,204   15   151  152  33CCCC  51,204,204   |
| mediumviolet        C71585  199,21,133   068  068  069  CC0099  204,0,153    |
| midnightblue        191970  25,25,112    213  210  214  000066  0,0,102      |
| mintcream           F5FFFA  245,255,250  000  000  001  FFFFFF  255,255,255  |
| mistyrose           FFE4E1  255,228,225  000  000  001  FFFFFF  255,255,255  |
| moccasin            FFE4B5  255,228,181  007  -    008  FFCCCC  255,204,204  |
| navajowhite         FFDEAD  255,222,173  009  -    009  FFCC99  255,204,153  |
| navy                000080  0,0,128      212  209  213  000099  0,0,153      |
| oldlace             FDF5E6  253,245,230  000  000  001  FFFFFF  255,255,255  |
| olivedrab           6B8E23  107,142,35   124  124  125  669933  102,153,51   |
| orange              FFA500  255,165,0    017  019  018  FF9900  255,153,0    |
| orangered           FF4500  255,69,0     029  002  030  FF3300  255,51,0     |
| orchid              DA70D6  218,112,214  055  055  056  CC66CC  204,102,204  |
| palegoldenrod       EEE8AA  238,232,170  002  244  003  FFFF99  255,255,153  |
| palegreen           98FB98  152,251,152  074  074  075  99FF99  153,255,153  |
| paleturquoise       AFEEEE  175,238,238  072  072  073  99FFFF  153,255,255  |
| palevioletred       DB7093  219,112,147  056  056  057  CC6699  204,102,153  |
| papayawhip          FFEFD5  255,239,213  001  -    002  FFFFCC  255,255,204  |
| peachpuff           FFDAB9  255,218,185  007  -    008  FFCCCC  255,204,204  |
| peru                CD853F  205,133,63   052  052  053  CC9933  204,153,51   |
| pink                FFC0CB  255,192,203  007  -    008  FFCCCC  255,204,204  |
| plum                DDA0DD  221,160,221  049  049  050  CC99CC  204,153,204  |
| powderblue          B0E0E6  176,224,230  078  078  079  99CCFF  153,204,255  |
| purple              A020F0  160,32,240   096  096  097  9933FF  153,51,255   |
| red                 FF0000  255,0,0      035  035  036  FF0000  255,0,0      |
| rosybrown           BC8F8F  188,143,143  050  050  051  CC9999  204,153,153  |
| royalblue           4169E1  65,105,225   163  163  164  3366CC  51,102,204   |
| saddlebrown         8B4513  139,69,19    101  101  102  993300  153,51,0     |
| salmon              FA8072  250,128,114  015  -    016  FF9966  255,153,102  |
| sandybrown          F4A460  244,164,96   015  -    016  FF9966  255,153,102  |
| seagreen            2E8B57  46,139,87    159  159  160  339966  51,153,102   |
| seashell            FFF5EE  255,245,238  000  000  001  FFFFFF  255,255,255  |
| sienna              A0522D  160,82,45    094  094  095  996633  153,102,51   |
| skyblue             87CEEB  135,206,235  078  078  079  99CCFF  153,204,255  |
| slateblue           6A5ACD  106,90,205   127  127  128  6666CC  102,102,204  |
| slategray           708090  112,128,144  086  086  087  999999  153,153,153  |
| snow                FFFAFA  255,250,250  000  000  001  FFFFFF  255,255,255  |
| springgreen         00FF7F  0,255,127    183  182  184  00FF66  0,255,102    |
| steelblue           4682B4  70,130,180   157  157  158  3399CC  51,153,204   |
| tan                 D2B48C  210,180,140  044  044  045  CCCC99  204,204,153  |
| thistle             D8BFD8  216,191,216  043  043  044  CCCCCC  204,204,204  |
| tomato              FF6347  255,99,71    022  024  023  FF6633  255,102,51   |
| turquoise           40E0D0  64,224,208   151  151  152  33CCCC  51,204,204   |
| violet              EE82EE  238,130,238  012  -    013  FF99FF  255,153,255  |
| violetred           D02090  208,32,144   062  062  063  CC3399  204,51,153   |
| wheat               F5DEB3  245,222,179  007  -    008  FFCCCC  255,204,204  |
| white               FFFFFF  255,255,255  000  000  001  FFFFFF  255,255,255  |
| whitesmoke          F5F5F5  245,245,245  000  000  001  FFFFFF  255,255,255  |
| yellow              FFFF00  255,255,0    005  004  006  FFFF00  255,255,0    |
| yellowgreen         9ACD32  154,205,50   082  082  083  99CC33  153,204,51   |
|____________________________________________________________________________80^
\n:COURTESY Tay Vaughan, July, 1996. Timestream, Inc.\n
:SEE \(URL `http://www.timestream.com/mmedia/graphics/colors/ns3names.txt'\).
:SEE :FILE w23fns.c struct colormap_t w32_color_map.\n
:SEE-ALSO `mon-help-color-functions',`mon-help-css-color'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-color-chart :insertp t)
    (mon-help-message-intrp "mon-help-color-chart")))
;;
;;; :TEST-ME (mon-help-color-chart)
;;; :TEST-ME (mon-help-color-chart t)
;;; :TEST-ME (describe-function 'mon-help-color-chart)
;;; :TEST-ME (apply 'mon-help-color-chart '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Wednesday June 17, 2009 @ 05:36.08 PM - by MON KEY>
;;;###autoload
(defun mon-help-easy-menu (&optional insertp intrp)
  "Following is a mapping for building a menu with easy-menu's `easy-menu-define'.\n
    ____________
   |            |                                                              
   | :EASY-MENU |                                                               
 __|____________|____________________________________________________________79.
|                                                                              |
|                      ,-+ :MENU-ROOT                                          |
| [SYMBOL MAPS DOC MENU                                                        |
.                  |                      ,-+ :MENU-CHILD                      |
.                  + (NAME CALLBACK ENABLE                                     |
.                  | ...... NAME ;string                                       |
.                  | ...... CALLBACK ;<COMMAND>|<LIST>                         |
.                  | ...... ENABLE ;<EXPRESSION>                               |
.                  :        | ... :filter . FUNCTION ;<FUNCTION>               |
.                  :        | ... :visible . INCLUDE ;<EXPRESSION>             |
.                  :        | ... :active . ENABLE ;<EXPRESSION>               |
.                  |  )                                                        |
.                  :__________.                      ,-+ :MENU-CHILD-ELEMENTS  |
.                             | [NAME CALLBACK ENABLE                          |
.                             |___.                                            |
.                             :   | ... :filter . FUNCTION ;<FUNCTION>         |
.                             :   | ... :visible . INCLUDE ;<EXPRESSION>       |
.                             :   | ... :active . ENABLE ;<EXPRESSION>         |
.                             :   | ... :label . FORM ;<EXPRESSION>            |
.                             :   | ... :keys . KEYS ;<STRING>                 |
.                             :   | ... :key-sequence . KEYS ;<STRING>|<VECTOR>|
.                             :   | ... :help . HELP ; <STRING>                |
.                             :   | ... :selected . SELECTED ;<EXPRESSION>     |
.                             :   | ... :style . STYLE ;<SYMBOL>               |
.                             :   :            |... toggle: radio: button:     |
.                             | ]                                              |
| ]                                                                            |
|____________________________________________________________________________79.\n
:SEE info node `(elisp)Defining Menus'\n
:SEE-ALSO `mon-help-custom-keywords', `mon-help-widgets',
`mon-help-plist-functions', `mon-help-color-chart',
`mon-help-faces', `mon-help-faces-basic', `mon-help-faces-themes',
`mon-help-eieio-defclass', `mon-help-eieio-functions',
`mon-help-eieio-methods'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-easy-menu :insertp t)
    (mon-help-message-intrp "mon-help-easy-menu")))
;;
;;; :TEST-ME (mon-help-easy-menu)
;;; :TEST-ME (mon-help-easy-menu t)
;;; :TEST-ME (describe-function 'mon-help-easy-menu)
;;; :TEST-ME (apply 'mon-help-easy-menu '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Friday June 19, 2009 @ 02:20.35 PM - by MON KEY>
;;;###autoload
(defun mon-help-widgets (&optional insertp intrp)
  "Help table for the widget interface.\n
 __________________________                                                  
|                          | :SEE info node `(widget)Introduction'           
| :WIDGET-TYPE-SYNTAX-OF   | :SEE info node `(elisp)Abstract Display'        
|__________________________|______________________________________________77.
|                                                                           |
| NAME ::= (NAME [KEYWORD ARGUMENT] ... ARGS)                               |
|            |       |        |          |                                  |
|      widget-name   |        |          + widget-specific                  |
|                    |        + prop-val                                    |
|                    |                                                      |
|    .---------------+ prop-key                                             |
|    |                                                                      |
|    |--+ format                                                            |
|    |                                                                      |
|    |   `%[  %]' | `%{  %}' |  `%v',   `%d', `%h', `%t', `%%'              |
|    |      ^          ^                  ^     ^     ^                     |
|    |......¦..........¦..................¦.....¦.....¦                     |
|           |          |                  |     |     |                     |
|           |          |--+ :sample-face  ._____.     |-+ :tag | :tag-glyph |
|           |                                |                              |
|           |--+ :button-face                |--+ :doc                      |
|                                            |                              |
|                                            |--+ :documentation-property   |
|--+ :value          ;init-arg                                              |
|                                                                           |
|--+ :button-prefix  ;nil | <STRING> | <SYMBOL>                             |
|                                                                           |
|--+ :button-suffix  ;nil | <STRING> | <SYMBOL>                             |
|                                                                           |
|--+ :help-echo      ; <STRING> | [ <FUNCTION> Arg ] | [ widget <STRING> ]  |
|                                                                           |
|--+ :follow-link    ;<mouse-1>                                             |
|                                                                           |
|--+ :indent         ;<INTEGER>                                             |
|                                                                           |
|--+ :offset         ;<INTEGER>                                             |
|                                                                           |
|--+ :extra-offset   ;<INTEGER>                                             |
|                                                                           |
|--+ :notify         ; [ <FUNCTION> arg1 &optional arg2 ]                   |
|                                                                           |
|--+ :menu-tag       ; :tag                                                 |
|                                                                           |
|--+ :menu-tag-get   ;[ <FUNCTION> ( :menu-tag | :tag | :value ) ]          |
|                                                                           |
|--+ :match          ;[ widget value ]                                      |
|                                                                           |
|--+ :validate       ;widget _._ `widget-children-validate'_                |
|                             |                             |               |
|                             |                             |--+ :children  |
|                             |--+ :error ;<STRING>                         |
|                                                                           |
|--+ :tab-order      ;{ `widget-forward' | `widget-backward' }              |
|                                                                           |
|--+ :parent         ;{ menu-choice item | editable-list element }          |
|                                                                           |
|--+ :sibling-args   ;{ radio-button-choice checklist }                     |
|___________________________________________________________________________|
|  __________________   ___________________       ____________________      |
| |                  | |                   |     |                    |     |
| | :WIDGET-BUTTONS  | | :WIDGET-FUNCTIONS |     | :WIDGET-NAVIGATION |     |
| |__________________| |___________________|  ___|____________________|___  |
| |                  | |                   | |                            | |
| | Option | Field   | | `widget-value'    | | <TAB> | M-<TAB> | S-<TAB>  | |
| |                  | | `widget-create'   | |    -------------------     | |
| |  [INS] | [DEL]   | | `widget-delete'   | |     `widget-forward'       | |
| |                  | | `widget-insert'   | |     `widget-backward'      | |
| |    [ ] | [X]     | | `widget-setup'    | |    -------------------     | |
| |                  | | `widget-get'      | |____________________________| |
| |    Embedded      | | `widget-put'      |        ____________________    |
| |                  | |___________________|       |                    |   |
| |    ( ) | (*)     |     _______________         | :WIDGET-BUTTON-ACT |   |
| |                  |    |               |      __|____________________|_  |
| |  [Apply Form]    |    | :WIDGET-FACES |     |                         | |
| |                  |   _|_______________|__   |     <RET> | Mouse-2     | |
| |  [Reset Form]    |  |                    |  |  ---------------------  | |
| |__________________|  | widget-mouse-face  |  |  `widget-button-press'  | |
|  ___________________  | widget-field-face  |  |  `widget-button-click'  | |
| |                   | | widget-button-face |  |  ---------------------  | |
| | :WIDGET-VARIABLES | |____________________|  |_________________________| |
| |___________________|______________________      _______________          |
| |                                          |    |               |         |
| |  `widget-keymap'                         |    | :WIDGET-LIKE  |         |
| |  `widget-global-map'                     |   _|_______________|__       |
| |  `widget-glyph-directory'  ;<DIRECTORY>  |  |                    |      |
| |  `widget-glyph-enable'     ;<BOOLEAN>    |  |   `x-popup-menu'   |      |
| |  `widget-button-prefix'    ;<STRING>     |  |      `imenu'       |      |
| |  `widget-button-suffix'    ;<STRING>     |  |     `speedbar'     |      |
| |__________________________________________|  |____________________|      |
|  _______________________________                                          |
| |                               |                                         |
| | :WIDGET-PLIST-PROP-ARGS-OTHER |                                         |
| |_______________________________|__________                               |
| |                                          |                              |
| | :action              :activate           |                              |
| | :active              :inline             |                              |
| | :always-active       :match              |                              |
| | :args                :match-inline       |                              |
| | :button-face-get     :menu-tag           |                              |
| | :buttons             :mouse-down-action  |                              |
| | :complete            :mouse-face         |                              |
| | :complete-function   :mouse-face-get     |                              |
| | :convert-widget      :mouse-face-get     |                              |
| | :copy                :notify             |                              |
| | :create              :prompt-value       |                              |
| | :deactivate          :sample-face-get    |                              |
| | :default-get         :sample-overlay     |                              |
| | :delete              :to                 |                              |
| | :doc                 :type               |                              |
| | :doc-overlay         :validate           |                              |
| | :field-overlay       :value-create       |                              |
| | :format              :value-delete       |                              |
| | :format-handler      :value-inline       |                              |
| | :from                :value-set          |                              |
| | :inactive            :value-to-external  |                              |
| | :indent              :value-to-internal  |                              |
| |__________________________________________|                              |
|_________________________________________________________________________77^\n
:SEE :FILE wid-edit.el button.el
:SEE-ALSO `mon-help-easy-menu', `mon-help-key-functions',
`mon-help-custom-keywords', `mon-help-plist-functions', `mon-help-color-chart',
`mon-help-faces', `mon-help-faces-basic', `mon-help-faces-themes',
`mon-help-eieio-defclass', `mon-help-eieio-functions',
`mon-help-eieio-methods'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-widgets :insertp t)
    (mon-help-message-intrp "mon-help-widgets")))
;;
;;; :TEST-ME (mon-help-widgets)
;;; :TEST-ME (mon-help-widgets t)
;;; :TEST-ME (describe-function 'mon-help-widgets)
;;; :TEST-ME (apply 'mon-help-widgets '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-26T13:00:54-05:00Z}#{10085} - by MON KEY>
;;;###autoload
(defun mon-help-custom-keywords (&optional insertp intrp)
  "Keywords used with defcustom, degroup, customize facilities.\n
;; :CUSTOM-COMMON-KEYWORDS
:link 
                 ¦ \(custom-manual        \"\(info-node\)Section\"\)
                 ¦ \(info-link            \"\(info-node\)Section\"\)
                 ¦ \(url-link              <STRING>\)
                 ¦ \(emacs-commentary-link <STRING>\)
                 ¦ \(emacs-library-link    <STRING>\)
                 ¦ \(file-link             <STRING>\)
                 ¦ \(function-link         <STRING>\)
                 ¦ \(file-link             <STRING>\)
                 ¦ \(variable-link         <VARIABLE>\)
                 ¦ \(custom-group-link     <SYMBOL>\)\n
:tag             <STRING>
:group           <SYMBOL>
:load            <STRING>
:require         <SYMBOL>
:version         <STRING>
:prefix          <STRING>              ; :NOTE `defgroup' only
:package-version \(<SYMBOL> . <STRING>\)\n
;; :DEFCUSTOM-KEYWORDS
:options         <LIST>
:set             <SYMBOL>
:get             <SYMBOL>
:risky           <PROPERTY-VALUE>
:safe            <PROPERTY-VALUE>
:set-after       <SYMBOL>
:intialize       <SYMBOL>
                 ¦ `custom-initialize-set'
                 ¦ `custom-initialize-default'
                 ¦ `custom-initialize-reset'
                 ¦ `custom-initialize-changed'
                 ¦ `custom-initialize-safe-set'
                 ¦ `custom-initialize-safe-default'\n
;; :CUSTOM-TYPES-SIMPLE
:type            <TYPE>
                 ¦ sexp           <FORM>
                 ¦ integer        <INTEGER>
                 ¦ number         <NUMBER> 
                 ¦ float          <FLOAT>
                 ¦ symbol         <SYMBOL>
                 ¦ string         <STRING>
                 ¦ regexp         <STRING>
                 ¦ character      <CHAR-CODE>
                 ¦ file           <STRING>
                 ¦ directory      <STRING>
                 ¦ hook           <LIST>
                 ¦ alist          <CONSED-LIST>
                 ¦ plist          <KEY-VALUE-PAIR>
                 ¦ function       <SYMBOL>
                 ¦ variable       <SYMBOL>
                 ¦ face           <SYMBOL>
                 ¦ boolean        <T-OR-NIL>
                 ¦ coding-system  <SYMBOL>
                 ¦ color          <STRING>|<RGB-HEX-VALUE>\n
;; :CUSTOM-TYPES-CONSTRUCTORS
list             <ELT-TYPES>
group            <ELT-TYPES>
vector           <ELT-TYPES>
radio            <ELT-TYPES>
choice           <ALT-TYPES>
const            <VAL-TYPEs>
other            <VAL-TYPES>
function-item    <FUNCTION>
cons             \(<CAR-TYPE> <CDR-TYPE>\)\n
;; :CUTSOM-USAGE
An alist of conses uses this form:\n
 :type '(alist :key-type integer :value-type string)\n
An alist that is not a cons uses one of the forms:\n
 :type '\(repeat \(group integer string\)\)\n
 :type '\(repeat \(list integer string\)\)\n
To link to a URL do:\n
 :link '(url-link :tag \"A search-engine\" \"http://www.google.com\"\)\n
To link to a file do:
 :link '(file-link :tag \"an interesting file\" \"~/.emacs\"\)\n
To link to an existing file of emacs lisp source code do:\n 
 :link '\(emacs-library-link  \"subr.el\"\)\n

:SEE info node `(elisp)Common Keywords'\n
:SEE info node `(elisp)Customization Types'\n
:SEE-ALSO `mon-help-widgets', `mon-help-easy-menu', `mon-help-plist-functions',
`mon-help-color-chart', `mon-help-key-functions', `mon-help-package-keywords',
`mon-help-faces', `mon-help-faces-basic', `mon-help-faces-themes',
`mon-help-eieio-defclass', `mon-help-eieio-functions',
`mon-help-eieio-methods'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-custom-keywords :insertp t)
    (mon-help-message-intrp "mon-help-custom-keywords")))
;;
;;; :TEST-ME (mon-help-custom-keywords)
;;; :TEST-ME (mon-help-custom-keywords t)
;;; :TEST-ME (describe-function 'mon-help-custom-keywords)
;;; :TEST-ME (apply mon-help-custom-keywords '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-25T17:33:41-05:00Z}#{10084} - by MON KEY>
;;;###autoload
(defun mon-help-char-functions (&optional insertp intrp)
  "A list of functions and vars related to chars and their properties.\n
;; :CHAR-CONVERSION
`char-to-string'
`clear-string'
`decode-char'
`encode-char'
`get-byte'
`make-char'
`multibyte-char-to-unibyte'
`split-char'
`string-to-char'             
`string-to-multibyte'
`subst-char-in-region'
`subst-char-in-string'
`unibyte-char-to-multibyte'\n
;; :CHAR-PREDICATES
`char-equal'
`char-displayable-p'
`char-valid-p'
`charsetp'
`char-table-p'
`characterp'
`char-or-string-p'
`buffer-chars-modified-tick'\n
;; :CHAR-POSITION-MOTION
`skip-chars-forward'
`skip-chars-backward'
`insert-char'
`get-file-char'
`backward-char'
`forward-char'
`following-char'
`preceding-char'
`char-before'
`char-after'\n
;; :CHAR-READERS                    
`read-char'
`read-char-choice'
`read-char-exclusive'
`read-char-by-name'
`read-quoted-char'                 
`read-quoted-char-radix' ;<VARIABLE>
`quoted-insert'
`ucs-insert'\n
;; :CHAR-DESCRIPTORS
`decode-char'
`split-char'
`max-char'                          
`text-char-description'     
`single-key-description'
`describe-char'
`describe-char-categories'
`describe-char-unicode-data' ;<DEPRECATED> :FILE descr-text.el
`describe-char-display'
`describe-char-padded-string'
`internal-describe-syntax-value'\n
;; :CHAR-ENCODING            :SEE `mon-help-char-coding-functions'
`auto-coding-alist'
`find-file-literally'
`recode-file-name'
`insert-file-contents-literally'
`insert-file-literally'\n
;; :CHAR-SEARCHING
`internal-char-font'
`search-unencodable-char'
`unencodable-char-position'
`find-multibyte-characters'
`find-auto-coding'\n
;; :CHAR-PROPERTY-FUNCTIONS 
`char-category-set'         ; :NOTE \(describe-char-categories \(char-category-set 33\)\)
`charset-plist'
`char-code-property-description'
`compose-chars'
`compose-string'
`compose-string-internal'
`decompose-string'
`define-char-code-property'
`get-char-code-property'
`get-char-property'
`get-char-property-and-overlay'
`put-char-code-property'
`char-code-property-alist'         ;<VARIABLE>
`char-code-property-documentation` ;<PROPERTY>\n
;; :CHAR-PROPERTIES-OF         
`bidi-class`                ; `mon-help-char-unidata-table' :BIDI-CLASS 
`canonical-combining-class` ; `mon-help-char-unidata-table' :CANONICAL-COMBINING-CLASS
`general-category`          ; `mon-help-char-unidata-table' :GENERAL-CATEGORY
`decimal-digit-value`       ; :SEE :FILE lisp/international/uni-decimal.el
`lowercase`                 ; :SEE :FILE lisp/international/uni-lowercase.el
`mirrored`                  ; :SEE :FILE lisp/international/uni-mirrored.el
`titlecase`                 ; :SEE :FILE lisp/international/uni-titlecase.el
`uppercase`                 ; :SEE :FILE lisp/international/uni-uppercase.el
`decomposition`
`digit`
`iso-10646-comment`
`name`
`numeric-value`
`old-name`\n
:SEE info node `(elisp)Character Properties'\n
:SEE info node `(elisp)Basic Char Syntax'\n
:SEE :FILE src/chartab.c src/charset.c admin/unidata-gen.el\n
:SEE-ALSO `mon-help-char-coding-functions', `mon-help-char-charset-functions',
`mon-help-display-table-functions', `mon-help-char-table-functions',
`mon-help-char-representation', `mon-help-binary-representation',
`mon-help-char-raw-bytes', `mon-help-char-composition', `mon-help-diacritics',
`mon-help-char-ascii', `mon-help-char-iso-8859-1', `mon-help-char-ecma-35',
`mon-help-char-ecma-48', `mon-help-read-functions', `mon-help-print-functions',
`mon-help-key-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-functions :insertp t)
    (mon-help-message-intrp "mon-help-char-functions")))
;;
;;; :TEST-ME (mon-help-char-functions)
;;; :TEST-ME (mon-help-char-functions t)
;;; :TEST-ME (describe-function 'mon-help-char-functions)
;;; :TEST-ME (apply 'mon-help-char-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2090
;;; :CREATED <Timestamp: #{2010-08-30T11:13:08-04:00Z}#{10351} - by MON KEY>
;;;###autoload
(defun mon-help-char-charset-functions (&optional insertp intrp)
  "A list of functions for working with Emacs characters and charsets.\n
;; :CHAR-CHARSET-HANDLERS-CHAR
`make-char'
`decode-char'
`encode-char'
`split-char'
`map-charset-chars'\n
;; :CHAR-CHARSET-HANDLERS-CHARSET
`set-charset-priority'
`define-charset'
`clear-charset-maps'
`declare-equiv-charset'
`define-charset-alias'
`define-charset-internal'\n
;; :CHAR-CHARSET-HANDLERS-PROPERTY
`unify-charset'
`charset-plist'
`set-charset-plist'
`put-charset-property'
`get-charset-property'\n
;; :CHAR-CHARSET-INSPECT
`charsetp'
`charset-description'
`charset-dimension'
`charset-info'
`charset-long-name'
`charset-short-name'
`charset-chars'
`charset-id'
`charset-id-internal'
`charset-priority-list'
`char-charset'
`describe-character-set'
`list-charset-chars'\n
;; :CHAR-CHARSET-INSPECT-CHARSET-FINAL
`get-unused-iso-final-char'
`charset-iso-final-char'
`iso-charset'\n
;; :CHAR-CHARSET-INSPECT-BUFFER
`char-width'
`charset-after'
`find-charset-region'
`find-charset-string'
`tab-width'\n
;; :CHAR-CHARSET-PROPERTIES
`:code-offset`
`:map`
`:subset` 
`:superset`
`:dimension`
`:long-name`
`:short--name`
`:code-space`
`:min-code`
`:max-code`
`:iso-final-char`
`:iso-revision-number`
`:emacs-mule-id`
`:ascii-compatible-p`
`:supplementary-p`
`:invalid-code`
`:map`
`:subset`
`:superset`
`:unify-map`\n
;; :CHAR-CHARSET-VARIABLES
`unibyte-display-via-language-environment'
`charset-map-path'
`charset-list'
`charset-revision-table'
`inhibit-load-charset-map'
`current-iso639-language'
`emacs-mule-charset-table'\n
;; :CHAR-CHARSETS
eight-bit-graphic
eight-bit-control
eight-bit
unicode-bmp
;; :CHAR-CHARSET-USAGE
\(charset-id-internal 'eight-bit\)\n;=> 4\n
\(charset-id-internal 'eight-bit-control\)\n;=> 7\n
\(charset-id-internal 'eight-bit-graphic\)\n;=> 8\n
\(charset-id-internal 'unicode-bmp\)\n;=> 144 ;\(#o220, #x90\)\n
\(char-charset #x3fffff\)\n;=> eight-bit\n
\(decode-char 'eight-bit 255\)\n;=> 4194303 ;(#o17777777, #x3fffff)\n
\(split-char #x3fffff\)\n;=> (eight-bit 255)\n
\(split-char #xff\)\n;=> \(unicode-bmp 0 232\)\n
:SEE info node `(elisp)Character Sets'\n
:SEE :FILE charset.c mule.el\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-charset-functions :insertp t)
    (mon-help-message-intrp "mon-help-char-charset-functions")))
;;
;;; :TEST-ME (mon-help-char-charset-functions)
;;; :TEST-ME (mon-help-char-charset-functions t)
;;; :TEST-ME (apply 'mon-help-char-charset-functions (t))

 
;;; ==============================
;;; :CHANGESET 1804
;;; :CREATED <Timestamp: #{2010-06-01T12:52:50-04:00Z}#{10222} - by MON KEY>
;;;###autoload
(defun mon-help-char-coding-functions (&optional insertp intrp)
  "A list of functions for working with Emacs character coding-systems.\n
;; :CODING-SYSTEM-DESCRIPTORS
`terminal-coding-system'
`list-coding-systems'
`print-designation'\n
;; :CODING-SYSTEM-GETTERS
`find-operation-coding-system'
`coding-system-list'\n
;; :CODING-SYSTEM-SETTERS
`set-selection-coding-system'
`set-next-selection-coding-system'
`set-buffer-process-coding-system'
`set-buffer-file-coding-system'
`set-coding-priority'
`set-file-name-coding-system'
`set-terminal-coding-system'
`set-language-environment'
`set-keyboard-coding-system'
`merge-coding-systems'
`autoload-coding-system'
`after-insert-file-set-coding'
`revert-buffer-with-coding-system'
`define-coding-system' (<NAME> <DOCSTRING> [<PROPERTY> <PVAL>]*)
 <PROPERTY>              <PVAL>
 -----------------------------
:mneumonic               <CHAR>
:coding-type             { charset utf-8 utf-16 iso-2022 emacs-mule
                           shift-jis ccl raw-text undecided }
                       ;; :CODING-TYPE-DEPENDENT-PROPERTIES
                          :flags <LIST> ;iso-2022 `coding-system-iso-2022-flags'
                          :designation <VECTOR>  ;iso-2022
                          :bom <BOOLEAN>|<CONS>  ;utf-16, utf-8
                          :endian { big little } ;utf-16
                          :ccl-encoder <SYMBOL>  ;ccl
                          :ccl-decoder <SYMBOL>  ;ccl
:eol-type                  { unix dos mac }
:charset-list              <LIST>
:ascii-compatible-p        <BOOLEAN>
:decode-translation-table  <TRANSLATION-TABLE>
:encode-translation-table  <TRANSLATION-TABLE>
:post-read-conversion      <FUNCTION>
:pre-write-conversion      <FUNCTION>
:default-char              <CHAR>
:for-unibyte               <BOOLEAN>
:mime-charset              <SYMBOL>
:mime-text-unsuitable      <SYMBOL>\n
;; :CODING-SYTEM-VARIABLES
`keyboard-coding-system'
`default-keyboard-coding-system'
`buffer-file-coding-system-explicit'
`buffer-file-coding-system'
`auto-coding-alist'
`file-coding-system-alist'
`process-coding-system-alist'
`network-coding-system-alist'
`current-language-environment'\n
:SEE info node `(elisp)Non-ASCII Characters in Strings'\n
:SEE info node `(elisp)Coding Systems'
:SEE info node `(emacs)Coding Systems'
:SEE info node `(elisp)Character Sets'
:SEE info node `(emacs)Charsets'\n
:SEE info node `(elisp)Character Properties'
:SEE info node `(elisp)Character Type'
:SEE info node `(elisp)Text Representations'\n
:ALIASED-BY `mon-help-charset-coding-functions'\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-coding-functions :insertp t)
    (mon-help-message-intrp "mon-help-char-coding-functions")))
;;
;;; :TEST-ME (mon-help-char-coding-functions)
;;; :TEST-ME (mon-help-char-coding-functions t)
;;; :TEST-ME (describe-function 'mon-help-char-coding-functions)
;;; :TEST-ME (apply 'mon-help-char-coding-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2090
;;; :CREATED <Timestamp: #{2010-08-30T11:21:38-04:00Z}#{10351} - by MON KEY>
;;;###autoload
(defun mon-help-char-table-functions (&optional insertp intrp)
  "A list of functions for working with Emacs character char-tables.\n
;; :CHAR-TABLE-FUNCTIONS
`modify-category-entry'
`define-category-entry'
`map-charset-chars'
`make-char-table'
`char-table-subtype'
`char-table-parent'
`char-table-p'
`char-table-extra-slot'
`optimize-char-table'
`map-char-table'
`set-char-table-default'
`set-char-table-parent'
`set-char-table-range'
`set-char-table-extra-slot'
`build-unicode-category-table'
`unify-charset'
`update-glyphless-char-display'\n
;; :CHAR-TABLE-FUNCTIONS-CASE
`get-upcase-table'
`copy-case-table'
`set-case-table'
`set-case-syntax'
`set-case-syntax-delims'
`set-case-syntax-pair'
`set-downcase-syntax'
`set-upcase-syntax'
`set-standard-case-table'
`with-case-table'\n
;; :CHAR-TABLES
`char-acronym-table'
`char-code-property-alist'
`char-code-property-table'  ; :NOTE \(get 'char-code-property-table 'char-table-extra-slots\)
`char-script-table'
`char-width-table'
`composition-function-table'
`find-word-boundary-function-table'\n
`keyboard-translate-table'
`printable-chars'
`standard-case-table'
`standard-category-table'
`standard-syntax-table'
`translate-region-internal'
`unicode-category-table'
`word-combining-categories'
`word-separating-categories'\n
;; :CHAR-TABLE-FUNCTIONS-DESCRIBE
`describe-vector'
`internal-describe-syntax-value'
`describe-buffer-case-table'
`help-describe-category-set'
`describe-categories'\n
;; :CHAR-TABLE-VARIABLES
`glyphless-char-control'\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-table-functions :insertp t)
    (mon-help-message-intrp "mon-help-char-table-functions")))
;;
;;; :TEST-ME (mon-help-char-table-functions)
;;; :TEST-ME (mon-help-char-table-functions t)
;;; :TEST-ME (apply 'mon-help-char-table-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2099
;;; :CREATED <Timestamp: #{2010-08-31T10:48:01-04:00Z}#{10352} - by MON KEY>
;;;###autoload
(defun mon-help-display-table-functions (&optional insertp intrp)
  "A list of functions for working with Emacs character display-tables.\n
;; :DISPLAY-TABLE-HANDLERS
`lookup-image-map'
`make-display-table'
`display-table-slot'       ; :NOTE Also a <PROPERTY>
`set-display-table-slot'
`set-window-display-table'
`window-display-table'\n
;; :DISPLAY-TABLE-DESCRIBE
`describe-current-display-table'
`describe-display-table'\n
;; :DISPLAY-TABLE-STANDARD-DISPLAY
`standard-display-8bit'
`standard-display-ascii'
`standard-display-default'
`standard-display-european'
`standard-display-european-internal'
`standard-display-g1'
`standard-display-graphic'
`standard-display-underline'\n
;; :DISPLAY-TABLE-FUNCTIONS-GLYPH
`dump-glyph-matrix'
`dump-frame-glyph-matrix'
`dump-glyph-row'
`create-glyph'
`make-glyph-code'
`glyph-char'
`glyph-face'\n
;; :DISPLAY-TABLE-PROPERTIES
`display-table-slot` ;:NOTE Also a <FUNCTION>\n
;; :DISPLAY-TABLE-VARIABLES
`glyph-table'
`standard-display-table'
`buffer-display-table'
;; :DISPLAY-VARIABLES
`redisplay-end-trigger-functions'
`display-pixels-per-inch'
`scroll-step'
`scroll-margins'
`scroll-conservatively'
`show-trailing-whitespace'
`nobreak-char-display'
`void-text-area-pointer'\n
:SEE :FILE lisp/disp-table.el src/dispnew.c\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-display-table-functions :insertp t)
    (mon-help-message-intrp "mon-help-display-table-functions")))
;;
;;; :TEST-ME (mon-help-display-table-functions)
;;; :TEST-ME (mon-help-display-table-functions t)
;;; :TEST-ME (apply 'mon-help-display-table-functions '(t))

 
;;; ==============================
;;; :CHANGESET 1986
;;; :CREATED <Timestamp: #{2010-07-16T16:30:11-04:00Z}#{10285} - by MON KEY>
;;;###autoload
(defun mon-help-char-unidata-table (&optional insertp intrp)
  "Table mapping certain sub-tables in `char-code-property-alist'.\n
;; :GENERAL-CATEGORY
`unidata-describe-general-category' ; :SEE :FILE uni-category.el
Lu <- Letter, Uppercase
Ll <- Letter, Lowercase
Lt <- Letter, Titlecase
Lm <- Letter, Modifier
Lo <- Letter, Other
Mn <- Mark, Nonspacing
Mc <- Mark, Spacing Combining
Me <- Mark, Enclosing
Nd <- Number, Decimal Digit
Nl <- Number, Letter
No <- Number, Other
Pc <- Punctuation, Connector
Pd <- Punctuation, Dash
Ps <- Punctuation, Open
Pe <- Punctuation, Close
Pi <- Punctuation, Initial quote
Pf <- Punctuation, Final quote
Po <- Punctuation, Other
Sm <- Symbol, Math
Sc <- Symbol, Currency
Sk <- Symbol, Modifier
So <- Symbol, Other              ; \(get-char-code-property 9658 'general-category\)
Zs <- Separator, Space
Zl <- Separator, Line
Zp <- Separator, Paragraph
Cc <- Other, Control
Cf <- Other, Format
Cs <- Other, Surrogate
Co <- Other, Private Use
Cn <- Other, Not Assigned\n
;; :BIDI-CLASS
L   <- Left-to-Right
LRE <- Left-to-Right Embedding
LRO <- Left-to-Right Override
R   <- Right-to-Left            ; (get-char-code-property 1488 'bidi-class)
AL  <- Right-to-Left Arabic
RLE <- Right-to-Left Embedding
RLO <- Right-to-Left Override
PDF <- Pop Directional Format
EN  <- European Number
ES  <- European Number Separator
ET  <- European Number Terminator
AN  <- Arabic Number
CS  <- Common Number Separator
NSM <- Non-Spacing Mark
BN  <- Boundary Neutral
B   <- Paragraph Separator
S   <- Segment Separator
WS  <- Whitespace
ON  <- Other Neutrals
`unidata-describe-bidi-class'    ; :SEE :FILE uni-bidi.el\n
;; :CANONICAL-COMBINING-CLASS
0   <- Spacing, split, enclosing, reordrant, and Tibetan subjoined
1   <- Overlays and interior
7   <- Nuktas
8   <- Hiragana/Katakana voicing marks
9   <- Viramas
10  <- Start of fixed position classes
199 <- End of fixed position classes
200 <- Below left attached
202 <- Below attached
204 <- Below right attached
208 <- Left attached \(reordrant around single base character\)
210 <- Right attached
212 <- Above left attached
214 <- Above attached
216 <- Above right attached
218 <- Below left
220 <- Below          ; \(get-char-code-property 804 'canonical-combining-class\)
222 <- Below right
224 <- Left \(reordrant around single base character\)
226 <- Right
228 <- Above left
230 <- Above
232 <- Above right
233 <- Double below
234 <- Double above
240 <- Below \(iota subscript\)
`unidata-describe-canonical-combining-class' ; :SEE :FILE uni-combining.el
`reference-point-alist'\n
;; :UNIDATA-TABLE-GENERATION
The unidata table generated when Emacs is dumped isn't really directly
accessible. This is because some of the functions call `byte-compile' when
building portions of the table to save space and improve efficient char related
lookups. These include:
`unidata-gen-table' 
`unidata-gen-table-character'
`unidata-gen-table-symbol'
`unidata-gen-table-integer'
`unidata-gen-table-numeric'
`unidata-gen-table-name'
`unidata-gen-table-decomposition'\n
;; :UNIDATA-FILE-GENERATION
`unidata-gen-files'
:SEE :FILE charprop.el
:SEE :FILE uni-combining.el
:SEE :FILE uni-bidi.el
:SEE :FILE uni-category.el
:SEE :FILE uni-name.el
:SEE :FILE uni-decomposition.el
:SEE :FILE uni-decimal.el
:SEE :FILE uni-digit.el
:SEE :FILE uni-numeric.el
:SEE :FILE uni-mirrored.el
:SEE :FILE uni-old-name.el
:SEE :FILE uni-comment.el
:SEE :FILE uni-uppercase.el
:SEE :FILE uni-lowercase.el
:SEE :FILE uni-titlecase.el\n
:SEE :FILE admin/unidata-gen.el\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-unidata-table :insertp t)
    (mon-help-message-intrp "mon-help-char-unidata-table")))
;;
;;; :TEST-ME (mon-help-char-unidata-table)
;;; :TEST-ME (mon-help-char-unidata-table t)
;;; :TEST-ME (describe-function 'mon-help-char-unidata-table)

 
;;; ==============================
;;; :CHANGESET 1843
;;; :CREATED <Timestamp: #{2010-06-11T14:32:05-04:00Z}#{10235} - by MON KEY>
;;;###autoload
(defun mon-help-char-composition (&optional insertp intrp)
  "List of functions related to char composition and glyphs.\n
;; :CHAR-COMPOSITION
`auto-compose-chars'
`compose-chars'
`compose-chars-after'
`compose-last-chars'
`compose-gstring-for-terminal'
`compose-gstring-for-graphic'
`compose-glyph-string'
`compose-glyph-string-relative'
`compose-region'
`compose-region-internal'
`compose-string'
`compose-string-internal'
`composition-get-gstring'
`decompose-composite-char'
`decode-composition-rule'
`decode-composition-components'
`decompose-region'
`decompose-string'
`encode-composition-components'
`encode-composition-rule'
`find-composition'
`find-composition-internal'\n
;; :CHAR-COMPOSITION-LG
`lglyph-adjustment'
`lglyph-ascent'
`lglyph-char'
`lglyph-code'
`lglyph-copy'
`lglyph-descent'
`lglyph-from'
`lglyph-lbearing'
`lglyph-rbearing'
`lglyph-set-adjustment'
`lglyph-set-char'
`lglyph-set-code'
`lglyph-set-from-to'
`lglyph-set-width'
`lglyph-to'
`lglyph-width'
`lgstring-char'
`lgstring-char-len'
`lgstring-font'
`lgstring-glyph'
`lgstring-glyph-len'
`lgstring-header'
`lgstring-insert-glyph'
`lgstring-set-glyph'
`lgstring-set-header'
`lgstring-set-id'
`lgstring-shaped-p'\n
;; :CHAR-GLYPH-FUNCTIONS
`create-glyph'
`make-glyph-code'
`glyph-char'
`glyph-face'\n
;; :CHAR-COMPOSITION-GLYPH-VARIABLES
`auto-composition-mode'
`glyph-table'
`compose-chars-after-function'
`composition-function-table'
`reference-point-alist'        ;<CONSTANT>\n
;; :CHAR-COMPOSITION-DISCUSSION
Emacs uses special text property `composition' to support character
composition.  A sequence of characters that have the same (i.e. eq)
`composition' property value is treated as a single composite
sequence \(we call it just `composition' here after\).  Characters in
a composition are all composed somehow on the screen.
The property value has this form when the composition is made:\n
     \(\(LENGTH . COMPONENTS\) . MODIFICATION-FUNC\)\n
then turns to this form:\n
     \(COMPOSITION-ID . \(LENGTH COMPONENTS-VEC . MODIFICATION-FUNC\)\)\n
when the composition is registered in composition_hash_table and
composition_table.  These rather peculiar structures were designed
to make it easy to distinguish them quickly (we can do that by
checking only the first element) and to extract LENGTH (from the
former form) and COMPOSITION-ID (from the latter form).\n
We register a composition when it is displayed, or when the width
is required \(for instance, to calculate columns\).\n
LENGTH -- Length of the composition.  This information is used to
     check the validity of the composition.\n
COMPONENTS --  Character, string, vector, list, or nil.\n
     If it is nil, characters in the text are composed relatively
     according to their metrics in font glyphs.\n
     If it is a character or a string, the character or characters
     in the string are composed relatively.\n
     If it is a vector or list of integers, the element is a
     character or an encoded composition rule.  The characters are
     composed according to the rules.  \(2N\)th elements are
     characters to be composed and \(2N+1\)th elements are
     composition rules to tell how to compose \(2N+2\)th element with
     the previously composed 2N glyphs.\n
COMPONENTS-VEC -- Vector of integers.  In a relative composition,
     the elements are the characters to be composed.  In a rule-base
     composition, the elements are characters or encoded
     composition rules.\n
MODIFICATION-FUNC -- If non nil, it is a function to call when the
     composition gets invalid after a modification in a buffer.  If
     it is nil, a function in `composition-function-table' of the
     first character in the sequence is called.\n
COMPOSITION-ID --Identification number of the composition.  It is
     used as an index to composition_table for the composition.\n
When Emacs has to display a composition or has to know its
displaying width, the function get_composition_id is called.  It
returns COMPOSITION-ID so that the caller can access the
information about the composition through composition_table.  If a
COMPOSITION-ID has not yet been assigned to the composition,
get_composition_id checks the validity of `composition' property,
and, if valid, assigns a new ID, registers the information in
composition_hash_table and composition_table, and changes the form
of the property value.  If the property is invalid,
get_composition_id returns -1 without changing the property value.\n
We use two tables to keep the information about composition;
composition_hash_table and composition_table.\n
The former is a hash table whose keys are COMPONENTS-VECs and
values are the corresponding COMPOSITION-IDs.  This hash table is
weak, but as each key \(COMPONENTS-VEC\) is also kept as a value of the
`composition' property, it won't be collected as garbage until all
bits of text that have the same COMPONENTS-VEC are deleted.\n
The latter is a table of pointers to `struct composition' indexed
by COMPOSITION-ID.  This structure keeps the other information.\n
In general, a text property holds information about individual
characters.  But, a `composition' property holds information about
a sequence of characters (in this sense, it is like the `intangible'
property).  That means that we should not share the property value
in adjacent compositions -- we can't distinguish them if they have the
same property.  So, after any changes, we call
`update_compositions' and change a property of one of adjacent
compositions to a copy of it.  This function also runs a proper
composition modification function to make a composition that gets
invalid by the change valid again.\n
As the value of the `composition' property holds information about a
specific range of text, the value gets invalid if we change the
text in the range.  We treat the `composition' property as always
rear-nonsticky (currently by setting default-text-properties to
\(rear-nonsticky \(composition\)\)\) and we never make properties of
adjacent compositions identical.  Thus, any such changes make the
range just shorter.  So, we can check the validity of the `composition'
property by comparing LENGTH information with the actual length of
the composition.\n
:SOURCE src/composite.c -- Comments at header of file.\n
:SEE info node `(elisp)Special Properties'
:SEE info node `(elisp)Glyphs'
:SEE-FILE lisp/composite.el src/font.h src/composite.c src/composite.h
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-composition :insertp t)
    (mon-help-message-intrp "mon-help-char-composition")))
;;
;;; :TEST-ME (mon-help-char-composition)
;;; :TEST-ME (mon-help-char-composition t)
;;; :TEST-ME (documentation 'mon-help-char-composition)
;;; :TEST-ME (apply 'mon-help-char-composition '(t))

 
;;; ==============================
;;; :RENAMED `mon-help-ASCII-chars' -> `mon-help-char-ascii'
;;; :CREATED <Timestamp: #{2009-09-01T17:34:29-04:00Z}#{09362} - by MON KEY>
;;;###autoload
(defun mon-help-char-ascii (&optional insertp intrp)
  "ASCII Character Tables.\n
    _____________
   |             |                                                      
   | :CHAR-ASCII |                                                      
 __|_____________|______________________________________________67.
|                                                                 |
| :CHAR-ASCII-OCTAL                                               |
|                                                                 |
| 000 NUL|001 SOH|002 STX|003 ETX|004 EOT|005 ENQ|006 ACK|007 BEL |
| 010 BS |011 HT |012 NL |013 VT |014 NP |015 CR |016 SO |017 SI  |
| 020 DLE|021 DC1|022 DC2|023 DC3|024 DC4|025 NAK|026 SYN|027 ETB |
| 030 CAN|031 EM |032 SUB|033 ESC|034 FS |035 GS |036 RS |037 US  |
| 040 SP |041  ! |042  \" |043  # |044  $ |045  % |046  & |047  '  |
| 050  ( |051  ) |052  * |053  + |054  , |055  - |056  . |057  /  |
| 060  0 |061  1 |062  2 |063  3 |064  4 |065  5 |066  6 |067  7  |
| 070  8 |071  9 |072  : |073  ; |074  < |075  = |076  > |077  ?  |
| 100  @ |101  A |102  B |103  C |104  D |105  E |106  F |107  G  |
| 110  H |111  I |112  J |113  K |114  L |115  M |116  N |117  O  |
| 120  P |121  Q |122  R |123  S |124  T |125  U |126  V |127  W  |
| 130  X |131  Y |132  Z |133  [ |134  \\ |135  ] |136  ^ |137  _  |
| 140  ` |141  a |142  b |143  c |144  d |145  e |146  f |147  g  |
| 150  h |151  i |152  j |153  k |154  l |155  m |156  n |157  o  |
| 160  p |161  q |162  r |163  s |164  t |165  u |166  v |167  w  |
| 170  x |171  y |172  z |173  { |174  | |175  } |176  ~ |177 DEL |
|_________________________________________________________________|
|                                                                 |
| :CHAR-ASCII-HEX                                                 |
|                                                                 |
| 00 NUL| 01 SOH| 02 STX| 03 ETX| 04 EOT| 05 ENQ| 06 ACK| 07 BEL  |
| 08 BS | 09 HT | 0A NL | 0B VT | 0C NP | 0D CR | 0E SO | 0F SI   |
| 10 DLE| 11 DC1| 12 DC2| 13 DC3| 14 DC4| 15 NAK| 16 SYN| 17 ETB  |
| 18 CAN| 19 EM | 1A SUB| 1B ESC| 1C FS | 1D GS | 1E RS | 1F US   |
| 20 SP | 21  ! | 22  \" | 23  # | 24  $ | 25  % | 26  & | 27  '   |
| 28  ( | 29  ) | 2a  * | 2b  + | 2c  , | 2d  - | 2e  . | 2f  /   |
| 30  0 | 31  1 | 32  2 | 33  3 | 34  4 | 35  5 | 36  6 | 37  7   |
| 38  8 | 39  9 | 3a  : | 3b  ; | 3c  < | 3d  = | 3e  > | 3f  ?   |
| 40  @ | 41  A | 42  B | 43  C | 44  D | 45  E | 46  F | 47  G   |
| 48  H | 49  I | 4a  J | 4b  K | 4c  L | 4d  M | 4e  N | 4f  O   |
| 50  P | 51  Q | 52  R | 53  S | 54  T | 55  U | 56  V | 57  W   |
| 58  X | 59  Y | 5a  Z | 5b  [ | 5c  \\ | 5d  ] | 5e  ^ | 5f  _   |
| 60  ` | 61  a | 62  b | 63  c | 64  d | 65  e | 66  f | 67  g   |
| 68  h | 69  i | 6a  j | 6b  k | 6c  l | 6d  m | 6e  n | 6f  o   |
| 70  p | 71  q | 72  r | 73  s | 74  t | 75  u | 76  v | 77  w   |
| 78  x | 79  y | 7a  z | 7b  { | 7c  | | 7d  } | 7e  ~ | 7f DEL  |
|_________________________________________________________________|
|                                                                 |
| :CHAR-ASCII-DECIMAL                                             |
|                                                                 |
|  0 NUL|  1 SOH|  2 STX|  3 ETX|  4 EOT|  5 ENQ|  6 ACK|  7 BEL  |
|  8 BS |  9 HT | 10 NL | 11 VT | 12 NP | 13 CR | 14 SO | 15 SI   |
| 16 DLE| 17 DC1| 18 DC2| 19 DC3| 20 DC4| 21 NAK| 22 SYN| 23 ETB  |
| 24 CAN| 25 EM | 26 SUB| 27 ESC| 28 FS | 29 GS | 30 RS | 31 US   |
| 32 SP | 33  ! | 34  \" | 35  # | 36  $ | 37  % | 38  & | 39  '   |
| 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  /   |
| 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7   |
| 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ?   |
| 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G   |
| 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O   |
| 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W   |
| 88  X | 89  Y | 90  Z | 91  [ | 92  \\ | 93  ] | 94  ^ | 95  _   |
| 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g   |
|104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o   |
|112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w   |
|120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 DEL  |
|_______________________________________________________________67.\n
:ALIASED-BY `mon-help-ascii-chars'\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-ascii :insertp t)
    (mon-help-message-intrp "mon-help-char-ascii")))
;;
;;; :TEST-ME (mon-help-char-ascii)
;;; :TEST-ME (mon-help-char-ascii t)
;;; :TEST-ME (describe-function 'mon-help-char-ascii)
;;; :TEST-ME (documentation 'mon-help-char-ascii)
;;; :TEST-ME (apply 'mon-help-char-ascii '(t))

 
;;; ==============================
;;; :RENAMED `mon-help-char-iso-8859-1' -> `mon-help-char-iso-8859-1'
;;; :CREATED <Timestamp: #{2009-09-01T17:34:24-04:00Z}#{09362} - by MON KEY>
;;;###autoload
(defun mon-help-char-iso-8859-1 (&optional insertp intrp)
  "ISO-8859-1 Character Table.\n
                      __________________                                  
                     |                  |                                 
                     | :CHAR-ISO-8859-1 |                                 
 ____________________|__________________|______________________________73.
|                                                                        |
|   0 NU    16 DL     32 SP    48 0     64 At   80 P      96 '!   112 p  |
|   1 SH    17 D1     33 !     49 1     65 A    81 Q      97 a    113 q  |
|   2 SX    18 D2     34 \"     50 2     66 B    82 R      98 b    114 r  |
|   3 EX    19 D3     35 Nb    51 3     67 C    83 S      99 c    115 s  |
|   4 ET    20 D4     36 DO    52 4     68 D    84 T     100 d    116 t  |
|   5 EQ    21 NK     37 %     53 5     69 E    85 U     101 e    117 u  |
|   6 AK    22 SY     38 &     54 6     70 F    86 V     102 f    118 v  |
|   7 BL    23 EB     39 '     55 7     71 G    87 W     103 g    119 w  |
|   8 BS    24 CN     40 (     56 8     72 H    88 X     104 h    120 x  |
|   9 HT    25 EM     41 )     57 9     73 I    89 Y     105 i    121 y  |
|  10 LF    26 SB     42 *     58 :     74 J    90 Z     106 j    122 z  |
|  11 VT    27 EC     43 +     59 ;     75 K    91 <(    107 k    123 (! |
|  12 FF    28 FS     44 ,     60 <     76 L    92 //    108 l    124 !! |
|  13 CR    29 GS     45 -     61 =     77 M    93 )>    109 m    125 !) |
|  14 SO    30 RS     46 .     62 >     78 N    94 '>    110 n    126 '? |
|  15 SI    31 US     47 /     63 ?     79 O    95 _     111 o    127 DT |
|________________________________________________________________________|
|                                                                        |
| 128 PA   144 DC   160 NS   176 DG   192 A!   208 D-   224 a!   240 d-  |
| 129 HO   145 P1   161 !I   177 +-   193 A'   209 N?   225 a'   241 n?  |
| 130 BH   146 P2   162 Ct   178 2S   194 A>   210 O!   226 a>   242 o!  |
| 131 NH   147 TS   163 Pd   179 3S   195 A?   211 O'   227 a?   243 o'  |
| 132 IN   148 CC   164 Cu   180 ''   196 A:   212 O>   228 a:   244 o>  |
| 133 NL   149 MW   165 Ye   181 My   197 AA   213 O?   229 aa   245 o?  |
| 134 SA   150 SG   166 BB   182 PI   198 AE   214 O:   230 ae   246 o:  |
| 135 ES   151 EG   167 SE   183 .M   199 C,   215 *X   231 c,   247 -:  |
| 136 HS   152 SS   168 ':   184 ',   200 E!   216 O/   232 e!   248 o/  |
| 137 HJ   153 GC   169 Co   185 1S   201 E'   217 U!   233 e'   249 u!  |
| 138 VS   154 SC   170 -a   186 -o   202 E>   218 U'   234 e>   250 u'  |
| 139 PD   155 CI   171 <<   187 >>   203 E:   219 U>   235 e:   251 u>  |
| 140 PU   156 ST   172 NO   188 14   204 I!   220 U:   236 i!   252 u:  |
| 141 RI   157 OC   173 --   189 12   205 I'   221 Y'   237 i'   253 y'  |
| 142 S2   158 PM   174 Rg   190 34   206 I>   222 TH   238 i>   254 th  |
| 143 S3   159 AC   175 'm   191 ?I   207 I:   223 ss   239 i:   255 y:  |
|______________________________________________________________________73.\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-iso-8859-1 :insertp t)
    (mon-help-message-intrp "mon-help-char-iso-8859-1")))
;;
;;; :TEST-ME (mon-help-char-iso-8859-1)
;;; :TEST-ME (mon-help-char-iso-8859-1 t)
;;; :TEST-ME (describe-function 'mon-help-char-iso-8859-1)
;;; :TEST-ME (documentation 'mon-help-char-iso-8859-1)
;;; :TEST-ME (describe-function 'mon-help-char-iso-8859-1)
;;; :TEST-ME (apply 'mon-help-char-iso-8859-1 '(t))

 
;;; ==============================
;;; :RENAMED `mon-help-cntl->hex->ecma-48' -> `mon-help-char-ecma-48'
;;; :COURTESY Micah Cowan  :HIS teseq.info of GNU Teseq VERSION: 1.0.0
;;; :CREATED <Timestamp: 2009-08-13-W33-4T16:30:22-0400Z - by MON KEY>
;;;###autoload
(defun mon-help-char-ecma-48 (&optional insertp intrp)
  "For reference, here's a table of the control characters (plus DEL). It
is based on the information from Table 1 of ECMA-48 /ISO/IEC 6429 
\(the control-key representation has been added\).\n
                       _______________                
                      |               |               
                      | :CHAR-ECMA-48 |               
 _____________________|_______________|____________53.
|                                                    |
| :HEX   :KEY    :NAME          :HEX    :KEY   :NAME |
|____________________________________________________|
|                                                    |
| x00     ^@      NUL            x10     ^P      DLE |
| x01     ^A      SOH            x11     ^Q      DC1 |
| x02     ^B      STX            x12     ^R      DC2 |
| x03     ^C      ETX            x13     ^S      DC3 |
| x04     ^D      EOT            x14     ^T      DC4 |
| x05     ^E      ENQ            x15     ^U      NAK |
| x06     ^F      ACK            x16     ^V      SYN |
| x07     ^G      BEL            x17     ^W      ETB |
| x08     ^H      BS             x18     ^X      CAN |
| x09     ^I      TAB            x19     ^Y      EM  |
| x0A     ^J      LF             x1A     ^Z      SUB |
| x0B     ^K      VT             x1B     ^[      ESC |
| x0C     ^L      FF             x1C     ^\\      IS4 |
| x0D     ^M      CR             x1D     ^]      IS3 |
| x0E     ^N      SO             x1E     ^^      IS2 |
| x0F     ^O      SI             x1F     ^_      IS1 |
| x7F     ^?      DEL                                |
|__________________________________________________53.\n
:ALIASED-BY `mon-help-ecma-48-chars-cntl->hex'\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-ecma-48 :insertp t)
    (mon-help-message-intrp "mon-help-char-ecma-48")))
;;
;;; :TEST-ME (mon-help-char-ecma-48)
;;; :TEST-ME (mon-help-char-ecma-48 t)
;;; :TEST-ME (describe-function 'mon-help-char-ecma-48)
;;; :TEST-ME (documentation 'mon-help-char-ecma-48)
;;; :TEST-ME (describe-function 'mon-help-char-ecma-48)
;;; :TEST-ME (apply 'mon-help-char-ecma-48 '(t))

 
;;; ==============================
;;; :RENAMED `mon-help-cntl->hex->ecma-35' -> `mon-help-char-ecma-35' 
;;; :COURTESY Micah Cowan :HIS teseq.info of GNU Teseq :VERSION 1.0.0
;;; :CREATED <Timestamp: 2009-08-13-W33-4T16:30:22-0400Z - by MON>
;;;###autoload
(defun mon-help-char-ecma-35 (&optional insertp intrp)
  "The ECMA-35/ISO/IEC 2200 standard defines an escape sequence to be a
sequence of characters beginning with ESC, with a final byte in the
range 'x30'-'x7E', and any number \(including zero\) of intermediate
bytes in the range 'x20'-'x2F'.  The following table has been provided
as a reference for finding which characters match which codes.\n
               _______________                
              |               |               
              | :CHAR-ECMA-35 |               
 _____________|_______________|____________45.
|      |                                     |
|      |  x2X   x3X   x4X   x5X   x6X   x7X  |
|______|_____________________________________|
|      |                                     |
| xX0  |  SPC    0     @     P     `     p   |
| xX1  |   !     1     A     Q     a     q   |
| xX2  |   \"     2     B     R     b     r   |
| xX3  |   #     3     C     S     c     s   |
| xX4  |   $     4     D     T     d     t   |
| xX5  |   %     5     E     U     e     u   |
| xX6  |   &     6     F     V     f     v   |
| xX7  |   '     7     G     W     g     w   |
| xX8  |   (     8     H     X     h     x   |
| xX9  |   )     9     I     Y     i     y   |
| xXA  |   *     :     J     Z     j     z   |
| xXB  |   +     ;     K     [     k     {   |
| xXC  |   ,     <     L     \\     l     |   |
| xXD  |   -     =     M     ]     m     }   |
| xXE  |   .     >     N     ^     n     ~   |
| xXF  |   /     ?     O     _     o    DEL  |
|__________________________________________45.\n
:ALIASED-BY `mon-help-cntl->hex->ecma-35'\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-ecma-35 :insertp t)
    (mon-help-message-intrp "mon-help-char-ecma-35")))
;;
;;; :TEST-ME (mon-help-char-ecma-35)
;;; :TEST-ME (mon-help-char-ecma-35 t)
;;; :TEST-ME (describe-function 'mon-help-char-ecma-35)
;;; :TEST-ME (documentation 'mon-help-char-ecma-35)
;;; :TEST-ME (describe-function 'mon-help-char-ecma-35)
;;; :TEST-ME (apply 'mon-help-char-ecma-35 '(t))

 
;;; ==============================
;;; Map mon-insert-uniccode to -> (ucs-insert "12C")
;;;###autoload
(defun mon-help-diacritics (&optional insertp intrp)
  "Insert commonly used diacritics and their keymaps at point.\n►►►\n
;; :CX8-DIACRITIC-SYMBOLS-E\n
à À - C-x 8 ` a
á Á - C-x 8 ' a
ã Ã - C-x 8 ~ a
å Å - C-x 8 / a
â Â - C-x 8 ^ a
ä Ä - C-x 8 \" a
ă Ă - (ucs-insert \"103\") (ucs-insert \"102\")
æ Æ - C-x 8 / e\n
;; :CX8-DIACRITIC-SYMBOLS-E
è È - C-x 8 ` e
é É - C-x 8 ' e
ë Ë - C-x 8 \" e
ê Ê - C-x 8 ^ e
ĕ Ĕ - (ucs-insert \"115\") (ucs-insert \"114\")\n
;; :CX8-DIACRITIC-SYMBOLS-I
í Í - C-x 8 ' i
ì Ì - C-x 8 ` i
ï Ï - C-x 8 \" i
î Î - C-x 8 ^ i
ĭ Ĭ - (ucs-insert \"12D\") (ucs-insert \"12C\")\n
;; :CX8-DIACRITIC-SYMBOLS-O
ó Ó - C-x 8 ' o
ò Ò - C-x 8 ` o
ø Ø - C-x 8 / o
ö Ö - C-x 8 \" o
ô Ô - C-x 8 ^ o
õ Õ - C-x 8 ~ o
ŏ Ŏ - (ucs-insert \"14F\") (ucs-insert \"14E\")
œ Œ - (ucs-insert \"153\") (ucs-insert \"152\")\n
;; :CX8-DIACRITIC-SYMBOLS-U
ú Ú - C-x 8 ' u
ù Ù - C-x 8 ` u
ü Ü - C-x 8 \" u
û Û - C-x 8 ^ u
ů Ů - (ucs-insert \"16F\") (ucs-insert \"16E\")
ŭ Ŭ - (ucs-insert \"16D\") (ucs-insert \"16C\")\n
;; :CX8-DIACRITIC-SYMBOLS-NON-VOWEL
ý Ý - C-x 8 ' y
ÿ   - C-x 8 \" y
ç Ç - C-x 8 , c
č Č - (ucs-insert \"10D\") (ucs-insert \"10C\")
ñ Ñ - C-x 8 ~ n
ň Ň - (ucs-insert \"148\") (ucs-insert \"147\")
ß   - C-x 8 \" s
ř Ř - (ucs-insert \"159\") (ucs-insert \"158\")
š Š - (ucs-insert \"161\") (ucs-insert \"160\")
ź Ź - (ucs-insert \"17A\") (ucs-insert \"179\")
ž Ž - (ucs-insert \"17E\") (ucs-insert \"17D\")
þ Þ - C-x 8 ~ t
ð Ð - C-x 8 ~ d\n
;; :CX8-DIACRITIC-SYMBOLS-CURRENCY
£ - C-x 8 L
¶ - C-x 8 P
§ - C-x 8 S
¥ - C-x 8 Y
¢ - C-x 8 c\n
;; :CX8-DIACRITIC-SYMBOLS-MATH
÷ - C-x 8 / /
¬ - C-x 8 ~ ~
× - C-x 8 x
¤ - C-x 8 $
± - C-x 8 +
\173 - C-x 8    ;SOFT-HYPHEN
· - C-x 8 .
¯ - C-x 8 =
µ - C-x 8 m
° - C-x 8 o     ; DEGREE
º - C-x 8 _ o   ; ORDINAL
µ - C-x 8 u
¾ - C-x 8 3 / 4
½ - C-x 8 1 / 2
¼ - C-x 8 1 / 4
¹ - C-x 8 ^ 1   ; SUPERSCRIPT-1
² - C-x 8 ^ 2   ; SUPERSCRIPT-2
³ - C-x 8 ^ 3   ; SUPERSCRIPT-3\n
;; :CX8-DIACRITIC-SYMBOLS
 « - C-x 8 <
 » - C-x 8 >
© - C-x 8 C
® - C-x 8 R
¡ - C-x 8 !
¿ - C-x 8 ?
¦ - C-x 8 |
ª - C-x 8 _ a
' - C-x 8 ' SPC
´ - C-x 8 ' '
¨ - C-x 8 \" \"
¸ - C-x 8 , ,
\xa0 - C-x 8 * SPC ; NO-BREAK-SPACE
► - (ucs-insert \"25BA\")\n
:NOTE C-x 8 RTN is bound to `ucs-insert'\n\n
;; :UCS-NAMES
`ucs-insert'
`ucs-names'
`ucs-completions'
`describe-char-unidata-list'
`describe-char-unicodedata-file'
`*mon-unidata-file-list*'
`mon-wget-unicodedata-files'\n
;;; ==============================\n
The Unicode data list can be found at:
:SEE (URL `http://www.unicode.org/Public/UNIDATA/UnicodeData.txt').
The Unicode latin scripts are found in several Unicode-Blocks, namely:
U+0000 - U+007F -> Controls and Basic Latin;
:SEE \(URL `http://www.decodeunicode.org/en/basic_latin'\)\n
U+0080 - U+009F -> Controls and Latin-1;
:SEE \(URL `http://www.decodeunicode.org/en/latin-1_supplement'\)\n
U+0100 - U+017F -> Latin Extended-A;
:SEE \(URL `http://www.decodeunicode.org/en/latin_extended-a'\)\n
U+0180 - U+024F -> Latin Extended-B;
:SEE \(URL `http://www.decodeunicode.org/en/latin_extended-b'\)\n
Character table for reverting ISO_8859-1 bytes -> UTF-8
:SEE \(URL `http://en.wikipedia.org/wiki/ISO_8859-1'\)\n
:SEE \(URL `http://en.wikipedia.org/wiki/ISO/IEC_8859'\)\n
:SEE (URL `http://unicode.coeurlumiere.com/').\n
Unicode Entity Codes for Phonetic/vowel Diacritics:
:SEE \(URL `http://tlt.its.psu.edu/suggestions/international/bylanguage/ipavowels.html'\)\n
:SEE (URL `http://www.alanwood.net/demos/ent4_frame.html')\n
HTML - Special Entity Codes:
:SEE \(URL `http://tlt.its.psu.edu/suggestions/international/web/codehtml.html'\)
XML Entity Definitions for Characters - W3C
:SEE (URL `http://www.w3.org/TR/xml-entity-names/')\n
:NOTE In particular the sources section, \"Section D\":
:SEE (URL `http://www.w3.org/TR/xml-entity-names/#source')
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (save-excursion
        (let* ((test-llm (and (buffer-local-value longlines-mode (current-buffer))))
               (is-on (and test-llm))
               (llm-off))
          (progn
            (if (buffer-local-value 'longlines-mode (current-buffer))
                (prog1
                    (longlines-mode nil)
                  (setq test-llm 'loc-buff-is-ll))
              (setq test-llm 'loc-buff-not-ll))
            (mon-help-function-spit-doc 'mon-help-diacritics :insertp t)
            (cond ((eq test-llm 'loc-buff-not-ll)
                   (setq test-llm nil))
                  ((eq test-llm 'loc-buff-is-ll)
                   (prog1
                       (longlines-mode t) ;
                     (setq test-llm nil)))))))
    (mon-help-message-intrp "mon-help-diacritics")))
;;
;;; :TEST-ME (mon-help-diacritics)
;;; :TEST-ME (mon-help-diacritics t)
;;; :TEST-ME (apply 'mon-help-diacritics '(t))

 
;;; ==============================
;;; :CHANGESET 2078
;;; :CREATED <Timestamp: #{2010-08-23T16:51:25-04:00Z}#{10341} - by MON KEY>
;;;###autoload
(defun mon-help-char-logic (&optional insertp intrp)
  "Some commonly used characters in propsitional, description, boolean logics.\n
∧ And
∨ Or
∩ Intersection - Conjunction
⊻ XOR Exclusive Disjunction
⊼ NAND 
∪ Union - Disjunction
∀ For All - Universal Restriction
∃ There Exists - Existential Restriction 
⊂ Subset Of
⊃ Superset Of
¬ Not - Negation - Complement
→ Implication
⊥ Contradiction
⊤ Tautology
⊢ Inference
↔ Material Equivalence
≣ Equivalence\n
:NOTE I'm not a logician, the table exists in order to help read logics.\n
:SEE-ALSO `mon-help-CL-bit-byte-bool-logic', `mon-help-CL-sequence-predicates'
`mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-logic :insertp t)
    (mon-help-message-intrp "mon-help-char-logic")))
;;
;;; :TEST-ME (mon-help-char-logic)
;;; :TEST-ME (mon-help-char-logic t)
;;; :TEST-ME (apply 'mon-help-char-logic (t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-28T16:25:12-05:00Z}#{09531} - by MON KEY>
;;;###autoload
(defun mon-help-char-representation (&optional insertp intrp)
  "Help for working with Emacs character representations.\n
:SEE info node `(elisp)Basic Char Syntax'\n
;; :CHAR-REPRESNTATION-DECIMAL
225
?\\á
\"\\341\"
\(identity ?\\\á\)        ;=> 225
\(identity \"\\341\")     ;=> \"á\"
\(char-to-string ?\\á)  ;=> \"á\"
\(char-to-string 225)  ;=> \"á\"\n
;; :CHAR-REPRESENTATION-OCTAL
#o341 
?\\341
\(identity #o341\)       ;=> 225
\(identity ?\\341\)       ;=> 225
\(char-to-string ?\\341\) ;=> á\n 
;; :CHAR-REPRESENTATION-HEX
#xe1 
\"\\xe1\" 
?\\xe1
\(identity \"\\xe1\"\)      ;=> \"á\"
\(identity #xe1\)        ;=> 225
\(identity ?\\xe1\)       ;=> 225
\(char-to-string ?\\xe1\) ;=> \"á\"\n
;; :CHAR-REPESENTATION-UNICODE
\"\\u25BA\"
?\\u25BA
\(identity ?\\u25BA\)       ;=> 9658
\(identity \"\\u25BA\"\)      ;=> \"►\"
\(char-to-string ?\\u25BA) ;=> \"►\"\n
;; :CHAR-REPESENTATION-BINARY             :SEE info node `(elisp)Integer Basics'
#b000 -> 0
#b001 -> 1
#b010 -> 2 
#b011 -> 3
#b100 -> 4\n
;; :CHAR-REPESENTATION-RADIX
#2r101010 ;=> 42          ; Radix  2 - Binary in #2r<INTEGER> notation
#b101010  ;=> 42          ; Radix  2 - Binary in #b<INTEGER> notation
#8r52     ;=> 42          ; Radix  8 - Octal in #8r<INTEGER> notation
#o52      ;=> 42          ; Radix  8 - Octal in #o<INTEGER> noatation
#16r2A    ;=> 42          ; Radix 16 - Hexadecimal in #16r<INTEGER> notation
#x2a      ;=> 42          ; Radix 16 - Hexadecimal in #x<INTEGER> notation
#28r1E    ;=> 42          ; Radix 28 - Ocotovigesimal in #28r<INTEGER> notation
#32r1A    ;=> 42          ; Radix 32 - Duotrigesimal in #32r<INTEGER> notation 
#36r16    ;=> 42          ; Radix 36 - Hexatrigesimal in #36r<INTEGER> notation\n
;; :CHAR-CONVERSION-IDIOMS
`format'
\(format \(concat \"\\x09 <- HERE BE A `%s'\\n\"
                  \"And which be control char: `%s'\\n\"
                  \"And which be decimal char: `%s'\\n\"
                  \"And which be octal char: `#o%o'\\nWhich is also being hex char: `#x%x'\"\)
        \(single-key-description ?\\x09\)
        \(text-char-description ?\\x09\)
        \(string-to-char \(format \"%s\" \"\\x09\"\)\) ?\\x9 ?\\11\)\n
`string'
\(apply 'string `(?/ ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t
                 ?u ?v ?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O
                 ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
                 ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\)))
 ;=> \"/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.!~*'()\"\n
;; :EMACS-CODESPACE
Emacs codespace extends well beyond that of the Unicode codepoint codespace.
In particular, Emacs exposes a range of integers which it terms 8-bit raw-bytes.
These are in the so called `octal range' 0255-0377. Though this `0NNN' notation is
widely by programmers the Emacs elisp reader doesn't understand it; however,
confusingly, when asked to `insert-byte', Emacs will insert a codepoint that is
visually similiar to the afforementioned `octal 0NNN' notation.  Thus, it is
important to understand that a codepoint is not the same thing as a character
and a character is not the same thing as the integer value Emacs uses internally
to represent it. This is an important distinction which can \(at times\) become
unclear when manipulating characters across different the coding sytems of
buffers, processes, environments, etc.

The decimal value 4194303 represents the uppermost of Emacs' internal
`codespace'.  Where this `codespace' is understood as the range of the set of
characters which may be represented by the positive numerical range of the
22-bit number corresponding to the integer return value of `max-char', e.g.:

 \(max-char\) => 4194303 \(#o17777777, #x3fffff\)

Such that `max-char's numerical value (and lesser positive values
therof) may be presented to the Emacs lisp readers in various ways
including -- and in addition to decimal (base 10) notation -- those
integer values represented with the reader syntax:

  #<radix>N and #<R>rN 

in any number of radix in incluing 10, 8, 16, and 2 as follows:

 decimal value     4194303    or #10r4194303\n
 octal value       #o17777777 or #8r17777777\n
 hexidecimal value #x3fffff   or #16r3fffff\n
 binary value      #b01111111111111111111111
                or #2r01111111111111111111111\n

Where the particular numeric value 4194303 is more widely understood as:
`raw-byte' 255 and equivalenlty (and more generally) being understood
as the uppermost in the so called `octal range': 0200-0377\n
Emacs displays the `octal range' and otherwise represents this range internally
at the upper bounds of its codespace  as the final range of 127 numeric
character values beginning from the code offset 4194176 \(inclusive\).
Such that the range of raw-bytes 127-255 beginning with the codespace's integer
value 4194176 and extendingto 4194303 e.g.:\n
 \(cons 4194176 \(+ 4194176 \(- 255 128\)\)\)\n
And may more generally be represented in Emacs as:\n
 - decimal range:     4194176 - 4194303\n
 - octal range:       #o17777600 - #o17777777\n
 - hexidecimal range: #x3fff80   - #x3fffff\n
 - binary range:      #b01111111111111110000000 - #b01111111111111111111111\n
 - code-point range:  0x80 - 0xFF\n
:UNICODE-CODESPACE-RANGE
\(cons 0 1114111\) ;;  #o4177777 #x10FFFF\n
:EMACS-CODESPACE-EXTENDED
 \(cons  1114112  ;; #o4200000, #x110000
        4194175\) ;; #o17777577 #x3fff7f\n
:EMACS-CODESPACE-RAW-BYTE-8-BIT
\(cons 4194176 ;; #o17777600 #x3fff80
      4194303\) ;; #o17777777 #x3fffff\n
\(let \(\(raw-cons \(cons \(get-byte nil \(char-to-string #x3fff80\)\)
                      \(get-byte nil \(char-to-string #o17777777\)\)\)\)\)
  \(cons \(unibyte-char-to-multibyte \(car raw-cons\)\)
        \(+ \(unibyte-char-to-multibyte \(car raw-cons\)\)
           \(- \(cdr raw-cons\) \(car raw-cons\)\)\)\)\)\n
\(cons 4194176 \(+ 4194176 \(- 255 128\)\)\)\n
\(eq \(max-char\) #x3fffff\)\n
\(eq \(max-char\) \(1- \(expt 2 22\)\)\)\n
\(eq \(max-char\) \(1- \(lsh \(expt 2 21\) 1\)\)\)\n
\(multibyte-char-to-unibyte #x3fffff\)\n
;; :CHAR-REPESENTATION-BYTE-CODE
`decode-char'
`encode-char'
`get-byte'
`make-bool-vector'
`set-buffer-multibyte'
`enable-multibyte-characters'
`multibyte-syntax-as-symbol'\n
;; :CHAR-REPESENTATION-BYTE-MON
`mon-get-bit-table'
`mon-bool-vector-pp'\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-char-representation :insertp t)
    (mon-help-message-intrp "mon-help-char-representation")))
;;
;;; :TEST-ME (mon-help-char-representation)
;;; :TEST-ME (mon-help-char-representation t)
;;; :TEST-ME (describe-function 'mon-help-char-representation)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-25T12:42:19-04:00Z}#{10167} - by MON>
;;;###autoload
(defun mon-help-char-raw-bytes (&optional intrp)
  "Return raw-bytes 200-377 in buffer named \"*RAW-BYTES*\".\n
The char 4194303 of return value is `max-char'.\n
:EXAMPLE\n\n\(mon-help-char-raw-bytes\)\n
:NOTE Regexps w/ raw-bytes are funky; from info node `(elisp)Syntax of Regexps'\n
 You cannot always match all non-ASCII characters with the regular expression
 `\"[\\200-\\377]\"'.  This works when searching a unibyte buffer or string
 \(:SEE info node `\(elisp\)Text Representations'\), but not in a multibyte
 buffer or string, because many non-ASCII characters have codes above octal
 0377.  However, the regular expression `\"[^\\000-\\177]\"' does match all
 non-ASCII characters \(see below regarding `^'\), in both multibyte and unibyte
 representations, because only the ASCII characters are excluded.\n
;; :CHAR-RAW-BYTE-COERCE
`decode-char'
`encode-char'
`string-to-unibyte'
`string-as-unibyte'
`unibyte-char-to-multibyte'
`multibyte-char-to-unibyte'
`string-to-multibyte'
`string-as-multibyte'
`unibyte-string'
`load-convert-to-unibyte'\n
;; :CHAR-RAW-BYTE-FUNCTIONS
`insert-byte'
`get-byte'
`find-multibyte-characters'
`multibyte-string-p'
`string-bytes'
`byteorder'
`make-bool-vector'
`bool-vector-p'
`make-byte-code'\n
;; :BYTE-FUNCTIONS-POSITION
`position-bytes'
`byte-to-position'
`search-unencodable-char'\n
;; :BYTE-FUNCTIONS-MULTI/UNIBYTE
`set-buffer-multibyte'
`enable-multibyte-characters'\n
;; :MON-CHAR-RAW-BYTE-FUNCTIONS
`mon-cln-eight-bit-raw'\n
:SEE info node `(elisp)Text Representations'
:SEE info node `(elisp)Coding Systems'\n
:SEE-ALSO `mon-help-char-functions', `mon-help-char-charset-functions',
`mon-help-char-coding-functions', `mon-help-char-composition',
`mon-help-char-table-functions', `mon-help-display-table-functions',
`mon-help-char-unidata-table', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `mon-help-char-representation',
`mon-help-diacritics', `mon-help-char-ascii', `mon-help-char-iso-8859-1',
`mon-help-char-ecma-35', `mon-help-char-ecma-48', `mon-help-char-logic',
`mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "p")
  (with-current-buffer (get-buffer-create "*RAW-BYTES*")
    (erase-buffer)
    (save-excursion
      (dolist (raw-byte (number-sequence 4194176 4194303))
        (insert (format "%c <- %d ¦ #o%o ¦ #x%x ¦ "raw-byte raw-byte raw-byte raw-byte)
                (format "(string-as-unibyte (char-to-string %d))\n" raw-byte))))
    (insert (mapconcat 'identity
                       '(";; The raw-bytes for character codepoints in the range:"
                         ";; :OCTAL       #8r17777600 - #8r17777777"
                         ";; :DECIMAL     #10r4194176  - #10r4194303"
                         ";; :HEXADECIMAL #16r3FFF80 -   #16r3FFFFF"
                         ";; :NOTE Unicode tops out at #8r4177777 #10r1114111 #16r10FFFF"
                         ";;       The character code 4194303 is `max-char'."
                         ";;       Internally Emacs codepoints are 22-bit integers."
                         ";;\n") "\n"))
    (display-buffer (current-buffer) t)))
;;
;;; :TEST-ME (mon-help-char-raw-bytes)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-25T13:18:17-04:00Z}#{10167} - by MON>
;;;###autoload
(defun mon-help-binary-representation (&optional insertp intrp)
  "Table enumerating binary representations and bitwise operations.\n
;; :BIT1-TO-BIT9
 ----------------------------------- 
¦256¦128¦ 64¦ 32¦ 16¦ 8 ¦ 4 ¦ 2 ¦ 1 ¦
 ----------------------------------- 
¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ -> 0   #b000000000 #x0   #o0   \(lsh 1 -29\)
¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 1 ¦ -> 1   #b000000001 #x1   #o1   \(lsh 1 0\)
¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 1 ¦ 0 ¦ -> 2   #b000000010 #x2   #o2   \(lsh 1 1\)
¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 1 ¦ 0 ¦ 0 ¦ -> 4   #b000000100 #x4   #o4   \(lsh 2 1\)
¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 1 ¦ 0 ¦ 0 ¦ 0 ¦ -> 8   #b000001000 #x8   #o10  \(lsh 4 1\)
¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 1 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ -> 16  #b000010000 #x10  #o20  \(lsh 8 1\)
¦ 0 ¦ 0 ¦ 0 ¦ 1 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ -> 32  #b000100000 #x20  #o40  \(lsh 16 1\)
¦ 0 ¦ 0 ¦ 1 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ -> 64  #b001000000 #x40  #o100 \(lsh 32 1\)
¦ 0 ¦ 1 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ -> 128 #b010000000 #x80  #o200 \(lsh 64 1\)
¦ 1 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ 0 ¦ -> 256 #b100000000 #x100 #o400 \(lsh 128 1\)
 -----------------------------------\n 
;; :BIT-BYTE-WORD
 ------------------------------------------------------------
¦                      16-Bit-Word                           ¦
 ------------------------------------------------------------
¦byte-2   \(short int\)                    ¦byte-1             ¦ \(8 bits per byte\)
 ------------------------------------------------------------
¦bit 13-16            ¦bit 9-12          ¦bit 5-8    ¦bit 1-4¦
 ------------------------------------------------------------
¦32768¦16384¦8192¦4096¦2048¦1024¦512¦256¦128¦64¦32¦16¦8¦4¦2¦1¦
¦    0¦    0¦   0¦   0¦   0¦   0¦  0¦  0¦  0¦ 0¦ 0¦ 0¦0¦0¦0¦0¦
 ------------------------------------------------------------\n
;; :BIT-HEX-BIN-OCT
 --------------------------------------
¦ HEX ¦ DEC  ¦ BINARY ¦ OCTAL ¦ BINARY ¦
¦--------------------------------------¦                       
¦  0  ¦  0   ¦  0000  ¦   0   ¦  000   ¦  :OCTAL->BINARY
¦  1  ¦  1   ¦  0001  ¦   1   ¦  001   ¦  #o17 => #10r15
¦  2  ¦  2   ¦  0010  ¦   2   ¦  010   ¦     1 -> 001            
¦  3  ¦  3   ¦  0011  ¦   3   ¦  011   ¦              \\__ #b001111 
¦  4  ¦  4   ¦  0100  ¦   4   ¦  100   ¦              /   \(eq #b001111 #o17\)
¦  5  ¦  5   ¦  0101  ¦   5   ¦  101   ¦     7 -> 111
¦  6  ¦  6   ¦  0110  ¦   6   ¦  110   ¦
¦  7  ¦  7   ¦  0111  ¦   7   ¦  111   ¦                                       
¦  8  ¦  8   ¦  1000  ¦       ¦        ¦                         
¦  9  ¦  9   ¦  1001  ¦       ¦        ¦  :HEX->BINARY                      
¦  A  ¦  10  ¦  1010  ¦       ¦        ¦  #xA7 => #10r167
¦  B  ¦  11  ¦  1011  ¦       ¦        ¦     A -> 1010         
¦  C  ¦  12  ¦  1100  ¦       ¦        ¦               \\__ #b10100111
¦  D  ¦  13  ¦  1101  ¦       ¦        ¦               /   \(eq #b10100111 #xA7\)
¦  E  ¦  14  ¦  1110  ¦       ¦        ¦     7 -> 0111
¦  F  ¦  15  ¦  1111  ¦       ¦        ¦
 --------------------------------------\n
;; :BITWISE-LOGICAL-AND-TABLE
`logand'
 ___________________________ 
|        |        |         |
| :VAL-1 | :VAL-2 | :RESULT |
|________|________|_________|
|        |        |         |
|   0    |   0    |    0    | \(logand 0 0\)
|   0    |   1    |    0    | \(logand 0 1\)
|   1    |   0    |    0    | \(logand 1 0\)
|   1    |   1    |    1    | \(logand 1 1\)
|________|________|_________|\n
;; :BITWISE-LOGICAL-OR-TABLE
`logior'
 ___________________________
|        |        |         |
| :VAL-1 | :VAL-2 | :RESULT |
|________|________|_________|
|        |        |         |
|   0    |    0   |     0   | \(logior 0 0\)
|   0    |    1   |     1   | \(logior 0 1\)
|   1    |    0   |     1   | \(logior 1 0\)
|   1    |    1   |     1   | \(logior 1 1\)
|________|________|_________|\n
;; :BITWISE-LOGICAL-EXCLUISVE-OR-TABLE
`logxor'
 ___________________________
|        |        |         |
| :VAL-1 | :VAL-2 | :RESULT |
|________|________|_________|
|        |        |         |
|   0    |    0   |     0   | \(logxor 0 0\)
|   0    |    1   |     1   | \(logxor 0 1\)
|   1    |    0   |     1   | \(logxor 1 0\)
|   1    |    1   |     0   | \(logxor 1 1\)
|________|________|_________|\n
;; :BITWISE-LOGICAL-NOT
`lognot'                                 ; Complement
         #b00000000000000000000000000101 ;=>  5
\(lognot #b00000000000000000000000000101\) ;=> -6\n
;; :BIT-SHIFT-LOGICAL-TABLE
 128  64  32  16  8   4   2   1
 -------------------------------
| 0 | 0 | 0 | 0 | 0 | 1 | 1 | 0 |      ->  6
 -------------------------------
                    __! __!
                 __/ __/ 
               ./  ./
 128  64  32  16  8   4   2   1
 -------------------------------
| 0 | 0 | 0 | 1 | 1 | 0 | 0 | 0 |      ->  24
 -------------------------------\n
;; :BIT-SHIFT
`lsh'                               ; Logical shift
`ash'                               ; Arithmetic shift\n
     #b00000110                     ;=> 6 
\(lsh #b00011000 -2\)                 ;=> 6\n
     #b00011000                     ;=> 24
\(lsh #b00000110  2\)                 ;=> 24\n
\(lsh 6     2\)                       ;=> 24
\(lsh #b110 #b10\)                    ;=> 24\n
\(lsh 24       -2\)                   ;=> 6
\(lsh #b11000 \(lognot #b01\)\)         ;=> 6\n
\(ash 4      1\)                      ;=> 8
\(lsh 4      1\)                      ;=> 8
\(ash #b100  #b01\)                   ;=> 8\n
\(lsh  -8              -1\)           ;=> 536870908
\(ash  -8              -1\)           ;=> -4
\(ash \(lognot #b111\) \(lognot #b0\)\)   ;=> -4\n
;; :BIT-29
`most-positive-fixnum'
`most-negative-fixnum'\n
\(identity most-positive-fixnum\)         ;=>  536870911 \(#o3777777777, #x1fffffff\)
\(identity most-negative-fixnum\)         ;=> -536870912 \(#o4000000000, #x20000000\)\n
    #b11111111111111111111111111111                     ;=>  536870911
\(1+ #b11111111111111111111111111111\)                    ;=> -536870912
\(1-     \(expt 2 29\)\)                                    ;=>  536870911
\(lognot \(expt 2 29\)\)                                    ;=>  536870911
\(lsh    #b11111111111111111111111111111 29\)             ;=> -536870912   
\(lognot #b11111111111111111111111111111\)                ;=> -536870912
\(lognot most-positive-fixnum\)                           ;=> -536870912
\(lsh \(lognot most-negative-fixnum\) 29\)                  ;=> -536870912
\(lognot \(lsh most-positive-fixnum 29\)\)                  ;=>  536870911
\(lognot most-negative-fixnum\)                           ;=>  536870911
\(lsh most-negative-fixnum -1\)                           ;=>  268435456
\(ash most-negative-fixnum -1\)                           ;=> -268435456
\(lsh \(lognot #b11111111111111111111111111111\) -29\)      ;=>  1
\(ash \(lognot #b11111111111111111111111111111\) -29\)      ;=> -1
\(eq \(lognot most-positive-fixnum\) most-negative-fixnum\) ;=>  t
\(eq \(lognot most-negative-fixnum\) most-positive-fixnum\) ;=>  t
\(eq      #b11111111111111111111111111111     most-positive-fixnum\)   ;=> t
\(eq \(lsh #b11111111111111111111111111111 29\) most-negative-fixnum\)   ;=> t\n
;; :BIT-BOUNDARIES
\(let \(\(gthr '\(1\)\)\)
  \(dotimes \(i 28 \(setq gthr \(nreverse gthr\)\)\)
    \(push \(* \(car gthr\) 2\) gthr\)\)
  \(setq gthr `\(,@gthr \(,\(length gthr\) ,\(reduce '+ gthr\)\)\)\)\)\n
;; :BIT-FLOAT
`logb'\n
;; :BIT-VECTOR-BOOLEAN
`make-bool-vector'
`bool-vector-p'
`mon-bool-vector-pp'\n
:ALIASED-BY `mon-help-bitwise-functions'\n
:SEE info node `(elisp)Bool-vectors'
:SEE info node `(elisp)Bitwise Operations'\n
:SEE-ALSO `mon-help-number-functions', `mon-help-char-functions',
`mon-help-char-charset-functions', `mon-help-char-coding-functions',
`mon-help-char-composition', `mon-help-char-table-functions',
`mon-help-display-table-functions', `mon-help-char-unidata-table',
`mon-help-char-raw-bytes', `mon-help-binary-representation',
`mon-help-char-representation', `mon-help-diacritics', `mon-help-char-ascii',
`mon-help-char-iso-8859-1', `mon-help-char-ecma-35', `mon-help-char-ecma-48',
`mon-help-char-logic', `mon-help-key-functions', `mon-help-read-functions',
`mon-help-print-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-binary-representation :insertp t)
    (mon-help-message-intrp "mon-help-binary-representation")))
;;
;;; :TEST-ME (describe-function 'mon-help-binary-representation)
;;; :TEST-ME (mon-help-binary-representation t)
;;; :TEST-ME (apply 'mon-help-binary-representation nil '(t))

 
;;; ==============================
;; :TODO `mon-help-binary-functions' and alias to `mon-help-byte-functions'
;; include the `bindat-*' functions
;; ;;
;;
;; `logb'
;; `byteorder'
;; `get-byte'
;; `insert-byte'
;; `string-bytes'
;; `unibyte-string'
;; `load-convert-to-unibyte'
;;
;; :BYTE-BOOL-VECTOR
;; `make-bool-vector'
;; `bool-vector-p'
;;
;; ;; :BYTE-FUNCTIONS-POSITION
;; `position-bytes'
;; `byte-to-position'
;; 
;; :BTYE-LOGIC
;; `logior', `logand', `logxor' `lognot' `lsh', `ash' 
;;
;; :BYTE-BUFFER-FIND
;; `find-multibyte-characters'
;; `search-unencodable-char'
;;
;; :BYTE-BUFFER-MULTIBYTE
;; `set-buffer-multibyte'
;; `enable-multibyte-characters'\n
;; `default-enable-multibyte-characters'
;; 
;; `decode-char'
;; `encode-char'
;;
;; `binary-overwrite-mode'
;; `buffer-file-typer'
;;
;; :BYTE-CONVERT
;; `multibyte-string-p'
;; `multibyte-char-to-unibyte'
;; `string-to-unibyte'
;; `string-as-unibyte'
;; `unibyte-char-to-multibyte'
;; `multibyte-char-to-unibyte'
;; `string-to-multibyte'
;; `string-as-multibyte'
;; `set-buffer-multibyte' { t | nil | to } 
;;
;;
;; :BYTE-FUNCTIONS-MON-LOCAL
;; `mon-bool-vector-pp'
;; `mon-get-bit-table'
;;
;; XREFS `mon-help-binary-representation', `mon-help-char-raw-bytes',

 
;;; ==============================
;;; :CHANGESET 2063
;;; :CREATED <Timestamp: #{2010-08-10T18:22:49-04:00Z}#{10322} - by MON KEY>
;;;###autoload
(defun mon-help-symbol-functions (&optional insertp intrp)
  "List of functions and variables related to Emacs symbol identity.\n
Enumerates functions, variables, and special forms, and C primitives related
symbol construction, \(un\)intermment, binding, unbinding, byte compiling,
evaling, interrogation, deconstructing, etc.\n
;; :SYMBOL-CONSTRUCTORS-LISP
`defconst'                                   ;<SPECIAL-OPERATOR>
`defun'                                      ;<SPECIAL-OPERATOR>
`defmacro'                                   ;<SPECIAL-OPERATOR>
`defsubst'
`defvar'                                     ;<SPECIAL-OPERATOR>\n
`&rest'                                      ; :NOTE \(symbol-plist '&rest\)
`&optional'                                  ; :NOTE \(symbol-plist '&optional\)
;; :SYMBOL-CONSTRUCTORS-LISP-CL
`defmacro*'
`defsubst*'
`defun*'
`define-compiler-macro' 
`defstruct'
`deftype'
`&key'                                       ; :NOTE \(symbol-plist '&optional\)\n
;; :SYMBOL-CONSTRUCTORS-LISP-EIEIO         ; :NOTE API, additional magic occurs.
`defclass'                                   :SEE `mon-help-eieio-defclass'
`defmethod'                                  :SEE `mon-help-eieio-methods'
`defgeneric'\n
;; :SYMBOL-CONSTRUCTORS-LISP-EMACS
`autoload'
`deftheme'
`defcustom'
`custom-declare-variable'
`defgroup'
`defface'
`define-prefix-command'\n
;; :SYMBOL-CONSTRUCTORS-INLINE 
`inline'                                   ; :NOTE \(get 'inline 'byte-optimizer\)
`defsubst'                                 ; :NOTE \(get 'defsubst 'byte-hunk-handler\)\n
`defsubst*'
:NOTE Following forms are evaluated by bytecomp.el and/or associated libraries.
 \(fset 'inline 'progn\)
 \(byte-defop-compiler-1 inline byte-compile-progn\)\n
;; :SYMBOL-CONSTRUCTORS-MACRO-COMPILER
`compiler-macroexpand'
`byte-compile-initial-macro-environment'     ;<CONSTANT>
`byte-compile-macro-environment'             :<VARIABLE>
`cl-compiler-macro`                          ;<PROPERTY>\n
;; :SYMBOL-CONSTRUCTORS-SET-EMACS
`setq'                                       ;<SPECIAL-OPERATOR>
`psetq'
`set'
`fset'
`set-variable'\n
;; :SYMBOL-CONSTRUCTORS-SET-CL-GENERALIZED
`defsetf'
`define-modify-macro'
`define-setf-expander'
`define-setf-method'
`setf'
`psetf'\n
;; :SYMBOL-CONSTRUCTORS-TEMPORARY
`internal-interpreter-environment'           ;<LEXBIND-VARIABLE>
`gensym'
`gentemp'                                    ; :NOTE Misnomer, interns a symbol
`*gensym-counter*'                           ;<VARIABLE>
`make-symbol'\n
;; :SYMBOL-CONSTRUCTORS-LOCAL
`lambda'
`let'                                        ;<SPECIAL-OPERATOR>
`let*'                                       ;<SPECIAL-OPERATOR>
`flet'
`labels'
`macroloet'
`symbol-macrolet'
`condition-case'                              ; :NOTE VAR arg is a local environ
`purify-flag'                                 ;<VARIABLE>
`max-specpdl-size'                            ;<VARIABLE>\n
;; :SYMBOL-CONSTRUCTORS-EMACS-OBJECT-LOCAL
`set-default'
`setq-default'                                ;<SPECIAL-OPERATOR>
`make-local-variable'
`make-variable-buffer-local'
`make-variable-frame-local'                   ;<DEPRECATED>\n
;; :SYMBOL-PREDICATES
`bound-and-true-p'
`boundp'
`byte-code-function-p'
`commandp'
`fboundp'
`featurep'
`functionp'                                   ;:NOTE C-Primitive in lexbind branch.
`keywordp'
`null'
`subrp'
`symbolp'
`apropos-macrop'
`user-variable-p'
`custom-variable-p'\n
;; :SYMBOL-PREDICATES-EMACS-OBJECT-LOCAL
`default-boundp'
`local-variable-if-set-p'
`local-variable-p'
`risky-local-variable-p'
`safe-local-variable-p'\n
;; :SYMBOL-GETTERS
`type-of'
`identity'
`indirect-variable'
`indirect-function'
`interactive-form'
`symbol-function'
`symbol-name'
`symbol-value'
`symbol-file'
`symbol-plist'
`subr-arity'   ; :NOTE Return \( <MIN-ARG> . { <MAX-ARG> | many | unevalled } \)
                              \(subr-arity \(indirect-function 'signal\)\)
                              \(subr-arity \(indirect-function 'apply\)\)
                              \(subr-arity \(indirect-function 'unwind-protect\)\)\n
;; :SYMBOL-GETTERS-EMACS-OBJECT-LOCAL
`kill-local-variable'
`kill-all-local-variables'
`hack-local-variables'
`buffer-local-variables'
`default-value'
`buffer-local-value'\n
;; :SYMBOL-TABLE
`intern'
`unintern'
`obarray'
`intern-soft'
`mapatoms' 
`makunbound'
`fmakunbound'
`set-advertised-calling-convention'
`advertised-signature-table'\n
;; :SYMBOL-INDIRECTION
`fset' 
`defalias'
`defvaralias'
`indirect-variable'
`indirect-function'\n
;; :SYMBOL-OBSOLETE
`define-obsolete-function-alias'
`define-obsolete-variable-alias'
`define-obsolete-face-alias'
`make-obsolete-variable'
`make-obsolete'\n
;; :SYMBOL-SPECIAL-FORMS   ; :SEE info node `(elisp)Special Forms'
`and'                      ; :NOTE Excluding those tagged \"<SPECIAL-OPERATOR>\"
`catch'
`cond'
`condition-case'
`if'
`interactive'
`or'
`prog1'
`prog2'
`progn'
`save-current-buffer'
`save-excursion'
`save-restriction'
`save-window-excursion'
`track-mouse'
`unwind-protect'
`while'
`with-output-to-temp-buffer'\n
;; :SYMBOL-SYNTAX-MODIFIERS
`function'                                   ;<SPECIAL-OPERATOR>
`quote'                                      ;<SPECIAL-OPERATOR>
`backquote'
`backquote-backquote-symbol'                 ;<CONSTANT>
`backquote-unquote-symbol'                   ;<CONSTANT>
`backquote-splice-symbol'                    ;<CONSTANT> 
\"`\" \",\" \",@\"                           ; :NOTE lread.c's \",.\" Qcomma_dot
\"'\" 
\"#'\"
\":\"
\"#s\"
\"#N=\" \"#N#\"
\"#:\"
\"#&\"
\"#@<COUNT>\"
\"#$\"
\"#\" \"#b\" \"#x\" \"#o\" \"#r<N>\" \"\\\\?\"\n
;; :SYMBOL-EVAL
`dont-compile'
`eval-and-compile'
`eval-when-compile'
`eval-when'
`eval-after-load'
`eval'
`funcall'
`apply'
`load-time-value'
`read'\n
;; :SYMBOL-EMACS-OBJECT-LOCAL-VARIABLES
`before-hack-local-variables-hook'
`hack-local-variables-hook'
`file-local-variables-alist'
`safe-local-variable-values'
`risky-local-variable-p'
`safe-local-variable-p'
`ignored-local-variables'
`enable-local-eval'
`enable-local-variables'
`safe-local-eval-forms'\n
;; :SYMBOL-PROPERTIES-EMACS-OBJECT-LOCAL
`safe-local-variable-values`
`risky-local-variable`
`safe-local-eval-function`                   ;<PROPERTY>
`permanent-local`                            ;<PROPERTY>\n
;; SYMBOL-PROPERTIES-EMACS-OBJECT-LOCAL-RISKY
-command
-frame-alist
-function
-functions
-hook
-hooks
-form
-forms
-map
-map-alist
-mode-alist
-program
-predicate\n
;; :SYMBOL-PRINTING-VARIABLES
`read-circle'
`printable-chars'
`print-circle'
`print-gensym'
`print-continuous-numbering'
`print-number-table'\n
;; :SYMBOL-BYTE
`byte-code-function-p'
`fetch-bytecode'                             :SEE :FILE eval.c
`byte-code'                                  :SEE :FILE bytecode.c
`make-byte-code'                             :SEE :FILE alloc.c
`make-bool-vector'
`string-bytes'\n
;; :SYMBOL-COMPILE-OPCODES                   :SEE :FILE bytecomp.el bytecomp.c 
`byte-defop'                                 ;<MACRO>
`byte-code-vector' 
`byte-stack+-info'
`byte-constant-limit'
`byte-goto-ops'\n
;; :SYMBOL-COMPILE
`byte-code'
`byte-compile'
`byte-compile-constp'
`byte-compile-file'
`byte-compile-lambda'
`byte-compile-lambda-form' 
`byte-compile-output-as-comment'
`byte-compile-output-docform'
`byte-compile-dynamic'                       ;<VARIABLE>
`byte-compile-warnings'                      ;<VARIABLE>
`byte-optimize-log'                          ;<VARIABLE>
`compile-defun'
`declare-function'
`disassemble'
`make-byte-code'
`macro-declaration-function'\n
;; :SYMBOL-C-PRIMITIVE-VARS
DEFVAR_BOOL
DEFVAR_INT
DEFVAR_LISP
`byte-boolean-vars'
`byte-optimize-log'\n 
;; :SYMBOL-LEXBIND
`special-variable-p'                          ;<LEXBIND-FUCTION>
`functionp'                                   ;:NOTE C-Primitive in lexbind branch.
`curry'                                       ;<LEXBIND-FUNCTION>
`lexical-binding'                             ;<LEXBIND-VARIABLE>\n
;; :SYMBOL-VALUES-MON-LOCAL
`mon-help-byte-code-vector-symbols'
`mon-help-permanent-locals-find'
`mon-help-byte-optimizer-find'
`mon-map-obarray-symbol-plist-props'
`*mon-help-permanent-locals*'
`*mon-help-byte-optimizer-vals*'
`*mon-help-autoload-vars*'
`*mon-help-pure-functions*'
`*mon-help-side-effect-free*'
`*mon-help-side-effect-and-error-free*'
`*mon-help-subrs*'
`*mon-help-subrs-false*'\n
;; :SYMBOL-COMPONENTS
Print name            :SEE info node `(elisp)Creating Symbols'
Value                 :SEE info node `(elisp)Accessing Variables'
Function              :SEE info node `(elisp)Function Cells'
Property list         :SEE info node `(elisp)Property Lists'\n
:NOTE Following is an example of the byte-compiler's gernation of \"#@LENGTH\"
as per `byte-compile-output-as-comment':\n
\(byte-compile-lambda \(lambda \(x y\) \"some lambda form\" \(+ x y\)\)\)\n
\(let \(\(bytecomp-outbuffer \(current-buffer\)\)\)
  \(byte-compile-output-as-comment
   \(byte-compile-lambda \(lambda \(x y\) \"some lambda form\" \(+ x y\)\)\) t\)\)\n
\(let \(\(cf \(byte-compile-lambda \(lambda \(x y\) \"some lambda form\" \(+ x y\)\)\)\)\)
 \(type-of  cf\)\)\n
;; `position-bytes' is the index into the file as a byte ind
:SEE info node `(elisp)Byte-Code Objects'
:SEE info node `(elisp)Byte Compilation'
:SEE info node `(elisp)Declaring Functions'
:SEE info node `(elisp)Circular Objects'
:SEE :FILE eval.c bytecode.c alloc.c lread.c
:SEE :FILE `byte-run.el', `bytecomp.el', `byte-opt.el'\n
:SEE-ALSO `mon-help-byte-compile-functions' .\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-symbol-functions :insertp t)
    (mon-help-message-intrp "mon-help-symbol-functions")))
;;
;;; :TEST-ME (mon-help-symbol-functions)
;;; :TEST-ME (mon-help-symbol-functions t)
;;; :TEST-ME (apply 'mon-help-symbol-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2115
;;; :CREATED <Timestamp: #{2010-09-08T16:07:55-04:00Z}#{10363} - by MON KEY>
;;;###autoload
(defun mon-help-byte-compile-functions (&optional insertp intrp)
  "List of funcitons and variables related to byte-compiling Emacs lisp.\n
;; :BYTE-COMPILE-BYTE-CODE
`byte-code'
`make-byte-code'
`fetch-bytecode'
`gnus-byte-code'
`binary-overwrite-mode'\n
;; :BYTE-COMPILE
`byte-compile'
`byte-compile-eval'
`byte-compile-eval-before-compile'
`byte-compile-file'
`byte-compile-cl-file-p'
`byte-compile-const-symbol-p'            ;<DEFSUBST>
`byte-compile-constp'                    ;<MACRO>
`byte-compile-set-symbol-position'
`byte-compile-obsolete'
`byte-compile-fdefinition'
`byte-compile-arglist-signature'
`byte-compile-arglist-signatures-congruent-p'
`byte-compile-print-syms'
`byte-compile-progn'
`byte-compile-close-variables'           ;<MACRO>
`byte-force-recompile'
`compile-defun'

`byte-compile-from-buffer'
`byte-compile-fix-header'
`byte-compile-insert-header'
`byte-compile-output-file-form'
`byte-compile-output-docform'
`byte-compile-keep-pending'
`byte-compile-flush-pending'
`byte-compile-const-symbol-p'

`macro-declaration-function'
`byte-compile-defmacro-declaration'
`byte-compile-sexp'
`byte-compile-byte-code-maker'
`byte-compile-byte-code-unmake'

;; :BYTE-COMPILE-BYTE-HUNK-HANDLERS
`byte-compile-file-form'
`byte-compile-file-form-defmumble'
`byte-compile-file-form-defsubst'
`byte-compile-file-form-autoload'
`byte-compile-file-form-defvar'
`byte-compile-file-form-define-abbrev-table'
`byte-compile-file-form-custom-declare-variable'
`byte-compile-file-form-require'
`byte-compile-file-form-progn'
`byte-compile-file-form-with-no-warnings'
`byte-compile-file-form-eval'
`byte-compile-file-form-defun'
`byte-compile-file-form-defmacro'

;; :BYTE-COMPILE-CL
`byte-compile-find-cl-functions'
`byte-compile-cl-functions'
`byte-compile-cl-warn'            ; <- Fucker that investigates forms for cl symbols.
`byte-compile-file-form-require'  ; <- Fucker that voices CL runtime warnings:
                                        \(byte-compile-warn \"cl package required at runtime\"\)
                                       As per return value of:     
                                        \(byte-compile-warning-enabled-p 'cl-functions\)\n
:NOTE These don't have warnings because have \"suitable\" compiler macros:
 \(remq nil \(delete-dups
  \(mapcar #'\(lambda \(cl-s\)
             \(when \(eq \(get cl-s 'byte-compile\) 'cl-byte-compile-compiler-macro\) cl-s\)\)
        byte-compile-cl-functions\)\)\)\n
:NOTE These are the byte-compiled during a macroexpansion:
 \(mon-map-obarray-symbol-plist-props 'cl-compiler-macro\)\n
;; :BYTE-COMPILE-WARNING
`byte-compile-print-syms'
`byte-compile-warning-enabled-p'
`byte-compile-arglist-warn'
`byte-compile-callargs-warn'
`byte-compile-cl-warn'
`byte-compile-format-warn'
`byte-compile-nogroup-warn'
`byte-compile-report-error'
`byte-compile-warn'
`byte-compile-warn-about-unresolved-functions'
`byte-compile-warn-obsolete'
`byte-compile-warning-series'                   ;<NOOP>
`displaying-byte-compile-warnings'              ;<MACRO>
`warning-series'
`byte-compile-warning-types'                    ;<CONSTANT>\n
;; :BYTE-COMPILE-OPCODE
`byte-defop'
`byte-defop-compiler-1'
`byte-extrude-byte-code-vectors'\n
;; :BYTE-COMPILE-OPTIMIZER
`byte-compile-butlast'
`byte-compile-inline-expand'
`byte-compile-log-lap'                          ;<MACRO>
`byte-compile-log-lap-1'
`byte-compile-nilconstp'
`byte-compile-splice-in-already-compiled-code'
`byte-compile-trueconstp'
`byte-compile-unfold-lambda'
`byte-compile-unfold-lambda'
`byte-decompile-bytecode'
`byte-decompile-bytecode-1'
`byte-inline-lapcode'
`byte-inline-lapcode'
`byte-optimize-all-constp'
`byte-optimize-and'
`byte-optimize-apply'
`byte-optimize-approx-equal'
`byte-optimize-associative-math'
`byte-optimize-binary-predicate'
`byte-optimize-body'
`byte-optimize-cond'
`byte-optimize-delay-constants-math'
`byte-optimize-divide'
`byte-optimize-featurep'
`byte-optimize-form'
`byte-optimize-form-code-walker'
`byte-optimize-funcall'
`byte-optimize-identity'
`byte-optimize-if'
`byte-optimize-inline-handler'
`byte-optimize-lapcode'
`byte-optimize-letX'
`byte-optimize-logmumble'
`byte-optimize-minus'
`byte-optimize-multiply'
`byte-optimize-nonassociative-math'
`byte-optimize-nth'
`byte-optimize-nthcdr'
`byte-optimize-or'
`byte-optimize-plus'
`byte-optimize-predicate'
`byte-optimize-quote'
`byte-optimize-set'
`byte-optimize-while'
`byte-optimize-zerop'\n
;; :BYTE-COMPILE-INSPECT-LOG
`byte-compile-log'                              ;<MACRO>
`byte-compile-log-file'
`byte-compile-log-1'
`byte-compile-log-warning'\n
;; :BYTE-COMPILE-INSPECT-DISASSEMBLE
`disassemble'
`disassemble-internal'
`disassemble-1'
`disassemble-offset'                         ; :NOTE Dynamic-scope magic here.\n
;; :BYTE-COMPILE-INSPECT
`byte-decompile-bytecode'
`byte-decompile-bytecode-1'
`byte-code-meter'                            ; :NOTE non-functional since circa 1991/2
`display-call-tree'
`byte-compile-generate-call-tree'
`byte-compile-dest-file'                     ;:NOTE Also a <VARIABLE>\n
;; :BYTE-COMPILE-VARIABLES-OPTIMIZE-OPCODE
`byte-code-vector'
`byte-boolean-vars'                          ; :NOTE DEFVAR_BOOL updates automatically
`byte-stack+-info'
`byte-goto-ops'
`byte-constant-limit'
`byte-tagref-ops'
`byte-conditional-ops'
`byte-after-unbind-ops'
`byte-compile-side-effect-and-error-free-ops'
`byte-compile-side-effect-free-ops'
`byte-after-unbind-ops'
`byte-compile-splice-in-already-compiled-code'
`byte-conditional-ops'
`byte-constref-ops'
`byte-tagref-ops'\n
;; :BYTE-CODE-COMPILE-VARIABLES
`bytecomp-version-regexp'
`no-byte-compile'
`byte-compile-delete-errors'
`byte-compile-noruntime-functions'
`old-style-backquotes'
`byte-compile-dynamic-docstrings'\n
;; :BYTE-CODE-COMPILE-VARIABLES-ENVIRONMENT
`byte-compile-initial-macro-environment'
`byte-compile-macro-environment'
`byte-compile-function-environment'\n
;; :BYTE-CODE-COMPILE-VARIABLES-STATEFUL
`print-gensym-alist'
`bytecomp-outbuffer'                            ; :NOTE :FILE cl.el binds this
`byte-compile-last-warned-form'
`byte-compile-last-logged-file'
`byte-compile-last-position'
`byte-compile-read-position'
`byte-compile-output'
`byte-compile-tag-number'  
`byte-compile-depth'
`byte-compile-maxdepth'
`byte-compile-unresolved-functions'
`byte-compiler-error-flag'
`byte-compile-free-references'
`byte-compile-free-assignments'
`byte-compile-constants'
`byte-compile-variables'\n
:BYTE-COMPILE-VARIABLES-INSPECT
`disassemble-column-1-indent'
`disassemble-column-2-indent'
`disassemble-recursive-indent'
`byte-compile-generate-call-tree'
`byte-compile-call-tree-sort'
`byte-compile-call-tree'\n
;; :BYTE-COMPILE-VARIABLES-INSPECT-LOG
`byte-compile-current-form'                  ; :NOTE Somtimes a keyword arg :end
`byte-compile-dest-file'                     ; :NOTE also a <FUNCTION>
`byte-compile-current-file'
`byte-compile-current-group'
`byte-compile-current-buffer'
`byte-optimize-log'\n
;; :BYTE-COMPILE-PROPERTIES
`byte-compile`                               ; :NOTE Also a <FUNCTION>
`byte-code-vector`
`byte-compile-format-like`
`byte-compile-negated-op`
`byte-hunk-handler`
`byte-obsolete-info`
`byte-obsolete-variable`
`byte-opcode-invert`
`byte-optimizer`
`byte-stack+-info`
`byte-hunk-handler`\n
;; :BYTE-COMPILE-PROPERTY-VALUES
`byte-compile-splice-in-already-compiled-code`
`byte-compile-file-form-defsubst`
`byte-compile-file-form-autoload`             
`byte-compile-file-form-defvar`
`byte-compile-file-form-define-abbrev-table`
`byte-compile-file-form-custom-declare-variable`
`byte-compile-file-form-require`
`byte-compile-file-form-progn`
`byte-compile-file-form-eval`
`byte-compile-file-form-with-no-warnings`
`byte-compile-file-form-defun`
`byte-compile-file-form-defmacro`
:NOTE (get 'byte-code 'byte-compile) 
      (get 'no-byte-compile 'safe-local-variable)
      (mapcar #'(lambda (bhh)
                  `(,bhh . ,(get bhh 'byte-hunk-handler)))
              '(defsubst autoload defvar defconst define-abbrev-table
                 custom-declare-variable require progn prog1 prog2
                 with-no-warnings defun eval defmacro))
;; :BYTE-COMPILE-WARNING-TYPES
`callargs`
`cl-functions`
`constants`
`free-vars`
`interactive-only`
`lexical`
`make-local`
`mapcar`
`noruntime`
`obsolete`
`redefine`
`suspicious`
`unresolved`\n
;; BYTE-COMPILE-LOCAL-VARIABLES
,----
| The `bytecomp-' prefix is applied to all local variables with
| otherwise common names in this and similar functions for the sake
| of the boundp test in byte-compile-variable-ref.
| :SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2008-01/msg00237.html')
| :SEE (URL `http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-02/msg00134.html')
| :NOTE Similar considerations apply to command-line-1 in startup.el.
| 
| `byte-compile-from-buffer', `byte-compile-file', `byte-recompile-directory',
| `byte-compile-keep-pending', `byte-compile-file-form-defmumble'
|
`----
`bytecomp-handler`
`bytecomp-arg` 
`bytecomp-dest`
`bytecomp-directories`
`bytecomp-directory`
`bytecomp-file`
`bytecomp-file-dir`
`bytecomp-filename`
`bytecomp-force`
`bytecomp-outbuffer`
`bytecomp-inbuffer`
`bytecomp-res`
`bytecomp-source`
`bytecomp-this-one`
`bytecomp-that-one`
`bytecomp-this-kind`
`bytecomp-that-kind`
`bytecomp-name`\n
;; :NOTE `cl-compiling-file' checks the value of `bytecomp-outbuffer` which
`byte-compile-from-buffer' binds locally.  Also, `load-time-value' leverages
this value to trigger loadtime evaluation of its FORM arg.
;; BYTE-COMPILE-BUFFER-ENVIRONMENT
;; (setq buffer-file-coding-system nil)
;; (set-buffer-multibyte t)
;; (float-output-format nil)
;; (print-length nil)
;; (case-fold-search nil)
;; (print-level nil)
;; (setq overwrite-mode 'overwrite-mode-binary)
;; (enable-local-variables :safe)
;; (enable-local-eval nil)
;; (default-value 'major-mode) 'emacs-lisp-mode 
;; (normal-mode t)
;;
;; :BYTE-COMPILE-LOAD-MAGIC  
,---- :SEE `byte-compile-insert-header'
| The magic number of .elc files is \";ELC\", or 0x3B454C43.  After
| that is the file-format version number \(18, 19, 20, or 23\) as a
| byte, followed by some nulls.  The primary motivation for doing
| this is to get some binary characters up in the first line of
| the file so that `diff' will simply say \"Binary files differ\"
| instead of actually doing a diff of two .elc files.  An extra
| benefit is that you can add this to /etc/magic:
|
|  0	string		;ELC		GNU Emacs Lisp compiled file,
|  >4	byte		x		version %d
`----
 ; => #x3b \(char-to-string #x3B\)
 E => #x45 \(char-to-string #x45\)
 L => #x4c \(char-to-string #x4C\)
 C => #x43 \(char-to-string #x43\)
 ;ELC => #x3B454C43\n

;; :BYTE-COMPILE-BYTE-CODE-OBJECTS
Following are the types of objects that `indirect-function' may return.\n
;; :COMPILED-FUNCTION
#[ \( <ARG-LIST> \) 
     <BYTE-STR> 
   [ <CONSTANTS>* ]  
     <CONST-CNT-INT>  ;; :NOTE 0 indexed
     \( <PATHNAME-FILE> . <OFFSET>\) ]\n
;; :COMPILED-MACRO
\(macro . 
       #[ \( <ARG-LIST> \) 
            <BYTE-STR> 
          [ <CONSTANTS>* ]  
            <CONST-CNT-INT>  ;; :NOTE 0 indexed
          \( <PATHNAME-FILE> . <OFFSET>\) ] \)\n
;; :SUBR-PRIMITIVE 
#<subr `SYMBOL-NAME`>\n
;; :INTERPRETED-FUNCTION
\(lambda \( <ARG-LIST> \) <DOCSTR> \( <FUNCTION-BODY> \)\)\n
;; :INTERPRETED-MACRO
\(macro lambda \( <ARG-LIST> \) <DOCSTR> \( <MACRO-BODY> \)\)\n
;; :AUTOLOAD-SYMBOL  ;:NOTE <TYPE> <- macro | keymap
\(autoload  <FILENAME> <DOCSTRING-OFFSET> <INTERACTIVE> <TYPE> \)\n

:NOTE Interestingly bytecomp.el makes use of `letf' :)
:SEE :FILE disass.el bytecomp.el byte-opt.el byte-run.el bytecode.c 
:SEE-ALSO `mon-help-byte-optimizer-find', `mon-help-symbol-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-byte-compile-functions :insertp t)
    (mon-help-message-intrp "mon-help-byte-compile-functions")))
;;
;;; :TEST-ME (mon-help-byte-compile-functions )
;;; :TEST-ME (mon-help-byte-compile-functions t)
;;; :TEST-ME (describe-function 'mon-help-byte-compile-functions)
;;; :TEST-ME (apply 'mon-help-byte-compile-functions nil (t)

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-11T15:07:34-04:00Z}#{10323} - by MON KEY>
(defun mon-help-byte-code-vector-symbols (&optional insertp intrp)
  ""
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-byte-code-vector-symbols :insertp t)
    (mon-help-message-intrp "mon-help-byte-code-vector-symbols")))
;;
;;; :TEST-ME (mon-help-byte-code-vector-symbols)
;;; :TEST-ME (mon-help-byte-code-vector-symbols t)
;;; :TEST-ME (documentation 'mon-help-byte-code-vector-symbols)
;;; :TEST-ME (apply 'mon-help-byte-code-vector-symbols '(t))
;;
(eval-when (compile load)
  (put 'mon-help-byte-code-vector-symbols 'function-documentation 
       (let (mbcvs-doc)
         (with-temp-buffer
           (save-excursion
             (let ((mbcvs-cnt 0))
               (insert "\n;; :BYTE-CODE-VECTOR-SYMBOLS\n")
               (mapc #'(lambda (mbcvs-L-0) 
                         (unless (or (null mbcvs-L-0) 
                                     (null (get mbcvs-L-0 'variable-documentation)))
                           (insert
                            (format "`%s'\n:indembcvs-L-0 %d\n:documentation\n%S\n\n"
                                    mbcvs-L-0 mbcvs-cnt 
                                    (mon-string-justify-left
                                     (documentation-property mbcvs-L-0 'variable-documentation)
                                     78 16))))
                         (incf mbcvs-cnt))
                     byte-code-vector)))
           ;; (while (search-forward-regexp "^:documentation\n                " nil t)
           (while (search-forward-regexp "^:documentation\n\"                " nil t)
             (replace-match ":documentation  \""))
           (setq mbcvs-doc (mon-buffer-sub-no-prop)))
         (concat 
          "List of non-nil elts in `byte-code-vector' with their index and docstrings.\n"
          mbcvs-doc
          ":SEE :FILE byte-opt.el\n\n"
          ":SEE-ALSO `byte-boolean-vars', `byte-stack+-info', `byte-optimize-log',\n"          
          "`mon-help-symbol-functions', `mon-help-byte-optimizer-find',\n"
          "`mon-map-obarray-symbol-plist-props', `mon-help-permanent-locals-find',\n"
          "`*mon-help-side-effect-free*', `*mon-help-side-effect-and-error-free*',\n"
          "`*mon-help-pure-functions*', `*mon-help-permanent-locals*', `*mon-help-subrs*',\n"
          "`*mon-help-byte-optimizer-vals*',  `gnus-byte-code'.\n►►►"))))

 
;;; ==============================
;;; :NOTE `¦' -> BROKEN BAR (ucs-insert #xa6)
;;; :CREATED <Timestamp: #{2009-12-11T11:44:07-05:00Z}#{09505} - by MON KEY>
;;;###autoload
(defun mon-help-ipv4-header (&optional insertp intrp)
  "The IPv4 header as per RFC-791 \(more or less\).
:SEE (URL `http://tools.ietf.org/rfc/rfc791.txt')\n
:BYTE-OFFSET                                                                 80.
`--> |0      ¦       ¦    1          ¦        2      ¦            3  |          
     |-------¦-------¦---------------¦---------------¦---------------|========  
  00 |VERSION|  :IHL |  :TOS         |         :TOTAL-LENGTH         |  ¦    ¦  
     |-------¦---------------------------------------¦---------------|  20   ¦    
  04 |       |  :IDENTIFICATION      |:FLAG|   :FRAGMENT-OFFSET      | Bytes ¦  
     |-------¦-------¦---------------¦---------------¦---------------|  ¦    ¦  
  08 | :TIME-TO-LIVE |  :PROTOCOL    |         :HEADR-CHECKSUM       |  ¦ IHL¦  
     |-------¦-------¦---------------¦---------------¦---------------|  ¦ Intrnt
  12 |       ¦       |  :SOURCE-ROUTE-LOCATOR        |               |  ¦ Header
     |-------¦-------¦---------------¦---------------¦---------------|  ¦ Length
  16 |       ¦       |  :DESTINATION-ROUTING-LOCATOR |               |  ¦    ¦  
     |-------¦-------¦---------------¦---------------¦---------------|====   ¦  
  20 |       ¦       |  :OPTIONS     |               |   :PADDING    |       ¦  
     |-------¦-------¦---------------¦---------------¦---------------|========  
:BIT |0 1 2 3¦4 5 6 7¦8 9 0 1 2 3 4 5¦6 7 8 9 0 1 2 3¦4 5 6 7 8 9 0 1|          
     |  Nib  ¦ Byte  ¦                     Word                      |          
                                                                             80^
:VERSION                      -> 4-bit. Version field.\n
:IHL \(Internet Header Length\) -> 4-bit. Number of 32-bit words in header.\n
:DS  \(Differentiated Service\) -> :SEE RFC-2474 & RFC-3168\n
:TOS \(Type of Service\)        -> 8-bit.\n
           0     1     2     3     4     5     6     7
        +-----+-----+-----+-----+-----+-----+-----+-----+
        |                 |     |     |     |     |     |
        |   :PRECEDENCE   |  D  |  T  |  R  |  0  |  0  |
        |                 |     |     |     |     |     |
        +-----+-----+-----+-----+-----+-----+-----+-----+\n
        o bits_0–2 -> :TOS-PRECEDENCE
                      111 -> Network Control 
                      110 -> Internetwork Control
                      101 -> CRITIC/ECP
                      100 -> Flash Override
                      011 -> Flash
                      010 -> Immediate
                      001 -> Priority
                      000 -> Routine\n
        o bit-3         0 -> Normal Delay
                        1 -> Low Delay\n
        o bit-4         0 -> Normal Throughput
                        1 -> High Throughput\n
        o bit-5         0 -> Normal Reliability 
                        1 -> High Reliability\n
        o bit-6         0 -> Normal Cost 
                        1 -> Minimize Monetary Cost :SEE RFC-1349\n
        o bit-7           -> undefined\n
:TOTAL-LENGTH   -> 16-bit. Define datagram size.\n
:IDENTIFICATION -> Identify fragments of original IP datagram.\n
:FLAG           -> 3-bit. Control or identify fragments.\n
                     0   1   2
                   +---+---+---+
                   |   | D | M |
                   | 0 | F | F |
                   +---+---+---+\n
                   :FLAG-ORDER-HIGH->LOW\n
                   bit-0 -> Reserved, must be zero.\n
                   bit-1 -> :DF-DO-NOT-FRAGMENT 
                             0 -> May Fragment
                             1 -> Don't Fragment\n
                   bit-2 -> :MF-MORE-FRAGMENTS
                             0 -> Last Fragment
                             1 -> More Fragments\n
:FRAGMENT-OFFSET    -> 13-bit in 8-byte blocks. 
                       Fragment offset rel. orig. unfragmented IP datagram.\n
:TTL (Time to Live) -> 8-bit. Limit datagram lifetime.\n
:PROTOCOL           -> Define protocol of IP datagram's data.\n
:HEADER-CHECKSUM    -> 16-bit. Checksum.\n
:SOURCE-ROUTING-LOCATOR -> 32-bit as 4-octet group. IPv4 address packet source.\n
:DESTINATION-ROUTING-LOCATOR -> As above, IPv4 packet reciever.\n
:SEE (URL `http://tools.ietf.org/rfc/rfc1349.txt')
:SEE (URL `http://tools.ietf.org/html/rfc2474.txt')
:SEE (URL `http://tools.ietf.org/rfc/rfc3168.txt')
:SEE (URL `http://en.wikipedia.org/wiki/IPv4')\n
:SEE-ALSO `*mon-iptables-alst*'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-ipv4-header :insertp t)
    (mon-help-message-intrp "mon-help-ipv4-header")))
;;
;;; :TEST-ME (mon-help-ipv4-header )
;;; :TEST-ME (mon-help-ipv4-header t)
;;; :TEST-ME (describe-function 'mon-help-ipv4-header)
;;; :TEST-ME (apply 'mon-help-ipv4-header '(t))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-31T17:51:05-04:00Z}#{10133} - by MON KEY>
;;;(defun mon-help-url-functions-usage ((&optional insertp intrp)
;;;(url-copy-file "http://www.w3.org/TR/CSS21/propidx.html" 
;;;                (concat temporary-file-directory"./test"))
;;;(url-insert-file-contents "http://www.w3.org/TR/CSS21/propidx.html")
;;; ==============================

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-27T06:39:21-04:00Z}#{09397} - by MON>
;;;###autoload
(defun mon-help-nclose-functions (&optional insertp intrp)
  "Functions for working with nclosemacs.
:SEE info node `(nclosemacs)Top'\n►►►\n
;; :NCLOSE-KNOWCESSING
`*nclose-suggest*'
`*nclose-volunteer*'
`*nclose-knowcess*'
`*nclose-reset-session*'
`*nclose-reset-globales*'
`*nclose-reset-signs*'\n
;; :NCLOSE-PRINT-OBJECTS-ENCYCLOPAEDIA
`*nclose-print-wm*'
`*nclose-print-hypos*'
`*nclose-print-object*'
`*nclose-print-instances*'\n
;; :NCLOSE-ADD-TO-KB-MACRO
 (`add-to-kb'
   (`@LHS'= <PATTERN-LISP-FORM>)
   (`@hypo' <HYPOTHESIS>)
  [(`@RHS'= [*RHS-lisp-form*]+) *string-documentation*])               \n
<PATTERN-LISP-FORM> - (form which yields a boolean)
  ,---------.
  |`all-in' |
  |`some-in'|>----<'CLASS-NAME INTEGER 'AND-OR-ELISP-FORM>
  |`oone-in'|                           |  ,-------------------------.
  |`none-in'|                           |  | string= string< string> |
  `---------'                           `--| eq eql equal            |
                                           | < > = /= >= <= + - * /  |
                                           | and or not null '()     |
                                           | memq memql member       |
                                           | yes no                  |
                                           `------------------------70
<HYPOTHESIS> - an hypothesis for the rule.\n
<RHS-LISP-FORM> - right-hand side actions+
  ,----
  | `@SET'
  |  |--+ `prop-in'
  |  |    (OBJECT-NAME PROPERTY-NAME [SCALAR-VALUE|LISP-FORM])
  |  |
  |  |--+ `member-in'
  |       (CLASS-NAME INTEGER PROPERTY-NAME [SCALAR-VALUE|LISP-FORM])
  `----\n
<STRING-DOCUMENTATION>\n"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-nclose-functions :insertp t)
    (mon-help-message-intrp "mon-help-nclose-functions")))
;;
;;; :TEST-ME (mon-help-nclose-functions)
;;; :TEST-ME (mon-help-nclose-functions t)
;;; :TEST-ME (describe-function 'mon-help-nclose-functions)

 
;;; ==============================
;;; :CREATED <Timestamp: Wednesday July 29, 2009 @ 11:57.11 AM - by MON KEY>
;;;###autoload
(defun mon-help-iso-8601 (&optional insertp intrp)
  "The full, extended format of ISO 8601 is as follows:\n
    1999-10-11T11:10:30,5-07:00\n
;; :ISO-8601-ELEMENTS\n
   1. the year with four digits
   2. a hyphen \(omitted in the basic format\)
   3. the month with two digits
   4. a hyphen \(omitted in the basic format\)
   5. the day of month with two digits
   6. the letter T to separate date and time
   7. the hour in the 24-hour system with two digits
   8. a colon \(omitted in the basic format\)
   9. the minute with two digits
  10. a colon \(omitted in the basic format\)
  11. the second with two digits
  12. a comma
  13. the fraction of the second with unlimited precision
  14. a plus sign or hyphen \(minus\) to indicate sign of time zone
  15. the hours of the time zone with two digits
  16. a colon \(omitted in the basic format\)
  17. the minutes of the time zone with two digits\n
:EXAMPLE\n
------------------------------
      2  4  6  8 10 12 14 16
      |  |  |  |  |  | |  |
      |  |  |  |  |  | |  |
  1999-10-11T11:10:30,5-07:00
   |    |  |  |  |  | |  |  |
   |    |  |  |  |  | |  |  |
   1    3  5  7  9 1113  15 17
------------------------------\n
;; :ISO-8601-OMISSION-RULES
The rules for omission of elements are quite simple. Elements from the time of
day may be omitted from the right and take their immediately preceding delimiter
with them. Elements from the date may be omitted from the left, but leave the
immediately following delimiter behind. When the year is omitted, it is replaced
by a hyphen. Elements of the date may also be omitted from the left, provided no
other elements follow, in which case they take their immediately preceding
delimiter with them. The letter T is omitted if the whole of the time of day or
the whole of the date are omitted. If an element is omitted from the left, it is
assumed to be the current value. \(In other words, omitting the century is really
dangerous, so I have even omitted the possibility of doing so.\) If an element is
omitted from the right, it is assumed to cover the whole range of values and
thus be indeterminate.\n
Every element in the time specification needs to be within the normal
bounds. There is no special consideration for leap seconds, although some might
want to express them using this standard.\n
A duration of time has a separate notation entirely, as follows:\n
    P1Y2M3DT4H5M6S>
    P7W\n
;; :ISO-8601-DURATION-ELEMENTS\n
   1. the letter P to indicate a duration
   2. the number of years
   3. the letter Y to indicate years
   4. the number of months
   5. the letter M to indicate months
   6. the number of days
   7. the letter D to indicate days
   8. the letter T to separate dates from times
   9. the number of hours
  10. the letter H to indicate hours
  11. the number of minutes
  12. the letter M to indicate minutes
  13. the number of seconds
  14. the letter S to indicate seconds\n
Or, for the second form, usually used alone\n
   1. the letter P to indicate a duration
   2. the number of weeks
   3. the letter W to indicate weeks\n
Any element \(number\) may be omitted from this specification and if so takes its
following delimited with it. Unlike the absolute time format, there is no
requirement on the number of digits, and thus no requirement for leading zeros.\n
A period of time is indicated by two time specifications, at least one of which
has to be absolute, separated by a single solidus \(slash\), and has the general
forms as follows:\n
    start/end
    start/duration
    duration/end\n
the end form may have elements of the date omitted from the left with the
assumption that the default is the corresponding value of the element from the
start form. Omissions in the start form follow the normal rules.\n
The standard also has specifications for weeks of the year and days of the week,
but these are used so rarely and are aesthetically displeasing so are gracefully
elided from the presentation.\n
:SOURCE Erik Naggum's \"The Long, Painful History of Time\
:ALIASED-BY `mon-help-time-iso-8601'
:SEE \(URL `http://naggum.no/lugm-time.html'\)
:SEE info node `(coreutils)Date input formats'\n
:SEE (URL `http://tools.ietf.org/rfc/rfc3339.txt')\n
:SEE-ALSO `mon-format-iso-8601-time', `mon-help-CL-local-time', 
`mon-help-time-functions', `mon-help-mon-time-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-iso-8601 :insertp t)
    (mon-help-message-intrp "mon-help-iso-8601")))
;; 
;;; :TEST-ME (mon-help-iso-8601)
;;; :TEST-ME (mon-help-iso-8601 t)
;;; :TEST-ME (describe-function 'mon-help-iso-8601)
;;; :TEST-ME (apply 'mon-help-iso-8601 '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-12T11:54:35-04:00Z}#{10236} - by MON KEY>
;;;###autoload
(defun mon-help-time-functions (&optional insertp intrp)
  "List of functions and variables related to time calculation conversion.\n
;; :TIME-FUNCTIONS
`current-time'
`current-time-zone'
`decode-time'
`encode-time'
`format-time-string'
`format-seconds'
`float-time'
`seconds-to-time'
`date-to-day'
`date-leap-year-p'
`date-to-time'
`safe-date-to-time'
`days-to-time'
`days-between'
`time-less-p'
`time-since'
`time-subtract'
`time-add'
`time-to-day-in-year'
`time-to-days'
`url-get-normalized-date'
`encode-time-value'
`with-decoded-time-value'\n
;; :TIME-FUNCTIONS-ISO
`diary-iso-date-forms'
`calendar-iso-to-absolute'
`calendar-iso-from-absolute'
`calendar-iso-date-string'
`calendar-iso-print-date'
`calendar-iso-read-date'
`diary-iso-date'\n
;; :TIME-FUNCTIONS-ERC
\n
;; :TIME-FUNCTIONS-ORG
`org-time-stamp-formats'
`org-display-custom-times'
`org-time-stamp-custom-formats'
`org-time-stamp-format'
`org-plain-time-of-day-regexp'
`org-stamp-time-of-day-regexp'
`org-plain-time-extension-regexp'\n
;; :TIMEZONE-FUNCTIONS
`timezone-time-from-absolute'
`timezone-time-zone-from-absolute'
`timezone-fix-time'
`timezone-last-day-of-month'
`timezone-leap-year-p'
`timezone-day-number'
`timezone-absolute-from-gregorian'
`timezone-parse-date'
`timezone-parse-time'
`timezone-zone-to-minute'
`timezone-make-date-arpa-standard';
`timezone-make-date-sortable'
`timezone-make-arpa-date'
`timezone-make-sortable-date'
`timezone-make-time-string'\n
;; :TIMEZONE-VARIABLES
`timezone-world-timezones'
`timezone-months-assoc'\n
;; :DATE-VARIABLES
`diary-european-date-forms'
`url-monthabbrev-alist'
`url-weekday-alist'
`parse-time-months'
`parse-time-weekdays'\n
:SEE info node `(coreutils)Date input formats'
:SEE :FILE calendar/parse-time.el calendar/time-date.el
:SEE-ALSO `mon-help-CL-local-time', `mon-help-number-functions',
`mon-help-mon-time-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-time-functions :insertp t)
    (mon-help-message-intrp "mon-help-time-functions")))
;;
;;; :TEST-ME (mon-help-time-functions)
;;; :TEST-ME (mon-help-time-functions t)
;;; :TEST-ME (describe-function 'mon-help-time-functions)
;;; :TEST-ME (apply 'mon-help-time-functions '(t))

 
;;; ==============================
;;; :CHANGESET 1850
;;; :CREATED <Timestamp: #{2010-06-12T12:10:09-04:00Z}#{10236} - by MON KEY>
;;;###autoload
(defun mon-help-mon-time-functions (&optional insertp intrp)
  "List `mon-*' fncns and variables to convert, match, and insert time values.\n
;; :MON-TIME-FUNCTIONS
`mon-get-current-year'
`mon-format-iso-8601-time'
`mon-conv-time-flt-pnt'
`mon-comp-times-flt-pnt'\n
;; MON-TIME-FUNCTION-FILE
`mon-file-older-than-file-p'
`mon-get-file-mod-times'\n
;; MON-TIME-FUNCTION-STAMP
`mon-accessed-stamp',
`mon-accessed-time-stamp'
`mon-date-stamp'
`mon-file-stamp'
`mon-file-stamp-buffer-filename' 
`mon-file-stamp-minibuffer'
`mon-lisp-stamp' 
`mon-stamp'
`mon-stamp-in-context'\n
;; MON-TIME-FUNCTION-REPLACE
`mon-num-to-month'
`mon-abr-to-month'
`mon-month-to-num'
`mon-num-to-month-whitespace'\n
;; MON-TIME-FUNCTION-EBAY
`mon-cln-ebay-time-string'
`mon-calculate-ebay-timezone-diff'
`mon-convert-ebay-time'
`mon-convert-ebay-time-mvb'\n
;; MON-TIME-FUNCTION-INTNL
`mon-ital-date-to-eng'
`mon-defranc-dates'\n
;; :MON-TIME-REGEXPS
`*regexp-MM->month*'
`*regexp-month->MM*'
`*regexp-month->canonical-ws*' 
`*regexp-bound-month->canonical*'
`*regexp-abrv-dotted-month->canonical*'
`*regexp-simple-abrv-month->canonical*'
`*regexp-MM->month-whitespace-aware*'
`*regexp-simple-abrv-month->canonical*'\n
;; :MON-TIME-REGEXP-CLEAN-PHILSP
`*regexp-philsp-apos*'
`*regexp-philsp-months*'
`*regexp-philsp-fix-month-dates*'\n
;; :MON-TIME-REGEXP-CLEAN-EBAY
`*regexp-clean-ebay-time-chars*'
`*regexp-clean-ebay-month->canonical-style1*'
`*regexp-clean-ebay-month->canonical-style2*'
`*regexp-clean-ebay-month->canonical-style3*'\n
;; :MON-TIME-REGEXP-INTNL
`*regexp-ital-to-eng*'
`*regexp-defranc-dates*'\n
;; :MON-TIME-VARIABLES
`*mon-timestamp-cond*'\n
;; :NAF-MODE-DATE-REGEXPS
`*naf-mode-db-flags-xrefs*'
`*naf-mode-date-xrefs*'
`*regexp-french-date-prefix*'
`*regexp-french-date-siecle*'
`naf-active-date-flags'
`naf-active-date-flags-paren'
`naf-active-date-flags-solo'
`naf-mode-accessed-by-flag'
`naf-mode-active-date'
`naf-mode-active-date-flags-paren'
`naf-mode-active-date-flags-solo'
`naf-mode-benezit-date'
`naf-mode-circa-dates'
`naf-mode-date-string'
`naf-mode-english-dates'
`naf-mode-english-days'
`naf-mode-french-dates'
`naf-mode-french-days'
`naf-mode-french-months'
`naf-mode-lifespan'
`naf-mode-simple-date'
`naf-mode-timestamp-flag'
`naf-mode-year-range'
`naf-month-abbrev-alist'
`naf-weekday-alist'\n
:SEE :FILE mon-time-utils.el mon-regexp-symbols.el naf-mode-dates.el naf-mode-db-flags.el\n
:SEE-ALSO `mon-help-time-functions', `mon-help-iso-8601', `mon-help-CL-local-time'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-mon-time-functions :insertp t)
    (mon-help-message-intrp "mon-help-mon-time-functions")))
;;
;;; :TEST-ME (mon-help-mon-time-functions)
;;; :TEST-ME (mon-help-mon-time-functions t)
;;; :TEST-ME (describe-function 'mon-help-mon-time-functions)
;;; :TEST-ME (apply 'mon-help-mon-time-functions '(t))

 
;;; ==============================
;;; :CHANGESET 2067
;;; :CREATED <Timestamp: #{2010-08-16T17:22:53-04:00Z}#{10331} - by MON KEY>
;;;###autoload
(defun mon-help-bookmark-functions (&optional insertp intrp)
  "List of the bookmark package's functions and variables.\n
;; :BOOKMARK-FUNCTIONS
`bookmark--jump-via'
`bookmark-alist-from-buffer'
`bookmark-all-names'
`bookmark-buffer-file-name'
`bookmark-buffer-name'
`bookmark-completing-read'
`bookmark-default-annotation-text'
`bookmark-default-handler'
`bookmark-delete'
`bookmark-edit-annotation'
`bookmark-edit-annotation-mode'
`bookmark-exit-hook-internal'
`bookmark-get-annotation'
`bookmark-get-bookmark'
`bookmark-get-bookmark-record'
`bookmark-get-filename'
`bookmark-get-front-context-string'
`bookmark-get-handler'
`bookmark-get-position'
`bookmark-get-rear-context-string'
`bookmark-grok-file-format-version'
`bookmark-handle-bookmark'
`bookmark-import-new-list'
`bookmark-insert'
`bookmark-insert-current-bookmark'
`bookmark-insert-file-format-version-stamp'
`bookmark-insert-location'
`bookmark-jump'
`bookmark-jump-noselect'
`bookmark-jump-other-window'
`bookmark-kill-line'
`bookmark-load'
`bookmark-locate'
`bookmark-location'
`bookmark-make-record'
`bookmark-make-record-default'
`bookmark-maybe-historicize-string'
`bookmark-maybe-load-default-file'
`bookmark-maybe-message'
`bookmark-maybe-rename'
`bookmark-maybe-sort-alist'
`bookmark-maybe-upgrade-file-format'
`bookmark-menu-popup-paned-menu'
`bookmark-name-from-full-record'
`bookmark-prop-get'
`bookmark-prop-set'
`bookmark-read-search-input'
`bookmark-relocate'
`bookmark-rename'
`bookmark-save'
`bookmark-send-edited-annotation'
`bookmark-set'
`bookmark-set-annotation'
`bookmark-set-filename'
`bookmark-set-front-context-string'
`bookmark-set-name'
`bookmark-set-position'
`bookmark-set-rear-context-string'
`bookmark-show-all-annotations'
`bookmark-show-annotation'
`bookmark-store'
`bookmark-time-to-save-p'
`bookmark-unload-function'
`bookmark-upgrade-file-format-from-0'
`bookmark-upgrade-version-0-alist'
`bookmark-write'
`bookmark-write-file'
`bookmark-yank-word'\n
;; :BOOKMARK-VARIABLES
`bookmark-map'
`bookmark-use-annotations'
`bookmark-save-flag'
`bookmark-default-file'
`bookmark-old-default-file'
`bookmark-file'
`bookmark-version-control'
`bookmark-completion-ignore-case'
`bookmark-sort-flag'
`bookmark-automatically-show-annotations'
`bookmark-bmenu-header-height'
`bookmark-bmenu-marks-width'
`bookmark-bmenu-file-column'
`bookmark-bmenu-toggle-filenames'
`bookmark-menu-length'
`bookmark-search-delay'
`bookmark-menu-heading'\n
;; :BOOKMARK-BMENU
`bookmark-bmenu-1-window'
`bookmark-bmenu-2-window'
`bookmark-bmenu-any-marks'
`bookmark-bmenu-backup-unmark'
`bookmark-bmenu-bookmark'
`bookmark-bmenu-delete'
`bookmark-bmenu-delete-backwards'
`bookmark-bmenu-edit-annotation'
`bookmark-bmenu-ensure-position'
`bookmark-bmenu-execute-deletions'
`bookmark-bmenu-filter-alist-by-regexp'
`bookmark-bmenu-goto-bookmark'
`bookmark-bmenu-hide-filenames'
`bookmark-bmenu-list'
`bookmark-bmenu-load'
`bookmark-bmenu-locate'
`bookmark-bmenu-mark'
`bookmark-bmenu-mode'
`bookmark-bmenu-other-window'
`bookmark-bmenu-other-window-with-mouse'
`bookmark-bmenu-relocate'
`bookmark-bmenu-rename'
`bookmark-bmenu-save'
`bookmark-bmenu-search'
`bookmark-bmenu-select'
`bookmark-bmenu-show-all-annotations'
`bookmark-bmenu-show-annotation'
`bookmark-bmenu-show-filenames'
`bookmark-bmenu-surreptitiously-rebuild-list'
`bookmark-bmenu-switch-other-window'
`bookmark-bmenu-this-window'
`bookmark-bmenu-toggle-filenames'
`bookmark-bmenu-unmark'\n
:SEE :FILE bookmark.el
:SEE-ALSO .\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-bookmark-functions :insertp t)
    (mon-help-message-intrp "mon-help-bookmark-functions")))
;;
;;; :TEST-ME (mon-help-bookmark-functions)
;;; :TEST-ME (mon-help-bookmark-functions t)
;;; :TEST-ME (apply 'mon-help-bookmark-functions '(t))

 
;;; ==============================
;;; <Timestamp: Saturday May 23, 2009 @ 04:03.59 PM - by MON KEY>
;;;###autoload
(defun mon-help-info-incantation (&optional insertp intrp)
  "To reference an info node in a docstring use the idiom:\n
\"info node `\(elisp\)Documentation Tips\'\" <- Without the \"_\" dbl-quotes.\n
To jump to an info node with an elisp expression:
\(info \"\(elisp\)Documentation Tips\"\) <- With the \" \" dbl-quotes!.
:SEE-ALSO `mon-help-tar-incantation', `mon-help-rename-incantation',
`mon-help-unix-commands', `mon-help-crontab', `mon-help-permissions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-info-incantation :insertp t)
    (mon-help-message-intrp "mon-help-info-incantation")))
;;
;;; :TEST-ME (mon-help-info-incantation t)
;;; :TEST-ME (mon-help-info-incantation)
;;; :TEST-ME (describe-function 'mon-help-info-incantation)
;;; :TEST-ME (apply 'mon-help-info-incantation '(t))

 
;;; ==============================
;;; :CHANGESET 1737 <Timestamp: #{2010-05-17T07:48:29-04:00Z}#{10201} - by MON KEY>
;;;###autoload
(defun mon-help-tar-incantation (&optional insertp intrp)
  "To help me remember how to do a tar.gz on a directory.
Because I never can remember tar's flags.
:NOTE\nOn w32 with gnuwin32 to unzip use `gzip.exe -d'
On w32 with gnuwin32 to pipe a tar to gz on w32 use `bsdtar.exe xvzf'.\n

:TAR-INCANTATION

 tar -czvf dir-name.tar.gz dir-name

Make an exclude file list, and get tar to use it:

 find ./<SOME-PATH>/<THING(S)-TO-FIND> > what-we-found
 tar -cvf <SOME-PATH-TARRED>.tar ./<SOME-PATH> -X what-we-found

Move the entire directory as an archive do:

 (cd <PATH-FROM> && tar Scpf <PATH-TO><TAR-FILE-NAME>.tar .)

Or, if you prefer, following is equivalent to above:
 tar -C <from-mount-point> -Scpvf <to-mount-point><TAR-FILE-NAME>.tar .

:SEE-ALSO `mon-help-rename-incantation', `mon-help-info-incantation',
`mon-help-du-incantation', `mon-help-unix-commands', `mon-help-permissions',
`mon-help-crontab'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-tar-incantation :insertp t)
    (mon-help-message-intrp "mon-help-tar-incantation")))
;;
;;; :TEST-ME (mon-help-tar-incantation t)
;;; :TEST-ME (mon-help-tar-incantation)
;;; :TEST-ME (describe-function 'mon-help-tar-incantation)
;;; :TEST-ME (apply 'mon-help-tar-incantation '(t))

;;; ==============================
;;;###autoload
(defun mon-help-rename-incantation (&optional insertp intrp)
  "Insert the rename idiom for BASH renaming.
# :RENAME-IDIOM
# for f in *FILENAME; do
#  base=`basename $f *FILENAME` #<-- note backtick!
# mv $f $base.NEWNAME
# done

# :EXAMPLE
for f in *.html.tmp; do
 base=`basename $f .html.tmp`
 mv $f $base.html
done 

:SEE-ALSO `mon-help-info-incantation',`mon-help-du-incantation',
`mon-help-unix-commands', `mon-help-permissions', `mon-help-crontab'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-rename-incantation :insertp t)
    (mon-help-message-intrp "mon-help-rename-incantation")))
;;
;;; :TEST-ME (mon-help-rename-incantation t)
;;; :TEST-ME (mon-help-rename-incantation)
;;; :TEST-ME (describe-function 'mon-help-rename-incantation)
;;; :TEST-ME (apply 'mon-help-rename-incantation '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-30T16:39:56-05:00Z}#{09491} - by MON KEY>
(defcustom *regexp-clean-du-flags* nil
  "An alist of short and long flags for the `du' command.\n
:SEE info node `(coreutils)du invocation'\n
:SEE (man \"du\")\n
:SEE-ALSO `mon-help-du-incantation', `mon-async-du-dir', `get-free-disk-space',
`directory-free-space-program', `directory-free-space-args'.\n►►►"
  :type  '(alist :key-type string 
                 :value-type (choice string (const :tag "nil" nil)))
  :group 'mon-doc-help-utils)
;;
;;
;;(eval-when (compile load)
(unless (and (intern-soft "*regexp-clean-du-flags*" obarray)
             (bound-and-true-p *regexp-clean-du-flags*))
  (setq *regexp-clean-du-flags*
        '(("-a" . "--all")
          ("--apparent-size")
          ("-b" . "--bytes")
          ("-B" . "--block-size")       ;SIZE & =SIZE
          ("-c" . "--total")
          ("-D" . "--dereference-args")
          ("--files-from")             ;=FILE
          ("-h" . "--human-readable")
          ("-H")
          ("-k")
          ("-l" . "--count-links")
          ("-L" . "--dereference")
          ("-P" . "--no-dereference")
          ("--max-depth")               ;=DEPTH
          ;; ("-0" . "--null")
          ("--si")
          ("-s" . "--summarize")
          ("-S" . "--separate-dirs")
          ("-x" . "--one-file-system")
          ("--exclude")                 ;=PATTERN
          ("-X" . "--exclude-from"))    ;FILE & =FILE)
        )
  (custom-note-var-changed '*regexp-clean-du-flags*))
;;
;;; :TEST-ME *regexp-clean-du-flags*
;;
;;; (progn (makunbound '*regexp-clean-du-flags*)(unintern "*regexp-clean-du-flags*" obarray))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-30T16:31:20-05:00Z}#{09491} - by MON KEY>
(defun mon-help-du-incantation (&optional insertp intrp)
  ""
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-du-incantation :insertp t)
    (mon-help-message-intrp "mon-help-du-incantation")))
;;
;; Now, tack the var list onto the docstring.
;; (eval-when-compile;; (compile load)
(let (mhdi-put-flags)
  (setq mhdi-put-flags "")
  (mapc #'(lambda (mhdi-L-0) 
            (setq mhdi-put-flags 
                  (concat 
                   (format "%s %s\n" 
                           (cond ((not (null (cdr mhdi-L-0))) (concat "`" (car mhdi-L-0) "'"))
                                 ((and (null (cdr mhdi-L-0)) 
                                       (not (eq (aref (car mhdi-L-0) 1) 45))) ;; Is it a `-'
                                  (concat "`" (car mhdi-L-0) "'"))
                                 ((and (null (cdr mhdi-L-0)) (eq (aref (car mhdi-L-0) 1) 45)) ;; Its a `-'
                                  (concat "     `" (car mhdi-L-0) "'"))
                                 (t (concat "`" (car mhdi-L-0) "'")))
                           
                           (or (and (cdr mhdi-L-0)(concat "`" (cdr mhdi-L-0) "'"))
                               ""))
                   mhdi-put-flags)))
        (reverse (symbol-value '*regexp-clean-du-flags*)))
  (mon-help-put-var-doc-val->func  'mhdi-put-flags  'mon-help-du-incantation
    ;; PRE-V-STR 
    (concat  "Short and long flags to the du command.\n\n"
             ";; :DU-FLAGS\n")
    ;; CUT-V-STR
    nil 
    ;; PST-V-STR
    (concat "\n\n:EXAMPLE\n shell> du -s --si <DIR>\n\n"
            ":SEE info node `(coreutils)du invocation'\n"
            ":SEE (man \"du\")\n"
            ":SEE-ALSO `*regexp-clean-du-flags*', `get-free-disk-space'\n"
            "`directory-free-space-program', `directory-free-space-args'.\n►►►")))
;;
;;; :TEST-ME (mon-help-du-incantation)
;;; :TEST-ME (mon-help-du-incantation t)
;;; :TEST-ME (describe-function 'mon-help-du-incantation)
;;; :TEST-ME (apply 'mon-help-du-incantation '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 04:35.09 PM - by MON KEY>
;;;###autoload
(defun mon-help-install-info-incantation (&optional insertp intrp)
  "Insert the install-info incantation.\n
Esp. needed on w32 as I can never remember it.\n
M-x cygwin-shell\install-info  info-file  /\\\"Program Files\\\"/Emacs/emacs/info/dir
M-x shell\install-info  info-file  \"/usr/info/dir\"\n
:SEE-ALSO `mon-help-rename-incantation', `mon-help-info-incantation',
`mon-help-du-incantation', `mon-help-unix-commands', `mon-help-permissions',
`mon-help-crontab'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (let ((info-incantation
             (cond
              ;; (IS-MON-P-W32
              ;;  "M-x cygwin-shell install-info info-file /\"Program Files\"/Emacs/emacs/info/dir")
              ;; (IS-MON-P-GNU
              ;;  "M-x shell\install-info  info-file  \"/usr/info/dir\"")
              ((eq system-type 'windows-nt)
               "\nM-x cygwin-shell install-info info-file /\"Program Files\"/Emacs/emacs/info/dir")
              ((or (eq system-type 'gnu/linux)  (eq system-type 'linux))
               "\nM-x shell\install-info  info-file  \"/usr/info/dir\"")
              (t "\nM-x shell\install-info  info-file  \"/path/to/info/dir\""))))
        (princ info-incantation (current-buffer)))
    (mon-help-message-intrp "mon-help-install-info-incantation")
    ))
;;
;;; :TEST-ME (mon-help-install-info-incantation t)
;;; :TEST-ME (mon-help-install-info-incantation t)
;;; :TEST-ME (describe-function 'mon-help-tar-incantation)
;;; :TEST-ME (apply 'mon-help-tar-incantation '(t))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-04T16:26:50-05:00Z}#{09453} - by MON KEY>
;;;###autoload
(defun mon-help-hg-archive (insertp intrp)
  "Idiom for creating an HG archive of a specific revision.
Lifted straight from the `hg' man page.\n
hg archive [OPTION]... DEST\n
By default, the revision used is the parent of the working directory.
Use -r/--rev to specify a different revision.\n
To specify the type of archive to create, use -t/--type.\n
;; :VALID-TYPES\n
\"files\" \(default\): a directory full of files
\"tar\": tar archive, uncompressed
\"tbz2\": tar archive, compressed using bzip2
\"tgz\": tar archive, compressed using gzip
\"uzip\": zip archive, uncompressed
\"zip\": zip archive, compressed using deflate\n
The exact name of the destination archive or directory is given using a format
string; :SEE 'hg help export' for details.\n
Each member added to an archive file has a directory prefix prepended.
Use -p/--prefix to specify a format string for the prefix.
The default is the basename of the archive, with suffixes removed.\n
;; :HG-ARCHIVE-OPTIONS
--no-decode 	do not pass files through decoders
-p, --prefix 	directory prefix for files in archive
-r, --rev 	revision to distribute
-t, --type 	type of distribution to create
-I, --include 	include names matching the given patterns
-X, --exclude 	exclude names matching the given patterns\n
:SEE (man \"hg\")
:SEE (URL `http://mercurial.selenic.com/quickstart/')
:SEE (URL `http://mercurial.selenic.com/guide/')
:SEE (URL `http://hgbook.red-bean.com/read/')\n
:SEE-ALSO `mon-help-rename-incantation', `mon-help-info-incantation',
`mon-help-du-incantation', `mon-help-unix-commands', `mon-help-permissions',
`mon-help-crontab'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-hg-archive :insertp t)
    (mon-help-message-intrp "mon-help-hg-archive")))
;;
;;; :TEST-ME (mon-help-hg-archive)
;;; :TEST-ME (mon-help-hg-archive t)
;;; :TEST-ME (describe-function 'mon-help-hg-archive)
;;; :TEST-ME (apply 'mon-help-hg-archive '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-07T19:33:41-05:00Z}#{09457} - by MON KEY>
;;;###autoload
(defun mon-help-crontab (&optional insertp intrp)
  "Help for the crontab tool.\n
;; :CRONTAB-ELEMENTS
MINUTE HOUR DOM MONTH DOW USER CMD\n
minute  This controls what minute of the hour the command will run on,
        and is between '0' and '59'\n
hour    This controls what hour the command will run on, and is specified in
        the 24 hour clock, values must be between 0 and 23 (0 is midnight)\n
dom     This is the Day of Month, that you want the command run on, e.g. to
        run a command on the 19th of each month, the dom would be 19.\n
month   This is the month a specified command will run on, it may be specified
        numerically (0-12), or as the name of the month (e.g. May)\n
dow     This is the Day of Week that you want a command to be run on, it can
        also be numeric (0-7) or as the name of the day (e.g. sun).\n
user    This is the user who runs the command.
        :NOTE User is not needed when editing your own user crontab.\n
cmd     This is the command that you want run. This field may contain
        multiple words or spaces.\n
 __________________________
|                          |
|  :FIELD         :VALUES  |
|   -----          ------  |
|  minute          0-59    |
|  hour            0-23    |
|  day of month    0-31    |
|  month           0-12    |
|  day of week     0-7     |
|__________________________|\n
 _________________________________________________________________
|                                                                 |
|  *     *     *     *     *    :COMMAND-TO-BE-EXECUTED           |
|  |     |     |     |     |                                      |
|  |     |     |     |     `----- day of week (0 - 6) (Sunday=0)  |
|  |     |     |     `------- month (1 - 12)                      |
|  |     |     `--------- day of month (1 - 31)                   |
|  |     `----------- hour (0 - 23)                               |
|  `------------- min (0 - 59)                                    |
|_________________________________________________________________|\n
 _____________________________________________________________________________
|                                                                             |
| :MIN | :HR | :DAY | :DOM | :DOW | :EXECUTION-TIME                           |
| 30      0     1    1,6,12   *   -> 00:30 Hrs on 1st of Jan, June & Dec.     |
| 0       20    *     10      1-5 -> 8.00 PM weekdays (Mon-Fri) in Oct.       |
| 0       0    1,15   *       *   -> Midnight on 1st & 15th of month.         |
| 5,10    0     10    *       1   -> 12.05,12.10 every Mon & 10th each month. |
|_____________________________________________________________________________|\n
 _____________________________________________________________________________
|                                                                             |
| $> crontab -e  ;Edit your crontab file, or create if doesn't already exist. |
| $> crontab -l  ;Display your crontab file.                                  |
| $> crontab -d  ;Remove crontab file :NOTE vcron, bcron, fcron use -r flag.  |
| $> crontab -v  ;Display the last time you edited your crontab file.         |
|_____________________________________________________________________________|\n

:EXAMPLE\n
 $> touch ~/test-cron\n
 $> crontab -e \n
 Add the following line to your crontab:\n
 * * * * * /bin/echo \"foobar\" >> ~/test-cron\n
 If the file grows steadily over the next few minutes crond is working.\n
 $> rm ~/test-cron\n
The default system crontab usually stashes files under /var/spool/cron/*
:NOTE It is _VERY_IMPORTANT_ to make sure your .bashrc has the following:\n
  export EDITOR=emacs\n
Some users mistakenly put `export EDITOR=vi'. Never do this. It causes headaches,
user-error, and may even make your CPU to stop working! (Hint just use `:q')\n
:SEE \(woman \"crond\"\) (woman \"crontab\")
:SEE (URL `http://www.gentoo.org/doc/en/cron-guide.xml')\n
:SEE (URL `http://wiki.archlinux.org/index.php/Cron')
:SEE-ALSO `mon-help-unix-commands', `mon-help-permissions',
`mon-help-tar-incantation', `mon-help-info-incantation'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-crontab :insertp t)
    (mon-help-message-intrp "mon-help-crontab")))
;;
;;; :TEST-ME (mon-help-crontab)
;;; :TEST-ME (mon-help-crontab t)
;;; :TEST-ME (describe-function 'mon-help-crontab)
;;; :TEST-ME (apply 'mon-help-crontab '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-16T14:03:44-05:00Z}#{09471} - by MON KEY>
;;;###autoload
(defun mon-help-permissions (&optional insertp intrp)
  "Emacs permission functions and tables for chown, chmod, chattr, umask, etc.\n
;; :EMACS-PERMISSIONS-FUNCTIONS
`file-attributes'
`file-modes-symbolic-to-number'
`file-modes-char-to-who'
`file-modes-char-to-right'
`file-modes-rights-to-number'
`mon-help-unix-commands'
`read-file-modes'
`set-file-modes'
`set-default-file-modes'\n
 _____________________  __________________________________________________77.
|                     ||                                                    | 
|     :UGOA           ||              :TRIPLET                              |
|_____________________||____________________________________________________|         
|                     ||                                                    |
| U - First  - Owner  || First        Second       Third                    |
| G - Second - Group  || r: readable  w: writable  x: executable            |
| O - Third  - Others ||                           s: executable + setuid   |
| A -        - All    ||                           S: setuid not executable |
|_____________________||____________________________________________________|
                                                                             
          _____________                      _________________________       
         |             |                    |                         |      
         | :OPERATORS  |                    | :TRIPLES-SYM->OCT->BIN  |      
 ________|_____________|_____________   ____|_________________________|_____ 
|                                    | |                                    |
|  ugoa  :WHO   usr grp other all    | |  --- --- --- : 000 : 000 000 000   |
|  +     :DO    Add permission       | |  --x --x --x : 111 : 001 001 001   |
|  -     :DO    Remove permission    | |  -w- -w- -w- : 222 : 010 010 010   |
|  =     :DO    Permission equal to  | |  -wx -wx -wx : 333 : 011 011 011   |
|  r     :SET   Read                 | |  r-- r-- r-- : 444 : 100 100 100   |
|  w     :SET   Write                | |  r-x r-x r-x : 555 : 101 101 101   |
|  x     :SET   Exectute             | |  rw- rw- r-w : 666 : 110 110 110   |
|  t     :SET   Sticky bit           | |  rwx rwx rwx : 777 : 111 111 111   |
|  s     :SET   UID or GID           | |                                    |
|____________________________________| |____________________________________|
                                                                              
             ___________________________________________________             
            |         |                           |             |            
            | :OCTAL  |        :SYMBOLIC          |   :BINARY   |            
            |_________|___________________________|_____________|            
            |                                                   |            
            |  0       --- no permission               0: 000   |            
            |  1       --x execute                     1: 001   |            
            |  2       -w- write                       2: 010   |            
            |  3       -wx write and execute           3: 011   |            
            |  4       r-- read                        4: 100   |            
            |  5       r-x read and execute            5: 101   |            
            |  6       rw- read and write              6: 110   |            
            |  7       rwx read, write and execute     7: 111   |            
            |___________________________________________________|            
                                                                             
                             _________________                               
                            |                 |                              
                            |  :USER-7-TABLE  |                              
 ___________________________|_________________|____________________________ 
|                                                                          | 
|  rwx rwx rwx :777                                                        | 
|  rwx rwx rw- :776  rwx rw- rw- :766                                      | 
|  rwx rwx r-x :775  rwx rw- r-x :765  rwx r-x r-x :755                    | 
|  rwx rwx r-- :774  rwx rwx r-- :764  rwx r-x r-- :754  rwx r-- r-- :744  | 
|  rwx rwx -wx :773  rwx rwx -wx :763  rwx r-x -wx :753  rwx r-- -wx :743  | 
|  rwx rwx -w- :772  rwx rwx -w- :762  rwx r-x -w- :752  rwx r-- -w- :742  | 
|  rwx rwx --x :771  rwx rwx --x :761  rwx r-x --x :751  rwx r-- --x :741  | 
|  rwx rwx --- :770  rwx rwx --- :760  rwx r-x --- :750  rwx r-- --- :740  | 
|                                                                          | 
|  rwx -wx -wx :733                                                        | 
|  rwx -wx -w- :732  rwx -w- -w- :722                                      | 
|  rwx -wx --x :731  rwx -w- --x :721  rwx --x --x :711                    | 
|  rwx -wx --- :730  rwx -w- --- :720  rwx --x --- :710  rwx --- --- :700  | 
|__________________________________________________________________________| 
                                                                          77^
:SEE info node `(coreutils)File permissions'\n
:SEE-ALSO `mon-help-unix-commands', `mon-help-permissions',
`mon-help-tar-incantation', `mon-help-info-incantation'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-permissions :insertp t)
    (mon-help-message-intrp "mon-help-permissions")))
;;
;;; :TEST-ME (mon-help-permissions)
;;; :TEST-ME (mon-help-permissions t)
;;; :TEST-ME (describe-function 'mon-help-permissions)
;;; :TEST-ME (apply 'mon-help-permissions '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-16T12:36:57-05:00Z}#{09471} - by MON KEY>
;;;###autoload
(defun mon-help-unix-commands (&optional insertp intrp)
  "Unix command line programs and builtins.\n
;; :FILE-SYSTEM-INFO-MAN-NODES
cd        ; :SEE info node `(coreutils)cp invocation'
chattr    ;
cksum     ; :SEE info node `(coreutils)cksum invocation'
cmp       ; :SEE info node `(coreutils) '
cp        ; :SEE info node `(coreutils)cp invocation'
cpio      ; :SEE info node `(cpio)'
dd        ; :SEE info node `(coreutils)dd invocation'
df        ; :SEE info node `(coreutils)df invocation'
dir       ; :SEE info node `(coreutils)dir invocation'
dircolors ; :SEE info node `(coreutils)dircolors invocation'
du        ; :SEE info node `(coreutils)du invocation'
find      ; :SEE info node `(find)'
fsck      ; :SEE info node `(coreutils) '
fuser     ; :SEE info node `(coreutils) '
gzip      ; :SEE info node `(gzip)'
install   ; :SEE info node `(coreutils)install invocation'
ln        ; :SEE info node `(coreutils)ln invocation'
ls        ; :SEE info node `(coreutils)'
lsattr    ; :SEE info node `(coreutils) '
lsof      ; :SEE info node `(coreutils) '
md5sum    ; :SEE info node `(coreutils)md5sum invocation'
mkdir     ; :SEE info node `(coreutils)mkdir invocation'
mknod     ; :SEE info node `(coreutils)mknod invocation'
mkfifo    ; :SEE info node `(coreutils)mkfifo invocation'
mount     ; :SEE info node `(coreutils) '
mv        ; :SEE info node `(coreutils)mv invocation'
pwd       ; :SEE info node `(coreutils)pwd invocation'
readlink  ; :SEE info node `(coreutils)readlink invocation'
rm        ; :SEE info node `(coreutils)rm invocation'
rmdir     ; :SEE info node `(coreutils)rmdir invocation'
shred     ; :SEE info node `(coreutils)shred invocation'
size      ; :SEE info node `(binutils)size'
sync      ; :SEE info node `(coreutils)sync invocation'
stat      ; :SEE info node `(coreutils)stat invocation'
sum       ; :SEE info node `(coreutils)sum invocation'
tar       ; :SEE info node `(tar)'
touch     ; :SEE info node `(coreutils)touch invocation'
unlink    ; :SEE info node `(coreutils)unlink invocation'
umask     ; :SEE info node `(coreutils)'
updatedb  ; :SEE info node `(find)Invoking updatedb'
vdir      ; :SEE info node `(coreutils)vdir invocation'\n
;; :PROCESSES-INFO-MAN-NODES
at        ; (woman \"at\")
chroot    ; :SEE info node `(coreutils)chroot invocation'
cron      ; :SEE (woman \"crontab\") :SEE-ALSO `mon-help-crontab'
exit      ; :SEE info node `(coreutils) '
kill      ; :SEE info node `(coreutils)kill invocation'
killall   ; :SEE info node `(coreutils) '
nice      ; :SEE info node `(coreutils)nice invocation'
nohup     ; :SEE info node `(coreutils)nohup invocation'
tty       ; :SEE info node `(coreutils)tty invocation'
ps        ; :SEE (woman \"ps\")
sleep     ; :SEE info node `(coreutils)sleep invocation'
stty      ; :SEE info node `(coreutils)stty invocation'
tee       ; :SEE info node `(coreutils)tee invocation'
time      ; :SEE info node `(coreutils)'
top       ; :SEE (woman \"top\")
wait\n
;; :USER-ENVIRONMENT-INFO-MAN-NODES
chmod     ; :SEE info node `(coreutils)chmod invocation' :SEE-ALSO `mon-help-permissions'
chown     ; :SEE info node `(coreutils)chown invocation'
chgrp     ; :SEE info node `(coreutils)chgrp invocation'
env       ; :SEE info node `(coreutils)env invocation'
finger
hostid    ; :SEE info node `(coreutils)hostid invocation'
id        ; :SEE info node `(coreutils)id invocation'
logname   ; :SEE info node `(coreutils)logname invocation'
printenv  ; :SEE info node `(coreutils)printenv invocation'
mesg
passwd
su        ; :SEE info node `(coreutils) '
sudo      ; :SEE (woman \"sudo\")
uptime
w
wall
write
uname     ; :SEE info node `(coreutils)uname invocation'
groups    ; :SEE info node `(coreutils)groups invocation'
users     ; :SEE info node `(coreutils)users invocation'
who       ; :SEE info node `(coreutils)who invocation'
whoami    ; :SEE info node `(coreutils)whoami invocation'\n
;; :TEXT-PROCESSING-INFO-MAN-NODES
awk      ; :SEE info node `(gawk)Invoking Gawk'
cmp      ; :SEE info node `(diff)Invoking cmp'
comm     ; :SEE info node `(coreutils)comm invocation'
cat      ; :SEE info node `(coreutils)cat invocation'
cut      ; :SEE info node `(coreutils)cut invocation'
csplit   ; :SEE info node `(coreutils)csplit invocation'
diff     ; :SEE info node `(diff)'
ex
expand   ; :SEE info node `(coreutils)expand invocation'
fmt      ; :SEE info node `(coreutils)fmt invocation'
fold     ; :SEE info node `(coreutils)fold invocation'
head     ; :SEE info node `(coreutils)head invocation'
iconv    ; :SEE info node `() '
join     ; :SEE info node `(coreutils)join invocation'
less
more
nl       ; :SEE info node `(coreutils)nl invocation'
objdump  ; :SEE info node `(binutils)objdump'
od       ; :SEE info node `(coreutils)od invocation'
patch    ; :SEE info node `(diff)Invoking patch'
paste    ; :SEE info node `(coreutils)paste invocation'
ptx      ; :SEE info node `(coreutils)ptx invocation'
ed       ; :SEE info node `(ed)'
sed      ; :SEE info node `(sed)'
sort     ; :SEE info node `(coreutils)sort invocation'
split    ; :SEE info node `(coreutils)split invocation'
strings  ; :SEE info node `(binutils)strings'
strip    ; :SEE info node `(binutils)strip'
tsort    ; :SEE info node `(coreutils)tsort invocation'
tac      ; :SEE info node `(coreutils)tac invocation'
tail     ; :SEE info node `(coreutils)tail invocation'
tr       ; :SEE info node `(coreutils)tr invocation'
unexpand ; :SEE info node `(coreutils)unexpand'
uniq     ; :SEE info node `(coreutils)uniq invocation'
wc       ; :SEE info node `(coreutils)wc invocation'\n
;; :SHELL-PROGRAMMING-INFO-MAN-NODES
alias
bash      ; :SEE info node `(bash)'
basename  ; :SEE info node `(coreutils)basename invocation'
dirname   ; :SEE info node `(coreutils)dirname invocation'
pathchk   ; :SEE info node `(coreutils) pathchk invocation'
unset     ; :SEE info node `(coreutils)'
echo      ; :SEE info node `(coreutils)echo invocation'
printf    ; :SEE info node `(coreutils)printf invocation'
yes       ; :SEE info node `(coreutils)yes invocation'
expr      ; :SEE info node `(coreutils)expr invocation'
false     ; :SEE info node `(coreutils)false invocation'
test      ; :SEE info node `(coreutils)test invocation'
true      ; :SEE info node `(coreutils)true invocation'
xargs     ; :SEE info node `(find)Multiple Files'\n
;; :NETWORKING-INFO-MAN-NODES
host
netstat
ping      ; :SEE (woman \"ping\")
netcat    ; :SEE (woman \"netcat\")
traceroute ; :SEE (woman \"traceroute\")
;; :SEARCHING-INFO-MAN-NODES
find      ; :SEE info node `(find)Invoking find'
grep      ; :SEE info node `(grep)'
locate    ; :SEE info node `(find)Invoking locate'
whereis   ; :SEE (woman \"whereis\")
which     ; :SEE (woman \"which\")\n
;; :MISCELLANEOUS-INFO-MAN-NODES
apropos   ; :SEE (woman \"apropos\")
banner    ; :SEE (woman \"\")
bc
cal
clear     ; :SEE (woman \"clear\")
date      ; :SEE info node `(coreutils)date invocation'
file
help
history   ; :SEE info node `(bash)Bash History Builtins'
info      ; :SEE info node `(info)'
lp
man       ; :SEE (woman \"man\")
pax
size      ; :SEE (woman \"size\")
tput
type
uname     ; :SEE info node `(coreutils)uname invocation'
whatis
:SEE-ALSO `mon-help-permissions', `mon-help-crontab', `mon-help-tar-incantation', \n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-unix-commands :insertp t)
    (mon-help-message-intrp "mon-help-unix-commands")))
;;
;;; :TEST-ME (mon-help-unix-commands)
;;; :TEST-ME (mon-help-unix-commands t)
;;; :TEST-ME (describe-function 'mon-help-unix-commands)
;;; :TEST-ME (apply 'mon-help-unix-commands '(t))

 
;;; ==============================
;;; :CREATED <Timestamp: Tuesday June 23, 2009 @ 11:37.05 AM - by MON KEY>
;;;###autoload
(defun mon-help-format-width (&optional insertp intrp)
  "Invoking `format' control string to specify padding using the width flag.\n
:SEE info node `(elisp)Formatting Strings'\n
:EXAMPLE
  \(let \(\(x 'test\) \(y \"\"\)\)
     \(format \"This is a %-9s.\\nThis is a %9s.\\nThis is a %s %4s.\" x x x y\)\)\n
 =>This is a test     .\n   This is a      test.\n   This is a test     .\n
\(format \"%4c%s%4cbubba2\" 32 'bubba1 32\)\n
 =>    bubba1    bubba2\n
:SEE-ALSO `mon-help-read-functions', `mon-help-print-functions',
`mon-help-char-representation', `format', `mon-string-justify-left',
`mon-string-fill-to-col', `mon-line-indent-from-to-col',
`mon-line-strings-pipe-to-col'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-format-width :insertp t)
    (mon-help-message-intrp "mon-help-format-width")))
;;
;;; :TEST-ME (mon-help-format-width t)
;;; :TEST-ME (mon-help-format-width)
;;; :TEST-ME (describe-function 'mon-help-format-width)
;;; :TEST-ME (apply 'mon-help-format-width '(t))

;;; ==============================
;;; :FIXME This function works... though it is ugly as hell... sorry Andy.
;;; :NOTE This fails: (elisp-index-search "elisp-index-search")
;;; :COURTESY Andy Stewart <lazycat.manatee@gmail.com>
;;; :SEE (URL `http://www.emacswiki.org/emacs/lazycat-toolkit.el')
;;; :MODIFICATIONS <Timestamp: #{2009-08-26T17:36:47-04:00Z}#{09353} - by MON KEY>
;;;###autoload
(defun mon-index-elisp-symbol ()
  "Find TOPIC in the indices of the Emacs Lisp Reference Manual.\n
:EXAMPLE\n\(elisp-index-search \"setq\")\n
:ALIASED-BY `mon-help-elisp-info'\n
:SEE-ALSO `finder-by-keyword', `mon-help-unix-commands', 
`Info-virtual-index-find-node', `Info-virtual-index'.\n►►►"
  (interactive)
  (let ((topic (symbol-name (symbol-at-point))))
    (setq topic 
          (read-string 
           (format ":FUNCTION `mon-index-elisp-symbol' -- subject to look up <%s>: " topic)
           nil nil topic))
    (info "elisp")
    (Info-index topic)))
;;
;;; :TEST-ME (elisp-index-search "setq")

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-06T14:38:14-04:00Z}#{10142} - by MON KEY>
;;;###autoload
(defun mon-help-w32-functions (&optional insertp intrp)
  "A list of W32 related functions and variables.\n  
;; :W32-DISPLAY-FUNCTIONS
`w32-define-rgb-color'
`w32-default-color-map'
`w32-window-exists-p'\n
;; :W32-DISPLAY-VARIABLES
`w32-strict-painting'
`w32-enable-palette'
`w32-enable-synthesized-fonts'
`w32-bdf-filename-alist'
`w32-strict-fontnames'\n
;; :W32-SYSTEM
`w32-shell-execute'
`w32-send-sys-command'
`w32-battery-status'\n
;; :W32-KEYS-FUNCTIONS
`w32-register-hot-key'
`w32-unregister-hot-key'
`w32-registered-hot-keys'
`w32-reconstruct-hot-key'
`w32-toggle-lock-key'\n
;; :W32-KEYS-VARIABLES
`w32-alt-is-meta'
`w32-pass-alt-to-system'
`w32-quit-key'
`w32-phantom-key-code'
`w32-enable-num-loc'
`w32-enable-caps-lock'
`w32-scroll-lock-modifier'
`w32-apps-modifier'
`w32-mouse-button-tolerance'
`w32-mouse-move-interval'
`w32-pass-extra-mouse-buttons-to-system'
`w32-pass-multimedia-buttons-to-system'
`w32-pass-rwindow-to-system'
`w32-pass-lwindow-to-system'
`w32-rwindow-modifier'
`w32-pass-multimedia-buttons-to-system'
`x-cut-buffer-max'                     ;<VARIABLE>\n
:SEE-ALSO `*w32-env-variables-alist*', `mon-help-w32-cmd-commands',
`mon-help-w32-env', `mon-help-w32-shell-execute', `mon-help-key-functions'.\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-w32-functions :insertp t)
    (mon-help-message-intrp "mon-help-w32-functions")))
;;
;;; :TEST-ME (mon-help-w32-functions)
;;; :TEST-ME (mon-help-w32-functions t)
;;; :TEST-ME (describe-function 'mon-help-w32-functions)
;;; :TEST-ME (apply mon-help-w32-functions nil '(t))

 
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-01T22:22:37-04:00Z}#{09405} - by MON KEY>
;;; :CREATED <Timestamp: Thursday July 02, 2009 @ 11:50.50 AM - by MON KEY>
;;(defvar *w32-env-variables-alist* nil
(defcustom *w32-env-variables-alist* nil
  "List of environment variables available in w32.\n
Each element of List has the form:\n
 (<SYMBOL> <%SYMBOL%> \"<SYM-DOCS>\")\n
- <SYMBOL> is an environmental variable, e.g. CMDLINE\n
- <%SYMBOL%> is its percent-escaped counterpart, e.g. %CMDLINE%\n
- <SYM-DOCS> is some documentation string pertaining to <SYMBOL>\n
:CALLED-BY `mon-help-w32-env'.\n
:SEE-ALSO .\n►►►"
  :type '(repeat (list (symbol :tag "<SYMBOL>")
                       (symbol :tag "<%SYMBOL%>")
                       (string :tag "<SYM-DOCS>")))
  :group 'mon-doc-help-utils)

;;
(unless (and (intern-soft "*w32-env-variables-alist*" obarray)
             (bound-and-true-p *w32-env-variables-alist*))
  (setq *w32-env-variables-alist*
        '((ALLUSERSPROFILE %ALLUSERSPROFILE%
                           "Local returns the location of the All Users Profile.")
          (APPDATA %APPDATA%
                   "Local returns the location where applications store data by default.")
          (CD %CD%
              "Local returns the current directory string.")
          ;;(CLIENTNAME %CLIENTNAME% "")
          (CMDCMDLINE %CMDCMDLINE%
                      "Local returns the exact command line used to start the current cmd.exe")
          (CMDEXTVERSION %CMDEXTVERSION%
                         "System returns the version number of the current Command Processor Extensions.")
          ;; (COMMONPROGRAMFILES %COMMONPROGRAMFILES% ""
          (COMPUTERNAME %COMPUTERNAME%
                        "System returns the name of the computer.")
          (COMSPEC %COMSPEC%
                   "System returns the exact path to the command shell executable.")
          (DATE %DATE%
                "System returns the current date. This variable uses the same format as the date /t command. Cmd.exe generates this variable. For more information about the date command, see the Date command.")
          (ERRORLEVEL %ERRORLEVEL%
                      "System returns the error code of the most recently used command. A non-0 value usually indicates an error.")
          ;; (FP_NO_HOST_CHECK  %FP_NO_HOST_CHECK% "" ;Consensus seems to be this is a frontpage related thing.
          (HOMEDRIVE %HOMEDRIVE%
                     "System returns which local workstation drive letter is connected to the user's home directory. This variable is set based on the value of the home directory. The user's home directory is specified in Local Users and Groups.")
          (HOMEPATH %HOMEPATH%
                    "System returns the full path of the user's home directory. This variable is set based on the value of the home directory. The user's home directory is specified in Local Users and Groups.")
          (HOMESHARE %HOMESHARE%
                     "System returns the network path to the user's shared home directory. This variable is set based on the value of the home directory. The user's home directory is specified in Local Users and Groups.")
          (LOGONSERVER %LOGONSERVER% ;; :SEE (URL `http://support.microsoft.com/kb/183495')
                       "Local returns the name of the domain controller that validated the current logon session.")
          (NUMBER_OF_PROCESSORS %NUMBER_OF_PROCESSORS%
                                "System specifies the number of processors installed on the computer.")
          (OS %OS%
              "System returns the OS name. Windows XP and Windows 2000 display the OS as Windows_NT.")
          (PATH %PATH%
                "System specifies the search path for executable files")
          (PATHEXT %PATHEXT%
                   "System returns a list of the file extensions that the OS considers to be executable.")
          (PROCESSOR_ARCHITECTURE %PROCESSOR_ARCHITECTURE%
                                  "System returns the processor's chip architecture. Values: x86, IA64.")
          (PROCESSOR_IDENTIFIER %PROCESSOR_IDENTIFIER%
                                "System returns a description of the processor.")
          (PROCESSOR_LEVEL %PROCESSOR_LEVEL%
                           "System returns the model number of the computer's processor.")
          (PROCESSOR_REVISION %PROCESSOR_REVISION%
                              "System returns the revision number of the processor.")
          (PROMPT %PROMPT%
                  "Local returns the command-prompt settings for the current interpreter. Cmd.exe generates this variable.")
          (RANDOM %RANDOM%
                  "System returns a random decimal number between 0 and 32767. Cmd.exe generates this variable.")
          ;;(SESSIONNAME %SESSIONNAME%  "")
          (SYSTEMDRIVE %SYSTEMDRIVE%
                       "System returns the drive containing the Windows root directory (i.e., the system root.")
          (SYSTEMROOT %SYSTEMROOT%
                      "System returns the location of the Windows root directory.")
          (TEMP %TEMP%
                "System and User return the default temporary directories for applications that are available to users who are currently logged on. Some applications require TEMP and others require TMP.")
          (TMP %TMP%
               "System and User return the default temporary directories for applications that are available to users who are currently logged on. Some applications require TEMP and others require TMP.")
          (TIME %TIME%
                "System returns the current time. This variable uses the same format as the time /t command. Cmd.exe generates this variable. For more information about the time command. :SEE-ALSO the Time command.")
          (USERDOMAIN %USERDOMAIN%
                      "Local returns the name of the domain that contains the user's account.")
          (USERNAME %USERNAME%
                    "Local returns the name of the user currently logged on.")
          (USERPROFILE %USERPROFILE%
                       "Local returns the location of the profile for the current user.")
          (WINDIR %WINDIR%
                  "System returns the location of the OS directory.")
          (PROGRAMFILES %PROGRAMFILES%
                        "Returns the location of the default install directory for applications.")))
  (custom-note-var-changed '*w32-env-variables-alist*)
  )
;;
;;; :TEST-ME (assoc 'TMP *w32-env-variables-alist*)
;;; :TEST-ME (car (assoc 'TMP *w32-env-variables-alist*))
;;; :TEST-ME (cadr (assoc 'TMP *w32-env-variables-alist*))
;;; :TEST-ME (caddr (assoc 'TMP *w32-env-variables-alist*))
;;; :TEST-ME (assoc "Program Files" *w32-env-variables-alist*)
;;; :TEST-ME (mapcar (lambda (x) (car x))*w32-env-variables-alist*)
;;
;;;(progn (makunbound '*w32-env-variables-alist*) (unintern "*w32-env-variables-alist*" obarray) )

 
;;; ==============================
;;; :REQUIRES `mon-string-justify-left'
;;; :MODIFICATIONS <Timestamp: #{2010-01-08T18:51:29-05:00Z}#{10015} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-01T22:22:37-04:00Z}#{09405} - by MON KEY>
;;; :CREATED <Timestamp: Thursday July 02, 2009 @ 11:50.50 AM - by MON KEY>
(defun mon-help-w32-env (&optional insertp intrp)
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-w32-env :insertp t)
    (mon-help-message-intrp "mon-help-w32-env")))
;;
;;; :TEST-ME (mon-help-w32-env )
;;; :TEST-ME (mon-help-w32-env )
;;; :TEST-ME (mon-help-w32-env )

;; Now we tack on a docstring using.
(eval-when (compile) ; load)
  (let ((tt-doc *w32-env-variables-alist*))
    (setq tt-doc
          (concat 
           "Environment variables available in w32.\n"
           "Called interactively with Prefix arg non-nil prints to current-buffer.\n\n---\n"
           "Alist of W32 Environmental variables in var `*w32-env-variables-alist*':\n\n"
           (mapconcat #'(lambda (x)
                          (let* ((k (car x))
                                 (v (cdr (assoc k tt-doc)))
                                 (v-var (car v))
                                 (v-doc (mon-string-justify-left (cadr v) 68 3)))
                            (format "-\n%s %s \n%s" k v-var v-doc)))
                      tt-doc "\n")
           "\n\n:EXAMPLE\n\n(assoc 'WINDIR *w32-env-variables-alist*)\n\n"
           ":EXAMPLE\n\n"
           "Open a cmd prompt and type echo %appdata% which should return\n"
           "the full path to your profile's Application Data directory.\n"
           "If calling from a batch file remember to quote the thusly %variable%\n"
           "or: set VARIABLE=value.\n\n"
           ":SOURCE\n:SEE \(URL `http://windowsitpro.com/article/articleid/23873/')\n"
           ":SEE \(URL `http://technet.microsoft.com/en-us/library/bb490954.aspx'\)\n"
           ":SEE-ALSO `mon-help-w32-env', `mon-help-w32-functions'. \n►►►\n"))
    (unless (get 'mon-help-w32-env 'function-documentation)
      (put 'mon-help-w32-env 'function-documentation tt-doc)))
) ;; :CLOSE eval-when

;;; :WAS
;;; (eval-when (compile eval)
;;; (mon-help-put-var-doc-val->func
;;;    '*w32-env-variables-alist*
;;;    'mon-help-w32-env
;;;   ;; PRE-V-STR
;;; "Environment variables available in w32.
;;; Called interactively with Prefix arg non-nil prints to current-buffer.\n\n---
;;; alist of W32 Environmental variables in var `*w32-env-variables-alist*':\n\n"
;;; ;; CUT-V-STR
;;; "\*List of environment variables available in w32.
;;; :CALLED-BY `mon-help-w32-env'."
;;; ;; PST-V-STR
;;; "\n\n:EXAMPLE\n(assoc 'WINDIR *w32-env-variables-alist*)\n
;;; :EXAMPLE
;;; Open a cmd prompt and type echo %appdata% which should return
;;; the full path to your profile's Application Data directory.
;;; If calling from a batch file remember to quote the thusly %variable%
;;; or: set VARIABLE=value.\n
;;; :SOURCE\n:SEE \(URL `http://windowsitpro.com/article/articleid/23873/')
;;; :SEE \(URL `http://technet.microsoft.com/en-us/library/bb490954.aspx'\)\n►►►"))
;;
;;; :TEST-ME (mon-help-w32-env t)
;;
;;;(progn (fmakunbound 'mon-help-w32-env) (unintern "mon-help-w32-env" obarray) )

 
;;; ==============================
;;; :COURTESY Aaaron Hawley :HIS
;;; :SEE (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;; :MODIFICATIONS <Timestamp: #{2010-02-26T14:21:58-05:00Z}#{10085} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-11-20T16:48:11-05:00Z}#{09475} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: Wednesday June 17, 2009 @ 11:31.47 AM - by MON KEY>
(defvar *mon-help-reference-keys* nil)
;;
;;
(unless (and (intern-soft "*mon-help-reference-keys*" obarray)
             (bound-and-true-p *mon-help-reference-keys*))
  (setq *mon-help-reference-keys*
"#:START:REFERENCE-SHEET#

;; :OPEN-KEY-BINDINGS
emacs& RET -- or click a graphical icon
emacs -nw RET -- open in terminal, not in a window
q -- clear splash screen

;; :EXIT-KEY-BINDINGS
C-x C-c -- save buffers and quit
C-] -- recursive edit

;; :CANCEL-KEY-BINDINGS
C-g -- a command
C-M-c -- recursive edit
M-x `top-level' -- all recursive edits

;; :RECURSIVE-EDIT-KEY-BINDINGS
C-] -- exit recursive edit
C-M-c -- cancel recursive edit
M-x top-level -- cancel all recursive edits

;; :CUSTOMIZE-KEY-BINDINGS
C-x C-f ~/.emacs RET -- Emacs initialization file
M-x `customize' -- main menu
M-x `customize-variable' -- variable
M-x `customize-apropos' -- search
M-x `customize-mode' -- mode
M-x `global-set-key' -- define key binding
M-x `local-set-key' -- define key binding for current buffer
M-x `display-time' -- show clock, system load and email flag in mode line
M-x `display-battery-mode' -- show system power
M-x `size-indication-mode' -- show size in mode line
M-x `column-number-mode' -- show column number in mode line
M-x `ruler-mode' -- add a ruler to the current buffer's window
M-x `auto-revert-mode' -- update file if changes on disk
M-x `global-auto-revert-mode' -- update any buffer's file
M-x `menu-bar-mode' -- toggle existence of drop-down menu
M-x `tool-bar-mode' -- toggle existence of clickable tool bar
M-x `scroll-bar-mode' -- toggle scroll bar
M-x `toggle-scroll-bar' -- toggle scroll bar in current frame
M-x `blink-cursor-mode' -- toggle blinking of cursor

;; :MENU-KEY-BINDINGS
M-` -- text interaction with drop-down menu
F10 -- same as previous
M-x `menu-bar-mode' -- toggle existence of drop-down menu
M-x `tool-bar-mode' -- toggle existence of clickable tool bar

;; :HELP-KEY-BINDINGS
C-h ? -- menu
C-h C-h -- menu
SPC -- scroll down menu
DEL -- scroll up menu
q -- close menu
C-h t -- tutorial
C-h r -- Emacs info manual :SEE :INFO-HELP-KEY-BINDINGS
C-h F -- Emacs FAQ
C-h c <KEY> -- what is command for KEY
C-h k <KEY> -- describe command for KEY
C-h w <COMMAND> RET -- where is key binding for COMMAND
C-h m -- show current modes
C-h b -- show current key bindings
C-h a <SEARCH> <TERMS> RET -- list commands matching SEARCH TERMS
C-h f <FUNCTION> RET -- describe FUNCTION
C-h v <VARIABLE> RET -- describe and show values for VARIABLE
TAB -- forward to cross-reference link
M-TAB -- backward cross-reference link
S-TAB -- backward cross-reference link
RET -- follow cross-reference
C-c C-c -- follow cross-reference
C-c C-b -- go back
q -- quit

;; :INFO-HELP-KEY-BINDINGS
C-h i -- open directory of manuals
C-h r -- open Emacs manual
q -- close
t -- table of contents \(menu\)
d -- back to directory
m <ENTRY> -- visit menu ENTRY in table of contents
TAB -- forward to cross-reference link
M-TAB -- backward cross-reference link
S-TAB -- backward cross-reference link
RET -- follow link
l -- back to last visited page
r -- forward to last visited page
C-v -- scroll down
SPC -- scroll down
M-v -- scroll up
DEL -- scroll up
b -- scroll up
n -- next node
p -- previous node
i <WORD> RET -- look for WORD in current manual's index
i RET -- go to index node
s <PATTERN> -- search forward for regular expression PATTERN
S <PATTERN> -- case-insensitive search for regular expression PATTERN
C-s <SEARCH> -- forward to SEARCH :SEE :SEARCH-KEY-BINDINGS
C-r <SEARCH> -- reverse to SEARCH
M-n -- make a duplicate buffer in other window :SEE :WINDOW-KEY-BINDINGS

;; :MINIBUFFER-KEY-BINDINGS
M-p -- previous input
M-n -- recent input
TAB -- complete name of buffer, file, symbol :SEE :COMPLETION-KEY-BINDINGS
C-i -- same as previous
M-r -- search for previous input
M-s -- search for previous input
C-h e -- show recently echoed messages
C-g -- exit

;; :COMPLETION-KEY-BINDINGS
TAB -- complete name of buffer, file, function, variable, ...
SPC -- completion, unless a file
? -- list completions
M-v -- go to \"completions\" buffer
<right> -- next completion
<left> -- previous completion
RET -- select completion
ESC ESC ESC -- exit back to minibuffer

;; :MODE-KEY-BINDINGS
C-h m -- help with current :SEE :HELP-KEY-BINDINGS
M-x `text-mode' -- for writing
M-x `fundamental-mode' -- a simple default
M-x `normal-mode' -- change back to what Emacs thought it was
M-x `customize-mode' -- customize current :SEE :CUSTOMIZE-KEY-BINDINGS

;; :FILE-KEY-BINDINGS
C-x C-f -- open
C-x C-f -- new
M-x `make-directory' RET RET -- including parent directories
C-x C-v -- close current and open another
C-x C-s -- save
C-x s -- prompt to save any buffer that has been modified
C-x C-f M-p RET -- open previously saved or opened
C-x C-w <PATH> RET -- save current to PATH
C-x C-w <DIRECTORY> RET -- save to DIRECTORY using file or buffer name
C-x C-q -- toggle as read only
C-x C-f archive.tar RET -- list contents of archive
C-x C-f file.gz RET -- open compressed Gzip
C-x C-f file.zip RET -- list contents of zip file
M-x `rename-file' -- rename current
M-x `view-file' -- view file :SEE :READ-ONLY-KEY-BINDINGS
<insert> -- change whether to use overwrite or switch back to insert mode
M-x `overwrite-mode' -- same as previous
M-x `binary-overwrite-mode' -- edit file as literal bytes
C-x d M-p RET R -- rename previously saved or opened
C-x i -- insert other file into current buffer
M-x `write-region' -- save region
C-x h M-x write-region -- save buffer once to alternate
M-x `find-file-at-point' -- open file name at point
M-x `revert-buffer' -- restore buffer with file on disk
M-x `recover-file' -- recover auto-save data after a crash
M-x `recover-session' -- recover all files with auto-save data
M-x `size-indication-mode' -- show size in mode line
M-x `auto-revert-mode' -- update file if changes on disk
M-x `global-auto-revert-mode' -- update any buffer's file
M-x `auto-revert-tail-mode' -- update end of file with changes on disk

;; :BUFFER-KEY-BINDINGS
M-< -- `beginning-of-buffer'
M-> -- end-of-buffer
C-x h -- mark
C-x k <BUFFER> RET -- kill BUFFER
C-x k RET -- kill current
C-x b RET -- switch to last buffer
C-x b <BUFFER> RET -- switch to BUFFER or make new BUFFER
C-x 4 b -- switch to a buffer in other window :SEE :WINDOW-KEY-BINDINGS
C-x 4 C-o -- show a buffer in other window :SEE :WINDOW-KEY-BINDINGS
C-x C-b -- list all :SEE :BUFFER-MENU-KEY-BINDINGS
M-x `bury-buffer' -- avoid switching to current buffer
C-x b M-p -- switch to previously switched buffer
C-x C-s -- save current contents to file on disk
C-x s -- save modified files
M-x `rename-buffer' RET <NAME> -- rename current to NAME
M-x `rename-uniquely' -- remove \"<X>\" suffix from buffer name if possible
M-x `revert-buffer' -- restore contents with file on disk :SEE :UNDO-KEY-BINDINGS
M-x `erase-buffer' -- delete everything :SEE :DELETE-KEY-BINDINGS
M-x `clone-indirect-buffer' -- open an indirect buffer based on current
C-x 4 c -- open an indirect buffer but in another window
C-h f car RET C-x o M-x clone-buffer RET C-h f cdr RET -- compare two functions

;; :READ-ONLY-KEY-BINDINGS
C-x C-r <FILE> RET -- open FILE as read only
C-x C-q -- toggle write status
M-x `view-mode' -- view mode for current buffer
M-x `view-file' <FILE> RET -- open FILE in view mode
M-x `view-buffer' -- view mode for other buffer
SPC -- `scroll-down'
DEL -- `scroll-up'
h -- view mode help
q -- turn off view mode
M-x `normal-mode' -- turn off `view-mode'

;; :WINDOW-KEY-BINDINGS
C-v -- scroll down
M-v -- scroll up
C-M-v -- scroll other window down
M-< -- `beginning-of-buffer'
M-> -- `end-of-buffer'
M-x `beginning-of-buffer-other-window' -- beginning of other buffer
M-x `end-of-buffer-other-window' -- end of other buffer
M-r -- move to first column of center line in display
C-0 M-r -- move to first column of first displayed line
M-- M-r -- move to first column of last displayed line
C-4 M-r -- move to first column of fourth displayed line
C-u - 3 M-r -- move to first column of third to last displayed line
C-x 2 -- `split-window-vertically' in two
C-x o -- `switch between-windows'
C-x 4 b -- switch to a buffer in other window
C-x 4 C-o -- show a buffer in other window
C-x 0 -- close current
C-x 1 -- close all others leaving current
C-x 4 f -- open file in other
C-x - -- shrink to fit text
C-x + -- equalize window heights
C-u 5 C-x ^ -- enlarge 5 lines taller
M- 5 C-x ^ -- shrink 5 lines shorter
C-x 3 -- split horizontally
C-u 5 C-x } -- enlarge 5 columns wider
M- 5 C-x } -- shrink 5 columns narrower
C-x < -- scroll horizontally right
C-x > -- scroll horizontally left
M-x `toggle-truncate-lines' -- change if long lines fold or are truncated

;; :BUFFER-MENU-KEY-BINDINGS
C-x C-b -- list
C-u C-x C-b -- list only buffers associated with files
SPC -- move down
n -- move down
C-n -- move down
p -- move up
C-p -- move up
% -- toggle current as read only :SEE :READ-ONLY-KEY-BINDINGS 
? -- show modes for current
g -- update list
T -- toggle list to buffers associated with files
C-o -- view current in other window :SEE :WINDOW-KEY-BINDINGS
RET -- view current in this window
e -- goto current in this window
f -- goto current in this window
1 -- goto current in only 1 window
2 -- goto current in only 1 window
V -- open current buffer in View mode :SEE :READ-ONLY-KEY-BINDINGS 
b -- bury current :SEE :BUFFER-KEY-BINDINGS 
m -- mark current and move down
C-d -- mark to delete current and move up
d -- mark to delete current and move down
k -- mark to delete current and move down
C-k -- mark to delete current and move down
x -- execute marks
q -- quit

;; :REDISPLAY-KEY-BINDINGS
C-l -- with line at center of window
C-0 C-l -- with current line at top of window
M-- C-l -- with current line at bottom of window
C-u -1 C-l -- same as previous
C-M-l -- try to make the top of the current function visible in the window
C-M-l C-M-l -- with current line at top of window

;; :COMMAND-KEY-BINDINGS
C-h l -- show recently typed keys
C-h e -- show recently echoed messages
C-x z -- repeat last command
C-x M-ESC -- edit and re-evaluate last command as Emacs Lisp
C-x M-: -- same as previous
M-x `command-history' -- show recently run commands
x -- run command at line in history

;; :ITERATIVE-COMMAND-KEY-BINDINGS
C-u -- repeat next command 4 times
M-- -- next command once in opposite direction
C-u 8 -- repeat next command 8 times
M-8 -- repeat next command 8 times
C-8 -- repeat next command 8 times
C-u 8 C-u -- repeat next command 8 times
C-8 -- repeat next command 8 times
M-- 3 -- repeat next command 3 times in opposite direction
C-u -3 -- repeat next command 3 times in opposite direction
C-u C-u -- repeat next command 16 times
C-u C-u C-u -- repeat next command 64 times
C-u 369 C-u 0 -- insert 369 zeros

;; :NON-ITERATIVE-COMMAND-KEY-BINDINGS
C-u -- toggle behavior of next command
M-- -- toggle behavior of next command with negative value

;; :MACRO-KEY-BINDINGS
C-x \( -- start recording macro
F3  -- same as previous
C-x \) -- finish recording macro
F4  -- same as previous
C-x e -- finish recording macro and run macro
C-x e -- run last macro
C-x C-k r -- go to each line in region and run last macro - `apply-macro-to-region-lines'
C-x C-k n -- name last macro
M-x <MACRO> -- run macro MACRO
C-x C-k b -- bind last macro to a key `kmacro-bind-to-key'
C-x C-k e -- edit last macro - `edit-kbd-macro'
C-x C-k e M-x <MACRO> -- edit named MACRO
C-x C-k e C-h l -- edit and make recently typed keys
C-x C-f ~/.emacs RET M-x insert-kbd-macro -- save macro

;; :KEYS-KEY-BINDINGS
C-h l -- show recently typed keys
M-x `global-set-key' -- set for all buffers
M-x `local-set-key' -- set for current buffer
C-q -- insert next character literally
C-q TAB -- insert literal tab character
C-q C-j -- insert literal newline
C-q C-m -- insert literal carriage return
C-q C-l -- insert literal form feed \(page delimiter\)
C-x @ c -- modify next key with Control
C-x @ m -- modify next key with Meta
C-x @ S -- modify next key with Shift
C-x @ h -- modify next key with Hyper
C-x @ s -- modify next key with Super
C-x @ a -- modify next key with Alt

;; :UNDO-KEY-BINDINGS
C-x u -- `undo', repeat to further undo
C-_ -- undo, repeat to further undo
C-/ -- undo, repeat to further undo
C-/ C-g C-/ -- undo, then redo
C-/ C-/ C-g C-/ C-/ -- undo, undo, then redo, redo
M-x `revert-buffer' -- restore buffer with file on disk
M-x `buffer-disable-undo' -- turn off for current buffer
M-x `buffer-enable-undo' -- turn on for current buffer

;; :SEARCH-KEY-BINDINGS
C-s <MATCH> -- forward to end of MATCH
C-r <MATCH> -- reverse to front of MATCH
C-r C-s <MATCH> -- forward to end of MATCH
C-s C-r <MATCH> -- reverse to front of MATCH
C-s <MATCH> C-s -- forward to end of second MATCH
C-r <MATCH> C-r -- reverse to front of second MATCH
DEL -- if not at first match, go to previous match
DEL -- if at first match, delete character from search string
C-M-w -- always delete character from search string
C-s <MATCH> C-s C-r -- forward to start of second MATCH
C-r <MATCH> C-r C-s -- reverse to end of second MATCH
C-s <MATCH> C-s C-s DEL -- forward to end of second MATCH
C-r <MATCH> C-r C-r DEL -- reverse to start of second MATCH
RET -- finish search
C-g -- cancel search if current search is successful
C-g -- undo search to last successful search
C-s C-j -- search for newline
C-s C-q C-m -- search for carriage return
C-s C-M-y -- search for current character
C-s C-M-y C-M-y -- search for next two characters
C-s C-M-y C-M-y DEL -- search for current character
C-s C-w -- search for rest of current word
C-s C-w C-w -- search for next two words
C-s C-w C-w DEL -- search for rest of current word
C-s C-y -- search for rest of current line
C-s C-y DEL -- undo search for rest of current line
C-s M-y -- search for last killed text :SEE :KILL-CUT-KEY-BINDINGS
C-s M-y -- search for last killed text :SEE :KILL-CUT-KEY-BINDINGS
C-s M-p -- show previous search
C-s M-n -- show oldest stored search
C-s M-TAB <BEGINNING>  -- complete for BEGINNING of stored searches
C-s C-s -- resume last search backward
C-r C-r -- resume last search forward
M-e -- edit search
M-r -- toggle regular expression search
M-c -- toggle case-sensitivity of search
M-% -- search, query, and replace :SEE :REPLACE-KEY-BINDINGS
C-s <SEARCH> M-% <REPLACE> -- interactive query SEARCH and REPLACE
M-x isearch-toggle-case-fold -- change case-sensitivity of all searches
C-h f isearch-forward RET -- help

;; :NON-INTERACTIVE-SEARCH-KEY-BINDINGS
C-s RET -- forward case-sensitive
C-r RET -- backward case-sensitive
C-s RET C-w -- forward word-based ignoring punctuation and whitespace
C-r RET C-w -- backward word-based ignoring punctuation and whitespace

;; :REGULAR-EXPRESSION-KEY-BINDINGS
C-M-s -- search forward
C-M-r -- search reverse
M-r -- toggle off regexp
C-M-s C-s -- repeat last regexp forward
C-M-r C-s -- repeat last regexp backward
C-r -- suspend replacement and editing buffer
C-M-c -- resume query and replace :SEE :RECURSIVE-EDIT-KEY-BINDINGS 
C-M-% -- regexp replace
C-M-s <SEARCH> M-% <REPLACE> -- interactive query replace :SEE :REPLACE-KEY-BINDINGS
M-x occur -- show matches in buffer
M-x count-matches -- count matches
M-x flush-lines -- delete matching lines
M-x keep-lines -- keep matching lines delete the rest

;; :REPLACE-KEY-BINDINGS
M-% -- search, query, and replace
C-M-% -- search regular expression, query, and replace
M-% RET -- resume last
C-M-% RET -- resume last as regexp
C-s <SEARCH> M-% <REPLACE> -- interactive
C-M-s <SEARCH> M-% <REPLACE> -- interactive with regexp
y -- replace one and go to next
SPC -- replace one and go to next
, -- replace but don't move
n -- skip
DEL -- skip
^ -- previous
! -- replace all
e -- edit replacement
C-r -- suspend to edit buffer
C-M-c -- finish edit and resume
RET -- stop
q -- stop
C-x d *.c RET Q int RET long -- replace \"long\" for \"int\" in .c files

;; :DELETE-KEY-BINDINGS
C-d -- current character
C-u C-d -- next 4 characters
C-u 8 C-d -- next 8 characters
C-u C-u -- next 16 characters
DEL -- character backwards \(backspace\)
M-- C-d -- same as previous
C-u C-u C-u DEL -- previous 64 characters
C-u 5 DEL -- previous 5 characters
M-x `delete-region' -- region
M-x `erase-buffer' -- entire buffer

;; :KILL-CUT-KEY-BINDINGS
C-SPC C-f C-w -- character
M-d -- word
C-k -- to end of line
C-0 C-k -- beginning of line
C-S-DEL -- entire line
C-1 C-k -- line including newline
M-- C-k -- to beginning of previous line
C-u C-k -- next 4 lines
M-k -- sentence :SEE :SENTENCE-KEY-BINDINGS
C-w -- region :SEE :REGION-KEY-BINDINGS
M-w -- region but don't delete \(copy\)
M-d -- word :SEE :WORD-KEY-BINDINGS
C-M-k -- sexp :SEE :SEXP-AND-PAREN-KEY-BINDINGS
M-DEL -- sexp backwards
C-M-w C-w -- append to next
C-M-w -- append to next
C-M-w C-w -- region appending to previous
C-M-w M-w -- region appending to previous, but don't delete \(copy\)
C-M-w C-k -- line appending to previous
C-M-w M-d -- word appending to previous
C-M-w M-k -- sentence appending to previous
C-M-w M-x `kill-paragraph' -- paragraph appending to previous
C-M-w C-M-k -- sexp appending to previous
C-M-w M-DEL -- sexp backward appending to previous
M-z -- delete everything to a character
C-1 M-z -- same as previous
M-- M-z -- delete everything to a character backwards
C-u -1 M-z -- same as previous
C-u 3 M-z -- delete everything to 3rd occurrence of a character

;; :YANK-PASTE-KEY-BINDINGS
C-y -- the last kill sequence
M-y -- the 2nd to last kill sequence

;; :MARK-KEY-BINDINGS
C-SPC -- set at current point
C-@ -- set at current point
C-x C-x -- toggle between current point and mark
C-x C-SPC -- move to last set mark
C-x C-@ -- move to last set mark
C-x h -- buffer
M-h -- paragraph
C-M-h -- function
C-x C-p -- page separated by form feed
M-@ -- word
C-M-@ -- sexp :SEE :SEXP-AND-PAREN-KEY-BINDINGS
C-M-SPC -- sexp
C-SPC C-SPC -- temporarily turn on transient mark mode
M-x `transient-mark-mode' -- turn on transient mark mode

;; :REGION-KEY-BINDINGS
C-SPC -- set end-point of region
C-@ -- set end-point of region
C-w -- kill
M-w -- kill but don't delete \(copy\)
M-= -- count lines and characters
C-x n n -- narrow
C-x n w -- widen

;; :WHITESPACE-KEY-BINDINGS
SPC -- insert space
TAB -- indent or insert tab :SEE :INDENT-KEY-BINDINGS
C-q TAB -- insert literal tab character
C-q C-l -- insert page separator
C-q 0 RET -- insert null
M-SPC -- remove all whitespace at point except one space
M-x `delete-trailing-whitespace' -- remove at end of all lines in buffer
C-a M-x `delete-whitespace-rectangle' -- remove at beginning of all lines in region
C-x h M-x `delete-whitespace-rectangle' -- remove at beginning of all lines

;; :INDENT-KEY-BINDINGS
TAB -- line with mode-specific rules
C-i -- line with mode-specific rules
M-m -- go to indentation at beginning of line
M-s -- center line
C-M-\\ -- region with mode-specific rules
C-x h C-M-\\ -- buffer
M-h C-M-\\ -- paragraph
C-M-h C-M-\\ -- defun
C-x C-p C-M-\\ -- page
C-M-SPC C-M-\\ -- sexp
C-x TAB -- region by one column
C-u 5 C-x TAB -- region by 5 columns
C-u - 2 C-x TAB -- region by 2 columns less
M-x `tabify' -- convert spaces to tabs

;; :NEWLINE-KEY-BINDINGS
RET -- one
C-m -- one
C-j -- one and indent
C-o -- one below current and indent
C-M-o -- keep text following point at same column
C-u 3 RET -- three
C-u 3 C-m -- three
C-u 3 C-j -- three and indent
C-u 3 C-o -- three below current and indent
C-u 3 C-M-o -- move text following point at same column three lines down

;; :LINE-KEY-BINDINGS
C-n -- next
C-p -- previous
C-a -- beginning
M-g g -- goto
M-g M-g -- goto
C-e -- end
C-k -- kill to end
C-0 C-k -- kill to beginning
C-a C-k -- kill from beginning to end
C-S-DEL -- kill from beginning to end including newline
C-a C-k C-k -- same as previous
C-1 C-k -- kill to end including newline
C-u C-k -- kill next 4
C-2 C-k -- kill next 2
M-- C-k -- kill to beginning of previous
M-^ -- merge current line with previous
C-u M-^ -- merge next line with current
C-x C-o -- when not empty line, remove all empty lines below current
C-x C-o -- when only empty line, remove all empty lines
C-x C-o -- when empty, remove all but one empty lines
M-= -- count lines in region
M-x occur -- show lines matches in buffer :SEE :OCCUR-KEY-BINDINGS
M-x `count-matches' -- count matches
M-x `flush-lines' -- delete matching lines
M-x `keep-lines' -- keep matching lines delete the rest
M-x `what-line' -- display number
C-x RET f unix RET -- change file to UNIX style line endings
C-x RET f dos RET -- change file to DOS
C-x RET f dos RET -- change file to Mac
M-x `line-number-mode' -- show line number in mode line
M-x `toggle-truncate-lines' -- change if long lines fold or are truncated

;; :CHAR-KEY-BINDINGS
C-f -- forward
C-b -- backward
C-d -- delete
M-SPC -- remove all whitespace at point except one space
M-x `describe-char' -- properties
C-q 0 RET -- insert null
C-q 40 RET -- insert space using octal value 40
M-x set-variable RET read-quoted-char-radix 16 -- use hex for C-q
C-q 20 RET -- insert space using hex value 20
M-x `set-variable' RET read-quoted-char-radix 10 -- use decimal for C-q
C-q 32 RET -- insert space using decimal value 32
C-u 8 C-q 0 RET -- insert 8 null characters

;; :WORD-KEY-BINDINGS
M-f -- forward
M-b -- backward
M-d -- kill forward
C-DEL -- kill backward
M-DEL -- kill backward
M-t -- transpose
M-@ -- mark
C-u 100 M-@ -- mark next 100
M-- 3 M-@ -- mark previous 3

;; :CAPITALIZATION-KEY-BINDINGS
M-l -- lowercase next word
M-- M-l -- lowercase previous word
C-u M-l -- lowercase next 4 words
M-u -- uppercase next word
M-- M-u -- uppercase previous word
C-u 2 M-l -- uppercase next 2
M-c -- capitalize next
M-- M-c -- capitalize previous
C-u 2 M-c -- capitalize next 2
C-x C-l -- lowercase region
C-x C-u -- uppercase region
M-x `capitalize-region' -- capitalize region

;; :SENTENCE-KEY-BINDINGS
M-a -- beginning
M-e -- end
M-k -- kill

;; :PARAGRAPH-KEY-BINDINGS
M-} -- forward
M-{ -- backward
M-h -- mark
M-g -- fill
C-u M-g -- fill and full justify
M-x `fill-region' -- fill all in region
C-u 72 C-x f -- set fill column to 72
M-x `kill-paragraph' -- kill to end
M-{ M-x kill-paragraph -- kill
M-x `transpose-paragraphs' -- transpose
M-S -- center
M-x `sort-paragraphs' -- alphabetically
M-x `reverse-region' -- sort
M-x `paragraph-indent-text-mode' -- expect leading space rather than empty lines
M-x `auto-refill-mode' -- automatically fill at the end of the line
M-x `refill-mode' -- automatically fill entire paragraph after each edit

;; :PAGE-KEY-BINDINGS
C-q C-l -- insert separator
C-x ] -- forward page
C-x l -- count lines
C-x n p -- narrow
C-x n w -- widen
M-x `sort-pages' -- alphabetically
M-x `what-page' -- display number

;; :SEXP-AND-PAREN-KEY-BINDINGS
M-\( -- insert opening and closing parentheses
C-M-f -- move to the next
C-M-b -- move backward
C-M-d -- move down into the expression
M-x up-list -- move forward and up and outside the current
C-M-k -- kill
C-M-DEL -- kill backward
C-M-@ -- `mark' 
C-M-t -- `transpose-sexps' - transpose S-exprressions
M-x `check-parens' -- match all open and closed parentheses in buffer

;; :COMMENT-KEY-BINDINGS
C-u M-; -- kill
C-SPC -- set end point of region
M-x `comment-region' -- comment the region
M-x `uncomment-region' -- uncomment the region 
M-x `comment-kill' -- kill
C-x ; -- set comments to start at point
M-- C-x ; -- kill comment on this line
C-u C-x ; -- insert and align or just align to column of previous comment

;; :OCCUR-KEY-BINDINGS
M-x `occur' -- show regexp match in buffer
C-u 3 M-x occur -- show matches with 3 lines of context
C-u - 3 M-x occur -- show matches with 3 lines before match

;; :SPELL-CHECK-KEY-BINDINGS
M-$ -- word
M-x `ispell-buffer' -- buffer
M-x `ispell-region' -- region
M-x `ispell-comments-and-strings' -- words and comments in source file
q -- quit
M-x `ispell-continue' -- resume suspended session
M-r -- edit word at point in buffer with recursive edit
C-M-c -- return to spell check by exiting recursive edit
4 -- use third suggested choice
0 -- use first suggested choice
? -- quick help
SPC -- continue
a -- accept for this session
A -- add to buffer local dictionary
r -- replace word with typed version
R -- replace every occurrence of word with typed version
X -- suspend
M-x `ispell-change-dictionary' -- change default dictionary

;; :TRANSPOSE-KEY-BINDINGS
C-t -- characters :SEE :CHAR-KEY-BINDINGS 
M-- C-t -- previous with its previous
C-u 3 C-t -- forward 3 characters
C-u C-t -- forward 4 characters
M-- 3 C-t -- backward 3 characters
C-u C-t -- backward 4 characters
M-t -- words :SEE :WORD-KEY-BINDINGS 
C-x C-t -- lines :SEE :LINE-KEY-BINDINGS 
M-x `transpose-paragraphs' -- paragraphs :SEE :PARAGRAPH-KEY-BINDINGS
C-M-t -- parenthetical expressions

;; :COMPOSITION-KEY-BINDINGS
C-\\ france-postfix RET -- set to French characters
C-\\ -- disable input method, subsequent re-enables
C-h C-\\ RET -- help with current input method
e ' -- insert a letter E acute
e ' ' -- insert a letter E and a quote character
a ` -- insert a letter A grave
e ` -- insert a letter E grave
u ` -- insert a letter U grave
a ^ -- insert a letter A circumflex
e ^ -- insert a letter E circumflex
i ^ -- insert a letter I circumflex
o ^ -- insert a letter O circumflex
u ^ -- insert a letter U circumflex
c , -- insert a letter C with cedilla
c , , -- insert a letter C and comma
e \" -- insert a letter E umlaut
i \" -- insert a letter I umlaut
u \" -- insert a letter U umlaut
< < -- insert an open quotation mark
> > -- insert a closed quotation mark
C-x RET C-\\ spanish-postfix RET -- change to Spanish characters
i ` -- insert a letter I grave
o ` -- insert a letter O grave
n ~ -- insert a letter N with tilde
C-x RET C-\\ german-postfix RET -- change to German characters
a e -- insert a letter A umlaut
a e e -- insert the letters A and E, no umlaut
o e -- insert a letter O umlaut
o e e -- insert the letters O and E, no umlaut
u e -- insert a letter U umlaut
u e e -- insert the letters U and E, no umlaut
s z -- insert the ligature eszett
s z z -- insert the letters S and Z

;; :TEXT-REGISTER-KEY-BINDINGS
C-x r s a -- store region as \"a\"
C-x r i a -- insert region stored in \"a\"
C-x r r a -- store rectangle as \"a\"

;; :POINT-REGISTER-KEY-BINDINGS
C-x r SPC a -- store current as \"a\"
C-x r j a -- move to point in \"a\"

;; :WINDOW-REGISTER-KEY-BINDINGS
C-x r w a -- store configuration of windows in frame
C-x r j a -- restore window configurations

;; :FRAME-REGISTER-KEY-BINDINGS
C-x r f a -- store window configuration for all frames
C-x r j a -- restore all window configurations

;; :NUMBER-REGISTER-KEY-BINDINGS
C-u 1 C-x r n a -- store 1 in \"a\"
C-u 1 C-x r + a -- add 1 to number in \"a\"
C-x r i a -- insert number in \"a\"

;; :POSITION-REGISTER-KEY-BINDINGS
C-x r m RET -- save default
C-x r m <NAME> RET -- save as NAME
C-x r b RET -- move to default
C-x r b <NAME> RET -- move to NAME
C-x r l -- list
M-x bookmark-save -- save positions to file

;; :COLUMN-KEY-BINDINGS
M-x `column-number-mode' -- show column number in mode line
C-u C-x C-n -- set column for line motion commands
C-x C-n -- unset goal column for line motion commands
M-x `ruler-mode' -- add a ruler to the current buffer's window

;; :RECTANGLE-KEY-BINDINGS
C-x r d -- delete, no kill
C-x r k -- kill
C-x r y -- `yank'
C-x r c -- blank out
C-x r t <STRING> -- replace each line with STRING
M-x `string-insert-rectangle' -- insert STRING at each line
M-x `delete-whitespace-rectangle' -- remove leading whitespace
C-x r r a -- store to register \"a\"

;; :TABLE-KEY-BINDINGS
M-x `table-recognize-table' -- activate table at point
C-u M-x `table-recognize-table' -- inactivate table at point
C-u 3 `table-insert-column' -- insert 3 columns
C-u 3 `table-delete-column' -- delete 3 columns

;; :DELIMITED-TEXT-KEY-BINDINGS
M-x `delimit-columns-customize' -- change settings
M-x `delimit-columns-rectangle' -- format rectangle
M-x `delimit-columns-region' -- format region

;; :ALIGN-KEY-BINDINGS

;; :DIRECTORY-KEY-BINDINGS
M-x cd -- change working
M-x `make-directory' RET <PATH> RET -- make PATH including any missing parents
C-x d RET -- list current
C-x C-f RET -- same as previous
C-x d .. RET -- list parent
C-x C-f .. RET -- same as previous
C-x C-d RET ^ -- same as previous
C-x C-d RET C-x C-j -- same as previous
C-x d <PATH> RET -- list PATH
C-x C-f <PATH> RET -- same as previous
M-x `dired-jump' RET -- go to current file in the listing
j -- move to file in listing
g -- reread the listing
C-s -- search listings :SEE :SEARCH-KEY-BINDINGS 
RET -- open file or directory
+ -- add new
i -- show listing of current subdirectory
> -- skip between directory listings
< -- skip between directory listings
C-x C-f <FILE> RET -- visit FILE :SEE :FILE-KEY-BINDINGS 
M o+r RET -- make current file world-readable
G <GROUP> RET -- change current file to GROUP
O <OWNER> RET -- change current file to OWNER
R <FILE> RET -- move current file to FILE
C <FILE> RET -- copy current file to file
P RET -- send current file to default printer
P RET M-DEL a2ps -- print current file in Postscript
P SPC -P SPC <PRINTER> -- send current file to <PRINTER>
T -- touch current file
H <FILE> RET -- hardlink current file to FILE
S <FILE> RET -- symlink current file to FILE
Y <FILE RET -- relative symlink current file to FILE
^ -- list parent
C-x C-j -- list parent
m -- mark current file
u -- unmark current file
d -- mark current file for deletion
t -- toggle marks
U -- unmark all files
C -- copy marked files to another directory
R -- move marked files to another directory
M-x `wdired-change-to-wdired-mode' -- manually edit listing with WDired
C-c C-c -- Exit WDired and commit the edits made to the listing

;; :TRAMP-KEY-BINDINGS
C-x C-f /HOST:DIR/FILE -- open FILE in DIR on remote HOST
C-x C-f /scp:HOST:DIR/FILE -- same but use secure copy \(SCP\)
C-x C-f /ssh:HOST:DIR/FILE -- same but demand the use of SSH
C-x C-f /ssh1:HOST:DIR/FILE -- same but demand version 1 of SSH
C-x C-f /HOST:DIR -- list contents of DIR on remote HOST
C-x d /HOST:DIR -- same as previous

;; :SHELL-KEY-BINDINGS
M-x `shell' -- new window
C-u M-x shell -- prompt for buffer name of new window
M-! -- run command
C-u M-! -- insert output of command
C-SPC -- set end point of region
M-| -- send region to command
C-u -- replace region with output of command
C-x C-w <FILE> RET -- save session transcript to FILE

;; :SHELL-SCRIPT-KEY-BINDINGS
C-x C-f file.sh RET -- start a script named file.sh
M-x `shell-script-mode' -- use shell script mode for current buffer
M-x `sh-mode' -- same as previous
C-c : -- specify shell and insert header
C-c C-x -- run the script
C-M-x -- execute region
M-a -- beginning of command
M-e -- end of command
C-M-a -- beginning of function
C-M-e -- end of function
TAB -- indent
C-j -- newline and indent
C-c < -- use indentation level of current line
C-c > -- analyze buffer's indentation and show inconsistencies
C-c = -- set indentation level for syntactic type at point
C-c ? -- show indentation level at point
C-c TAB -- insert if statement
C-c C-f -- insert for statement
C-c C-c -- insert case statement
C-c C-t -- insert syntax for temporary file
C-c \( -- insert syntax for function

;; :COMPILE-KEY-BINDINGS
M-x `compile' -- execute a compilation command
M-x `recompile' -- execute last compilation command
C-c C-k -- kill
RET -- go to source code for error specified at point
C-c C-c -- same as previous
M-n -- next error
M-p -- previous error
M-} -- errors for next file
M-{ -- errors for previous file
C-x ` -- go to source code for next error

;; :LISP-KEY-BINDINGS
C-M-a -- beginning of defun
C-M-e -- end of defun
C-M-n -- forward sexp
C-M-p -- backward sexp
C-M-u -- upward sexp
C-M-d -- down sexp
M-x `up-list' -- upward sexp and forward
M-\( -- insert parens for sexp
C-m-t -- transpose sexp
C-M-SPC -- mark sexp
C-c C-z -- run interpreter
C-M-x -- eval expression at point
M-; -- insert new comment
C-u M-; -- kill current comment

;; :EMACS-LISP-KEY-BINDINGS
C-x C-e -- evaluate expression before point
C-u C-x C-e -- evaluate expression and insert result at point
C-M-x -- evaluate current defun
M-: -- prompt for expression then evaluate
C-u M-: -- eval expression and insert result at point
TAB -- indent
C-M-q -- indent expression after point
M-TAB -- complete symbol at point
M-x `eval-region' -- evaluate expressions in region
M-x `eval-buffer' -- evaluate buffer
M-x `load-file' RET <FILE> RET -- load FILE
M-x `load-file' RET RET -- load current file
M-x `load-libary' -- load library
M-x `byte-compile-file' RET <FILE> RET -- byte compile current FILE
M-x `byte-compile-file' RET RET -- same as previous
M-x `byte-recompile-directory' RET -- byte compile every file, recursively
M-x `find-function' RET <FUNCTION> RET -- go to definition of FUNCTION
M-x `find-function' RET RET -- go to definition of function at point
M-x `find-variable' RET <VARIABLE> RET -- go to definition of VARIABLE
M-x `find-variable' RET RET -- go to definition of variable at point
M-x `find-library' <LIBRARY> -- go to LIBRARY
M-x `emacs-lisp-mode' -- start Emacs Lisp mode if not started
M-x `checkdoc' -- validate coding style
M-x `checkdoc-ispell' -- and spell check comments and documentation strings
M-x `set-variable' RET debug-on-error RET t RET -- enable debugger on error
M-x `set-variable' RET debug-on-quit RET t RET -- enable debugger on quit
M-x `set-variable' RET debug-on-error RET nil RET -- disable debugger on error
M-x `set-variable' RET debug-on-quit RET nil RET -- enable debugger on quit

;; :ELISP-INTERACTION-KEY-BINDINGS
M-x `lisp-interaction-mode' -- evaluate expressions interactively
C-j -- evaluate sexp before point and insert results on next line
C-M-x -- evaluate current defun :SEE :EMACS-LISP-KEY-BINDINGS 

;; :ELISP-DEBUG-KEY-BINDINGS
M-x `toggle-debug-on-error' -- change whether to start session on error
M-x `toggle-debug-on-quit' -- change whether C-g starts session
M-x `debug-on-entry' RET <FUNCTION> RET -- debug FUNCTION
h -- help
SPC -- move down
C-n -- move down
5 SPC -- move 5 down
- 2 SPC -- move 2 up
C-p -- move up
2 C-p -- backward 2 lines
TAB -- go up level in expression
S-TAB -- go down lower level
RET -- visit help or source location for thing at point
c -- complete evaluation level at current point
q -- quit
d -- step into
b -- set breakpoint
u -- unset breakpoint
j -- set breakpoint and continue
r -- prompt for return value then continue
e -- prompt for expression then evaluate
R -- prompt for expression then evaluate and record it
l -- list functions debugged on entry
M-x `cancel-debug-on-entry' <FUNCTION> RET -- don't debug FUNCTION
M-x cancel-debug-on-entry RET RET -- don't debug for any function

;; :SOURCE-LEVEL-DEBUGGER-KEY-BINDINGS
M-x `edebug-defun' -- turn on instrumentation for current function definition
C-u C-M-x -- same as previous
C-M-x -- turn off instrumentation for current function definition
SPC -- step expression in source
C-x X SPC -- from any buffer, step into expression in source
t -- slowly step
T -- step fast
S -- stop stepping
n -- step to next expression
i -- step in
o -- step out
f -- step forward
r -- show last result again in minibuffer
b -- set breakpoint
u -- unset breakpoint
C-c C-d -- unset breakpoint
x <EXPRESSION> RET -- set conditional break on result of EXPRESSION
B -- move to next breakpoint
g -- continue until next breakpoint
B -- continue to next breakpoint
c -- continue to breakpoints slowly
C -- continue to breakpoints fast
S -- stop continuing
G -- stop debugging and finish
P -- visit buffer before running Edebug
v -- visit buffer before running Edebug
p -- momentarily visit buffer before running Edebug
w -- move back to current point in source
C-c C-l -- move back to current point in source
C-x X w -- from any buffer, move back to current point in source
? -- help
e -- prompt for expression then evaluate
d -- show backtrace
= -- display frequencies in comments for each line for current function
a -- abort
C-] -- abort
q -- quit
Q -- quit

;; :FINDER-KEY-BINDINGS
C-h P -- list keywords
M-x `finder-commentary' RET <LIBRARY> RET -- Describe LIBRARY
? -- help
n -- move down
p -- move up
RET -- for keyword at point, list Emacs Lisp libraries
RET -- for package at point, show commentary for Emacs
f -- same as previous
SPC -- same as previous
d -- back to beginning of package directory
q -- quit

;; :C-KEY-BINDINGS

;; :ETAGS-KEY-BINDINGS
M-! etags *.[ch] -- index .c and .h files in current directory
C-u M-x `visit-tags-table' -- set index file for current buffer
M-x `visit-tags-table' -- globally set index file
M-. -- go to definition of symbol in index
C-M-. -- go to definition for a regular expression in index
C-u M-. -- go to next definition
M-- M-. -- go to previous definition
M-* -- return back to before you started
M-x `tags-search' -- go to entry for regular expression in index
M-, -- go to next entry in index
M-x `tags-query-replace' -- search and replace for regular expression
M-TAB -- complete tag at point
C-u M-TAB -- complete language symbol, avoid tags, at point

;; :GDB-KEY-BINDINGS

;; :DIFF-KEY-BINDINGS
M-x `diff' RET <OLD> RET <NEW> RET -- compare OLD file with NEW file
M-x diff RET RET -- same as previous
C-u M-x diff -- compare files but prompt for Diff switches
M-x `diff-buffer-with-file' -- compare buffer with file on disk
M-x `diff-backup' -- compare current file with backup on disk
M-x `diff-mode' -- start Diff Mode if not already started for a file
C-c C-c -- go to corresponding location in target \(new\) file
C-u C-c C-c -- go to corresponding location in source \(old\) file
C-u C-u C-c C-c -- always go to corresponding location in source file
C-c C-a -- apply current hunk
C-u C-c C-a -- revert current hunk
C-c C-t -- test current hunk
C-c C-t -- test current hunk in reverse
M-n -- move start of next hunk
M-p -- move to start of previous hunk
M-} -- move to start of next file in multiple file patch
M-{ -- move to start of previous file in multiple file patch
C-c C-n -- narrow to hunk
C-x n w -- widen
C-u C-c C-n -- narrow to file of multiple file patch
M-k -- kill the current hunk
M-K -- kill the current file in multiple file patch
C-c C-s -- split the hunk in two
C-c C-r -- reverse direction of entire patch
C-u C-c C-r -- reverse direction of patch in region
C-x 4 a -- new change log entry using context of current location
C-c C-u -- convert the entire buffer from unified to context format
C-u C-c C-u -- convert the entire buffer from context to unified format
C-c C-u -- convert the entire buffer
C-c C-e -- start ediff session

;; :VC-VERSION-CONTROL-KEY-BINDINGS
C-x v i -- register file
C-x v v -- check in or out, depending on the current state
C-c C-c -- finish editing log for check in
C-u C-x v v -- check in or out a specific revision
C-x v ~ -- open past revision in new window
C-x v = -- diff with current revision
C-u C-x v = -- diff with specific revision
C-x v l -- show log
C-x v u -- undo checkout
C-x v c -- delete the latest revision
C-x v g -- annotate file by each line showing when added and by whom
C-x v d -- show checked out files
C-x v s RET <NAME> RET -- tag all the files in directory with NAME
C-u C-x v s RET <NAME> RET -- tag files and create branch
C-x v r <NAME> -- recursively checkout files for a snapshot
C-x v a -- update ChangeLog :SEE :CHANGELOG-KEY-BINDINGS
C-x v m -- merge two revisions
C-x v h -- insert revision header keyword
M-x `vc-resolve-conflicts' -- start ediff-merge session on a file with conflict markers

;; :CHANGELOG-KEY-BINDINGS
C-x 4 a -- start new entry using context of current file
C-x 4 a -- start new entry in current log file
C-c C-p -- insert previous log from version control
M-q -- fill paragraph following syntax rules
M-x `change-log-merge' RET <FILE> RET -- merge current with log FILE
C-x v a -- generate entries from version control

;; :MERGE-CONFLICT-KEY-BINDINGS
M-x `smerge-mode' -- start Smerge Mode if not started
C-c ^ n -- move to next
C-c ^ p -- move to previous
C-c ^ b -- keep base
C-c ^ m -- keep mine
C-c ^ o -- keep other
C-c ^ RET -- keep what is under point
C-c ^ a -- keep all
C-c ^ c -- combine current with next
C-c ^ r -- auto resolve
M-x `smerge-resolve-all' -- auto resolve entire buffer

;; :GREP-KEY-BINDINGS
M-x `grep'  RET <REGEXP> SPC <FILES> RET -- show matches in FILES for REGEXP
M-x `lgrep' RET <REGEXP> RET <FILES> RET -- show matches in FILES for REGEXP
M-x lgrep RET <REGEXP> RET RET -- show matches in all C files
M-x lgrep RET <REGEXP> RET ch RET -- same as previous
M-x lgrep RET <REGEXP> RET c RET -- show matches in C source files
M-x lgrep RET <REGEXP> RET h RET -- show matches in header files
M-x lgrep RET <REGEXP> RET l RET -- show matches in ChangeLog files
M-x lgrep RET <REGEXP> RET m RET -- show matches in Make files
M-x lgrep RET <REGEXP> RET tex RET -- show matches in TeX files
M-x lgrep RET <REGEXP> RET *.html RET -- show matches in HTML files
M-x `egrep' RET <REGEXP> RET <FILES> RET -- extended regular expressions
M-x `igrep' RET <REGEXP> RET <FILES> RET -- case insensitive matching
M-x `grep-find' RET <REGEXP> RET -- show matches in entire directory tree
M-x `rgrep' RET <REGEXP> RET *.html RET RET -- same, but HTML files
M-x rgrep RET <REGEXP> RET RET RET -- same, but C files
M-x rgrep RET <REGEXP> RET el RET RET -- same, but Emacs Lisp files

;; :LOCATE-KEY-BINDINGS
M-x `locate' RET <PATTERN> RET -- show files matching PATTERN
M-x `locate-with-filter' RET <PATTERN> RET <REGEXP> -- same, but also match REGEXP
M-x locate-with-filter -- show
C-n -- next matched file
C-p -- previous matched file
RET -- visit current file at
C-o -- open file in other window
V -- open current file in dired :SEE :DIRECTORY-KEY-BINDINGS 

;; :CALENDAR-KEY-BINDINGS

;; :DIARY-KEY-BINDINGS

;; :ABBREV-KEY-BINDINGS

;; :DABBREV-KEY-BINDINGS

;; :AUTOINSERT-KEY-BINDINGS

;; :EDIFF-KEY-BINDINGS

;; :EMERGE-KEY-BINDINGS

;; :SORT-KEY-BINDINGS

;; :BROWSE-URL-KEY-BINDINGS

;; :EMAIL-KEY-BINDINGS

;; :HTML-KEY-BINDINGS
C-h m -- help
C-c C-v -- view current file in Web browser
C-c C-s -- toggle to view in Web browser on each save
C-c 8 -- toggle inserting of non-ASCII characters as entities
C-c TAB -- toggle invisibility of tags
M-x `html-imenu-index' -- add index menu to menu bar for current file
M-x `set-variable' RET `sgml-xml-mode' RET t -- turn on XHTML tags
C-c C-d -- delete current tag
C-c DEL -- delete current tag
C-u C-c C-d -- delete next 4 tags
C-c C-f -- skip forward tag
C-u 5 C-c C-f -- skip forward 5 tags
C-c C-f -- skip backward tag
C-u C-c C-f -- skip backward 4 tags
C-c C-t html RET <TITLE>  -- start file with TITLE
C-c 1 -- insert level one heading
C-c 2 -- insert level two heading
C-c 3 -- insert level three heading
C-c 4 -- insert level four heading
C-c 5 -- insert level five heading
C-c 6 -- insert level six heading
C-c RET -- insert paragraph tag
C-c / -- close paragraph tag
C-c C-j -- insert line break tag
C-c C-c - -- insert horizontal rule
C-c C-c h -- insert link
C-c C-c n -- insert page anchor
C-c C-c i -- insert image
C-c C-c o -- insert ordered list
C-c C-c u -- insert unordered list
C-c C-c i -- insert list-item
C-u M-o b -- insert bold tag
C-u M-o i -- insert italic tag
C-u M-o b -- insert bold tag
C-u M-o i -- insert italic tag
C-u M-o i -- insert underline tag
C-c C-a -- insert attributes to current tag
C-c C-t em RET -- insert emphasis tag
C-c C-t strong RET -- insert strong emphasis tag
C-c C-t code RET -- insert source code tag
C-c C-t dfn RET -- insert source code tag
C-c C-t kbd RET -- insert keyboard text tag
C-c C-t samp RET -- insert sample text tag
C-c C-t var RET -- insert sample text tag
C-c C-t pre RET -- insert preformatted text tag
C-c C-t span RET class RET <CLASS> RET -- insert span tag for text of CLASS
C-c C-t dl RET <TERM> RET RET -- insert definition list with TERM
C-c C-t table RET h RET d RET DEL RET -- insert 1-by-1 table
C-c C-t -- prompt for tag name and possible attributes, then insert
C-u 3 C-c C-t -- prompt for tag, and surround next 3 words with tag
C-1 C-c C-t -- prompt for tag, and surround next word with tag
M-- C-c C-t -- prompt for tag, and surround region with tag
C-c ? RET -- describe current tag
C-c C-n M-SPC -- insert non-breaking space entity
M-; -- insert comment
C-u M-; -- kill comment
M-x `sgml-show-context' -- display hierarchy of tags for point
M-x `sgml-validate' -- check markup with external tool

;; :OUTLINE-KEY-BINDINGS

;; :SQL-KEY-BINDINGS

;; :CALC-KEY-BINDINGS

;; :TIMECLOCK-KEY-BINDINGS
M-x `timeclock-in' -- start a project
M-x `timeclock-out' -- stop working on the current project
M-x `timeclock-when-to-leave-string' -- report time to leave
M-x `timeclock-visit-timelog' -- visit timelog file
M-x `timeclock-reread-log' -- reread timelog file after crash
M-x timeclock-reread-log -- reread timelog file after edited
M-x timeclock-reread-log -- reread timelog file after restarting emacs

;; :GAMES-KEY-BINDINGS
M-x `5x5' -- fill a 5-by-5 grid
M-x `blackbox' -- find balls in a box
M-x `doctor' -- psychoanalysis
M-x `dunnet' -- text adventure
M-x `gomoku' -- try to get 5 in a row
M-x `mpuz' -- multiplication puzzle
M-x `pong' -- classic video tennis
M-x `snake' -- eat dots but not yourself or the walls
M-x `tetris' -- stack blocks
M-x `type-break-mode' -- be told when to take breaks

;; :ANIMATION-KEY-BINDINGS
M-x `animate-birthday-present' RET <NAME> RET -- Birthday wishes to NAME
M-x `butterfly-mode' -- strike the drive platter and flip the desired bit
M-x `dissociated-press' -- scramble current text in another buffer
M-x `hanoi' -- towers of hanoi
M-x `life' -- the game of life
M-x `studlify-buffer' -- give text in buffer strange capitalization
M-x `studlify-region' -- give text in region strange capitalization
M-x `zone' -- display text tricks

#:END:REFERENCE-SHEET#")) ; :CLOSE unless
;;
;;; :TEST-ME (symbol-value '*mon-help-reference-keys*)
;;
;;;(progn (makunbound '*mon-help-reference-keys*)
;;;       (unintern "*mon-help-reference-keys*" obarray)
;;;       (makunbound '*reference-sheet-help-A-HAWLEY*)
;;;       (unintern "*reference-sheet-help-A-HAWLEY*" obarray) )

;; (assoc 'meta-tags *mon-help-mon-tags-alist*)

;;; ==============================

 
;;; ==============================
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T13:24:05-04:00Z}#{10361} - by MON KEY>
(defun mon-help-bind-help-keys-loadtime (&optional w-msg-user)
  "Loadtime function to build docs for `mon-help-keys' and `*mon-help-reference-keys*'.\n
Create function-documentation with `*mon-help-reference-keys*'s variable-documentation.\n
:SEE-ALSO .\n►►►"
  (let ((self-puke *mon-help-reference-keys*))
    (dolist (i '(("^#:START:REFERENCE-SHEET#$" . "")
                 ("^#:END:REFERENCE-SHEET#$" . "")))
      (setq self-puke (replace-regexp-in-string (car i) (cdr i) self-puke)))
    (setq self-puke
	  (concat
	   "*Other Emacs reference sheets fit on conveniently sized cards. This one won't.\n"
	   "Instead it tries to tell you everything about doing things in Emacs.\n"
	   ;; :WAS (symbol-value '*mon-help-reference-keys*)
	   self-puke
	   ;; :WAS *mon-help-reference-keys*
	   "\n;; :TAGS-MON-LOCAL-FOR-KEY-BINDINGS\n\n"
	   (mon-string-justify-left 
	    (mapconcat #'identity 
		       (cadr (assoc 'meta-tags-keybindings *mon-help-mon-tags-alist*)) " ")
	    68 2) 
	   "\n\n:NOTE Use with `mon-help-keys-wikify' for rapid EmacsWiki-fication.\n" 
	   "The list above can be kept properly escaped by evaluating:\n"
	   "`mon-help-escape-for-ewiki' and `mon-help-unescape-for-ewiki' respectively.\n\n"
	   ":SEE \(URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley')\n"
	   ":SEE \(URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source'\).\n\n"
	   ":SEE-ALSO `mon-help-keys', `mon-help-key-functions', `*mon-help-reference-keys*',\n"
	   "`*mon-help-reference-keywords*'.\n►►►"))
    (when (null (get '*mon-help-reference-keys* 'variable-documentation))
      (if (atom w-msg-user)
          (setq w-msg-user (list '*mon-help-reference-keys*))
        (push '*mon-help-reference-keys* w-msg-user))
      (put '*mon-help-reference-keys* 'variable-documentation self-puke)))
  (let ((mhk-put (plist-get (symbol-plist '*mon-help-reference-keys*) 'variable-documentation)))
    ;; Knock of the leading var `*'.
    (setq mhk-put (substring mhk-put 1))
    (setq mhk-put (replace-regexp-in-string " `mon-help-keys',?" "" mhk-put))
    (when (null (get 'mon-help-keys 'function-documentation))
      (if (atom w-msg-user)
          (setq w-msg-user (list 'mon-help-keys))
        (push 'mon-help-keys w-msg-user)))
    (put 'mon-help-keys 'function-documentation mhk-put)
    (if w-msg-user
        (message (concat ":FUNCTION `mon-help-bind-help-keys-loadtime' "
                         "-- docstrings for symbols "
                         (if (consp w-msg-user)
                             (mapconcat #'(lambda (ldtm) 
                                            (format "`%s'" (symbol-name ldtm)))
                                        (setq w-msg-user (delq t w-msg-user)) " ")
                           "`*mon-help-reference-keys*' and `mon-help-keys'")
                         " bound")))))
;;
;;; (progn (put '*mon-help-reference-keys* 'variable-documentation nil)
;;;         (put 'mon-help-keys 'function-documentation nil))
;;
;;; :TEST-ME (mon-help-bind-help-keys-loadtime t)
;;; :TEST-ME (mon-help-bind-help-keys-loadtime)
;;
;;; :TEST-ME (describe-variable '*mon-help-reference-keys*)
;;; :TEST-ME (describe-function 'mon-help-keys)

 
;;; ==============================
;;; Function documents itself by snarfing from the variable-documentation
;;; property value of `*mon-help-reference-keys*' and putting that
;;; as the value for its own function-documentation prop.
;;; :CREATED <Timestamp: Thursday July 02, 2009 @ 05:59.17 PM - by MON KEY>
(defun mon-help-keys (&optional insertp intrp)
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-keys :insertp t)
    (mon-help-message-intrp "mon-help-keys")))
;;
;;; :TEST-ME (mon-help-keys t)
;;; :TEST-ME (mon-help-keys nil t)
;;; :TEST-ME (describe-function 'mon-help-keys)
;;; :TEST-ME (apply 'mon-help-keys nil '(t))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-26T20:17:09-05:00Z}#{10086} - by MON KEY>
(defun mon-help-keys-wikify-anchors ()
  "Replace semicolon prefixed upcased keywords in buffer with eWiki anchored style.\n
Replaces the following:\n
  \"^;; :SOME-KEYWORD\"  -> \"||||'''[::SOME-KEYWORD]'''||\"
  \":SEE :SOME-KEYWORD\" -> \(See [[SOME-KEYWORD]]\)\n
Returns a list of the form:\n
 \(\(\":ANIMATION-KEY-BINDINGS\" \"[[ANIMATION-KEY-BINDINGS]]\"\) 
   { ... }
  \(\":TIMECLOCK-KEY-BINDINGS\" \"[[TIMECLOCK-KEY-BINDINGS]]\"\)\)\n
List is suitable for building xrefs and TOC's on emacs-wiki.\n
:NOTE These regexps create permanent visible anchors on the e-wiki. To prevent
erroneous instantiation of anchors you should map the car of return value
against symbols in the alist key `mon-help-keys-keywords` of
`*mon-help-reference-keywords*' to verify set union and xref consistency.\n
:CALLED-BY `mon-help-keys-wikify'\n
:SEE-ALSO `mon-help-keys-wikify', `mon-get-next-face-property-change',
`*mon-help-reference-keys*', `mon-help-escape-for-ewiki',
`mon-help-keys-wikify-anchors' `mon-help-unescape-for-ewiki', `mon-help-keys',
`mon-help-key-functions'.\n►►►"
  ;; :NOTE In loop below following regexp/replace pair works fine locally:
  ;;  ("^;; :\\([A-Z-]+\\)" . "||||'''[::" anch "]'''||")
  ;; However, calling fncn `mon-help-keys-wikify' has a regexp/replace pair:
  ;;  ("^\\(.+?\\)\\( -- \\(.*\\)\\)?$" . "||##\\1##||\\3||")
  ;; which clobbers the match pattern above. For our purposes here it is
  ;; easier to accomodate the clobbering of the latter than to pre-empt
  ;; it. What this means is we wind up performing an extra match/replace
  ;; loop over the entire set. Once with the no-op replacement in
  ;; `mon-help-keys-wikify' e.g the regexp/replace pair:
  ;;  ("^;; \\(:[A-Z-]+\\)" . ";; \\1") 
  ;; And a second time here to normalize damage done by the regexp/replace pair:
  ;;  ("^\\(.+?\\)\\( -- \\(.*\\)\\)?$" . "||##\\1##||\\3||")
  (let (mhrkk anch-ls)  
    (save-excursion
      (mon-g2be -1)
      (while (search-forward-regexp "^||##;; :\\([A-Z-]+\\)##||||" nil t)
        (let ((anch (match-string 1)))
          (progn (replace-match (concat "||||'''[::" anch "]'''||"))
                 (push  `(,(concat ":" anch)
                          ,(concat "[::" anch "]")
                          ,(concat "[[" anch "]]")) anch-ls))))
      (mapc #'(lambda (q)
                (push `(,(format " :SEE %s" q)
                         ,(format " (See [[%s]])" (substring (format "%s" q) 1)))
                      mhrkk))
            (cadr (assq 'mon-help-keys-keywords *mon-help-reference-keywords*)))
      (setq mhrkk (nreverse mhrkk))
      (let ((case-fold-search nil))
        (dolist (see mhrkk)
          (mon-g2be -1)
          (while (search-forward-regexp (car see) nil t)
            (replace-match (cadr see) t))))
      anch-ls)))
;;
;;;  Manual:show-paren-mode
;;; [:HiddenAnchors] -> [[#HiddenAnchors]]

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-27T14:04:42-05:00Z}#{10086} - by MON KEY>
(defun mon-help-keys-wikify-heading (wikified-list heading-level heading-title)
  "Return a heading title and list of keys.
Return value is added to the head of list returned from `mon-help-keys-wikify'.
WIKIFIED-LIST is a list of the form:\n
 (\"string\"\n
HEADING-LEVEL a number of the heading level depth to retrun.
HEADING-TITLE a string used for the title of the return heading.\n
:EXAMPLE\n\n(let (mhkwh `(,(
\(mon-help-keys-wikify-heading 2 \"Keybinding Table Sections\"\)\n
:SEE-ALSO :SEE-ALSO `mon-help-unescape-for-ewiki', `mon-help-keys-wikify'
`*mon-help-reference-keys*', `*mon-help-reference-keywords*'.\n►►►"
  (let* ((wk-lst wikified-list)
         (lvl (if (> heading-level 4) ;; Clamp it to at most 4 levels deep.
                  (make-string 4 61) 
                  (make-string heading-level 61)))
         (hdng (concat lvl heading-title lvl "\n"))
         (key-lst (mapconcat #'(lambda (anchr-l)
                                 (concat "* " anchr-l))
                             (mapcar 'caddr (cadr wk-lst))
                             "\n")))
    (setq key-lst `(,(concat hdng key-lst)
                      ,(car wk-lst)
                      ,(cadr wk-lst)))))

;;; ==============================
;;; :COURTESY Aaaron Hawley
;;; :SEE (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
;;; :MODIFICATIONS <Timestamp: Wednesday June 17, 2009 @ 11:31.47 AM - by MON KEY>
(defun mon-help-keys-wikify (&optional use-var insrtp intrp)
  "Return content between delimiters wikified for insertion/update to EmacsWiki.\n
Content should be delimited at top and bottom by:\n
#:START:REFERENCE-SHEET#\n
 { ... Reference-Sheet-by-Aaron-Hawley ... }\n
#:END:REFERENCE-SHEET#\n
When optional arg USE-VAR is non-nil wikifiy `*mon-help-reference-keys*' content
instead of contents of buffer from point.\n
When optional arg INSRTP is non-nil or called-interactively insert wikified
reference sheet at point replacing the existing region if found. When either
INSRTP or INTRP is non-nil and the delimiter can't be found use value of
`*mon-help-reference-keys*'.\n
Return value is a three elt list with the form:\n
\(\"== Some Appropiate Heading == 
  * [[SOME-KEYWORD]]
    { ... * <ANCHOR-XREF> ... }
  * [[SOME-KEYWORD-N]]\"
 \" { ... Big wikified Table sectioned by <VISIBLE-ANCHOR> ... } \"
 \(\":SOME-KEYWORD\" \"[::SOME-KEYWORD]\" \"[[SOME-KEYWORD]]\"\)
   { ... \(<KEY> <VISIBLE-ANCHOR> <ANCHOR-XREF>\) ... }
 \(\":SOME-KEYWORD-N\" \"[::SOME-KEYWORD-N]\" \"[[SOME-KEYWORD-N]]\"\)\)\n
:EXAMPLE\n\n\(mon-help-keys-wikify-TEST\)\n
:SEE (URL `http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley_source')
:SEE (URL `http://www.emacswiki.org/emacs/PermanentAnchors')\n
:SEE-ALSO `mon-help-keys-wikify-anchors', `mon-help-keys-wikify-heading',
`*mon-help-reference-keywords*', `mon-get-next-face-property-change',
`mon-help-escape-for-ewiki', `mon-help-unescape-for-ewiki', `mon-help-keys',
`mon-help-key-functions'.\n►►►"
  (interactive "i\ni\np")
  (let (wikify-this start-at end-at in-bfr accum-anchors shw-msg)
    (save-excursion
      (unless use-var
        (search-forward-regexp "^\\(#:START:REFERENCE-SHEET#\\)$" nil t)
        (if (match-beginning 1)
            (setq start-at (match-beginning 1))
            (progn 
              (search-backward-regexp "^\\(#:START:REFERENCE-SHEET#\\)$" nil t)
              (if (match-beginning 1)
                  (setq start-at (match-beginning 1))
                  (setq use-var t)))))
      (unless use-var 
        (search-forward-regexp "^\\(#:END:REFERENCE-SHEET#\\)$" nil t)
        (if (match-beginning 1)
            (setq end-at (match-end 1))
            (progn 
              (search-backward-regexp "^\\(#:END:REFERENCE-SHEET#\\)$" nil t)
              (if (match-beginning 1)
                  (setq end-at (match-end 1))
                  (setq use-var t))))))
    (cond ((and (not use-var) start-at end-at)
           (setq wikify-this (mon-buffer-sub-no-prop start-at end-at)))
          ((and (or insrtp intrp) (not start-at) (not end-at))
           (setq shw-msg
                 (concat
                  ":FUNCTION `mon-help-keys-wikify' -- " (if intrp " INTRP " " INSRTP ")
                  "didn't match delimited region used USE-VAR value `*mon-help-reference-keys*'"))))
    (setq in-bfr 
          ;;(with-temp-buffer
          (with-current-buffer (get-buffer-create "*TEST-MDHU*")
            (erase-buffer)
            (save-excursion
                     (if use-var
                         ;;(insert *mon-help-reference-keys*)
                         ;;(insert wikify-this)))
                         (prin1 *mon-help-reference-keys* (current-buffer))
                         (prin1 wikify-this (current-buffer))))
            ;; Parse buffer contents as a string in `emacs-lisp-mode' first to get at the 
            ;; font-lock-constant-face props with `mon-get-next-face-property-change'
            ;; Add props to get them back out post regex looping. We need to add the ewiki 
            ;; Manual:show-paren-mode links and it won't be pretty if we're not in the right
            ;; syntax-mode and properly font-locked.
            ;; grab the buffer contents to a var and regurgitate them back in with `princ'.
            ;;(emacs-lisp-mode) ;; (when (equal mode-name "Emacs-Lisp") (fundamental-mode))
            ;; (mon-get-next-face-property-change 'font-lock-constant-face from-posn)
            ;; (while (plist-conses
            ;; (add-text-properties 
            (let ((regexp-replace-list
                          '(("^\\(\"?\\)\\(#:START:REFERENCE-SHEET#\\)" . "")
                            ("^\\(#:END:REFERENCE-SHEET#\\)\\(\"?\\)" . "")
                            ;; :WAS ("^\\(.*\\):$" . "||||**\\1**||")
                            ;; :NOTE the new match is a kludge. we replace just to stay in loop.
                            ("^;; \\(:[A-Z-]+\\)" . ";; \\1")
                            ("^\\(.+?\\)\\( -- \\(.*\\)\\)?$" . "||##\\1##||\\3||")
                            ("^\n" . ""))))
                     ;;(flush-lines "^;")
                     ;;(regexpl-search-replace-list regexp-replace-list))
                     (dolist (rrl regexp-replace-list)
                       (mon-g2be -1)
                       (while (search-forward-regexp (car rrl) nil t)
                         (replace-match (cdr rrl)))))    
                   (setq accum-anchors (mon-help-keys-wikify-anchors))
                   ;;(buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
                   ;; :NOTE don't clobber the text-props we have stuff on them.
                   ;; :WAS (buffer-substring (buffer-end 0) (buffer-end 1)) ))
                   (mon-buffer-sub-no-prop )))
    (setq in-bfr 
          (mon-help-keys-wikify-heading `(,in-bfr ,accum-anchors) 2 "Keybinding Table Sections"))
    (if (or intrp insrtp)
        (progn
          (when wikify-this (delete-region start-at end-at))
          (save-excursion
            (newline)
            (princ (concat (car in-bfr) "\n" (cadr in-bfr)) (current-buffer))
            (newline))
          (if shw-msg ;;(let ((warning-suppress-types '(mon-doc-help-utils)))
              (display-warning 'mon-doc-help-utils shw-msg :debug
                               (get-buffer-create "*MON-HELP-KEYS-WIKIFY*")))) ;)
        in-bfr)))
;;
;;; :TEST-ME (mon-help-keys-wikify t t)
;;
;; (search-forward-regexp "^\\(.+?\\)\\( -- \\(.*\\)\\)?$")
;; (search-forward-regexp "\\(|\\{4\\}'\\{3\\}\\[::.*]'\\{3\\}|\\{2\\}\\)")



;;; ==============================
;;; :NOTE Maybe MON remembers stealing this from Pascal Bourguignon?
;;; :MODIFICATIONS <Timestamp: Saturday May 30, 2009 @ 06:26.12 PM - by MON KEY>
(defun mon-help-escape-for-ewiki (&optional start end ref-sheet)
  "Escape special characters in the region as if a Lisp string.\n
Inserts backslashes in front of special characters \(namely `\\' backslash, `\"'
double quote, and `\(' `\)' parens in the region, according to the docstring
escape requirements.\n
Don't expect good results evaluation this form on strings with regexps.\n
:NOTE region should only contain the characters actually comprising the string
supplied without the surrounding quotes.\n
:SEE-ALSO `mon-help-unescape-for-ewiki', `mon-help-keys-wikify'
`*mon-help-reference-keys*', `*mon-help-reference-keywords*'.\n►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (let (ref-start ref-end)
	(if ref-sheet
	   (progn
	     (search-forward-regexp "^\\(#:START:REFERENCE-SHEET#\\)$" nil t)
	     (setq ref-start  (match-beginning 1))
	     (search-forward-regexp "^\\(#:END:REFERENCE-SHEET#\\)$" nil t)
	     (setq ref-end  (match-beginning 1)))
	  (progn
	    (setq ref-start start)
	    (setq ref-end end)))
      (narrow-to-region ref-start ref-end)
      (goto-char ref-start)
      (while (search-forward "\\" nil t)
	(replace-match "\\\\" nil t))
      (goto-char ref-start)
      (while (search-forward "\"" nil t)
	(replace-match "\\\"" nil t))
      ;;MON KEY additions
      (goto-char ref-start)
      (while (search-forward "(" nil t)
	(replace-match "\\\(" nil t))
      (goto-char ref-start)
      (while (search-forward ")" nil t)
	(replace-match "\\\)" nil t))))))
;;
;;; :TEST-ME
;;; (save-excursion (let ((this-point (point)))
;;; (newline)(princ *mon-help-reference-keys* (current-buffer))
;;; (goto-char this-point)(mon-help-escape-for-ewiki nil nil t)))

;;; ==============================
;;; :NOTE Maybe MON remember stealing this from Pascal Bourguignon?
;;; :MODIFICATIONS <Timestamp: Saturday May 30, 2009 @ 06:26.12 PM - by MON KEY>
(defun mon-help-unescape-for-ewiki (&optional start end ref-sheet)
  "Unescape special characters from the CL string specified by the region.\n
This amounts to removing preceeding backslashes from characters they escape.\n
Don't expect good results evaluation this form on strings with regexps.\n
:NOTE Region should only contain the characters actually comprising the string
without the surrounding quotes.\n
:SEE-ALSO `mon-help-escape-for-ewiki', `mon-help-keys-wikify',
`mon-get-next-face-property-change', `*mon-help-reference-keys*',
`*mon-help-reference-keywords*'.\n►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (let (ref-start ref-end)
	(if ref-sheet
	   (progn
	     (search-forward-regexp "^\\(\"#:START:REFERENCE-SHEET#\\)$" nil t)
	     (setq ref-start  (match-beginning 1))
	     (search-forward-regexp "^\\(#:END:REFERENCE-SHEET#\"\\)$" nil t)
	     (setq ref-end  (match-beginning 1)))
	  (progn
	    (setq ref-start start)
	    (setq ref-end end)))
	  (narrow-to-region ref-start ref-end)
	  (goto-char ref-start)
	  (while (search-forward "\\" nil t)
	    (replace-match "" nil t)
	    (forward-char))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-26T13:39:13-05:00Z}#{10085} - by MON KEY>
;; (let ((this-point (point))
;;       (mhrk '*mon-help-reference-keys*))
;;   (with-current-buffer 
;;       (get-buffer-create (upcase (symbol-name mhrk)))
;;     (erase-buffer)
;;     (prin1 (symbol-value (intern-soft (symbol-name mhrk)))(current-buffer))
;;     (goto-char this-point)
;;;     mon-help-escape-for-ewiki
;;     (mon-help-unescape-for-ewiki (buffer-end 0) (buffer-end 1))
;;   (display-buffer (current-buffer) t)))

;;; ==============================
;;; :REQUIRES `mon-check-feature-for-loadtime' <- mon-utils.el
;;; :CHANGESET 2020 <Timestamp: #{2010-07-31T19:46:37-04:00Z}#{10306} - by MON KEY>y
;;; :CREATED <Timestamp: #{2010-02-24T14:22:02-05:00Z}#{10083} - by MON KEY>
(defun mon-help-utils-loadtime (&optional w-msg-user)
  "Loadtime function to pull in additional libraries after mon-doc-help-utils.\n
When optional arg W-MSG-USER is non-nil message user that the libraries were
checked and required when available.\n
Following libraries pulled in by a require statement if present in load-path:\n
:FILE mon-doc-help-pacman.el
:FILE mon-doc-help-proprietary.el
:FILE mon-doc-help-css.el
:FILE mon-doc-help-tidy.el
:FILE mon-doc-help-mail.el\n
Evaluates `mon-help-bind-help-keys-loadtime' to bind:
 `mon-help-keys' and `*mon-help-reference-keys*'\n
When predicate `IS-MON-SYSTEM-P' returns non-nil evaluate:\n
 `elint-scan-doc-file'
 `mon-help-permanent-locals-find' and bind `*mon-help-permanent-locals*'
 `mon-help-byte-optimizer-find' and bind  `*mon-help-byte-optimizer-vals*'\n
:SEE-ALSO `mon-bind-nefs-photos-at-loadtime', `mon-bind-cifs-vars-at-loadtime',
`mon-bind-doc-help-proprietery-vars-at-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-set-register-tags-loadtime',
`mon-CL-cln-colon-swap'.\n►►►"
  (let ((myb-msg-usr '("-- performed feature checks at loadtime")))
    (when (or 
           (progn
             ;; :Fall through fo MON-SYSTEMS
             (when (and (intern-soft "IS-MON-SYSTEM-P" obarray)  ;; *IS-MON-OBARRAY*
                        (bound-and-true-p IS-MON-SYSTEM-P))
               (when (mon-help-byte-optimizer-find)
                 (push ":EVAULATED `mon-help-byte-optimizer-find'" myb-msg-usr)
                 (setq w-msg-user t))
               ;; (when (fboundp 'mon-check-feature-for-loadtime)
	       (when (mon-help-permanent-locals-find nil t)
		 (push ":EVALUATED `mon-help-permanent-locals-find'" myb-msg-usr)
		 (setq w-msg-user t)))
             (when (mon-help-bind-help-keys-loadtime t)
               (push ":EVALUATED `mon-help-bind-help-keys-loadtime'" myb-msg-usr)
               (setq w-msg-user t))
             (when (mon-check-feature-for-loadtime 'mon-doc-help-pacman) 
               (push ":REQUIRED `mon-doc-help-pacman'" myb-msg-usr)
               (setq w-msg-user t))
             (when (mon-check-feature-for-loadtime 'mon-doc-help-css) 
               (push ":REQUIRED `mon-doc-help-css'" myb-msg-usr)
               (setq w-msg-user t)) 
             (when (mon-check-feature-for-loadtime 'mon-doc-help-tidy) 
               (push ":REQUIRED `mon-doc-help-tidy'" myb-msg-usr)
               (setq w-msg-user t)) 
             (when (mon-check-feature-for-loadtime 'mon-doc-help-mail) 
               (push ":REQUIRED `mon-doc-help-mail'" myb-msg-usr)               
               (setq w-msg-user t))
             ;; Load doc functions which can't possibly be GPL/GFDL e.g MS-C0RP API etc.
             (when (mon-check-feature-for-loadtime 'mon-doc-help-proprietary)
               (push ":REQUIRED `mon-doc-help-proprietary'" myb-msg-usr)
               (setq w-msg-user t)))
           w-msg-user)
      (mon-message :msg-spec `(":FUNCTION `mon-help-utils-loadtime' "
                               ,(mapconcat #'identity (setq myb-msg-usr (nreverse myb-msg-usr)) "\n"))))))

;;; ==============================
(provide 'mon-doc-help-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:


;;; ================================================================
;;; mon-doc-help-utils.el ends here
;;; EOF

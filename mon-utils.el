;;; mon-utils.el --- common utilities and BIG require for other of MON packages
;; -*- mode: EMACS-LISP; -*-
;;; ================================================================
;; Copyright © 2008-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2008-09
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: development, extensions, convenience, environment,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; Provides common utilities and BIG require for other of MON's packages.
;;
;; FUNCTIONS:►►►
;; `mon-after-mon-utils-loadtime', `mon-recover-nil-t-default-plist',
;; `mon-get-emacsd-paths',
;;
;;
;; `mon-view-help-source', `mon-index-elisp-symbol',
;; `mon-map-obarray-symbol-plist-props', 
;;
;; `mon-semnav-up', 
;; `mon-extend-selection', 
;; `mon-looking-back-p', `mon-match-at-point',
;; 
;; `mon-escape-lisp-string-region', `mon-unescape-lisp-string-region',
;; 
;; 
;; `mon-quote-sexp',
;;
;; `mon-princ-cb',
;; `mon-pretty-print-sexp-at-point',
;;
;; `mon-eval-sexp-at-point', `mon-eval-print-last-sexp',
;; `mon-eval-expression', `mon-toggle-eval-length', 
;;
;; `mon-dump-object-to-file', `mon-byte-compile-and-load', 
;; `mon-compile-when-needed', `mon-load-or-alert',
;;
;; `mon-nuke-and-eval', `mon-unbind-defun', `mon-unbind-symbol',
;; `mon-unbind-function', `mon-unbind-command', `mon-unbind-variable',
;;
;; FUNCTIONS:◄◄◄
;; 
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-utils-post-load-requires*', `*mon-recover-nil-t-default-plist*',
;; `*mon-default-comment-start*', `*mon-xrefs-xrefs*',
;;
;; :GROUPS
;; `mon-base', `mon-xrefs',
;;
;; ALIASED:
;; :NOTE Aliases defined in :FILE mon-aliases.el
;;
;;  <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-lisp-escape-region'          -> `mon-escape-lisp-string-region'
;; `mon-lisp-unescape-region'        -> `mon-unescape-lisp-string-region'
;;
;; `mon-register-append'             -> `mon-append-to-register'
;;
;; `mon-string-escape-lisp-region'   -> `mon-escape-lisp-string-region'
;;
;; `mon-string-unescape-lisp-region' -> `mon-unescape-lisp-string-region'
;;
;; DEPRECATED:
;; `mon-string-from-sequence2' ;; :REMOVED
;;
;; RENAMED:
;; `mon-make-a-pp'                   -> `mon-pretty-print-sexp-at-point'
;; `mon-stringify-list'              -> `mon-string-ify-list'
;; `mon-split-string-line'           -> `mon-line-string-get'

;; `mon-kill-ring-save-w-props'      -> `mon-get-text-properties-region-to-kill-ring'
;;
;; MOVED:
;; `mon-index-elisp-symbol'                       -> mon-doc-help-utils.el
;;
;; `*mon-default-comment-start*'                  <- mon-time-utils.el
;;
;; `mon-async-du-dir'                             -> mon-dir-utils.el
;;
;; `mon-next-almost-prime'                        -> mon-randomize-utils.el
;; `mon-gensym-counter-randomizer'                -> mon-randomize-utils.el
;; `mon-make-random-state'                        -> mon-randomize-utils.el
;; `mon-generate-prand-id'                        -> mon-randomize-utils.el
;; `mon-generate-prand-seed'                      -> mon-randomize-utils.el
;; `mon-string-wonkify'                           -> mon-randomize-utils.el
;; `mon-generate-WPA-key'                         -> mon-randomize-utils.el
;;
;; `mon-list-all-properties-in-buffer'            -> mon-text-property-utils.el
;; `mon-get-text-properties-region-to-kill-ring'  -> mon-text-property-utils.el
;; `mon-nuke-text-properties-buffer'              -> mon-text-property-utils.el
;; `mon-remove-text-property'                     -> mon-text-property-utils.el 
;; `mon-get-face-at-posn'                         -> mon-text-property-utils.el
;; `mon-get-face-at-point'                        -> mon-text-property-utils.el
;; `mon-remove-single-text-property'              -> mon-text-property-utils.el
;; `mon-nuke-text-properties-region'              -> mon-text-property-utils.el
;; `mon-get-text-properties-category'             -> mon-text-property-utils.el
;; `mon-nuke-overlay-buffer'                      -> mon-text-property-utils.el
;;
;; `mon-plist-keys'                               -> mon-plist-utils.el
;; `mon-plist-remove!'                            -> mon-plist-utils.el
;; `mon-plist-remove-consing'                     -> mon-plist-utils.el
;; `mon-plist-remove-if'                          -> mon-plist-utils.el
;;
;; `mon-list-nshuffle-TEST'                       -> mon-testme-utils.el
;; `mon-gensym-counter-randomizer-TEST'           -> mon-testme-utils.el
;; `mon-line-strings-to-list-TEST'                -> mon-testme-utils.el
;; `mon-with-inhibit-buffer-read-only-TEST'       -> mon-testme-utils.el
;; `mon-line-dolines-TEST'                        -> mon-testme-utils.el
;; `mon-line-dolines-setup-TEST'                  -> mon-testme-utils.el
;; `mon-with-buffer-undo-disabled-TEST'           -> mon-testme-utils.el
;; `mon-with-inhibit-buffer-read-only-PP-TEST'    -> mon-testme-utils.el
;; `mon-string-split-TEST'                        -> mon-testme-utils.el
;; `mon-line-strings-bq-qt-sym-bol-TEST'          -> mon-testme-utils.el
;;
;; `mon-string-alpha-list'                        -> mon-alphabet-list-utils.el
;; `*mon-alphabet-as-type-generate*'              -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-type'                         -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-defun'                        -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-bc'                           -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-map-bc'                       -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-doc-loadtime'                 -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-map-fun-prop'                 -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-unintern-fun'                 -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-keyU->num'               -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-keyD->num'               -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-symU->num'               -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-symD->num'               -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-stringU->num'            -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-stringD->num'            -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-keyU->stringU'           -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-cons-keyD->stringD'           -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-plistU->stringU'              -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-plistD->stringD'              -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-plistU->num'                  -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-plistD->num'                  -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-list-stringU'                 -> mon-alphabet-list-utils.el 
;; `mon-alphabet-as-list-stringD'                 -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-list-symbolU'                 -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-list-symbolD'                 -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-stringU-w-nl'                 -> mon-alphabet-list-utils.el  
;; `mon-alphabet-as-stringD-w-nl'                 -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-stringU-w-spc'                -> mon-alphabet-list-utils.el
;; `mon-alphabet-as-stringD-w-spc'                -> mon-alphabet-list-utils.el
;;
;; `mon-foreach'                                  -> mon-macs.el
;; `mon-for'                                      -> mon-macs.el
;; `mon-loop'                                     -> mon-macs.el
;; `mon-moveq'                                    -> mon-macs.el
;; `mon-line-dolines'                             -> mon-macs.el
;; `defconstant'                                  -> mon-macs.el
;; `defparameter'                                 -> mon-macs.el
;; `mon-with-buffer-undo-disabled'                -> mon-macs.el
;; `mon-buffer-exists-p'                          -> mon-macs.el
;; `mon-check-feature-for-loadtime'               -> mon-macs.el
;; `mon-with-inhibit-buffer-read-only'            -> mon-macs.el
;; `mon-gensym'                                   -> mon-macs.el
;; `mon-with-gensyms'                             -> mon-macs.el
;; `mon-nshuffle-vector'                          -> mon-macs.el
;; `mon-mapcar-mac'                               -> mon-macs.el
;; `mon-list-sift'                                -> mon-macs.el
;; `mon-with-print-gensyms'                       -> mon-macs.el
;; `mon-equality-for-type'                        -> mon-macs.el
;;
;; `mon-map-windows->plist'                       -> mon-window-utils.el
;; `mon-twin-horizontal'                          -> mon-window-utils.el
;; `mon-twin-vertical'                            -> mon-window-utils.el
;; `mon-toggle-menu-bar'                          -> mon-window-utils.el
;; `mon-scratch'                                  -> mon-window-utils.el
;; `mon-switch-to-messages'                       -> mon-window-utils.el
;; `mon-kill-completions'                         -> mon-window-utils.el
;; `mon-flip-windows'                             -> mon-window-utils.el
;;
;; `mon-g2be'                                     -> mon-buffer-utils.el
;; `mon-buffer-sub-no-prop'                       -> mon-buffer-utils.el
;; `mon-buffer-sub-no-prop-check'                 -> mon-buffer-utils.el
;; `mon-buffer-narrowed-p'                        -> mon-buffer-utils.el
;; `mon-buffer-empty-p'                           -> mon-buffer-utils.el
;; `mon-buffer-exists-so-kill'                    -> mon-buffer-utils.el
;; `mon-get-buffer-hidden'                        -> mon-buffer-utils.el
;; `mon-get-buffer-window-if'                     -> mon-buffer-utils.el
;; `mon-print-buffer-object-readably'             -> mon-buffer-utils.el
;; `mon-get-buffer-w-mode'                        -> mon-buffer-utils.el
;; `mon-buffer-name->kill-ring'                   -> mon-buffer-utils.el
;; `mon-append-to-buffer'                         -> mon-buffer-utils.el
;; `mon-make-shell-buffer'                        -> mon-buffer-utils.el
;; `mon-shell'                                    -> mon-buffer-utils.el
;;
;; `mon-get-bit-table'                            -> mon-type-utils.el
;; `mon-function-object-p'                        -> mon-type-utils.el
;; `mon-equality-or-predicate'                    -> mon-type-utils.el
;; `mon-booleanp'                                 -> mon-type-utils.el
;; `mon-booleanp-to-binary'                       -> mon-type-utils.el
;; `mon-zero-or-onep'                             -> mon-type-utils.el
;; `mon-string-or-null-and-zerop'                 -> mon-type-utils.el
;; `mon-string-not-null-nor-zerop'                -> mon-type-utils.el
;; `mon-sequence-all-booleanp'                    -> mon-type-utils.el
;; `mon-list-proper-p'                            -> mon-type-utils.el
;; `mon-sequence-mappable-p'                      -> mon-type-utils.el
;; `mon-get-bit-table'                            -> mon-type-utils.el
;; `mon-char-code'        			  -> mon-type-utils.el
;; `mon-alpha-char-p'     			  -> mon-type-utils.el
;; `mon-is-digit'         			  -> mon-type-utils.el
;; `mon-is-letter'        			  -> mon-type-utils.el
;; `mon-is-alphanum'      			  -> mon-type-utils.el
;; `mon-is-digit-simp'    			  -> mon-type-utils.el
;; `mon-is-letter-simp'   			  -> mon-type-utils.el
;; `mon-is-alphanum-simp' 			  -> mon-type-utils.el
;; `mon-symbol-to-string' 			  -> mon-type-utils.el
;; `mon-string-to-symbol' 			  -> mon-type-utils.el
;; `mon-string-to-sequence'                       -> mon-type-utils.el
;; `mon-string-from-sequence'                     -> mon-type-utils.el
;; `*mon-bit-table*'                              -> mon-type-utils.el
;;
;; `*mon-equality-or-predicate-function-types*'   -> mon-type-utils-vars.el
;; `*mon-function-object-types*'                  -> mon-type-utils-vars.el
;; `*mon-special-forms-types*'                    -> mon-type-utils-vars.el
;; `*mon-non-mappable-object-types*'              -> mon-type-utils-vars.el
;;
;; `mon-set-difference'                           -> mon-seq-utils.el
;; `mon-intersection'                             -> mon-seq-utils.el
;; `mon-maptree'                                  -> mon-seq-utils.el
;; `mon-combine'                                  -> mon-seq-utils.el
;; `mon-mismatch'                                 -> mon-seq-utils.el
;; `mon-transpose'                                -> mon-seq-utils.el
;; `mon-remove-if-not'                            -> mon-seq-utils.el
;; `mon-remove-if'                                -> mon-seq-utils.el
;; `mon-delete-if'                                -> mon-seq-utils.el
;; `mon-member-if'                                -> mon-seq-utils.el
;; `mon-union'                                    -> mon-seq-utils.el
;; `%mon-list-reorder'                            -> mon-seq-utils.el
;; `mon-list-add-non-nil'                         -> mon-seq-utils.el
;; `mon-list-match-tails'                         -> mon-seq-utils.el
;; `mon-list-filter'                              -> mon-seq-utils.el
;; `mon-remove-dups'                              -> mon-seq-utils.el
;; `mon-list-make-unique'                         -> mon-seq-utils.el
;; `mon-list-last'                                -> mon-seq-utils.el
;; `mon-delete-first'                             -> mon-seq-utils.el
;; `mon-deleql-dups'                              -> mon-seq-utils.el
;; `mon-delq-dups'                                -> mon-seq-utils.el
;; `mon-list-shuffle-safe'                        -> mon-seq-utils.el
;; `mon-list-nshuffle'                            -> mon-seq-utils.el
;; `mon-delq-cons'                                -> mon-seq-utils.el
;; `mon-maybe-cons'                               -> mon-seq-utils.el
;; `mon-list-string-longest'                      -> mon-seq-utils.el
;; `mon-sublist-gutted'                           -> mon-seq-utils.el
;; `mon-assoc-replace'                            -> mon-seq-utils.el
;; `mon-list-reorder'                             -> mon-seq-utils.el
;; `mon-list-merge'                               -> mon-seq-utils.el
;; `mon-recursive-apply'                          -> mon-seq-utils.el
;; `mon-list-flatten-rotated'                     -> mon-seq-utils.el
;; `mon-flatten'                                  -> mon-seq-utils.el
;; `mon-moveq'                                    -> mon-seq-utils.el
;; `mon-map-append'                               -> mon-seq-utils.el
;; `mon-sublist'                                  -> mon-seq-utils.el
;; `mon-subseq'                                   -> mon-seq-utils.el
;; `mon-maplist'                                  -> mon-seq-utils.el
;; `mon-mapcon'                                   -> mon-seq-utils.el
;; `mon-mapcan'                                   -> mon-seq-utils.el
;; `mon-mapcar'                                   -> mon-seq-utils.el
;; `mon-mapl'                                     -> mon-seq-utils.el
;; `mon-map1'                                     -> mon-seq-utils.el
;; `mon-mapc'                                     -> mon-seq-utils.el
;; `mon-elt-<elt'                                 -> mon-seq-utils.el
;; `mon-elt->elt'                                 -> mon-seq-utils.el
;; `mon-elt-<'                                    -> mon-seq-utils.el
;; `mon-elt->'                                    -> mon-seq-utils.el
;;
;; `mon-postion-for-x-popup-menu'                 -> mon-event-utils.el
;; `mon-choose-from-menu'                         -> mon-event-utils.el 
;; `mon-scroll-up-in-place'                       -> mon-event-utils.el
;; `mon-scroll-down-in-place'                     -> mon-event-utils.el
;; `mon-line-move-n'                              -> mon-event-utils.el
;; `mon-line-move-prev'                           -> mon-event-utils.el
;; `mon-line-move-next'                           -> mon-event-utils.el
;; `mon-rotate-ascii-cursor'                      -> mon-event-utils.el
;; `mon-test-keypresses'                          -> mon-event-utils.el
;; `mon-read-keys-as-string'                      -> mon-event-utils.el
;; `mon-read-multiple'                            -> mon-event-utils.el
;; `mon-abort-recursive-edit'                     -> mon-event-utils.el
;; `mon-abort-autosave-when-fucked'               -> mon-event-utils.el
;; `mon-append-to-register'                       -> mon-event-utils.el
;; `mon-append-to-buffer'                         -> mon-event-utils.el
;; `mon-kill-appending'                           -> mon-event-utils.el
;; `*mon-popup-pos-x-offset*'                     -> mon-event-utils.el
;; `*mon-ascii-cursor-state*'                     -> mon-event-utils.el
;;
;; `mon-line-strings-bq-qt-sym-bol'               -> mon-line-utils.el
;; `mon-line-strings-qt-region'                   -> mon-line-utils.el
;; `mon-line-strings-indent-to-col'               -> mon-line-utils.el
;; `mon-line-indent-from-to-col'                  -> mon-line-utils.el
;; `mon-line-strings-pipe-bol'                    -> mon-line-utils.el
;; `mon-line-strings-pipe-to-col'                 -> mon-line-utils.el
;; `mon-line-strings'                             -> mon-line-utils.el
;; `mon-line-strings-region'                      -> mon-line-utils.el
;; `mon-line-string-insert-chars-under'           -> mon-line-utils.el
;; `mon-line-strings-one-list'                    -> mon-line-utils.el
;; `mon-line-strings-to-list'                     -> mon-line-utils.el
;; `mon-line-string-rotate-name'                  -> mon-line-utils.el
;; `mon-line-string-unrotate-namestrings'         -> mon-line-utils.el
;; `mon-line-string-rotate-namestrings'           -> mon-line-utils.el
;; `mon-line-string-rotate-namestrings-combine'   -> mon-line-utils.el
;; `mon-line-count-region'                        -> mon-line-utils.el
;; `mon-line-count-matchp'                        -> mon-line-utils.el
;; `mon-line-length-max'                          -> mon-line-utils.el
;; `mon-line-count-buffer'                        -> mon-line-utils.el
;; `mon-line-find-duplicates'                     -> mon-line-utils.el
;; `mon-spacep'                                   -> mon-line-utils.el
;; `mon-spacep-not-bol'                           -> mon-line-utils.el
;; `mon-spacep-is-bol'                            -> mon-line-utils.el
;; `mon-spacep-is-after-eol'                      -> mon-line-utils.el
;; `mon-spacep-is-after-eol-then-graphic'         -> mon-line-utils.el
;; `mon-spacep-at-eol'                            -> mon-line-utils.el
;; `mon-spacep-first'                             -> mon-line-utils.el
;; `mon-line-bol-is-eol'                          -> mon-line-utils.el
;; `mon-line-previous-bol-is-eol'                 -> mon-line-utils.el
;; `mon-line-next-bol-is-eol'                     -> mon-line-utils.el
;; `mon-line-eol-is-eob'                          -> mon-line-utils.el
;;
;; `mon-string-repeat'                            -> mon-string-utils.el
;; `mon-string-sub-old->new'                      -> mon-string-utils.el
;; `mon-string-replace-char'                      -> mon-string-utils.el
;; `mon-string-split-on-regexp'                   -> mon-string-utils.el
;; `mon-string-ify-list'                          -> mon-string-utils.el
;; `mon-string-chop-spaces'                       -> mon-string-utils.el
;; `mon-string-has-suffix'                        -> mon-string-utils.el
;; `mon-string-position'                          -> mon-string-utils.el
;; `mon-string-sort-descending'                   -> mon-string-utils.el
;; `mon-string-after-index'                       -> mon-string-utils.el
;; `mon-string-upto-index'                        -> mon-string-utils.el
;; `mon-string-index'                             -> mon-string-utils.el
;; `mon-string-fill-to-col'                       -> mon-string-utils.el
;; `mon-string-justify-left'                      -> mon-string-utils.el
;; `mon-string-spread'                            -> mon-string-utils.el
;; `mon-string-split'                             -> mon-string-utils.el
;; `mon-string-to-hex-list'                       -> mon-string-utils.el
;; `mon-string-from-hex-list'                     -> mon-string-utils.el
;; `mon-string-to-hex-string'                     -> mon-string-utils.el
;; `mon-string-to-hex-list-cln-chars'             -> mon-string-utils.el
;;
;; `mon-get-syntax-at'                            -> mon-word-syntax-utils.el
;; `mon-get-syntax-class-at'                      -> mon-word-syntax-utils.el
;; `mon-line-test-content'                        -> mon-word-syntax-utils.el
;; `mon-word-get-next'                            -> mon-word-syntax-utils.el
;; `mon-word-get-list-in-buffer'                  -> mon-word-syntax-utils.el
;; `mon-word-reverse-region'                      -> mon-word-syntax-utils.el
;; `mon-word-iterate-over'                        -> mon-word-syntax-utils.el
;; `mon-word-iterate-over'                        -> mon-word-syntax-utils.el
;; `mon-word-count-analysis'                      -> mon-word-syntax-utils.el
;; `mon-word-count-occurrences'                   -> mon-word-syntax-utils.el
;; `mon-word-count-region'                        -> mon-word-syntax-utils.el
;; `mon-word-count-chars-region'                  -> mon-word-syntax-utils.el
;;
;; `mon-get-system-specs'                         -> mon-env-proc-utils.el
;; `mon-get-env-vars-symbols'                     -> mon-env-proc-utils.el
;; `mon-get-env-vars-strings'                     -> mon-env-proc-utils.el
;; `mon-get-env-vars-emacs'                       -> mon-env-proc-utils.el
;; `mon-get-sys-proc-list'                        -> mon-env-proc-utils.el
;; `mon-insert-sys-proc-list'                     -> mon-env-proc-utils.el
;; `mon-get-proc-w-name'                          -> mon-env-proc-utils.el
;; `mon-get-process'                              -> mon-env-proc-utils.el
;; `mon-escape-string-for-cmd'                    -> mon-env-proc-utils.el   
;; `mon-terminal'                                 -> mon-env-proc-utils.el
;; `mon-cmd'                                      -> mon-env-proc-utils.el
;; `mon-firefox'                                  -> mon-env-proc-utils.el
;; `mon-conkeror'                                 -> mon-env-proc-utils.el
;;
;; `mon-rectangle-capitalize'                     -> mon-rectangle-utils.el
;; `mon-rectangle-upcase'                         -> mon-rectangle-utils.el
;; `mon-rectangle-downcase'                       -> mon-rectangle-utils.el
;; `mon-rectangle-apply-on-region-points'         -> mon-rectangle-utils.el
;; `mon-rectangle-operate-on'                     -> mon-rectangle-utils.el
;; `mon-rectangle-sum-column'                     -> mon-rectangle-utils.el
;; `mon-rectangle-columns'                        -> mon-rectangle-utils.el
;; `mon-kill-rectangle-w-beer-belly'              -> mon-rectangle-utils.el
;;
;; REQUIRES:
;;
;; TODO:
;;
;; NOTES:
;; `mon-get-process' uses `flet' cl--find-if            -> `find-if'
;;
;; SNIPPETS:
;;
;; THIRD PARTY CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-utils.el
;; FIRST-PUBLISHED: AUGUST 2009
;;
;; EMACS-WIKI: (URL `http://www.emacswiki.org/emacs/MonUtils')
;; FIRST-PUBLISHED: <Timestamp: #{2009-12-22T03:43:27-05:00Z}#{09522} - by MON>
;;
;; FILE-CREATED:
;; <Timestamp: 2008-09 - by MON KEY>
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
;;; ===================================
;; Copyright © 2008-2011 MON KEY 
;;; ===================================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :NOTE before :FILE mon-error-utils.el mon-text-property-utils.el
;;; :CHANGESET 2171
;;; :CREATED <Timestamp: #{2010-10-02T11:32:40-04:00Z}#{10396} - by MON KEY>
(defgroup mon-base nil
  "Top level group from which other mon related packages and groups inherit.\n
:SEE-ALSO .\n►►►"
  :prefix "mon-"
  :prefix "*mon-"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-utils.el')"
          "http://www.emacswiki.org/emacs/mon-utils.el")
  :link '(emacs-library-link 
          :tag ":FILE mon-utils.el" 
          "mon-utils.el")
  :group 'local)

;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T14:24:52-05:00Z}#{11022} - by MON KEY>
(defgroup mon-xrefs nil
  "Customization group for xrefing variables `*mon-.*-xrefs*'.\n
An xrefing variable holds a list of symbol names provided by a `mon-*.el' feature.\n
The intent in maintaining these xrefing lists is to allow eventual aggregation
of of per symbol graphs of related/associated symbol names for use in a context
oriented extension to the Emacs *Help* facility.\n
:NOTE While it is relatively straight forward to locate these symbols with `features',
`featurep', `load-history', etc. the respective return values of these functions
are not necessarily easy to parse/disambiguate esp. w/re to docstrings and
relative to the symbol-type.\n
Likewise, Emacs doesn't effectively provide a centralized/canonical per feature
location where one can store aggregate metadata regarding the \"auxillary\" data
about the symbols provided by a given feature. Such a feature, (were it to
exist) would likely benefit from the use of plists esp. where these data
structures are a natural lisp extension and are pre-established with a
privileged state as a symbol cell and can be easily extended by attaching
hash-tables as values for plist properties. However, effective utilization of
such plists demands that we establish a global symbol specially dedicated to
hold this data. Note too that there is no immediate utility in store this data
directly in hash-tableas the Elisp API does not expose the ability to
attach/access hash-tables on a symbol cell e.g. there is not a `symbol-plist'
equivalent named `symbol-hash-table' and we must still establish a global
special value to hold the hash-table.
Note also that we do not consider either `defcustom' or is the package feature
of Emacs 24+ \(at least in their current configurations\) applicable to these
needs.\n
:SEE-ALSO `*mon-xrefs-xrefs*', `*mon-regexp-symbols-xrefs*',
`*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*naf-mode-xref-of-xrefs*'.\n►►►"
  :link '(emacs-library-link :tag ":FILE mon-utils.el" "mon-utils.el")
  :group 'mon-base)

(require 'edebug)
(require 'easymenu)
(require 'bytecomp)
(require 'macroexp)

;;; ==============================
;;; :NOTE The :CONSTANT `IS-MON-SYSTEM-P' is bound in:
;;; :FILE mon-default-start-loads.el
;;; If this is not present on your system these MON features won't load.
;;; To fix this, you can do one of the following:
;;;   a) Load mon-default-start-loads.el
;;;   b) Bind `IS-MON-SYSTEM-P' to t here
;;;   c) Replace "(when IS-MON-SYSTEM-P" with "(when t"
;;;   d) Remove  "(when IS-MON-SYSTEM-P" and comment out unwanted features
;;; The preferred solution is `a' as it provides other system conditionals you
;;; will most likely need anyway with most of the `mon-*.el' packages.
;; (when (and (intern-soft "IS-MON-SYSTEM-P" obarray)
;;            (bound-and-true-p IS-MON-SYSTEM-P))

 
;;; ==============================
;;; :TODO Extend this list into an alist with elements containing 
;;; plist keys :requires :required-by :optional etc.
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T16:54:51-04:00Z}#{10361} - by MON KEY>
(defvar *mon-utils-post-load-requires* nil
  "*List of features loaded by feature mon-utils.el\n
:CALLED BY `mon-utils-require-features-at-loadtime'\n
:SEE-ALSO `mon-after-mon-utils-loadtime'.\n►►►")
;;
(unless (and (intern-soft "*mon-utils-post-load-requires*" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p *mon-utils-post-load-requires*))
  (setq *mon-utils-post-load-requires*
        '(mon-macs
          mon-type-utils-vars      ;; Required by mon-type-utils.el
          mon-type-utils
          mon-error-utils
          mon-plist-utils          ;; Required by mon-text-property-utils.el
          mon-text-property-utils 
          mon-seq-utils
          mon-buffer-utils
          mon-window-utils
          mon-randomize-utils
          mon-event-utils           ;; safe after above
          mon-line-utils
          mon-region-utils
          mon-string-utils
          mon-env-proc-utils
          mon-rectangle-utils
          mon-word-syntax-utils
          mon-testme-utils
          ;;
          mon-alphabet-list-utils
          mon-regexp-symbols
          mon-time-utils
          ;;
          ;; :FILE mon-replacement-utils.el :BEFORE :FILE mon-dir-utils.el mon-insertion-utils.el
          mon-replacement-utils 
          ;; :FILE mon-dir-utils.el :LOADS mon-dir-locals-alist.el 
          ;; :REQUIRES  mon-hash-utils.el, mon-replacement-utils.el, 
          ;;            mon-css-color.el, mon-rename-image-utils.el
          mon-dir-locals-alist
          mon-dir-utils
          mon-dir-utils-local
          mon-button-utils
          mon-cifs-utils
          mon-insertion-utils
          naf-mode-insertion-utils
          ;; :FILE mon-get-mon-packages.el :AFTER-LOAD :FILE mon-wget-utils.el
          mon-wget-utils 
          mon-url-utils
          mon-hash-utils
          mon-doc-help-utils
          mon-doc-help-CL
          mon-tramp-utils
          naf-skeletons
          ;; :NOTE `naf-mode` already have been pulled in by 
          ;; :FILE mon-default-start-loads.el
          naf-mode 
          ebay-template-mode
          mon-empty-registers
          mon-iptables-vars
          mon-iptables-regexps
          mon-get-freenode-lisp-logs
          mon-mysql-utils
          )))

;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T14:27:34-05:00Z}#{11022} - by MON KEY>
(defcustom *mon-xrefs-xrefs*
  '(*mon-regexp-symbols-xrefs* *mon-default-loads-xrefs*
  *mon-default-start-loads-xrefs* *mon-dir-locals-alist-xrefs*
  *mon-error-utils-xrefs* *mon-testme-utils-xrefs* *mon-buffer-utils-xrefs*
  *mon-button-utils-xrefs* *mon-bzr-utils-xrefs* *mon-keybindings-xrefs*
  *mon-line-utils-xrefs* *mon-macs-xrefs* *mon-post-load-hooks-xrefs*
  *mon-plist-utils-xrefs* *mon-seq-utils-xrefs* *mon-slime-xrefs*
  *mon-string-utils-xrefs* *mon-type-utils-xrefs* *mon-window-utils-xrefs*
  *naf-mode-xref-of-xrefs* *naf-mode-faces-xrefs* *naf-mode-date-xrefs*
  *mon-ulan-utils-xrefs* *mon-xrefs-xrefs*)
  "Meta list of mon-xrefing variables, e.g. those named `*mon-.*-xrefs*'.\n
:SEE-ALSO `*mon-regexp-symbols-xrefs*', `*mon-default-loads-xrefs*',
`*mon-default-start-loads-xrefs*', `*mon-dir-locals-alist-xrefs*',
`*mon-testme-utils-xrefs*', `*mon-button-utils-xrefs*',
`*naf-mode-xref-of-xrefs*', `*mon-xrefs-xrefs*'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-xrefs)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-24T14:12:12-04:00Z}#{09436} - by MON KEY>
(defcustom *mon-default-comment-start* ";;; "
  "Comment prefix for `mon-comment-divider-w-len'.\n
This is a cheap work around so we don't have to deal with `comment-start' with
`mon-comment-*' functions which might rely on or calculate a string/substring
inidex per the value of this var.\n
Its value is a string consisting of one or more identical graphic prefix chars
followed by a single space \(char 32\). Default is \";;; \"\n
When an alternate value is supplied it should have the following format:\n
 \"<PREFIX-CHARS>+<SPC>\"\n
Such that evaluation of the following two forms returns non-nil:\n
\(= \(mon-list-last \(append *mon-default-comment-start* nil\)\) 32\)\n
\(let* \(\(*mon-default-comment-start* \";;[ \"\)
       \(sub-cmnt-start \(nreverse \(cdr \(reverse \(append *mon-default-comment-start* nil\)\)\)\)\)\)
  \(catch 'prefix-not-same   
    \(equal *mon-default-comment-start*
           \(concat 
            \(mapc #'\(lambda \(x\)
                      \(unless \(= x \(car sub-cmnt-start\)\)
                        \(throw 'prefix-not-same 
                               \(format \"prefix chars not identical, failed-at: %c\" x\)\)\)\)
                  sub-cmnt-start\)
            \" \"\)\)\)\)\n
:EXAMPLE\n\n(symbol-value '*mon-default-comment-start*)\n
\(let \(\(*mon-default-comment-start* \"%% \"\)\)
  *mon-default-comment-start*\)\n
 \(mon-comment-divider-w-len 36\)\n
:NOTE This variable is dynamically let-bound by `mon-*' functions.\n
:SEE-ALSO `*mon-default-comment-divider*', `mon-set-buffer-local-comment-start',
`mon-set-buffer-local-comment-start-init', `mon-comment-divider-to-col',
`mon-comment-divider-to-col-four', `mon-comment-divider-w-len',
`mon-comment-lisp-to-col'.\n►►►"
  ;; :TODO The `:type` key should have required match see docstring for potential tests:
  :type  'string
  :group 'mon-base)

;;; ==============================
;; :TODO The `:type` key should have required match.
;;; :CREATED <Timestamp: #{2009-10-24T12:07:10-04:00Z}#{09436} - by MON KEY>
(defcustom *mon-default-comment-divider* (mon-comment-divider-w-len 30)
  "Preferred mon-comment-divider for lisp source sectioning.\n
The first four chars should have the same form as `*mon-default-comment-start*'
such that (substring 
Default is the return value of following form:\n
 \(mon-comment-divider-w-len 30\)\n
:CALLED-BY `mon-comment-divider', `mon-comment-divider-to-col'\n
:SEE-ALSO `mon-comment-divider-w-len', `mon-comment-divider-to-col'\n►►►"
  :type  'string  
  :group 'mon-base)

;;; ==============================
;;; :TODO Factor this var away.
;;; :CREATED <Timestamp: Saturday July 18, 2009 @ 11:59.08 AM - by MON KEY>
(defcustom *mon-timestamp-cond* nil
  "A list of filenames which get alternative timestamp name strings.\n
car is a filename and must be a string without whitespace, delimiters or punctuation.\n
cadr is a username to insert when timestamping in a file matching car.\n
These values are used to inform calling functions according to some heuristic.\n
:NOTE Used for file based conditional timestamps and obfuscating files/source.
:SEE-ALSO `mon-timestamp', `mon-accessed-time-stamp', `mon-accessed-stamp',
`naf-mode-timestamp-flag'.\n►►►"
  :type '(repeat (list string string))
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T15:39:13-04:00Z}#{10381} - by MON>
(defvar *mon-recover-nil-t-default-plist* nil
  "Bound at loadtime to the default plists of `nil' and `t'.\n
Recover plists with `mon-recover-nil-t-default-plist' if they become corrupted.\n
:EXAMPLE\n\n*mon-recover-nil-t-default-plist*\n
\(memq :nil-default-plist *mon-recover-nil-t-default-plist*\)\n
\(memq :t-default-plist *mon-recover-nil-t-default-plist*\)\n
:SEE-ALSO `setplist', `symbol-plist', `mon-after-mon-utils-loadtime',
`mon-run-post-load-hooks'.\n►►►")

;;; ==============================
;;; :TODO This should be evaluated before slime.el b/c she is the one stepping
;;;       on `nil's plist! Currently this isn't happening as it would require
;;;       moving it much further up in the loadtime sequence...
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T16:01:23-04:00Z}#{10381} - by MON KEY>
(defun mon-recover-nil-t-default-plist (&optional intrp)
  "Recover the default loadtime plists value of `nil' and `t'.\n
When called at loadtime and `*mon-recover-nil-t-default-plist*' is void bind it.\n
Called in a post-loadtime environement restores the plist values stored in
`*mon-recover-nil-t-default-plist*' keys :nil-default-plist :t-default-plist.\n
:EXAMPLE\n\n\(mon-recover-nil-t-default-plist t\)\n
:SEE-ALSO `setplist', `symbol-plist', `mon-after-mon-utils-loadtime', 
`mon-run-post-load-hooks'.\n►►►"
  (interactive "p")
  (if (and (intern-soft "*mon-recover-nil-t-default-plist*" obarray)
           (bound-and-true-p *mon-recover-nil-t-default-plist*))
      (let ((nval (cadr (memq :nil-default-plist *mon-recover-nil-t-default-plist*)))
            (tval (cadr (memq :t-default-plist *mon-recover-nil-t-default-plist*))))
        (if intrp 
            (when (y-or-n-p (format 
                             (concat 
                              ":FUNCTION `mon-recover-nil-t-default-plist' --\n"
                              "Current plist for `nil' is:\n %S\n"
                              "Loadtime plist for `nil' was:\n %S\n"
                              "Current plist for `t' is:\n %S\n"
                              "Loadtime plist for `t' was:\n %S\n"
                              "Rebind plists of `t' and `nil' to default loadtime values?: ")
                             (symbol-plist nil) nval (symbol-plist t) tval))
              (setplist nil nval)
              (setplist t   tval)
              (minibuffer-message (concat 
                                   ":FUNCTION `mon-recover-nil-t-default-plist' "
                                   "-- rebound plists of `t' and `nil' to default loadtime values")))
          (and (setplist nil nval)
               (setplist t   tval)
               t)))
    (progn
      (setq *mon-recover-nil-t-default-plist*
            `(:nil-default-plist ,(symbol-plist nil) :t-default-plist  ,(symbol-plist t)))
      (mon-message :msg-spec '(":FUNCTION `mon-recover-nil-t-default-plist' "
                               "-- :VARIALBE `*mon-recover-nil-t-default-plist*' bound at loadtime")))))

;;; ==============================
;;; :NOTE `mon-get-mon-emacsd-paths' is an interpreted function in:
;;; :FILE mon-default-start-loads.el
;;; :MOVED into `mon-utils-require-features-at-loadtime'
;; (when (fboundp 'mon-get-mon-emacsd-paths)
;;   (fset 'mon-get-emacsd-paths 
;;         (byte-compile
;;          (indirect-function 'mon-get-mon-emacsd-paths)))
;;   (message 
;;    "`byte-compile'd :FUNCTION `mon-get-mon-emacsd-paths' at loadtime"))

;;; ==============================
;;; :PREFIX "mcffl-"
;;; :NOTE Adding an optional arg W-SIGNAL-ERROR doesn't really make sense as the
;;; check for filename is made explicit with this marcro.
;;; :CREATED <Timestamp: #{2010-02-24T15:15:25-05:00Z}#{10083} - by MON KEY>
(defmacro mon-check-feature-for-loadtime (feature-as-symbol &optional req-w-filname)
  "Test if library FEATURE-AS-SYMBOL is in load path. If so, require feature.\n
The arg FEATURE-AS-SYMBOL is a quoted symbol.\n
When optional arg REQ-W-FILNAME is non-nil second arg to `require' will given as
the filename of feature FEATURE-AS-SYMBOL when it is in loadpath.\n
:EXAMPLE\n\n\(pp-macroexpand-expression 
          '\(mon-check-feature-for-loadtime 'mon-test-feature-fl\)\)\n
:SEE-ALSO `mon-run-post-load-hooks', `mon-purge-cl-symbol-buffers-on-load',
`mon-after-mon-utils-loadtime', `mon-unbind-command', `mon-unbind-symbol',
`mon-unbind-function', `mon-unbind-variable', `mon-unbind-defun',
`mon-compile-when-needed' `mon-load-or-alert', `mon-byte-compile-and-load',
`mon-dump-object-to-file', `mon-nuke-and-eval'.\n►►►"
  (declare (indent 2) (debug t))
  (let ((mcffl-lcl (make-symbol "mcffl-lcl"))
        (mcffl-chk (make-symbol "mcffl-chk")))
    `(let (,mcffl-lcl ,mcffl-chk)
       (setq ,mcffl-lcl (locate-library (format "%s" ,feature-as-symbol)))
       (if ,mcffl-lcl
           (unless (featurep ,feature-as-symbol)
             (if ,req-w-filname
                 (require ,feature-as-symbol ,mcffl-lcl t) ;;,(if w-signal-error nil t))
               (require ,feature-as-symbol nil t))) ;;,(if w-signal-error t nil))))
         (error 
          (concat ":MACRO `mon-check-feature-for-loadtime' "
                  "-- arg FEATURE-AS-SYMBOL does not find a feature or file, got: %s")
          ,feature-as-symbol)))))
;;
;; (put 'mon-check-feature-for-loadtime 'lisp-indent-function <INT>) 
;;
;;
;;; :TEST-ME (pp-macroexpand-expression 
;;;           '(mon-check-feature-for-loadtime 'mon-test-feature-fl))
;;; :TEST-ME (pp-macroexpand-expression 
;;;           '(mon-check-feature-for-loadtime 'mon-test-feature-fl t))
;; 
;;; :TEST-ME 
;;; (let ((mtff (concat default-directory "mon-test-feature-fl.el")))
;;;   (unwind-protect   
;;;        (progn
;;;          (with-temp-file mtff
;;;            (insert 
;;;             (concat 
;;;              "\(defvar *mon-test-feature-var* t\n"
;;;              "\"Docstring present if `mon-test-feature-fl' is feature in environment.\"\)\n"
;;;              "\(provide 'mon-test-feature-fl\)"))
;;;            (mon-check-feature-for-loadtime 'mon-test-feature-fl)
;;;            (message (documentation-property '*mon-test-feature-var* 'variable-documentation))
;;;            (sit-for 4))
;;;          (progn (unload-feature 'mon-test-feature-fl) (delete-file mtff)))))

;;; ==============================
;;; :PREFIX "murfal-"
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T17:05:28-04:00Z}#{10361} - by MON KEY>
(defun mon-utils-require-features-at-loadtime ()
  "Evaluated as the last form in mon-utils.el\n
Evaluates macro `mon-check-feature-for-loadtime' for each feature listed in
varaible `*mon-utils-post-load-requires*'\n
:SEE-ALSO `mon-after-mon-utils-loadtime', `mon-run-post-load-hooks'.\n►►►"
  (let (murfal-did-rqr)
    (dolist (murfal-D-1 *mon-utils-post-load-requires*)
      (let ((murfal-D-1-lcl-rqrd (mon-check-feature-for-loadtime murfal-D-1)))
        (when murfal-D-1-lcl-rqrd 
          (push (symbol-name murfal-D-1-lcl-rqrd) murfal-did-rqr))))
    (when (consp murfal-did-rqr)
      (mapc #'(lambda (murfal-L-1) 
                (message 
                 (concat ":FUNCTION `mon-utils-require-features-at-loadtime' "
                         "-- :FEATURE mon-utils :REQUIRED :FEATURE " murfal-L-1 " on load")))
            murfal-did-rqr)))
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
    ;; Load here instead of from :FILE naf-mode.el
    (require 'naf-mode-sql-skeletons nil t)
    (require 'mon-dbc-xml-utils)))

;;; ==============================
;;; :TODO Build additional fncn/macro to populate docstrings at loadtime.
;;; :CREATED <Timestamp: #{2010-02-24T15:15:09-05:00Z}#{10083} - by MON KEY>
(defun mon-after-mon-utils-loadtime ()
  "List of packages and functions to load or eval.\n
These are brought into the current environment after mon-utils.el is loaded.\n
Called with \(eval-after-load \"mon-utils\" '\(mon-after-mon-utils-loadtime\)\)\n
Evaluated in :FILE mon-post-load-hooks.el\n
Evaluates the following functions per feature:\n
 `mon-alphabet-as-doc-loadtime'        <- mon-alphabet-list-utils
 `mon-bind-nefs-photos-at-loadtime'    <- mon-dir-utils-local
 `mon-bind-cifs-vars-at-loadtime'      <- mon-cifs-utils
 `mon-tramp-utils-loadtime'            <- mon-tramp-utils
 `mon-set-register-tags-loadtime'      <- mon-empty-registers
 `mon-cntl-char-registers-loadtime'    <- mon-empty-registers
 `mon-help-utils-loadtime'             <- mon-doc-help-utils
 `mon-help-utils-CL-loadtime'          <- mon-doc-help-CL
 `mon-bind-mon-help-CL-pkgs-loadtime'  <- mon-doc-help-CL
 `mon-CL-cln-colon-swap'               <- mon-cl-compat-regexps
 `mon-bind-mysql-help-table-loadtime'  <- mon-mysql-utils
 `mon-bind-iptables-vars-at-loadtime'  <- mon-iptables-regexps
 `mon-bind-doc-help-proprietery-vars-at-loadtime' <- mon-doc-help-proprietary
 `mon-css-complete-loadtime'           <- mon-css-complete\n
Adds feature requirements:\n
 mon-get-mon-packages <- :AFTER mon-wget-utils
 mon-boxcutter
 google-define-redux
 mon-bzr-utils
 mon-eight-bit-raw-utils\n
Peforms loadtime evaluation of functions defined in mon-utils.el:\n
 `mon-recover-nil-t-default-plist'
 `mon-get-bit-table'\n
:SEE-ALSO `after-load-alist', `mon-check-feature-for-loadtime',
`mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-run-post-load-hooks',
`mon-purge-cl-symbol-buffers-on-load', `mon-unbind-defun',
`mon-after-mon-utils-loadtime', `mon-compile-when-needed' `mon-load-or-alert',
`mon-byte-compile-and-load', `mon-dump-object-to-file', `mon-nuke-and-eval',
`mon-cl-compat-loadtime'.\n►►►"
  (progn 
    (mon-recover-nil-t-default-plist)
    (mon-get-bit-table)
    (mon-alphabet-as-doc-loadtime 
      (mon-alphabet-as-map-bc *mon-alphabet-as-type-generate*))
    ;; :NOTE `mon-get-mon-emacsd-paths' is an interpreted function in:
    ;; :FILE mon-default-start-loads.el
    (when (and (intern-soft "mon-get-mon-emacsd-paths" obarray)
               (fboundp (intern-soft "mon-get-mon-emacsd-paths" obarray)))
      (fset 'mon-get-emacsd-paths 
            (byte-compile
             (indirect-function 'mon-get-mon-emacsd-paths)))
      (message (concat ":FUNCTION `mon-after-mon-utils-loadtime'"
                       "`byte-compile'd `mon-get-mon-emacsd-paths' at loadtime")))
    (eval-after-load "naf-mode-faces"     '(mon-bind-naf-face-vars-loadtime t))
    ;; (eval-after-load "mon-dir-utils"      '(mon-bind-nefs-photos-at-loadtime))
    (eval-after-load "mon-dir-utils-local" '(mon-bind-nefs-photos-at-loadtime))
    (eval-after-load "mon-replacement-utils" '(mon-make-iso-latin-1-approximation-loadtime))
    (eval-after-load "mon-doc-help-utils" '(mon-help-utils-loadtime t))
    ;; :NOTE Moved (mon-help-utils-CL-loadtime t) -> `mon-run-post-load-hooks'
    (eval-after-load "mon-doc-help-CL"    '(mon-bind-mon-help-CL-pkgs-loadtime t))
    ;; :NOTE See docs `mon-bind-cifs-vars-at-loadtime' and notes at BOF
    ;; mon-cifs-utils.el for alternative application with args 
    ;; NO-MISC-PATH NO-MAP-MOUNT-POINTS e.g.: 
    ;; (eval-after-load 'mon-cifs-utils '(mon-bind-cifs-vars-at-loadtime nil t)) 
    (eval-after-load "mon-cifs-utils"           '(mon-bind-cifs-vars-at-loadtime))
    (eval-after-load "mon-cl-compat-regexps"    '(mon-CL-cln-colon-swap t))    
    (eval-after-load "mon-empty-registers"      '(progn (mon-set-register-tags-loadtime t)
                                                        (mon-cntl-char-registers-loadtime)))
    (eval-after-load "mon-tramp-utils"          '(mon-tramp-utils-loadtime))
    (eval-after-load "mon-iptables-regexps"     '(mon-bind-iptables-vars-at-loadtime t))
    (eval-after-load "mon-doc-help-proprietary" 
      '(progn
         (unless (bound-and-true-p *mon-help-w32-CMD-commands-TEMP*)
           (load-library "mon-doc-help-proprietary"))
         (mon-bind-doc-help-proprietery-vars-at-loadtime *mon-help-w32-CMD-commands-TEMP*)))
    (eval-after-load "mon-wget-utils" 
      '(mon-check-feature-for-loadtime    'mon-get-mon-packages))
    (eval-after-load "mon-css-complete"   '(mon-css-complete-loadtime t))
    (eval-after-load "mon-mysql-utils"    '(mon-bind-mysql-help-table-loadtime t))
    (mon-check-feature-for-loadtime       'google-define-redux)
    (mon-check-feature-for-loadtime       'mon-bzr-utils)
    (mon-check-feature-for-loadtime       'mon-eight-bit-raw-utils)
    (mon-check-feature-for-loadtime       'mon-drive-transfer-utils)
    (mon-check-feature-for-loadtime       'mon-jg-directory-creator)
    (mon-check-feature-for-loadtime       'mon-color-utils)
    (eval-after-load "mon-boxcutter" '(boxcutter-mkdir-loadtime))
    (mon-check-feature-for-loadtime   'mon-boxcutter)
    (mon-check-feature-for-loadtime   'thumbs)
    (mon-check-feature-for-loadtime   'mon-aliases)))

;;; ==============================
;;; :PREFIX "mmap-"
;;; :COURTESY Andy Stewart <lazycat.manatee@gmail.com> :WAS `match-at-point'
;;; :SEE (URL `http://www.emacswiki.org/emacs/lazycat-toolkit.el')
;;; :CHANGESET 1768 <Timestamp: #{2010-05-25T19:21:55-04:00Z}#{10212} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday June 03, 2009 @ 06:18.14 PM - by MON KEY>
(defun mon-match-at-point (match-regexp)
  "Return the buffer substring around point matching MATCH-REGEXP.\n
Look for a match starting at or before point.\n
Move back a character at a time while still looking at a match ending at the
same point.\n
If no match is found at or before point, return the first match after point, or
nil if there is no match in the buffer.\n
:SEE-ALSO `looking-at', `looking-at-p'.\n►►►"
  (let (mmap-bkup mmap-beg mmap-end)
    (save-excursion
      (setq mmap-bkup
            (or (save-match-data (looking-at match-regexp))
                (and (search-forward-regexp match-regexp nil 'limit)
                     (setq mmap-end t)
                     (goto-char (match-beginning 0))
                     nil)
                ;; Failed search doesn't change match-data.
                (search-backward-regexp match-regexp nil t)))
      (when (or mmap-bkup mmap-end) 
        (setq mmap-beg (match-beginning 0))
        (setq mmap-end (match-end 0)))
      (if mmap-bkup
          (while (and (not (bobp))
                      (progn (backward-char) t)
                      (save-match-data (looking-at match-regexp))
                      (= (match-end 0) mmap-end))
            (setq mmap-beg (point)))
        (or (bobp) (search-forward-regexp match-regexp nil t))))
    (and mmap-beg
         (progn 
           (goto-char mmap-end)
           t)
         (mon-buffer-sub-no-prop  mmap-beg mmap-end))))

;;; ==============================
;;; :PREFIX "mlbp-"
;;; :CHANGESET 2035
;;; :CREATED <Timestamp: #{2010-08-04T20:00:08-04:00Z}#{10313} - by MON KEY>
(defun mon-looking-back-p (regexp &optional limit greedy)
  "Like `looking-back' but doesn't modify the match data.\n
:EXAMPLE\n\n\(let \(\(mb4-aftr \(list :old-match-data \(match-data t\)\)\)\)
  \(save-excursion 
    \(end-of-line 3\)
    \(when \(mon-looking-back-p \"^OMG theres a bubba back there!\"\)
      \(unless \(equal \(match-data t\) mb4-aftr\)
        \(setq mb4-aftr 
              \(nconc '\(:buba-change-match-data nil\) mb4-aftr\)\)\)
      \(setq mb4-aftr \(nconc '\(:found-bubba t\) mb4-aftr\)\)\)\)
  mb4-aftr\)\n\nOMG theres a bubba back there!\n
:SEE-ALSO `looking-at-p', `inhibit-changing-match-data'.\n►►►"
  (let ((mlbp-beg (point))
	(mlbp-psn
	 (let ((inhibit-changing-match-data t))
           (save-excursion
             (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                  (point))))))
    (if (and greedy mlbp-psn)
	(save-restriction
	  (narrow-to-region (mon-g2be -1 t) mlbp-beg)
	  (while (and (> mlbp-psn (point-min))
		      (save-excursion
			(goto-char mlbp-psn)
			(backward-char 1)
			(looking-at-p (concat "\\(?:"  regexp "\\)\\'"))))
	    (setq mlbp-psn (1- mlbp-psn)))
	  (save-excursion
	    (goto-char mlbp-psn)
	    (looking-at-p (concat "\\(?:"  regexp "\\)\\'")))))
    (not (null mlbp-psn))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-12T14:07:56-04:00Z}#{09376} - by MON KEY>
(defun mon-string-read-match-string (&optional match-subexp)
  "Make `match-string' return more than just the last string matched.\n
Strip the # char from the side-effect value returned by match-string.
When MATCH-SUBEXP is non-nil return match-string of nth subexp.
The function match-string carries more data than just the string it returns.
These datum include:
 from-idx to-idx of subexp match location;
if match string is fontified and the face used @ from-sub-idx to-sub-idx;
if match string carries text properties and if so the stickiness
of these props @ from-sub-idx to-sub-idx;
However, this data is not accessible to read because match-string returns as an
unreadable object with the '#' prefix so we strip it.\n
:EXAMPLE 
:NOTE Evaluate following in *scratch* (or equiv) with `emacs-lisp-mode' t.
> \(search-forward-regexp \"\\\\\(\\\\\(\\=[\\\\\)\\\\\([0-9\\=]\\\\\\={8,10\\\\}\\\\\)\\\\\(]\\\\\)\\\\\)\" nil t\)
\"[500006383]\"
> (match-string 0)\n; => #\(\"[500006383]\" 0 11 \(fontified t face font-lock-string-face\)\)
> \(search-forward-regexp \"\\\\\(\\\\\(\\=[\\\\\)\\\\\([0-9\\=]\\\\\\={8,10\\\\}\\\\\)\\\\\(]\\\\\)\\\\\)\" nil t\)
\"[500006383]\"
> (mon-string-read-match-string)
; => \(\"[500006383]\" 0 11 \(fontified t face font-lock-string-face\)\)\n
:SEE-ALSO `mon-string-replace-char', `mon-looking-back-p'.\n►►►"
  (let ((msrms-mtch (if (and (= (match-beginning 0) 1)(> (point) (mon-g2be -1 t) )) 
                        nil ;; Last search didn't move point was a dud don't proceed.
                      (car (read-from-string 
                            (substring (format "%S" (match-string (if match-subexp match-subexp 0))) 1))))))
    msrms-mtch))
;;
;;; :TEST-ME (search-forward-regexp "\\(\\(\\[\\)\\([0-9]\\{8,10\\}\\)\\(]\\)\\)" nil t)
;;;        [500006383]
;;; :TEST-ME (mon-string-read-match-string)
;;; :TEST-ME (mon-string-read-match-string 4)

 
;;; ===================================
;; :WHITESPACE 
;;; EOL, BOL, EOB, BOB, LEP, LBP, etc.
;;; ===================================

;;; ==============================
;;; Word, Line, String Related utils
;;; ==============================

;;; ==============================
;;; :PREFIX "mvhs-"
(defun mon-view-help-source ()
  "
:SEE-ALSO `mon-get-text-properties-category', `mon-line-test-content'.\n►►►"
  (interactive)
  (eval-when-compile (require 'ffap))
  (unwind-protect			;body
      (let (mvhs-get-bfr)
        (if (or (equal (buffer-name)(help-buffer))
                (string= "*Help*" (buffer-name)))
            (save-window-excursion
              (mon-g2be -1)
              (while (not (eobp))       ;
                (let ((mvhs-next-chng
                       (or (next-property-change (point) (current-buffer))
                           (point-max)))
                      mvhs-this-chng)
                  (progn
                    (goto-char mvhs-next-chng)
                    (setq mvhs-this-chng (mon-get-text-properties-category))
                    ;; Whats up with all the needless `and's?
                    (when (and (and (string= mvhs-this-chng 'help-function-def-button))
                               (and (ffap-file-at-point)))
                      (let* ((mvhs-ffap-fl (ffap-file-at-point)))
                        (view-file-other-window (ffap-file-at-point))
                        (setq mvhs-get-bfr (find-buffer-visiting mvhs-ffap-fl)))
                      mvhs-get-bfr)))))))))

;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-05-27T20:09:25-04:00Z}#{10214} - by MON KEY>
(defun mon-map-obarray-symbol-plist-props (w-prop-sym &optional display-in-buffer intrp)
  "Map atoms in obarray looking for the plist property W-PROP-SYM.\n
Return the list of symbols with plist property W-PROP-SYM.\n
When called-interactively or optional arg DISPLAY-IN-BUFFER is non-nil return
values to buffer named \"*OBARRY-W-PROP-SYM-MATCHES*\".\n
If DISPLAY-IN-BUFFER is a string or buffer object that satisfies `buffer-live-p'
return results in that buffer creating one if it doesn't exist.\n
:EXAMPLE\n\n\(mon-map-obarray-symbol-plist-props 'permanent-local\)\n
\(mon-map-obarray-symbol-plist-props 'permanent-local t\)\n
\(mon-map-obarray-symbol-plist-props 'side-effect-free \(get-buffer-create \"*SIDE-EFFECT-FREE*\"\)\)\n
\(mon-map-obarray-symbol-plist-props 'side-effect-free \"*SIDE-EFFECT-FREE*\"\)\n
:SEE-ALSO `mon-plist-remove!', `mon-map-obarray-symbol-plist-props',
`mon-plist-remove-if', `mon-plist-remove-consing', `remf', `remprop',
`mon-plist-keys', `mon-help-plist-functions', `mon-help-plist-properties'.\n►►►"
  (interactive "SSearch obarray for plist property: \npp")
  ;; (intern-soft (read-string "w-prop-sym: ") )
  (let* ((mmospp-clbrp 'mmospp-clbrp)
         (mmospp-bfr (when display-in-buffer 
                       (cond ((buffer-live-p display-in-buffer) 
                              (setplist mmospp-clbrp '(no-clobber t))
                              display-in-buffer)
                             ((stringp display-in-buffer)
                              (when (buffer-live-p (get-buffer display-in-buffer)) 
                                (setplist mmospp-clbrp '(no-clobber t))
                                (get-buffer-create display-in-buffer)))
                             ((or intrp t) 
                              (setplist mmospp-clbrp '(no-clobber nil))
                              (get-buffer-create "*OBARRY-W-PROP-SYM-MATCHES*"))))))
    ;; (get mmospp-clbrp 'no-clobber)))
    ;; (mon-map-obarray-symbol-plist-props nil (current-buffer))
    (when (and mmospp-bfr (not (get mmospp-clbrp 'no-clobber)))
      (with-current-buffer mmospp-bfr (erase-buffer)))
    (mapatoms #'(lambda (mmospp-L-1) 
                  (when (plist-get (symbol-plist mmospp-L-1)  w-prop-sym)
                    (if display-in-buffer
                        (progn
                          ;;(princ (identity mmospp-L-1)  (get-buffer mmospp-bfr))
                          (princ (format "%s\n" (identity mmospp-L-1))
                                 (get-buffer mmospp-bfr)))
                      ;;(princ "\n" (get-buffer mmospp-bfr)))
                      (push mmospp-L-1 mmospp-bfr)))))
    (if display-in-buffer 
        (with-current-buffer mmospp-bfr
          (if (eq (mon-g2be -1 t)(mon-g2be 1 t))
              (unwind-protect
                  (message (concat ":FUNCTION `mon-map-obarray-symbol-plist-props' " 
                                   "-- search in obarray for symbols with property `%S' "
                                   "did not return displayable results") w-prop-sym)
                (when (eq (current-buffer) 
                          (get-buffer "*OBARRY-W-PROP-SYM-MATCHES*"))
                  (kill-buffer)))
            (progn
              (sort-lines nil (mon-g2be -1 t)(mon-g2be 1 t))
              (mon-line-strings-bq-qt-sym-bol (mon-g2be -1 t)(1- (mon-g2be 1 t)) t)
              (let ((comment-start ";;")
                    (comment-style 'indent))
                (comment-region (mon-g2be -1 t)(mon-g2be 1 t)))
              (emacs-lisp-mode)
              (mon-g2be -1)
              (insert (format ";; (mon-map-obarray-symbol-plist-props '%S)" w-prop-sym))
              (display-buffer mmospp-bfr t)))))
    mmospp-bfr))
;;
;;; :TEST-ME (mon-map-obarray-symbol-plist-props 'permanent-local)
;;; :TEST-ME (mon-map-obarray-symbol-plist-props 'permanent-local t)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Saturday May 30, 2009 @ 06:26.12 PM - by MON KEY>
(defun mon-escape-lisp-string-region (start end)
  "Escape special characters in the region as if a lisp string.
Insert backslashes in front of special characters (namely  `\' backslash,
`\"' double quote, `(' `)' parens in the region, according to the docstring escape 
requirements.\n
Region should only contain the characters actually comprising the string
supplied without the surrounding quotes.\n
:NOTE Don't evaluate on docstrings containing regexps and expect sensible
return values.\n
:ALIASED-BY `mon-string-escape-lisp-region'
:ALIASED-BY `mon-lisp-escape-region'\n
:SEE-ALSO `mon-unescape-lisp-string-region', `mon-escape-string-for-cmd',
`mon-exchange-slash-and-backslash', `mon-quote-sexp'.\n►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (search-forward "\\" nil t)
	(replace-match "\\\\" nil t))
      (goto-char start)
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" nil t))
      ;; :MON-ADDITIONS
      (goto-char start)
      (while (search-forward "(" nil t)
	(replace-match "\\\(" nil t))
      (goto-char start)
      (while (search-forward ")" nil t)
	(replace-match "\\\)" nil t)))))

;;; ==============================
(defun mon-unescape-lisp-string-region (start end)
  "Unescape special characters inregion from START to END.\n
This amounts to removing preceeding backslashes from characters they escape.\n
:NOTE region should only contain the characters actually comprising the string
without the surrounding quotes.\n
:ALIASED-BY `mon-string-unescape-lisp-region'
:ALIASED-BY `mon-lisp-unescape-region'\n
:SEE-ALSO `mon-escape-lisp-string-region', `mon-quote-sexp',
`mon-escape-string-for-cmd', `mon-exchange-slash-and-backslash'.\n►►►"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (search-forward "\\" nil t)
	(replace-match "" nil t)
	(forward-char)))))

;;; ==============================
;;; :COURTESY :FILE custom.el :WAS `custom-quote'
;;; :CHANGESET 1997
;;; :CREATED <Timestamp: #{2010-07-27T15:29:09-04:00Z}#{10302} - by MON KEY>
(defun mon-quote-sexp (sexp)
  "Quote SEXP as if by `quote' if it is not self quoting.\n
:EXAMPLE\n\n(mon-quote-sexp '(a list))\n
\(mon-quote-sexp #'\(lambda \(x\)\(x\)\)\)\n
\(mon-quote-sexp '\(lambda \(x\)\(x\)\)\)\n
\(mon-quote-sexp '\(a . b\)\)\n
\(mon-quote-sexp :test)\n
\(mon-quote-sexp #:is-not\)\n
:SEE-ALSO `mon-escape-lisp-string-region', `mon-unescape-lisp-string-region'.\n►►►"
  (if (or (memq sexp '(t nil))
	  (keywordp (intern-soft (format "%S" sexp) obarray))
	  (and (listp sexp) (eq (car sexp) '(lambda)))
	  (stringp sexp)
	  (numberp sexp)
	  (vectorp sexp)
          ;; :NOTE Commented in original defun in custom.el
          ;;  	  (and (fboundp 'characterp)
          ;;  	       (characterp sexp))
	  )
      sexp
    (list 'quote sexp)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-17T20:25:27-05:00Z}#{09515} - by MON KEY>
(defun mon-toggle-eval-length (&optional new-depth intrp)
  "Toggle or set a new value for `eval-expression-print-length' variable.\n
Return value of `eval-expression-print-length'.\n
When `eval-expression-print-length' is nil and NEW-DEPTH is non-nil or
called-interactively with prefix arg use length, else set length to 12 - the
default.\n
When `eval-expression-print-length' is non-nil set length to nil.
When `eval-expression-print-length' and NEW-DEPTH are non-nil set length.\n
:EXAMPLE\n\n\(mon-toggle-eval-length\)\n\n\(mon-toggle-eval-length 16\)\n
\(mon-toggle-eval-length nil t\)\n\n\(mon-toggle-eval-length 1 t\)\n
\(mon-toggle-eval-length\)\n
:SEE-ALSO `mon-toggle-truncate-line', `mon-naf-mode-toggle-restore-llm',
`print-length'.\n►►►"  
  (interactive "P\np")
  (let (mtel-nd)
    (if (and new-depth intrp)
        (setq mtel-nd 
              (read-number (concat ":FUNCTION `mon-toggle-eval-length' "
                                   " -- new length for eval-expression-print-length: ")))
      (setq mtel-nd new-depth))
    (cond ((not eval-expression-print-length)
           (if mtel-nd 
               (setq eval-expression-print-length mtel-nd)
             (setq eval-expression-print-length 12)))
          (eval-expression-print-length 
           (if mtel-nd 
               (setq eval-expression-print-length mtel-nd)
             (setq eval-expression-print-length nil))))
    eval-expression-print-length))
;;
;;; :TEST-ME (mon-toggle-eval-length)
;;; :TEST-ME (mon-toggle-eval-length 16)
;;; :TEST-ME (mon-toggle-eval-length nil t)
;;; :TEST-ME (mon-toggle-eval-length 1 t)
;;; :TEST-ME (mon-toggle-eval-length)
;;
;;; eval-expression-print-length

;;; ==============================
;;; :CHANGESET 2233 <Timestamp: #{2010-11-06T20:47:12-04:00Z}#{10446} - by MON KEY>
;;; :CREATED: <Timestamp: #{2009-10-20T15:56:02-04:00Z}#{09432} - by MON>
(defun mon-pretty-print-sexp-at-point (start end &optional CL->downcase);; intrp)
  "Pretty print the region in buffer. Do not move point.\n
 When CL->DOWNCASE is non-nil it is used to clean CL that is `UPCASE'd.\n
:NOTE This function is meant for interactive use only.\n
:SEE-ALSO `mon-princ-cb', `mon-eval-sexp-at-point', `mon-eval-expression',
`mon-eval-print-last-sexp', `mon-toggle-eval-length'.\n►►►"
  (interactive "r\nP")
  ;; (interactive "r\nP\nip")
  ;; (and (not intrp)
  ;;      (or (null start)
  ;;          (null end))
  ;;      (or (not (integer-or-marker-p start))
  ;;          (not (integer-or-marker-p end)))
  ;;      (or (and (use-region-p)
  ;;               (setq start (region-beginning)
  ;;                     end   (region-end)))
  ;;          (error (concat ":FUNCTON `mon-pretty-print-sexp-at-point' "
  ;;                         "-- with region START to END in funky state " 
  ;;                         "unable to satisfy `use-region-p'"))))
  (let ((fw  (if CL->downcase
                 "(save-excursion (newline) (princ (downcase (pp '("
               "(save-excursion (newline) (princ (pp '("))
        (bw  (if CL->downcase
                 "))) (current-buffer)))"
               ")) (current-buffer)))"))
        (mk1 (make-marker))
        (mk2 (make-marker))
        (mk3 (make-marker))
        (mk4 (make-marker)))
    (unwind-protect
        (save-restriction    
          (narrow-to-region start end)
          (goto-char start)
          (set-marker mk3 start)
          (set-marker mk4 end)
          (mon-wrap-with (concat fw "\n") (concat "\n" bw) t)
          (search-forward-regexp (concat "\n" bw))
          (set-marker mk2 (point))
          (eval (preceding-sexp))
          (search-backward-regexp (concat "\n" bw) nil t)
          (set-marker mk1 (point))
          (delete-region mk1 mk2)
          (search-backward-regexp (concat fw "\n") nil t)
          (set-marker mk1 (point))
          (search-forward-regexp (concat fw "\n") nil t)
          (set-marker mk2 (point))
          (delete-region mk1 mk2)
          (delete-region mk3 mk4)
          (forward-sexp)
          (set-marker mk2 (point))
          (backward-sexp)
          (set-marker mk1 (point))
          (delete-region mk1 (1+ mk1))
          (delete-region mk2 (1- mk2))
          (backward-delete-char 1))
      (mapc #'(lambda (mrk)
                (set-marker mrk nil))
            (list mk1 mk2 mk3 mk4)))))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 02:13.22 PM - by MON KEY>
(defun mon-princ-cb (&optional insrtp intrp)
  "Wrap region in a princ->current-buffer to eval and print newline\\result
after point.\n
:SEE-ALSO `mon-eval-sexp-at-point', `mon-pretty-print-sexp-at-point', `mon-eval-expression',
`mon-eval-print-last-sexp'.\n►►►"
  (interactive "i\np")
  (save-excursion
    ;; (let (sexp-pnt
    (mon-wrap-text "(progn (newline) (princ '\n" "\n(current-buffer)))" insrtp intrp)))

;;; ==============================
;;; :PREFIX "mesap-"
;;; :NOTE Consider using: (pp-eval-last-sexp t)
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 03:14.44 PM - by MON KEY>
(defun mon-eval-sexp-at-point ()
  "Evaluate S-expression at point print commented result on newline.\n
Return point after commented result. Best on trivial expressions.\n
:EXAMPLE\n\(+ 1 3)\n;;;=> 4\n^point^\n
:SEE-ALSO `mon-princ-cb', `mon-pretty-print-sexp-at-point', `mon-eval-expression',
`mon-eval-print-last-sexp', `mon-eval-sexp-at-point', `mon-toggle-eval-length'.
►►►"
  (interactive)
  (let* ((mesap-wrap (sexp-at-point))
	 (mesap-val (eval mesap-wrap))      
	 ;; :WAS (mesap-comnt "\n;;;=>")
         (mesap-comnt "\n;;;=> ")
	 (mesap-comn-sxp (format "%S%s%S"  mesap-wrap mesap-comnt mesap-val))
	 (mesap-bnds))
    (save-excursion
      (if (not (eobp))
	  (forward-line)
	(newline))
      (insert mesap-comn-sxp))
    (setq mesap-bnds (bounds-of-thing-at-point 'sexp))
    ;; :WAS (delete-region (car mesap-bnds) (cdr mesap-bnds))
    (setq mesap-wrap (delete-and-extract-region (car mesap-bnds) (cdr mesap-bnds)))
    (when (mon-line-bol-is-eol)
      (delete-char 1))
    ;; :WAS (search-forward-regexp "^;;;=> .*$" nil t)
    (search-forward-regexp (format "^;;;=> %S" mesap-val) nil t)
    mesap-val))
;;
(defun mon-eval-print-last-sexp ()
  "Like `eval-print-last-sexp' but does not move point.\n
:SEE-ALSO `mon-eval-expression', `mon-eval-sexp-at-point',
`mon-pretty-print-sexp-at-point', `mon-princ-cb', `mon-toggle-eval-length',
`pp-eval-last-sexp'.\n►►►"
  (interactive)
  (save-excursion
    (eval-print-last-sexp)))
;;
;;; :TEST-ME (+ 1 3) (mon-eval-print-last-sexp)

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 25, 2009 @ 12:59.22 PM - by MON KEY>
(defun mon-eval-expression (eval-expression-arg &optional eval-expression-insert-value)
  "This is `eval-expression' with EVAL-EXPRESSION-INSERT-VALUE defaulted to t.\n
Gets us eval-expression automatically inserted into current-buffer.\n
:SEE-ALSO `mon-eval-print-last-sexp', `mon-eval-sexp-at-point',
`mon-pretty-print-sexp-at-point', `mon-princ-cb', `mon-toggle-eval-length'.\n►►►"
  (interactive
   (list (let ((minibuffer-completing-symbol t))
	   (read-from-minibuffer "Eval: "
				 nil read-expression-map t
				 'read-expression-history))
         ;; :NOTE Only point of this is to set current-prefix-arg default to t.
         ;; :CHANGED
         ;; current-prefix-arg)) 
	 t))
  (if (null eval-expression-debug-on-error)
      (setq values (cons (eval eval-expression-arg) values))
    (let ((old-value (make-symbol "t")) new-value)
      (let ((debug-on-error old-value))
	(setq values (cons (eval eval-expression-arg) values))
	(setq new-value debug-on-error))
      (unless (eq old-value new-value)
	(setq debug-on-error new-value))))
  (let ((print-length eval-expression-print-length)
	(print-level  eval-expression-print-level))
    (if eval-expression-insert-value
	(with-no-warnings
          (let ((standard-output (current-buffer)))
            (prin1 (car values))))
      (prog1
          (prin1 (car values) t)
        (let ((str (eval-expression-print-format (car values))))
          (if str (princ str t)))))))

;;; ==============================
;;; :TODO `mon-semnav-up' and `mon-extend-selection' need default key bindings.
;;; :COURTESY Nikolaj Schumacher :VERSION 2008-10-20
(defun mon-extend-selection (arg &optional incremental)
  "Mark symbol surrounding point.\n
Subsequent calls mark higher levels of sexps.\n
:SEE-ALSO `mon-semnav-up'.\n►►►"
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
	 (or (and transient-mark-mode mark-active)
	     (eq last-command this-command))))
  (if incremental
      (progn
        (mon-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (mon-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;;; ==============================
;;; :COURTESY Nikolaj Schumacher  :VERSION 2008-10-20
;;; :SEE (URL `http://xahlee.org/emacs/syntax_tree_walk.html')
;;; :CREATED <Timestamp: Sunday January 18, 2009 - by MON KEY>
(defun mon-semnav-up (arg)
  "
:SEE-ALSO `mon-extend-selection'.\n►►►"
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;;; ==============================
;;; :PREFIX "msc-"
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :WAS `show-columns'
;;; :CREATED <Timestamp: 2009-08-04-W32-2T19:07:47-0400Z - by MON KEY>
(defun mon-show-columns ()
  "Show a numbered column display above the current line.\n
With ARG, begin column display at current column, not at left margin.\n
:SEE-ALSO `mon-indent-lines-from-to-col', `mon-line-strings-pipe-to-col',
`mon-string-fill-to-col', `mon-comment-lisp-to-col',
`mon-mysql-cln-pipes-map-col-field', `mon-mysql-csv-map-col-field',
`mon-mysql-get-field-col', `mon-rectangle-columns',
`mon-rectangle-sum-column', `show-point-mode'.\n►►►"
  (interactive)
  (let* ((msc-lead-blnks
          (if (null current-prefix-arg) 0 (current-column)))
         (msc-col-dsply (concat (make-string msc-lead-blnks ?\ )
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789."
                                 "123456789.123456789.123456789.")))
    (save-excursion
      (forward-line -1)
      (momentary-string-display
       (substring msc-col-dsply
                  0 (min (1- (window-width)) (length msc-col-dsply)))
       (point)))))
;;
;;; :TEST-ME (call-interactively 'mon-show-columns)


;;; ==============================
;;; :PREFIX "mnae-"
;;; :COURTESY Dave Pearson <davep@davep.org> :HIS nukneval.el :WAS `nuke-and-eval'
;;; :VERSION 1.1 Copyright 2002 GPL V2 (URL `http://www.davep.org/emacs/nukneval.el')
;;; :NOTE Nukes and reevaluates an elisp buffer. 
;;; ==============================
(defun mon-nuke-and-eval ()
  "Attempt to cleanly re-evaluate a buffer of elisp code.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-compile-when-needed' `mon-load-or-alert',
`mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-after-mon-utils-loadtime'.\n►►►"
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (loop for form = (condition-case nil
                         (read (current-buffer))
                       (error nil))
          while form
          do (let ((mnae-typ (car form))
                   (mnae-nm (cadr form)))
               (cond
                ((memq mnae-typ '(defun defun* defsubst defalias defmacro))
                 (fmakunbound mnae-nm))
                ((memq mnae-typ '(defvar defparameter defconst defcustom))
                 (makunbound mnae-nm))))))
  (eval-buffer))

;;; ==============================
;;; :COURTESY Dave Pearson <davep@davep.org> :HIS unbind.el
;;; :VERSION 1.3 - Copyright 2002 - GPL v2 (URL `http://www.davep.org/emacs/unbind.el')
;;; :WAS `unbind-command'    -> `mon-unbind-command'
;;; :WAS `unbind-variable'   -> `mon-unbind-variable'
;;; :WAS `unbind-defun'      -> `mon-unbind-defun'
;;; :WAS `unbind-symbol'     -> `mon-unbind-symbol'
;;; :WAS `unbind-function'   -> `mon-unbind-function'
;;; :NOTE Commands for unbinding things.
;;; ==============================
;;;###autoload
(defun mon-unbind-defun ()
  "Unbind the `defun' near `point' in `current-buffer'.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-compile-when-needed' `mon-load-or-alert',
`mon-byte-compile-and-load', `mon-dump-object-to-file', `mon-nuke-and-eval',
`mon-after-mon-utils-loadtime'.\n►►►"
  (interactive)
  (save-excursion
    (if (and (beginning-of-defun) (looking-at "(defun"))
        (fmakunbound (cadr (read (current-buffer))))
      (error ":FUNCTION `mon-unbind-defun' -- no defun found near point"))))
;; :KEEP-WITH-ABOVE
;;;###autoload
(defun mon-unbind-symbol (symbol)
  "Totally unbind SYMBOL.\n
Includes unbinding function binding, variable binding, and property list.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive "S:FUNCTION `mon-unbind-symbol' -- unbind symbol: ")
  (fmakunbound symbol)
  (makunbound symbol)
  (setf (symbol-plist symbol) nil))
;; :KEEP-WITH-ABOVE
;;;###autoload
(defun mon-unbind-function (fncn-symbol)
  "Remove the function binding of FNCN-SYMBOL.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive "aFunction: ")
  (fmakunbound fncn-symbol))
;; :KEEP-WITH-ABOVE
;;;###autoload
(defun mon-unbind-command (cmd-symbol)
  "Remove the command binding of CMD-SYMBOL.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive "CCommand: ")
  (fmakunbound cmd-symbol))
;; :KEEP-WITH-ABOVE
;;;###autoload
(defun mon-unbind-variable (var-symbol)
  "Remove the variable binding of VAR-SYMBOL.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive (list 
                (completing-read "Variable: "
                                 (loop for s being the symbols
                                    when (boundp s) collect (list (symbol-name s))))))
  (makunbound (if (stringp var-symbol) (intern var-symbol) var-symbol)))
;;; 
;; 
;;; ==============================
;;; :END Pearson's commands for unbinding things.
;;; ==============================

;;; ==============================
;;; :COURTESY Thierry Volpiatto :WAS `tv-dump-object-to-file'
;;; :SEE (URL `http://lists.gnu.org/archive/html/emacs-devel/2009-09/msg00846.html')
;;; :CREATED <Timestamp: #{2009-10-01T12:31:29-04:00Z}#{09404} - by MON>
;;;###autoload
(defun mon-dump-object-to-file (obj file)
  "Save symbol object OBJ to the byte compiled version of FILE.\n
OBJ can be any lisp object, list, hash-table, etc.
FILE is an elisp file with ext *.el.
Loading the *.elc file will re-institute object.\n
:NOTE This function utilizes an documented feature of `eval-when-compile'. It
can be interesting way to save a persistent elisp object. Using `setf' combined
with `eval-when-compile' is a convenient way to save lisp objects like
hash-table.
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  ;; (require 'cl) ;; Be sure we use the CL version of `eval-when-compile'.
  (if (file-exists-p file)
      (error (concat ":FUNCTION `mon-dump-object-to-file' "
                     "-- already existent FILE: %s") file)
    (with-temp-file file
      (erase-buffer)
      (let* ((str-obj (symbol-name obj))
             (fmt-obj (format "(setq %s (eval-when-compile %s))" str-obj str-obj)))
        (insert fmt-obj)))
    (byte-compile-file file)
    (delete-file file)
    (message (concat ":FUNCTION `mon-dump-object-to-file' "
                     "-- OBJ `%s' dumped to FILE: %sc")
             obj file)))

;;; ==============================
;;;###autoload
(defun mon-byte-compile-and-load ()
  "Byte compile and load the current .el file.\n
The function `byte-compile-file' was only easily accesible from the menu.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive)
  (byte-compile-file buffer-file-name t))


;;; ==============================
;;; :PREFIX "mcwn-"
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el
;;; :SEE (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :WAS `ff/compile-when-needed' -> `mon-compile-when-needed'
;;; ==============================
;;;###autoload
(defun mon-compile-when-needed (comp-fname)
  "Compile the given file with COMP-FNAME only if needed.\n
Add .el if required, and use `load-path' to find it.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (if (not (string-match "\.el$" comp-fname))
      (mon-compile-when-needed (concat comp-fname ".el"))
    (mapc #'(lambda (dir)
              (let* ((mcwn-src (concat dir "/" comp-fname)))
                (when (file-newer-than-file-p mcwn-src (concat mcwn-src "c"))
                  (message (concat ":FUNCTION `mon-compile-when-needed' "
                                   (if (let ((byte-compile-verbose nil))
                                         (condition-case nil
                                             (byte-compile-file mcwn-src)
                                           (error nil)))
                                       "-- compiled: %s"
                                     "-- failed compilation of: %s"))
                           mcwn-src))))
          load-path)))

;;; ==============================
;;; :PREFIX "mloa-"
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :WAS `ff/load-or-alert'
;;; This is useful when using the same .emacs in many places.
;;; :MODIFICATIONS <Timestamp: #{2010-03-05T16:17:17-05:00Z}#{10095} - by MON KEY>
;;;###autoload
(defun mon-load-or-alert (lib-name &optional compile-when-needed)
  "Try to load the specified file LIB-NAME.\n
When optional arg COMPILE-WHEN-NEEDED `mon-compile-when-needed' the file
LIB-NAME first. Insert a warning message in a load-warning buffer in case of
failure.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (when compile-when-needed (mon-compile-when-needed lib-name))
  (if (load lib-name t nil) 
      t
      (let ((mloa-bfr (get-buffer-create "*MON-LOADING-WARNINGS*")))
        (display-buffer mloa-bfr)
        (set-buffer mloa-bfr)
        (insert 
         (propertize "Warning: :FUNCTION `mon-load-or-alert' -- " 
                     'face 'font-lock-warning-face 'fontified t)
         " could not load '" lib-name "'\n")
        (fit-window-to-buffer (get-buffer-window mloa-bfr))
        (set-buffer-modified-p nil))
      nil))

;;; ==============================
(provide 'mon-utils)
;;; ==============================

(eval-after-load "mon-utils" '(mon-utils-require-features-at-loadtime))

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ==============================
;;; mon-utils.el ends here
;;; EOF

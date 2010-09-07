;;; mon-utils.el --- common utilities and BIG require for other of MON packages
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2008-2010 MON KEY. All rights reserved.
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
;; `scratch', `switch-to-messages',
;; `scroll-down-in-place', `scroll-up-in-place', `mon-kill-appending',
;; `mon-kill-completions', `mon-flip-windows', `mon-twin-horizontal',
;; `mon-twin-vertical', `mon-get-face-at-point', `mon-toggle-menu-bar',
;; `mon-append-to-register', `mon-append-to-buffer', `mon-region-position',
;; `mon-region-length', `mon-region-unfill', `mon-region-capitalize',
;; `mon-region-reverse', `mon-toggle-trunc', `mon-inhibit-read-only',
;; `mon-inhibit-modification-hooks', `mon-inhibit-point-motion-hooks',
;; `mon-toggle-read-only-point-motion', `mon-wrap-selection',
;; `mon-wrap-text', `mon-wrap-with', `mon-choose-from-menu',
;; `mon-match-at-point', `mon-spacep', `mon-spacep-not-bol',
;; `mon-spacep-is-bol', `mon-spacep-is-after-eol',
;; `mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
;; `mon-spacep-first', `mon-line-bol-is-eol',
;; `mon-line-previous-bol-is-eol', `mon-line-next-bol-is-eol',
;; `mon-line-eol-is-eob', `mon-line-end-or-code-end', `mon-line-get-next',
;; `mon-line-count-region', `mon-line-count-matchp', `mon-line-length-max',
;; `mon-is-digit', `mon-is-letter', `mon-is-alphanum',
;; `mon-is-digit-simp', `mon-is-letter-simp', `mon-is-alphanum-simp',
;; `mon-string-justify-left', `mon-string-to-sequence',
;; `mon-string-from-sequence', `mon-string-alpha-list', `mon-string-index',
;; `mon-string-position', `mon-string-has-suffix', `mon-string-ify-list',
;; `mon-string-split-on-regexp', `mon-string-sub-old->new',
;; `mon-string-split-line', `mon-string-ify-current-line',
;; `mon-word-get-list-in-buffer', `mon-word-get-next',
;; `mon-word-reverse-region', `mon-word-iterate-over',
;; `mon-word-count-analysis', `mon-word-count-occurrences',
;; `mon-word-count-region', `mon-word-count-chars-region',
;; `mon-rectangle-columns', `mon-rectangle-sum-column',
;; `mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
;; `mon-rectangle-downcase', `mon-rectangle-upcase',
;; `mon-rectangle-capitalize', `mon-line-test-content', `mon-get-text-properties-category',
;; `mon-view-help-source', `mon-index-elisp-symbol',
;; `mon-plist-keys', `mon-plist-remove!', `mon-plist-remove-consing',
;; `mon-plist-remove-if', `mon-list-all-properties-in-buffer',
;; `mon-nuke-text-properties-buffer', `mon-remove-text-property',
;; `mon-remove-single-text-property', `mon-nuke-text-properties-region',
;; `mon-get-text-properties-region-to-kill-ring', `mon-nuke-overlay-buffer', 
;; `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
;; `mon-flatten', `mon-combine', `mon-recursive-apply', `mon-mismatch',
;; `mon-escape-lisp-string-region', `mon-unescape-lisp-string-region',
;; `mon-princ-cb', `mon-eval-sexp-at-point', `mon-eval-print-last-sexp',
;; `mon-extend-selection', `mon-semnav-up', `mon-eval-expression',
;; `mon-nuke-and-eval', `mon-unbind-defun', `mon-unbind-symbol',
;; `mon-unbind-function', `mon-unbind-command', `mon-unbind-variable',
;; `mon-byte-compile-and-load', `mon-compile-when-needed',
;; `mon-load-or-alert', `mon-cmd', `mon-terminal', `mon-string-to-symbol'
;; `mon-line-find-duplicates', `mon-test-keypresses', `mon-line-strings-one-list',
;; `mon-line-strings-to-list', `mon-line-strings-to-list-TEST',
;; `mon-line-string-rotate-name', `mon-line-string-rotate-namestrings',
;; `mon-line-string-unrotate-namestrings', `mon-symbol-to-string',
;; `mon-line-string-rotate-namestrings-combine', `mon-intersection',
;; `mon-sublist', `mon-sublist-gutted', `mon-map-append',
;; `mon-string-chop-spaces', `mon-maptree', `mon-transpose',
;; `mon-dump-object-to-file'`mon-string-upto-index',
;; `mon-string-after-index', `mon-line-strings-bq-qt-sym-bol',
;; `mon-get-proc-w-name', `mon-get-sys-proc-list', `mon-insert-sys-proc-list',
;; `mon-generate-prand-id', `mon-generate-prand-seed',
;; `mon-sha1-region', 
;; `mon-escape-string-for-cmd', `mon-line-strings-qt-region',
;; `mon-buffer-name->kill-ring', `mon-make-a-pp',
;; `mon-string-to-hex-string', `mon-generate-WPA-key',
;; `mon-async-du-dir', `mon-make-shell-buffer', `mon-shell',
;; `mon-line-strings-pipe-bol', `mon-line-strings-indent-to-col',
;; `mon-line-strings-region', `mon-line-indent-from-to-col', 
;; `mon-get-system-specs', `mon-string-fill-to-col',
;; `mon-line-strings-pipe-to-col', `mon-line-strings',
;; `mon-get-process', `mon-toggle-eval-length',
;; `mon-line-string-insert-chars-under', `mon-alphabet-as-type',
;; `mon-get-env-vars-strings',  `mon-get-env-vars-symbols',
;; `mon-get-env-vars-emacs', `mon-get-emacsd-paths',
;; `mon-string-replace-char', `mon-string-to-hex-list',
;; `mon-buffer-exists-so-kill', `mon-string-wonkify',
;; `mon-after-mon-utils-loadtime', `mon-line-count-buffer',
;; `mon-g2be', `mon-string-sort-descending',
;; `mon-with-inhibit-buffer-read-only-TEST', `mon-remove-if',
;; `mon-string-to-hex-list-cln-chars',
;; `mon-line-dolines-setup-TEST', `mon-line-dolines-TEST',
;; `mon-map-obarray-symbol-plist-props', `mon-image-verify-type',
;; `mon-with-buffer-undo-disabled-TEST',
;; `mon-line-move-n', `mon-line-move-prev', `mon-line-move-next',
;; `mon-abort-recursive-edit', `mon-rotate-ascii-cursor',
;; `mon-get-syntax-class-at', `mon-string-spread', `mon-quote-sexp',
;; `mon-delq-cons', `mon-list-proper-p', `mon-list-make-unique', 
;; `mon-list-match-tails', `mon-list-reorder', `mon-maybe-cons',
;; `mon-gensym-counter-randomizer', `mon-gensym-counter-randomizer-TEST',
;; `mon-list-nshuffle' `mon-list-nshuffle-TEST', `mon-looking-back-p',
;; `mon-delq-dups', `mon-make-random-state', `mon-next-almost-prime',
;; `mon-list-shuffle-safe', `mon-bool-vector-pp', `mon-get-bit-table',
;; `mon-abort-autosave-when-fucked', `mon-get-buffer-hidden',
;; `mon-map-windows->plist', `mon-list-merge',
;; FUNCTIONS:◄◄◄
;; 
;; MACROS:
;; `mon-foreach', `mon-for',  `mon-loop', `mon-moveq',
;; `mon-line-dolines', `defconstant', `defparameter',
;; `mon-with-buffer-undo-disabled', `mon-get-face-at-posn',
;; `mon-buffer-exists-p', `mon-check-feature-for-loadtime',
;; `mon-with-inhibit-buffer-read-only', `mon-gensym',
;; `mon-with-gensyms',  `mon-nshuffle-vector', `mon-mapcar'
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-utils-post-load-requires*', `*mon-ascii-cursor-state*', `*mon-bit-table*',
;;
;; ALIASED:
;; `mon-scratch'                     -> `scratch'
;; `mon-string-combine-and-quote'    -> `combine-and-quote-strings'
;; `mon-string-split-and-unquote'    -> `split-string-and-unquote'
;; `mon-string->symbol'              -> `mon-string-to-symbol'
;; `mon-symbol->string'              -> `mon-symbol-to-string'
;; `mon-string-from-symbol'          -> `mon-symbol-to-string'
;; `mon-string<-symbol'              -> `mon-symbol-to-string'
;; `mon-switch-to-messages'          -> `switch-to-messages'
;; `mon-indent-lines-from-to-col'    -> `mon-line-indent-from-to-col'
;; `mon-replace-char-in-string'      -> `mon-string-replace-char'
;; `mon-remove-char-in-string'       -> `mon-string-replace-char'
;; `mon-generate-wonky'              -> `mon-string-wonkify'
;; `mon-line-keep-match'             -> `keep-lines'
;; `mon-line-delete-match'           -> `flush-lines'
;; `mon-line-count-match'            -> `how-many'
;; `mon-make-list-alphabet'          -> `mon-alphabet-as-type'
;; `mon-string-set-char-at-idx'      -> `store-substring'     
;; `mon-string-insert-string-at-idx' -> `store-substring'
;; `mon-kill-ring-save-w-props'      -> `mon-get-text-properties-region-to-kill-ring'                  
;; `mon-sequence-to-string'          -> `mon-string-from-sequence'
;; `mon-seq->string'                 -> `mon-string-from-sequence'
;; `mon-buffer-get-w-mode'           -> `mon-get-buffer-w-mode'
;; `mon-buffer-get-word-count'       -> `mon-word-count-occurrences'
;; `proper-list-p'                   -> `mon-list-proper-p'
;; `with-gensyms'                    -> `mon-with-gensyms'
;; `mon-get-next-almost-prime'       -> `mon-next-almost-prime'
;; `next-almost-prime'               -> `mon-next-almost-prime'
;; `nshuffle-vector'                 -> `mon-nshuffle-vector'
;; `delq-dups'                       -> `mon-delq-dups' 
;; `mon-get-hidden-buffers'          -> `mon-get-buffer-hidden'
;; `mon-byte-table-bits'             -> `mon-get-bit-table'
;; `mon-bit-table-bits'              -> `mon-get-bit-table'
;; `mon-bool-vector-to-list'         -> `mon-bool-vector-pp'
;; `buffer-exists-p'                 -> `mon-buffer-exists-p'
;; `debug-on-error-toggle'           -> `toggle-debug-on-error'
;; `mon-get-window-plist'            -> `mon-map-windows->plist'
;; `mon-help-hidden-buffers'         -> `mon-get-buffer-hidden'
;; `mon-merge-list'                  -> `mon-list-merge'
;; `mon-buffer-do-with-undo-disabled' -> `mon-with-buffer-undo-disabled'
;; `mon-get-text-properties-region->kill-ring' -> `mon-get-text-properties-region-to-kill-ring'
;; DEPRECATED:
;; `mon-string-from-sequence2' ;; :REMOVED
;;
;; RENAMED:
;; `mon-trunc'                       -> `mon-toggle-truncate-line'
;; `mon-stringify-list'              -> `mon-string-ify-list'
;; `mon-split-string-line'           -> `mon-string-split-line'
;; `mon-what-face'                   -> `mon-get-face-at-point'
;; `mon-line-strings-to-list-*test*' -> `mon-line-strings-to-list-TEST'
;; `scroll-up-in-place'              -> `mon-scroll-up-in-place'
;; `scroll-down-in-place'            -> `mon-scroll-down-in-place'
;; `mon-kill-ring-save-w-props'      -> `mon-get-text-properties-region-to-kill-ring'
;; `boxcutter-verify-image-type'     -> `mon-image-verify-type'
;;
;; MOVED:
;; `mon-coerce->char'                             -> mon-empty-registers.el
;; `mon-decode-meta-key-event'                    -> mon-empty-registers.el
;; `mon-catch-meta-key'                           -> mon-empty-registers.el
;; `mon-index-elisp-symbol'                       -> mon-doc-help-utils.el
;; `mon-cmd'                                      <- default-start-loads.el
;; `mon-terminal'                                 <- default-start-loads.el
;; `boxcutter-verify-image-type'                  <- mon-boxcutter.el
;; `mon-list-all-properties-in-buffer'            <- mon-text-property-utils.el
;; `mon-get-text-properties-region-to-kill-ring'  <- mon-text-property-utils.el
;; `mon-nuke-text-properties-buffer'              <- mon-text-property-utils.el
;; `mon-remove-text-property'                     <- mon-text-property-utils.el 
;; `mon-get-face-at-posn'                         <- mon-text-property-utils.el
;; `mon-get-face-at-point'                        <- mon-text-property-utils.el
;; `mon-remove-single-text-property'              <- mon-text-property-utils.el
;; `mon-nuke-text-properties-region'              <- mon-text-property-utils.el
;; `mon-get-text-properties-category'             <- mon-text-property-utils.el
;; `mon-nuke-overlay-buffer'                      <- mon-text-property-utils.el
;;
;; REQUIRES:
;;
;; TODO:
;;
;; NOTES:
;; `mon-maptree' uses `flet' cl--every                  -> `every'
;; `mon-get-process' uses `flet' cl--find-if            -> `find-if'
;; `mon-combine' uses `flet' cl--mapcan                 -> `mapcan'
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
;; Copyright © 2008-2010 MON KEY 
;;; ===================================

;;; CODE:


;;; ==============================
;;; :CHANGESET 2088
;;; :CREATED <Timestamp: #{2010-08-25T18:54:20-04:00Z}#{10343} - by MON KEY>
(unless (and (intern-soft "debug-on-error-toggle")
             (fboundp 'debug-on-error-toggle))
  (defalias 'debug-on-error-toggle 'toggle-debug-on-error))

;;; ==============================
(eval-when-compile (require 'cl)) ;; `mon-word-iterate-over', `mon-loop'
;;
;; (eval-when-compile (require 'mon-cl-compat nil t))
;;
(declare-function w32-shell-execute "w32fns.c")

(require 'mon-text-property-utils)
(require 'bytecomp)

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
;; (when (and (intern-soft "IS-MON-SYSTEM-P")
;;            (bound-and-true-p IS-MON-SYSTEM-P))
;;   (require 'mon-regexp-symbols)
;;   (require 'mon-time-utils)
;;   (require 'mon-replacement-utils) ;; :BEFORE :FILE mon-dir-utils.el mon-insertion-utils.el
;;   (require 'mon-dir-locals-alist)
;;   ;; :FILE mon-dir-utils.el :LOADS mon-dir-locals-alist.el 
;;   ;;                        :REQUIRES  mon-hash-utils.el, mon-replacement-utils.el, 
;;   ;;                                   mon-css-color.el, mon-rename-image-utils.el
;;   (require 'mon-dir-utils)
;;   (require 'mon-cifs-utils)
;;   (require 'mon-insertion-utils)
;;   (require 'mon-testme-utils)
;;   (require 'naf-mode-insertion-utils)
;;   (require 'mon-wget-utils) ;; :FILE mon-get-mon-packages.el :AFTER-LOAD
;;   (require 'mon-url-utils)
;;   (require 'mon-hash-utils)
;;   (require 'mon-doc-help-utils)
;;   (require 'mon-doc-help-CL)
;;   (require 'mon-tramp-utils)
;;   (require 'naf-skeletons)
;;   (require 'naf-mode) ;; :NOTE Should be required already in :FILE mon-default-start-loads.el
;;   (require 'ebay-template-mode)
;;   (require 'mon-empty-registers)
;;   (require 'mon-iptables-vars)
;;   (require 'mon-iptables-regexps)
;;   (require 'mon-mysql-utils)
;;   (require 'naf-mode-sql-skeletons nil t) ;; Load here instead of from :FILE naf-mode.el
;;   )



;;; ==============================
;;; :TODO extend this list into an alist with elements containing 
;;; plist keys :requires :required-by :optional etc.
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T16:54:51-04:00Z}#{10361} - by MON KEY>
(defvar *mon-utils-post-load-requires* nil
  "List of features loaded by feature mon-utils.el\n
:CALLED BY `mon-utils-require-features-at-loadtime'\n
:SEE-ALSO `mon-after-mon-utils-loadtime'.\n►►►")
;;
(unless (and (intern-soft "*mon-utils-post-load-requires*")
             (bound-and-true-p *mon-utils-post-load-requires*))
  (setq *mon-utils-post-load-requires*
        '(mon-regexp-symbols
          mon-time-utils
          ;; :FILE mon-replacement-utils.el :BEFORE :FILE mon-dir-utils.el mon-insertion-utils.el
          mon-replacement-utils 
          ;; :FILE mon-dir-utils.el :LOADS mon-dir-locals-alist.el 
          ;; :REQUIRES  mon-hash-utils.el, mon-replacement-utils.el, 
          ;;            mon-css-color.el, mon-rename-image-utils.el
          mon-dir-locals-alist  
          mon-dir-utils
          mon-cifs-utils
          mon-insertion-utils
          mon-testme-utils
          naf-mode-insertion-utils
          ;; :FILE mon-get-mon-packages.el :AFTER-LOAD :FILE mon-wget-utils.el
          mon-wget-utils 
          mon-url-utils
          mon-hash-utils
          mon-doc-help-utils
          mon-doc-help-CL
          mon-tramp-utils
          naf-skeletons
          ;; :NOTE Should be required already in :FILE mon-default-start-loads.el
          naf-mode 
          ebay-template-mode
          mon-empty-registers
          mon-iptables-vars
          mon-iptables-regexps
          mon-mysql-utils)))

;;; ==============================
;;; :NOTE `mon-get-mon-emacsd-paths' is an interpreted function in.
;;; :FILE mon-default-start-loads.el
;;; :CREATED <Timestamp: #{2010-03-08T19:29:59-05:00Z}#{10102} - by MON KEY>
(when (fboundp 'mon-get-mon-emacsd-paths)
  (fset 'mon-get-emacsd-paths 
        (symbol-function 'mon-get-mon-emacsd-paths)))

;;; ==============================
;;; :NOTE Adding an optional arg W-SIGNAL-ERROR doesn't really make sense as the
;;; check for filename is made explicit with this marcro.
;;; :CREATED <Timestamp: #{2010-02-24T15:15:25-05:00Z}#{10083} - by MON KEY>
(defmacro mon-check-feature-for-loadtime (feature-as-symbol &optional req-w-filname)
  "Test if library FEATURE-AS-SYMBOL is in load path if so require feature.\n
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
  (let ((mcffl (make-symbol "mcffl"))
        (mcffl-chk (make-symbol "chk")))
    `(let (,mcffl ,mcffl-chk)
       (setq ,mcffl (locate-library (format "%s" ,feature-as-symbol)))
       (if ,mcffl
           (unless (featurep ,feature-as-symbol)
             (if ,req-w-filname
                 (require ,feature-as-symbol ,mcffl t) ;;,(if w-signal-error nil t))
               (require ,feature-as-symbol nil t))) ;;,(if w-signal-error t nil))))
         (error 
          (concat ":MACRO `mon-check-feature-for-loadtime' "
                  "-- arg FEATURE-AS-SYMBOL does not find a feature or file: %s")
          ,feature-as-symbol)))))
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
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T17:05:28-04:00Z}#{10361} - by MON KEY>
(defun mon-utils-require-features-at-loadtime ()
  "Evaluated as the last form in mon-utils.el\n
Evaluates macro `mon-check-feature-for-loadtime' for each feature listed in
varaible `*mon-utils-post-load-requires*'\n
:SEE-ALSO `mon-after-mon-utils-loadtime'.\n►►►"
  ;; (when (and (intern-soft "IS-MON-SYSTEM-P")
  ;; 	       (bound-and-true-p IS-MON-SYSTEM-P))
  (let (did-rqr)
    (dolist (req *mon-utils-post-load-requires*)
      (let ((rqrd (mon-check-feature-for-loadtime req)))
        (when rqrd (push (symbol-name rqrd) did-rqr))))
    (when (consp did-rqr)
      (mapc #'(lambda (msg) 
                (message (concat ":FUNCTION `mon-utils-require-features-at-loadtime' "
                                 "-- :FEATURE mon-utils :REQUIRED :FEATURE" msg " on load")))
            did-rqr)))
  (when (and (intern-soft "IS-MON-SYSTEM-P")
             (bound-and-true-p IS-MON-SYSTEM-P))
    ;; Load here instead of from :FILE naf-mode.el
    (require 'naf-mode-sql-skeletons nil t)))

;;; ==============================
;;; :TODO Build additional fncn/macro to populate docstrings at loadtime.
;;; :CREATED <Timestamp: #{2010-02-24T15:15:09-05:00Z}#{10083} - by MON KEY>
(defun mon-after-mon-utils-loadtime ()
  "List of packages and functions to load or eval.\n
These are brought into the current environment after mon-utils.el is loaded.\n
Called with \(eval-after-load \"mon-utils\" '\(mon-after-mon-utils-loadtime\)\)\n
Evaluated in :FILE mon-post-load-hooks.el\n
Evaluates the following functions per feature:
 `mon-bind-nefs-photos-at-loadtime'    <- mon-dir-utils 
 `mon-bind-cifs-vars-at-loadtime'      <- mon-cifs-utils
 `mon-tramp-utils-loadtime'            <- mon-tramp-utils
 `mon-set-register-tags-loadtime'      <- mon-empty-registers
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
:SEE-ALSO `after-load-alist', `mon-check-feature-for-loadtime',
`mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-run-post-load-hooks',
`mon-purge-cl-symbol-buffers-on-load', `mon-unbind-defun',
`mon-after-mon-utils-loadtime', `mon-compile-when-needed' `mon-load-or-alert',
`mon-byte-compile-and-load', `mon-dump-object-to-file', `mon-nuke-and-eval',
`mon-cl-compat-loadtime'.\n►►►"
  (progn 
    (mon-get-bit-table)
    (eval-after-load "naf-mode-faces"     '(mon-bind-naf-face-vars-loadtime t))
    (eval-after-load "mon-dir-utils"      '(mon-bind-nefs-photos-at-loadtime))
    (eval-after-load "mon-doc-help-utils" '(mon-help-utils-loadtime t))
    ;; :NOTE Moved (mon-help-utils-CL-loadtime t) -> `mon-run-post-load-hooks'
    (eval-after-load "mon-doc-help-CL"    '(mon-bind-mon-help-CL-pkgs-loadtime t))
    ;; :NOTE See docs `mon-bind-cifs-vars-at-loadtime' and notes at BOF
    ;; mon-cifs-utils.el for alternative application with args 
    ;; NO-MISC-PATH NO-MAP-MOUNT-POINTS e.g.: 
    ;; (eval-after-load 'mon-cifs-utils '(mon-bind-cifs-vars-at-loadtime nil t)) 
    (eval-after-load "mon-cifs-utils"           '(mon-bind-cifs-vars-at-loadtime))
    (eval-after-load "mon-cl-compat-regexps"    '(mon-CL-cln-colon-swap t))    
    (eval-after-load "mon-empty-registers"      '(mon-set-register-tags-loadtime t))
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
    (if (and (intern-soft "IS-W32-P") 
             (bound-and-true-p IS-W32-P))
        (mon-check-feature-for-loadtime   'mon-boxcutter)
      (mon-check-feature-for-loadtime     'thumbs))))

;;; ==============================
;;; <Timestamp: #{2010-07-28T19:49:04-04:00Z}#{10303} - by MON KEY>
;;; :TODO Abstract following to a macro for use with all for all loadtime
;;; functions that bind big lists.
;;
;; (let ((old-msgs (get-buffer-create "*OLD-MESSAGES*"))
;;       (msgs (get-buffer-create "*Messages*")))
;;   (unwind-protect 
;;       (progn
;;         (with-current-buffer (get-buffer msgs)
;;           (buffer-swap-text old-msgs))
;;         (setq <SOME-BIG-VAR-VALUE> <SOME-BIG-VALUE-IN-MESSAGES>)
;;         (with-current-buffer (get-buffer old-msgs)
;;           (buffer-swap-text msgs)))
;;     (with-current-buffer (get-buffer old-msgs)
;;       (kill-buffer (current-buffer)))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-cl.el :LICENSE LGPL
;;; :NOTE Could prop just defalias this but...
;;; :DOCSTRING is same as `defconstant'
;;; :CREATED <Timestamp: #{2010-01-15T15:46:09-05:00Z}#{10025} - by MON KEY>
(defmacro defconstant (symbol initvalue &optional docstring)
  `(defconst ,symbol ,initvalue ,docstring))
;;
;; Now, tack on some docs.
(eval-when (compile load)
  (let ((defcon-d (replace-regexp-in-string "^(fn.*)$" "" (documentation 'defconst))))
    (setq defcon-d
          (concat defcon-d
                  (mapconcat #'identity
                             '(":NOTE This is a CL compatibility feature, it macro-expands to elisp's `defconst'.\n"
                               ":SEE info node `(CL)Porting Common Lisp'.\n"
                               ":SEE-ALSO `defparameter', `defvar', `defcustom', `set-variable',"
                               "`make-local-variable', `make-variable-buffer-local', `make-symbol', `intern',"
                               "`intern-soft', `obarray', `boundp', `bound-and-true-p', `makunbound', `unintern'."
                               "►►►") "\n")))
    (plist-put (symbol-plist 'defconstant) 'function-documentation defcon-d)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-cl.el :LICENSE LGPL
;;; :DOCSTRING paraphrased from dpansr3.
;;; :NOTE Could prop just defalias this but...
;;; :CREATED <Timestamp: #{2010-01-15T15:46:09-05:00Z}#{10025} - by MON KEY>
(defmacro defparameter (name &optional initial-value docstring)
  "Unconditionally assign the INITIAL-VALUE to the dynamic variable named NAME.\n
In contrast to `defvar' the defparameter macro assigns INITIAL-VALUE (if
supplied) to the dynamic variable named NAME only if NAME is not already bound.\n
If no INITIAL-VALUE is supplied, `defvar' leaves the value cell of the dynamic
variable named NAME undisturbed; if NAME was previously bound, its old value
persists, and if it was previously unbound, it remains unbound.\n
If DOCSTRING is supplied, it is attached to NAME as a documentation
string of kind variable.\n
:NOTE This is a CL compatibility feature and expands to elisp's defvar.\n
:SEE info node `(elisp)Defining Variables'\n
:SEE-ALSO `defconstant', `defconst', `defcustom', `set-variable', 
`make-local-variable', `make-variable-buffer-local', `user-variable-p',
`make-symbol', `intern', `intern-soft', `obarray', `boundp', `bound-and-true-p',
`makunbound', `unintern'.\n►►►"
  `(progn
     (defvar ,name nil ,docstring)
     (setq   ,name ,initial-value)))
;;
;; ,---- :UNCOMMENT-TO-TEST
;; | (progn
;; |   (defparameter *bubba* "bubba")
;; |   (let* ((bub-s '*bubba*)
;; |          (bub-n (symbol-name bub-s))
;; |          (bub-v (symbol-value bub-s))
;; |          (msg (format (concat 
;; |                        "We just made %S the value of parameter %s.\n"
;; |                        "Now, say goodbye to %s and his %S.")
;; |                       bub-v bub-n bub-n bub-v)))
;; |     (makunbound '*bubba*) (unintern '*bubba*)
;; |     (message msg)))
;; `----

;;; ==============================
;;; :NOTE A macro'd version of `shuffle-vector' :FILE lisp/play/cookie1.el
;;; :CREATED <Timestamp: #{2010-07-31T11:34:53-04:00Z}#{10306} - by MON>
(defmacro mon-nshuffle-vector (mixup-vector)
  "Destructive random permutation of MIXUP-VECTOR elts, return MIXUP-VECTOR.\n
All permutations are equally likely.\n
:EXAMPLE\n\n\\(pp-macroexpand-expression 
 '\(mon-nshuffle-vector [37 41 43 47 53 59]\)\)\n
:ALIASED-BY `nshuffle-vector'\n
:SEE-ALSO `mon-list-nshuffle', `mon-list-shuffle-safe',`shuffle-vector', `slime-shuffle-list'.\n►►►"
  ;; :NOTE This is called repeatedly by `mon-*-gensym' procedures so we
  ;; need to gensym the local vars by hand.
  (declare (indent 0) (debug t))
  (let ((nsv-vec    (make-symbol "nsv-vec"))
        (nsv-incr   (make-symbol "nsv-incr"))
        (nsv-rndmz  (make-symbol "nsv-rndr"))
        (nsv-temp   (make-symbol "nsv-temp"))
        (nsv-len    (make-symbol "nsv-len")))
    `(let ((,nsv-vec ,mixup-vector)
           (,nsv-incr 0)
           (,nsv-len (length ,mixup-vector))
           ,nsv-rndmz
           ,nsv-temp)
       (while (< ,nsv-incr ,nsv-len)
         (setq ,nsv-rndmz (+ ,nsv-incr (random (- ,nsv-len ,nsv-incr))))
         (setq ,nsv-temp (aref ,nsv-vec ,nsv-incr))
         (aset ,nsv-vec ,nsv-incr (aref ,nsv-vec ,nsv-rndmz))
         (aset ,nsv-vec ,nsv-rndmz ,nsv-temp)
         (setq ,nsv-incr (1+ ,nsv-incr)))
       ,nsv-vec)))
;;
(unless (and (intern-soft "nshuffle-vector")
             (fboundp 'nshuffle-vector))
  (defalias 'nshuffle-vector 'mon-nshuffle-vector))
;;
;;; (pp-macroexpand-expression '(mon-nshuffle-vector [37 41 43 47 53 59]))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-07-29T15:07:15-04:00Z}#{10304} - by MON>
(defmacro mon-gensym (&optional prefix counter)
  "Generate a new uninterned symbol.\n
When optional arg PREFIX (a string) return a symbol-name by appending the value
of `*gensym-counter*' to PREFIX. The default prefix is \"M\".\n
When optional arg COUNTER satisfies the predicate `integerp' and PREFIX
satisfies the predicate `stringp' it is appended to PREFIX instead of
`*gensym-counter*'s value.\n
Like the `gensym' function in CL package but defined as a macro instead.\n
:EXAMPLE\n\n\(pp-macroexpand-expression '\(mon-gensym\)\)\n
\(pp-macroexpand-expression '\(mon-gensym \"EG\" 666\)\)\n
:SEE-ALSO `mon-gensym-counter-randomizer', `mon-with-gensyms',
`mon-gensym-counter-randomizer-TEST'.\n►►►"
  (declare (indent 0) (debug t))
  (let ((gs-pfix (make-symbol "gs-pfix"))
        (gs-num  (make-symbol "gs-num"))) ;; (print-gensym t))
    `(let ((,gs-pfix (cond ((and ,prefix (stringp ,prefix)) ,prefix)
                           ((and ,prefix (not (stringp ,prefix)))
                            (symbol-name ,prefix))
                           (t "M")))
           (,gs-num  (if (and ,prefix ,counter (integerp ,counter))
                         ,counter
                       (prog1 
                           *gensym-counter*
                         (incf *gensym-counter*)))))
       (make-symbol (format "%s%d" ,gs-pfix ,gs-num)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-07-29T17:45:55-04:00Z}#{10304} - by MON>
(defun mon-gensym-counter-randomizer (randomize-sym/str)
  "Generate a semi-reandomized integer from RANDOMIZE-SYM/STR.\n
Helper function for `mon-with-gensyms'.\n
:EXAMPLE\n\n(mon-gensym-counter-randomizer-TEST \"bubba\" 10000\)\n
\(mon-gensym-counter-randomizer-TEST 'bu  10000\)\n
:NOTE On average this function will return ~45-60 duplicates per 10,000
invocations per seed symbol. IOW we can could create an average of ~9948 unique
`bubba's if we batched inside a procedure capable of accounting for collisions.\n
:SEE-ALSO `mon-gensym', `mon-gensym-counter-randomizer-TEST'.\n►►►"
  (let ((mgcr-pr1
         [37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131
             137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227
             229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317
             331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431
             433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541])
        (mgcr-pr2 (make-vector 6 0)) ;; 6th elt of is length randomize-sym/str
        (mgcr-pr3 (make-vector 6 0)) ;; 6th elt of is string-bytes randomize-sym/str
        (mgcr-merp [3 5 7 13 17 19]) 
        (mgcr-rtn randomize-sym/str))
    ;; Fill vectors mgcr-pr2 and mgcr-pr3 with primes from mgcr-pr1 
    (dolist (mgcr-shfv '(mgcr-pr2 mgcr-pr3))
      (mon-nshuffle-vector mgcr-pr1)
      (dotimes (mgcr-mkv 5)
        (aset (symbol-value mgcr-shfv) mgcr-mkv (aref mgcr-pr1 mgcr-mkv))))
    ;; :DEBUGGING  `(,mgcr-pr2 ,mgcr-pr3) 
    ;;
    ;; Put randomize-sym/str on a sub section of the shuffled mgcr-pr1 vector.
    ;; This assures we get at least 5 non-null char values when 
    ;; (< (length mgcr-rtn) 5) Then we add at least one more char for variance.
    (let* ((mgcr-rndmz-sym/str (or (and (stringp mgcr-rtn) mgcr-rtn)
                                   (format "%s" mgcr-rtn)))
           (mgcr-rndmz-len  (length mgcr-rndmz-sym/str))
           (mgcr-rndmz-trunc-len (- mgcr-rndmz-len 6))
           (mgcr-subv1 (if (or (zerop mgcr-rndmz-trunc-len) (natnump mgcr-rndmz-trunc-len)) 
                           ;; Only get 1 extra value in range 0-88 e.g. (1- (length mgcr-pr1)).
                           (random 87) 
                         ;; Its negative, get the difference.
                         (+ 88 mgcr-rndmz-trunc-len))) 
           (mgcr-subv2  (if (or (zerop mgcr-rndmz-trunc-len) (natnump mgcr-rndmz-trunc-len))
                            (1+ mgcr-subv1) 
                          (- mgcr-subv1 mgcr-rndmz-trunc-len))))
      (aset mgcr-pr2 5 mgcr-rndmz-len)
      (aset mgcr-pr3 5 (string-bytes mgcr-rndmz-sym/str))
      (setq mgcr-pr1
            (vconcat
             (substring
              (concat 
               (substring (concat (mon-nshuffle-vector mgcr-pr1) "") mgcr-subv1 mgcr-subv2)
               (if (zerop mgcr-rndmz-trunc-len) 
                   (substring mgcr-rndmz-sym/str 0 5)
                 mgcr-rndmz-sym/str))
              -6))))
    (mon-nshuffle-vector mgcr-pr1)
    ;; :DEBUGGING (concat (mon-gensym-counter-randomizer "bubba") "")
    ;;
    ;; Shuffle the hell out of it then maximize the list vals.
    (setq mgcr-rtn 
          (mapcar #'(lambda (mgcr-stl-char)
                      (mon-nshuffle-vector mgcr-pr2) 
                      (mon-nshuffle-vector mgcr-pr3)
                      (mon-nshuffle-vector mgcr-merp)
                      (let (mgcr-gthr)
                        (dotimes (i 5 
                                    (setq mgcr-gthr (lsh (apply '+ mgcr-gthr) 
                                                         (- (aref mgcr-merp 0) (aref mgcr-merp 1)))))
                          (push (* (aref mgcr-pr2 i) (aref mgcr-pr3 i) mgcr-stl-char) mgcr-gthr))))
                  mgcr-pr1))
    (setq mgcr-rtn (apply '+ mgcr-rtn))
    ;; Shift it around but keep it signed.
    (setq mgcr-rtn (abs (ash mgcr-rtn (- (aref mgcr-merp 2) (aref mgcr-merp 3)))))
    ;; Make sure we have a value over 10000 else recurse
    (if (or (null mgcr-rtn)
            (< mgcr-rtn 10000)
            (= mgcr-rtn 0))
        (mon-gensym-counter-randomizer randomize-sym/str)
      mgcr-rtn)))
;;
;;; :TEST-ME (mon-gensym-counter-randomizer-TEST "bubba" 10000)
;;; :TEST-ME (mon-gensym-counter-randomizer-TEST 'bu  10000)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-07-31T14:41:01-04:00Z}#{10306} - by MON>
(defun mon-gensym-counter-randomizer-TEST (w-test-str/sym generate-n-results)
  "Test function for `mon-gensym-counter-randomizer'.\n
Return results in buffer named \"*MON-GENSYM-COUNTER-RANDOMIZER-TEST*\".\n
Arg W-TEST-STR/SYM is a string or symbol to build results with.\n
ARG GENERATE-N-RESULTS is the number of results to generate.\n
:EXAMPLE\n\n(mon-gensym-counter-randomizer-TEST \"bubba\" 10000)
:SEE-ALSO `mon-gensym', `with-gensyms', `mon-gensym-counter-randomizer'.\n►►►"
  (let ((mgcr-tst-buf (get-buffer-create "*MON-GENSYM-COUNTER-RANDOMIZER-TEST*"))
        mlf-dups)
    (with-current-buffer (get-buffer mgcr-tst-buf)
      (erase-buffer)
      (with-temp-message (concat ":FUNCTION `mon-gensym-counter-randomizer-TEST' "
                                 (format "-- still processing arg %S %d times ... " 
                                         w-test-str/sym generate-n-results))
        (save-excursion
          (dotimes (i generate-n-results)
            (princ (mon-gensym-counter-randomizer w-test-str/sym) (current-buffer))
            (newline))
          (sort-lines nil (buffer-end 0) (buffer-end 1)))
        (setq mlf-dups (mon-line-find-duplicates))
        (save-excursion
          (apply 'insert 
                 `(";;; :TESTING-FUNCTION `mon-gensym-counter-randomizer'\n"
                   ";;; :WITH-ARG w-test-str/sym " ,(format "%S" w-test-str/sym) "\n"
                   ";;; :WITH-ARG generate-n-results " ,(format "%d" generate-n-results) "\n"
                   ,@(unless (null mlf-dups)
                       (list (make-string 68 59) "\n"
                             ";;; :FOUND-N-DUPLICATES " 
                             (format "%d" (length mlf-dups)) "\n"
                             ";;; :AS-PERCENTAGE " 
                             (format "%%%.5f" (/ (float (length mlf-dups)) 10000)) "\n"
                             ";;; :DUPLICATES-FOUND\n\n"
                             (format "%s" mlf-dups) "\n\n"))
                   ,(make-string 68 59) "\n"
                   ";;; :GENERATED-RESULTS\n\n")))
        (display-buffer (current-buffer) t)))))
;;
;;; :TEST-ME (mon-gensym-counter-randomizer-TEST "bubba" 10000)
;;; :TEST-ME (mon-gensym-counter-randomizer-TEST 'bubba  10000)
;;; :TEST-ME (mon-gensym-counter-randomizer-TEST "bu"  10000)


;;; ==============================
;;; :NOTE This appears to work similiarly to Lars Brinkhoff's version but
;;;  without the CL requirements for `assert' `every' and `mapcar*'.
;;; :SEE (URL `http://www.emacswiki.org/emacs/macro-utils.el')
;;;   (assert (every #'symbolp symbols))
;;;    `(let ,(mapcar* #'list symbols '#1=((mon-gensym) . #1#))
;;;      ,@body))
;;; :CREATED <Timestamp: #{2010-07-29T20:09:35-04:00Z}#{10304} - by MON>
(defmacro mon-with-gensyms (syms &rest body)
  "Execute BODY in a context where the variables in SYMS are bound to
freshly allocated uninterned symbol as returned by `mon-gensym'.\n
:EXAMPLE\n\n\(unwind-protect
    \(progn
      \(defmacro tt--mgs \(arg1 arg2 arg3\)
        \(mon-with-gensyms 
          \(somea someb somec get-some\)
          `\(let \(\(,somea ,arg1\)
                 \(,someb ,arg2\)
                 \(,somec ,arg3\)
                 \(,get-some \(\)\)\)
             \(dolist \(q \(list ,somea ,someb ,somec\)
                        \(setq ,get-some 
                              \(mapconcat 'identity \(nreverse ,get-some\) \"\\n\"\)\)\)
               \(push \(concat \"a name: \" q \) ,get-some\)\)
             \(and \(null \(fboundp 'tt--mgs\)\)
                  \(equal ,get-some \"a name: bubba\\na name: sally\\na name: suzy\"\)\)\)\)\)
       \(pp-macroexpand-expression '\(tt--mgs \"bubba\" \"sally\" \"suzy\"\)\)\)
  \(progn \(fmakunbound 'tt--mgs\) \(unintern 'tt--mgs\)\)\)\n
:ALIASED-BY `with-gensyms'\n
:SEE-ALSO `mon-gensym-counter-randomizer', `mon-gensym'.\n"
  (declare (indent 0) (debug t))
  `(let ,(mapcar #'(lambda (mks)
                     `(,mks (mon-gensym 
                              ,(symbol-name mks) ;;,(format "%s" mks)
                              ,(mon-gensym-counter-randomizer mks)))) syms)
     ,@body))
;; 
(when (and (intern-soft "IS-MON-SYSTEM-P") 
           (not (fboundp 'with-gensyms)))
  (defalias 'with-gensyms 'mon-with-gensyms))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;|
;;| (unwind-protect
;;|     (progn
;;|       (defmacro tt--mgs (arg1 arg2 arg3)
;;|         (mon-with-gensyms 
;;|           (somea someb somec get-some)
;;|           `(let ((,somea ,arg1)
;;|                  (,someb ,arg2)
;;|                  (,somec ,arg3)
;;|                  (,get-some ()))
;;|              (dolist (q (list ,somea ,someb ,somec)
;;|                         (setq ,get-some 
;;|                               (mapconcat 'identity (nreverse ,get-some) "\n")))
;;|                (push (concat "a name: " q ) ,get-some))
;;|              (and (null (fboundp 'tt--mgs))
;;|                   (equal ,get-some "a name: bubba\na name: sally\na name: suzy")))))
;;|        (pp-macroexpand-expression '(tt--mgs "bubba" "sally" "suzy")))
;;|   (progn (fmakunbound 'tt--mgs) (unintern 'tt--mgs)))
;;|
;;`----

;;; ==============================
;;; :COURTESY :FILE gnus/gnus-util.el :WAS `gnus-mapcar'
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T16:36:17-04:00Z}#{10361} - by MON KEY>
(defmacro mon-mapcar (function seq1 &rest seqs2_n)
  "Apply FUNCTION to each element of SEQ1 and make a list of the results.\n
If there are several &rest  sequences, FUNCTION is called with that many arguments.
Mapping terminates with the shortest sequence.\n
With just one sequence, this is like `mapcar'.\n
With several, it is like the Common Lisp `mapcar' function extended to arbitrary
sequence types.\n
:EXAMPLE\n\n\(mon-mapcar 'list '\(a b c\) '\(1 2 3\) '\(d e f\) '\(4 5 6\)\)\n
\(mon-mapcar 'cons '\(a b c\) '\(1 2 3\)\)\n
\(pp-macroexpand-expression '\(mon-mapcar 'cons '\(a b c\) '\(1 2 3\)\)\)\n
\(let* \(\(tmp-alst '\(\(KEY-D D0 D1 D2\)\)\)
       \(tmp-keys '\(KEY-A KEY-B KEY-C\)\)
       \(tmp-vals '\(\(A0 A1 A2\) \(B0 B1 B2\) \(C0 C1 C2\)\)\)
       \(rtn-mon-mapcar tmp-alst\)
       rtn-pairlis\)
  \(setq rtn-mon-mapcar \(nconc \(mon-mapcar #'cons tmp-keys tmp-vals\) tmp-alst\)\)
  \(setq rtn-pairlis \(pairlis tmp-keys tmp-vals tmp-alst\)\)
  `\(:TREE-EQUAL-MMC-PAIRL ,\(tree-equal rtn-mon-mapcar rtn-pairlis\)
    :MON-MAPCAR/PAIRLIS ,rtn-mon-mapcar :CL-PKG/PAIRLIS ,rtn-pairlis\)\)\n
:NOTE Last example is basically Emacs lisp's version of Common Lisp's `parilis'.\n
:SEE-ALSO .\n►►►"
  (if (not seqs2_n)
      `(mapcar ,function ,seq1)
    (let* ((mmc-seqs      (cons seq1 seqs2_n))
           (mmc-cnt       0)
           (mmc-heads     (mapcar #'(lambda (seq) ;; :NOTE arg SEQ prevents backquote expansion to: "(lambda nil"
                                      (make-symbol (concat "head" (number-to-string (setq mmc-cnt (1+ mmc-cnt))))))
                                  mmc-seqs))
           (mmc-rslt      (make-symbol "mmc-rslt"))
           (mmc-rslt-tl   (make-symbol "mmc-rslt-tl")))
      `(let* ,(let* ((bindings  (cons nil nil))
                     (mmc-heads  mmc-heads))
                (nconc bindings (list (list mmc-rslt '(cons nil nil))))
                (nconc bindings (list (list mmc-rslt-tl mmc-rslt)))
                (while mmc-heads
                  (nconc bindings (list (list (pop mmc-heads) (pop mmc-seqs)))))
                (cdr bindings))
         (while (and ,@mmc-heads)
           (setcdr ,mmc-rslt-tl 
                   (cons 
                    (funcall ,function ,@(mapcar #'(lambda (mmc-h1) 
                                                     (list 'car mmc-h1))
                                                 mmc-heads)) 
                    nil))
           (setq ,mmc-rslt-tl (cdr ,mmc-rslt-tl)
                 ,@(apply 'nconc (mapcar #'(lambda (mmc-h2) 
                                             (list mmc-h2 (list 'cdr mmc-h2))) 
                                         mmc-heads))))
         (cdr ,mmc-rslt)))))
         

;;; ==============================
;;; :COURTESY Raphael Van Dyck :HIS km-frames.el :WAS `with-file-buffer'
;;; :SEE (URL `http://www.algo.be/dev-logiciels.htm')
;;; :CREATED <Timestamp: #{2009-10-23T15:17:35-04:00Z}#{09435} - by MON KEY>
(defmacro mon-with-file-buffer (buffer-var file &rest body)
  "Evaluate BODY with BUFFER-VAR bound to buffer visiting FILE.\n :SEE-ALSO
`mon-with-buffer-undo-disabled', `mon-buffer-exists-p', `mon-buffer-written-p',
`mon-buffer-exists-so-kill', `mon-print-in-buffer-if-p',
`mon-get-buffer-w-mode', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir',
`with-current-buffer', `with-temp-file', `with-temp-buffer',
`mon-help-buffer-functions'.\n►►►"
  (let ((file-var (make-symbol "file"))
        (buffer-already-there-p-var (make-symbol "buffer-already-there-p")))
    `(let* ((,file-var ,file)
            (,buffer-var (get-file-buffer ,file-var))
            (,buffer-already-there-p-var ,buffer-var))
       (unless ,buffer-already-there-p-var
         (setq ,buffer-var (find-file-noselect ,file-var)))
       (unwind-protect
            (progn ,@body)
         (unless ,buffer-already-there-p-var
           (kill-buffer ,buffer-var))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-22T16:45:38-04:00Z}#{09434} - by MON>
(defun mon-buffer-name->kill-ring (&optional or-buffer insrtp)
  "Put buffer-name of current-buffer on kill-ring.\n
When OR-BUFFER is non-nil put that buffer's name on kill ring instead.
When INSRTP is non-nil or called-interactively with prefix arg insert 
buffer-name at point. Does not move point.\n
:EXAMPLE\\n(mon-buffer-name->kill-ring)\n
\(call-interactively 'mon-buffer-name->kill-ring)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-buffer-exists-so-kill',
`mon-buffer-written-p', `mon-print-in-buffer-if-p', `mon-with-file-buffer',
`mon-with-buffer-undo-disabled', `mon-get-buffer-w-mode',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `mon-help-buffer-functions'.\n►►►"
  (interactive "i\nP")
  (let ((kn (kill-new (format "%S" (buffer-name or-buffer)))))
    (if insrtp 
        (save-excursion (newline) (princ kn (current-buffer)))
        (princ kn))))

;;; ==============================
;;; :COURTESY :FILE gnus-util.el :WAS `gnus-buffer-exists-p'
;;; :ADDED let wrapper gensym for local var BUFF-P
;;; :CREATED <Timestamp: #{2010-02-04T14:17:59-05:00Z}#{10054} - by MON KEY>
(defmacro mon-buffer-exists-p (buffer-to-check)
  "Return buffer-name of BUFFER-TO-CHECK if it exists.\n
:EXAMPLE\n\n\(mon-buffer-exists-p \(current-buffer\)\)\n
\(prog2 \(get-buffer-create \"*BAD-IF-NOT-KILLED*\"\)
    \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)
  \(kill-buffer \(mon-buffer-exists-p \"*BAD-IF-NOT-KILLED*\"\)\)\)\n
:ALIASED-BY `buffer-exists-p'\n
:SEE-ALSO `mon-buffer-exists-so-kill', `mon-with-file-buffer',
`mon-with-buffer-undo-disabled' `mon-buffer-written-p',
`mon-print-in-buffer-if-p', `mon-buffer-name->kill-ring',
`mon-get-buffer-w-mode', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir',
`with-current-buffer', `with-temp-file', `with-temp-buffer'.\n►►►"
  (declare (indent 2) (debug t))
  (let ((buff-p (make-symbol "buff-p")))
    `(let ((,buff-p ,buffer-to-check))
       (when ,buff-p
         (funcall (if (stringp ,buffer-to-check) 'get-buffer 'buffer-name)
                  ,buff-p)))))
;;
(when (and (intern-soft "IS-MON-SYSTEM-P")
           (bound-and-true-p IS-MON-SYSTEM-P)
           (if (intern-soft "buffer-exists-p")
               (not (fboundp 'buffer-exists-p))
             t))
  (defalias 'buffer-exists-p 'mon-buffer-exists-p))
;;
;;; :TEST-ME (mon-buffer-exists-p (current-buffer))
;;; :TEST-ME (prog2 (get-buffer-create "*BAD-IF-NOT-KILLED*") 
;;;                 (mon-buffer-exists-p "*BAD-IF-NOT-KILLED*")
;;;                 (kill-buffer (mon-buffer-exists-p "*BAD-IF-NOT-KILLED*")))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-13T11:50:55-04:00Z}#{10237} - by MON>
(defmacro mon-with-buffer-undo-disabled (&rest body)
  "Evaluate BODY in an`buffer-disable-undo' set non-nil.\n
Arg BODY occurs inside an `unwind-protect' finishing with `buffer-enable-undo'.\n
:EXAMPLE\n
\(pp-macroexpand-expression
 '\(progn
    \(mon-with-buffer-undo-disabled
     \(save-excursion
       \(insert \"bubba\"\)\)
     \(kill-line\)
     \(if \(and buffer-undo-list \(atom buffer-undo-list\)\)
         \(message \"Toggled buffer-undo-list %S\" buffer-undo-list\)
       \(message \":BUFFER-UNDO-LIST -- fail!\" buffer-undo-list\)\)
     \(sit-for 2\)\)
    \(save-excursion \(insert \"I'm the bubba that got saved\"\)\)
    \(kill-line\)
    buffer-undo-list\)\)\n
\(mon-with-buffer-undo-disabled-TEST\)\n
\(mon-with-buffer-undo-disabled-TEST 'force-fail\)\n\n
:NOTE Useful for forms which programatically `erase-buffer' contents and the undo list
is not needed.\n
:ALIASED-BY `mon-buffer-do-with-undo-disabled'\n
:SEE-ALSO `mon-with-buffer-undo-disabled-TEST', `buffer-undo-list', 
`mon-buffer-exists-p', `mon-buffer-written-p', `mon-buffer-exists-so-kill',
`mon-print-in-buffer-if-p', `mon-get-buffer-w-mode',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer', `mon-help-buffer-functions'.\n►►►"
  (declare (indent 2) (debug t))
  `(unwind-protect
       (progn
         (buffer-disable-undo)
         ,@body)
     (buffer-enable-undo)))
;;
(defalias 'mon-buffer-do-with-undo-disabled 'mon-with-buffer-undo-disabled)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-14T15:41:21-04:00Z}#{10241} - by MON KEY>
(defun mon-with-buffer-undo-disabled-TEST (&optional force-fail)
  "Test function for `mon-with-buffer-undo-disabled' macro.\n
When optional arg FORCE-FAIL is non-nil force test failure.\n
:EXAMPLE\n\n\(mon-with-buffer-undo-disabled-TEST\)\n
\(mon-with-buffer-undo-disabled-TEST 'force-fail\)\n
:SEE-ALSO `buffer-undo-list'.\n►►►"
  (let ((rnd-char (mon-nshuffle-vector (vconcat (append (number-sequence 97 122)
                                                   (number-sequence 65 90)))))
        bul)
    (with-temp-buffer 
      (mon-with-buffer-undo-disabled (setq bul buffer-undo-list))
      (unless force-fail
        (dotimes (jnk 100) 
          (insert (make-string (random 79) (elt rnd-char (random 52))) "\n"))
        (delete-region (random (1- (buffer-end 1))) (random (1- (buffer-end 1)))))
      (setq bul (cons bul buffer-undo-list)))
    (cond ((or (not (consp bul)) (<= (length bul) 1))
           (error (concat ":MACRO `mon-with-buffer-undo-disabled' "
                          "-- failed to toggle `buffer-undo-list' in temp-buffer")))
          ((and (car bul) (atom (car bul)))
           (message (concat ":MACRO `mon-with-buffer-undo-disabled' "
                            "-- success toggling `buffer-undo-list' in temp-buffer"))))))
;;
;;; :TEST-ME (mon-with-buffer-undo-disabled-TEST)
;;; :TEST-ME (mon-with-buffer-undo-disabled-TEST t)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-05T14:21:16-05:00Z}#{10055} - by MON KEY>
(defun mon-buffer-exists-so-kill (buffer-to-kill)
  "If BUFFER-TO-KILL exists kill it.\n
Return `#<killed buffer>' if buffered killed, else nil.\n
:EXAMPLE\n\n\(let \(\(not-much-longer \(get-buffer-create \"not-much-longer\"\)\)\)
  \(mon-buffer-exists-so-kill \(buffer-name not-much-longer\)\)\)\n
:SEE-ALSO `mon-buffer-exists-p', `mon-with-file-buffer', `mon-buffer-written-p',
`mon-buffer-name->kill-ring', `mon-print-in-buffer-if-p',
`mon-with-buffer-undo-disabled', `mon-get-buffer-w-mode',
`mon-get-buffer-parent-dir', `mon-get-proc-buffers-directories',
`mon-get-buffers-directories', `mon-string-split-buffer-name',
`mon-string-split-buffer-parent-dir', `with-current-buffer', `with-temp-file',
`with-temp-buffer', `mon-help-buffer-functions'.\n►►►"
  (let ((mbep (mon-buffer-exists-p buffer-to-kill)))
    (if (when mbep (kill-buffer mbep))        
        (get-buffer mbep))))

;;; ==============================
;;; :COURTESY :FILE slime.el :WAS `slime-recently-visited-buffer'
;;; :CHANGESET 1967
;;; :CREATED <Timestamp: #{2010-07-12T12:42:51-04:00Z}#{10281} - by MON KEY>
(defun mon-get-buffer-w-mode (w-mode &optional not-visible-only)
  "Return the most recently visited buffer whose major-mode is W-MODE.\n
When optional arg NOT-VISIBLE-ONLY is non-nil only considers buffers that are
not already visible. Default is to consider all buffers on all frames.\n
:EXAMPLE\n\n\(mon-get-buffer-w-mode 'help-mode 'not-visible-only\)\n
\(mon-get-buffer-w-mode 'fundamental-mode\)\n
:ALIASED-BY `mon-buffer-get-w-mode'\n
:SEE-ALSO `mon-buffer-exists-p', `mon-with-file-buffer', `mon-buffer-written-p',
`mon-buffer-name->kill-ring', `mon-print-in-buffer-if-p',
`mon-with-buffer-undo-disabled', `mon-get-buffer-parent-dir',
`mon-get-proc-buffers-directories', `mon-get-buffers-directories',
`mon-string-split-buffer-name', `mon-string-split-buffer-parent-dir',
`with-current-buffer', `with-temp-file', `with-temp-buffer'.\n►►►"
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode w-mode))
                  (not (string-match "^ " (buffer-name buffer)))
                  (null (if not-visible-only
                            (get-buffer-window buffer 'visible)
                          (get-buffer-window buffer t))))
        return buffer
        finally (error (concat ":FUNCTION `mon-get-buffer-w-mode' "
                               "-- can not locate buffer W-MODE: %S") w-mode)))
;;
(defalias 'mon-buffer-get-w-mode 'mon-get-buffer-w-mode)

;;; ==============================
;;; Following does image extension type checking. Can be used elsewhere as well.
;;; :REQUIRES `image-type-available-p', `image-type-from-file-name' in :FILE image.el
;;; :RENAMED `boxcutter-verify-image-type' -> `mon-image-verify-type'
;;; :MODIFICATIONS <Timestamp: #{2010-05-28T13:40:43-04:00Z}#{10215} - by MON>
;;; :CREATED <Timestamp: #{2009-10-17T17:58:20-04:00Z}#{09426} - by MON>
(defun mon-image-verify-type (verify-image-type)
  "Verify that image-type VERIFY-IMAGE-TYPE can be manipulated with ImageMagick's convert.\n
Return a canonical representation for VERIFY-IMAGE-TYPE.\n
Valid VERIFY-IMAGE-TYPE args are:\n
 {jpg png gif tiff jpeg xpm xbm pbm bmp psd}\n
:NOTE VERIFY-IMAGE-TYPE is not case sensitive, can be a string or symbol, and
any leading `.' in the file extension will be stripped away.\n
:EXAMPLE\n\(mon-image-verify-type 'jpg\)
\(mon-image-verify-type '.jpg\)
\(mon-image-verify-type \"jpg\"\)
\(mon-image-verify-type \".jpg\"\)
\(mon-image-verify-type \"I WILL FAIL with nil\"\)
\(mon-image-verify-type '\(I WILL FAIL with error\)\)\n
:NOTE Any potentially any file format that is RW by ImageMagick's 
convert command could be supported. For a complete list of formats supported:
:SEE (URL `http://www.imagemagick.org/script/formats.php')
:CALLED-BY `boxcutter-big-n-small'\n
:ALIASED-BY `boxcutter-verify-image-type' \(when \(and \(featurep 'mon-boxcutter\)\)\)\n
:SEE-ALSO `mon-check-image-type', `*boxcutter-conversion-program*',
`image-type-available-p', `image-type-from-file-name',\n
`image-file-name-extensions', `image-type-file-name-regexps',
`image-file-name-regexps'.\n►►►"
  (eval-when-compile (require 'image))
  (let ((v-type verify-image-type))
    (car (member
          (cond ((stringp v-type)
                 (if (image-type-from-file-name
                      (let* ((v-type-str v-type)
                             (chk-ftyp (string-match-p "\\." v-type-str)))
                        (if chk-ftyp
                            v-type-str
                          (setq v-type-str (concat "." v-type-str)))))
                     (let* ((ext-is-str (downcase v-type)) ;we still need to check for leading `.'
                            (ext-seq (mon-string-to-sequence ext-is-str)))
                       (mon-string-to-symbol
                        (if (eq (elt ext-seq 0) 46)
                            (mon-string-from-sequence (cdr ext-seq))
                          ext-is-str)))))
                ((symbolp v-type)
                 (let* ((f-sym-str (mon-string-from-symbol v-type))
                        (chk-ftyp (string-match-p "\\." f-sym-str)))
                   (if chk-ftyp
                       (if (<= chk-ftyp 1)
                           f-sym-str
                         (setq f-sym-str (concat "." f-sym-str)))
                     f-sym-str)
                   (if (image-type-available-p
                        (image-type-from-file-name f-sym-str))
                       (mon-string-to-symbol
                        (mon-string-after-index f-sym-str "."))
                     v-type)))
                ((or (listp v-type) (vectorp v-type)
                     (and (stringp v-type) (arrayp v-type)))
                 (error (concat ":FUNCTION `mon-image-verify-type' "
                                "-- %s is a %s - not a valid arg for `verify-image-type'")
                        v-type (type-of v-type)))
                (t (error (concat  ":FUNCTION `mon-image-verify-type' "
                                   "-- this argument sux no further type checking try again"))))
          '(jpg png gif tiff jpeg xpm xbm pbm bmp)))))
;;
;;; :TEST-ME (mon-image-verify-type 'jpg)
;;; :TEST-ME (mon-image-verify-type '.jpg)
;;; :TEST-ME (mon-image-verify-type "jpg")
;;; :TEST-ME (mon-image-verify-type ".jpg")
;;; :TEST-ME (mon-image-verify-type "I WILL FAIL with nil")
;;; :TEST-ME (mon-image-verify-type '(I WILL FAIL with error))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-09T11:54:07-05:00Z}#{09503} - by MON>
(defun mon-get-system-specs (&optional insrtp intrp)
  "Return the output of shell-command 'uname -a'.\n
When called-interactively or INSRTP is non-nil insert at point.\n
Does not move point.\n
:EXAMPLE\n(mon-get-system-specs)\n
:SEE-ALSO `system-name', `mon-get-env-vars-strings', `mon-get-env-vars-symbols',
`mon-get-env-vars-emacs', `mon-get-proc-w-name', `mon-get-sys-proc-list',
`mon-insert-sys-proc-list'.\n►►►"
  (interactive "i\np")
  (if (executable-find "uname")
      (let ((unm (shell-command-to-string "uname -a")))
        (setq unm (replace-regexp-in-string "[[:blank:]]+" " " unm))
        (if (or insrtp intrp)
            (save-excursion 
              (newline)
              (princ unm (current-buffer)))
            unm))
      (when (equal system-type 'windows-nt)
        (message "The command `uname -a' is not available"))))
;;
;;; :TEST-ME (mon-get-system-specs)
;;; :TEST-ME (mon-get-system-specs t)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-18T20:20:35-05:00Z}#{10032} - by MON>
(defun mon-get-env-vars-symbols ()
  "Return a list of symbols for current-process' environmental-variables.\n
like `mon-get-env-vars-strings' but returns symbols instead of strings.\n
:EXAMPLE\n\n(mon-get-env-vars-symbols)\n
:SEE-ALSO `mon-get-env-vars-strings', `mon-get-system-specs', 
`mon-help-emacs-introspect', `process-environment', `initial-environment',
`getenv', `setenv'.\n►►►"
  (interactive) 
  (let ((mgepev process-environment)
        gthr-pe)
    (dolist (mgepev-i mgepev gthr-pe)
      (when (string-match "\\(.*\\)=.*" mgepev-i)
        (push (car (read-from-string mgepev-i (match-beginning 0) (match-end 1))) gthr-pe)))))

;;; ==============================
;;; :COURTESY :FILE emacs/lisp/env.el :WAS `read-envvar-name' 
;;; :CREATED <Timestamp: #{2009-10-16T15:29:37-04:00Z}#{09425} - by MON KEY>
(defun mon-get-env-vars-strings (&optional as-strings insrtp intrp)
  "Return a list strings for the current process' enviromental variables.
When AS-STRINGS is non-nil or called with a prefix-arg return as strings.
When insrtp or called-interactively insert returned vars at point.\n
:SEE-ALSO `mon-get-env-vars-symbols', `mon-get-env-vars-emacs',
`mon-get-system-specs', `mon-help-emacs-introspect', 
`process-environment', `initial-environment', `setenv', `getenv'.\n►►►"
  (interactive "P\ni\np")
  (let ((getenvs
         (mapcar #'(lambda (enventry)
                   (let ((str (substring enventry 0
                                         (string-match "=" enventry))))
                     (if (multibyte-string-p str)
                         (decode-coding-string
                          str locale-coding-system t)
                         str)))
                 ;; :NOTE Why did this use append here?
                 (append process-environment))))
    (setq getenvs (sort getenvs 'string<))
    (when as-strings
           (setq getenvs (concat "\"" (mapconcat 'identity getenvs "\"\n\"") "\"")))
    (cond ((or insrtp intrp)
        ;; (mapc (lambda (x) (prin1 x (current-buffer))) getenvs)
           (if as-strings
               (princ getenvs (current-buffer))
               (prin1 getenvs (current-buffer))))
          (t (if as-strings
                 (prin1 getenvs)
                 getenvs)))))
;;
;;; :TEST-ME (mon-get-env-vars-strings t)
;;; :TEST-ME (mon-get-env-vars-strings nil nil)
;;; :TEST-ME (mon-get-env-vars-strings  t t)
;;; :TEST-ME (mon-get-env-vars-strings  nil t)
;;; :TEST-ME (princ (mon-get-env-vars-strings t) (current-buffer))
;;; :TEST-ME (prin1 (mon-get-env-vars-strings t) (current-buffer))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-22T16:29:45-05:00Z}#{10035} - by MON>
(defun mon-get-env-vars-emacs (&optional insrtp intrp)
  "Return an list of the current environmental variables of the running Emacs.\n
Alist Keys have the form:\n
 \(:ENV-VAR \"VALUE-STRING\"\)
 \(ENV_VAR \"VALUE-STRING\"\)\n
For each colon prefixed :ENV-VAR key there is an equivalent symbol key. Each has
an identical value.\n
:EXAMPLE\n\n\(mon-get-env-vars-emacs\)\n
\(mapcar 'car \(mon-get-env-vars-emacs\)\)\n
:NOTE MON stores some local variables in `*mon-misc-path-alist*'. When this
symbol is present values associated with the key ``the-emacs-vars'' will also included
when generating the return value.\n
When called-interactively pretty-print return value in buffer named
\"*MON-EMACS-ENV-VARS*\".\n
:SEE info node `(emacs)General Variables'.\n
:SEE-ALSO `mon-get-env-vars-strings', `mon-get-env-vars-symbols'
`mon-get-system-specs', `mon-insert-sys-proc-list',
`mon-get-sys-proc-list', `mon-get-proc-w-name', `mon-get-process',
`mon-help-process-functions', `mon-help-emacs-introspect', `emacs-pid',
`process-environment', `initial-environment', `getenv', `setenv'.\n►►►"
  (interactive "i\np")
  (let ((emacs-env-vars (mon-intersection 
                         (mon-get-env-vars-symbols)
                         (append 
                          ;; MON-LOCAL-VARS                          
                          (when (bound-and-true-p *mon-misc-path-alist*)
                            (cadr (assoc 'the-emacs-vars *mon-misc-path-alist*)))
                          (do* ((i '(;; :LENNART-EMACS-W32-VARS
                                     EMACSCLIENT_STARTING_SERVER EMACS_SERVER_FILE
                                     ;; :GNUS-MAIL
                                     ;; MH NNTPSERVER REPLYTO SAVEDIR SMTPSERVER  MAIL
                                     ;; ORGANIZATION VERSION_CONTROL HISTFILE EMAIL EMACS_UNIBYTE CDPATH
                                     ;; :STANDARD-EMACS-VARS 
                                     emacs_dir INFOPATH ESHELL  INCPATH
                                     EMACSLOADPATH EMACSDATA EMACSPATH EMACSDOC SHELL TERM)   i)
                                (j (pop i) (pop i))
                                (k))
                               ((null j) (nreverse k))
                            (when (getenv (format "%s" j))(push j k))))
                         nil t))
        gthr-env-vars)
    (dolist (v emacs-env-vars
             (setq gthr-env-vars (nreverse gthr-env-vars)))
      (let ((is-var (car (memq v emacs-env-vars)))
            (this-var-list-pairs))
        (unless (null is-var)
          (let* ((frmt-var (upcase (format "%s" is-var)))
                 (get-var-val (file-truename (getenv frmt-var))))
            (push `(,(car (read-from-string (subst-char-in-string 95 45 (concat ":" frmt-var))))
                     . ,get-var-val)
                  this-var-list-pairs)
            (push `(,(car (read-from-string frmt-var)) . ,get-var-val)
                  this-var-list-pairs)))
        (setq this-var-list-pairs (nreverse this-var-list-pairs))
        (push (car this-var-list-pairs) gthr-env-vars)
        (push (cadr this-var-list-pairs) gthr-env-vars)))
    insrtp ;; Not currently evaluating INSRTP.
    (when intrp 
      (let ((meev (get-buffer-create "*MON-EMACS-ENV-VARS*")))
        (with-current-buffer (buffer-name meev)
          (erase-buffer)
          (pp-display-expression gthr-env-vars (buffer-name meev))
          (mon-g2be) 
          (insert ";; :MON-EMACS-ENV-VARS output from:\n;; (mon-get-env-vars-emacs nil t)\n;;\n"))))))
;;
;;; :TEST-ME (mon-get-env-vars-emacs)
;;; :TEST-ME (mapcar 'car (mon-get-env-vars-emacs))

;;; ==============================
;;; :NOTE MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; :CHANGESET 1708 <Timestamp: #{2010-04-12T17:02:32-04:00Z}#{10151} - by MON KEY>
;;; :CHANGESET 1703 <Timestamp: #{2010-04-07T14:39:56-04:00Z}#{10143} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-16T15:49:07-04:00Z}#{09425} - by MON KEY>
(defun mon-get-sys-proc-list (&optional intrp)
  "Return a full lisp list of current system-proceses.\n
When called-interactively return results in buffer \"*MON-GET-SYS-PROCESSES*\".\n
:EXAMPLE:\n\n(mon-get-sys-proc-list)\n\n\(mon-get-sys-proc-list t\)\n
:SEE-ALSO `mon-get-process',`mon-insert-sys-proc-list',
`mon-help-process-functions', `mon-get-env-vars-strings',
`mon-get-env-vars-symbols', `mon-get-env-vars-emacs', `mon-get-system-specs',
`mon-help-emacs-introspect', `emacs-pid'.\n►►►"
  (interactive "p")
  (let (mgspl)
    (dolist (sys-proc (list-system-processes) (setq mgspl (nreverse mgspl)))
      (push (process-attributes sys-proc) mgspl))
    (when intrp 
      (pp-display-expression mgspl "*MON-GET-SYS-PROCESSES*"))))
;;
;;; :TEST-ME (mon-get-sys-proc-list)
;;; :TEST-ME (mon-get-sys-proc-list t)

;;; ==============================
;;; :NOTE MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; CREATED: <Timestamp: #{2009-10-16T15:54:29-04:00Z}#{09425} - by MON KEY>
(defun mon-insert-sys-proc-list ()
  "Insert a full lisp list of current system-proceses at point.
Does not move point.\n
:SEE-ALSO `mon-get-process', `mon-get-sys-proc-list', 
`mon-insert-sys-proc-list', `emacs-pid',
`mon-get-env-vars-strings', `mon-get-env-vars-symbols'
`mon-get-env-vars-emacs', `mon-get-system-specs'
`mon-help-emacs-introspect'.\n►►►"
  (interactive)
  (save-excursion
    (newline)
    (mapc #'(lambda (x)
              (princ (concat ";;;\n" (pp x))(current-buffer)))
          (mon-get-sys-proc-list))))

;;; ==============================
;;; :NOTE MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; :CREATED <Timestamp: #{2009-10-16T16:34:48-04:00Z}#{09425} - by MON KEY>
(defun mon-get-proc-w-name (comm)
  "Return list of `process-attributes' lists for Command name COMM.
COMM (a string) is an executable name. 
On w32 it is not required give the .exe suffix.\n
:EXAMPLE\n\(mon-get-proc-w-name \"emacs\"\)\n
:SEE-ALSO `mon-get-process', `mon-get-sys-proc-list', `mon-get-sys-proc-list'.
`mon-help-process-functions'.\n►►►"
  (let (fnd-proc gthr)
    (mapc #'(lambda (x)
              (let ((t-aso (assoc 'comm x)))
                (if (string-match comm (cdr t-aso)) ;"emacs.exe"
                    (setq fnd-proc (cons x fnd-proc)))))
          (mon-get-sys-proc-list))
    fnd-proc))
;;
;;; :TEST-ME (mon-get-proc-w-name "emacs")
;;; :TEST-ME (mon-get-proc-w-name "svchost")
;;; :TEST-ME (mon-get-proc-w-name "bubba")
;;; :TEST-ME (mon-get-proc-w-name (invocation-name))
                               
;;; ==============================
;;; :NOTE Built to test for "mysql" command before looking for a comint.
;;;       MON recently found the :FILE proced.el 
;;;       Some of this might be accomplished with that excellent package.
;;; :NOTE `mon-get-process' uses `flet' cl--find-if -> `find-if'
;;; :CREATED <Timestamp: #{2009-12-09T20:02:30-05:00Z}#{09503} - by MON>
(defun mon-get-process (&optional proc-comm)
  "Find the process-id for process invoked with command.
When PROC-COMM is non-nil it is a command name to find.
Default is value of \(invocation-name\).\n
:NOTE This function is GNU/Linux centric! However, unlike `mon-get-proc-w-name'
this function can match multiple processes with identical invocation commands.\n
:EXAMPLE\n\n\(if IS-W32-P 
    \(mon-get-process \(concat \(invocation-name\) \".exe\"\)\)
    \(mon-get-process \(invocation-name\)\)\)\n
:SEE-ALSO `mon-insert-sys-proc-list', `mon-get-sys-proc-list',
`mon-help-process-functions'.\n►►►"
  (interactive)
  (let* ((pmatch)
         (prc-l (nreverse (list-system-processes)))
         (map-prc #'(lambda (u) 
                      (flet ((cl--fi (cl-pred cl-list &rest cl-keys) ;; `find-if'
                               (apply 'find nil cl-list :if cl-pred cl-keys)))
                        (let ((got-it 
                               (cl--fi  #'(lambda (z) 
                                            (and (eql (car z) 'comm)
                                                 (equal (cdr z) 
                                                        (if proc-comm 
                                                            proc-comm 
                                                            (invocation-name)))))
                                        (process-attributes u))))
                          (when got-it (if (not prc-l)
                                           (push `(,u ,got-it) prc-l)
                                           (push u pmatch))))))))
    (mapc map-prc prc-l)
    (if pmatch
        (progn 
          (setq prc-l nil)
          (mapc map-prc pmatch)
          ;; :WAS (if prc-l prc-l))
          (or prc-l))
        pmatch)))
;;
;;; :TEST-ME (if IS-W32-P 
;;;              (mon-get-process (concat (invocation-name) ".exe"))
;;;              (mon-get-process (invocation-name)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-01T13:54:34-05:00Z}#{09492} - by MON KEY>
(defun mon-make-shell-buffer ()
  "Return a new *shell* buffer.\n
If *shell* exists increment by 1 and return *shell-N*.\n
:EXAMPLE\n\(let \(\(kl-bf \(mon-make-shell-buffer\)\)\)
  \(progn \(momentary-string-display 
          \(format \" The buffer %s is about to die\" kl-bf\) \(point\)\)
         \(kill-buffer kl-bf\)\)\)\n
:NOTE could also be accomplished similarly with:\n
\(get-buffer-create \(generate-new-buffer-name \"*shell*\"\)\)\n
But, this way MON has fine-grain control over the assigned name suffix.\n
:CALLED-BY `mon-shell'\n
:SEE-ALSO `generate-new-buffer', `generate-new-buffer-name',
`mon-make-shell-buffer', `mon-terminal', `mon-help-process-functions',
`shell'.\n►►►"
  (let (buffs buffs-str)
    (setq buffs (with-temp-buffer
                  (princ
                   (buffer-list)
                   (current-buffer))
                  (buffer-substring-no-properties
                   (buffer-end 0)
                   (buffer-end 1))))
    (setq buffs (read buffs))
    (setq buffs (mapcar #'(lambda (x) (format "%s" x)) buffs))
    (mapc #'(lambda (x)
              (when (string-match-p "\\*shell" x)
                (push x buffs-str)))
          buffs)
    (setq buffs (car buffs-str))
    (cond ((null buffs) (get-buffer-create "*shell*"))
          ((= (length buffs) 7) (get-buffer-create "*shell-1*"))
          ((> (length buffs) 7) 
           (get-buffer-create
            (format "*shell-%d*" 
                    (1+ (string-to-number (substring buffs 7 8)))))))))
;;
;;; :TEST-ME (mon-make-shell-buffer)
;;; :TEST-ME (let ((kl-bf (mon-make-shell-buffer)))
;;;              (prog1 (princ kl-bf) (kill-buffer kl-bf)))

;;; ==============================
;;; :NOTE I tried to figure out how to do this with `defadvice'... that was bad!
;;; :CREATED <Timestamp: #{2009-12-01T15:18:38-05:00Z}#{09492} - by MON KEY>
(defun mon-shell ()
  "Return *shell* buffer.\n
If *shell* exists increment by 1 and return *shell-N*.\n
:SEE-ALSO `mon-make-shell-buffer', `mon-terminal', `mon-help-process-functions',
`shell'.\n►►►"
  (interactive)
  (shell (mon-make-shell-buffer)))
;;
;;; :TEST-ME (progn (mon-shell) (mon-shell))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-22T16:22:02-04:00Z}#{10252} - by MON>
(defvar *mon-ascii-cursor-state* nil
  "Variable to hold state for `mon-rotate-ascii-cursor'.
Its value is set during execution by that functions ROTATE-PRED arg.
:SEE-ALSO .\n►►►")

;;; ==============================
;;; :TODO This needs to be re-written as a macro in the style of `with-temp-message'
;;; :CREATED <Timestamp: #{2010-06-18T13:17:42-04:00Z}#{10245} - by MON>
(defun mon-rotate-ascii-cursor (rotate-pred &optional rotate-message)
  "Spin an ASCII cursor while ROTATE-PRED evaluates t.
ROTATE-PRED is a function which modifies the value of the global variable
`*mon-ascii-cursor-state*' after each full rotation of the cursor.  On entry to this
function the the value of `*mon-ascii-cursor-state*' is nil. On exit it is set to
nil. This function will signal an error if `*mon-ascii-cursor-state*' is void.\n
Optional arg is a string to display before while cursor while it is spinning.
Default is \"processing .... \"
:EXAMPLE\n\n\(progn
  \(setq *mon-ascii-cursor-state* 8\)
  \(mon-rotate-ascii-cursor 
   #'\(lambda \(\) 
       \(if \(= *mon-ascii-cursor-state* 0\) 
           \(setq *mon-ascii-cursor-state* nil\)
         \(setq *mon-ascii-cursor-state* \(1- *mon-ascii-cursor-state*\)\)\)\)
   \"running `mon-rotate-ascii-cursor' example ... \"\)
  \(message \(format \":VARIABLE `*mon-ascii-cursor-state*' is: %S\"
                   \(symbol-value *mon-ascii-cursor-state*\)\)\)\)\n
:SEE-ALSO `dotimes-with-progress-reporter', `progress-reporter-done',
`progress-reporter-update', `make-progress-reporter',
`progress-reporter-force-update', `progress-reporter-do-update'
`url-display-percentage', `url-show-status'.\n►►►"
  (when (null *mon-ascii-cursor-state*)
    (error (concat ":FUNCTION `mon-rotate-ascii-cursor' "
                   "-- global variabe `*mon-ascii-cursor-state*' null")))
  (unwind-protect 
      (let ((mrac-msg (or rotate-message "processing .... ")))
        (while *mon-ascii-cursor-state*
          (dolist (rot '(92 45 124 47 45))
            (message (concat mrac-msg (char-to-string rot)))
            (sit-for .1))
          (funcall rotate-pred)))
    (setq *mon-ascii-cursor-state* nil)))
;;
;;,---- :UNCOMMENT-TO-TEST
;;| (progn
;;|   (setq *mon-ascii-cursor-state* 8)
;;|   (mon-rotate-ascii-cursor 
;;|    #'(lambda () 
;;|        (if (= *mon-ascii-cursor-state* 0) 
;;|            (setq *mon-ascii-cursor-state* nil)
;;|          (setq *mon-ascii-cursor-state* (1- *mon-ascii-cursor-state*))))
;;|    "running `mon-rotate-ascii-cursor' example ... ")
;;|   (message (format ":VARIABLE `*mon-ascii-cursor-state*' is: %S"
;;|                    (symbol-value *mon-ascii-cursor-state*))))
;;`----


;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-01-19T18:51:37-05:00Z}#{10032} - by MON>
;;; :CREATED <Timestamp: #{2009-12-01T01:12:29-05:00Z}#{09492} - by MON KEY>
(defun mon-async-du-dir (the-dir)
  "Return a sorted du \(big->small\)for DIR in buffer `*DU-<DIR>'.\n
Invoke du as an asynchronous shell command.\n
:EXAMPLE\n\n\(mon-async-du-dir data-directory)\n
:SEE-ALSO `mon-help-du-incantation', `*regexp-clean-du-flags*',
`get-free-disk-space', `directory-free-space-program',
`directory-free-space-args'.\n►►►"
  (interactive "DDirectory to du: ") ;(read-directory-name "Directory to du :" nil nil t)))
  (if (fboundp 'async-shell-command)      
      (let ((dir-du
             (file-name-as-directory
              (file-truename
               (if (file-name-absolute-p the-dir)
                   the-dir
                 (expand-file-name the-dir))))))
        (async-shell-command 
         (format "du %s | sort -nr" dir-du)
         (get-buffer-create (format "*DU-%s" dir-du))))
    (message (concat ":FUNCTION `mon-async-du-dir' "
                      "-- the du command is not available on w32"))))
;;
;;; :TEST-ME (mon-async-du-dir data-directory)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-06T16:04:09-04:00Z}#{09412} - by MON KEY>
(when (or (featurep 'mon-default-start-loads) IS-MON-SYSTEM-P)
  (defun mon-load-cedet ()
    "Load CEDET if it isn't already.\n
Alias 'slot-makunbound -> `slot-makeunbound'.
This function will be :DEPRECATED once EMACS <-> CEDET merge is complete.\n►►►"
    (interactive)
    (progn
      (if (and IS-MON-P (not (featurep 'cedet)))
          (load-file  (concat *mon-site-lisp-root* "/cedet-cvs/common/cedet.el")))
      (message (concat ":FUNCTION `mon-load-cedet' "
                       "-- CEDET already loaded or your of a MONish way"))
      ;;
      ;; :REMOVE-ME once slot-makeunbound is removed/renamed/aliased in cedet/eieio.el
      (unless (fboundp 'slot-makunbound)
        (defalias 'slot-makunbound 'slot-makeunbound))))
  )                                     ; :CLOSE when
;;; ==============================
(defun mon-terminal ()
  "When `gnu-linuxp' launch a terminal.\n
When `win32p' invoke Cygwin Bash in cmd console.\n
:SEE-ALSO `mon-cmd' which when win32p returns the NT Command console.
`mon-shell', `mon-make-shell-buffer', `w32shell-cmd-here', `w32shell-cmd',
`w32shell-explorer', `mon-help-process-functions'.\n►►►"
  (interactive)
  (cond  (IS-BUG-P (message "You don't have the goods for this"))
         (IS-MON-P-W32 (w32-shell-execute "open" "cmd.exe" "C:\\Cygwin.bat"))
         (IS-MON-P-GNU (shell-command "terminal"))))

;;; ==============================
(defun mon-cmd ()
  "When `win32p' launch the NT Command console.\n
When `gnu-linuxp' launch a terminal \(mrxvt\).\n
:SEE `mon-terminal' which when `win32p' gives a Cygwin bash shell wrapped
in a cmd console.\n
:SEE-ALSO `mon-shell', `mon-make-shell-buffer', `w32shell-cmd-here',
`w32shell-cmd', `w32-shell-execute', `w32shell-explorer', `shell-command',
`shell', `mon-help-process-functions'.\n►►►"
  (interactive)
  (cond ((and (intern-soft "IS-W32-P")
              (bound-and-true-p IS-W32-P))
         (w32-shell-execute "open" "cmd"))
        ((and (intern-soft "IS-GNU-P")
              (bound-and-true-p IS-GNU-P)
              ;; :WAS (shell-command "terminal")
              (shell-command "mrxvt")))))

;;; ==============================
(defun mon-firefox (url &optional intrp)
  "Jump to the running firefox and open URL in new tab.\n
:SEE-ALSO `browse-url-firefox-program',`mon-conkeror',
`browse-url-generic-program', `browse-url-browser-function',
`browse-url-generic'.\n►►►"
  (interactive "i\np")
  (when intrp
    (setq url (read-string
               (concat ":FUNCTION `mon-firefox' "
                       "-- which URL: "))))
  (browse-url-firefox url))

;;; ==============================
(defun mon-conkeror (url)
  "Launch or find a running conkeror web browser with URL.\n
:NOTE To enusre Emacs gets existing conkeror process put following in
conkeror-rc file:
 url_remoting_fn = load_url_in_new_buffer;
 require\(\"clicks-in-new-buffer.js\");\n
:SEE-ALSO `mon-firefox', `browse-url-firefox-program',
`browse-url-generic-program',`browse-url-browser-function',
`browse-url-generic'.\n►►►"
  (interactive "s:FUNCTION `mon-conkeror' -- what URL: ")
  (eval-when-compile (require 'browse-url))
  (if (string-match "conkeror" browse-url-generic-program)
      (cond ;; :NOTE These conditionals are here so we can adjust as needed.
       ((and (intern-soft "IS-MON-P-W32")
             (bound-and-true-p IS-MON-P-W32))
        (browse-url-generic url))
       ((and (intern-soft "IS-MON-P-GNU")
             (bound-and-true-p IS-MON-P-GNU))
        (browse-url-generic url))
       ((and (intern-soft "IS-BUG-P")
             (bound-and-true-p IS-BUG-P))
        (browse-url-generic url))
       ((and (intern-soft "IS-MON-SYSTEM-P")
             (bound-and-true-p IS-MON-SYSTEM-P))
        (browse-url-generic url))
       ((and (intern-soft "IS-NOT-A-MON-SYSTEM") 
             (bound-and-true-p IS-NOT-A-MON-SYSTEM))
        (browse-url-generic url))
       (t (error (concat ":FUNCTION `mon-conkeror' "
                         "-- can not grok your system and/or "
                         "conkeror not set as `browse-url-generic-program'"))))
    (error (concat ":FUNCTION `mon-conkeror' "
                   "-- conkeror not set `browse-url-generic-program'"))))

;;; ==============================
;;; :CHANGESET 1751 <Timestamp: #{2010-05-21T16:27:55-04:00Z}#{10205} - by MON KEY>
(defun mon-scratch (&optional w-this-scratch)
  "Switch to *scratch* buffer in other window.\n
When *scratch* buffer does not exist, get \(or create\) it now!\n
When W-THIS-SCRATCH is non-nil or called-interactively with prefix arg if
current-buffer is \"*scratch*\" erase buffer contents else find an empty scratch buffer.\n
:EXAMPLE\n\n\(mon-scratch\)\n
:SEE-ALSO `mon-switch-to-mesages', `mon-kill-completions'.\n►►►"
  (interactive "P")
  (let ((confirm-nonexistent-file-or-buffer nil))
    ;; (same-window-buffer-names '("*scratch*")))
    (if (or current-prefix-arg w-this-scratch)
        (if (equal (buffer-name (current-buffer)) "*scratch*")
            (with-current-buffer (current-buffer) (erase-buffer))
          (switch-to-buffer-other-window "*scratch*" t))
      (switch-to-buffer-other-window "*scratch*"  t))
    )) ;; (lisp-interaction-mode)
;;
;;; (with-current-buffer "*scratch*"  (display-buffer (current-buffer)))    
;;
;;; :TEST-ME (mon-scratch)
;;; :TEST-ME (mon-scratch t)

;;; ==============================
(defun mon-switch-to-messages ()
  "Select buffer *Messages* in the current window.\n
:SEE-ALSO `mon-scratch', `mon-kill-completions'\n►►►"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;; ==============================
;;; :RENAMED `scroll-down-in-place' -> `mon-scroll-down-in-place'
(defun mon-scroll-down-in-place (&optional down-by)
  "Scroll with the cursor in place, moving screen DOWN-BY instead.\n
:SEE-ALSO `mon-scroll-up-in-place', `scroll-up', `scroll-down',
`mon-line-move-n', `mon-line-move-next'.\n►►►"
  (interactive "p")
  (let* ((inhibit-redisplay t)
        (down-by (abs (or down-by 1)))
        (next-screen-context-lines down-by))
    (if (forward-line (- down-by))
        (ignore-errors (scroll-down down-by)))))

;;; ==============================
;;; :RENAMED `scroll-up-in-place' -> `mon-scroll-up-in-place'
(defun mon-scroll-up-in-place (&optional up-by)
  "Scroll with the cursor in place, moving the screen UP-BY lines instead.\n
:SEE-ALSO `mon-scroll-down-in-place', `scroll-up', `scroll-down'
`mon-line-move-n', `mon-line-move-prev'.\n►►►"
  (interactive "P")
  (let* ((inhibit-redisplay t)
         (up-by (abs (or up-by 1)))
         (next-screen-context-lines up-by))
    (when (forward-line up-by)
      (ignore-errors (scroll-up up-by)))))

;;; ==============================
;;; :CREATED <Timestamp: Friday March 20, 2009 @ 09:17.35 PM - by MON KEY>
(defun mon-kill-appending (beg end)
  "Append the region current to the kill ring without killing it.\n
Like `append-next-kill' but skips the C M-w M-w finger-chord hoop jump.
:SEE-ALSO .\n►►►"
  (interactive "r")
  (progn 
    (append-next-kill)
    (kill-ring-save beg end)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday March 05, 2009 @ 04:49.29 PM - by MON KEY>
(defun mon-kill-completions ()
  "Kill *Completions* buffer without leaving point.\n
:SEE-ALSO `mon-scratch', `mon-switch-to-messages'.\n►►►"
  (interactive)
  (save-excursion
    (when (get-buffer-window "*Completions*")
      (progn
	(switch-to-completions)
	(delete-completion-window)))))

;;; ==============================
(defun mon-flip-windows ()
  "Swap current buffer display with buffer in other window.\n
:SEE-ALSO `mon-twin-vertical', `mon-twin-horizontal'.\n►►►"
  (interactive)
  (let ((cur-buffer (current-buffer))
        (top-buffer)
        (bottom-buffer))
    (pop-to-buffer (window-buffer (frame-first-window)))
    (setq top-buffer (current-buffer))
    (other-window 1)
    (setq bottom-buffer (current-buffer))
    (switch-to-buffer top-buffer)
    (other-window -1)
    (switch-to-buffer bottom-buffer)
    (pop-to-buffer cur-buffer)))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el
;;; :SEE (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :WAS `ff/twin-horizontal-current-buffer' -> `mon-twin-horizontal'
(defun mon-twin-horizontal () 
  "Split current-buffer horizontally.\n
:SEE-ALSO `mon-twin-vertical', `mon-flip-windows'.\n►►►"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows))
;;
;;; :WAS `ff/twin-vertical-current-buffer' -> `mon-twin-vertical'
(defun mon-twin-vertical () 
  "Split current-buffer vertically.\n
:SEE-ALSO `mon-twin-horizontal', `mon-flip-windows'.\n►►►"
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (balance-windows))

;;; ==============================
(defun mon-toggle-menu-bar ()
  "Toggle the top menu bar.\nGets the max editor screen for your money!\n
:SEE-ALSO `mon-toggle-dired-dwim-target', `mon-toggle-truncate-line'
`mon-toggle-eval-length', `mon-naf-mode-toggle-restore-llm',
`mon-toggle-show-point-mode'.\n►►►"
  (interactive)
  (let ((mtmb-height (frame-height)))
    (menu-bar-mode nil)
    (set-frame-height (selected-frame)
                      (if menu-bar-mode
                          (1- mtmb-height)
                        (1+ mtmb-height)))
    (force-mode-line-update t)))

;;; ==============================
;;; :COURTESY Thierry Volpiatto :HIS tv-utils.el :WAS `tv-append-to-register'
;;; :CREATED <Timestamp: Tuesday June 16, 2009 @ 07:09.33 PM - by MON KEY>
(defun mon-append-to-register (register start end &optional delete-region-p)
  "Append region to text in register REGISTER.\n
When non-nil prefix arg DELETE-REGION-P will delete region as well.
Called programaticaly, takes four args: REGISTER, START, END and DELETE-REGION-P.
START and END are buffer positions indicating what to append.
Redefines `append-to-register' with a \"\n\".\n
:SEE-ALSO `mon-append-to-buffer', `mon-kill-appending', `mon-append-to-register'.\n►►►"
  (interactive "cAppend to register: \nr\nP")
  (let ((matr-reg (get-register register))
        (matr-text (filter-buffer-substring start end)))
    (set-register
     register (cond ((not matr-reg) matr-text)
                    ((stringp matr-reg) (concat matr-reg "\n" matr-text))
                    ;;(t (error "Register does not contain text")))))
                    (t (error (concat ":FUNCTION `mon-append-to-register' "
                                      "-- REGISTER does not contain text"))))))
  (if delete-region-p (delete-region start end)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 18, 2009 @ 11:26.02 AM - by MON KEY>
(defun mon-append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.\n
Insert it into BUFFER before point.\n
BUFFER names an existing buffer.\n
START and END specify the portion of the current buffer to be copied.\n
This is an alternative definition of `append-to-buffer' with a \"\n\".\n
:SEE-ALSO `mon-append-to-register', `mon-kill-appending'.\n►►►"
  (interactive `(,(read-buffer 
                   (concat ":FUNCTION `mon-append-to-buffer'"
                           "-- append to buffer: ")
                   (other-buffer (current-buffer) t))
                 ,@(if (use-region-p)
                       `(,(region-beginning) ,(region-end))
                     `(,(point) ,(point)))))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
	     (windows (get-buffer-window-list append-to t t))
	     point)
	(set-buffer append-to)
	(setq point (point))
	(barf-if-buffer-read-only)
	(newline)  ;; :ADDED `newline', else identical to `append-to-buffer'.
	(insert-buffer-substring oldbuf start end)
	(dolist (window windows)
	  (when (= (window-point window) point)
	    (set-window-point window (point))))))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-03-20T12:59:35-04:00Z}#{10116} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-03-10T20:04:58-05:00Z}#{10104} - by MON KEY>
(defun mon-g2be (&optional min/max)
  "Move point as with `goto-char' to `point-max' or `point-min'.\n
If optional arg MIN/MAX is non-nil and greater than 0 goto-char `point-max'.
Default is to goto-char `point-min'.\n
:EXAMPLE\n\n(mon-g2be)\n\n(mon-g2be 0)\n\n(mon-g2be 1)\n
:SEE-ALSO `point-min', `point-max', `buffer-end'.\n►►►"
  (goto-char
   ;; :WAS (if (> min/max 0) (point-max) (point-min))))
   (if (and min/max (> min/max 0))
       (point-max) (point-min))))

;;; ==============================
(defun mon-region-position (&optional insrtp intrp)
  "Return details of the postion of current region.\n
Return value is a list of key value pairs as returned by accessor functions:\n
  <KEY>             <VALUE>         ;;  <ACCESSOR>
 :USE-REGION-P     { t | nil }      ;; `use-region-p' 
 :REGION-ACTIVE-P  { t | nil }      ;; `region-active-p' 
 :REG-BEG          <INTEGER>        ;; `region-beginning'
 :REG-END          <INTEGER>        ;; `region-end'\n
When optional arg insrtp is non-nil or called interactively return value is a a
string prefixed by:\n
 \":FUNCTION `mon-region-position' -- current region \"\n
When insrtp is non-nil or called-interactively with a prefix arg the string is
inserted in current-buffer at point. Does not move point.\n
When called-interactively and insrtp is ommitted string is echod to the
minibuffer as if by `message'.\n
:EXAMPLE\n\n\(mon-region-position\)\n\n\(mon-region-position nil t)\n
\(save-excursion \(set-mark \(point\)\) \(forward-char 3\) \(mon-region-position nil t\)\)\n
\(save-excursion \(set-mark \(point\)\) \(forward-char 3\) \(mon-region-position\)\)\n
:SEE-ALSO `mon-region-unfill', `mon-region-capitalize', `mon-region-reverse'.\n►►►"
  (interactive "P\np")
  (let ((mrp `(:USE-REGION-P    ,(use-region-p) 
               :REGION-ACTIVE-P ,(region-active-p)  
               :REG-BEG         ,(region-beginning)
               :REG-END         ,(region-end))))
    (if (or insrtp intrp)
        (progn
          (setq mrp 
                (concat ":FUNCTION `mon-region-position' "
                         "-- current region " 
                         (mapconcat #'(lambda (fr) (format "%s" fr))
                                    mrp " ")))
          (cond (insrtp (save-excursion (insert mrp)))
                (intrp (message mrp))))
      mrp)))
         
;;; ==============================
;;; :CHANGESET 1788
;;; :CREATED <Timestamp: #{2010-05-28T14:44:04-04:00Z}#{10215} - by MON KEY>
(defun mon-region-length (&optional insrtp intrp)
  "Return the length of active region.\n
Signal an error if `region-active-p' is null.\n
When optional arg INSRTP is non-nil or called-interactively with prefix arg
insert length of region at region-end.\n
When called-interactively and INSRTP is ommitted message the region length.\n
:EXAMPLE\n\n\(progn\n \(push-mark \(line-beginning-position\) nil t\)
  \(mon-region-length nil t\)\)\n
:SEE-ALSO `mon-region-unfill',`mon-region-capitalize',`mon-region-reverse'.\n►►►"
  (interactive "P\np")
  (unless (region-active-p)
    (error ":FUNCTION `mon-region-length' -- there is no active region"))
  (let* ((rerb (- (region-end) (region-beginning)))
         ;;(rerb-frmt  (format ":REGION-LENGTH %d" rerb)))
         (rerb-frmt (format (concat ":FUNCTION `mon-region-length' " 
                                    "-- :REGION-LENGTH %d") rerb)))
    (cond ((and intrp (not insrtp)) (message rerb-frmt))
           (insrtp (unless (eq (point) (region-end))
                     (goto-char (region-end)))
                   (insert rerb-frmt))
           (t rerb))))
;;
;;; :TEST-ME (progn (push-mark (line-beginning-position) nil t)
;;;                    (mon-region-length nil t))

;;; ==============================
(defun mon-region-unfill (start end)
  "Do the opposite of `fill-region'.\n
Stuff all paragraphs paragraphs in the current region into long lines.\n
:SEE-ALSO `mon-line-strings-indent-to-col', `mon-line-indent-from-to-col',
`mon-string-fill-to-col', `mon-comment-divide->col'.\n►►►"
  (interactive "r")
  (let ((fill-column 9000))
    (fill-region start end)))

;;; ==============================
(defun mon-region-capitalize (start end)
  "Capitalize the region.\n
\"mon-\" function name wrapper for consistency, and to aid completion
because we also have `mon-rectangle-capitalize'.\n
This function is a 1:1 duplicate of `capitalize-region'.\n
:SEE-ALSO `mon-region-unfill', `mon-region-length', `mon-region-reverse'.\n►►►"
  (interactive "r")
  (capitalize-region start end))

;;; ==============================
(defun mon-region-reverse (reg-begin reg-end &optional insrtp intrp)
  "Reverse the characters in the region.\n
When called-interactively insert the reversed as with princ.
When INSRTP is non-nil insert the reversed as with princ.
Insertion does not move point. Insertion is whitespace agnostic.\n
:SEE-ALSO `mon-word-reverse-region', `mon-region-unfill',
`mon-region-capitalize'.\n►►►"
  (interactive "r\ni\np")
  (let ((m-reg-rev 
         (apply 'concat 
                (reverse 
                 (split-string 
                  (buffer-substring-no-properties reg-begin reg-end) "")))))
    (cond (intrp (save-excursion 
                   (delete-region reg-begin reg-end)
                   (princ m-reg-rev (current-buffer))))
          (insrtp (save-excursion 
                    (delete-region reg-begin reg-end)
                    (prin1 m-reg-rev (current-buffer))))
          (t m-reg-rev))))

;;; ==============================
;;; :NOTE consider macrology? BUGGY but :WORKING-AS-OF
;;; :CREATED <Timestamp: #{2009-09-09T12:29:52-04:00Z}#{09373} - by MON>
(defun mon-test-keypresses (&optional first second third)
  "Use to test if additioanl optional prefix args have been passed to interactive.\n
:EXAMPLE\nM-34 M-x mon-test-keypresses\n
=> \(\(meta . 51\) \(meta . 52\) \(meta . 120\) mon-test-keypresses\)
:SEE-ALSO `event-basic-type', `this-command-keys-vector',
`event-modifiers', `current-prefix-arg'.\n►►►"
  (interactive "P\nP\np")
  (let ((accum-string '())
	(accum-event '())
	(self 'mon-test-keypresses))
    (mapc #'(lambda (x) 
            (cond ((= x 13) nil)
                  ((or (eql (car (event-modifiers x)) 'meta)
                       (eql (car (event-modifiers x)) 'control))
                   (setq accum-event (cons (cons (car (event-modifiers x)) (event-basic-type x)) accum-event)))
                  (t (setq accum-string (cons (char-to-string (event-basic-type x)) accum-string)))))
          (this-command-keys-vector))
    (setq accum-event (reverse accum-event))
    (setq accum-string (reverse accum-string))
    (setq accum-string (apply 'concat accum-string))
    (setq accum-string `(,@accum-event ,(if (string= accum-string self) self accum-string)))
        (prin1 accum-string)))
;;
;;; :TEST-ME (mon-test-keypresses 1 2 3) ;->("cj")("cj")
;;; :TEST-ME (call-interactively 'mon-test-keypresses);-> ("cj")("cj")

;;; ==============================
;;; :CHANGESET 1898
;;; :CREATED <Timestamp: #{2010-06-18T15:18:46-04:00Z}#{10245} - by MON KEY>
(defun mon-abort-recursive-edit ()
  "Try to exit gracefully from hung/corrupted `recursive-edit' mini-buffer.\n
Repeatedly invoke `exit-recursive-edit' and `abort-recursive-edit' when
`recursion-depth' is greater than 0.\n
:SEE-ALSO `mon-abort-autosave-when-fucked', `exit-recursive-edit',
`abort-recursive-edit', `command-error-function', `throw-on-input',
`mon-help-key-functions'.\n►►►"
  (interactive)
  (while (> (recursion-depth) 0)
    (progn (abort-recursive-edit)
           (exit-recursive-edit))))

;;; ==============================
;;; :CHANGESET 2087
;;; :CREATED <Timestamp: #{2010-08-25T18:12:21-04:00Z}#{10343} - by MON KEY>
(defun mon-abort-autosave-when-fucked (&optional fucked-minibuffer-count)
  "HELP! Getme the fuck out of autosave hell.\n
Optional arg FUCKED-MINIBUFFER-COUNT is a positive integer specifying the
maximum number of screwed up minibuffers e.g. those named \" *Minibuf-<N>*\"
which need to be destroyed indiscriminately. Default is 300.\n
This happens when visiting a file encoded with raw-bytes and `auto-save-default'
is non-nil; sometimes when we accidently C-g to escape the autosave prompt: the
entire window/buffer/minibuffer stack gets corrupted such that every subsequent
C-g generates a new minibuffer which prompts: 
 \"Select coding system (default raw-text):\"
and with each one looking for some non-existent temporary buffer to do their
work in, e.g. the \" *Format Temp %<N>*\" buffer created by
`format-annotate-function' which can sometimes cause something else to trigger a
message about a missing \"*Warnings*\" buffer most likely as per the internal
function `coding-system-require-warning' and her compatriots
`select-safe-coding-system-function', `select-safe-coding-system',
`coding-system-for-write', and `universal-coding-system-argument'.\n
Let binds the following auto-save-* variables to their non-middling states:\n
 `auto-save-interval' 0
 `auto-save-timeout'  0 
 `auto-save-default' nil\n
Return value is a `yes-or-no-p' prompt which reinstates a clean minibuffer.\n
:NOTE Emcas-devels when you allow creation of non completion accessible
whitespace prefixed mini-buffers you should make _DAMN_ sure that calling
functions don't implode! I loathe this practice of hiding buffers from the user.
My Emacs, my buffers!
:SEE-ALSO `mon-abort-recursive-edit', `exit-recursive-edit',
`abort-recursive-edit', `command-error-function', `throw-on-input'.\n►►►"
  (interactive)
  (let ((auto-save-interval 0)
        (auto-save-timeout  0)
        (auto-save-default nil))
    (dolist (i (number-sequence 0 (or fucked-minibuffer-count 300)))
      (let ((kill-MB (get-buffer (format " *Minibuf-%d*" i))))
        (when kill-MB
          (with-current-buffer kill-MB
            (kill-this-buffer)))))
    (yes-or-no-p 
     (concat ":FUNCTION `mon-abort-autosave-when-fucked' "
             "-- my work is done here, are you glad to have your minibuffer back: "))))

;;; ==============================
(defun mon-inhibit-read-only (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-read-only' t.\n
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.\n
:EXAMPLE\n\n\(let \(\(tt \(propertize \"I'm read only!\" 'read-only t\)\)
      \(buffer-read-only nil\)\)
  \(line-move -5\) 
  \(insert tt\)\(sit-for 2\)\(beginning-of-line\)
  \(mon-inhibit-read-only 'kill-line\)\)\n
:SEE-ALSO `mon-with-inhibit-buffer-read-only',
`mon-with-inhibit-buffer-read-only-TEST', `mon-inhibit-modification-hooks',
`mon-inhibit-point-motion-hooks', `mon-toggle-read-only-point-motion',
`view-read-only'.\n►►►"
(let ((re-inhibit (if (not inhibit-read-only) t nil)))
  (unwind-protect
      (progn 
	(setq inhibit-read-only t)
	(eval `(,func-arg)))
    (when re-inhibit (setq inhibit-read-only nil)))))
;;
;;; :TEST-ME (let ((tt (propertize "I'm read only!" 'read-only t))
;;;                   (buffer-read-only nil))
;;;               (line-move -5) 
;;;               (insert tt)(sit-for 2)(beginning-of-line)
;;;               (mon-inhibit-read-only 'kill-line))


;;; ==============================
;;; :CHANGESET 2088
;;; :CREATED <Timestamp: #{2010-08-27T20:26:53-04:00Z}#{10345} - by MON KEY>
(defun mon-get-buffer-hidden (&optional intrp)
  "Return a list conses of the currently hidden buffers.\n
Elements of list have the form:\n
 (<BUFFER-NAME> . <BUFFER>)\n
Hidden buffers are those that completion can't find because the buffers name
begins with withespace. Generally these are considered \"internal\" buffers that
the user doesn't need to see and therefor not useful for completion. However, on
occasion it can be useful to know what you don't know. Some commonly hidden
buffers include:
 \" *autoload*\" \" *autoload-file*\" \" apropos-temp\" \" *Bookmarks*\"
 \" *completion-save-buffer*\" \" *code-conversion-work*\" \" *code-converting-work*\"
 \" *Custom-Work*\"  \" *DOC*\" \" *Echo Area <N>*\" \" *Format Temp <N>*\"
 \" *info tag table*\" \" *IDO Trace*\" 
 \" *dired-check-process output*\" \" *dot-dired*\" \" *gnus work*\"
 \" *ido session*\" \" *info-browse-tmp*\" 
 \" *jka-compr-error*\" \" *jka-compr-error*\"
 \" *jka-compr-flc-temp*\" \" *jka-compr-wr-temp*\"
 \" *Marked Files*\"  \" *Minibuf-<N>*\"
 \" *recover*\" \" *spool temp*\" \" *string-output*\" \" *temp*\" 
 \" temp-info-look\" \" *Temp Input History*\" \" *text-props*\"
 \" widget-choose\" \" *Unicode Data*\" \" *url-work\" 
:EXAMPLE\n\n\(mon-get-buffer-hidden\)\n
\(mon-get-buffer-hidden\)\n
:ALIASED-BY `mon-get-hidden-buffers'\n
:ALIASED-BY `mon-help-hiddent-buffers'\n
:SEE-ALSO `mon-help-buffer-spc-*DOC*', `mon-abort-autosave-when-fucked',
`mon-abort-recursive-edit'.\n►►►"
  (interactive "P")
  (let ((bl (buffer-list))
        rslt)
    (dolist (dobl bl (unless (null rslt)
                       (setq rslt (nreverse rslt))))
      (when (eq ?\s (aref (buffer-name dobl) 0))
        (push `(,(buffer-name dobl) . ,dobl) rslt)
        rslt))))
;;
(defalias 'mon-get-hidden-buffers 'mon-get-buffer-hidden) 
(defalias 'mon-help-hidden-buffers 'mon-get-buffer-hidden)
;;
;;; :TEST-ME (mon-get-buffer-hidden)
;;; :TEST-ME (mon-get-buffer-hidden t)


;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-26T14:56:18-04:00Z}#{10125} - by MON KEY>
(defmacro mon-with-inhibit-buffer-read-only (&rest uninhibited-body)
  "Temporairly set value of `buffer-read-only' nil in current-buffer.\n
Execute the UNINHIBITED-BODY inside an unwind-protect form which restores value
of `buffer-read-only'.\n
:EXAMPLE\n\n\(with-current-buffer \(progn 
                       \(describe-function 'mon-with-inhibit-buffer-read-only\) 
                       \(get-buffer \"*Help*\"\)\)
  \(mon-with-inhibit-buffer-read-only 
      \(sit-for 1\) \(forward-line 3\)
      \(dotimes \(i 3\) 
        \(progn \(kill-line\) \(insert \"A LINE JUST DIED HERE\\n\"\) \(sit-for 1\)\)\)\)
  \(message \"buffer-read-only value in *Help* buffer rebound -> %s\"
           \(buffer-local-value 'buffer-read-only \(get-buffer \"*Help*\"\)\)\)\)\n
\(mon-with-inhibit-buffer-read-only-TEST\)\n
\(mon-with-inhibit-buffer-read-only-TEST t\)\n
:SEE-ALSO `mon-inhibit-read-only', `mon-with-inhibit-buffer-read-only-TEST'.
`mon-inhibit-modification-hooks', `mon-inhibit-point-motion-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
  (declare (indent 3) (debug t))
  (let ((re-inhibit-bro (make-symbol "re-inhibit-bro")))
    `(let ((,re-inhibit-bro (buffer-local-value buffer-read-only (current-buffer))))
       (with-current-buffer (current-buffer)
         (unwind-protect        
              (progn
                (set (make-local-variable 'buffer-read-only) nil)
                ,@uninhibited-body)
           (when ,re-inhibit-bro (set (make-local-variable 'buffer-read-only) t)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-26T15:25:28-04:00Z}#{10125} - by MON KEY>
(defun mon-with-inhibit-buffer-read-only-TEST (&optional w-display-buffer)
  "Test function for `mon-with-inhibit-buffer-read-only'.
:EXAMPLE\n\n(mon-with-inhibit-buffer-read-only-TEST)\n
\(mon-with-inhibit-buffer-read-only-TEST t\)\n
:SEE-ALSO `mon-build-copyright-string-TEST', `mon-help-keys-wikify-TEST',
`mon-help-propertize-regexp-symbol-defs-TEST', `mon-help-propertize-tags-TEST',
`mon-help-regexp-symbol-defs-TEST', `mon-help-CL-wget-pkgs-TEST',
`mon-inhibit-read-only-TEST', `mon-line-strings-to-list-TEST',
`mon-user-system-conditionals-TEST', `mon-wget-list-to-script-TEST'.\n►►►"
  (let ((mwirot "*MON-WITH-INHIBIT-BUFFER-READ-ONLY-TEST*")
        shw-msg)
    (with-current-buffer (get-buffer-create mwirot)
      (set (make-local-variable 'buffer-read-only) t)
      (mon-with-inhibit-buffer-read-only
          (save-excursion (dotimes (txt 14) (insert "These lines of text\n")))
          (when w-display-buffer (display-buffer (current-buffer) t))
          (when w-display-buffer (sit-for 1))
        (dotimes (miro 4)
          (mon-with-inhibit-buffer-read-only 
              (forward-line 3) (kill-line)
              (when w-display-buffer (sit-for 1)))))
      (setq shw-msg 
            (if (buffer-local-value buffer-read-only (current-buffer))
                (concat ":MACRO `mon-with-inhibit-buffer-read-only' "
                        "-- Buffer is buffer-read-only, successfully re-inhibited buffer")
              (concat ":MACRO `mon-with-inhibit-buffer-read-only' "
                      "-- failed to re-inhibit buffer-read-only")))
      (if (not w-display-buffer)
          (when (eq (get-buffer mwirot) (current-buffer))
            (kill-buffer mwirot))))
    (message shw-msg)))
;;
;;; :TEST-ME (mon-with-inhibit-buffer-read-only-TEST)
;;; :TEST-ME (mon-with-inhibit-buffer-read-only-TEST t)

;;; ==============================
(defun mon-inhibit-modification-hooks (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-modification-hooks' t.\n
FUNC-ARG is a function which evaluates without parameters.
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.\n
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-point-motion-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
  (let ((re-inhibit (if (not inhibit-modification-hooks) t nil)))
    (unwind-protect
         (progn 
           (setq inhibit-modification-hooks t)
           (eval `(,func-arg)))
      (when re-inhibit (setq inhibit-modification-hooks nil)))))

;;; ==============================
(defun mon-inhibit-point-motion-hooks (func-arg)
  "Evaluate FUNC-ARG at point with `inhibit-point-motion-hooks' t.\n
Evaluation occurs inside an unwind protect so 'safe-enough' 
for invoking 'one off functions' such-as `kill-line' without
the tedium of building the entire scaffolding.\n
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-modification-hooks',
`mon-toggle-read-only-point-motion'.\n►►►"
  (let ((re-inhibit (if (not inhibit-point-motion-hooks) t nil)))
    (unwind-protect
	(progn 
	  (setq inhibit-point-motion-hooks t)
	  (eval `(,func-arg)))
      (when re-inhibit (setq inhibit-point-motion-hooks nil)))))

;;; ==============================
;;; :TODO I'm no longer sure if this is TRT should we be checking for
;;; `buffer-local-variables'/`buffer-local-value' of these too?
;;; :CREATED <Timestamp: Monday June 15, 2009 @ 05:36.12 PM - by MON KEY>
(defun mon-toggle-read-only-point-motion ()
  "Toggle `inhibit-read-only' and `inhibit-point-motion-hooks'.\n
:SEE-ALSO `mon-inhibit-read-only', `mon-inhibit-point-motion-hooks',
`mon-inhibit-modification-hooks', `mon-naf-mode-toggle-restore-llm'.\n►►►"
  (interactive)
  (if (or (bound-and-true-p inhibit-read-only)
          (bound-and-true-p inhibit-point-motion-hooks)) ;;inhibit-read-only))
      (progn
	(setq inhibit-read-only nil)
	(setq inhibit-point-motion-hooks nil))
    (progn
      (setq inhibit-read-only t)
      (setq inhibit-point-motion-hooks t))))

;;; ==============================
;;; :COURTESY Stefan Reichor <stefan@xsteve.at> :HIS xsteve-functions.el
(defun mon-wrap-selection (&optional front-arg rear-arg)
  "Wraps contents region with a front and rear delimeter.\n\n
:PROMPT-FOR 
            Front Delimiter:  <- Delmiter for beginning of region
            Rear Delimiter:   <- Delmiter for end of region\n\n
:EXAMPLE
          Point/Mark of region contain: My cats breath smells like cat food
          Front's prompt is provided: |[
          Rear's prompt is provided:  ]|
          Return: |[My cats breath smells like catfood]|\n
:SEE-ALSO `mon-wrap-url', `mon-wrap-span', `mon-wrap-text', `mon-wrap-with'.\n►►►"
  (interactive)
  (let* ((in-front (or front-arg (read-string "Front Delimiter: ")))
         (in-rear (or rear-arg (read-string "Rear Delimiter: "))))
    (if mark-active
        (progn
          (save-excursion
            (goto-char (region-beginning))
            (insert in-front))
          (save-excursion
            (goto-char (region-end))
            (insert in-rear)))
      (insert in-front)
      (save-excursion
        (insert in-rear)))))

;;; ==============================
;;; :RENAMED `mon-trunc' -> `mon-toggle-truncate-line'
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T15:45:18-04:00Z}#{09441} - by MON KEY>
(defun mon-toggle-truncate-line (&optional intrp)
  "Toggle the truncate-line variable and redraw the display.\n
:SEE-ALSO `mon-toggle-eval-length', `print-length',
`mon-toggle-dired-dwim-target', `mon-toggle-menu-bar'
`mon-toggle-show-point-mode', `mon-naf-mode-toggle-restore-llm', 
`mon-toggle-read-only-point-motion', `mon-inhibit-modification-hooks',
`mon-inhibit-point-motion-hooks', `mon-inhibit-read-only'.\n►►►"
  (interactive "p")
  (toggle-truncate-lines nil)
  (if intrp (message
             (concat ":FUNCTION `mon-toggle-truncate-line' " 
                     (if truncate-lines
                         "-- truncating lines (... $)"
                       "-- wrapping lines (...\\)"))))
  (redraw-display))


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
              (read-number "New length for eval-expression-print-length: "))
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
;;; :NOTE To remind us where we're going:
;;; (defun mon-wrap-artist-name () "" (interactive)
;;;   (mon-wrap-text "\\@:artist[" "]")
(defun mon-wrap-text (arap brap &optional insrtp intrp)
  "Return current word with the string args ARAP and BRAP.\n
When optional arg INSRTP is non-nil or called-interactively insert wrapped word
at point. Does not move point.\n
:EXAMPLE\n\n\(mon-wrap-text \"\\\\@:artist[\" \"]\"\)Some-Name\n
:SEE-ALSO `mon-wrap-selection', `mon-wrap-url', `mon-wrap-span',
`mon-wrap-with'.\n►►►"
  (interactive "i\np")
  (save-excursion
    (let (p1 p2 word-to-wrap)
      (if (and transient-mark-mode mark-active)
          (progn (setq p1 (region-beginning)) (setq p2 (region-end)))
        (progn
          (skip-chars-backward "-A-Za-z")
          (setq p1 (point))
          (skip-chars-forward "-A-Za-z")
          (setq p2 (point))))
      (setq word-to-wrap (buffer-substring-no-properties p1 p2))
      (if (or insrtp intrp)
          (progn
            (goto-char p2) (insert brap)
            (goto-char p1) (insert arap))
        (concat arap word-to-wrap brap)))))
;;
;;; :TEST-ME (mon-wrap-text "\\@:artist[" "]")Some-Name
;;; :TEST-ME (mon-wrap-text "\\@:artist[" "]" t)Some-Name

;;; ==============================
(defun mon-wrap-with (front-wrap back-wrap)
  "Wrap the current word or region with FRONT-WRAP and BACK-WRAP.\n
:SEE-ALSO `mon-wrap-selection', `mon-wrap-url', `mon-wrap-span',
`mon-wrap-text', `mon-wrap-with'.\n►►►"
  (interactive "sEnter string for front-wrap:\nsEnter String for back-wrap: ")
  (mon-wrap-text front-wrap back-wrap))

;;; ==============================
;;; :COURTESY Sandip Chitale <sandipchitale@attbi.com>
(defun mon-choose-from-menu (menu-title menu-items)
  "Choose from a list of choices from a popup menu.\
:EXAMPLE\n\(mon-choose-from-menu 
 \"Bubbas-Choice\" '\(\"one-bubba\" \"two-bubba\" \"three-bubba\"\)\)\n
:SEE-ALSO `choose-completion', `x-popup-menu'.\n►►►"
  (let (mcfm-item mcfm-item-list)
    (while menu-items
      (setq mcfm-item (car menu-items))
      (if (consp mcfm-item)
          (setq mcfm-item-list 
                (cons (cons (car mcfm-item) (cdr mcfm-item) ) mcfm-item-list))
        (setq mcfm-item-list 
              (cons (cons mcfm-item mcfm-item) mcfm-item-list)))
      (setq menu-items (cdr menu-items)))
    (x-popup-menu t (list menu-title (cons menu-title (nreverse mcfm-item-list))))))
;;
;;; :TEST-ME (mon-choose-from-menu "Bubbas-Choice" '("one-bubba" "two-bubba" "three-bubba"))

;;; ==============================
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
  (let (backup pamm-start pamm-end)
    (save-excursion
      (setq backup
            (or (save-match-data (looking-at match-regexp))
                (and (search-forward-regexp match-regexp nil 'limit)
                     (setq pamm-end t)
                     (goto-char (match-beginning 0))
                     nil)
                ;; Failed search doesn't change match-data.
                (search-backward-regexp match-regexp nil t)))
      (when (or backup pamm-end) 
        (setq pamm-start (match-beginning 0))
        (setq pamm-end (match-end 0)))
      (if backup
          (while (and (not (bobp))
                      (progn (backward-char) t)
                      (save-match-data (looking-at match-regexp))
                      (= (match-end 0) pamm-end))
            (setq pamm-start (point)))
        (or (bobp) (re-search-forward match-regexp nil t))))
    (and pamm-start
         (progn 
           (goto-char pamm-end)
           t)
         (buffer-substring-no-properties pamm-start pamm-end))))

;;; ===================================
;; :WHITESPACE 
;;; EOL, BOL, EOB, BOB, LEP, LBP, etc.
;;; ===================================

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `space-p'
;;; :MODIFICATIONS <Timestamp: Tuesday February 10, 2009 @ 04:11.49 PM - by MON KEY>
(defun mon-spacep (&optional pt after)
  "Return non-nil when char before point is a 'space' character.\n
If non-nil, PT (a char position) returns t for a'space' before/after PT.\n
If AFTER is non-nil return t when char after point is a 'space'.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-cln-spc-tab-eol'.\n►►►"
  (let* ((look-pt (and pt pt))
	 (pc (cond
	      ((and pt after) 
	       (char-after look-pt))
	      ((and after)
	       (char-after))
	      ((and pt)
	       (char-before look-pt))
	      ((and (not pt) (not after))
	       (char-before))))
	 (space-char '(9 10 11 12 13 32))
	 (test-char (member pc space-char)))
    (when test-char t)))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:11.19 PM - by MON KEY>
(defun mon-spacep-not-bol (&optional intrp)
  "Return non-nil if character after point at BOL is not a space.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep', `mon-line-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol'.\n►►►"
  (interactive "p")
  (let* ((char-bol (char-after (point-at-bol)))
	 (space-char '(9 10 11 12 13 32))
	 (not-space (not (member char-bol space-char))))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-not-bol' "
                                  "-- char after point at BOL"
                                  (if not-space " _NOT_ " " IS ") "whitespace")))
          (t not-space))))
;; 
;;; :TEST-ME (mon-spacep-not-bol)
;;; :TEST-ME (mon-spacep-not-bol t)


;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:11.19 PM - by MON KEY>
(defun mon-spacep-is-bol (&optional intrp)
  "Return non-nil if character after point at BOL _is_ a space.\n
:SEE-ALSO `mon-spacep-not-bol', `mon-spacep', `mon-line-bol-is-eol', 
`mon-line-next-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol'.\n►►►"
  (interactive "p")
  (let* ((char-bol (char-after (point-at-bol)))
	 (space-char '(9 10 11 12 13 32))
	 (is-space (numberp (car (member char-bol space-char)))))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-is-bol' "
                                  "-- char after point at BOL"
                                  (if is-space " IS " " _NOT_ ") "whitespace")))
          (t is-space))))
;; 
;;; :TEST-ME (mon-spacep-is-bol)
;;; :TEST-ME (mon-spacep-is-bol t)

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 05:39.17 PM - by MON KEY>
(defun mon-spacep-is-after-eol (&optional intrp)
  "Return non-nil if character after eol _is_ a space.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol', `mon-cln-spc-tab-eol'.\n►►►"
  (interactive "p")
  (let* ((after-eol (char-after (1+ (line-end-position))))
	 (space-char '(9 10 11 12 13 32))
	 (is-space (numberp (car (member after-eol space-char))))
	 (rtrn is-space))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-is-after-bol' "
                                  "-- whitespace" (if rtrn  " IS " " _NOT_ ") 
                                  "after EOL")))
          (t rtrn))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 05:54.27 PM - by MON KEY>
(defun mon-spacep-is-after-eol-then-graphic (&optional intrp)
  "Return non-nil if character after eol _is_ a space and next char is not.\n
:EXAMPLE\n\n(mon-spacep-is-after-eol-then-graphic t\)
\(mon-spacep-is-after-eol-then-graphic t)\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-at-eol',`mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let* ((after-eol (char-after (1+ (line-end-position))))
	 (after-eol-then (char-after (+ (line-end-position) 2)))	 
	 (space-char '(9 10 11 12 13 32))
	 (is-space (numberp (car (member after-eol space-char))))
	 (not-space (not (member after-eol-then space-char)))
         (rtrn (and is-space not-space)))
    (cond (intrp (message
                  (concat ":FUNCTION `mon-spacep-is-after-eol-then-graphic' "
                          "-- space or tab" (if rtrn " IS " " _NOT_")
                          "at beggining of next line " 
                          (if rtrn "and next char IS " 
                            "or next char _NOT_ ") 
                          "graphic")))
          (t rtrn))))
;;
;;; :TEST-ME (mon-spacep-is-after-eol-then-graphic)
;;; :TEST-ME (mon-spacep-is-after-eol-then-graphic t)
;;; :TEST-ME (mon-spacep-is-after-eol-then-graphic t)


;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:58.38 PM - by MON KEY>
(defun mon-spacep-at-eol (&optional intrp)
  "Return non-nil if character at eol is either TAB (char 9) or SPC (char 32).\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol',`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive "p")
  (let ((rtrn  (or (= (char-before (point-at-eol)) 9)
                   (= (char-before (point-at-eol)) 32))))
    (cond (intrp (message (concat ":FUNCTION `mon-spacep-at-eol' "
                                  "-- space or tab" (if rtrn " _IS_ " " _NOT_ ")
                                  "at EOL")))
          (t rtrn))))

;;; ==============================
;;; :COURTESY Andy Stewart <lazycat.manatee@gmail.com> :WAS `colp'
;;; :SEE (URL `http://www.emacswiki.org/emacs/lazycat-toolkit.el')
(defun mon-spacep-first ()
  "Return non-nil if point is first non-whitespace character of line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol'.\n►►►"
  (let (current-point)
    (setq current-point (point))
    (save-excursion
      (back-to-indentation)
      (equal current-point (point)))))

;;; ==============================
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:17.51 PM - by MON KEY>
(defun mon-line-bol-is-eol (&optional intrp)
  "Return non-nil if postion at beginning of line is eq end of line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol'.\n►►►"
  (interactive "p")
  (let ((bol-eol(= (line-end-position) (line-beginning-position))))
    (cond (intrp (concat ":FUNCTION `mon-line-bol-is-eol' "
                         " -- BOL " (if bol-eol " _IS_ " " _NOT_ ") " EOL"))
          (t bol-eol))))
;;
;;; :TEST-ME (save-excursion (previous-line) (beginning-of-line) (mon-line-bol-is-eol))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-15T18:16:04-04:00Z}#{09424} - by MON KEY>
;;; Tired of compiler warnings for `previous-line' changed to (forward-line - n)
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:38.46 PM - by MON KEY>
(defun mon-line-previous-bol-is-eol (&optional intrp move-times)
  "Return t if position at beginning of previous line is eq end of line.
When not called-interactively MOVE-TIMES arg examines Nth previous line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol',`mon-cln-spc-tab-eol'.\n►►►"
  (interactive "p")
  (let ((p-bol-eol (save-excursion 
                     ;;(previous-line) move-times) 
                     (forward-line (if move-times (- move-times) (- 1)))
		     (beginning-of-line) 
		     (mon-line-bol-is-eol))))
    (cond (intrp
           (concat ":FUNCTION `mon-line-previous-bol-is-eol' "
                   "-- previous line " (if p-bol-eol " _IS_ " " _NOT_ ")
                   "BOL and"  (if p-bol-eol " _IS_ " " _NOT_ ") "EOL"))
          (t p-bol-eol))))
;;
;;; :TEST-ME  (mon-line-previous-bol-is-eol)
;;; :TEST-ME  (mon-line-previous-bol-is-eol 4)

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-15T18:17:13-04:00Z}#{09424} - by MON KEY>
;;; Tired of compiler warnings for `next-line' changed to (forward-line n)
;;; :CREATED <Timestamp: Thursday May 07, 2009 @ 03:38.46 PM - by MON KEY>
(defun mon-line-next-bol-is-eol (&optional intrp move-times)
  "Return t if position at beginning of previous line is eq end of line.\n
When not called-interactively MOVE-TIMES arg examines Nth previos line.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-previous-bol-is-eol',
`mon-spacep-is-after-eol', `mon-spacep-is-after-eol-then-graphic',
`mon-spacep-at-eol',`mon-cln-spc-tab-eol'.\n►►►"
(interactive "p")
  (let ((n-bol-eol (save-excursion 
                     (forward-line (if move-times move-times))
                     ;; (next-line move-times) 
		     (beginning-of-line) 
		     (mon-line-bol-is-eol))))
    (cond (intrp
           (concat ":FUNCTION `mon-line-next-bol-is-eol' "
                   "-- next line BOL "(if n-bol-eol " _IS_ " " _NOT_ ") "EOL"))
          (t n-bol-eol))))

;;
;;; :TEST-ME (mon-line-next-bol-is-eol)

;;; ==============================
;;; :CREATED <Timestamp: Friday May 08, 2009 @ 05:58.27 PM - by MON KEY>
(defun mon-line-eol-is-eob (&optional intrp)
  "Return t if point EOL is also EOB \(point-max\).\n
:NOTE Does not test for narrowing!\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-line-eol-is-eob'
`mon-spacep-at-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic'.\n►►►"
  (interactive "p")
  (let ((rtrn (= (point-at-eol) (buffer-end 1))))
    (cond (intrp
           (concat 
            ":FUCTION `mon-line-eol-is-eob' "
            "-- EOL" (if rtrn " _IS_ " " _NOT_ ") "EOB"))
          (t rtrn))))
;;
;;; :TEST-ME (mon-line-eol-is-eob t)
;;; :TEST-ME (with-temp-buffer (mon-line-eol-is-eob t))

;;; ==============================
;;; "To get the same same type of functionality at the end of the line, try this
;;; function. I bind it to my <end> key just like the <home> key above. It jumps
;;; between the actual end-of-line and the end of the code line which is different
;;; if the line has comments on the end."
;;; :SEE (URL `http://www.emacswiki.org/emacs/BackToIndentationOrBeginning')
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:36.44 PM - by MON KEY>
;;; ==============================
(defun mon-line-end-or-code-end () 
  "Move point to EOL. If point is already there, to EOL sans comments.\n
That is, the end of the code, ignoring any trailing comment or whitespace.\n
:NOTE this does not handle 2 character  comment starters like // or /*.
Instances of such chars are be skipped.\n
:SEE-ALSO `mon-spacep-is-bol', `mon-spacep-not-bol',
`mon-spacep', `mon-line-bol-is-eol', `mon-line-next-bol-is-eol',
`mon-line-previous-bol-is-eol', `mon-spacep-is-after-eol',
`mon-spacep-is-after-eol-then-graphic', `mon-spacep-at-eol',
`mon-cln-spc-tab-eol'.\n►►►"
  (skip-chars-backward " \t")
  (let ((mleoce-pt (point))
        (mleoce-lbp (line-beginning-position))
        mleoce-lim)
      (when (re-search-backward "\\s<" mleoce-lbp t)
	(setq mleoce-lim (point))
	(if (re-search-forward "\\s>" (1- mleoce-pt) t)
	    (goto-char mleoce-pt)
	  (goto-char mleoce-lim)               ; test here ->
          (while (looking-back "\\s<" (1- (point)))
            (backward-char))
          (skip-chars-backward " \t")))))

;;; ==============================
;;; :COURTESY :FILE thing-at-point.el
;;; :TODO Wrap in a function and install under bol/eol funcs in mon-utils.el
;;; :CREATED <Timestamp: #{2009-09-14T15:15:57-04:00Z}#{09381} - by MON KEY>
;;; (funcall (lambda () (if (bolp) (forward-line -1) (beginning-of-line))))


;;; ==============================
;;; :CHANGESET 1885
;;; :CREATED <Timestamp: #{2010-06-16T11:43:14-04:00Z}#{10243} - by MON KEY>
(defun mon-line-move-n (&optional move-cnt) 
  "Move cursor as if by `line-move' with its NOERROR arg non-nil.\n
When optional arg MOVE-CNT non-nil move cursor n lines.\n
Default is to move 0 lines.
:EXAMPLE\n\n\(mon-line-move-next\)
\(mon-line-move-next 3\)\n
\(mon-line-move-next -3\)\n
:NOTE Function is intended for use with `mon-keybind-*' functions which would
otherwise duplicate anonymous forms with identical behavior.\n
:SEE-ALSO `mon-line-move-prev', `mon-line-move-next',
`mon-scroll-down-in-place', `mon-scroll-up-in-place'.\n►►►"
  (interactive "P")
  (line-move (or (and (integerp move-cnt) move-cnt) 0) t))

;;; ==============================
;;; :CHANGESET 1885
;;; :CREATED <Timestamp: #{2010-06-16T12:09:24-04:00Z}#{10243} - by MON KEY>
(defun mon-line-move-next (&optional move-next-cnt)
  "Move cursor vertically forward as if by `mon-line-move-n'.\n
When optional arg MOVE-NEXT-CNT non-nil move cursor n lines. Default is 1.\n
:EXAMPLE\n\n(mon-line-move-next)\n\n\(mon-line-move-next 3\)\n
:NOTE Function is intended for invocation from `mon-keybind-*' functions which would
otherwise duplicate anonymous forms with identical behavior.\n
:SEE-ALSO `mon-line-move-prev', `mon-scroll-down-in-place',
`mon-scroll-up-in-place'.\n►►►"
  (interactive "P")
  (mon-line-move-n 
   (or (and (integerp move-next-cnt) (abs move-next-cnt)) 1)))

;;; ==============================
;;; :CHANGESET 1885
;;; :CREATED <Timestamp: #{2010-06-16T12:09:54-04:00Z}#{10243} - by MON KEY>
(defun mon-line-move-prev (&optional move-prev-cnt)
  "Move cursor vertically forward as if by `mon-line-move-n'.\n
When optional arg MOVE-PREV-CNT non-nil move cursor n lines. Default is -1.\n
:EXAMPLE\n\n\(mon-line-move-prev\)\n\n\(mon-line-move-prev 3)\n
:NOTE Function is intended for invocation from `mon-keybind-*' functions which would
otherwise duplicate anonymous forms with identical behavior.\n
:SEE-ALSO `mon-line-move-next', `mon-scroll-down-in-place',
`mon-scroll-up-in-place'.\n►►►"
  (interactive "P")
  (mon-line-move-n 
   (or (and (integerp move-prev-cnt) (- (abs move-prev-cnt))) -1)))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of May 27, 2001
;;; :WAS `get-next-line' -> `mon-line-get-next'
;;; :CHANGESET 1771 <Timestamp: #{2010-05-25T19:42:55-04:00Z}#{10212} - by MON KEY>
(defun mon-line-get-next ()
  "Return the next line in the buffer, leaving point following it.\n
Return nil at `end-of-buffer'.\n
:EXAMPLE\n\n(mon-line-get-next)\nBubba on a line.\n
\(save-excursion \(equal \(mon-line-get-next\) \"\"\)\)\n
:SEE-ALSO `mon-string-ify-current-line'.\n►►►"
  (let (mlgn-start)
    (setq mlgn-start (progn (forward-line 0) (point)))
    (forward-line 1)
    (if (eql mlgn-start (point))
	nil
      (progn (setq mlgn-start (point)) 
             (buffer-substring-no-properties 
              mlgn-start 
              (progn (end-of-line) (point)))))))
;;
;;; :TEST-ME (save-excursion (equal (mon-line-get-next) ""))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-25T17:59:00-05:00Z}#{10084} - by MON KEY>
(defalias 'mon-line-keep-match  'keep-lines)
(defalias 'mon-line-delete-match 'flush-lines)
(defalias 'mon-line-count-match  'how-many)

;;; ==============================
;;; :CHANGESET 1708 <Timestamp: #{2010-04-12T16:41:21-04:00Z}#{10151} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-08T15:18:51-04:00Z}#{09372} - by MON>
(defun mon-line-find-duplicates (&optional insrtp intrp)
  "Locate adjacent duplicate lines in buffer.\n
Functions which find duplicate lines don't always sort lines.
Where lines of a file are presorted can be use to locate duplicates before
removing, i.e. situations of type: `uniquify-maybe'.  Can extend
`find-duplicate-lines' by comparing its result list with one or more of the list
comparison procedures `set-difference', `union', `intersection', etc.\n
:SEE-ALSO `mon-line-find-duplicates-cln', `mon-cln-uniq-lines',
`mon-cln-blank-lines', `mon-line-get-next', `uniq', `uniq-region'.\n►►►"
  (interactive "i\np")
  (let ((max-pon (line-number-at-pos (point-max)))
	gather-dups)
    (save-excursion
      (while (< (line-number-at-pos) max-pon) (= (forward-line) 0)
	   (let ((mlfd-this-line (buffer-substring-no-properties 
                             (line-beginning-position 1) (line-end-position 1)))
		 (mlfd-next-line (buffer-substring-no-properties 
                             (line-beginning-position 2) (line-end-position 2))))
	     (when (equal mlfd-this-line mlfd-next-line)  
               (setq gather-dups (cons mlfd-this-line gather-dups))))))
    (setq gather-dups (remove "" gather-dups))
    (if (or insrtp intrp)
          (progn (newline) (princ gather-dups (current-buffer)) (newline))
        gather-dups)))

;;; ==============================
(defun mon-line-count-region (start end)
  "Return a mini-buffer message with regions' number of lines and characters.\n
:SEE-ALSO `mon-line-count-buffer', `mon-word-count-chars-region',
`mon-word-count-region', `mon-word-count-analysis',
`mon-word-count-occurrences'.\n►►►"
  (interactive "r")
  (count-lines-region start end))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-01T11:05:33-05:00Z}#{10091} - by MON KEY>
(defun mon-line-count-buffer (&optional some-other-buffer intrp)
 "Return cons'd list of number of lines and chars in buffer.\n
Car of return value is number of lines cdr is the number of chars.\n
When optional arg SOME-OTHER-BUFFER is non-nil return line and char count for
that buffer. Default is current-buffer.\n
:EXAMPLE\n\n\(mon-line-count-buffer\)\n\n(apply 'mon-line-count-buffer nil '(t)\)\n
\(let \(\(mlcb-tmp-buf \(get-buffer-create \"*MLCB-TMP-BUF*\"\)\)
      \(mlcb-rnd-str \(random 1024\)\) 
      \(mlcb-rnd-len \(random 68\)\)
      mlcb-cnt\)
  \(unwind-protect
       \(progn
         \(with-current-buffer mlcb-tmp-buf
           \(dotimes \(r \(1- \(/ mlcb-rnd-str mlcb-rnd-len\)\)\)
             \(princ 
               \(concat \(make-string mlcb-rnd-len 32\) \"\\n\"\)
              \(current-buffer\)\)\)\)
         \(setq mlcb-cnt \(mon-line-count-buffer mlcb-tmp-buf\)\)\)
    \(kill-buffer mlcb-tmp-buf\)\)
  mlcb-cnt\)\n
:SEE-ALSO `mon-line-count-region', `mon-line-count-match',
`mon-line-count-matchp', `mon-word-count-analysis',
`mon-word-count-chars-region', `mon-word-count-occurrences',
`mon-word-count-region', `count-lines', `buffer-size', `line-number-at-pos'.
►►►"
  (interactive "i\np")
  (let (mlcb)
    (setq mlcb (if some-other-buffer
                   (with-current-buffer some-other-buffer
                     `(,(line-number-at-pos (buffer-end 1)) . ,(buffer-size)))
                   `(,(line-number-at-pos (buffer-end 1)) . ,(buffer-size))))
    (if intrp (prin1 mlcb) mlcb)))
;;
;;; :TEST-ME (mon-line-count-buffer)
;;; :TEST-ME (apply 'mon-line-count-buffer nil '(t))

;;; ==============================
;; :TODO The default value for BOL-CHAR-TEST needs to be refactored/extended.
;;; :CREATED <Timestamp: Thursday April 30, 2009 @ 04:42.13 PM - by MON KEY>
(defun mon-line-count-matchp (test-from line-count &optional bol-char-test intrp)
  "Return non-nil when number of lines in region is eq LINE-COUNT.\n
Arg TEST-FROM is a buffer pos to start counting from.\n
:SEE-ALSO `mon-word-count-chars-region', `mon-word-count-region',
`mon-line-count-buffer', `mon-word-count-analysis',
`mon-word-count-occurrences'.\n►►►"
  ;;(interactive "r")
  (save-excursion
    (let ((rg-start (line-number-at-pos test-from))
	  ;; Apparently when this was first written I decided that # was a good
	  ;; default BOL char to test for.
          (bct (if (and bol-char-test) 
                   bol-char-test 
                   35))  ;; (char-to-string 35) -> #
	  rg-end rg-diff)
      (progn
	(goto-char test-from)
	(line-move line-count)
	(cond ((eq (char-after (point)) bct)
	       (move-to-column 7))
	      ((eolp) (setq rg-end (line-number-at-pos (point))))))
      (setq rg-diff (- rg-end rg-start))
      (message (concat ":FUNCTION `mon-line-count-matchp' "
                       "-- line-count: %d matches: %S")
               rg-diff (eq rg-diff line-count))
      (eq rg-diff line-count))))

;;; ==============================
(defun mon-line-length-max (&optional intrp)
  "Return the maximum line length of the current buffer.\n
When called-interactively return message in mini-buffer:
\"The longest line in buffer `mon-utils.el' ends at column 115.\"\n
:SEE-ALSO `mon-line-count-buffer', `mon-region-length'.\n►►►"
(interactive "p")
  (let ((max-len 0))
    (save-excursion
      (goto-char (point-min))
      (while (eq (forward-line) 0)
        (end-of-line)
        (when (> (current-column) max-len)
          (setq max-len (current-column)))))
    (if intrp  (message 
                (concat ":FUNCTION `mon-line-length-max' " 
                        "-- buffer: %s longest line ends at column: %d")
                (buffer-name (current-buffer)) max-len)
      max-len)))
;;
;;; :TEST-ME (mon-line-length-max)
;;; :TEST-ME (mon-line-length-max t)

;;; ==============================
;;; Word, Line, String Related utils
;;; ==============================

;;; ==============================
;;; :TODO Add accessors/defaliases for some of these. To
;;; :CREATED <Timestamp: #{2010-01-12T17:06:34-05:00Z}#{10022} - by MON>
(defun mon-alphabet-as-type (type); up down)
"Reutrn an alpabetized sequence of TYPE.\n
Possible args for TYPE are:\n
 :UPCASE-VERSION       :DOWNCASE-VERSION
 `cons-keyU->num'      `cons-keyD->num'
 `cons-keyU->stringU'  `cons-keyD->stringD'
 `cons-symU->num'      `cons-symD->num'
 `cons-stringU->num'   `cons-stringD->num'
 `plistU->stringU'     `plistD->stringD'
 `plistU->num'         `plistD->num'
 `list-stringU'        `list-stringD'
 `list-symbolU'        `list-symbolD'
 `stringU-w-spc'       `stringD-w-spc'
 `stringU-w-nl'        `stringD-w-nl'\n
:EXAMPLE\n
\(mon-alphabet-as-type 'cons-keyU->num\)\n\(mon-alphabet-as-type 'cons-keyD->num\)
\(mon-alphabet-as-type 'cons-keyU->stringU\)\n\(mon-alphabet-as-type 'cons-keyD->stringD\)
\(mon-alphabet-as-type 'cons-symU->num\)\n\(mon-alphabet-as-type 'cons-symD->num\)
\(mon-alphabet-as-type 'cons-stringU->num\)\n\(mon-alphabet-as-type 'cons-stringD->num\)
\(mon-alphabet-as-type 'plistU->stringU\)\n\(mon-alphabet-as-type 'plistD->stringD\)
\(mon-alphabet-as-type 'plistU->num\)\n\(mon-alphabet-as-type 'plistD->num\)
\(mon-alphabet-as-type 'list-stringU\)\n\(mon-alphabet-as-type 'list-stringD\)
\(mon-alphabet-as-type 'list-symbolU\)\n\(mon-alphabet-as-type 'list-symbolD\)
\(mon-alphabet-as-type 'stringU-w-nl\)\n\(mon-alphabet-as-type 'stringD-w-nl\)
\(mon-alphabet-as-type 'stringU-w-spc\)\n\(mon-alphabet-as-type 'stringD-w-spc\)\n
:NOTE This procedure isn't necessarily efficient but it does have the benefit
of being entirely self contained, and therefor does not rely on external calls.\n
:ALIASED-BY `mon-make-list-alphabet'\n
:SEE-ALSO `mon-alphabet-as-cons-keyU->num', `mon-alphabet-as-cons-keyD->num',
`mon-alphabet-as-cons-symU->num', `mon-alphabet-as-cons-symD->num',
`mon-alphabet-as-cons-stringU->num', `mon-alphabet-as-cons-stringD->num',
`mon-alphabet-as-cons-keyU->stringU', `mon-alphabet-as-cons-keyD->stringD',
`mon-alphabet-as-plistU->stringU', `mon-alphabet-as-plistD->stringD',
`mon-alphabet-as-plistU->num', `mon-alphabet-as-plistD->num',
`mon-alphabet-as-list-stringU', `mon-alphabet-as-list-stringD',
`mon-alphabet-as-list-symbolU', `mon-alphabet-as-list-symbolD',
`mon-alphabet-as-stringU-w-nl', `mon-alphabet-as-stringD-w-nl',
`mon-alphabet-as-stringU-w-spc', `mon-alphabet-as-stringD-w-spc',
`mon-set-mon-alphabet-as-doc-loadtime', `mon-string-to-symbol',
`mon-symbol-to-string', `mon-string-alpha-list', `mon-is-alphanum',
`mon-is-letter', `mon-is-digit', `mon-is-alphanum-simp', `mon-is-letter-simp',
`mon-is-digit-simp', `mon-string-ify-list', `mon-string-chop-spaces',
`mon-string-replace-char', `mon-string-from-sequence',
`mon-string-to-sequence'.\n►►►"
(let ((maat-alph '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L 
              ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z))
      (tycon #'(lambda (typ)
                (mon-alphabet-as-type typ)))
      (ns26 (number-sequence 1 26))
      (maat-abet))
  (case type 
    (plistU->stringU 
     (while maat-alph (funcall #'(lambda (p)
                                   (let ((s (funcall 'string p)))
                                     (push (read (upcase (concat ":" s))) maat-abet)
                                     (push (upcase s) maat-abet)))
                               (pop maat-alph)))
     (setq maat-abet  (nreverse maat-abet)))
    (plistD->stringD 
     (let ((psd (mapcar #'(lambda (sm) (+ sm 32)) maat-alph)))
       (while psd 
         (funcall #'(lambda (smD)                                    
                      (let ((smd (funcall 'string smD)))
                        (push (read (concat ":" smd)) maat-abet)
                        (push smd maat-abet)))
                  (pop psd)))
       (setq maat-abet (nreverse maat-abet))))
    (plistU->num 
     (let ((pln (funcall tycon 'plistU->stringU)))
       (dotimes (pp (length pln) (setq maat-abet pln))
         (when (eq (logand pp 1) 0)
           (plist-put pln (elt pln pp) 
                      (if (> pp 0) (1+ (/ pp 2)) (1+ pp)))))))
    (plistD->num
     (let ((plnD (funcall tycon 'plistD->stringD)))
       (dotimes (ppD (length plnD) (setq maat-abet plnD))
         (when (eq (logand ppD 1) 0)
           (plist-put plnD (elt plnD ppD)
                      (if (> ppD 0) (1+ (/ ppD 2)) (1+ ppD)))))))
    (cons-keyU->stringU 
     (let ((plc (funcall tycon 'plistU->stringU)))
       (while plc
         (funcall #'(lambda (c)
                      (push `(,c . ,(pop plc)) maat-abet)) (pop plc))))
     (setq maat-abet (nreverse maat-abet)))
    (cons-keyD->stringD 
     (let ((kdsd (funcall tycon 'cons-keyU->stringU)))
       (mapc #'(lambda (D)
                 (let ((mk-sml (char-to-string (+ (string-to-char (cdr D)) 32))))
                   (setcar D (read (concat ":" mk-sml)))
                   (setcdr D mk-sml)))
             kdsd)
       (setq maat-abet kdsd)))
    (cons-keyU->num 
     (let ((plc (funcall tycon 'plistU->num)))
       (while plc
         (funcall #'(lambda (c)
                      (push `(,c . ,(pop plc)) maat-abet)) (pop plc))))
     (setq maat-abet (nreverse maat-abet)))
    (cons-keyD->num 
     (let ((plcD (funcall tycon 'plistD->num)))
       (while plcD
         (funcall #'(lambda (cD)
                      (push `(,cD . ,(pop plcD)) maat-abet)) (pop plcD))))
     (setq maat-abet (nreverse maat-abet)))
    (cons-symU->num 
     (let (cc)
       (do ((i (funcall tycon 'list-symbolU) (setq i (cdr i)))
            (j ns26 (setq j (cdr j))))
           ((null i) (setq cc (nreverse cc)))
         (push `(,(car i) . ,(car j)) cc))))
    (cons-symD->num 
     (let (cc)      
       (do ((i (funcall tycon 'list-symbolD) (setq i (cdr i)))
            (j ns26 (setq j (cdr j))))
           ((null i) (setq cc (nreverse cc)))
         (push `(,(car i) . ,(car j)) cc))))
    (cons-stringU->num
     (let (cc)
       (do ((i (funcall tycon 'list-stringU) (setq i (cdr i)))
            (j ns26 (setq j (cdr j))))
           ((null i) (setq cc (nreverse cc)))
         (push `(,(car i) . ,(car j)) cc))))
    (cons-stringD->num
     (let (cc)
       (do ((i (funcall tycon 'list-stringD) (setq i (cdr i)))
            (j ns26 (setq j (cdr j))))
           ((null i) (setq cc (nreverse cc)))
         (push `(,(car i) . ,(car j)) cc))))
    (list-stringD 
     (setq maat-abet (mapcar #'cdr (funcall tycon 'cons-keyD->stringD))))
    (list-stringU 
     (setq maat-abet (mapcar #'cdr (funcall tycon 'cons-keyU->stringU))))
    (list-symbolU 
     (let ((tmp-obu (make-vector 26 nil))
           (poplsu (funcall tycon 'list-stringU)))
       (while poplsu (push (intern (pop poplsu) tmp-obu) maat-abet))
       (setq maat-abet (nreverse maat-abet))))
    (list-symbolD 
     (let ((tmp-obd (make-vector 26 nil))
           (poplsd (funcall tycon 'list-stringD)))
       (while poplsd (push (intern (pop poplsd) tmp-obd) maat-abet))
       (setq maat-abet (nreverse maat-abet))))
    (stringU-w-spc 
     (mapconcat #'identity (funcall tycon 'list-stringU) " "))
    (stringU-w-nl 
     (mapconcat #'identity (funcall tycon 'list-stringU) "\n"))
    (stringD-w-spc 
     (mapconcat #'identity (funcall tycon 'list-stringD) " "))
    (stringD-w-nl 
     (mapconcat #'identity (funcall tycon 'list-stringD) "\n")))))
;;
(defalias 'mon-make-list-alphabet 'mon-alphabet-as-type)
;;
;;; :TEST-ME (mon-alphabet-as-type 'cons-keyU->num)
;;; :TEST-ME (mon-alphabet-as-type 'cons-keyD->num)
;;; :TEST-ME (mon-alphabet-as-type 'cons-symU->num)
;;; :TEST-ME (mon-alphabet-as-type 'cons-symD->num)
;;; :TEST-ME (mon-alphabet-as-type 'cons-stringU->num)
;;; :TEST-ME (mon-alphabet-as-type 'cons-stringD->num)
;;; :TEST-ME (mon-alphabet-as-type 'cons-keyU->stringU)
;;; :TEST-ME (mon-alphabet-as-type 'cons-keyD->stringD)
;;; :TEST-ME (mon-alphabet-as-type 'plistU->stringU)
;;; :TEST-ME (mon-alphabet-as-type 'plistD->stringD)
;;; :TEST-ME (mon-alphabet-as-type 'plistU->num)
;;; :TEST-ME (mon-alphabet-as-type 'plistD->num)
;;; :TEST-ME (mon-alphabet-as-type 'list-stringU)
;;; :TEST-ME (mon-alphabet-as-type 'list-stringD)
;;; :TEST-ME (mon-alphabet-as-type 'list-symbolU)
;;; :TEST-ME (mon-alphabet-as-type 'list-symbolD)
;;; :TEST-ME (mon-alphabet-as-type 'stringU-w-nl)
;;; :TEST-ME (mon-alphabet-as-type 'stringD-w-nl)
;;; :TEST-ME (mon-alphabet-as-type 'stringU-w-spc)
;;; :TEST-ME (mon-alphabet-as-type 'stringD-w-spc)


(defun mon-alphabet-as-cons-keyU->num ()
  (mon-alphabet-as-type 'cons-keyU->num))

(defun mon-alphabet-as-cons-keyD->num ()
  (mon-alphabet-as-type 'cons-keyD->num))

(defun mon-alphabet-as-cons-symU->num ()
  (mon-alphabet-as-type 'cons-symU->num))

(defun mon-alphabet-as-cons-symD->num ()
  (mon-alphabet-as-type 'cons-symD->num))

(defun mon-alphabet-as-cons-stringU->num ()
  (mon-alphabet-as-type 'cons-stringU->num))

(defun mon-alphabet-as-cons-stringD->num ()
  (mon-alphabet-as-type 'cons-stringD->num))

(defun mon-alphabet-as-cons-keyU->stringU ()
  (mon-alphabet-as-type 'cons-keyU->stringU))

(defun mon-alphabet-as-cons-keyD->stringD ()
  (mon-alphabet-as-type 'cons-keyD->stringD))

(defun mon-alphabet-as-plistU->stringU ()
  (mon-alphabet-as-type 'plistU->stringU))

(defun mon-alphabet-as-plistD->stringD ()
  (mon-alphabet-as-type 'plistD->stringD))

(defun mon-alphabet-as-plistU->num ()
  (mon-alphabet-as-type 'plistU->num))

(defun mon-alphabet-as-plistD->num ()
  (mon-alphabet-as-type 'plistD->num))

(defun mon-alphabet-as-list-stringU ()
  (mon-alphabet-as-type 'list-stringU))

(defun mon-alphabet-as-list-stringD ()
  (mon-alphabet-as-type 'list-stringD))

(defun mon-alphabet-as-list-symbolU ()
  (mon-alphabet-as-type 'list-symbolU))

(defun mon-alphabet-as-list-symbolD ()
  (mon-alphabet-as-type 'list-symbolD))

(defun mon-alphabet-as-stringU-w-nl ()
  (mon-alphabet-as-type 'stringU-w-nl))

(defun mon-alphabet-as-stringD-w-nl ()
  (mon-alphabet-as-type 'stringD-w-nl))

(defun mon-alphabet-as-stringU-w-spc ()
  (mon-alphabet-as-type 'stringU-w-spc))

(defun mon-alphabet-as-stringD-w-spc ()
  (mon-alphabet-as-type 'stringD-w-spc))

(defun mon-set-mon-alphabet-as-doc-loadtime ()
  "Put docstrings on the `mon-alphabet-as-type' convenience functions.\n
:SEE-ALSO .\n►►►"
  (let ((maat-cons
         '((mon-alphabet-as-cons-keyU->num       . "cons-keyU->num")
           (mon-alphabet-as-cons-keyD->num       . "cons-keyD->num")
           (mon-alphabet-as-cons-symU->num       . "cons-symU->num")
           (mon-alphabet-as-cons-symD->num       . "cons-symD->num")
           (mon-alphabet-as-ons-stringU->num     . "ons-stringU->num")
           (mon-alphabet-as-cons-stringD->num    . "cons-stringD->num")
           (mon-alphabet-as-cons-keyU->stringU   . "cons-keyU->stringU")
           (mon-alphabet-as-cons-keyD->stringD   . "cons-keyD->stringD")
           (mon-alphabet-as-plistU->stringU      . "plistU->stringU")
           (mon-alphabet-as-plistD->stringD      . "plistD->stringD")
           (mon-alphabet-as-plistU->num          . "plistU->num")
           (mon-alphabet-as-plistD->num          . "plistD->num")
           (mon-alphabet-as-list-stringU         . "list-stringU")
           (mon-alphabet-as-list-stringD         . "list-stringD")
           (mon-alphabet-as-list-symbolU         . "list-symbolU")
           (mon-alphabet-as-list-symbolD         . "list-symbolD")
           (mon-alphabet-as-stringU-w-nl         . "stringU-w-nl")
           (mon-alphabet-as-stringD-w-nl         . "stringD-w-nl")
           (mon-alphabet-as-stringU-w-spc        . "stringU-w-spc")
           (mon-alphabet-as-stringD-w-spc        . "stringD-w-spc")))
        (maat-doc-tmplt 
         (mapconcat #'identity 
                    '("Convenience function for `mon-alphabet-as-type' with arg '%s.\\n"
                      ":EXAMPLE\\n\\n\\(%S\\)\\n"
                      ":SEE-ALSO `mon-alphabet-as-cons-keyU->num', `mon-alphabet-as-cons-keyD->num',"
                      "`mon-alphabet-as-cons-symU->num', `mon-alphabet-as-cons-symD->num',"
                      "`mon-alphabet-as-cons-stringU->num', `mon-alphabet-as-cons-stringD->num',"
                      "`mon-alphabet-as-cons-keyU->stringU', `mon-alphabet-as-cons-keyD->stringD',"
                      "`mon-alphabet-as-plistU->stringU', `mon-alphabet-as-plistD->stringD',"
                      "`mon-alphabet-as-plistU->num', `mon-alphabet-as-plistD->num',"
                      "`mon-alphabet-as-list-stringU', `mon-alphabet-as-list-stringD',"
                      "`mon-alphabet-as-list-symbolU', `mon-alphabet-as-list-symbolD',"
                      "`mon-alphabet-as-stringU-w-nl', `mon-alphabet-as-stringD-w-nl',"
                      "`mon-alphabet-as-stringU-w-spc', `mon-alphabet-as-stringD-w-spc',"
                      "`mon-set-mon-alphabet-as-doc-loadtime', `mon-string-to-symbol',"
                      "`mon-symbol-to-string', `mon-string-alpha-list', `mon-is-alphanum',"
                      "`mon-is-letter', `mon-is-digit', `mon-is-alphanum-simp', `mon-is-letter-simp',"
                      "`mon-is-digit-simp', `mon-string-ify-list', `mon-string-chop-spaces',"
                      "`mon-string-replace-char', `mon-string-from-sequence',"
                      "`mon-string-to-sequence'.\\n►►►") "\n")))
    (dolist (maat maat-cons)
      (put (car maat) 'function-documentation (format maat-doc-tmplt (cdr maat))))))


;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-digit'
(defun mon-is-digit (maybe-digit-char)
"Reutrn non-nil when MAYBE-DIGIT-CHAR is a digit character.\n
:SEE-ALSO `mon-is-digit-simp', `mon-is-letter', `mon-is-alphanum',
`mon-string-index', `mon-string-position'.\n►►►"
  (cond ((stringp maybe-digit-char) 
         (mon-is-digit (string-to-char maybe-digit-char)))
        ((integerp maybe-digit-char) 
         (and (<= ?0 maybe-digit-char) 
              (<= maybe-digit-char ?9)))
        (t nil)))
;;
;;; :TEST-ME (mon-is-digit (char-after (point)))8
;;; :TEST-ME (mon-is-digit (char-after (point)))x

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-letter'
(defun mon-is-letter (maybe-alpha-char)
"Return non-nil when MAYBE-ALPHA-CHAR is an alpha character.\n
:EXAMPLE\n\(mon-is-alphanum \(char-after \(point\)\)\)\\?56
\(mon-is-alphanum \(char-after \(point\)\)\)8\n
:SEE-ALSO `mon-is-digit', `mon-is-alphanum', `mon-string-index',
`mon-string-position', `mon-alphabet-as-type'.\n►►►"
  (cond ((stringp maybe-alpha-char) 
         (mon-is-letter (string-to-char maybe-alpha-char)))
        ((integerp maybe-alpha-char) 
         (not (equal (downcase maybe-alpha-char) 
                     (upcase maybe-alpha-char))))
        (t nil)))
;;
;;; :TEST-ME (mon-is-letter (char-after (point)))x
;;; :TEST-ME (mon-is-letter (char-after (point)))8
;;; :TEST-ME (mon-is-letter ?á)


;;; ==============================
(defun mon-is-alphanum (maybe-alphanum)
  "Return t when MAYBE-ALPHANUM is either an alpha character or integer.\n
:EXAMPLE\n\(mon-is-alphanum \(char-after \(point\)\)\)\C-h 
\(mon-is-alphanum \(char-after \(point\)\)\)8\n
:SEE-ALSO `mon-is-digit', `mon-is-digit-2', `mon-string-index', 
`mon-string-position', `mon-alphabet-as-type'.\n►►►"
  (or (mon-is-letter maybe-alphanum)
      (mon-is-digit maybe-alphanum)))
;;
;;; :TEST-ME (mon-is-alphanum "8")
;;; :TEST-ME (mon-is-alphanum "A")
;;; :TEST-ME (mon-is-alphanum "a")
;;; :TEST-ME (mon-is-alphanum "?")
;;; :TEST-ME (mon-is-alphanum (char-to-string 88)) ;X
;;; :TEST-ME (mon-is-alphanum (char-to-string 10)) ;C-j LF newline
;;; :TEST-ME (mon-is-alphanum (char-to-string 32)) ;SPC
;;; :TEST-ME (mon-is-alphanum ?\C-m)
;;; :TEST-ME (mon-is-alphanum ?\13)
;;; :TEST-ME (mon-is-alphanum 13)
;;; :TEST-ME (mon-is-alphanum "13")
;;; :TEST-ME (mon-is-alphanum (let ((what ?\b)) (format "%s" what)))
;;; :TEST-ME (progn (insert ?\8) (mon-is-alphanum (char-before (point))))
;;; :TEST-ME (progn (insert 8) (mon-is-alphanum (char-before (point))))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-isdigit'
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:27:00-0400Z - by MON KEY>
(defun mon-is-digit-simp (maybe-digit-char)
 "Return non-nil if MAYBE-DIGIT-CHAR is a digit character.\n
Unlike `mon-is-digit' fails when other than \\? prefixed digit.
Wants char literals.\n:EXAMPLE\n\(mon-is-digit-simp ?0\)
\(mon-is-digit-simp \"0\"\)\n\(mon-is-digit \"0\"\)\n
:SEE-ALSO `mon-is-letter-simp', `mon-is-alphanum-simp',
`mon-string-index', `mon-string-position', `mon-alphabet-as-type'.\n►►►"
 (and (>= maybe-digit-char ?0) (<= maybe-digit-char ?9)))
;;
;;; :TEST-ME (mon-is-digit-simp ?0)

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-isalpha'
;;; :MODIFICATIONS <Timestamp: #{2010-03-30T16:35:04-04:00Z}#{10132} - by MON KEY>
;;; :RENAMED Arg `C' -> SIMP-LTR
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:26:57-0400Z - by MON KEY>
(defun mon-is-letter-simp (simp-ltr) 
 "Return non-nil when SIMP-LTR is an alphabetic character, and otherwise, nil.
Unlike `mon-is-letter' fails when other than \\? prefixed chars.
Wants char literals.\n
:EXAMPLE\n\(mon-is-letter-simp ?x\)\n
\(mon-is-letter-simp \"x\"\)
\(mon-is-letter \"x\"\)\n
:SEE-ALSO `mon-is-digit-simp',`mon-is-alphanum-simp'.
`mon-string-index', `mon-string-position', `mon-alphabet-as-type'.\n►►►"
 (or  (and (>= simp-ltr ?a) (<= simp-ltr ?z))
      (and (>= simp-ltr ?A) (<= simp-ltr ?Z))))
;;
;;; :TEST-ME (mon-is-letter-simp ?x)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-03-W32-1T15:18:01-0400Z - by MON KEY>
(defun mon-is-alphanum-simp (simp-alph)
  "Return t when SIMP-ALPH is either an alpha character or integer.\n
Unlike `mon-is-alphanum' fails when other than \\? prefixed chars or digits.
Wants char literals.\n
\(mon-is-alphanum-simp ?8\)             
\(mon-is-alphanum-simp ?A\)             
\(mon-is-alphanum-simp \"8\"\)            
\(mon-is-alphanum-simp \"A\"\)            
\(mon-is-alphanum-simp \(prin1-char 88\)\)
\(mon-is-alphanum \(char-to-string 88\)\)\n
:SEE-ALSO `mon-is-digit-simp' `mon-is-letter-simp',
`mon-string-index', `mon-string-position', `mon-alphabet-as-type'.\n►►►"
  (or (mon-is-letter-simp simp-alph)
      (mon-is-digit-simp simp-alph)))
;;
;;; :TEST-ME (mon-is-alphanum-simp ?8)
;;; :TEST-ME (mon-is-alphanum-simp ?A)
;;; :TEST-ME (mon-is-alphanum-simp "8") ;should fail
;;; :TEST-ME (mon-is-alphanum-simp "A");should fail
;;; :TEST-ME (mon-is-alphanum-simp (prin1-char 88)) ;should fail


;;; ==============================
;;; :COURTESY calendar/calendar.el :WAS `calendar-string-spread'
;;; The algorithm is based on equation (3.25) on page 85 of Concrete
;;; Mathematics by Ronald L. Graham, Donald E. Knuth, and Oren Patashnik,
;;; Addison-Wesley, Reading, MA, 1989.
;;; :CHANGESET 1995
;;; :CREATED <Timestamp: #{2010-07-26T18:06:51-04:00Z}#{10301} - by MON KEY>
(defun mon-string-spread (string-bag char length)
  "Concatenate list of STRINGS separated with copies of CHAR to fill LENGTH.\n
The effect is as if `mapconcat' but separating pieces are balanced if possible.\n
Each item of STRINGS is evaluated before concatenation so, in effect it can also
be an expression that evaluates to a string.\n
If LENGTH is too short, the STRINGS are concatenated and the result truncated.\n
:EXAMPLE\n\n\(let \(\(base-str \"string\"\)
      \(base-str-inc 0\)
      \(biggest-rand 0\)
      \(base-str-rndmz 
       #'\(lambda \(eg-str\)  
           \(let \(\(rand-pad \(random 16\)\)
                 \(rand-pad-char \(elt \(string-to-list \"#-._+\"\) \(random 4\)\)\)
                 rand-len\)
             \(setq eg-str
                   \(concat 
                    \(make-string  rand-pad rand-pad-char\)
                    \" \" eg-str \(number-to-string base-str-inc\) \" \"
                    \(make-string  rand-pad rand-pad-char\)\)\)
             \(setq rand-len \(length eg-str\)\)
             \(when \(> rand-len biggest-rand\)
               \(setq biggest-rand rand-len\)\)
             eg-str\)\)\)\)
  \(mon-string-spread
   `\(,\(funcall base-str-rndmz base-str\)
     \(identity ,\(funcall base-str-rndmz base-str\)\)
     \(apply 'string \(append ,\(funcall base-str-rndmz base-str\) nil\)\)\)
   \(elt \(number-sequence 33 47 1\) \(random 16\)\)
   \(* \(+ biggest-rand 2\) 3\)\)\)\n
:SEE-ALSO `mon-string-fill-to-col', `truncate-string-to-width',
`mon-line-strings-indent-to-col', `mon-line-indent-from-to-col',
`mon-string-set-char-at-idx' `mon-string-insert-string-at-idx',
`mon-string-index', `mon-string-upto-index', `mon-string-after-index',
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces',
`mon-string-ify-list', `mon-string-replace-char', `mon-string-sub-old->new',
`mon-string-repeat'.\n►►►"
  (let* ((str-bag (mapcar 'eval
                          (if (< (length string-bag) 2)
                              (append (list "") string-bag (list ""))
                            string-bag)))
         (mss-n (- length (length (apply 'concat str-bag))))
         (mss-m (1- (length str-bag)))
         (mss-s (car str-bag))
         (mss-strings (cdr str-bag))
         (mss-i 0))
    (dolist (1string mss-strings)
      (setq mss-s (concat mss-s
                          (make-string (max 0 (/ (+ mss-n mss-i) mss-m)) char)
                          1string)
            mss-i (1+ mss-i)))
    (substring mss-s 0 length)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((base-str "string")
;; |       (base-str-inc 0)
;; |       (biggest-rand 0)
;; |       (base-str-rndmz #'(lambda (eg-str)  
;; |                           (let ((rand-pad (random 16))
;; |                                 (rand-pad-char (elt (string-to-list "#-._+") (random 4)))
;; |                                 rand-len)
;; |                             (setq eg-str
;; |                                   (concat 
;; |                                    (make-string  rand-pad rand-pad-char)
;; |                                    " " eg-str (number-to-string base-str-inc) " "
;; |                                    (make-string  rand-pad rand-pad-char)))
;; |                             (setq rand-len (length eg-str))
;; |                             (when (> rand-len biggest-rand)
;; |                               (setq biggest-rand rand-len))
;; |                             eg-str
;; |                             ))))
;; |   (mon-string-spread
;; |    `(,(funcall base-str-rndmz base-str)
;; |      (identity ,(funcall base-str-rndmz base-str))
;; |      (apply 'string (append ,(funcall base-str-rndmz base-str) nil)))
;; |    (elt (number-sequence 33 47 1) (random 16))
;; |    (* (+ biggest-rand 2) 3)))
;; `----


;;; ==============================
;;; :COURTESY Pascal Bourguignon :HIS pjb-strings.el :WAS `string-justify-left'
;;; :CHANGESET 1738 <Timestamp: #{2010-05-17T08:57:22-04:00Z}#{10201} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-02-03T18:08:59-05:00Z}#{10053} - by MON KEY>
;;; :ADDED `save-match-data' for `split-string'
;;; :RENAMED LEFT-MARGIN arg -> lft-margin. `left-margin' is a global var.
;;; :MODIFICATIONS <Timestamp: #{2010-02-20T14:55:40-05:00Z}#{10076} - by MON KEY>
;;; Added optional arg NO-RMV-TRAIL-WSPC. Relocated save-match-data and
;;; conditional type error checks. Rewrote docstring
(defun mon-string-justify-left (justify-string &optional justify-width 
                                               lft-margin no-rmv-trail-wspc)
  "Return a left-justified string built from JUSTIFY-STRING.\n
When optional arg JUSTIFY-WIDTH is non-nil it is a width JUSTIFY-STRING to
counting from column 0.  Default JUSTIFY-WIDTH is `current-column' or 72.\n
When optional arg LFT-MARGIN it is a column to JUSTIFY-STRING beginning from.
Default is `left-margin' or 0.\n
The word separators are those of `split-string':
      [ \\f\\t\\n\\r\\v]+
This means that JUSTIFY-STRING is justified as one paragraph.\n
When NO-RMV-TRAIL-WSPC is non-nil do not remove trailing whitespace.
Default is to remove any trailing whiespace at end of lines.\n
:EXAMPLE\n
\(let \(\(jnk-arg '\(\(68 4\) \(18 8 t\)\)\) ;;<- With and without arg NO-RMV-TRAIL-WSPC
      jnk jnk1\)
  \(dotimes \(j 2 
              \(with-current-buffer 
                  \(get-buffer-create \"*MON-STRING-JUSTIFY-LEFT-EG*\"\)
                \(erase-buffer\)
                \(insert \";; :FUNCTION `mon-string-justify-left'\\n;;\\n\"
                        \(mapconcat 'identity \(nreverse jnk1\) \"\\n\"\)\)
                \(display-buffer \(current-buffer\) t\)\)\)
    \(dotimes \(i 8 
                \(progn
                  \(push \(format \(if \(= j 0\) 
                                    \";; :FIRST-TIME-W-ARGS %S\\n\" 
                                  \"\\n;; :SECOND-TIME-W-ARGS %S\\n\"\)
                                \(car jnk-arg\)\) 
                        jnk1\)
                  \(push \(apply 'mon-string-justify-left jnk \(pop jnk-arg\)\) jnk1\)
                  \(setq jnk nil\)\)\)
                \(dolist \(i '\(64 94\)\)
                  \(setq jnk 
                        \(concat \" \" 
                                \(make-string \(elt \(mon-nshuffle-vector [7 5 3 9]\) 3\) i\) 
                      jnk\)\)\)\)\)\)\n
:CALLED-BY `google-define-parse-buffer'\n
:SEE-ALSO `mon-string-fill-to-col', `truncate-string-to-width', `mon-string-spread'.\n►►►"
  (let* ((lft-margin (if (null lft-margin) (or left-margin 0) lft-margin)) 
         (msjl-width (if (null justify-width) (or fill-column 72) justify-width))
         (msjl-string (if (not (stringp justify-string)) 
                          (error  (concat ":FUNCTION `string-justify-left' "
                                          "-- arg JUSTIFY-STRING must be a string"))
                        justify-string))
         (msjl-col (if (not (and (integerp justify-width) (integerp lft-margin)))
                       (error (concat  ":FUNCTION `string-justify-left' "
                                       "-- arg LFT-MARGIN or JUSTIFY-WIDTH not an integer"))
                     lft-margin))
         (msjl-split (save-match-data (split-string msjl-string))) ;; :WAS splited
         (msjl-margin (make-string lft-margin 32)) ;; :WAS margin
         (msjl-jstfy (substring msjl-margin 0 msjl-col)) ;; :WAS justified
         (msjl-word)
         (msjl-word-len 0)
         (msjl-sep ""))
    (while msjl-split
      (setq msjl-word (car msjl-split))
      (setq msjl-split (cdr msjl-split))
      (setq msjl-word-len (length msjl-word))
      (if (> msjl-word-len 0)
          (if (>= (+ msjl-col (length msjl-word)) msjl-width)
              (progn
                (setq msjl-jstfy (concat msjl-jstfy "\n" msjl-margin msjl-word))
                (setq msjl-col (+ left-margin msjl-word-len)))
              (progn
                (setq msjl-jstfy (concat msjl-jstfy msjl-sep msjl-word))
                (setq msjl-col (+ msjl-col 1 msjl-word-len)))))
      (setq msjl-sep " "))
    (when (< msjl-col msjl-width) 
      (setq msjl-jstfy (concat msjl-jstfy (make-string (- msjl-width msjl-col) 32)))) ;;))
    (if no-rmv-trail-wspc
        msjl-jstfy
      (setq msjl-jstfy (replace-regexp-in-string "[[:space:]]+$" "" msjl-jstfy)))))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| (let ((jnk-arg '((68 4) (18 8 t))) ;;<- With and without no-rmv-trail-wspc arg.
;;|       jnk jnk1)
;;|   (dotimes (j 2 
;;|               (with-current-buffer 
;;|                   (get-buffer-create "*MON-STRING-JUSTIFY-LEFT-EG*")
;;|                 (erase-buffer)
;;|                 (insert ";; :FUNCTION `mon-string-justify-left'\n;;\n"
;;|                         (mapconcat 'identity (nreverse jnk1) "\n"))
;;|                 (display-buffer (current-buffer) t)))
;;|     (dotimes (i 8 
;;|                 (progn
;;|                   (push (format (if (= j 0) 
;;|                                     ";; :FIRST-TIME-W-ARGS %S\n" 
;;|                                   "\n;; :SECOND-TIME-W-ARGS %S\n")
;;|                                 (car jnk-arg)) 
;;|                         jnk1)
;;|                   (push (apply 'mon-string-justify-left jnk (pop jnk-arg)) jnk1)
;;|                   (setq jnk nil)))
;;|                 (dolist (i '(64 94))
;;|                   (setq jnk 
;;|                         (concat " " 
;;|                                 (make-string (elt (mon-nshuffle-vector [7 5 3 9]) 3) i) 
;;|                       jnk))))))
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-09T12:02:47-05:00Z}#{09503} - by MON>
(defun mon-string-fill-to-col (str to-col)
  "Return a string STR filled to column number TO-COL.\n
:EXAMPLE\n(mon-string-fill-to-col (mon-get-system-specs) 72)\n
:SEE-ALSO `mon-line-strings-indent-to-col', `mon-line-indent-from-to-col',
`mon-string-justify-left', `truncate-string-to-width'.\n►►►"
  (let (msftc-fstr)  
    (setq msftc-fstr 
          (with-temp-buffer
            (let ((fill-column to-col))
              (insert str)
              (fill-region (buffer-end 0) (buffer-end 1))
              (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))))
    msftc-fstr))
;;
;;; :TEST-ME (mon-string-fill-to-col (mon-get-system-specs) 72)

;;; ==============================
;;; :NOTE Alias these and don't forget to use them!
;;; :CREATED <Timestamp: Wednesday July 01, 2009 @ 06:32.08 PM - by MON KEY>
(defalias 'mon-string-combine-and-quote 'combine-and-quote-strings) 
;;
(defalias 'mon-string-split-and-unquote 'split-string-and-unquote)
;;
(defalias 'mon-replace-char-in-region 'subst-char-in-region)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-02T12:47:38-05:00Z}#{10092} - by MON KEY>
;;; `store-substring' <- mule-util.el
(defalias 'mon-string-set-char-at-idx 'store-substring
 "Set OBJ (string or character) at index IDX of STRING.\n
:EXAMPLE\n\n\(length \"bubba\"\)\n
\(mon-string-set-char-at-idx \"bubba\" 4 \"s\"\)\n
\(mon-string-set-char-at-idx \"bubba\" 4 \\=?s\)\n
\(mon-string-set-char-at-idx \"bubba\" 5 \\=?s\) ;out of bounds\n
:NOTE This function appears to retain existing text-properties.\n
:SEE-ALSO `aset', `aref', `vconcat', `string-to-list', `string-to-vector'.\n►►►")
;;
(defalias 'mon-string-insert-string-at-idx 'store-substring
  "Set OBJ (string or character) at index IDX of STRING.\n
:EXAMPLE\n\n\(length \"bubba\"\)\n
\(store-substring \"bubba\" 0 \"B\"\)
\(store-substring \"bubba\" 0 \" a\"\)
\(store-substring \"bubba\" 0 \"bubba\"\)
\(store-substring \"bubba\" 4 \"ba\"\)      ;out of bounds
\(store-substring \"bubba\" 3 \"bas\"\)     ;out of bounds
:NOTE This function appears to retain existing text-properties.\n
:SEE-ALSO `aset', `aref', `vconcat', `string-to-list', `string-to-vector'.\n►►►")

;;; ==============================
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
  (let ((start (point))
	(pos
	 (let ((inhibit-changing-match-data t))
           (save-excursion
             (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                  (point))))))
    (if (and greedy pos)
	(save-restriction
	  (narrow-to-region (point-min) start)
	  (while (and (> pos (point-min))
		      (save-excursion
			(goto-char pos)
			(backward-char 1)
			(looking-at-p (concat "\\(?:"  regexp "\\)\\'"))))
	    (setq pos (1- pos)))
	  (save-excursion
	    (goto-char pos)
	    (looking-at-p (concat "\\(?:"  regexp "\\)\\'")))))
    (not (null pos))))

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
  (let ((msrms-mtch (if (and (= (match-beginning 0) 1)(> (point) (point-min))) 
                        nil ;; Last search didn't move point was a dud don't proceed.
                      (car (read-from-string 
                            (substring (format "%S" (match-string (if match-subexp match-subexp 0))) 1))))))
    msrms-mtch))
;;
;;; :TEST-ME (search-forward-regexp "\\(\\(\\[\\)\\([0-9]\\{8,10\\}\\)\\(]\\)\\)" nil t)
;;;        [500006383]
;;; :TEST-ME (mon-string-read-match-string)
;;; :TEST-ME (mon-string-read-match-string 4)

;;; ==============================
;;; :NOTE I hope this isn't reinventing the wheel here... 
;;;       If not, WTF? why isn't this in Emacs already?
;;; :MODIFICATIONS <Timestamp: #{2009-10-14T11:06:04-04:00Z}#{09423} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-08-26T17:08:02-04:00Z}#{09353} - by MON KEY>
(defun mon-string-to-symbol (str &optional start end)
  "Return string STR as a symbol.\n
When optional args START and END are non-nil delimit the 
substring of str they default to 0 and (length string) respectively.
:EXAMPLE\n\(mon-string-to-symbol \"Bubba\")\n
\(mon-string-to-symbol \(mon-symbol->string 'bubba\)\)\n
\(mon-string-to-symbol \"mon-string-to-symbol\" 4 10\)\n
\(mon-string-to-symbol \"mon-string-to-symbol\" 4)\n
:SEE-ALSO `mon-symbol-to-string', `mon-string-to-sequence',
`mon-string-from-sequence', `mon-string-alpha-list',
`mon-string-index', `mon-string-has-suffix', `mon-alphabet-as-type',
`mon-string-replace-char'.\n►►►"
  (car (read-from-string str start end)))
;;
(defalias 'mon-string->symbol 'mon-string-to-symbol)
;;
;;; :TEST-ME (mon-string-to-symbol "bubba")
;;; :TEST-ME (mon-string-to-symbol "mon-string-to-symbol" 4 10)
;;; :TEST-ME (mon-string-to-symbol "mon-string-to-symbol" 4)
;;; :TEST-ME (mon-string-to-symbol (mon-symbol->string 'bubba))

;;; ==============================
;;; :NOTE Periodically MON is completely at a loss for how to accomplish this.
;;;       Lets make _damn_ sure it never happens again!!
;;; :CHANGESET 1911 <Timestamp: #{2010-06-22T15:13:06-04:00Z}#{10252} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-29T21:00:43-04:00Z}#{09403} - by MON KEY>
(defun mon-symbol-to-string (symbol-to-frob) 
  "Return SYMBOL as a string.\n
:EXAMPLE\n(mon-symbol-to-string 'bubba)\n
\(mon-symbol-to-string \(mon-string-to-symbol \"bubba\"\)\)\n
\(progn
  \(unintern \(intern-soft \"some-uninterned-symbol\"\)\)
  \(mon-symbol-to-string
   \(make-symbol \"some-uninterned-symbol\"\)\)\)\n
:SEE-ALSO `mon-string-to-symbol', `mon-string-to-sequence',
`mon-string-from-sequence', `mon-alphabet-as-type',
`mon-string-replace-char', `symbol-name'.\n►►►"
  (or (and (intern-soft symbol-to-frob)
           (symbol-name symbol-to-frob))
      ;; Which is more correct? (format "%s" symbol) Or:
      (format "%S" symbol-to-frob)))
;;
(defalias 'mon-symbol->string    'mon-symbol-to-string)
(defalias 'mon-string-from-symbol 'mon-symbol-to-string)
(defalias 'mon-string<-symbol    'mon-symbol-to-string)
;;
;;; :TEST-ME (mon-symbol->string 'bubba)
;;; :TEST-ME (mon-symbol->string (mon-string-to-symbol "bubba"))
;;; :TEST-ME (progn (unintern (intern-soft "some-uninterned-symbol"))
;;;            (mon-symbol-to-string (make-symbol "some-uninterned-symbol")))
   
;;; ==============================
;;; :CHANGESET 1899 <Timestamp: #{2010-06-22T14:58:14-04:00Z}#{10252} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday June 24, 2009 @ 11:50.11 AM - by MON KEY>
(defun mon-string-to-sequence (string-to-frob &rest more-strings)
  "Return string STRING-TO-FROB as a list of chars.\n
When rest arg MORE-STRINGS is non-nil each additional string is converted chars
and added to the list of returned chars.\n
Signal an error if MORE-STRINGS does not satisfy predicate `string-or-null-p'.\n
:EXAMPLE\n\n\(mon-string-to-sequence \"?\\C-lstring\"\)\n
\(apply 'string \(mon-string-to-sequence \"?\\C-lstring\"\)\)\n
\(mon-string-to-sequence \"str1\" \"str2\"\)\n
\(mon-string-to-sequence \"str1\" \"str2\" \"str3\"\)\n
\(mon-string-to-sequence \"str1\" \"str2\" nil \"str3\"\)\n
\(apply 'mon-string-to-sequence \"str1\" \"str2\" nil \"str3\" nil '\(\"more string\"\)\)\n
:SEE-ALSO `mon-string-from-sequence', `mon-string-index', `mon-string-position',
`mon-string-alpha-list', `mon-is-alphanum', `mon-is-digit', `mon-is-letter',
`mon-alphabet-as-type', `mon-string-replace-char', `string-to-list',
`string-to-vector'.\n►►►"
  ;; :NOTE `string-to-list' does this: (append string nil)
  (if more-strings
      (let ((msts-w/more 
             (progn
               (mapc #'(lambda (msts-chk-str) 
                         (unless (string-or-null-p msts-chk-str)
                           (error (concat 
                                   ":FUNCTION `mon-string-to-sequence' "
                                   "-- arg MORE-STRINGS must satisfy `string-or-null-p'"))))
                     more-strings)
               `(,string-to-frob ,@more-strings nil))))
        (apply 'append (car msts-w/more) (cdr msts-w/more)))
    (let (msts-to-seq)
      (mapc #'(lambda (mstsL) 
                (push mstsL msts-to-seq)) string-to-frob)
      (nreverse msts-to-seq))))
;;
;;; :TEST-ME (mon-string-to-sequence "?\C-lstring")
;;; :TEST-ME (apply 'string (mon-string-to-sequence "?\C-lstring"))
;;; :TEST-ME (mon-string-to-sequence "str1" "str2")
;;; :TEST-ME (mon-string-to-sequence "str1" "str2" "str3")
;;; :TEST-ME (mon-string-to-sequence "str1" "str2" nil "str3")
;;; :TEST-ME (apply 'mon-string-to-sequence "str1" "str2" nil "str3" nil '("more string"))
;;; :TEST-ME (mon-string-to-sequence "str1" "str2" "str3" '("str4")) ;; <- error

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-09T16:07:57-04:00Z}#{09415} - by MON>
;;; :CREATED <Timestamp: #{2009-09-30T13:31:42-04:00Z}#{09403} - by MON KEY>
(defun mon-string-from-sequence (stringify-seq &rest other-seqs)
  "Return STRINGIFY-SEQ - a sequence of character integers - as a string.\n
When OTHER-SEQS is non-nil these can be lists (quoted), vectors, or strings in
any combination these will be concatenated to return value also.\n
:EXAMPLE\n\n\(mon-string-from-sequence '(115 116 114 105 110 103))\n
\(mon-string-from-sequence\n '(115 116 114 105 110 103 48)
 '\(115 116 114 105 110 103 115 49\)\n '\(115 116 114 50\)\)\n
\(mon-string-from-sequence \(number-sequence 0 127\)\)\n
\(mon-string-from-sequence\n '\(98 117 98 98 97 115\)
 \"string0\"\n [32 98 117 98 98 97 115 32]
 '\(115 116 114 105 110 103 49 32\)\n [98 117 98 98 97 32]
 '\(103 111 116 32 110 105 108\)\)\n
:ALIASED-BY `mon-sequence-to-string', `mon-seq->string'\n
:SEE-ALSO `mon-string-index',`mon-string-position', `mon-string-alpha-list',
`mon-is-alphanum',`mon-is-digit',`mon-is-letter', `mon-alphabet-as-type',
`mon-string-replace-char'.\n►►►"
  (let ((g-str (lambda (x) (apply 'string x)))
        (chk-seqs (when (and other-seqs (sequencep other-seqs))
                    (mapcar #'(lambda (x) 
                                (cond ((vectorp x) (append x nil))
                                      ((stringp x) (mon-string-to-sequence x))
                                      ((listp  x) x)))
                            other-seqs)))
        (seq-seqs))
    (while chk-seqs 
      (push (funcall g-str (pop chk-seqs)) seq-seqs))
    (setq seq-seqs (nreverse seq-seqs))
    (push (funcall g-str 
                   (if (nlistp stringify-seq)
                       (cond ((vectorp stringify-seq) 
                              (append stringify-seq nil))
                             ((stringp stringify-seq) 
                              (mon-string-to-sequence stringify-seq)))
                     stringify-seq)) seq-seqs)
    (apply 'concat (car seq-seqs) (cdr seq-seqs))))
;;
(unless (fboundp (intern-soft "mon-sequence-to-string"))
  (defalias 'mon-sequence-to-string 'mon-string-from-sequence))
(unless (fboundp (intern-soft "mon-seq->string"))
  (defalias 'mon-seq->string 'mon-string-from-sequence))
;;
;;; :TEST-ME (mon-string-from-sequence '(98 117 98 98 97))
;;; :TEST-ME (mon-string-from-sequence (string-to-list "bubba"))
;;; :TEST-ME (mon-string-from-sequence '(98 117 98 98 97 115 97) (string-to-list "bubba"))
;;; :TEST-ME (mon-string-from-sequence '(98 117 98 98 97) [98 117 98 98 97 115 97])
;;; :TEST-ME (mon-string-from-sequence '(98 117 98 98 97) 
;;;          "string0" [98 117 98 98 97 115 97]  "string" [98 117 98 98 97 115 97])

;;; ==============================
;;; :CREATED <Timestamp: Thursday June 25, 2009 @ 11:17.43 AM - by MON KEY>
(defun mon-string-alpha-list (from-letter to-letter &optional as-symb)
  "Return alphabetized list of ASCII character strings FROM-LETTER TO-LETTER.\n
If either FROM-LETTER or TO-LETTER is upper-cased list returned 
will be in upper cased. When TO-LETTER comes before FROM-LETTER in a 
lexicographic sort the two args are swapped; this check is exclusive of case
check.\n\n:EXAMPLE\n
\(mon-string-alpha-list \"a\" \"f\"\)\n\(mon-string-alpha-list \"A\" \"F\"\)
\(mon-string-alpha-list \"l\" \"G\"\)\n\(mon-string-alpha-list \"g\" \"l\"\)\n
:NOTE Use this to get a list of symbols instead:\n
\(princ \(mon-string-alpha-list \"m\" \"r\"\)\)\n
:SEE-ALSO `mon-alphabet-as-type', `number-sequence', `mon-string-to-sequence', 
`mon-string-from-sequence',  `mon-is-alphanum', `mon-is-digit',
`mon-is-letter'.\n►►►"
  (let ((frm (string-to-char from-letter))
        (to (string-to-char to-letter))
        (swap)
        (rtn))
    (cond ((and (and (>= frm 65) (<= frm 90))
                (and (>= to 97) (<= to 127)))
           (setq to (- to 32)))
          ((and (and (>= to 65) (<= to 90))
                (and (>= frm 97) (<= frm 127)))
           (setq frm (- frm 32))))
    (when (< to frm)
      (setq swap frm)
      (setq frm to)
      (setq to swap))
    (split-string (mon-string-from-sequence (number-sequence frm to)) "" t)))
;;
;;; :TEST-ME (mon-string-alpha-list "a" "z")
;;; :TEST-ME (mon-string-alpha-list "A" "Z")
;;; :TEST-ME (mon-string-alpha-list "Z" "A")
;;; :TEST-ME (mon-string-alpha-list "z" "a")
;;; :TEST-ME (mon-string-alpha-list "Z" "a")
;;; :TEST-ME (mon-string-alpha-list "a" "Z")
;;; :TEST-ME (mon-string-alpha-list "z" "Z")
;;; :TEST-ME (mon-string-alpha-list "A" "a")

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-index'
(defun mon-string-index (string-to-idx needle &optional frompos)
  "Return the position in STRING of the beginning of first occurence of NEEDLE.\n
Return nil if needle is not found. NEEDLE is a char, number, or string.
When FROMPOS is non-nil begin search for needle from position. 
Default is to search from start of string.\n
:EXAMPLE\n\(mon-string-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-upto-index', `mon-string-after-index',
`mon-alphabet-as-type', `mon-string-position', `mon-string-has-suffix',
`mon-string-chop-spaces', `mon-string-replace-char'.\n►►►"
  (string-match 
   (regexp-quote 
    (cond ((or (characterp needle) (numberp needle)) (format "%c" needle))
          ((stringp needle) needle)
          (t (error (concat ":FUNCTION `mon-string-index' "
                            "-- arg NEEDLE expecting number or string")))))
   string-to-idx frompos))
;; 
;;; :TEST-ME (mon-string-index "string before ### string after" "###")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-01T15:16:26-04:00Z}#{09404} - by MON KEY>
(defun mon-string-upto-index (in-string upto-string)
  "Return substring of IN-STRING UPTO-STRING.\n
The arg UPTO-STRING is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-upto-index \"string before ### string after\" \"###\"\)\n  
:SEE-ALSO `mon-string-index', `mon-string-after-index'
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces',
`mon-string-replace-char'.\n►►►"
  (substring in-string 0 (mon-string-index in-string upto-string)))
;;
;;; :TEST-ME (mon-string-upto-index "string before ### string after" "###")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-01T15:16:29-04:00Z}#{09404} - by MON KEY>
(defun mon-string-after-index (in-str after-str)
  "Return substring of IN-STR AFTER-STR.\n
AFTER-STR is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-after-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-upto-index', `mon-string-position',
`mon-string-has-suffix', `mon-string-chop-spaces',
`mon-string-replace-char'.\n►►►"
  (substring in-str (+ (mon-string-index in-str after-str) (length after-str))))
;;
;;; :TEST-ME (mon-string-after-index "string before ### string after" "###")

;;; ==============================
;;; :NOTE This is a slow implementation.
;;; :CREATED <Timestamp: #{2010-03-23T17:38:29-04:00Z}#{10122} - by MON>
(defun mon-string-sort-descending (list-to-sort)
  "Destructively sort the list of strings by length in descending order.\n
:EXAMPLE\n\n\(let \(\(mk-str-l\)\)
  \(dotimes \(i 16 \(mon-string-sort-descending mk-str-l\)\)
    \(push \(make-string \(random 24\) 42\) mk-str-l\)\)\)\n
:SEE-ALSO .\n►►►"
  (let ((mssd-srt-l list-to-sort)
        (mssd-srt-pred #'(lambda (prd1 prd2) 
                      (let ((prd1-l (length prd1))
                            (prd2-l (length prd2)))
                        (> prd1-l prd2-l)))))
    (setq mssd-srt-l (sort mssd-srt-l mssd-srt-pred))))
;;
;;; :TEST-ME (let ((mk-str-l))
;;;               (dotimes (i 16 (mon-string-sort-descending mk-str-l))
;;;                 (push (make-string (random 24) 42) mk-str-l)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-position'
(defun mon-string-position (string substr &optional frompos)
  "Return the position in STRING of the first occurence of SUBSTR
searching FROMPOS, or from the start if FROMPOS is absent or nil. 
If the SUBSTR is not found, then return nil.\n
:EXAMPLE\n\(mon-string-position \"dogmeat\" \"meat\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-upto-index', `mon-string-after-index',
`mon-string-to-sequence', `mon-string-from-sequence',
`mon-string-replace-char'.\n►►►"
  (string-match (regexp-quote substr) string frompos))
;;
;;; :TEST-ME (mon-string-position "dogmeat" "meat")

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-has-suffix'
(defun mon-string-has-suffix (string suffix)
  "Return t when STRING has SUFFIX as a component.\n
:EXAMPLE\n\n\(mon-string-has-suffix \"dogmeat\" \"meat\"\)\n
\(mon-string-has-suffix \"butt\" \"chicken-butt\"\)\n
:SEE-ALSO `mon-string-position', `mon-string-index',
`mon-string-upto-index', `mon-string-after-index',
`mon-string-replace-char'.\n►►►"
  (cond ((or (not (stringp string)) (not (stringp suffix)))
         (error (concat ":FUNCTION `mon-string-has-suffix' "
                        "-- args STRING and SUFFIX must be strings")))
        ((< (length string) (length suffix)) nil)
        (t (string-equal 
            (substring string (- (length string) (length suffix)))
            suffix))))
;;
;;; :TEST-ME (mon-string-has-suffix "dogmeat" "meat")
;;; :TEST-ME (mon-string-has-suffix "butt" "chicken-butt")

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-advices.el :WAS `pjb-chop-spaces'
;;; :CREATED <Timestamp: #{2009-09-28T16:39:34-04:00Z}#{09401} - by MON>
(defun mon-string-chop-spaces (string)
  "Return a substring of `string' with spaces removed left and right.\n
:SEE-ALSO `mon-string-split-on-regexp', `mon-string-sub-old->new', 
`mon-string-chop-spaces', `mon-string-position', `mon-string-index',
`mon-string-upto-index', `mon-string-after-index', `mon-alphabet-as-type',
`mon-string-replace-char'.\n►►►"
  (let ((i 0)
        (l (1- (length string)))
        (space 32))
    (while (and (< 0 l) (eq (aref string l) space))
      (setq l (1- l)))
    (setq l (1+ l))
    (while (and (< i l) (eq (aref string i) space))
      (setq i (1+ i)))
    (substring string i l)))
;;
;;; :TEST-ME (mon-string-chop-spaces " some string no spaces ")

;;; ==============================
;;; :RENAMED `mon-stringify-list' -> `mon-string-ify-list'
(defun mon-string-ify-list (string-given)
  "Return a list of strings by breaking STRING-GIVEN at space boundaries.\n
:EXAMPLE\n\(mon-string-ify-list \"Make this sentence a list of strings\")\n
:SEE-ALSO `mon-stringify-list' ,`mon-insert-string-ify', 
`mon-string-ify-current-line', `mon-line-get-next', 
`mon-word-get-list-in-buffer', `mon-alphabet-as-type',
`mon-string-replace-char'.\n►►►"
  (let ((string string-given) list)
    (store-match-data nil)
    (while (string-match " *\\([^ ]+\\) *" string (match-end 0))
      (setq list
	    (cons (substring string (match-beginning 1) (match-end 1)) list)))
     (nreverse list)))
;;
;;; :TEST-ME (mon-stringify-list \"Make this sentence a list of strings\")

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `bibtex-split'
(defun mon-string-split-on-regexp (str regexp)
 "Return list of strings splitting STR at REGEXP.\n
This function is patterned after the awk split() function.\n
:SEE-ALSO `mon-string-chop-spaces', `mon-string-sub-old->new',
`mon-string-position', `mon-string-index', `mon-string-replace-char'.\n►►►"
 (if (or (null str) (null regexp)) ;then return nil if either s or regexp is nil
     nil
   (let ((p nil) 
         (k 0)) ; Else split the string and return a list of strings.
     (while (and (< k (length str)) (string-match regexp str k))
       (setq p (nconc p (list (substring str k (match-beginning 0)))))
       (setq k (match-end 0)))
     (setq p (nconc p (list (substring str k))))
     p)))
;;
;;; :TEST-ME (mon-string-split-on-regexp "split-on-split" "-split")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-30T15:07:18-05:00Z}#{10046} - by MON KEY>
(defun mon-string-replace-char (from-char target-string)
  "Return TARGET-STRING with all instances of FROM-CHAR removed.\n
Signal and error if either TARGET-STRING or FROM-CHAR evaluate non-nil for
`stringp' and `chararacterp' respectively.\n
:EXAMPLE\n
\(mon-string-replace-char 0 \"\x0I'm\x0 an\x0 ugly\x0 string.\")\n
:SEE-ALSO `subst-char-in-string', `mon-string-from-hex-list', 
`mon-string-to-hex-string', `mon-help-char-representation'.\n►►►"
  (if (and (characterp from-char) (stringp target-string))
      (let ((ts (append target-string nil)))
        (setq ts (apply 'string (remq from-char ts))))
    (error 
     (concat ":FUNCTION `mon-string-replace-char' "
             "-- arg FROM-CHAR not a valid char or TARGET-STRING not a string"))))
;;
(defalias 'mon-replace-char-in-string 'mon-string-replace-char)
;;
(defalias 'mon-remove-char-in-string 'mon-string-replace-char)
;;
;;; :TEST-ME (mon-remove-char-in-string 0 "\x0I'm\x0 an\x0 ugly\x0 string.")

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `melvyl-sub'
;;; :CREATED <Timestamp: 2009-08-03-W32-1T10:26:52-0400Z - by MON KEY>
(defun mon-string-sub-old->new (old new str)
 "Return copy of STR with first occurrence of OLD substituted by NEW.\n
:SEE-ALSO `mon-string-split-on-regexp', `mon-string-chop-spaces',
`mon-string-position', `mon-string-index', `mon-string-replace-char'.\n►►►"
 (let ((k 0))
   ;; (debug)
   (while (and (< k (1+ (- (length str) (length old))))
               (not (string-equal old (substring str k (+ k (length old))))))
     (setq k (1+ k)))
   (if (and (< k (1+ (- (length str) (length old))))
            (string-equal old (substring str k (+ k (length old)))))
       (concat (substring str 0 k) new
               (substring str (+ k (length old)) (length str)))
     str)))
;;
;;; :TEST-ME (mon-string-sub-old->new"old" "new" "old old new")

;;; ==============================
;;; :COURTESY Jared D. :WAS `string-repeat'
;;; :SEE (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :MODIFICATIONS <Timestamp: #{2009-08-19T20:13:32-04:00Z}#{09344} - by MON KEY>
(defun mon-string-repeat (str n &optional insrtp w/spc intrp)
  "Return the string STR N times.\n
When optional INSRTP is non-nil or called-interactively insert STR at point.
Does not move point. 
When W/SPC is non-nil return string with whitespace interspersed.\n
:EXAMPLE\n\(mon-string-repeat \"bubba\" 3 nil t\)\n
:SEE-ALSO `mon-insert-string-ify', `mon-string-incr', 
`mon-insert-string-n-fancy-times', `mon-insert-string-n-times',
`mon-string-replace-char'.\n►►►"
  (interactive 
   (list 
    (replace-regexp-in-string "[\\[:space:]]+$" ""  ;; :WAS "[[:space:]]+$" "" 
                              (read-string  (concat ":FUNCTION `mon-string-repeat' "
                                                    "-- string to repeat: ")))
    (read-number (concat ":FUNCTION `mon-string-repeat' "
                         "-- times to repeat: "))
    nil
    (yes-or-no-p (concat ":FUNCTION `mon-string-repeat' "
                 "-- with whitespace: "))))
  (let ((retval ""))
    (dotimes (i n)
      (if w/spc 
          (setq retval (concat retval str " "))
        (setq retval (concat retval str))))
    (if (or insrtp intrp)
        (save-excursion (insert retval)))
    retval))
;;
;;; :TEST-ME (mon-string-repeat "bubba" 3)
;;; :TEST-ME (mon-string-repeat "bubba" 3 t)
;;; :TEST-ME (mon-string-repeat "bubba" 3 t t)
;;; :TEST-ME (mon-string-repeat "bubba" 3 nil t)
;;; :TEST-ME (call-interactively 'mon-string-repeat) 

;;; ==============================
;;; :COURTESY Drew Adams :HIS strings.el
;;; :RENAMED `mon-split-string-line' -> `mon-string-split-line'
;;; :MODIFICATIONS <Timestamp: #{2009-09-23T18:49:22-04:00Z}#{09393} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2010-01-11T23:34:36-05:00Z}#{10021} - by MON>
(defun mon-string-split-line (&optional buffer insrtp intrp)
  "Return current line of text in BUFFER as a string.
When INSRTP is non-nil or called interactively insert return string at point. 
Does not move-point.\n
:SEE-ALSO `mon-line-strings-qt-region', `mon-line-strings-to-list',
`mon-stringify-list', `mon-insert-string-ify', `mon-line-drop-in-words',
`mon-string-ify-current-line',`mon-word-get-list-in-buffer',
`mon-string-replace-char'.\n►►►"
(interactive "i\ni\np")
(let ((splt-str-s)
      (splt-str-e)
      (splt-str))  
  ;; :WAS (setq buffer (or buffer (current-buffer)))
  ;;       (save-excursion (set-buffer buffer)
  (save-excursion    
    (with-current-buffer (if buffer (get-buffer buffer) (current-buffer))
      (setq splt-str
            (buffer-substring-no-properties 
             (progn (end-of-line 1) (setq splt-str-e (point)))
             (progn (beginning-of-line 1) (setq splt-str-s (point)))))))
  (if (or insrtp intrp)
      (if (not buffer-read-only)      
          (save-excursion (prin1 splt-str (current-buffer)))
          (prin1 splt-str))
    splt-str)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-15T18:19:18-04:00Z}#{09424} - by MON KEY>
;;; Adjust for compile Warning: `mapcar' called for effect; use `mapc' - so using it.
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 03:08.46 PM - by MON KEY>
(defun mon-string-ify-current-line (&optional intrp split-on delim)
  "Return line at point as a list of strings.\n
When non-nil split-on is a string which should be split on.
When non-nil delim is a delimter to be concatenated to _front_ of each string. 
Called interacively kills current line replacing with string per-word
unless in an unreadable buffer where just retruns.
Neither SPLIT-ON nor DELIM have an effect when Invoked interactively.\n
:EXAMPLE
\(mon-string-ify-current-line\) split me to a list of strings
\(mon-string-ify-current-line nil \"s\" \"S\"\) split me to a list of strings
\(mon-string-ify-current-line nil nil \"|\"\) split me to a list of strings\n\n
:SEE-ALSO `mon-line-strings-qt-region', `mon-line-strings-to-list',
`mon-string-ify-list', `mon-insert-string-ify', `mon-string-split-line',
`mon-line-drop-in-words', `mon-word-get-list-in-buffer',
`mon-string-replace-char'.\n►►►"
  (interactive "p")
  (let* ((sp (if split-on " "))
	 (dlm (cond (delim delim)
		    ((not delim)
		     (if intrp  "\""  ""))))
	 (ss (split-string (mon-string-split-line) split-on t)))
    (cond ((and intrp (not buffer-read-only))
	   (save-excursion
	     (progn 
	       (kill-line)
               ;; :WAS (mapcar '(lambda (x) (princ (format "%s%s%s " dlm x dlm) (current-buffer))) ss)
               (mapc #'(lambda (x) (princ (format "%s%s%s " dlm x dlm) (current-buffer))) ss)
	       (delete-char -1))) ss)
	  ((and intrp buffer-read-only)
	   (progn
	     (kill-new (format "%S" ss))
	     (message (concat ":FUNCTION `mon-string-ify-current-line' " 
                              "-- buffer is read only, line split is on kill ring\n %S")
                      ss)))
	  ((and (not intrp) dlm)
	   (let (ss2)
             (setq ss2 nil)
	     (mapc #'(lambda (x) (setq ss2 (cons (format "%s%s" dlm x) ss2))) ss)
       	     ;; :WAS (mapcar '(lambda (x) (setq ss2 (cons (format "%s%s" dlm x) ss2)))ss)
	     ss2))
	  (t ss))))
;;
;;; :TEST-ME (mon-string-ify-current-line) ;split me to a list of strings
;;; :TEST-ME (mon-string-ify-current-line nil \"s\" \"S\"\) split me to a list of strings
;;; :TEST-ME (mon-string-ify-current-line nil nil \"|\"\) split me to a list of strings

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-09T16:14:41-05:00Z}#{09503} - by MON>
(defun mon-line-strings (start end &optional insrtp intrp)
  "Return lines of region from START to END as strings.\n
Each line is replaced with a quoted string.
When called-interactively or INSRTP is non-nil replace region with strings and 
move point to START.\n
:EXAMPLE\n(mon-help-overlay-for-example 'mon-line-strings 5 'line)\n
►\nHassan-i Sabbah\nTristan and Iseult\nBroder Rusche
Pier Gerlofs Donia\nBöŏvarr Bjarki\n◄\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-strings-region',
`mon-line-strings-region-delimited',
`mon-line-strings-qt-region',`mon-line-drop-in-words',
`mon-string-ify-list',`mon-string-ify-current-line'.\n►►►"
  (interactive "r\ni\np")
  (let ((st-beg (make-marker))
        (st-end (make-marker))
        (ln-str))
    (set-marker st-beg start)
    (set-marker st-end end)
    (setq ln-str (buffer-substring-no-properties st-beg st-end))
    (setq ln-str
          (with-temp-buffer 
            (insert ln-str)
            (goto-char (buffer-end 0))
            (while (search-forward-regexp "^\\(.*\\)$" nil t)
              (replace-match "\"\\1\""))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
      (if (or insrtp intrp)
          (progn
            (delete-region st-beg st-end)
            (insert ln-str)
            (goto-char st-beg))
          ln-str)))
;;
;;; :TEST-ME
;;; (let ((legs)
;;;       (legb (1+ (search-forward-regexp "►")))
;;;       (lege (- (search-forward-regexp "◄") 2)))
;;;   (setq legs (mon-line-strings legb lege)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |►
;; |Hassan-i Sabbah
;; |Tristan and Iseult
;; |Broder Rusche
;; |Pier Gerlofs Donia
;; |Böŏvarr Bjarki
;; |◄
;; `----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-08T12:36:48-05:00Z}#{09502} - by MON>
(defun mon-line-strings-region (start end &optional insrtp intrp)
  "Return each line of region as a string followed by a `\n'.
When called-interactively or INSRTP is non-nil insert strings at point.
Does not move point.\n
Use with concat for formated indentation in source.\n
:EXAMPLE\n\(mon-help-overlay-for-example 'mon-line-strings-region 4 'line)\n
►\nI-will-be-a-string\nI too will be a string.\nMe as well.
More stringification here\n◄\n
:SEE-ALSO `mon-line-strings', `mon-line-strings-to-list',
`mon-line-strings-region-delimited', `mon-line-strings-bq-qt-sym-bol',
`mon-string-ify-list', `mon-string-ify-current-line', `mon-string-split-line',
`mon-line-drop-in-words', `mon-cln-up-colon'.\n►►►"
  (interactive "r\ni\np")
  (let ((qt-lns)
        (ln-r-st (make-marker))
        (ln-r-end (make-marker)))
    (set-marker ln-r-st start)
    (set-marker ln-r-end end)
    (setq qt-lns (buffer-substring-no-properties ln-r-st ln-r-end))
    (setq qt-lns 
          (with-temp-buffer 
            (insert qt-lns)
            (goto-char (buffer-end 0))
            (while (search-forward-regexp "^\\(.*\\)$" nil t)
              (replace-match "\"\\1\\\\n\"" t)) ;; Do not alter case.
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if (or insrtp intrp)
        (save-excursion 
          (delete-region ln-r-st ln-r-end)
          (goto-char ln-r-st)
          (insert qt-lns))
        qt-lns)))
;;
;;; :TEST-ME (mon-line-strings-region
;;;           (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |►
;; |I-will-be-a-string
;; |I too will be a string.
;; |Me as well.
;; |More stringification here
;; |◄
;; `----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-23T16:16:47-04:00Z}#{09435} - by MON KEY>
(defun mon-line-strings-qt-region (start end &optional insrtp intrp)
  "Return symbols at each BOL in region wrapped in double-quotes `\"'.
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.
Line's symbol should be without trailing whitespace.
If whitespace is present at EOL it is destructively removed.
When following characters are at BOL no replacement is peformed on symbol:
  ;( ) ` ' \" Likewise, do not replace if \" or ' follows symbol.\n
:NOTE will not quote symbols containing whitespace.\n
:EXAMPLE\n\n\(mon-help-overlay-for-example 'mon-line-strings-qt-region 4 'line\)
\(princ (mon-line-strings-qt-region
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"◄\"\) 2\)\)\)
\n►\nI-will-be-a-string\n\"I-am-almost-a-string\nI-am-a-half-string\"
I-am-not-a-string'\n◄\n 
:SEE-ALSO `mon-line-strings-bq-qt-sym-bol', `mon-line-strings-pipe-bol',
`mon-line-strings-region-delimited', `mon-cln-up-colon',
`mon-line-strings',`mon-line-strings-indent-to-col', `mon-line-strings-to-list',
`mon-line-strings-region', `mon-string-ify-list', `mon-string-ify-current-line',
`mon-string-split-line', `mon-line-drop-in-words'.\n►►►"
  (interactive "r\ni\np")
  (let (rtn-v)
    (setq rtn-v (buffer-substring-no-properties start end))
    (setq rtn-v
          (with-temp-buffer 
            (insert rtn-v)
            (delete-trailing-whitespace)
            (goto-char (buffer-end 0))
            (while (not (= (line-end-position) (buffer-end 1)))
              (beginning-of-line)            
              (when ;; Use `looking-at-p' here instead?
                  (looking-at "^\\([^;`'()\"\\[:blank:]]\\)\\([\\[:graph:]]+[^\"']\\)$")
                (replace-match (concat "\"" (match-string-no-properties 0) "\"")))
              (forward-line 1)
              (when (and (= (line-end-position) (buffer-end 1))
                         (looking-at ;; use `looking-at-p' here instead?
                          "^\\([^;`'()\\[:blank:]]\\)\\([\\[:graph:]]+\\([^\"']\\)\\)$"))
                (replace-match (concat "\"" (match-string-no-properties 0) "\""))))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if (or insrtp intrp)
        (save-excursion (delete-region start end) (insert rtn-v))
        rtn-v)))
;;
;;; :TEST-ME
;;; (princ (mon-line-strings-qt-region
;;;  (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2)))
;; ,---- :UNCOMMENT-TO-TEST first case should pass, the rest fail
;; |►
;; |I-will-be-a-string
;; |"I-am-almost-a-string
;; |I-am-a-half-string"
;; |I-am-not-a-string'
;; |►
;; `----

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-10-23T18:04:19-04:00Z}#{09435} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-10-16T14:30:28-04:00Z}#{09425} - by MON KEY>
;;; Updated to find with trailing symbols or (and EOL (not WSP)).
;;; :CREATED <Timestamp: #{2009-10-06T14:45:00-04:00Z}#{09412} - by MON KEY>
(defun mon-line-strings-bq-qt-sym-bol (start end &optional insrtp intrp)
  "Return symbols at BOL in region wrapped in backquote and quote.
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.
When following characters are at BOL no replacement is peformed on symbol:
^ ; , . ( ) < > ` ' # ► \| Likewise, do not replace if ' follows symbol.\n
:EXAMPLE\n\n(mon-line-strings-bq-qt-sym-bol
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"◄\"\) 2\)\)\n
►\ncall-next-method &rest replacement-args
call-next-method &rest replacement-args
`call-next-method &rest replacement-args
call-next-method' &rest replacement-args\n◄\n
\(mon-line-strings-bq-qt-sym-bol-TEST\)\n
:SEE-ALSO `mon-line-strings-bq-qt-sym-bol-TEST', `mon-line-strings',
`mon-line-strings-qt-region', `mon-cln-up-colon',
`mon-line-strings-region-delimited', `mon-line-strings-pipe-bol',
`mon-line-strings-indent-to-col', `mon-line-strings-to-list',
`mon-line-strings-region', `mon-string-ify-list', `mon-string-ify-current-line',
`mon-string-split-line', `mon-line-drop-in-words'.\n►►►"
  (interactive "r\ni\np")
  (let (rtn-v)
    (setq rtn-v (buffer-substring-no-properties start end))
    (setq rtn-v
          (with-temp-buffer 
            (insert rtn-v)
            (goto-char (buffer-end 0))
            (while (not (= (line-end-position) (buffer-end 1)))
              (beginning-of-line)            
              (save-match-data
                (when (looking-at "^\\([^;,.()<>`'#► ]\\)[\\[:graph:]]+[^']$")
                  (replace-match (concat "`" (match-string-no-properties 0) "'"))))
              (forward-line 1)
              (save-match-data
                (when (and (= (line-end-position) (buffer-end 1))       
                           (looking-at "^\\([^;,.()<>`'#► ]\\)[\\[:graph:]]+[^' ]$"))
                  (replace-match  (concat "`" (match-string-no-properties 0) "'"))))
              (save-match-data
                (when (and (= (line-end-position) (buffer-end 1))
                           (looking-at "^\\([^;,.()<>`'#► ]\\)[\\[:graph:]]+[^' ]$"))
                  (replace-match  (concat "`" (match-string-no-properties 0) "'"))))
              )
            (goto-char (buffer-end 0))
            ;; :WAS            
            ;; (search-forward-regexp 
            ;;  "^\\([^;,.()<>`'#►\|\\[:blank:]][\\[:graph:]]+[^'\\[:blank:]]+\\)\\( \\)\\(.*\\)$" nil t) 
            ;;  (replace-match "`\\1'\\2\\3"))
            ;; :WAS "^\\([^;,.()<>`'#►\|\\[:blank:]]\\)\\([\\[:graph:]]+[^']\\)\\([^^']\\)\\([ ]\\{1,2\\}\\)\\(.*\\)$" nil t)
            ;;       (replace-match "`\\1\\2' \\5"))
            (while 
                (search-forward-regexp 
                 "^\\([^;,.()<>`'#►\|\\[:blank:]]\\)\\([\\[:graph:]]+[^']\\)\\([^^ ']\\)\\([ ]\\{1,2\\}\\)\\(.*\\)$" nil t)
              ;; ^^^^1^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^2^^^^^^^^^^^^^^^^^^^^^^^3^^^^^^^^^^4^^^^^^^^^^^^^^^^^5^^^^^
              (replace-match "`\\1\\2\\3' \\5"))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if (or insrtp intrp)
        (save-excursion (delete-region start end) (insert rtn-v))
        rtn-v)))

;;; ==============================
;;; :CHANGESET 1789
;;; :CREATED <Timestamp: #{2010-05-29T20:29:13-04:00Z}#{10216} - by MON KEY>
(defun mon-line-strings-bq-qt-sym-bol-TEST ()
  "Test function for `mon-line-strings-bq-qt-sym-bol'.\n
Return restults to buffer named \"*mon-line-strings-bq-qt-sym-bol-TEST*\".\n
:SEE-ALSO .\n►►►"
  (with-current-buffer 
      (get-buffer-create "*mon-line-strings-bq-qt-sym-bol-TEST*")
    (let ((mlsbqsb-str  (mapconcat 'identity
                                   '("►" "call-next-method"  
                                     "call-next-method &rest replacement-args"
                                     "call-next-method  &rest replacement-args"
                                     "`call-next-method  &rest replacement-args"
                                     "call-next-method' &rest replacement-args"
                                     " call-next-method'  &rest replacement-args"
                                     "`call-next-method" "call-next-method'" 
                                     " call-next-method" " call-next-method'"
                                     " call-next-method" "◄") "\n"))
          mlsbqsb-mrk)
      (save-excursion (insert mlsbqsb-str))
      (save-excursion 
        (mon-line-strings-bq-qt-sym-bol 
         (1+ (search-forward-regexp "►"))(search-forward-regexp "◄")  t))
      (save-excursion 
        (insert ":TESTING `mon-line-strings-bq-qt-sym-bol'.\n"
                "Original lines commented below.\n"
                "With uncommented lines only first three should have succeeded.\n"
                mlsbqsb-str)
        (setq mlsbqsb-mrk (point)))
      (emacs-lisp-mode)
      (comment-region (point) mlsbqsb-mrk))
    (display-buffer (current-buffer) t)))
;; 
;;; :TEST-ME `mon-line-strings-bq-qt-sym-bol-TEST'

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-02T11:58:40-05:00Z}#{09493} - by MON>
(defun mon-line-strings-pipe-bol (start end &optional insrtp intrp)
  "Return BOL in region replaced with `| '.
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.\n
:EXAMPLE\n\(save-excursion\n \(mon-line-strings-pipe-bol
   \(1+ \(search-forward-regexp \"►\"\)\)
   \(- \(search-forward-regexp \"◄\"\) 2\)\)\)\n
►\n Craig Balding\n Emmanuel Bouillon\n Bernardo Damele Assumpcao Guimarase
 Jean-Paul Fizaine\n Rob Havelt\n Chris Wysopal\n◄\n 
:SEE-ALSO `mon-line-strings-pipe-to-col', `mon-line-strings-bq-qt-sym-bol', 
`mon-line-strings', `mon-line-strings-qt-region',  `mon-line-strings-region', 
`mon-line-strings-indent-to-col', `mon-line-strings-to-list'.\n►►►"
  (interactive "r\ni\np")
  (let ((replc)
        (r-beg (make-marker))
        (r-end (make-marker)))
    (set-marker r-beg start)
    (set-marker r-end end)
    (setq replc (buffer-substring-no-properties r-beg r-end))
    (setq replc
          (with-temp-buffer 
            (insert replc)
            (goto-char (buffer-end 0))
            (while (search-forward-regexp "^\\(.*\\)$" nil t)
              (replace-match "| \\1"))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
      (if (or insrtp intrp)
          (save-excursion
            (delete-region r-beg r-end)
            (insert replc))
          replc)))
;;
;;; :TEST-ME (save-excursion (mon-line-strings-pipe-bol
;;;          (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2)))
;;
;; ,---- :UNCOMMENT-TO-TEST
;; |►
;; | Craig Balding
;; | Emmanuel Bouillon
;; | Bernardo Damele Assumpcao Guimarase
;; | Jean-Paul Fizaine
;; | Rob Havelt
;; | Chris Wysopal
;; |◄
;; `----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-02T11:58:40-05:00Z}#{09493} - by MON>
(defun mon-line-strings-indent-to-col (start end col &optional insrtp intrp)
  "Return region lines indented to column number COL.\n
When called-interactively with non-nil prefix arg COL return region indented to
column number. When prefix arg is nil prompt for COL.\n
When INSRTP is non-nil or called-interactively replace active region and
move point to region-beginning.\n
:NOTE following example used in conjunction with `mon-line-strings-pipe-bol'.\n
:EXAMPLE\n\(let \(\(rs \(1+ \(search-forward-regexp \"►\"\)\)\)
      \(re \(- \(search-forward-regexp \"◄\"\) 2\)\)\n      \(tmp\)\)
  \(setq tmp \(buffer-substring-no-properties rs re\)\)
  \(setq tmp \(with-temp-buffer \n              \(insert tmp\)
              \(mon-line-strings-pipe-bol \(buffer-end 0\) \(buffer-end 1\) t\)
              \(mon-line-strings-indent-to-col \(buffer-end 0\) \(buffer-end 1\) 7 t\)
              \(buffer-substring-no-properties \(buffer-end 0\) \(buffer-end 1\)\)\)\)
  tmp\)\n\n►\nCraig Balding\nEmmanuel Bouillon\nBernardo Damele Assumpcao Guimaraes
Jean-Paul Fizaine\nRob Havelt\nChris Wysopal\n◄\n
:SEE-ALSO `mon-line-indent-from-to-col', `mon-line-strings-pipe-to-col',
`mon-comment-divider->col', `mon-comment-lisp-to-col',
`mon-line-strings', `mon-string-fill-to-col',
`mon-line-strings-qt-region', `mon-line-strings-region', 
`mon-line-strings-bq-qt-sym-bol',`mon-line-strings-to-list'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((coln (if (and intrp (not col))
                  (read-number "Indent to column number: ")
                  col))
        (lreplc)
        (lr-beg (make-marker))
        (lr-end (make-marker)))
    (set-marker lr-beg start)
    (set-marker lr-end end)
    (setq lreplc (buffer-substring-no-properties lr-beg lr-end))
    (setq lreplc
          (with-temp-buffer 
            (insert lreplc)
            (goto-char (buffer-end 0))
            (while (not (mon-line-eol-is-eob))
              (indent-line-to coln)
              (line-move 1 t)
              (when (mon-line-eol-is-eob)
                (indent-line-to coln)))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if (or insrtp intrp)
        (save-excursion
          (delete-region lr-beg lr-end)
          (insert lreplc))
        lreplc)))
;;
;;; :TEST-ME 
;;; (save-excursion 
;;;   (let ((rs (1+ (search-forward-regexp "►")))
;;;         (re (- (search-forward-regexp "◄") 2)))
;;;     (goto-char rs)
;;;     (mon-line-strings-pipe-bol rs re t)
;;;     (goto-char rs)
;;;     (mon-line-strings-indent-to-col rs re 7 t)))
;;
;; ,---- :UNCOMMENT-TO-TEST
;; ,►
;; |Craig Balding
;; |Emmanuel Bouillon
;; |Bernardo Damele Assumpcao Guimaraes
;; |Jean-Paul Fizaine
;; |Rob Havelt
;; |Chris Wysopal
;; |◄
;; `----

;;; ==============================
;;; :NOTE Does not work for one line regions.
;;; :CREATED <Timestamp: #{2009-12-08T18:12:32-05:00Z}#{09502} - by MON>
(defun mon-line-indent-from-to-col (from-col to-col start end &optional intrp)
  "Indent to column starting FROM-COL identing TO-COL in region START to END.\n
When called-interactively prompt for column numer of FROM-COL and TO-COL.\n
:NOTE Does not work for one line regions.\n
:SEE :FILE align.el for alternative approaches.\n
:SEE-ALSO `mon-line-strings-indent-to-col', `mon-line-strings-pipe-to-col',
`mon-string-fill-to-col',`mon-comment-divider->col',
`mon-comment-lisp-to-col'.\n►►►"
  (interactive "i\ni\ni\ni\np")
  (let ((frm-c (cond (from-col from-col)
                     ((or intrp t)
                      (read-number "col to start from: "
                                   (car (posn-actual-col-row (posn-at-point)))))))
        (to-c (cond (to-col to-col)
                    ((or intrp t)
                     (read-number "col to indent to: " 
                                  (car (posn-actual-col-row (posn-at-point)))))))
        (c-start (cond (start start)
                       ((region-active-p) (region-beginning))))
        (c-end (cond (end end)
                     ((region-active-p) (region-end))))
        (s-mark  (make-marker))
        (e-mark  (make-marker))
        (indent-wrk t))
    (set-marker s-mark c-start)
    (set-marker e-mark c-end)
    (progn 
      (goto-char c-start)
      ;; (line-move -1) ;; :TODO Add logic for the single lined region.
      (beginning-of-line))
    (while indent-wrk
      (move-to-column frm-c)
      (indent-to-column to-c)
      (cond ((< (line-number-at-pos (line-end-position 2)) (line-number-at-pos e-mark))
             (prog1  
                 (line-move 1 t) 
               (beginning-of-line)))
            ((>= (line-number-at-pos (line-end-position 2)) (line-number-at-pos e-mark))
             (progn  
               (line-move 1 t)
               (beginning-of-line) 
               (move-to-column frm-c)
               (indent-to-column to-c)
               (setq indent-wrk nil)))))))
;;
(defalias 'mon-indent-lines-from-to-col 'mon-line-indent-from-to-col)
;;
;;; :TEST-ME (let ((st-pnt (make-marker))
;;                 (nd-pnt (make-marker))
;;                 (fndr  #'(lambda (y) (search-forward-regexp y nil t))))
;;             (set-marker st-pnt (funcall fndr "^►")) ;st-pnt)
;;             (set-marker nd-pnt (funcall fndr "◄$")) ; nd-pnt)
;;             (goto-char st-pnt)
;;             (mon-line-indent-from-to-col 24 32 st-pnt nd-pnt)
;;             (goto-char st-pnt)
;;             (mon-line-indent-from-to-col 46 58 st-pnt nd-pnt))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |►emacsen.auto_apart     001           001
;; |emacsen.rug_compat_42   00            00
;; |emacsen.rug_compt_adorn 00            00       
;; |emacsen.cache_empire    080           080      
;; |emacsen.hashdelimiter   no-hash       no-hash
;; |emacsen.rookie_romain   no value      no value◄
;; `----

;;; ==============================
;;; :COURTESY Pascal .J Bourguignon :WAS `dolines'
;;; :CHANGESET 1773 <Timestamp: #{2010-05-26T16:14:14-04:00Z}#{10213} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-12-28T15:57:08-05:00Z}#{09531} - by MON KEY>
(defmacro* mon-line-dolines (start-end &body body)
  "Executes the body with START-END `destructoring-bind'ed to the start and
end of each line of the current-buffer in turn.\n
:EXAMPLE\n\n(mon-line-dolines-TEST)\n
:SEE (URL `http://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00614.html')\n
:SEE-ALSO `mon-line-dolines-TEST', `mon-line-dolines-setup-TEST'.\n►►►"
  (declare (indent 2) (debug t))
  (let ((mld-vline  (make-symbol "mld-vline"))
        (mld-sm     (make-symbol "mld-sm"))
        (mld-em     (make-symbol "mld-em")))
    (destructuring-bind (start-var end-var) start-end
      `(let((,mld-sm (make-marker))
            (,mld-em (make-marker)))
         (unwind-protect
             ;; :WAS (progn
             (save-restriction
               (when (use-region-p) ;; (region-active-p) 
                 (narrow-to-region (region-beginning) (region-end)))
                 (goto-char (buffer-end 0))
                 (while (< (point) (buffer-end 1))
                   (let ((,mld-vline (point)))
                     (set-marker ,mld-sm (point))
                     (set-marker ,mld-em (goto-char (line-end-position)))
                     (let ((,start-var  (marker-position ,mld-sm))
                           (,end-var    (marker-position ,mld-em)))
                       ,@body)
                     (goto-char ,mld-vline)
                     (forward-line 1))))
               (set-marker ,mld-sm nil)
               (set-marker ,mld-em nil))
         nil))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-26T15:07:47-04:00Z}#{10213} - by MON KEY>
(defun mon-line-dolines-setup-TEST ()
  "Helper for `mon-line-dolines' macro's test function `mon-line-dolines-TEST'.\n
:SEE (URL `http://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00614.html')\n
:SEE-ALSO .\n►►►"
  (mon-line-dolines 
      (start end)
      (goto-char start)
    (when (search-forward-regexp "^\\( *?[0-9]*\\)\\( *?[0-9]*\\)\\( *[0-9]*\\)$" end  t)
      (let ((day   (match-string 1))
            (month (match-string 2))
            (year  (match-string 3)))
        (with-current-buffer (get-buffer-create "*GOT-DAYS*")
          (insert day "\n"))
        (with-current-buffer (get-buffer-create "*GOT-MONTHS*")
          (insert month "\n"))       
        (with-current-buffer (get-buffer-create "*GOT-YEARS*")
          (insert year "\n")))))
  (let ((if-buff '("*GOT-YEARS*" "*GOT-MONTHS*" "*GOT-DAYS*"))
        this-window-buff)
    (setq this-window-buff 
          (cons (buffer-name (current-buffer))
                (or (get-buffer-window)
                    (car (get-buffer-window-list (current-buffer))))))
    (unless (and (car this-window-buff) (cdr this-window-buff) this-window-buff)
      (progn 
        (with-current-buffer
            (get-buffer-create "*MON-LINE-DOLINES-TEST*")
          (pop-to-buffer (current-buffer) t)
          (setq this-window-buff 
                (cons (buffer-name (current-buffer))
                      (get-buffer-window (current-buffer)))))))
    (unwind-protect
        (progn
          (when (one-window-p) (split-window-vertically))
          (dolist (ifb if-buff 
                       (dolist (is-b if-buff)
                         (select-window (next-window))
                         (set-window-buffer (selected-window) is-b)
                         (unless (null if-buff) (pop if-buff)
                                 (unless (null if-buff)
                                   (split-window-horizontally)))))
            (unless (mon-buffer-exists-p ifb) (remove ifb if-buff))))
      (progn
        (set-window-buffer 
         (or (get-buffer-window (car this-window-buff)) (cdr this-window-buff))
         (car this-window-buff))
        (pop-to-buffer (car this-window-buff))
        (when (equal (buffer-name (current-buffer))
                     "*MON-LINE-DOLINES-TEST*")
          (save-excursion (mon-line-dolines-TEST)))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-26T15:28:44-04:00Z}#{10213} - by MON>
(defun mon-line-dolines-TEST ()
  "Test function for `mon-line-dolines' macro.\n
Return the output of that macro as per its original intended use.\n
Values returned in 3 (or 4) seperate buffers named:\n
 \"*GOT-YEARS*\" \"*GOT-MONTHS*\" \"*GOT-DAYS*\"\n
When current-buffer does not have a display or is read-only return additional
details in buffer named \"*MON-LINE-DOLINES-TEST*\".\n
:SEE (URL `http://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00614.html')\n
:SEE-ALSO `mon-line-dolines-setup-TEST'.\n►►►"
  (let ((mldt (mapconcat 'identity
                         '(":DATE        :MONTH           :YEAR"
                           ""
                           "12              5           1955"
                           "30              6           1931"
                           "                3           1918"
                           "28                          1877"
                           "39              2           1888") "\n"))
        (exit-c 78)
        got-N)
    (save-window-excursion
      (if (and (not (equal (buffer-name) " *temp*"))
               (not (equal (buffer-name) "*MON-LINE-DOLINES-TEST*"))
               (or (not (get-buffer-window (current-buffer)))
                   buffer-read-only))
          (with-temp-buffer 
            (save-excursion (insert  mldt))
            (mon-line-dolines-setup-TEST))
        (cond  ((equal (buffer-name) "*MON-LINE-DOLINES-TEST*")
                (save-excursion
                  (insert ";;; With this data:\n\n"
                          mldt "\n\n"
                          "\n;;;Evaluated `mon-line-dolines' with `mon-line-dolines-setup-TEST' as below:\n\n"
                          (pp (symbol-function 'mon-line-dolines-setup-TEST))))
                (emacs-lisp-mode)
                (while (not got-N)
                  (when (eq (read-event 
                             (format (concat "Type `%c' to continue and kill *TEST* buffers,"
                                             "or %s to exit without killing test buffers.")
                                     exit-c (key-description [7])))
                            exit-c)
                    (let (bld-ksb)
                      (dolist (ksb 
                               '("*MON-LINE-DOLINES-TEST*" "*GOT-YEARS*" "*GOT-MONTHS*" "*GOT-DAYS*")
                               (kill-some-buffers bld-ksb))
                        (when (get-buffer ksb)
                          (push (get-buffer ksb) bld-ksb))))
                    (setq got-N t))))
               (t (insert  "\n;;;Evaluated `mon-line-dolines' with `mon-line-dolines-setup-TEST'.\n\n"
                           "\n;; With this data:\n\n")
                  (push-mark nil nil t)
                  (insert mldt "\n\n")
                  (mon-line-dolines-setup-TEST)
                  (unless (equal (buffer-name) "*MON-LINE-DOLINES-TEST*")
                    (while (not got-N)
                      (when (eq (read-event 
                                 (format (concat "Type `%c' to continue and kill *TEST* buffers,"
                                                 "or %s to exit without killing test buffers.")
                                         exit-c (key-description [7])))
                                exit-c)
                        (let (bld-ksb)
                          (dolist (ksb 
                                   '("*MON-LINE-DOLINES-TEST*" "*GOT-YEARS*" "*GOT-MONTHS*" "*GOT-DAYS*")
                                   (kill-some-buffers bld-ksb))
                            (when (get-buffer ksb)
                              (push (get-buffer ksb) bld-ksb))))
                        (setq got-N t))))))))))
;;
;;; TEST-ME (mon-line-dolines-TEST)

;;; ==============================
;;; :NOTE (length "=> TO-COLM-NUM-19-!")
;;; :CREATED <Timestamp: #{2009-12-09T15:07:13-05:00Z}#{09503} - by MON>
(defun mon-line-strings-pipe-to-col (start end &optional to-col insrtp intrp)
  "Return region's BOL piped and indented to column number.\n
When TO-COL is non-nil return region indented TO-COL, default column number 7.
When called-interactively or INSRTP is non-nil replace region.\n
:EXAMPLE\n\n\(let \(\(reb \(1+ \(search-forward-regexp \"►\"\)\)\)
      \(ree \(- \(search-forward-regexp \"◄\"\) 2\)\)\)
  \(momentary-string-display
   \(concat \"\\n\\n=> TO-THE-19th-COL-!\\n\\n\"
           \(mon-line-strings-pipe-to-col reb ree 19\)
           \"\\n\\n... and beyond ... :\)\\n\"\) \(point\)\)\)\n
\(mon-help-overlay-for-example 'mon-line-strings-pipe-to-col nil 'region 28\)\n
►\nWilliam Gibson\nBruce Sterling\nDan Brown\nNeal Stephenson\nLoyd Blankenship
Erik Gordon Corley\n◄\n
:SEE :FILE align.el for alternative approaches.\n
:SEE-ALSO `mon-line-strings-pipe-bol', `mon-line-strings-indent-to-col',
`mon-line-strings', `mon-line-indent-from-to-col',
`mon-comment-divider->col', `mon-comment-lisp-to-col'.\n►►►"
  (interactive "i\n\i\nP\ni\np")
  (let  ((rg-b (make-marker))
         (rg-e (make-marker))
         (tmp-pipe))
    (set-marker rg-b (cond (intrp (region-beginning))
                           (start start)))
    (set-marker rg-e (cond (intrp (region-end))
                           (end end)))
    (setq tmp-pipe (buffer-substring-no-properties rg-b rg-e))
    (setq tmp-pipe 
          (with-temp-buffer 
            (insert tmp-pipe)
            (mon-line-strings-pipe-bol (buffer-end 0) (buffer-end 1) t)
            (mon-line-strings-indent-to-col 
             (buffer-end 0) (buffer-end 1) (if to-col to-col 7) t)
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if (or insrtp intrp)
        (save-excursion 
          (delete-region rg-b rg-e)
          (goto-char rg-b)
          (insert tmp-pipe))
        tmp-pipe)))
;;
;;; :TEST-ME 
;;; (let ((reb (1+ (search-forward-regexp "►")))
;;;       (ree (- (search-forward-regexp "◄") 2)))
;;;   (mon-line-strings-pipe-to-col reb ree 12))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | ►
;; | William Gibson
;; | Bruce Sterling
;; | Dan Brown
;; | Neal Stephenson
;; | Loyd Blankenship
;; | Erik Gordon Corley
;; | ◄
;; `----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-13T09:30:42-04:00Z}#{09377} - by MON>
(defun mon-line-strings-to-list (start end &optional w-cdr w-wrap insrtp intrp)
  "Return region's lines as list, each list elt contains string content of line.
Region between START END should be passed as a line per string/symbol.
Strips trailing whitespace. Does not preseve tabs converts them to spaces.
When W-CDR is non-nil or called-interactively with prefix-arg return each
element of list with an empty string as cdr.\n\n:EXAMPLE\n
Mon Key\nMON\nMon\nMON KEY\n\n;; When W-CDR nil:
=>\((\"Mon Key\"\)\n   \(\"MON\"\)\n   \(\"Mon\"\)\n   \(\"MON KEY\"\)\)\n
;; When W-CDR non-nil:\n=>\(\(\"Mon Key\" \"\"\)\n   \(\"MON\" \"\"\)
   (\"Mon\" \"\"\)\n   \(\"MON KEY\" \"\"\)\)\n
\(mon-line-strings-to-list-TEST t nil\)\n
\(mon-line-strings-to-list-TEST\)\n
:SEE-ALSO `mon-line-strings-to-list-TEST', `mon-line-strings-one-list',
`mon-line-strings-region-delimited', `mon-line-string-rotate-name',
`mon-line-string-rotate-namestrings', `mon-line-string-unrotate-namestrings',
`mon-line-string-rotate-namestrings-combine', `mon-make-lastname-firstname',
`mon-make-name-lispy', `mon-make-names-list', `mon-string-ify-current-line',
`mon-line-strings-qt-region', `mon-string-ify-list', `mon-string-split-line',
`mon-line-strings', `mon-line-strings-region', `mon-line-drop-in-words'.\n►►►"
  (interactive "r\ni\nP\ni\np") ;; (interactive "r\nP\ni\ni\np") make w-cdr the pref arg
  (let ((start-reg start)
        (end-reg end)
        (rgn-l))
    (setq rgn-l (buffer-substring-no-properties start end))
    (save-excursion
      (setq rgn-l (with-temp-buffer
                    (insert rgn-l) 
                    (untabify (point-min) (point-max))
                    (mon-cln-trail-whitespace) ;; (point-min) (point-max))
                    (goto-char (point-min))
                    (while (search-forward-regexp "^\\(.*\\)$" nil t)
                      (if w-cdr 
                          (replace-match "(\"\\1\" \"\")")
                        (replace-match "(\"\\1\")")))
		    (goto-char (point-max)) 
		    (if w-wrap (insert "))") (insert ")"))
		    (goto-char (point-min))
		    (if w-wrap
			(save-excursion 
			  (insert "(;; defvar defconst let let* setq\n'("))
		      (indent-pp-sexp 1)
		      (insert "("))
			(buffer-substring-no-properties (point-min) (point-max)))))
      (if (or insrtp intrp)
	  (save-excursion (delete-region start-reg end-reg)(insert rgn-l))
	rgn-l)))

;;; ==============================
;;; :RENAMED `mon-line-strings-to-list-*test*' -> `mon-line-strings-to-list-TEST'
;;; :CREATED <Timestamp: #{2009-09-13T09:28:46-04:00Z}#{09377} - by MON>
(defun mon-line-strings-to-list-TEST (&optional with-cdr with-wrap insrtp)
  "Test function for `mon-line-strings-to-list'.\n
:SEE-ALSO `mon-build-copyright-string-TEST', `mon-help-regexp-symbol-defs-TEST', 
`mon-help-propertize-regexp-symbol-defs-TEST', 
`mon-help-regexp-symbol-defs-TEST', `mon-help-CL-wget-pkgs-TEST', 
`mon-help-propertize-tags-TEST', `mon-insert-test-cases'.\n►►►"
  (let ((st01 (make-marker))
        (en01 (make-marker))
        (t-str (concat "hendr erit\norci\nultrices\naugue\nAliquam\n"
                       "odio\nNam\ne ros\nurna\naliquam\nvitae\nlacinia")))
    (cond ((not insrtp)
           (with-temp-buffer
             (insert t-str)
             (mon-line-strings-to-list (point-min) (point-max) with-cdr with-wrap)))
          (insrtp 
           (set-marker st01 (point))
           (insert t-str)
           (set-marker en01 (point))
           (goto-char st01)
           (mon-line-strings-to-list st01 en01 with-cdr with-wrap t)))))
;;
;;; :TEST-ME (mon-line-strings-to-list-TEST)
;;; :TEST-ME (mon-line-strings-to-list-TEST t nil)
;;; :TEST-ME (mon-line-strings-to-list-TEST t t)
;;; :TEST-ME (mon-line-strings-to-list-TEST t nil t)
;;; :TEST-ME (mon-line-strings-to-list-TEST t t t)
;;
;;;(progn (newline) (mon-line-strings-to-list-TEST t t))
;;;(progn (newline) (mon-line-strings-to-list-TEST nil t))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-17T12:48:16-05:00Z}#{10027} - by MON>
(defun mon-line-strings-one-list (start end &optional rep-rgn-w-list intrp)
  "Return lines in region from START TO END as a list of strings.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-strings',
`mon-line-strings-region-delimited', `mon-string-ify-list'.\n►►►"
  (interactive "r\n\i\np")
  (let (lstr1l)
    (setq lstr1l (buffer-substring-no-properties start end))
    (with-temp-buffer      
      (insert lstr1l)
      (setq lstr1l (mon-line-strings (buffer-end 0) (buffer-end 1))))
    (if (or rep-rgn-w-list intrp)
        (save-excursion 
          (goto-char start)
          (delete-region start end)
          (princ (list lstr1l) (current-buffer)))
      lstr1l)))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-19T13:53:29-04:00Z}#{09386} - by MON>
(defun mon-line-string-rotate-name (name-str-or-elt &optional as-list)
  "Rotate the namestring NAME-STR-OR-ELT. 
Return the last whitespace delimited name in string at head top of string.
Remaining names in string returned inside a parenthetical group.
NAME-STR-OR-ELT is a string containing one nameform or one elt listsame 
holding a string containing one nameform.\n
:EXAMPLE\n\(mon-line-string-rotate-name \"István Tisza\")\n
\(mon-line-string-rotate-name '\(\"Stanisław Marcin Ulam\")\)\n
\(mon-line-string-rotate-name '\(\"Dmitri Pavlovich Romanov\")\)\n
\(mapconcat #'\(lambda \(x\) \(mon-line-string-rotate-name x\)\)
           '\(\(\"George Charles Aid\"\)\(\"Thomas Pollock Anshutz\"\)
             \(\"Cecilia Beaux\"\)\(\"Frank Weston Benson\"\)
             \(\"Thomas Hart Benton\"\)\(\"Saul Bernstein\"\)
             \(\"George Biddle\"\)\(\"Gutzon Borglum\"\)\)
           \"\\n\"\)\n
:SEE-ALSO `mon-line-strings', `mon-line-strings-to-list',
`mon-line-string-rotate-namestrings', `mon-line-string-unrotate-namestrings',
`mon-line-string-rotate-namestrings-combine', `mon-make-lastname-firstname',
`mon-make-name-lispy', `mon-make-names-list', `mon-line-strings-region'.\n►►►"
(let* ((nm-or-elt (if (atom name-str-or-elt)
                      name-str-or-elt
                      (let ((get-head name-str-or-elt))
                        (while (consp get-head)
                          (setq get-head (car get-head)))
                        get-head)))
       (the-split (split-string nm-or-elt))
       (split-len (length the-split))
       (last-in (cond ((= split-len 1) (format "%s" (car the-split)))
                      ((> split-len 1) 
                       (let ((rot-split 
                              (append (edmacro-subseq the-split -1)
                                      (edmacro-subseq the-split 0 (1- split-len)))))
                         (format "%s %s" (car rot-split) (cdr rot-split))))
                      ((= split-len 0) nil))))
  (if as-list (list last-in) last-in)))
;;
;;; :TEST-ME (mon-line-string-rotate-name "Elvis")
;;; :TEST-ME (mon-line-string-rotate-name "István Tisza")
;;; :TEST-ME (mon-line-string-rotate-name "Thomas Pollock Anshutz")
;;; :TEST-ME (mon-line-string-rotate-name "Thomas Pollock Anshutz" t)
;;; :TEST-ME (mon-line-string-rotate-name '("Thomas Pollock Anshutz") t)
;;; :TEST-ME (mapc #'(lambda (x) (princ (concat "\n" (mon-line-string-rotate-name x)) (current-buffer)))
;;;        '(("George Charles Aid")("Thomas Pollock Anshutz")("Cecilia Beaux")("Frank Weston Benson")
;;;          ("Thomas Hart Benton")("Saul Bernstein")("George Biddle")("Gutzon Borglum")))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-22T16:39:59-04:00Z}#{09392} - by MON KEY>
(defun mon-line-string-rotate-namestrings (start end &optional as-strings insrtp intrp)
  "Rotate namestrings in region. Namestring are formatted one name per line
Firstname Middlenames Lastname. Return Lastname (Firstname Middlename).
When AS-STRINGS is non-nil retrun namestrings as strings as with prin1.
When INSRTP is non-nil or called-interactively insert rotated names at point.
Does not move point.\n
:SEE-ALSO `mon-line-string-unrotate-namestrings', `mon-line-string-rotate-name', 
`mon-line-string-rotate-namestrings-combine', `mon-line-strings-to-list',
`mon-make-lastname-firstname', `mon-make-names-list', `mon-make-name-lispy',
`mon-line-strings-region'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((r-nms-strt start)
	(r-nms-end  end)
	(get-namestrings))
    (setq get-namestrings 
	  (mapconcat #'(lambda (x) (mon-line-string-rotate-name (car x))) 
     		     (read (mon-line-strings-to-list r-nms-strt r-nms-end)) "\n"))
    (if (or insrtp intrp)
        (progn
          (save-excursion
	  (delete-region r-nms-strt r-nms-end)
	  (if as-strings
              (mapc #'(lambda (x) (newline) (prin1 x (current-buffer)))
                    (split-string get-namestrings "\n"))
              (insert get-namestrings)))
          (when as-strings (delete-char 1)))
      (if as-strings 
          (split-string get-namestrings "\n") 
        get-namestrings))))
;;
;;; :TEST-ME 
;;; (mon-line-string-rotate-namestrings 
;;;    (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2))
;;; (mon-line-string-rotate-namestrings 
;;;    (1+ (search-forward-regexp "◄")) (- (search-forward-regexp "◄") 2) t)
;;;
;; ,---- :UNCOMMENT-TO-TEST
;; |►
;; |George Charles Aid
;; |Thomas Pollock Anshutz
;; |Cecilia Beaux
;; |Frank Weston Benson
;; |Thomas Hart Benton
;; |Saul Bernstein
;; |George Biddle
;; |Gutzon Borglum
;; |◄
;; `----

;; ==============================
;;; :CREATED <Timestamp: #{2009-09-23T20:12:26-04:00Z}#{09394} - by MON KEY>
(defun mon-line-string-unrotate-namestrings (start end &optional as-strings insrtp intrp)
  "Unrotate namestrings in region.\n
Namestrings are formatted name per line e.g. `Lastname (Firstname Middlenames)'
Return `Firstname Middlename Lastname'
When INSRTP is non-nil or Called-interactively insert rotated names at point.
Does not move point. When AS-STRINGS is non-nil return rotated names as strings.\n
:EXAMPLE\n\(mon-line-string-unrotate-namestrings 
   (1+ \(search-forward-regexp \"►\"\)) \(- \(search-forward-regexp \"◄\"\) 2\)\)\n
►\nKennan (George Frost)\nAlbert (Lukács János)\nAchesonn (Dean Gooderham)
Harriman (William Averell)\nMcCloy (John Jay)\nBohlen (Charles Eustis)
Lovett (Robert Abercrombie)\n◄\n
:SEE-ALSO `mon-line-string-rotate-name', `mon-line-string-rotate-namestrings',
`mon-line-string-rotate-namestrings-combine', `mon-line-strings-to-list',
`mon-make-lastname-firstname', `mon-make-name-lispy', `mon-make-names-list',
`mon-line-strings-region'.\n►►►"
  (interactive "r\nP\ni\np")
  (let ((s-r start)
        (e-r end)
        (go-temp))
    (setq go-temp (buffer-substring-no-properties s-r e-r))
    (save-excursion
      (setq go-temp
            (with-temp-buffer
              (insert go-temp)
              (whitespace-cleanup)
              (goto-char (buffer-end 0))
              (while (search-forward-regexp  
                      "^\\([A-z-]+\\) \\((\\)\\(.*\\)\\()\\)$" (buffer-end 1) t)
                      ;;^^1^^^^^^^^^^^^^2^^^^^^^3^^^^^^4^^^^
                (replace-match  "\\3 \\1"))
              (if as-strings
                  (mon-line-strings-to-list (buffer-end 0) (buffer-end 1))
                (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))))
    (if (or insrtp intrp)
        (progn
          (save-excursion 
            (delete-region s-r e-r)
            (if as-strings
                (let ((as-str (read go-temp)))
                  (mapc #'(lambda (x) (newline) (prin1 (car x) (current-buffer))) as-str))
              (insert go-temp)))
          (when as-strings (delete-char 1)))
      ;; elseif
      (if as-strings
          (let ((as-str (read go-temp))
                (rtn-str))
            (setq rtn-str (mapcar #'(lambda (x) (car x)) as-str))
            rtn-str)
        go-temp))))
;;
;;,---- :UNCOMMENT-TO-TEST:
;;| (mon-line-string-unrotate-namestrings 
;;|   (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2))
;;| 
;;| ►
;;| George Frost Kennan
;;| Dean Gooderham Acheson
;;| William Averell Harriman
;;| Lukács János Albert 
;;| John Jay McCloy
;;| Charles Eustis Bohlen 
;;| Robert Abercrombie Lovett
;;| ◄
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-24T14:18:44-04:00Z}#{09394} - by MON>
(defun mon-line-string-rotate-namestrings-combine (start end &optional insrtp intrp)
  "Return lists of namestrings from START to END both rotated and normalalized.
Elements of list returned have the form:
\(\"Fname Lname\" \"Lname \(Fname\)\"\)\n
:EXAMPLE\n\(mon-line-string-rotate-namestrings-combine
   (1+ \(search-forward-regexp \"►\"\)) \(- \(search-forward-regexp \"◄\"\) 2\)\)\n
►\nEmil Max Hödel\nJohn Wilkes Booth\nLeon Frank Czolgosz\nLee Harvey Oswald
Dmitry Grigoriyevich Bogrov\nPaul Gorguloff\nJohn Bellingham
Charles Julius Guiteau\n◄\n
:SEE-ALSO `mon-line-string-rotate-namestrings',
`mon-line-string-unrotate-namestrings', `mon-make-lastname-firstname',
`mon-make-name-lispy', `mon-make-names-list', `mon-line-string-rotate-name',
`mon-line-strings-to-list', `mon-line-string-insert-chars-under'.\n►►►"
  (interactive "r\ni\np")
  (let ((rotd-nms (mon-line-string-rotate-namestrings start end t))
        (unrotd-nms)
        (combined))
    (with-temp-buffer
      (progn
        (save-excursion
          (mapc #'(lambda (x) (newline) (princ x (current-buffer))) rotd-nms))
        (delete-char 1))
      (setq unrotd-nms
            (mon-line-string-unrotate-namestrings (point-min) (point-max) t)))
    (mapc #'(lambda (x)
            (let ((orig (pop rotd-nms)))
              (setq combined (cons `(,x ,orig) combined))))
          unrotd-nms)
    (if (or insrtp intrp)
        (prin1 combined (current-buffer))
        combined)))
;;
;; ,---- :UNCOMMENT-TO-TEST
;; | 
;; | (mon-line-string-rotate-namestrings-combine
;; |    (1+ (search-forward-regexp "►")) (- (search-forward-regexp "◄") 2))
;; | 
;; | ►
;; | Emil Max Hödel
;; | John Wilkes Booth
;; | Leon Frank Czolgosz
;; | Lee Harvey Oswald
;; | Dmitry Grigoriyevich Bogrov
;; | Paul Gorguloff
;; | John Bellingham
;; | Charles Julius Guiteau
;; | ◄
;; `----


;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-20T16:16:44-04:00Z}#{09432} - by MON>
(defun mon-line-string-insert-chars-under (&optional with-char intrp)
  "Insert a string of `='s (char 61) beneath the current line.\n
Inserted string has the length of current line. Does not move point.
When WITH-CHAR (char or string) is non-nil insert that char instead.
When called-interactively with prefix-arg prompt for a char to use.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-strings-region-delimited'.\n►►►"
  (interactive "P\np")
  (let ((ln-spec
         (if (looking-at "^$")
             (error (concat ":FUNCTION `mon-line-string-insert-chars-under'"
                            "-- no line at point: %d") (point))
           (bounds-of-thing-at-point 'line)))
        (with-char (if (and with-char intrp)
                       (read-char (concat ":FUNCTION `mon-line-string-insert-chars-under'"
                                "-- char to use: "))
                     with-char)))
    (save-excursion
      (end-of-line)
      (when (= (buffer-end 1)(cdr ln-spec))
        (setcdr ln-spec (1+ (cdr ln-spec))))
      (open-line 1)
      (forward-char 1)
      (insert (make-string ;;(- (1- (cdr ln-spec)) (car ln-spec)) 
               (- (1- (cdr ln-spec)) (car ln-spec))
               (if with-char                              
                   (if (stringp with-char)
                       (string-to-char with-char)
                     with-char)
                 61))))))
;;
;;; :TEST-ME (mon-line-string-insert-chars-under)
;;; :TEST-ME (mon-line-string-insert-chars-under 9658)
;;; :TEST-ME (mon-line-string-insert-chars-under "►")
;;; :TEST-ME (mon-line-string-insert-chars-under t t)

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :WAS `get-next-word' -> `mon-word-get-next'
(defun mon-word-get-next ()
  "Return the next 'word' in the buffer.\n
Point is left following the word.\n
When `eopb' return nil with point unchanged.\n
:NOTE Words motion is per `inhibit-field-text-motion' where a word is defined by
`forward-word' according to the syntax-table settings.\n
:SEE-ALSO `mon-line-get-next', `mon-word-get-list-in-buffer'.\n►►►"
  (let (mwgn-start mwgn-end)
    (if (eobp)
	nil
      (progn
	(setq mwgn-start (point))
	(forward-word 1)
	(setq mwgn-end (point))
	(forward-word -1)
	(if (< (point) mwgn-start)           ;; Then we're already past last word.
	    (progn
	      (goto-char (buffer-end 0))
              nil)
	  (setq mwgn-start (point))
	  (goto-char mwgn-end)
	  (buffer-substring-no-properties mwgn-start mwgn-end))))))


;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS clsc.el :VERSION 1.53 of 2001-05-27
;;; :CREATED <Timestamp: #{2010-08-20T13:24:05-04:00Z}#{10335} - by MON KEY>
(defun mon-word-get-list-in-buffer (&optional intrp)
  "Convert the entire buffer to a list of `newline' separated ``words''
in a new buffer `\"*MON-WORD-LIST*\", where a word is defined by `forward-word'
according to the syntax-table settings.\n
:EXAMPLE\n\n(mon-word-get-list-in-buffer\)\n
\(mon-word-get-list-in-buffer t\)\n
:NOTE Apply `sort-lines', `unique-lines', etc. to obtain a list of all the
unique words in a buffer of document.\n
:ALIASED-BY `mon-buffer-get-word-list'\n
:SEE-ALSO `mon-word-count-occurrences', `mon-line-strings-to-list',
`mon-string-ify-current-line', `mon-stringify-list', `mon-dropin-line-word',
`mon-insert-string-ify', `mon-word-count-analysis', `mon-word-count-region',
`mon-word-count-chars-region'.\n►►►"
  ;; :WAS
  ;; (interactive)
  ;; (let (word)
  ;;   (with-output-to-temp-buffer "*MON-WORD-LIST*"
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (while (setq word (mon-word-get-next))
  ;;         (princ (format "%s\n" word)))))))
  (interactive "p")
  (let* ((MWL (get-buffer-create "*MON-WORD-LIST*"))
         (standard-output (get-buffer MWL))
         mwglib)
    (with-current-buffer MWL
      (erase-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (setq mwglib (mon-word-get-next))
        (princ (format "%s\n" mwglib))))
    (setq mwglib (buffer-name (current-buffer)))
    (with-current-buffer  MWL
      (setq MWL `(:BUFFER ,mwglib 
                  :TOTAL-WORDS ,(count-lines (buffer-end 0) (buffer-end 1))))
      (if intrp 
          (progn
            (goto-char (buffer-end 0))
            (save-excursion            
              (insert  ";; :FUNCTION `mon-word-get-list-in-buffer'\n;; ")
              (princ MWL (current-buffer))
              (insert "\n" (make-string 68 59) "\n"))
              (display-buffer (current-buffer) t))
        (progn
          (setq MWL (append MWL (mon-line-strings-one-list (buffer-end 0) (buffer-end 1))))
          (kill-buffer (current-buffer)))))
    MWL))
;;
(defalias 'mon-buffer-get-word-list 'mon-word-get-list-in-buffer)

;;; ==============================
(defun mon-word-reverse-region (beg end)
  "Reverse the order of words in region.\n
:SEE-ALSO `mon-region-reverse'.\n►►►"
  (interactive "r")
  (apply 'insert
         (reverse (save-match-data 
                    (split-string
                     (delete-and-extract-region beg end) "\\b")))))

;;; ==============================
;;; :COURTESY Jonathan Rockway :VERSION 2009-01-18
;;; :SEE (URL `http://blog.jrock.us/articles/Iterators%20in%20elisp.pod')
;;; :REQUIRES (require 'cl)
(defun mon-word-iterate-over (buffer)
  "Return an iterator that gets the next word in buffer.\n
Uses lexical-let for a lambda closure over buf and pos.
Extract one word at a time by calling (funcall next-word).\n
:EXAMPLE For BUFFER test-buffer containing \"This is text.\"
\(setq next-word \(mon-word-iterate-over-in \(get-buffer \"test buffer\")))
The first time next-word is called, return \"This\".
The next time, retrun \" is\". Then, \" text.\". 
Finally, return nil forever.\n
:SEE-ALSO `mon-word-get-list-in-buffer'.\n►►►"
  (lexical-let ((buf buffer)(pos 1))
    (lambda ()
      (save-excursion
        (let ((cur (current-buffer)) result)
          (switch-to-buffer buf)
          (goto-char pos)
          (forward-word)
          (let ((pt (point)))
            (if (not (eq pos pt))
                (progn 
                  (setq result (buffer-substring-no-properties pos pt))
                  (setq pos pt))))
          (switch-to-buffer cur) result)))))

;;; ==============================
;;; :CHANGESET 1973 <Timestamp: #{2010-07-12T20:20:45-04:00Z}#{10281} - by MON KEY>
(defun mon-word-count-analysis (start end &optional intrp)
  "Count number of times each word is used in the region.
Count anything with word syntax when `with-syntax-table' uses`standard-syntax-table'.\n
:EXAMPLE\n\n\(mon-word-count-analysis \(buffer-end 0\) \(buffer-end 1\)\)\n
\(mon-word-count-analysis \(buffer-end 0\) \(buffer-end 1\) t\)\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-chars-region',
`mon-word-count-occurrences', `mon-word-count-region', 
`mon-word-get-list-in-buffer'.\n►►►"
  (interactive "r\np")
  (let (words hshd-wrds oba)
    (setq oba (make-vector (/ (- end start) 2) 0))
    (setq words (make-hash-table :test #'eq :weakness 'key))
    (unwind-protect 
        (narrow-to-region start end)
      (save-excursion
        (goto-char (buffer-end 0))
        (with-syntax-table (syntax-table)
          (while (search-forward-regexp "\\w+" end t)
            (let* ((word (intern (match-string-no-properties 0) oba))
                   (cell (gethash word words)))
              (if (and (not (numberp word)) cell)
                  (puthash word (1+ cell) words)
                (puthash word 1 words))))))
      (widen))
    (maphash #'(lambda (k v) (push `(,k . ,v) hshd-wrds)) words)
    (setq hshd-wrds (nreverse hshd-wrds))
    (if intrp
        (message (concat ":FUNCTION `mon-word-count-analysis' "
                         "-- :WORDS-FOUND %S")
                 hshd-wrds)
      `(:WORDS-FOUND ,@hshd-wrds))))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el :WAS `ff/word-occurrences'
;;; :SEE (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :SEE git clone http://fleuret.org/git/elisp/ 
;;; :CHANGESET 1969 <Timestamp: #{2010-07-12T13:40:08-04:00Z}#{10281} - by MON KEY>
;;; Added `with-current-buffer', `message', `with-silent-modifications', `window-min-height'
(defun mon-word-count-occurrences (&optional intrp)
  "Display in a new buffer the list of words sorted by number of occurrences.\n
Count contains multiple occurences of words with > word-length 3 in buffer.\n
Return results and display in buffer named \"*WORD-COUNT*\".\n
:EXAMPLE\n\n\(mon-word-count-occurrences\)\n
\(mon-word-count-occurrences t\)\n
:ALIASED-BY `mon-buffer-get-word-count'\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-region', `mon-word-count-analysis',
`mon-word-count-chars-region', `mon-word-get-list-in-buffer'.\n►►►"
  (interactive "p")
  (let ((cnt-buf (when intrp (get-buffer-create "*WORD-COUNT*")))
        (dpt-buffer (when intrp (current-buffer)))
        (dpt-window (when intrp (get-buffer-window (current-buffer))))
        (map (when intrp (make-sparse-keymap)))
        (nb (make-hash-table))
        (st (make-hash-table))
        (words-chars (mon-word-count-chars-region (buffer-end 0) (buffer-end 1)))
        result)
    ;; Collect all words into a pair of hash-tables.
    (save-excursion
      (goto-char (buffer-end 0))
      (with-syntax-table (standard-syntax-table)
        (while (re-search-forward "\\([\\-a-zA-Z\\\\]+\\)" nil t)
          (let* ((s (downcase (match-string-no-properties 1)))
                 (k (sxhash s)))
            (puthash k s st)
            (puthash k (1+ (gethash k nb 0)) nb)))))
    (if (<= (hash-table-count nb) 0)
        (if intrp 
            (message 
             (concat ":FUNCTION `mon-word-count-occurrences' "
                     "-- did not find re-occurences for words with > word-length 3 in buffer: %s") dpt-buffer)
          `(,@words-chars :WORDS-W-LEN-GT-3 0))
      (progn
        (maphash #'(lambda (key value)
                     (setq result (cons (cons value (gethash key st)) result)))
                 nb)
        ;; Find the longest string
        (if (not intrp)
            (progn (setq nb) (setq st))
          (progn
            (setq nb)
            (setq st '(0 . " "))
            (dolist (strl result (setq st (length (cdr st))))
              (let ((len-strl (length (cdr strl))))
                (when (> len-strl (car st)) 
                  (setq st `(,len-strl . ,(cdr strl))))))))
        (setq result (sort result #'(lambda (a b) (> (car a) (car b)))))
        (dolist (wc result (setq nb (nreverse nb)))
          (if (and (> (car wc) 1)
                   ;; No leading backslash and at least four characters.
                   (string-match "^[^\\]\\{4,\\}" (cdr wc)))
              (if intrp 
                  (push (concat (cdr wc) 
                                (if (< (length (cdr wc)) st)
                                    (make-string (- st (length (cdr wc))) 32)
                                  " ")
                                (number-to-string (car wc))) nb)
                (push `(,(cdr wc) . ,(car wc)) nb))))
        (if (not intrp)
            `(,@words-chars :WORDS-W-LEN-GT-3 ,(length nb) (:WORDS-W-COUNTS ,@nb))
          ;; Create the result buffer if called-interactively
          (with-current-buffer cnt-buf
            (erase-buffer)
            (set (make-local-variable 'show-trailing-whitespace) nil)
            (save-excursion
              (insert ";; :W-FUNCTION     `mon-word-count-occurrences' \n"
                      ";; :IN-BUFFER       " (buffer-name dpt-buffer) "\n"
                      ";; :W-SYNTAX-TABLE `standard-syntax-table'\n"
                      ";; " (mapconcat #'(lambda (wrdch) (format "%s" wrdch)) words-chars " ") "\n"
                      ";; :NOTE To return to buffer counted type: \"C-c q\"\n;;\n"
                      (cond ((> st 8) (concat ";; :WORD"  (make-string (- st 9) 32) ":COUNT\n"))
                            ((< st 8) (concat ";; :WORD  :COUNT\n")))
                      (mapconcat 'identity nb "\n")))
            (define-key map "\C-cq" `(lambda () 
                                       (interactive)
                                       (if (buffer-live-p ,(buffer-name dpt-buffer))
                                           (progn (switch-to-buffer ,(buffer-name dpt-buffer))
                                                  (when (get-buffer "*WORD-COUNT*")
                                                    (kill-buffer (get-buffer "*WORD-COUNT*"))))
                                         (kill-this-buffer))))
            (use-local-map map)
            (display-buffer (current-buffer) t)
            (with-silent-modifications
              (let ((window-min-height 10)
                    (win-max-height-maybe 
                     (let* ((wh (window-height 
                                 (if (equal dpt-buffer (window-buffer dpt-window))
                                     dpt-window
                                   (other-window 1))))
                            (wh/2 (cond ((>= (length nb) 20)
                                         (+ wh (/ (length nb) 2)))
                                        ((>= (+ wh (length nb)) 20)
                                         (+ wh (length nb)))
                                        (t 12))))
                       wh/2)))
                (fit-window-to-buffer 
                 (get-buffer-window (current-buffer)) win-max-height-maybe 10)))))))))
;; 
(defalias 'mon-buffer-get-word-count 'mon-word-count-occurrences)

;;; =======================
(defun mon-word-count-region (start end &optional intrp)
  "Return the number of words in the region.\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-chars-region',
`mon-word-count-analysis', `mon-word-count-occurrences',
`mon-word-get-list-in-buffer'.\n►►►"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (buffer-end 0))
      (let ((matches (count-matches "\\sw+")))
        (if intrp 
            (message (concat ":FUNCTION `mon-word-count-region' "
                             "-- :WORDS %d in the region") matches)
          `(:WORDS ,matches))))))

;;; ==============================
(defun mon-word-count-chars-region (beginning end &optional intrp)
  "Return message indicating the number of words and chars that are in a region.\n
:EXAMPLE\n\n\(mon-word-count-chars-region \(buffer-end 0\) \(buffer-end 1\)\)\n
\(mon-word-count-chars-region \(buffer-end 0\) \(buffer-end 1\) t\)\n
:SEE-ALSO `mon-line-count-region', `mon-word-count-region',
`mon-word-count-analysis', `mon-word-count-occurrences', 
`mon-word-get-list-in-buffer', `mon-string-from-sequence'.\n►►►"
  (interactive "r\np")
  (let ((w-count 0) 
        (char-count (- end beginning)))
    (save-excursion
      (goto-char beginning)
      (while (and (< (point) end) (search-forward-regexp "\\w+\\W*" end t))
        (incf w-count))
      (if intrp 
          (message  (concat ":FUNCTION `mon-word-count-chars-region' "
                            "-- counted :WORDS %d :CHARS %d")
                    w-count char-count)
        `(:WORDS ,w-count :CHARS ,char-count)))))

;;; ==============================
;;; :COURTESY Henrik Enberg but prob. pulled out of:
;;; :COURTESY Stefan Reichor, stefan@xsteve.at :HIS xsteve-functions.el  
;;; :NOT-WORKING-AS-OF
;;; :CREATED <Timestamp: Tuesday February 17, 2009 @ 04:53.44 PM - by MON KEY>
;;; ==============================
;; (defun mon-query-remove-doubled-words (&optional force)
;;   "Find all doubled words and ask to remove them.
;; With optional arg FORCE remove them without asking."
;;   (interactive "P")
;;   (let ((case-fold-search t)
;; 	(del-counter 0))
;;     (while (re-search-forward
;; 	    "\\(\\<\\w\\{3,\\}\\>\\)[ \t\n]*\\(\\1\\)" nil t)
;;       (replace-highlight (match-beginning 2) (match-end 2))
;;       (unwind-protect
;; 	  (when (or force (y-or-n-p "Remove this doubled word? "))
;; 	    (delete-region (match-beginning 2) (match-end 2))
;; 	    (canonically-space-region (match-beginning 0) (match-end 0))
;; 	    (setq del-counter (1+ del-counter)))
;; 	(replace-dehighlight)))
;;     (if (> del-counter 0)
;; 	(message "Removed %d doubled %s." del-counter
;; 		 (if (< del-counter 1) "words" "word"))
;;       (message "No doubled words found or removed."))))
;;; =======================

;;; ==============================
;;; :RANDOM UID's
;;; ==============================

;;; ==============================
;;; :COURTESY :FILE fns.c `next_almost_prime'
;;; :CREATED <Timestamp: #{2010-08-05T16:09:48-04:00Z}#{10314} - by MON>
(defun mon-next-almost-prime (w-integer)
  "From W-INTEGER return either a prime or a number nearer the next prime.\n
Return a value I where (>= I w-integer) and (>= W-INTEGER 0).\n
Useful for generating a reasonable arg for `make-hash-table's :size keyword.\n
:EXAMPLE\n\n\(let* \(\(prms-lt-100 '\(2 3 5 7 11 13 17 19 23 29 31 37 41 
                     43 47 53 59 61 67 71 73 79 83 89 97\)\)
       \(n->100 \(number-sequence 1 99 1\)\)
       next-almost\)
  \(mapc #'\(lambda \(nx-ap\)
            \(let \(\(non-prime \(unless \(memq nx-ap prms-lt-100\)
                               nx-ap\)\)\)
              \(when non-prime 
                \(push `\(,nx-ap . ,\(mon-next-almost-prime non-prime\)\) 
                      next-almost\)\)\)\)
        n->100\)
  \(setq next-almost \(nreverse next-almost\)\)\)\n
\(let \(\(n->100 \(number-sequence 100 300 1\)\)
      next-almost\)
  \(mapc #'\(lambda \(nx-ap\)
            \(push `\(,nx-ap . ,\(mon-next-almost-prime nx-ap\)\) 
                      next-almost\)\) 
        n->100\)
  \(setq next-almost \(nreverse next-almost\)\)\)\n
:NOTE The original C definition:\n
  ,----
  |  int
  |  next_almost_prime (int n)
  |  {
  |    if (n % 2 == 0)
  |      n += 1;
  |    if (n % 3 == 0)
  |      n += 2;
  |    if (n % 7 == 0)
  |      n += 4;
  |    return n;
  |  }
  `----\n
:ALIASED-BY `mon-get-next-almost-prime'
:ALIASED-BY `next-almost-prime'\n
:SEE-ALSO `mon-gensym-counter-randomizer', `mon-make-random-state',
`mon-generate-prand-seed', `mon-generate-WPA-key', `mon-generate-prand-id',
`mon-string-wonkify', `random'.\n►►►"
  (let ((napN w-integer))
    (when (= (% napN 2) 0) (setq napN (1+ napN)))
    (when (= (% napN 3) 0) (setq napN (+ napN 2)))
    (when (= (% napN 7) 0) (setq napN (+ napN 4)))
    ;; (when (= (% napN 5) 0) (setq napN (+ (- napN 2) 3)))
    ;; (when (= (% napN 2) 0) (setq napN 
    ;;                              (mon-next-almost-prime napN)))
  napN))
;;
(defalias 'mon-get-next-almost-prime 'mon-next-almost-prime)
;;
(unless (and (intern-soft "next-almost-prime")
             (fboundp 'next-almost-prime))
  (defalias 'next-almost-prime 'mon-next-almost-prime))

;;; ==============================
;;; :CHANGESET 2043
;;; :CREATED <Timestamp: #{2010-08-04T22:08:00-04:00Z}#{10313} - by MON KEY>
(defun mon-make-random-state ()
  "Return an integer with some randomness.\n
:EXAMPLE\n\n\(mon-make-random-state\)\n
:SEE-ALSO `mon-next-almost-prime', `mon-gensym-counter-randomizer', `mon-make-random-state',
`mon-generate-prand-seed', `mon-generate-WPA-key', `mon-generate-prand-id',
`mon-string-wonkify', `random'.\n►►►"
  (let ((mmrst8 
         (lsh (+ cons-cells-consed floats-consed vector-cells-consed symbols-consed
                 string-chars-consed misc-objects-consed intervals-consed
                 strings-consed
                 pure-bytes-used
                 num-nonmacro-input-events
                 (floor (* gcs-done (sqrt gcs-done)) 1)
                 (floor gc-elapsed 1))
              (aref (mon-nshuffle-vector [17 13 5 19 11 7 3 23]) 0))))
    (logxor
     (if (not (zerop mmrst8)) mmrst8 (mon-make-random-state))
     (random))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-13T17:40:20-04:00Z}#{09422} - by MON>
(defun mon-generate-prand-id (&optional cnt)
  "Return a pseudo-rand UID.
Return value is a 40 char hex string generated as sha1 sum from seed
`mon-generate-prand-seed'. When > CNT 1 return N UID's.\n
:EXAMPLE\n(mon-generate-prand-id 6)\n
:NOTE Only the first sum has random qualities.
      Subsequent sha1 sums are taken from sum calculated in previous iteration.
      Thus, if CNT is 4 then the sha1 of sum1 -> sum2 -> sum3 -> sum4.\n
This means:\n
 a) the return value of all elts after car are _not_ random at all;\n
 b) where UID assignment occurs in parallel with time-stamping we can infer
    when the UID was generated relative the index of previous/subsequent elts.
    This is a Featured-Bug®.\n
:SEE-ALSO `mon-string-to-hex-string', `mon-generate-WPA-key', `mon-string-wonkify',
`url-hexify-string', `url-unhex-string', `url-unhex'.\n►►►"
  (eval-when-compile (require 'sha1))
  (let ((gthr)
        (ccnt (if cnt cnt 1)))
    (do* ((i 1 (1+ i))
          (j (sha1 (mon-generate-prand-seed)) (sha1 (car gthr)))
          (k (push j gthr) (push j gthr)))
        ((>= i ccnt) k))
    (nreverse gthr)))
;;;
;;; :TEST-ME 
;;; (save-excursion 
;;;   (newline) 
;;;   (dolist (i (mon-generate-prand-id 1000))
;;;     (newline) (prin1 i (current-buffer))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-12T15:07:02-04:00Z}#{09421} - by MON KEY>
(defun mon-generate-prand-seed ()
  "Generate a seed for 'unique random identifier' a 32 character hex string.
Seed is only pseudo random/unique but it will suffice for our needs.
Don't call this function in a loop it won't work b/c TIME is slow as hell.
Instead, use as a seed for `mon-generate-prand-id'.
On MON system min. 0.85 seconds is needed between calls to produce unique id's.\n
:EXAMPLE\n(mon-generate-prand-id)\n
\(let \(\(i 11\) \(k\)\)
  \(while \(/= i 0\)
    \(sleep-for 0.85\)
    \(setq k \(cons `\(,\(mon-generate-prand-id\)\) k\)\)
    \(setq i \(1- i\)\)\)
\(prin1 k\)\)\n
:SEE-ALSO `mon-generate-WPA-key', `mon-string-wonkify', `mon-list-nshuffle',
`mon-nshuffle-vector', `url-cache-create-filename-using-md5'.\n►►►"
  ;(eval-when-compile (require 'cookie1))
  (let* ((pseudo-r #'(lambda () 
                       (mon-string-to-sequence 
                        (number-to-string (abs (random t))))))
         (seq->v #'(lambda (x) (apply 'vector x)))
         (shufv #'(lambda (x) 
                    ;; :WAS (shuffle-vector x))))                    
                    (mon-nshuffle-vector x))))
    (md5    
     (mon-string-from-sequence
      (funcall shufv
               (funcall seq->v    
                        (mon-string-to-sequence
                         (md5
                          (mon-string-from-sequence
                           (funcall shufv
                                    (vconcat
                                     (funcall seq->v
                                              (mon-string-to-sequence
                                               (md5 
                                                (mon-string-from-sequence
                                                 (funcall shufv 
                                                          (funcall seq->v  
                                                                   (nreverse  
                                                                    (funcall pseudo-r))))))))
                                     (funcall shufv 
                                              (funcall seq->v 
                                                       (funcall pseudo-r))))))))))))))
;;;
;;; :TEST-ME (mon-generate-prand-seed)
;;; :TEST-ME (length (mon-generate-prand-seed))
;;; :TEST-ME (let ((i 11) (k))
;;;               (while (/= i 0)
;;;                 (sleep-for 0.85)
;;;                 (setq k (cons `(,(mon-generate-prand-seed)) k))
;;;                 (setq i (1- i)))
;;;               (prin1 k))

;;; ==============================
;;; :NOTE Used in :FILE mon-site-local-defaults.el 
;;; This function shadows that symbol so it can be compiled.
;;; :CREATED <Timestamp: #{2010-02-10T19:47:57-05:00Z}#{10064} - by MON KEY>
(defun mon-string-wonkify (wonk-words wonkify-n-times)
  "Wonkify the string WONK-WORDS.\n
:EXAMPLE\n\n\(mon-string-wonkify \"These are some wonky words\" 10\)\n
\(mon-string-wonkify \"These are some wonky words\" 3\)\n
:SEE-ALSO `mon-zippify-region', `mon-generate-prand-seed', `mon-generate-prand-id',
`mon-generate-WPA-key', `mon-nshuffle-vector', `mon-list-nshuffle'\n►►►" 
  ;; (eval-when-compile (require 'cookie1)) for `shuffle-vector'
  (let* ((wonkify wonk-words)
        (wonk-usr #'(lambda (l eo) 
                      (let (new-round)
                        (dolist (u l)
                          (let ((U u))
                            (dotimes (i (random (length U)))
                              (let ((rnd (random (length U))))
                                (setf (nth rnd U)
                                      (if eo (upcase (nth rnd U)) (downcase (nth rnd U))))))
                            (push (apply 'string U) new-round)))
                        (setq wonkify new-round))))
        (seqify #'(lambda (q)
                    (let (reseq)
                      (mapc #'(lambda (s) (push (string-to-list s) reseq)) 
                            (cond ((listp q) q)
                                  ((stringp q) (list q))))
                      reseq)))
        (lcl-gcd #'(lambda (&rest lcl-gcd-args)
                     (let ((a (abs (or (pop lcl-gcd-args) 0))))
                       (while lcl-gcd-args
                         (let ((b (abs (pop lcl-gcd-args))))
                           (while (> b 0) (setq b (% a (setq a b))))))
                       a))))
    (setq wonkify (make-list wonkify-n-times (car (funcall seqify  wonkify))))
    (do ((w wonkify-n-times))
         ((< w 0)  wonkify)
      (setq w (1- w))
      (setq wonkify (apply 'vector wonkify))
      (setq wonkify (mon-nshuffle-vector wonkify)) ;;(shuffle-vector wonkify))
      (setq wonkify (append wonkify nil))
      (when (stringp (car wonkify))
        (setq wonkify (funcall seqify wonkify)))
      ;; :WAS (funcall wonk-usr wonkify (if (eq (gcd w 2) 2) t)))))
      ;; (funcall wonk-usr wonkify (if (eq (cl::gcd w 2) 2) t)))))
      (funcall wonk-usr wonkify 
               (if (eq (apply lcl-gcd `(,w 2)) 2) t)))))

;;  :)
(defalias 'mon-generate-wonky 'mon-string-wonkify)
;;
;;; :TEST-ME (mon-string-wonkify "These are some wonky words" 10)
;;; :TEST-ME (mon-string-wonkify "These are some wonky words" 3)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-10T13:17:24-04:00Z}#{10146} - by MON>
(defun mon-string-to-hex-list-cln-chars (rep-str)
  "Return list of hex chars in REP-STR with chars: `,' `00' `\\' removed.\n
:EXAMPLE\n\n\(mon-string-to-hex-list-cln-chars
 \"72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\\ \"\)\n
\(mon-hex-list-as-string
 \(mon-string-to-hex-list-cln-chars 
  \"72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\\ \"\)\)\n
:NOTE Intended usage is for quick conversion of registry keys e.g.:\n
 [HKEY_CLASSES_ROOT\\InternetShortcut\\shell\\printto\\command]
 @=hex(2):72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\\
 { ... }
 22,00,00,00\n
:SEE-ALSO `mon-string-to-hex-string', `mon-string-from-hex-list',
`mon-string-to-hex-list', `url-hexify-string', `url-unhex-string',
`url-unhex'.\n►►►"
  (car (read-from-string 
        (format "(%s)" (replace-regexp-in-string "00\\|[,\]" " " rep-str)))))
;;
;;: :TEST-ME (mon-string-to-hex-list-cln-chars 
;;;              "72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\ ")
;;; :TEST-ME(mon-hex-list-as-string
;;;             (mon-string-to-hex-list-cln-chars 
;;;              "72,00,75,00,6e,00,64,00,6c,00,6c,00,33,00,32,00,2e,00,65,00,78,00,65,\ "))
 
;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-06T17:41:33-05:00Z}#{09455} - by MON>
(defun* mon-string-to-hex-string (&key hxify-str w-dlim prand-hex-len)
  "Return HXIFY-STR as a string of hex numbers.  When keyword W-DLIM is non-nil
delimit hex numbers w-dlim.  When keyword PRAND-HEX-LEN (a number >= 80 ) is
non-nil, return a pseudo-random string of length N generated with
`mon-generate-prand-id'. Useful for generating throw-away WPA keys.\n
:EXAMPLE\n\(mon-string-to-hex-string :hxify-str \"bubba\"\)
\(mon-string-to-hex-string :hxify-str \"bubba\" :w-dlim \":\"\)
\(mon-string-to-hex-string :hxify-str \"bubba\" :w-dlim \" \"\)
\(mon-string-to-hex-string :prand-hex-len 64\)
\(mon-string-to-hex-string :prand-hex-len 81\) ;<-Should Fail.\n
:NOTE :HEXADECIMAL 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
      :DECIMAL     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15\n
:SEE-ALSO `mon-string-from-hex-list', `mon-string-to-hex-list',
`mon-string-to-hex-list-cln-chars', `mon-generate-WPA-key',
`mon-generate-prand-seed', `mon-string-replace-char',
`hexl-hex-string-to-integer', `url-hexify-string', `url-unhex-string',
`url-unhex'.\n►►►"
  (let (msths)
    (unless prand-hex-len
      ;; :NOTE Consider using (append hxify-str nil) instead of the mapping.
      (mapc #'(lambda (x) (setq msths (cons x msths)))  hxify-str)
      (setq msths (reverse msths))
      (setq msths
            (mapconcat #'(lambda (x) (format "%x" x))
                       msths (if (and w-dlim (stringp w-dlim)) w-dlim ""))))
    (when prand-hex-len 
      (if (<= prand-hex-len 80)
          (setq msths
                (substring 
                 (concat (car (mon-generate-prand-id))
                         (car (mon-generate-prand-id))) 0 prand-hex-len))
        (error (concat ":FUCTION `mon-string-to-hex-string' "
                       "-- %s is too large or not a number") prand-hex-len)))
    msths))
;;
;;; :TEST-ME (mon-string-to-hex-string :hxify-str "bubba")
;;; :TEST-ME (mon-string-to-hex-string :hxify-str "bubba" :w-dlim ":")
;;; :TEST-ME (mon-string-to-hex-string :hxify-str "bubba" :w-dlim " ")
;;; :TEST-ME (mon-string-to-hex-string :prand-hex-len 64)
;;; :TEST-ME (mon-string-to-hex-string :prand-hex-len 81) ;Should Fail.

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-07T14:50:16-05:00Z}#{09456} - by MON>
(eval-when-compile (require 'hexl)) ;; `hexl-hex-string-to-integer'a
(defun mon-string-from-hex-list (hx-l)
  "Return HX-l \(a list of hex chars) as a string.\n
Useful for working with w32 registry keys of type REG_BINARY.
:EXAMPLE\n\(mon-hex-list-as-string 
 '(43 00 3a 00 5c 00 50 00 72 00 6f 00 67 00 72 00 61 00 6d 00 20 00 46 00 69 00
 6c 00 65 00 73 00 5c 00 74 00 65 00 78 00 6c 00 69 00 76 00 65 00 5c 00 32 00
 30 00 30 00 38 00 5c 00 62 00 69 00 6e 00 5c 00 77 00 69 00 6e 00 33 00 32 00
 5c 00 70 00 61 00 74 00 67 00 65 00 6e 00 2e 00 65 00 78 00 65 \)\)\n
\(mon-hex-list-as-string
 \(split-string \(mon-string-to-hex-string :hxify-str \"bubba\" :w-dlim \":\"\) \":\" t\)\)\n
:NOTE :HEXADECIMAL 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
      :DECIMAL     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
:SEE-ALSO `mon-string-to-hex-list', `mon-string-to-hex-string',
`mon-string-to-hex-list-cln-chars', `hexl-hex-string-to-integer',
`url-hexify-string', `url-unhex-string', `url-unhex'.\n►►►"
  (let (hex-key-as-strings hex-key-as-int)
    (mapc #'(lambda (hk-s) (push (format "%s" hk-s) hex-key-as-strings))
          hx-l)
    (mapc #'(lambda (hs-i) (push (hexl-hex-string-to-integer hs-i) hex-key-as-int))
          hex-key-as-strings)
    (mon-string-from-sequence hex-key-as-int)))
;;
(defalias 'mon-hex-list-as-string 'mon-string-from-hex-list)
;;
;;; :TEST-ME 
;;; (mon-string-from-hex-list
;;;  (split-string 
;;;   (mon-string-to-hex-string :hxify-str "bubba" :w-dlim ":")
;;;   ":" t))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-30T19:41:27-05:00Z}#{10047} - by MON KEY>
(defun mon-string-to-hex-list (string-hexify)
  "Return string as a list of hex values.\n
:NOTE Can roundtrip the output of `mon-string-to-hex-list'.\n
:EXAMPLE\n\(mon-string-to-hex-list \"bùbbä_◄\\\"\t\\\"►mô'búbbá\"\)\n
\(mon-string-from-hex-list 
  \'(62 f9 62 62 e4 5f 25c4 22 9 22 25ba 6d f4 27 62 fa 62 62 e1\)\)\n
\(mon-string-from-hex-list 
  \(mon-string-to-hex-list \"bùbbä_◄\\\"\t\\\"►mô'búbbá\"\)\)\n
:NOTE :HEXADECIMAL 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
      :DECIMAL     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15\n
:SEE-ALSO `mon-string-from-hex-list', `mon-generate-prand-id',
`mon-string-to-hex-list-cln-chars', `mon-string-to-hex-string',
`hexl-hex-string-to-integer', `url-hexify-string', `url-unhex-string',
`url-unhex'.\n►►►"
  (let (hexamification)
    (mapc #'(lambda (hx)
              (let ((car-char (car (read-from-string (format "%x" hx)))))
                (push car-char hexamification)))
          (if (stringp string-hexify)
              (append string-hexify nil)
            (error (concat ":FUNCTION `mon-string-to-hex-string' "
                           " -- arg STRING-HEXIFY not a string"))))
    (setq hexamification (nreverse hexamification))))
;; 
;;; :TEST-ME (mon-string-to-hex-list "bubba\x09mo'bubba")
;;; :TEST-ME (mon-string-from-hex-list (mon-string-to-hex-list "bubba"))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-06T17:49:43-05:00Z}#{09455} - by MON KEY>
(defun mon-generate-WPA-key (&optional insrtp intrp)
  "Return a 64 char pseudo-random hex-string.
When INSRTP is non-nil or called-interactively insert string at point.
Does not move point.\n
:EXAMPLE\n(mon-generate-WPA-key)\n
:SEE-ALSO `mon-generate-prand-id', `mon-generate-prand-seed',
`mon-string-wonkify', `mon-string-from-hex-list',
`mon-string-to-hex-list', `mon-string-to-hex-string'.\n►►►"
  (interactive "i\np")
  (let ((wk (mon-string-to-hex-string :prand-hex-len 64)))
    (if (or insrtp intrp)
        (save-excursion (princ (format " \"%s\" " wk) (current-buffer)))
        wk)))
;;
;;; :TEST-ME (mon-generate-WPA-key)
;;; :TEST-ME (call-interactively 'mon-generate-WPA-key)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-21T14:27:09-04:00Z}#{09433} - by MON KEY>
(defun mon-sha1-region (start end &optional insrtp intrp)
  "Return the sha1sum for contents of region.\n
When INSRTP is non-nil or called-interactively insert sha1 on newline.
Does not move point.\n
:SEE-ALSO `sha1-region', `sha1-string'.\n►►►."
  (interactive "r\ni\np")
  (eval-when-compile (require 'sha1))
  (let ((sha1-r (sha1-region start end)))
    ;; (sha1-string (buffer-substring-no-properties start end))))
    (if (or insrtp intrp)
        (save-excursion (newline)(princ sha1-r (current-buffer)))
        sha1-r)))


;;; ==============================
;;; :CHANGESET 2064
;;; :CREATED <Timestamp: #{2010-08-13T19:53:24-04:00Z}#{10325} - by MON KEY>
(defun mon-bool-vector-pp (bool-vec)
"Return print representation of bool-vector BOOL-VECT in various formats.\n
When BOOL-VECT length is greater than 29 return value has the format:\n
 \(:gray-table [101 ... 010] :true-table [t t ... nil t] :bool-vector BOOL-VEC \)\n
When BOOL-VECT length is less than 29 in addition to the values above return
value contains the following key value pairs:\n
 ( :binary \"#b10 ... 01\" :decimal 123 ... 9 :octal \"#o7 ... 77\" :hex \"#xFF...00\" 
   :gray-table [101 ... 010] :true-table [t t ... nil t] :bool-vector BOOL-VEC \)\n
:EXAMPLE\n\n\(setq tt--bv \(make-bool-vector 28 t\)\)\n
 ;=> #&29\"\\377\\377\\377\x1f\"\n
\(vconcat tt--bv\)\n
 ;=> [t t t t t t t t t t t t t t t t t t t t t t t t t t t t t]\n
\(dolist \(tgl \(number-sequence 1 29 3\) tt--bv\)
    \(aset tt--bv tgl nil\)\)\n
 ;=> #&29\"m\\333\\266\xd\"\n
\(vconcat tt--bv\)\n
 ;=> [t nil t t nil t t nil t t nil t t nil t t nil t t nil t t nil t t nil t t nil]
 ;       1       4       7        10     13     16       19       22     25      28\n
 \(mon-bool-vector-pp tt--bv\)\n
 ;=> \(:binary \"#b10110110110110110110110110110\"
      :decimal 383479222
      :octal  \"#o2666666666\"
      :hex    \"#x16db6db6\"
      :bin-table [1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0]
      :true-table [t nil t t nil t t nil t t nil t t nil
                    t t nil t t nil t t nil t t nil t t nil]
      :bool-vector #&29\"m\\333\\266\xd\"\)\n
;; Beyond the 29th bit\n
\(setq tt--bv \(make-bool-vector 31 t\)\)\n
 ;=> #&31\"\\377\\377\\377\x7f\"\n
\(dolist \(tgl \(number-sequence 1 30 3\) tt--bv\)
   \(aset tt--bv tgl nil\)\)\n
 ;=> #&31\"m\\333\\266m\"\n
\(mon-bool-vector-pp tt--bv\)\n
 ;=> \(:bintable  [1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1]
       :true-table [t nil t t nil t t nil t t nil t t nil t
                   t nil t t nil t t nil t t nil t t nil t t]
      :bool-vector #&31\"m\\333\\266m\"\)\n
 \(unintern 'tt--bv\)\n
:ALIASED-BY `mon-bool-vector-to-list'\n
:NOTE Assumes return value of `byteorder' is 108 \(big end first\).\n
:SEE-ALSO `mon-get-bit-table', `*mon-bit-table*', `mon-help-char-raw-bytes',
`mon-help-binary-representation'.\n►►►"
 (if (= (length bool-vec) 0)
     (error (concat 
             ":FUNCTION `mon-bool-vector-pp' "
             "-- arg BOOL-VEC has length 0 Emacs won't fetch that index, IHMO an Emacs bug"))
   (let* ((bv-len (length bool-vec))
          (w-props (<= bv-len 29))
          ;; vector of numberic 0's
          (to-bin (make-vector bv-len 0))
          ;; Make a string array of Os (char 48) -> "000 {...} 000"                 
          (to-bin-str (when w-props (make-string bv-len 48))))
     (dotimes (blv bv-len)
       ;; If current element bool-vector is `t'
       (when (aref bool-vec blv)
         ;; Set string char "0" at idx to "1"
         (when w-props (aset to-bin-str blv 49))
         ;; Set numeric 0 at vector idx to numeric 1
         (aset to-bin blv 1)))
     (if w-props
         (let ( ;; read the decimal numeric value of var to-bin-str "#b10{...}01"
               ;; (string-to-number "11111111111111111111111111111" 2)
               ;; :WAS (bin-str-dec (read to-bin-str)))
               (bin-str-dec (string-to-number to-bin-str 2)))
           ;; Make a binary number string "#b10{...}01" for `read'
           ;; pad it out to the MSB (bit 29) w/ "0"s
           (when w-props 
             (setq to-bin-str 
                   (concat "#b" (make-string (- 29 bv-len) 48) to-bin-str)))
            
           `(:binary      ,to-bin-str
             :decimal     ,bin-str-dec
             :octal       ,(format "#o%o" bin-str-dec)
             :hex         ,(format "#x%x" bin-str-dec)
             :bin-table   ,to-bin
             :true-table  ,(vconcat bool-vec)
             :bool-vector ,bool-vec))

       `(:bin-table  ,to-bin 
         :true-table  ,(vconcat bool-vec)
         :bool-vector ,bool-vec)))))
;;
(defalias 'mon-bool-vector-to-list   'mon-bool-vector-pp)
;;
;;; :TEST-ME (progn (setq tt--bv (make-bool-vector 29 t)) (mon-bool-vector-pp tt--bv))
;;; :TEST-ME (progn (setq tt--bv (make-bool-vector 256 t)) (mon-bool-vector-pp tt--bv))
;;; :TEST-ME (progn (setq tt--bv (make-bool-vector 0 t)) (mon-bool-vector-pp tt--bv))

;;; ==============================
;;; :type '(plist :value-type  (boolean t nil)) ;;(choice :tag "Yes, bind this variable at loadtime"  t "No, do not bind" nil)
;;; :CHANGESET 2064
;;; :CREATED <Timestamp: #{2010-08-16T20:10:59-04:00Z}#{10331} - by MON KEY>
(defvar *mon-bit-table* nil
  "Variable caching the results of `mon-get-bit-table'.\n
:EXAMPLE\n\n\(mapcar #'\(lambda \(urng\) 
            \(memq :max-unsigned urng\)\) *mon-bit-table*\)\n
:SEE-ALSO `mon-bool-vector-pp', `mon-help-binary-representation',
`mon-help-char-raw-bytes'.\n►►►")

;;; ==============================
;;; :CHANGESET 2064
;;; :CREATED <Timestamp: #{2010-08-16T20:11:02-04:00Z}#{10331} - by MON KEY>
(defun mon-get-bit-table (&optional dsplyp intrp)
  "Return a list of bit values for bits 1-29.\n
When optional arg DSPLYP is non nil or called-interactively display results in
buffer named \"*MON-BIT-TABLE*\".\n
Return a list with the format:\n
 \(:bit-<N> :bit-weight N :2^ N :max-signed N :max-unsigned \(N . N\)\)\n
The values of these keys map as follows:\n
:bit-<N>       The bit identity \(0 indexed\)
:byte          The byte of the bit.
:2^            The bits value as a power of 2.
:bit-dec       The decimal value of bit at index.
:bit-oct       The octal value of bit at index.
:bit-hex       The hex value of bit at index.
:max-int       The maximimun signed number representable by bit.
:max-uint      A cons bounding the maximum unsigned range representable by bit.\n
:EXAMPLE\n\n\(assq :bit-28 \(mon-get-bit-table\)\)\n
\(memq :max-uint \(assq :bit-29 \(mon-get-bit-table\)\)\)\n
\(mon-get-bit-table t\)\n
:NOTE The first time this function is called it caches the results of its
evaluation to the variable `*mon-bit-table*'.\n
:ALIASED-BY `mon-byte-table-bits'
:ALIASED-BY `mon-bit-table-bits'\n
:SEE-ALSO `mon-bool-vector-pp' `mon-help-binary-representation', `mon-help-char-raw-bytes'.\n►►►"
  (interactive "i\np")
  (let ((gthr (when (bound-and-true-p *mon-bit-table*)
                *mon-bit-table*)))
    (when (null gthr)
      (dotimes (i 29 (progn (setq gthr (nreverse gthr))
                            (setq *mon-bit-table* gthr)))
        (let* ((nxt-i (1+ i))
               (bky       (car (read-from-string (format ":bit-%d" nxt-i))))
               (bit-wgt   (expt 2 i))
               (byt-wgt   (if (eq (% nxt-i 8) 0)
                              (/ nxt-i 8)
                            (1+ (/ (- nxt-i (% nxt-i 8)) 8))))
               (oct-wgt (make-symbol (format "#o%o" bit-wgt)))
               (hex-wgt (make-symbol (format "#x%X" bit-wgt)))
               ;; What no CL `reduce' at compile time w/out a
               ;; byte-compile-warning?  Thanks Emacs for protecting my
               ;; namespace... After all its not like `reduce' isn't stupid
               ;; fukcking usefull!!! Goddamn how I loathe thee CL runtime ban.
               ;; :WAS (mx-sgn (+ bit-wgt (reduce '+ gthr :key 'caddr)))
               ;; 
               (mx-sgn  (apply #'+ bit-wgt (mapcar #'(lambda (fk-rdc) (nth 6 fk-rdc)) gthr)))
               (unsgn-bot (/ (lognot mx-sgn) 2))
               (unsgn-top (lognot unsgn-bot)))
          (push `(,bky :byte ,byt-wgt :2^ ,i 
                       :bit-weight ,bit-wgt :bit-oct ,oct-wgt :bit-hex ,hex-wgt 
                       :max-int ,mx-sgn :max-uint (,unsgn-bot . ,unsgn-top)) gthr))))
    (when (or intrp dsplyp)
      (with-current-buffer 
          (get-buffer-create (upcase (symbol-name '*mon-bit-table*)))
        (erase-buffer)
        (save-excursion 
          (princ gthr (current-buffer))
          (newline))
        (down-list)
        (ignore-errors (while (forward-list) (newline)))
        (save-excursion
          (goto-char (buffer-end 0))
          (forward-list)
          (backward-char 2)
          (delete-char 1))
        (emacs-lisp-mode)
        (display-buffer (current-buffer) t)))
    gthr))
;;
;; :NOTE This is a bad name but its hard to fit it in with my other mnemonics :(
(defalias 'mon-byte-table-bits 'mon-get-bit-table)
(defalias 'mon-bit-table-bits  'mon-get-bit-table)
;;
;;; :TEST-ME (progn (makunbound '*mon-bit-table*) (mon-get-bit-table t))
;;; :TEST-ME (assq :bit-29 (mon-get-bit-table))
;;; :TEST-ME (memq :max-unsigned (assq :bit-29 (mon-get-bit-table)))

;; (
;;; ==============================
;;; :RECTANGLE-RELATED-FUNCTIONS
;;; ==============================

;;; ==============================
;;; :BUG #1184 of Thu, 16 Oct 2008 19:45:02 UTC
;;; "document how to deal with beer belly rectangles"
;;; :SEE (URL `http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=1184')
;;; :TODO Currently only handles situations where point is at column 0.\n
;;; :CREATED <Timestamp: #{2010-01-10T21:26:28-05:00Z}#{10017} - by MON>
(defun mon-kill-rectangle-w-beer-belly (belly-start belly-end)
  "Like kill-rectangle but adds trailing whitespace when column at mark is less
than the longest line in rectangle.\n
Does not handle situations where point is not at column 0.\n
The
following
is a rectangle-w-beer-belly.
The paragraph you are reading is a
potential rectangle. It is a PITA for the 
`kill-rectangle' command because it is hard to
put point and mark around being in that it is fat
in the middle. Thus, while it is easy to mark
its left side, how are you going to mark its
right upper or lower corner without
altering the buffer to add spaces
in order to get the cursor there?\n
:SEE-ALSO `mon-rectangle-apply-on-region-points', `mon-rectangle-capitalize',
`mon-rectangle-columns', `mon-rectangle-downcase', `mon-rectangle-operate-on',
`mon-rectangle-sum-column', `mon-rectangle-upcase', `mon-line-length-max',
`mon-line-indent-from-to-col', `mon-line-strings-indent-to-col'.\n►►►"
  (interactive "r\n")
  (let ((max-len 0)
        (fat-belly))
    (unwind-protect
         (narrow-to-region belly-start belly-end)
      (goto-char (point-min))
      (while (eq (forward-line) 0)
        (end-of-line)
        (when (> (current-column) max-len)
          (setq max-len (current-column))))
      (when (= (current-column) max-len)
        (setq fat-belly t)
        (kill-rectangle belly-start belly-end))
      (unless fat-belly
        (setq fat-belly 
              (buffer-substring-no-properties belly-start belly-end))
        (goto-char belly-start)
        (kill-line)
        (while (eq (forward-line) 0) (kill-line))
        (when (stringp fat-belly)
          (with-temp-buffer 
            (insert fat-belly)
            (goto-char (point-min))
            (while (eq (forward-line) 0)
              (let ((lebp `(,(line-beginning-position) . ,(line-end-position))))
                (unless (= (- (car lebp) (cdr lebp)) max-len)
                  (end-of-line) 
                  (insert (make-string (- max-len (- (cdr lebp) (car lebp))) 32)))))
            (kill-rectangle (buffer-end 0) (buffer-end 1)))))
      (widen))))

;;; ==============================
;;; :CREATED <Timestamp: Friday June 05, 2009 @ 07:03.00 PM - by MON KEY>
(defun mon-rectangle-columns (start end)
  "Return column positions at START and END.\n
Mostly useful as a code template for rectangle related functions.\n
:SEE-ALSO `mon-rectangle-sum-column'.\n►►►"
  (interactive "r")
  (let ((col-s (make-marker))
	(col-e (make-marker))
	(cols))
    (save-excursion
      (goto-char start)
      (set-marker col-s (point))
      (goto-char end)
      (set-marker col-e (point)))
    (setq cols    `(,(car (nth 6 (posn-at-point (marker-position col-s))))
		    ,(car (nth 6 (posn-at-point (marker-position col-e))))))
    cols))

;;; ==============================
;;; :COURTESY Alex Schroeder
;;; :MODIFICATIONS Charlie Hethcoat <- Improved number regex.
;;; :MODIFICATIONS <Timestamp: #{2010-03-09T14:47:59-05:00Z}#{10102} - by MON KEY>
;;; Now dumps to temp-buffer. Added optional arg INTRP.
(defun mon-rectangle-sum-column (start end &optional intrp)
  "Add all integer, decimal, and floating-point numbers in selected rectangle.\n
Numbers which can be read include (nonexhaustive):\n
 2 +2 -2 2. +2. -2. 2.0 +2.0 -2.0 2e0 +2e0 -2e0 2E0 2e+0 2e-0, 2.e0, 2.0e0, etc.\n
:SEE-ALSO `mon-rectangle-columns', `mon-string-incr-padded',
`mon-line-number-region', `mon-string-incr', `mon-line-number-region-incr'.\n►►►"
  (interactive "r\np")
  (let ((rec-sumd 0))
    (save-excursion
      (kill-rectangle start end)
      (exchange-point-and-mark)
      (yank-rectangle)
      ;; :WAS (set-buffer (get-buffer-create "*calc-sum*")) 
      ;;        (erase-buffer) (yank-rectangle) (exchange-point-and-mark)
      (with-temp-buffer
        (save-excursion (yank-rectangle))
        (while (re-search-forward
                "[-+]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?"
                nil t)
          (setq rec-sumd (+ rec-sumd (string-to-number (match-string 0))))))
      (if intrp
          (message "Sum: %f" rec-sumd)
          rec-sumd))))

;;; ==============================
;;; :NOTE Functions for modifying buffer contents or display.
;;; Brings in `operation-on-rectangle' for the old-school holmessss.
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el 
;;; :WAS `operate-on-rectangle' -> `apply-on-rectangle' -> `mon-rectangle-operate-on'
;;; ==============================
(defun mon-rectangle-operate-on (function start end &rest args)
  "Call FUNCTION for each line of rectangle with corners at START, END.
FUNCTION is called with two arguments: the start and end columns of the
rectangle, plus ARGS extra arguments.  Point is at the beginning of line when
the function is called.\n
:SEE `apply-on-rectangle' in :FILE rect.el
:SEE-ALSO `mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
`mon-rectangle-downcase', `mon-rectangle-upcase', `mon-rectangle-capitalize'.\n►►►"
  (let (startcol startpt endcol endpt)
    (save-excursion
      (goto-char start)
      (setq startcol (current-column))
      (beginning-of-line)
      (setq startpt (point))
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endpt (point-marker))
      ;; Ensure the start column is the left one.
      (if (< endcol startcol)
	  (let ((col startcol))
	    (setq startcol endcol endcol col)))
      ;; Start looping over lines.
      (goto-char startpt)
      (while (< (point) endpt)
	(apply function startcol endcol args)
	(forward-line 1)))))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-apply-on-region-points (fun start end &rest args)
  "Like `apply-on-rectangle', but pass points in the buffer instead of columns.\n
:SEE-ALSO`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points',
`mon-rectangle-downcase' `mon-rectangle-upcase' `mon-rectangle-capitalize'.\n►►►"
  (mon-rectangle-operate-on
   (lambda (bcol ecol)
     (apply fun
            (progn
              (move-to-column bcol 'coerce)
              (point))
            (progn
              (move-to-column ecol 'coerce)
              (prog1
                  (point)
                (beginning-of-line)))
            args))
   start end))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-downcase (beg end)
  "Convert the marked rectangle to lower case.\n
:SEE-ALSO `mon-rectangle-upcase', `mon-rectangle-capitalize',
`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points'.\n►►►"
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'downcase-region beg end))

;;; ==============================
;;; :COURTESY Noah Friedman <friedman@splode.com> :HIS buffer-fns.el
(defun mon-rectangle-upcase (beg end)
  "Convert the marked rectangle to upper case.
:SEE-ALSO `mon-rectangle-downcase' ,`mon-rectangle-operate-on',
`mon-rectangle-apply-on-region-points', `rect.el'."
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'upcase-region beg end))

;;; ==============================
(defun mon-rectangle-capitalize (beg end)
  "Convert the marked rectangle to Title case.\n
:SEE-ALSO `mon-rectangle-downcase', `mon-rectangle-upcase', `mon-rectangle-capitalize'
`mon-rectangle-operate-on', `mon-rectangle-apply-on-region-points'.\n►►►"
  (interactive "r")
  (mon-rectangle-apply-on-region-points 'mon-region-capitalize beg end))

;;; ==============================
;;; :TEXT-PROPERTIES
;;; ==============================

;; ;;; ==============================
;; ;;; :RENAMED `mon-kill-ring-save-w-props' -> `mon-get-text-properties-region-to-kill-ring'
;; ;;; :CREATED: <Timestamp: #{2009-10-11T08:44:59-04:00Z}#{09417} - by MON KEY>
;; (defun mon-get-text-properties-region-to-kill-ring (start end &optional no-strip)
;;   "Copy region _with_ text-properties to kill-ring.\n
;; If a leading `#' is present in string strip it.\n
;; When NO-STRIP in non-nil or called-interactively with prefix arg do not strip
;; leading `#'.\n
;; :NOTE Function is yank-handler agnostic w/re to 2nd optional arg of `kill-new'.\n
;; :ALIASED-BY `mon-get-text-properties-region->kill-ring'
;; :ALIASED-BY `mon-kill-ring-save-w-props'\n
;; :SEE-ALSO `mon-get-text-properties-region',`mon-line-test-content',
;; `mon-list-all-properties-in-buffer', `mon-nuke-text-properties-buffer',
;; `mon-nuke-text-properties-region', `mon-remove-text-property',
;; `mon-remove-single-text-property', `mon-help-text-property-functions',
;; `mon-help-text-property-function-ext'.\n►►►"
;;   (interactive "r\nP")
;;   (let (get-str) 
;;     (setq get-str (format "%S" (buffer-substring start end)))
;;     (kill-new 
;;      (substring 
;;       get-str 
;;       (if no-strip 
;;           0
;;           (if (= (string-match "^#" get-str) 0)
;;               1 
;;               0))))))
;; ;;
;; (defalias 'mon-get-text-properties-region->kill-ring 'mon-get-text-properties-region-to-kill-ring)
;; ;;
;; (defalias 'mon-kill-ring-save-w-props 'mon-get-text-properties-region-to-kill-ring)

;;; ==============================
;;; :CHANGESET 1974
;;; :CREATED <Timestamp: #{2010-07-13T14:21:34-04:00Z}#{10282} - by MON KEY>
(defun mon-get-syntax-class-at (psn)
  "Return `syntax-class' at PSN.\
This is just a combination of `syntax-after' and `syntax-class'.\n
 (logand (syntax-class (char-syntax (char-after (point))))
:EXAMPLE\n\n(mon-get-syntax-class-at (1+ (point))) (\n
:SEE-ALSO `syntax-after', `syntax-class', `string-to-syntax', `char-syntax'
`mon-help-syntax-class', `mon-help-syntax-functions'.\n►►►"
  (let ((after-syn 
         (progn 
           (unless (or (< psn (point-min)) (>= psn (point-max)))
             (let ((st (if parse-sexp-lookup-properties
                           (get-char-property psn 'syntax-table))))
               (if (consp st) st
                 (aref (or st (syntax-table)) (char-after psn))))))))
    (and after-syn (logand (car after-syn) 65535))))
;;
;; This is neat:
;; (lsh 1 17) => 131072 
;; (logior 131072 255) => 131327  
;; (- 131327 131072) => 255 (#o377, #xff)

;;; ==============================
;;; :CREATED <Timestamp: Monday May 11, 2009 @ 05:07.49 PM - by MON KEY>
(defun mon-line-test-content (syn-sym &optional rtrn-as-list)
  "Examine Syntax Location of SYN-SYM from point.\n
When syntax SYN-SYM is t advances point to end of syntax.
Return a formatted string describing syntax locations.
SYN-SYM arg is a symbol of type: 'word 'whitespace or 'punctuation.
When RTRN-AS-LIST is non-nil returns as list.\n
:EXAMPLE\n
\(mon-line-test-content 'word)word =>
\"[line:413 word:word word-start:20267 word-end:20271]\"\n
\(mon-line-test-content 'word t\)word => 
\(413 word \"word\" 20269 20273\)
\(car cadr caddr cadddr cddddr\)
 line type found satart end\n
\(if \(> \(skip-syntax-forward \"^-\"\) 0\)
     \(mon-line-test-content 'whitespace t\)\)word more-word\n
:NOTE Function relies on current buffers local syntax table.\n
:SEE-ALSO `mon-get-syntax-class-at', `mon-get-text-properties-category', `mon-view-help-source',
`mon-help-syntax-class', `mon-help-syntax-functions'.\n►►►"
  (let* ((syntax-type (cond 
		       ((eq syn-sym 'word) 'word)
		       ((eq syn-sym 'whitespace) 'whitespace)
		       ((eq syn-sym 'punctuation) 'punctuation)))
	 (syntax (cond 
		  ((eq syntax-type 'word)  '(syntax-type "\w" "word-start:" "word-end:"))
		  ((eq syntax-type 'whitespace) '(syntax-type "-" "spc-start:" "spc-end:"))
		  ((eq syntax-type 'punctuation) '(syntax-type "." "punct-start:" "punct-end:"))))
	 (start-of (caddr syntax))
	 (end-of (cadddr syntax))
	 (starting) (ending) (tlc-marker) (next))
    (setq tlc-marker (point-marker))
    (setq next (skip-syntax-forward (cadr syntax)))
    (let* ((lnap (line-number-at-pos))
	   (range-start (marker-position tlc-marker))
	   (range-end (point))
	   (at-here (buffer-substring-no-properties range-start range-end))
	   (syn-match at-here)
	   (syn-is (cond	   
		    ((and (eq syntax-type 'whitespace) (eq (string-match " " syn-match) 0)) t)
		    ((and (eq syntax-type 'whitespace) (not (eq (string-match " " syn-match) 0))) nil)
	   	    ;; not testing for numbers add another case if thats whats wanted
	   	    ((and (eq syntax-type 'word) (eq (string-match "[[:alpha:]]" syn-match) 0)) t)
	   	    ((and (eq syntax-type 'word) (not (eq (string-match "[[:alpha:]]" syn-match) 0))) nil)
	   	    ((and (eq syntax-type 'punctuation) (eq (string-match "[[:punct:]]" syn-match) 0)) t)
	   	    ((and (eq syntax-type 'punctuation) (not (eq (string-match "[[:punct:]]" syn-match) 0))) nil)))
	   (result-loc (cond 
			((and syn-is (eq syntax-type 'word)) ;test word
			 (format "[line:%d %s:%s %s%d %s%d]" 
				 lnap syntax-type syn-match start-of range-start end-of range-end))
			((and (not syn-is) (eq syntax-type 'word))
			 (format "[line:%d %s:_no_ %s%d %s%d]"
				 lnap syntax-type  start-of  range-start  end-of range-end))
			((and syn-is (eq syntax-type 'whitespace)) ;test whitespace
			 (format "[line:%d %s:_yes_ %s%d %s%d]" 
				 lnap syntax-type start-of range-start end-of range-end))
			((and (not syn-is) (eq syntax-type 'whitespace))
			 (format "[line:%d %s:_no_ %s%d %s%d]"
				 lnap syntax-type start-of  range-start  end-of range-end))
			((and syn-is (eq syntax-type 'punctuation)) ;test punctuation
			 (format "[line:%d %s:%s %s%d %s%d]" 
				 lnap syntax-type syn-match start-of range-start end-of range-end))
			((and (not syn-is) (eq syntax-type 'punctuation))
			 (format "[line:%d %s:_no_ %s%d %s%d]"
				 lnap syntax-type start-of  range-start  end-of range-end))))
	   (result-location (cond 
			     ((and syn-is (eq syntax-type 'word)) ;test word
			      `(,lnap ,syntax-type ,syn-match ,range-start ,range-end))
			     ((and (not syn-is) (eq syntax-type 'word))
			      `(,lnap ,syntax-type nil ,range-start ,range-end))
			     ((and syn-is (eq syntax-type 'whitespace))	;test whitespace
			      `(,lnap ,syntax-type ,syn-match ,range-start ,range-end))
			     ((and (not syn-is) (eq syntax-type 'whitespace))
			      `(,lnap ,syntax-type nil ,range-start ,range-end))
			     ((and syn-is (eq syntax-type 'punctuation)) ;test punctuation
			      `(,lnap ,syntax-type ,syn-match ,range-start ,range-end))
			     ((and (not syn-is) (eq syntax-type 'punctuation))
			      `(,lnap ,syntax-type nil ,range-start ,range-end)))))
      (if rtrn-as-list
	  result-location
	result-loc))))
;;
;;; :TEST-ME (mon-line-test-content 'word t)this-word
;;; :TEST-ME (mon-line-test-content 'word)this-word
;;; :TEST-ME (mon-line-test-content 'word) this-word
;;; :TEST-ME (mon-line-test-content 'word t) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace t) this-word
;;; :TEST-ME (mon-line-test-content 'whitespace)this-word
;;; :TEST-ME (mon-line-test-content 'whitespace t)this-word
;;; :TEST-ME (mon-line-test-content 'punctuation t),this-word
;;; :TEST-ME (mon-line-test-content 'punctuation),this-word
;;; :TEST-ME (mon-line-test-content 'punctuation t)this-word
;;; :TEST-ME (mon-line-test-content 'punctuation),his-word
;;; :TEST-ME (car (mon-line-test-content 'word t))word
;;; :TEST-ME (car (nthcdr 1 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 2 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 3 (mon-line-test-content 'word t)))word
;;; :TEST-ME (car (nthcdr 4 (mon-line-test-content 'word t)))word
;;; :TEST-ME (mon-line-test-content 'word)word => "[line:413 word:word word-start:20267 word-end:20271]"
;;; :TEST-ME (mon-line-test-content 'word t)word => (413 word "word" 20269 20273)
;;; :TEST-ME (car cadr caddr cadddr cddddr)
;;; :TEST-ME (if (> (skip-syntax-forward "^-") 0) (mon-line-test-content 'whitespace t))word more-word

;;; ==============================
;; (defun mon-get-text-properties-category ()
;;   "Test for a category text-property.\n
;; Helper function for `mon-view-help-source'\n
;; :SEE-ALSO `mon-get-text-properties-category', `mon-view-help-source',
;; `mon-line-test-content'.\n►►►"
;;   (let* ((to-view ((lambda () (text-properties-at (point)))))
;; 	 (my-props `(,@to-view))
;; 	 (prop-value (plist-get my-props 'category)))
;;     prop-value))
;;
(defun mon-view-help-source ()
  "
:SEE-ALSO `mon-get-text-properties-category', `mon-line-test-content'.\n►►►"
  (interactive)
  (eval-when-compile (require 'ffap))
  (unwind-protect			;body
       (let ((gb))
         (if (or (equal (buffer-name)(help-buffer))
                 (string= "*Help*" (buffer-name)))
             (save-window-excursion
               (goto-char (point-min))
               (while (not (eobp))      ;
                 (let ((this-change)
                       (next-change
                        (or (next-property-change (point) (current-buffer))
                            (point-max))))
                   (progn
                     (goto-char next-change)
                     (setq this-change (mon-get-text-properties-category))
                     (when (and
                            (and (string= this-change 'help-function-def-button))
                            (and (ffap-file-at-point)))
                       (let* ((f-to-get-fl (ffap-file-at-point)))
                         (view-file-other-window (ffap-file-at-point))
                         (setq gb (find-buffer-visiting f-to-get-fl)))
                       gb)))))))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-16T17:19:31-05:00Z}#{10026} - by MON KEY>
(defun mon-plist-remove! (symbol property-indicator)
  "Remove from SYMBOL's plist the PROPERTY-INDICATOR and its value.\n
Like `cl-remprop' and CL's `remprop' but without the latter's `remf'.\n
:EXAMPLE\n\n\(let \(the-pl\) 
  \(setplist the-pl \(mon-alphabet-as-type 'plistD->num\)\)
  \(dolist \(p-rmv 
            \(subseq 
             \(mapcar 'car \(mon-alphabet-as-type 'cons-keyD->num\)\)
             0 8\)
           \(symbol-plist the-pl\)\)
    \(mon-plist-remove! the-pl p-rmv\)\)\)\n
:SEE-ALSO `mon-plist-remove-if',`mon-plist-keys', `mon-plist-remove-consing',
`remf', `remprop'.\n►►►"
  (let* ((CLDOREMF
          #'(lambda  (PLST TAG)
              (let ((p (cdr PLST)))
                (while (and (cdr p) (not (eq (car (cdr p)) TAG))) 
                  (setq p (cdr (cdr p))))
                (and (cdr p) (progn (setcdr p (cdr (cdr (cdr p)))) t)))))
         (CLREMPROP 
          #'(lambda (sym tag)
              (let ((plst (symbol-plist sym)))
                (if (and plst (eq tag (car plst)))
                    (progn (setplist sym (cdr (cdr plst))) t)
                    (funcall CLDOREMF plst tag))))))
    (funcall CLREMPROP symbol property-indicator)))
    
;;;(mon-plist-remove! (mon-alphabet-as-type 'plistD->num) :l)
;;
;;; :TEST-ME 
;;; (let (the-pl) 
;;;   (setplist the-pl (mon-alphabet-as-type 'plistD->num))
;;;   (dolist (p-rmv 
;;;             (subseq 
;;;              (mapcar 'car (mon-alphabet-as-type 'cons-keyD->num))
;;;              0 8)
;;;            (symbol-plist the-pl))
;;;     (mon-plist-remove! the-pl p-rmv)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon 
;;; :HIS common-lisp/list.lisp :VERSION 2008-06-24 :WAS `PLIST-REMOVE'
;;; :NOTE I lifted this one from a Common Lisp file without realizing it would
;;; cause problems w/ Emacs lisp b/c `remf' is a macro intended for plists as
;;; generalized variable. Emacs compiler nags b/c `remf' calls `cl-do-remf' at
;;; runtime. As such, we have commented this one out, however, we leave it here
;;; as a reminder to be on the watch for the demons of hubris... Who in their
;;; right mind is shadowing the symbol `cl-do-remf'?  Come on, enough already,
;;; these types of compiler 'Warnings' don't protect the user's namespace!!!!
;;; :CREATED <Timestamp: #{2009-09-28T17:32:55-04:00Z}#{09401} - by MON>
;;
;; (defun mon-plist-remf (plist prop)
;;   "Return PLIST with PROP removed using `remf'.\n
;; :EXAMPLE (mon-plist-remf \(mon-alphabet-as-type 'plistD->num\) :l)
;; :SEE-ALSO `mon-plist-remove', `mon-plist-keys'.\n►►►"
;;   (remf plist prop) plist)
;;
;;; :TEST-ME (mon-plist-remf (mon-alphabet-as-type 'plistD->num) :l)

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS `plist-remove'
;;; :CREATED <Timestamp: #{2010-01-16T16:55:29-05:00Z}#{10026} - by MON KEY>
(defun mon-plist-remove-consing (plist key)
  "Return a new plist with the each element of PLIST but the one with KEY.
:NOTE A suffix in result may be a suffix of plist too.\n
:EXAMPLE\n\n(mon-plist-remove-consing (mon-alphabet-as-type 'plistD->num) :l)\n
:SEE-ALSO `mon-plist-remove!', `mon-plist-remove-if',
`mon-plist-remove-consing', `remf', `remprop', `mon-plist-keys',
`mon-help-plist-functions', `mon-help-plist-properties'.\n►►►"
  (if (eq (car plist) key)
      (cdr (cdr plist))
    (cons (car plist) (cons (cadr plist) 
                            (mon-plist-remove-consing (cddr plist) key)))))
;;
;;; :TEST-ME (mon-plist-remove-consing (mon-alphabet-as-type 'plistD->num) :l)

;;; ==============================
;;; :NOTE Inspired by Pascal Bourguignon's Common Lisp implementation of 
;;; `PLIST-REMOVE' above. Following doesn't call `remf' and handles predicates.
;;; :CREATED <Timestamp: #{2010-01-13T15:41:30-05:00Z}#{10023} - by MON KEY>
(defun mon-plist-remove-if (plist prop &optional plist-pred with-debug)
  "Return PLIST with PROP removed.\n
By default comparison for PROP is made `eq' as it is with CL's `remprop'.\n
When optional arg PLIST-PRED is either `eql' or `equal' the plist property
comparison is made with that predicate and `memql' or `member' counterparts.\n
If optional arg WITH-DEBUG is non-nil output as with `message' when 
return value is `equal' the initial arg given for PLIST.\n
:EXAMPLE\n\n\(mon-plist-remove-if  
 \(mon-plist-remove-if 
  \(mon-plist-remove-if \(mon-alphabet-as-type 'plistD->num\) :f\)
  :a 'eql\)
 :c 'equal\)\n
\(mon-plist-remove-if '\(this list-a that listb\) 'that\)\n
\(mon-plist-remove-if '\(this list-a that listb\) 'this 'eql\)\n
\(mon-plist-remove-if '\(this list-a that listb\) 'nope\)\n
\(mon-plist-remove-if '\(this list-a that listb\) 'nope 'eql t\)\n
\(mon-plist-remove-if '\(this list-a that listb\) \"this\"\)\n
\(mon-plist-remove-if '\(\"this\" list-a \"that\" list-b\) \"this\"\)\n
\(mon-plist-remove-if '\(\"this\" list-a \"that\" list-b\) \"this\" nil t\)\n
\(mon-plist-remove-if '\(\"this\" list-a \"that\" list-b\) \"this\" 'equal\)\n
\(mon-plist-remove-if '\(\"this\" list-a \"that\" list-b\) \"this\" 'eql t\)\n
:SEE-ALSO `mon-plist-remove!', `mon-plist-remove-consing', `mon-plist-keys',
`remf', `remprop', `mon-help-plist-functions', `mon-help-plist-properties'.\n►►►"
  (let* ((pl plist)
         (pred
          (if plist-pred 
              (cond ((eq plist-pred 'eql) `((memql prop pl)  (eql p prop)))
                    ((eq plist-pred 'equal) `((member prop pl)  (equal p prop)))
                    (t `((memq prop pl)  (eq p prop))))
              `((memq prop pl)  (eq p prop))))
         (idx  (eval (car pred)))
         nw)
    (if (and idx (funcall (caadr pred) (car idx) (car pl))) ;;(eval `(,(caadr pred) (car idx) (car pl))))
        (if (> (length pl) 2)
            (progn
              (dotimes (l 2 pl) (pop pl))
              (setq nw pl))
            nw)
        (if idx
            (progn
              (dotimes (i 2) (pop idx))
              (while pl 
                (funcall #'(lambda (p)
                             (if (eval (cadr pred))
                              ;;(funcall (caadr pred) (cadadr pred) (car (cddadr pred)))
                              ;;(eval `(,(caadr pred) ,(cadadr pred) ,(car (cddadr pred))))
                              (setq pl nil)
                              (push p nw))) 
                         (pop pl)))
              (setq nw (nconc (nreverse nw) idx)))
            (setq nw pl))) ;;(error "%S is not a property in %S" prop plist)
    (if (equal pl nw)
        (progn
          (when (or with-debug (eq (caadr pred) 'eq))
            (message "%S `equal' original PLIST when using PLIST-PRED predicates `%s' and `%s'" 
                     plist (caadr pred) (caar pred)))
          nw)
        nw)))
;;
;;; :TEST-ME (mon-plist-remove-if '(this list-a that listb) "this")
;;; :TEST-ME (mon-plist-remove-if '("this" list-a "that" list-b) "this")
;;; :TEST-ME (mon-plist-remove-if '("this" list-a "that" list-b) "this" nil t)
;;; :TEST-ME (mon-plist-remove-if '("this" list-a "that" list-b) "this" 'equal)
;;; :TEST-ME (mon-plist-remove-if '("this" list-a "that" list-b) "this" 'eql t)
;;; :TEST-ME (mon-plist-remove-if '(this list-a that listb) 'this 'eql)
;;; :TEST-ME (mon-plist-remove-if '(this list-a that listb) 'nope 'eql t)
;;; :TEST-ME (mon-plist-remove-if '(this list-a that listb) 'this 'eql)
;;; :TEST-ME (mon-plist-remove-if '(this list-a that listb) 'nope)
;;; :TEST-ME (mon-plist-remove-if '(this list-a that listb) 'that)
;;; :TEST-ME (mon-plist-remove-if (mon-alphabet-as-type 'plistD->num) :l)
;;; :TEST-ME (mon-plist-remove-if 
;;;           (mon-plist-remove-if 
;;;            (mon-plist-remove-if (mon-alphabet-as-type 'plistD->num) :l) 
;;;            :a 'eql)
;;;           :t 'equal)

;; ;;; ==============================
;; ;;; :NOTE Keep with `mon-list-all-properties-in-buffer'.
;; ;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;; ;;; `mon-nuke-text-properties-buffer'
;; (defun mon-plist-keys (plist)
;;   "Cons up a plist of keys with PLIST.\n
;; :EXAMPLE\n\(mon-plist-keys \(mon-alphabet-as-type 'plistD->num\)\)\n
;; :SEE-ALSO `mon-plist-remove', `mon-help-plist-functions',
;; `mon-map-obarray-symbol-plist-props', `mon-plist-remove-if',
;; `mon-plist-remove-consing', `remf', `remprop'.\n►►►"
;;   (if (null plist)
;;       plist
;;       (cons (car plist) (mon-plist-keys (cddr plist)))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-05-27T20:09:25-04:00Z}#{10214} - by MON KEY>
(defun mon-map-obarray-symbol-plist-props (plist-sym &optional display-in-buffer)
  "Map atoms in obarray looking for the plist property PLIST-SYM.\n
Return the list of symbols with plist property PLIST-SYM.\n
When optional arg DISPLAY-IN-BUFFER is non-nil return values to buffer named
\"*OBARRY-PLIST-SYM-MATCHES*\".\n
:EXAMPLE\n\n\(mon-map-obarray-symbol-plist-props 'permanent-local\)\n
\(mon-map-obarray-symbol-plist-props 'permanent-local t\)\n
:SEE-ALSO `mon-plist-remove', `mon-map-obarray-symbol-plist-props',
`mon-plist-remove-if', `mon-plist-remove-consing', `remf', `remprop',
`mon-help-plist-functions', `mon-help-plist-properties'.\n►►►"
  (let ((mmospp (when display-in-buffer 
                  (get-buffer-create              
                   "*OBARRY-PLIST-SYM-MATCHES*"))))
    (when display-in-buffer (with-current-buffer mmospp (erase-buffer)))
    (mapatoms #'(lambda (sym) 
                  (when (plist-get (symbol-plist sym)  plist-sym)
                    (if display-in-buffer
                        (progn
                          ;;(princ (identity sym)  (get-buffer mmospp))
                          (princ (format "%s\n" (identity sym))
                                 (get-buffer mmospp)))
                          ;;(princ "\n" (get-buffer mmospp)))
                      (push sym mmospp)))))
    (if display-in-buffer 
        (with-current-buffer mmospp
          (sort-lines nil (buffer-end 0)(buffer-end 1))
          (mon-line-strings-bq-qt-sym-bol (buffer-end 0) (1- (buffer-end 1)) t)
          (let ((comment-start ";;")
                (comment-style 'indent))
            (comment-region (buffer-end 0)(buffer-end 1)))
          (emacs-lisp-mode)
          (goto-char (buffer-end 0))
          (insert (format ";; (mon-map-obarray-symbol-plist-props '%S)" plist-sym))
          (display-buffer mmospp t))
      mmospp)))
;;
;;; :TEST-ME (mon-map-obarray-symbol-plist-props 'permanent-local)
;;; :TEST-ME (mon-map-obarray-symbol-plist-props 'permanent-local t)


;;; ==============================
;;; :NOTE Fashioned after `with-help-window's `list-of-window-tuples' :FILE help.el
;;; :CHANGESET 2101
;;; :CREATED <Timestamp: #{2010-08-31T19:04:24-04:00Z}#{10352} - by MON KEY>
(defun mon-map-windows->plist ()
  "Return a list of plist's mapping window properties of windows on all frames.\n
Does not return for mini-buffers.\n
plist elements of returned list have the format:\n
 \(:window                 <WINDOW>
  :window-frame           <FRAME>
  :window-buffer          <BUFFER>
  :window-buffer-name     <BUFFER-NAME>
  :window-buffer-visiting <FILE-NAME> 
  :window-point           <INTEGER>
  :window-start           <INTEGER>
  :window-end             <INTEGER>
  :window-point-col-row \(:posn-col <INTEGER> 
                         :posn-row <INTEGER>\)
  :window-edges \(:left   <INTEGER> 
                 :top    <INTEGER>
                 :right  <INTEGER>
                 :bottom <INTEGER>\)
  :window-point-char-width-height \(:char-width  <INTEGER>
                                   :char-height <INTEGER>\)\)\n
:EXAMPLE\n
\(mapcar #'\(lambda \(pl\) \(plist-get pl :window-buffer-name\)\) \(mon-map-windows->plist\)\)\n
\(mapcar #'\(lambda \(pl\) \(plist-get pl :window\)\) \(mon-map-windows->plist\)\)\n
\(mapcar #'\(lambda \(pl\) \(plist-get pl :window-point\)\) \(mon-map-windows->plist\)\)\n
:ALIASED-BY `mon-get-window-plist'
:SEE-ALSO `mon-plist-keys', `mon-map-obarray-symbol-plist-props', 
`mon-list-all-properties-in-buffer', `mon-help-window-functions',
`mon-help-plist-functions', `mon-help-plist-properties'.\n►►►"
  (let (wdo-l)
    (walk-windows
     #'(lambda (window)
         (let* ((wdo         window)
                (wdo-pnt     (window-point wdo))
                (wdo-dbind   
                 (destructuring-bind (dbwdo psn-area psn-area-cns tsmp obj 
                                            pos pos-col/row img img-x/y w/h)
                     (posn-at-point wdo-pnt wdo)
                   (list :window dbwdo
                         :window-frame (window-frame   dbwdo)
                         :window-buffer (window-buffer dbwdo)
                         :window-buffer-name (buffer-name (window-buffer dbwdo))
                         :window-buffer-visiting (buffer-file-name (window-buffer dbwdo))
                         :window-point pos
                         :window-start (window-start dbwdo)    
                         :window-end   (window-end   dbwdo)
                         :window-point-col-row 
                         `(:posn-col ,(car pos-col/row) :posn-row ,(cdr pos-col/row))
                         :window-edges 
                         (destructuring-bind (lft top rgt btm) (window-edges dbwdo)
                           (list :left lft :top top :right rgt :bottom btm))
                         ;; psn-area psn-area-cns ;; tsmp ;; obj ;; img img-x/y
                         :window-point-char-width-height 
                         `(:char-width ,(car w/h) :char-height ,(cdr w/h))
                         ))))
           (push wdo-dbind wdo-l)))
     'no-mini t)
    wdo-l))
;;
(unless (and (intern-soft "mon-get-window-plist")
             (fboundp 'mon-get-window-plist))
  (defalias 'mon-get-window-plist 'mon-map-windows->plist))

;; ;;; ==============================
;; ;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;; ;;; :NOTE Keep with `mon-nuke-text-properties-buffer', `mon-plist-keys'
;; ;;; :CHANGED `set-buffer' -> `with-current-buffer' 
;; ;;; :CHANGED `delete-duplicates' -> `delete-dups'
;; ;;; :ADDED (&optional start-range end-range buffer) :WAS (buffer)
;; ;;; :MODIFICATIONS <Timestamp: #{2010-01-11T23:39:18-05:00Z}#{10021} - by MON KEY>
;; (defun mon-list-all-properties-in-buffer (&optional start-range end-range buffer)
;;   "List text-properties in current-buffer.\n
;; When BUFFER is non-nil list its text-properties instead.\n
;; :EXAMPLE\n(mon-list-all-properties-in-buffer)\n
;; :SEE-ALSO `mon-nuke-text-properties-buffer', `mon-plist-keys',
;; `mon-plist-remove', `mon-help-plist-functions',
;; `mon-help-text-property-functions'.\n►►►"
;;   (save-excursion
;;     (with-current-buffer 
;;         (if buffer (get-buffer buffer) (current-buffer))      
;;       ;; :NOTE Don't remove commented `delete-duplicates' version below.
;;       ;;      `delete-dups' uses `equal' maybe we want to use CL features later.
;;       ;; :WAS (delete-duplicates 
;;       ;;       (loop for i 
;;       ;;        from (or start-range (point-min)) 
;;       ;;        to (or end-range (point-max))
;;       ;;       (delete-duplicates (mon-plist-keys (text-properties-at i nil))))))))
;;       ;;; (delete-duplicates 
;;       ;;;  (loop for i 
;;       ;;;     from (or start-range (point-min)) 
;;       ;;;     to (or end-range (point-max))
;;       ;;;     nconc (delete-duplicates (mon-plist-keys (text-properties-at i nil))))))))      
;;       (delete-dups
;;       (loop for i from (or start-range (point-min)) to (or end-range (point-max))
;;          nconc (delete-dups (mon-plist-keys (text-properties-at i nil))))))))
;; ;;
;; ;;; ==============================
;; ;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el
;; ;;; :NOTE Keep with `mon-list-all-properties-in-buffer', `mon-plist-keys'
;; (defun mon-nuke-text-properties-buffer ()
;; "Remove text-properites in buffer.\n
;; :SEE-ALSO `mon-remove-text-property', `mon-remove-single-text-property', 
;; `mon-nuke-text-properties-region', `mon-help-text-property-functions',
;; `mon-nuke-overlay-buffer'.\n►►►"
;;   (interactive)
;;   (remove-list-of-text-properties (point-min) (point-max)
;;    (mon-list-all-properties-in-buffer)));; (buffer-name (current-buffer)))))

;; ;;; ==============================
;; ;;; :NOTE First Implemented to zap linger `color-occur' overlays when occur buffer
;; ;;;  is killed with kill-buffer as `color-occur-remove-overlays' doesn't get invoked.
;; ;;; :CREATED <Timestamp: #{2010-02-09T11:11:52-05:00Z}#{10062} - by MON KEY>
;; (defun mon-nuke-overlay-buffer (overlay-prop overlay-val)
;;   "Remove all overlay props with OVERLAY-NAME and OVERLAY-VAL in current-buffer.\n
;; :EXAMPLE\n\n\(unwind-protect
;;      \(let* \(\(sb \(save-excursion \(search-forward-regexp \"^►.*◄$\"  nil t\)\)\)
;;             \(molay \(make-overlay \(match-beginning 0\) \(match-end 0\) \(current-buffer\)\)\)\)
;;        \(overlay-put molay 'face 'minibuffer-prompt\)
;;        \(sit-for 2\)\)
;;   \(mon-nuke-overlay-buffer 'face 'minibuffer-prompt\)\)\n
;; ►I'm lingering◄\n
;; :SEE-ALSO `mon-help-find-result-for-overlay', `mon-help-overlay-for-example',
;; `mon-help-overlay-on-region', `mon-help-overlay-result',
;; `mon-nuke-overlay-buffer'.\n►►►"
;;  (remove-overlays (buffer-end 0) (buffer-end 1) overlay-prop overlay-val))
;;
;;; `mon-nuke-overlay-buffer'
;;; :TODO add a hook using to `kill-buffer-hook' or some such that removes the 
;;; `color-occur-face' correctly e.g.
;;;  (mon-nuke-overlay-buffer 'face 'color-occur-face)

;; ;;; ==============================
;; ;;; :COURTESY  ../emacs/lisp/font-lock.el 
;; ;;; :NOTE For completeness: this is to `remove-text-properties' as
;; ;;; `put-text-property' ; is to `add-text-properties', etc. Included therein but
;; ;;; commented out by SM as 'Additional text property functions' these may
;; ;;; eventually become C builtins.
;; ;;; For consistency: maybe this should be called `remove-single-property' like
;; ;;; `next-single-property-change' (not `next-single-text-property-change'), etc.
;; ;;; :WAS `remove-text-property'        -> ../emacs/lisp/font-lock.el
;; (defun mon-remove-text-property (start end property &optional object)
;;   "Remove a property from text from START to END.\n
;; Argument PROPERTY is the property to remove.\n
;; Optional argument OBJECT is the string or buffer containing the text.\n
;; Return t if the property was actually removed, nil otherwise.\n
;; :SEE-ALSO `mon-remove-single-text-property', `remove-text-properties',
;; `mon-nuke-text-properties-region', `add-text-properties', `put-text-property',
;; `next-single-property-change', `mon-list-all-properties-in-buffer',
;; `mon-nuke-overlay-buffer', `mon-help-text-property-functions', 
;; `mon-help-text-property-functions-ext'.\n►►►"
;;   (remove-text-properties start end (list property) object))
;; ;;
;; ;;; :WAS `remove-single-text-property' -> ../emacs/lisp/font-lock.el
;; (defun mon-remove-single-text-property (start end prop value &optional object)
;;  "Remove a specific property value from text from START to END.\n
;; Arguments PROP and VALUE specify the property and value to remove.\n
;; The resulting property values are not equal to VALUE nor lists containing VALUE.\n
;; Optional argument OBJECT is the string or buffer containing the text.\n
;; :SEE-ALSO `remove-text-property', `mon-nuke-text-properties-region',
;; `mon-nuke-overlay-buffer', `add-text-properties', `put-text-property',
;; `next-single-property-change', `mon-list-all-properties-in-buffer',
;; `mon-help-text-property-functions', `mon-help-text-property-functions-ext'.\n►►►"
;;  (let ((start (text-property-not-all start end prop nil object)) next prev)
;;    (while start
;;      (setq next (next-single-property-change start prop object end)
;; 	    prev (get-text-property start prop object))
;;      (cond ((and (symbolp prev) (eq value prev))
;; 	     (mon-remove-text-property start next prop object))
;; 	    ((and (listp prev) (memq value prev))
;; 	     (let ((new (delq value prev)))
;; 	       (cond ((null new)
;; 		      (mon-remove-text-property start next prop object))
;; 		     ((= (length new) 1)
;; 		      (put-text-property start next prop (car new) object))
;; 		     (t
;; 		      (put-text-property start next prop new object))))))
;;      (setq start (text-property-not-all next end prop nil object)))))

;; ;;; ==============================
;; ;;; :COURTESY Noah Friedman :HIS buffer-fns.el
;; (defun mon-nuke-text-properties-region (beg end)
;;   "Eliminate all text properties in current buffer from BEG to END.\n
;; :NOTE Only removes text properties, does not remove overlays.\n
;; :SEE-ALSO `remove-text-property', `mon-remove-single-text-property',
;; `mon-nuke-overlay-buffer', `add-text-properties', `put-text-property',
;; `next-single-property-change', `mon-list-all-properties-in-buffer',
;; `mon-help-text-property-functions'.\n►►►"
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region beg end)
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (let ((inhibit-read-only t)
;;               (plist (text-properties-at (point)))
;;               (next-change (or (next-property-change (point) (current-buffer))
;;                                (point-max))))
;;           (remove-text-properties (point) next-change plist (current-buffer))
;;           (goto-char next-change))))))


;;; ==============================
;;; :ELISP-RELATED
;;; ==============================

;;; ==============================
;;; :COURTESY Andy Stewart :HIS lazycat-toolkit.el
;;; :WAS insert-after -> `mon-elt->' 
;;; :WAS insert-before -> `mon-elt-<'
;;; :WAS list-set-elt -> `mon-elt->elt'
;;; :WAS list-exchange-els -> `mon-elt-<elt'
;;; :CREATED <Timestamp: Wednesday June 03, 2009 @ 06:31.33 PM - by MON KEY>
;;; ==============================
(defun mon-elt-> (list aft-el el)
  "Insert EL after AFT-EL in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-remove-if', `mon-mapcar'.\n►►►"
  (push el (cdr (member aft-el list)))
  list)
;;
(defun mon-elt-< (list bef-el el)
  "Insert EL before BEF-EL in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-remove-if', `mon-mapcar'.\n►►►"
  (nreverse (mon-elt-> (nreverse list) bef-el el)))
;;
(defun mon-elt->elt (list old-el new-el)
  "Set OLD-EL to NEW-EL in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-mapcar'.\n►►►"
  (setcar (member old-el list) new-el)
  list)
;;
(defun mon-elt-<elt (list el1 el2)
  "Exchange places of EL1 and EL2 in LIST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-remove-if', `mon-mapcar'.\n►►►"
  (when (or (null (member el1 list))
            (null (member el2 list)))
    (error ":FUNCTION `mon-elt-<elt' -- arg EL1 or EL2 not in LIST")))


;;; ==============================
;;; :COURTESY :FILE macroexp.el :WAS `maybe-cons'
;;; :CHANGESET 2017
;;; :CREATED <Timestamp: #{2010-07-31T16:27:59-04:00Z}#{10306} - by MON KEY>
(defun mon-maybe-cons (car cdr original-cons)
  "Return \(CAR . CDR\), using ORIGINAL-CONS if possible.\n
:EXAMPLE\n\n
:SEE-ALSO `mon-delq-cons', `mon-list-make-unique', `mon-list-match-tails',
`mon-list-reorder', `mon-list-proper-p', `mon-intersection', `mon-remove-if',
`mon-combine', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-mapcar'.\n►►►"
  (if (and (eq car (car original-cons)) 
           (eq cdr (cdr original-cons)))
      original-cons
    (cons car cdr)))

;;; ==============================
;;; :COURTESY :FILE lisp/format.el :WAS `format-delq-cons'
;;; :CHANGESET 2000 
;;; :CREATED <Timestamp: #{2010-07-27T16:38:27-04:00Z}#{10302} - by MON KEY>
(defun mon-delq-cons (w-cons w-list)
  "Remove the given CONS from LIST by side effect and return the new LIST.\n
:NOTE CONS may be the first elt of LIST, to ensure changing value of `foo' do:\n
 \(setq foo \(mon-delq-cons element foo\)\)\n
:SEE-ALSO `mon-delq-dups', `mon-remove-dups', `mon-remove-if',
`mon-list-make-unique', `mon-maybe-cons', `mon-list-match-tails',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle',
`mon-list-shuffle-safe', `mon-list-proper-p', `mon-intersection', `mon-combine',
`mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', .\n►►►"
  (if (eq w-cons w-list)
      (cdr w-list)
    (let ((p w-list))
      (while (not (eq (cdr p) w-cons))
	(if (null p) 
            (error (concat ":FUNCTION `mon-delq-cons' "
                         " -- cell of W-LIST not an element: %S") p))
	(setq p (cdr p)))
      ;; Now (cdr p) is the cons to delete
      (setcdr p (cdr w-cons))
      w-list)))

;;; ==============================
;;; :WAS `randomize'
;;; :COURTESY gene.ressler@gmail.com comp.lang.lisp 2010-08-01
(defun mon-list-nshuffle (mk-list-random)
  "Destructively permute a list in place by changing cdr's.\n
It provably generates all permutations with equal probability, is
probabilistically faster than quicksort on randomly ordered input, and
uses O(log n) space.\n
:EXAMPLE\n\n\(mon-list-nshuffle-TEST 1000\)\n
\(let \(\(froggy '\(\"went\" \"a\" \"courtin'\" \"and\" \"he\" \"did\" \"ride\"\)\)
      bubba\)
  \(setq bubba froggy\)
  \(setq froggy `\(,:froggy-shuffled ,\(mon-list-nshuffle froggy\) 
                 ,:froggy-clobbered ,bubba\)\)\)
:NOTE To to be sure of changing the value of froggy, setters should write:\n
 \(setq froggy \(mon-list-nshuffle froggy\)\)\n 
:SEE (URL `http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/230204ed6c092b6d#')
:SEE-ALSO `mon-list-shuffle-safe', `mon-nshuffle-vector', `mon-delq-dups',
`mon-remove-dups', `mon-remove-if', `mon-list-make-unique', `mon-maybe-cons',
`mon-list-match-tails', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe',`mon-list-proper-p',
`mon-intersection', `mon-combine', `mon-map-append', `mon-maptree',
`mon-mapcar',`mon-transpose', `mon-flatten', `mon-recursive-apply',
`mon-sublist', `mon-sublist-gutted', `mon-remove-dups', `mon-assoc-replace',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`slime-shuffle-list', `shuffle-vector'.\n►►►"
  (labels ((mrndz-f (mrndz-p mrndz-len mrndz-tl)
              (cond ((null mrndz-p) mrndz-tl)
                   (t (loop
                         with mrndz-ne = (random mrndz-len)
                         with mrndz-e = nil
                         with mrndz-n1 = 0
                         with mrndz-n2 = 0
                         with mrndz-l1 = nil
                         with mrndz-l2 = nil
                         with mrndz-nxt = nil
                         repeat mrndz-len
                         do
                           (setf mrndz-nxt (cdr mrndz-p))
                           (cond ((zerop mrndz-ne) (setf mrndz-e mrndz-p))
                                 (t
                                  (cond ((zerop (random 2))
                                         (setf (cdr mrndz-p) mrndz-l1)
                                          (setf mrndz-l1 mrndz-p)
                                         (incf mrndz-n1))
                                        (t
                                         (setf (cdr mrndz-p) mrndz-l2)
                                         (setf mrndz-l2 mrndz-p)
                                         (incf mrndz-n2)))))
                           (decf mrndz-ne)
                           (setf mrndz-p mrndz-nxt)
                         finally
                           (setf (cdr mrndz-e) mrndz-tl)
                           (return (if (> mrndz-n1 mrndz-n2)
                                       (mrndz-f mrndz-l1 mrndz-n1 (mrndz-f mrndz-l2 mrndz-n2 mrndz-e))
                                       (mrndz-f mrndz-l2 mrndz-n2 (mrndz-f mrndz-l1 mrndz-n1 mrndz-e)))))))))
    (mrndz-f mk-list-random (length mk-list-random) nil)))

;;; ==============================
;;; :COURTESY gene.ressler@gmail.com comp.lang.lisp 2010-08-01
;;; :CREATED <Timestamp: #{2010-08-03T18:29:33-04:00Z}#{10312} - by MON>
(defun mon-list-nshuffle-TEST (w-test-times)
  "Test function for `mon-list-nshuffle'\n
Return results of applying `mon-list-nshuffle'W-TEST-TIMES in buffer
named \"*MON-LIST-NSHUFFLE-TEST*\"\n
:EXAMPLE\n\n\(mon-list-nshuffle-TEST '\(a b c d\) 100\)\n
\(mon-list-nshuffle '\(\"a\" \"b\" \"c\" \"d\"\) 100\)\n
:SEE-ALSO `mon-nshuffle-vector', `mon-list-shuffle-safe'.\n►►►"
  (with-current-buffer 
      (get-buffer-create "*MON-LIST-NSHUFFLE-TEST*")
    (erase-buffer)
    (insert ";; :FUNCTION `mon-list-nshuffle-TEST'\n" 
            ";; :W-TEST-TIMES " (number-to-string w-test-times) "\n"
            (make-string 68 59) "\n"
            ";; Number of times each list occurred\n"
            ";; |RANDOMIZED-LIST|    |COUNT|\n\n")
    (loop with count-table = (make-hash-table :test #'equal); :size w-test-times)
          repeat w-test-times ;; repeat 1000000
          for perm = (mon-list-nshuffle (list 'a 'b 'c 'd))
          do (incf (gethash perm count-table 0))
          finally (loop
                   for perm being each hash-key in count-table
                   using (hash-value count)
                   ;; :WAS CL format string: 
                   ;; do (format t "~a: ~a~%" perm count))))
                   do (princ (format "      %s         ; %s\n" perm count) 
                             (current-buffer))))
    (goto-char (buffer-end 0))
    (display-buffer (current-buffer) t)))
;;
;;; :TEST-ME (mon-list-nshuffle-TEST 10000)
;;; :TEST-ME (mon-list-nshuffle-TEST 100)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-08-09T16:40:02-04:00Z}#{10321} - by MON>
(defun mon-list-shuffle-safe (list-to-shuffle)
  "Shuffle contents of LIST-TO-SHUFFLE non-destructively.\n
When LIST-TO-SHUFFLE is non-nil, not a `type-of' cons, or does not satisfy the
predicate `mon-list-proper-p' signal an error.\n
:EXAMPLE\n\n\(let \(\(tst-mlss '\(a \(b . c\) q\)\)\)
  `\(:w-shuffle ,\(mon-list-shuffle-safe tst-mlss\) :w/o-shuffle ,tst-mlss\)\)\n
;; :NOTE Following will fail:
\(mon-list-proper-p '\(a \(b . c\) . q\)\)\n
\(mon-list-shuffle-safe nil\)\n
\(mon-list-shuffle-safe '\(\)\)\n
\(mon-list-shuffle-safe \"won't shuffle\"\)\n
:SEE-ALSO `mon-list-nshuffle', `mon-nshuffle-vector', `mon-delq-dups',
`mon-remove-dups', `mon-remove-if', `mon-list-make-unique', `mon-maybe-cons',
`mon-list-match-tails', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe',`mon-list-proper-p',
`mon-intersection', `mon-combine', `mon-map-append', `mon-maptree',
`mon-mapcar', `mon-transpose', `mon-flatten', `mon-recursive-apply', `mon-sublist',
`mon-sublist-gutted', `mon-remove-dups', `mon-assoc-replace', `mon-moveq',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-list-nshuffle-TEST', `shuffle-vector', `slime-shuffle-list'.\n►►►"
  (let ((mlss-lst (copy-tree list-to-shuffle)))
    (if (not (and (eq (type-of mlss-lst) 'cons)
                  (mon-list-proper-p mlss-lst)))
        (error (concat ":FUNCTION `mon-list-shuffle-safe' "
                       "-- arg LIST-TO-SHUFFLE null or not `mon-list-proper-p'"))
      (setq mlss-lst (append (mon-nshuffle-vector (vconcat mlss-lst)) nil)))))
;;
;;; :TEST-ME (let ((tst-mlss '(a (b . c) q)))
;;;          `(:w-shffl ,(mon-list-shuffle-safe tst-mlss) :w/o-shffl ,tst-mlss))

;;; ==============================
;;; :CHANGESET 2035
;;; :CREATED <Timestamp: #{2010-08-04T21:51:44-04:00Z}#{10313} - by MON KEY>
(defun mon-delq-dups (dup-list)
  "Like `delete-dups' but destructively removes `eq' duplicates from DUP-LIST.\n
Store the result in DUP-LIST and return it.  DUP-LIST must be a proper list.\n
Of several `eq' occurrences of an element in DUP-LIST, the first one is kept.\n
:EXAMPLE\n\n\(concat 
 \(mon-delq-dups \(append \(vconcat \"ABRAHADABRA\"\) nil\)\) 
 \"\"\)\n
:ALIASED-BY `delq-dups'\n
:SEE-ALSO `mon-delq-cons', `mon-remove-dups', `mon-remove-if',
`mon-list-make-unique', `mon-assoc-replace', `mon-list-match-tails',
`mon-list-reorder', `mon-maybe-cons', `mon-list-proper-p', `mon-intersection',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose',
`mon-flatten', `mon-recursive-apply', `mon-sublist', `mon-sublist-gutted',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt'.\n►►►"
  (let ((dq-tail dup-list))
    (while dq-tail
      (setcdr dq-tail (delq (car dq-tail) (cdr dq-tail)))
      (setq dq-tail (cdr dq-tail))))
  dup-list)
;;
(unless (and (intern-soft "delq-dups")
             (fboundp 'delq-dups))
  (defalias 'delq-dups 'mon-delq-dups))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (string-equal 
;; |  (concat (mon-delq-dups (append (vconcat "ABRAHADABRA") nil))  "")
;; |  "ABRHD")
;; `----

;;; ==============================
;;; :COURTESY bytecomp.el, gnus-util.el  
;;; :WAS `byte-compile-delete-first' :WAS `gnus-delete-first'
(defun mon-delete-first (list-elt in-list)
  "Delete by side effect the first occurrence of LIST-ELT as member of IN-LIST.\n
:SEE-ALSO .\n"
  ;; :NOTE `defsubst'd in bytecomp.el
  ;; (if (eq (car list) elt)
  ;;     (cdr list)
  ;;   (let ((total list))
  ;;     (while (and (cdr list)
  ;;       	  (not (eq (cadr list) elt)))
  ;;       (setq list (cdr list)))
  ;;     (when (cdr list)
  ;;       (setcdr list (cddr list)))
  ;;     total))
  (byte-compile-delete-first list-elt in-list))

;;; ==============================
;;; :COURTESY :FILE lisp/format.el :WAS `format-make-relatively-unique'
;;; :CHANGESET 2001
;;; :CREATED <Timestamp: #{2010-07-27T16:48:53-04:00Z}#{10302} - by MON KEY>
(defun mon-list-make-unique (list-a list-b &optional as-two-list)
  "Delete common elements of LIST-A and LIST-B.\n
Return a two elt list of with common elts of LIST-A and LIST-B removed.\n
Comparison as if by `equal'/`member'.
When optional arg AS-TWO-LIST is non-nil return as two elt list.\n
:EXAMPLE\n\n\(mon-list-make-unique '\(a b c d e f\) '\(a c e q r z\)\)\n
\(mon-list-make-unique '\(\"a\" b c d e f\) '\(a c e q r z\)\)\n
\(mon-list-make-unique '\(\"a\" b c d e g \(g g\)\) '\(\"a\" b c d e f \(f f\)\) t\)\n
:SEE-ALSO `mon-delq-dups', `mon-delq-cons', `mon-remove-if', `mon-remove-dups',
`mon-list-match-tails', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe', `mon-list-proper-p',
`mon-intersection', `mon-combine', `mon-map-append', `mon-maptree',
`mon-mapcar', `mon-transpose', `mon-flatten', `mon-recursive-apply',
`mon-sublist', `mon-sublist-gutted', `mon-assoc-replace', `mon-moveq',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt'.\n►►►"
  (let* ((mlma-acopy (copy-sequence list-a))
	 (mlma-bcopy (copy-sequence list-b))
	 (mlma-tail mlma-acopy))
    (while mlma-tail
      (let ((mlma-dup (member (car mlma-tail) mlma-bcopy))
            (mlma-next (cdr mlma-tail)))
        (when mlma-dup (setq mlma-acopy (mon-delq-cons mlma-tail mlma-acopy)
                             mlma-bcopy (mon-delq-cons mlma-dup mlma-bcopy)))
        (setq mlma-tail mlma-next)))
    (if as-two-list
         `(,mlma-acopy ,mlma-bcopy)
      (cons mlma-acopy mlma-bcopy))))
;;
;;; :TEST-ME (mon-list-make-unique '("a" b c d e g (g g)) '("a" b c d e f (f f)))
;;; :TEST-ME (cdr (mon-list-make-unique '("a" b c d e g (g g)) '("a" b c d e f (f f))))
;;; :TEST-ME (car (mon-list-make-unique '("a" b c d e g (g g)) '("a" b c d e f (f f))))
;;; :TEST-ME (mon-list-make-unique '("a" b c d e g (g g)) '("a" b c d e f (f f)) t)
;;; :TEST-ME (cdr (mon-list-make-unique '("a" b c d e g (g g)) '("a" b c d e f (f f)) t))
;;; :TEST-ME (equal (mon-list-make-unique '(a b c d e f) '(a c e q r z)) '((b d f) q r z))
;;; :TEST-ME (equal (mon-list-make-unique '("a" b c d e f) '(a c e q r z)) '(("a" b d f) a q r z))

;;; ==============================
;;; :COURTESY Jared D. :WAS `remove-dupes'
;;; (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :CREATED <Timestamp: #{2009-08-19T20:10:43-04:00Z}#{09344} - by MON KEY>
(defun mon-remove-dups (list)
  "Remove duplicate adjoining elts in LIST.\n
:SEE-ALSO `mon-list-make-unique', `mon-delq-dups', `mon-delq-cons',
`mon-remove-if', `mon-intersection', `mon-combine', `mon-map-append',
`mon-mapcar', `mon-maptree', `mon-transpose', `mon-flatten', `mon-recursive-apply',
`mon-sublist', `mon-sublist-gutted', `mon-remove-dups', `mon-assoc-replace',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-list-match-tails', `mon-list-reorder', `mon-list-proper-p',
`mon-maybe-cons'.\n►►►"
  (let (tmp-list head)
    (while list
      (setq head (pop list))
      (unless (equal head (car list))
        (push head tmp-list)))
    (reverse tmp-list)))

;;; ==============================
;;; :COURTESY :FILE lisp/format.el  :WAS `format-common-tail'
;;; :CHANGESET 2001
;;; :CREATED <Timestamp: #{2010-07-27T16:48:50-04:00Z}#{10302} - by MON KEY>
(defun mon-list-match-tails (comp-a comp-b)
  "Given two lists that have a common tail, return the common tail.\n
Compare and return the part of COMP-A satisfying `equal' predicate for the
equivalent part of COMP-B.\n
When the last items of the two do not satsify equivialence return nil.\n
:EXAMPLE\n\n\(mon-list-match-tails '\(\"a\" b c d e g \) '\(q z w b e g\)\)\n
\(mon-list-match-tails '\(\"a\" b c \"d\" e g q\) '\(a \"b\" \"c\" \"d\" \"e\" g q\)\)\n
\(mon-list-match-tails '\(b c \(\"d\" e g q\)\) '\(\"b\" \"c\" \(\"d\" e g q\)\)\)\n
:SEE-ALSO `mon-list-make-unique', `mon-delq-dups', `mon-list-proper-p',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle',
`mon-list-shuffle-safe', `mon-intersection', `mon-remove-if', `mon-combine',
`mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-maybe-cons', `mon-delq-cons', `mon-remove-dups',
`mon-sublist', `mon-sublist-gutted', `mon-assoc-replace', `mon-moveq',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt'.\n►►►"
  (let ((mlct-a (length comp-a))
	(mlct-b (length comp-b)))
    ;; Make sure they are the same length
    (if (> mlct-a mlct-b)
	(setq comp-a (nthcdr (- mlct-a mlct-b) comp-a))
      (setq comp-b (nthcdr (- mlct-b mlct-a) comp-b))))
  (while (not (equal comp-a comp-b))
    (setq comp-a (cdr comp-a)
          comp-b (cdr comp-b)))
    comp-a)
;;
;;; :TEST-ME (mon-list-match-tails '("a" b c d e g ) '(q z w b e g))
;;; :TEST-ME (mon-list-match-tails '("a" b c "d" e g q) '(a "b" "c" "d" "e" g q))
;;; :TEST-ME (mon-list-match-tails '(b c ("d" e g q)) '("b" "c" ("d" e g q)))

;;; ==============================
;;; :COURTESY :FILE lisp/format.el :WAS `format-proper-list-p'
;;; :CHANGESET 2001
;;; :CREATED <Timestamp: #{2010-07-27T16:48:36-04:00Z}#{10302} - by MON KEY>
(defun mon-list-proper-p (putatively-proper)
  "Return t if list PUTATIVELY-PROPER is a proper list.\n
A proper list is a list ending with a nil or cdr, not an atom.\n
:EXAMPLE\n\n\(mon-list-proper-p '\(a . b\)\)\n
\(mon-list-proper-p '\(a  b\)\)\n
\(mon-list-proper-p nil\)\n
\(mon-list-proper-p '\(\)\)\n
\(mon-list-proper-p '\(nil\)\)\n
\(mon-list-proper-p '\(\(\) nil\)\)\n
\(mon-list-proper-p '\(\(\) nil . a\)\)\n
:ALIASED-BY `proper-list-p'\n
:SEE-ALSO `mon-maybe-cons', `mon-list-match-tails', `mon-list-make-unique',
`mon-delq-cons', `mon-delq-cons', `mon-remove-dups', `mon-remove-if',
`mon-list-reorder', `mon-assoc-replace', `mon-intersection', `mon-combine',
`mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt'.\n►►►"
  (when (listp putatively-proper)
    (while (consp putatively-proper)
      (setq putatively-proper (cdr putatively-proper)))
    (null putatively-proper)))
;;
(unless (and (intern-soft "proper-list-p")
             (fboundp 'proper-list-p))
  (defalias 'proper-list-p 'mon-list-proper-p))
;;
;;; :TEST-ME (mon-list-proper-p '(a . b))
;;; :TEST-ME (mon-list-proper-p '(a  b))

;;; ==============================
;;; :COURTESY :FILE lisp/format.el :WAS `format-reorder'
;;; :CHANGESET 2001
;;; :CREATED <Timestamp: #{2010-07-27T16:47:20-04:00Z}#{10302} - by MON KEY>
(defun mon-list-reorder (list-items list-order &optional remv-dups)
  "Arrange LIST-ITEMS to follow partial order of ORDER-LIST.\n
Elts of LIST-ITEMS `equal' to elts of ORDER-LIST arranged to follow ORDER-LIST.\n
Any unmatched elts of LIST-ITEMS will occur last in return value \(including
duplicate elts\).\n
When optional arg REMV-DUPS is non-nil remove duplicate elements.\n
:EXAMPLE\n\n\(mon-list-reorder '\(2 6 3 2 1\) '\(1 2 3 4 5 6\)\)\n
\(mon-list-reorder '\(q w b c s a w\) '\(a b c q z w\)\)\n
\(mon-list-reorder '(q w b c s a w) '(a b c q z w) t)\n
:SEE-ALSO `mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-maybe-cons', `mon-delq-cons', `mon-list-make-unique', `mon-delq-dups',
`mon-list-match-tails', `mon-list-proper-p', `mon-intersection',
`mon-remove-if', `mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-recursive-apply', `mon-sublist',
`mon-sublist-gutted', `mon-remove-dups', `mon-assoc-replace', `mon-moveq',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt'.\n►►►"
  (let ((lo list-order)
        (li list-items)
        rtn)
    (setq rtn
          (if lo
              (let ((item (member (car lo) li)))
                (if item
                    (cons (car item)
                          (mon-list-reorder (mon-delq-cons item li)
                                            (cdr lo)))
                  (mon-list-reorder li (cdr lo))))
            li))
    (if remv-dups
        (delete-dups rtn)
      rtn)))
;;
;;; :TEST-ME (mon-list-reorder '(2 6 3 2 1) '(1 2 3 4 5 6))
;;; :TEST-ME (mon-list-reorder '(q w b c s a w) '(a b c q z w))
;;; :TEST-ME (mon-list-reorder '(q w b c s a w) '(a b c q z w) t)

;;; ==============================
;;; :COURTESY Tassilo Horn :HIS lisp/doc-view.el :WAS `doc-view-remove-if'
;;; :MODIFICATIONS <Timestamp: #{2010-04-01T11:22:35-04:00Z}#{10134} - by MON KEY>
;;; :RENAMED parameter PREDICATE -> RMV-IF-PREDICATE; LIST -> RMV-LIST; 
;;; :RENAMED local var NEW-LIST -> MRI-NEW-LIST
;;; :CREATED <Timestamp: #{2010-04-01T11:08:43-04:00Z}#{10134} - by MON KEY>
(defun mon-remove-if (rmv-if-predicate rmv-list)
  "Return RMV-LIST with all items removed that satisfy RMV-PREDICATE.\n
RMV-IF-PREDICATE is unary function.\n
:EXAMPLE\n\n\(let \(\(mri-l \(number-sequence 0 10\)\)\)
  \(mon-remove-if #'\(lambda \(chk-it\) \(oddp chk-it\)\) mri-l\)\)\n
:SEE-ALSO `mon-intersection', `mon-remove-if', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-delq-cons', `mon-list-make-unique', `mon-list-match-tails',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-list-reorder', `mon-list-proper-p', `mon-maybe-cons',
`mon-delq-cons'.\n►►►"
  (let (mri-new-list)
    (dolist (mri-item rmv-list (setq mri-new-list (nreverse mri-new-list)))
      (when (not (funcall rmv-if-predicate mri-item))
	(push mri-item mri-new-list)))))
;;
;;; :TEST-ME (let ((mri-l (number-sequence 0 10)))
;;;             (mon-remove-if #'(lambda (chk-it) (oddp chk-it)) mri-l))
;;; :WANTING (0 2 4 6 8 10)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-22T15:12:02-05:00Z}#{10035} - by MON>
(defun mon-intersection (list1 list2 &optional do-eql do-eq)
  "Combine list1 and list2 using a set-intersection operation.\n
The result list contains all items that appear in both list1 and list2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original list1 and list2.\n
By default comparsion made as with `member'.\n
When optional arg DO-EQl uses `memql'.\n
When optional arg DO-EQ uses `memq'.\n
:EXAMPLE\n
\(mon-intersection \(number-sequence 8 20 2\) \(number-sequence 0 20 4\)\)
\(mon-intersection \(number-sequence 8 20 2\) \(number-sequence 8 20 2\)\)
\(mon-intersection \(number-sequence 8 20 2\) nil\)
\(mon-intersection nil \(number-sequence 8 20 2\)\)
\(mon-intersection nil nil\)
\(mon-intersection '\(\"str1\" sym1 \"str2\"\) '\(sym1 \"str2\"\)\)       ;`member'
\(mon-intersection '\(\"str1\" sym1 \"str2\"\) '\(sym1 \"str2\"\) t\)     ;`memql'
\(mon-intersection '\(\"str1\" \"str2\"\) '\(\"str2\"\) t\)               ;`memql'
\(mon-intersection '\(\"str1\" sym1 \"str2\"\) '\(sym1 \"str2\"\) nil t\) ;`memq'
\(mon-intersection '\(\"str1\" \"str2\"\) '\(\"str2\"\) nil t\)           ;`memq'
\(mon-intersection '\(sym1 sym2\) '\(sym1\)\)                       ;`memq'
\(mon-intersection \(number-sequence 8 20 2\) 8)                 ;Signal Error.\n
:NOTE Like `intersection' from :FILE cl-seq.el adapted for use without keywords
      Does not provide intelligent type checking.\n
:SEE-ALSO `mon-sublist', `mon-sublist-gutted', `mon-mapcar', `mon-map-append',
`mon-maptree', `mon-transpose', `mon-flatten', `mon-combine',
`mon-recursive-apply', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-remove-if', `mon-delq-cons', `mon-list-make-unique',
`mon-list-match-tails', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe', `mon-list-proper-p',
`smtpmail-intersetion'.\n►►►"
  (unless (and (or (consp list1) (null list1))
               (or (consp list2) (null list2)))
    (error (concat ":FUNCTION `mon-intersection' "
                   "-- args LIST1 and LIST2 must be either a list or nil")))
  (and list1 list2
     (if (equal list1 list2)
           list1
	   (let ((mintr-res nil)
                 (mintr-l1 list1)
                 (mintr-l2 list2)
                 (comp-with #'(lambda (l1 l2) 
                                (if (or do-eql do-eq)
                                    (cond (do-eql (memql l1 l2))
                                          (do-eq  (memq l1 l2)))
                                    (member l1 l2)))))
	     (or (>= (length mintr-l1) (length mintr-l2))
		 (setq mintr-l1 (prog1 mintr-l2 (setq mintr-l2 mintr-l1))))
	     (while mintr-l2
	       (when (funcall comp-with (car mintr-l2) mintr-l1) (push (car mintr-l2) mintr-res))
	       (pop mintr-l2))
	     (unless (null mintr-res)
               (setq mintr-res (nreverse mintr-res)))))))
;;
;;; (defun hfy-interq (set-a set-b)
;;;   "Return the intersection \(using `eq'\) of 2 lists."
;;;   (let ((sa set-a) (interq nil) (elt nil))
;;;     (while sa
;;;       (setq elt (car sa)
;;;             sa  (cdr sa))
;;;       (if (memq elt set-b) (setq interq (cons elt interq)))) interq))

;;; ==============================
;; :COURTESY :FILE lisp/mail/smtpmail.el :WAS `smtpmail-intersection'
;; (defun smtpmail-intersection (list1 list2)
;;   (let ((result nil))
;;     (dolist (el2 list2)
;;       (when (memq el2 list1)
;; 	(push el2 result)))
;;     (nreverse result)))
;;; ==============================

;;; ==============================
;;; :COURTESY Jean-Marie Chauvet :HIS ncloseemacs-ml-dataset.el :WAS `sublist'
;;; :CREATED <Timestamp: #{2009-09-19T19:10:14-04:00Z}#{09386} - by MON>
(defun mon-sublist (skip-n return-n in-list)
  "RETURN-N elements IN-LIST skipping the first SKIP-N.\n
:EXAMPLE\n\n\(let \(\(piece-work
       '\(A B \(C D\) E \(F G\) \(Q \(H I\)\) K\)\)\)
       ; 0 1   2   3  4     5        6
  \(mon-sublist 4 2 piece-work\)\)\n
; => \((F G) (Q (H I)))
     ; 4     5\n
:SEE-ALSO `mon-sublist-gutted' `mon-list-proper-p', `mon-maybe-cons',
`mon-remove-dups', `mon-remove-if', `mon-delq-cons', `mon-delq-dups'
`mon-list-make-unique', `mon-list-match-tails', `mon-assoc-replace',
`mon-moveq', `mon-flatten', `mon-transpose', `mon-maptree', `mon-mapcar',
`mon-recursive-apply', `mon-map-append', `mon-combine', `mon-intersection',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe', `mon-list-reorder'.\n►►►"
  (let* ((sub (nthcdr skip-n in-list)) 
	 (q (length sub)))
    (reverse (nthcdr (- q return-n) (reverse sub)))))
;;
;;; :TEST-ME (mon-sublist 0 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 3 3 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 5 2 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 1 2 '(A B (C D) E (F G) (Q (H I)) K))  
;;; :TEST-ME (mon-sublist 6 1 '(A B (C D) E (F G) (Q (H I)) K))  

;;; ==============================
;;; :COURTESY Jean-Marie Chauvet :HIS ncloseemacs-ml-dataset.el :WAS `sublist-rest'
;;; :CREATED <Timestamp: #{2009-09-19T18:55:37-04:00Z}#{09386} - by MON>
(defun mon-sublist-gutted (gut-from-n to-n-ards gut-list)
  "Return GUT-LIST with GUTS-FROM-N TO-N-ARDS extracted.\n
:EXAMPLE\n\(let \(\(eviscerate-me 
       '\(A B \(C D\) E \(F G\) \(Q \(H I\)\) K\)\)\)
        ;0 1   2   3  4     5        6
  \(mon-sublist-gutted 4 2 eviscerate-me\)\)\n;=> \(A B \(C D\) E K\)\n
     ;0 1   2   3 6\n
:SEE-ALSO `mon-sublist', `mon-intersection', `mon-combine', `mon-map-append',
`mon-mapcar', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-remove-if', `mon-remove-dups', `mon-delq-dups',
`mon-delq-cons', `mon-list-make-unique', `mon-assoc-replace',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-maybe-cons', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-list-match-tails', `mon-list-reorder',
`mon-list-proper-p'.\n►►►"
  (let* ((pre-guts 
          (nthcdr (length (nthcdr gut-from-n gut-list)) (reverse gut-list))) ;; pre-guts reversed
	 (post-guts 
          (nthcdr (+ to-n-ards (length pre-guts)) gut-list)))
    (append (reverse pre-guts) post-guts))) ;;:WAS (append prefix postfix)
;;
;;; :TEST-ME (mon-sublist-gutted 3 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 5 2 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 5 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 0 6 '(A B (C D) E (F G) (Q (H I)) K))

;;; ==============================
;;; :COURTESY Jean-Marie Chauvet nclose-eieio.el :WAS `map-append'
;;; :CREATED <Timestamp: #{2009-09-21T15:26:14-04:00Z}#{09391} - by MON KEY>
(defun mon-map-append (mapping-l)
  "Append all sublists in list.\n
:SEE-ALSO `mon-intersection', `mon-combine', `mon-mapcar', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-recursive-apply', `mon-list-proper-p',
`mon-maybe-cons', `mon-sublist', `mon-sublist-gutted', `mon-list-match-tails',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-delq-cons', `mon-delq-dups', `mon-list-make-unique', `mon-remove-if',
`mon-remove-dups', `mon-assoc-replace', `mon-list-reorder',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe'.\n►►►"
  (cond ((null mapping-l) nil)
	(t (append (car mapping-l) (mon-map-append (cdr mapping-l))))))

;;; ==============================
;;; :COURTESY Jared D. :WAS `assoc-replace'
;;; (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :MODIFICATIONS <Timestamp: #{2010-02-10T20:17:04-05:00Z}#{10064} - by MON KEY>
;;; Now returns the full association not just the value of key.
;;; :CREATED <Timestamp: #{2009-08-19T20:00:51-04:00Z}#{09344} - by MON KEY>
(defun mon-assoc-replace (seq1 seq2)
  "Return alist with elts of the alist SEQ1 substituted with the element of
SEQ1 where the car of elt SEQ1 matches the car of elt SEQ2.\n
:EXAMPLE\n\n\(mon-assoc-replace '\(\(a \(a c d\)\) \(b \(c d e\)\) \(c \(f g h\)\)\)
                   '\(\(a \(c d g\)\) \(b \(c d f\)\) \(g \(h g f\)\)\)\)\n
:SEE-ALSO `mon-delq-dups', `mon-delq-cons', `mon-remove-dups',`mon-remove-if',
`mon-list-make-unique', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe', `mon-intersection', `mon-combine',
`mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-list-match-tails', `mon-sublist',
`mon-sublist-gutted', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-maybe-cons', `mon-list-proper-p'.\n►►►"
  (let (mar-rtn)
    (setq mar-rtn
          (mapcar #'(lambda (elem)
                      (let* ((key (car elem))
                             (val (assoc key seq2)))
                        ;; :WAS (if (cadr val) val elem))) seq1)) 
                        (if val val elem))) seq1))))
;;                          
;;; :TEST-ME (mon-assoc-replace '((a (a c d)) (b (c d e)) (c (f g h)))
;;;                             '((a (c d g)) (b (c d f)) (g (h g f))))
    
;; ==============================
;;; :COURTESY Henry Kautz :HIS refer-to-bibtex.el :WAS `moveq'
;;; :CREATED <Timestamp: 2009-08-04-W32-2T18:57:30-0400Z - by MON KEY>
(defmacro mon-moveq (new old)
  "Set NEW to OLD and set OLD to nil.\n
:SEE-ALSO `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-list-proper-p', `mon-maybe-cons' `mon-sublist', `mon-sublist-gutted',
`mon-remove-if', `mon-intersection', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-delq-cons', `mon-delq-dups', `mon-remove-if',
`mon-remove-dups', `mon-list-make-unique', `mon-assoc-replace',
`mon-list-match-tails', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe'.\n►►►"
  (list 'progn (list 'setq new old) (list 'setq old 'nil)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-list.el :WAS `flatten'
(defun mon-flatten (tree)
  "Return a tree containing all the elements of the `tree'.\n
:SEE-ALSO `mon-list-proper-p', `mon-maybe-cons', `mon-intersection',
`mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose',
`mon-flatten', `mon-recursive-apply', `mon-list-match-tails', `mon-sublist',
`mon-sublist-gutted', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-remove-if', `mon-remove-dups', `mon-delq-dups'
`mon-delq-cons', `mon-list-make-unique', `mon-remove-dups', `mon-assoc-replace',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle',
`mon-list-shuffle-safe'.\n►►►"
  (do ((result nil)
       (stack  nil))
      ((not (or tree stack)) (nreverse result))
    (cond ((null tree)
	   (setq tree (pop stack)))
	  ((atom tree)
	   (push tree result)
	   (setq tree (pop stack)))
	  ((listp (car tree))
	   (push (cdr tree) stack)
	   (setq tree (car tree)))
	  (t (push (car tree) result)
	     (setq tree (cdr tree))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS list.lisp :WAS TRANSPOSE
;;; :CREATED <Timestamp: #{2009-09-28T17:40:47-04:00Z}#{09401} - by MON>
(defun mon-transpose (tree)
  "Return a tree where all the CAR and CDR are exchanged.\n
:SEE-ALSO `mon-mismatch', `mon-intersection', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-flatten', `mon-recursive-apply',
`mon-sublist', `mon-sublist-gutted', `mon-list-match-tails',
`mon-list-proper-p', `mon-maybe-cons', `mon-moveq', `mon-elt->', `mon-elt-<',
`mon-elt->elt', `mon-elt-<elt', `mon-delq-cons', `mon-delq-dups'
`mon-remove-if', `mon-remove-dups', `mon-assoc-replace', `mon-list-make-unique',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle'\n►►►"
  (if (atom tree)
      tree
      (cons (mon-transpose (cdr tree)) (mon-transpose (car tree)))))
;;
;;; :TEST-ME (mon-transpose '(a (bb cc) dd)))
;;; :TEST-ME (mon-flatten (mon-transpose '(a (bb cc) dd)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-15T14:40:11-04:00Z}#{10242} - by MON KEY>
(defun* mon-mismatch (sqn1 sqn2 &key (sqn1-str 0)
                                    (sqn1-end (length sqn1))
                                    (sqn2-str 0)
                                    (sqn2-end (length sqn2)))
  "Implementation of `edmacro-mismatch' function with keywords.\n
Compare sqn1 with sqn2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorted sequence.\n
:EXAMPLE\n
\(mon-sequence-mismatch  '\(a b c 1 8\) '\(a b c 2 9\)\)\n
\(mon-sequence-mismatch  '\(a b c 2 8\) '\(a b c 2 8\) :sqn1-str 2  :sqn2-str 2\)\n
\(mon-sequence-mismatch  '\(a b c 2 8\) '\(a b c a b c 2 8 a b c\) :sqn2-str 3\)\n
:NOTE `edmacro-mismatch' was a kludge needed in order to use CL `mismatch'.\n
Should byte withouth CL package warnings.\n
:SEE-ALSO `mon-sublist', `mon-sublist-gutted', `mon-mapcar', `mon-map-append',
`mon-string-chop-spaces', `mon-maptree', `mon-transpose', `mon-elt->',
`mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `mon-flatten', `mon-combine',
`mon-recursive-apply', `mon-delq-cons', `mon-list-make-unique',
`mon-list-match-tails', `mon-list-reorder', `mon-list-proper-p',
`mon-maybe-cons'.\n►►►"
  (edmacro-mismatch sqn1 sqn2 sqn1-str sqn1-end  sqn2-str sqn2-end))

;;; ==============================
;;; :NOTE `mon-combine' uses `flet' cl--mapcan -> `mapcan'
;;; :COURTESY Pascal J. Bourguignon :HIS ???
(defun mon-combine (&rest args)
  "Return the set of tuples built taking one item in order from each list
in ARGS.\n:EXAMPLE\n\n\(mon-combine '\(www ftp\) '\(exa\) '\(com org\)\)\n
;=> \(\(www exa com\) \(www exa org\) \(ftp exa com\) \(ftp exa org\)\)\n
:SEE-ALSO `mon-intersection', `mon-remove-if', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-list-match-tails', `mon-list-proper-p',
`mon-maybe-cons', `mon-sublist', `mon-sublist-gutted', `mon-moveq', `mon-elt->',
`mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `mon-delq-cons', `mon-delq-dups',
`mon-remove-dups', `mon-remove-if', `mon-assoc-replace', `mon-list-make-unique',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle'\n►►►"
  ;; :NOTE cl--mapcan -> `mapcan' from cl*.el 
  (flet ((cl--mapcan (func seq &rest rest)
           (apply 'nconc (apply 'mapcar* func seq rest))))
    (cond ((null args) '(nil))
          ((consp (car args))
           (cl--mapcan
             (lambda (item) (apply (function mon-combine) item (cdr args)))
                       (car args)))
          (t (cl--mapcan
              (lambda (rest) (list (cons (car args) rest)))
                         (apply (function mon-combine) (cdr args)))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS list.lisp :WAS `maptree'
;;; :MODIFICATIONS <Timestamp: #{2010-01-16T19:20:11-05:00Z}#{10027} - by MON>
;;; `mon-maptree' uses `flet' cl--every -> `every'
;;; :CREATED <Timestamp: #{2009-09-28T17:40:37-04:00Z}#{09401} - by MON>
(defun mon-maptree (fun &rest trees)
  "Map function FUN over trees or TREES.\n
:EXAMPLE\n\(mon-maptree
 #'\(lambda \(x\) \(when \(stringp x\) \(prin1 x\)\)\)
   '\(a \(\"b\" b cc \"bb\"\) dd\)\)\n
:SEE-ALSO `mon-intersection', `mon-remove-if', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-list-match-tails', `mon-list-proper-p',
`mon-maybe-cons', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-sublist', `mon-sublist-gutted', `mon-remove-if',
`mon-remove-dups', `mon-delq-dups', `mon-delq-cons', `mon-list-make-unique',
`mon-assoc-replace', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle'\n►►►"
  ;; :WAS
  ;; (cond ((null trees) nil)
  ;;       ((every (function null)  trees) nil)
  ;;       ((every (function atom)  trees) (apply fun trees))
  ;;       ((every (function consp) trees)
  ;;        (cons (apply (function mon-maptree) fun (mapcar (function car) trees))
  ;;              (apply (function mon-maptree) fun (mapcar (function cdr) trees))))
  ;;       (t nil)))
  ;; :NOTE flet `every' to prevent compiler warning.
  (flet ((cl--every (cl-pred cl-seq &rest cl-rest)
           (if (or cl-rest (nlistp cl-seq))
               (catch 'cl-every-mon
                 (apply 'map nil
                        (function (lambda (&rest cl-x)
                          (or (apply cl-pred cl-x) (throw 'cl-every-mon nil))))
                        cl-seq cl-rest) t)
               (while (and cl-seq (funcall cl-pred (car cl-seq)))
                 (setq cl-seq (cdr cl-seq)))
               (null cl-seq))))
    (cond ((null trees) nil)
          ((cl--every (function null)  trees) nil)
          ((cl--every (function atom)  trees) (apply fun trees))
          ((cl--every (function consp) trees)
           (cons (apply (function mon-maptree) fun (mapcar (function car) trees))
                 (apply (function mon-maptree) fun (mapcar (function cdr) trees))))
          (t nil))))
;;
;;; :TEST-ME (mon-maptree #'(lambda (x) (when (stringp x) (prin1 x)))  '(a ("b" b cc) dd))
;;; => "b" (nil ("b" nil nil) nil)

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS recursive-apply?
(defun mon-recursive-apply (atom-func list-a list-b)
  "Apply recursively the function atom-func on each and every pairs
that can be found recursively in the two parallel structures a-list
and b-list. Only the elements from a-list must be an atom to be passed
to atom-func.\n
:EXAMPLE(1)\n\n\(mon-recursive-apply '+ '\(\(1 2\) \(3 4\)\) '\(\(1 0\) \(0 1\)\)\)\n
:EXAMPLE(2)\n\n(mon-recursive-apply\n #'\(lambda \(atom other\) \(cons atom other\)\)
   '\(apple orange peach\) '\(\(red yellow green\) \(orange\) \(yellow white\)\)\)\n
:SEE-ALSO `mon-intersection', `mon-remove-if', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-list-proper-p', `mon-maybe-cons',
`mon-list-match-tails', `mon-sublist', `mon-sublist-gutted', `mon-moveq',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `mon-delq-cons',
`mon-delq-dups', `mon-remove-dups', `mon-remove-if', `mon-list-make-unique',
`mon-assoc-replace', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle'.\n►►►"
  (cond ((null list-a) nil)
        ((atom list-a) (apply atom-func (list list-a list-b)))
        (t (cons (mon-recursive-apply atom-func (car list-a) (car list-b)) 
                 (mon-recursive-apply atom-func (cdr list-a) (cdr list-b))))))


;;; ==============================
;;; :COURTESY :FILE gnus-util.el :WAS `gnus-merge'
;;; :CHANGESET 2112
;;; :CREATED <Timestamp: #{2010-09-06T21:02:35-04:00Z}#{10361} - by MON KEY>
(defun mon-list-merge (type list1 list2 pred)
  "Destructively merge lists LIST1 and LIST2 to produce a new list.\n
Argument TYPE is for compatibility and ignored.\n
Ordering of the elements is preserved according to PRED, a `less-than'
predicate on the elements.\n
:ALIASED-BY `mon-merge-list'\n
:SEE-ALSO `mon-intersection', `mon-remove-if', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-list-match-tails', `mon-list-proper-p',
`mon-maybe-cons', `mon-sublist', `mon-sublist-gutted', `mon-moveq', `mon-elt->',
`mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `mon-delq-cons', `mon-delq-dups',
`mon-remove-dups', `mon-remove-if', `mon-assoc-replace', `mon-list-make-unique',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle'\n►►►"
  (let ((mlm-res nil))
    (while (and list1 list2)
      (if (funcall pred (car list2) (car list1))
          (push (pop list2) mlm-res)
        (push (pop list1) mlm-res)))
    (nconc (nreverse mlm-res) list1 list2)))
;;
(unless (and (intern-soft "mon-merge-list")
             (fboundp 'mon-merge-list))
  (defalias 'mon-merge-list 'mon-list-merge))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS foreach
(defmacro mon-foreach (w-var on-list &rest body)
  "A foreach style macro idiom for looping W-VARS ON-LIST with BODY.\n
:EXAMPLE\n\n\(mon-foreach for-var                 ; <- w-var
             '\(1 2 3 4)              ; <- on-list
             \(+ for-var for-var\)\)    ; <- body\n
\(pp-macroexpand-expression 
 '\(mon-foreach for-var '\(1 2 3 4\) \(+ for-var for-var\)\)\)\n
:SEE-ALSO `mon-for', `mon-loop', `mon-mapcar'.\n►►►"
  `(mapcar #'(lambda (,w-var) ,@body) ,on-list))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS `for'
(defmacro mon-for (var init final &rest body)
  "Execute a simple for loop .\n
:EXAMPLE\n\n\(mon-for i  1  10  \(print i\)\)\n
\(pp-macroexpand-expression '\(mon-for i 1 10  \(print i\)\)\)\n
:SEE-ALSO `mon-foreach', `mon-loop', `mon-mapcar'.\n►►►"
  (let ((mf-tempvar (make-symbol "mf-tempvar")))
    `(let ((,var ,init)
           (,mf-tempvar ,final))
       (if (< ,var ,mf-tempvar)
           (while (<= ,var ,mf-tempvar)
             ,@body
             (setq ,var (+ ,var 1)))
         (while (>= ,var ,mf-tempvar)
           ,@body
           (setq ,var (- ,var 1)))))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `rloop'
;;; :MODIFICATIONS <Timestamp: #{2009-09-29T13:35:36-04:00Z}#{09402} - by MON KEY>
(defmacro mon-loop (clauses &rest body)
  "Macro to execute a loop over clauses.\n
:SEE-ALSO `mon-foreach', `mon-for', `mon-mapcar'.\n►►►"
  (if (null clauses)
      `(progn ,@body)
    `(loop ,@(car clauses) do (mon-loop ,(cdr clauses) ,@body))))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-10-22T17:58:11-04:00Z}#{09434} - by MON>
(defun mon-escape-string-for-cmd (unescape a-string &rest more-strings)
  "Return A-STRING escaped for passing to the w32 cmd.exe e.g `/' -> `\\\\'.
When MORE-STRINGS is non-nil escape these also.\n
When UNESCAPE is non-nil unescape A-STRING and/or MORE-STRINGS.\n
:SEE-ALSO `convert-standard-filename', `w32-shell-dos-semantics'.
`w32-quote-process-args', `mon-exchange-slash-and-backslash',
`mon-escape-lisp-string-region', `mon-unescape-lisp-string-region'.\n►►►"
  (let ((got-more-p (if more-strings
                        (cons a-string more-strings)
                        a-string))
        (rgxp-rplc (if unescape
                       #'(lambda (u)(replace-regexp-in-string  "\\\\" "/" u))
                       #'(lambda (e)(replace-regexp-in-string "/" "\\\\" e)))))
    (if (consp got-more-p)
        (mapconcat rgxp-rplc got-more-p " ")
        (funcall rgxp-rplc got-more-p))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Saturday May 30, 2009 @ 06:26.12 PM - by MON KEY>
(defun mon-escape-lisp-string-region (start end)
  "Escape special characters in the region as if a lisp string.
Insert backslashes in front of special characters (namely  `\' backslash,
`\"' double quote, `(' `)' parens in the region, according to the docstring escape 
requirements.\n
Region should only contain the characters actually comprising the string
supplied without the surrounding quotes.\n
:NOTE\n Don't evaluate on docstrings containing regexps and expect sensible
return values.\n
:SEE-ALSO `mon-unescape-lisp-string-region', `mon-escape-string-for-cmd',
`mon-exchange-slash-and-backslash'.\n►►►"
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
  "Unescape special characters from the CL string specified by the region.\n
This amounts to removing preceeding backslashes from characters they escape.\n
:NOTE region should only contain the characters actually comprising the string
without the surrounding quotes.\n
:SEE-ALSO `mon-escape-lisp-string-region', `mon-escape-string-for-cmd',
`mon-exchange-slash-and-backslash'.\n►►►"
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
  "Quote SEXP if it is not self quoting.\n
:EXAMPLE\n\n
:SEE-ALSO .\n►►►"
  (if (or (memq sexp '(t nil))
	  (keywordp sexp)
	  (and (listp sexp)
	       (memq (car sexp) '(lambda)))
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
;;; :TODO This is a bad name, fix it.
;;; :CREATED: <Timestamp: #{2009-10-20T15:56:02-04:00Z}#{09432} - by MON>
(defun mon-make-a-pp (start end &optional CL->downcase)
    "Pretty print the region in buffer. Do not move point.\n
 When CL->DOWNCASE is non-nil it is used to clean CL that is `UPCASE'd.\n
:SEE-ALSO `mon-princ-cb', `mon-eval-sexp-at-point', `mon-eval-expression',
`mon-eval-print-last-sexp', `mon-toggle-eval-length'.\n►►►"
    (interactive "r\nP")
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
      (set-marker mk3 start)
      (set-marker mk4 end)
      (mon-wrap-with (concat fw "\n") (concat "\n" bw))
      (search-forward-regexp (concat "\n" bw))
      (set-marker mk2 (point))
      (eval (preceding-sexp))
      (search-backward-regexp (concat "\n" bw))
      (set-marker mk1 (point))
      (delete-region mk1 mk2)
      (search-backward-regexp (concat fw "\n"))
      (set-marker mk1 (point))
      (search-forward-regexp (concat fw "\n"))
      (set-marker mk2 (point))
      (delete-region mk1 mk2)
      (delete-region mk3 mk4)
      (forward-sexp)
      (set-marker mk2 (point))
      (backward-sexp)
      (set-marker mk1 (point))
      (delete-region mk1 (1+ mk1))
      (delete-region mk2 (1- mk2))
      (backward-delete-char 1)))

;;; ==============================
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 02:13.22 PM - by MON KEY>
(defun mon-princ-cb ()
  "Wrap region in a princ->current-buffer to eval and print newline\\result
  after point.\n
:SEE-ALSO `mon-eval-sexp-at-point', `mon-make-a-pp', `mon-eval-expression',
`mon-eval-print-last-sexp'.\n►►►"
  (interactive)
  (save-excursion
    ;; (let (sexp-pnt
  (mon-wrap-text "(progn(newline)(princ\n" "\n(current-buffer)))")))

;;; ==============================
;;; :NOTE Consider using: (pp-eval-last-sexp t)
;;; :CREATED <Timestamp: Wednesday May 20, 2009 @ 03:14.44 PM - by MON KEY>
(defun mon-eval-sexp-at-point ()
  "Evaluate S-expression at point print commented result on newline.\n
Return point after commented result. Best on trivial expressions.\n
:EXAMPLE\n\(+ 1 3)\n;;;=> 4\n^point^\n
:SEE-ALSO `mon-princ-cb', `mon-make-a-pp', `mon-eval-expression',
`mon-eval-print-last-sexp', `mon-eval-sexp-at-point', `mon-toggle-eval-length'.
►►►"
  (interactive)
  (let* ((wrap (sexp-at-point))
	 (val (eval wrap))      
	 ;; :WAS (comnt "\n;;;=>")
         (comnt "\n;;;=> ")
	 (comn-sexp (format "%S%s%S"  wrap comnt val))
	 (bnds))
    (save-excursion
      (if (not (eobp))
	  (forward-line)
	(newline))
      (insert comn-sexp))
    (setq bnds (bounds-of-thing-at-point 'sexp))
    ;; :WAS (delete-region (car bnds) (cdr bnds))
    (setq wrap (delete-and-extract-region (car bnds) (cdr bnds)))
    (when (mon-line-bol-is-eol)
      (delete-char 1))
    ;; :WAS (search-forward-regexp "^;;;=> .*$" nil t)
    (search-forward-regexp (format "^;;;=> %S" val) nil t)
    val
    ))
;;
(defun mon-eval-print-last-sexp ()
  "Like `eval-print-last-sexp' but does not move point.\n
:SEE-ALSO `mon-eval-expression', `mon-eval-sexp-at-point',
`mon-make-a-pp', `mon-princ-cb', `mon-toggle-eval-length',
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
`mon-make-a-pp', `mon-princ-cb', `mon-toggle-eval-length'.\n►►►"
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
	(print-level eval-expression-print-level))
    (if eval-expression-insert-value
	(with-no-warnings
          (let ((standard-output (current-buffer)))
            (prin1 (car values))))
      (prog1
          (prin1 (car values) t)
        (let ((str (eval-expression-print-format (car values))))
          (if str (princ str t)))))))

;;; ==============================
;;; :CHANGESET 1980
;;; :CREATED <Timestamp: #{2010-07-16T12:57:27-04:00Z}#{10285} - by MON KEY>
;; (defun mon-error-format (symbol-type symbol-name descr-spec &rest fmt-args)
;;   "Return a formatted error string.\n
;; SYMBOL-TYPE is the type of symbol to report about it is one of:
;;  function macro variable constant\n
;; SYMBOL-NAME is the name of the symbol for report.\n
;; DESCR-SPEC format spec string as per format.\n
;; FMT-ARGS any number of individual arguments to format DESCR-SPEC string.\n
;; Return value is a string with the form:
;;  \":<SYMBOL-TYPE> <SYMBOL-NAME> \\n
;;   -- <DESCR-SPEC>\"
;; "
;;   (let* ((styp (case symbol-type
;;                  (function ":FUNCTION")
;;                  (macro ":MACRO")
;;                  (variable ":VARIABLE")
;;                  (constant ":CONSTANT")
;;                  ;;(face ":FACE "))
;;                  ;;(theme ":THEME "))
;;                  ))
;;          (synm (format "`%s'" symbol-name))
;;          (rtn-fmt
;;           (apply 'format (concat styp " " synm " \n-- " descr-spec) fmt-args)))
;;     (signal 'error (list rtn-fmt))))
;; 
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (mon-error-format 'function 'mon-error-format 
;; |                   "has signalled an error with FMT-ARGS %d and %d" 5 5)
;; | 
;; | (mon-error-format 'function 'mon-error-format 
;; |                   "has signalled an error with FMT-ARGS")
;; | 
;; | (mon-error-format 'variable 'mon-error-format 
;; |                   " signalled an error with FMT-ARGS %d and %d" 5 5)
;; | 
;; | (mon-error-format 'constant 'mon-error-format 
;; |                   " signalled an error with FMT-ARGS %d and %d" 5 5)
;; `----


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
 (let* ((leading-blanks
         (if (null current-prefix-arg) 0 (current-column)))
        (column-display (concat (make-string leading-blanks ?\ )
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
      (substring column-display
                 0 (min (1- (window-width)) (length column-display)))
      (point)))))
;;
;;; :TEST-ME (call-interactively 'mon-show-columns)

;;; ==============================
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
          do (let ((type (car form))
                   (name (cadr form)))
               (cond
                 ((memq type '(defun defun* defsubst defalias defmacro))
                  (fmakunbound name))
                 ((memq type '(defvar defparameter defconst defcustom))
                  (makunbound name))))))
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
(defun mon-unbind-symbol (symbol)
  "Totally unbind SYMBOL.\n
Includes unbinding function binding, variable binding, and property list.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive "SSymbol: ")
  (fmakunbound symbol)
  (makunbound symbol)
  (setf (symbol-plist symbol) nil))
;; :KEEP-WITH-ABOVE
(defun mon-unbind-function (fncn-symbol)
  "Remove the function binding of FNCN-SYMBOL.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive "aFunction: ")
  (fmakunbound fncn-symbol))
;; :KEEP-WITH-ABOVE
(defun mon-unbind-command (cmd-symbol)
  "Remove the command binding of CMD-SYMBOL.\n
:SEE-ALSO `mon-unbind-command', `mon-unbind-symbol', `mon-unbind-function',
`mon-unbind-variable', `mon-unbind-defun', `mon-compile-when-needed',
`mon-load-or-alert', `mon-byte-compile-and-load', `mon-dump-object-to-file',
`mon-nuke-and-eval', `mon-after-mon-utils-loadtime'.\n►►►"
  (interactive "CCommand: ")
  (fmakunbound cmd-symbol))
;; :KEEP-WITH-ABOVE
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
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el
;;; :SEE (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :WAS `ff/compile-when-needed' -> `mon-compile-when-needed'
;;; ==============================
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
              (let* ((src (concat dir "/" comp-fname)))
                (when (file-newer-than-file-p src (concat src "c"))
                  (message 
                   (concat ":FUNCTION `mon-compile-when-needed' "
                           (if (let ((byte-compile-verbose nil))
                                 (condition-case nil
                                     (byte-compile-file src)
                                   (error nil)))
                               "-- compiled: %s"
                             "-- failed compilation of: %s"))
                   src))))
          load-path)))

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :WAS `ff/load-or-alert'
;;; This is useful when using the same .emacs in many places.
;;; :MODIFICATIONS <Timestamp: #{2010-03-05T16:17:17-05:00Z}#{10095} - by MON KEY>
;;; ==============================
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
      (let ((buf (get-buffer-create "*MON-LOADING-WARNINGS*")))
        (display-buffer buf)
        (set-buffer buf)
        (insert 
         (propertize "Warning: :FUNCTION `mon-load-or-alert' -- " 
                     'face 'font-lock-warning-face 'fontified t)
         " could not load '" lib-name "'\n")
        (fit-window-to-buffer (get-buffer-window buf))
        (set-buffer-modified-p nil))
      nil))

;;; (cl-func cl-seq &rest cl-rest)
;; (apply 'nconc (apply 'mon-mapcar func cl-seq cl-rest))
;;; ==============================
(provide 'mon-utils)
;;; ==============================

(eval-after-load "mon-utils" 
  '(mon-utils-require-features-at-loadtime))

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ==============================
;;; mon-utils.el ends here
;;; EOF

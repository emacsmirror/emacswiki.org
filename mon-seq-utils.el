;;; mon-seq-utils.el --- procedures for frobbing sequences
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-seq-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-20T20:17:45-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-seq-utils provides procedures for frobbing sequences.
;;
;; FUNCTIONS:►►►
;; `mon-set-difference'
;; `mon-intersection'
;; `mon-maptree'
;; `mon-combine'
;; `mon-mismatch'
;; `mon-every'
;; `mon-transpose'
;; `mon-remove-if-not'
;; `mon-remove-if'
;; `mon-delete-if'
;; `mon-member-if'
;; `mon-union'
;;
;; `%mon-list-reorder'
;;
;; `mon-list-add-non-nil'
;; `mon-list-ensure'
;; `mon-list-match-tails'
;; `mon-list-filter'
;; `mon-remove-dups'
;; `mon-list-make-unique'
;; `mon-list-last'
;; `mon-delete-first'
;; `mon-deleql-dups'
;; `mon-delq-dups'
;; `mon-list-shuffle-safe'
;; `mon-list-nshuffle'
;; `mon-delq-cons'
;; `mon-maybe-cons'
;; `mon-list-string-longest'
;;
;; `mon-sublist-gutted'
;; `mon-assoc-replace'
;; `mon-list-reorder'
;; `mon-list-merge'
;; `mon-recursive-apply'
;; `mon-list-flatten-rotated'
;; `mon-list-variant-forms'
;; `mon-flatten'
;; `mon-moveq'
;; `mon-list-intersperse'
;;
;; `mon-list-permute-1'
;; `mon-list-permute-variants'
;;
;; `mon-pairlis'
;; `mon-map-append'
;; `mon-sublist'
;; `mon-subseq'
;; `mon-maplist'
;; `mon-mapcon'
;; `mon-mapcan'
;; `mon-mapcar'
;; `mon-map'
;; `mon-mapl'
;; `mon-map1'
;; `mon-mapc'
;;
;; `mon-elt-<elt'
;; `mon-elt->elt'
;; `mon-elt-<'
;; `mon-elt->'
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
;; FACES:
;;
;; VARIABLES:
;; `*mon-seq-utils-xrefs*'
;;
;; GROUPS:
;; `mon-seq-utils'
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; <UNQUALIFIED-ALIAS>                  <PREFIX>-<NON-CORE-SYMBOL>
;;
;; `delq-dups'                       -> `mon-delq-dups' 
;;
;; <PREFIX>-<QUALIFIED>                 <CORE-SYMBOL>
;; `mon-delq-alist'                  -> `assq-delete-all'
;;
;; <PREFIX>-<QUALIFIED>                 <PREFIX>-<CORE-SYMBOL>
;; `mon-sort-alist'                  -> `edebug-sort-alist'
;; `mon-list-chop'                   -> `ido-chop'
;;
;; <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-string-longest-in-list'      -> `mon-list-string-longest'
;; `mon-seqeunce-reorder'            -> `mon-list-reorder'
;; `mon-reorder-vector'              -> `mon-list-reorder'
;; `mon-list-combine'                -> `mon-combine' 
;; `mon-list-deleql-dups'            -> `mon-deleql-dups'
;; `mon-list-delete-first'           -> `mon-delete-first'
;; `mon-list-delete-if'              -> `mon-delete-if'
;; `mon-list-delq-dups'              -> `mon-delq-dups'
;; `mon-list-flatten-rotated'        -> `mon-rotate-flatten-list'
;; `mon-list-intersect'              -> `mon-intersection'
;; `mon-list-member-if'              -> `mon-member-if'
;; `mon-list-nqueue'                 -> `mon-moveq'
;; `mon-list-recurse-apply'          -> `mon-recursive-apply'
;; `mon-list-remove-dups'            -> `mon-remove-dups'
;; `mon-list-remove-if'              -> `mon-remove-if'
;; `mon-list-remove-if-not'          -> `mon-remove-if-not'
;; `mon-list-set-diff'               -> `mon-set-difference'
;; `mon-list-union'                  -> `mon-union'
;; `mon-list-union'                  -> `mon-union'
;; `mon-map-combine'                 -> `mon-combine'
;; `mon-merge-list'                  -> `mon-list-merge'
;; `mon-delete-dups-eql'             -> `mon-deleql-dups'
;; `mon-flatten'                     -> `mon-list-flatten'
;; `mon-list-ify-bool-vector'        -> `mon-bool-vector-pp' 
;; `mon-bool-vector-to-list'         -> `mon-bool-vector-pp'
;; `mon-boolean-vector-to-list'      -> `mon-bool-vector-pp'
;; `mon-pop2'                        -> `cl-pop2'
;; `mon-list-pop2'                   -> `cl-pop2'
;; `mon-list-transpose'              -> `mon-transpose' 
;; `mon-list-permute-combine'        -> `mon-permute-combine'
;; `mon-list-permute-combine-1'      -> `mon-permute-combine-1'
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-variations'                  -> `mon-list-variant-forms'
;; `mon-permutations'                -> `mon-list-permute-variants'
;; `mon-perms'                       -> `mon-list-permute-1'
;;
;; MOVED:
;; `mon-list-variant-forms'          <- mon-name-utils.el
;; `mon-list-permute-variants'       <- mon-name-utils.el
;; `mon-list-permute-1'              <- mon-name-utils.el
;; `mon-permute-combine'             <- mon-name-utils.el
;; `mon-permute-combine-1'           <- mon-name-utils.el
;;
;; TODO:
;;
;; NOTES:
;; `mon-maptree' uses `flet' cl--every                  -> `every'
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;; Following functions were sourced from Steel Bank Common Lisp
;; :FILE sblc/src/code/list.lisp circa SBCL 1.0.42.*
;; :RENAMED-TO      :FROM
;; `mon-map1'    <- MAP1 
;; `mon-mapl'    <- MAPL
;; `mon-maplist' <- MAPLIST
;; `mon-mapcar'  <- MAPCAR 
;; `mon-mapcan'  <- MAPCAN
;; `mon-mapcon'  <- MAPCON
;; While some minor modifications were made w/re namespacing w/ additional Emacs
;; lispification to `mon-map1' the SBCL license is still applicable.  
;; :SEE (URL `http://www.sbcl.org/history.html')
;; :SEE (URL `http://sbcl.cvs.sourceforge.net/*checkout*/sbcl/sbcl/COPYING?revision=HEAD')
;;
;; The SBCL CREDITS file notes that much of the list.lisp functions can be
;; traced to Skef Wholey and the CMU SPICE lisp with additional code descending
;; from Joe Ginder and Carl Ebeling). It is unclear how much has changed since
;; that time.
;;
;;
;; URL: http://www.emacswiki.org/emacs/mon-seq-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-11-25T02:27:00-05:00Z}#{10476} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-seq-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-20T20:17:45-05:00Z}#{10466} - by MON KEY>
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

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))


;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T19:01:43-05:00Z}#{11022} - by MON KEY>
(defgroup mon-seq-utils nil
  "Customization group for variables and functions of :FILE mon-seq-utils.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag "\n:EMACSWIKI-FILE (URL `http://www.emacswiki.org/emacs/mon-seq-utils.el')" 
          "http://www.emacswiki.org/emacs/mon-seq-utils.el")
  :link '(emacs-library-link 
          :tag ":FILE mon-seq-utils.el" 
          "mon-seq-utils.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T19:01:45-05:00Z}#{11022} - by MON KEY>
(defcustom *mon-seq-utils-xrefs* 
  '(mon-elt-> mon-elt-< mon-elt->elt mon-elt-<elt mon-maybe-cons mon-delq-cons
    mon-list-nshuffle mon-list-shuffle-safe mon-delq-dups mon-deleql-dups
    mon-delete-first mon-list-last mon-list-make-unique mon-remove-dups
    mon-list-filter mon-list-match-tails mon-list-add-non-nil mon-list-ensure
    %mon-list-reorder mon-list-reorder mon-union mon-member-if mon-delete-if
    mon-remove-if-not mon-remove-if mon-intersection mon-set-difference
    mon-pairlis mon-map mon-map1 mon-mapc mon-mapcar mon-mapcan mon-mapl
    mon-mapcon mon-maplist mon-subseq mon-sublist mon-sublist-gutted
    mon-map-append mon-assoc-replace mon-moveq mon-flatten
    mon-list-flatten-rotated mon-transpose mon-list-intersperse mon-every
    mon-mismatch mon-maptree mon-recursive-apply mon-list-merge mon-combine
    mon-list-variant-forms mon-list-permute-variants mon-list-permute-1
    mon-list-permute-2 mon-permute-combine mon-permute-combine-1
    mon-list-string-longest mon-bool-vector-pp *mon-seq-utils-xrefs*)
  "Xrefing list of mon sequences related symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-seq-utils.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-buffer-utils-xrefs*',
`*mon-line-utils-xrefs*', `*mon-plist-utils-xrefs*' `*mon-seq-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-seq-utils
  :group 'mon-xrefs)

 
;;; ==============================
;;; :ELISP-RELATED
;;; ==============================

;;; ==============================
;;; :CHANGESET 2202 <Timestamp: #{2010-10-21T13:03:09-04:00Z}#{10424} - by MON KEY>
;;; :CREATED <Timestamp: Wednesday June 03, 2009 @ 06:31.33 PM - by MON KEY>
;;; ==============================
(defun mon-elt-> (w-after-lst after-elt w-insert-after-elt)
  "Insert W-INSERT-AFTER-ELT AFTER-ELT into W-AFTER-LST.\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-remove-if', `mon-mapcar'.\n►►►"
  (unless (mon-list-proper-p  w-after-lst) 
    (mon-list-proper-p-ERROR :w-error  t
                             :function "mon-elt->"
                             :locus    "w-after-lst"
                             :got-val  w-after-lst))
  (push w-insert-after-elt (cdr (member after-elt w-after-lst)))
  w-after-lst)
;;
(defun mon-elt-<  (w-before-lst before-elt w-insert-before-elt)
  "Insert W-INSERT-BEFORE-ELT before BEFORE-ELT in W-BEFORE-LST.\n
:EXAMPLE\n\n\(let \(\(mutate-lst  '\(a b c d\)\)\)
              \(mon-elt-< mutate-lst 'b '\(bu bb a\)\)
              mutate-lst\)\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-remove-if', `mon-mapcar'.\n►►►"
  (unless (mon-list-proper-p  w-before-lst) 
    (mon-format :w-fun  #'error 
                :w-spec '(":FUNCTION `mon-elt-<' " 
                          "-- arg W-BEFORE-LST does not satisfy `mon-list-proper-p', got: %S")
                :w-args w-before-lst))
  (nreverse (mon-elt-> (nreverse w-before-lst) before-elt w-insert-before-elt)))
;;
(defun mon-elt->elt (w-old-make-new-lst w-old-elt w-new-elt)
  "Set W-OLD-ELT to W-NEW-ELT in W-OLD-MAKE-NEW-LST.\n
:EXAMPLE\n\n\(mon-elt-<elt '\(a b c d\) 'd  'b\)\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-mapcar'.\n►►►"
  (unless (mon-list-proper-p  w-old-make-new-lst) 
    (mon-list-proper-p-ERROR :w-error  t
                             :function "mon-elt->elt" 
                             :locus    "w-old-make-new-lst"
                             :got-val  w-old-make-new-lst))
  (setcar (member w-old-elt w-old-make-new-lst) w-new-elt)
  w-old-make-new-lst)
;;
(defun mon-elt-<elt (w-exchange-lst w-exchange-from-elt w-exchange-to-elt) ;; (list el1 el2)
  "Exchange places of w-exchange-from-elt and w-exchange-to-elt in W-EXCHANGE-LST.\n
W-EXCHANGE-LST is a proper-list which satisfies the predicate `mon-list-proper-p'.
Signal an error when it is not.\n
Args W-EXCHANGE-FROM-ELT W-EXCHANGE-TO-ELT are existing elts in list which satisfy `member'
Signal an error when wither is not.\n
:EXAMPLE\n\n\(mon-elt-<elt '\(a b c d\) 'd  'b\)\n
:SEE-ALSO `mon-elt->', `mon-elt->elt', `mon-elt-<', `mon-elt-<elt',
`mon-sublist', `mon-sublist-gutted', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-intersection', `mon-remove-if', `mon-mapcar'.\n►►►"
  (or (and (not (mon-list-proper-p  w-exchange-lst))
           (mon-list-proper-p-ERROR :w-error  t
                                    :function "mon-elt-<elt"
                                    :locus    "w-exchange-lst"
                                    :got-val  w-exchange-lst))
      (and (null (member w-exchange-from-elt w-exchange-lst))
           (mon-format :w-fun  #'error 
                       :w-spec '(":FUNCTION `mon-elt-<elt' "
                                 "-- arg W-EXCHANGE-FROM-ELT not member " 
                                 "of W-EXCHANGE-LST, got: %S")
                       :w-args w-exchange-from-elt))
      (and (null (member w-exchange-to-elt  w-exchange-lst))
           (mon-format :w-fun  #'error 
                       :w-spec '(":FUNCTION `mon-elt-<elt' "
                                 "-- arg W-EXCHANGE-TO-ELT not member "
                                 "of W-EXCHANGE-LST, got: %S")
                       :w-args w-exchange-to-elt)))
  (mon-elt->elt w-exchange-lst w-exchange-from-elt w-exchange-to-elt)
  (mon-elt->elt w-exchange-lst w-exchange-to-elt w-exchange-from-elt))


;;; ==============================
;;; :COURTESY :FILE macroexp.el :WAS `maybe-cons'
;;; :CHANGESET 2017
;;; :CREATED <Timestamp: #{2010-07-31T16:27:59-04:00Z}#{10306} - by MON KEY>
(defun mon-maybe-cons (w-car w-cdr original-cons)
  "Return a consed pair W-CAR and W-CDR. 
If W-CAR and W-CDR are each cons cells of ORIGINAL-CONS return ORIGINAL-CONS.\n
If not, cons em up.\n
:EXAMPLE\n\n\(let \(\(oc '\(q . z\)\)\)
 `\(,\(mon-maybe-cons 'q  'z oc\) ,\(mon-maybe-cons 'q  '8 oc\) ,oc\)\)=n
:SEE-ALSO `mon-delq-cons', `mon-list-make-unique', `mon-list-match-tails',
`mon-list-reorder', `mon-list-proper-p', `mon-intersection', `mon-remove-if',
`mon-combine', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-mapcar'.\n►►►"
  (if (and (eq w-car (car original-cons)) 
           (eq w-cdr (cdr original-cons)))
      original-cons
    (cons w-car w-cdr)))

;;; ==============================
;;; :PREFIX "mdc-"
;;; :COURTESY :FILE lisp/format.el :WAS `format-delq-cons'
;;; :CHANGESET 2000 
;;; :CREATED <Timestamp: #{2010-07-27T16:38:27-04:00Z}#{10302} - by MON KEY>
(defun mon-delq-cons (w-cons w-list)
  "Remove the given CONS from LIST by side effect and return the new LIST.\n
:NOTE CONS may be the first elt of LIST, to ensure changing value of `foo' do:\n
 \(setq foo \(mon-delq-cons element foo\)\)\n
:SEE-ALSO `mon-delq-dups', `mon-remove-dups', `mon-remove-if',
`mon-list-make-unique', `mon-delq-alist' `mon-maybe-cons', `mon-list-match-tails',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle',
`mon-list-shuffle-safe', `mon-list-proper-p', `mon-intersection', `mon-combine',
`mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt'.\n►►►"
  (if (eq w-cons w-list)
      (cdr w-list)
    (let ((mdc-p w-list))
      (while (not (eq (cdr mdc-p) w-cons))
	(and (null mdc-p) 
             (mon-format :w-fun  #'error 
                         :w-spec '(":FUNCTION `mon-delq-cons' "
                                   "-- cell of W-LIST not an element, "
                                   "got: %S")
                         :w-args mdc-p))
	(setq mdc-p (cdr mdc-p)))
      ;; Now (cdr p) is the cons to delete
      (setcdr mdc-p (cdr w-cons))
      w-list)))

;;; ==============================
;;; :CHANGESET 2211
;;; :CREATED <Timestamp: #{2010-10-27T13:45:58-04:00Z}#{10433} - by MON KEY>
;; (unless (and (intern-soft "mon-list-get-non-zerop")
;;              (fboundp 'mon-list-get-non-zerop))
;; (defalias 'mon-list-get-non-zerop 'version-list-not-zero))

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
;;; :PREFIX "mlss-"
;;; :CREATED <Timestamp: #{2010-08-09T16:40:02-04:00Z}#{10321} - by MON>
(defun mon-list-shuffle-safe (list-to-shuffle &optional w-no-errors)
  "Shuffle contents of LIST-TO-SHUFFLE non-destructively.\n
When LIST-TO-SHUFFLE is non-nil, not a `type-of' cons or vector, or does not
satisfy the predicate `mon-list-proper-p' signal an error.\n
When optional arg W-NO-ERRORS is non-nil do not signal an error instead return nil.\n
:EXAMPLE\n\n\(let \(\(tst-mlss '\(a \(b . c\) q\)\)\)
  `\(:w-shuffle ,\(mon-list-shuffle-safe tst-mlss\) :w/o-shuffle ,tst-mlss\)\)\n
\(mon-list-shuffle-safe '\(a b c q\)\)\n
\(mon-list-shuffle-safe '\(a \(b . c\) q\)\)\n
\(mon-list-shuffle-safe  [a \(b . c\) q]\)\n
\(mon-list-shuffle-safe  [a \[b c\] q]\)\n
\(mon-list-shuffle-safe nil\)\n
\(mon-list-shuffle-safe '\(\)\)\n
;; :NOTE Following will fail:
\(mon-list-proper-p '\(a \(b . c\) . q\)\)\n
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
  (let* ((vflg (vectorp list-to-shuffle))
         (mlss-lst (or (and vflg (copy-tree list-to-shuffle t))
                       (copy-tree list-to-shuffle))))
    (if (null mlss-lst)
        mlss-lst
      (if (or vflg
              (and (not vflg)
                   (eq (type-of mlss-lst) 'cons)
                   (mon-list-proper-p mlss-lst)))
          (setq mlss-lst 
                (or (and vflg (mon-nshuffle-vector mlss-lst))
                    (append (mon-nshuffle-vector (vconcat mlss-lst)) nil)))
        (if w-no-errors
            nil
          ;; :NOTE what about`mon-list-proper-p-ERROR'
          (mon-format :w-fun  #'error  
                      :w-spec '(":FUNCTION `mon-list-shuffle-safe' "
                                "-- arg LIST-TO-SHUFFLE null or not "
                                "`mon-list-proper-p', got: %S")
                      :w-args list-to-shuffle))))))
;;
;;; :TEST-ME (let ((tst-mlss '(a (b . c) q)))
;;;          `(:w-shffl ,(mon-list-shuffle-safe tst-mlss) :w/o-shffl ,tst-mlss))

;;; ==============================
;;; :PREFIX "mdd-"
;;; :CHANGESET 2035
;;; :CREATED <Timestamp: #{2010-08-04T21:51:44-04:00Z}#{10313} - by MON KEY>
(defun mon-delq-dups (dup-list)
  "Like `delete-dups' but destructively removes `eq' duplicates from DUP-LIST.\n
Store the result in DUP-LIST and return it.  DUP-LIST must be a proper list.\n
Of several `eq' occurrences of an element in DUP-LIST, the first one is kept.\n
:EXAMPLE\n\n\(concat 
 \(mon-delq-dups \(append \(vconcat \"ABRAHADABRA\"\) nil\)\) 
 \"\"\)\n
:ALIASED-BY `delq-dups'
:ALIASED-BY `mon-list-delq-dups'\n
:SEE-ALSO `mon-delq-cons', `mon-delq-alist', `mon-remove-dups', `mon-remove-if',
`mon-list-make-unique', `mon-assoc-replace', `mon-list-match-tails',
`mon-list-reorder', `mon-maybe-cons', `mon-list-proper-p', `mon-intersection',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose',
`mon-flatten', `mon-recursive-apply', `mon-sublist', `mon-sublist-gutted',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt'.\n►►►"
  (let ((md-dq-tail dup-list))
    (while md-dq-tail
      (setcdr md-dq-tail (delq (car md-dq-tail) (cdr md-dq-tail)))
      (setq   md-dq-tail (cdr md-dq-tail))))
  dup-list)
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (string-equal 
;; |  (concat (mon-delq-dups (append (vconcat "ABRAHADABRA") nil))  "")
;; |  "ABRHD")
;; `----

;;; ==============================
;;; :PREFIX "mdql-"
;;; :CREATED <Timestamp: #{2010-09-21T19:49:18-04:00Z}#{10382} - by MON>
(defun mon-deleql-dups (dup-eql-list)
  "Destructively removes `eql' duplicates from DUP-EQL-LIST.\n
Like `mon-delq-dups' and `delete-dups' but only deletes elts from list that 
are the same Lisp object.\n
:EXAMPLE\n\n
\(let \(\(cs \(copy-sequence \"q\"\)\)\)
  \(mon-deleql-dups `\(a 2.2 ,cs b q 3.3 ,cs f d e 3.3 e 1 a d  2.2 f 1 g\)\)\)\n
\(mon-deleql-dups `\(a 2.2 \"q\" b q 3.3 \"q\" f d e 3.3 e 1 a d  2.2 f 1 g\)\)\n
\(length `\(\(a a\) #2=\(#1=a #1#\) #2#\)\)\n
\(length \(mon-deleql-dups `\(\(a a\) #2=\(#1=a #1#\) #2#\)\)\)\n
:NOTE In the second example the \"q\" strings are no eql.\n
:ALIASED-BY `mon-delete-dups-eql'
:ALIASED-BY `mon-list-deleql-dups'\n
:SEE-ALSO `mon-delete-first', `mon-list-last', `mon-delete-if',
`mon-remove-if-not', `mon-remove-dups'.\n►►►"
  (let (mdql-gthr-eql mdql-fnlz-eql)
    (while dup-eql-list
      (let* ((mdql-mmql-pop (car (push (pop dup-eql-list) mdql-gthr-eql)))
             (mdql-is-mmql (memql mdql-mmql-pop dup-eql-list)))
        (if (null mdql-is-mmql)
            (push (pop mdql-gthr-eql) mdql-fnlz-eql)
          (while (and mdql-is-mmql (memql mdql-mmql-pop mdql-is-mmql)) 
            (let ((mdql-in-mmql (memql mdql-mmql-pop mdql-is-mmql)))
              (when mdql-in-mmql (setq mdql-is-mmql (cdr mdql-is-mmql)))
              (when (null (memql mdql-mmql-pop dup-eql-list))
                (push (pop mdql-gthr-eql) mdql-fnlz-eql)))))))
    (setq dup-eql-list (nreverse mdql-fnlz-eql))))

;;; ==============================
;;; :PREFIX "mdf-"
;;; :COURTESY bytecomp.el, gnus-util.el  
;;; :WAS `byte-compile-delete-first' :WAS `gnus-delete-first'
(defun mon-delete-first (list-elt in-list &optional w-pred)
  "Delete by side effect the first `eq' occurrence of LIST-ELT IN-LIST.\n
Optional arg W-PRED is a predicate symbol one of `equal', `eql', or `eq' default
is `eq'.
:EXAMPLE\n\n \(let \(\(mdf '\(\(a b c\) b\)\)\) \(mon-delete-first '\(a b c\) mdf\) mdf\)\n
:ALIASED-BY `mon-list-delete-first'\n
:SEE-ALSO `mon-delq-dups', `mon-deleql-dups', `mon-list-last', `mon-delete-if',
`mon-remove-if-not', `mon-remove-dups', `mon-list-make-unique',  `mon-list-sift',
`mon-equality-or-predicate', `mon-equality-for-type', `car-less-than-car'.\n"
  ;; :NOTE `defsubst'd in bytecomp.el
  ;; (byte-compile-delete-first list-elt in-list))
  (if ;; :WAS (eq (car in-list) list-elt)
      (case w-pred
        (equal (equal (car in-list) list-elt))
        (eql   (eql (car in-list) list-elt))
        (t     (eq (car in-list) list-elt)))
      ;; :WAS (cdr in-list)
      (setq in-list (cdr in-list))
    (let ((mdf-total in-list))
      (while (and (cdr in-list)
                  ;; :WAS (not (eq (cadr in-list) list-elt))
                  (not (case w-pred
                         (equal (equal (cadr in-list) list-elt))
                         (eql   (eql   (cadr in-list) list-elt))
                         (t     (eq    (cadr in-list) list-elt)))))
        (setq in-list (cdr in-list)))
      (when (cdr in-list)
        (setcdr in-list (cddr in-list)))
      mdf-total))
  in-list)

;;; ==============================
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T17:18:18-04:00Z}#{10381} - by MON KEY>
(defun mon-list-last (in-list) ;; :NOTE Can't `defsubst' inline b/c it recurses.
  "Return the last element IN-LIST.\n
When IN-LIST satisfies `listp' and `mon-list-proper-p' return the car of last
elt in list else return the cdr of IN-LIST.\n
When IN-LIST does not satisfy `listp' return IN-LIST.\n
:EXAMPLE\n\n\(mon-list-last '\(a . b\)\)\n
\(mon-list-last '\(a b\)\)\n
\(mon-list-last '\(a b \"c\"\)\)\n
\(mon-list-last '\(a d a b . \"e\"\)\)\n
\(mon-list-last nil\)\n
:NOTE Contrast to `last' which when IN-LIST is a dotted list does not returns
the dotted list at tail not the atom in cdr of dotted list, e.g.\n
 \(last '\(a d \(a b . \"e\"\)\)\)\n
 \(mon-list-last '\(a d \(a b . \"e\"\)\)\)\n
 \(last '\(a d . \(a b . \"e\"\)\)\)\n
 \(mon-list-last '\(a d . \(a b . \"e\"\)\)\)\n
 \(last '\(\"e\" nil\)\)\n
 \(mon-list-last '\(\"e\" nil\)\)\n
 \(last '\(nil \"e\"\)\)\n
 \(mon-list-last '\(nil \"e\"\)\)\n
:SEE-ALSO `mon-list-sift', `mon-list-filter', `mon-delete-first',
`mon-list-proper-p', `mon-sequence-mappable-p'.\n►►►"
  (if (listp in-list)
      (cond ((mon-list-proper-p  in-list)
             (car (last in-list)))
            (t (if (atom in-list)
                   in-list
                 (progn (while (consp in-list)
                          (unless (or (null (cdr in-list))
                                      (atom in-list))
                            (setq in-list (cdr in-list)))
                          (setq in-list (mon-list-last in-list)))
                        in-list))))
    in-list))

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
;;; :PREFIX "mrd-"
;;; :COURTESY Jared D. :WAS `remove-dupes'
;;; (URL `http://curiousprogrammer.wordpress.com/2009/07/26/emacs-utility-functions/')
;;; :CREATED <Timestamp: #{2009-08-19T20:10:43-04:00Z}#{09344} - by MON KEY>
(defun mon-remove-dups (maybe-twins-lst)
  "Remove duplicate adjoining elts in MAYBE-TWINS-LST.\n
:EXAMPLE\n\n\(mon-remove-dups '\(a a b b c a c c d\)\)\n
:ALIASED-BY `mon-list-remove-dups'\n
:SEE-ALSO `mon-list-make-unique', `mon-delq-dups', `mon-delq-alist',
`mon-delete-first', `mon-delq-cons', `mon-remove-if', `mon-intersection',
`mon-combine', `mon-map-append', `mon-mapcar', `mon-maptree', `mon-transpose',
`mon-flatten', `mon-recursive-apply', `mon-sublist', `mon-sublist-gutted',
`mon-remove-dups', `mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<',
`mon-elt->elt', `mon-elt-<elt', `mon-list-match-tails', `mon-list-reorder',
`mon-list-proper-p', `mon-maybe-cons'.\n►►►"
  (let (mrd-tmp-list mrd-head)
    (while maybe-twins-lst
      (setq mrd-head (pop maybe-twins-lst))
      (unless (equal mrd-head (car maybe-twins-lst))
        (push mrd-head mrd-tmp-list)))
    (reverse mrd-tmp-list)))
;;
;; :TEST-ME (mon-remove-dups '(a a b b c a c c d))

;;; ==============================
;;; :PREFIX "mrf-"
;;; :COURTESY :FILE pcomplete.el :WAS `pcomplete-pare-list'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T17:19:26-04:00Z}#{10381} - by MON KEY>
(defun mon-list-filter (from-lst match-lst &optional w-pred)
  "Destructively remove FROM-LST all elements matching any in MATCH-LST.
Return the resultant list.\n
Test is as if by `equal'.
When W-PRED is non-nil, it is a function which further filters FROM-LST for removal.\n
:EXAMPLE\n\n\(let \(\(l1 '\(a b c q e d 1\)\)
      \(l2 '\(q e d \"1\"\) \)\)
`\(:l1-filtered ,\(mon-list-filter l1 l2\) :l1-modified ,l1\)\)\n
\(let \(\(l1 '\(a b c q e d 1\)\)
      \(l2 '\(q e d \"1\"\) \)\)
`\(:l1-filtered-w-pred 
  ,\(mon-list-filter l1 l2 #'\(lambda \(ch\) \(and \(> \(mon-char-code ch\) 97\)\)\)\)
   :l1-modified ,l1\)\)\n
:SEE-ALSO `mon-list-sift', `mon-list-last', `mon-list-match-tails'.\n►►►"
  (while (and from-lst (or (and match-lst (member (car from-lst) match-lst))
                           (and w-pred
                                (funcall w-pred (car from-lst)))))
    (setq from-lst (cdr from-lst))) 
  (let ((mlf-w-match from-lst))
    (while mlf-w-match
      (while (and (cdr mlf-w-match)
		  (or (and match-lst (member (cadr mlf-w-match) match-lst))
		      (and w-pred
			   (funcall w-pred (cadr mlf-w-match)))))
	(setcdr mlf-w-match (cddr mlf-w-match)))
      (setq mlf-w-match (cdr mlf-w-match))))
  from-lst)

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
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `car-less-than-car'.\n►►►"
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
;;; :PREFIX "mlann-"
;;; :CHANGESET 2119
;;; :CREATED <Timestamp: #{2010-09-13T14:58:49-04:00Z}#{10371} - by MON KEY>
(defun mon-list-add-non-nil (add-elts-to add-elts-frm)
  "Add non-nil elements to tail of list.\n
ADD-ELTS-TO list with ADD-ELTS-FRM list.\n
:EXAMPLE\n\n\(let \(\(atl  '\(8 9 10\)\)
      \(afl  '\(a nil b nil c nil d\)\)\)
  `\(:ADD-ELTS-TO ,atl 
    :ADD-ELTS-FRM ,afl
    :RETURN ,\(mon-list-add-non-nil atl afl\)\)\)\n
:SEE-ALSO `mon-list-last', `mon-delete-first', `mon-list-make-unique',
`mon-delq-dups', `mon-list-proper-p', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe', `mon-intersection',
`mon-remove-if', `mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-recursive-apply', `mon-maybe-cons',
`mon-delq-cons', `mon-remove-dups', `mon-sublist', `mon-sublist-gutted',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt'.\n►►►"
  (let ((mlann-atl 
         (and (not (unless (consp add-elts-to)
                     (mon-format :w-fun  #'error 
                                 :w-spec '(":FUNCTION `mon-list-add-non-nil' " 
                                           "-- arg ADD-ELTS-TO does not satisfy `consp', "
                                           "got: %S type-of: %S")
                                 :w-args `(,add-elts-to ,(type-of add-elts-to)))))
              (reverse add-elts-to)))
        (mlann-afl 
         (and (not (unless (consp add-elts-frm)
                     (mon-format :w-fun  #'error 
                                 :w-spec '(":FUNCTION `mon-list-add-non-nil' " 
                                           "-- arg ADD-ELTS-FRM does not satisfy `consp', "
                                           "got: %S type-of: %S")
                                 :w-args `(,add-elts-frm ,(type-of add-elts-frm)))))
              add-elts-frm)))
    (dolist (mlann-i  mlann-afl (nreverse mlann-atl))
      (unless (car (push mlann-i mlann-atl))
        (pop mlann-atl)))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (let ((atl  '(8 9 10)) (afl  '(a nil b nil c nil d)))
;; |   `(:ADD-ELTS-TO ,atl :ADD-ELTS-FRM ,afl 
;; |     :RETURN ,(mon-list-add-non-nil atl afl)))
;; `----


;;; ==============================
;;; :COURTESY lisp/erc/erc.el :WAS `erc-list' -- use or/and/nlistp instead of if/listp
;;; :CHANGESET 2367
;;; :CREATED <Timestamp: #{2010-12-17T12:42:55-05:00Z}#{10505} - by MON KEY>r
(defun mon-list-ensure (obj-or-lst)
  "Return OBJ-OR-LST as a list, when OBJ-OR-LST is `nlistp' make it a list.\n
:EXAMPLE\n\n\(mon-list-ensure '\(a\)\)\n
\(mon-list-ensure 'a\)\n
\(mon-list-ensure  \(mon-list-ensure 'a\)\)\n
\(mon-list-ensure '\(\)\)\n
\(mon-list-ensure nil\)\n
\(mon-list-ensure \(mon-list-ensure nil\)\)\n
:SEE-ALSO `mon-list-proper-p', `mon-list-add-non-nil', `mon-maybe-cons',
`mon-delq-cons', `consp', `listp', `nlistp', `atom'.\n►►►"
  (or (and (nlistp obj-or-lst)
           (list obj-or-lst))
      obj-or-lst))


;;; ==============================
;;; :PREFIX "mlr-"
;;; :COURTESY :FILE lisp/format.el :WAS `format-reorder'
;;; :CHANGESET 2233
;;; :ADDED Ability to support vectors transparently.
;;; :CHANGESET 2001
;;; :CREATED <Timestamp: #{2010-10-29T13:17:11-04:00Z}#{10435} - by MON KEY>
(defun %mon-list-reorder (list-items list-order &optional remv-dups)
  "Build the partial order return value for `mon-list-reorder'.\n 
Args LIST-ITEMS and LIST-ORDER are actual proper-lists.\n
Return value from `mon-list-reorder' is coerced back to vector when SEQ-ITEMS
arg was `vectorp'.\n
:SEE-ALSO `mon-list-reorder-TEST'.\n►►►"
  (let ((mlr-lst-itm  list-items)
        (mlr-lst-ordr list-order)
        mlr-rtn)
    (setq mlr-rtn
          (if mlr-lst-ordr
              (let ((mlr-itm (member (car mlr-lst-ordr) mlr-lst-itm)))
                (if mlr-itm
                    (cons (car mlr-itm)
                          (mon-list-reorder (mon-delq-cons mlr-itm mlr-lst-itm)
                                            (cdr mlr-lst-ordr)))
                  (mon-list-reorder mlr-lst-itm (cdr mlr-lst-ordr))))
            mlr-lst-itm))
    (if remv-dups
        (delete-dups mlr-rtn)
      mlr-rtn)))

;;; ==============================
;;; :CHANGESET 2233
;;; :CREATED <Timestamp: #{2010-07-27T16:47:20-04:00Z}#{10302} - by MON KEY>
(defun mon-list-reorder (seq-items seq-order &optional remv-dups)
  "Arrange SEQ-ITEMS to follow partial order of SEQ-ORDER.\n
Arrange elts of SEQ-ITEMS `equal' to elts of SEQ-ORDER to follow SEQ-ORDER.\n
Any unmatched elts of SEQ-ITEMS will occur last in return value \(including
duplicate elts\).\n
Args SEQ-ITEMS and SEQ-ORDER are either proper-lists or vectors and should
satisfy either `mon-list-proper-p' or `vectorp', signal an error if not.\n
When SEQ-ITEMS is `vectorp' return a vector with partial order of SEQ-ORDER.\n
When SEQ-ITEMS is null return nil.\n
When SEQ-ORDER is null return SEQ-ITEMS.\n
When optional arg REMV-DUPS is non-nil return value with duplicate elements
removed \(which may be useful when the two sets are arbitrarily disjoint\).\n
Abstractly, when elt in SEQ-ORDER has a match in SEQ-ITEMS the order of elt in
SEQ-ORDER takes precedence else default to elt at index in SEQ-ITEMS, e.g.:\n
 \(mon-list-reorder 
   ; 0 1 2  3      4 5 6 7   ; index \(imaginary\)
   '\(2 6 3 \(a . b\) 1 7 7 7\)  ; seq-items
   '\(1 2 3  4      5 6 7 8\)\) ; seq-order\n
;=> \(1 2 3 6 7 \(a . b\) 7 7\)
     0 1 2 3 4  5      6 7   ; return value index 
     5 0 2 1 6  3      6 7   ; seq-items index
     4 1 2 5 5  -      6 -   ; seq-order index\n
:EXAMPLE\n\n\(mon-list-reorder '\(2 6 3 2 1\) '\(1 2 3 4 5 6\)\)\n
\(mon-list-reorder '\(q w b c s a w\) '\(a b c q z w\)\)\n
\(mon-list-reorder '(q w b c s a w) '(a b c q z w) t)\n
\(mon-list-reorder '\(q w \"string\" b c a w\) '\(a \"string\" b c q z w\)\)\n
\(mon-list-reorder '\(\(e . g\) q w b c s a w\) '\(a b c q z w\)\)\n
\(mon-list-reorder '\(q w [1 2 3] b c a w\) '\(a b c q z w\)\)\n
\(mon-list-reorder [a b q z w c] '\(a b c q z w\)\)\n
\(mon-list-reorder [a b q z w c] '[a b c q z w]\)\n
\(mon-list-reorder '\(a b q z w c\) '[a b c q z w]\)\n
\(mon-list-reorder nil nil\)\n
\(mon-list-reorder nil '\(z z a b z c q w z\)\)\n
\(mon-list-reorder '\(z z a b z c q w z\) nil\)\n
\(mon-list-reorder '\(z z a b z c q w z\) nil t\)\n
\(mon-list-reorder [z z a b z c q w z] nil\)\n
\(mon-list-reorder [z z a b z c q w z] nil t\)\n
\(mon-list-reorder-TEST\)\n
:NOTE Actual sequence reordering performed by `%mon-list-reorder'.\n
:ALIASED-BY `mon-sequence-reorder'
:ALIASED-BY `mon-reorder-vector'\n
:SEE-ALSO `mon-list-reorder-TEST', `mon-nshuffle-vector', `mon-list-nshuffle',
`mon-list-shuffle-safe', `mon-maybe-cons', `mon-delq-cons',
`mon-list-make-unique', `mon-delq-dups', `mon-list-match-tails',
`mon-list-proper-p', `mon-intersection', `mon-remove-if', `mon-combine',
`mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `car-less-than-car'.\n►►►"
  (let (mlr-vec-flag)
    (if ;; Both SEQ-ITEMS/SEQ-ORDER or SEQ-ITEMS null. Don't bother.
        (or (and (not seq-items) (not seq-order)) (not seq-items))
        nil
      (and 
       ;; Take a gander at SEQ-ITEMS and record its type 'vec or '(...)      
       (or 
        ;; SEQ-ITEMS might be `vectorp'. If so, store that for later.
        (setq mlr-vec-flag (and (vectorp seq-items) 'vec))
        ;; SEQ-ITEMS can't be nil but could be a consed pair.
        ;; If its not, we've got a proper list, store that for later.
        (setq mlr-vec-flag (and (mon-list-proper-p seq-items) seq-items))
        ;; Its either a consed pair or something else. So, bail.
        (mon-format :w-fun #'error 
                    :w-spec '(":FUNCTION `mon-list-reorder' "
                              "-- arg SEQ-ITEMS not `mon-list-proper-p', "
                              "got: %S type-of: %S")
                    :w-args `(,seq-items ,(cdr (mon-sequence-mappable-p seq-items t t)))))
       (or ;; When SEQ-ORDER is null return will always be SEQ-ITEMS so return.
        ;; (and (null seq-order)  seq-items) 
        ;; But, make sure to remove dups when REMV-DUPS is non-nil.
        (and (null seq-order)
             (or (and remv-dups
                      (setq seq-items 
                            (or (and (eq mlr-vec-flag 'vec)
                                     (vconcat (delete-dups (append seq-items nil))))
                                (delete-dups seq-items))))
                 seq-items))
        (and 
         ;; Its not null. Sould now be either `vectorp' or `mon-list-proper-p'.
         (or 
          ;; Check if SEQ-ORDER is `vectorp'. If so, coerce to list. 
          (and (vectorp seq-order) (setq seq-order (append seq-order nil)))
          (or 
           ;; Make sure its not a consed pair. 
           (mon-list-proper-p seq-order) 
           ;; Anything else signals an error.
           (mon-format :w-fun #'error 
                       :w-spec '(":FUNCTION `mon-list-reorder' "
                                 "-- arg SEQ-ORDER not `mon-list-proper-p', "
                                 "got: %S type-of: %S")
                       :w-args `(,seq-order ,(cdr (mon-sequence-mappable-p seq-order t t))))))
         ;; Unless we signaled above don't stop inside the `and'.
         nil)
        ;; We now know we have sequences, no consed pairs or null values.
        (and  
         (or 
          ;; Check if SEQ-ITEMS is `vectorp'. If so, coerce to list and move on.
          (and (eq mlr-vec-flag 'vec) (setq seq-items (append seq-items nil))) t)
         (or 
          ;; When SEQ-ITEMS is `vectorp', return a vector.  We're done.
          (and (eq mlr-vec-flag 'vec) 
               (vconcat (%mon-list-reorder seq-items seq-order remv-dups)))
          ;; Args are `mon-list-proper-p', w/ `mlr-vec-flag` holding SEQ-ITEMS.
          (%mon-list-reorder mlr-vec-flag  seq-order remv-dups))))))))
;;
;;; :TEST-ME (mon-list-reorder-TEST)

;;; ==============================
;;; :CHANGESET 2178
;;; :CREATED <Timestamp: #{2010-10-04T22:35:56-04:00Z}#{10401} - by MON KEY>
(defun mon-union (lst-1 lst-2 &optional predicate)
  "Like `union' but without the CL keywords.\n
Combine lst1 and lst2 using a set-union operation.\n
The result list contains all items that appear in either lst1 or lst2.\n
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original lst1 and lst2.\n
:EXAMPLE\n\n\(mon-union '\(a b c d e\) '\(f q g h\)\)\n
\(mon-union '\(a b c d e\) nil\)\n
\(mon-union '\(c b a \"a\" d e\)  '\(\"a\" \"b\" \"c\" \"d\" \"e\"\)\)\n
\(mon-union '\(c b a \"a\" d e\)  '\(\"a\" \"b\" \"c\" \"d\" \"e\"\)
           #'\(lambda \(q y\) \(and \(stringp q\) \(not \(stringp y\)\)\)\)\)\n
\(let \(\(L1 '\(a b c d e\)\)
      \(L2 '\(f q g h\)\)\)
  `\(,\(mon-union L1 L2\) ,L1 ,L2\)\)\n
:ALIASED-BY `mon-list-union'\n
:SEE-ALSO `mon-intersection', `mon-member-if', `mon-equality-or-predicate',
`cvs-union', `car-less-than-car'.\n►►►"
  (or (and (null lst-1) lst-2)
      (and (null lst-2) lst-1)
      (and (equal lst-1 lst-2) lst-1)
      (let (pred)        
        (or (>= (length lst-1) (length lst-2))
            (setq lst-1 (prog1 lst-2 (setq lst-2 lst-1))))
        (while lst-2
          (or (mon-equality-or-predicate (or predicate 'memq) (car lst-2) lst-1)
              ;; (memq (car lst-2) lst-1)
              (push (car lst-2) lst-1))
          (pop lst-2))
        lst-1)))

;;; ==============================
;;; :PREFIX "mmi-"
;;; :COURTESY lisp/erc/erc-compat.el :WAS `erc-member-if'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T20:41:52-04:00Z}#{10381} - by MON KEY>
(defun mon-member-if (predicate in-list)
  "Find the first item satisfying PREDICATE in IN-LIST.\n
Return the sublist of IN-LIST whose car matches.\n
:EXAMPLE\n\n\(mon-member-if #'\(lambda \(p\) \(>= p 6\)\) '\(3 2 6 7\)\)\n
:NOTE This is a keywordless replacement for CL's `member-if'.\n
:ALIASED-BY `mon-list-member-if'\n
:SEE-ALSO `mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan', `mon-mapcon',
`mon-remove-if', `mon-remove-if-not', `mon-delete-if', `mon-char-code',
`mon-union', `mon-intersection'.\n►►►"
  ;; :TODO incorporate `mon-function-object-p'/`mon-mappable-sequence-p'
  (unless (functionp predicate)
    (mon-format :w-fun #'error 
                :w-spec '(":FUNCTION `mon-member-if' "
                          "-- arg PREDICATE does not satisfy `functionp', "
                          "got: %S type-of: %S")
                :w-args  `(,predicate ,(mon-function-object-p predicate))))
  (let ((mmi-ptr in-list))
    (catch 'mmi-found
      (while mmi-ptr
	(when (funcall predicate (car mmi-ptr))
	  (throw 'mmi-found mmi-ptr))
	(setq mmi-ptr (cdr mmi-ptr))))))

;;; ==============================
;;; :COURTESY lisp/erc/erc-compat.el :WAS `erc-delete-if'
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T20:41:43-04:00Z}#{10381} - by MON KEY>
(defun mon-delete-if (predicate in-seq)
  "Remove all items satisfying PREDICATE in IN-SEQ.\n
This is a destructive function: it reuses the storage of IN-SEQ
whenever possible.\n
:EXAMPLE\n\n\(mon-delete-if 'stringp '\(\"a\" \"b\" d q \(a \"a\" \"b\"\)\)\)
:NOTE This is a keywordless replacement for CL's `delete-if'.
:ALIASED-BY `mon-list-delete-if'\n
:SEE-ALSO `mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan', `mon-mapcon',
`mon-remove-if', `mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-char-code', `mon-subseq', `car-less-than-car'.\n►►►"
  ;; :TODO incorporate `mon-function-object-p'/`mon-mappable-sequence-p'
  (unless (functionp predicate)
    (mon-format :w-fun #'error 
                :w-spec '(":FUNCTION `mon-delete-if' "
                          "-- arg PREDICATE does not satisfy `functionp', "
                          "got: %S type-of: %S")
                :w-args  `(,predicate ,(mon-function-object-p predicate))))
  ;; remove from car
  (while (when (funcall predicate (car in-seq))
	   (setq in-seq (cdr in-seq))))
  ;; remove from cdr
  (let ((mdi-ptr in-seq)
	(mdi-nxt (cdr in-seq)))
    (while mdi-nxt
      (when (funcall predicate (car mdi-nxt))
	(setcdr mdi-ptr (if (consp mdi-nxt)
                            (cdr mdi-nxt)
                          nil)))
      (setq mdi-ptr (cdr mdi-ptr))
      (setq mdi-nxt (cdr mdi-ptr))))
  in-seq)

;;; ==============================
;;; :PREFIX "mrin-"
;;; :COURTESY lisp/erc/erc-compat.el :WAS erc-remove-if-not
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T20:41:35-04:00Z}#{10381} - by MON KEY>
(defun mon-remove-if-not (predicate in-seq)
  "Remove all items not satisfying PREDICATE IN-SEQ.\n
This is a non-destructive function; it makes a copy of IN-SEQ to
avoid corrupting IN-SEQ.\n
:EXAMPLE\n\n
:NOTE This is a keywordless replacement for CL's `remove-if-not'.\n
:ALIASED-BY `mon-list-remove-if-not'\n
:SEE-ALSO `mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan', `mon-mapcon',
`mon-remove-if', `mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-subseq', `mon-intersection', `mon-char-code', `car-less-than-car'.\n►►►"
  ;; :TODO incorporate `mon-function-object-p'/`mon-mappable-sequence-p'/`mon-equality-or-predicate'
  (unless (functionp predicate)
    (mon-format :w-fun #'error 
                :w-spec '(":FUNCTION `mon-remove-if-not' "
                          "-- arg PREDICATE does not satisfy `functionp', "
                          "got: %S type-of: %S")
                :w-args  `(,predicate ,(mon-function-object-p predicate))))
  (let (mrin-seq)
    (dolist (mrin-el in-seq)
      (when (funcall predicate mrin-el)
	(setq mrin-seq (cons mrin-el mrin-seq))))
    (nreverse mrin-seq)))

;;; ==============================
;;; :PREFIX "mri-"
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
:NOTE This is a keywordless replacement for CL's `remove-if'.\n
:ALIASED-BY `mon-list-remove-if'\n
:SEE-ALSO `mon-intersection', `mon-remove-if', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-sublist', `mon-sublist-gutted', `mon-remove-dups',
`mon-assoc-replace', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-delq-cons', `mon-list-make-unique', `mon-list-match-tails',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-list-reorder', `mon-list-proper-p', `mon-maybe-cons',
`mon-delq-cons', `car-less-than-car'.\n►►►"
  ;; :TODO incorporate `mon-function-object-p'/`mon-mappable-sequence-p'/`mon-equality-or-predicate'
  (unless (functionp rmv-if-predicate)
    (mon-format :w-fun #'error 
                :w-spec '(":FUNCTION `mon-remove-if' "
                          "-- arg RMV-IF-PREDICATE does not satisfy `functionp', "
                          "got: %S type-of: %S")
                :w-args  `(,rmv-if-predicate ,(mon-function-object-p rmv-if-predicate))))
  (let (mri-new-list)
    (dolist (mri-item rmv-list (setq mri-new-list (nreverse mri-new-list)))
      (when (not (funcall rmv-if-predicate mri-item))
	(push mri-item mri-new-list)))))
;;
;;; :TEST-ME (let ((mri-l (number-sequence 0 10)))
;;;             (mon-remove-if #'(lambda (chk-it) (oddp chk-it)) mri-l))
;;; :WANTING (0 2 4 6 8 10)

;;; ==============================
;;; :PREFIX "mintr-"
;;; :CREATED <Timestamp: #{2010-01-22T15:12:02-05:00Z}#{10035} - by MON>
(defun mon-intersection (list1 list2 &optional do-eql do-eq)
  "Combine LIST1 and LIST2 using a set-intersection operation.\n
The result list contains all items that appear in both LIST1 and LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.\n
By default comparsion made as with `member'.\n
When optional arg DO-EQl uses `memql'.\n
When optional arg DO-EQ uses `memq'.\n
:EXAMPLE\n
\(mon-intersection \(number-sequence 8 20 2\) \(number-sequence 0 20 4\)\)\n
\(mon-intersection \(number-sequence 8 20 2\) \(number-sequence 8 20 2\)\)\n
\(mon-intersection \(number-sequence 8 20 2\) nil\)\n
\(mon-intersection nil \(number-sequence 8 20 2\)\)\n
\(mon-intersection nil nil\)\n
\(mon-intersection '\(\"str1\" sym1 \"str2\"\) '\(sym1 \"str2\"\)\)       ;`member'\n
\(mon-intersection '\(\"str1\" sym1 \"str2\"\) '\(sym1 \"str2\"\) t\)     ;`memql'\n
\(mon-intersection '\(\"str1\" \"str2\"\) '\(\"str2\"\) t\)               ;`memql'\n
\(mon-intersection '\(\"str1\" sym1 \"str2\"\) '\(sym1 \"str2\"\) nil t\) ;`memq'\n
\(mon-intersection '\(\"str1\" \"str2\"\) '\(\"str2\"\) nil t\)           ;`memq'\n
\(mon-intersection '\(sym1 sym2\) '\(sym1\)\)                       ;`memq'\n
;; :Following fail successfully
\(mon-intersection '\(a . b\)   '\(q e d a\)\)\n
\(mon-intersection '\(q e d a\) '\(a . b\)\)\n
\(mon-intersection \(number-sequence 8 20 2\) 8)\n
:NOTE Like `intersection' from :FILE cl-seq.el adapted for use without keywords
and does not provide intelligent type checking.\n
:ALIASED-BY `mon-list-intersect'\n
:SEE-ALSO `mon-set-difference', `mon-remove-if', `mon-mapcar', `mon-map-append',
`mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan', `mon-mapcon',
`mon-remove-if-not', `mon-delete-if', `mon-member-if', `mon-subseq',
`mon-char-code' `mon-sublist', `mon-sublist-gutted', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `mon-delq-cons',
`mon-list-make-unique', `mon-list-match-tails', `mon-list-reorder',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe',
`mon-list-proper-p', `hfy-interq', `smtpmail-intersection', `car-less-than-car'.\n►►►"
  ;; :TODO Use `mon-list-proper-p'/`mon-sequence-mappable-p' here instead.
  ;; :WAS (unless (and (or (consp list1) (null list1))
  ;;              (or (consp list2) (null list2)))
  ;;   (error (concat ":FUNCTION `mon-intersection' "
  ;;                  "-- args LIST1 and LIST2 must be either a list or nil, " 
  ;;                  " LIST1 got: %S LIST2 got: %S")
  ;;          list1 list2))
  (and (or (mon-list-proper-p list1)
           (mon-list-proper-p-ERROR :w-error t
                                    :fun-name "mon-intersection"
                                    :locus "list1"
                                    :got-val list1))
       (or (mon-list-proper-p list2)
           (mon-list-proper-p-ERROR :w-error t
                                    :fun-name "mon-intersection"
                                    :locus "list2"
                                    :got-val list2)))
  (and list1 list2
       (if (equal list1 list2)
           list1
         (let ((mintr-res nil)
               (mintr-l1 list1)
               (mintr-l2 list2)
               (mintr-cmpare-w #'(lambda (mintr-L-1 mintr-L-2) 
                                   (if (or do-eql do-eq)
                                       (cond (do-eql (memql mintr-L-1 mintr-L-2))
                                             (do-eq  (memq mintr-L-1 mintr-L-2)))
                                     (member mintr-L-1 mintr-L-2)))))
           (or (>= (length mintr-l1) (length mintr-l2))
               (setq mintr-l1 (prog1 mintr-l2 (setq mintr-l2 mintr-l1))))
           (while mintr-l2
             (when (funcall mintr-cmpare-w (car mintr-l2) mintr-l1)
               (push (car mintr-l2) mintr-res))
             (pop mintr-l2))
           (unless (null mintr-res)
             (setq mintr-res (nreverse mintr-res)))))))

;;; ==============================
;;; :COURTESY :FILE htmlfontify.el 
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
;;; :COURTESY ediff-util.el `ediff-set-difference', `ediff-member' 
;;; :CHANGESET 2233
;;; :CREATED <Timestamp: #{2010-11-02T14:14:09-04:00Z}#{10442} - by MON KEY>
(defun mon-set-difference (set1-lst set2-lst comparison-func)
  "Return all items that appear in SET1-LST but not SET2-LST.\n
The result list contains the set-difference operation of SET1-LST and SET2-LST.
Eliminates duplicates between SET1-LST and SET2-LST using COMPARISON-FUNC.\n
:EXAMPLE\n\n
\(let \(\(seta '\(b c q t\)\)
      \(setb '\(b r z v t\)\)\)
  \(equal
   \(set-difference seta setb :test 'eq\)
   \(mon-set-difference seta setb 'eq\)\)\)\n
:ALIASED-BY `mon-list-set-diff'\n
:SEE-ALSO `mon-equality-or-predicate', `mon-intersection', `mon-union',
`mon-mismatch', `mon-remove-if', `mon-mapcar', `mon-map-append', `mon-mapl',
`mon-maplist', `mon-mapcar', `mon-mapcan', `mon-mapcon', `mon-remove-if-not',
`mon-delete-if', `mon-member-if', `mon-subseq', `mon-char-code', `car-less-than-car'.\n►►►"
  (let ((msd-rslt (list 'a)) ;; The 'a gives `list' something to hold onto.
        ;; :WAS `ediff-member' Use comparison-func to decide who is a member
        (msd-pred 
         #'(lambda (msd-L-1-elt msd-L-1-lst)
             (while (and msd-L-1-lst 
                         ;; :WAS (not (funcall comparison-func (car msd-L-1-lst) msd-L-1-elt)) )
                         (not (mon-equality-or-predicate comparison-func (car msd-L-1-lst) msd-L-1-elt)))
               (setq msd-L-1-lst (cdr msd-L-1-lst)))
             msd-L-1-lst)))
    (while set1-lst
      (or 
       ;; :WAS (ediff-member (car set1-lst) (cdr msd-rslt) comparison-func)
       (funcall msd-pred (car set1-lst) (cdr msd-rslt))
       ;; :WAS (ediff-member (car set1-lst) set2-lst comparison-func)
       (funcall msd-pred (car set1-lst) set2-lst)
       (nconc msd-rslt (list (car set1-lst))))
      (setq set1-lst (cdr set1-lst)))
    ;; :WAS (cdr msd-rslt)))
    (nreverse (cdr msd-rslt))))

;;; ==============================
;; mon-pairlis
;; (pairlis (number-sequence 1 26) (mon-alphabet-as-list-symbolD))
;; (mon-alphabet-as-list-symbolD)
;; 
;;; (let ((bubba '(a b c d)))
;; (pairlis (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)
;;          (a b c d e f g h i j k l m n o p q r s t u v w x y z) 

;;; ==============================
;;; :NOTE The ANSI spec says: 
;;; "The consequences are undefined if KEYS and DATA are not of the same length."
;;; Both Clisp and SBCL signal when  (/= (length KEYS-LST) (length VAL-LST)) 
;;; Emacs Lisp's implementation of `pairlis' when either KEYS or DATA run out.
;;; All else being equal, I prefer the latter behaviour.
;;;
;; It would seem holds then that W-ALIST should be a properly dotted list...
;; Though both Emacs lisp, SBCL, and CLISP `pairlis' will IMHO incorrectly
;; "fuck anything (ostensible alist) that moves".
(defun mon-pairlis (keys-lst vals-lst &optional w-alist)
  "Like `pairlis' but with better type checking.\n
Args KEYS-LST and VALS-LST must each satisfy `mon-list-proper-p' 
Signal a `mon-list-proper-p-ERROR' if any one does not.\n
When supplied optional arg W-ALIST must satisfy `mon-list-proper-and-dotted-p'.
:EXAMPLE\n\n\(mon-pairlis \(number-sequence 1 26\) \(mon-alphabet-as-list-symbolD\)\)\n
\(let \(\(append-to '\(\(\"bubba\" . \"BUBBA\"\) \(\"BUBBA\" . \"bubba\"\)\)\)\)
  \(mon-pairlis \(number-sequence 1 26\)
               \(number-sequence 5 \(* 5  26\) 5\)
               append-to\)\)\n
;; :Following fail succesfully
\(mon-pairlis '\(\"a\" \"b\" . \"c\"\) '\(a b c\)\)\n
\(mon-pairlis '\(\"a\" \"b\" \"c\"\) '\(a b . c\)\)\n
:SEE-ALSO `mon-mapcar', `car-less-than-car'.\n►►►"
  (nconc 
   (mon-mapcar #'cons 
               (or (and (mon-list-proper-p keys-lst) keys-lst)
                   (mon-list-proper-p-ERROR :w-error t
                                            :fun-name "mon-pairlis" 
                                            :locus "KEYS-LST"
                                            :got-val keys-lst))
               (or (and (mon-list-proper-p vals-lst) vals-lst)
                   (mon-list-proper-p-ERROR :w-error t
                                            :fun-name "mon-pairlis" 
                                            :locus "VALS-LST"
                                            :got-val vals-lst)))
   (and w-alist (mon-list-proper-and-dotted-p w-alist))))

;;; ==============================
;;; :COURTESY `cvs-map' pcvs.el
(defun mon-map (w-rslt-type map-fun map-lst &rest map-lsts)
  "MAP-FUN across sequences MAP-LST and MAP-LSTS and return value W-RSLT-TYPE.\n
MAP-FUN is a function object.\n
Argument MAP-LST and rest args MAP-LSTS are sequences satisfying `mon-sequence-mappable-p'.\n
W-RSLT-TYPE is a symbol naming the type of return value. It is one of:
  list vector string array array character float
Signal an error if the collected result of mapping MAP-FUN can not be coerced to
the type specified of W-RSLT-TYPE. Additionally, an error is signalled if the
following applicable constraints hold for the value of mapped thing:\n
- When type is float and mapped thing does not satisfy `number-or-marker-p'.\n
- When type is character and mapped thing is neither `stringp' nor `symbolp' or
  does not satisfy `mon-string-not-null-nor-zerop'.
  :NOTE This latter constraint prevents attempts to perform character
  \"coercion\" of null value and empty strings and provides an alternative
  behaviour than that of the `coerce' function defined in :FILE cl-extra.el.\n
:EXAMPLE\n\n\(map 'list #'- '\(1 2 3 4\)\)\n
\(map 'string #'\(lambda \(x\) \(if \(oddp x\) ?1 ?0\)\) '\(1 2 3 4\)\)\n
\(map 'vector #'cons \"abc\" \"de\"\)
:SEE-ALSO `mon-every', `mon-map1', `cvs-map', `map', `coerce', `type-of',`typep'.\n►►►
\n(fn W-RSLT-TYPE MAP-FUNC MAP-LST MAP-LSTS)"
  (setq map-lsts (nconc map-lst map-lsts))
  (let ((mmp-gthr ()))
    (while (not (mon-every #'null map-lsts))
      (push (apply map-fun (mapcar #'car map-lsts)) mmp-gthr)
      (setq map-lsts (mapcar #'cdr map-lsts)))
    (setq mmp-gthr (nreverse mmp-gthr))
    (setq mmp-gthr
          ;; :NOTE Following conditional is a modified version of the `case'
          ;; form of coerce from :FILE cl-extra.el
          (cond ((eq w-rslt-type 't) mmp-gthr)
                ((eq w-rslt-type 'list)
                 ;; :WAS (if (listp mmp-gthr) mmp-gthr (append mmp-gthr nil))
                 (or (and (listp mmp-gthr) mmp-gthr) (append mmp-gthr nil)))
                ((eq w-rslt-type 'vector) 
                 ;; :WAS (if (vectorp mmp-gthr) mmp-gthr (vconcat mmp-gthr)))
                 (or (and (vectorp mmp-gthr) mmp-gthr) 
                     ;; (and (mon-sequence-mappable-p [] nil t)
                     (vconcat mmp-gthr)))
                ((eq w-rslt-type 'string) 
                 (if (stringp mmp-gthr) mmp-gthr (concat mmp-gthr)))
                ;; (or (stringp mmp-gthr) mmp-gthr (concat mmp-gthr)))
                ((eq w-rslt-type 'array) 
                 (if (arrayp mmp-gthr) mmp-gthr (vconcat mmp-gthr)))
                ((and (eq w-rslt-type 'character) 
                      (aref 
                       (or (and (stringp mmp-gthr)
                                (or (mon-string-not-null-nor-zerop mmp-gthr)
                                    (mon-string-not-null-nor-zerop-ERROR 
                                     :fun-name "mon-map"
                                     :locus "w-rslt-type"
                                     :got-val mmp-gthr
                                     :w-error t)))
                           (and (symbolp mmp-gthr) 
                                (or (mon-string-not-null-nor-zerop
                                     ;; The `and' is to catch situations where mmp-gthr is `null'.
                                     (and mmp-gthr (setq mmp-gthr (symbol-name mmp-gthr))))
                                    (mon-string-not-null-nor-zerop-ERROR 
                                     :fun-name "mon-map"
                                     :locus "w-rslt-type"
                                     :got-val mmp-gthr
                                     :w-error t))))
                       0)
                      ;; (<= (max-char) ...?...) (max-char)
                      ))
                ;; :NOTE Following form is from definition of `coerce' in :FILE lisp/emacs-lisp/cl-extra.el 
                ;; We can't just this merge that form here verbatim b/c it recurses
                ;; and therefore signals a byte-compiler warning. So, we coalesce the
                ;; `stringp' and `symbolp' checks into a single conditional in the
                ;; above form instead and add additional checks which guard against
                ;; attempts to perform character "coercion" of null values and empty strings.
                ;;
                ;; ((and (eq w-rslt-type 'character) (symbolp mmp-gthr)) 
                ;;  (coerce (symbol-name mmp-gthr) w-rslt-type))
                ;; 
                ((eq w-rslt-type 'float) 
                 (or (and (number-or-marker-p mmp-gthr) (float mmp-gthr))
                     (mon-format :w-fun #'error
                                 :w-spec '(":FUNCTION `mon-map' "
                                           "Arg W-RSLT-TYPE was float, "
                                           "mapped thing must satisfy `number-or-marker-p' to coerce "
                                           "mapped thing is type-of: %s with non-coerceable value: %S")
                                 :w-args `(,(type-of mmp-gthr) ,mmp-gthr))))
                ((eq 'w-rslt-type (type-of mmp-gthr)) mmp-gthr)
                (t (mon-format :w-fun #'error
                               :w-spec '(":FUNCTION `mon-map' "
                                         "Can not coerce mapped thing to W-RSLT-TYPE-TYPE %s, "
                                         "mapped thing is type-of: %s with non-coerceable value: %S")
                               :w-args `(,w-rslt-type ,(type-of mmp-gthr) ,mmp-gthr)))))))

;;; ==============================
;;; :PREFIX "mmp1-"
;;; :COURTESY SBCL :FILE sblc/src/code/list.lisp
;;; :MODIFICATIONS Now uses catch/throw instead of `return' and `setcdr' instead
;;; of `rplacd'. local var arglists now uses `mon-copy-list-mac' instead of
;;; `copy-list' to silence byte-compiler.  Elided the outer let binding of fun
;;; around `%coerce-callable-to-fun' on `fun-designator'.
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T21:07:39-04:00Z}#{10381} - by MON KEY>
(defun mon-map1 (fun-designator original-arglists accumulate take-car)
  "Helper function for implementation of CL mapping functions.\n
Used to implement following MON versions of:\n
 `mon-mapc' `mon-mapl' `mon-maplist' 
 `mon-mapcar' `mon-mapcan' `mon-mapcon'\n
Map the designated FUN-DESIGNATOR over ORIGINAL-ARGLISTS in an appropriate way.
Mapping is complete when any of the arglists runs out.\n
Until then, cdr down the arglists applying FUN-DESIGNATOR and ACCUMULATE
results as specified.\n
FUNCTION-DESIGNATOR ia a function that must take as many arguments as there are
lists in ORIGINAL-ARGLISTS.
ORIGINAL-ARGLISTS
ACCUMULATE is keyword style symbol either `:nconc` or :`list`.\n
TAKE-CAR
The mapping operation involves applying FUN-DESIGNATOR to successive sets of
ORIGINAL-ARGLISTS in which one argument is obtained from each sequence.\n
Except for `mon-mapc' and `mon-mapl', the result returned contains the results
returned by FUN-DESIGNATOR.\n
Whereas with `mon-mapc' and `mon-mapl', the resulting sequence
returned is the first-list-arg of ORIGINAL-ARGLISTS.\n
FUN-DESIGNATOR is called first on all the elements with index \"0\", then on
all those with index \"1\", and so on.  RESULT-TYPE specifies the type of
the resulting sequence.\n
 `mon-mapc'    fun-designator &rest original-arglists -> first-list-arg
 `mon-mapl'    fun-designator &rest original-arglists -> first-list-arg
 `mon-mapcar'  fun-designator &rest original-arglists -> result-list
 `mon-maplist' fun-designator &rest original-arglists -> result-list
 `mon-mapcan'  fun-designator &rest original-arglists -> concatenated-results
 `mon-mapcon'  fun-designator &rest original-arglists -> concatenated-results\n
:NOTE These mapping functions sourced from Steel Bank Common Lisp \(SBCL\).\n
:SEE :FILE sblc/src/code/list.lisp
:SEE-ALSO `mon-remove-if-not', `mon-delete-if', `mon-member-if', `mon-subseq',
`mon-intersection', `mon-mismatch', `mon-set-difference', `mon-char-code'
`cvs-map'.\n►►►"
  ;; :TODO Use (memq (mon-function-object-p fun-designator)
  ;;                 (remq 'macro *mon-function-object-types*))
  (unless 
      (functionp fun-designator) ;; (functionp 'subrp) (functionp '(lambda (x) x)) 
    ;; :WAS (error (concat ":FUNCTION `mon-map1' "
    ;;                "-- arg FUN-DESIGNATOR does not satisfy `functionp'"))
    (mon-format :w-fun #'error
                :w-spec '(":FUNCTION `mon-map1' " 
                          "-- arg FUN-DESIGNATOR does not satisfy `functionp', "
                          " got: %S type-of: %S")
                :w-args  `(,fun-designator ,(mon-function-object-p fun-designator))))
  ;;
  ;; :NOTE CL ANSI spec says:
  ;; "If FUNCTION is a symbol, it is `coerce'd to a function as if by
  ;; `symbol-function'."
  ;; :WAS (let ((fun (%coerce-callable-to-fun fun-designator)))
  ;;
  (let* (;; :WAS (arglists  (copy-list original-arglists))
         (mmp1-arg-lsts (mon-copy-list-mac original-arglists))
         (mmp1-rtn-list (list nil))
         (mmp1-tmp mmp1-rtn-list))
    (do ((mmp1-rslt nil)
         (mmp1-args '() '()))
        ((catch 'mmp1-is-null ;; :ADDED
           (dolist (mmp1-thrw mmp1-arg-lsts nil) 
             (when (null mmp1-thrw) ;; :WAS (return t)))
               (throw 'mmp1-is-null t))))
         (if accumulate
             (cdr mmp1-rtn-list)
           (car original-arglists)))
      (do ((mmp1-arg-l mmp1-arg-lsts (cdr mmp1-arg-l)))
          ((null mmp1-arg-l))
        (push (if take-car (caar mmp1-arg-l) (car mmp1-arg-l)) mmp1-args)
        (setf (car mmp1-arg-l) (cdar mmp1-arg-l)))
      (setq mmp1-rslt (apply fun-designator (nreverse mmp1-args)))
      (case accumulate
        (:nconc (setq mmp1-tmp (last (nconc mmp1-tmp mmp1-rslt))))
        ;; :WAS (:list (rplacd mmp1-tmp (list mmp1-rslt)) (setq mmp1-tmp (cdr mmp1-tmp)))
        (:list (setcdr mmp1-tmp (list mmp1-rslt)) (setq mmp1-tmp (cdr mmp1-tmp))))))) 

;;; ==============================
;;; :CHANGESET 2233
;;; :CREATED <Timestamp: #{2010-11-01T10:55:50-04:00Z}#{10441} - by MON KEY>
(defun mon-mapc (mapc-fun mapc-lst &rest more-lsts)
  ;; #!+sb-doc
  "Apply function MAPC-FUN to successive elements of MAPC-LST and MORE-LSTS.\n
`mon-mapc' is like `mon-mapcar' except that the results of applying MAPC-FUN
are not accumulated.  Return the second argument.\n
With one sequence like elisp's `mapc' with several, it is like the Common Lisp
`mapc' but without the extension to arbitrary sequence types.\n
:EXAMPLE\n
\(let \(\(dummy nil\)\)
  `\(#::w-result ,\(mon-mapc #'\(lambda \(&rest x\) 
                             \(setq dummy \(append dummy x\)\)\)
                         '\(1 2 3 4\)
                         '\(a b c d e\)
                         '\(x y z\)\)
              #::dummy ,dummy\)\)\n
:SEE-ALSO `mon-map1', `mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan',
`mon-mapcon', `mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-intersection', `mon-set-difference', `mon-mismatch', `mon-subseq'.\n►►►"
  (if more-lsts
      (mon-map1 mapc-fun (cons mapc-lst more-lsts) nil t)
    (mapc mapc-fun mapc-lst)))

;;; ==============================
;;; :COURTESY sblc/src/code/list.lisp
;;; :MODIFICATIONS renamed args function -> mapcar-fun mapcar-list -> mapcan-list
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T21:07:32-04:00Z}#{10381} - by MON KEY>
(defun mon-mapcar (mapcar-fun mapcar-lst &rest more-lsts)
  ;;#!+sb-doc
  "Apply MAPCAR-FUN to successive elements of MAPCAR-LST.\n
Return a list of values returned by successive evaluations of MAPCAR-FUN.\n
`mon-mapcar' operates on successive elements of the list arguments.\n
MAPCAR-FUN is applied to the first element of each list,, then to the second
element of each list, and so on. Iteration terminates when the shortest
list runs out, any excess elements remaining in other lists are ignored.\n
:EXAMPLE\n\n\(mon-mapcar #'\(lambda \(x &rest y\) \(cons x y\)\)
 '\(b q\) '\(c r\) '\(d s\) '\(q f\) '\(r g\) '\(s t\) 
 '\(b q\) '\(c r\) '\(d s\) '\(q f\) '\(r g\) '\(s t\)\)\n
;=> \(\(b c d q r s b c d q r s\) \(q r s f g t q r s f g t\)\)\n
\(mon-mapcar #'car '\(\(1 a\) \(2 b\) \(3 c\)\)\)\n
\(mon-mapcar #'abs '\(3 -4 2 -5 -6\)\)\n
\(mon-mapcar #'cons '\(a b c\) '\(1 2 3\)\)\n
:SEE-ALSO `mon-map1', `mon-mapc', `mon-mapl', `mon-maplist', `mon-mapcar',
`mon-mapcan', `mon-mapcon', `mon-remove-if-not', `mon-delete-if',
`mon-member-if', `mon-intersection', `mon-subseq', `mon-char-code'.\n►►►"
  (mon-map1 
   ;; fun-designator
   mapcar-fun 
   ;; original-arglists   
   (cons mapcar-lst more-lsts)
   ;; accumulate 
   :list  
   ;; take-car
   t
   ))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |
;; | ;; :NOTE Examples from the CL-ansi spec:    :EXPECT
;; |
;; | (mon-mapcar #'car '((1 a) (2 b) (3 c)))  ;; (1 2 3)
;; | (mon-mapcar #'abs '(3 -4 2 -5 -6))       ;; (3 4 2 5 6)
;; | (mon-mapcar #'cons '(A B C) '(1 2 3))    ;; ((A . 1) (B . 2) (C . 3))
;; |
;; `----

;;; ==============================
;;; :COURTESY sblc/src/code/list.lisp
;;; :MODIFICATIONS Renamed args function -> mapcan-fun list -> mapcan-list
;;; :CREATED <Timestamp: #{2010-09-19T18:25:08-04:00Z}#{10377} - by MON>
(defun mon-mapcan (mapcan-fun mapcan-lst &rest rest-lst)
  ;;#!+sb-doc
  "Apply MAPCAN-FUN to successive elts of MAPCAN-LST and if provided REST-LST.\n
Return `nconc' of MAPCAN-FUN results.\n
`mon-mapcan' is like `mon-mapcar' except the results of applying MAPCAN-FUN are
combined into a list as if by `nconc' rather than `list', e.g.:\n
 \(mon-mapcand f x1 { ... } xn\) ≡ \(apply #'nconc \(mapcar f x1 { ... } xn\)\)\n
:EXAMPLE\n\n\(mon-mapcan #'\(lambda \(x &rest y\) \(cons x y\)\)
            '\(b q\) '\(c r\) '\(d s\) '\(q f\) '\(r g\) '\(s t\) 
            '\(b q\) '\(c r\) '\(d s\) '\(q f\) '\(r g\) '\(s t\)\)\n
\(mapcan #'\(lambda \(x &rest y\) \(cons x y\)\)
        '\(b q\) '\(c r\) '\(d s\) '\(q f\) '\(r g\) '\(s t\) 
        '\(b q\) '\(c r\) '\(d s\) '\(q f\) '\(r g\) '\(s t\)\)\n
\(mon-mapcan #'\(lambda \(x y\) \(if \(null x\) nil \(list x y\)\)\)
          '\(nil nil nil d e\)
          '\(1 2 3 4 5 6\)\)\n
\(mon-mapcan #'\(lambda \(x\) \(and \(numberp x\) \(list x\)\)\)
            '\(a 1 b c 3 4 d 5\)\)\n
:SEE-ALSO `mon-map1', `mon-mapc', `mon-mapl', `mon-maplist', `mon-mapcar',
`mon-mapcan', `mon-mapcon', `mon-intersection', `mon-subseq'.\n►►►"
  (mon-map1 mapcan-fun (cons mapcan-lst rest-lst) :nconc t))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | 
;; | ;; :NOTE Examples from the CL-ansi spec:          :EXPECT
;; | 
;; | (mon-mapcan #'(lambda (x y) 
;; |                 (if (null x) nil (list x y))) ;; (D 4 E 5)
;; |             '(nil nil nil D E) ;<- Cheating :)
;; |             '(1 2 3 4 5 6))
;; | 
;; | (mon-mapcan #'(lambda (x) 
;; |                 (and (numberp x) (list x)))   ;; (1 3 4 5)
;; |             '(a 1 b c 3 4 d 5))
;; | 
;; `----

;;; ==============================
;;; :COURTESY sblc/src/code/list.lisp
;;; :MODIFICATIONS renamed args function -> mapl-fun list -> mapl-list
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T21:07:11-04:00Z}#{10381} - by MON KEY>
(defun mon-mapl (mapl-fun mapl-lst &rest more-lsts)
  ;;#!+sb-doc
  "Apply MAPL-FUN to successive cdrs of MAPL-LST. Return nil.\n
`mon-mapl' is like `mon-maplist' except that the results of applying MAPL-FUN
are not accumulated; mapl-lst is returned.\n
:EXAMPLE\n\n\(let \(bubba\)
  `\(#::SIDE-EFFECT 
    ,\(mon-mapl #'\(lambda \(x\) \(push x bubba\)\) '\(1 2 3 4\)\)
    #::RETURN-VAL ,bubba\)\)\n
\(let \(bubba\)
  \(mon-mapl #'\(lambda \(x &rest y\) 
                \(push `\(,x ,@y\) bubba\)\)
            '\(1 2 3 4\) '\(5 6 7 8\) '\(9 10 11 12\)\)
  \(setq bubba \(nreverse bubba\)\)\)\n
\(let \(\(list-args '\(\(1 2 3 4\) \(5 6 7 8\) \(9 10 11 12\)\)\)
      bubba\)
  \(apply #'mon-mapl #'\(lambda \(x &rest y\) 
                        \(push `\(,x ,@y\) bubba\)\)
         list-args\)
  \(push \(mapcar #'car \(setq bubba \(nreverse bubba\)\)\)
        bubba\)\)\n
\(let \(\(list-args '\(\(a b c \"d\"\) \(e \"f\" g h\) \(i j \"k\" l\)\)\)
       bubba\)
  \(apply #'mon-mapl #'\(lambda \(x &rest y\) 
                        \(push `\(,x ,@y\) bubba\)\)
         list-args\)
  \(push \(mapcar #'car \(setq bubba \(nreverse bubba\)\)\) bubba\)
  \(mapc #'\(lambda \(stc\)
            \(setq stc \(assoc \(car \(setq stc \(assoc stc bubba\)\)\) \(cdr bubba\)\)\)
            \(setcdr stc `\(,@\(cdr stc\) ,@\(cdr \(assoc \(car stc\) list-args\)\)\)\)\)
        \(mapcar #'car 
                \(setq list-args 
                      \(mapcar #'\(lambda \(mpl\)
                                  \(setq mpl 
                                        `\(,mpl 
                                          ,\(apply #'append 
                                                  \(assoc mpl \(cdr bubba\)\)\)\)\)\)
                              \(car bubba\)\)\)\)\)
  bubba\)\n
:SEE-ALSO `mon-map1', `mon-mapc', `mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan',
`mon-mapcon', `mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-intersection', `mon-subseq', `mon-char-code'.\n►►►"
  (mon-map1 mapl-fun (cons mapl-lst more-lsts) nil nil))

;;; ==============================
;;; :COURTESY sblc/src/code/list.lisp
;;; :MODIFICATIONS renamed args function -> mapcon-fun list -> mapcon-list
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T21:06:57-04:00Z}#{10381} - by MON KEY>
(defun mon-mapcon (mapcon-fun mapcon-lst &rest more-lsts)
  ;;#!+sb-doc
  "Apply MAPCON-FUN to successive cdrs of MAPCON-LST. Return `nconc'd results.\n
mon-mapcon is like `mon-maplist' except that the results of applying MAPCON-FUN
are combined into a list as if by `nconc' rather than `list', e.g:
 \(mon-mapcon f x1 ... xn\) ≡ \(apply #'nconc \(mon-maplist f x1 ... xn\)\)\n
:EXAMPLE\n\n\(mon-mapcon #'\(lambda \(x &rest y\) `\(,\(cadr x\) ,\(car x\)\)\)
            '\(b q \(c r\) \(d s\)\)\)\n
\(mapcon #'\(lambda \(x &rest y\) `\(,\(cadr x\) ,\(car x\)\)\)
            '\(b q \(c r\) \(d s\)\)\)\n
:SEE-ALSO `mon-map1', `mon-mapc', `mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan',
`mon-mapcon', `mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-intersection', `mon-subseq', `mon-char-code'.\n►►►"
  (mon-map1 mapcon-fun (cons mapcon-lst more-lsts) :nconc nil))

;;; ==============================
;;; :COURTESY sblc/src/code/list.lisp
;;; :MODIFICATIONS renamed args function -> maplist-fun list -> maplist-list
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-20T21:06:44-04:00Z}#{10381} - by MON KEY>
(defun mon-maplist (maplist-fun maplist-lst &rest more-lsts)
  ;;  #!+sb-doc
  "Apply MAPLIST-FUN to successive cdrs of MAPLIST-LST. Return list of results.\n
`mon-maplist' is like `mon-mapcar' except that MAPLIST-FUN is applied to
successive sublists of the MAPLIST-LST and (optionally) MORE-LSTS.
MAPLIST-FUN is first applied to the list arguments themselves, and then to the
cdr of each list, and then to the cdr of the cdr of each list, and so on.\n
:EXAMPLE\n\n\(mon-maplist #'\(lambda \(x &rest y\) `\(,\(cadr x\) ,\(car x\)\)\)
            '\(b q \(c r\) \(d s\)\)\)\n
\(maplist #'\(lambda \(x &rest y\) `\(,\(cadr x\) ,\(car x\)\)\)
            '\(b q \(c r\) \(d s\)\)\)\n
\(mon-maplist #'append '\(1 2 3 4\) '\(1 2\) '\(1 2 3\)\)\n
\(mon-maplist #'\(lambda \(x\) \(cons 'foo x\)\) '\(a b c d\)\)\n
\(mon-maplist #'\(lambda \(x\) 
                 \(if \(member \(car x\) \(cdr x\)\) 0 1\)\)
             '\(a b a c d b c\)\)\n
:NOTE In last example, an entry is 1 if the corresponding element of the input
list was the last instance of that element in the input list.\n
:SEE-ALSO `mon-map1', `mon-mapc', `mon-mapl', `mon-maplist', `mon-mapcar', `mon-mapcan',
`mon-mapcon', `mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-intersection', `mon-subseq', `mon-char-code'.\n►►►"
  (mon-map1 maplist-fun (cons maplist-lst more-lsts) :list nil))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; |
;; | ;; :NOTE Examples from the CL-ansi spec:                 :EXPECT
;; | 
;; | (mon-maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))     ;; ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3))
;; | (mon-maplist #'(lambda (x) (cons 'FOO x)) '(A B C D)) ;; ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D))
;; | (mon-maplist #'(lambda (x)                            ;; (0 0 1 0 1 1 1)
;; |                 (if (member (car x) (cdr x)) 0 1))
;; |                '(a b a c d b c))
;; `----

;;; ==============================
;;; :TODO write `mon-subseq-TEST'
;;; :PREFIX "msbsq-"
;;; :COURTESY `widget-sublist'/`subseq'
;;; :CHANGESET 2119
;;; :CREATED <Timestamp: #{2010-09-14T14:37:01-04:00Z}#{10372} - by MON KEY>
(defun mon-subseq (in-seq seq-start &optional seq-end)
  "Return the sublist of SEQ from SEQ-START to SEQ-END.\n
SEQ-END are positive integer indexes into SEQ.\n
If SEQ-END is omitted, it defaults to the length of SEQ.\n
:EXAMPLE\n\n\(mon-subseq \"bubbas\" 3  3\)\n
\(mon-subseq \"bubbas\" 3\) \n
\(mon-subseq '\(0 1 2 3 4 5\)  2 3\)\n
\(mon-subseq '\(0 1 2 3 4 5\)  2\)\n
\(mon-subseq  [0 1 2 3 4 5]  1 3\)\n
\(mon-subseq  [0 1 2 3 4 5]  1\)\n
\(mon-subseq \(make-bool-vector 4 t\) 0 3\)\n
\(mon-subseq \(make-bool-vector 3 t\) 0\)\n
\(mon-subseq \(make-bool-vector 0 t\) 0 0\)\n
;; :NOTE Following fail:\n
\(mon-subseq composition-function-table 1 3\)\n
\(mon-subseq \(make-bool-vector 4 t\)  3 5\)\n
\(mon-subseq  [0 1 2 3 4 5]  7 3\)\n
\(mon-subseq \"bubbas\"\  3 7\)\n
\(mon-subseq '\(0 1 2 3 4 5\) -1  3\)\n
\(mon-subseq  [0 1 2 3 4 5] 3  0\)\n
\(mon-subseq \"bubbas\"  3 -7\)\n
\(mon-subseq  [0 1 2 3 4 5]  6 -3\)\n
\(mon-subseq  [0 1 2 3 4 5] -3\)\n
\(mon-subseq '\(0 1 2 3 4 5\)  7  6\)\n
\(mon-subseq  nil  3  0\)\n
:SEE-ALSO `mon-map1', `mon-mapc', `mon-mapl', `mon-maplist', `mon-mapcar',
`mon-mapcan', `mon-mapcon', `mon-map-append', `mon-remove-if-not',
`mon-remove-if', `mon-delete-if', `mon-member-if', `mon-char-code',
`mon-intersection' `mon-mismatch', `mon-set-difference', `subseq',
`mon-sublist-gutted' `mon-list-proper-p', `mon-maybe-cons', `mon-remove-dups',
`mon-remove-if', `mon-delq-cons', `mon-delq-dups' `mon-list-make-unique',
`mon-list-match-tails', `mon-assoc-replace', `mon-moveq', `mon-flatten',
`mon-transpose', `mon-maptree', `mon-mapcar', `mon-recursive-apply',
`mon-combine', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-moveq', `mon-nshuffle-vector', `mon-list-nshuffle',
`mon-list-shuffle-safe', `mon-list-reorder', `car-less-than-car'.\n►►►"
  (cond ((null in-seq) 
         (mon-format :w-fun #'error 
                     :w-spec '(":FUNCTON `mon-subseq'"
                               "-- arg IN-SEQ can not be null")))
        ((listp in-seq)
         (and (or seq-end (setq seq-end (length in-seq)))
              (or (and (>= (length in-seq) seq-start) (>= (length in-seq) seq-end)
                       (>= seq-end seq-start) (>= seq-start 0) (>= seq-end 0))
                  (signal 'args-out-of-range `(,seq-start . ,seq-end))))
         (if (> seq-start 0) 
             (setq in-seq (nthcdr seq-start in-seq)))
         (if seq-end
             (unless (<= seq-end seq-start)
               (setq in-seq (copy-sequence in-seq))
               (setcdr (nthcdr (- seq-end seq-start 1) in-seq) nil)
               in-seq)
           (copy-sequence in-seq)))
        ((arrayp in-seq)
         (let ((msbsq-ez-cas (type-of in-seq)))
           (and (or (eq msbsq-ez-cas 'string) (eq msbsq-ez-cas 'vector)
                    (and (eq msbsq-ez-cas 'char-table)
                         (signal 'wrong-type-argument `(vectorp . ,msbsq-ez-cas)))
                    (prog1 (eq msbsq-ez-cas 'bool-vector)
                      (setq in-seq (vconcat in-seq))))
                (or (and (or seq-end (setq seq-end (length in-seq)))
                         (>= (length in-seq) seq-start) (>= (length in-seq) seq-end)
                         (>= seq-end seq-start) (>= seq-start 0) (>= seq-end 0))
                    (signal 'args-out-of-range `(,seq-start . ,seq-end)))
                (substring in-seq seq-start seq-end))))))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (mon-subseq "bubbas" 3  3)
;; | (mon-subseq "bubbas" 3) 
;; | (mon-subseq '(0 1 2 3 4 5)  2 3)
;; | (mon-subseq '(0 1 2 3 4 5)  2)
;; | (mon-subseq  [0 1 2 3 4 5]  1 3)
;; | (mon-subseq  [0 1 2 3 4 5]  1)
;; | (mon-subseq (make-bool-vector 4 t) 0 3)
;; | (mon-subseq (make-bool-vector 3 t) 0)
;; | (mon-subseq (make-bool-vector 0 t) 0 0)
;; | ;; :NOTE Following fail:
;; | (mon-subseq composition-function-table 1 3)
;; | (mon-subseq (make-bool-vector 4 t)  3 5)
;; | (mon-subseq  [0 1 2 3 4 5]  7 3)
;; | (mon-subseq  [0 1 2 3 4 5]  6 -3)
;; | (mon-subseq  [0 1 2 3 4 5] -3)
;; | (mon-subseq "bubbas"  3 7)
;; | (mon-subseq "bubbas"  3 -7)
;; | (mon-subseq '(0 1 2 3 4 5) -1  3)
;; | (mon-subseq '(0 1 2 3 4 5)  1  -3)
;; | (mon-subseq '(0 1 2 3 4 5)  7  6)
;; | (mon-subseq  [0 1 2 3 4 5] -3  )
;; | (mon-subseq  nil  3  0)
;; `----

;;; ==============================
;;; :PREFIX "msbl-"
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
  (let* ((msbl-sub (nthcdr skip-n in-list)) 
	 (msbl-q (length msbl-sub)))
    (reverse (nthcdr (- msbl-q return-n) (reverse msbl-sub)))))
;;
;;; :TEST-ME (mon-sublist 0 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 3 3 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 5 2 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist 1 2 '(A B (C D) E (F G) (Q (H I)) K))  
;;; :TEST-ME (mon-sublist 6 1 '(A B (C D) E (F G) (Q (H I)) K))  

;;; ==============================
;;; :PREFIX "msblg-"
;;; :COURTESY Jean-Marie Chauvet :HIS ncloseemacs-ml-dataset.el :WAS `sublist-rest'
;;; :CREATED <Timestamp: #{2009-09-19T18:55:37-04:00Z}#{09386} - by MON>
(defun mon-sublist-gutted (gut-from-n to-n-ards gut-lst)
  "Return GUT-LST with GUTS-FROM-N TO-N-ARDS extracted.\n
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
  (let* ((msblg-pre-guts 
          (nthcdr (length (nthcdr gut-from-n gut-lst)) (reverse gut-lst))) ;; msblg-pre-guts reversed
	 (msblg-pst-guts 
          (nthcdr (+ to-n-ards (length msblg-pre-guts)) gut-lst)))
    (append (reverse msblg-pre-guts) msblg-pst-guts))) ;;:WAS (append prefix postfix)
;;
;;; :TEST-ME (mon-sublist-gutted 3 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 5 2 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 5 1 '(A B (C D) E (F G) (Q (H I)) K))
;;; :TEST-ME (mon-sublist-gutted 0 6 '(A B (C D) E (F G) (Q (H I)) K))

;;; ==============================
;;; :COURTESY Jean-Marie Chauvet nclose-eieio.el :WAS `map-append'
;;; :CREATED <Timestamp: #{2009-09-21T15:26:14-04:00Z}#{09391} - by MON KEY>
(defun mon-map-append (mapping-lst)
  "Append all subseqs \(proper-lists or vectors\) in MAPPING-LST.\n
:EXAMPLE\n\n\(mon-map-append '\(\(1 a\) \(2 b\) \(3 c\)\)\)\n
\(mon-map-append '\([1 a] [2 b] [3 c]\)\)\n
:NOTE Compare with `mon-flatten' with handles both conses and vectors:\n
\(mon-flatten '\([1 a] \(2 . b\) [3 c]\)\)\n
:SEE-ALSO `mon-intersection', `mon-combine', `mon-mapcar', `mon-maptree',
`mon-transpose', `mon-flatten', `mon-recursive-apply', `mon-list-proper-p',
`mon-maybe-cons', `mon-sublist', `mon-sublist-gutted', `mon-list-match-tails',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-delq-cons', `mon-delq-dups', `mon-list-make-unique', `mon-remove-if',
`mon-remove-dups', `mon-assoc-replace', `mon-list-reorder',
`mon-nshuffle-vector', `mon-list-nshuffle', `mon-list-shuffle-safe'.\n►►►"
  (cond ((null mapping-lst) nil)
	(t (append (car mapping-lst) (mon-map-append (cdr mapping-lst))))))

;;; ==============================
;;; :PREFIX "mar-"
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
`mon-elt-<elt', `mon-maybe-cons', `mon-list-proper-p', `car-less-than-car'.\n►►►"
  (let (mar-rtn)
    (setq mar-rtn
          (mapcar #'(lambda (mar-L-1)
                      (let* ((mar-L-1-key (car mar-L-1))
                             (mar-L-1-val (assoc mar-L-1-key seq2)))
                        ;; :WAS (if (cadr val) val elem))) seq1)) 
                        (if mar-L-1-val mar-L-1-val mar-L-1))) seq1))))
;;                          
;;; :TEST-ME (mon-assoc-replace '((a (a c d)) (b (c d e)) (c (f g h)))
;;;                             '((a (c d g)) (b (c d f)) (g (h g f))))

;; ==============================
;;; :COURTESY Henry Kautz :HIS refer-to-bibtex.el :WAS `moveq'
;;; (URL `http://www.tex.ac.uk/tex-archive/bibliography/bibtex/utils/refer-tools/refer-to-bibtex.el')
;;; :CREATED <Timestamp: 2009-08-04-W32-2T18:57:30-0400Z - by MON KEY>
(defmacro mon-moveq (enqueue-new with-old-to-null)
  "Set value of ENQUEUE-NEW to value of WITH-OLD-TO-NULL return ENQUEUE-NEW.\n
On return, ENQUEUE-NEW has WITH-OLD-TO-NULL's value and WITH-OLD-TO-NULL is
well... null.\n
:EXAMPLE\n\n(let ((new1 '(a b c)) (old1 '(q e d)))
  `(,(mon-moveq new1 old1) ,old1))\n
:ALIASED-BY `mon-list-nqueue'\n
:SEE-ALSO `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-list-proper-p', `mon-maybe-cons' `mon-sublist', `mon-sublist-gutted',
`mon-remove-if', `mon-intersection', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-delq-cons', `mon-delq-dups', `mon-remove-if',
`mon-remove-dups', `mon-list-make-unique', `mon-assoc-replace',
`mon-list-match-tails', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `mon-list-shuffle-safe', `car-less-than-car'.\n►►►"
  ;; :WAS  (list 'progn (list 'setq enqueue-new with-old-to-null) 
  ;;       (list 'setq with-old-to-null 'nil)) )
  `(prog1 
       (setq ,enqueue-new ,with-old-to-null) 
     (setq ,with-old-to-null nil)) )

;;; ==============================
;;; :PREFIX "mfltn-"
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-list.el :WAS `flatten'
(defun mon-flatten (tree-to-flatten)
  "Return sequence TREE-TO-FLATTEN as a proper-list with all elements flattened.\n
TREE-TO-FLATTEN is a proper-list, consed pair, or vector.\n
:EXAMPLE\n\n\(mon-flatten '\(\(1 a\) \(2 b\) \(3 c\)\)\)\n
\(mon-flatten '\([1 a] \(2 . b\) [3 c]\)\)\n
\(mon-flatten '\(\(1 . a\) \(2 . b\) \(3 . c\)\)\)\n
:NOTE Compare with `mon-map-append' with also handles vectors but not conses:\n
\(mon-map-append '\([1 a] \(2 b\) [3 c]\)\)\n
:ALIASED-BY `mon-list-flatten'\n
:SEE-ALSO `mon-list-proper-p', `mon-maybe-cons', `mon-intersection',
`mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree-to-flatten', `mon-transpose',
`mon-flatten', `mon-recursive-apply', `mon-list-match-tails', `mon-sublist',
`mon-sublist-gutted', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-remove-if', `mon-remove-dups', `mon-delq-dups'
`mon-delq-cons', `mon-list-make-unique', `mon-remove-dups', `mon-assoc-replace',
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle',
`mon-list-shuffle-safe'.\n►►►"
  ;; (copy-tree tree-to-flatten)
  (do ((mfltn-rslt   nil)
       (mfltn-stack  nil))
      ((not (or tree-to-flatten mfltn-stack)) (nreverse mfltn-rslt))
    (cond ((null tree-to-flatten)
           (setq tree-to-flatten (pop mfltn-stack)))
          ;; :WAS ((atom tree-to-flatten)
          ((and (atom tree-to-flatten) 
                ;; :ADDED support for vectors
                (not (vectorp tree-to-flatten)))
           (push tree-to-flatten mfltn-rslt)
           (setq tree-to-flatten (pop mfltn-stack)))
          ((listp (car tree-to-flatten))
           (push (cdr tree-to-flatten) mfltn-stack)
           (setq tree-to-flatten (car tree-to-flatten)))
          ;; :ADDED support for vectors
          ((vectorp (car tree-to-flatten))
           (push (cdr tree-to-flatten) mfltn-stack)
           (setq tree-to-flatten (append (car tree-to-flatten) nil)))
          ;;
          (t (push (car tree-to-flatten) mfltn-rslt)
             (setq tree-to-flatten (cdr tree-to-flatten))))))

;;; ==============================
;; mon-list-flatten-rotated
;;; :CHANGESET 2202 <Timestamp: #{2010-10-20T12:28:20-04:00Z}#{10423} - by MON KEY>
(defun mon-list-flatten-rotated (lst-of-lsts)
  "Flatten LST-OF-LSTS - a list of lists.\n
:EXAMPLE\n\n\(mon-list-flatten-rotated '\(\(a b c\) \(1 \(\(2 3\)\)\)\)\)\n
:ALIASED-BY `mon-rotate-flatten-list'\n
:SEE-ALSO `mon-flatten', `mon-rotate-string', `mon-rotate-next',
`mon-rotate-region', `mon-rotate-get-rotations-for',
`mon-string-rotate-to-regexp', `mon-indent-or-rotate'.\n►►►"
  (if (null lst-of-lsts)
      lst-of-lsts
    (if (listp lst-of-lsts)
	(append (mon-list-flatten-rotated (car lst-of-lsts))
		(mon-list-flatten-rotated (cdr lst-of-lsts)))
      (list lst-of-lsts))))
;;
;;; :TEST-ME (mon-list-flatten-rotated '((a b c) (1 ((2 3)))))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS list.lisp :WAS TRANSPOSE
;;; :CREATED <Timestamp: #{2009-09-28T17:40:47-04:00Z}#{09401} - by MON>
(defun mon-transpose (trans-tree)
  "Return TRANS-TREE transposed such that all car and cdr's are exchanged.\n
:EXAMPLE\n\n\(mon-transpose '\(a \(bb cc\) dd\)\)\n
\(mon-flatten \(mon-transpose '\(a \(bb cc\) dd\)\)\)\n
:ALIASED-BY `mon-list-transpose'\n
:SEE-ALSO `mon-mismatch', `mon-intersection', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptrans-tree', `mon-flatten', `mon-recursive-apply',
`mon-sublist', `mon-sublist-gutted', `mon-list-match-tails',
`mon-list-proper-p', `mon-maybe-cons', `mon-moveq', `mon-elt->', `mon-elt-<',
`mon-elt->elt', `mon-elt-<elt', `mon-delq-cons', `mon-delq-dups'
`mon-remove-if', `mon-remove-dups', `mon-assoc-replace', `mon-list-make-unique',
`mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle'. `car-less-than-car'.\n►►►"
  (if (atom trans-tree)
      trans-tree
      (cons (mon-transpose (cdr trans-tree))
            (mon-transpose (car trans-tree)))))
;;
;;; :TEST-ME (mon-transpose '(a (bb cc) dd))
;;; :TEST-ME (mon-flatten (mon-transpose '(a (bb cc) dd)))

;;; ==============================
;;; :COURTESY Barry Margolin's 
;;; (URL `http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/2d71b553b62e20b5#')
;;; :CHANGESET 2331
;;; :CREATED <Timestamp: #{2010-11-27T17:06:03-05:00Z}#{10476} - by MON KEY>
(defun mon-list-intersperse (w-lst w-item)
  "Intersperse sequence W-LST W-ITEM.\n
:EXAMPLE\n\n\(mon-list-intersperse (number-sequence 0 6) '^\)\n
\(mon-mapcar #'\(lambda \(x &rest y\)
                \(mon-list-intersperse `\(,x ,@y\) '_\)\)
            '\(a s d f g\) '\(g f d s a\)
            '\(q w e r t\) '\(t r e w q\)\)\n
:SEE-ALSO `mon-transpose', `mon-map-append', `mon-mapcan', `mon-mapcon'.\n►►►"
  (let ((mil-list* ;; this is cl.el's `list*' 
         #'(lambda (mil-L-1-arg &rest mil-L-1-rest)
             (cond ((not mil-L-1-rest) mil-L-1-arg)
                   ((not (cdr mil-L-1-rest)) 
                    ;; (cons mil-L-1-arg (car mil-L-1-rest))
                    (nconc mil-L-1-arg (car mil-L-1-rest)))
                   (t (let* ((mil-L-1-len (length mil-L-1-rest))
                             (mil-L-1-cpy (copy-sequence mil-L-1-rest))
                             (mil-L-1-tl  (nthcdr (- mil-L-1-len 2) mil-L-1-cpy)))
                        (setcdr mil-L-1-tl (cadr mil-L-1-tl))
                        (cons mil-L-1-arg mil-L-1-cpy)))))))
    (if (null (cdr w-lst))
        w-lst
      ;; If we could use cl.el's `list*', then following would work:
      ;; :WAS (list* (car w-lst) w-item
      ;;          (mon-intersperse-list (cdr w-lst) w-item))
      (apply mil-list* 
             (car w-lst) 
             (list w-item (mon-list-intersperse (cdr w-lst) w-item))))))


;; SOME is similar, except as soon as the predictae returns a non-NIL
;; value, SOME returns that value.  If the end of a sequence is reached,
;; SOME returns NIL.

;;; ==============================
;;; :CHANGESET 2356
;;; :CREATED <Timestamp: #{2010-12-07T22:34:24-05:00Z}#{10492} - by MON KEY>
(defun mon-every (ev-pred ev-lst &rest ev-lsts)
  "Return true if predicate ev-pred is true of every element of ev-lst or ev-lsts.
When EV-LSTS is non-nil the predicate EV-PRED must take as many arguments as
there are sequences.\n
Evaluate EV-PRED predicate and one or more sequences \(a list or array\).
As soon as an invocation of the predictae returns NIL, MON-EVERY returns NIL.
If the end of a sequence is reached, EVERY returns non-NIL.  So EVERY
checks to see if all members of the sequence(s) pass the predicate
function.\n
Each sequence should satisfy `mon-sequence-mappable-p', signal an error if not.\n
:EXAMPLE\n\n
\(mon-every #'zerop \(make-vector 3 0\)\)\n
\(mon-every #'null \(make-bool-vector 3 nil\)\)\n
\(mon-every #'stringp \(make-list 3 \"bubba\"\)\)\n
\(mon-every #'characterp \(make-string 7 42\)\)\n
\(mon-every #'\(lambda \(x y\) \(evenp \(+ x y\)\)\) '\(1 2 3\) '\(1 2 3\)\)\n
\(let \(\(3-lst \(make-list 8 3\)\)\)
  \(mon-every #'\(lambda \(x y z\)
                 \(and \(= x 3\) \(= y 3\) \(= z 3\)\)\)
             3-lst 3-lst 3-lst\)\)
\(mon-every #'\(lambda \(x y z\) \(and \(null x\) \(null y\) \(null z\)\)\)
           [nil nil nil] \(make-bool-vector 3 nil\) '\(nil nil nil\)\)\n
;; Following evaluates non-nil even though mmm is not numberp b/c it exceeds the
;; bounds of the 1st and thrid list, e.g. we never see it.\n
\(mon-every #'\(lambda \(x y z\) 
                 \(and \(numberp x\) \(numberp y\) \(numberp z\)\)\)
             '\(0 1 2 3 4\) '\(5 6 7 8 9 mmm\) '\(4 3 2 1 0\)\)\n
:SEE-ALSO `mon-mapc', `mon-sequence-mappable-p', `mon-mismatch',
`cvs-every', `car-less-than-car'.\n►►►"
  (setq ev-lsts
        (mapcar #'(lambda (me-L-1-lsts)
                    (or (and (consp me-L-1-lsts) me-L-1-lsts)
                        (and (memq (cdr (mon-sequence-mappable-p me-L-1-lsts nil t))
                                   '(vector bool-vector string))
                             (append me-L-1-lsts nil))
                        (mon-format :w-fun #'error 
                                    :w-spec '(":FUNCTION `mon-every' "
                                              "an arg did not `mon-sequence-mappable-p' "
                                              "and was not non-coerceable, got: %S")
                                    :w-args me-L-1-lsts)))
                `(,ev-lst ,@ev-lsts)))
  (catch 'me-every-failed
    (apply #'mon-mapc 
           #'(lambda  (&rest me-L-1-lsts) 
               (or (apply ev-pred me-L-1-lsts)
                   (throw 'me-every-failed nil)))
           
           (car ev-lsts)
           (cdr ev-lsts))
    t))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-15T14:40:11-04:00Z}#{10242} - by MON KEY>
(defun* mon-mismatch (sqn1 sqn2 &key (sqn1-str 0) (sqn1-end (length sqn1))
                           (sqn2-str 0) (sqn2-end (length sqn2)))
  "Implementation of `edmacro-mismatch' function with keywords.\n
Compare SQN1 with SQN2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorted sequence.\n
:EXAMPLE\n
\(mon-sequence-mismatch  '\(a b c 1 8\) '\(a b c 2 9\)\)\n
\(mon-sequence-mismatch  '\(a b c 2 8\) '\(a b c 2 8\) :sqn1-str 2  :sqn2-str 2\)\n
\(mon-sequence-mismatch  '\(a b c 2 8\) '\(a b c a b c 2 8 a b c\) :sqn2-str 3\)\n
:NOTE `edmacro-mismatch' was a kludge needed in order to use CL `mismatch'.\n
Should byte-compile without CL runtime package warnings.\n
:ALIASED-BY `mon-list-mismatch'\n
:SEE-ALSO `mon-intersection', `mon-set-difference', `mon-every', `mon-sublist',
`mon-sublist-gutted', `mon-mapcar', `mon-map-append', `mon-string-chop-spaces',
`mon-maptree', `mon-transpose', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-flatten', `mon-combine', `mon-recursive-apply',
`mon-delq-cons', `mon-list-make-unique', `mon-list-match-tails',
`mon-list-reorder', `mon-list-proper-p', `mon-maybe-cons', `car-less-than-car'.\n►►►"
  (edmacro-mismatch sqn1 sqn2 sqn1-str sqn1-end  sqn2-str sqn2-end))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS list.lisp :WAS `maptree'
;;; :MODIFICATIONS <Timestamp: #{2010-01-16T19:20:11-05:00Z}#{10027} - by MON>
;;; `mon-maptree' uses `flet' cl--every -> `every'
;;; :CREATED <Timestamp: #{2009-09-28T17:40:37-04:00Z}#{09401} - by MON>
(defun mon-maptree (fun &rest trees)
  "Map function FUN over trees or TREES.\n
:EXAMPLE\n\n\(mon-maptree #'stringp '\(a \(\"b\" b cc\) dd\)\)\n
\(mon-maptree  #'\(lambda \(x\) \(when \(stringp x\) x\)\)
               '\(a \(\"b\" b cc \"bb\"\) dd\)\)\n
:SEE-ALSO `mon-every', `mon-intersection', `mon-remove-if', `mon-combine',
`mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-list-match-tails', `mon-list-proper-p',
`mon-maybe-cons', `mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt',
`mon-elt-<elt', `mon-sublist', `mon-sublist-gutted', `mon-remove-if',
`mon-remove-dups', `mon-delq-dups', `mon-delq-cons', `mon-list-make-unique',
`mon-assoc-replace', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `car-less-than-car'.\n►►►"
  ;; :WAS
  ;; (cond ((null trees) nil)
  ;;       ((every (function null)  trees) nil)
  ;;       ((every (function atom)  trees) (apply fun trees))
  ;;       ((every (function consp) trees)
  ;;        (cons (apply (function mon-maptree) fun (mapcar (function car) trees))
  ;;              (apply (function mon-maptree) fun (mapcar (function cdr) trees))))
  ;;       (t nil)))
  (cond ((null trees) nil)
        ((mon-every (function null)  trees) nil)
        ((mon-every (function atom)  trees) (apply fun trees))
        ((mon-every (function consp) trees)
         (cons (apply (function mon-maptree) fun (mapcar (function car) trees))
               (apply (function mon-maptree) fun (mapcar (function cdr) trees))))
        (t nil))) ;))
;;
;;; :TEST-ME (mon-maptree #'stringp '(a ("b" b cc) dd))
;;; => "b" (nil ("b" nil nil) nil)

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS recursive-apply?
(defun mon-recursive-apply (atom-fun list-a list-b)
  "Apply ATOM-FUN recursively on each pair in parallel structures A-LIST and LIST-B.
Only the elements from LIST-A must be an atom to be passed to ATOM-FUN.\n
:EXAMPLE\n\n\(mon-recursive-apply #'\(lambda \(atom other\) \(cons atom other\)\)
                     '\(apple orange peach\) 
                     '\(\(red yellow green\) \(orange\) \(yellow white\)\)\)\n
:ALIASED-BY `mon-list-recurse-apply'\n
:SEE-ALSO `mon-intersection', `mon-remove-if', `mon-combine', `mon-mapcar',
`mon-map-append', `mon-maptree', `mon-transpose', `mon-flatten',
`mon-recursive-apply', `mon-list-proper-p', `mon-maybe-cons',
`mon-list-match-tails', `mon-sublist', `mon-sublist-gutted', `mon-moveq',
`mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt', `mon-delq-cons',
`mon-delq-dups', `mon-remove-dups', `mon-remove-if', `mon-list-make-unique',
`mon-assoc-replace', `mon-list-reorder', `mon-nshuffle-vector',
`mon-list-nshuffle', `car-less-than-car'.\n►►►"
  (cond ((null list-a) nil)
        ((atom list-a) (apply atom-fun (list list-a list-b)))
        (t (cons (mon-recursive-apply atom-fun (car list-a) (car list-b)) 
                 (mon-recursive-apply atom-fun (cdr list-a) (cdr list-b))))))

;;; ==============================
;;; :PREFIX "mlm-"
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
`mon-list-reorder', `mon-nshuffle-vector', `mon-list-nshuffle',
`car-less-than-car'.\n►►►"
  (let ((mlm-res nil))
    (while (and list1 list2)
      (if (funcall pred (car list2) (car list1))
          (push (pop list2) mlm-res)
        (push (pop list1) mlm-res)))
    (nconc (nreverse mlm-res) list1 list2)))


;;; ==============================
;;; :PREFIX "mcmbn-"
;;; :COURTESY Pascal J. Bourguignon :HIS ???
(defun mon-combine (&rest combine-lsts)
  "Return the set of tuples built taking one item in order from each list
in COMBINE-LSTS.\n
:EXAMPLE\n\n\(mon-combine '\(www ftp\) '\(exa\) '\(com org\)\)\n
;=> \(\(www exa com\) \(www exa org\) \(ftp exa com\) \(ftp exa org\)\)\n
:ALIASED-BY `mon-map-combine'
:ALIASED-BY `mon-list-combine'\n
:SEE-ALSO `mon-intersection', `mon-set-difference', `mon-remove-if',
`mon-combine', `mon-mapcar', `mon-map-append', `mon-maptree', `mon-transpose',
`mon-flatten', `mon-recursive-apply', `mon-list-match-tails',
`mon-list-proper-p', `mon-maybe-cons', `mon-sublist', `mon-sublist-gutted',
`mon-moveq', `mon-elt->', `mon-elt-<', `mon-elt->elt', `mon-elt-<elt',
`mon-delq-cons', `mon-delq-dups', `mon-remove-dups', `mon-remove-if',
`mon-assoc-replace', `mon-list-make-unique', `mon-list-reorder',
`mon-nshuffle-vector', `mon-list-nshuffle', `car-less-than-car'.\n►►►"
  ;; :NOTE cl--mapcan -> `mapcan' from cl*.el 
  ;; :WAS (defun mon-combine (&rest rest) 
  ;; (flet ((cl--mapcan (func seq &rest rest)
  ;;          (apply 'nconc (apply 'mapcar* func seq rest))))
  ;;   (cond ((null args) '(nil))
  ;;         ((consp (car args))
  ;;          (cl--mapcan
  ;;            (lambda (item) (apply (function mon-combine) item (cdr args)))
  ;;                      (car args)))
  ;;         (t (cl--mapcan
  ;;             (lambda (rest) (list (cons (car args) rest)))
  ;;                        (apply (function mon-combine) (cdr args)))))))
  (cond ((null combine-lsts) '(nil))
        ((consp (car combine-lsts))
         (mon-mapcan #'(lambda (mcmbn-L-1) 
                         (apply #'mon-combine mcmbn-L-1 (cdr combine-lsts)))
                     (car combine-lsts)))
        (t (mon-mapcan #'(lambda (mcmbn-L-rst) 
                           (list (cons (car combine-lsts) mcmbn-L-rst)))
                       (apply #'mon-combine (cdr combine-lsts))))))
;;
;;; :TEST-ME (mon-combine '(www ftp) '(exa) '(com org))

;;; ==============================
;;; :NOTE I mis-interpreted the `permut' in Pascal's `permutation's which is
;;;  duplicate agnositic in the true sense of the combinatoric use of a perm.
;;; What was wanted was a `combin-utation'. Pascal suggested as a solution the
;;; following helper fncn which can be leveraged by `mon-list-permute-variants'.
;;; :PREFIX "mvrtn-"
;;; :COURTESY Pascal J. Bourguignon :WAS `variations' :SOURCE email-correspondence
;;; :CREATED <Timestamp: #{2010-02-09T18:42:33-05:00Z}#{10062} - by MON KEY>
(defun mon-list-variant-forms (variegate-item variegate-lst)
  "Return the variant forms of VARIEGATE-ITEM in VARIEGATE-LST.\n
ITEM's postion is cycled for each possible position in LIST.\n
:EXAMPLE\n\n\(mon-list-variant-forms 'a '\(b c d\)\)\n
\(mon-list-variant-forms 'a '\(\)\)\n
:SEE-ALSO `mon-list-permute-variants', `mon-list-permute-1',
`mon-list-permute-2',`mon-permute-combine', `mon-permute-combine-1',
`mon-permute-combine-functions-TEST'.\n►►►"
  (if (null variegate-lst)
      (list (list variegate-item))
    (cons (cons variegate-item variegate-lst)
          (mapcar #'(lambda (mvrtn-L-1) 
                      (cons (car variegate-lst) mvrtn-L-1))
                  (mon-list-variant-forms variegate-item (cdr variegate-lst))))))
;;
;;; :TEST-ME (mon-list-variant-forms 'a '(b c d))
;;; :TEST-ME (mon-list-variant-forms 'a '())

;;; ==============================
;;; :SEE Notes for `mon-list-variant-forms' above.
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `permutations'
;;; :WAS (defun permutations (list)
;;;   (mapcan #'(lambda (item)
;;;             (if (= 1 (length list))
;;;                 (list (list item))
;;;                 (mapcar #'(lambda (rest) (cons item rest))
;;;                         (permutations (remove item list)))))
;;;           list))
;;; ==============================
;;; :PREFIX "mprmt-"
;;; :COURTESY Pascal J. Bourguignon :WAS `permutations' :SOURCE email-correspondence
;;; :CREATED <Timestamp: #{2010-02-09T18:43:44-05:00Z}#{10062} - by MON KEY>
(defun mon-list-permute-variants (elements)
  "Return a perumuted list each elt of ELEMENTS.\n
Result is a list of permuted lists.\n
:EXAMPLE\n\n\(mon-list-permute-variants '\(a b c d\)\)\n
:NOTE This is one of three list permuting functions.
This one accumulates permuatations by leveraging `mon-mapcan',
`mon-list-variant-forms' and recursion.\n
:SEE-ALSO `mon-list-permute-1', `mon-list-permute-2',`mon-permute-combine',
`mon-permute-combine-1', `mon-permute-combine-functions-TEST'.\n►►►"
  (cond ((null elements) (list elements))
        ((null (cdr elements)) (list elements))
        (t (mon-mapcan #'(lambda (mprmt-L-1)
                           (mon-list-variant-forms (car elements) mprmt-L-1))
                       (mon-list-permute-variants (cdr elements))))))
;;
;;; :TEST-ME (mon-list-permute-variants '(a b c d))

;;; =======================
;;; :PREFIX "mprm-"
;;; :COURTESY Christoph Conrad <cc@cli.de>
;;; :CHANGESET 2331 <Timestamp: #{2010-11-27T16:11:58-05:00Z}#{10476} - by MON KEY>
;;; :CHANGESET 2112
(defun mon-list-permute-1 (perm-lst)
  "Return a permuted list each elt of PERM-LST.\n
PERM-LST is a list of elts to permute.\n
\(with-current-buffer 
    \(get-buffer-create \"*MON-LIST-PERMUTE-1*\"\) 
  \(pp \(mon-list-permute-1 '\(a b c d\)\) \(current-buffer\)\)
  \(display-buffer \(current-buffer\)\)\)
:NOTE This is one of three list permuting functions. 
This one accumulates permuatations by leveraging nested `mon-mapcan' forms,
recursion and non-local exits with catch/throw.\n
:SEE-ALSO , `mon-list-variant-forms', `mon-list-permute-1',
`mon-list-permute-2', `mon-permute-combine', `mon-permute-combine-1',
`mon-permute-combine-functions-TEST'.\n►►►"
  (if (atom perm-lst)
      (list perm-lst)
    (mon-mapcan #'(lambda (mprm-L-1)    
                    (mon-mapcan #'(lambda (mprm-L-2)
                                    (list (cons mprm-L-1 mprm-L-2)))
                                (mon-list-permute-1
                                 (catch 'mon-list-permute-1-removed-1
                                   (let ((mprm-itr -1))
                                     (mapc #'(lambda (mprm-L-3-elt)
                                               (incf mprm-itr)
                                               (when (equal mprm-L-3-elt mprm-L-1)
                                                 (throw 'mon-list-permute-1-removed-1
                                                        (mon-sublist-gutted mprm-itr 1 perm-lst))))
                                           perm-lst)))) ))
                perm-lst)))
;;;
;;; :TEST-ME (mon-list-permute-1 '("a_thing" "b_thing" "c_thing" "d_same_thing"))
;;; :TEST-ME (mon-list-permute-1 '(a b c d))

;;; ==============================
;;; :COURTESY Thomas A. Russ comp.lang.lisp Date: 2000/09/26
;;; Re: Q: tail-recursive procedure to generate all permutations of a list
;;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/18821391508def0d')
(defun mon-list-permute-2 (perm-lst)
  "Return a list of all permutations of the input PERM-LST.\n
PERM-LST is a list of lists.\n
:EXAMPLE\n\n\(mon-list-permute-2 '\(a b c d\)\)\n
:NOTE This is one of three list permuting functions.
This one accumulates permutations as if by `loop'.\n
:SEE-ALSO `mon-list-permute-1', `mon-list-permute-variants',
`mon-list-variant-forms', `mon-permute-combine', `mon-permute-combine-1'.\n►►►"
  (if (< (length perm-lst) 2)
      (list perm-lst)
    (loop with len = (length perm-lst)
          with first-element = (car perm-lst)
          with result = nil
          for seq in (mon-list-permute-2 (cdr perm-lst))
          do (loop for i below (length perm-lst)
                   as tail = seq then (cdr tail)
                   do (push ;; :WAS (nconc (subseq seq 0 i)
                       (nconc  (mon-subseq seq 0 i)
                               (cons first-element tail))
                       result))
          finally (return result))))

;;; ==============================
;;; :PREFIX "mpc-"
;;; :COURTESY Erann Gat <gat@robotics.jpl.nasa.gov>
;;; :SOURCE Followup-To: comp.lang.lisp - :DATE Wed, 11 Jan 1995 11:33:58
;;; :CREATED <Timestamp: Wednesday July 22, 2009 @ 10:23.12 AM - by MON KEY>
(defun mon-permute-combine (prm-cmbn-lst1 prm-cmbn-lst2)
  "Efficient \"permute-combine\", works with PRM-CMBN-LST1 of arbitrary length.\n
:EXAMPLE\n\n\(mon-permute-combine '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
\(pp-display-expression 
 \(mon-permute-combine '\(a b \"W\" 1\) '\(Z E \"Q\" 9\)\)
 \"*MON-PERMUTE-COMBINE*\"\)\n
\(mon-permute-combine-functions-TEST\)\n
:NOTE This is one of two permute-combine functions.
This one leverages nested dolist forms and non-local exits when testing
membership inside the innermost dolist loop.\n
:ALIASED-BY `mon-list-permute-combine'\n
:SEE-ALSO `mon-permute-combine-1', `mon-list-variant-forms',
`mon-list-permute-variants', `mon-list-permute-1', `mon-list-permute-2',
`mon-permute-combine-functions-TEST'.\n►►►"
  (if (null prm-cmbn-lst1)
      (list '()) ;; '(())
    (let ((mpc-lst3 (mon-permute-combine (cdr prm-cmbn-lst1) prm-cmbn-lst2))
          (mpc-rslt nil))
      (dolist (mpc-do-a prm-cmbn-lst2)
        (dolist (mpc-do-b mpc-lst3)
          ;; Common Lisp can do this: 
          ;;  (unless (member a b :key #'second)
          ;; and it appears to work with Elisp if we substitute `member*':
          ;;  (unless (member* mpc-do-a mpc-do-b :key 'second)
          ;;    (push (cons (list (car prm-cmbn-lst1) mpc-do-a) mpc-do-b) mpc-rslt))
          ;; But, byte-compiler whines about using `member*' with :key, so we
          ;; are forced to this shite:
          ;; 
          ;; :WAS 
          ;; (unless (member* mpc-do-a mpc-do-b :key 'second)
          ;; (push (cons (list (car prm-cmbn-lst1) mpc-do-a) mpc-do-b) mpc-rslt)) ))
          (unless (catch 'mpc-found
                    (and (mapc #'(lambda (mpc-L-1) 
                                   (when (equal mpc-L-1 mpc-do-a) 
                                     (throw 'mpc-found t)))
                               (mapcar #'cadr mpc-do-b))
                         nil))
            (push (cons (list (car prm-cmbn-lst1) mpc-do-a) mpc-do-b) mpc-rslt)) ))
      (setq mpc-rslt (nreverse mpc-rslt)))))
;;
;;; ==============================
;;; :PREFIX "mpc1-" 
(defun mon-permute-combine-1 (combine-lst1 combine-lst2)
  "Permutations/combinations permute COMBINE-LST1 with COMBINE-LST2.\n
:EXAMPLE\n\n\(mon-permute-combine-1 '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
\(pp-display-expression 
 \(mon-permute-combine-1 '\(a b \"W\" 1\) '\(Z E \"Q\" 9\)\)
 \"*MON-PERMUTE-COMBINE*\"\)\n
\(mon-permute-combine-functions-TEST\)\n
:NOTE This is one of two permute-combine functions.
This one leverages `mon-mapcan' and recurses inside the an inner mapcar form.\n
:ALIASED-BY `mon-list-permute-combine-1'\n
:SEE-ALSO `mon-permute-combine', `mon-list-variant-forms',
`mon-list-permute-variants', `mon-list-permute-1', `mon-list-permute-2',
`mon-permute-combine-functions-TEST'.\n►►►"
  (if (null combine-lst1)
      (list '());; '(())
    (mon-mapcan #'(lambda (mpc1-L-1)
                    (mapcar #'(lambda (mpc1-L-2) 
                                (cons (list (car combine-lst1) mpc1-L-1) mpc1-L-2))
                            (mon-permute-combine-1 (cdr combine-lst1) 
                                                   (remove mpc1-L-1 combine-lst2))))
                combine-lst2)))

;;; ==============================
;;; NOT-WORKING
;;; :PREFIX "mpc2-"
;; (defun mon-permute-combine-2 (combine-lst-1 combine-lst-2)
;;   "Permutations/combinations permute COMBINE-LST-1 withe COMBINE-LST-2.\n
;; :EXAMPLE:\n\n\(mon-permute-combine-2 '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
;; :SEE-ALSO `mon-permute-combine', , `mon-list-variant-forms',
;; `mon-permute-combine-functions-TEST', `mon-list-permute-variants', `mon-list-permute-1'.\n►►►"
;;   (let ((mpc2-rslt nil))
;;     (dolist (mpc2-D-1 combine-lst-1) ;; (mpc2-D-1 combine-lst-2)
;;       (dolist (mpc2-D-2 combine-lst-2)
;;         (unless (equal mpc2-D-1 mpc2-D-2) 
;;           ;; :NOTE BUGGY!
;;           ;; (push (list (list (car combine-lst-1) mpc2-D-1) (list (car combine-lst-2) mpc2-D-2)) mpc2-rslt))))
;;           ;;mpc2-rslt))
;;           (push (list (list (car combine-lst-1) mpc2-D-2)
;;                       (list (car combine-lst-2) mpc2-D-1))
;;                 mpc2-rslt))))
;;         ;;mpc2-rslt))
;;           ;; (push (list (list (car combine-lst-1) mpc2-D-1) 
;;           ;;             (list (cadr combine-lst-2) mpc2-D-2))  ;; (list (cadr combine-lst-1) mpc2-D-2))
;;           ;;       mpc2-rslt)
;;     (setq mpc2-rslt (nreverse mpc2-rslt))))

;;; ==============================
;;; :CHANGESET 2291
;;; :CREATED <Timestamp: #{2010-11-10T14:15:34-05:00Z}#{10453} - by MON KEY>
(defun mon-list-string-longest (check-string-lst)
  "Return length of longest string in sequence CHECK-STRING-LST.\n
Return value is two elt list, car is an integer, cadr is CHECK-STRING-LST.\n
If a string does not satisfy `mon-string-not-null-nor-zerop' signal an error.\n
:EXAMPLE\n\n\(mon-list-string-longest '\(\"EDITOR\" \"HOME\" \"LANG\"\)\)\n
\(= \(car \(mon-list-string-longest '\(\"EDITOR\" \"HOME\" \"LANG\"\)\)\) 6\)\n
\(and \(null \(ignore-errors 
             \(mon-list-string-longest '\(\"EDITOR\" \"HOME\" \"LANG\" 8\)\)\)\)
     \"errored successfully\"\)\n
\(let* \(\(vec-w-lngst \(mon-list-string-longest '\(\"EDITOR\" \"HOME\" \"LANG\"\)\)\)
       \(w-vec  \(make-vector \(car vec-w-lngst\) nil\)\)\)
  \(dotimes \(wv \(car vec-w-lngst\) 
               `\(,\(concat w-vec\) ,vec-w-lngst ,w-vec\)\)
    \(let \(\(w-str \(caadr vec-w-lngst\)\)\)
      \(aset w-vec wv \(aref w-str wv\)\)\)\)\)\n
:NOTE The error signalling is intended as a hard filter for functions which
coerce string elements to vectors.  By preventing null and zerop length strings,
a calling function which maps a sequence of strings can reuse an existing vector
rather than repeatedly instantiating a new vector with each iteration.\n
:ALIASED-BY `mon-string-longest-in-list'\n
:SEE-ALSO `mon-sequence-mappable-p' ,`mon-list-string-reader',
`mon-string-or-null-and-zerop', `string-or-null-p'.\n►►►"
  (let ((mlsrca-lngst 0))
    (mapc #'(lambda (mlsr-L-1) 
              (or (and (mon-string-not-null-nor-zerop mlsr-L-1)
                       (setq mlsr-L-1 (length mlsr-L-1))
                       (or (and (> mlsr-L-1 mlsrca-lngst)
                                (setq mlsrca-lngst mlsr-L-1))
                           t))
                  (mon-error-string-err-format 
                   "mon-list-string-longest" "mlsr-L-1" mlsr-L-1 t)))
          check-string-lst)
    (list mlsrca-lngst check-string-lst)))

 
;;; ==============================
;;; :CHANGESET 2064
;;; :CREATED <Timestamp: #{2010-08-13T19:53:24-04:00Z}#{10325} - by MON KEY>
(defun mon-bool-vector-pp (bool-vec)
"Return print representation of bool-vector BOOL-VECT in various formats.\n
When BOOL-VECT length is greater than 29 return value has the format:\n
 \( :bit-string \"#*10 ... 01\" :true-table [t t ... nil t] :bool-vector BOOL-VEC \)\n
When BOOL-VECT length is less than 29 in addition to the values above return
value contains the following key value pairs:\n
 ( :binary \"#b10 ... 01\" :decimal 123 ... 9 :octal \"#o7 ... 77\" :hex \"#xFF...00\" 
   :bin-table [101 ... 010] :true-table [t t ... nil t] :bool-vector BOOL-VEC \)\n
The difference in return value for the first key as `:bit-string` instead of
`:binary` and with the string prefixed by sharpsign asterisks \"#*\" instead of
sharpsign b \"#b\" ensures that Emacs Lisp reader won't try to read the :binary
string as a number _but_ still allows us a to use the value where reasonable
with Common Lisp as a `simple-bit-vector' e.g.:\n
 \(setf  *<SOME-BIT-ARRAY>* \(make-array 8 :element-type 'bit\)\)\n
:EXAMPLE\n
\(mon-sublist 0 2 \(mon-bool-vector-pp \(make-bool-vector 29 t\)\)\)\n
\(mon-sublist 0 2 \(mon-bool-vector-pp \(make-bool-vector 30 nil\)\)\)\n
\(setq tt--bv \(make-bool-vector 28 t\)\)\n
 ;=> #&29\"\\377\\377\\377\x1f\"\n
\(vconcat tt--bv\)\n
 ;=> [t t t t t t t t t t t t t t t t t t t t t t t t t t t t t]\n
\(dolist \(tgl \(number-sequence 1 28 3\)\)
  \(unless \(eq tgl 28\)
    \(aset tt--bv tgl nil\)\)\)
\n
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
 ;=> \(:bin-table  [1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1]
      :true-table [t nil t t nil t t nil t t nil t t nil t
                    t nil t t nil t t nil t t nil t t nil t t]
      :bool-vector #&31\"m\\333\\266m\"\)\n
\(unintern \"tt--bv\" obarray\)\n
:NOTE Changing the prefix for the :binary keyword gets us Common Lisp's print
representation for `bit-vector's e.g.:\n
\(let \(\(bv \(make-bool-vector 29 t\)\)
      el/cl\)
  \(dolist \(tgl \(number-sequence 1 28 3\) bv\)
    \(aset bv tgl nil\)\)
  \(setq el/cl `\(:elisp-bv ,@\(mon-subseq \(mon-bool-vector-pp bv\) 1 2\)\)\)
  `\(:common-bv ,\(store-substring \(copy-sequence \(cadr el/cl\)\) 1 42\) ,@el/cl\)\)\n
:ALIASED-BY `mon-bool-vector-to-list'
:ALIASED-BY `mon-boolean-vector-to-list'
:ALIASED-BY `mon-list-ify-bool-vector'\n
:NOTE Assumes return value of `byteorder' is 108 \(big end first\).\n
:SEE-ALSO `mon-get-bit-table', `*mon-bit-table*', `mon-booleanp',
`mon-booleanp-to-binary', `mon-zero-or-onep', `mon-help-char-raw-bytes',
`mon-help-binary-representation', `logb', `fillarray'.\n►►►"
 (if (= (length bool-vec) 0)
     (mon-format :w-fun #'error 
               :w-spec '(":FUNCTION `mon-bool-vector-pp' "
                         "-- arg BOOL-VEC has length 0 "
                         "Emacs won't fetch that index, IHMO an Emacs bug"))
   (let* ((bv-len (length bool-vec))
          (w-props (<= bv-len 29))
          ;; vector of numberic 0's
          (to-bin (make-vector bv-len 0))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; :NOTE Can't the entire array frobbing rigmarole below be replaced:
          ;;
          ;; (mapconcat #'(lambda (vc) (format "%d" (mon-booleanp-to-binary vc))) bool-vec "") 
          ;;
          ;; Including when 0 length bvs:
          ;;
          ;; (and (mon-string-or-null-and-zerop
          ;;       (mapconcat #'(lambda (vc) 
          ;;                      (format "%d" (mon-booleanp-to-binary vc)))
          ;;                  (make-bool-vector 0 t) "")) 0)
          ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
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
         ;; Don't let it be readabe by Elisp, but let Common Lisp still
         ;; have a chance at frobbing it as a `simple-bit-vector'
       `(:bit-string ,(concat "#*" (mapconcat #'(lambda (vc) 
                                     (format "%d" (mon-booleanp-to-binary vc)))
                                 bool-vec ""))
         :bin-table   ,to-bin 
         :true-table  ,(vconcat bool-vec)
         :bool-vector ,bool-vec)))))
;;
;;; :TEST-ME (progn (setq tt--bv (make-bool-vector 29 t)) (mon-bool-vector-pp tt--bv))
;;; :TEST-ME (progn (setq tt--bv (make-bool-vector 256 t)) (mon-bool-vector-pp tt--bv))
;;; :TEST-ME (progn (setq tt--bv (make-bool-vector 0 t)) (mon-bool-vector-pp tt--bv))

;;; ==============================
;;; :SOURCE lisp/net/tramp-compat.el 
;;; :NOTE Following is an alternate implementation of `copy-tree' appearing in
;;; subr.el of emacs-version 23.2
;;;
;; ;; `copy-tree' is a built-in function in XEmacs.  In Emacs 21, it is
;; ;; an autoloaded function in cl-extra.el.  Since Emacs 22, it is part
;; ;; of subr.el.  There are problems when autoloading, therefore we test
;; ;; for `subrp' and `symbol-file'.  Implementation is taken from Emacs 23.
;; (defun tramp-compat-copy-tree (tree)
;;   "Make a copy of TREE (compat function)."
;;   (if (or (subrp 'copy-tree) (symbol-file 'copy-tree))
;;       (funcall (symbol-function 'copy-tree) tree)
;;     (let (result)
;;       (while (consp tree)
;; 	(let ((newcar (car tree)))
;; 	  (if (consp (car tree))
;; 	      (setq newcar (tramp-compat-copy-tree (car tree))))
;; 	  (push newcar result))
;; 	(setq tree (cdr tree)))
;;       (nconc (nreverse result) tree))))
;;; ==============================

;;; ==============================
(provide 'mon-seq-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-seq-utils.el ends here
;;; EOF

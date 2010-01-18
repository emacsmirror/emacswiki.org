;;; this is mon-cl-compat-regexps.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-cl-compat-regexps provides regular expressions for replacing the
;;; symbol-names from the cl-seq.el package with a `cl::' prefix. These are
;;; intended for use when with the mon-cl-compat.el package and allows for rapid
;;; source transformation in packages which use (eval-when-compile (require
;;; 'mon-cl-compat)) to prevent emacs compile time warnings w/re runtime calls
;;; to cl-*.el functions. The variable `*regexp-clean-cl-symbols*' can be called
;;; interactively as the argument to `mon-replace-region-regexp-lists' to
;;; quickly adjust your elisp source for use with mon-cl-compat.el.
;;; 
;;; All in all a sad state of affairs...
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*regexp-clean-cl-symbols*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES: When adding symbols to this the variable `*regexp-clean-cl-symbols*' in
;;; order to prevent inadverdently replacing strings to soon and thereby
;;; clobbering or otherwise doubling subportions of a symbol name it is necesary
;;; to ensure that elts of regexp list are given in most -> least specific
;;; order. IOW, where a symbol is prefixed by the name of another like symbol
;;; with a suffix, the longer suffixed symbol must come before the shorter
;;; prefixed symbol:
;;; 
;;;  ("remove-if-not" "cl::remove-if-not") ; :BEFORE `remove-if'
;;;  ("remove-if" "cl::remove-if")         ; :AFTER `remove-if-not'
;;
;;; Likewise, some symbols are prefixed with `cl-' these follow the same rule:
;;
;;;  ("cl-position" "cl::cl-position")    ; :BEFORE `position'  
;;;  ("position"   "cl::position")        : :AFTER `cl-position'
;; 
;;; Be careful with the destrucive symbols `nSYMBOL'. These need to appear
;;; before there non-destructive counterparts.
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;;
;;; THIRD-PARTY-CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-cl-compat-regexps.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2010-01-17T23:06:27-05:00Z}#{10027} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2010-01-17T02:42:34-05:00Z}#{10027} - by MON>
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
;;; ================================================================
;;; Copyright © 2010 MON KEY 
;;; ==============================
;;; CODE:

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-17T16:51:22-05:00Z}#{10027} - by MON KEY>
(defvar *regexp-clean-cl-symbols*
  '( ;; :CL-SEQ-VARIABLES
    ("cl-test-not" "cl::test-not")
    ("cl-test" "cl::test")
    ;;
    ("cl-if-not" "cl::if-not")
    ("cl-if" "cl::if")
    ;;
    ("cl-key\[^s]" "cl::key")
    ("cl-alist" "cl::alist")
    ;; :CL-SEQ-MACROS
    ("cl-parsing-keywords" "cl::parsing-keywords")
    ("cl-check-key" "cl::check-key")
    ("cl-check-test-nokey" "cl::check-test-nokey")
    ("cl-check-test" "cl::check-test")
    ("cl-check-match" "cl::check-match")
    ;; :CL-SEQ-FUNCTIONS
    ("reduce"  "cl::reduce")
    ("fill"    "cl::fill")
    ("replace" "cl::replace")
    ;;
    ("remove-duplicates" "cl::remove-duplicates")
    ("remove-if-not" "cl::remove-if-not")
    ("remove-if" "cl::remove-if")
    ("remove\\*" "cl::remove")
    ;;
    ("cl-delete-duplicates" "cl::cl-delete-duplicates")
    ("delete-duplicates" "cl::delete-duplicates")
    ;;
    ("delete-if-not" "cl::delete-if-not")
    ("delete-if"     "cl::delete-if")
    ("delete\\*"      "cl::delete*")
    ;;
    ("nsubstitute-if-not"  "cl::nsubstitute-if-not")
    ("nsubstitute-if"      "cl::nsubstitute-if")
    ("nsubstitute"         "cl::nsubstitute")
    ;;
    ("substitute-if-not" "cl::substitute-if-not")
    ("substitute-if"     "cl::substitute-if")
    ("substitute"        "cl::substitute")
    ;;
    ("find-if-not" "cl::find-if-not")
    ("find-if"     "cl::find-if")
    ("find"        "cl::find")
    ;;
    ("cl-position"     "cl::cl-position")
    ("position-if-not" "cl::position-if-not")
    ("position-if" 	  "cl::position-if")
    ("position"        "cl::position")
    ;;
    ("count-if-not" "cl::count-if-not")
    ("count-if"     "cl::count-if")
    ("count"        "cl::count")
    ;;
    ("mismatch" "cl::mismatch")
    ;;
    ("search" "cl::search")
    ;;
    ("stable-sort" "cl::stable-sort")
    ("sort\\*" "cl::sort*")
    ;;
    ("merge" "cl::merge")
    ;;
    ("member-if-not" "cl::member-if-not")
    ("member-if" "cl::member-if")
    ("member\\*" "cl::member*")
    ("cl-member" "cl::cl-member")
    ;;
    ("cl-adjoin" "cl::cl-adjoin")
    ;;
    ("rassoc-if-not" "cl::rassoc-if-not")
    ("rassoc-if" "cl::rassoc-if")
    ("rassoc\*" "cl::rassoc*")
    ;;
    ("assoc-if-not" "cl::assoc-if-not")
    ("assoc-if" "cl::assoc-if")
    ("assoc\\*" "cl::assoc*")
    ;;
    ("nunion" "cl::nunion")
    ("union"  "cl::union")
    ;;
    ("nintersection" "cl::nintersection")
    ("intersection" "cl::intersection")
    ;;
    ("nset-difference" "cl::nset-difference")
    ("set-difference" "cl::set-difference")
    ;;
    ("nset-exclusive-or" "cl::nset-exclusive-or")
    ("set-exclusive-or" "cl::set-exclusive-or")
    ;;
    ("subsetp" "cl::subsetp")
    ;;
    ("nsubst-if-not" "cl::nsubst-if-not")
    ("nsubst-if"     "cl::nsubst-if")
    ("nsubst"        "cl::nsubst")
    ;;
    ("subst-if-not" "cl::subst-if-not")
    ("subst-if" "cl::subst-if")
    ;;
    ("cl-nsublis-rec" "cl::cl-nsublis-rec")
    ("nsublis" "cl::nsublis")
    ;;
    ("cl-sublis-rec" "cl::cl-sublis-rec")
    ("sublis" "cl::sublis")
    ;;
    ("cl-tree-equal-rec" "cl::cl-tree-equal-rec")
    ("tree-equal" "cl::tree-equal")
    ;; :CL-*-FUNCTION
    ("subseq" "cl::subseq")
    ("ldiff" "cl::ldiff")
    ("coerce" "cl::coerce")
    ("typep" "cl::typep")
    ("cl-make-type-test" "cl::cl-make-type-test"))
"List of repexp pairs for use with `mon-replace-region-regexp-lists'.\n
Elements of list have the form:\n
 \(\"symbol\" \"cl::symbol\"\)\n
:NOTE In order to prevent inadverdently replacing strings to soon and thereby
clobbering or otherwise doubling subportions of a symbol name it is necesary
to ensure that elts of regexp list are given in most -> leas specific
order. IOW, where a symbol is prefixed by the name of another like symbol
with a suffix, the longer suffixed symbol must come before the shorter
prefixed symbol:\n
 \(\"remove-if-not\" \"cl::remove-if-not\"\) ; :BEFORE `remove-if'
 \(\"remove-if\" \"cl::remove-if\"\)         ; :AFTER  `remove-if-not'\n
Likewise, some symbols are prefixed with `cl-' these follow the same rule:\n
 \(\"cl-position\" \"cl::cl-position\"\)     ; :BEFORE `position'
 \(\"position\"   \"cl::position\"\)         : :AFTER  `cl-position'\n
Be careful with the destrucive symbols `nSYMBOL'. These need to appear
before there non-destructive counterparts.\n
This variable can be used as the argument to `mon-replace-region-regexp-lists'
when called-interactively to quickly adjust your elisp source for use with:
:FILE mon-cl-compat.el\n
:SEE-ALSO .\n►►►")
;;
;;; :TEST-ME *regexp-clean-cl-symbols*
;;
;;;(progn (makunbound '*regexp-clean-cl-symbols*)
;;;       (unintern '*regexp-clean-cl-symbols*) ) 

;;; :SYMBOL-LIST
;;; (mapc #'(lambda (q)
;;;           (princ (concat (cadr q) "\n") (current-buffer)))
;;;       *regexp-clean-cl-symbols*)

;;; ==============================
(provide 'mon-cl-compat-regexps)
;;; ==============================

;;; ================================================================
;;; mon-cl-compat-regexps.el ends here
;;; EOF

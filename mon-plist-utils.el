;;; mon-plist-utils.el --- functions for working plist-like lists
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-plist-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-08T11:07:39-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-plist-utils provides { some description here. }
;;
;; FUNCTIONS:►►►
;; `mon-plist-keys', `mon-plist-values', `mon-plist-remove-consing',
;; `mon-plist-remove-if',
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
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-plist-keys'           <- mon-utils.el 
;; `mon-plist-remove!'        <- mon-utils.el
;; `mon-plist-remove-if'      <- mon-utils.el
;; `mon-plist-remove-consing' <- mon-utils.el
;; 
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-plist-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-plist-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-08T11:07:39-05:00Z}#{10451} - by MON KEY>
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

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `plist-keys'
(defun mon-plist-keys (in-plist)
  "Return a list of keys in IN-PLIST \(conses\).\n
W-PLIST is any proper-list for which satisfies the test:\n
 \(zerop \(% \(length IN-PLIST\) 2\)\)\n
:EXAMPLE\n\n\(let \(\(w-plist \(mon-alphabet-as-type 'plistD->num\)\)
      gthr\)
  \(push `\(#::plist-orig ,w-plist\) gthr\)
  \(push `\(#::plist-keys ,\(mon-plist-keys w-plist\)\) gthr\)\)\n
\(let \(gthr\)
  \(mon-mapc #'\(lambda \(&rest x\) \(setq gthr \(append gthr x\)\)\)
            \(mon-alphabet-as-list-symbolD\)
            \(number-sequence 1 26\)\)
  \(setq gthr `\(#::list-keys ,\(mon-plist-keys gthr\) #::w-list ,gthr\)\)\)\n
\(mon-plist-keys-TEST\)\n
:SEE-ALSO `mon-plist-values', `mon-help-plist-functions',
`mon-map-obarray-symbol-plist-props', `mon-plist-remove-if',
`mon-plist-remove-consing', `mon-plist-remove', `remf', `remprop',
`mon-plist-keys-TEST'.\n►►►"
  (if (null in-plist)
      in-plist
    (cons (car in-plist) (mon-plist-keys (cddr in-plist)))))
;;
;;; :TEST-ME (mon-plist-keys-TEST)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-11-08T12:07:36-05:00Z}#{10451} - by MON KEY>
(defun mon-plist-values (in-plist)
  "Return a list of property values in IN-PLIST.\n
IN-PLIST is any proper-list satisfying `mon-list-proper-p' and the test:\n
 \(zerop \(% \(length IN-PLIST\) 2\)\)\n
When IN-PLIST is not null and above constraints are not met, signal an error.\n
:EXAMPLE\n\n\(mon-plist-keys \(mon-alphabet-as-type 'plistD->num\)\)\n
\(let \(\(chk-rtn \(mon-alphabet-as-plistD->num\)\)
      rtn\)
  \(push `\(#::plist-orig ,chk-rtn\) rtn\)
  \(push `\(#::plist-keys ,\(mon-plist-keys chk-rtn\)\) rtn\)
  \(push `\(#::plist-values ,\(mon-plist-values chk-rtn\)\) rtn\)\)\n
\(mon-plist-values-TEST\)\n
;; Follwing fail successfully:\n
\(mon-plist-values \(nconc \(mon-alphabet-as-plistD->num\) '\(a\)\)\)\n
\(mon-plist-values \(nconc \(mon-alphabet-as-plistD->num\) 'a\)\)\n
:SEE-ALSO `mon-plist-keys', `mon-plist-remove', `mon-help-plist-functions',
`mon-map-obarray-symbol-plist-props', `mon-plist-remove-if',
`mon-plist-remove-consing', `remf', `remprop', `mon-plist-values-TEST'.\n►►►"
  (if (and (or (mon-list-proper-p in-plist)
               (error (concat ":FUNCTION `mon-plist-values' "
                              "-- arg IN-PLIST not `mon-list-proper-p', got: %S type-of: %S")
                      in-plist (type-of in-plist)))
           (or (zerop (% (length in-plist) 2))
               (error (concat ":FUNCTION `mon-plist-values' "
                              "-- arg IN-PLIST length not `evenp', got-length: %d ")
                      (length in-plist)))
           (null in-plist))
      in-plist
    (let ((cpy-w-pl (copy-sequence in-plist))
          gthr-keys)
      (while cpy-w-pl
        (push (pop (cdr cpy-w-pl)) gthr-keys)
        (setq cpy-w-pl (cdr cpy-w-pl)))
      (setq gthr-keys (nreverse gthr-keys)))))
;;
;;; :TEST-ME (mon-plist-values-TEST)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-16T17:19:31-05:00Z}#{10026} - by MON KEY>
(defun mon-plist-remove! (pl-sym property-indicator)
  "Remove from PL-SYM's plist the PROPERTY-INDICATOR and its value.\n
Like `cl-remprop' and CL's `remprop' but without the latter's `remf'.\n
:EXAMPLE\n\n\(let \(the-pl\) 
  \(setplist the-pl \(mon-alphabet-as-type 'plistD->num\)\)
  \(dolist \(p-rmv 
            \(subseq 
             \(mapcar 'car \(mon-alphabet-as-type 'cons-keyD->num\)\)
             0 8\)
           \(pl-sym-plist the-pl\)\)
    \(mon-plist-remove! the-pl p-rmv\)\)\)\n
:SEE-ALSO `mon-plist-remove-if',`mon-plist-keys', `mon-plist-remove-consing',
`remf', `remprop', `mon-remove-if-not', `mon-delete-if', `mon-member-if'
`mon-help-plist-functions'.\n►►►"
  (let* ((CLDOREMF
          #'(lambda  (PLST TAG)
              (let ((mpr-p (cdr PLST)))
                (while (and (cdr mpr-p) (not (eq (car (cdr mpr-p)) TAG))) 
                  (setq mpr-p (cdr (cdr mpr-p))))
                (and (cdr mpr-p) (progn (setcdr mpr-p (cdr (cdr (cdr mpr-p)))) t)))))
         (CLREMPROP 
          #'(lambda (sym tag)
              (let ((mpr-plst (symbol-plist sym)))
                (if (and mpr-plst (eq tag (car mpr-plst)))
                    (progn (setplist sym (cdr (cdr mpr-plst))) t)
                    (funcall CLDOREMF mpr-plst tag))))))
    (funcall CLREMPROP pl-sym property-indicator)))
;;    
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
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-utilities.el :WAS `plist-remove'
;;; :CREATED <Timestamp: #{2010-01-16T16:55:29-05:00Z}#{10026} - by MON KEY>
(defun mon-plist-remove-consing (rmv-in-plist but-key)
  "Return a new rmv-in-plist with the each element of RMV-IN-PLIST but BUT-KEY.
:NOTE A suffix in result may be a suffix of RMV-IN-PLIST too.\n
:EXAMPLE\n\n(mon-plist-remove-consing (mon-alphabet-as-type 'plistD->num) :l)\n
:SEE-ALSO `mon-plist-remove!', `mon-plist-remove-if',
`mon-plist-remove-consing', `remf', `remprop', `mon-plist-keys',
`mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-help-plist-functions', `mon-help-plist-properties'.\n►►►"
  (if (eq (car rmv-in-plist) but-key)
      (cdr (cdr rmv-in-plist))
    (cons (car rmv-in-plist) 
          (cons (cadr rmv-in-plist) 
                (mon-plist-remove-consing (cddr rmv-in-plist) but-key)))))
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
`remf', `remprop', `mon-remove-if-not', `mon-delete-if', `mon-member-if',
`mon-help-plist-functions', `mon-help-plist-properties'.\n►►►"
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
(provide 'mon-plist-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-plist-utils.el ends here
;;; EOF


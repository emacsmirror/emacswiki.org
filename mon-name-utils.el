;;; mon-name-utils.el --- procedures to rotatate or permute string-like name forms
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-name-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-06-22T13:11:57-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: matching, convenience, extensions

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; Utils for rotation on name-forms. Used with `naf-mode'
;;
;; FUNCTIONS:►►►
;; `mon-string-rotate-name', `mon-make-name-return', `mon-make-name-lispy',
;; `mon-make-names-list', `mon-permute-combine', `mon-permute-combine-2',
;; `mon-list-variant-forms', `mon-list-permutations', `mon-perms',
;; `mon-permute-combine-functions-TEST', `mon-string-csv-regexp',
;; `mon-string-csv-rotate', `mon-csv-to-perms', `mon-string-permute-line',
;; `mon-string-splice-sep', `mon-string->strings-splice-sep',
;; `mon-string-to-regexp', `mon-string-infix',
;; `mon-rotate-region', `mon-rotate-string', `mon-rotate-next',
;; `mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp', 
;; `mon-indent-or-rotate', 
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;; `mon-intern-artist'
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
;; `*rotate-text-rotations*'
;;
;; ALIASED/ADVISED/SUBST'D:
;; `mon-permute-string'                 -> `mon-string-permute'
;;
;; DEPRECATED:
;; `mon-perms' :USE `mon-purmutations'
;;
;; RENAMED:
;; `mon-strings-splice-sep'             -> `mon-string-splice-sep'
;; `mon-string2strings-splice-sep'      -> `mon-string->strings-splice-sep'
;; `rotate-next'                        -> `mon-rotate-next'
;; `mon-csv-string-to-regexp'           -> `mon-string-csv-regexp'
;; `mon-rotate-keywords'                -> `mon-string-csv-rotate'
;; `mon-perm-words'                     -> `mon-string-permute-line'
;; `naf-unrotate-canonical'             -> `mon-string-rotate-name'
;;
;; MOVED:
;; `mon-permute-combine-functions-TEST' -> mon-testme-utils.el
;; `mon-list-flatten-rotated'           -> mon-utils.el
;; `mon-string-infix'                   -> mon-string.el
;; `mon-string-permute'                 -> mon-string.el
;; `mon-string-splice-sep'              -> mon-string.el
;; `mon-string->strings-splice-sep'     -> mon-string.el
;; `mon-string-to-regexp'               -> mon-string.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;; cl.el `mon-permute-combine', `mon-perms'
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-name-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-09-22} - by MON KEY>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-name-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-06-22T13:11:57-04:00Z} - by MON KEY>
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
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================


;;; CODE:

 
(eval-when-compile (require 'cl)) 

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
(defmacro mon-intern-artist (functionname texttoinsert)
  "Intern a function with name \"⍟:FUNCTIONNAME\". When invoked will insert
the string TEXTTOINSERT. Default is ⍟ if ⍟ is `char-displayable-p' 
Else uses @artist:\n
:EXAMPLE\n\n\(mon-intern-artist \"Cappiello\" \"Cappiello (Leonetto)\"\)\n
 => ⍟:Cappiello\n
 (⍟:Cappiello)\n => Cappiello (Leonetto)\n
:NOTE \(describe-char \(point\)\)⍟\n
:SEE-ALSO .\n►►►"
  (let ((pre-insert-char
         (if (char-displayable-p ?\u235F)
             "⍟:"
           "@artist:")))
    `(defun ,(intern (concat pre-insert-char functionname)) ()
       (interactive)
       (insert ,texttoinsert))))

;;; ==============================
;;; :NOTE Needs nested capture-groups for ", Jr." ", Sr." ", II" etc.
;;; :MODIFICATIONS <Timestamp: #{2009-08-13T16:37:24-04:00Z}#{09334} - by MON KEY>
(defun mon-string-rotate-name (&optional start end to-rotate insertp intrp)
  "Return an unrotated nameform with namestring in region.\n
:EXAMPLE\n\n\(mon-string-rotate-name nil nil \"Cappiello \(Leonetto Doctorow\)\")\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-line-string-unrotate-namestrings',
`mon-line-string-rotate-namestrings', `mon-line-strings-to-list',
`mon-make-lastname-firstname', `mon-make-name-lispy',
`mon-make-names-list'.\n►►►"
  (interactive "r\ni\ni\np")
  (let (msrn-str)
    (setq msrn-str 
          (if (or intrp (use-region-p))
              (mon-buffer-sub-no-prop start end)
            to-rotate))
    (setq msrn-str
          (with-temp-buffer
            (insert msrn-str)
            (mon-g2be -1)
            (whitespace-cleanup)  ;; :WAS (mon-kill-whitespace)
            (mon-g2be 1) ;; Needed?
	    ;; Needs nested capture-groups for ", Jr." ", Sr." ", II" etc.
	    (while (search-forward-regexp "^\\(.*\\) (\\(.*\\))$" nil t)
	      (replace-match "\\2 \\1" ))
            (mon-buffer-sub-no-prop) ))
    (cond (intrp
           (save-excursion
             (when intrp
               (delete-region start end)
               (insert msrn-str))))
          ((and (use-region-p) (not intrp))
           (if insertp
               (save-excursion
                 (delete-region start end)
                 (insert msrn-str))
             msrn-str))
          ((and to-rotate insertp (not intrp) (not (use-region-p)))
               (save-excursion
                 (insert msrn-str)))
          ((and to-rotate (not insertp) (not intrp) (not (use-region-p)))
           msrn-str))))
;;
;;; :TEST-ME (mon-string-rotate-name nil nil "Cappiello (Leonetto Doctorow)" )
;;; :TEST-ME (mon-string-rotate-name nil nil "Cappiello (Leonetto Doctorow)" t)
;;; :TEST-ME (apply 'mon-string-rotate-name nil '(nil "Cappiello (Leonetto Doctorow)"))

;;; ==============================
;;; :TODO :RENAME-ME mon-line-string-rotate-namestrings-fast
(defun mon-make-lastname-firstname (start end &optional intrp)
  "Return region as a list of names.\n
Region should contain two name instances \"Firstname\" \"Lastname\"  per line.\n
:EXMAMPLE\n\n\(save-excursion 
  \(goto-char \(1+ \(search-forward-regexp \"^►\" nil t\)\)\)
  \(let \(\(botp #'\(lambda \(\) 
                  `\(,\(line-beginning-position\) . ,\(line-end-position\)\)\)\)
        \(mhor #'\(lambda \(bd shw\) 
                  \(mon-help-overlay-result \(car bd\) \(cdr bd\) 78 shw\)\)\)\)
    \(dotimes \(i 3\)
      \(let \(\(bdN \(funcall botp\)\)\)
        \(funcall mhor bdN \(mon-make-lastname-firstname \(car bdN\) \(cdr bdN\)\)\)
        \(line-move-1 1\)\)\)\)\)\n
►\nFirstname1 Lastname1\nFirstname2 Firstname2\nFirstname3 Lastname3\n◄\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-string-rotate-namestrings',
`mon-line-string-unrotate-namestrings', `mon-make-name-lispy',
`mon-make-names-list'.\n►►►"
  (interactive "r\np")
  (let ((get-mmlf (mon-buffer-sub-no-prop start end)))
    (setq get-mmlf
          (with-temp-buffer 
            (insert get-mmlf)
            (mon-g2be -1)
            (while (search-forward-regexp "\\(\\w+\\) \\(\\w+\\)" nil t)
              (replace-match "\\2 (\\1)"))
            (mon-buffer-sub-no-prop) ))
    (if intrp
        (save-excursion 
          (goto-char start)
          (delete-region start end)
          (insert get-mmlf))
        get-mmlf)))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 22, 2009 @ 01:07.15 PM - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-09-24T13:08:18-04:00Z}#{09394} - by MON KEY>
(defun mon-make-names-list (start end &optional insrtp intrp)
  "Return list of names escaped for double quotes and parens.\n
:EXAMPLE\n\n\(mon-make-names-list
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"◄\"\) 2\)\)
►\nSome \(Dude1 Name1\)\nSome \(Dude2 Name2\)\nSome \(Dude3 Name3\)\n◄\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-string-rotate-namestrings',
`mon-line-string-unrotate-namestrings', `mon-make-lastname-firstname',
`mon-make-name-lispy'.\n►►►"
  (interactive "r\nP\np")
  (let ((mmnl-rspr3
         (mon-replace-string-pairs-region-no-insert start end
                                                    '(("^" "\(\"")
                                                      ("\)\\([:space:]?\\)$" "\)\"\)")))))
    (cond (intrp (save-excursion (prin1 mmnl-rspr3 (current-buffer))))
          (insrtp (prin1 mmnl-rspr3 (current-buffer)))
          (t mmnl-rspr3))))

;;; :TEST-ME (mon-make-names-list  
;;;         (1+ (search-forward-regexp "►")) (- (search-forward-regexp "►") 2))
;;
;;,---- :UNCOMMENT-T0-TEST
;;| ►
;;| Some (Dude1 Name1)
;;| Some (Dude2 Name2)
;;| Some (Dude3 Name3)
;;| ►
;;`----

;;; =======================
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 04:17.10 PM - by MON KEY>
;;; :WAS `mon-rotate-keywords' -> `mon-string-csv-rotate'
(defun mon-string-csv-rotate (csv-string &optional intrp)
  "Returns CSV-STRING \(Comma seperated string\) list in regexp optimized form.\n
CSV-STRING may be a comma-separated string.\n
:SEE-ALSO `mon-string-csv-regexp'.\n►►►"
  (interactive "sKeywords (comma-separated) :\np")
  (let ((rtn (concat "\\(" (mon-string-csv-regexp csv-string) "\\)")))
    (if intrp 
	(prin1 rtn (current-buffer))
      rtn)))
;;   
;;; :TEST-ME (mon-string-csv-rotate "Apropos, Info, Prin1, Princ")

;;; =======================
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 04:20.37 PM - by MON KEY>
;;; :WAS `mon-csv-string-to-regexp' -> `mon-string-csv-regexp'
(defun mon-string-csv-regexp (csv-string)
  "Translate CSV-STRING \(Comma Separated Values\) into regexp.\n
:EXAMPLE\n\n\(mon-string-csv-regexp \"A,B,C\"\)
;=> \\\\(A.*B.*C\\\\|A.*C.*B\\\\|B.*A.*C\\\\|B.*C.*A\\\\|C.*A.*B\\\\|C.*B.*A\\\\)
\(mon-string-csv-regexp \"Somedudes, name\"\)\)
;=> Somedudes.*name\\\\|name.*Somedudes\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-list-variant-forms',
`mon-permute-combine-functions-TEST', `mon-list-permutations', `mon-string-permute',
`mon-perms'.\n►►►"
  ;;(let* ((l (mon-perms (split-string str ",\\s-*"))))
  (let* ((mscr-lst (mon-list-permutations 
                    (save-match-data (split-string csv-string ",\\s-*")))))
    (mapconcat #'(lambda (mscr-L-n) 
                   (mapconcat #'identity mscr-L-n ".*"))
               mscr-lst "\\|")))
;;
;;; :TEST-ME (mon-string-csv-regexp "A,B,C")
;;; :TEST-ME (mon-string-csv-regexp "Somedudes, Name")

;;; ==============================
;;; :NOTE Not quite right yet.
(defun mon-csv-to-perms (csv-perm-string)
  "Return CSV-PERM-STRING to rotated form seperated by \" | \".
CSV-PERM-STRING a string of comma separated names, it has the form:\n
 \"Somedudes, Name\"\n 
Return value has the form:\n
 \"Somedudes, Name | Name, Somedudes\"\n
:EXAMPLE\n\n\(mon-csv-to-perms \"Somedudes, Name\"\)\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2',
`mon-list-variant-forms', `mon-permute-combine-functions-TEST',
`mon-list-permutations', `mon-string-permute', `mon-perms'.\n►►►"
  ;; (let* ((l (mon-perms (split-string str ",\\s-*"))))
  ;; why let*?
  (let* ((mctp-prm-lst (mon-list-permutations 
                        (save-match-data (split-string csv-perm-string ",\\s-*")))))
    (mapconcat #'(lambda (mctp-L-n) 
                   (mapconcat #'identity mctp-L-n ", "))
               mctp-prm-lst " | ")))
;;
;;; :TEST-ME (equal (mon-csv-to-perms "Somedudes, Name")
;;;                 "Somedudes, Name | Name, Somedudes")


;;; http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/4233ed82942f6998/13088d79f275e84b

;;; =======================
;;; :PREFIX "mprm-"
;;; :COURTESY Christoph Conrad <cc@cli.de>
;;; Rewrote without CL requirements -- no longer require remove*
;;; :CHANGESET 2112
(defun mon-perms (perm-list)
  "Return a permuted list each elt of PERM-LIST.\n
PERM-LIST is a list of elts to permute.\n
\(with-current-buffer 
    \(get-buffer-create \"*MON-PERMS*\"\) 
  \(pp \(mon-perms '\(a b c d\)\) \(current-buffer\)\)
  \(display-buffer \(current-buffer\)\)\)
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-list-variant-forms',
`mon-permute-combine-functions-TEST'.\n►►►"
  (if (atom perm-list)
      (list perm-list)
    (mon-mapcan #'(lambda (mprm-L-1)    
                    (mon-mapcan #'(lambda (mprm-L-2)
                                    (list (cons mprm-L-1 mprm-L-2)))
                                (mon-perms
                                 (catch 'mon-perms-removed-1
                                   (let ((itr -1))
                                     (mapc #'(lambda (mprm-L-3-elt)
                                               (incf itr)
                                               (when (equal mprm-L-3-elt mprm-L-1)
                                                 (throw 'mon-perms-removed-1
                                                        (mon-sublist-gutted itr 1 perm-list))))
                                           perm-list)))) ))
                perm-list)))

;;
;;; :TEST-ME (mon-perms '("Some_thing" "Name_thing" "More_thing" "Of_the_same_thing"))
;;; :TEST-ME (mon-perms '(Some_thing Name_thing More_thing Of_the_same_thing))

;;; ==============================
;;; :COURTESY Thomas A. Russ comp.lang.lisp Date: 2000/09/26
;;; Re: Q: tail-recursive procedure to generate all permutations of a list
;;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/18821391508def0d')
(defun mon-list-permute2 (perm-lst)
  "Return a list of all permutations of the input PERM-LST.\n
PERM-LST is a list of list.\n
:EXAMPLE\n\n(mon-list-permute2 '(a b c d))
:SEE-ALSO .\n►►►"
  (if (< (length perm-lst) 2)
      (list perm-lst)
    (loop with len = (length perm-lst)
          with first-element = (car perm-lst)
          with result = nil
          for seq in (mon-list-permute2 (cdr perm-lst))
          do (loop for i below (length perm-lst)
                   as tail = seq then (cdr tail)
                   do (push ;; :WAS (nconc (subseq seq 0 i)
                       (nconc  (mon-subseq seq 0 i)
                               (cons first-element tail))
                       result))
          finally (return result))))

;;; ==============================
;;; :PREFIX "mpc-"
;;; :NOTE Uses dolist -> dolist-> memeber &key -> push -> nreverse idiom.
;;; :COURTESY Erann Gat <gat@robotics.jpl.nasa.gov>
;;; :SOURCE Followup-To: comp.lang.lisp - :DATE Wed, 11 Jan 1995 11:33:58
;;; :SEE (URL `http://groups.google.com/group/comp.lang.lisp/msg/f1e3d259f7b2b15b')
;;; :CREATED <Timestamp: Wednesday July 22, 2009 @ 10:23.12 AM - by MON KEY>
(defun mon-permute-combine (prm-cmbn-lst1 prm-cmbn-lst2)
  "Efficient \"permute-combine\", works with PRM-CMBN-LST1 of arbitrary length.\n
:EXAMPLE\n\n\(mon-permute-combine '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
\(mon-permute-combine '\(a b\) '\(1 2 3 4\)\)\n
:NOTE Adapted from solution presented here:\n
:SEE (URL `http://groups.google.com/group/comp.lang.lisp/msg/f1e3d259f7b2b15b')\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-list-variant-forms',
`mon-permute-combine-functions-TEST', `mon-list-permutations', `mon-perms'.\n►►►"
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
;;; (mon-permute-combine '(a b) '(1 2 3 4))
;;
;;; :TEST-ME See below for `mon-permute-combine-functions-TEST'
;;
;;; :PREFIX "mpc1-"
(defun mon-permute-combine-1 (combine-lst1 combine-lst2)
  "Permutations/combinations permute COMBINE-LST1 with COMBINE-LST2.\n
ELISP <-> CL portable. Uses double lambda recursion `mon-mapcan' -> `mapcar' > recurse.\n
:EXAMPLE\n\n\(mon-permute-combine-1 '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-list-variant-forms',
`mon-permute-combine-functions-TEST', `mon-list-permutations', `mon-perms'.\n►►►"
  (if (null combine-lst1)
      (list '());; '(())
    (mon-mapcan #'(lambda (mpc1-L-1)
                    (mapcar #'(lambda (mpc1-L-2) 
                                (cons (list (car combine-lst1) mpc1-L-1) mpc1-L-2))
                            (mon-permute-combine-1 (cdr combine-lst1) 
                                                   (remove mpc1-L-1 combine-lst2))))
                combine-lst2)))
;;
;;; :PREFIX "mpc2-"
(defun mon-permute-combine-2 (combine-lst-1 combine-lst-2)
  "Permutations/combinations permute COMBINE-LST-1 withe COMBINE-LST-2.\n
:EXAMPLE:\n\n\(mon-permute-combine-2 '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-list-variant-forms',
`mon-permute-combine-functions-TEST', `mon-list-permutations', `mon-perms'.\n►►►"
  (let ((mpc2-rslt nil))
    (dolist (mpc2-D-1 combine-lst-2)
      (dolist (mpc2-D-2 combine-lst-2)
        (unless (equal mpc2-D-1 mpc2-D-2)
          ;; :NOTE BUGGY!
          ;;(push (list (list (car combine-lst-1) mpc2-D-1) (list (cadr combine-lst-2) b)) mpc2-rslt))))
          ;;mpc2-rslt))
          (push (list (list (car combine-lst-1) mpc2-D-1) 
                      (list (cadr combine-lst-1) mpc2-D-2))
                mpc2-rslt))))
    (setq mpc2-rslt (nreverse mpc2-rslt))))


;;; ============================================
;; :ROTATE-TEXT

;;; ============================================
;;; :CREATED: <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
;;; :SEE (URL `http://www.emacswiki.org/emacs/RotateText')
;;; :COURTESY Michal Nazarewicz <mina86/AT/tlen.pl> VERSION: v0.3 
;;; :MODIFICATIONS Aaron Hawley aaron.s.hawley@gmail.com
;;;
;;; :NOTE Following were originally loaded from site-lisp.
;;;       The code was buggy and didn't work but there were some gems in it
;;;       After Aaron Hawley cleaned it up on the emacs-wiki it worked.
;;;      I converted the namespace of my local copy to mon-*  and moved it here. 
;;;     The original stuff can be found in:
;;; :FILE no-site-lisp/rotate-text/rotate-text.el
;;; :FILE no-site-lisp/rotate-text/rot8.el 

;;; ==============================
;;; Like DoReMi-style commands that rotate a selected buffer string, replacing
;;; it in the buffer by each of a series of predefined replacements, in turn.
;;; ;; When writing graphical code, words need to be changed like “width” to
;;; “height”, or the word “left” to “right”. A “text rotator” in Emacs could
;;; make these changes in a single key binding. With the the point in Emacs on
;;; the word “width” and then the key sequence for the code rotator command (the
;;; original poster had in mind the divide key / on the keypad), it would be
;;; replaced with “height”.  ;; Various sets of words could be be rotated. If
;;; the selected text matches an item in any of the following lists then it
;;; replaces it with the next one in the list. (Or if its the last the first):
;;;
;;;  (width height) (left top right bottom) (start end) (red orange yellow green
;;;   blue indigo violet) (xx-small x-small small normal large x-large xx-large)
;;;  (zero one two) ... easy to define your own sets.
;;;
;;; This version of rotate-region builds a giant RegularExpression for finding
;;; matches rather than iterating on entries in the data structures. It is
;;; written defensively with error-checking, and rotates text “in the buffer”
;;; for the ‘rotate-region’ command. It has a ‘rotate-string’ procedure. At the
;;; end is a key binding for an ‘indent-or-rotate’ command.

;;; List of lists of words to rotate among.
;;; ==============================
;;(eval-when-compile
(defvar *rotate-text-rotations* nil
  "A dynamically assigned list of strings to rotate upon.\n
:EXAMPLE\n\n\(setq  *rotate-text-rotations*
       '\(\(\"width\" \"height\"\) 
         \(\"left\" \"top\" \"right\" \"bottom\"\)
         \(\"red\" \"orange\" \"yellow\" \"green\" \"indigo\" \"violet\" \"blue\"\)
         \(\"xx-small\" \"x-small\" \"small\" \"normal\" \"large\" \"x-large\" \"xx-large\"\)
         \(\"zero\" \"one\" \"two\"\)\)\)\n
:CALLED-BY `mon-rotate-region', `mon-rotate-string', `mon-rotate-next',
`mon-rotate-get-rotations-for'\n
:SEE-ALSO .\n►►►")
;;)

;;; :TEST-ME
;;; (setq  *rotate-text-rotations*
;;; '(("width" "height") 
;;;   ("left" "top" "right" "bottom")
;;;   ("red" "orange" "yellow" "green" "indigo" "violet" "blue")
;;;   ("xx-small" "x-small" "small" "normal" "large" "x-large" "xx-large")
;;;   ("zero" "one" "two")))

;;; ==============================
;;; :PREFIX "mrr-"
;;; :RENAMED `rotate-convert-rotations-to-regexp' -> `mon-string-rotate-to-regexp'
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-region (beg end &optional rotations w-case-ignored)
  "Rotate all matches in `*rotate-text-rotations*' between point and mark.\n
When optional arg ROTATIONS is ommitted defaults to `*rotate-text-rotations*'.
Signals an error when both are null.\n
When optional arg W-CASE-IGNORED is non-nil associations for ROTATE-STRING
accumulated as if by `member-ignore-case' default is as if by `member'. 
Likewise, when W-CASE-IGNORED is non-nil returned associations filtered by
`replace-match' with FIXEDCASE arg nil when W-CASE-IGNORED is ommitted is
the FIXEDCASE is non-nil. This means that when W-CASE-IGNORED is non-nil the
return value for ROTATE-STRING may have be case altered if this is not what is
wanted do not pass a non-nil value for w-case-ignored.\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-list-flatten-rotated', `mon-indent-or-rotate'.\n►►►"
  (interactive "r")
  (if (and (null rotations) (null *rotate-text-rotations*))
      (error (concat ":FUNCTION `mon-rotate-region' "
                     "-- arg ROTATIONS and/or variable `*rotate-text-rotations*' null"))
    (let ((mrr-rgx ;; `rotate-convert-rotations-to-regexp'
           (mon-string-rotate-to-regexp 
            (or rotations *rotate-text-rotations*)))
          (mrr-end-mrk (copy-marker end)))
      (save-excursion
        (goto-char beg)
        (while (search-forward-regexp mrr-rgx (marker-position mrr-end-mrk) t)
          (let* ((mrr-fnd (match-string 0))
                 (mrr-rplc (mon-rotate-next mrr-fnd 
                                            (or rotations *rotate-text-rotations*)
                                            w-case-ignored)))
            (replace-match mrr-rplc (if w-case-ignored nil t))))))))

;;; ==============================
;;; :RENAMED `rotate-string' -> `mon-rotate-string' -> mon-name-utils.el
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-string (rotate-string &optional rotations w-case-ignored)
  "Rotate all matches in ROTATE-STRING using associations in ROTATIONS.\n
When optional arg ROTATIONS is ommitted defaults to `*rotate-text-rotations*'.
Signals an error when both are null.\n
When optional arg W-CASE-IGNORED is non-nil associations for ROTATE-STRING
accumulated as if by `member-ignore-case' default is as if by `member'. 
Likewise, when W-CASE-IGNORED is non-nil returned associations filtered by
`replace-match' with FIXEDCASE arg nil when W-CASE-IGNORED is ommitted is
the FIXEDCASE is non-nil. This means that when W-CASE-IGNORED is non-nil the
return value for ROTATE-STRING may have be case altered if this is not what is
wanted do not pass a non-nil value for w-case-ignored.\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-list-flatten-rotated', `mon-indent-or-rotate'.\n►►►"
  (if (and (null rotations) (null *rotate-text-rotations*))
      (error (concat ":FUNCTION `mon-rotate-string' "
                     "-- arg ROTATIONS and/or variable `*rotate-text-rotations*' null"))
    (let ((mrs-rgx (mon-string-rotate-to-regexp (or rotations *rotate-text-rotations*)))
          (mrs-strt 0))
      (save-match-data
        (while (string-match mrs-rgx rotate-string mrs-strt)
          (let* ((mrs-fnd (match-string 0 rotate-string))
                 (mrs-rplc (mon-rotate-next mrs-fnd
                                            (or rotations *rotate-text-rotations*)
                                            w-case-ignored)))
            (setq mrs-strt (+ (match-end 0)
                              (- (length mrs-rplc) (length mrs-fnd))))
            (setq rotate-string 
                  (replace-match mrs-rplc (if w-case-ignored nil t) t rotate-string)))))
      rotate-string)))

;;; ==============================
;;; :RENAMED `rotate-next' -> `mon-rotate-next'
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-next (after-string &optional rotations w-case-ignored) ;; w-case-ignored
  "Return the next element after AFTER-STRING in ROTATIONS.\n
When optional arg ROTATIONS is ommitted defaults to `*rotate-text-rotations*'.
Signals an error when both are null.\n
When optional arg W-CASE-IGNORED is non-nil membership of next element
is as if by `member-ignore-case' default is as if by `member'.
:SEE-ALSO `mon-rotate-after-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-after-string-rotate-to-regexp',
`mon-list-flatten-rotated', `mon-indent-or-rotate'.\n►►►"
  (if (and (null rotations) (null *rotate-text-rotations*))
      (error (concat ":FUNCTION `mon-rotate-next' "
                     "-- arg ROTATIONS and/or variable `*rotate-text-rotations*' null"))
    (let ((mrn-rtns (mon-rotate-get-rotations-for 
                     after-string
                     (or rotations *rotate-text-rotations*) w-case-ignored)))
      (if (> (length mrn-rtns) 1)
          (error (concat ":FUNCTION `mon-rotate-next' " 
                         "-- ambiguous rotation for %s") after-string)
        (if (< (length mrn-rtns) 1)
            (error  (concat ":FUNCTION `mon-rotate-next' " 
                            "-- unknown rotation for %s") after-string)
          (let ((occurs-in-mrn-rtns 
                 (if w-case-ignored
                     (member-ignore-case after-string (car mrn-rtns))
                   (member after-string (car mrn-rtns)))))
            (if (null occurs-in-mrn-rtns)
                ;; If we get this far, this should not occur:
                (error (concat ":FUNCTION `mon-rotate-next' " 
                               "-- unknown rotation for %s") after-string)
              (if (null (cdr occurs-in-mrn-rtns))
                  (caar mrn-rtns)
                (cadr occurs-in-mrn-rtns)))))))))

;;; ==============================
;;; :RENAMED `rotate-get-rotations-for' -> `mon-rotate-get-rotations-for' -> mon-name-utils.el
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-get-rotations-for (string &optional rotations w-case-ignored)
  "Return the string rotations for STRING in ROTATIONS.\n
When optional arg ROTATIONS is ommitted defaults to `*rotate-text-rotations*'.
Signals an error when both are null.\n
When optional arg W-CASE-IGNORED is non-nil test for STRING membership in the
rotations list is as if by `member-ignore-case' default is as if by `member'.
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-list-flatten-rotated', `mon-indent-or-rotate'.\n►►►"
  (if (and (null rotations) (null *rotate-text-rotations*))
      (error (concat ":FUNCTION `mon-rotate-get-rotations-for' "
                     "-- arg ROTATIONS and/or variable `*rotate-text-rotations*' null"))
    (remq nil (mapcar #'(lambda (mrgrf-rot) 
                          (when (if w-case-ignored
                                    (member-ignore-case string mrgrf-rot)
                                  (member string mrgrf-rot))
                            mrgrf-rot))
                      (or rotations *rotate-text-rotations*)))))

;;; ==============================
;;; :RENAMED `rotate-convert-rotations-to-regexp' -> `mon-string-rotate-to-regexp'
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 04:13.28 PM - by MON KEY>
(defun mon-string-rotate-to-regexp (rotations)
  "Flatten ROTATIONS and optimized for regexp with `regexp-opt'.\n
ROTATIONS is a list(s) strings.\n 
:EXAMPLE\n\n\(mon-string-rotate-to-regexp 
 '\(\"This\" \"is\" \"a\" \"list\" \"to\" \"rotate\"\)\)\n
 \(mon-string-rotate-to-regexp 
 '\(\(\"This\" \"is\" \"a\" \"list\" \"to\" \"rotate\"\) 
   \(\"Thi\" \"sis\" \"ali\" \"stt\" \"oro\" \"tat\" \"e\"\) 
   \(\"rot\" \"ate\" \"tol\" \"ist\" \"ais\" \"Thi\" \"s\"\)\)\)\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-list-flatten-rotated', `mon-indent-or-rotate'.\n►►►"
  (regexp-opt (mon-list-flatten-rotated rotations)))
;;
;; ,---- :UNCOMMENT-BELOW-TO-TEST
;; | (equal (mon-string-rotate-to-regexp
;; |         '(("This" "is" "a" "list" "to" "rotate")
;; |           ("Thi" "sis" "ali" "stt""oro" "tat" "e")
;; |           ("rot" "ate" "tol" "ist" "ais" "Thi" "s")))
;; |        (concat "\\(?:This?\\|a\\(?:is\\|li\\|te\\)\\|ist?\\|list\\|oro\\|rot\\"
;; |                "(?:ate\\)?\\|s\\(?:is\\|tt\\)\\|t\\(?:at\\|ol?\\)\\|[aes]\\)"))
;; `----


;;; ==============================
;;; :NOTE (local-set-key [tab] 'indent-or-rotate)
;;; :RENAMED `indent-or-rotate' -> `mon-rotate-or-indent' -> mon-name-utils.el
(defun mon-indent-or-rotate (&optional w-case-ignored)
  "If point is at end of a word rotate else indent the line.\n
:SEE-ALSO `mon-rotate-region', `mon-rotate-string', `mon-rotate-next', 
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-list-flatten-rotated', `mon-indent-or-rotate'.\n►►►"
  (interactive)
  (if (looking-at-p "\\>")
      (mon-rotate-region 
       ;; :NOTE could `prog1' this also
       (save-excursion (forward-word -1) (point)) (point)
       ;; 
       ;; (or rotations *rotate-text-rotations*)
       ;; (mon-line-strings-one-list (line-beginning-position) (line-end-position))
       ;; (mon-string (mon-buffer-sub-no-prop (line-beginning-position) (line-end-position))
       w-case-ignored
       )
    (indent-for-tab-command)))

;;; =======================
;;; :RENAMED `naf-make-name-return' -> `mon-make-name-return'
;;; :NOTE The dog looked to me and said, "Why are you here?"
(defun mon-make-name-return (name-region)
  "mon-make-name-return\n
:SEE-ALSO .\n►►►"
   (let* ((mmnr-rgn name-region)
          (mmnr-tmp-nm (save-match-data (split-string mmnr-rgn)))
          (mmnr-to-put (reverse (cons (butlast mmnr-tmp-nm) (last mmnr-tmp-nm)))))
     (insert (format "\n%s %s" (car mmnr-to-put) (cadr mmnr-to-put)))))

;;; ==============================
(defun mon-make-name-lispy (lispy-name-region)
  "Return LISPY-NAME-REGION as list as a stringified names.\n
Names rotated as:
 (\"Lastname\" (\"&restnames\")) to parens with quote ' escaped by two slashes.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-string-rotate-namestrings',
`mon-line-string-unrotate-namestrings', `mon-make-lastname-firstname',
`mon-make-name-lispy', `mon-make-names-list'.\n►►►"
   (setq lispy-name-region  
         (if (string-match "'" lispy-name-region)
             (replace-match "\\'" nil t lispy-name-region)))
   (let* ((mmnl-rgn lispy-name-region)
          (mmnl-tmp-nm (split-string mmnl-rgn))
          (mmnl-to-put (reverse (cons (butlast mmnl-tmp-nm) (last mmnl-tmp-nm)))))
    mmnl-to-put))

;;; ==============================
;;; :WORKING-AS-OF
;;; :CREATED <Timestamp: Friday February 13, 2009 @ 09:16.54 PM - by MON KEY>
;;; ==============================
;;; Regexp template for finding nameforms in regions - used in `mon-cln-ulan'.
;;;            (region-name (when (and transient-mark-mode mark-active)
;;; 	      (buffer-substring-no-properties (region-beginning) (region-end))))
;;;              (test-name (when (and region-name)
;;; 	       (cond
;;; 		((string-match "\\(\\([A-Z][a-z]+\\)\\([: :](\\)\\([A-Z][a-z]+\\)\\()\\)\\)" region-name) 
;;; 		 (concat (match-string 2 region-name) "%2C+"  (match-string 4 region-name)))
;;; 		((string-match "\\(\\([A-Z][a-z]+\\)\\(,[: :]\\)\\([A-Z][a-z]+\\)\\)" region-name)
;;; 		 (concat (match-string 2 region-name) "%2C+" (match-string 4 region-name)))
;;; 		((string-match "\\(\\([A-Z][a-z]+\\)\\([: :]\\)\\([A-Z][a-z]+\\)\\)" region-name)
;;; 		 (concat (match-string 4 region-name) "%2C+" (match-string 2 region-name))))))
;;; ==============================

;;; ==============================
;;; Function is correct. but,
;;; :NOT-WORKING-AS-OF
;;; :CREATED <Timestamp: Monday February 16, 2009 @ 08:38.20 PM - by MON KEY>
;;;(defun make-list-of-string ()
;;; (interactive)
;;; (with-temp-buffer 
;;;   (goto-char (buffer-end 0))
;;;   (while (and (line-move-1 1))
;;;     (let ((bol (beginning-of-line))
;;;	   (eol (end-of-line)))
;;;       ;; is replace-string-region supposed to call here? was it defined?
;;; ;;       (replace-string-region bol eol)))))  


;;; =======================
;;; (defun naf-canonical-name-form (Lastname Firstname &optional Middlename &rest Restname)
;;; "conditionally test on a function called naf-canonical-name-form which takes
;;; four args returning the list accoriding to
;;; the position of names - to be used  ina functional style"
;;; `(,Lastname ,Firstname ,Middlename ,Restname))
;;; ;
;;; (....
;;; (let* (;(name-form `(,Lastname ,Firstname ,Middlename ,Restname))
;;;         (LN (first name-form))
;;;         (FN (second name-form))
;;;         (MN (third name-form))
;;;         (RN (fourth name-form)))
;;;   (print (not LN)) ;evaluates to nil if set - t if not
;;;   (print (not FN)) ;evaluates to nil if set - t if not
;;;   (print (not MN)) ;evaluates to nil if set - t if not
;;;   (print (not RN)) ;evaluates to nil if set - t if not
;;;   (format "%s %s %s %s" LN FN MN RN))

;;; ==============================
;;; This isn't working but almost.
;;(setq the-region "Firstname d'Middlename MoreName AnotherName Lastname")
;;; (defun naf-make-name ()
;;; (let* ((region the-region)
;;;        (temp-name (split-string region))
;;;        (to-put (reverse (cons (butlast temp-name) (last temp-name)))))
;;; (princ to-put (current-buffer))))
;;; ==============================

;;; ==============================
(provide 'mon-name-utils)
;;; ==============================

;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ====================================================================
;;; mon-name-utils.el ends here
;;; EOF

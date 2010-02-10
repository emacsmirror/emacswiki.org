;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is naf-name-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; Utils for rotation on name-forms. Used with `naf-mode'
;;;
;;; FUNCTIONS:►►►
;;; `naf-unrotate-canonical', `naf-make-name-return', `naf-make-name-for-lisp'
;;; `mon-make-names-list', `mon-permute-combine', `mon-permute-combine-2',
;;; `mon-variations', `mon-permutations', `mon-perms',
;;; `mon-test-permute-combine-functions', `mon-string-csv-regexp',
;;; `mon-string-csv-rotate', `mon-csv-to-perms', `mon-string-permute-line'
;;; `mon-string-splice-sep', `mon-string->strings-splice-sep'
;;; `mon-convert-list-regexp', `mon-string-infix'
;;; `mon-rotate-region', `mon-rotate-string', `mon-rotate-next'
;;; `mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp', 
;;; `mon-rotate-flatten-list',`mon-indent-or-rotate', `mon-string-permute'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;; `mon-intern-artist'
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;; `mon-permute-string' -> `mon-string-permute'
;;;
;;; DEPRECATED:
;;; `mon-perms' USE: `mon-purmutations'
;;;
;;; RENAMED:
;;; `mon-strings-splice-sep'        -> `mon-string-splice-sep'
;;; `mon-string2strings-splice-sep' -> `mon-string->strings-splice-sep'
;;; `rotate-next'                   -> `mon-rotate-next'
;;; `mon-csv-string-to-regexp'      -> `mon-string-csv-regexp'
;;; `mon-rotate-keywords'           -> `mon-string-csv-rotate'
;;; `mon-perm-words'                -> `mon-string-permute-line'
;;;
;;; MOVED:
;;;
;;; REQUIRES:
;;; 'cl used-by:`mon-permute-combine', `mon-perms'
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/naf-name-utils.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-09-22} - by MON KEY>
;;; 
;;; FILE-CREATED:
;;; <Timestamp: Wednesday July 22, 2009 @ 01:11.57 PM - by MON KEY>
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
;;; Copyright © 2009, 2010 MON KEY 
;;; ==============================
;;; CODE:

;; `mon-permutations', `mon-perms', `mon-permute-combine' 
(eval-when-compile (require 'cl)) 

;;; ==============================
(defmacro mon-intern-artist (functionname texttoinsert)
  "Intern a function with name \"⍟:FUNCTIONNAME\". When invoked will insert
the string TEXTTOINSERT. Default is ⍟ if ⍟ is `char-displayable-p' 
Else uses @artist:\n
:EXAMPLE
>(mon-intern-artist \"Cappiello\" \"Cappiello (Leonetto)\")
 => ⍟:Cappiello
> (⍟:Cappiello)
 => Cappiello (Leonetto)
:NOTE ⍟ <- \(mon-insert-unicode \"235F\"\)
      Name: APL FUNCTIONAL SYMBOL CIRCLE STAR\n
:SEE-ALSO .\n►►►"
  (let ((pre-insert-char
         (if (char-displayable-p ?\u235F)
             "⍟:"
           "@artist:")))
    `(defun ,(intern (concat pre-insert-char functionname)) ()
       (interactive)
       (insert ,texttoinsert))))

;;; ==============================
;;; :TODO :RENAME-ME
;;; :NOTE Needs nested capture-groups for ", Jr." ", Sr." ", II" etc.
;;; :MODIFICATIONS <Timestamp: #{2009-08-13T16:37:24-04:00Z}#{09334} - by MON KEY>
(defun naf-unrotate-canonical (&optional start end to-rotate insertp intrp)
  "Return an unrotated nameform withe namestring in region.\n
:EXAMPLE\n\(naf-unrotate-canonical nil nil \"Cappiello \(Leonetto Doctorow\)\")\n
:SEE-ALSO; `mon-line-string-unrotate-namestrings'
`mon-line-string-rotate-namestrings' `mon-line-strings-to-list', 
`mon-make-lastname-firstname', `naf-make-name-for-lisp', `mon-make-names-list'.\n
Used in `naf-mode'.\n►►►"
  (interactive "r\ni\ni\np")
  (let (mystr)
    (setq mystr 
          (if (or intrp (use-region-p))
              (buffer-substring-no-properties start end)
            to-rotate))
    (setq mystr
          (with-temp-buffer
            (insert mystr)
	    (goto-char (point-min))
            (whitespace-cleanup)  ;; :WAS (mon-kill-whitespace)
	    (goto-char (point-min)) ;; Needed?
	    ;; Needs nested capture-groups for ", Jr." ", Sr." ", II" etc.
	    (while (search-forward-regexp "^\\(.*\\) (\\(.*\\))$" nil t)
	      (replace-match "\\2 \\1" ))
	    (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (cond (intrp
           (save-excursion
             (when intrp
               (delete-region start end)
               (insert mystr))))
          ((and (use-region-p) (not intrp))
           (if insertp
               (save-excursion
                 (delete-region start end)
                 (insert mystr))
             mystr))
          ((and to-rotate insertp (not intrp) (not (use-region-p)))
               (save-excursion
                 (insert mystr)))
          ((and to-rotate (not insertp) (not intrp) (not (use-region-p)))
           mystr))))
;;
;;; :TEST-ME (naf-unrotate-canonical nil nil "Cappiello (Leonetto Doctorow)" )
;;; :TEST-ME (naf-unrotate-canonical nil nil "Cappiello (Leonetto Doctorow)" t)
;;; :TEST-ME (apply 'naf-unrotate-canonical nil '(nil "Cappiello (Leonetto Doctorow)"))

;;; ==============================
;;; :TODO :RENAME-ME mon-line-string-rotate-namestrings-fast
(defun mon-make-lastname-firstname (start end &optional intrp)
  "Return region as a list of names.
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
:SEE-ALSO `mon-line-strings-to-list', `mon-line-string-rotate-namestrings'
`mon-line-string-unrotate-namestrings', `naf-make-name-for-lisp',
`mon-make-names-list'.\n►►►"
  (interactive "r\np")
  (let ((get-mmlf (buffer-substring-no-properties start end)))
    (setq get-mmlf
          (with-temp-buffer 
            (insert get-mmlf)
            (goto-char (buffer-end 0))
            (while (search-forward-regexp "\\(\\w+\\) \\(\\w+\\)" nil t)
              (replace-match "\\2 (\\1)"))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (if intrp
        (save-excursion 
          (goto-char start)
          (delete-region start end)
          (insert get-mmlf))
        get-mmlf)))

;;; ==============================
;;; :TODO :RENAME-ME -> `mon-make-name-for-lisp'
(defun naf-make-name-for-lisp (the-region)
  "Return a list as a stringified name rotated as:
 (\"Lastname\" (\"&restnames\")) to parens with quote ' escaped by two slashes.\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-string-rotate-namestrings'
`mon-line-string-unrotate-namestrings', `mon-make-lastname-firstname',
`naf-make-name-for-lisp', `mon-make-names-list'.\n►►►"
  (setq the-region  (if (string-match "'" the-region)
                        (replace-match "\\'" nil t the-region)))
  (let* ((region the-region)
         (temp-name (split-string region))
         (to-put (reverse (cons (butlast temp-name) (last temp-name)))))
    to-put))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: Wednesday July 22, 2009 @ 01:07.15 PM - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2009-09-24T13:08:18-04:00Z}#{09394} - by MON KEY>
(defun mon-make-names-list (start end &optional insrtp intrp)
  "Return list of names escaped for double quotes and parens.\n
:EXAMPLE\n\(mon-make-names-list
 \(1+ \(search-forward-regexp \"►\"\)\) \(- \(search-forward-regexp \"►\"\) 2\)\)
►\nSome \(Dude1 Name1\)\nSome \(Dude2 Name2\)\nSome \(Dude3 Name3\)\n►\n
:SEE-ALSO `mon-line-strings-to-list', `mon-line-string-rotate-namestrings'
`mon-line-string-unrotate-namestrings', `mon-make-lastname-firstname',
`naf-make-name-for-lisp'.\n►►►"
  (interactive "r\nP\np")
  (let ((rspr3
         (mon-replace-string-pairs-region-no-insert start end
				       '(("^" "\(\"")
					 ("\)\\([:space:]?\\)$" "\)\"\)")))))
    (cond (intrp (save-excursion (prin1 rspr3 (current-buffer))))
          (insrtp (prin1 rspr3 (current-buffer)))
          (t rspr3))))

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
  "Translate CSV-STRING (comma separated values string) into regexp.\n
:EXAMPLE\n\(mon-string-csv-regexp \"A,B,C\"\)
=> \\\\(A.*B.*C\\\\|A.*C.*B\\\\|B.*A.*C\\\\|B.*C.*A\\\\|C.*A.*B\\\\|C.*B.*A\\\\)
\(mon-string-csv-regexp \"Somedudes, name\"\)\)
=> Somedudes.*name\\\\|name.*Somedudes\n►►►"
  ;;(let* ((l (mon-perms (split-string str ",\\s-*"))))
  (let* ((l (mon-permutations 
             (save-match-data (split-string csv-string ",\\s-*")))))
    (mapconcat #'(lambda (n) (mapconcat 'identity n ".*"))) l "\\|")))
;;
;;; :TEST-ME (MON-STRING-CSV-REGEXP "A,B,C")
;;; :TEST-ME (mon-string-csv-regexp "Somedudes, name")

;;; ==============================
;;; :NOTE Not quite right yet.
(defun mon-csv-to-perms (str)
  "Translate comma separated name into a rotated name seperated by \" | \".
:EXAMPLE\nSomedudes, name\n transfromed to:\nSomedudes, name | name, Somedudes
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions', `mon-permutations', `mon-string-permute',
`mon-perms'.\n►►►"
  ;;(let* ((l (mon-perms (split-string str ",\\s-*"))))
  (let* ((l (mon-permutations 
             (save-match-data (split-string str ",\\s-*")))))
    (mapconcat #'(lambda (n) (mapconcat 'identity n ", ")) l " | ")))
                   
;;; =======================
;;; :COURTESY Christoph Conrad <cc@cli.de>
;;; :REQUIRES CL `remove*'
(defun mon-perms (perm-list)
  "Return a perumuted list each elt of LIST. Result is a list of permuted lists.
DEPRECATE: This provides identical fuctionality of `mon-permutations' but 
wit CL requirements. Use `mon-permutations' instead.\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions'.\n►►►"
  (if (null perm-list)
      (list '())
    (mapcan #'(lambda (a)
                ;;CL (mapcan #'(lambda (p)
                (mapcan #'(lambda (p)
			    (list (cons a p)))
			(mon-perms (remove* a perm-list :count 1))))
	    perm-list)))
;;
;;; :TEST-ME (mon-perms '("Some_thing" "Name_thing" "More_thing" "Of_the_same_thing"))
;;; :TEST-ME (mon-perms '(Some_thing Name_thing More_thing Of_the_same_thing))

;;; ==============================
;;; :NOTE I mis-interpreted the `permut' in Pascal's `permutation's which is
;;;  duplicate agnositic in the true sense of the combinatoric use of a perm.
;;; What was wanted was a `combin-utation'. Pascal suggested as a solution the
;;; following helper fncn which can be leverage by `mon-permutations'
;;; :COURTESY Pascal J. Bourguignon :WAS `variations' :SOURCE email-correspondence
;;; :CREATED <Timestamp: #{2010-02-09T18:42:33-05:00Z}#{10062} - by MON KEY>
(defun mon-variations (item list)
  "Return the variant forms of ITEM in LIST.\n
ITEM's postion is cycled for each possible position in LIST.\n
:EXAMPLE\n\(mon-variations 'a '\(b c d\)\)\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions', `mon-permutations', `mon-perms'.\n►►►"
 (if (null list)
     (list (list item))
     (cons (cons item list)
           (mapcar #'(lambda (rest) (cons (car list) rest))
                   (mon-variations item (cdr list))))))
;;
;;; :TEST-ME (mon-variations 'a '(b c d))

;;; ==============================
;;; :SEE Notes for `mon-variations' above.
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `permutations'
;;; :WAS (defun permutations (list)
;;;   (mapcan #'(lambda (item)
;;;             (if (= 1 (length list))
;;;                 (list (list item))
;;;                 (mapcar #'(lambda (rest) (cons item rest))
;;;                         (permutations (remove item list)))))
;;;           list))
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :WAS `permutations' :SOURCE email-correspondence
;;; :CREATED <Timestamp: #{2010-02-09T18:43:44-05:00Z}#{10062} - by MON KEY>
(defun mon-permutations (elements)
  "Return a perumuted list each elt of ELEMENTS.\n
Result is a list of permuted lists.
:EXAMPLE\n\(mon-permutations '\(a b c d\)\)\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions', `mon-permutations', `mon-perms'.\n►►►"
 (cond ((null elements) (list elements))
       ((null (cdr elements)) (list elements))
       (t (mapcan #'(lambda (subperm)
                      (mon-variations (car elements) subperm))
                  (mon-permutations (cdr elements))))))
;;
;;; :TEST-ME (mon-permutations '(a b c d))

;;; ==============================
;;; :NOTE BROKEN!!! `mon-permutations' is truncating the return value...
;;; :CREATED <Timestamp: #{2010-02-07T19:03:12-05:00Z}#{10057} - by MON KEY>
(defun mon-string-permute (permute-string &optional intrp)
  "Return PERMUTE-STRING permutatations.
When called-interactively or optional arg INTRP is non-nil prompt:
 \"String to permut :\" and insert PERMUTE-STRING in current-buffer.
:EXAMPLE\n\(mon-string-permute \"bubba\")\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions', `mon-permutations', `mon-perms'.\n►►►"
  (interactive "i\np")
  (let ((prm-str (if intrp (read-string "String to permute :")
                     permute-string))
        (rtn-prm))
    (setq rtn-prm (delete-dups (mon-permutations prm-str)))
    (setq rtn-prm (mapcar #'(lambda (p) (apply 'string p)) rtn-prm))
    (when intrp 
      (save-excursion
        (newline)
        (insert (mapconcat #'identity rtn-prm "\n"))))
    rtn-prm))
;;
(defalias 'mon-permute-string 'mon-string-permute)
;;
;;; :TEST-ME (mon-string-permute "bubba")

;;; ==============================
;;; :COURTESY Erann Gat <gat@robotics.jpl.nasa.gov>
;;; :SOURCE Followup-To: comp.lang.lisp - :DATE Wed, 11 Jan 1995 11:33:58
;;; :CREATED <Timestamp: Wednesday July 22, 2009 @ 10:23.12 AM - by MON KEY>
(defun mon-permute-combine (l1 l2)
  "Efficient version that works for L1 of arbitrary length.
Uses dolist -> dolist-> memeber &key -> push -> nreverse.\n
:EXAMPLE\n\(mon-permute-combine '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions', `mon-permutations', `mon-perms'.\n►►►"
  (if (null l1)
      '(())
      (let ((l3 (mon-permute-combine (cdr l1) l2))
            (result nil))
        (dolist (a l2)
          (dolist (b l3)
            ;; :CL (unless (member a b :key #'second)
            ;; :ELISP use member*: 
            (unless (member* a b :key 'second)
              (push (cons (list (car l1) a) b) result))))
        (nreverse result))))
;;
;;; :TEST-ME See below for `mon-test-permute-combine-functions'
;;
(defun mon-permute-combine-1 (l1 l2)
  "Permutations/combinations permute L1 with L2.
ELISP <-> CL portable. Uses double lambda recursion mapcan -> mapcar > recurse.\n
:EXAMPLE\n\(mon-permute-combine-1 '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)\n
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions', `mon-permutations', `mon-perms'.\n►►►"
  (if (null l1)
      '(())
    (mapcan #'(lambda (a)
                (mapcar #'(lambda (b) (cons (list (car l1) a) b))
                       (mon-permute-combine-1 (cdr l1) (remove a l2))))
	      l2)))
;;
;;; :TEST-ME See below for `mon-test-permute-combine-functions'
;;
(defun mon-permute-combine-2 (l1 l2)
  "Permutations/combinations permute L1 withe L2.
:EXAMPLE:\n\(mon-permute-combine-2 '\(a b \"StringC\" 1\) '\(1 \"string2\" 3 A\)\)
:SEE-ALSO `mon-permute-combine', `mon-permute-combine-2', `mon-variations',
`mon-test-permute-combine-functions', `mon-permutations', `mon-perms'.\n►►►"
 (let ((result nil))
   (dolist (a l2)
     (dolist (b l2)
       (unless (eq a b)
	 ;; :NOTE BUGGY!
         ;;(push (list (list (car l1) a) (list (car l2) b)) result))))
	 ;;result))
	 (push (list (list (first l1) a) (list (second l1) b)) result))))
   (nreverse result)))
;;
;;; :TEST-ME See below for `mon-test-permute-combine-functions'

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-02-09T20:23:43-05:00Z}#{10063} - by MON KEY>
(defun mon-test-permute-combine-functions ()
  "Assure functional equivalence of permute/combine functionss.\n
Return test-case results in buffer named ``*TEST-PERMUTE-COMBINE*''.\n
Test equivalence of functions:
 `mon-permute-combine', `mon-permute-combine-1', `mon-permute-combine-2'\n
:EXAMPLE\n\n\(mon-test-permute-combine-functions\)\n
:SEE-ALSO `mon-variations', `mon-permutations', `mon-perms'.\n►►►"
  (let* ((sab '("StringA" "StringB"))
         (s1-4 '("String1" "String2" "String3" "String4"))
         (s-symab '(a b))
         (s-num1-4 '(1 2 3 4))
         (spc-n   (mon-permute-combine s-symab s-num1-4))
         (spc-2-n (mon-permute-combine s-symab s-num1-4))
         (spc-3-n (mon-permute-combine s-symab s-num1-4))
         (spc-s   (mon-permute-combine sab s1-4))
         (spc-2-s (mon-permute-combine-1 sab s1-4))
         (spc-3-s (mon-permute-combine-2 sab s1-4))
         (spc-str-n   (mon-permute-combine sab s-num1-4))
         (spc-str-n-2 (mon-permute-combine-1 sab s-num1-4))
         (spc-str-n-3 (mon-permute-combine-2 sab s-num1-4))
         (spc-str-symb   (mon-permute-combine sab s-symab))
         (spc-str-symb-2 (mon-permute-combine-1 sab s-symab))
         (spc-str-symb-3 (mon-permute-combine-2 sab s-symab))
         (spcn= (and (equal spc-n spc-2-n) (equal spc-2-n spc-3-n)))
         (spcs= (and (equal spc-s spc-2-s) (equal spc-2-s spc-3-s)))
         (spc-str-n= (and (equal spc-str-n spc-str-n-2) (equal spc-str-n-2 spc-str-n-3)))
         (spc-str-symb-= (and (equal spc-str-symb spc-str-symb-2) (equal spc-str-symb-2 spc-str-symb-3)))
         (frmt-list
          '(((spcn=          . "numbers - %S ;")
             (spcs=          . "Strings - %S ;")
             (spc-str-n=     . "Strings w/ numbers - %S ;")
             (spc-str-symb-= . "Strings w/ Symbols - %S ;"))
            (mon-permute-combine    (spc-s spc-n spc-str-n spc-str-symb))
            (mon-permute-combine-1  (spc-2-s spc-2-n spc-str-n-2 spc-str-symb-2))
            (mon-permute-combine-2  (spc-3-s spc-3-n spc-str-n-3 spc-str-symb-3))))
         (dlm ";;; ==============================")
         (tpc (get-buffer-create "*TEST-PERMUTE-COMBINE*"))
         rslt)
    (dolist (hd (pop frmt-list)
             (setq rslt (concat dlm "\n;;; "
                                (mapconcat #'(lambda (x) 
                                               (format "`%s', " x))(mapcar 'car frmt-list) "")
                                "\n" dlm "\n" (mapconcat #'identity (nreverse rslt) "\n"))))
      (push (format (concat ";Functions return `equal' structure with "(cdr hd)) 
                    (symbol-value (car hd))) rslt))
    (dolist (prm frmt-list)
      (let ((prmdo prm)
            this-rslt)
        (dolist (i (cadr prmdo)
                 (setq rslt (concat rslt (concat (format "\n%s\n;;; `%s'\n%s\n" dlm (car prmdo) dlm)
                                                 (mapconcat #'identity (nreverse this-rslt) "\n")))))
          (push (format ";`%s'\n  %S\n" (car prmdo) (symbol-value i)) ;i)
                this-rslt))))
    (with-current-buffer tpc
      (princ rslt tpc)
      (emacs-lisp-mode)
      (goto-char (buffer-end 0)))
    (display-buffer tpc t)))
;; 
;;; :TEST-ME (mon-test-permute-combine-functions)

;;; ==============================
;;; :COURTESY: Pascal J. Bourguignon :HIS pjb-emacs.el :WAS `perm-words'
;;; :WAS `mon-perm-words' ->`mon-string-permute-line'
(defun mon-string-permute-line (&optional delimit-with)
  "Insert after current line all the permutations of the words on the current line.\n
:SEE-ALSO `mon-permutations', `mon-string-ify-current-line'.\n►►►"
  (interactive)
  (let ((words (car (read-from-string
                     (format "(%s)" 
			     (buffer-substring-no-properties
			      (progn (beginning-of-line) (point))
			      (progn (end-of-line) (point)))))))
	(delim (if delimit-with delimit-with "")))
    (end-of-line)
    (insert "\n")
    (dolist (line (mon-permutations words))
      (dolist (word line)
        (insert (format "%s %s"
			(if (and (listp word) (eq 'quote (car word))) 
			    (cadr word) word)
			delim))) ;)))
      (insert "\n"))))
;;
;;; :TEST-ME word word2 word3 word4 <-M-x mon-perm-words

;;; ==============================
;;; :RENAMED `mon-strings-splice-sep' -> `mon-string-splice-sep'
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 07:28.25 AM - by MON KEY>
(defun mon-string-splice-sep (strings &optional seperator insert-str insertp intrp)
  "Return concatenation of STRINGS spliced together with separator SEP.\n
When SEPERATOR (a string) is non-nil it's value inserted between STRINGS.
Default is \" | \" which can be useful to build up name lists in `naf-mode'.
Called interactively inserts converted string at point.
When INSERT-STR is non-nil insert result in buffer as string else print as with prin1.
When INSERTP is non-nil and INSERT-STR nil print as with prin1.
:SEE-ALSO .\n►►►"
(interactive (list
              (read-string "String to Splice :")
              (read-string "Separate with :")
              (yes-or-no-p "Insert as string? :")
              nil
              t))
  (let ((str)
	(sep (if seperator seperator " | ")))
    (while strings
      (setq str (concat str (car strings)))
      (if (cdr strings)
	  (setq str (concat str sep)))
      (setq strings (cdr strings)))
    (cond (intrp 
           (if insert-str
               (princ str (current-buffer))
             (prin1 str (current-buffer))))
          ((and (not intrp) (or insert-str insertp))
           (if insert-str
               (princ str (current-buffer))
             (prin1 str (current-buffer))))
          ((and (not intrp) (not insert-str) (not insertp))
           str))))
;;
;;; TEST-ME (MON-STRING-SPLICE-SEP '("AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA") " " )
;;; TEST-ME (MON-STRING-SPLICE-SEP '("AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA") "|" NIL T)
;;; TEST-ME (MON-STRING-SPLICE-SEP '("AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA") "|" T T)
;;; TEST-ME (MON-STRING-SPLICE-SEP '("AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA") "| ")
;;; TEST-ME (MON-STRING-SPLICE-SEP '("AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA") " |")
;;; TEST-ME (MON-STRING-SPLICE-SEP '("AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA" "AAOA") "_")
;;; TEST-ME (mon-string-splice-sep '("aaoa" "aaoa" "aaoa" "aaoa" "aaoa" "aaoa" "aaoa") " - ")

;;; ==============================
;;; :RENAMED `mon-string2strings-splice-sep' -> `mon-string->strings-splice-sep'
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 02:51.45 PM - by MON KEY>
(defun mon-string->strings-splice-sep (string2strings &optional seperator insertp w-princ)
  "Like `mon-strings-splice-sep' but converts string -> strings before seperation.
STRING2STRINGS \(a string\) is the string to split.
SEPERATOR \(a string\) delimits the return value - default is \" | \"
Called interactively or when INSERTP non-nil inserts converted string at point.
Called with prefix arg or when W-PRINC non-nil insert as with princ.\n
:SEE-ALSO `mon-string-ify-list'.\n►►►"
(interactive (list (read-string "String to Splice :")
                   (read-string "Separate with :")
                   t
                   (yes-or-no-p "(Y) inserts unquated (N) to insert string ? :")))
  (let* ((strs (mon-string-ify-list string2strings))
         (str-splc (mon-string-splice-sep strs seperator)))
   (when insertp
     (if w-princ
         (princ str-splc (current-buffer))
       (prin1 str-splc (current-buffer))))
     str-splc))

;;; :TEST-ME (mon-string->strings-splice-sep  "Make this sentence a list of strings" "++")
;;; :TEST-ME (mon-string->strings-splice-sep  "Make this sentence a list of strings")
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" nil t)
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" nil t t)
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" "*_*" t)
;;; :TEST-ME (mon-string->strings-splice-sep  "This is my String" "*_*" t t)
;;; :TEST-ME (call-interactively 'mon-string->strings-splice-sep)

;;; =======================
(defun mon-convert-list-regexp (string-to-cnv &optional insertp intrp)
"Return a regex-opt'd list of strings.\n
String obtained by splitting read-string from mini-buffer.\n
:SEE-ALSO .\n►►►"
  (interactive "sstring to convert :\nP\np")
  (let*  ((stringify (mon-string-ify-list string-to-cnv))
	  (converted (mon-string-rotate-to-regexp stringify)))
    (if (or insertp intrp)
	(prin1 converted (current-buffer))
      converted)))

;;; :TEST-ME (mon-convert-list-regexp 
;;;  "Returns a regex opt list of strings obtained by breaking the string the user entered at the")
;;; 
;;; :TEST-ME (mon-convert-list-regexp "Returns a regexp-opt list of strings splitting" t)
;;; :TEST-ME (mon-convert-list-regexp "Returns a regexp-opt list of strings splitting" t)
;;; :TEST-ME (call-interactively 'mon-convert-list-regexp)

;;; ==============================
;;; :COURTESY Marc Tfardy  
;;; :SOURCE Newsgroups: comp.emacs :SUBJECT Re: re-search-forward and assoc list
;;; "Definition of string-infix: 
;;; (I know, there is a ELISP function that do the same thing, but I can't remember
;;;  its name so I wrote it myself and put to my private lisp lib.)
;;; :MODIFICATONS <Timestamp: Sunday May 31, 2009 @ 08:22.06 AM - by MON KEY>
;;; :CREATED <Timestamp: Friday March 27, 2009 @ 04:50.09 PM - by MON KEY>
;;; ==============================
(defun mon-string-infix (string-list infix)
   "Creates a string by all from the STRING-LIST, which are separated by INFIX.\n
EXAMPLE:\n{A function that changes certain strings according to a-list key-value pairs.} 
\(defun foo \(\)
   \(interactive\)
   \(setq replace-alist '\(\(\"x\" . \"bar\"\) \(\"y\" . \"foo\"\)\)\)
   \(while \(re-search-forward 
             \(concat \"\\\\(\" (string-infix (mapcar 'car replace-alist) \"\\\\|\") \"\\\\)\") nil t)
     \(replace-match \(cdr \(assoc-string \(match-string 1\) replace-alist\)\)\)\)\)\n
:SEE-ALSO .\n►►►"
   (cond ((null string-list)
          "")
         ((null (cdr string-list))
          (car string-list))
         ((cdr string-list)
          (concat (car string-list) infix (mon-string-infix (cdr string-list) infix)))))

;;; ==============================

;;; ==============================
;;; :WORKG-AS-OF
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
;;;   (goto-char (point-min))
;;;   (while (and (line-move-1 1))
;;;     (let ((bol (beginning-of-line))
;;;	   (eol (end-of-line)))
;;;       ;; is replace-string-region supposed to call here? was it defined?
;;; ;;       (replace-string-region bol eol)))))  

;;; ==============================
;;; :WAS Works but don't use.
;;; (defun naf-make-name-for-lisp (the-region)
;;;   (let* ((region the-region)
;;; 	 (temp-name (split-string region))
;;; 	 (to-put (reverse (cons (butlast temp-name) (last temp-name)))))
;;; 	;  to-put)
;;;     (format "%s %s"
;;; 	    (car to-put)
;;; 	    (cadr to-put))))

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
  "Should be set to a list of string rotations to rotate upon.\n
:EXAMPLE\n\(setq  *rotate-text-rotations*
       '\(\(\"width\" \"height\"\) 
         \(\"left\" \"top\" \"right\" \"bottom\"\)
         \(\"red\" \"orange\" \"yellow\" \"green\" \"indigo\" \"violet\" \"blue\"\)
         \(\"xx-small\" \"x-small\" \"small\" \"normal\" \"large\" \"x-large\" \"xx-large\"\)
         \(\"zero\" \"one\" \"two\"\)\)\)\n
:CALLED-BY `mon-rotate-region'
:CALLED-BY `mon-rotate-string'
:CALLED-BY `mon-rotate-next'
:CALLED-BY `mon-rotate-get-rotations-for'.\n►►►")
;;)

;;; :TEST-ME
;;; (setq  *rotate-text-rotations*
;;; '(("width" "height") 
;;;   ("left" "top" "right" "bottom")
;;;   ("red" "orange" "yellow" "green" "indigo" "violet" "blue")
;;;   ("xx-small" "x-small" "small" "normal" "large" "x-large" "xx-large")
;;;   ("zero" "one" "two")))

;;; ==============================
;;; :RENAMED `rotate-convert-rotations-to-regexp' -> `mon-string-rotate-to-regexp'
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-region (beg end &optional rotations) ;testing optional-arg
  "Rotate all matches in `*rotate-text-rotations*' between point and mark.\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-rotate-flatten-list', `mon-indent-or-rotate' .\n►►►"
  (interactive "r")
  (let ((regexp (mon-string-rotate-to-regexp ;rotate-convert-rotations-to-regexp
		 (or rotations *rotate-text-rotations*)))
	(end-mark (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
	(let* ((found (match-string 0))
	       (replace (mon-rotate-next found)))
	  (replace-match replace))))))


;;; ==============================
;;; :RENAMED `rotate-string' -> `mon-rotate-string' -> naf-name-utils.el
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-string (string &optional rotations)
  "Rotate all matches in STRING using associations in ROTATIONS.
If ROTATIONS are not given it defaults to `*rotate-text-rotations*'.
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-rotate-flatten-list', `mon-indent-or-rotate'.\n►►►"
  (let ((regexp (mon-string-rotate-to-regexp ;rotate-convert-rotations-to-regexp
		 (or rotations *rotate-text-rotations*)))
	(start 0))
    (while (string-match regexp string start)
      (let* ((found (match-string 0 string))
	     (replace (mon-rotate-next
		       found
		       (or rotations *rotate-text-rotations*))))
	(setq start (+ (match-end 0)
		       (- (length replace) (length found))))
	(setq string (replace-match replace nil t string))))
    string))

;;; ==============================
;;; :RENAMED `rotate-next' -> `mon-rotate-next'
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-next (string &optional rotations)
  "Return the next element after STRING in ROTATIONS.\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-rotate-flatten-list', `mon-indent-or-rotate'.\n►►►"
  (let ((rots (mon-rotate-get-rotations-for
	       string
	       (or rotations *rotate-text-rotations*))))
    (if (> (length rots) 1)
	(error (format "Ambiguous rotation for %s" string))
      (if (< (length rots) 1)
	  ;; If we get this far, this should not occur:
	  (error (format "Unknown rotation for %s" string))
	(let ((occurs-in-rots (member string (car rots))))
	  (if (null occurs-in-rots)
	      ;; If we get this far, this should *never* occur:
	      (error (format "Unknown rotation for %s" string))
	  (if (null (cdr occurs-in-rots))
	      (caar rots)
	    (cadr occurs-in-rots))))))))


;;; ==============================
;;; :RENAMED `rotate-get-rotations-for' -> `mon-rotate-get-rotations-for' -> naf-name-utils.el
;;; :CREATED <Timestamp: Tuesday June 02, 2009 @ 05:52.27 PM - by MON KEY>
(defun mon-rotate-get-rotations-for (string &optional rotations)
  "Return the string rotations for STRING in ROTATIONS.
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-rotate-flatten-list', `mon-indent-or-rotate'.\n►►►"
  (remq nil (mapcar (lambda (rot) (if (member string rot) rot))
		    (or rotations *rotate-text-rotations*))))

;;; ==============================
;;; :RENAMED `rotate-convert-rotations-to-regexp' -> `mon-string-rotate-to-regexp'
;;; :CREATED <Timestamp: Sunday May 31, 2009 @ 04:13.28 PM - by MON KEY>
(defun mon-string-rotate-to-regexp (rotations)
  "Flatten a rotated list and optimized for regexp with `regexp-opt'.\n
EXAMPLE1:
 \(mon-string-rotate-to-regexp 
 '\(\"This\" \"is\" \"a\" \"list\" \"to\" \"rotate\"\)\)\n=
 \(mon-string-rotate-to-regexp 
 '\(\(\"This\" \"is\" \"a\" \"list\" \"to\" \"rotate\"\) 
   \(\"Thi\" \"sis\" \"ali\" \"stt\" \"oro\" \"tat\" \"e\"\) 
   \(\"rot\" \"ate\" \"tol\" \"ist\" \"ais\" \"Thi\" \"s\"\)\)\)\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-rotate-flatten-list', `mon-indent-or-rotate'.\n►►►"
  (regexp-opt (mon-rotate-flatten-list rotations)))

;;; :TEST-ME
;;; (mon-string-rotate-to-regexp
;;;   '(("This" "is" "a" "list" "to" "rotate")
;;;     ("Thi" "sis" "ali" "stt""oro" "tat" "e")
;;;     ("rot" "ate" "tol" "ist" "ais" "Thi" "s")))


;;; ==============================
;;; :RENAMED: `rotate-flatten-list' -> `mon-rotate-flatten-list' -> naf-name-utils.el
(defun mon-rotate-flatten-list (list-of-lists)
  "Flattens LIST-OF-LISTS - a list of lists.\n
:EXAMPLE\n\(mon-rotate-flatten-list '\(\(a b c\) \(1 \(\(2 3\)\)\)\)\)\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-rotate-flatten-list', `mon-indent-or-rotate'.\n►►►"
  (if (null list-of-lists)
      list-of-lists
    (if (listp list-of-lists)
	(append (mon-rotate-flatten-list (car list-of-lists))
		(mon-rotate-flatten-list (cdr list-of-lists)))
      (list list-of-lists))))
;;
;;; :TEST-ME (mon-rotate-flatten-list '((a b c) (1 ((2 3)))))

;;; ==============================
;;; :NOTE (local-set-key [tab] 'indent-or-rotate)
;;; :RENAMED `indent-or-rotate' -> `mon-rotate-or-indent' -> naf-name-utils.el
(defun mon-indent-or-rotate ()
  "If point is at end of a word, then else indent the line.\n
:SEE-ALSO `mon-rotate-string', `mon-rotate-next', `mon-rotate-region',
`mon-rotate-get-rotations-for', `mon-string-rotate-to-regexp',
`mon-rotate-flatten-list', `mon-indent-or-rotate'.\n►►►"
  (interactive)
  (if (looking-at "\\>")
      (mon-rotate-region (save-excursion (forward-word -1) (point))
                         (point))
      (indent-for-tab-command)))


;;; =======================
;;; :NOTE The dog looked to me and said, "Why are you here?"
(defun naf-make-name-return (the-region)
   (let* ((region the-region)
          (temp-name (split-string region))
          (to-put (reverse (cons (butlast temp-name) (last temp-name)))))
     (insert (format "\n%s %s"
                     (car to-put)
                     (cadr to-put)))))

;;; ==============================
(provide 'naf-name-utils)
;;; ==============================

;;; ==============================
;;; naf-name-utils.el ends here
;;; EOF

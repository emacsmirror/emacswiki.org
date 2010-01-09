;;; this is mon-doc-help-utils-supplemental.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-doc-help-utils-supplemental is required and should be present in Emacs
;;; load-path when using mon-doc-help-utils. It provides the specific
;;; subfeatures required to bootstrap mon-doc-help-utils.  In order to load and
;;; byte-compile mon-doc-help-utils a few subfeatures need to be present. If you
;;; do not wish to load the full feauture set of the following packages
;;; mon-doc-help-utils-supplemental.el is careful to load only the neccesary
;;; functions and variables listed below:
;;;
;;; :FILE mon-insertion-utils.el 
;;;       | -> `mon-insert-lisp-testme'
;;;       | -> `comment-divider'
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-insertion-utils.el')
;;;
;;; :FILE mon-regexp-symbols.el 
;;;       | -> `*regexp-symbol-defs*'          
;;;       | -> `mon-test->*regexp-symbol-defs*'
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-regexp-symbols.el')
;;;
;;; :FILE mon-utils.el
;;;       | -> `mon-string-index'      
;;;       | -> `mon-string-upto-index' 
;;;       | -> `mon-string-after-index'
;;;       | -> `mon-string-justify-left'
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;;
;;; :NOTE While mon-doc-help-utils-supplemental.el will provide the necessary
;;; features in order to get mon-doc-help-utils bootstrapped wherever possible
;;; MON encourages you to also use the above required packages in addition to the
;;; supplemental. As such, where those packages are present, the supplemental
;;; will not shadow any additional functionality extensions which they provide.
;;;
;;; That said, while each of the above packages provides useful and nice
;;; facilities which extend the standard Emacs distribution you may not wish to
;;; load all of them into your Emacs esp. as MON does not generally use autoload
;;; cookies. MON routinely runs the entire suite of mon-*.el and naf-mode-*.el
;;; packages as interpreted code with little performance impact and generally
;;; unless your embedding Emacs loading lots of third party packages rarely
;;; poses much concern on modern systems IOW use the following with impunity:
;;; (require 'mon-superbig-package) 
;;; 
;;; TODO:
;;; Hopefully sometime in the near future the Emacs-devels will begin using the
;;; bzr and Launchpad features of distributed version control to build a better
;;; Emacs Lisp 'packaging' tool that can aid in some of this minor dependency
;;; issues and this type of stuff won't be quite as big a problem (and instead
;;; we'll all move to grokking DAGs with recursive dependency cycles). In the
;;; interim MON is still using Mercurial. Contact MON for access to a stripped
;;; hg archive of all MON's current Elisp source.
;;
;;; NOTES: 
;;; The contents of this file used to be inlined within mon-doc-help-utils.el
;;; However, as this mon-doc-help-utils has grown conisderably in size and
;;; scope, it no longer makes sense to maintain what is essentially duplicate
;;; code inside that file and it was extracted to this file on:
;;; <Timestamp: #{2009-12-21T12:25:58-05:00Z}#{09521} - by MON>
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: 
;;; (URL `http://www.emacswiki.org/emacs/mon-doc-help-utils-supplemental.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-12-21T21:20:06-05:00Z}#{09522} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-12-21T13:15:00-05:00Z}#{09521} - by MON>
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
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :LOAD-SPECIFIC-PROCEDURES :IF-NOT-FEATURE-P `mon-insertion-utils.el'
;;; `comment-divider', `mon-insert-lisp-testme'
(unless (featurep 'mon-insertion-utils)
  (unless (fboundp 'comment-divider)
;;; :MODIFICATIONS <Timestamp: #{2009-08-25T14:09:37-04:00Z}#{09352} - by MON KEY>
(defun comment-divider (&optional not-insert intrp)
  "Insert default comment divider at point.
When called-interactively inserts at point:\n
;;; ==============================\n
When optional arg not-insert is non-nil return comment divider as string.\n
:EXAMPLE\n\(comment-divider t\)\n
:SEE-ALSO `mon-comment-divide->col', `mon-lisp-comment-to-col'
`mon-insert-php-comment-divider', `mon-insert-lisp-stamp'. ►►►"
  (interactive "i\np")
  (if (or (not not-insert) intrp)
      (insert ";;; ==============================")
    ";;; =============================="))
) ;; :CLOSE fboundp comment-divider
;;
;;; :TEST-ME (comment-divider t)
;;; :TEST-ME (comment-divider)
;;; :TEST-ME (call-interactively 'comment-divider)
;;
(unless ;(and (featurep 'mon-insertion-utils) 
    (fboundp 'mon-insert-lisp-testme) ;))
;;; :CREATED <Timestamp: 2009-07-31-W31-5T13:53:52-0400Z - by MON KEY>
(defun mon-insert-lisp-testme (&optional search-func test-me-count insertp intrp)
  "Insert at point a newline and commented test-me string.
When non-nil SEARCH-FUNC will search backward for a function name and include it
in the test-me string.\n
When non-nil TEST-ME-COUNT insert test-me string N times. Default is 1\(one\).\n
When prefix arg TEST-ME-COUNT is non-nil inerts N number of ';;; :TEST-ME '
strings prompting y-or-n-p if we want to include the function name in insertion.\n
When INSERTP is non-nil insert the test-me string(s) in current buffer at point.\n
Use at the end of newly created elisp functions to provide example test cases.\n
Regexp held by global var `*regexp-symbol-defs*'.\n
:SEE-ALSO `mon-insert-doc-help-tail', `mon-test->*regexp-symbol-defs*'
`mon-insert-doc-help-tail', `mon-insert-lisp-stamp', `mon-insert-copyright',
`mon-insert-lisp-CL-file-template', `comment-divider',
`comment-divider-to-col-four', `mon-insert-lisp-evald'.\n►►►"
  (interactive "i\np\ni\np")
  (let* ((get-func)
         (tmc (cond ((and intrp (> test-me-count 1))
                      (if ((lambda () (yes-or-no-p "Search-function-name? :")))
                          (progn (setq get-func t) test-me-count)
                        (progn (setq get-func nil) test-me-count)))
                    ((not test-me-count) 1)
                    (t  test-me-count)))
         (func (if (or search-func get-func)
                   (save-excursion
                     (search-backward-regexp  *regexp-symbol-defs*)
                     (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (test-me-string (if (or search-func get-func)
                             (format ";;; :TEST-ME (%s )" func)
                           ";;; :TEST-ME "))
         (limit (make-marker))
         (cnt tmc)
         (return-tms))
    (while (>= cnt 1)
      (setq return-tms (concat test-me-string "\n" return-tms))
      (setq cnt (1- cnt)))
    (if (or intrp insertp)
          (progn
            (save-excursion
              (when insertp (newline))
              (when (not (bolp))(beginning-of-line))
              (princ return-tms (current-buffer))
              (set-marker limit (point)))
          (search-forward-regexp
           (format "%s$" test-me-string) (marker-position limit) t)
          ;; :NOTE Is t needed to prevent return buffer posn when called externally?
          t) 
          return-tms)))
) ;; :CLOSE fboundp mon-insert-lisp-testme
) ;; :CLOSE featurep mon-insertion-utils
;;
;;; ,---- :UNCOMMENT-TO-TEST
;;; | (defun some-function (&optional optional)
;;; | (defun some-function-22 (&optional optional)
;;; | (defun *some/-symbol:->name<-2* (somevar
;;; | (defmacro some-macro ()
;;; | (defmacro some-macro*:22 (&rest)
;;; | (defun *some/-symbol:->name<-2* (somevar
;;; | (defvar *some-var* 'var
;;; | (defun *some/-symbol:->name<-2* 'somevar
;;; `----
;;
;;; :TEST-ME (let ((find-def* *regexp-symbol-defs*)) (search-backward-regexp find-def*))
;;; 
;;
;;; :TEST-ME `(,(match-beginning 3) ,(match-end 3))
;;; :TEST-ME (match-sring 1) ;grp 1=>"(defun* some-func:name* ("
;;; :TEST-ME (match-sring 2) ;grp 2=>"(defun* "
;;; :TEST-ME (match-string 3) ;grp 3=>"some-macro*:22"
;;; :TEST-ME (match-sring 4) ;grp 4=>" ("
;;
;;; :TEST-ME (mon-insert-lisp-testme)
;;; :TEST-ME (mon-insert-lisp-testme t 3 )
;;; :TEST-ME (mon-insert-lisp-testme nil 3)
;;; :TEST-ME (mon-insert-lisp-testme nil 3 t)
;;; :TEST-ME (mon-insert-lisp-testme t 3 t)
;;; :TEST-ME (mon-insert-lisp-testme t nil t)
;;; :TEST-ME (mon-insert-lisp-testme nil nil t)
;;; :TEST-ME (mon-insert-lisp-testme nil nil nil)

;;; ==============================
;;; :LOAD-SPECIFIC-PROCEDURES :IF-NOT-FEATURE-P `mon-regexp-symbols.el'
;;; `*regexp-symbol-defs*', `mon-test->*regexp-symbol-defs*'
;; 
(unless (featurep 'mon-regexp-symbols)
(unless (bound-and-true-p *regexp-symbol-defs*)
;;; :CREATED <Timestamp: 2009-08-03-W32-1T11:04:11-0400Z - by MON KEY>
(defvar *regexp-symbol-defs* nil
  "*Regexp for finding lisp definition forms defun, defmacro, defvar.\n
:CALLED-BY `mon-insert-lisp-testme',`mon-insert-doc-help-tail',
`mon-test->*regexp-symbol-defs*'.\n►►►")
;;
(when (not (bound-and-true-p *regexp-symbol-defs*))
  (setq *regexp-symbol-defs*
        (concat
         ;; :FIXME doesn't catch on cases where the lambda list is on the next line.
         ;;...1..
         "^\\((" ;; Opening paren.
         ;; grp 2 -> `defun' `defun*' `defmacro' `defmacro*' `defsubst' `defsubst*'
         ;;          `defvar' `defconst' `defcustom' `deftheme'
         ;;..2................................................
         "\\(def\\(?:c\\(?:onst\\|ustom\\)\\|face\\|macro\\*?\\|subst\\*?\\|theme\\|un\\*?\\|var\\)\\)"
         ;;.3....................         ;; :NOTE leading whitepspace.
         " \\([A-Za-z0-9/><:*-]+\\)"      ;; grp 3 -> *some/-symbol:->name<-2*.
         ;;...4........................
         "\\(\\( (\\)\\|\\( '\\)\\|\\( `\\)\\)\\)" ;; grp 4 -> ` (' or ` ''.
         )))
) ;; :CLOSE BATP *regexp-symbol-defs*
;;
;;; :TEST-ME  *regexp-symbol-defs*
;;; :SEE-BELOW for-additional-tests-with `mon-test->*regexp-symbol-defs*'
;;
;;;(progn (makunbound '*regexp-symbol-defs*) (unintern '*regexp-symbol-defs*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-02T16:11:07-04:00Z}#{09363} - by MON KEY>
(unless (fboundp 'mon-test->*regexp-symbol-defs*)
(defun mon-test->*regexp-symbol-defs* (&optional insertp intrp)
  "Test function to ensure that the regexp in var `*regexp-symbol-defs*' works.\n
:SEE-ALSO `mon-insert-lisp-testme',`mon-insert-doc-help-tail'. ►►►"
  (interactive "i\np")
  (let ((find-def* *regexp-symbol-defs*)
        (the-str))
    (save-excursion  (search-backward-regexp find-def*))
    (setq the-str
          (concat "\nmatch-string1: "(match-string-no-properties 2) " start2: "
                  (number-to-string (match-beginning 2)) " end2: "
                  (number-to-string (match-end 2)) "\nmatch-string3: "
                  (match-string-no-properties 3)" start3: "
                  (number-to-string (match-beginning 3))  " end3: "
                  (number-to-string (match-end 3))"\nmatch-string4: "
                  (match-string-no-properties 4) " start4: "
                  (number-to-string (match-beginning 4))  " end4: "
                  (number-to-string (match-end 4))))
    (if (or insertp intrp)
        (save-excursion (princ  the-str (current-buffer)))
      (momentary-string-display the-str (point)))))
) ;; :CLOSE BATP mon-test->*regexp-symbol-defs*
) ;; :CLOSE featurep mon-regexp-symbols
;;;
;;; :TEST-ME  *regexp-symbol-defs*
;;; (progn (makunbound '*regexp-symbol-defs*)
;;;        (unintern '*regexp-symbol-defs*) )
;;
;;; ,---- :UNCOMMENT-TO-TEST
;;; | (defun some-function (&optional optional)
;;; | (defun some-function-22 (&optional optional)
;;; | (defun *some/-symbol:->name<-2* (somevar
;;; | (defmacro some-macro ()
;;; | (defmacro some-macro*:22 (&rest)
;;; | (defun *some/-symbol:->name<-2* (somevar
;;; | (defvar *some-var* 'var
;;; | (defun *some/-symbol:->name<-2* 'somevar
;;; | (defmacro some-macro*:22 (&rest)
;;; | (defun *some/-symbol:->name<-2* (somevar
;;; | (defvar *some-var* 'var
;;; | (defun *some/-symbol:->name<-2* 'somevar
;;; | (defmacro* some-macro*:22 (&rest)
;;; | (defun* *some/-symbol:->name<-2* (somevar
;;; | (defsubst *some/subtst-symbol:->name<-2* (
;;; | (defsubst* *some/subtst-symbol:->name<-2* (
;;; | (defconst *some/-symbol:->name<-2* (somevar
;;; | (defcustom *some/-custom-symbol:->name<-2* 'somecustom
;;; | (defconst *some/-symbol:->name<-2* (someconst
;;; | (defface *some/-face-symbol:->name<-2* (someface
;;; | (defgroup *some/-group-symbol:->name<-2* (somegroup
;;; | (deftheme *some/-theme-symbol:->name<-2* (sometheme
;;; `----
;;;
;;; :TEST-ME (mon-test->*regexp-symbol-defs*)

;;; ==============================
;;; :LOAD-SPECIFIC-PROCEDURES :IF-NOT-FEATURE-P `mon-utils.el'
;;; `mon-string-index', `mon-string-upto-index', `mon-string-after-index'
(unless (and (featurep 'mon-utils)
             (fboundp 'mon-string-index))
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `string-index'
(defun mon-string-index (string-to-idx needle &optional frompos)
  "Return the position in STRING of the beginning of first occurence of NEEDLE.
Return nil if needle is not found. NEEDLE is a char, number, or string.
When FROMPOS is non-nil begin search for needle from position.
Default is to search from start of string.\n
:EXAMPLE\n\(mon-string-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-upto-index', `mon-string-after-index',
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces'.\n►►►"
  (string-match
   (regexp-quote
    (cond ((or (characterp needle) (numberp needle)) (format "%c" needle))
          ((stringp needle) needle)
          (t (error "string-index expects a needle, number or string as 2nd argument."))))
   string-to-idx frompos))
) ;; :CLOSE 1st foundp  mon-string-index
;;
(unless (and (featurep 'mon-utils)
             (fboundp 'mon-string-upto-index))
;;; :CREATED <Timestamp: #{2009-10-01T15:16:26-04:00Z}#{09404} - by MON KEY>
(defun mon-string-upto-index (in-string upto-string)
  "Return substring of IN-STRING UPTO-STRING.
UPTO-STRING is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-upto-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-after-index'
`mon-string-position', `mon-string-has-suffix', `mon-string-chop-spaces'.\n►►►"
  (substring in-string 0
             (mon-string-index in-string upto-string)))
) ;; :CLOSE 2nd fboundp mon-string-upto-index
;;
(unless (and (featurep 'mon-utils)
             (fboundp 'mon-string-after-index))
;;; :CREATED <Timestamp: #{2009-10-01T15:16:29-04:00Z}#{09404} - by MON KEY>
(defun mon-string-after-index (in-str after-str)
  "Return substring of IN-STR AFTER-STR.
AFTER-STR is a simple string. No regexps, chars, numbers, lists, etc.\n
:EXAMPLE\n\(mon-string-after-index \"string before ### string after\" \"###\"\)\n
:SEE-ALSO `mon-string-index', `mon-string-upto-index', `mon-string-position',
`mon-string-has-suffix', `mon-string-chop-spaces'.\n►►►"
  (substring in-str
             (+ (mon-string-index in-str after-str) (length after-str))))
) ;; :CLOSE 3rd foundp mon-string-after-index
;;
(unless (and (featurep 'mon-utils)
             (fboundp 'mon-string-justify-left))
;;; ==============================
;;; :COURTESY Pascal Bourguignon :HIS pjb-strings.el :WAS `string-justify-left'
(defun mon-string-justify-left (string &optional width left-margin)
  "Return a left-justified string built from string.\n
:NOTE The default width is 72 characters, the default left-margin is 0.  
      The width is counted from column 0.
      The word separators are those of split-string:
      [ \\f\\t\\n\\r\\v]+
      Which means that the string is justified as one paragraph.\n
:SEE-ALSO `mon-string-fill-to-col'.\n►►►"
  (if (null width) (setq width 72))
  (if (null left-margin) (setq left-margin 0))
  (if (not (stringp string)) 
      (error "string-justify-left: The first argument must be a string."))
  (if (not (and (integerp width) (integerp left-margin)))
      (error "string-justify-left: The optional arguments must be integers."))
  (let* ((margin (make-string left-margin 32))
         (splited (split-string string))
         (col left-margin)
         (justified (substring margin 0 col))
         (word)
         (word-length 0)
         (separator ""))
    (while splited
      (setq word (car splited))
      (setq splited (cdr splited))
      (setq word-length (length word))
      (if (> word-length 0)
          (if (>= (+ col (length word)) width)
              (progn
                (setq justified (concat justified "\n" margin word))
                (setq col (+ left-margin word-length)))
            (progn
              (setq justified (concat justified separator word))
              (setq col (+ col 1 word-length)))))
      (setq separator " "))
    (if (< col width)
        (setq justified (concat justified (make-string (- width col) 32))))
    justified))
) ;; :CLOSE 4th foundp `mon-string-justify-left'
;;
;;; :TEST-ME (mon-string-upto-index "string before ### string after" "###")
;;; :TEST-ME (mon-string-index "string before ### string after" "###")
;;; :TEST-ME (mon-string-after-index "string before ### string after" "###")
;;; :TEST-ME 
;;; (mon-string-justify-left 
;;;  (let (jnk)
;;;    (dotimes (i 8 jnk) 
;;;      (dolist (i '(64 94))
;;;        (setq jnk (concat " " (make-string (elt (shuffle-vector [7 5 3 9]) 3) i) jnk))))) 68)

;;; ==============================
(provide 'mon-doc-help-utils-supplemental)
;;; ==============================

;;; ================================================================
;;; mon-doc-help-utils-supplemental.el ends here
;;; EOF

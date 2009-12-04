;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this is mon-empty-registers.el
;;; ================================================================
;;; DESCRIPTION:
;;; Some handy register locations with unfilled contents.
;;; Idea :COURTESY Nelson H. F. Beebe <beebe@math.utah.edu> :HIS bibtex-regs.el
;;; 
;;; FUNCTIONS:►►►
;;; `mon-reset-registers', `mon-set-all-registers-to-char'
;;; `mon-query-replace-register1<-reg2', `mon-coerce->char'
;;; `mon-decode-meta-key-event', `mon-catch-meta-key'
;;; `mon-set-register->tags', `mon-make-set-register->tags-docs'
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
;;; `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
;;; `*mon-digit-registers*', `*mon-digit-shifted-registers*',
;;; `*mon-symbol-registers*', `*mon-upper-case-registers*',
;;; `*mon-lower-case-registers*', `*register-of-registers*'
;;; `*mon-register-config-tags*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;; `mon-prin1-char->?char' -> `prin1-char'
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;; `mon-query-replace-register1<-reg2' <- ./naf-mode-replacements.el
;;; `mon-coerce->char'                  <- ./mon-utils.el
;;; `mon-decode-meta-key-event'         <- ./mon-utils.el
;;; `mon-catch-meta-key'                <- ./mon-utils.el
;;;
;;; REQUIRES:
;;; 'cl -> `mon-reset-registers' uses `defun*', `pairlis', etc.
;;; `mon-doc-help-utils.el' -> `mon-help-put-var-doc-val->func'
;;; `mon-time-utils.el' -> `mon-stamp'
;;; (optional `mon-utils') -> `mon-is-digit', `mon-is-letter', `mon-string->symbol'
;;;
;;; TODO:
;;;
;;; NOTES:
;;; Nothing in this file takes advantage of `prin1-char'. Can it?
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-empty-registers.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-02T17:12:10-04:00Z} - by MON> 
;;; 
;;; FILE CREATED:
;;; <Timestamp: Tuesday August 04, 2009 @ 07:31.09 PM - by MON KEY>
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

;;; ==============================
;;; `mon-reset-registers' uses defun*
(eval-when-compile (require 'cl))
;;; ==============================

;;; ==============================
;;; For code posted to emacs-wiki, needed when mon-utils.el is not loaded. 
(eval-when-compile
  (unless (featurep 'mon-utils)
    (unless (functionp 'mon-is-digit)
;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-digit'
(defun mon-is-digit (x)
"t when X is a digit character.\n
:SEE-ALSO `mon-is-letter'.\n►►►"
  (cond ((stringp x) (mon-is-digit (string-to-char x)))
        ((integerp x) (and (<= ?0 x) (<= x ?9)))
        (t nil)))
) ;; inner-unless1
;;
;;; :TEST-ME (mon-is-digit (char-after (point)))
;;; :TEST-ME (mon-is-digit (char-after (point)))

;;; ==============================
;;; :COURTESY Pascal J. Bourguignon :HIS pjb-strings.el :WAS `is-letter'
(unless (functionp 'mon-is-letter)
(defun mon-is-letter (x)
"t when X is an alpha character.\n
:SEE-ALSO `mon-is-digit'.\n►►►"
  (cond ((stringp x) (mon-is-letter (string-to-char x)))
        ((integerp x) (not (equal (downcase x) (upcase x))))
        (t nil)))
) ;; inner-unless2
;;
;;; :TEST-ME (mon-is-letter (char-after (point)))x
;;; :TEST-ME (mon-is-letter (char-after (point)))8
;;; :TEST-ME (mon-is-letter ?x)

(unless (functionp 'mon-string-to-symbol)
;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-26T17:08:02-04:00Z}#{09353} - by MON KEY>
(defun mon-string-to-symbol (str)
  "Return string STR as a symbol.\n
:EXAMPLE\n\(mon-string-to-symbol \"Bubba\")\n►►►"
  (car (read-from-string str)))
) ;; inner-unless3
;;
;;; :TEST-ME (mon-string-to-symbol "Bubba")
) ;; outur-unless
) ;; eval-when

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-03T14:40:12-05:00Z}#{09494} - by MON>
(defvar *mon-register-config-tags*
  '((:W-COMMENT-PFX . ((87  . ":WAS")
                       (70  . ":FROM")
                       (84  . ":TO")
                       (83  . ":SEE")
                       (78  . ":NOTE")
                       (79  . ":OLD")
                       (77  . ":MATCH")
                       (69  . ":EXAMPLE")))
    (:TIMESTAMPED . ((67 .  ":CHANGED")
                     (65 . ":ADDED")))
    (:NO-COMMENT-PFX . ((115 . "shell>"))))
  "*Alist of char and tag strings for commenting source.
Keys map as follows: 
:W-COMMENT-PFX ->  Strings with whitespace wrapping.
:TIMESTAMPED -> Strings with a timestamp appended per `mon-stamp'.
:NO-COMMENT-PFX -> Strings without whitespace wrapping.\n
:CALLED-BY `mon-set-register->tags'.
:SEE-ALSO `mon-make-set-register->tags-docs'\n►►►")
;;
;;; :TEST-ME (assoc :W-COMMENT-PFX *mon-register-config-tags*) 
;;; :TEST-ME (assoc :TIMESTAMPED *mon-register-config-tags*)
;;; :TEST-ME (assoc :NO-COMMENT-PFX *mon-register-config-tags*)
;;
;; (progn (makunbound '*mon-register-config-tags*) (unintern '*mon-register-config-tags*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-03T17:50:24-05:00Z}#{09494} - by MON KEY>
(defun* mon-set-register->tags (&key sharp semic)
  ""
  (interactive)
  (let ((mp-keys *mon-register-config-tags*)
        (setr-wsp #'(lambda (x) 
                      (set-register (car x) 
                                    (cond (sharp (concat  "# " (cdr x) " "))
                                          (semic (concat  ";; " (cdr x) " "))
                                          (t (concat  ";; " (cdr x) " "))))))
        (setr-no-wsp #'(lambda (x) (set-register (car x) (cdr x))) )
        ;; Generates the backquote lambda template:
        ;; #'(lambda () (interactive) (insert (concat "# :ADDED " (mon-stamp)))))
        (setr-tstamp #'(lambda (x)          
                         `(global-set-key 
                           ,(concat "\C-cri" (char-to-string (car x)))
                           #'(lambda () 
                               (interactive)
                               (insert (concat 
                                        ,(cond (sharp (concat  "# " (cdr x) " "))
                                               (semic (concat  ";; " (cdr x) " "))
                                               (t (concat  ";; " (cdr x) " " )))
                                        (mon-stamp))))) ;backquote template
                         )                              ;;outer lambda
          ))
    (mapc #'(lambda (x) (funcall setr-wsp x)) (cdr (assoc :W-COMMENT-PFX mp-keys)))
    (mapc #'(lambda (x) (funcall setr-no-wsp x)) (cdr (assoc :NO-COMMENT-PFX mp-keys)))
    (mapc #'(lambda (x) (eval (funcall setr-tstamp x))) (cdr (assoc :TIMESTAMPED mp-keys)))))
;;  
;;; :TEST-ME (progn (mon-set-register->tags :sharp t) (insert-register 77))
;;; :TEST-ME (progn (mon-set-register->tags :semic t)(insert-register 77))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-03T17:32:01-05:00Z}#{09494} - by MON>
(defun mon-make-set-register->tags-docs ()
"Helper function puts register contents/keybinding values on 
docstring of `mon-set-register->tags'.\n
:SEE-ALSO `*mon-register-config-tags*'.\n►►►"
  (let* ((build-doc)
        (do-s t)
        (map-sym #'(lambda (q) 
                     (mapc #'(lambda (u) (push u build-doc))
                           (mapcar 'car (cdr (assoc q *mon-register-config-tags*))))))
        (map-kys #'(lambda (z)
                     (if do-s
                         (princ 
                          (format "\C-xri%s -> %s\n" (char-to-string z)(get-register z))
                          (current-buffer))
                         (progn 
                           (princ 
                            (format "%s -> " (concat "\C-cri" (char-to-string z)))
                            (current-buffer))
                           (funcall (key-binding (concat "\C-cri" (char-to-string z))))
                           (newline))))))

    (princ "\n;; :SEMI-C Style\n" (current-buffer))
    (mon-set-register->tags)
    (funcall map-sym :W-COMMENT-PFX)
    (funcall map-sym :NO-COMMENT-PFX)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)
    (setq do-s nil
          build-doc nil)
    (funcall map-sym :TIMESTAMPED)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)
    (princ "\n;; :SHARP Style\n" (current-buffer))
    (mon-set-register->tags :sharp t)
    (setq do-s t
          build-doc nil)
    (funcall map-sym :W-COMMENT-PFX)
    (funcall map-sym :NO-COMMENT-PFX)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)
    (setq do-s nil
          build-doc nil)
    (funcall map-sym :TIMESTAMPED)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)))

;; Now, tack on the docstring for `mon-set-register->tags'
(eval-when-compile
  (let (put-reg) 
  (setq put-reg
        (with-temp-buffer
          (mon-make-set-register->tags-docs)
          (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
  (mon-help-put-var-doc-val->func 'put-reg 'mon-set-register->tags
  "Set registers for config annotations which need BOL commented.\n
When keyword arg SHARP is non-nil strings are wrapped in whitespace e.g.
\"# :<TAG> \"\n
When keyword arg SEMIC is non-nil strings are wrapped in whitespace e.g.
\";; :<TAG> \"  This is the default format if neither keyword arg is non-nil.\n
Register strings are built according to the alist key values of:
`:W-COMMENT-PFX' `:NO-COMMENT-PFX' `:TIMESTAMPED' 
in the variable `*mon-register-config-tags*'.\n
:NOTE Elements of the alist key `:NO-COMMENT-PFX' are not wrapped.\n
Elements of the alist key `:TIMESTAMPED' are bound with non-standard
keybindings: \"\C-cri<CHAR>\" :WARNING! bound as with `global-set-key'.\n
Elements of `:W-COMMENT-PFX' `:NO-COMMENT-PFX' have standard register
insertion keybindings: \"\C-xri<CHAR>\"\n\n:EXAMPLE"
nil
"\n:SEE-ALSO `mon-make-set-register->tags-docs'\n►►►")))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T16:12:00-0400Z - by MON KEY>
(defvar *mon-cntl-char-registers* nil)
;;
(when (not (bound-and-true-p  *mon-cntl-char-registers*))
  (setq *mon-cntl-char-registers*
        ;;`(,@(number-sequence 1 8) ,@(number-sequence 11 26))
        '(?\C-a ?\C-b ?\C-c ?\C-d ?\C-e ?\C-f ?\C-g ?\C-h ?\C-k ?\C-l 
                ?\C-m ?\C-n ?\C-o ?\C-p ?\C-r ?\C-s ?\C-t ?\C-u ?\C-v ?\C-w ?\C-x ?\C-y ?\C-z)))

;;; ==============================
;;; Build vars documentation string. The 23 `nth' calls are prob. costlty...
;;; But, character representations disappear when uploaded to emacs-wiki.
;;; ("^A "^B "^C "^D" "^E" "^F" "^G" "^H" "^K" "^L" "^M" "^N"
;;;   0    1   2   3    4    5    6    7    8    9   10   11 
;;;  "^O" "^P" "^Q" "^R" "^S" "^T" "^U" "^V" "^W" "^X" "^Y" "^Z")
;;;   12   13   14   15   16   17   18   19   20    21  22    23
(eval-when-compile
  (let ((self-puke)
      (char-rep (mapcar 'char-to-string `(,@(number-sequence 1 8) ,@(number-sequence 11 26)))))
    (setq self-puke
          (format
  "*List of character literals or Control Chars 'C-[a-z]' - ASCII chars 1-26. \n
  1 \"%s\"    2 \"%s\"    3 \"%s\"   4 \"%s\"    5 \"%s\"    6 \"%s\"
 \"?\\C-a\"   \"?\\C-b\"   \"?\\C-c\"  \"?\\C-d\"   \"?\\C-e\"   \"?\\C-f\"\n
  7 \"%s\"    8 \"%s\"    9  TAB   10 LF    11 \"%s\"   12 \"%s\"
 \"?\\C-g\"   \"?\\C-h\"   \"?\\C-i\"  \"\?\\C-j\"   \"?\\C-k\"   \"?\\C-l\"\n
 13 \"%s\"   14 \"%s\"   15 \"%s\"  16 \"%s\"   17 \"%s\"   18 \"%s\" 
 \"?\\C-m\"   \"?\\C-n\"   \"?\\C-o\"  \"?\\C-p\"   \"\?\\C-q\"   \"?\\C-r\"\n 
 19 \"%s\"   20 \"%s\"   21 \"%s\"  22 \"%s\"   23 \"%s\"   24 \"%s\"
 \"?\\C-s\"   \"?\\C-t\"   \"?\\C-u\"  \"?\\C-v\"   \"?\\C-w\"   \"?\\C-x\"\n 
 25 \"%s\"   26 \"%s\"
 \"?\\C-y\"   \"?\\C-z\"\n
:NOTE chars 9, 10, 17 e.g TAB and LF 'C-q' are not bound in the VARS list.
They are included here for completeness.\n
:EXAMPLE
\(concat \"?\"\(mapconcat 'chanr-to-string *mon-cntl-char-registers*  \" ?\") 
\(mapconcat 'char-to-string *mn-cntl-char-registers*  \" \")\n
:SEE-ALSO `*mon-digit-registers*',`*mon-digit-shifted-registers*',
`*mon-symbol-registers*', `*mon-upper-case-registers*',
`*mon-lower-case-registers*', `*registr-of-registers*'.\n►►►"
(nth 0 char-rep)(nth 1 char-rep)(nth 2 char-rep)(nth 3 char-rep)(nth 4 char-rep)(nth 5 char-rep) ;1-6
(nth 6 char-rep)(nth 7 char-rep)(nth 8 char-rep)(nth 9 char-rep) ;7-12
(nth 10 char-rep)(nth 11 char-rep)(nth 12 char-rep)(nth 13 char-rep)(nth 14 char-rep)(nth 15 char-rep) ;13-18
(nth 16 char-rep)(nth 17 char-rep)(nth 18 char-rep)(nth 19 char-rep)(nth 20 char-rep)(nth 21 char-rep) ;19-24
(nth 22 char-rep)(nth 23 char-rep) ;25-26
))
(put '*mon-cntl-char-registers* 'variable-documentation  self-puke)))
;;
;;; :TEST-ME  *mon-cntl-char-registers* 
;;; :TEST-ME (symbol-value '*mon-cntl-char-registers*)
;;; :TEST-ME (get '*mon-cntl-char-registers* 'variable-documentation)
;;; :TEST-ME (describe-variable '*mon-cntl-char-registers*)
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-cntl-char-registers*  " ?"))
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-cntl-char-registers*  " ?"))
;;; :TEST-ME (mapconcat 'char-to-string *mon-cntl-char-registers*  " ")
;;
;;;(progn (makunbound '*mon-cntl-char-registers*) (unintern '*mon-cntl-char-registers*))


;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:49:10-0400Z - by MON KEY>
;; Numbers ASCII 48-57
;;(48 49 50 51 52 53 54 55 56 57)
(defvar *mon-digit-registers* nil
  "*Digit chars in 0-9. ASCII range 48-57.\n
;; 48 49 50 51 52 53 54 55 56 57
;; \?0 \?1 \?2 \?3 \?4 \?5 \?6 \?7 \?8 \?9 \n
:EXAMPLE
\(concat \"?\"\(mapconcat 'char-to-string *mon-digit-registers*  \" ?\"))
\(mapconcat 'char-to-string *mon-digit-registers*  \" \")\n
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-digit-shifted-registers*',
`*mon-symbol-registers*', `*mon-upper-case-registers*',
`*mon-lower-case-registers*', `*registr-of-registers*'.\n►►►")
;;
(when (not (bound-and-true-p  *mon-digit-registers*))
  (setq *mon-digit-registers*
        '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
;;
;;; :TEST-ME  *mon-digit-registers*
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-digit-registers*  " ?")))
;;; :TEST-ME (mapconcat 'char-to-string *mon-digit-registers*  "\" \"" )
;;
;;;(progn (makunbound '*mon-digit-registers*) (unintern '*mon-digit-registers*))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:49:04-0400Z - by MON KEY>
(defvar *mon-digit-shifted-registers* 'nil
  "Symbol chars for shifted digits \(keyboard keys 1-0\).\n
;; 33  64  35  36  37  94  38  42   40   41 
;; ?!  ?@  ?#  ?$  ?%  ?^  ?&  ?*  ?\\(  ?\\) 
;;  1   2   3   4   5   6   7   8    9    0 \n
:EXAMPLE
\(concat \"?\" \(mapconcat 'char-to-string *mon-digit-shifted-registers*  \" ?\")) 
\(mapconcat 'char-to-string *mon-digit-shifted-registers*  \" \")
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-symbol-registers*', `*mon-upper-case-registers*',
`*mon-lower-case-registers*', `*registr-of-registers*'.\n►►►")
;;
(when (not (bound-and-true-p  *mon-digit-shifted-registers*))
  (setq *mon-digit-shifted-registers*
        '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?))))
;;
;;; :TEST-ME  *mon-digit-shifted-registers*
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-digit-shifted-registers*  " ?" ))
;;; :TEST-ME (mapconcat 'char-to-string *mon-digit-shifted-registers*  "\" \"" )
;;
;;;(progn (makunbound '*mon-digit-shifted-registers*) (unintern '*mon-digit-shifted-registers*))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:49:01-0400Z - by MON KEY>
(defvar *mon-symbol-registers* 'nil
  "Symbol chars in ASCII ranges 43-47, 60-62, 92-95, 123-126.\n
;; 43 45 46 47 60 61 62 91  92 93 95 123 124 125 126
;; \?+ \?- \?. \?/ \?< \?= ?\> \?[ \?\\\\ \?] \?_  \?{  \?|  \?}  \?~ \n
:EXAMPLE
\(concat \"?\" \(mapconcat 'char-to-string *mon-symbol-registers*  \" ?\"))
\(mapconcat 'char-to-string *mon-symbol-registers*  \" \" )
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-digit-shifted-registers*',
`*mon-upper-case-registers*', `*mon-lower-case-registers*'
`*registr-of-registers*'.\n►►►")
;;
(when (not (bound-and-true-p  *mon-symbol-registers*))
  (setq *mon-symbol-registers*
        '(?+ ?- ?. ?/ ?< ?= ?> ?\\ ?\[ ?\] ?_  ?{  ?|  ?}  ?~)))
;;
;;; :TEST-ME  *mon-symbol-registers*
;;; :TEST-ME (mapconcat 'char-to-string *mon-symbol-registers*  " ")
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-symbol-registers*  " ?"))
;;
;;;(progn (makunbound '*mon-symbol-registers*) (unintern '*mon-symbol-registers*))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:48:54-0400Z - by MON KEY>;; 
(defvar *mon-upper-case-registers* nil
  "*Uppercase Letters ASCII chars 65-90.\n
;; 65 66 67 68 69 70 71 72 73 74 75 76 77 
;; \?A \?B \?C \?D \?E \?F \?G \?H \?I \?J \?K \?L \?M \n;; 
;; 78 79 80 81 82 83 84 85 86 87 88 89 90   
;; \?N \?O \?P \?Q \?R \?S \?T \?U \?V \?W \?X \?Y \?Z \n
:EXAMPLE
\(concat \"?\" \(mapconcat 'char-to-string *mon-upper-case-registers*  \" ?\"\)\)
\(mapconcat 'char-to-string *mon-upper-case-registers*  \" \" \)
\(mapcar 'char-to-string *mon-upper-case-registers*\)
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-digit-shifted-registers*',
`*mon-symbol-registers*', `*mon-lower-case-registers*',
`*registr-of-registers*'.\n►►►")
;;
(when (not (bound-and-true-p  *mon-upper-case-registers*))
  (setq *mon-upper-case-registers*
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M 
    ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z)))
;;
;;; :TEST-ME  *mon-upper-case-registers*
;;; :TEST-ME (mapconcat 'char-to-string *mon-upper-case-registers*  " " )
;;; :TEST-ME (mapconcat 'char-to-string *mon-upper-case-registers*  " ?" )
;;
;;;(progn (makunbound '*mon-upper-case-registers*) (unintern '*mon-upper-case-registers*))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:48:49-0400Z - by MON KEY>
(defvar *mon-lower-case-registers* nil
  "*Lowercase Letters ASCII chars 97-122.\n
;;  97  98  99 100 101 102 103 104 105 106 107 108 109 
;;  \?a  \?b  \?c  \?d  \?e  \?f  \?g  \?h  \?i  \?j  \?k  \?l  \?m \n;; 
;; 110 111 112 113 114 115 116 117 118 119 120 121 122
;;  \?n  \?o  \?p  \?q  \?r  \?s  \?t  \?u  \?v  \?w  \?x  \?y  \?z \n
:EXAMPLE 
\(concat \"?\" \(mapconcat 'char-to-string *mon-lower-case-registers*  \" ?\"))
\(mapconcat 'char-to-string *mon-lower-case-registers*  \" \") 
\(mapcar 'char-to-string *mon-lower-case-registers*)
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-digit-shifted-registers*',
`*mon-symbol-registers*', `*mon-upper-case-registers*',
`*registr-of-registers*'.\n►►►")
;;
(when (not (bound-and-true-p  *mon-lower-case-registers*))
  (setq *mon-lower-case-registers*
        '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m 
          ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))
;;
;;; :TEST-ME  *mon-lower-case-registers*
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-lower-case-registers*  " ?"))
;;; :TEST-ME (mapconcat 'char-to-string *mon-lower-case-registers*  " ")
;;; :TEST-ME (mapcar 'char-to-string *mon-lower-case-registers*)
;;
;;;(progn (makunbound '*mon-lower-case-registers*) (unintern '*mon-lower-case-registers*))  

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-07T19:20:57-04:00Z}#{09325} - by MON KEY>
(defvar *registr-of-registers* nil
   "*List mapping symbols to register vars.
:CNTRL    -> `*mon-cntl-char-registers*';
:DIGIT    -> `*mon-digit-registers*';
:DIGIT-S  -> `*mon-digit-shifted-registers*';
:SYMBOL   -> `*mon-symbol-registers*';
:UPPER    -> `*mon-upper-case-registers*';
:LOWER    -> `*mon-lower-case-registers*';.\n►►►")
;;
(when (not (bound-and-true-p *registr-of-registers*))
(setq *registr-of-registers*
      '((cntrl   *mon-cntl-char-registers*)
        (digit   *mon-digit-shifted-registers*)
        (digit-S *mon-digit-shifted-registers*)
        (symbol  *mon-symbol-registers*)     
        (upper   *mon-upper-case-registers*) 
        (lower   *mon-lower-case-registers*))))
;;
;;; :TEST-ME  *registr-of-registers*
;;
;;;(progn (makunbound '*registr-of-registers*)  (unintern '*registr-of-registers*))

;;; ==============================
;;; :NOTE To remember function exists. `prin1-char' is defined in lisp-mode.el
;;; :CREATED <Timestamp: #{2009-09-02T10:49:12-04:00Z}#{09363} - by MON KEY>
(defalias 'mon-prin1-char->?char 'prin1-char
  "Return a string representing char as a character rather than as an integer.
If char is not a character, return nil.\n
:EXAMPLE\n\(prin1-char 32\) ;=>\"? \"\n\(prin1-char 63\) ;=>\"??\"
\(prin1-char 10\) ;=>\"?\\\\C-j\"\n►►►")
;;
;;; :TEST-ME (mon-prin1-char->?char 32)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-03-W32-1T18:47:33-0400Z - by MON KEY>
(defun mon-coerce->char (thing)
  "Convert THING with length of 1 to a char as per `string-to-char'.
THING can be a number, symbol, or string. IF THING is characterp returns thing.
If coercion of thing fails throw an error.\n►►►"
 (cond   ((mon-is-digit thing)
          (cond ((stringp thing)               
                 (cond ((= (length thing) 1) (string-to-char thing))
                       ((> (length thing) 1) (mon-coerce->char (string-to-number thing)))))
                ((not (stringp thing)) thing))) 
         ((mon-is-letter thing)
          (if (stringp thing)
              (if (= (length thing) 1)
                  (string-to-char thing)
                (error (format "%s has a length of %s, can only coerce strings of length 1" thing (length thing))))
            thing))
         ((and (numberp thing)
               (not (mon-is-digit thing))
               (not (mon-is-letter thing))
               (cond ((and (wholenump thing) (not (floatp thing)))
                      (if (> 10 thing) 
                          (string-to-char (number-to-string thing))
                        thing))
                     ((floatp thing)
                      (error (format "Can't coerce %S '%S' to char" (type-of thing) thing)))
                     ((not (wholenump thing))
                      (error (format "Can't coerce %S '%S' to char" (type-of thing) thing))))))
         ((stringp thing)
          (cond ((/= (string-to-number  thing) 0)
                 (mon-coerce->char (string-to-number thing)))
                ;; :MODIFICATIONS <Timestamp: #{2009-08-26T15:31:55-04:00Z}#{09353} - by MON KEY>
                ;; The logic on this was busted. we're throwing an error uncondtionally the truth.
                ;; Most likely was transcribing an if and didn't close it
                ;; ((= (length thing) 1)
                ;;  (string-to-char thing)
                ;;  (error (format "%s has a length of %s, can only coerce strings of length 1" thing (length thing))))))
                ((= (length thing) 1) (string-to-char thing))
                ((or (= (length thing) 0) (> (length thing) 1))
                 (error (format "%s has a length of %s, can only coerce strings of length 1" thing (length thing))))))
         ((eq (type-of thing) 'symbol)
          (let ((thing-string (format "%s" (identity thing))))
            (if (= (length thing-string) 1)
                 (mon-coerce->char thing-string)
              (error (format "Can't coerce %S '%s' with > length 1" (type-of thing) thing-string)))))
         (t (error   (format "Can't coerce %S '%S' to char" (type-of thing) thing )))))
;; 
;;; :TEST-ME (mon-coerce->char 'b)
;;; :TEST-ME (mon-coerce->char "b")
;;; :TEST-ME (mon-coerce->char ?b)
;;; :TEST-ME (mon-coerce->char 8)
;;; :TEST-ME (mon-coerce->char '8)
;;; :TEST-ME (mon-coerce->char "8")
;;; :TEST-ME (mon-coerce->char 44)
;;; :TEST-ME (mon-coerce->char "44")
;;; :TEST-ME (mon-coerce->char '44)
;;; :TEST-ME (mon-coerce->char ?8)
;;; FOLLOWING-SHOULD-FAIL:
;;; :TEST-ME (mon-coerce->char 8.8)
;;; :TEST-ME (mon-coerce->char "8.8")
;;; :TEST-ME (mon-coerce->char -8)
;;; :TEST-ME (mon-coerce->char (get-buffer (buffer-name)))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-04-W32-2T17:43:27-0400Z - by MON KEY>
(defun mon-decode-meta-key-event (event)
  "The key <meta> lives on the 2**27 bit:\n
\(expt 2 27) ;=> 134217728\n
<Meta>-some-ASCII-key in range 1-127 is 2**27 + char\n
So, to decode M-3 i.e. '<meta>-3' do this:\n
\(- \(+ ?3 \(expt 2 27\)\) \(expt 2 27\)\) ;=> 51 
e.g. \(- 134217779  134217728\) ;=> 51\n
:EXAMPLE\n\(mon-decode-meta-key-event 134217771\)\n
:SEE-ALSO `mon-catch-meta-key' `mon-coerce->char', `mon-string-to-symbol'.\n►►►"
  (let ((M-key (expt 2 27)))
    ;; (event-key event))
    (if (> event M-key)
        (list (car (event-modifiers event))
              ;; (- event-key M-key))
              (mon-string-to-symbol (char-to-string (event-basic-type event)))))))
;;              
;;; :TEST-ME (mon-decode-meta-key-event 134217771)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-04-W32-2T19:26:05-0400Z - by MON KEY>
(defun mon-catch-meta-key (&optional event->string) ;(event-vect)
  "Return the first <meta>-? key prefix call to wrapper function.
When optional arg EVENT->STRING is non-nil return a meta event as a string.
When a meta-key event is not present return the first event modifer passed.
Can be alled programatically within a wrapper functions.\n
:EXAMPLE\n\(mon-catch-meta-key\)   ;<- M-3 C-x C-e ;=> (meta 51)
\(mon-catch-meta-key t\) ;<- M-3 C-x C-e ;=>\"meta-3\"\n
:SEE-ALSO `mon-decode-meta-key-event', `mon-catch-meta-key' `mon-coerce->char',
`mon-string-to-symbol'.\n►►►"
  (let ((key-seq (listify-key-sequence (this-command-keys-vector)));event-vect))
        (map-events))
    (setq map-events
          (mapcar '(lambda (x) (car (event-modifiers x)))
                  key-seq))
    (if (equal (position 'meta map-events) 0)
        (cond (event->string
               (format "%s-%s" 
                       (car (event-modifiers (car key-seq)))
                       (char-to-string (event-basic-type (car key-seq)))))
               ;; (if (and (>= (event-basic-type (car key-seq)) 48)
               ;;          (<= (event-basic-type (car key-seq)) 57))
               ;;   (mon-string-to-symbol 
               ;;  (event-basic-type (car key-seq))))
              (t (list  (car (event-modifiers (car key-seq)))
                        (event-basic-type (car key-seq)))))
      (car (event-modifiers (car key-seq))))))
;;
;;; :TEST-ME (mon-catch-meta-key);<- M-3 C-x C-e ;=> (meta 51)
;;; :TEST-ME (mon-catch-meta-key t) ;<- M-3 C-x C-e ;=>"meta-3"
;;; :TEST-ME (let ((event (mon-catch-meta-key)))
;;;               (when (listp event)(cadr event)))

;;; ==============================
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `qr12' 
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T15:28:22-0400Z - by MON KEY>
;;; :NOTE The original didn't read registers they were hard bound to ?1 and ?2.
(defun mon-query-replace-register1<-reg2 (register1 register2 &optional start end use-regexp)
 "query-replace contents of REGISTER1 with REGISTER2 in the buffer.\n
Does not move point.\n►►►"
 (interactive (list 
               (read-string "Replace string-matching contents of register :")
               (read-string "With string-matching contents of register :")
               (when (use-region-p) (region-beginning)) 
               (when (use-region-p) (region-end))))
 (let ((r1 (mon-coerce->char register1))
       (r2 (mon-coerce->char register2)))
   (save-excursion
     (save-restriction
       (when start (narrow-to-region start end))
       (goto-char (point-min))
       (if current-prefix-arg 
           (query-replace-regexp (get-register r1) (get-register r2))
         (query-replace (get-register r1) (get-register r2)))
       ))))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-08T17:04:24-04:00Z}#{09326} - by MON KEY>
(defun* mon-reset-registers (&key intrp cntrl digit digit-S symbol upper lower all)
  "Reset the contents each all registers associated with keywords.
:CNTRL    -> `*mon-cntl-char-registers*';
:DIGIT    -> `*mon-digit-registers*';
:DIGIT-S  -> `*mon-digit-shifted-registers*';
:SYMBOL   -> `*mon-symbol-registers*';
:UPPER    -> `*mon-upper-case-registers*';
:LOWER    -> `*mon-lower-case-registers*';
:ALL      ->  everything in `*registr-of-registers*'
When called interactively or INTRP is non-nil resets all registers.\n
:SEE-ALSO `mon-set-all-registers-to-char', `*registr-of-registers*'.\n►►►"
  (interactive (list :intrp t))
(let ((reg-keys `(,cntrl ,digit ,digit-S ,symbol ,upper ,lower))
      (reg-lists  '(cntrl  digit  digit-S  symbol  upper lower))
      (pairs)
      (make-reg-list)
      (pop-registers))
  (setq pairs (pairlis reg-keys reg-lists))
  (setq make-list '())
(if (or intrp all)
    ;; (setq make-reg-list(mapcar (lambda (x) (symbol-value (cadr x))) *registr-of-registers*))
    (progn
      (mapc (lambda (x) 
              (setq make-reg-list (cons (symbol-value (cadr x)) make-reg-list))) 
            *registr-of-registers*)
      (setq make-reg-list (reverse make-reg-list)))
  (progn
    (mapc (lambda (x)
            (when (car x)
              (setq make-reg-list (cons (symbol-value (cadr (assoc (cdr x) *registr-of-registers*))) make-reg-list))))
          pairs)
    (setq make-reg-list (reverse make-reg-list))))
(setq pop-registers make-reg-list)
(while pop-registers
  (let ((popd (pop pop-registers)))
    (mapc (lambda (x)
            (set-register x nil))
          popd))))
(when intrp (message "All registers were emptied.")))
;;
;;; ==============================
;;; :TEST-ME
;;; Evaluate following form to refill registers with non-nil val:
;;; (mon-set-all-registers-to-char)
;;;
;;; :TEST-ME (mon-reset-registers :cntrl t)
;;; (get-register 26)  ; => :CNTRL
;;;
;;; (mon-set-all-registers-to-char)
;;; :TEST-ME (mon-reset-registers :all t)
;;; (get-register 26)  ; => :CNTRL
;;; (get-register ?9)  ; => :DIGIT  
;;; (get-register ?*)  ; => :DIGIT-S
;;; (get-register ?|)  ; => :SYMBOL 
;;; (get-register ?A)  ; => :UPPER  
;;; (get-register ?z)  ; => :LOWER  
;;;
;;; (mon-set-all-registers-to-char)
;;; :TEST-ME (mon-reset-registers :intrp t)
;;; (mon-set-all-registers-to-char)
;;;(call-interactively 'mon-reset-registers)
;;
;;; :TEST-ME
;;(tree-equal
;; (mon-reset-registers :all t)
;; (mon-reset-registers :cntrl t :digit t :digit-S t :symbol t :upper t :lower t))
;; (mon-reset-registers :all t)
;; (mon-reset-registers :cntrl t :digit t :digit-S t :symbol t :upper t :lower t))
;; (symbol-value (cadr (assoc 'cntrl *registr-of-registers*)))
;;
;;   (setq which-list '())
;;   (if all ;gather map all values
;;       (mapcar (lambda (x) 
;;                 (setq which-list 
;;                       (cons (symbol-value (car x  *registr-of-registers*)))
;;                             which-list)))
;; ;              *registr-of-registers*)
;;   (mapcar reg-lists
;;           (lambda (x) 
;;             (setq which-list 
;;                   (cons (symbol-value (car (assoc x  *registr-of-registers*)))
;;                         which-list)))
;;           *registr-of-registers*))
;;
;;(mapconcat 'char-to-string *mon-digit-registers*  \"( \")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-08T16:22:27-04:00Z}#{09326} - by MON KEY>
(defun mon-set-all-registers-to-char ()
  "Set all registers to the string representation of their char-code.
Useful when testing/debugging register contents. Examining an empty register
returns nil. Having zapped all register contents it is difficult to tell 
whether if it was emptied programatically or simply never set! We need to reset
all registers to 'something' in order to test they are _NOT_ empty.
:SEE-ALSO `mon-reset-registers', `*registr-of-registers*'.\n►►►"
  (interactive)
  (progn
    ;; :CNTRL-CHAR-REGISTERS
    (set-register ?\C-a   "C-a") (set-register ?\C-a   "C-a")  (set-register ?\C-b   "C-b")
    (set-register ?\C-c   "C-c") (set-register ?\C-d   "C-d")  (set-register ?\C-e   "C-e")
    (set-register ?\C-f   "C-f") (set-register ?\C-g   "C-g")  (set-register ?\C-h   "C-h")
    (set-register ?\C-i   "C-i") (set-register ?\C-j   "C-j")  (set-register ?\C-k   "C-k")
    (set-register ?\C-l   "C-l") (set-register ?\C-m   "C-m")  (set-register ?\C-n   "C-n")
    (set-register ?\C-o   "C-o") (set-register ?\C-p   "C-p")  (set-register ?\C-q   "C-q")
    (set-register ?\C-r   "C-r") (set-register ?\C-s   "C-s")  (set-register ?\C-t   "C-t")
    (set-register ?\C-u   "C-u") (set-register ?\C-v   "C-v")  (set-register ?\C-w   "C-w")
    (set-register ?\C-x   "C-x") (set-register ?\C-y   "C-y")  (set-register ?\C-z   "C-z")
    ;; :SYMBOL-REGISTER
    (set-register ?+      "?+")  (set-register ?-      "?-")   (set-register ?.      "?.")
    (set-register ?/      "?/")  (set-register ?<      "?<")   (set-register ?=      "?=")
    (set-register ?>      "?>")  (set-register ?\\    "?\\")   (set-register ?\[     "?[")
    (set-register ?\]     "?]")  (set-register ?_      "?_")   (set-register ?{      "?{")
    (set-register ?|      "?|")  (set-register ?}      "?}")   (set-register ?~      "?~")
    ;; :DIGIT-REGISTERS
    (set-register ?0      "?0")  (set-register ?1      "?1")   (set-register ?2      "?2")
    (set-register ?3      "?3")  (set-register ?4      "?4")   (set-register ?5      "?5")
    (set-register ?6      "?6")  (set-register ?7      "?7")   (set-register ?8      "?8")
    (set-register ?9      "?9")
    ;; :DIGIT-SHIFTED-REGISTERS
    (set-register ?!      "?!")  (set-register ?@      "?@")   (set-register ?#      "?#")
    (set-register ?$      "?$")  (set-register ?%      "?%")   (set-register ?^      "?^")
    (set-register ?&      "?&")  (set-register ?*      "?*")   (set-register ?(      "?(")
    (set-register ?)      "?)")
    ;; :UPPERCASE-REGISTERS
    (set-register ?A      "?A")  (set-register ?B      "?B")  (set-register ?C      "?C")
    (set-register ?D      "?D")  (set-register ?E      "?E")  (set-register ?F      "?F")
    (set-register ?G      "?G")  (set-register ?H      "?H")  (set-register ?I      "?I")
    (set-register ?J      "?J")  (set-register ?K      "?K")  (set-register ?L      "?L")
    (set-register ?M      "?M")  (set-register ?N      "?N")  (set-register ?O      "?O")
    (set-register ?P      "?P")  (set-register ?Q      "?Q")  (set-register ?R      "?R")
    (set-register ?S      "?S")  (set-register ?T      "?T")  (set-register ?U      "?U")
    (set-register ?V      "?V")  (set-register ?W      "?W")  (set-register ?X      "?X")
    (set-register ?Y      "?Y")  (set-register ?Z      "?Z")
    ;; :LOWERCASE-REGISTERS
    (set-register ?a      "?a")  (set-register ?b      "?b")  (set-register ?c      "?c")
    (set-register ?d      "?d")  (set-register ?e      "?e")  (set-register ?f      "?f")
    (set-register ?g      "?g")  (set-register ?h      "?h")  (set-register ?i      "?i")
    (set-register ?j      "?j")  (set-register ?k      "?k")  (set-register ?l      "?l")
    (set-register ?m      "?m")  (set-register ?n      "?n")  (set-register ?o      "?o")
    (set-register ?p      "?p")  (set-register ?q      "?q")  (set-register ?r      "?r")
    (set-register ?s      "?s")  (set-register ?t      "?t")  (set-register ?u      "?u")
    (set-register ?v      "?v")  (set-register ?w      "?w")  (set-register ?x      "?x")
    (set-register ?y      "?y")
    (message "all registers set to char-representation")))

;;; ==============================
;;; Uncomment and evaluate to indescriminately reset all registers to empty strings.
;; (progn
;;   ;; :CONTROL-CHAR REGISTES
;;   (set-register ?\C-a   "")  (set-register ?\C-a   "")  (set-register ?\C-b   "")
;;   (set-register ?\C-c   "")  (set-register ?\C-d   "")  (set-register ?\C-e   "")
;;   (set-register ?\C-f   "")  (set-register ?\C-g   "")  (set-register ?\C-h   "")
;;   (set-register ?\C-i   "")  (set-register ?\C-j   "")  (set-register ?\C-k   "")
;;   (set-register ?\C-l   "")  (set-register ?\C-m   "")  (set-register ?\C-n   "")
;;   (set-register ?\C-o   "")  (set-register ?\C-p   "")  (set-register ?\C-q   "")
;;   (set-register ?\C-r   "")  (set-register ?\C-s   "")  (set-register ?\C-t   "")
;;   (set-register ?\C-u   "")  (set-register ?\C-v   "")  (set-register ?\C-w   "")
;;   (set-register ?\C-x   "")  (set-register ?\C-y   "")  (set-register ?\C-z   "")
;;   ;; :SYMBOL-REGISTER
;;   (set-register ?+      "")   (set-register ?-     "")  (set-register ?.      "")
;;   (set-register ?/      "")  (set-register ?<      "")  (set-register ?=      "")
;;   (set-register ?>      "")  (set-register ?\\     "")  (set-register ?\[     "")
;;   (set-register ?\]     "")  (set-register ?_      "")  (set-register ?{      "")
;;   (set-register ?|      "")  (set-register ?}      "")  (set-register ?~      "")
;;   ;; :DIGIT-REGISTERS
;;   (set-register ?0      "")  (set-register ?1      "")  (set-register ?2      "")
;;   (set-register ?3      "")  (set-register ?4      "")  (set-register ?5      "")
;;   (set-register ?6      "")  (set-register ?7      "")  (set-register ?8      "")
;;   (set-register ?9      "")
;;   ;; :DIGIT-SHIFTED-REGISTERS
;;   (set-register ?!      "")  (set-register ?@      "")  (set-register ?#      "")
;;   (set-register ?$      "")  (set-register ?%      "")  (set-register ?^      "")
;;   (set-register ?&      "")  (set-register ?*      "")  (set-register ?(      "")
;;   (set-register ?)      "")
;;   ;; :UPPERCASE-REGISTERS
;;   (set-register ?A      "")  (set-register ?B      "")  (set-register ?C      "")
;;   (set-register ?D      "")  (set-register ?E      "")  (set-register ?F      "")
;;   (set-register ?G      "")  (set-register ?H      "")  (set-register ?I      "")
;;   (set-register ?J      "")  (set-register ?K      "")  (set-register ?L      "")
;;   (set-register ?M      "")  (set-register ?N      "")  (set-register ?O      "")
;;   (set-register ?P      "")  (set-register ?Q      "")  (set-register ?R      "")
;;   (set-register ?S      "")  (set-register ?T      "")  (set-register ?U      "")
;;   (set-register ?V      "")  (set-register ?W      "")  (set-register ?X      "")
;;   (set-register ?Y      "")  (set-register ?Z      "")
;;   ;; :LOWERCASE-REGISTERS
;;   (set-register ?a      "")  (set-register ?b      "")  (set-register ?c      "")
;;   (set-register ?d      "")  (set-register ?e      "")  (set-register ?f      "")
;;   (set-register ?g      "")  (set-register ?h      "")  (set-register ?i      "")
;;   (set-register ?j      "")  (set-register ?k      "")  (set-register ?l      "")
;;   (set-register ?m      "")  (set-register ?n      "")  (set-register ?o      "")
;;   (set-register ?p      "")  (set-register ?q      "")  (set-register ?r      "")
;;   (set-register ?s      "")  (set-register ?t      "")  (set-register ?u      "")
;;   (set-register ?v      "")  (set-register ?w      "")  (set-register ?x      "")
;;   (set-register ?y      "")
;;   (message "all registers set to empty-strings."))
;;; ==============================

;;; ==============================
(provide 'mon-empty-registers)
;;; ==============================

;;; ==============================
;;; mon-empty-registers.el ends here
;;; EOF

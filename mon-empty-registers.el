 ;;; mon-empty-registers.el --- fill and empty register locations en masse.
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-empty-registers.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-08-04T19:31:09-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: registers, convenience, data

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-empty-registers provides utilities for filling/emptying register
;; locations en masse.  Also provides interactive tools for register centric
;; coercion, manipulation, roundtripping of chars, strings, etc.
;; Idea :COURTESY Nelson H. F. Beebe <beebe@math.utah.edu> :HIS bibtex-regs.el
;;
;;
;; FUNCTIONS:►►►
;; `mon-reset-registers', `mon-set-all-registers-to-char',
;; `mon-query-replace-register1<-reg2', `mon-set-register->tags',
;; `mon-make-set-register->tags-docs', `mon-set-register->tags-semic',
;; `mon-set-register->tags-sharp', `mon-set-register-tags-loadtime',
;; `mon-cntl-char-registers-loadtime',
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
;; `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
;; `*mon-digit-registers*', `*mon-digit-shifted-registers*',
;; `*mon-symbol-registers*', `*mon-upper-case-registers*',
;; `*mon-lower-case-registers*', `*register-of-registers*',
;; `*mon-register-config-tags*',
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `*registr-of-registers*' -> `*mon-registr-of-registers*'
;;
;; MOVED:
;; 
;; `mon-query-replace-register1<-reg2' <- mon-replacement-utils.el
;; `mon-coerce->char'                  -> mon-type-utils.el
;; `mon-catch-meta-key'                -> mon-event-utils.el
;; `mon-decode-meta-key-event'         -> mon-event-utils.el
;;
;; TODO:
;; Nothing in this file takes advantage of `prin1-char'. Can it?
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;; 'cl -> `mon-reset-registers' uses `defun*' etc.
;; `mon-help-put-var-doc-val->func' <- `mon-doc-help-utils.el'
;; `mon-stamp'                      <- `mon-time-utils.el' 
;; `mon-is-digit'                   <- `mon-utils'
;; `mon-is-letter'                  <- `mon-utils'
;; `mon-string->symbol'             <- `mon-utils'
;; 
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-empty-registers.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-09-02T17:12:10-04:00Z} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-empty-registers. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-08-04T19:31:09-04:00Z}#{} - by MON KEY>
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
;;; :SNIPPET (string-to-char "d")
;;; :CREATED <Timestamp: #{2009-12-03T14:40:12-05:00Z}#{09494} - by MON>
(defvar *mon-register-config-tags* nil
  "*Alist of char and tag strings for commenting source.
Keys map as follows:
:W-COMMENT-PFX ->  Strings with whitespace wrapping.
:TIMESTAMPED -> Strings with a timestamp appended per `mon-stamp'.
:NO-COMMENT-PFX -> Strings without whitespace wrapping.\n
:CALLED-BY `mon-set-register->tags'.\n
:NOTE Bound with `eval-after-load' call to `mon-set-register-tags-loadtime'.\n
:SEE-ALSO `mon-make-set-register->tags-docs'.\n►►►")
;;
;;; :NOTE Following commented out:
;;; <Timestamp: #{2010-01-13T12:13:02-05:00Z}#{10023} - by MON>
;;; Now binding this var with `mon-set-register-tags-loadtime' at loadtime.
;;; If there are no problems/conflicts can safeley remove this block.
;;; (eval-when (compile load)
;;; (unless (bound-and-true-p *mon-register-config-tags*)
;;;   (setq *mon-register-config-tags*
;;;         '((:W-COMMENT-PFX  . ((87  . ":WAS")           ;; W
;;;                               (70  . ":FROM")          ;; F
;;;                               (84  . ":TO")            ;; T
;;;                               (83  . ":SEE")           ;; S
;;;                               (78  . ":NOTE")          ;; N
;;;                               (79  . ":OLD")           ;; O
;;;                               (77  . ":MATCH")         ;; M
;;;                               (67  . ":CONFLICT")      ;; C
;;;                               (69  . ":EXAMPLE")       ;; E
;;;                               (85  . ":SOURCE")        ;; U
;;;                               (100 . ":DEFAULT")       ;; d
;;;                               (109 . ":MODIFICATIONS") ;; m
;;;                               (102 . ":FILE")          ;; f
;;;                               (104 . ":CHANGESET")     ;; h
;;;                               (116 . ":TEST-ME")       ;; 6
;;;                               (118 . ":VARIABLE")))    ;; v
;;;            (:TIMESTAMPED    . ((99 .  ":CHANGED")      ;; c 
;;;                                (97 .  ":ADDED")))      ;; a
;;;            (:NO-COMMENT-PFX . ((115 . "shell> "))))))  ;; s
;;; )
;;; ) ;; :CLOSE eval-when
;;
;;
;;; :TEST-ME (assoc :W-COMMENT-PFX *mon-register-config-tags*) 
;;; :TEST-ME (assoc :TIMESTAMPED *mon-register-config-tags*)
;;; :TEST-ME (assoc :NO-COMMENT-PFX *mon-register-config-tags*)
;;
;; (progn (makunbound '*mon-register-config-tags*) (unintern "*mon-register-config-tags*" obarray))

;;; ==============================
;;; :NOTE Portions of docstring attached later.
;;; :CREATED <Timestamp: #{2009-12-03T17:50:24-05:00Z}#{09494} - by MON KEY>
;; (eval-and-compile
(defun* mon-set-register->tags (&key sharp semic)
  "
:SEE-ALSO `mon-set-all-registers-to-char', `mon-set-register->tags'
`mon-set-register->tags-semic', `mon-set-register->tags-sharp',
`mon-set-register-tags-loadtime'.\n►►►"
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
                                        (mon-stamp))))) ;; Backquote template
                         )                              ;; Outer lambda
          ))
    (mapc #'(lambda (x) (funcall setr-wsp x)) (cdr (assoc :W-COMMENT-PFX mp-keys)))
    (mapc #'(lambda (x) (funcall setr-no-wsp x)) (cdr (assoc :NO-COMMENT-PFX mp-keys)))
    (mapc #'(lambda (x) (eval (funcall setr-tstamp x))) (cdr (assoc :TIMESTAMPED mp-keys)))))
;; )
;;  
;;; :TEST-ME (progn (mon-set-register->tags :sharp t) (insert-register 77))
;;; :TEST-ME (progn (mon-set-register->tags :semic t)(insert-register 77))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-07T17:29:54-05:00Z}#{09501} - by MON>
(defun mon-set-register->tags-semic ()
"Set the prefix comment style of `mon-set-register->tags' to `;; '.\n
:SEE-ALSO `mon-set-register->tags-sharp', `*mon-register-config-tags*'.\n►►►"
  (interactive)
  (mon-set-register->tags :semic t))
;;
;;; :TEST-ME (call-interactively 'mon-set-register->tags-semic)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-07T17:29:52-05:00Z}#{09501} - by MON>
(defun mon-set-register->tags-sharp ()
"Set the prefix comment style of `mon-set-register->tags' to `# '.\n
:SEE-ALSO `mon-set-register->tags-semic', `*mon-register-config-tags*'.\n►►►"
  (interactive)
  (mon-set-register->tags :sharp t))
;;
;;; :TEST-ME (call-interactively 'mon-set-register->tags-sharp)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-03T17:32:01-05:00Z}#{09494} - by MON>
(defun mon-make-set-register->tags-docs ()
  "Helper function puts register contents/keybinding values on 
docstring of `mon-set-register->tags'.\n
:NOTE Docstring is added at loadtime with `eval-after-load's call to
`mon-set-register-tags-loadtime'.\n
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
    (princ "\n;; :SEMI-C-STYLE\n" (current-buffer))
    ;; :WAS (mon-set-register->tags)
    (mon-set-register->tags-semic)
    (funcall map-sym :W-COMMENT-PFX)
    (funcall map-sym :NO-COMMENT-PFX)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)
    (setq do-s nil
          build-doc nil)
    (funcall map-sym :TIMESTAMPED)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)
    (princ "\n;; :SHARP-STYLE\n" (current-buffer))
    ;; :WAS (mon-set-register->tags :sharp t)
    (mon-set-register->tags-sharp)
    (setq do-s t
          build-doc nil)
    (funcall map-sym :W-COMMENT-PFX)
    (funcall map-sym :NO-COMMENT-PFX)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)
    (setq do-s nil
          build-doc nil)
    (funcall map-sym :TIMESTAMPED)
    (mapc #'(lambda (s) (funcall map-kys s)) build-doc)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-13T11:42:27-05:00Z}#{10023} - by MON KEY>
(defun mon-set-register-tags-loadtime (&optional w-msg-user)
  "Loadtime procedure to bootstrap `mon-set-register-*' functions and variables.
:CALLED-BY `eval-after-load', peforms the following tasks:\n
 o Bind the alist key/value pairs for `*mon-register-config-tags*'.\n
 o Add `mon-set-register->tags' docstring per `*mon-register-config-tags*' vals.\n
When optional arg W-MSG-USER is non-nil message user that variable
`*mon-register-config-tags*' were bound at loadtime.\n
:SEE-ALSO `mon-set-register->tags-semic', `mon-set-register->tags-sharp',
`mon-make-set-register->tags-docs', `mon-bind-cifs-vars-at-loadtime',
`mon-bind-iptables-vars-at-loadtime', `mon-bind-nefs-photos-at-loadtime',
`mon-bind-doc-help-proprietery-vars-at-loadtime',
`mon-set-register-tags-loadtime', `mon-CL-cln-colon-swap', 
`mon-after-mon-utils-loadtime', `mon-check-feature-for-loadtime'.\n►►►"
  (unless (bound-and-true-p *mon-register-config-tags*)
    (setq w-msg-user t)
    (setq *mon-register-config-tags*
          '((:W-COMMENT-PFX  . ((87  .  ":WAS")     ;; W
                                (70  .  ":FROM")    ;; F
                                (84  .  ":TO")      ;; T
                                (83  .  ":SEE")     ;; S
                                (78  .  ":NOTE")    ;; N
                                (79  .  ":OLD")     ;; O
                                (77  .  ":MATCH")   ;; M
                                (67  .  ":CONFLICT") ;; C
                                (69  .  ":EXAMPLE")  ;; E
                                (85  .  ":SOURCE")   ;; U
                                (99  .  ":CHANGED") ;; c  :NOTE non-timestamped version using Cxri-c
                                (97  .  ":ADDED") ;; a  :NOTE non-timestamped version using Cxri-cr
                                (100 .  ":DEFAULT")      ;; d
                                (109 .  ":MODIFICATIONS") ;; m
                                (102 .  ":FILE")          ;; f
                                (104 .  ":CHANGESET")     ;; h
                                (116 .  ":TEST-ME")       ;; 6
                                (118 .  ":VARIABLE")))    ;; v
            (:TIMESTAMPED    . ((97  .  ":ADDED") ;; a :NOTE timestamped version using Ccri-a
                                (99  .  ":CHANGED"))) ;; c :NOTE timestamped version using Ccri-c 
            (:NO-COMMENT-PFX . ((115 . "shell> "))))) ;; s
    ;;
    ;; Now, tack on the docstring for `mon-set-register->tags'
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
        "\n:NOTE Bound with `eval-after-load' call to `mon-set-register-tags-loadtime'.\n
:SEE-ALSO `mon-set-register->tags-semic', `mon-set-register->tags-sharp',
`mon-make-set-register->tags-docs'.\n►►►"))
    (when w-msg-user
      (message (concat ":FUNCTION `mon-set-register-tags-loadtime' "
                   "-- bound  `*mon-register-config-tags*' and documented "
                   "`mon-set-register->tags' at loadtime")))))
;;
;;; :TEST-ME *mon-register-config-tags*
;;; :TEST-ME (describe-function 'mon-set-register->tags)

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T16:12:00-0400Z - by MON KEY>
(defvar *mon-cntl-char-registers* nil)
;;
(unless (bound-and-true-p  *mon-cntl-char-registers*)
  (setq *mon-cntl-char-registers*
        ;;`(,@(number-sequence 1 8) ,@(number-sequence 11 26))
        '(?\C-a ?\C-b ?\C-c ?\C-d ?\C-e ?\C-f ?\C-g ?\C-h ?\C-k ?\C-l 
          ?\C-m ?\C-n ?\C-o ?\C-p ?\C-r ?\C-s ?\C-t ?\C-u ?\C-v ?\C-w
          ?\C-x ?\C-y ?\C-z)))
;;
;;; ==============================
;;; :NOTE Building vars documentation string.
;;; The 23 `nth' calls are prob. costlty... But, character representations
;;; disappear when uploaded to emacs-wiki.
;;; 
;;; ("^A "^B "^C "^D" "^E" "^F" "^G" "^H" "^K" "^L" "^M" "^N"
;;;   0    1   2   3    4    5    6    7    8    9   10   11 
;;;  "^O" "^P" "^Q" "^R" "^S" "^T" "^U" "^V" "^W" "^X" "^Y" "^Z")
;;;   12   13   14   15   16   17   18   19   20    21  22    23
;;;
(defun mon-cntl-char-registers-loadtime ()
  "Loadtime funtion to document the variable `*mon-cntl-char-registers*'.\n
When `IS-MON-SYSTEM-P' evaluated at loadtime by `mon-after-mon-utils-loadtime'.\n
:EXAMPLE\n\n\(progn \(mon-cntl-char-registers-loadtime\)
       \(get '*mon-cntl-char-registers* 'variable-documentation\)\)\n
:SEE-ALSO .\n►►►"
  (let ((char-rep (mapcar #'char-to-string `(,@(number-sequence 1 8) ,@(number-sequence 11 26))))
        self-puke)
    (setq self-puke
          (format
           (concat
            "List of character literals or Control Chars 'C-[a-z]' - ASCII chars 1-26.\n\n"
            "Table maps as follows:\n\n"
            " <INT> <CHAR-REPRESENTATION>\n"
            " <KEY-COMMAND>\n\n"
            "  1 \"%s\"    2 \"%s\"    3 \"%s\"   4 \"%s\"    5 \"%s\"    6 \"%s\"\n "
            " \"?\\C-a\"   \"?\\C-b\"   \"?\\C-c\"  \"?\\C-d\"   \"?\\C-e\"   \"?\\C-f\"\n\n"
            "  7 \"%s\"    8 \"%s\"    9  TAB   10 LF     11 \"%s\"   12 \"%s\"\n "
            " \"?\\C-g\"   \"?\\C-h\"   \"?\\C-i\"  \"\?\\C-j\"   \"?\\C-k\"   \"?\\C-l\"\n\n "
            " 13 \"%s\"   14 \"%s\"   15 \"%s\"  16 \"%s\"   17 \"%s\"   18 \"%s\"\n "
            " \"?\\C-m\"   \"?\\C-n\"   \"?\\C-o\"  \"?\\C-p\"   \"\?\\C-q\"   \"?\\C-r\"\n\n "
            " 19 \"%s\"   20 \"%s\"   21 \"%s\"  22 \"%s\"   23 \"%s\"   24 \"%s\"\n "
            " \"?\\C-s\"   \"?\\C-t\"   \"?\\C-u\"  \"?\\C-v\"   \"?\\C-w\"   \"?\\C-x\"\n\n "
            " 25 \"%s\"   26 \"%s\"\n "
            " \"?\\C-y\"   \"?\\C-z\"\n\n"
            ":NOTE The chars 9, 10, and 17 e.g. TAB and LF 'C-q' are not bound in VARS list.\n"
            "They are included here for completeness.\n\n"
            ":EXAMPLE\n\n\(mapconcat #'char-to-string *mon-cntl-char-registers*  \" \"\)\n\n"
            ":SEE-ALSO `*mon-digit-registers*',`*mon-digit-shifted-registers*',\n"
            "`*mon-symbol-registers*', `*mon-upper-case-registers*',\n"
            "`*mon-lower-case-registers*', `*mon-registr-of-registers*',\n"
            "`*mon-ascii-alpha-chars*', `*mon-digit-chars*', `*mon-whitespace-chars*',\n"
            "`mon-help-ascii-chars'.\n►►►")
           ;; 1-6
           (nth 0 char-rep)(nth 1 char-rep)(nth 2 char-rep)
           (nth 3 char-rep)(nth 4 char-rep)(nth 5 char-rep)
           ;; 7-12
           (nth 6 char-rep)(nth 7 char-rep)
           (nth 8 char-rep)(nth 9 char-rep)
           ;; 13-18
           (nth 10 char-rep)(nth 11 char-rep)(nth 12 char-rep)
           (nth 13 char-rep)(nth 14 char-rep)(nth 15 char-rep)
           ;; 19-24
           (nth 16 char-rep)(nth 17 char-rep)(nth 18 char-rep)
           (nth 19 char-rep)(nth 20 char-rep)(nth 21 char-rep)
           ;; 25-26
           (nth 22 char-rep)(nth 23 char-rep)))
    (put '*mon-cntl-char-registers* 'variable-documentation  self-puke))
  (message (concat ":VARIABLE `*mon-cntl-char-registers*' "
                   "-- now has property `variable-documentation`")))
;;
;;; :TEST-ME  *mon-cntl-char-registers* 
;;; :TEST-ME (symbol-value '*mon-cntl-char-registers*)
;;; :TEST-ME (get '*mon-cntl-char-registers* 'variable-documentation)
;;; :TEST-ME (describe-variable '*mon-cntl-char-registers*)
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-cntl-char-registers*  " ?"))
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-cntl-char-registers*  " ?"))
;;; :TEST-ME (mapconcat 'char-to-string *mon-cntl-char-registers*  " ")
;;
;;;(progn (makunbound '*mon-cntl-char-registers*) (unintern "*mon-cntl-char-registers*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:49:10-0400Z - by MON KEY>
(defvar *mon-digit-registers* nil
  "*Digit chars in 0-9. ASCII range 48-57.\n
;; :CHAR-CODE  48 49 50 51 52 53 54 55 56 57
;; :DIGITS     \?0 \?1 \?2 \?3 \?4 \?5 \?6 \?7 \?8 \?9 \n
:EXAMPLE\n\n(concat \"?\"\(mapconcat 'char-to-string *mon-digit-registers*  \" ?\"\)\)\n
\(mapconcat 'char-to-string *mon-digit-registers*  \" \"\)\n
\(aref *mon-digit-chars* \(car *mon-digit-registers*\)\)
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-digit-shifted-registers*',
`*mon-symbol-registers*', `*mon-upper-case-registers*',
`*mon-lower-case-registers*', `*mon-registr-of-registers*',
`*mon-ascii-alpha-chars*', `*mon-whitespace-chars*', `*mon-digit-chars*',
`mon-help-ascii-chars'.\n►►►")
;;
(unless (bound-and-true-p  *mon-digit-registers*)
  (setq *mon-digit-registers*
        '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
;;
;;; :TEST-ME  *mon-digit-registers*
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-digit-registers*  " ?")))
;;; :TEST-ME (mapconcat 'char-to-string *mon-digit-registers*  "\" \"" )
;;
;;;(progn (makunbound '*mon-digit-registers*) (unintern "*mon-digit-registers*" obarray))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:49:04-0400Z - by MON KEY>
(defvar *mon-digit-shifted-registers* 'nil
  "Symbol chars for shifted digits \(keyboard keys 1-0\).\n
;; :CHAR-CODE    33  64  35  36  37  94  38  42   40   41
;; :SHIFTED      ?!  ?@  ?#  ?$  ?%  ?^  ?&  ?*  ?\\(  ?\\)
;; :NOSHIFT       1   2   3   4   5   6   7   8    9    0\n
:EXAMPLE\n\n\(concat \"?\" \(mapconcat 'char-to-string *mon-digit-shifted-registers*  \" ?\"\)\)\n
\(mapconcat 'char-to-string *mon-digit-shifted-registers*  \" \"\)\n
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-symbol-registers*', `*mon-upper-case-registers*',
`*mon-lower-case-registers*', `*mon-registr-of-registers*',
`*mon-ascii-alpha-chars*', `*mon-whitespace-chars*', `*mon-digit-chars*',
`mon-help-ascii-chars'.\n►►►")
;;
(unless (bound-and-true-p  *mon-digit-shifted-registers*)
  (setq *mon-digit-shifted-registers*
        '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?))))
;;
;;; :TEST-ME  *mon-digit-shifted-registers*
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-digit-shifted-registers*  " ?" ))
;;; :TEST-ME (mapconcat 'char-to-string *mon-digit-shifted-registers*  "\" \"" )
;;
;;;(progn (makunbound '*mon-digit-shifted-registers*) (unintern "*mon-digit-shifted-registers*" obarray))


;;; ==============================
;;; :NOTE ] (\xe93, #o135, #x5d) "?\\\135" 
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:49:01-0400Z - by MON KEY>
(defvar *mon-symbol-registers* 'nil
  "Symbol chars in ASCII ranges 43-47, 60-62, 92-95, and 123-126.\n
;; :CHAR-CODE  43 45 46 47 ¦ 60 61 62 ¦ 91 92  93 95 ¦ 123 124 125 126
;; :ASCII-REP  \?+ \?- \?. \?/ ¦ \?< \?= ?\> ¦ \?[ \?\\\\ \?\] \?_ ¦  \?{  \?|  \?}  \?~\n
:EXAMPLE\n\n\(concat \"?\" \(mapconcat 'char-to-string *mon-symbol-registers*  \" ?\"\)\)\n
\(mapconcat 'char-to-string *mon-symbol-registers*  \" \" \)\n
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-digit-shifted-registers*',
`*mon-upper-case-registers*', `*mon-lower-case-registers*',
`*mon-registr-of-registers*', `*mon-ascii-alpha-chars*',
`*mon-whitespace-chars*', `*mon-digit-chars*', `mon-help-ascii-chars'.\n►►►")
;;
(unless (bound-and-true-p  *mon-symbol-registers*)
  (setq *mon-symbol-registers*
        '(?+ ?- ?. ?/ ?< ?= ?> ?\\ ?\[ ?\] ?_  ?{  ?|  ?}  ?~)))
;;
;;; :TEST-ME  *mon-symbol-registers*
;;; :TEST-ME (mapconcat 'char-to-string *mon-symbol-registers*  " ")
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-symbol-registers*  " ?"))
;;
;;;(progn (makunbound '*mon-symbol-registers*) (unintern "*mon-symbol-registers*" obarray))

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:48:54-0400Z - by MON KEY>;; 
(defvar *mon-upper-case-registers* nil
  "*Uppercase Letters ASCII chars 65-90.\n
;; :CHAR-CODE   65 66 67 68 69 70 71 72 73 74 75 76 77 
;; :ASCII-REP   \?A \?B \?C \?D \?E \?F \?G \?H \?I \?J \?K \?L \?M\n
;; :CHAR-CODE   78 79 80 81 82 83 84 85 86 87 88 89 90
;; :ASCII-REP   \?N \?O \?P \?Q \?R \?S \?T \?U \?V \?W \?X \?Y \?Z\n
:EXAMPLE\n
\(concat \"?\" \(mapconcat 'char-to-string *mon-upper-case-registers*  \" ?\"\)\)\n
\(mapconcat 'char-to-string *mon-upper-case-registers*  \" \" \)\n
\(mapcar 'char-to-string *mon-upper-case-registers*\)\n
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-digit-shifted-registers*',
`*mon-symbol-registers*', `*mon-lower-case-registers*',
`*mon-registr-of-registers*', .\n►►►")
;;
(unless (bound-and-true-p  *mon-upper-case-registers*)
  (setq *mon-upper-case-registers*
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M 
    ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z)))
;;
;;; :TEST-ME  *mon-upper-case-registers*
;;; :TEST-ME (mapconcat 'char-to-string *mon-upper-case-registers*  " " )
;;; :TEST-ME (mapconcat 'char-to-string *mon-upper-case-registers*  " ?" )
;;
;;;(progn (makunbound '*mon-upper-case-registers*) (unintern "*mon-upper-case-registers*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: 2009-08-05-W32-3T15:48:49-0400Z - by MON KEY>
(defvar *mon-lower-case-registers* nil
  "*Lowercase Letters ASCII chars 97-122.\n
  :CHAR-CODE   97  98  99 100 101 102 103 104 105 106 107 108 109
  :ASCII-REP   \?a  \?b  \?c  \?d  \?e  \?f  \?g  \?h  \?i  \?j  \?k  \?l  \?m\n
  :CHAR-CODE  110 111 112 113 114 115 116 117 118 119 120 121 122
  :ASCII-REP   \?n  \?o  \?p  \?q  \?r  \?s  \?t  \?u  \?v  \?w  \?x  \?y  \?z\n
:EXAMPLE\n
\(concat \"?\" \(mapconcat 'char-to-string *mon-lower-case-registers*  \" ?\"\)\)\n
\(mapconcat 'char-to-string *mon-lower-case-registers*  \" \"\)\n
\(mapcar 'char-to-string *mon-lower-case-registers*\)\n
:SEE-ALSO `*mon-cntl-char-registers*', `*mon-cntl-char-registers*',
`*mon-digit-registers*', `*mon-digit-shifted-registers*',
`*mon-symbol-registers*', `*mon-upper-case-registers*',
`*mon-registr-of-registers*', `*mon-ascii-alpha-chars*',
`*mon-whitespace-chars*', `*mon-digit-chars*', `mon-help-ascii-chars'.\n►►►")
;;
(unless (bound-and-true-p  *mon-lower-case-registers*)
  (setq *mon-lower-case-registers*
        '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m 
          ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))
;;
;;; :TEST-ME  *mon-lower-case-registers*
;;; :TEST-ME (concat "?" (mapconcat 'char-to-string *mon-lower-case-registers*  " ?"))
;;; :TEST-ME (mapconcat 'char-to-string *mon-lower-case-registers*  " ")
;;; :TEST-ME (mapcar 'char-to-string *mon-lower-case-registers*)
;;
;;;(progn (makunbound '*mon-lower-case-registers*) (unintern "*mon-lower-case-registers*" obarray) )

;;; ==============================
;;; :RENAMED `*registr-of-registers*' -> `*mon-registr-of-registers*'
;;; :CREATED <Timestamp: #{2009-08-07T19:20:57-04:00Z}#{09325} - by MON KEY>
(defvar *mon-registr-of-registers* nil
   "List mapping symbols to register vars.\n
:CNTRL    -> `*mon-cntl-char-registers*';
:DIGIT    -> `*mon-digit-registers*';
:DIGIT-S  -> `*mon-digit-shifted-registers*';
:SYMBOL   -> `*mon-symbol-registers*';
:UPPER    -> `*mon-upper-case-registers*'
:LOWER    -> `*mon-lower-case-registers*'\n
:SEE-ALSO `*mon-ascii-alpha-chars*', `*mon-whitespace-chars*',
`*mon-digit-chars*', `mon-help-ascii-chars'.\n►►►")
;;
(unless (bound-and-true-p *mon-registr-of-registers*)
  (setq *mon-registr-of-registers*
        '((cntrl   *mon-cntl-char-registers*)
          (digit   *mon-digit-shifted-registers*)
          (digit-S *mon-digit-shifted-registers*)
          (symbol  *mon-symbol-registers*)     
          (upper   *mon-upper-case-registers*) 
          (lower   *mon-lower-case-registers*))))
;;
;;; :TEST-ME *mon-registr-of-registers*
;;; :TEST-ME (assoc 'lower *mon-registr-of-registers*)
;;; :TEST-ME (symbol-value (cadr (assoc 'lower *mon-registr-of-registers*)))
;;
;;;(progn (makunbound '*mon-registr-of-registers*)  (unintern "*mon-registr-of-registers*" obarray) )

;;; ==============================
;;; :PREFIX "mqrrr-"
;;; :COURTESY Nelson H. F. Beebe :HIS bibtools.el :WAS `qr12' 
;;; :NOTE The original didn't read registers they were hard bound to ?1 and ?2.
;;; :CHANGESET 1751 <Timestamp: #{2010-05-20T11:18:50-04:00Z}#{10204} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: 2009-08-04-W32-2T15:28:22-0400Z - by MON KEY>
(defun mon-query-replace-register1<-reg2 (register1 register2 &optional start end use-regexp)
  "Replace occurences of REGISTER1 in buffer or region with contents of REGISTER2.\n
When optional args START END are non-nil limit replacement to region.\n
When optional arg USE-REGEXP is non-nil or called-interactively with prefix-arg 
replace contents of buffer or region as if by `query-replace-regexp'.\n
Default is to replace as if by `query-replace'.
Does not move point.\n
:SEE-ALSO `mon-coerce->char', `mon-set-all-registers-to-char',
`mon-reset-registers'.\n►►►"
  (interactive (list 
                (read-string "Replace string-matching contents of register: ")
                (read-string "With string-matching contents of register: ")
                (when (use-region-p) (region-beginning)) 
                (when (use-region-p) (region-end))
                current-prefix-arg))
  (let ((mqrrr-rgstr1 (mon-coerce->char register1))
        (mqrrr-rgstr2 (mon-coerce->char register2)))
    (save-excursion
      (save-restriction
        (when start (narrow-to-region start end))
         (mon-g2be -1)
        (if current-prefix-arg 
            (query-replace-regexp (get-register mqrrr-rgstr1) (get-register mqrrr-rgstr2))
          (query-replace (get-register mqrrr-rgstr1) (get-register mqrrr-rgstr2)))
        (when start (widen))))))

;;; ==============================
;;; :PREFIX "mrr-"
;;; :CREATED <Timestamp: #{2009-08-08T17:04:24-04:00Z}#{09326} - by MON KEY>
(defun* mon-reset-registers (&key intrp cntrl digit digit-S symbol upper lower all)
  "Reset the contents each all registers associated with keywords.\n
:CNTRL    -> `*mon-cntl-char-registers*';
:DIGIT    -> `*mon-digit-registers*';
:DIGIT-S  -> `*mon-digit-shifted-registers*';
:SYMBOL   -> `*mon-symbol-registers*';
:UPPER    -> `*mon-upper-case-registers*';
:LOWER    -> `*mon-lower-case-registers*';
:ALL      ->  everything in `*mon-registr-of-registers*'
When called interactively or INTRP is non-nil resets all registers.\n
:SEE-ALSO `mon-set-all-registers-to-char', `*mon-registr-of-registers*'.\n►►►"
  (interactive (list :intrp t))
  (let ((mrr-rgstr-kys `(,cntrl ,digit ,digit-S ,symbol ,upper ,lower))
        (mrr-rgstr-lst  '(cntrl  digit  digit-S  symbol  upper lower))
        mrr-prd-lst
        mrr-mk-rgstr-lst
        mrr-rgstr-pop)
    ;; :WAS (setq mrr-prd-lst (pairlis mrr-rgstr-kys mrr-rgstr-lst))
    ;; :WAS (setq mrr-prd-lst (cl::pairlis mrr-rgstr-kys mrr-rgstr-lst))
    (setq mrr-prd-lst (mon-mapcar 'cons mrr-rgstr-kys mrr-rgstr-lst))
    (setq mrr-mk-rgstr-lst '())
    (if (or intrp all)
        ;; (setq mrr-mk-rgstr-lst(mapcar (lambda (x) (symbol-value (cadr x))) *mon-registr-of-registers*))
        (progn
          (mapc #'(lambda (mrr-L-1) 
                    (setq mrr-mk-rgstr-lst (cons (symbol-value (cadr mrr-L-1)) mrr-mk-rgstr-lst))) 
                *mon-registr-of-registers*)
          (setq mrr-mk-rgstr-lst (reverse mrr-mk-rgstr-lst)))
      (progn
        (mapc #'(lambda (mrr-L-2)
                  (when (car mrr-L-2)
                    (setq mrr-mk-rgstr-lst 
                          (cons (symbol-value 
                                 (cadr (assoc (cdr mrr-L-2) *mon-registr-of-registers*)))
                                mrr-mk-rgstr-lst))))
              mrr-prd-lst)
        (setq mrr-mk-rgstr-lst (reverse mrr-mk-rgstr-lst))))
    (setq mrr-rgstr-pop mrr-mk-rgstr-lst)
    (while mrr-rgstr-pop
      (let ((mrr-pop-lst (pop mrr-rgstr-pop)))
        (mapc #'(lambda (mrr-L-3)
                  (set-register mrr-L-3 nil))
              mrr-pop-lst))))
  (when intrp 
    (message  (concat ":FUNCTION `mon-reset-registers' "
                      "-- all registers were emptied"))))

;; (mon-reset-registers :cntrl t)
;;
;;; ==============================
;;; Evaluate following form to refill registers with non-nil val:
;;; (mon-set-all-registers-to-char)
;;;
;;; (mon-reset-registers :cntrl t)
;;; (get-register 26)  ; => :CNTRL
;;;
;;; (mon-set-all-registers-to-char)
;;;
;;; (mon-reset-registers :all t)
;;; (get-register 26)  ; => :CNTRL
;;; (get-register ?9)  ; => :DIGIT  
;;; (get-register ?*)  ; => :DIGIT-S
;;; (get-register ?|)  ; => :SYMBOL 
;;; (get-register ?A)  ; => :UPPER  
;;; (get-register ?z)  ; => :LOWER  
;;;
;;; (mon-set-all-registers-to-char)
;;;
;;; (mon-reset-registers :intrp t)
;;; (mon-set-all-registers-to-char)
;;; (call-interactively 'mon-reset-registers)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-08T16:22:27-04:00Z}#{09326} - by MON KEY>
(defun mon-set-all-registers-to-char ()
  "Set all registers to the string representation of their char-code.\n
Useful when testing/debugging register contents. Examining an empty register
returns nil. Having zapped all register contents it is difficult to tell 
whether if it was emptied programatically or simply never set! We need to reset
all registers to 'something' in order to test they are _NOT_ empty.
:SEE-ALSO `mon-reset-registers', `*mon-registr-of-registers*'.\n►►►"
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

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ==============================
;;; mon-empty-registers.el ends here
;;; EOF

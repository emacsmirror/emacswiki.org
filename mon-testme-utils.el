;;; mon-testme-utils.el --- templates for inserting ``;;; :TEST-ME ''
;; -*- mode: EMACS-LISP; -*-
;;; ================================================================
;;; DESCRIPTION:
;;; mon-testme-utils provides templates for inserting ``;;; :TEST-ME ''. 
;;;
;;; FUNCTIONS:►►►
;;; `mon-insert-test-cases', `mon-insert-lisp-testme', `mon-insert-lisp-testme-fancy', 
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
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;; `mon-insert-lisp-testme'       <- mon-insertion-utils.el
;;; `mon-insert-lisp-testme-fancy' <- mon-insertion-utils.el
;;; `mon-insert-test-cases'        <- mon-insertion-utils.el
;;;
;;; TODO:
;;;
;;; NOTES:
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
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-testme-utils.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2010-02-11T19:48:58-05:00Z}#{10065} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2010-01-22T17:20:30-05:00Z}#{10035} - by MON KEY>
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

(eval-when-compile (require 'cl))

;; `cl::position' <- `mon-insert-lisp-testme-fancy'
;; `cl::intersection' <- `mon-insert-lisp-testme-fancy'
(eval-when-compile (require 'mon-cl-compat))

;;; ==============================
;;; :NOTE See below @BOF for unfinished version mon-insert-lisp-testme-fancy
;;; :CREATED <Timestamp: 2009-07-31-W31-5T13:53:52-0400Z - by MON KEY>
(defun mon-insert-lisp-testme (&optional search-func test-me-count insertp intrp)
  "Insert at point a newline and commented test-me string.\n
When non-nil SEARCH-FUNC will search backward for a function name and include it
in the test-me string.\n
When non-nil TEST-ME-COUNT insert test-me string N times. Default is 1\(one\).
When prefix arg TEST-ME-COUNT is non-nil inerts N number of ';;; :TEST-ME ' strings
and prompts y-or-n-p if we want to include the function name in insertions.
When INSERTP is non-nil insert the test-me string(s) in current buffer at point.
Use at the end of newly created elisp functions to provide example test cases.
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
          t) ; t needed here to prevent returning buffer position when called externally?
        return-tms)))
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
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T16:03:23-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Wednesday March 04, 2009 @ 06:16.40 PM - by MON KEY>
(defun mon-insert-test-cases (&optional insertp intrp)
  "Easily identified tracing vars for debugging defuns mon-*- and naf-mode-*.
Unbinds all previously bound variables:
test-aaa, test-bbb, test-ccc, test-ddd,
test-aa, test-AA, test-ag, test-AG, test-a4, test-A4\n
Rebinds vars 'default values'.  Called-interactively or INSERTP non-nil insert
test-cases at point.\n
:EXAMPLE\n\(mon-insert-test-cases\)\n:SEE-ALSO `mon-insert-lisp-testme'.\n►►►"
  (interactive "i\np") 
  (let ((put-tests))
    (mapc 'makunbound  '(test-aaa test-bbb test-ccc test-ddd test-aa
                         test-AA test-ag test-AG test-a4 test-A4))
    (let ((testing-list 
           '((setq test-aaa '(("1111" "2222") ("3333" "4444") ("5555" "6666") ("7777" "8888") ("9999" "x0x0")))
             (setq test-bbb '(("aaaa" "bbbb") ("cccc" "dddd") ("eeee" "ffff") ("gggg" "hhhh") ("iiii" "jjjj")))
             (setq test-ccc '(("AAAA" "BBBB") ("CCCC" "DDDD") ("EEEE" "FFFF") ("GGGG" "HHHH") ("IIII" "JJJJ")))
             (setq test-ddd '(("a1A1" "b2B2") ("c3C3" "d4D4") ("e5E5" "f6F6") ("g7G7" "h8H8") ("i9I9" "j1J0")))
             (setq test-aa '((a a)(b b)(c c)(d d))) (setq test-AA '((A A)(B B)(C C)(D D)))
             (setq test-ag '((a b)(b c)(d e)(f g))) (setq test-AG '((A B)(B C)(D E)(F G)))
             (setq test-a4 '((a 1) (b 2)(c 3)(d 4))) (setq test-A4 '((A 1)(B 2)(C 3)(D 4))))))
      (setq put-tests (mapconcat (lambda (x)  (format ";;; %S" x)) testing-list "\n")))
    (when (or insertp intrp)
      (save-excursion (newline)(insert put-tests)))
    put-tests))
;;
;;; :TEST-ME (mon-insert-test-cases t)
;;; :TEST-ME (mon-insert-test-cases)
;;; :TEST-ME (call-interactively 'mon-insert-test-cases)

;;; ==============================
;;; :WORKING-ON-THIS AS-OF: 2009-07-18
;;  (defun mon-insert-lisp-evald (&optional gthrp semicolons-p nlp intrp)
;; "GTHRP -> gather-line-p; NLP -> new-lines-p;"
;;  (interactive "i\ni\ni\p")
;;  (let ((line-posns)
;;       (str-len)
;;       (multi-ln-p)
;;       (evald-string)
;;       (semi-c (if semicolons-p
;;                   (cond ((numberp semicolons-p)  semicolons-p)
;;                         ((not (numberp semicolons-p))
;;                          (error "passed `%s' - a %S - as arg value. SEMICOLONS-P need a number."
;;                                 semicolons-p (type-of semicolons-p))))
;;                 1))
;;   ;; check if we have a region and/or are collecting the line
;;   (when (or gthrp use-region-p)
;;     (if use-region-p
;;         (setq line-posns `(,(region-beginning) ,(region-end)))
;;       (setq line-posns
;;             (cond ((bolp)
;;                    `(,(line-beginning-position)
;;                      ,(if (mon-line-bol-is-eol)
;;                           (line-beginning-position)
;;                         (line-end-position))))
;;                   ((not (bolp))
;;                    (progn
;;                      (goto-char (line-beginning-position))
;;                      `(,(line-beginning-position) ,(line-end-position))))))))
;;   ;; new we can set the string
;;   (setq evald-string (buffer-substring-no-properties (car line-posns) (cadr line-posns)))
;;   ;;get the string's length so we can cut it up if needed
;;   (setq str-len (length evald-string))
;;   (cond ((and use-region-p                                  ;region is active
;;               (< str-len fill-column)                   ;don't split at fill column
;;               (= (line-number-at-pos  (car line-posns)) ;not spanning multiple lines
;;                  (line-number-at-pos  (cadr line-posns))))
;;          (setq multi-ln-p nil))
;;         ((and use-region-p
;;               (/= (line-number-at-pos  (car line-posns))
;;                   (line-number-at-pos  (cadr line-posns))))
;;          (progn
;;            (setq multi-ln-p t)
;;            (setq evald-string  (split-string evald-string "\n"))))
;;         ;;region prob. isn't active but,
;;         ((> str-len fill-column) ;string is LONG, so split it.
;;          (let* ((split-at fill-column)
;;                 (split-times (/ str-len fill-column))
;;                 (remn (mod str-len fill-column))
;;                 (sub-str-pos)
;;                 (split-on))
;;            ;;build divisors of string length  |-and remainder
;;            ;;=>(0 80 160 240 320 400 480 560  579)
;;              (setq sub-str-pos
;;                    (do ((i 0 (+ i 1))
;;                         (j 0 (+ fill-column j))
;;                         (k '() (cons j k)))
;;                        ((= split-times (1- i)) k)))
;;              (setq sub-str-pos (cons (+ (car sub-str-pos) remn) l))
;;              (setq sub-str-pos (nreverse sub-str-pos))
;;              (while sub-str-pos
;;                (push (substring evald-string (car sub-str-pos) (cadr sub-str-pos)) split-on)
;;                (pop sub-str-pos))
;;              (setq split-on (reverse split-on))
;;              (setq evald-string split-on)
;;              (setq multi-ln-p t)))
;;            )
;;   ;;finish me
;;   (let* ((semi-top (if ((and use-region-p (> 1 semi-c))
;;                         (concat (make-string (1- semi-c 59)))
;;                         (make-string semi-c 59))))
;;          (top-ln (if nlp
;;                      (concat "\n" semi-top "=>")
;;                    (concat semi-top "=>"))
;;                  ;;(pop evald-string)
;;                  ;;(mapconcat function sequence separator)
;;                  ;; (mapconcat (lambda (x) (concat ";;; " x)) split-on  "\n"))
;;                  (mapconcat (lambda (x) (concat (semiconcancat) (car x)))
;;                             evald-string "$")
;; ;;    (delete-and-extract-region (car line-posns) (cadr line-posns)
;;                  ))))))
;;; ==============================
;;; ==============================

;; (setq (str-len-b (length my-str))
;; (setq my-str
;; "Phasellus porta auctor urna sed elementum. Etiam vel aliquam magna. Nam posuere, quam eu rhoncus tempus, tellus metus vulputate lacus, ac mattis arcu felis ut enim. Suspendisse potenti. Fusce massa diam, semper ut tempus sit amet, feugiat a arcu. Maecenas quis arcu eu felis porttitor interdum sit amet sed ipsum. Cras semper nunc enim, et iaculis urna. Nunc at purus eros, sit amet vulputate lectus. Proin viverra ante vel mi rutrum in posuere tortor mattis. Aenean quis quam sapien. Fusce sed erat leo, vitae feugiat dolor. Ut id mi massa. Curabitur sollicitudin tristique tortor, tincidunt vulputate nibh sagittis a. Proin turpis leo, fringilla sit amet sodales at, malesuada vitae diam. Cras facilisis, odio a elementum varius, nunc dolor porttitor massa, at condimentum metus purus vel ligula. Ut quam est, ornare at congue eu, hendrerit id nulla. Nunc id ligula sapien. Integer non tortor id mauris consectetur accumsan. Donec in orci vel purus pharetra pulvinar ut nec nunc.")


;; (let*  (;temporary
;;         ;;my-str ;evald-string
;;         ;;stre-len-b strn-len
;;         (str-len-b (length my-str))
;;         (split-at fill-column)
;;         (split-times (/ str-len-b split-at))
;;         (remn (mod str-len-b fill-column))
;;         (l)
;;         (split-on))
;;   (setq l
;;         (do ((i 0 (+ i 1))
;;              (j 0 (+ fill-column j))
;;              (k '() (cons j k)))
;;             ((= split-times (1- i)) k)))
;;   (setq l (cons (+ (car l) remn) l))
;;   (setq l (nreverse l)) ;=>(0 80 160 240 320 400 480 560 579)
;;   (while l
;;     (push (substring my-str (car l) (cadr l)) split-on)
;;     (pop l))
;;   (setq split-on (reverse split-on))
;;   (mapconcat (lambda (x) (concat ";;; " x)) split-on  "\n"))
;;; ==============================

;;; ==============================
;;; beginning-of-defun
;;; :FIXME REGEXP doesn't catch on cases where the lambda list is on the next line.
;;; :FIXME Insertion of defvar test-me's should either:
;;;        use a (symbol-value VAR), or; 
;;;        not be placed in side a list form.
;;;
;;; :TODO 
;;; - Consider use of defmacro/defvar/defun* .defvar will need to 
;;;   track on varnames with '*"
;;;
;;; - Provide a facility to include unbinding strings or add a new func e.g.:
;;;   (progn(makunbound 'some-func-or-var) (unintern 'some-func-or-var))
;;;
;;; - The test-me subr should insert differently depending on symbol 'type' 
;;;   and cnt esp. for use with mon-insert-doc-help tail.
;;;   4 'test-me's for functions
;;;   ":TEST-ME (<FNAME>)"
;;;   ":TEST-ME (<FNAME> t)"
;;;   ":TEST-ME (describe-function '<FNAME>)"
;;;   ":TEST-ME (call-interactively '<FNAME>)"
;;;   When a variable is found: should insert:
;;;   ":TEST-ME <VARNAME>"
;;;   ":TEST-ME (describe-variable '<VARNAME>)"
;;;   When a face is found: should insert:
;;;   ":TEST-ME (describe-face '<FACENAME>)"

;;; ==============================
;;; :UNFINISHED-AS-OF <Timestamp: #{2009-10-05T16:03:34-04:00Z}#{09411} - by MON KEY>
;;; :CREATED <Timestamp: 2009-07-31-W31-5T13:53:52-0400Z - by MON KEY>
(defun mon-insert-lisp-testme-fancy (&optional search-func test-me-count insertp intrp)
  "Insert at point a newline and commented test-me string.
When non-nil SEARCH-FUNC will search backward for a function name and include it 
in the test-me string.
When non-nil TEST-ME-COUNT will insert test-me string N times. 
Default is to insert 1\(one\) time.
When prefix arg TEST-ME-COUNT is non-nil inerts N number of ';;;test-me' strings 
and prompt y-or-n-p if we should include the function name with insertion.
When INSERTP is non-nil insert the test-me string(s) in current buffer at point.
Use at the end of newly created elisp functions to provide example test cases.
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
         (sym-nm (if (or search-func get-func)
                   (save-excursion 
                     (search-backward-regexp  *regexp-symbol-defs*)
                     (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (sym-typ (match-string-no-properties 2))
         (fun-syms '("defun" "defun*" "defmacro" "defmacro*" "defsubst" "defsubst*"))
         (var-syms '("defvar" "defconst" "defcustom"))
         ;; (sym-name &key :alt-cookie :do-var :insertp :do-face :do-group :do-theme
         (sym-typ-cond (cond ((car (member found fun-syms))
                              (concat 
                               ";;; :TEST-ME (%s)"
                               (when (car (assoc 'interactive (symbol-function sym-nm)))
                                 "\n;;; :TEST-ME \(call-interactively '%s\)")
                               ;; ==============================
                               ;;Working on this- So finish it then already... 
                               (let ((tst-intrp-l (mon-help-function-args sym-nm))
                                     ;;:WAS (intrp-posn (car (intersection                                      
                                     (intrp-posn (car (cl::intersection 
                                                       '(insertp insert-p insrtp insrt-p 
                                                         :insertp :insert-p :insrtp :insrt-p)
                                                       tst-intrp-l)))
                                     ;; :UNFINISHED-AS-OF 
                                     ;; <Timestamp: #{2009-10-05T16:03:34-04:00Z}#{09411} - by MON KEY>
                                     )
                                     (if intrp-posn
                                         ;;:WAS (position inrp-posn (mon-help-function-args sym-nm)))
                                         (cl::position inrp-posn (mon-help-function-args sym-nm)))
                                     )
                               )) ;; :CLOSE first cond.
                             ;;
                             ((car (member found var-syms)) 
                              ";;; :TEST-ME %s")
                             ((string= found "defface")
                              (concat ";;; :TEST-ME \(facep '%s\)\n"
                                      ";;; :TEST-ME \(face-default-spec '%s\)"))
                             ((string= found "defgroup")
                              ";;; :TEST-ME %s")
                             ((string= found "deftheme")
                              ";;; :TEST-ME \(custom-theme-p '%s\)")
                             (t ";;; :TEST-ME %s")))
         (test-me-string (if (or search-func get-func)
                             ;;  (format  ";;; :TEST-ME (%s )" sym-nm)
                             (format sym-typ-cond sym-nm)
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
          t) ; t needed here to prevent returning buffer position when called externally?
        return-tms)))
;;;
;; :TEMPLATE-FOR-GATHERING-PREV-FUNCTION-DATA
;;
;; (search-backward-regexp *regexp-symbol-defs*)
;; (sym (match-string-no-properties 3)) ;symbol name
;; (found (match-string-no-properties 2)) ;symbol type
;;
;; (cond ((functionp sym-name)
;;        (or (documentation sym-name)
;;            (when (stringp (caddr (symbol-function 'mon-help-function-spit-doc)))
;;              (stringp (caddr (symbol-function 'mon-help-function-spit-doc)))))) ;funcs, macros
;;       (do-var (or (plist-get (symbol-plist sym-name) 'variable-documentation)
;;                   (documentation-property sym-name 'variable-documentation))) ;var, const, customs
;;       (do-face (or (face-documentation sym-name)
;;                    (plist-get (symbol-plist sym-name) 'face-documentation)
;;                    (documentation-property sym-name 'face-documentation))) ;; faces
;;       (do-group (or (plist-get (symbol-plist sym-name) 'group-documentation)
;;                     (documentation-property sym-name 'group-documentation))) ;; groups
;;       (do-theme (or (plist-get (symbol-plist sym-name) 'theme-documentation)
;;                     (documentation-property sym-name 'theme-documentation))) ;; consider 'theme-settings
;;       (t (documentation sym-name)))
;;
;; ;;(mon-help-function-spit-doc (sym-name &key :alt-cookie :do-var :insertp :do-face :do-group :do-theme)
;; (sym-str-cond (cond ((car (member found '("defun" "defun*" "defmacro" "defmacro*")))
;;                           "      (mon-help-function-spit-doc '%s :insertp t)\n")
;;                      ((car (member found '("defvar" "defconst" "defcustom")))
;;                      "      (mon-help-function-spit-doc '%s :do-var t :insertp t)\n")
;;                      ((string= found "defface")
;;                      "      (mon-help-function-spit-doc '%s :do-face t :insertp t)\n")
;;                      ((string= found "defgroup")
;;                       "      (mon-help-function-spit-doc '%s :do-group t :insertp t)\n")
;;                      ((string= found "deftheme")
;;                       "      (mon-help-function-spit-doc '%s :do-theme t :insertp t)\n")
;;                      (t "      (mon-help-function-spit-doc '%s :insertp t)\n")))
;;; ==============================

;;; ==============================
(provide 'mon-testme-utils)
;;; ==============================

;;; ================================================================
;;; mon-testme-utils.el ends here
;;; EOF
